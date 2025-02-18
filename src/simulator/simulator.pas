{====================================================================
 Project:      LEPANET
 Version:      0.1
 Module:       simulator
 Description:  a form that runs a simulation of the pipe network
               while displaying its progress
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit simulator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Math;

{$I ..\timetype.txt} //Defines C's 'long' data type for different platforms

type

  { TSimulationForm }

  TSimulationForm = class(TForm)
    CancelBtn: TButton;
    OkBtn: TButton;
    StatusLabel: TLabel;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    ErrorCode: Integer;
    procedure RunSimulation;
    procedure ShowRunStatus;
    procedure RunSolver;
    procedure RunHydraulics;
    procedure RunQuality;
    procedure RunMsxQuality;
    procedure SaveDemandDeficit(var F: TFileStream);
    procedure SaveEmitterFlow(var F: TFileStream);
    procedure SaveNodeLeakage(var F: TFileStream);
    procedure SavePipeLeakage(var F: TFileStream);
  public

  end;

var
  SimulationForm: TSimulationForm;

implementation

{$R *.lfm}

uses
  project, results, energycalc, epanet2, epanetmsx;

const
  TXT_STATUS_NONE = 'Unable to run simulator.';
  TXT_STATUS_WRONGVERSION = 'Run was unsuccessful. Wrong version of simulator.';
  TXT_STATUS_FAILED = 'Run was unsuccessful due to system error.';
  TXT_STATUS_ERROR = 'Run was unsuccessful. See Status Report for reasons.';
  TXT_STATUS_WARNING =
    'Warning messages were generated. See Status Report for details.';
  TXT_STATUS_SUCCESS = 'Run was successful.';
  TXT_STATUS_SHUTDOWN =
   'Simulator performed an illegal operation and was shut down.';
  TXT_STATUS_CANCELLED = 'Run cancelled by user.';
  TXT_OPENING     = 'Opening EPANET solver ...';
  TXT_SOLVING_HYD = 'Solving hydraulics at hour ';
  TXT_SOLVING_WQ  = 'Solving quality at hour ';

{ TSimulationForm }

procedure TSimulationForm.FormCreate(Sender: TObject);
//
//  Form's OnCreate handler
//
begin
  // Position the OK button on top of the Cancel button
  OkBtn.Visible := False;
  OkBtn.Top := CancelBtn.Top;
  OkBtn.Left := CancelBtn.Left;
end;

procedure TSimulationForm.CancelBtnClick(Sender: TObject);
//
//  OnClick handler for the Cancel button
//
begin
  RunStatus := rsCancelled;
end;

procedure TSimulationForm.FormActivate(Sender: TObject);
//
//  Runs the simulation when the form is activated
//
begin
  RunSimulation;

  // Hide the Cancel button and show the OK button
  CancelBtn.Visible := False;
  OkBtn.Visible := True;
  OkBtn.SetFocus;
end;

procedure TSimulationForm.OkBtnClick(Sender: TObject);
//
//  OnClick handler for the OK button
//
begin
  Hide;
end;

procedure TSimulationForm.RunSimulation;
//
//  Executes a simulation of the pipe network
//
begin
  // Prepare the project for a new simulation
  Project.RunStatus := rsNone;
  Project.HasResults := false;
  Project.MsxFlag := Length(Project.MsxInpFile) > 0;
  results.CloseOutFile;
  ErrorCode := 0;
  ENclearreport;

  // Run the EPANET hydraulic and water quality solvers
  RunSolver;
  ShowRunStatus;

  // Copy EPANET's status report to the project's auxilary file
  ENcopyreport(PAnsiChar(AuxFile));
end;

procedure TSimulationForm.ShowRunStatus;
//
//  Display the final status of the simulation run
//
var
  MsxRunStatus: TRunStatus;
begin
  // Open EPANET's binary output file to retrieve simulation status
  if not (RunStatus in [rsCancelled, rsShutdown]) then
  begin
    if ErrorCode > 0 then RunStatus := rsError
    else if not FileExists(Project.OutFile) then RunStatus := rsFailed
    else RunStatus := results.OpenOutFile(Project.OutFile);

    // Open the MSX output file
    if MsxFlag and (RunStatus in [rsSuccess, rsWarning]) then
    begin
      if not FileExists(Project.MsxOutFile) then RunStatus := rsFailed
      else
      begin
        MsxRunStatus := results.OpenMsxOutFile(Project.MsxOutFile);
        if MsxRunStatus <> rsSuccess then RunStatus := MsxRunStatus;
      end;
    end;
  end;

  // Display run status message
  case RunStatus of
    rsShutdown:     StatusLabel.Caption := TXT_STATUS_SHUTDOWN;
    rsNone:         StatusLabel.Caption := TXT_STATUS_NONE;
    rsWrongVersion: StatusLabel.Caption := TXT_STATUS_WRONGVERSION;
    rsFailed:       StatusLabel.Caption := TXT_STATUS_FAILED;
    rsError:        StatusLabel.Caption := TXT_STATUS_ERROR;
    rsWarning:      StatusLabel.Caption := TXT_STATUS_WARNING;
    rsSuccess:      StatusLabel.Caption := TXT_STATUS_SUCCESS;
    rsCancelled:    StatusLabel.Caption := TXT_STATUS_CANCELLED;
  end;
  if RunStatus in [rsSuccess, rsWarning] then Project.HasResults := true;
end;

procedure TSimulationForm.RunSolver;
//
//  Runs the hydraulic & water quality simulation
//
var
  FPUExceptionMask: TFPUExceptionMask;
begin
  // Save and re-set FPU exception mask
  FPUExceptionMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);

  // Run EPANET's hydraulics solver
  ErrorCode := 0;
  if ErrorCode = 0 then RunHydraulics;

  // Run EPANET's water quality solver
  if (ErrorCode < 100) and (RunStatus <> rsCancelled) then
  begin
    if MsxFlag then RunMsxQuality else RunQuality;
  end;

  // Restore FPU exception mask
  SetExceptionMask(FPUExceptionMask);
end;

procedure TSimulationForm.RunHydraulics;
var
  t, tstep: TimeType;
  rptStep: TimeType;
  F: TFileStream;
begin
  // Create a demand deficit file (since the current EPANET solver
  // doesn't record demand deficit in its output file)
  F := TFileStream.Create(project.DmndFile, fmCreate);
  try
    // Open EPANET's hydraulics solver
    epanet2.ENgettimeparam(EN_REPORTSTEP, rptStep);
    energycalc.Start;
    ErrorCode := epanet2.ENopenH();
    if ErrorCode = 0 then
    begin

      // Initialize hydraulics solver to save its results to file
      epanet2.ENinitH(EN_SAVE);
      t := 0;

      // Solve hydraulics in each period
      repeat
        // Update display of simulation progress
        if t mod 3600 = 0 then
        begin
          StatusLabel.Caption := TXT_SOLVING_HYD + IntToStr(t div 3600);
          Application.ProcessMessages;
        end;

        // Solve hydraulics at current time
        ErrorCode := epanet2.ENrunH(t);

        // Save demand deficit & leakage to file if at a reporting time
        if t mod rptStep = 0 then
        begin
          SaveDemandDeficit(F);
          SaveEmitterFlow(F);
          SaveNodeLeakage(F);
          SavePipeLeakage(F);
        end;

        // Determine size of next hydraulic time step
        tstep := 0;
        if ErrorCode <= 100 then ErrorCode := epanet2.ENnextH(tstep);

        // Update system energy usage over the time step
        energycalc.Update(Integer(t), Integer(tstep));
      until (tstep = 0) or (ErrorCode > 100) or (RunStatus = rsCancelled);
    end;

    // Close hydraulics solver
    energycalc.Finish;
    epanet2.ENcloseH();

    // Save hydraulic results for use by MSX solver
    if (ErrorCode <= 100) and (RunStatus <> rsCancelled) and MsxFlag then
    begin
      epanet2.ENsaveH;
      epanet2.ENsavehydfile(PAnsiChar(MsxHydFile));
    end;

    // Ignore any warning code
    if ErrorCode <= 100 then ErrorCode := 0;
  finally
    F.Free;
  end;
end;

procedure TSimulationForm.RunQuality;
//
//  Runs EPANET's single species water quality solver
//
var
  t, tstep: TimeType;
begin
  // Open quality solver
  ErrorCode := epanet2.ENopenQ();
  if ErrorCode = 0 then
  begin

    // Initialize WQ solver
    epanet2.ENinitQ(EN_SAVE);
    t := 0;

    //  Solve WQ in each period
    repeat
      if t mod 3600 = 0 then
      begin
        StatusLabel.Caption := TXT_SOLVING_WQ + IntToStr(t div 3600);
        Application.ProcessMessages;
      end;
      ErrorCode := epanet2.ENrunQ(t);
      tstep := 0;
      if ErrorCode <= 100 then ErrorCode := epanet2.ENnextQ(tstep);
    until (tstep = 0) or (ErrorCode > 100) or (RunStatus = rsCancelled);
  end;

  // Close WQ solver
  epanet2.ENcloseQ();
end;

procedure TSimulationForm.RunMsxQuality;
//
//  Runs EPANET-MSX's multi-species water quality solver
//
var
  t, tleft: Double;
  OldHour, NewHour: Int64;
begin
  // Open MSX solver and make saved hydraulic results available to it
  ErrorCode := epanetmsx.MSXopen(PAnsiChar(MsxInpFile));
  if ErrorCode = 0 then ErrorCode := epanetmsx.MSXusehydfile(PAnsiChar(MsxHydFile));
  if ErrorCode = 0 then
  begin
    // Initialize WQ solver
    ErrorCode := epanetmsx.MSXinit(1);
    t := 0;
    tleft := 0;
    OldHour := -1;
    NewHour := 0;

    // Solve WQ in each period
    repeat
      if NewHour > OldHour then
      begin
        OldHour := NewHour;
        StatusLabel.Caption := TXT_SOLVING_WQ + IntToStr(NewHour);
       Application.ProcessMessages;
      end;
      ErrorCode := epanetmsx.MSXstep(t, tleft);
      NewHour := Trunc(t/3600);
    until (tleft = 0) or (ErrorCode > 0) or (RunStatus = rsCancelled);
  end;

  // Save WQ results and close MSX solver
  if (tleft = 0) and (ErrorCode = 0) and (RunStatus <> rsCancelled) then
  begin
    ErrorCode := epanetmsx.MSXsaveoutfile(PAnsiChar(MsxOutFile));
  end;
  epanetmsx.MSXclose;
end;

procedure TSimulationForm.SaveDemandDeficit(var F: TFileStream);
//
//  Saves current demand deficit at each network node to file
//
var
  Deficits: array of Single;
  Demands: array of Single;
  I, N, ByteCount: Integer;
begin
  if F = nil then exit;
  ENgetcount(EN_NODECOUNT, N);
  SetLength(Deficits, N);
  SetLength(Demands, N);
  ByteCount := N * sizeof(Single);
  epanet2.ENgetnodevalues(EN_DEMANDDEFICIT, Deficits);
  epanet2.ENgetnodevalues(EN_FULLDEMAND, Demands);
  for I := 0 to N-1 do
  begin
    if Demands[I] > 0.0 then
      Deficits[I] := Deficits[I] / Demands[I] * 100.;
  end;
  F.Write(Deficits[0], ByteCount);
end;

procedure TSimulationForm.SaveEmitterFlow(var F: TFileStream);
//
//  Saves current leakage flow from each network node to file
//
var
  Flows: array of Single;
  N, ByteCount: Integer;
begin
  if F = nil then exit;
  epanet2.ENgetcount(EN_NODECOUNT, N);
  SetLength(Flows, N);
  ByteCount := N * sizeof(Single);
  epanet2.ENgetnodevalues(EN_EMITTERFLOW, Flows);
  F.Write(Flows[0], ByteCount);
end;

procedure TSimulationForm.SaveNodeLeakage(var F: TFileStream);
//
//  Saves current leakage flow from each network node to file
//
var
  Leakages: array of Single;
  N, ByteCount: Integer;
begin
  if F = nil then exit;
  epanet2.ENgetcount(EN_NODECOUNT, N);
  SetLength(Leakages, N);
  ByteCount := N * sizeof(Single);
  epanet2.ENgetnodevalues(EN_LEAKAGEFLOW, Leakages);
  F.Write(Leakages[0], ByteCount);
end;

procedure TSimulationForm.SavePipeLeakage(var F: TFileStream);
//
//  Saves current leakage flow from each network link to file
//
var
  Leakages: array of Single;
  N, ByteCount: Integer;
begin
  if F = nil then exit;
  epanet2.ENgetcount(EN_LINKCOUNT, N);
  SetLength(Leakages, N);
  ByteCount := N * sizeof(Single);
  epanet2.ENgetlinkvalues(EN_LINK_LEAKAGE, Leakages);
  F.Write(Leakages[0], ByteCount);
end;

end.

