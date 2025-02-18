{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       projectsummary
 Description:  a form that displays a summary of a project's objects
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit projectsummary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLtype,
  ComCtrls;

{$I ..\timetype.txt}

type

  { TSummaryForm }

  TSummaryForm = class(TForm)
    TreeView1: TTreeView;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    procedure ShowNodeCount;
    procedure ShowLinkCount;
    procedure ShowCurveCount;
    procedure ShowPatternCount;
    procedure ShowControlCount;

  public

  end;

var
  SummaryForm: TSummaryForm;

implementation

{$R *.lfm}

uses
  epanet2, project, utils, config;

{ TSummaryForm }

procedure TSummaryForm.FormShow(Sender: TObject);
var
  I: Integer;
  S: String;
  T: TimeType;
begin
  Color := Config.ThemeColor;
  Font.Size := Config.FontSize;
  ShowNodeCount;
  ShowLinkCount;
  ShowCurveCount;
  ShowPatternCount;
  ShowControlCount;
  ENgetflowunits(I);
  Epanet2.ENgettimeparam(EN_DURATION, T);
  with TreeView1 do
  begin
    Items[29].Text := Project.FlowUnitsStr[I] + ' Flow Units';
    Items[30].Text := Project.GetHlossModelStr + ' Head Loss Model';
    Items[31].Text := Project.GetDemandModelStr + ' Demand Model';
    S := Project.GetQualModelStr;
    if not SameText(S, 'No Quality') then S := S + ' Quality Model'
    else S := S + ' Model';
    Items[32].Text := S;
    Items[33].Text := utils.Time2Str(T) + ' Hour Duration' ;
  end;

end;

procedure TSummaryForm.ShowNodeCount;
var
  Count, I, J: Integer;
  JuncCount: Integer = 0;
  ResvCount: Integer = 0;
  TankCount: Integer = 0;
begin
  ENgetcount(EN_NODECOUNT, Count);
  TreeView1.Items[1].Text := IntToStr(Count) + ' Nodes';
  for I := 1 to Count do
  begin
    J := GetNodeType(I);
    case J of
      nJunction: Inc(JuncCount);
      nReservoir: Inc(ResvCount);
      nTank: Inc(TankCount);
    end;
  end;
  with TreeView1 do
  begin
    Items[2].Text := IntToStr(JuncCount) + ' Junctions';
    Items[3].Text := IntToStr(ResvCount) + ' Reservoirs';
    Items[4].Text := IntToStr(TankCount) + ' Tanks';
  end;
end;

procedure TSummaryForm.ShowLinkCount;
var
  Count, I, J, K: Integer;
  PipeCount: Integer = 0;
  PumpCount: Integer = 0;
  ValveCount: Integer = 0;
  ValveTypeCount: array[0..6] of Integer = (0,0,0,0,0,0,0);
begin
  ENgetcount(EN_LINKCOUNT, Count);
  TreeView1.Items[5].Text := IntToStr(Count) + ' Links';
  for I := 1 to Count do
  begin
    J := GetLinkType(I);
    case J of
      lPipe: Inc(PipeCount);
      lPump: Inc(PumpCount);
      lValve: Inc(ValveCount);
    end;
    if J = lValve then
    begin
      Epanet2.ENgetlinktype(I, K);
      K := K - EN_PRV;
      Inc(ValveTypeCount[K]);
    end;
  end;
  with TreeView1 do
  begin
    Items[6].Text := IntToStr(PipeCount) + ' Pipes';
    Items[7].Text := IntToStr(PumpCount) + ' Pumps';
    Items[8].Text := IntToStr(ValveCount) + ' Valves';
    for K := 0 to 6 do
    begin
      Items[9+K].Text := IntToStr(ValveTypeCount[K]) + ' ' +
        ValveTypeStr[K] + 's';
    end;
  end;
end;

procedure TSummaryForm.ShowCurveCount;
var
  Count: Integer;
  I, K: Integer;
  CurveTypeCount: array[0..5] of Integer = (0,0,0,0,0,0);
begin
  ENgetcount(EN_CURVECOUNT, Count);
  TreeView1.Items[17].Text := IntToStr(Count) + ' Data Curves';
  for I := 1 to Count do
  begin
    Epanet2.ENgetcurvetype(I, K);
    Inc(CurveTypeCount[K]);
  end;
  for K := 0 to 5 do
  begin
    TreeView1.Items[18+K].Text := IntToStr(CurveTypeCount[K]) + ' ' +
      CurveTypeStr[K] + ' Curves';
  end;
end;

procedure TSummaryForm.ShowPatternCount;
var
  Count: Integer;
begin
  ENgetcount(EN_PATCOUNT, Count);
  TreeView1.Items[24].Text := IntToStr(Count) + ' Time Patterns';
end;

procedure TSummaryForm.ShowControlCount;
var
  SimpleCount: Integer;
  RuleCount: Integer;
begin
  ENgetcount(EN_CONTROLCOUNT, SimpleCount);
  ENgetcount(EN_RULECOUNT, RuleCount);
  TreeView1.Items[25].Text := IntToStr(SimpleCount+RuleCount) + ' Link Controls';
  TreeView1.Items[26].Text := IntToStr(SimpleCount) + ' Simple Controls';
  TreeView1.Items[27].Text := IntToStr(RuleCount) + ' Rule Based Controls';
end;

procedure TSummaryForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

end.

