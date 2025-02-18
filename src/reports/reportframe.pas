{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       reportframe
 Description:  a frame used to display different reports
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit reportframe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons,
  calibrationrpt, Dialogs, StdCtrls;

const
  ReportTypeStr: array[0..9] of String =
    ('Status Report', 'Pumping Report', 'Calibration Report',
     'Network Nodes Report', 'Network Links Report', 'Time Series Plot',
     'Hydraulic Profile Plot', 'System Flow Report', 'Energy Report',
     'Parameter Variability Plot');

type

  // Types of reports
  TReportType = (rtStatus,        // Simulation status report
                 rtPumping,       // Pumping report
                 rtCalib,         // Calibration report
                 rtNodes,         // Network nodes report
                 rtLinks,         // Network links report
                 rtTimeSeries,    // Time series report
                 rtProfile,       // Hyd. profile report
                 rtSysFlow,       // System flows report
                 rtEnergy,        // Energy report
                 rtRange,         // Interquartile range plot
                 rtNone);

  { TReportFrame }

  TReportFrame = class(TFrame)
    CloseBtn: TSpeedButton;
    MinimizeBtn: TSpeedButton;
    ResizeBtn: TSpeedButton;
    MenuBtn: TSpeedButton;
    TopPanel: TPanel;
    procedure CloseBtnClick(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure FrameExit(Sender: TObject);
    procedure ResizeBtnClick(Sender: TObject);
    procedure MenuBtnClick(Sender: TObject);
    procedure MinimizeBtnClick(Sender: TObject);
  private
    ReportType : TReportType;
    procedure CreateReport(RptType: TReportType);
  public
    Report : TFrame;
    procedure Init;
    procedure Close;
    procedure ShowReport(RptType: TReportType);
    procedure ChangeTimePeriod;
    procedure RefreshReport;
    procedure ClearReport;
    procedure CloseReport;
  end;

implementation

{$R *.lfm}

uses
  main, project, statusrpt, sysflowrpt, pumpingrpt, timeseriesrpt, networkrpt,
  energyrpt, rangerpt, hydprofilerpt;

procedure TReportFrame.Init;
begin
  ResizeBtn.ImageIndex := 2;
  ResizeBtn.Hint := 'Maximize';
  ReportType := rtNone;
  Report := nil;
end;

procedure TReportFrame.Close;
begin
  CloseReport;
end;

procedure TReportFrame.ShowReport(RptType: TReportType);
begin
  if RptType = ReportType then
  begin
    if ReportType = rtTimeSeries then
      TTimeSeriesFrame(Report).DataMenuItemClick(self)
    else if ReportType = rtProfile then
      THydProfileFrame(Report).MnuProfileClick(self);
  end
  else CreateReport(RptType);
  with MainForm.ReportPanel  do
  begin
    if Height < 2 then Height := MainForm.MainPanel.Height div 2;
  end;
  MainForm.ReportSplitter.Visible := True;
end;

procedure TReportFrame.FrameEnter(Sender: TObject);
begin
  MainForm.KeyPreview := False;
end;

procedure TReportFrame.CloseBtnClick(Sender: TObject);
begin
  CloseReport;
end;

procedure TReportFrame.FrameExit(Sender: TObject);
begin
  MainForm.KeyPreview := True;
end;

procedure TReportFrame.ResizeBtnClick(Sender: TObject);
begin
  // Resize button image is Maximize
  if ResizeBtn.ImageIndex = 2 then
  begin
    MainForm.ReportPanel.Height := MainForm.MainPanel.Height -
      MainForm.ReportSplitter.Height;
    ResizeBtn.ImageIndex := 4;
    ResizeBtn.Hint := 'Restore';
  end

  // Resize button image is Restore
  else begin
    MainForm.ReportPanel.Height := MainForm.MainPanel.Height div 2;
    ResizeBtn.ImageIndex := 2;
    ResizeBtn.Hint := 'Maximize';
  end;
end;

procedure TReportFrame.MenuBtnClick(Sender: TObject);
begin
  if Report = nil then exit;
  case ReportType of
    rtStatus:     TStatusRptFrame(Report).ShowPopupMenu;
    rtEnergy:     TEnergyRptFrame(Report).ShowPopupMenu;
    rtRange:      TRangeRptFrame(Report).ShowPopupMenu;
    rtCalib:      TCalibRptFrame(Report).ShowPopupMenu;
    rtSysFlow:    TSysFlowFrame(Report).ShowPopupMenu;
    rtPumping:    TPumpingRptFrame(Report).ShowPopupMenu;
    rtTimeSeries: TTimeSeriesFrame(Report).ShowPopupMenu;
    rtProfile:    THydProfileFrame(Report).ShowPopupmenu;
    rtNodes,
    rtLinks:      TNetworkRptFrame(Report).ShowPopupMenu;
  end;
end;

procedure TReportFrame.MinimizeBtnClick(Sender: TObject);
begin
  ResizeBtn.ImageIndex := 4;
  ResizeBtn.Hint := 'Restore';
  with MainForm.ReportPanel  do
  begin
    Height := TopPanel.Height + MainForm.ReportSplitter.Height;
  end;
end;

procedure TReportFrame.CreateReport(RptType: TReportType);
begin
  if Report <> nil then CloseReport;
  ReportType := RptType;
  case ReportType of
  rtStatus:
    begin
      Report := TStatusRptFrame.Create(self);
    end;
  rtEnergy:
    begin
      Report := TEnergyRptFrame.Create(self);
    end;
  rtCalib:
    begin
      Report := TCalibRptFrame.Create(self);
      TCalibRptFrame(Report).InitReport;
    end;
  rtRange:
    begin
      Report := TRangeRptFrame.Create(self);
      TRangeRptFrame(Report).InitReport;
    end;
  rtProfile:
    begin
    Report := THydProfileFrame.Create(self);
    THydProfileFrame(Report).InitReport;
    end;
  rtSysFlow:
    begin
      Report := TSysFlowFrame.Create(self);
    end;
  rtPumping:
    begin
      Report := TPumpingRptFrame.Create(self);
      TPumpingRptFrame(Report).InitReport;
    end;
  rtTimeSeries:
    begin
      Report := TTimeSeriesFrame.Create(self);
      TTimeSeriesFrame(Report).InitReport;
    end;
  rtNodes:
    begin
      Report := TNetworkRptFrame.Create(Self);
      TNetworkRptFrame(Report).InitReport(cNodes);
    end;
  rtLinks:
    begin
      Report := TNetworkRptFrame.Create(Self);
      TNetworkRptFrame(Report).InitReport(cLinks);
    end;
  end;
  TopPanel.Caption := ReportTypeStr[Integer(ReportType)];
  Report.Parent := self;
  Report.Align := alClient;
  MainForm.ReportPanel.Show;
  RefreshReport;
end;

procedure TReportFrame.ClearReport;
begin
  if Report = nil then exit;
  case ReportType of
    rtStatus:     TStatusRptFrame(Report).ClearReport;
    rtCalib:      TCalibRptFrame(Report).ClearReport;
    rtSysFlow:    TSysFlowFrame(Report).ClearReport;
    rtTimeSeries: TTimeSeriesFrame(Report).ClearReport;
  end;
end;

procedure TReportFrame.RefreshReport;
begin
  if Report = nil then exit;
  case ReportType of
    rtStatus:     TStatusRptFrame(Report).RefreshReport;
    rtEnergy:     TEnergyRptFrame(Report).RefreshReport;
    rtRange:      TRangeRptFrame(Report).RefreshReport;
    rtCalib:      TCalibRptFrame(Report).Refreshreport;
    rtSysFlow:    TSysFlowFrame(Report).RefreshReport;
    rtPumping:    TPumpingRptFrame(Report).RefreshReport;
    rtTimeSeries: TTimeSeriesFrame(Report).RefreshReport;
    rtNodes,
    rtLinks:      TNetworkRptFrame(Report).RefreshReport;
  end;
end;

procedure TReportFrame.ChangeTimePeriod;
begin
  if Report = nil then exit;
  if ReportType in [rtNodes, rtLinks] then
    TNetworkRptFrame(Report).RefreshReport
  else if ReportType = rtProfile then
    THydProfileFrame(Report).RefreshReport;
end;

procedure TReportFrame.CloseReport;
begin
  // Close and free the current report being displayed
  if Report = nil then exit;
  case ReportType of
    rtCalib:       TCalibRptFrame(Report).CloseReport;
    rtPumping:     TPumpingRptFrame(Report).CloseReport;
    rtTimeSeries:  TTimeSeriesFrame(Report).CloseReport;
    rtProfile:     THydProfileFrame(Report).CloseReport;
  end;
  if Report is TNetworkRptFrame then TNetworkRptFrame(Report).CloseReport;
  FreeAndNil(Report);
  ReportType := rtNone;

  // Reset the image used on the frame's resize button to Maximize
  ResizeBtn.ImageIndex := 2;

  // Hide report panel and its splitter
  MainForm.ReportPanel.Hide;
  MainForm.ReportPanel.Height := MainForm.MainPanel.Height div 2;
  MainForm.ReportSplitter.Hide;
end;

end.

