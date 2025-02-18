{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       sysflowrpt
 Description:  a frame that displays a system flow report
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit sysflowrpt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons, Graphics, Menus,
  TAGraph, TASeries, TASources, TAStyles, TAGUIConnectorBGRA,
  TACustomSeries, TAIntervalSources, TATransformations, TAChartUtils;

type

  { TSysFlowFrame }

  TSysFlowFrame = class(TFrame)
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    ChartGUIConnectorBGRA1: TChartGUIConnectorBGRA;
    ChartStyles1: TChartStyles;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    ExportMenu: TPopupMenu;
    ListChartSource1: TListChartSource;
    Separator1: TMenuItem;
    MnuTimeOfDay: TMenuItem;
    MnuCopy: TMenuItem;
    MnuSave: TMenuItem;
    procedure FrameResize(Sender: TObject);
    procedure MnuCopyClick(Sender: TObject);
    procedure MnuSaveClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure MnuTimeOfDayClick(Sender: TObject);
  private
    Produced: Double;
    Consumed: Double;
    Stored: Double;
    Dcf: Double;
    Vcf: Double;
    Dt: Double;
    procedure GetSystemFlowVolumes(T: Integer);
    function  GetInitialStorage: Double;
  public
    procedure ClearReport;
    procedure RefreshReport;
    procedure ShowPopupMenu;
  end;

implementation

{$R *.lfm}

uses
  main, project, mapthemes, results, utils, epanet2;

const
  // Flow conversion factors
  // (Must be in same order as FlowUnitsStr listed in project.pas)
  FlowUcf: array[0..10] of Double =
    (1.0,         // CFS
     448.831,     // GPMperCFS
     0.64632,     // MGDperCFS
     0.5382,      // IMGDperCFS
     1.9837,      // AFDperCFS
     28.317,      // LPSperCFS
     1699.0,      // LPMperCFS
     2.4466,      // MLDperCFS
     101.94,      // CMHperCFS
     2446.6,      // CMDperCFS
     0.028317);   // CMSperCFS

  ImageFileFilter: string = 'PNG File|*.png|JPEG File|*.jpg|BMP File|*.bmp';

procedure TSysFlowFrame.ClearReport;
begin
  ListChartSource1.Clear;
end;

procedure TSysFlowFrame.RefreshReport;
var
  T: Integer;
  X, Xstart, Xstep: TDateTime;
begin
  // Flow units conversion factor to CFS,
  // volume units conversion (ft3 -> Mgal or ft3 -> m3)
  Dcf := FlowUcf[project.FlowUnits];
  Vcf := 0.000007480519;
  if project.GetUnitsSystem = usSI then Vcf := 0.028317;

  // Reporting time step (in sec)
  Dt := results.Rstep;

  // Adjustment for single period run
  if results.Nperiods = 1 then Dt := 3600;

  // Set start time and report step to hours
  Xstart := results.Rstart / 3600;
  Xstep := results.Rstep / 3600;

  // Setup bottom axis
  if MnuTimeOfDay.Checked then
  begin
    Xstart := Xstart / 24;
    Xstep := Xstep / 24;
    Chart1.BottomAxis.Marks.Style := smsLabel;
    Chart1.BottomAxis.Marks.Source := DateTimeIntervalChartSource1;
    Chart1.BottomAxis.Title.Visible := False;
  end else
  begin
    Chart1.BottomAxis.Marks.Style := smsValue;
    Chart1.BottomAxis.Marks.Source := nil;
    Chart1.BottomAxis.Title.Visible := True;
  end;
  Chart1.BottomAxis.Marks.Visible := (results.Nperiods > 1);

  // Assign left axis title with proper units
  if project.GetUnitsSystem = usSI then
    Chart1.LeftAxis.Title.Caption := 'Cubic Meters'
  else
    Chart1.LeftAxis.Title.Caption := 'Million Gallons';

  // Get initial volume stored in tanks (in ft3)
  Stored := GetInitialStorage;

  // Populate the chart with flow volumes in each reporting period
  ListChartSource1.Clear;
  for T := 0 to results.Nperiods - 1 do
  begin
    GetSystemFlowVolumes(T);
    if (T*results.Rstep) mod 3600 < 1 then
    begin
      X := Xstart + (T * Xstep);
      with ListChartSource1 do begin
        AddXYList(X, [Stored*Vcf, Produced*Vcf, Consumed*Vcf]);
      end;
    end;
  end;

  // Adjustment for single period run
  if results.Nperiods = 1 then with ListChartSource1 do
      AddXYList(1, [Stored*Vcf, Produced*Vcf, Consumed*Vcf]);
end;

procedure TSysFlowFrame.ShowPopupMenu;
var
  P : TPoint;
begin
  P := Self.ClientToScreen(Point(0, 0));
  ExportMenu.PopUp(P.x,P.y);
end;

function TSysFlowFrame.GetInitialStorage: Double;
var
  I: Integer;
  V: Single;
begin
  Result := 0;
  for I := 1 to Project.GetItemCount(cNodes) do
  begin
    if Project.GetNodeType(I) = nTank then
    begin
      ENgetnodevalue(I, EN_INITVOLUME, V);
      Result := Result + V;
    end;
  end;
  if GetUnitsSystem = usSI then Result := Result * 35.31467;
end;

procedure TSysFlowFrame.CloseBtnClick(Sender: TObject);
begin
  MainForm.ReportFrame.CloseReport;
end;

procedure TSysFlowFrame.MnuTimeOfDayClick(Sender: TObject);
begin
  MnuTimeOfDay.Checked := not MnuTimeOfDay.Checked;
  RefreshReport;
end;

procedure TSysFlowFrame.MnuCopyClick(Sender: TObject);
begin
  Chart1.CopyToClipboardBitmap;
end;

procedure TSysFlowFrame.FrameResize(Sender: TObject);
begin
  utils.ResizeControl(Chart1, ClientWidth, ClientHeight, 90, 50, 16, 0);
end;

procedure TSysFlowFrame.MnuSaveClick(Sender: TObject);
begin
  with MainForm.SaveDialog1 do
  begin
    FileName := '*.png';
    Filter := 'Portable Network Graphic File|*.png';
    DefaultExt := '*.png';
    if Execute then Chart1.SaveToFile(TPortableNetworkGraphic, FileName);
  end;
end;

procedure TSysFlowFrame.GetSystemFlowVolumes(T: Integer);
var
  I: Integer;
  D: Single;
begin
  Produced := 0;
  Consumed := 0;

  // Visit each network node
  for I := 1 to Project.GetItemCount(cNodes) do
  begin
    // Get the node's demand volume over report time step in ft3
    D := MapThemes.GetNodeValue(I, MapThemes.ntDemand, T);
    if D = MISSING then continue;
    D := D / Dcf * Dt;

    // Update stored volume if node is a Tank
    if Project.GetNodeType(I) = nTank then Stored := Stored + D

    // Otherwise add to volume consumed for demand > 0
    // or to volume produced for demand < 0
    else if D > 0 then Consumed := Consumed + D
    else Produced := Produced - D;
  end;
end;

end.

