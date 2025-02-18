{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       energyrpt
 Description:  a frame that displays an energy balance report
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit energyrpt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Menus, Buttons,
  Dialogs, Graphics, Math, Clipbrd, Types, TAGraph, TASeries, TASources,
  TAGUIConnectorBGRA;

type

  { TEnergyRptFrame }

  TEnergyRptFrame = class(TFrame)
    Chart1: TChart;
    Chart1PieSeries1: TPieSeries;
    Chart1PieSeries2: TPieSeries;
    ChartGUIConnectorBGRA1: TChartGUIConnectorBGRA;
    ExportMenu: TPopupMenu;
    PerformanceLabel: TLabel;
    MetricsLabel: TLabel;
    MetricValuesLabel: TLabel;
    ReqPressLabel: TLabel;
    ListChartSource1: TListChartSource;
    MnuCopy: TMenuItem;
    MnuSave: TMenuItem;
    MainPanel: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    procedure CloseBtnClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure MnuCopyClick(Sender: TObject);
    procedure MnuSaveClick(Sender: TObject);
  private
    procedure CopyMainPanel(FileName: String);
    procedure SaveToPngFile(Bmp: TBitmap; FileName: String);

  public
    procedure ClearReport;
    procedure RefreshReport;
    procedure ShowPopupMenu;

  end;

implementation

{$R *.lfm}

uses
  main, project, energycalc;

procedure TEnergyRptFrame.CloseBtnClick(Sender: TObject);
begin
  MainForm.ReportFrame.CloseReport;
end;

procedure TEnergyRptFrame.FrameResize(Sender: TObject);
begin
  MainPanel.Left := (self.ClientWidth - MainPanel.Width) div 2;
  Chart1.Left := MainPanel.Left + 24;
end;

procedure TEnergyRptFrame.ShowPopupMenu;
var
  P : TPoint;
begin
  P := Self.ClientToScreen(Point(0, 0));
  ExportMenu.PopUp(P.x,P.y);
end;

procedure TEnergyRptFrame.MnuCopyClick(Sender: TObject);
begin
  CopyMainPanel('');
end;

procedure TEnergyRptFrame.MnuSaveClick(Sender: TObject);
begin
  with MainForm.SaveDialog1 do
  begin
    FileName := '*.png';
    Filter := 'Portable Network Graphic File|*.png';
    DefaultExt := '*.png';
    if Execute then CopyMainPanel(FileName);
  end;
end;

procedure TEnergyRptFrame.ClearReport;
begin
  Chart1PieSeries1.Clear;
end;

procedure TEnergyRptFrame.RefreshReport;
var
  I: Integer;
  Einput, Eoutput, Efactor, E: Double;
  Metric: array[0..5] of String;
  Eunits, Txt: String;
begin
  // Total energy input and output
  Einput := (Energy[eInflows] + Energy[ePumping] + Energy[eTankOut]);
  Eoutput := (Energy[eDemands] + Energy[eLeakage] + Energy[eFriction] +
    Energy[eTankIn]);

  // Choose between Kwh and MwH units
  if (Einput > 1000) or (Eoutput > 1000) then
  begin
    Efactor := 1000;
    Eunits := 'MwH/Day';
  end
  else
  begin
    Efactor := 1;
    Eunits := 'KwH/Day';
  end;

  // Assign energy values to chart
  for I := eInflows to eTankIn do
  begin
    E := Energy[I] / Efactor;
    if E = 0 then E := NAN;
    ListChartSource1[I-1]^.Y := E;
  end;

  // Update chart title
  Chart1.Title.Text.Clear;
  Chart1.Title.Text.Add('System Energy Balance (' + Eunits + ')');

  // Update chart footer
  Chart1.Foot.Text.Clear;
  Chart1.Foot.Text.Add(Format('Total Energy Supplied (Es):    %.1f %s',
    [Einput / Efactor, Eunits]));
  Chart1.Foot.Text.Add(Format('Total Energy Consumed:       %.1f %s',
    [Eoutput / Efactor, Eunits]));
  Chart1.Refresh;

  // Assign performance metrics
  ReqPressLabel.Caption := Format('to meet demand at %s', [energycalc.PreqStr]);
  for I := 0 to 5 do Metric[I] := 'N/A';
  if Einput > 0 then
  begin
    Metric[0] := Format('%.2f', [Energy[eDemands] / Einput]);
    Metric[1] := Format('%.2f', [Energy[eFriction] / Einput]);
    Metric[2] := Format('%.2f', [Energy[eLeakage] / Einput]);
  end;
  if Energy[eMinUse] > 0 then
  begin
    Metric[3] := Format('%.2f', [Einput / Energy[eMinUse]]);
    Metric[4] := Format('%.2f', [Energy[eDemands] / Energy[eMinUse]]);
    Metric[5] := Format('%.2f', [Energy[eMinUse] / Efactor]);
  end;
  Txt := Metric[0];
  for I := 1 to 5 do
  begin
    Txt := Txt + LineEnding + LineEnding + Metric[I];
  end;
  MetricValuesLabel.Caption := Txt;
end;

procedure TEnergyRptFrame.CopyMainPanel(FileName: String);
var
  Bmp: TBitmap;
  R1, R2: TRect;
begin
  Bmp := TBitmap.Create;
  try
    // R1 contains the Chart's area
    R1 := Rect(0, 0, Chart1.Width-1, Chart1.Height-1);

    // R2 contains the area on the MainPanel where the Chart appears
    R2 := R1;
    OffsetRect(R2, Chart1.Left - MainPanel.Left, Chart1.Top);

    // Copy the MainPanel's contents to bitmap Bmp -- the Chart is not included
    Bmp.SetSize(MainPanel.Width, MainPanel.Height);
    MainPanel.PaintTo(Bmp.Canvas,0,0);

    // Copy the Chart onto bitmap Bmp in its properly offset location
    Bmp.Canvas.CopyRect(R2, Chart1.Canvas, R1);

    // Save bitmap Bmp either to clipboard or file
    if Length(FileName) = 0 then Bmp.SaveToClipboardFormat(CF_BITMAP)
    else SaveToPngFile(Bmp, FileName);
  finally
    Bmp.Free;
  end;
end;

procedure TEnergyRptFrame.SaveToPngFile(Bmp: TBitmap; FileName: String);
var
  Pic: TPicture;
begin
  Pic := TPicture.Create;
  try
    Pic.Assign(Bmp);
    Pic.SaveToFile(FileName, 'png');
  finally
    Pic.Free;
  end;
end;

end.

