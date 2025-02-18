{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       rangerpt
 Description:  a frame that plots interquartile ranges
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit rangerpt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Menus, Buttons, Dialogs, Math,
  TAGraph, TASeries, TASources, TAStyles, TAGUIConnectorBGRA,
  Graphics, fgl, TACustomSeries, TATransformations, TAIntervalSources,
  TAChartUtils;

type

  { TRangeRptFrame }

  TRangeRptFrame = class(TFrame)
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartGUIConnectorBGRA1: TChartGUIConnectorBGRA;
    ChartStyles1: TChartStyles;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    ExportMenu: TPopupMenu;
    ListChartSource1: TListChartSource;
    Separator1: TMenuItem;
    MnuSettings: TMenuItem;
    MnuCopy: TMenuItem;
    MnuSave: TMenuItem;
    TaskDialog1: TTaskDialog;
    procedure FrameResize(Sender: TObject);
    procedure MnuCopyClick(Sender: TObject);
    procedure MnuSaveClick(Sender: TObject);
    procedure MnuSettingsClick(Sender: TObject);
  private
    ChartIsShowing: Boolean;
    PlotTimeOfDay: Boolean;
    PlotParam, ParamIndex, ParamType: Integer;
    P1, P2, Prange, Pmin, Pmax: Double;
    procedure SetupChartAxes;
    procedure GetPlotParamRange(T: Integer);
    function  GetNodeParamValue(I, T: Integer): Single;
    function  GetLinkParamValue(I, T: Integer): Single;
  public
    procedure InitReport;
    procedure ClearReport;
    procedure RefreshReport;
    procedure ShowPopupMenu;
  end;

implementation

{$R *.lfm}

uses
  main, project, mapthemes, results, utils;

{ TRangeRptFrame }

type
  TDoubleList = specialize TFPGList<Double>;

const
  PlotParams: array[0..5] of Integer =
    (ntDemand, ntDmndDfct, ntPressure, ltVelocity, ltHeadloss, ltLeakage);
  LastNodeParam = 2;

function DoubleCompare(const A, B: Double): Integer;
begin
  if A > B then Result := 1
  else if A < B then Result := -1
  else result := 0;
end;

procedure TRangeRptFrame.ShowPopupMenu;
var
  P : TPoint;
begin
  P := Self.ClientToScreen(Point(0, 0));
  ExportMenu.PopUp(P.x,P.y);
end;

procedure TRangeRptFrame.MnuCopyClick(Sender: TObject);
begin
  Chart1.CopyToClipboardBitmap;
end;

procedure TRangeRptFrame.MnuSaveClick(Sender: TObject);
begin
  with MainForm.SaveDialog1 do
  begin
    FileName := '*.png';
    Filter := 'Portable Network Graphic File|*.png';
    DefaultExt := '*.png';
    if Execute then Chart1.SaveToFile(TPortableNetworkGraphic, FileName);
  end;
end;

procedure TRangeRptFrame.MnuSettingsClick(Sender: TObject);
//
//  Executes TaskDialog1 to select a parameter whose variation is plotted
//
var
  NewParamIndex: Integer;
  NewPlotTimeOfDay: Boolean;
begin
  with TaskDialog1 do
  begin
    // Add an OK button to the dialog
    CommonButtons := [];
    Buttons.Clear;
    with Buttons.Add do
    begin
      Caption := 'OK';
      ModalResult := mrOK;
    end;

    // If chart was already shown then add a Cancel button to the dialog
    if ChartIsShowing then with Buttons.Add do
    begin
      Caption := 'Cancel';
      ModalResult := mrCancel;
    end;

    // Indicate that the chart has already been shown
    ChartIsShowing := True;

    // Make the current parameter be the default radio button on the dialog
    RadioButtons[ParamIndex].Default := True;
    if PlotTimeOfDay then Flags := Flags + [tfVerificationFlagChecked];

    // Display the dialog
    if Execute then
    begin
      // OK button was pressed
      if ModalResult = mrOK then
      begin
        // Get user's choice of parameter to plot
        NewParamIndex := TaskDialog1.RadioButton.Index;
        NewPlotTimeOfDay := tfVerificationFlagChecked in TaskDialog1.Flags;

        // Refresh the plot if parameter has changed
        if (NewParamIndex <> ParamIndex) or
           (NewPlotTimeOfDay <> PlotTimeOfDay) then
        begin
          ParamIndex := NewParamIndex;
          PlotParam := PlotParams[ParamIndex];
          if ParamIndex <= LastNodeParam then
            ParamType := cNodes
          else
            ParamType := cLinks;
          PlotTimeOfDay := NewPlotTimeOfDay;
          RefreshReport;
        end;
      end;
    end;
  end;
end;

procedure TRangeRptFrame.InitReport;
begin
  Prange := 50;
  PlotParam := mapthemes.ntPressure;
  ParamIndex := 2;
  ParamType := project.cNodes;
  ChartIsShowing := False;
  PlotTimeOfDay := False;
end;

procedure TRangeRptFrame.ClearReport;
begin
  ListChartSource1.Clear;
end;

procedure TRangeRptFrame.RefreshReport;
var
  T, Dt: Integer;
  X: TDateTime;
  Xstart, Xstep: TDateTime;
begin
  // Setup chart axes
  SetupChartAxes;

  // Find number of reporting steps between each hour
  Dt := 1;
  if results.Duration > 6 * 3600 then
    Dt := 3600 div results.Rstep;
  if Dt < 1 then Dt := 1;

  // Set start time and report step to hours
  Xstart := results.Rstart / 3600;
  Xstep := results.Rstep / 3600;

  // Set to decimal days if plotting time of day
  if PlotTimeOfDay then
  begin
    Xstart := Xstart / 24;
    Xstep := Xstep / 24;
  end;

  // Clear each data series
  MainForm.Cursor:= crHourglass;
  ListChartSource1.Clear;
  Chart1LineSeries1.Clear;
  Chart1LineSeries2.Clear;

  // Populate the chart with PlotParam's range for each hour
  T := 0;
  while T < results.Nperiods do
  begin
    GetPlotParamRange(T);
    X := Xstart + (T * Xstep);
    with ListChartSource1 do AddXYList(X, [P1, P2]);
    Chart1LineSeries1.AddXY(X, Pmin);
    Chart1LineSeries2.AddXY(X, Pmax);
    T := T + Dt;
  end;

  // Adjustment for single period run
  if results.Nperiods = 1 then with ListChartSource1 do
  begin
    X := Xstart + Xstep;
    AddXYList(X, [P1, P2]);
    Chart1LineSeries1.AddXY(X, Pmin);
    Chart1LineSeries2.AddXY(X, Pmax);
  end;
  MainForm.Cursor := crDefault;

  // Display chart settings dialog when chart first shown
  if not ChartIsShowing then
  begin
    MnuSettingsClick(self);
  end;

end;

procedure TRangeRptFrame.SetupChartAxes;
var
  S: String;
begin
  // Assign chart title and left axis title
  if ParamType = cNodes then S := mapthemes.NodeThemes[PlotParam].Name
  else S := mapthemes.LinkThemes[PlotParam].Name;
  Chart1.Title.Text.Clear;
  Chart1.Title.Text.Add('Variation in System ' + S);
  Chart1.leftAxis.Title.Caption :=
    mapthemes.GetThemeUnits(ParamType, PlotParam);

  // Setup bottom axis
  if PlotTimeOfDay then
  begin
    Chart1.BottomAxis.Marks.Style := smsLabel;
    Chart1.BottomAxis.Marks.Source := DateTimeIntervalChartSource1;
    Chart1.BottomAxis.Title.Visible := False;
  end else
  begin
    Chart1.BottomAxis.Marks.Style := smsValue;
    Chart1.BottomAxis.Marks.Source := nil;
    Chart1.BottomAxis.Title.Visible := True;
  end;

  // Adjustment for single period run
  Chart1.BottomAxis.Marks.Visible := (results.Nperiods > 1);
end;

procedure TRangeRptFrame.GetPlotParamRange(T: Integer);
var
  I, Nitems: Integer;
  N: Double;
  V: Single;
  Vlist: TDoubleList;
begin
  // Create a list of doubles
  Vlist := TDoubleList.Create;

  // Add parameter value for each network object to the Vlist
  Nitems := project.GetItemCount(ParamType);
  for I := 1 to Nitems do
  begin
    if ParamType = cNodes then V := GetNodeParamValue(I, T)
    else V := GetLinkParamValue(I, T);
    if V = MISSING then continue;
    Vlist.Add(V);
  end;

  // Sort the values in the Vlist
  Vlist.Sort(@DoubleCompare);

  // Locate the values that form the desired percentile range
  N := Vlist.Count;
  if Prange = 100 then I := 0 else I := Floor((0.5 - Prange/2/100) * N);
  P1 := Vlist[I];
  if Prange = 100 then I := Vlist.Count-1 else I :=  Floor((0.5 + Prange/2/100) * N);
  P2 := Vlist[I];
  P2 := P2 - P1;
  Pmin := min(Vlist[0], P1);
  Pmax := Vlist[Vlist.Count-1];
  Vlist.Free;
end;

function TRangeRptFrame.GetNodeParamValue(I, T: Integer): Single;
var
  V: Single;
begin
  // Only junction nodes with positive demand are considered
  Result := MISSING;
  if project.GetNodeType(I) <> nJunction then exit;
  if mapthemes.GetNodeValue(I, mapthemes.ntDemand, T) < 0.01 then exit;
  Result := mapthemes.GetNodeValue(I, PlotParam, T);
end;

function TRangeRptFrame.GetLinkParamValue(I, T: Integer): Single;
begin
  // Only pipe links are considered
  Result := MISSING;
  if project.GetLinkType(I) > lPipe then exit;
  Result := mapthemes.GetLinkValue(I, PlotParam, T);
end;

procedure TRangeRptFrame.FrameResize(Sender: TObject);
begin
  utils.ResizeControl(Chart1, ClientWidth, ClientHeight, 90, 50, 16, 0);
end;

end.

