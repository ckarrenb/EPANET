{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       timeseriesrpt
 Description:  A frame that displays a time series report
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit timeseriesrpt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, Menus, Grids, Buttons,
  TAGraph, TASeries, TATransformations, TAIntervalSources, TAGUIConnectorBGRA,
  TAChartUtils, LCLType, Types, Clipbrd, StrUtils;

const
  MaxSeries = 6;

type

  // Properties of a data series to be displayed
  TDataSeries = record
    ObjType: Integer;    // Node or Link
    ObjIndex: Integer;   // Node/Link index
    ObjParam: Integer;   // Parameter to be plotted
    PlotAxis: Integer;   // Plot on left (0) or right (1) axis
    Title: string;       // Title used in legend
  end;

  { TTimeSeriesFrame }

  TTimeSeriesFrame = class(TFrame)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Chart1LineSeries3: TLineSeries;
    Chart1LineSeries4: TLineSeries;
    Chart1LineSeries5: TLineSeries;
    Chart1LineSeries6: TLineSeries;
    LeftChartAxisTransformationsAutoScaleAxisTransform: TAutoScaleAxisTransform;
    RightChartAxisTransformations: TChartAxisTransformations;
    LeftChartAxisTransformations: TChartAxisTransformations;
    ChartGUIConnectorBGRA1: TChartGUIConnectorBGRA;
    ChartPage: TPage;
    DataGrid: TDrawGrid;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    ChartMenuItem: TMenuItem;
    RightChartAxisTransformationsAutoScaleAxisTransform: TAutoScaleAxisTransform;
    SaveMenuItem: TMenuItem;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    TableMenuItem: TMenuItem;
    DataMenuItem: TMenuItem;
    SettingsMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    Notebook1: TNotebook;
    PopupMenu1: TPopupMenu;
    TablePage: TPage;
    procedure ChartMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure DataGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DataGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure DataMenuItemClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SettingsMenuItemClick(Sender: TObject);
    procedure TableMenuItemClick(Sender: TObject);
  private
    ShowingChart: Boolean;
    PlotTimeOfDay: Boolean;
    Nseries: Integer;
    DataSeries: array[0..MaxSeries-1] of TDataSeries;
    procedure PlotSeries(I: Integer);
    procedure RefreshDataGrid;
    function  GetDataGridValue(C: Integer; R: Integer): String;
    procedure GetDataGridContents(Slist: TStringList);
    procedure SetupChart;
    procedure SaveChart;
    procedure SaveTable;

  public
    procedure InitReport;
    procedure CloseReport;
    procedure ClearReport;
    procedure RefreshReport;
    procedure ShowPopupMenu;
    procedure SetDataSeries(NewDataSeries: array of TDataSeries;
      NewPlotTimeOfDay: Boolean; HasChanged: Boolean);
    function  GetObjStr(ObjType: Integer; Item: Integer): string;
    function  GetParamStr(ObjType: Integer; Param: Integer): string;

  end;

implementation

{$R *.lfm}

uses
  main, project, config, mapthemes, results, utils, chartoptions;

const
  SeriesColors: array[1..MaxSeries] of TColor =
    ($E5B533, $CC66AA, $CC99, $33BBFF, $4444FF, clBlack);

{ TTimeSeriesFrame }

procedure TTimeSeriesFrame.InitReport;
var
  I, FontSize: Integer;
begin

  // Set alternate row color for the data grid
  DataGrid.AlternateColor := config.AlternateColor;

  // Initialize object being plotted in each data series
  for I := 0 to High(DataSeries) do
  begin
    DataSeries[I].ObjType := -1;
    DataSeries[I].PlotAxis := 0;
  end;

  // Assign colors to each data series
  for I := 1 to MaxSeries do
  begin
    with FindComponent('Chart1LineSeries' + IntToStr(I)) as TLineSeries do
      SeriesColor := SeriesColors[I];
  end;

  // Set font size to that used by project
  FontSize := config.FontSize;
  for I := 0 to 2 do
    Chart1.AxisList[I].Title.LabelFont.Size := FontSize;
  Chart1.Legend.Font.Size := FontSize;

  // Start with no data series specified
  Nseries := 0;
  ShowingChart := True;
  PlotTimeOfDay := False;
  Notebook1.PageIndex := Notebook1.IndexOf(ChartPage);
  Chart1.Visible := False;

  // Bring up the Time Series Selector frame
  DataMenuItemClick(Self);
end;

procedure TTimeSeriesFrame.CloseReport;
var
  I: Integer;
begin
  for I := 1 to MaxSeries do
    with FindComponent('Chart1LineSeries' + IntToStr(I)) as TLineSeries do
      Clear;
  MainForm.TseriesSelectorFrame.Visible := False;
end;

procedure TTimeSeriesFrame.ShowPopupMenu;
var
  P : TPoint;
begin
  P := Self.ClientToScreen(Point(0, 0));
  PopupMenu1.PopUp(P.x,P.y);
end;

procedure TTimeSeriesFrame.DataMenuItemClick(Sender: TObject);
begin
  MainForm.HideLeftPanelFrames;
  MainForm.TseriesSelectorFrame.Visible := True;
  MainForm.TseriesSelectorFrame.Init(DataSeries, PlotTimeOfDay);
end;

procedure TTimeSeriesFrame.FrameResize(Sender: TObject);
begin
  utils.ResizeControl(Chart1, ClientWidth, ClientHeight, 90, 50, 16, 0);
end;

procedure TTimeSeriesFrame.SaveMenuItemClick(Sender: TObject);
begin
  if ShowingChart then SaveChart
  else SaveTable;
end;

procedure TTimeSeriesFrame.SaveChart;
begin
  with MainForm.SaveDialog1 do
  begin
    FileName := '*.png';
    Filter := 'Portable Network Graphic File|*.png';
    DefaultExt := '*.png';
    if Execute then Chart1.SaveToFile(TPortableNetworkGraphic, FileName);
  end;
end;

procedure TTimeSeriesFrame.SaveTable;
var
  Slist: TStringList;
begin
  with MainForm.SaveDialog1 do begin
    FileName := '*.txt';
    Filter := 'Text File|*.txt|All Files|*.*';
    DefaultExt := '*.txt';
    if Execute then
    begin
      Slist := TStringList.Create;
      try
        GetDataGridContents(Slist);
        Slist.SaveToFile(FileName);
      finally
        Slist.Free;
      end;
    end;
  end;
end;

procedure TTimeSeriesFrame.SettingsMenuItemClick(Sender: TObject);
var
  OptionsForm: TChartOptionsForm;
begin
  OptionsForm := TChartOptionsForm.Create(self);
  with OptionsForm do
  try
    SetOptions(Chart1);
    ShowModal;
    if ModalResult = mrOK then GetOptions(Chart1);
  finally
    Free;
  end;
end;

procedure TTimeSeriesFrame.TableMenuItemClick(Sender: TObject);
begin
  ShowingChart := False;
  Notebook1.PageIndex := Notebook1.IndexOf(TablePage);
end;

procedure TTimeSeriesFrame.CopyMenuItemClick(Sender: TObject);
var
  Slist: TStringList;
begin
  if ShowingChart then Chart1.CopyToClipboardBitmap
  else begin
    Slist := TStringList.Create;
    try
      GetDataGridContents(Slist);
      Clipboard.AsText := Slist.Text;
    finally
      Slist.Free;
    end;
  end;
end;

procedure TTimeSeriesFrame.DataGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  with Sender as TDrawGrid do
    Canvas.TextRect(aRect, aRect.Left+2, aRect.Top+2,
      GetDataGridValue(aCol, aRow));
end;

procedure TTimeSeriesFrame.DataGridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  MyTextStyle: TTextStyle;
begin
  MyTextStyle := DataGrid.Canvas.TextStyle;
  if aRow = 0 then
  begin
    MyTextStyle.SingleLine := false;
    MyTextStyle.Alignment := taCenter;
  end
  else if aCol > 0 then
    MyTextStyle.Alignment := taRightJustify;
  DataGrid.Canvas.TextStyle := MyTextStyle;
end;

procedure TTimeSeriesFrame.SetDataSeries(NewDataSeries: array of TDataSeries;
  NewPlotTimeOfDay: Boolean; HasChanged: Boolean);
//
// This procedure is called by the Time Series Selector frame to
// transfer its selections (stored in NewDataSeries) to the
// DataSeries array. HasChanged is TRUE if the NewDataSeries is not
// the same as the current DataSeries.
//
var
  I: Integer;
begin
  // If the report has no data series assigned then close it
  if NewDataSeries[0].ObjType < 0 then
  begin
    MainForm.ReportFrame.CloseReport;
    exit;
  end;

  // The current DataSeries array has not been changed
  if not HasChanged then
  begin
    if not Chart1.Visible then MainForm.ReportFrame.CloseReport;
    exit;
  end;

  // Transfer the NewDataSeries to this report's DataSeries
  for I := 0 to High(DataSeries) do
  begin
    DataSeries[I] := NewDataSeries[I];

    // Add a legend title if none exists
    with DataSeries[I] do
    begin
      if Length(Title) = 0 then
        Title := GetObjStr(ObjType, ObjIndex-1) + ' ' +
          GetParamStr(ObjType, ObjParam);
    end;
  end;
  PlotTimeOfDay := NewPlotTimeOfDay;

  // Display the data series in chart or table form
  SetupChart;
  if ShowingChart then
    ChartmenuItemClick(Self)
  else
    TableMenuItemClick(Self);
  RefreshReport;
end;

procedure TTimeSeriesFrame.ChartMenuItemClick(Sender: TObject);
begin
  Notebook1.PageIndex := Notebook1.IndexOf(ChartPage);
  ShowingChart := true;
end;

function TTimeSeriesFrame.GetObjStr(ObjType: Integer; Item: Integer): string;
begin
  Result := Project.GetObjectStr(ObjType, Item);
end;

function TTimeSeriesFrame.GetParamStr(ObjType: Integer; Param: Integer): string;
begin
  if ObjType = cNodes then
    Result := MapThemes.NodeThemes[Param].Name
  else
    Result := MapThemes.LinkThemes[Param].Name;
end;

procedure TTimeSeriesFrame.PlotSeries(I: Integer);
var
  T: Integer;
  Y: Double;
  X: TDateTime;
  Xstart, Xstep: TDateTime;
begin
  // Get a reference to the I-th line series being plotted
  with FindComponent('Chart1LineSeries' + IntToStr(I+1)) as TLineSeries do
  begin
    // Clear and deactivate the series
    Clear;
    Active := false;

    // Exit if the data series wasn't assigned an object to plot
    if DataSeries[I].ObjType < 0 then exit;

    // Save which Y-axis (left or right) the series is plotted on
    if DataSeries[I].PlotAxis = 1 then
    begin
      Chart1.AxisList[2].Visible := true;
      AxisIndexY := 2;
    end
    else
    begin
      Chart1.AxisList[0].Visible := true;
      AxisIndexY := 0;
    end;

    // Convert the starting time and reporting interval to Date/Time values
    // (decimal days)
    Xstart := results.Rstart / 3600;
    Xstep := results.Rstep / 3600;
    if PlotTimeOfDay then
    begin
      Xstart := Xstart / 24;
      Xstep := Xstep / 24;
    end;

    // Add the parameter value of the object assigned to the data series
    // in each reporting time period
    for T := 0 to results.Nperiods - 1 do
    begin
      X := Xstart + (T * Xstep);
      with DataSeries[I] do
      begin
        if ObjType = cNodes then
          Y := MapThemes.GetNodeValue(ObjIndex, ObjParam, T) else
          Y := MapThemes.GetLinkValue(ObjIndex, ObjParam, T);
      end;
      if Y = MISSING then continue else AddXY(X, Y);
    end;

    // Adjustment for single period run
    if results.Nperiods = 1 then
    begin
      X := Xstart + Xstep;
      AddXY(X, Y);
    end;
    Chart1.BottomAxis.Marks.Visible := (results.Nperiods > 1);

    // Add the data series title to the plot and activate it
    Title := DataSeries[I].Title;
    Active := true;
  end;
  Chart1.Visible := True;
end;

procedure TTimeSeriesFrame.RefreshDataGrid;
begin
  with DataGrid do
  begin
    Clear;
    RowCount := Results.Nperiods + 1;
    RowHeights[0] := 2 * DataGrid.DefaultRowHeight;
    Refresh;
  end;
end;

function TTimeSeriesFrame.GetDataGridValue(C: Integer; R: Integer): String;
//
// Gets time series value for column C, row R of the report table.
//
var
  I: Integer;
begin
  // I is 0-based index into DataSeries array
  I := C - 1;

  // Column 0 displays time
  Result := '';
  if C = 0 then
  begin
    if R = 0 then Result := 'Time' + LineEnding +'(hrs)'
    else Result := Results.GetTimeStr(R-1);
  end

  // Column C displays results for DataSeries[I]
  else if DataSeries[I].ObjType >= 0 then
  begin
    // Column header
    if R = 0 then
      Result := GetObjStr(DataSeries[I].ObjType, DataSeries[I].ObjIndex - 1) +
                LineEnding +
                GetParamStr(DataSeries[I].ObjType, DataSeries[I].ObjParam)
    // Time series value (previously stored in chart's LineSeries)
    else with FindComponent('Chart1LineSeries' + IntToStr(C)) as TLineSeries do
      Result := FloatToStrF(GetYValue(R-1), ffFixed, 7, config.DecimalPlaces) + '  ';
  end;
end;

procedure TTimeSeriesFrame.SetupChart;
var
  I: Integer;
  UnitsStr: String;
  LeftAxisTitle: String = '';
  RightAxisTitle: String = '';
begin
  // Examine each data series
  Nseries := 0;
  for I := 0 to High(DataSeries) do
  begin
    // Break at first empty data series
    if DataSeries[I].ObjType < 0 then break;
    Inc(Nseries);

    // Get the series' parameter units
    UnitsStr := mapthemes.GetThemeUnits(DataSeries[I].ObjType, DataSeries[I].ObjParam);

    // If series uses a left axis
    if DataSeries[I].PlotAxis = 0 then
    begin
      if not AnsiContainsStr(LeftAxisTitle, UnitsStr) then
      begin
        if Length(LeftAxisTitle) = 0 then LeftAxisTitle := UnitsStr
        else LeftAxisTitle := LeftAxisTitle + ', ' + UnitsStr;
      end;
    end

    // Repeat above steps if series uses a right axis
    else if DataSeries[I].PlotAxis = 1 then
    begin
      if not AnsiContainsStr(RightAxisTitle, UnitsStr) then
      begin
        if Length(RightAxisTitle) = 0 then RightAxisTitle := UnitsStr
        else RightAxisTitle := RightAxisTitle + ', ' + UnitsStr;
      end;
    end;
  end;

  // Assign titles to chart's axes
  Chart1.AxisList[0].Title.Caption := LeftAxisTitle;
  Chart1.AxisList[2].Title.Caption := RightAxisTitle;
  Chart1.Legend.Visible := true;
  Chart1.Title.Visible := false;
end;

procedure TTimeSeriesFrame.GetDataGridContents(Slist: TStringList);
var
  I, R: Integer;
  S: string;
begin
  // Add a title to the contents' stringlist
  S := Project.GetTitle(0);
  Slist.Add(S);
  S := 'Time Series Report';
  Slist.Add(S);
  Slist.Add('');

  // The DataGrid's header row contains text on two lines -- add each as
  // a separate row to the contents' stringlist
  with DataGrid do
  begin
    S := 'Time      ';
    for I := 1 to Nseries do
      S := S + #9 +
        Format('%-20s',
          [GetObjStr(DataSeries[I-1].ObjType, DataSeries[I-1].ObjIndex - 1)]);
    Slist.Add(S);
    S := '(hrs)     ';
    for I := 1 to Nseries do
      S := S + #9 +
        Format('%-20s',
          [GetParamStr(DataSeries[I-1].ObjType, DataSeries[I-1].ObjParam)]);
    Slist.Add(S);
  end;

  // Then add each successive row of the DataGrid to the stringlist
  for R := 1 to DataGrid.RowCount - 1 do
  begin
    S := Format('%-10s', [GetDataGridValue(0,R)]);
    for I := 1 to Nseries do
      S := S + #9 + Format('%-20s', [GetDataGridValue(I, R)]);
    Slist.Add(S);
  end;
end;

procedure TTimeSeriesFrame.ClearReport;
begin
end;

procedure TTimeSeriesFrame.RefreshReport;
var
  I: Integer;
begin

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
  Chart1.AxisList[2].Visible := false;

  // Plot each data series
  for I := 0 to High(DataSeries) do PlotSeries(I);

  // Add data series values to the DataGrid control
  RefreshDataGrid;
end;

end.

