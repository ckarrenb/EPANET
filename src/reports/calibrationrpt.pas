{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       calibrationrpt
 Description:  A frame that displays a calibration report
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit calibrationrpt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls, Grids,
  Buttons, Menus, TAGraph, TASeries, TALegend, TATypes, TAChartUtils, Math,
  Graphics, Dialogs;

type

  TSums = record
    N: Integer;
    SumX, SumY, SumX2, SumY2, SumXY, SumE, SumE2: double;
  end;

  { TCalibRptFrame }

  TCalibRptFrame = class(TFrame)
    ClearBtn: TBitBtn;
    ComputedSeries: TLineSeries;
    CopyMnuItem: TMenuItem;
    CorrelationPlot: TChart;
    DataGrid: TStringGrid;
    ErrorStatsMemo: TMemo;
    Exportmenu: TPopupMenu;
    Label1: TLabel;
    Label2: TLabel;
    LoadBtn: TBitBtn;
    LocationsHeader: TPanel;
    LocationsListBox: TListBox;
    LocationsPanel: TPanel;
    MeasuredSeries: TLineSeries;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    Page4: TPage;
    SaveBtn: TBitBtn;
    SaveMnuItem: TMenuItem;
    TabControl1: TTabControl;
    TimeSeriesChart: TChart;
    VariableCombo: TComboBox;
    procedure ClearBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure CopyMnuItemClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure LocationsListBoxClick(Sender: TObject);
    procedure Page1Exit(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SaveMnuItemClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure TabControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure VariableComboChange(Sender: TObject);
  private
    ObjectType: Integer;
    LocationIndex: Integer;
    VariableIndex: Integer;
    DataHasChanged: Boolean;
    procedure SetDataGridHeadings;
    procedure LoadCalibData(Fname: String);
    procedure GetLocationsList(LocationsList: TStringList);
    procedure FillLocationsListBox(LocationsList: TStringList);
    procedure LoadMeasuredData(aSeries: TLineSeries; aLocation: String);
    procedure RefreshTimeSeriesPlot;
    procedure RefreshErrorStats;
    procedure InitCorrelationPlot;
    procedure FinalizeCorrelationPlot(Rcoeff: Double);
    function  GetSimulatedValue(T: Single): Single;
    function  ReadDataGrid: Boolean;
    procedure InitSums(var Sums: TSums);
    procedure UpdateSums(X, Y: Double; var Sums: TSums);
    function  FindCorrelCoeff(Sums: TSums): Double;
    procedure DisplayErrorStats(Sums: array of TSums);
    procedure SaveChartToFile(aChart: TChart);
    function  GetFileName(Fname: string; Ftypes: string; DefType: string): string;

  public
    procedure InitReport;
    procedure CloseReport;
    procedure ClearReport;
    procedure RefreshReport;
    procedure ShowPopupMenu;

  end;

implementation

{$R *.lfm}

uses
  main, project, mapthemes, results, utils;

const
    MarkerColors : array[0..14] of TColor =
    (clBlack, clRed, clPurple, clLime, clBlue, clFuchsia, clAqua,
     clGray, clGreen, clMaroon, clYellow, clTeal, clSilver, clNavy,
     clOlive);
    TXT_HEADING1 =
    '                Num    Observed    Computed    Mean     RMS';
    TXT_HEADING2 =
    '  Location      Obs        Mean        Mean   Error   Error';
    TXT_HEADING3 =
    '  ---------------------------------------------------------';

procedure TCalibRptFrame.LoadBtnClick(Sender: TObject);
begin
  // Use the main form's OpenDialog component to select
  // a file containing calibration data and load it into
  // the DataGrid on the form's Calibration Data page.
  with MainForm.OpenDialog1 do
  begin
    FileName := '*.dat';
    Filter := 'Data File|*.dat|All Files|*.*';
    DefaultExt := '*.dat';
    if Execute then LoadCalibData(FileName);
  end;
end;

procedure TCalibRptFrame.ClearBtnClick(Sender: TObject);
begin
  DataGrid.Clean;
  DataGrid.RowCount := 2;
  LocationsListBox.Clear;
  SetDataGridHeadings;
  DataHasChanged := True;
end;

procedure TCalibRptFrame.CloseBtnClick(Sender: TObject);
begin
  MainForm.ReportFrame.CloseReport;
end;

procedure TCalibRptFrame.CopyMnuItemClick(Sender: TObject);
begin
  with Notebook1 do
  begin
    if PageIndex = IndexOf(Page1) then
      DataGrid.CopyToClipboard
    else if PageIndex = IndexOf(Page2) then
      TimeSeriesChart.CopyToClipboardBitmap
    else if PageIndex = IndexOf(Page3) then
      CorrelationPlot.CopyToClipboardBitmap
    else if PageIndex = IndexOf(Page4) then with ErrorStatsmemo do
    begin
      SelectAll;
      CopyToClipboard;
      SelLength := 0;
    end;
  end;
end;

procedure TCalibRptFrame.LocationsListBoxClick(Sender: TObject);
begin
  // After a new location is selected by the user the plot of
  // simulated and observed results is refreshed using data
  // for that location.
  RefreshTimeSeriesPlot;
end;

procedure TCalibRptFrame.ShowPopupMenu;
var
  P : TPoint;
begin
  P := Self.ClientToScreen(Point(0, 0));
  ExportMenu.PopUp(P.x,P.y);
end;

procedure TCalibRptFrame.Page1Exit(Sender: TObject);
begin
  if DataGrid.Modified then DataHasChanged := True;
  if DataHasChanged then RefreshReport;
  DataGrid.Modified := False;
end;

procedure TCalibRptFrame.SaveBtnClick(Sender: TObject);
var
  Fname: String;
begin
  Fname := GetFileName('*.dat', 'Calibration File|*.dat|', 'dat');
  if Length(Fname) > 0 then
    DataGrid.SaveToCSVFile(Fname, ' ', false);
end;

procedure TCalibRptFrame.SaveMnuItemClick(Sender: TObject);
var
  Fname: String;
begin
  with Notebook1 do
  begin
    if PageIndex = IndexOf(Page1) then
      SaveBtnClick(Sender)
    else if PageIndex = IndexOf(Page2) then
      SaveChartToFile(TimeSeriesChart)
    else if PageIndex = IndexOf(Page3) then
      SaveChartToFile(CorrelationPlot)
    else if PageIndex = IndexOf(Page4) then
    begin
      Fname := GetFileName('*.txt', 'Text File|*.txt|', 'txt');
      if Length(Fname) > 0 then
        ErrorStatsMemo.Lines.SaveToFile(Fname);
    end;
  end;
end;

procedure TCalibRptFrame.TabControl1Change(Sender: TObject);
begin
  if TabControl1.TabIndex = 0 then
  begin
    Notebook1.PageIndex := 0;
    DataHasChanged := False;
    DataGrid.Modified := False;
  end
  else begin
    Notebook1.PageIndex := TabControl1.TabIndex;
    if DataHasChanged then RefreshReport;
  end;
end;

procedure TCalibRptFrame.TabControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  // If on Calibration Data page, check if allowed to select a new tab
  if (TabControl1.TabIndex = 0) and DataHasChanged then
    AllowChange := ReadDataGrid
  else AllowChange := True;
end;

procedure TCalibRptFrame.VariableComboChange(Sender: TObject);
begin
  // Detemine type of object (nodes or links) selected
  ObjectType := project.cNodes;
  if VariableCombo.ItemIndex in [2,3] then
    ObjectType := project.cLinks;

  // Detrmine index of variable selected
  with VariableCombo do
  begin
    case VariableCombo.ItemIndex of
    0: VariableIndex := mapthemes.ntHead;
    1: VariableIndex := mapthemes.ntPressure;
    2: VariableIndex := mapthemes.ltFlow;
    3: VariableIndex := mapthemes.ltVelocity;
    else VariableIndex := mapthemes.FirstNodeQualTheme + ItemIndex - 4;
    end;
  end;
  SetDataGridHeadings;
  DataHasChanged := True;
end;

procedure TCalibRptFrame.SetDataGridHeadings;
begin
  // Fill in captions for the DataGrid's header (row 0)
  with DataGrid do
  begin
    if ObjectType = cNodes then
      Cells[0,0] := 'Node ID'
    else
      Cells[0,0] := 'Link ID';
    Cells[1,0] := 'Time (Hrs)';
    Cells[2,0] := VariableCombo.Text;
  end;
end;

procedure TCalibRptFrame.LoadCalibData(Fname: String);
var
  DataList: TStringList;
  Tokens: TStringArray;
  I, J: Integer;
  S: String;
begin
  DataGrid.Clear;
  J := 1;
  DataList := TStringList.Create();
  try
    DataList.LoadFromFile(Fname);
    DataGrid.RowCount := DataList.Count + 1;
    SetDataGridHeadings;
    for I := 0 to DataList.Count - 1 do
    begin
      S := Trim(DataList[I]);
      if S.StartsWith(';') then continue;
      Tokens := S.Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
      if Length(Tokens) = 2 then
      begin
        DataGrid.Cells[0,J] := DataGrid.Cells[0,J-1];
        DataGrid.Cells[1,J] := Tokens[0];
        DataGrid.Cells[2,J] := Tokens[1];
      end
      else if Length(Tokens) = 3 then
      begin
        DataGrid.Cells[0,J] := Tokens[0];
        DataGrid.Cells[1,J] := Tokens[1];
        DataGrid.Cells[2,J] := Tokens[2];
      end
      else continue;
      Inc(J);
    end;
    DataGrid.Modified := True;
  finally
    DataList.Free;
  end;
end;

procedure TCalibRptFrame.GetLocationsList(LocationsList: TStringList);
var
  S: String;
  S1: String;
  I: Integer;
begin
  // Set the text of the previous location to blank
  S1 := '';

  with DataGrid do
  begin
    // Start with row 1 since row 0 is the header
    for I := 1 to RowCount-1 do
    begin
      // Skip over current row if it has no location or its
      // location is the same as the row above it
      S := Trim(Cells[0, I]);
      if Length(S) = 0 then continue;
      if SameText(S, S1) then continue;
      if LocationsList.IndexOf(S) >= 0 then continue;

      // Check that location exists
      if project.GetItemIndex(ObjectType, S) <= 0 then continue;

      // Add the location to the list
      LocationsList.Add(S);
      S1 := S;
    end;
  end;
end;

procedure TCalibRptFrame.FillLocationsListBox(LocationsList: TStringList);
var
  OldLocation: String;
  NewIndex: Integer;
begin
  OldLocation := '';
  with LocationsListBox do
    if Items.Count > 0 then OldLocation := Items[ItemIndex];
  LocationsListBox.Clear;
  LocationsListBox.Items.Assign(LocationsList);
  NewIndex := LocationsListBox.Items.IndexOf(OldLocation);
  if NewIndex < 0 then NewIndex := 0;
  LocationsListBox.ItemIndex := NewIndex;
end;

procedure TCalibRptFrame.LoadMeasuredData(aSeries: TLineSeries;
  aLocation: String);
var
  X, Y: Single;
  I: Integer;
begin
  // Loop through each row of the DataGrid
  with DataGrid do
  begin
    for I := 1 to RowCount - 1 do
    begin
      // The row's location value matches the target location
      if SameText(Cells[0,I], aLocation) then
      begin
        X := utils.Str2Seconds(Cells[1,I]) / 3600.0;
        if (X >= 0) and utils.Str2Float(Cells[2,I], Y) then
          aSeries.AddXY(X, Y);
      end;
    end;
  end;
end;

procedure TCalibRptFrame.RefreshTimeSeriesPlot;
var
  T: Integer;
  X, Y: Double;
  Location: String;
begin
  // Get location's index
  with LocationsListBox do Location := Items[ItemIndex];
  LocationIndex := project.GetItemIndex(ObjectType, Location);
  TimeSeriesChart.Title.Text[0] := VariableCombo.Text + ' for ' +
    LocationsHeader.Caption + ' '  + Location;

  // Load computed results into ComputedSeries line series
  ComputedSeries.Clear;
  ComputedSeries.Active := false;
  for T := 0 to Results.Nperiods - 1 do
  begin
    X := (Results.Rstart + T * Results.Rstep) / 3600.;
    with ComputedSeries do
    begin
      if ObjectType = cNodes then
        Y := MapThemes.GetNodeValue(LocationIndex, VariableIndex, T)
      else
        Y := MapThemes.GetLinkValue(LocationIndex, VariableIndex, T);
      if Y <> MISSING then AddXY(X, Y);
    end;
  end;
  ComputedSeries.Active := true;

  // Load measured values into MeasuredSeries point series
  MeasuredSeries.Clear;
  MeasuredSeries.Active := false;
  LoadMeasuredData(MeasuredSeries, Location);
  MeasuredSeries.Active := True;
end;

procedure TCalibRptFrame.RefreshErrorStats;
var
  I, N: Integer;
  SeriesIndex: Integer;
  Location: String;
  X, Y, T: Single;
  Sums: array of TSums;
  Rcoeff: Double;
begin
  // Initialize
  InitCorrelationPlot;
  N := LocationsListBox.Items.Count + 1;
  SetLength(Sums, N);
  for I := 0 to N-1 do InitSums(Sums[I]);

  // Retrieve values for each measured and corresponding simulated result
  with DataGrid do
  begin
    for I := 1 to RowCount - 1 do
    begin
      Location := Cells[0,I];
      SeriesIndex := LocationsListBox.Items.IndexOf(Location) + 1;
      if SeriesIndex < 1 then continue;
      LocationIndex := project.GetItemIndex(ObjectType, Location);
      if LocationIndex < 1 then continue;
      if not utils.Str2Float(Cells[1,I], T) then continue;
      if not utils.Str2Float(Cells[2,I], X) then continue;
      Y := GetSimulatedValue(T);
      if Y = MISSING then continue;

      // Update correlation plot
      with CorrelationPlot.Series[SeriesIndex] as TLineSeries do
        AddXY(X, Y);

      // Update correlation statistics
      UpdateSums(X, Y, Sums[0]);
      UpdateSums(X, Y, Sums[SeriesIndex]);
    end;
  end;

  // Finalize correlation plot
  Rcoeff := FindCorrelCoeff(Sums[0]);
  FinalizeCorrelationPlot(Rcoeff);

  // Display error statistics
  DisplayErrorStats(Sums);
  SetLength(Sums, 0);
end;

procedure TCalibRptFrame.InitCorrelationPlot;
var
  I: Integer;
  aSeries: TLineSeries;
  ColorIndex: Integer;
begin
  // Free any existing data series on the CorrelationPlot
  with CorrelationPlot do
    while SeriesCount > 0 do Series[0].Free;
  CorrelationPlot.Title.Text[0] := 'Correlation Plot for ' + VariableCombo.Text;

  // Create a series for the perfect correlation line
  aSeries := TLineSeries.Create(CorrelationPlot.Owner);
  with aSeries as TLineSeries do
  begin
    ShowInLegend := False;
    CorrelationPlot.AddSeries(aSeries);
  end;

  // Create a line series for each measurement location
  ColorIndex := -1;
  for I := 0 to LocationsListBox.Items.Count - 1 do
  begin
    Inc(ColorIndex);
    if ColorIndex > High(MarkerColors) then
      ColorIndex := 0;
    aSeries := TLineSeries.Create(CorrelationPlot.Owner);
    with aSeries as TLineSeries do
    begin
      Title := LocationsListBox.Items[I];
      ShowPoints := True;
      ShowLines := False;
      LineType := ltNone;
      Pointer.Style := psDiagCross;
      Pointer.Pen.Width := 2;
      Pointer.Pen.Color := MarkerColors[ColorIndex];
      SeriesColor := MarkerColors[ColorIndex];
      CorrelationPlot.AddSeries(aSeries);
    end;
  end;
end;

procedure TCalibRptFrame.FinalizeCorrelationPlot(Rcoeff: Double);
var
  Z1, Z2: Double;
  Extent: TDoubleRect;
begin
  // Add perfect correlation line to plot as Series 0
  Extent := CorrelationPlot.GetFullExtent;
  Z1 := Extent.a.X;
  Z1 := Min(Z1, Extent.a.Y);
  Z2 := Extent.b.X;
  Z2 := Max(Z2, Extent.b.Y);
  with CorrelationPlot.Series[0] as TLineSeries do
  begin
    AddXY(Z1, Z1);
    AddXY(Z2, Z2);
  end;

  // Add correlation coefficient to plot
  CorrelationPlot.Foot.Text[0] := Format('Correlation Coeff. = %.2f',
    [Rcoeff]);
end;

function TCalibRptFrame.GetSimulatedValue(T: Single): Single;
var
  P1, P2: Integer;
  Y1, Y2, T1: Single;
begin
  // Default result is MISSING
  Result := MISSING;

  // Convert time from hours to seconds
  T := T * 3600.0;

  // Find reporting periods that contains time T
  P1 := Floor((T - results.Rstart) / results.Rstep);
  if (P1 < 0) then exit;
  if P1 = results.Nperiods then P2 := P1 else P2 := P1 + 1;

  // Find simulated results for these periods
  if ObjectType = cNodes then
  begin
    Y1 := MapThemes.GetNodeValue(LocationIndex, VariableIndex, P1);
    Y2 := MapThemes.GetNodeValue(LocationIndex, VariableIndex, P2);
  end
  else begin
    Y1 := MapThemes.GetLinkValue(LocationIndex, VariableIndex, P1);
    Y2 := MapThemes.GetLinkValue(LocationIndex, VariableIndex, P2);
  end;
  if (Y1 = MISSING) or (Y2 = MISSING) then exit;

  // Interpolate to find Y at time T
  T1 := P1 * results.Rstep;
  Result := Y1 + (Y2 - Y1) * (T - T1) / results.Rstep;
end;

function TCalibRptFrame.ReadDataGrid: Boolean;
var
  LocationsList: TStringList;
begin
  // Place object type in LocationsHeader
  if ObjectType = cNodes then
    LocationsHeader.Caption := 'Node'
  else
    LocationsHeader.Caption := 'Link';

  // Get a list of measurement locations
  LocationsList := TStringList.Create;
  try
    GetLocationsList(LocationsList);

    // Issue error message if no locations
    if LocationsList.Count = 0 then
    begin
      utils.MsgDlg('There are no calibration data to process.', mtInformation, [mbOk]);
      Result := False;
    end else

    // Otherwise fill the LocationListBox on the Time Series Plot page
    begin
      FillLocationsListBox(LocationsList);
      Result := True;
    end;
  finally
    LocationsList.Free;
  end;
end;

procedure TCalibRptFrame.InitSums(var Sums: TSums);
begin
  with Sums do
  begin
    N := 0;
    SumX := 0;
    SumY := 0;
    SumX2 := 0;
    SumY2 := 0;
    SumXY := 0;
    SumE := 0;
    SumE2 := 0;
  end;
end;

procedure TCalibRptFrame.UpdateSums(X, Y: Double; var Sums: TSums);
var
  E: Double;
begin
  with Sums do
  begin
    Inc(N);
    SumX := SumX + X;
    SumY := SumY + Y;
    SumX2 := SumX2 + (X*X);
    SumY2 := SumY2 + (Y*Y);
    SumXY := SumXY + (X*Y);
    E := Abs(X - Y);
    SumE := SumE + E;
    SumE2 := SumE2 + (E*E);
  end;
end;

function TCalibRptFrame.FindCorrelCoeff(Sums: TSums): Double;
var
  T1, T2, T3, T4: Double;
begin
  with Sums do
  begin
    T1 := N * SumX2 - (SumX * SumX);
    T2 := N * SumY2 - (SumY * SumY);
    T3 := N * SumXY - (SumX * SumY);
    T4 := T1 * T2;
    if T4 <= 0 then
      Result := 0.0
    else
      Result := T3 / Sqrt(T4);
  end;
end;

procedure TCalibRptFrame.DisplayErrorStats(Sums: array of TSums);
var
  Title: String;
  Location: String;
  N, Xmean, Ymean, Rcoeff: Double;
  I: Integer;
  MeanSums: TSums;
begin
  InitSums(MeanSums);
  Title := project.GetTitle(0);
  with ErrorStatsMemo.Lines do
  begin
    Clear;
    if Length(Title) > 0 then
    begin
      Add('  ' + Title);
      Add('');
    end;
    with VariableCombo do
      Add('  Calibration Report for ' + Items[ItemIndex]);
    Add('');
    Add(TXT_HEADING1);
    Add(TXT_HEADING2);
    Add(TXT_HEADING3);
    for I := 1 to Length(Sums)-1 do
    begin
      Location := LocationsListBox.Items[I-1];
      N := Sums[I].N;
      Xmean := Sums[I].SumX / N;
      Ymean := Sums[I].SumY / N;
      Add(Format('  %-14s%3.0f%12.2f%12.2f%8.3f%8.3f',
        [Location, N, Xmean, Ymean, Sums[I].SumE/N, Sqrt(Sums[I].SumE2/N)]));
      UpdateSums(Xmean, Ymean, MeanSums);
    end;
    Add(TXT_HEADING3);
    Location := 'Network';
    N := Sums[0].N;
    Add(Format('  %-14s%3.0f%12.2f%12.2f%8.3f%8.3f',
      [Location, N, Sums[0].SumX/N, Sums[0].SumY/N, Sums[0].SumE/N,
      Sqrt(Sums[0].SumE2/N)]));
    Rcoeff := FindCorrelCoeff(MeanSums);
    Add('');
    Add('  Correlation Between Means: ' + Format('%.3f',[Rcoeff]));
  end;
end;

procedure TCalibRptFrame.SaveChartToFile(aChart: TChart);
var
  Fname: string;
begin
  Fname := GetFileName('*.png', 'PNG File|*.png|', 'png');
  if Length(Fname) > 0 then
    aChart.SaveToFile(TPortableNetworkGraphic, Fname);
end;

function TCalibRptFrame.GetFileName(Fname: string; Ftypes: string;
  DefType: string): string;
begin
  Result := '';
  with MainForm.SaveDialog1 do begin
    FileName := Fname;
    Filter := Ftypes;
    DefaultExt := DefType;
    if Execute then Result := FileName;
  end;
end;

procedure TCalibRptFrame.InitReport;
var
  I: Integer;
begin
  // Fill the VariableCombo combobox with the names of
  // variables that can be calibrated to
  with VariableCombo.Items do
  begin
    Add('Head');
    Add('Pressure');
    Add('Flow');
    Add('Velocity');
    for I := mapthemes.FirstNodeQualTheme to mapthemes.NodeThemeCount -1 do
      Add(mapthemes.NodeThemes[I].Name);
  end;
  VariableCombo.ItemIndex := 1;
  VariableComboChange(self);
  Notebook1.PageIndex := Notebook1.IndexOf(Page1);
end;

procedure TCalibRptFrame.CloseReport;
begin
  ClearReport;
end;

procedure TCalibRptFrame.ClearReport;
begin
  DataGrid.Clear;
  LocationsListBox.Items.Clear;
  ComputedSeries.Clear;
  MeasuredSeries.Clear;
  with CorrelationPlot do
    while SeriesCount > 0 do Series[0].Free;
  ErrorStatsMemo.Clear;
end;

procedure TCalibRptFrame.RefreshReport;
begin
  if LocationsListBox.Items.Count = 0 then exit;
  RefreshTimeSeriesPlot;
  RefreshErrorStats;
  DataHasChanged := False;
end;

end.

