{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       chartoptions
 Description:  a form that selects display options for a TChart
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit chartoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ColorBox, Spin, ExtCtrls, Buttons, Math, TAGraph, TASeries, TALegend, TATypes,
  SpinEx, FPCanvas;

const
  MaxSeries = 6;

type
  TSeriesOptions = record
    LineColor: TColor;
    LineStyle: Integer;
    LineWidth: Integer;
    LineVisible: Boolean;
    PointsColor: TColor;
    PointsSize: Integer;
    PointsStyle: Integer;
    PointsVisible: Boolean;
    Title: string;
    ShowInLegend: Boolean;
  end;

type
  TAxisOptions = record
    Caption: string;
    Grid: Boolean;
  end;

type

  { TChartOptionsForm }

  TChartOptionsForm = class(TForm)
    AxisBtn0: TRadioButton;
    AxisBtn1: TRadioButton;
    AxisBtn2: TRadioButton;
    AxisMarksFontLabel: TLabel;
    AxisMarksLabel: TLabel;
    AxisTitleEdit: TEdit;
    AxisTitleFontLabel: TLabel;
    Bevel1: TBevel;
    ChartAreaColorBox: TColorBox;
    ChartTitleEdit: TEdit;
    ColorDialog1: TColorDialog;
    DataSeriesComboBox: TComboBox;
    FontDialog1: TFontDialog;
    FramePlotAreaBox: TCheckBox;
    GeneralTab: TTabSheet;
    GridVisibleCheckBox: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LgndColorBox: TColorBox;
    LgndColumnsSpinEdit: TSpinEdit;
    LgndFontLabel: TLabel;
    LgndFramedCheckBox: TCheckBox;
    LgndOnPanelCheckBox: TCheckBox;
    LgndPositionComboBox: TComboBox;
    LgndVisibleCheckBox: TCheckBox;
    LgndWidthSpinEdit: TSpinEdit;
    LineColorBox: TColorBox;
    LineStyleComboBox: TComboBox;
    LineVisibleCheckBox: TCheckBox;
    LineWidthSpinEdit: TSpinEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    CheckBox1: TCheckBox;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    PlotAreaColorBox: TColorBox;
    PointsColorBox: TColorBox;
    PointsSizeSpinEdit: TSpinEdit;
    PointsStyleComboBox: TComboBox;
    PointsVisibleCheckBox: TCheckBox;
    SeriesLinesTab: TTabSheet;
    SeriesPointsTab: TTabSheet;
    SeriesTitleEdit: TEdit;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TitleFontLabel: TLabel;
    procedure AxisBtnClick(Sender: TObject);
    procedure AxisMarksFontLabelClick(Sender: TObject);
    procedure AxisTitleFontLabelClick(Sender: TObject);
    procedure ChartAreaColorBoxGetColors(Sender: TCustomColorBox;
      Items: TStrings);
    procedure DataSeriesComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LgndFontLabelClick(Sender: TObject);
    procedure SeriesColorBoxGetColors(Sender: TCustomColorBox; Items: TStrings);
    procedure TitleFontLabelClick(Sender: TObject);
  private
    AxisOptions: array [0..2] of TAxisOptions;
    SeriesOptions: array of TSeriesOptions;
    SelectedAxis: Integer;
    SelectedSeries: Integer;
    SeriesCount: Integer;
    procedure SetGeneralOptions(aChart: TChart);
    procedure SetLegendOptions(aChart: TChart);
    procedure SetAxesOptions(aChart: TChart);
    procedure SetSeriesOptions(aChart: TChart);
    procedure GetGeneralOptions(aChart: TChart);
    procedure GetLegendOptions(aChart: TChart);
    procedure GetAxesOptions(aChart: TChart);
    procedure GetSeriesOptions(aChart: TChart);
    procedure SetSelectedAxisOptions;
    procedure GetSelectedAxisOptions;
    procedure SetSelectedSeriesOptions;
    procedure GetSelectedSeriesOptions;

  public
    procedure SetOptions(aChart: TChart);
    procedure GetOptions(aChart: TChart);

  end;

var
  ChartOptionsForm: TChartOptionsForm;

implementation

{$R *.lfm}

uses
  config;

const
  LgndPositions: array[0..7] of string =
  ('Top Left', 'Center Left', 'Bottom Left', 'Top Center', 'Bottom Center',
   'Top Right', 'Center Right', 'Bottom Right');

  LineStyles: array[0..4] of string =
 ('Solid', 'Dash', 'Dot', 'Dash Dot', 'Dash Dot Dot');

  PointStyles: array[0..11] of string =
  ('None', 'Rectangle', 'Circle', 'Cross', 'Diagonal Cross', 'Star',
   'Low Bracket', 'High Bracket', 'Left Bracket', 'Right Bracket',
   'Diamond', 'Triangle');

procedure TChartOptionsForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Color := Config.ThemeColor;
  PageControl1.ActivePageIndex := 0;
  with LgndPositionComboBox do
    for I := 0 to High(LgndPositions) do
      Items.Add(LgndPositions[I]);
  LgndPositionComboBox.ItemIndex := 3;
  with LineStyleComboBox do
    for I := 0 to High(LineStyles) do
      Items.Add(LineStyles[I]);
  LineStyleComboBox.ItemIndex := 5;
  with PointsStyleComboBox do
    for I := 0 to High(PointStyles) do
      Items.Add(PointStyles[I]);
  PointsStyleComboBox.ItemIndex := 9;
end;

procedure TChartOptionsForm.SetOptions(aChart: TChart);
begin
  SetGeneralOptions(aChart);
  if aChart.AxisList[0].Visible then
    SelectedAxis := 0
  else
    SelectedAxis := 1;
  SetAxesOptions(aChart);
  SetLegendOptions(aChart);
  SetSeriesOptions(aChart);
end;

procedure TChartOptionsForm.TitleFontLabelClick(Sender: TObject);
begin
  with FontDialog1 do
  begin
    Font := ChartTitleEdit.Font;
    if Execute then
      ChartTitleEdit.Font := Font;
  end;
end;

procedure TChartOptionsForm.AxisBtnClick(Sender: TObject);
begin
  GetSelectedAxisOptions;
  with Sender As TRadioButton do
    SelectedAxis := Tag;
  SetSelectedAxisOptions;
end;

procedure TChartOptionsForm.AxisMarksFontLabelClick(Sender: TObject);
begin
  with FontDialog1 do
  begin
    Font := AxisMarksLabel.Font;
    if Execute then AxisMarksLabel.Font := Font;
  end;
end;

procedure TChartOptionsForm.AxisTitleFontLabelClick(Sender: TObject);
begin
  with FontDialog1 do
  begin
    Font := AxisTitleEdit.Font;
    if Execute then AxisTitleEdit.Font := Font;
  end;
end;

procedure TChartOptionsForm.ChartAreaColorBoxGetColors(Sender: TCustomColorBox;
  Items: TStrings);
begin
  Items.AddObject('Panel',TObject(PtrInt(config.ThemeColor)));
end;

procedure TChartOptionsForm.DataSeriesComboBoxChange(Sender: TObject);
begin
  GetSelectedSeriesOptions;
  SelectedSeries := DataSeriesComboBox.ItemIndex + 1;
  SetSelectedSeriesOptions;
end;

procedure TChartOptionsForm.LgndFontLabelClick(Sender: TObject);
begin
  with FontDialog1 do
  begin
    Font := SeriesTitleEdit.Font;
    if Execute then
      SeriesTitleEdit.Font := Font;
  end;
end;

procedure TChartOptionsForm.SeriesColorBoxGetColors(Sender: TCustomColorBox;
  Items: TStrings);
begin
  Items.AddObject('Pastel Blue',TObject($E5B533));
  Items.AddObject('Pastel Purple',TObject($CC66AA));
  Items.AddObject('Pastel Green',TObject($CC99));
  Items.AddObject('Pastel Orange',TObject($33BBFF));
  Items.AddObject('Pastel Red',TObject($4444FF));
end;

procedure TChartOptionsForm.SetGeneralOptions(aChart: TChart);
begin
  ChartAreaColorBox.Selected := aChart.Color;
  PlotAreaColorBox.Selected := aChart.BackColor;
  FramePlotAreaBox.Checked := aChart.Frame.Visible;
  ChartTitleEdit.Font.Assign(aChart.Title.Font);
  with aChart.Title do
    if Text.Count > 0 then
      ChartTitleEdit.Text := Text[0];
end;

procedure TChartOptionsForm.SetAxesOptions(aChart: TChart);
var
  I: Integer;
begin
  for I := 0 to 2 do
  begin
    with FindComponent('AxisBtn' + IntToStr(I)) as TRadioButton do
      Enabled := aChart.AxisList[I].Visible;
    AxisOptions[I].Caption:= aChart.AxisList[I].Title.Caption;
    AxisOptions[I].Grid := aChart.AxisList[I].Grid.Visible;
  end;
  AxisTitleEdit.Font.Assign(aChart.AxisList[1].Title.LabelFont);
  AxisMarksLabel.Font.Assign(aChart.AxisList[1].Marks.LabelFont);
  SetSelectedAxisOptions;
end;

procedure TChartOptionsForm.SetSelectedAxisOptions;
begin
  with AxisOptions[SelectedAxis] do
  begin
    GridVisibleCheckBox.Checked := Grid;
    AxisTitleEdit.Text := Caption;
  end;
end;

procedure TChartOptionsForm.SetLegendOptions(aChart: TChart);
begin
  LgndPositionComboBox.ItemIndex := Ord(aChart.Legend.Alignment);
  LgndVisibleCheckBox.Checked := aChart.Legend.Visible;
  LgndFramedCheckBox.Checked := aChart.Legend.Frame.Visible;
  LgndOnPanelCheckBox.Checked := aChart.Legend.UseSideBar;
  LgndColorBox.Selected := aChart.Legend.BackgroundBrush.Color;
  LgndColumnsSpinEdit.Value := aChart.Legend.ColumnCount;
  LgndWidthSpinEdit.Value := aChart.Legend.SymbolWidth;
  SeriesTitleEdit.Font.Assign(aChart.Legend.Font);
end;

procedure TChartOptionsForm.SetSeriesOptions(aChart: TChart);
var
  I, N: Integer;
  aSeries: TLineSeries;
begin
  N := Math.Min(aChart.SeriesCount, MaxSeries);
  SeriesCount := N;
  SetLength(SeriesOptions, SeriesCount);
  for I := 0 to SeriesCount-1 do
  begin
    aSeries := TLineSeries(aChart.Series[I]);

    with SeriesOptions[I] do
    begin
      Title:= aSeries.Title;
      LineColor := aSeries.LinePen.Color;
      LineStyle := Ord(aSeries.LinePen.Style);
      if LineStyle > High(LineStyles) then LineStyle := 0;
      LineWidth := aSeries.LinePen.Width;
      LineVisible := aSeries.ShowLines;
      PointsColor := aSeries.Pointer.Brush.Color;
      PointsSize := aSeries.Pointer.HorizSize;
      PointsStyle := Ord(aSeries.Pointer.Style);
      if PointsStyle > High(PointStyles) then PointsStyle := 0;
      PointsVisible := aSeries.ShowPoints;
      ShowInLegend := aSeries.ShowInLegend;
    end;
    DataSeriesComboBox.Items.Add('Series ' + IntToStr(I+1));
  end;
  DataSeriesComboBox.ItemIndex := 0;
  SelectedSeries := 0;
  SetSelectedSeriesOptions;
end;

procedure TChartOptionsForm.SetSelectedSeriesOptions;
begin
  with SeriesOptions[SelectedSeries] do
  begin
    SeriesTitleEdit.Text := Title;
    LineColorBox.Selected := LineColor;
    LineStyleComboBox.ItemIndex := LineStyle;
    LineWidthSpinEdit.Value := LineWidth;
    LineVisibleCheckBox.Checked := LineVisible;
    PointsColorBox.Selected := PointsColor;
    PointsStyleComboBox.ItemIndex := PointsStyle;
    PointsSizeSpinEdit.Value := PointsSize;
    PointsVisibleCheckBox.Checked := PointsVisible;
  end;
end;

procedure TChartOptionsForm.GetOptions(aChart: TChart);
begin
  GetGeneralOptions(aChart);
  GetAxesOptions(aChart);
  GetLegendOptions(aChart);
  GetSeriesOptions(aChart);
end;

procedure TChartOptionsForm.GetGeneralOptions(aChart: TChart);
var
  Title: String;
begin
  aChart.Color := ChartAreaColorBox.Selected;
  aChart.BackColor := PlotAreaColorBox.Selected;
  aChart.Frame.Visible := FramePlotAreaBox.Checked;
  aChart.Title.Brush.Color := aChart.Color;
  aChart.Title.Font.Assign(ChartTitleEdit.Font);
  aChart.Title.Text.Clear;
  Title := ChartTitleEdit.Text;
  if Length(Title) > 0 then
  begin
    aChart.Title.Text.Add(Title);
    aChart.Title.Visible := True;
  end
  else aChart.Title.Visible := False;
end;

procedure TChartOptionsForm.GetAxesOptions(aChart: TChart);
var
  I: Integer;
  Orientation: Integer = 0;
begin
  GetSelectedAxisOptions;
  for I := 0 to 2 do
  begin
    if not aChart.AxisList[I].Visible then continue;
    aChart.AxisList[I].Title.Caption := AxisOptions[I].Caption;
    aChart.AxisList[I].Title.LabelFont.Assign(AxisTitleEdit.Font);
    aChart.AxisList[I].Marks.LabelFont.Assign(AxisMarksLabel.Font);
    case I of
    0: Orientation := 900;
    1: Orientation := 0;
    2: Orientation := -900;
    end;
    aChart.AxisList[I].Title.LabelFont.Orientation := Orientation;
    aChart.AxisList[I].Grid.Visible := AxisOptions[I].Grid;
  end;
end;

procedure TChartOptionsForm.GetSelectedAxisOptions;
begin
  with AxisOptions[SelectedAxis] do
  begin
    Grid := GridVisibleCheckBox.Checked;
    Caption := AxisTitleEdit.Text;
  end;
end;

procedure TChartOptionsForm.GetLegendOptions(aChart: TChart);
begin
  aChart.Legend.Alignment := TLegendAlignment(LgndPositionComboBox.ItemIndex);
  aChart.Legend.Visible := LgndVisibleCheckBox.Checked;
  aChart.Legend.UseSideBar := LgndOnPanelCheckBox.Checked;
  aChart.Legend.BackgroundBrush.Color := LgndColorBox.Selected;
  aChart.Legend.ColumnCount := LgndColumnsSpinEdit.Value;
  aChart.Legend.SymbolWidth := LgndWidthSpinEdit.Value;
  aChart.Legend.Frame.Visible := LgndFramedCheckBox.Checked;
  aChart.Legend.Font.Assign(SeriesTitleEdit.Font);
  with aChart.Legend do
  begin
    if Frame.Visible then
      BackgroundBrush.Style := bsSolid
    else
      BackgroundBrush.Style := bsClear;
  end;
end;

procedure TChartOptionsForm.GetSeriesOptions(aChart: TChart);
var
  I: Integer;
  aSeries: TLineSeries;
begin
  GetSelectedSeriesOptions;
  for I := 0 to SeriesCount-1 do
  begin
    aSeries := TLineSeries(aChart.Series[I]);
    with SeriesOptions[I] do
    begin
      aSeries.Title := Title;;
      aSeries.LinePen.Color := LineColor;
      aSeries.LinePen.Style := TFPPenStyle(LineStyle);
      aSeries.LinePen.Width := LineWidth;
      aSeries.ShowLines := LineVisible;
      aSeries.Pointer.Brush.Color := PointsColor;
      aSeries.Pointer.HorizSize := PointsSize;
      aSeries.Pointer.VertSize := PointsSize;
      aSeries.Pointer.Style := TSeriesPointerStyle(PointsStyle);
      aSeries.ShowPoints := PointsVisible;
      aSeries.ShowInLegend := ShowInLegend;
    end;
  end;
end;

procedure TChartOptionsForm.GetSelectedSeriesOptions;
begin
  with SeriesOptions[SelectedSeries] do
  begin
    Title := SeriesTitleEdit.Text;
    LineColor := LineColorBox.Selected;
    LineStyle := LineStyleComboBox.ItemIndex;
    LineWidth := LineWidthSpinEdit.Value;
    LineVisible := LineVisibleCheckBox.Checked;
    PointsColor := PointsColorBox.Selected;
    PointsStyle := PointsStyleComboBox.ItemIndex;
    PointsSize := PointsSizeSpinEdit.Value;
    PointsVisible := PointsVisibleCheckBox.Checked;
  end;
end;

end.

