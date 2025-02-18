{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       themelegend
 Description:  a dialog form that edits the legend used to
               display a theme on the pipe network map
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit themelegend;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Menus, lclIntf, mapthemes;

type

  { TLegendEditorForm }

  TLegendEditorForm = class(TForm)
    HelpBtn: TButton;
    IntervalScaleBtn: TButton;
    ReverseColorsBtn: TButton;
    OkBtn: TButton;
    CancelBtn: TButton;
    ColorPaletteBtn: TButton;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape4: TShape;
    Shape3: TShape;
    Shape5: TShape;
    procedure ColorPaletteBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure IntervalScaleBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure ReverseColorsBtnClick(Sender: TObject);
    procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    LegendType: Integer;

  public
    Modified: Boolean;
    procedure LoadData(aType: Integer; aCaption: String; Colors: array of TColor;
      Intervals: TLegendIntervals);
    procedure UnloadData(var Colors: array of TColor; var Intervals: TLegendIntervals);

  end;

var
  LegendEditorForm: TLegendEditorForm;

implementation

{$R *.lfm}

uses
  main, themepalette, themerange, project, config, utils;

{ TLegendEditorForm }

procedure TLegendEditorForm.FormCreate(Sender: TObject);
begin
  Font.Size := config.FontSize;
end;

procedure TLegendEditorForm.OkBtnClick(Sender: TObject);
var
  I: Integer;
  X1, X2: Single;
  S: String;
begin
  X1 := -1e50;
  X2 := 0;
  for I := 1 to 4 do
  begin
    with FindComponent('Edit' + IntToStr(I)) as TEdit do S := Text;
    if not Utils.Str2Float(S, X2) then
    begin
      utils.MsgDlg('"' + S + '" is not a valid number.', mtError, [mbOk], self);
      Exit;
    end
    else if X2 < X1 then
    begin
      utils.MsgDlg('Values must be in ascending order.', mtError, [mbOk], self);
      Exit;
    end
    else X1 := X2;
  end;
  ModalResult := mrOk;
end;

procedure TLegendEditorForm.ReverseColorsBtnClick(Sender: TObject);
var
  TmpColors: array [1..5] of TColor;
  I: Integer;
begin
  for I := 1 to 5 do
    with FindComponent('Shape' + IntToStr(I)) as TShape do
      TmpColors[6-I] := Brush.Color;
  for I := 1 to 5 do
    with FindComponent('Shape' + IntToStr(I)) as TShape do
      Brush.Color := TmpColors[I];
end;

procedure TLegendEditorForm.ColorPaletteBtnClick(Sender: TObject);
var
  I: Integer;
  ThemePaletteForm: TThemePaletteForm;
begin
  ThemePaletteForm := TThemePaletteForm.Create(self);
  try
    ThemePaletteForm.ShowModal;
    if ThemePaletteForm.ModalResult = mrOK then
    begin
      for I := 1 to 5 do
      begin
        with FindComponent('Shape' + IntToStr(I)) as TShape do
          Brush.Color := ThemePaletteForm.Colors[I];
      end;
    end;
  finally
    ThemePaletteForm.Free;
  end;
end;

procedure TLegendEditorForm.IntervalScaleBtnClick(Sender: TObject);
var
  I: Integer;
  Vmin: Double = 0;
  Vmax: Double = 0;
  Vinterval: Double;
  ThemeRangeForm: TThemeRangeForm;
begin
  if not mapthemes.GetMinMaxValues(LegendType, Vmin, Vmax) then
  begin
    utils.MsgDlg('No values exist for selected map theme.', mtInformation, [mbOk], self);
    exit;
  end;

  ThemeRangeForm := TThemeRangeForm.Create(self);
  try
    ThemeRangeForm.SetRange(Vmin, Vmax);
    ThemeRangeForm.ShowModal;

    if ThemeRangeForm.ModalResult = mrOK then
    begin
      ThemeRangeForm.GetRange(Vmin, Vmax);
      Vinterval := (Vmax - Vmin) / 5;
      utils.AutoScale(Vmin, Vmax, Vinterval);
      for I := 1 to 4 do
      begin
        with FindComponent('Edit' + IntToStr(I)) as TEdit do
        begin
          Text := FloatToStr(Vmin + I * Vinterval);
        end;
      end;
    end;

  finally
    ThemeRangeForm.Free;
  end;
end;

procedure TLegendEditorForm.Shape1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    ColorDialog1.Color := TShape(Sender).Brush.Color;
    try
      if ColorDialog1.Execute then
      begin
        TShape(Sender).Brush.Color := ColorDialog1.Color;
        Modified := True;
      end;
    finally
    end;
  end;
end;

procedure TLegendEditorForm.LoadData(aType: Integer; aCaption: String;
  Colors: array of TColor; Intervals: TLegendIntervals);
var
  I: Integer;
begin
  Modified := false;
  LegendType := aType;
  IntervalScaleBtn.Enabled := Project.GetItemCount(aType) > 0;
  Label1.Caption := aCaption;
  for I := 1 to 5 do
    with FindComponent('Shape' + IntToStr(I)) as TShape do
      Brush.Color := Colors[I-1];
  for I := 1 to 4 do
    with FindComponent('Edit' + IntToStr(I)) as TEdit do
      Text := Intervals.Labels[I];
end;

procedure TLegendEditorForm.UnloadData(var Colors: array of TColor;
  var Intervals: TLegendIntervals);
var
  I: Integer;
  S: String;
begin
  for I := 1 to 5 do
    with FindComponent('Shape' + IntToStr(I)) as TShape do
      Colors[I-1] := Brush.Color;
  for I := 1 to 4 do
    with FindComponent('Edit' + IntToStr(I)) as TEdit do
    begin
      S := Trim(Text);
      Intervals.Labels[I] := S;
      utils.Str2Float(S, Intervals.Values[I]);
    end;
end;

procedure TLegendEditorForm.HelpBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('#map_legend');
end;

end.

