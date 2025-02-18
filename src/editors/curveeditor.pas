{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       curveeditor
 Description:  a dialog form that edits a Data Curve object
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit curveeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls, TAGraph, TASeries, Math;

const
  XLabel: array[0..5] of string =
    ('Depth', 'Flow', 'Flow', 'Flow', 'X', '% Open');

  YLabel: array[0..5] of string =
    ('Volume', 'Head', 'Efficiency', 'Head Loss', 'Y', '% Full Flow');

type

  { TCurveEditorForm }

  TCurveEditorForm = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    PreviewChart: TChart;
    DataSeries1: TLineSeries;
    DescripEdit: TEdit;
    IdEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    DataGrid: TStringGrid;
    TypeCombo: TComboBox;
    procedure IdEditChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DataGridValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
    procedure TypeComboChange(Sender: TObject);
  private
    CurveType: Integer;
    OldId: String;
    procedure PlotCurve;

  public
    CurveIndex: Integer;
    HasChanged: Boolean;
    procedure SetCurveData(const Index: Integer);
    function  GetCurveData: Boolean;

  end;

var
  CurveEditorForm: TCurveEditorForm;

implementation

{$R *.lfm}

uses
  project, projectbuilder, utils, config, curveviewer, epanet2;

const
  TINY: Double = 1.e-6;

procedure TCurveEditorForm.FormCreate(Sender: TObject);
begin
  Color := config.ThemeColor;
  DataGrid.FixedColor := Color;
  Font.Size := config.FontSize;
  TypeCombo.Items.AddStrings(Project.CurveTypeStr, true);
  with DataGrid do
  begin
    Cells[0,0] := XLabel[ctGeneric];
    Cells[1,0] := YLabel[ctGeneric];
  end;
end;

procedure TCurveEditorForm.SetCurveData(const Index: Integer);
var
  I : Integer;
  N : Integer;
  X : Single;
  Y : Single;
begin
  N := 0;
  X := 0;
  Y := 0;
  CurveIndex := Index;
  if Index < 0 then
  begin
    OldId := projectbuilder.FindUnusedID(cCurves, 0);
    IdEdit.Text := OldId;
    TypeCombo.ItemIndex := ctGeneric;
    HasChanged := True;
  end else
  begin
    epanet2.ENgetcurvelen(Index, N);
    epanet2.ENgetcurvetype(Index, CurveType);
    OldId:= project.GetID(cCurves, Index);
    IdEdit.Text := OldId;
    TypeCombo.ItemIndex := CurveType;
    DescripEdit.Text:= project.GetComment(EN_CURVE, Index);
    with DataGrid do
    begin
      RowCount := N + 2;
      Cells[0,0] := XLabel[CurveType];
      Cells[1,0] := YLabel[CurveType];
      for I := 1 to N do
      begin
        epanet2.ENgetcurvevalue(Index, I, X, Y);
        Cells[0,I] := Float2Str(X, 4);
        Cells[1,I] := Float2Str(Y, 4);
      end;
    end;
    PlotCurve;
    HasChanged := False;
  end;
  Caption := 'Data Curve Editor';
end;

function TCurveEditorForm.GetCurveData: Boolean;
var
  X : array of Single;
  Y : array of Single;
  XX, YY: Single;
  N : Integer;
  I : Integer;
  ID, Msg: String;
begin
  // Check for valid curve ID
  Result := false;
  ID := Trim(IdEdit.Text);
  if ID <> OldID then
  begin
    Msg := project.GetIdError(cCurves, ID);
    if Length(Msg) > 0 then
    begin
      utils.MsgDlg(Msg, mtError, [mbOK]);
      Exit;
    end;
  end;

  // Extract X, Y values from grid's cells
  N := DataGrid.RowCount - 1;
  SetLength(X, N);
  SetLength(Y, N);
  XX := 0;
  YY := 0;
  N := 0;
  with DataGrid do for I := 1 to RowCount-1 do
  begin
    if utils.Str2Float(Cells[0,I], XX)
    and utils.Str2Float(Cells[1,I], YY) then
    begin
      X[N] := XX;
      Y[N] := YY;
      Inc(N);
    end;
  end;

  // Check for at least one data point
  if N = 0 then
  begin
      utils.MsgDlg('Curve has no data points.', mtError, [mbOK]);
      exit;
  end;

  // Check for valid X-values
  for I := 1 to N-1 do
  begin
    if X[I-1] > X[I] then
    begin
      utils.MsgDlg('Invalid curve data - X values must be in ascending order.',
        mtError, [mbOK]);
      exit;
    end;
  end;

  // Create new curve if not editing an existing curve
  if CurveIndex < 0 then
  begin
    if epanet2.ENaddcurve(PChar(ID)) > 0 then
    begin
      utils.MsgDlg('Unable to add a new curve.', mtError, [mbOK]);
      exit;
    end;
    CurveIndex := project.GetItemCount(project.cCurves);
    HasChanged := True;
  end;

  // Assign curve properties
  project.SetItemID(cCurves, CurveIndex, ID);
  epanet2.ENsetcurve(CurveIndex, X[0], Y[0], N);
  epanet2.ENsetcurvetype(CurveIndex, TypeCombo.ItemIndex);
  epanet2.ENsetcomment(EN_CURVE, CurveIndex, PAnsiChar(DescripEdit.Text));
  Result := true;
end;

procedure TCurveEditorForm.OkBtnClick(Sender: TObject);
begin
  if GetCurveData then ModalResult := mrOK;
end;

procedure TCurveEditorForm.IdEditChange(Sender: TObject);
begin
  HasChanged := True;
end;

procedure TCurveEditorForm.DataGridValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
var
  S: string;
  Y: Single;
begin
  Y := 0;
  S := Trim(NewValue);
  if Length(S) > 0 then
  begin
    if utils.Str2Float(S, Y) then PlotCurve else
    begin
      utils.MsgDlg(S + ' is not a valid number.', mtError, [mbOK]);
      NewValue := OldValue;
      exit;
    end;
  end else PlotCurve;
  if NewValue <> OldValue then HasChanged := True;
end;

procedure TCurveEditorForm.TypeComboChange(Sender: TObject);
begin
  CurveType := TypeCombo.ItemIndex;
  with DataGrid do
  begin
    Cells[0,0] := XLabel[CurveType];
    Cells[1,0] := YLabel[CurveType];
  end;
  PlotCurve;
  HasChanged := True;
end;

procedure TCurveEditorForm.PlotCurve;
var
  X, Y: array of Double;
  A, B: Single;
  I, N: Integer;
begin
  PreviewChart.BottomAxis.Title.Caption := XLabel[CurveType];
  PreviewChart.LeftAxis.Title.Caption := YLabel[CurveType];
  N := DataGrid.RowCount - 1;
  SetLength(X, N);
  SetLength(Y, N);
  N := 0;
  with DataGrid do for I := 1 to RowCount - 1 do
  begin
    if Utils.Str2Float(Cells[0,I], A) and Utils.Str2Float(Cells[1,I], B) then
    begin
      X[N] := A;
      Y[N] := B;
      Inc(N);
    end;
  end;
  curveviewer.PlotCurve(PreviewChart, CurveType, X, Y, N);
  SetLength(X, 0);
  Setlength(Y, 0);
end;

end.

