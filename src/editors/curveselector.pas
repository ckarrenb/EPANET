{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       curveselector
 Description:  a form that selects from a project's set of Data
               Curves
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit curveselector;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  LCLtype, TAGraph, TASeries, Types;

type

  { TCurveSelectorForm }

  TCurveSelectorForm = class(TForm)
    AddBtn: TButton;
    CurveChart: TChart;
    CurveSeries: TLineSeries;
    DeleteBtn: TButton;
    HelpBtn: TButton;
    EditBtn: TButton;
    OkBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    CurvesGrid: TStringGrid;
    procedure AddBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure CurvesGridClick(Sender: TObject);
    procedure CurvesGridPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    function  EditCurveData(const Index: Integer): Integer;
    procedure ShowCurveProperties(I: Integer);
    procedure PlotCurve(CurveIndex: Integer);

  public
    HasChanged: Boolean;
    SelectedName: String;
    SelectedIndex: Integer;
    procedure Setup(ItemName: String);

  end;

var
  CurveSelectorForm: TCurveSelectorForm;

implementation

{$R *.lfm}

uses
  main, project, curveeditor, curveviewer, config, utils, epanet2;

{ TCurveSelectorForm }

procedure TCurveSelectorForm.FormCreate(Sender: TObject);
begin
  Color := config.ThemeColor;
  CurvesGrid.FixedColor := Color;
  Font.Size := config.FontSize;
end;

procedure TCurveSelectorForm.CurvesGridClick(Sender: TObject);
var
  I: Integer;
begin
  I := CurvesGrid.Row - 1;
  EditBtn.Enabled := I > 0;
  DeleteBtn.Enabled := I > 0;
  if I > 0 then
  begin
    PlotCurve(I);
  end else
  begin
    CurveSeries.Clear;
  end;
end;

procedure TCurveSelectorForm.CurvesGridPrepareCanvas(Sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if aRow > 0 then with Sender as TStringGrid do
  begin
    if gdSelected in aState then
    begin
      Canvas.Brush.Color := $00FFE8CD;
      Canvas.Font.Color := clBlack;
    end;
  end;
end;

procedure TCurveSelectorForm.HelpBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('#data_curves');
end;

procedure TCurveSelectorForm.OkBtnClick(Sender: TObject);
begin
  with CurvesGrid do
    if Row > 1 then
    begin
      SelectedName := Cells[0, Row];
      SelectedIndex := Row-1;  //Adjust for <blank> row 1
    end;
  ModalResult := mrOK;
end;

procedure TCurveSelectorForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TCurveSelectorForm.EditBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  // Convert selected grid row value to EPANET curve index
  Index := CurvesGrid.Row - 1;

  // Call Curve editor if selected row greater than first "blank" row
  if Index >= 1 then
  begin
    EditCurveData(Index);
    ShowCurveProperties(Index+1);
  end;
end;

procedure TCurveSelectorForm.AddBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  // Edit data for non-existent curve - it will
  // create the curve and return its index if not cancelled
  Index := EditCurveData(-1);

  // Add a new row to the grid displaying all curves
  if Index > 0 then with CurvesGrid do
  begin
    RowCount := RowCount + 1;
    ShowCurveProperties(Index + 1);
    Row := RowCount - 1;
  end;
end;

procedure TCurveSelectorForm.DeleteBtnClick(Sender: TObject);
var
  CurveIndex: Integer;
  Msg: String;
  ItemName: String;
begin
  // EPANET index corresponding to current selected row of CurvesGrid
  // (1-based and accounting for <blank> 1st row)
  CurveIndex := CurvesGrid.Row - 1;
  if CurveIndex < 1 then exit;
  ItemName := CurvesGrid.Cells[0,CurveIndex];

  // Verify deletion using 0-based index used by EPANET project
  if config.ConfirmDeletions then
  begin
    Msg := 'Do you wish to delete ' +
           project.GetItemTypeStr(cCurves, CurveIndex-1) +
           project.GetItemID(cCurves, CurveIndex-1);
    Msg := Msg + '?';
    if utils.MsgDlg(Msg, mtConfirmation, [mbYes, mbNo]) = mrNo then exit;
  end;

  // Delete item from project and update CurvesGrid
  project.DeleteItem(cCurves, CurveIndex);
  Setup(ItemName);
end;

procedure TCurveSelectorForm.Setup(ItemName: String);
var
  I, StartRow: Integer;
begin
  SelectedName := '';
  SelectedIndex := 0;
  StartRow := 0;
  with CurvesGrid do
  begin
    RowCount := project.GetItemCount(project.cCurves) + 2;
    Cells[0,1] := '<blank>';
    for I := 2 to RowCount - 1 do
    begin
      ShowCurveProperties(I);
      if Cells[0,I] = ItemName then StartRow := I
    end;
    Row := StartRow;
  end;
end;

function TCurveSelectorForm.EditCurveData(const Index: Integer): Integer;
var
  CurveEditorForm: TCurveEditorForm;
begin
  Result := Index;
  CurveEditorForm := TCurveEditorForm.Create(self);
  try
    CurveEditorForm.SetCurveData(Index);
    CurveEditorForm.ShowModal;
    if (CurveEditorForm.ModalResult = mrOK) and
      CurveEditorForm.HasChanged then
    begin
      Result := CurveEditorForm.CurveIndex;
      project.HasChanged := True;
      project.UpdateResultsStatus;
      PlotCurve(CurveEditorForm.CurveIndex);
    end;
  finally
    CurveEditorForm.Free;
  end;
end;

procedure TCurveSelectorForm.ShowCurveProperties(I: Integer);
begin
  with CurvesGrid do
  begin
    Cells[0,I] := project.GetItemID(project.cCurves, I-2);
    Cells[1,I] := project.GetComment(EN_CURVE, I-1);
  end;
end;

procedure TCurveSelectorForm.PlotCurve(CurveIndex: Integer);
var
  CurveType: Integer;
  I, N: Integer;
  A, B: Single;
  X, Y: array of Double;
begin
  epanet2.ENgetcurvetype(CurveIndex, CurveType);
  epanet2.ENgetcurvelen(CurveIndex, N);
  Setlength(X, N+1);
  SetLength(Y, N+1);
  for I := 1 to N do
  begin
    epanet2.ENgetcurvevalue(CurveIndex, I, A, B);
    X[I-1] := A;
    Y[I-1] := B;
  end;
  CurveChart.BottomAxis.Title.Caption := curveeditor.XLabel[CurveType];
  CurveChart.LeftAxis.Title.Caption := curveeditor.YLabel[CurveType];
  curveviewer.PlotCurve(CurveChart, CurveType, X, Y, N);
  SetLength(X, 0);
  Setlength(Y, 0);
end;

end.

