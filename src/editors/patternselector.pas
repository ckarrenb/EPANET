{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       patternselector
 Description:  a form that selects from a project's set of Time
               Patterns
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit patternselector;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  LCLtype, TAGraph, TASeries, TACustomSeries;

type

  { TPatternSelectorForm }

  TPatternSelectorForm = class(TForm)
    AddBtn: TButton;
    PatternChart: TChart;
    PatternChartAreaSeries: TAreaSeries;
    HelpBtn: TButton;
    CancelBtn: TButton;
    DeleteBtn: TButton;
    EditBtn: TButton;
    Label1: TLabel;
    OkBtn: TButton;
    PatternsGrid: TStringGrid;
    procedure AddBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure PatternsGridClick(Sender: TObject);
    procedure PatternsGridPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    procedure ShowPatternProperties(I: Integer);
    function  EditPatternData(const Index: Integer): Integer;
    procedure PlotPattern(PatIndex: Integer);

  public
    SelectedName: String;
    SelectedIndex: Integer;
    HasChanged: Boolean;
    procedure Setup(ItemName: String);
  end;

var
  PatternSelectorForm: TPatternSelectorForm;

implementation

{$R *.lfm}

uses
  main, project, patterneditor, config, utils, epanet2;

// Definition of TimeType (seconds) depending on OS
{$I ..\timetype.txt}

{ TPatternSelectorForm }

procedure TPatternSelectorForm.FormCreate(Sender: TObject);
begin
  Color := config.ThemeColor;
  PatternsGrid.FixedColor := Color;
  Font.Size := config.FontSize;
  SelectedName := '';
  HasChanged := False;
end;

procedure TPatternSelectorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if HasChanged then
  begin
    Project.HasChanged := True;
    Project.UpdateResultsStatus;
  end;
end;

procedure TPatternSelectorForm.PatternsGridClick(Sender: TObject);
var
  I: Integer;
begin
  I := PatternsGrid.Row - 1;
  EditBtn.Enabled := I > 0;
  DeleteBtn.Enabled := I > 0;
  if I > 0 then
  begin
    PlotPattern(I);
  end
  else PatternChartAreaSeries.Clear;
end;

procedure TPatternSelectorForm.PatternsGridPrepareCanvas(Sender: TObject; aCol,
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

procedure TPatternSelectorForm.AddBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  // Edit data for non-existent pattern - it will
  // create the pattern and return its index if not cancelled
  Index := EditPatternData(-1);

  // Add a new row to the grid that lists all patterns
  if Index > 0 then
  begin
    PatternsGrid.RowCount := PatternsGrid.RowCount + 1;
    PatternsGrid.Row := PatternsGrid.RowCount - 1;
    ShowPatternProperties(Index + 1);
  end;
end;

procedure TPatternSelectorForm.EditBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  // Convert selected grid row value to EPANET pattern index
  Index := PatternsGrid.Row - 1;

  // Call Pattern editor if selected row is greater than first "blank" row
  if Index >= 1 then
  begin
    EditPatternData(Index);
    ShowPatternProperties(Index + 1);
  end;
end;

procedure TPatternSelectorForm.DeleteBtnClick(Sender: TObject);
var
  Index: Integer;
  Msg: String;
  ItemName: String;
begin
  // EPANET pattern index corresponding to current selected row
  // of PatternsGrid (1-based and accounting for <blank> 1st row)
  Index := PatternsGrid.Row - 1;
  if Index < 1 then exit;
  ItemName := PatternsGrid.Cells[0,Index];

  // Verify deletion using 0-based index used by EPANET project
  if config.ConfirmDeletions then
  begin
    Msg := 'Do you wish to delete Pattern ' + PatternsGrid.Cells[0,Index+1] + '?';
    if utils.MsgDlg(Msg, mtConfirmation, [mbYes, mbNo]) = mrNo then exit;
  end;

  // Delete item from project and update PatternsGrid
  project.DeleteItem(cPatterns, Index);
  HasChanged := True;
  Setup(ItemName);
end;

procedure TPatternSelectorForm.HelpBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('#time_patterns');
end;

procedure TPatternSelectorForm.OkBtnClick(Sender: TObject);
begin
  if PatternsGrid.Row > 1 then
  begin
    SelectedName := PatternsGrid.Cells[0, PatternsGrid.Row];
    SelectedIndex := PatternsGrid.Row-1;  //Adjust for <blank> row 1
  end;
  ModalResult := mrOK;
end;

procedure TPatternSelectorForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TPatternSelectorForm.Setup(ItemName: String);
var
  I, StartRow: Integer;
begin
  SelectedName := '';
  SelectedIndex := 0;
  StartRow := 0;
  PatternsGrid.RowCount := project.GetItemCount(project.cPatterns) + 2;
  PatternsGrid.Cells[0,1] := '<blank>';
  for I := 2 to PatternsGrid.RowCount - 1 do
  begin
    ShowPatternProperties(I);
    if PatternsGrid.Cells[0,I] = ItemName then StartRow := I;
  end;
  PatternsGrid.Row := StartRow;
end;

procedure TPatternSelectorForm.ShowPatternProperties(I: Integer);
var
  ID, Comment: String;
begin
  // I is PatternsGrid row index where first two rows are for header
  // and the blank pattern
  ID := project.GetItemID(project.cPatterns, I-2);
  Comment := project.GetComment(EN_TIMEPAT, I-1);
  PatternsGrid.Cells[0,I] := ID;
  PatternsGrid.Cells[1,I] := Comment;
end;

function TPatternSelectorForm.EditPatternData(const Index: Integer): Integer;
var
  WasEdited: Boolean;
  PatEditor: TPatternEditorForm;
begin
  WasEdited := False;
  Result := Index;
  PatEditor := TPatternEditorForm.Create(self);
  try
    PatEditor.SetPatternData(Index);
    PatEditor.ShowModal;
    if (PatEditor.ModalResult = mrOK) and PatEditor.HasChanged then
    begin
      WasEdited := True;
      Result := PatEditor.PatternIndex;
      PlotPattern(PatEditor.PatternIndex);
    end;
  finally
    PatEditor.Free;
  end;
  if WasEdited then HasChanged := True;
end;

procedure TPatternSelectorForm.PlotPattern(PatIndex: Integer);
var
  I: Integer;
  N: Integer;
  X: Single = 0;
  Y: Single = 0;
  T: TimeType = 3600;
  DT: Single;
begin
  // Get pattern length
  epanet2.ENgetpatternlen(PatIndex, N);

  // Get time interval in hours
  epanet2.ENgettimeparam(EN_PATTERNSTEP, T);
  DT := T / 3600.;

  // Add time, multiplier pairs to chart data series
  PatternChartAreaSeries.Clear;
  PatternChartAreaSeries.Active := False;
  X := 0;
  for I := 1 to N do
  begin
    epanet2.ENgetpatternvalue(PatIndex, I, Y);
    PatternChartAreaSeries.AddXY(X, Y, '');
    X := X + DT;
  end;

  // Repeat final point
  if N > 0 then PatternChartAreaSeries.AddXY(X, Y, '');
  PatternChartAreaSeries.Active := True;
end;

end.

