{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       patterneditor
 Description:  a dialog form that edits a Time Pattern object
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit patterneditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  LCLtype, TAGraph, TASeries, TACustomSeries;

{$I ..\timetype.txt}

const
  MAXPERIODS = 24;
  TXT_PATTERN_FILTER = 'Pattern files (*.PAT)|*.PAT|All files|*.*';
  TXT_PATTERN_HEADER = 'EPANET Pattern Data';

type

  { TPatternEditorForm }

  TPatternEditorForm = class(TForm)
    LoadBtn: TButton;
    HelpBtn: TButton;
    SaveBtn: TButton;
    OkBtn: TButton;
    CancelBtn: TButton;
    PatternChart: TChart;
    DescripEdit: TEdit;
    IdEdit: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    MultiplierSeries: TAreaSeries;
    DataGrid: TStringGrid;
    procedure HelpBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure DataGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DataGridValidateEntry(Sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private
    OldID: String;
    procedure GetMultipliers;
    procedure SetMultipliers;
    procedure PlotPattern;
    procedure LoadPatternData(Filename: String);
    procedure SavePatternData(Filename: String);

  public
    PatternIndex: Integer;
    HasChanged: Boolean;
    procedure SetPatternData(const Index: Integer);
    function  GetPatternData: Boolean;
  end;

var
  PatternEditorForm: TPatternEditorForm;

implementation

{$R *.lfm}

{ TPatternEditorForm }

uses
  main, project, projectbuilder, utils, config, epanet2;

procedure TPatternEditorForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Color := config.ThemeColor;
  DataGrid.FixedColor := Color;
  Font.Size := config.FontSize;
  with DataGrid do
  begin
    ColCount := MAXPERIODS + 1;
    RowCount := 2;
    Cells[0,0] := 'Period';
    Cells[0,1] := 'Multiplier';
    for I := 1 to ColCount-1 do Cells[I,0] := IntToStr(I);
    Cells[1,1] := '1.0';
  end;
end;

procedure TPatternEditorForm.DataGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if DataGrid.Col < DataGrid.ColCount - 1 then exit;
  if Key = VK_RIGHT then with DataGrid do
  begin
    ColCount := ColCount + 1;
    Cells[ColCount-1,0] := IntToStr(ColCount-1);
    Refresh;
    Col := ColCount - 1;
  end;
end;

procedure TPatternEditorForm.SetPatternData(const Index: Integer);
begin
  PatternIndex := Index;
  if Index < 0 then
  begin
    OldId := projectbuilder.FindUnusedID(cPatterns, 0);
    IdEdit.Text := OldId;
    HasChanged := True;
  end else
  begin
    OldId:= project.GetID(cPatterns, Index);
    IdEdit.Text := OldId;
    DescripEdit.Text:= project.GetComment(EN_TIMEPAT, Index);
    SetMultipliers;
    HasChanged := False;
  end;
end;

function TPatternEditorForm.GetPatternData: Boolean;
var
  ID, Msg: String;
begin
  // Check for valid pattern ID
  Result := False;
  ID := Trim(IdEdit.Text);
  if ID <> OldID then
  begin
    Msg := project.GetIdError(cPatterns, ID);
    if Length(Msg) > 0 then
    begin
      utils.MsgDlg(Msg, mtError, [mbOK]);
      Exit;
    end;
  end;

  // Create new pattern if not editing an existing pattern
  if PatternIndex < 0 then
  begin
    if epanet2.ENaddpattern(PChar(ID)) > 0 then
    begin
      utils.MsgDlg('Unable to add a new pattern.', mtError, [mbOK]);
      exit;
    end;
    PatternIndex := project.GetItemCount(project.cPatterns);
    HasChanged := True;
  end;

  // Assign edited properties to pattern
  project.SetItemID(cPatterns, PatternIndex, ID);
  epanet2.ENsetcomment(EN_TIMEPAT, PatternIndex, PAnsiChar(DescripEdit.Text));
  GetMultipliers;
  Result := true;
end;

procedure TPatternEditorForm.OkBtnClick(Sender: TObject);
begin
  if GetPatternData then ModalResult := mrOK;
end;

procedure TPatternEditorForm.LoadBtnClick(Sender: TObject);
begin
  with MainForm.OpenDialog1 do
  begin
    Filter := TXT_PATTERN_FILTER;
    Filename := '*.pat';
    if Execute then LoadPatternData(Filename);
  end;
end;

procedure TPatternEditorForm.HelpBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('#time_patterns');
end;

procedure TPatternEditorForm.SaveBtnClick(Sender: TObject);
begin
  with MainForm.SaveDialog1 do
  begin
    Filter := TXT_PATTERN_FILTER;
    Filename := '*.pat';
    if Execute then SavePatternData(Filename);
  end;
end;

procedure TPatternEditorForm.DataGridValidateEntry(Sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
var
  S: String;
  Y: Single = 0;
begin
  S := Trim(NewValue);
  if Length(S) > 0 then
  begin
    if utils.Str2Float(S, Y) then PlotPattern else
    begin
       utils.MsgDlg(S + ' is not a valid number.', mtError, [mbOK]);
      NewValue := OldValue;
      exit;
    end;
  end
  else PlotPattern;
  if NewValue <> OldValue then HasChanged := True;
end;

procedure TPatternEditorForm.SetMultipliers;
var
  I : Integer;
  Imax : Integer = 0;
  V : Single;
begin
  if PatternIndex > 0 then
  begin
    epanet2.ENgetpatternlen(PatternIndex, Imax);
    with DataGrid do
    begin
      if Imax > MAXPERIODS then ColCount := Imax + 1;
      for I := 1 to ColCount-1 do Cells[I,0] := IntToStr(I);
      for I := 1 to Imax do
      begin
        V := 0;
        epanet2.ENgetpatternvalue(PatternIndex, I, V);
        Cells[I,1] := Float2Str(V, 4);
      end;
    end;
  end;
  PlotPattern;
  HasChanged := False;
end;

procedure TPatternEditorForm.GetMultipliers;
var
  Multipliers: array of Single;
  I, N: Integer;
begin
  N := MultiplierSeries.Count - 1;
  if N <= 0 then
  begin
    N := 1;
    SetLength(Multipliers, N);
    Multipliers[0] := 1.0;
  end
  else
  begin
    SetLength(Multipliers, N);
    for I := 0 to N -1 do
      Multipliers[I] := Single(MultiplierSeries.GetYValue(I));
  end;
  ENsetpattern(PatternIndex, Multipliers[0], N);
end;

procedure TPatternEditorForm.PlotPattern;
var
  I: Integer;
  X: Single = 0;
  Y: Single = 0;
  T: TimeType = 3600;
  DT: Single;
begin
  // Get time interval in hours
  epanet2.ENgettimeparam(EN_PATTERNSTEP, T);
  DT := T / 3600.;

  // Set bottom axis label
  PatternChart.BottomAxis.Title.Caption := 'Time (Time Period = ' +
    Format('%.2f', [DT]) + ' hrs)';

  // Add time, multiplier pairs to chart data series
  with MultiplierSeries do
  begin
    Clear;
    Active := False;
    BeginUpdate;
    X := 0;
    with DataGrid do for I := 1 to ColCount-1 do
    begin
      if Length(Trim(Cells[I,1])) > 0 then
      begin
        Y := StrToFloatDef(Cells[I,1], 0);
        AddXY(X, Y, '');
        X := X + DT;
      end;
    end;

    // Repeat final point
    if Count > 0 then
    begin
      AddXY(X, Y, '');
      Active := True;
    end;
    EndUpdate;
  end;
end;

procedure TPatternEditorForm.LoadPatternData(Filename: String);
var
  I, J, K: Integer;
  Y: Single;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(Filename);
    K := Lines.Count - 2;  //1st two lines are a header & description
    if K > 0 then
    begin
      DescripEdit.Text := Lines[1];
      with DataGrid do
      begin
        BeginUpdate;
        ColCount := K + 1;  //Account for header column
        J := 2;             //Index of 1st multiplier in file
        for I := 1 to K do
        begin
          Cells[I, 0] := IntToStr(I);
          if not utils.Str2Float(Lines[J], Y) then
            Cells[I, 1] := '0.0000'
          else
            Cells[I, 1] := Lines[J];
          Inc(J);
        end;
        EndUpdate;
      end;
    end;
    PlotPattern;
    HasChanged := True;
  finally
    Lines.Free;
  end;
end;

procedure TPatternEditorForm.SavePatternData(Filename: String);
var
  I, J, K: Integer;
  Y: Single;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add(TXT_PATTERN_HEADER);
    Lines.Add(DescripEdit.Text);
    with DataGrid do
    begin
      for I := 1 to ColCount - 1 do
        Lines.Add(Cells[I,1]);
    end;
    Lines.SaveToFile(Filename);
  finally
    Lines.Free;
  end;
end;

end.

