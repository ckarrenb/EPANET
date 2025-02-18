{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       ruleseditor
 Description:  a form that edits a project's rule-based controls
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit ruleseditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, StrUtils,
  lclIntf, LCLtype, Grids, ExtCtrls, Buttons;

type

  { TRulesEditorForm }

  TRulesEditorForm = class(TForm)
    InsertBtn: TBitBtn;
    EditBtn: TBitBtn;
    DeleteBtn: TBitBtn;
    MoveDnBtn: TBitBtn;
    MoveUpBtn: TBitBtn;
    Panel1: TPanel;
    HelpBtn: TButton;
    CancelBtn: TButton;
    Panel2: TPanel;
    RuleMemo: TMemo;
    OkBtn: TButton;
    RuleGrid: TStringGrid;
    procedure DeleteBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure InsertBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure MoveDnBtnClick(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure RuleGridCheckboxToggled(Sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure RuleGridPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure RuleGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure OkBtnClick(Sender: TObject);
  private
    NewRules: TStringList;
    OldRules: TStringList;
    OldRulesEnabled: TStringList;
    EditAction: Integer;
    procedure GetObjectInfo(ObjIndex: Integer; ObjCode: Integer;
      var ObjType: Integer; var ObjID: String);
    function  GetRuleAsString(R: Integer): String;
    procedure GetPremises(R: Integer; N: Integer; var Rule: String);
    procedure GetActions(R: Integer; nThenActions: Integer; nElseActions: Integer;
      var Rule: String);
    function  GetAnAction(LinkIndex: Integer; Status: Integer; Setting: Single): String;
    procedure DeleteRules;
    function  ReplaceRules(var BadRuleIndex: Integer): Integer;
    procedure RestoreRules;
    procedure SetButtonStates;
    procedure ShowRule(I: Integer);
    function  GetRuleID(Rule: String): String;
    procedure EditRule(Rule: String);
    procedure ReplaceRule(Rule: String);
  public
    HasChanged: Boolean;
    procedure LoadRules;
  end;

var
  RulesEditorForm: TRulesEditorForm;

implementation

{$R *.lfm}

uses
  main, project, config, ruleedit, utils, epanet2;

const
  LogWord:  array[1..5] of String =
    ('IF  ', 'AND ', 'OR  ', 'THEN', 'ELSE');
  VarWord: array[0..12] of String =
    ('DEMAND', 'HEAD', 'GRADE', 'LEVEL', 'PRESSURE', 'FLOW', 'STATUS',
     'SETTING', 'POWER', 'TIME', 'CLOCKTIME', 'FILLTIME', 'DRAINTIME');
  ObjWord: array[0..8] of String =
    ('JUNCTION', 'RESERVOIR', 'TANK', 'PIPE', 'PUMP', 'VALVE', 'NODE',
     'LINK', 'SYSTEM');
  RelWord: array[0..9] of String =
    ('=', '<>', '<=', '>=', '<', '>', 'IS', 'NOT', 'BELOW', 'ABOVE');
  StatusWord: array[1..3] of String =
    ('OPEN', 'CLOSED', 'ACTIVE');

  Editing = 1;
  Inserting = 2;

{ TRulesEditorForm }

procedure TRulesEditorForm.FormCreate(Sender: TObject);
begin
  Color := config.ThemeColor;
  Font.Size := config.FontSize;
  RuleMemo.Font.Size := config.FontSize;
  Rulememo.Font.Color := clBackground;  //clGrayText;
  NewRules := TStringList.Create;
  OldRules := TStringList.Create;
  OldRulesEnabled := TStringList.Create;
end;

procedure TRulesEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  NewRules.Free;
  OldRules.Free;
  OldRulesEnabled.Free;
end;

procedure TRulesEditorForm.OkBtnClick(Sender: TObject);
var
  Err: Integer = 0;
  BadRuleIndex: Integer = 0;
  ErrMsg: String = '';
begin
  if HasChanged then Err := ReplaceRules(BadRuleIndex);
  if Err = 0 then ModalResult := mrOK else
  begin
    ErrMsg := 'Error ' + IntToStr(Err) + ' occurred in Rule ' +
      RuleGrid.Cells[1,BadRuleIndex] + LineEnding + LineEnding +
      'Please continue editing.';
    utils.MsgDlg(ErrMsg, mtError, [mbOK], self);
    RestoreRules;
  end;
end;

procedure TRulesEditorForm.HelpBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('#rule_based_controls_editor');
end;

procedure TRulesEditorForm.SetButtonStates;
var
  Status: Boolean;
begin
  Status := True;
  if RuleGrid.RowCount = 1 then Status := False;
  EditBtn.Enabled := Status;
  DeleteBtn.Enabled := Status;
  MoveUpBtn.Enabled := Status;
  MoveDnBtn.Enabled := Status;
  if RuleGrid.Row = 1 then MoveUpBtn.Enabled := False;
  if RuleGrid.Row >= RuleGrid.RowCount-1 then MoveDnBtn.Enabled := False;
end;

procedure TRulesEditorForm.InsertBtnClick(Sender: TObject);
begin
  EditAction := Inserting;
  EditRule('');
end;

procedure TRulesEditorForm.EditBtnClick(Sender: TObject);
begin
  EditAction := Editing;
  EditRule(RuleMemo.Text);
end;

procedure TRulesEditorForm.MoveDnBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  HasChanged := True;
  Index := RuleGrid.Row;
  RuleGrid.MoveColRow(False, Index, Index + 1);
  NewRules.Exchange(Index-1, Index);
  RuleGrid.Row := Index + 1;
  ShowRule(Index);
end;

procedure TRulesEditorForm.MoveUpBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  HasChanged := True;
  Index := RuleGrid.Row - 1;
  RuleGrid.MoveColRow(False, Index, Index + 1);
  NewRules.Exchange(Index - 1, Index);
  RuleGrid.Row := Index;
  ShowRule(Index - 1);
end;

procedure TRulesEditorForm.RuleGridCheckboxToggled(Sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
  HasChanged := True;
end;

procedure TRulesEditorForm.DeleteBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  HasChanged := True;
  Index := RuleGrid.Row;
  if Index = 0 then exit;
  RuleGrid.DeleteRow(Index);
  NewRules.Delete(Index-1);
  Index := RuleGrid.Row;
  ShowRule(Index-1);
  SetButtonStates;
end;

procedure TRulesEditorForm.EditRule(Rule: String);
var
  WasEdited: Boolean = False;
  EditedRule: String = '';
  RuleEditForm: TRuleEditForm;
begin
  RuleEditForm := TRuleEditForm.Create(self);
  try
    RuleEditForm.LoadRule(Rule);
    if RuleEditForm.ShowModal = mrOK then
    begin
      if RuleEditForm.HasChanged then
      begin
        WasEdited := True;
        RuleEditForm.UnloadRule(EditedRule);
      end;
    end;
  finally
    RuleEditForm.Free;
  end;
  if WasEdited then ReplaceRule(EditedRule);
end;

procedure TRulesEditorForm.ReplaceRule(Rule: String);
var
  Index: Integer;
begin
  if EditAction = Editing then
  begin
    Index := RuleGrid.Row;
    NewRules[Index-1] := Rule;
    RuleGrid.Cells[1, Index] := GetRuleID(Rule);
  end;
  if EditAction = Inserting then
  begin
    Index := RuleGrid.Row + 1;
    NewRules.Insert(Index-1, Rule);
    RuleGrid.InsertRowWithValues(Index, ['1', GetRuleID(Rule)]);
    RuleGrid.Row := Index;
  end;
  ShowRule(Index - 1);
  HasChanged := True;
  SetButtonStates;
end;

procedure TRulesEditorForm.RuleGridPrepareCanvas(Sender: TObject; aCol,
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

procedure TRulesEditorForm.RuleGridSelection(Sender: TObject; aCol,
  aRow: Integer);
const
  OldRow: Integer = 0;
begin
  if (aRow <> OldRow) and (aRow >= 1) then
  begin
    OldRow := aRow;
    ShowRule(aRow-1);
  end;
end;

procedure TRulesEditorForm.GetObjectInfo(ObjIndex: Integer; ObjCode: Integer;
      var ObjType: Integer; var ObjID: String);
begin
  ObjID := '';
  if ObjCode = EN_R_NODE then
  begin
    epanet2.ENgetnodetype(ObjIndex, ObjType);
    ObjID := project.GetID(cNodes, ObjIndex);
  end
  else if ObjCode = EN_R_LINK then
  begin
    epanet2.ENgetlinktype(ObjIndex, ObjType);
    if ObjType < EN_PIPE then ObjType := EN_PIPE
    else if ObjType > EN_PUMP then ObjType := EN_PUMP + 1;
    ObjType := EN_TANK + ObjType;
    ObjID := project.GetID(cLinks, ObjIndex);
  end
  else ObjType := 8;
end;

procedure TRulesEditorForm.LoadRules;
var
  nRules: Integer;
  R: Integer;
  ID: array[0..EN_MAXID+1] of AnsiChar;
  Rule: String;
  EnabledCode: Integer = 1;
begin
  nRules := 0;
  epanet2.ENgetcount(EN_RULECOUNT, nRules);
  RuleGrid.RowCount := nRules + 1;
  for R := 1 to nRules do
  begin
    epanet2.ENgetruleID(R, ID);
    RuleGrid.Cells[1,R] := ID;
    ENgetruleenabled(R, EnabledCode);
    RuleGrid.Cells[0,R] := IntToStr(EnabledCode);
    Rule := GetRuleAsString(R);
    NewRules.Add(Rule);
    OldRules.Add(Rule);
    OldRulesEnabled.Add(RuleGrid.Cells[0,R]);
  end;
  if nRules > 0 then ShowRule(0);
  SetButtonStates;
  HasChanged := False;
end;

procedure TRulesEditorForm.ShowRule(I: Integer);
var
  Rule: String;
begin
  RuleMemo.Clear;
  if I < 0 then exit;
  Rule := NewRules[I];
  RuleMemo.Text := Rule;
  SetButtonStates;
end;

function TRulesEditorForm.GetRuleAsString(R: Integer): String;
var
  nPremises: Integer = 0;
  nThenActions: Integer = 0;
  nElseActions: Integer = 0;
  priority: Single = 0;
  id: array[0..EN_MAXID+1] of AnsiChar;
  Rule: String;
begin
  Rule := 'Rule ';
  epanet2.ENgetruleID(R, id);
  Rule := Rule + id;
  epanet2.ENgetrule(R, nPremises, nThenActions, nElseActions, priority);
  GetPremises(R, nPremises, Rule);
  GetActions(R, nThenActions, nElseActions, Rule);
  if priority > 0 then
    Rule := Rule + sLineBreak + Format('Priority %0.0f', [priority]);
  Result := Rule;
end;

function TRulesEditorForm.GetRuleID(Rule: String): String;
var
  RuleList: TStringList;
begin
  RuleList := TStringList.Create;
  try
    RuleList.Text := Trim(Rule);
    Result := RuleList[0];
    Result := Trim(StringReplace(Result, 'RULE', '', [rfIgnoreCase]));
  finally
    RuleList.Free;
  end;
end;

procedure TRulesEditorForm.GetPremises(R: Integer; N: Integer; var Rule: String);
var
  line: String;
  p: Integer;
  logop: Integer = 0;
  objCode: Integer = 0;
  objType: Integer = 0;
  objIndex: Integer = 0;
  objID: String;
  varCode: Integer = 0;
  relop: Integer = 0;
  status: Integer = 0;
  setting: Single = 0;
begin
  objID := '';
  for p := 1 to N do
  begin
    epanet2.ENgetpremise(R, p, logop, objCode, objIndex, varCode, relop,
      status, setting);
    if p = 1 then logop := 1;
    line := LogWord[logop] + ' ';
    GetObjectInfo(objIndex, objCode, objType, objID);
    line := line + ObjWord[objType] + ' ' + objID + ' ' + VarWord[varCode] +
            ' ' + RelWord[relop] + ' ';
    if setting <= EN_MISSING then line := line + StatusWord[status]
    else line := line + Format('%0.4f', [setting]);
    Rule := Rule + sLineBreak + line;
  end;
end;

procedure TRulesEditorForm.GetActions(R: Integer; nThenActions: Integer;
  nElseActions: Integer; var Rule: String);
var
  i: Integer;
  linkIndex: Integer = 0;
  status: Integer = 0;
  setting: Single = 0;
  line: String;
begin
  for i := 1 to nThenActions do
  begin
    epanet2.ENgetthenaction(R, i, linkIndex, status, setting);
    if i = 1 then line := 'THEN ' else line := 'AND ';
    line := line + GetAnAction(linkIndex, status, setting);
    Rule := Rule + sLineBreak + line;
  end;
  for i := 1 to nElseActions do
  begin
    epanet2.ENgetelseaction(R, i, linkIndex, status, setting);
    if i = 1 then line := 'ELSE ' else line := 'AND ';
    line := line + GetAnAction(linkIndex, status, setting);
    Rule := Rule + sLineBreak + line;
  end;
end;

function TRulesEditorForm.GetAnAction(LinkIndex: Integer; Status: Integer;
  Setting: Single): String;
var
  objType: Integer = 0;
  objID: String;
begin
  Result := '';
  epanet2.ENgetlinktype(linkIndex, objType);
  if objType < EN_PIPE then objType := EN_PIPE
  else if objType > EN_PUMP then objType := EN_PUMP + 1;
  objType := EN_TANK + objType;
  objID := project.GetID(cLinks, linkIndex);
  Result := Result + ObjWord[objType] + ' ' + objID;
  if setting <= EN_MISSING
  then Result := Result + ' STATUS = ' + StatusWord[status]
  else Result := Result + Format(' SETTING = %0.4f', [setting]);
end;

procedure TRulesEditorForm.DeleteRules;
var
  nRules: Integer;
  R: Integer;
begin
  nRules := 0;
  epanet2.ENgetcount(EN_RULECOUNT, nRules);
  for R := nRules downto 1 do epanet2.ENdeleterule(R);
end;

function TRulesEditorForm.ReplaceRules(var BadRuleIndex: Integer): Integer;
var
  I, N: Integer;
  Rule: String;
begin
  Result := 0;
  DeleteRules;
  Rule := '';
  BadRuleIndex := 0;
  N := NewRules.Count;
  for I := 1 to N do
  begin
    Rule := NewRules[I-1];
    Result := epanet2.ENaddrule(PAnsiChar(Rule));
    if Result > 0 then
    begin
      BadRuleIndex := I;
      exit;
    end;
    if SameText(RuleGrid.Cells[0,I], '0') then
      epanet2.ENsetruleenabled(I, 0);
  end;
end;

procedure TRulesEditorForm.RestoreRules;
var
  I: Integer;
  N: Integer;
  Rule: String;
begin
  DeleteRules;
  N := OldRules.Count;
  for I := 0 to N-1 do
  begin
    Rule := OldRules[I];
    epanet2.ENaddrule(PAnsiChar(Rule));
    if SameText(OldRulesEnabled[I], '0') then
      epanet2.ENsetruleenabled(I+1, 0);
  end;
end;

end.
