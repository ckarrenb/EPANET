{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       sourceeditor
 Description:  a dialog form that edits a Water Quality source
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit sourceeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  lclIntf, EditBtn, SpinEx;

type

  { TSourceEditorForm }

  TSourceEditorForm = class(TForm)
    CancelBtn: TButton;
    HelpBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OkBtn: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    PatternEdit: TEditButton;
    SourceTypeCombo: TComboBox;
    StrengthEdit: TFloatSpinEditEx;
    procedure PatternEditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure PatternComboChange(Sender: TObject);
    procedure StrengthEditChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    NodeIndex: Integer;
  public
    HasChanged: Boolean;
    procedure LoadSource(const Index: Integer);
    function  GetSourceStrength: String;
  end;

var
  SourceEditorForm: TSourceEditorForm;

implementation

{$R *.lfm}

uses
  main, project, utils, config, patternselector, epanet2;

procedure TSourceEditorForm.FormCreate(Sender: TObject);
begin
  Color := config.ThemeColor;
  Font.Size := config.FontSize;
end;

procedure TSourceEditorForm.OkBtnClick(Sender: TObject);
var
  I: Integer;
  T: Integer;
  P: Integer;
  V: Single;
  Pattern: String;
begin
  Pattern := Trim(PatternEdit.Text);
  if Length(Pattern) = 0 then P := 0 else
  begin
    P := project.GetItemIndex(cPatterns, Pattern);
    if P = 0 then
    begin
      showmessage('Pattern ' + Pattern + ' does not exist.');
      exit;
    end;
  end;

  T := SourceTypeCombo.ItemIndex;
  V := StrengthEdit.Value;
  epanet2.ENsetnodevalue(NodeIndex, EN_SOURCETYPE, T);
  epanet2.ENsetnodevalue(NodeIndex, EN_SOURCEQUAL, V);
  epanet2.ENsetnodevalue(NodeIndex, EN_SOURCEPAT, P);
  ModalResult := mrOK;
end;


procedure TSourceEditorForm.PatternEditButtonClick(Sender: TObject);
var
  I: Integer;
  S: String;
begin
  S := PatternEdit.Text;
  with TPatternSelectorForm.Create(self) do
  try
    Setup(S);
    ShowModal;
    if ModalResult = mrOK then
      PatternEdit.Text := SelectedName;
  finally
    Free;
  end;
end;

procedure TSourceEditorForm.PatternComboChange(Sender: TObject);
begin
  HasChanged := True;
end;

procedure TSourceEditorForm.StrengthEditChange(Sender: TObject);
begin
  HasChanged := True;
end;

procedure TSourceEditorForm.HelpBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('#source_quality');
end;

procedure TSourceEditorForm.LoadSource(const Index: Integer);
var
  I: Integer;
  V: Single = 0;
begin
  NodeIndex := Index;
  SourceTypeCombo.ItemIndex := 0;
  if epanet2.ENgetnodevalue(Index, EN_SOURCETYPE, V) = 0 then
  begin
    SourceTypeCombo.ItemIndex := Round(V);
    epanet2.ENgetnodevalue(Index, EN_SOURCEQUAL, V);
    StrengthEdit.Value := V;
    epanet2.ENgetnodevalue(Index, EN_SOURCEPAT, V);
    PatternEdit.Text := project.GetID(cPatterns, Round(V));
  end;
  HasChanged := False;
end;

function TSourceEditorForm.GetSourceStrength: String;
begin
  Result := utils.Float2Str(StrengthEdit.Value, 4);
end;

end.

