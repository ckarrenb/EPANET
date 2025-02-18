{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       ruleedit
 Description:  a dialog form edits a rule-based control
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit ruleedit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TRuleEditForm }

  TRuleEditForm = class(TForm)
    AcceptBtn: TBitBtn;
    HelpBtn: TBitBtn;
    CancelEditBtn: TBitBtn;
    Label1: TLabel;
    RuleMemo: TMemo;
    procedure AcceptBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure RuleMemoChange(Sender: TObject);
  private

  public
    HasChanged: Boolean;
    procedure LoadRule(Rule: String);
    procedure UnloadRule(var Rule: String);
  end;

var
  RuleEditForm: TRuleEditForm;

implementation

{$R *.lfm}

uses
  main, config;

{ TRuleEditForm }

procedure TRuleEditForm.FormCreate(Sender: TObject);
begin
  Color := config.ThemeColor;
  Font.Size := config.FontSize;
  RuleMemo.Font.Size := config.FontSize;
end;

procedure TRuleEditForm.AcceptBtnClick(Sender: TObject);
begin
  Hide;
end;

procedure TRuleEditForm.HelpBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('#rule_based_controls');
end;

procedure TRuleEditForm.RuleMemoChange(Sender: TObject);
begin
  HasChanged := True;
end;

procedure TRuleEditForm.LoadRule(Rule: String);
begin
  if Length(Rule) = 0 then
  begin
    RuleMemo.Lines.Add('RULE ');
    RuleMemo.Lines.Add('IF   ');
    RuleMemo.Lines.Add('THEN ');
    RuleMemo.SelStart := 5;
  end else
  begin
    RuleMemo.Text := Rule;
    RuleMemo.SelStart := 0;
  end;
  HasChanged := False;
end;

procedure TRuleEditForm.UnloadRule(var Rule: String);
begin
  Rule := RuleMemo.Text;
end;

end.

