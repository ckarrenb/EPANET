{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       controledit
 Description:  a dialog form edits simple control statement
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit controledit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TControlEditForm }

  TControlEditForm = class(TForm)
    AcceptBtn: TBitBtn;
    AtBtn: TRadioButton;
    CancelEditBtn: TBitBtn;
    HelpBtn: TButton;
    IfBtn: TRadioButton;
    Label5: TLabel;
    LevelComboBox: TComboBox;
    LevelEdit: TEdit;
    LinkComboBox: TComboBox;
    LinkEdit: TEdit;
    NodeEdit: TEdit;
    NodeLabel: TLabel;
    procedure AcceptBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure IfBtnChange(Sender: TObject);
  private
    function  PropertiesToStr: String;
    function  ValidateControl: Boolean;
  public
    EditedControl: String;
    procedure SetControlProperties(const aControl: String);
  end;

var
  ControlEditForm: TControlEditForm;

implementation

{$R *.lfm}

uses
  main, project, config, utils;

const
  LevelTypeItems: String = 'BELOW,ABOVE';
  TimeTypeItems: String = 'TIME,CLOCKTIME';

{ TControlEditForm }

procedure TControlEditForm.FormCreate(Sender: TObject);
begin
  Color := config.ThemeColor;
  Font.Size := config.FontSize;
  EditedControl := '';
end;

procedure TControlEditForm.HelpBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('#simple_controls');
end;

procedure TControlEditForm.AcceptBtnClick(Sender: TObject);
begin
  if not ValidateControl then exit;
  EditedControl := PropertiesToStr;
  ModalResult := mrOK;
end;

procedure TControlEditForm.IfBtnChange(Sender: TObject);

// Change the ControlEdit panel between editing a Level-based control
// versus a Time-based control

begin
  LevelComboBox.Clear;
  LevelEdit.Clear;

  // IfBtn checked means we're editing a Level-Based control
  if IfBtn.Checked then
  begin
    NodeLabel.Visible := True;
    NodeEdit.Visible := True;
    LevelComboBox.Items.AddCommaText(LevelTypeItems);
  end else

  // Otherwise we're editing a Time-based control
  begin
    NodeLabel.Visible := False;
    NodeEdit.Visible := False;
    LevelComboBox.Items.AddCommaText(TimeTypeItems);
  end;
  LevelComboBox.ItemIndex := 0;
end;

procedure TControlEditForm.SetControlProperties(const aControl: String);

// Fill form's editing controls with properties extracted from aControl

var
  aList: TStringList;
begin
  if Length(aControl) = 0 then exit;
  aList := TStringList.Create;
  try
    aList.DelimitedText := aControl;
    LinkEdit.Text := aList[1];
    LinkComboBox.Text := aList[2];
    if SameText(aList[4], 'NODE') then
    begin
      NodeLabel.Visible := True;
      NodeEdit.Visible := True;
      IfBtn.Checked := True;
      LevelComboBox.Items.Clear;
      LevelComboBox.Items.AddCommaText(LevelTypeItems);
      NodeEdit.Text := aList[5];
      LevelComboBox.Text := aList[6];
      LevelEdit.Text := aList[7];
    end else
    begin
      NodeLabel.Visible := False;
      NodeEdit.Visible := False;
      AtBtn.Checked := True;
      LevelComboBox.Items.Clear;
      LevelComboBox.Items.AddCommaText(TimeTypeItems);
      if SameText(aList[4], 'TIME') then
        LevelComboBox.ItemIndex := 0
      else
        LevelComboBox.ItemIndex := 1;
      LevelEdit.Text := aList[5];
    end;
  finally
    aList.Free;
  end;
end;

function TControlEditForm.PropertiesToStr: String;
begin
  Result := '';
  Result := 'LINK ' + LinkEdit.Text + ' ' + LinkComboBox.Text;
  if IfBtn.Checked then
    Result := Result + ' IF NODE ' + NodeEdit.Text + ' '
  else
    Result := Result + ' AT ';
  Result := Result + LevelComboBox.Text + ' ' + LevelEdit.Text;
end;

function TControlEditForm.ValidateControl: Boolean;
var
  X: Single = 0;
begin
  Result := False;
  if project.GetItemIndex(cLinks, LinkEdit.Text) <= 0 then
  begin
    utils.MsgDlg('Link ' + LinkEdit.Text + ' does not exist.',
      mtError, [mbOK]);
    LinkEdit.SetFocus;
    exit;
  end;
  with LinkComboBox do
  begin
    if (ItemIndex < 0) and (utils.Str2Float(Text, X) = False) then
    begin
      utils.MsgDlg('Invalid link setting.', mtError, [mbOK]);
      SetFocus;
      exit;
    end;
  end;

  if NodeEdit.Visible then
  begin
    if project.GetItemIndex(cNodes, NodeEdit.Text) <= 0 then
    begin
      utils.MsgDlg('Node ' + NodeEdit.Text + ' does not exist.',
        mtError, [mbOK]);
      NodeEdit.SetFocus;
      exit;
    end;
    with LevelEdit do
    begin
      if (utils.Str2Float(Text, X) = False) then
      begin
        utils.MsgDlg('Invalid node level.', mtError, [mbOK]);
        SetFocus;
        exit;
      end;
    end;
  end

  else with LevelEdit do
  begin
    if (utils.Str2Float(Text, X) = False) and (utils.Str2Seconds(Text) < 0) then
    begin
      utils.MsgDlg('Invalid time value.', mtError, [mbOK]);
      SetFocus;
      exit;
    end;
  end;
  Result := True;
end;

end.

