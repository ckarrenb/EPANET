{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       configeditor
 Description:  a dialog form that edits program preferences
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit configeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  lclIntf, ExtCtrls;

const
  GrayTheme: Integer = $F1F1F1; {F8F8F8;}
  BlueTheme: Integer = $FDEEE3;

type

  { TConfigForm }

  TConfigForm = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    Label1: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private

  public
    procedure GetPreferences(var ClearFileList: Boolean);
    procedure SetPreferences;

  end;

var
  ConfigForm: TConfigForm;

implementation

{$R *.lfm}

uses
  main, config;

procedure TConfigForm.FormCreate(Sender: TObject);
begin
  Color := Config.ThemeColor;
  Font.Size := Config.FontSize;
end;

procedure TConfigForm.SetPreferences;
begin
  CheckBox1.Checked := config.MapHiliter;
  CheckBox2.Checked := config.MapHinting;
  CheckBox3.Checked := config.ConfirmDeletions;
  CheckBox4.Checked := config.ShowWelcomePage;
  CheckBox5.Checked := config.ShowSpeedBar;
  CheckBox6.Checked := SameText(config.IconFamily, 'Office');
  CheckBox7.Checked := config.ThemeColor = BlueTheme;
  CheckBox8.Checked := config.OpenLastFile;
  CheckBox9.Checked := config.BackupFile;
  SpinEdit1.Value := config.DecimalPlaces;
end;

procedure TConfigForm.HelpBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('#program_preferences');
end;

procedure TConfigForm.GetPreferences(var ClearFileList: Boolean);
begin
  config.MapHiliter := CheckBox1.Checked;
  config.MapHinting := CheckBox2.Checked;
  config.ConfirmDeletions := CheckBox3.Checked;
  config.ShowWelcomePage := CheckBox4.Checked;
  config.ShowSpeedBar := CheckBox5.Checked;
  if CheckBox6.Checked then
    config.IconFamily := 'Office'
  else config.IconFamily := 'Material';
  if CheckBox7.Checked then config.ThemeColor := BlueTheme
  else config.ThemeColor := GrayTheme;
  config.OpenLastFile := CheckBox8.Checked;
  config.BackupFile := CheckBox9.Checked;
  config.DecimalPlaces := SpinEdit1.Value;
  ClearFileList := CheckBox10.Checked;
end;

end.

