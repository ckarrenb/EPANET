{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       config
 Description:  reads, saves and edits program preferenecs
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Graphics, SysUtils, Controls, IniFiles;

const
  WinFontSize = 9;
  NixFontSize = 9;

var
  MapHiliter: Boolean;
  MapHinting: Boolean;
  ShowNotifiers: Boolean;
  ConfirmDeletions: Boolean;
  ShowWelcomePage: Boolean;
  ShowSpeedBar: Boolean;
  OpenLastFile: Boolean;
  BackupFile: Boolean;
  ThemeColor: TColor;
  AlternateColor: TColor;
  DecimalPlaces: Integer;
  IconFamily: String;
  FontSize: Integer;

  procedure ReadPreferences(FileName: string);
  procedure SavePreferences(FileName: string);
  procedure EditPreferences(var ClearFileList: Boolean);

implementation

uses
  main, configeditor;

procedure ReadPreferences(FileName: string);
var
  Ini: TIniFile;
begin
  MapHiliter := True;
  MapHinting := True;
  ShowNotifiers := True;
  ConfirmDeletions := True;
  ShowWelcomePage := True;
  ShowSpeedBar := True;
  IconFamily := 'Material';
  OpenLastFile := True;
  BackupFile := False;
  ThemeColor := GrayTheme;
  AlternateColor := $F6F6F3;
  DecimalPlaces := 2;

  {$ifdef UNIX}
    FontSize := NixFontSize;
  {$else}
    FontSize := WinFontSize;
  {$endif}

  if FileExists(FileName) then
  begin
    Ini := TIniFile.Create(FileName);
    try
      MapHiliter := Ini.ReadBool('Preferences', 'Map Hiliting', True);
      MapHinting := Ini.ReadBool('Preferences', 'Map Hinting', True);
      ConfirmDeletions := Ini.ReadBool('Preferences', 'Confirm Deletions', True);
      ShowWelcomePage := Ini.ReadBool('Preferences', 'Show Welcome Page', True);
      ShowSpeedBar := Ini.ReadBool('Preferences', 'Show Speed Bar', True);
      OpenLastFile := Ini.ReadBool('Preferences', 'Open Last File', True);
      BackupFile := Ini.ReadBool('Preferences', 'Backup File', False);
      DecimalPlaces := Ini.ReadInteger('Preferences', 'Decimal Places', 2);
      ThemeColor := Ini.ReadInteger('Preferences', 'Theme Color', ThemeColor);
      IconFamily := Ini.ReadString('Preferences', 'Icon Family', 'Material');
    finally
      Ini.Free;
    end;
  end;
end;

procedure SavePreferences(FileName: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteBool('Preferences', 'Map Hiliting', MapHiliter);
    Ini.WriteBool('Preferences', 'Map Hinting', MapHinting);
    Ini.WriteBool('Preferences', 'Confirm Deletions', ConfirmDeletions);
    Ini.WriteBool('Preferences', 'Show Welcome Page', ShowWelcomePage);
    Ini.WriteBool('Preferences', 'Show Speed Bar', ShowSpeedBar);
    Ini.WriteBool('Preferences', 'Open Last File', OpenLastFile);
    Ini.WriteBool('Preferences', 'Backup File', BackupFile);
    Ini.WriteInteger('Preferences', 'Decimal Places', DecimalPlaces);
    Ini.WriteInteger('Preferences', 'Theme Color', ThemeColor);
    Ini.WriteString('Preferences', 'Icon Family', IconFamily);
  finally
    Ini.Free;
  end;
end;

procedure EditPreferences(var ClearFileList: Boolean);
var
  OldThemeColor: TColor;
  OldIconFamily: String;
begin
  OldThemeColor := ThemeColor;
  OldIconFamily := IconFamily;
  with TConfigForm.Create(MainForm) do
  try
    SetPreferences;
    ShowModal;
    if ModalResult = mrOK then
    begin
      GetPreferences(ClearFileList);
      MainForm.MainMenuFrame.SpeedPanel1.Visible := ShowSpeedBar;
      if not SameText(OldIconFamily, IconFamily) then
        MainForm.MainMenuFrame.SetIconFamily(IconFamily);
      if OldThemeColor <> ThemeColor then with MainForm do
      begin
        Color := ThemeColor;
        Refresh;
        MainMenuFrame.SetColorTheme;
      end;
    end;
  finally
    Free;
  end;
end;

end.
