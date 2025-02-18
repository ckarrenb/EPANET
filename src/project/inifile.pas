{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       inifile
 Description:  saves and retrieves project settings to an inifile
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit inifile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Dialogs, StrUtils, Graphics, Forms;

procedure SaveFormPosition(FileName: string);
procedure ReadFormPosition(FileName: string);
procedure ReadAppDefaults(FileName: string);
procedure WriteAppDefaults(FileName: string);
procedure ReadProjectDefaults(FileName: string; var WebMapSource: Integer);
procedure WriteProjectDefaults(FileName: string; WebMapSource: Integer);
procedure WriteProjectMapOptions(FileName: string; WebMapSource:Integer);

implementation

uses
  project, main, mapoptions, epanet2;

const
  DefProps: array[1..project.MAX_DEF_PROPS] of string =
    ('0', '20', '50',      // Node elevation, Tank height & diameter
     '1000', '12', '130'); // Pipe length, diameter & roughness

  DefOptions: array[1..project.MAX_DEF_OPTIONS] of string =
    ('gpm', 'H-W', '40',   // Flow units & head loss model
     '50',                 // Max trials
     '0.001', '0', '0');   // Accuracy, flow & head tolerances

procedure SaveFormPosition(FileName: string);
var
  Ini: TIniFile;
  W, H, L, T: LongInt;
  DPI: LongInt;
begin
  DPI := Screen.PixelsPerInch;
  Ini := TIniFile.Create(FileName);
  try
    with mainForm do
    begin
      L := (Left * 96) div DPI;
      T := (Top * 96) div DPI;
      W := (Width * 96) div DPI;
      H := (Height * 96) div DPI;
      Ini.WriteInteger('MainForm', 'Left', L);
      Ini.WriteInteger('MainForm', 'Top', T);
      Ini.WriteInteger('MainForm', 'Width', W);
      Ini.WriteInteger('MainForm', 'Height', H)
    end;
  finally
    Ini.Free;
  end;

end;

procedure ReadFormPosition(FileName: string);
var
  Ini: TIniFile;
  W, H: Integer;
begin
  W := Screen.Width - Screen.Width div 4;
  H := Screen.Height - Screen.Height div 4;
  if not FileExists(FileName) then
  begin
    mainForm.BoundsRect := Bounds(0, 0, W, H);
    exit;
  end;

  Ini := TIniFile.Create(FileName);
  try
    mainForm.BoundsRect := Bounds(
      Ini.ReadInteger('MainForm', 'Left', 0),
      Ini.ReadInteger('MainForm', 'Top', 0),
      Ini.ReadInteger('MainForm', 'Width', W),
      Ini.ReadInteger('MainForm', 'Height', H));
  finally
    Ini.Free;
  end;
end;

procedure ReadAppDefaults(FileName: string);
var
  I: Integer;
  Ini: TIniFile;
  Options: array [1..project.MAX_DEF_OPTIONS] of string;
begin

  if not FileExists(FileName) then
  begin
    for I := 1 to project.MAX_ID_PREFIXES do
      project.IDprefix[I] := '';
    for I := 1 to project.MAX_DEF_PROPS do
      project.DefProps[I] := DefProps[I];
    for I := 1 to project.MAX_DEF_OPTIONS do
      Options[I] := DefOptions[I];
  end
  else
  begin
    Ini := TIniFile.Create(FileName);
    try
      for I := 1 to project.MAX_ID_PREFIXES do
        project.IDprefix[I] := Ini.ReadString('ID_PREFIXES', IntToStr(I), '');
      for I := 1 to project.MAX_DEF_PROPS do
        project.DefProps[I] := Ini.ReadString('DEFAULTS', IntToStr(I), DefProps[I]);
      for I := 1 to project.MAX_DEF_OPTIONS do
        Options[I] := Ini.ReadString('OPTIONS', IntToStr(I), DefOptions[I]);
    finally
      Ini.Free;
    end;
  end;
  project.SetDefHydOptions(Options);
  epanet2.ENsetoption(EN_STATUS_REPORT, EN_NORMAL_REPORT);
end;

procedure WriteAppDefaults(FileName: string);
var
  I: Integer;
  Ini: TIniFile;
  Options: array [1..project.MAX_DEF_OPTIONS] of string;
begin
  if not FileExists(FileName) then exit;
  Ini := TIniFile.Create(FileName);
  try
    for I := 1 to project.MAX_ID_PREFIXES do
      Ini.WriteString('ID_PREFIXES', IntToStr(I), project.IDprefix[I]);
    for I := 1 to project.MAX_DEF_PROPS do
      Ini.WriteString('DEFAULTS', IntToStr(I), project.DefProps[I]);
    project.GetDefHydOptions(Options);
    for I := 1 to MAX_DEF_OPTIONS do
      Ini.WriteString('OPTIONS', IntToStr(I), Options[I]);
  finally
    Ini.Free;
  end;
end;

procedure ReadProjectDefaults(FileName: string; var WebMapSource: Integer);
var
  I: Integer;
  Ini: TIniFile;
  S: String;
begin
  WebMapSource := -1;
  if not FileExists(Filename) then exit;
  Ini := TIniFile.Create(FileName);
  try
    // Project default settings
    for I := 1 to project.MAX_ID_PREFIXES do
      project.IDprefix[I] := Ini.ReadString('ID_PREFIXES', IntToStr(I),
        project.IDprefix[I]);
    for I := 1 to project.MAX_DEF_PROPS do
      project.DefProps[I] := Ini.ReadString('DEFAULTS', IntToStr(I),
        project.DefProps[I]);

    // MSX file name
    S := Ini.ReadString('MSX', 'FILE', '');
    S := AnsiReplaceStr(S, '"', '');
    if Length(S) = 0 then
      project.MsxInpFile := ''
    else if Length(ExtractFilePath(S)) > 0 then
      project.MsxInpFile := S
    else
      project.MsxInpFile := ExtractFilePath(project.InpFile) + S;

    // Map display options
    with MainForm.MapFrame.Map.Options do
    begin
      NodeSize := Ini.ReadInteger('MAP', 'NODESIZE', DefaultOptions.NodeSize);
      ShowNodesBySize := Ini.ReadBool('MAP', 'SHOWNODESBYSIZE', DefaultOptions.ShowNodesBySize);
      ShowNodeBorder := Ini.ReadBool('MAP', 'SHOWNODEBORDER', DefaultOptions.ShowNodeBorder);
      LinkSize := Ini.ReadInteger('MAP', 'LINKSIZE', DefaultOptions.LinkSize);
      ShowLinksBySize := Ini.ReadBool('MAP', 'SHOWLINKSBYSIZE', DefaultOptions.ShowLinksBySize);
      ShowLinkBorder := Ini.ReadBool('MAP', 'SHOWLINKBORDER', DefaultOptions.ShowLinkBorder);
      S := ColorToString(DefaultOptions.BackColor);
      BackColor := StringToColor(Ini.ReadString('MAP', 'BACKCOLOR', S));
    end;
    WebMapSource := Ini.ReadInteger('MAP', 'WEBMAPSOURCE', -1);
  finally
    Ini.Free;
  end;
end;

procedure WriteProjectDefaults(FileName: string; WebMapSource: Integer);
var
  I: Integer;
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try

    try
      // Project default settings
      for I := 1 to project.MAX_ID_PREFIXES do
        Ini.WriteString('ID_PREFIXES', IntToStr(I), project.IDprefix[I]);
      for I := 1 to project.MAX_DEF_PROPS do
        Ini.WriteString('DEFAULTS', IntToStr(I), project.DefProps[I]);

      // MSX file name
      if SameText(ExtractFilePath(project.MsxInpFile),
        ExtractFilePath(project.InpFile))
      then Ini.WriteString('MSX', 'FILE', '"' +
        ExtractFileName(project.MsxInpFile) + '"')
      else Ini.WriteString('MSX', 'FILE', '"' + project.MsxInpFile + '"');

      // Map display options
      with MainForm.MapFrame.Map.Options do
      begin
        Ini.WriteInteger('MAP', 'NODESIZE', NodeSize);
        Ini.WriteBool('MAP', 'SHOWNODESBYSIZE', ShowNodesBySize);
        Ini.WriteBool('MAP', 'SHOWNODEBORDER', ShowNodeBorder);
        Ini.WriteInteger('MAP', 'LINKSIZE', LinkSize);
        Ini.WriteBool('MAP', 'SHOWLINKSBYSIZE', ShowLinksBySize);
        Ini.WriteBool('MAP', 'SHOWLINKBORDER', ShowLinkBorder);
        Ini.WriteString('MAP', 'BACKCOLOR', ColorToString(BackColor));
      end;
      Ini.WriteInteger('MAP', 'WEBMAPSOURCE', WebMapSource);

    // Catch any exception thrown
    except
    end;

  finally
    Ini.Free;
  end;
end;

procedure WriteProjectMapOptions(FileName: string; WebMapSource:Integer);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try

    try
      with MainForm.MapFrame.Map.Options do
      begin
        Ini.WriteInteger('MAP', 'NODESIZE', NodeSize);
        Ini.WriteBool('MAP', 'SHOWNODESBYSIZE', ShowNodesBySize);
        Ini.WriteBool('MAP', 'SHOWNODEBORDER', ShowNodeBorder);
        Ini.WriteInteger('MAP', 'LINKSIZE', LinkSize);
        Ini.WriteBool('MAP', 'SHOWLINKSBYSIZE', ShowLinksBySize);
        Ini.WriteBool('MAP', 'SHOWLINKBORDER', ShowLinkBorder);
        Ini.WriteString('MAP', 'BACKCOLOR', ColorToString(BackColor));
      end;
      Ini.WriteInteger('MAP', 'WEBMAPSOURCE', WebMapSource);
    except
    end;

  finally
    Ini.Free;
  end;
end;

end.

