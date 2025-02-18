{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       mapquery
 Description:  a frame that locates network objects that meet a
               specified criterion.
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}
unit mapquery;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ExtCtrls, Graphics,
  LCLtype, Dialogs, Math;

type

  { TMapQueryFrame }

  TMapQueryFrame = class(TFrame)
    FindBtn: TBitBtn;
    CloseBtn: TSpeedButton;
    FindCbx: TComboBox;
    ParamCbx: TComboBox;
    ConditionCbx: TComboBox;
    ValueEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ResultPanel: TPanel;
    TopPanel: TPanel;
    procedure CloseBtnClick(Sender: TObject);
    procedure FindCbxChange(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
    procedure ValueEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ResultPanelClick(Sender: TObject);
    procedure FindBtnClick(Sender: TObject);
    procedure TargetChange(Sender: TObject);
  private
    Target: Single;
    FilteredCount: Integer;
    function IsFiltered(const Value: Single): Boolean;
  public
    NodeQuery: Boolean;
    LinkQuery: Boolean;
    procedure Init;
    procedure Show;
    procedure Hide;
    procedure UpdateResults;
    function GetFilteredNodeColor(const NodeIndex: Integer): TColor;
    function GetFilteredLinkColor(const LinkIndex: Integer): TColor;
  end;

implementation

{$R *.lfm}

uses
  main, project, mapframe, mapthemes, config, utils;

{ TMapQueryFrame }

procedure TMapQueryFrame.Init;
begin
  FindCbx.ItemIndex := 0;
  ParamCbx.ItemIndex := 0;
  NodeQuery := True;
  LinkQuery := False;
end;

procedure TMapQueryFrame.Show;
begin
  TopPanel.Color := config.ThemeColor;
  ValueEdit.Text := '';
  ResultPanel.Caption := '';
  NodeQuery := False;
  LinkQuery := False;
  FindCbxChange(self);
  Visible := True;
end;

procedure TMapQueryFrame.Hide;
begin
 Visible := False;
end;

procedure TMapQueryFrame.CloseBtnClick(Sender: TObject);
begin
 Hide;
 NodeQuery := False;
 LinkQuery := False;
 MainForm.MapFrame.RedrawMap;
end;

procedure TMapQueryFrame.FindCbxChange(Sender: TObject);
var
  I, N: Integer;
  MainViewCombo: TComboBox;
begin
  ParamCbx.Clear;
  if FindCbx.ItemIndex = 0 then
  begin
    NodeQuery := True;
    LinkQuery := False;
    MainViewCombo := MainForm.MainMenuFrame.ViewNodeCombo;
  end
  else
  begin
    NodeQuery := False;
    LinkQuery := True;
    MainViewCombo := MainForm.MainMenuFrame.ViewLinkCombo;
  end;
  N := MainViewCombo.Items.Count;
  for I := 1 to N-1 do
    ParamCbx.Items.Add(MainViewCombo.Items[I]);
  I := MainViewCombo.ItemIndex - 1;
  if I < 0 then I := 0;
  ParamCbx.ItemIndex := I;
end;

procedure TMapQueryFrame.ValueEditChange(Sender: TObject);
begin
  ResultPanel.Caption := '';
end;

procedure TMapQueryFrame.ValueEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then FindBtnClick(Sender);
end;

procedure TMapQueryFrame.ResultPanelClick(Sender: TObject);
begin

end;

procedure TMapQueryFrame.FindBtnClick(Sender: TObject);
begin
  Target := 0;
  if Length(ValueEdit.Text) = 0 then exit;
  if not Utils.Str2Float(ValueEdit.Text, Target) then
  begin
    MsgDlg('"' + ValueEdit.Text + '" is not a valid number.', mtError, [mbOK], MainForm);
    exit;
  end;
  UpdateResults;
end;

procedure TMapQueryFrame.TargetChange(Sender: TObject);
begin
  ResultPanel.Caption := '';
end;

procedure TMapQueryFrame.UpdateResults;
begin
  ResultPanel.Caption := '';
  if not Utils.Str2Float(ValueEdit.Text, Target) then exit;
  FilteredCount := 0;
  if NodeQuery then
  begin
    MainForm.MainMenuFrame.ViewNodeCombo.ItemIndex := ParamCbx.ItemIndex + 1;
    MapThemes.ChangeTheme(MainForm.LegendTreeView, cNodes,
      MainForm.MainMenuFrame.ViewNodeCombo.ItemIndex);
  end;
  if LinkQuery then
  begin
    MainForm.MainMenuFrame.ViewLinkCombo.ItemIndex := ParamCbx.ItemIndex + 1;
    MapThemes.ChangeTheme(MainForm.LegendTreeView, cLinks,
      MainForm.MainMenuFrame.ViewLinkCombo.ItemIndex);
  end;
  MainForm.MapFrame.RedrawMap;
  ResultPanel.Caption := IntToStr(FilteredCount) + ' items found.';
end;

function TMapQueryFrame.GetFilteredNodeColor(const NodeIndex: Integer): TColor;
var
  Value: Single;
  Theme: Integer;
  TimePeriod: Integer;
begin
  Result := clGray;
  if not NodeQuery then exit;
  Theme := ParamCbx.ItemIndex + 1;
  TimePeriod := mapthemes.TimePeriod;
  Value := mapthemes.GetNodeValue(NodeIndex, Theme, TimePeriod);
  if (Value <> MISSING) and IsFiltered(Value) then
  begin
    Inc(FilteredCount);
    Result := $00277FFF;
  end;
end;

function TMapQueryFrame.GetFilteredLinkColor(const LinkIndex: Integer): TColor;
var
  Value: Single;
  Theme: Integer;
  TimePeriod: Integer;
begin
  Result := clGray;
  if not LinkQuery then exit;
  Theme := ParamCbx.ItemIndex + 1;
  TimePeriod := mapthemes.TimePeriod;
  Value := mapthemes.GetLinkValue(LinkIndex, Theme, TimePeriod);
  if Theme = ltFlow then Value := Abs(Value);
  if (Value <> MISSING) and IsFiltered(Value) then
  begin
    Inc(FilteredCount);
    Result := $00277FFF;
  end;
end;

function TMapQueryFrame.IsFiltered(const Value: Single):Boolean;
begin
  Result := False;
  case ConditionCbx.ItemIndex of
  0: if Value < Target then Result := True;
  1: if Value = Target then Result := True;
  2: if Value > Target then Result := True;
  end;
end;

end.

