{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       hydprofilerpt
 Description:  A frame that displays a hydraulic profile report
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit hydprofilerpt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, TAGraph, TASeries,
  TAGUIConnectorBGRA, TACustomSeries, TASources, TAStyles, TAChartCombos,
  Buttons, Dialogs, Menus, Graphics, StdCtrls;

type

  { THydProfileFrame }

  THydProfileFrame = class(TFrame)
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    ChartStyles1: TChartStyles;
    ExportMenu: TPopupMenu;
    ListChartSource1: TListChartSource;
    MnuProfile: TMenuItem;
    MnuCopy: TMenuItem;
    MnuSave: TMenuItem;
    ChartGUIConnectorBGRA1: TChartGUIConnectorBGRA;
    Separator1: TMenuItem;
    procedure Chart1AreaSeries1GetMark(out AFormattedMark: String;
      AIndex: Integer);
    procedure FrameResize(Sender: TObject);
    procedure MnuCopyClick(Sender: TObject);
    procedure MnuProfileClick(Sender: TObject);
    procedure MnuSaveClick(Sender: TObject);
  private
    Nlinks: Integer;
    LinksList: TStringList;
    NodesList: TStringList;
    procedure AddToPlot(X: Double; I, T: Integer);

  public
    procedure InitReport;
    procedure CloseReport;
    procedure RefreshReport;
    procedure ShowPopupMenu;
    procedure SetProfileLinks(ProfileLinks: TStrings);

  end;

implementation

{$R *.lfm}

uses
  main, project, profileselector, mapthemes, results, utils, epanet2;

{ THydProfileFrame }

procedure THydProfileFrame.InitReport;
var
  S: String;
begin
  LinksList := TStringList.Create;
  NodesList := TStringList.Create;
  Nlinks := 0;
  if Project.GetUnitsSystem = usUS then S := 'Feet' else S := 'Meters';
  with Chart1.BottomAxis do
  begin
    Title.Caption := 'Distance (' + S + ')';
    Intervals.MaxLength := 100;
  end;
  Chart1.LeftAxis.Title.Caption := S;

  // Bring up the Profile Selector frame
  MnuProfileClick(Self);
end;

procedure THydProfileFrame.CloseReport;
begin
  LinksList.Free;
  NodesList.Free;
  MainForm.ProfileSelectorFrame.Visible := False;
end;

procedure THydProfileFrame.SetProfileLinks(ProfileLinks: TStrings);
begin
  LinksList.Assign(ProfileLinks);
  RefreshReport;
end;

procedure THydProfileFrame.RefreshReport;
var
  I, N0, N1, N2, N3, N4, T, LinkIndex: Integer;
  X: Double;
  D: Single;

begin
  // Clear the current profile plot
  ListChartSource1.Clear;
  NodesList.Clear;
  X := 0;

  // Must have at least one link in a profile
  Nlinks := LinksList.Count;
  if Nlinks < 1 then exit;

  // Set time period to current period shown on network map
  T := mapthemes.TimePeriod;
  Chart1.Title.Text.Clear;
  Chart1.Title.Text.Add('Hydraulic Profile at ' +
    results.GetTimeStr(T) + ' hrs');

  // Detrmine the profile's initial starting node N0
  LinkIndex := project.GetItemIndex(cLinks, LinksList[0]);
  if not project.GetLinkNodes(LinkIndex, N1, N2) then exit;
  N0 := N1;
  if Nlinks > 1 then
  begin
    LinkIndex := project.GetItemIndex(cLinks, LinksList[1]);
    if project.GetLinkNodes(LinkIndex, N3, N4) then
    begin
      if (N1 = N3) or (N1 = N4) then N0 := N2 else N0 := N1;
    end;
  end;

  // Examine each link in the profile
  for I := 0 to Nlinks - 1 do
  begin
    // Add the current starting node to the plot
    AddToPlot(X, N0, T);

    // Update plot distance
    LinkIndex := project.GetItemIndex(cLinks, LinksList[I]);
    Epanet2.ENgetlinkvalue(LinkIndex, EN_LENGTH, D);
    if D <= 0 then D := 10;
    X := X +  D;

    // Find the next starting node
    if not project.GetLinkNodes(LinkIndex, N1, N2) then continue;
    if N1 = N0 then N0 := N2 else N0 := N1;
  end;

  // Add the last node to the plot
  AddToPlot(X, N0, T);
end;

procedure THydProfileFrame.AddToPlot(X: Double; I, T: Integer);
var
  E, H: Double;
begin
  E := project.GetNodeParam(I, EN_ELEVATION);
  H := mapthemes.GetNodeValue(I, ntHead, T);
  with ListChartSource1 do AddXYList(X, [E, H-E]);
  NodesList.Add(project.GetID(cNodes, I));
end;

procedure THydProfileFrame.ShowPopupMenu;
var
  P : TPoint;
begin
  P := Self.ClientToScreen(Point(0, 0));
  ExportMenu.PopUp(P.x,P.y);
end;

procedure THydProfileFrame.MnuCopyClick(Sender: TObject);
begin
  Chart1.CopyToClipboardBitmap;
end;

procedure THydProfileFrame.MnuProfileClick(Sender: TObject);
begin
  MainForm.HideLeftPanelFrames;
  MainForm.ProfileSelectorFrame.Visible := True;
  MainForm.ProfileSelectorFrame.Init(LinksList);
end;

procedure THydProfileFrame.MnuSaveClick(Sender: TObject);
begin
  with MainForm.SaveDialog1 do
  begin
    FileName := '*.png';
    Filter := 'Portable Network Graphic File|*.png';
    DefaultExt := '*.png';
    if Execute then Chart1.SaveToFile(TPortableNetworkGraphic, FileName);
  end;
end;

procedure THydProfileFrame.Chart1AreaSeries1GetMark(out AFormattedMark: String;
  AIndex: Integer);
begin
  AFormattedMark := NodesList[aIndex];
end;

procedure THydProfileFrame.FrameResize(Sender: TObject);
begin
  utils.ResizeControl(Chart1, ClientWidth, ClientHeight, 100, 50, 4, 0);
end;

end.

