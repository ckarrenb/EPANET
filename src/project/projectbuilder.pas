{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       projectbuilder
 Description:  Adds new objects to a project
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit projectbuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Dialogs;

function  FindUnusedID(Category: Integer; SubCategory: Integer): String;
procedure AddNode(NodeType: Integer; Xcoord: Double; Ycoord: Double);
procedure AddLink(LinkType: Integer; Node1: Integer; Node2: Integer);
procedure AddPattern;
procedure AddCurve;
procedure AddLabel(Location: TPoint; Xcoord: Double; Ycoord: Double);
procedure ImportShapeFile;
procedure ImportDxfFile;

implementation

uses
  main, project, projectframe, mapframe, maplabel, labeleditor,
  utils, shpimporter, dxfimporter, epanet2;


function FindUnusedID(Category: Integer; SubCategory: Integer): String;
var
  I: Integer;
  N: Integer;
begin
  // IDprefix array contains prefixes for Junctions, Reservoirs, Tanks,
  // Pipes, Pumps, Valves, Patterns, and Curves in that order.
  case Category of
  cNodes:    I := 1 + SubCategory;
  cLinks:    I := 3 + SubCategory;
  cPatterns: I := 7;
  cCurves:   I := 8;
  else       I := 0;
  end;
  N := 0;
  while true do
  begin
    Inc(N);
    Result := Project.IDprefix[I] + IntToStr(N);
    if Project.GetItemIndex(Category, Result) = 0 then
      break;
  end;
end;

procedure AddNode(NodeType: Integer; Xcoord: Double; Ycoord: Double);
var
  ID: string;
  NodeIndex: Integer = 0;
  Err: Integer;
begin
  ID := FindUnusedID(cNodes, NodeType);
  Err := Epanet2.ENaddnode(PChar(ID), NodeType, NodeIndex);
  if Err = 0 then
  begin
    Epanet2.ENsetcoord(NodeIndex, Xcoord, Ycoord);
    Epanet2.ENsetnodevalue(NodeIndex, EN_ELEVATION,
      StrToFloatDef(Project.DefProps[1], 0));
    if NodeType = nTank then
    begin
      Epanet2.ENsetnodevalue(NodeIndex, EN_MAXLEVEL,
        StrToFloatDef(Project.DefProps[2], 0.0));
      Epanet2.ENsetnodevalue(NodeIndex, EN_TANKDIAM,
        StrToFloatDef(Project.DefProps[3], 0.0));
    end;
    MainForm.MapFrame.RedrawMap;
    MainForm.ProjectFrame.SelectItem(cNodes, NodeIndex-1);
    Project.HasChanged := True;
    Project.UpdateResultsStatus;
  end
  else Utils.MsgDlg('Unable to add a new node.', mtError, [mbOK]);
end;

procedure AddLinkVertices(LinkIndex: Integer);
var
  X: array[0..Project.MAX_VERTICES] of Double;
  Y: array[0..Project.MAX_VERTICES] of Double;
  N: Integer = 0;
begin
  X[0] := 0;
  Y[0] := 0;
  MainForm.MapFrame.GetVertices(X, Y, N);
  if N > 0 then
  begin
    Epanet2.ENsetvertices(LinkIndex, X[0], Y[0], N);
    Project.HasChanged := True;
  end;
end;

procedure AddLink(LinkType: Integer; Node1: Integer; Node2: Integer);
var
  LinkID: string;
  FromNodeID, ToNodeID: string;
  LinkIndex: Integer = 0;
  Length: Single = 0;
  Err: Integer;
begin
  LinkID := FindUnusedID(cLinks, LinkType);
  FromNodeID := Project.GetID(cNodes, Node1);
  ToNodeID := Project.GetID(cNodes, Node2);
  if LinkType = lValve then LinkType := EN_TCV;
  Err := Epanet2.ENaddlink(Pchar(LinkID), LinkType, PChar(FromNodeID),
    PChar(ToNodeID), LinkIndex);
  if Err = 0 then
  begin
    AddLinkVertices(LinkIndex);
    if LinkType = lPipe then
    begin
      if Project.AutoLength then
        Length := Project.FindLinkLength(LinkIndex)
      else
        Length := StrToFloatDef(Project.DefProps[4], 0.0);
      ENsetpipedata(LinkIndex, Length, StrToFloatDef(Project.DefProps[5], 0.0),
        StrToFloatDef(Project.DefProps[6], 0.0), 0.0);
    end;
    MainForm.MapFrame.RedrawMap;
    MainForm.ProjectFrame.SelectItem(cLinks, LinkIndex-1);
    Project.HasChanged := True;
    Project.UpdateResultsStatus;
  end
  else Utils.MsgDlg('Unable to add a new link.', mtError, [mbOK]);
end;

procedure AddPattern;
var
  ID: string;
  Err: Integer;
begin
  ID := FindUnusedID(cPatterns, 0);
  Err := Epanet2.ENaddpattern(PChar(ID));
  if Err = 0 then with MainForm.ProjectFrame do
  begin
    Project.HasChanged := True;
    Project.UpdateResultsStatus;
  end
  else
    Utils.MsgDlg('Unable to add a new pattern.', mtError, [mbOK]);
end;

procedure AddCurve;
var
  ID: string;
  Err: Integer;
begin
  ID := FindUnusedID(cCurves, 0);
  Err := Epanet2.ENaddcurve(PChar(ID));
  if Err = 0 then with MainForm.ProjectFrame do
  begin
    Project.HasChanged := True;
    Project.UpdateResultsStatus;
  end
  else
    Utils.MsgDlg('Unable to add a new curve.', mtError, [mbOK]);
end;

function GetLabelText(Location: TPoint): String;
var
  LabelEditorForm: TLabelEditorForm;
begin
  Result := '';
  LabelEditorForm := TLabelEditorForm.Create(MainForm.MapFrame);
  with LabelEditorForm do
  try
    Left := Location.x;
    Top := Location.Y;
    if ShowModal = mrOK then Result := Edit1.Text;
  finally
    Free;
  end;
end;

procedure AddLabel(Location: TPoint; Xcoord: Double; Ycoord: Double);
var
  S: string;
  MapLabel: TMapLabel;
begin
  S := GetLabelText(Location);
  if Length(S) = 0 then exit;
  MapLabel := TMapLabel.Create;
  MapLabel.X := Xcoord;
  MapLabel.Y := Ycoord;
  Project.MapLabels.AddObject(S, Maplabel);
  MainForm.MapFrame.RedrawMap;
  MainForm.ProjectFrame.SelectItem(cLabels, Project.MapLabels.Count-1);
  Project.HasChanged := True;
end;

procedure ImportShapeFile;
begin
  with TShpImporterForm.Create(MainForm) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure ImportDxfFile;
begin
  with TDxfImporterForm.Create(MainForm) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.

