{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       dxfloader
 Description:  reads contents of a DXF file
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit dxfloader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StrUtils, Dialogs;

type
  TDxfOptions = record
    CoordUnits:       Integer;
    SnapTol:          Double;
    ComputeLengths:   Boolean;
  end;

const
  MISSING = -1.0E40;
  _POLYLINE   = 0;
  _LWPOLYLINE = 1;
  _LINE       = 2;
  _VERTEX     = 3;
  _SEQEND     = 4;

  IN_NONE     = 0;
  IN_POLYLINE = 1;
  IN_LINE     = 2;
  IN_VERTEX   = 3;

  luFEET   = 1;
  luMETERS = 2;

  Keywords: array[0..4] of String =
    ('POLYLINE', 'LWPOLYLINE', 'LINE', 'VERTEX', 'SEQEND');

procedure LoadDxfFile(DxfFileName: String; Layers: TStringList;
            DxfOptions: TDxfOptions);

function  FindEntitiesSection(var F: TextFile): Boolean;

procedure GetLinkVertices(var F: TextFile; Layers: TStringList;
            var Vx: array of Double; var Vy: array Of Double; var Vcount: Integer);

implementation

uses
  main, project, projectbuilder, mapcoords, utils, epanet2;

var
  X, Y:        Double;      // Current coordinates being processed
  StatusCode:  Integer;     // Current status of reading the DXF file
  CoordCount:  Integer;     // Count of processed link coordinates
  SnapTol:     Double;
  ComputeLengths: Boolean;
  LengthUcf:   Double;

function FindEntitiesSection(var F: TextFile): Boolean;
// Find where 'ENTITIES' section of DXF file begins.

var
  Code: Integer;
  Value: String;
begin
  Result := True;
  while not Eof(F) do
  begin
    ReadLn(F, Code);
    ReadLn(F, Value);
    if (Code = 2) and SameText(Value, 'ENTITIES') then exit;
  end;
  Result := False;
end;

procedure DoKeyWord(Kw: Integer);
begin
  case Kw of
    _POLYLINE, _LWPOLYLINE:
    begin
      StatusCode := IN_POLYLINE;
      CoordCount := 0;
    end;
    _LINE:
    begin
      StatusCode := IN_LINE;
      CoordCount := 0;
    end;
    _VERTEX:
    begin
      if StatusCode <> IN_NONE then StatusCode := IN_VERTEX;
    end;
    _SEQEND:
    begin
      StatusCode := IN_NONE;
      CoordCount := 0;
    end;
  end;
end;

procedure AddVertex(Code: Integer; V: Double; var Vx: array of Double;
            var Vy: array Of Double; var Vcount: Integer);
// Save the coordinates of a vertex read from file to arrays Vx and Vy.

begin
  Inc(CoordCount);
  case Code of
    10: X := V;
    20: Y := V;
    11: X := V;
    21: Y := V;
  end;
  if not Odd(CoordCount) then
  begin
    Vx[Vcount] := X;
    Vy[Vcount] := Y;
    if Vcount < Project.MAX_VERTICES then Inc(Vcount);
  end;
end;

procedure GetLinkVertices(var F: TextFile; Layers: TStringList;
            var Vx: array of Double; var Vy: array Of Double; var Vcount: Integer);
// Read the vertices that comprise a network link from the DXF file.

var
  Code: Integer;
  S: String;
  V: Double;
begin
  // Initialize status variables
  StatusCode := IN_NONE;
  CoordCount := 0;
  Vcount := 0;

  // Read pairs of lines from DXF file
  while not Eof(F) do
  begin
    Readln(F,Code);
    Readln(F,S);

    // Check if reached end of current link being processed
    if SameText(S, 'SEQEND') and (StatusCode in [IN_LINE, IN_VERTEX]) then exit;

    // Process DXF code
    case Code of
    // Key word text
    0:  DoKeyWord(AnsiIndexText(S, Keywords));

    // Layer name
    8:  if (StatusCode in [IN_LINE,IN_POLYLINE]) and (Layers.Count > 0) then
          if (Layers.IndexOf(S) < 0) then StatusCode := IN_NONE;

    // Coordinate vertex value
    10, 11, 20, 21:
        begin
          utils.Str2Float(S, V);
          if (StatusCode = IN_LINE) or (StatusCode = IN_VERTEX) then
            AddVertex(Code,V,Vx,Vy,Vcount);
        end;
    end;
  end;
end;

function GetNearestNode(P: TDoublePoint): Integer;
//  Find the index of the project node closest to point P that is
//  within the snap tolerance. Return 0 if there is no such node.

var
  J, Jmin: Integer;
  D, Dmin: Double;
  Pj: TDoublePoint;
begin
  Jmin := 0;
  Dmin := 1.0e40;

  // Examine each project node
  for J := 1 to project.GetItemCount(cNodes) do
  begin

    // Find Manhattan distance between point P and the node point Pj
    if not project.GetNodeCoord(J, Pj.X, Pj.Y) then continue;
    D := mapcoords.ManhattanDistance(P, Pj);

    // Update minimum distance found
    if D < Dmin then
    begin
      Dmin := D;
      Jmin := J;
    end;
  end;

  // Check that minimum distance is within snap tolerance
  if Dmin <= SnapTol then Result := Jmin else Result := 0;
end;

function AddNode(P: TDoublePoint): Integer;
// Add a node with coordinates P to the project.

var
  ID: String;
  Err: Integer;
  NodeIndex: Integer;
begin
  // Obtain an ID name for the node
  Result := 0;
  ID := projectbuilder.FindUnusedID(cNodes, project.nJunction);

  // Try to add the node to the EPANET project
  Err := epanet2.ENaddnode(PAnsiChar(ID), EN_JUNCTION, NodeIndex);

  // Set node's coordinates and default elevation
  if Err = 0 then
  begin
    epanet2.ENsetcoord(NodeIndex, P.X, P.Y);
    epanet2.ENsetnodevalue(NodeIndex, EN_ELEVATION,
      StrToFloatDef(project.DefProps[1], 0));
    Result := NodeIndex;
  end;
end;

function GetEndNode(P: TDoublePoint): String;
// Get the name of a project node that is close to the end node
// of a link with coordinates P. Add a new node if an existing
// can't be found.

var
  J: Integer;
begin
  // Link node is within snap tolerance of an existing node
  Result := '';
  J := GetNearestNode(P);
  if J > 0 then Result := project.GetID(cNodes, J)

  // Otherwise add a new node
  else begin
    J := AddNode(P);
    if J > 0 then Result := project.GetID(cNodes, J);
  end;
end;

procedure SetLinkProps(LinkIndex: Integer);
// Assign default properties to a network link read from the DXF file.

var
  Len, Diameter, Roughness: Single;
begin
  // Get default pipe length
  Len := StrToFloatDef(project.DefProps[4], 0.0);

  // Compute it if that option was selected
  if project.AutoLength or ComputeLengths then
    Len := project.FindLinkLength(LinkIndex) * LengthUcf;

  // Assign pipe diameter
  Diameter := StrToFloatDef(project.DefProps[5], 0.0);

  // Assign pipe roughness
  Roughness := StrToFloatDef(project.DefProps[6], 0.0);

  // Assign properties to the pipe (last argument is for minor loss coeff.)
  epanet2.ENsetpipedata(LinkIndex, Len, Diameter, Roughness, 0.0);
end;

function NewLink(StartNode: String; EndNode: String): Integer;
// Add a new link to the project between nodes StartNode and EndNode.

var
  LinkIndex, Err: Integer;
  LinkID: String;
begin
  // Assign link an ID name
  Result := 0;
  LinkID := projectbuilder.FindUnusedID(cLinks, EN_PIPE);

  // Try adding it to the project
  Err := epanet2.ENaddlink(Pchar(LinkID), EN_PIPE, PChar(StartNode),
    PChar(EndNode), LinkIndex);

  // Set its properties -- return its index in the project's list of links
  if Err = 0 then
  begin
    SetLinkProps(LinkIndex);
    Result := LinkIndex;
  end;
end;

procedure AddLink(var Vx: array of Double; var Vy: array Of Double;
            Vcount: Integer);
// Add a new link and its end nodes to the project.

var
  StartNode: String;
  EndNode: String;
  LinkIndex: Integer;
  P: TDoublePoint;
begin
  // Must have at least 2 vertices
  if Vcount < 2 then exit;

  // Find start node of the link
  P.X := Vx[0];
  P.Y := Vy[0];
  StartNode := GetEndNode(P);
  if Length(StartNode) = 0 then exit;

  // Find end node of the link
  P.X := Vx[Vcount-1];
  P.Y := Vy[Vcount-1];
  EndNode := GetEndNode(P);
  if Length(EndNode) = 0 then exit;

  // Add the link and its vertices to the project
  LinkIndex := NewLink(StartNode, EndNode);
  if (LinkIndex > 0) and (Vcount > 2) then
    epanet2.ENsetvertices(LinkIndex, Vx[1], Vy[1], Vcount-2);
end;

procedure LoadDxfFile(DxfFileName: String; Layers: TStringList;
            DxfOptions: TDxfOptions);
// Extract a set of pipe links and their end nodes from a DXF file.

var
  F: TextFile;
  Vx: array[0..project.MAX_VERTICES] of Double;
  Vy: array[0..project.MAX_VERTICES] of Double;
  Vcount: Integer;
begin
  SnapTol := DxfOptions.SnapTol;
  ComputeLengths := DxfOptions.ComputeLengths;
  LengthUcf := 1.0;
  if (DxfOptions.CoordUnits = luMETERS) and (project.GetUnitsSystem = usUS) then
    LengthUcf := 3.28084
  else if (DxfOptions.CoordUnits = luFEET) and (project.GetUnitsSystem = usSI) then
    LengthUcf := 1 / 3.28084;

  AssignFile(F, DxfFileName);
  try
    Reset(F);
    if not FindEntitiesSection(F) then exit;
    while not Eof(F) do
    begin
      Vcount := 0;
      GetLinkVertices(F, Layers, Vx, Vy, Vcount);
      AddLink(Vx, Vy, Vcount);
    end;
    MainForm.MapFrame.SetExtent(mapcoords.GetBounds(MainForm.MapFrame.GetExtent));
    MainForm.MapFrame.DrawFullextent;
    Project.HasChanged := True;
    Project.UpdateResultsStatus;
  finally
    CloseFile(F);
  end;
end;

end.

