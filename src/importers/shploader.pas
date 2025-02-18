{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       shploader
 Description:  loads the contents of a shapefile into a project
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit shploader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, StrUtils, Math;

type
  TShpOptions = record
    NodeFileName:     String;
    LinkFileName:     String;
    NodeAttribs:      array[1..6] of Integer;
    NodeUnits:        array[1..6] of String;
    LinkAttribs:      array[1..9] of Integer;
    LinkUnits:        array[1..9] of String;
    CoordUnits:       Integer;
    SnapTol:          Double;
    SnapUnits:        Integer;
    ComputeLengths:   Boolean;
  end;

const
  nID        = 1;
  nType      = 2;
  nDescrip   = 3;
  nTag       = 4;
  nElev      = 5;
  nDemand    = 6;

  lID        = 1;
  lType      = 2;
  lStartNode = 3;
  lEndNode   = 4;
  lDescrip   = 5;
  lTag       = 6;
  lLength    = 7;
  lDiam      = 8;
  lRough     = 9;

var
  ShpOptions: TShpOptions;

procedure LoadShapeFile(theShpOptions: TShpOptions);

implementation

uses
  main, mapcoords, project, projectbuilder, shpapi, epanet2;

const
  FlowPerCFS: array[0..10] of Double =
    (1.0, 448.831, 0.64632, 0.5382, 1.9837,  {CFS, GPM, MGD, IMGD, AFD per CFS}
     28.317, 1699.0, 2.4466, 101.94, 2446.6, {LPS, LPM, MLD, CMH, CMD per CFS}
     0.028317); {CMS per CFS}

var
  FieldType: array of DBFFieldType;

function GetStringAttrib(Dbf: DBFHandle; I, J: Integer; var S: String): Boolean;
begin
  S := '';
  Result := False;
  if J < 0 then exit;
  if FieldType[J] <> FTString then exit;
  S := shpapi.DBFReadStringAttribute(Dbf, I, J);
  Result := True;
end;

function GetNumericalAttrib(Dbf: DBFHandle; I, J: Integer; var V: Double): Boolean;
begin
  V := 0;
  Result := False;
  if J < 0 then exit;
  if FieldType[J] = FTInteger then
    V := shpapi.DBFReadIntegerAttribute(Dbf, I, J)
  else if FieldType[J] = FTDouble then
    V := shpapi.DBFReadDoubleAttribute(Dbf, I, J)
  else exit;
  Result := True;
end;

function GetID(Dbf: DBFHandle; I, J: Integer): String;
//  Read ID string from field J of the I-th record in a dBase file.

begin
  if FieldType[J] = FTString then
    Result := shpapi.DBFReadStringAttribute(Dbf, I, J)
  else if FieldType[J] = FTInteger then
    Result := IntToStr(shpapi.DBFReadIntegerAttribute(Dbf, I, J))
  else
    Result := '';
end;

{==================== NODE FUNCTIONS =======================================}

function GetNodeID(Dbf: DBFHandle; NodeType: Integer; I: Integer): String;
// Retrieve the ID string of the I-th record in a nodes dBase file.

var
  J, Index: Integer;
  ID: String;
begin
  // Try reading ID from dBase file
  ID := '';
  if Dbf <> nil then
  begin
    J := ShpOptions.NodeAttribs[nID];
    if J >= 0 then ID := GetID(Dbf, I, J);
  end;

  // Check if ID used by another node
  if Length(ID) > 0 then
  begin
    epanet2.ENgetnodeindex(PAnsiChar(ID), Index);
    if Index > 0 then ID := '';
  end;

  // If ID still blank then find an unused one
  if Length(ID) = 0 then
    ID := projectbuilder.FindUnusedID(cNodes, NodeType);
  Result := ID;
end;

function GetNodeType(Dbf: DBFHandle; I: Integer): Integer;
// Return the type of node appearing as the I-th record of a nodes dBase file.

var
  J, K: Integer;
  S: String;
begin
  // Assume node type is a Junction
  Result := project.nJunction;
  if Dbf <> nil then
  begin
    // J is the 0-based field index for node type in the dBase file
    J := ShpOptions.NodeAttribs[nType];
    if J >= 0 then
    begin
      // Node type appears as a string attribute
      if FieldType[J] = FTString then
      begin
        S := shpapi.DBFReadStringAttribute(Dbf, I, J);
        // See if string S is a Reservoir or a Tank
        if StartsText('RES', S) then Result := project.nReservoir
        else if StartsText('TANK', S) then Result := project.nTank;
      end;

      // Node type appears as an integer type code
      if FieldType[J] = FTInteger then
      begin
        K := shpapi.DBFReadIntegerAttribute(Dbf, I, J);
        if K = project.nReservoir then Result := K
        else if K = project.nTank then Result := K;
      end;
    end;
  end;
end;

function NodeUcf(Attrib: Integer): Double;
// Return a units conversion factor for a given node attribute.

var
  S: String;
  I, J: Integer;
begin
  // Find the units S that the user specified for the attribute
  Result := 1;
  S := ShpOptions.NodeUnits[Attrib];
  if Length(S) = 0 then exit;
  
  // Find the conversion factor for Elevation
  if Attrib = nElev then
  begin
    if SameText(S, 'METERS') and (project.GetUnitsSystem = usUS) then
      Result := 3.28084
    else if SameText(S, 'FEET') and (project.GetUnitsSystem = usSI) then
      Result := 1 / 3.28084;
  end;
  
  // Find the conversion factor for base Demand
  if Attrib = nDemand then
  begin
    I := AnsiIndexText(S, project.FlowUnitsStr); //Attrib flow units index
    epanet2.ENgetflowunits(J);                   //Project flow units index
    if I >= 0 then
      Result := FlowPerCFS[J] / FlowPerCFS[I];
  end;
end;

procedure SetNodeProps(Dbf: DBFHandle; I: Integer; NodeIndex: Integer);
// Set the properties of the project node with index NodeIndex to the
//  corresponding attributes stored in the I-th dBase file record.

var
  S: String;
  V: Double;
begin
  // Node description comment
  if GetStringAttrib(Dbf, I, ShpOptions.NodeAttribs[nDescrip], S)
  and (Length(S) > 0)
  then epanet2.ENsetcomment(EN_NODE, NodeIndex, PAnsiChar(S));
  
  // Node elevation
  if GetNumericalAttrib(Dbf, I, ShpOptions.NodeAttribs[nElev], V) then
    epanet2.Ensetnodevalue(NodeIndex, EN_ELEVATION, NodeUcf(nElev)*V);
    
  // Node demand
  if GetNumericalAttrib(Dbf, I, ShpOptions.NodeAttribs[nDemand], V) then
    epanet2.Ensetnodevalue(NodeIndex, EN_BASEDEMAND, NodeUcf(nDemand)*V);
end;

function AddNode(Dbf: DBFHandle; I: Integer;  X,Y: Double): Integer;
// Add a new node from the I-th record in a nodes shape file.

var
  ID: String;
  Err: Integer;
  NodeType, NodeIndex: Integer;
begin
  // Determine what is the node's subtype
  Result := 0;
  NodeType := GetNodeType(Dbf, I);

  // Obtain an ID name for the node
  ID := GetNodeID(Dbf, NodeType, I);

  // Try to add the node to the EPANET project
  Err := epanet2.ENaddnode(PAnsiChar(ID), NodeType, NodeIndex);

  // Node was successfully added
  if Err = 0 then
  begin
    // Set the node's coordinates and its default properties
    epanet2.ENsetcoord(NodeIndex, X, Y);
    epanet2.ENsetnodevalue(NodeIndex, EN_ELEVATION,
      StrToFloatDef(project.DefProps[1], 0));
    if NodeType = nTank then
    begin
      epanet2.ENsetnodevalue(NodeIndex, EN_MAXLEVEL,
        StrToFloatDef(project.DefProps[2], 0.0));
      epanet2.ENsetnodevalue(NodeIndex, EN_TANKDIAM,
        StrToFloatDef(project.DefProps[3], 0.0));
    end;

    // Assign any node properties contained in the dBase file
    if (Dbf <> nil) then SetNodeProps(Dbf, I, NodeIndex);
    Result := NodeIndex;
  end;
end;

procedure GetFieldTypes(Dbf: DBFHandle);
// Store the type code (FTString, FTInteger, or FTDouble) of each attribute
// of a dBase file in the module-level FieldType array.

var
  I, N: Integer;
  Fname: array[0..XBASE_FLDNAME_LEN_READ] of Char;
  Fwidth, Fdec: Integer;
begin
  N := shpapi.DBFGetFieldCount(Dbf);
  SetLength(FieldType, N);
  for I := 0 to N-1 do
    FieldType[I] := shpapi.DBFGetFieldInfo(Dbf, I, Fname, Fwidth, Fdec);
end;

procedure LoadNodes;
// Load contents of a Nodes shape file and its dBase file into the project.

var
  Filename: String;
  Shp: SHPHandle;
  ShpObj: PShpObject;
  Dbf: DBFHandle;
  Count, I: Integer;
  ShapeType: Integer;
  MinBound: array [0..3] of Double;
  MaxBound: array [0..3] of Double;
  X, Y: Double;
begin
  with ShpOptions do
  begin
    if Length(NodeFileName) = 0 then exit;
    Filename := ChangeFileExt(NodeFileName, '.shp');
    Count := 0;
    Shp := nil;
    Dbf := nil;
    try

      // Open the shape file and its dBase file
      Shp := shpapi.SHPOpen(PAnsiChar(Filename), 'rb');
      if Shp = nil then exit;
      Filename := ChangeFileExt(NodeFileName, '.dbf');
      if FileExists(Filename) then
        Dbf := shpapi.DBFOpen(PAnsiChar(Filename), 'rb');

      // Save field type of each dBase attribute in FieldType array
      if Dbf <> nil then GetFieldTypes(Dbf);

      // Find the number of node objects (Count) in the file
      shpapi.SHPGetInfo(Shp, Count, ShapeType, MinBound, MaxBound);

      // Add each node in the file to the project
      for I := 0 to Count-1 do
      begin
        ShpObj := shpapi.SHPReadObject(Shp, I);
        if ShpObj = nil then continue;
        X := ShpObj^.padfX[0];
        Y := ShpObj^.padfY[0];
        shpapi.SHPDestroyObject(ShpObj);
        AddNode(Dbf, I, X, Y);
      end;

    finally
      SetLength(FieldType, 0);
      shpapi.SHPClose(Shp);
      shpapi.DBFClose(Dbf);
    end;
  end;
end;

{==================== LINK FUNCTIONS =======================================}

function GetLinkType(Dbf: DBFHandle; I: Integer): Integer;
// Return the type of link appearing as the I-th record of a links dBase file.

var
  J, K: Integer;
  S: String;
begin
  Result := project.lPipe;
  if Dbf <> nil then
  begin
    // J is the 0-based field index for link type in the dBase file
    J := ShpOptions.LinkAttribs[lType];
    if J >= 0 then
    begin
      if FieldType[J] = FTString then
      begin
        S := shpapi.DBFReadStringAttribute(Dbf, I, J);
        if StartsText('PUMP', S) then Result := project.lPump
        else if StartsText('VALVE', S) then Result := project.lValve;
      end;
      if FieldType[J] = FTInteger then
      begin
        K := shpapi.DBFReadIntegerAttribute(Dbf, I, J);
        if K = project.lPump then Result := K
        else if K = project.lValve then Result := K;
      end;
    end;
  end;
end;

procedure AddLinkVertices(ShpObj: PShpObject; LinkIndex: Integer);
//  Add the vertex points contained in a polyline shape object to those
//  of the network link with index LinkIndex.

var
  X: array[0..Project.MAX_VERTICES] of Double;
  Y: array[0..Project.MAX_VERTICES] of Double;
  X0, Y0: Double;
  X1, Y1: Double;
  N, Vcount, J: Integer;
begin
  // Find the number of vertices in the shape object and set the
  // number currently added to 0
  N := Min(ShpObj^.nVertices, project.MAX_VERTICES);
  Vcount := 0;

  // If there are at least 3 vertices (that include the line endpoints)
  if N >= 3 then
  begin

    // Save the starting vertex coordinates
    X0 := ShpObj^.padfX[0];
    Y0 := ShpObj^.padfY[0];

    // Visit each vertex in the shape object (not including the ending one)
    for J := 1 to N-2 do
    begin
      // Retrieve the vertex coordinates from the shape object
      X1 := ShpObj^.padfX[J];
      Y1 := ShpObj^.padfY[J];

      // Check that this vertex doesn't lay on top of the previous one
      if (X1 = X0) or (Y1 = Y0) then continue;

      // Store the vertex's coordinates in the local arrays
      X[Vcount] := X1;
      Y[Vcount] := Y1;
      Inc(Vcount);

      // Replace the previous vertex coordinates
      X0 := X1;
      Y0 := Y1;
    end;

    // Transfer the locally stored vertices to those for link LinkIndex
    if Vcount > 0 then
      epanet2.ENsetvertices(LinkIndex, X[0], Y[0], Vcount);
  end;
end;

function GetLinkValue(Dbf: DBFHandle; I: Integer; J: Integer): Single;
// Retrieve the value of link property J from the corresponding attribute
// in the I-th record of dBase file Dbf.

var
  V: Double;
begin
  GetNumericalAttrib(Dbf, I, ShpOptions.LinkAttribs[J], V);
  Result := V;
end;

function LinkUcf(Attrib: Integer): Double;
// Compute a units conversion factor for an imported link attribute.

var
  V: Single;
  S: String;
begin
  Result := 1;
  S := ShpOptions.LinkUnits[Attrib];
  if Length(S) = 0 then exit;

  // Unit conversion factor for length
  if Attrib = lLength then
  begin
    if SameText(S, 'METERS') and (project.GetUnitsSystem = usUS) then
      Result := 3.28084
    else if SameText(S, 'FEET') and (project.GetUnitsSystem = usSI) then
      Result := 1 / 3.28084;
  end;

  // Unit conversion factor for diameter
  if Attrib = lDiam then
  begin
    if SameText(S, 'MILLIMETERS') and
      (project.GetUnitsSystem = usUS) then Result := 0.03937
    else if SameText(S, 'INCHES') and
      (project.GetUnitsSystem = usSI) then Result := 25.4;
  end;

  // Unit conversion factor for D-W roughness
  if Attrib = lRough then
  begin
    epanet2.ENgetoption(EN_HEADLOSSFORM, V);
    if Round(V) = EN_DW then
    begin
      if SameText(S, 'MILLIMETERS') and
        (project.GetUnitsSystem = usUS) then Result := 39.37
      else if SameText(S, 'INCHES') and
        (project.GetUnitsSystem = usSI) then Result := 25.4
    end;
  end;
end;

procedure SetLinkProps(Dbf: DBFHandle; I: Integer; LinkType: Integer; LinkIndex: Integer);
// Set the properties of a link of type LinkType with index LinkIndex to the
// corresponding attributes stored in the I-th record of thr dBase file.

var
  Len, Diameter, Roughness: Single;
  J: Integer;
  S: String;
begin
  // Only pipe links have assigned properties
  if LinkType <> lPipe then exit;

  // Set link description
  GetStringAttrib(Dbf, I, ShpOptions.LinkAttribs[lDescrip], S);
  if Length(S) > 0 then epanet2.ENsetcomment(EN_Link, LinkIndex, PAnsiChar(S));

  // Get default pipe length
    Len := StrToFloatDef(project.DefProps[4], 0.0);

  // Override default length if supplied in dBse file
  if ShpOptions.LinkAttribs[lLength] >= 0 then
    Len := GetLinkValue(Dbf, I, lLength) * LinkUcf(lLength)

  // Or compute it if that option was selected
  else if project.AutoLength or ShpOptions.ComputeLengths then
    Len := project.FindLinkLength(LinkIndex) * LinkUcf(llength);

  // Retrieve pipe diameter
  if ShpOptions.LinkAttribs[lDiam] >= 0 then
    Diameter := GetLinkValue(Dbf, I, lDiam) * LinkUcf(lDiam)
  else
    Diameter := StrToFloatDef(project.DefProps[5], 0.0);

  // Retrieve pipe roughness
  if ShpOptions.LinkAttribs[lRough] >= 0 then
    Roughness := GetLinkValue(Dbf, I, lRough) * LinkUcf(lRough)
  else
    Roughness := StrToFloatDef(project.DefProps[6], 0.0);

  // Assign properties to the pipe (last argument is for minor loss coeff.)
    epanet2.ENsetpipedata(LinkIndex, Len, Diameter, Roughness, 0.0);
end;

function GetLinkID(Dbf: DBFHandle; LinkType: Integer; I: Integer): String;
// Retrieve the ID string for the I-th record in a links dBase file.

var
  J, Index: Integer;
  ID: String;
begin
  // Try reading ID from dBase file
  ID := '';
  if Dbf <> nil then
  begin
    J := ShpOptions.LinkAttribs[lID];
    if J >= 0 then ID := GetID(Dbf, I, J);
  end;

  // Check if ID used by another link
  if Length(ID) > 0 then
  begin
    epanet2.ENgetlinkindex(PAnsiChar(ID), Index);
    if Index > 0 then ID := '';
  end;

  // If ID still blank then find an unused ID
  if Length(ID) = 0 then
    ID := projectbuilder.FindUnusedID(cLinks, LinkType);
  Result := ID;
end;

function NewLink(Dbf: DBFHandle; I: Integer; StartNode: String; EndNode: String): Integer;
// Add a new link to the project between nodes StartNode and EndNode.

var
  LinkType, LinkIndex, Err: Integer;
  LinkID: String;
begin
  // Determine the link's type
  Result := 0;
  LinkType := GetLinkType(Dbf, I);
  if LinkType = lValve then LinkType := EN_TCV;
  
  // Assign it an ID name
  LinkID := GetLinkID(Dbf, LinkType, I);
  
  // Try adding it to the project
  Err := epanet2.ENaddlink(Pchar(LinkID), LinkType, PChar(StartNode),
    PChar(EndNode), LinkIndex);
    
  // Set its properties -- return its index in the project's list of links
  if Err = 0 then
  begin
    SetLinkProps(Dbf, I, LinkType, LinkIndex);
    Result := LinkIndex;
  end;
end;

function GetNearestNode(P: TDoublePoint): Integer;
//  Find the index of the project node closest to point P that is
//  within the snap tolerance. Return 0 if there is no such node.

var
  J, Jmin: Integer;
  D, Dmin, Dsnap, Xj, Yj: Double;
  Pj: TDoublePoint;
begin
  Jmin := 0;
  Dmin := 1.0e40;

  // Set snap tolerance in meters
  Dsnap := ShpOptions.SnapTol + 0.0001;
  if ShpOptions.SnapUnits = muFeet then Dsnap := Dsnap * 0.3048;

  // Convert X,Y in degrees to meters
  if ShpOptions.CoordUnits = muDegrees then
    P := mapcoords.FromWGS84ToWebMercator(P);

  // Examine each project node
  for J := 1 to project.GetItemCount(cNodes) do
  begin

    // Find Manhattan distance between point P and the node
    if not project.GetNodeCoord(J, Pj.X, Pj.Y) then continue;
    if ShpOptions.CoordUnits = muDegrees then
      Pj := mapcoords.FromWGS84ToWebMercator(Pj);
    D := mapcoords.ManhattanDistance(P, Pj);

    // Update minimum distance found
    if D < Dmin then
    begin
      Dmin := D;
      Jmin := J;
    end;
  end;

  // Check that minimum distance is within snap tolerance
  if Dmin <= Dsnap then Result := Jmin else Result := 0;
end;

function GetEndNode(Dbf: DBFHandle; P: TDoublePoint; I: Integer; WhichEnd: Integer): String;
// Get the name of a node at one end of the I-th link read from a shape file.

var
  J: Integer;
begin
  // See if the associated dBase file contains the link's node name
  Result := '';
  if Dbf <> nil then
  begin
    // Retrieve node name from links dBase file
    J := ShpOptions.LinkAttribs[WhichEnd];
    if J >= 0 then Result := GetID(Dbf, I, J);

    // Check that node name appears in the project
    if epanet2.ENgetnodeindex(PAnsiChar(Result), J) > 0 then Result := '';
  end;

  // Otherwise see if link node is within snap tolerance of an existing node
  if Length(Result) = 0 then
  begin
    J := GetNearestNode(P);
    if J > 0 then Result := project.GetID(cNodes, J)

    // Otherwise add a new node
    else begin
      J := AddNode(Nil, I, P.X, P.Y);
      if J > 0 then Result := project.GetID(cNodes, J);
    end;
  end;
end;

procedure AddLink(ShpObj: PShpObject; Dbf: DBFHandle; I: Integer);
// Add a new link to the project from the I-th object in a links shape file.

var
  StartNode: String;
  EndNode: String;
  LinkIndex: Integer;
  N: Integer;
  P: mapcoords.TDoublePoint;
begin
  // Find number of vertices in the link
  N := ShpObj^.nVertices;
  if N < 2 then exit;

  // Find start node of the link
  P.X := ShpObj^.padfX[0];
  P.Y := ShpObj^.padfY[0];
  StartNode := GetEndNode(Dbf, P, I, lStartNode);
  if Length(StartNode) = 0 then exit;

  // Find end node of the link
  P.X := ShpObj^.padfX[N-1];
  P.Y := ShpObj^.padfY[N-1];
  EndNode := GetEndNode(Dbf, P, I, lEndNode);
  if Length(EndNode) = 0 then exit;

  // Add the link and its vertices to the project
  LinkIndex := NewLink(Dbf, I, StartNode, EndNode);
  if LinkIndex > 0 then AddLinkVertices(ShpObj, LinkIndex);
end;

procedure LoadLinks;
// Load contents of a Links shape file and its dBase file into the project.

var
  Filename: String;
  Shp: SHPHandle;
  ShpObj: PShpObject;
  Dbf: DBFHandle;
  Count, I: Integer;
  ShapeType: Integer;
  MinBound: array [0..3] of Double;
  MaxBound: array [0..3] of Double;
  X, Y: Double;
begin
  with ShpOptions do
  begin
    if Length(LinkFileName) = 0 then exit;
    Filename := ChangeFileExt(LinkFileName, '.shp');
    Count := 0;
    Shp := nil;
    Dbf := nil;
    try

      // Open the shape file and its dBase file
      Shp := shpapi.SHPOpen(PAnsiChar(FileName), 'rb');
      if Shp = nil then exit;
      Filename := ChangeFileExt(LinkFileName, '.dbf');
      if FileExists(Filename) then
        Dbf := shpapi.DBFOpen(PAnsiChar(Filename), 'rb');

      // Save field type of each dBase attribute to FieldType array
      if Dbf <> nil then GetFieldTypes(Dbf);

      // Find the number of link objects (Count) in the file
      shpapi.SHPGetInfo(Shp, Count, ShapeType, MinBound, MaxBound);

      // Add each link in the file to the project
      for I := 0 to Count-1 do
      begin
        ShpObj := shpapi.SHPReadObject(Shp, I);
        if ShpObj = nil then continue;
        AddLink(ShpObj, Dbf, I);
        shpapi.SHPDestroyObject(ShpObj);
      end;

    finally
      SetLength(FieldType, 0);
      shpapi.SHPClose(Shp);
      shpapi.DBFClose(Dbf);
    end;
  end;
end;

procedure LoadShapeFile(theShpOptions: TShpOptions);
// Load node and link data from shape files into the current project.

begin
  ShpOptions := theShpOptions;
  project.MapUnits := ShpOptions.CoordUnits;
  LoadNodes;
  LoadLinks;
  MainForm.MapFrame.SetExtent(MapCoords.GetBounds(MainForm.MapFrame.GetExtent));
  MainForm.MapFrame.DrawFullextent;
  project.HasChanged := True;
  project.UpdateResultsStatus;
end;

end.
