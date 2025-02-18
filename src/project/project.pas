{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       project
 Description:  sets, saves and retrieves project information
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, StrUtils;

{$I ..\timetype.txt}

type
  TRunStatus = (rsSuccess, rsWarning, rsError, rsWrongVersion,
                rsFailed, rsShutdown, rsCancelled, rsNone);

const
  FlowUnitsStr: array[0..10] of string =
    ('cfs', 'gpm', 'mgd', 'imgd', 'afd',
     'lps', 'lpm', 'mld', 'cmh', 'cmd', 'cms');

  HlossModelStr: array[0..2] of string =
    ('H-W', 'D-W', 'C-M');

  DemandModelStr: array[0..1] of string =
    ('DDA', 'PDA');

  QualModelStr: array[0..3] of string =
    ('No Quality', 'Chemical', 'Water Age', 'Source Trace');

  MixingModelStr: array[0..3] of string =
    ('Mixed', '2Comp', 'FIFO', 'LIFO');

  MassUnitsStr: array[0..1] of string = ('mg/L', 'ug/L');

  MapUnitsStr: array[0..3] of string = ('None', 'Feet', 'Meters', 'Degrees');

  OptionsStr: array[0..4] of string =
    ('Hydraulics', 'Demands', 'Quality', 'Times', 'Energy');

  ControlsStr: array[0..1] of string =
    ('Simple', 'Rule-Based');

  StatusStr: array[0..2] of string =
    ('Closed', 'Open', 'CV');

  ValveTypeStr: array[0..6] of string =
    ('PRV', 'PSV', 'PBV', 'FCV', 'TCV', 'GPV', 'PCV');

  ValveStatusStr: array[0..2] of string =
    ('Closed', 'Open', 'None');

  CurveTypeStr: array[0..5] of string =
    ('Volume', 'Pump', 'Efficiency', 'Head Loss', 'Generic', 'Valve');

  StatusRptStr: array[0..2] of string =
    ('None', 'Normal', 'Full');

  StatisticStr: array[0..4] of string =
    ('None', 'Averages', 'Minima', 'Maxima', 'Ranges');
    
  NoYesStr: array[0..1] of string = ('No', 'Yes');

  ItemTypeStr: array[0..7] of string =
    ('Project Title', 'Analysis Options', 'Network Nodes', 'Network Links',
     'Control Actions', 'Map Labels', 'Time Patterns', 'Data Curves');

  // Item category types
  cTitle    = 0;
  cOptions  = 1;
  cNodes    = 2;
  cLinks    = 3;
  cControls = 4;
  cLabels   = 5;
  cPatterns = 6;
  cCurves   = 7;

  // Option types
  oHydraul = 0;
  oDemands = 1;
  oQuality = 2;
  oTimes   = 3;
  oEnergy  = 4;

  // Node types
  nJunction  = 0;
  nReservoir = 1;
  nTank      = 2;

  // Link types
  lCVPipe    = 0;
  lPipe      = 1;
  lPump      = 2;
  lValve     = 3;

  // Curve types
  ctVolume   = 0;
  ctPump     = 1;
  ctEffic    = 2;
  ctHloss    = 3;
  ctGeneric  = 4;
  ctValve    = 5;

  // Single species quality
  qtNone     = 0;
  qtChem     = 1;
  qtAge      = 2;
  qtTrace    = 3;

  //Quality models
  qmNone     = 0;
  qmSingle   = 1;
  qmMulti    = 2;

  //Map coordinates units
  muNone     = 0;
  muFeet     = 1;
  muMeters   = 2;
  muDegrees  = 3;

  //Unit system
  usUS       = 0;
  usSI       = 1;

  //Results status
  rsNotAvailable  = 0;
  rsUpToDate      = 1;
  rsNeedsUpdating = 2;

  //Results types
  rtNone   = 0;
  rtMinima = 1;
  rtMaxima = 2;
  rtRanges = 3;

  MAX_VERTICES = 100;    // Maximum vertices per link
  MAX_ID_PREFIXES = 8;   // Objects with ID prefixes
  MAX_DEF_OPTIONS = 7;   // Default hydraulic options
  MAX_DEF_PROPS = 6;     // Default node/link properties

var
  InpFile:    string;        // Name of input file
  RptFile:    string;        // Name of report file
  OutFile:    string;        // Name of binary output file
  DmndFile:   string;        // Name of demand output file
  AuxFile:    string;        // Name of a temporary auxilary file
  MsxInpFile: string;        // Name of MSX input file
  MsxOutFile: string;        // Name of temporary MSX output file
  MsxHydFile: string;        // Name of temporary MSX hydraulics file

  FlowUnits:     Integer;    // Units of all flow rates
  MapUnits:      Integer;    // Units of map coordinates
  MapEPSG:       Integer;    // Current map EPSG code
  OldMapUnits:   Integer;    // Map units before before coord. transform
  OldMapEPSG:    Integer;    // EPSG code before coord. transform
  StatusRptType: Integer;    // Type of status report to produce
  ResultsStatus: Integer;    // Status of most current results

  RunStatus:        TRunStatus;  // Status of a simulation run
  MapLabels:        TStringList; // List of map label objects
  Properties:       TStringList; // List of an object's properties
  CopiedProperties: TStringList; // List of copied object's properties

  HasChanged:     Boolean;   // True if project data have changed
  AutoLength:     Boolean;   // True if pipe lengths found from map
  HasResults:     Boolean;   // True if simulation results available
  OutFileOpened:  Boolean;   // True if simulation output file is open
  MsxFileOpened:  Boolean;   // True if multi-species output file is open
  DmndFileOpened: Boolean;   // True if demand output file is open
  MsxFlag:        Boolean;   // True for mult-species analysis

  IDprefix:   array[1..MAX_ID_PREFIXES] of string;
  DefOptions: array[1..MAX_DEF_OPTIONS] of string;
  DefProps:   array[1..MAX_DEF_PROPS] of string;

procedure Open;
procedure Init;
procedure Clear;
procedure Close;

{**********************************************************************
 NOTE: An 'Item' argument indexes items in the network data base
       starting from 0 while an 'Index' argument indexes them starting
       from 1 which is the convention used in the EPANET API functions.
***********************************************************************}

function  GetItemID(Category: Integer; Item: Integer): string;
function  GetItemTypeStr(Category: Integer; Item: Integer): string;
function  GetItemIndex(Category: Integer; ID: String): Integer;
function  GetItemCount(Category: Integer): Integer;
function  IsEmpty: Boolean;

function  GetTitle(Item: Integer): string;
function  GetID(Category: Integer; Index: Integer): string;
function  GetIdError(Category: Integer; ID: string): string;
function  GetComment(Category: Integer; Index: Integer): string;
function  GetTag(Category: Integer; Index: Integer): string;

function  GetNodeType(Index: Integer): Integer;
function  GetLinkType(Index: Integer): Integer;
function  GetLinkNodes(Link: Integer; var Node1, Node2: Integer): Boolean;
function  FindLinkLength(LinkIndex: Integer): Single;
procedure AdjustLinkLengths(NodeIndex: Integer);
procedure SetPipeLength(PipeIndex: Integer);
procedure ReverseLinkNodes(Link: Integer);
procedure SetFlowUnits(Units: String);
procedure SetHeadLossModel(ModelIndex: Integer);
procedure SetDemandModel(ModelIndex: Integer);

function  GetPatternNames: String;
function  GetCurveNames(aCurveType: Integer): String;
function  GetCurveType(CurveIndex: Integer): Integer;

function  GetNodeParam(NodeIndex: Integer; Param: Integer): Single;
function  GetLinkParam(LinkIndex: Integer; Param: Integer): Single;
function  GetSourceQual(NodeIndex: Integer): Single;
function  GetPumpCount: Integer;
function  GetUnitsSystem: Integer;
function  GetResultIndex(Category: Integer; Index: Integer): Integer;
function  GetStatisticsType: Integer;
function  GetHlossModelStr: String;
function  GetDemandModelStr: String;
function  GetQualModelStr: String;
function  GetObjectStr(Category: Integer; Item: Integer): String;

function  GetNodeCoord(NodeIndex: Integer; var X, Y: Double): Boolean;
procedure SetNodeCoord(NodeIndex: Integer; X: Double; Y: Double);
function  GetLinkCoord(LinkIndex: Integer; var X, Y: Double): Boolean;
function  GetLabelCoord(LabelIndex: Integer; var X, Y: Double): Boolean;
procedure SetLabelCoord(LabelIndex: Integer; X: Double; Y: Double);
function  GetVertexCoord(LinkIndex: Integer; Vertex: Integer; var X, Y: Double):
          Boolean;
function  SetVertexCoord(LinkIndex: Integer; Vertex: Integer; X, Y: Double):
          Boolean;
function  GetVertexCount(LinkIndex: Integer): Integer;
procedure SetVertexCoords(LinkIndex: Integer; X: array of Double;
          Y: array of Double; Count: Integer);

procedure SetTitle(Line1: String; Line2: String; Line3: String);
procedure SetItemID(Category: Integer; Index: Integer; ID: String);
procedure SetDefHydOptions(Options: array of string);
procedure GetDefHydOptions(var Options: array of string);

procedure DeleteItem(Category: Integer; Index: Integer);
procedure DeleteLabelAnchors(NodeID: String);
function  CanPasteItem(Item: Integer; CopiedCategory: Integer;
          CopiedType: Integer):Boolean;

function  Load(FileName: string): Integer;
function  Save(FileName: string): Boolean;
procedure UpdateResultsStatus;

implementation

uses
  main, mapcoords, maplabel, results, projectmapdata, utils, epanet2;

procedure Open;
//
// Open the EPANET project
//
begin
  RptFile := SysUtils.GetTempFileName('', 'EN_Rpt_');
  OutFile := SysUtils.GetTempFileName('', 'EN_Out_');
  DmndFile := SysUtils.GetTempFileName('', 'EN_Dmnd_');
  AuxFile := SysUtils.GetTempFileName('', 'EN_Aux_');
  MsxOutFile := SysUtils.GetTempFileName('', 'EN_Msx_');
  MsxHydFile := SysUtils.GetTempFileName('', 'EN_Hyd_');
  Properties := TStringList.Create;
  CopiedProperties := TStringList.Create;
  MapLabels := TStringList.Create;
  Init;
end;

procedure Init;
//
// Initialize the project
//
begin
  InpFile := '';
  MsxInpFile := '';
  Properties.Clear;
  MapLabels.Clear;
  epanet2.ENinit(PAnsiChar(RptFile), PAnsiChar(OutFile), epanet2.EN_GPM,
    epanet2.EN_HW);
  AutoLength := false;
  MapUnits := muNone;
  MapEPSG := 0;
  OldMapUnits := MapUnits;
  OldMapEPSG := MapEPSG;
  HasChanged := false;
  HasResults := false;
  RunStatus := rsNone;
  ResultsStatus := rsNotAvailable;
  OutFileOpened := false;
  MsxFileOpened := false;
  DmndFileOpened := false;
  MsxFlag := false;
  StatusRptType := epanet2.EN_NORMAL_REPORT;
  CopiedProperties.Clear;
end;

procedure Clear;
//
// Clear the project's contents.
//
var
  I: Integer;
begin
  Results.CloseOutFile;
  epanet2.ENclose;
  for I := 0 to MapLabels.Count - 1 do MapLabels.Objects[I].Free;
  MapLabels.Clear;
  InpFile := '';
  HasChanged := false;
  HasResults := false;
end;

procedure Close;
//
// Close the project
//
begin
  // Clear all project data
  Clear;

  // Delete all temporary files
  SysUtils.DeleteFile(OutFile);
  SysUtils.DeleteFile(DmndFile);
  SysUtils.DeleteFile(RptFile);
  SysUtils.DeleteFile(AuxFile);
  SysUtils.DeleteFile(MsxOutFile);
  SysUtils.DeleteFile(MsxHydFile);

  // Free all stringlists
  Properties.Free;
  CopiedProperties.Free;
  MapLabels.Free;
end;

function GetItemCount(Category: Integer): Integer;
//
// Retrieve the number of a particular category of objects.
//
begin
  Result := 0;
  case Category of
    cTitle:
      Result := 3;   //The EPANET API restricts titles to 3 lines.
    cOptions:
      Result := High(OptionsStr)+1;
    cNodes:
      epanet2.ENgetcount(EN_NODECOUNT, Result);
    cLinks:
      epanet2.ENgetcount(EN_LINKCOUNT, Result);
    cControls:
      Result := 2;   //Simple & rule-based controls
    cLabels:
      Result := MapLabels.Count;
    cPatterns:
      epanet2.ENgetcount(EN_PATCOUNT, Result);
    cCurves:
      epanet2.ENgetcount(EN_CURVECOUNT, Result);
  end;
end;

function IsEmpty: Boolean;
begin
  Result := True;
  if (GetItemCount(cNodes) > 0) or (Maplabels.Count > 0) then
    Result := False;;
end;

function GetItemID(Category: Integer; Item: Integer): string;
//
// Retrieve the ID label of a particular object.
//
begin
  Result := '';
  if Item < 0 then Exit;
  case Category of
    cTitle:    Result := GetTitle(Item);
    cOptions:  Result := OptionsStr[Item];
    cControls: Result := ControlsStr[Item];
    cLabels:   Result := MapLabels[Item];
    else       Result := GetID(Category, Item+1);
  end;
end;

function  GetItemIndex(Category: Integer; ID: string): Integer;
//
// Retrieve the index of an object given its ID label.
//
var
  Index: Integer;
  pID: PAnsiChar;
begin
  Index := 0;
  pID := PAnsiChar(ID);
  case Category of
    cNodes:    epanet2.ENgetnodeindex(pID, Index);
    cLinks:    epanet2.ENgetlinkindex(pID, Index);
    cPatterns: epanet2.ENgetpatternindex(pID, Index);
    cCurves:   epanet2.ENgetcurveindex(pID, Index);
    cLabels:   Index := MapLabels.IndexOf(ID);
  end;
  Result := Index;
end;

function  GetID(Category: Integer; Index: Integer): string;
//
// Retrieve the ID label of an object given its index.
//
var
  ID: array[0..EN_MAXID+1] of AnsiChar;
begin
  ID := '';
  case Category of
    cNodes:    epanet2.ENgetnodeid(Index, ID);
    cLinks:    epanet2.ENgetlinkid(Index, ID);
    cPatterns: epanet2.ENgetpatternid(Index, ID);
    cCurves:   epanet2.ENgetcurveid(Index, ID);
  end;
  Result := ID;
end;

function GetIdError(Category: Integer; ID: string): string;
var
  N: Integer;
begin
  Result := '';
  N := Length(ID);
  if (Pos(' ', ID) > 0) or (Pos(';', ID) > 0) then
    Result := 'ID names cannot contain spaces or semi-colons.'
  else if (N > EN_MAXID) or (N = 0) then
    Result := 'ID names cannot be blank or exceed ' + IntToStr(EN_MAXID) +
      ' characters.'
  else if Project.GetItemIndex(Category, ID) > 0 then
    Result := 'ID name already in use.';
end;

function GetTitle(Item: Integer): string;
//
// Retrieve a given line of the project's title.
//
var
  Lines: array[0..2] of String;
  I: Integer;
begin
  for I := 0 to 2 do Lines[I] := StringOfChar(#0, EN_MAXMSG+1);
  epanet2.ENgettitle(PAnsiChar(Lines[0]), PAnsiChar(Lines[1]), PAnsiChar(Lines[2]));
  for I := 0 to 2 do SetLength(Lines[I], Pos(#0, Lines[I]) - 1);
  Result := Lines[Item];
end;

function  GetComment(Category: Integer; Index: Integer): string;
//
// Retrieve the comment string assigned to a particular object.
//
var
  Comment: AnsiString;
begin
  Comment := StringOfChar(#0, EN_MAXMSG+1);
  epanet2.ENgetcomment(Category, Index, PAnsiChar(Comment));
  SetLength(Comment, Pos(#0, Comment) - 1);
  Result := Comment;
end;

function  GetTag(Category: Integer; Index: Integer): string;
//
// Retrieve the tag string assigned to a particular object.
//
var
  Tag: AnsiString;
begin
  Tag := StringOfChar(#0, EN_MAXMSG+1);
  epanet2.ENgettag(Category, Index, PAnsiChar(Tag));
  SetLength(Tag, Pos(#0, Tag) - 1);
  Result := Tag;
end;


function  GetItemTypeStr(Category: Integer; Item: Integer): string;
//
// Retrieve the type name of a particular object.
//
var
  I: Integer = 0;
begin
  Result := '';
  case Category of
    cNodes:
      begin
        epanet2.ENgetnodetype(Item+1, I);
        if I = EN_RESERVOIR then Result := 'Reservoir '
        else if I = EN_TANK then Result := 'Tank '
        else Result := 'Junction ';
      end;
    cLinks:
      begin
        epanet2.ENgetlinktype(Item+1, I);
        if I = EN_PUMP then Result := 'Pump '
        else if I >= EN_PRV then Result := 'Valve '
        else Result := 'Pipe ';
      end;
    cPatterns: Result := 'Pattern ';
    cCurves: Result := 'Curve ';
  end;
end;

function GetNodeType(Index: Integer): Integer;
//
// Retrieve the type code for a particular node object.
//
var
  NodeType: Integer = 0;
begin
  epanet2.ENgetnodetype(Index, NodeType);
  Result := NodeType;
end;

function  GetLinkType(Index: Integer): Integer;
//
// Retrieve the type code for a particular link object.
//
var
  LinkType: Integer = 0;
begin
  epanet2.ENgetlinktype(Index, LinkType);
  if LinkType > lPump then LinkType := lValve;
  Result := LinkType;
end;

function GetLinkNodes(Link: Integer; var Node1, Node2: Integer): Boolean;
//
// Retrieve the indexes of a link's end nodes.
//
begin
  if epanet2.ENgetlinknodes(Link, Node1, Node2) = 0 then Result := true
  else Result := false;
end;

procedure ReverseLinkNodes(Link: Integer);
//
// Reverses a link's start and end nodes (including vertices)
//
var
  I, J, N, Node1, Node2: Integer;
  X, Y: array of Double;
begin
  if epanet2.ENgetlinknodes(Link, Node1, Node2) = 0 then
  begin
    epanet2.ENsetlinknodes(Link, Node2, Node1);
    N := GetVertexCount(Link);
    if N > 0 then
    begin
      SetLength(X, N);
      SetLength(Y, N);
      for I := 1 to N do
      begin
        J := N - I;
        GetVertexCoord(Link, I, X[J], Y[J]);
      end;
      SetVertexCoords(Link, X, Y, N);
      SetLength(X, 0);
      SetLength(Y, 0);
    end;
    HasChanged := True;
  end;
end;

function  GetSourceQual(NodeIndex: Integer): Single;
//
// Retrieve the strength of a node's water quality source.
//
begin
  Result := 0;
  epanet2.ENgetnodevalue(NodeIndex, EN_SOURCEQUAL, Result);
end;

function GetNodeCoord(NodeIndex: Integer; var X, Y: Double): Boolean;
//
// Retrieve a node's coordinates.
//
begin
  if epanet2.ENgetcoord(NodeIndex, X, Y) = 0 then Result := true
  else Result := false;
end;

procedure SetNodeCoord(NodeIndex: Integer; X: Double; Y: Double);
//
// Set the coordinates of a node.
//
begin
  epanet2.ENsetcoord(NodeIndex, X, Y);
end;

function GetLinkCoord(LinkIndex: Integer; var X, Y: Double): Boolean;
//
// Retrieve the coordinates of a link's midpoint.
//
var
  N: Integer;
  N1: Integer = 0;
  N2: Integer = 0;
  X1: Double = 0;
  Y1: Double = 0;
  X2: Double = 0;
  Y2: Double = 0;
begin
  // Link has vertices - use middle vertex
  N := GetVertexCount(LinkIndex);
  if N > 0 then
  begin
    // N is index of middle vertex
    if Odd(N) then N := (N div 2) + 1 else N := N div 2;
    Result := GetVertexCoord(LinkIndex, N, X, Y);
  end else

  // Link has no vertices - use mid-point of end nodes
  begin
    Result := GetLinkNodes(LinkIndex, N1, N2);
    if Result then Result := GetNodeCoord(N1, X1, Y1);
    if Result then Result := GetNodeCoord(N2, X2, Y2);
    if Result then
    begin
      X := (X1 + X2) / 2.;
      Y := (Y1 + Y2) / 2;
    end;
  end;
end;

function  GetLabelCoord(LabelIndex: Integer; var X, Y: Double): Boolean;
//
// Retrieve the coordinates of the top-left point of a map label.
//
var
  MapLabel: TMapLabel;
begin
  Result := true;
  MapLabel := TMapLabel(MapLabels.Objects[LabelIndex-1]);
  if MapLabel = nil then Result := false else
  begin
    X := MapLabel.X;
    Y := MapLabel.Y;
  end;
end;

procedure SetLabelCoord(LabelIndex: Integer; X: Double; Y: Double);
//
// Set the coordinates of the top-left point of a map label.
//
var
  MapLabel: TMapLabel;
begin
  MapLabel := TMapLabel(MapLabels.Objects[LabelIndex-1]);
  if MapLabel <> nil then
  begin
    MapLabel.X := X;
    MapLabel.Y := Y;
  end;
end;

function  GetNodeParam(NodeIndex: Integer; Param: Integer): Single;
//
// Retrieve the value of a specific parameter for a node.
//
begin
  Result := 0;
  epanet2.ENgetnodevalue(NodeIndex, Param, Result);
end;

function  GetLinkParam(LinkIndex: Integer; Param: Integer): Single;
//
// Retrieve the value of a specific parameter for a link.
//
begin
  Result := 0;
  epanet2.ENgetlinkvalue(LinkIndex, Param, Result);
end;

function  GetPumpCount: Integer;
//
// Retrieve the number of pumps in the project.
//
var
  I: Integer;
  LinkType: Integer = 0;
begin
  Result := 0;
  for I := 1 to GetItemCount(cLinks) do
  begin
    epanet2.ENgetlinktype(I, LinkType);
    if LinkType = EN_PUMP then Inc(Result);
  end;
end;

function GetVertexCount(LinkIndex: Integer): Integer;
//
// Retrieve the number of vertices assigned to a link.
//
var
  Count: Integer = 0;
begin
  Result := 0;
  if epanet2.ENgetvertexcount(LinkIndex, Count) = 0 then Result := Count;
  if Result > MAX_VERTICES then Result := MAX_VERTICES;
end;

function GetVertexCoord(LinkIndex: Integer; Vertex: Integer; var X, Y: Double): Boolean;
//
// Retrieve the coordinates of a particular link vertex.
//
begin
  if epanet2.ENgetvertex(LinkIndex, Vertex, X, Y) > 0
  then Result := false
  else Result := true;
end;

function  SetVertexCoord(LinkIndex: Integer; Vertex: Integer; X, Y: Double):
          Boolean;
//
// Set the coordinates of a particular link vertex.
//
begin
  if epanet2.ENsetvertex(LinkIndex, Vertex, X, Y) > 0
  then Result := false
  else Result := true;
  HasChanged := True;
end;

procedure SetVertexCoords(LinkIndex: Integer; X: array of Double;
          Y: array of Double; Count: Integer);
//
// Set the coordinates for all of a link's vertices.
//
begin
  epanet2.ENsetvertices(LinkIndex, X[0], Y[0], Count);
  HasChanged := True;
end;

function GetPatternNames: String;
//
// Retrieve the ID names of all of a project's Time Patterns.
//
var
  I, N: Integer;
begin
  Result := '';
  N := GetItemCount(cPatterns);
  if N > 0 then for I := 1 to N do
    Result := Result + #13 + GetID(cPatterns, I);
end;

function  GetCurveNames(aCurveType: Integer): String;
//
// Retrieve the ID names of all of a project's Data Curves.
//
var
  I, CurveType, N: Integer;
begin
  Result := '';
  N := GetItemCount(cCurves);
  if N > 0 then for I := 1 to N do
  begin
    CurveType := ctGeneric;
    ENgetcurvetype(I, CurveType);
    if (CurveType = aCurveType) or (CurveType = ctGeneric) then
      Result := Result + #13 + GetID(cCurves, I);
  end;
end;

function GetCurveType(CurveIndex: Integer): Integer;
//
// Retrieve the type code of a particular Data Curve.
//
var
  CurveType : Integer = ctGeneric;
begin
  if ENgetcurvetype(CurveIndex, CurveType) = 0
  then Result := curveType
  else Result := ctGeneric;
end;

function GetUnitsSystem: Integer;
//
// Retrieve the project's unit system.
//
var
  I: Integer = 0;
begin
  epanet2.ENgetflowunits(I);
  if I >= EN_LPS then
    Result := usSI
  else
    Result := usUS;
end;

procedure SetFlowUnits(Units: String);
//
// Set project's flow units
//
var
  I: Integer;
begin
  I := AnsiIndexText(Units, FlowUnitsStr);
  if (I < 0) or (I = FlowUnits) then exit;
  ENsetflowunits(I);
  MainForm.UpdateStatusBar(sbFlowUnits, Units);
  HasChanged := True;
end;

procedure SetHeadLossModel(ModelIndex: Integer);
//
// Set project's choice of head loss formula
//
var
  OldIndex: Single;
begin
  if (ModelIndex < Low(HlossModelStr)) or
     (ModelIndex > High(HlossModelStr)) then exit;
  ENgetoption(EN_HEADLOSSFORM, OldIndex);
  if Round(OldIndex) = ModelIndex then exit;
  ENsetoption(EN_HEADLOSSFORM, ModelIndex);
  MainForm.UpdateStatusBar(sbHeadLoss, HlossModelStr[ModelIndex]);
  HasChanged := True;
end;

procedure SetDemandModel(ModelIndex: Integer);
//
// Set project's choice of demand model
//
var
  OldIndex: Integer = 0;
  Pmin: Single = 0;
  Preq: Single = 0;
  Pexp: Single = 0;
begin
  if (ModelIndex < Low(DemandModelStr)) or
     (ModelIndex > High(DemandModelStr)) then exit;
  ENgetdemandmodel(OldIndex, Pmin, Preq, Pexp);
  if OldIndex = ModelIndex then exit;
  ENsetdemandmodel(ModelIndex, Pmin, Preq, Pexp);
  MainForm.UpdateStatusBar(sbDemands, DemandModelStr[ModelIndex]);
  HasChanged := True;
end;

procedure DeleteItem(Category: Integer; Index: Integer);
//
// Remove a particular object from the project.
//
var
  R: Integer;
begin
  case Category of
    cNodes:
      begin
        DeleteLabelAnchors(GetID(cNodes, Index));
        R := epanet2.ENdeletenode(Index, EN_UNCONDITIONAL);
        if R > 0 then
        begin
          showmessage('Node could not be deleted.');
        end;
      end;
    cLinks:    epanet2.ENdeletelink(Index, EN_UNCONDITIONAL);
    cPatterns: epanet2.ENdeletepattern(Index);
    cCurves:   epanet2.ENdeletecurve(Index);
    cLabels:
      begin
        MapLabels.Objects[Index-1].Free;
        MapLabels.Delete(Index-1);
      end;
  end;
  HasChanged := True;
  UpdateResultsStatus;
end;

procedure DeleteLabelAnchors(NodeID: String);
//
// Remove a specific node from being the anchor node for a label.
//
var
  I: Integer;
  MapLabel: TMapLabel;
begin
  for I := 0 to MapLabels.Count - 1 do
  begin
    MapLabel := TMapLabel(MapLabels.Objects[I]);
    if MapLabel.AnchorNode = NodeID then MapLabel.AnchorNode := '';
  end;
end;

function CanPasteItem(Item: Integer; CopiedCategory: Integer;
  CopiedType: Integer):Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if (CopiedCategory = cNodes) and (CopiedType = GetNodeType(Item+1)) then
    Result := True
  else if (CopiedCategory = cLinks) then
  begin
    if CopiedType = lValve then
    begin
      I := EN_PRV + AnsiIndexText(CopiedProperties[7], ValveTypeStr);
      ENgetLinkType(Item+1, J);
      if I = J then
        Result := True;
    end
    else if CopiedType = GetLinkType(Item+1) then
      Result := True
  end;
end;

function Load(FileName: string): Integer;
//
// Load the data from an EPANET-formatted file into the project.
//
var
  Extent: TDoubleRect;
begin
  // Clear any existing project
  Clear;

  // Use EPANET API to read project data from input file
  Result := epanet2.ENopenX(PAnsiChar(FileName), PAnsiChar(RptFile),
    PAnsiChar(OutFile));

  // Input file was read successfully
  if (Result = 0) or (Result = 200) then
  begin
    InpFile := FileName;
    epanet2.ENgetflowunits(FlowUnits);

    // Read map-related data in input file not read by ENopenX
    projectmapdata.ReadMapData(InpFile);

    // Find the coordinates of the rectangle that contains all network objects
    Extent := MapCoords.GetBounds(MainForm.MapFrame.GetExtent);
    MainForm.MapFrame.SetExtent(Extent);
  end

  // Input file failed to be read -- start a new empty project
  else Init;
  epanet2.ENcopyreport(PAnsiChar(AuxFile));
end;

function Save(FileName: string): Boolean;
//
// Save the project's data to an EPANET-formatted file.
//
var
  ErrCode: Integer;
begin
  ErrCode := epanet2.ENsaveinpfile(PAnsiChar(FileName));
  Result := (ErrCode = 0);
  if Result then
  begin
    projectmapdata.SaveMapData(FileName);
    HasChanged := False;
  end;
end;

procedure SetTitle(Line1: String; Line2: String; Line3: String);
//
// Set the project's title lines.
//
begin
  epanet2.ENsettitle(PAnsiChar(Line1), PAnsiChar(Line2), PAnsiChar(Line3));
end;

function  GetResultIndex(Category: Integer; Index: Integer): Integer;
//
// Get the position in which the results for a given node or link was
// written to the output file.
//
begin
  Result := 0;
  if Category = cNodes then
    epanet2.ENgetresultindex(EN_NODE, Index, Result)
  else if Category = cLinks then
    epanet2.ENgetresultindex(EN_LINK, Index, Result)
end;

procedure SetItemID(Category: Integer; Index: Integer; ID: String);
//
// Set the ID label for a given object.
//
var
  pID: PAnsiChar;
begin
  pID := PAnsiChar(ID);
  case Category of
    cNodes:    epanet2.ENsetnodeid(Index, pID);
    cLinks:    epanet2.ENsetlinkid(Index, pID);
    cPatterns: epanet2.ENsetpatternid(Index, pID);
    cCurves:   epanet2.ENsetcurveid(Index, pID);
    cLabels:   MapLabels[Index] := ID;
  end;
end;

procedure GetDefHydOptions(var Options: array of string);
//
//  Retrieve the text of hydraulic options that have default values.
//
var
  I: Integer = 0;
  X: Single = 0;
  DemandModel: Integer;
  Pmin, Preq, Pexp: Single;
begin
  epanet2.ENgetflowunits(I);
  Options[0] := FlowUnitsStr[I];
  epanet2.ENgetoption(EN_HEADLOSSFORM, X);
  Options[1] := HLossModelStr[Round(X)];
  epanet2.ENgetdemandmodel(DemandModel, Pmin, Preq, Pexp);
  Options[2] := Utils.Float2Str(Preq, 3);
  epanet2.ENgetoption(EN_TRIALS, X);
  Options[3] := IntToStr(Round(X));
  epanet2.ENgetoption(EN_ACCURACY, X);
  Options[4] := Utils.Float2Str(X, 8);
  epanet2.ENgetoption(EN_HEADERROR, X);
  Options[5] := Utils.Float2Str(X, 8);
  epanet2.ENgetoption(EN_FLOWCHANGE, X);
  Options[6] := Utils.Float2Str(X, 8);
end;

procedure SetDefHydOptions(Options: array of string);
//
//  Assign values to default hydraulic options.
//
var
  I: Integer;
  P: Single;
begin
  I := AnsiIndexText(Options[0], FlowUnitsStr);
  if I >= 0 then
  begin
    ENsetflowunits(I);
    FlowUnits := I;
    MainForm.UpdateStatusBar(sbFlowUnits, Options[0]);
  end;
  I := AnsiIndexText(Options[1], HlossModelStr);
  if I >= 0 then
  begin
    ENsetoption(EN_HEADLOSSFORM, I);
    MainForm.UpdateStatusBar(sbHeadLoss, Options[1]);
  end;
  Utils.Str2Float(Options[2], P);
  ENsetdemandmodel(EN_DDA, 0, P, 0.5);
  ENsetoption(EN_TRIALS, StrToFloat(Options[3]));
  ENsetoption(EN_ACCURACY, StrToFloat(Options[4]));
  ENsetoption(EN_HEADERROR, StrToFloat(Options[5]));
  ENsetoption(EN_FLOWCHANGE, StrToFloat(Options[6]));
end;

function  GetHlossModelStr: String;
//
//  Get the name of the project's head loss function.
//
var
  Value: Single = 0;
begin
  ENgetoption(EN_HEADLOSSFORM, Value);
  Result := HlossModelStr[Round(Value)];
end;

function  GetDemandModelStr: String;
//
//  Get the name of the project's demand model.
//
var
  I: Integer;
  Pmin: Single = 0;
  Preq: Single = 0;
  Pexp: Single = 0;
begin
  ENgetdemandmodel(I, Pmin, Preq, Pexp);
  Result := DemandModelStr[I];
end;

function  GetQualModelStr: String;
//
//  Get the name of the project's type of water quality model.
//
var
  QualCode: Integer = 0;
  ChemName: array[0..EN_MAXID] of AnsiChar;
  QualUnits: array[0..EN_MAXID] of AnsiChar;
  TraceNodeIndex: Integer;
begin
  if Length(MsxInpFile) > 0 then Result := 'Multi-Species'
  else
  begin
    Epanet2.ENgetqualinfo(QualCode, ChemName, QualUnits, TraceNodeIndex);
    Result := QualModelStr[qualCode];
    if (QualCode = qtChem) and (Length(ChemName) > 0) then Result := ChemName;
  end;
end;

function  GetObjectStr(Category: Integer; Item: Integer): String;
//
//  Get a string with an object's type and ID name
//
begin
  Result := GetItemTypeStr(Category, Item) + GetItemID(Category, Item);
end;

function MapDistance(X1, Y1, X2, Y2: Double): Single;
//
//  Compute the distance between a pair of map coordinates.
//
var
  Distance, Dx, Dy: Double;
begin
  if MapUnits = muDegrees then
    Distance := Utils.Haversine(X1, Y1, X2, Y2)
  else
  begin
    Dx := X2 - X1;
    Dy := Y2 - Y1;
    Distance := Sqrt(Dx*Dx + Dy*Dy);
  end;
  Result := Single(Distance);
end;

function  FindLinkLength(LinkIndex: Integer): Single;
//
//  Find the length of a link.
//
var
  J, Node1, Node2, Units: Integer;
  X1, Y1, X2, Y2: Double;
begin
  // Length is 0 for non-pipe links
  Result := 0;
  if GetLinkType(LinkIndex) > lPipe then exit;

  // Get coordinates of pipe's start node
  if epanet2.ENgetlinknodes(LinkIndex, Node1, Node2) <> 0 then exit;
  if not GetNodeCoord(Node1, X1, Y1) then exit;

  // Add length between each vertex to total length
  for J := 1 to GetVertexCount(LinkIndex) do
  begin
    if not GetVertexCoord(LinkIndex, J, X2, Y2) then continue;
    Result := Result + MapDistance(X1, Y1, X2, Y2);
    X1 := X2;
    Y1 := Y2;
  end;

  // Add length to pipe's end node to total length
  if GetNodeCoord(Node2, X2, Y2) then
    Result := Result + MapDistance(X1, Y1, X2, Y2);

  // Apply proper unit conversion to result
  Units := GetUnitsSystem;
  if (MapUnits = muFeet) and (Units = usSI) then
    Result := Result * 0.3048
  else if (MapUnits = muMeters) and (Units = usUS) then
    Result := Result / 0.3048
  else if (MapUnits = muDegrees) and (Units = usUS) then
    Result := Result / 0.3048;
end;

procedure AdjustLinkLengths(NodeIndex: Integer);
var
  I, Node1, Node2: Integer;
  Length: Single;
begin
  if not AutoLength then exit;
  for I := 1 to GetItemCount(cLinks) do
  begin
    if GetLinkType(I) > lPipe then continue;
    if epanet2.ENgetlinknodes(I, Node1, Node2) <> 0 then continue;
    if (Node1 = NodeIndex) or (Node2 = NodeIndex) then
    begin
      Length := FindLinkLength(I);
      if Length > 0 then ENsetlinkvalue(I, EN_LENGTH, Length);
    end;
  end;
end;

procedure SetPipeLength(PipeIndex: Integer);
var
  NewLength: Single;
begin
  NewLength := FindLinkLength(PipeIndex);
  ENsetlinkvalue(PipeIndex, EN_LENGTH, NewLength);
  with MainForm.ProjectFrame.PropEditor do
  begin
    EditorMode := False;
    Cells[1,6] := Float2Str(NewLength, 4);
    EditorMode := True;
  end;
end;

function  GetStatisticsType: Integer;
var
  T: TimeType;
begin
  ENgettimeparam(EN_STATISTIC, T);
  Result := Integer(T);
end;

procedure UpdateResultsStatus;
begin
  if (ResultsStatus = rsUpToDate) and HasChanged then
  begin
    ResultsStatus := rsNeedsUpdating;
    MainForm.UpdateStatusBar(sbResults, 'Results Need Updating');
  end;
end;
end.


