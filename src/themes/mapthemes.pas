{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       mapthemes
 Description:  Manages the display of node and link themes on
               the pipe network map
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit mapthemes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Graphics, Dialogs, Controls, Math;

const

  // Node themes
  ntElevation  = 1;
  ntBaseDemand = 2;
  ntDemand     = 3;
  ntDmndDfct   = 4;
  ntEmittance  = 5;
  ntLeakage    = 6;
  ntHead       = 7;
  ntPressure   = 8;
  FirstNodeResultTheme = 3;
  FirstNodeQualTheme = 9;

  // Link Themes
  ltDiameter   = 1;
  ltLength     = 2;
  ltRoughness  = 3;
  ltFlow       = 4;
  ltVelocity   = 5;
  ltHeadloss   = 6;
  ltLeakage    = 7;
  FirstLinkResultTheme = 4;
  FirstLinkQualTheme = 8;

  MISSING = -1.E10;   //Missing value
  MAXLEVELS = 4;      //Number of color-coded levels

type
  TMapTheme = record
    Name        : string;                 //Theme name
    SourceIndex : Integer;                //Index used by data source
    DefIntervals: array[1..MAXLEVELS] of string;  //Default display intervals
end;

  TLegendIntervals = record
    Labels: array[1..MAXLEVELS] of string;
    Values: array[1..MAXLEVELS] of Single;
  end;

const
  BaseNodeThemes: array[0..ntPressure] of TMapTheme =
  (
    (Name:'None';
     SourceIndex:-1;
     DefIntervals:('','','','')),

    (Name:'Elevation';
     SourceIndex:0;    // = EN_ELEVATION
     DefIntervals:('25','50','75','100')),

    (Name:'Base Demand';
     SourceIndex:1;    // = EN_BASEDEMAND
     DefIntervals:('25','50','75','100')),

    (Name:'Actual Demand';
     SourceIndex:0;
     DefIntervals:('25','50','75','100')),

     (Name:'Demand Deficit';
      SourceIndex:100;
      DefIntervals:('25','50','75','100')),

      (Name:'Emitter Demand';
       SourceIndex:100;
       DefIntervals:('5','10','20','50')),

     (Name:'Leakage Demand';
      SourceIndex:100;
      DefIntervals:('5','10','20','50')),

    (Name:'Hydraulic Head';
     SourceIndex:1;
     DefIntervals:('25','50','75','100')),

    (Name:'Pressure';
     SourceIndex:2;
     DefIntervals:('25','50','75','100'))
  );

  BaseLinkThemes: array[0..7] of TMapTheme =
  (
    (Name:'None';
     SourceIndex:-1;
     DefIntervals:('','','','')),

    (Name:'Diameter';
     SourceIndex:0;    // = EN_DIAMETER
     DefIntervals:('6','12','24','36')),

    (Name:'Length';
     SourceIndex:1;    // = EN_LENGTH
     DefIntervals:('100','500','1000','5000')),

    (Name:'Roughness';
     SourceIndex:2;    // = EN_ROUGHNESS
     DefIntervals:('50','75','100','125')),

    (Name:'Flow';
     SourceIndex:0;
     DefIntervals:('25','50','75','100')),

    (Name:'Velocity';
     SourceIndex:1;
     DefIntervals:('0.01','0.1','1.0','2.0')),

    (Name:'Unit Head Loss';
     SourceIndex:2;
     DefIntervals:('0.025','0.05','0.075','0.1')),

    (Name:'Leakage';
     SourceIndex:200;
     DefIntervals:('5','10','20','50'))
  );

  DefQualIntervalLabels:  array[1..MAXLEVELS] of String =
    ('1', '10', '50', '80');
  DefQualIntervalValues: array[1..MAXLEVELS] of Single =
    (1, 10, 50, 80);

  DefLegendColors: array[0..MAXLEVELS] of TColor =  //order is BB GG RR
    ($00BE9270, $00EAD999, $001DE6B5, $000EC9FF, $00277FFF);
//    ($00540144, $008b513b, $008d9021, $0063c85c, $0025e7fd); //Viridis Scale

var
  LinkColors: array[0..MAXLEVELS] of TColor;
  LinkIntervals: array of TLegendIntervals;
  LinkTheme: Integer;
  LinkThemes: array of TMapTheme;
  LinkThemeCount: Integer;
  NodeColors: array[0..MAXLEVELS] of TColor;
  NodeIntervals: array of TLegendIntervals;
  NodeTheme: Integer;
  NodeThemes: array of TMapTheme;
  NodeThemeCount: Integer;
  QualThemeCount: Integer;
  QualThemeUnits: String;
  TimePeriod: Integer;

procedure ChangeTimePeriod(NewTimePeriod: Integer);
procedure ChangeTheme(ThemeViewer: TTreeView; ThemeType: Integer; NewTheme: Integer);
function  EditNodeLegend: Boolean;
function  EditLinkLegend: Boolean;
function  GetCurrentThemeValue(ObjType: Integer; ObjIndex: Integer): Single;
function  GetFlowDir(LinkIndex: Integer): Integer;
function  GetLinkColor(LinkIndex: Integer; var ColorIndex: Integer): TColor;
function  GetLinkValue(LinkIndex: Integer; aTheme: Integer;
          aTimePeriod: Integer): Single;
function  GetMinMaxValues(ObjType: Integer; var Vmin: Double; var Vmax: Double): Boolean;
function  GetNodeColor(NodeIndex: Integer; var ColorIndex: Integer): TColor;
function  GetNodeValue(NodeIndex: Integer; aTheme: Integer;  aTimePeriod: Integer): Single;
function  GetThemeUnits(ThemeType: Integer; aTheme: Integer): String;
procedure InitColors;
procedure InitThemes(ThemeViewer: TTreeView);
procedure ResetThemes;
procedure SetBaseMapVisible(IsVisible: Boolean);
procedure SetInitialTheme(ThemeType: Integer; aTheme: Integer);
procedure UpdateLegend(StartNode: TTreeNode; ThemeType: Integer);

implementation

uses
  main, project, themelegend, results, utils;

// Retrieve numerical theme value of currently selected object
function GetCurrentThemeValue(ObjType: Integer; ObjIndex: Integer): Single;
begin
  if ObjType = cNodes then
    Result := GetNodeValue(ObjIndex, NodeTheme, TimePeriod)
  else if ObjType = cLinks then
    Result := GetLinkValue(ObjIndex, LinkTheme, TimePeriod)
  else Result := MISSING;
end;

// Retrieve numerical theme value of a particular node
function  GetNodeValue(NodeIndex: Integer; aTheme: Integer;
  aTimePeriod: Integer): Single;
var
  ParamIndex, ResultIndex: Integer;
begin
  if aTheme <= 0 then
    Result := MISSING
  else begin
    if aTheme < FirstNodeResultTheme then
    begin
      ParamIndex := NodeThemes[aTheme].SourceIndex;
      Result := Project.GetNodeParam(NodeIndex, ParamIndex)
    end

    else if not Project.HasResults then
      Result := MISSING

    else begin
      ParamIndex := NodeThemes[aTheme].SourceIndex;
      ResultIndex := Project.GetResultIndex(cNodes, NodeIndex);

      if ResultIndex < 1 then
        Result := MISSING

      else begin
        if MsxFileOpened and (aTheme >= FirstNodeQualTheme) then
          Result := Results.GetNodeMsxValue(ResultIndex, ParamIndex, aTimePeriod)

        else if (aTheme = ntDmndDfct) then
        begin
          if DmndFileOpened then
            Result := Results.GetDmndDfctValue(ResultIndex, aTimePeriod)
          else
            Result := MISSING
        end

        else if (aTheme = ntEmittance) then
        begin
          if DmndFileOpened then
            Result := Results.GetEmitterFlowValue(ResultIndex, aTimePeriod)
          else
            Result := MISSING
        end

        else if (aTheme = ntLeakage) then
        begin
          if DmndFileOpened then
            Result := Results.GetNodeLeakageValue(ResultIndex, aTimePeriod)
          else
            Result := MISSING
        end

        else if OutFileOpened then
        begin
          Result := Results.GetNodeValue(ResultIndex, ParamIndex, aTimePeriod);
        end

        else
          Result := MISSING;
      end;
    end;
  end;
end;

// Retrieve numerical theme value of a particular link
function  GetLinkValue(LinkIndex: Integer; aTheme: Integer;
  aTimePeriod: Integer): Single;
var
  ParamIndex, ResultIndex: Integer;
begin
  if aTheme <= 0 then
    Result := MISSING

  else begin
    // Theme is a design parameter (diameter, length, etc.)
    if aTheme < FirstLinkResultTheme then
    begin
      ParamIndex := LinkThemes[aTheme].SourceIndex;
      Result := Project.GetLinkParam(LinkIndex, ParamIndex);
    end

    // Theme is a simulation result but no results exist
    else if not Project.HasResults then
      Result := MISSING

    else begin

      // Get the index of the theme with respect to its
      // position in the binary output file EPANET generates
      ParamIndex := LinkThemes[aTheme].SourceIndex;
      ResultIndex := Project.GetResultIndex(cLinks, LinkIndex);

      if ResultIndex < 1 then
        Result := MISSING
      else begin

        // Theme is a multi-species water quality variable
        if MsxFileOpened and (aTheme >= FirstLinkQualTheme) then
          Result := Results.GetLinkMsxValue(ResultIndex, ParamIndex, aTimePeriod)

        // Theme is leakage rate which is saved to a separate auxilary
        // output file
        else if aTheme = ltLeakage then
        begin
          if DmndFileOpened then
            Result := Results.GetLinkLeakageValue(ResultIndex, aTimePeriod)
          else
            Result := MISSING;
        end

        // Theme is one saved to EPANET's binary output file
        else if OutFileOpened then
        begin
          // The single species water quality results are the 4th variable
          // (index of 3) saved to the output file
          if aTheme = FirstLinkQualTheme then ParamIndex := 3;
          Result := Results.GetLinkValue(ResultIndex, ParamIndex, aTimePeriod);
        end

        else
          Result := MISSING;
      end;
    end;
  end;
end;

// Find flow direction (+1/-1) of a particular link
function GetFlowDir(LinkIndex: Integer): Integer;
var
  FlowIndex, ResultIndex: Integer;
begin
  Result := 1;
  if Project.HasResults then
  begin
    FlowIndex := LinkThemes[ltFlow].SourceIndex;
    ResultIndex := Project.GetResultIndex(cLinks, LinkIndex);
    if (ResultIndex >= 1) and
       (Results.GetLinkValue(ResultIndex, FlowIndex, TimePeriod) < 0) then
       Result := -1;
  end;
end;

// Initialize the LayersTreeView used to display map legends in the main window.
procedure InitThemes(ThemeViewer: TTreeView);
var
  TreeNode: TTreeNode;
  I, J: Integer;
  S: string;
begin
  // Initialize themes and time period
  NodeTheme := 0;
  LinkTheme := 0;
  QualThemeCount := 0;
  TimePeriod := 0;

  // Set StateIndex of all nodes in the ThemeViewer TreeView to 1 (i.e., checked)
  for I := 0 to ThemeViewer.Items.Count-1 do
  begin
    TreeNode := Themeviewer.Items[I];
    if TreeNode.StateIndex = 0 then TreeNode.StateIndex := 1;
  end;

  // Hide the node & link themes tree nodes
  TreeNode := Utils.FindTreeNode(ThemeViewer, 'Nodes').GetNext;
  TreeNode.Visible := false;
  TreeNode := Utils.FindTreeNode(ThemeViewer, 'Links').GetNext;
  TreeNode.Visible := false;

  // Hide the basemap tree node
  TreeNode := Utils.FindTreeNode(ThemeViewer, 'Basemap');
  TreeNode.Visible := false;

  // Load base node themes
  NodeThemeCount := ntPressure + 1;
  SetLength(NodeThemes, NodeThemeCount);
  for I := 0 to NodeThemeCount-1 do
    NodeThemes[I] := BaseNodeThemes[I];

  // Assign default legend intervals for node themes
  SetLength(NodeIntervals, NodeThemeCount);
  for I := 1 to NodeThemeCount-1 do
  begin
    for J := 1 to MAXLEVELS do
    begin
      S := NodeThemes[I].DefIntervals[J];
      NodeIntervals[I].Labels[J] := S;
      Utils.Str2Float(S, NodeIntervals[I].Values[J]);
    end;
  end;

  // Load base link themes
  LinkThemeCount := ltLeakage + 1;
  SetLength(LinkThemes, LinkThemeCount);
  for I := 0 to LinkThemeCount-1 do
    LinkThemes[I] := BaseLinkThemes[I];

  // Assign default legend intervals for link themes
  SetLength(LinkIntervals, LinkThemeCount);
  for I := 1 to LinkThemeCount-1 do
  begin
    for J := 1 to MAXLEVELS do
    begin
      S := LinkThemes[I].DefIntervals[J];
      LinkIntervals[I].Labels[J] := S;
      Utils.Str2Float(S, LinkIntervals[I].Values[J]);
    end;
  end;
end;

procedure InitColors;
var
  I: Integer;
begin
  // Assign default colors to legend intervals
  for I := 0 to High(DefLegendColors) do
  begin
    NodeColors[I] := DefLegendColors[I];
    LinkColors[I] := DefLegendColors[I];
  end;
end;

procedure ResetThemes;
var
  I, J,
  OldThemeCount,
  OldQualThemeCount: Integer;
  LastNonQualSourceIndex: Integer;
begin
  if Project.HasResults then
  begin
    OldQualThemeCount := QualThemeCount;
    QualThemeCount := Results.GetQualCount;

    OldThemeCount := NodeThemeCount;
    NodeThemeCount := ntPressure + QualThemeCount + 1;
    if NodeThemeCount <> OldThemeCount then
    begin
      SetLength(NodeThemes, NodeThemeCount);
      SetLength(NodeIntervals, NodeThemeCount);
    end;
    LastNonQualSourceIndex := NodeThemes[FirstNodeQualTheme-1].SourceIndex;
    for I := FirstNodeQualTheme to NodeThemeCount - 1 do
    begin
      J := I - FirstNodeQualTheme;
      if MsxFlag then
        NodeThemes[I].SourceIndex := J
      else
        NodeThemes[I].SourceIndex := LastNonQualSourceIndex + J + 1;
      NodeThemes[I].Name := GetQualName(J);
      if J > OldQualThemeCount - 1 then
      begin
        NodeIntervals[I].Labels := DefQualIntervalLabels;
        NodeIntervals[I].Values := DefQualIntervalValues;
      end;
    end;

    OldThemeCount := LinkThemeCount;
    LinkThemeCount := ltLeakage + QualThemeCount + 1;
    if LinkThemeCount <> OldThemeCount then
    begin
      SetLength(LinkThemes, LinkThemeCount);
      SetLength(LinkIntervals, LinkThemeCount);
    end;
    for I := FirstLinkQualTheme to LinkThemeCount - 1 do
    begin
      J := I - FirstLinkQualTheme;
      if MsxFlag then
        LinkThemes[I].SourceIndex := J
      else
        LinkThemes[I].SourceIndex := I - FirstLinkResultTheme;
      LinkThemes[I].Name := GetQualName(J);
      if J > OldQualThemeCount - 1 then
      begin
        LinkIntervals[I].Labels := DefQualIntervalLabels;
        LinkIntervals[I].Values := DefQualIntervalValues;
      end;
    end;
  end
  else
  begin
    OldThemeCount := NodeThemeCount;
    NodeThemeCount := ntPressure + 1;
    if NodeThemeCount <> OldThemeCount then
    begin
      SetLength(NodeThemes, NodeThemeCount);
      SetLength(NodeIntervals, NodeThemeCount);
    end;
    OldThemeCount := LinkThemeCount;
    LinkThemeCount := ltLeakage + 1;
    if LinkThemeCount <> OldThemeCount then
    begin
      SetLength(LinkThemes, LinkThemeCount);
      SetLength(LinkIntervals, LinkThemeCount);
    end;
  end;

  MainForm.MainMenuFrame.ResetMapThemes;
  MainForm.MapFrame.RedrawMap;
end;

// Change a network map legend when a theme changes.
procedure ChangeTheme(ThemeViewer: TTreeView; ThemeType: Integer; NewTheme: Integer);
var
  TreeNode: TTreeNode;
  CategoryName: string;
  ThemeName: string;
  ThemeUnits: string;
begin
  // Select theme's parameters
  if ThemeType = cNodes then
  begin
    CategoryName := 'Nodes';
    ThemeName := NodeThemes[NewTheme].Name;
    NodeTheme := NewTheme;
  end
  else if ThemeType = cLinks then
  begin
    CategoryName := 'Links';
    ThemeName := LinkThemes[NewTheme].Name;
    LinkTheme := NewTheme;
  end
  else
    exit;

  // Find the LayersTreeView node that contains the theme's name
  TreeNode := Utils.FindTreeNode(ThemeViewer, CategoryName);
  if TreeNode <> nil then
  begin
    TreeNode := TreeNode.GetNext;

    // Change the theme name
    if TreeNode <> nil then
    begin
      ThemeUnits := GetThemeUnits(ThemeType, NewTheme);
      if Length(ThemeUnits) > 0 then ThemeUnits := ' (' + ThemeUnits + ')';
      TreeNode.Text := ThemeName + ThemeUnits;
      TreeNode.Visible := (NewTheme > 0);
    end;
  end;

  // Update the theme's legend
  if NewTheme > 0 then UpdateLegend(TreeNode, ThemeType);
end;

// Turn basemap layer on/off
procedure SetBaseMapVisible(IsVisible: Boolean);
var
  TreeNode: TTreeNode;
begin
  TreeNode := Utils.FindTreeNode(MainForm.LegendTreeView, 'Basemap');
  TreeNode.Visible := IsVisible;
  if IsVisible then
    TreeNode.StateIndex := 1
  else
    TreeNode.StateIndex := 0;
end;

// Update the display of a map theme's legend.
procedure UpdateLegend(StartNode: TTreeNode; ThemeType: Integer);
var
  I: Integer;
  TreeNode: TTreeNode;
  S1, S2: string;
  Intervals: TLegendIntervals;
begin
  if ThemeType = cNodes then
    Intervals := NodeIntervals[NodeTheme]
  else if ThemeType = cLinks then
    Intervals := LinkIntervals[LinkTheme]
  else
    exit;
  TreeNode := StartNode;
  S1 := Intervals.Labels[1];
  S2 := S1;
  for I := 0 to MAXLEVELS do
  begin
    TreeNode := TreeNode.GetNext;      // Next legend item
    TreeNode.Visible := true;
    if I = 0 then
      TreeNode.Text := ' < ' + S2
    else if I = MAXLEVELS then
      TreeNode.Text := ' > ' + S1
    else begin
      S2 := Intervals.Labels[I+1];
      if SameText(S1, S2) then
      begin
        TreeNode.Text := '';
        TreeNode.Visible := false;
      end
      else
        TreeNode.Text := ' ' + S1 + ' - ' + S2;
      S1 := S2;
    end;
  end;
end;

procedure UpdateLegendMarkers(LegendType: Integer; Colors: array of TColor);

// Change the colors of the bitmaps in the main form's LegendImageList
// used to display a legend in the form's LayersTreeView.

var
  Marker: TBitmap;
  R: TRect;
  I: Integer;
  Ioffset: Integer;  // Node or Link offset into the LegendImageList
begin
  Marker := TBitmap.Create;
  try
    Marker.PixelFormat := pf32bit;
    MainForm.LegendImageList.GetBitmap(0, Marker);
    Marker.Canvas.Brush.Style := bsSolid;
    R := Rect(0, 0, Marker.Width, Marker.Height);
    if LegendType = cNodes then
      Ioffset := 2
    else
      Ioffset := 7;
    for I := 0 to MAXLEVELS do
    begin
      Marker.Canvas.Brush.Color := Colors[I];
      Marker.Canvas.Rectangle(R);
      MainForm.LegendImageList.Replace(I + Ioffset, Marker, nil);
    end;
  finally
    Marker.Free;
  end;
end;

function EditNodeLegend: Boolean;

// Launch the LegendEditorForm to change the node legend.

begin
  Result := false;
  with TLegendEditorForm.Create(MainForm) do
  try
    LoadData(cNodes, NodeThemes[NodeTheme].Name, NodeColors,
      NodeIntervals[NodeTheme]);
    ShowModal;
    if ModalResult = mrOk then
    begin
      UnloadData(NodeColors, NodeIntervals[NodeTheme]);
      UpdateLegendMarkers(cNodes, NodeColors);
      ChangeTheme(MainForm.LegendTreeView, cNodes, NodeTheme);
      Result := true;
    end;
  finally
    Free;
  end;
end;

function EditLinkLegend: Boolean;

// Launch the LegendEditorForm to change the link legend.

begin
  Result := false;
  with TLegendEditorForm.Create(MainForm) do
  try
    LoadData(cLinks, LinkThemes[LinkTheme].Name, LinkColors,
      LinkIntervals[LinkTheme]);
    ShowModal;
    if ModalResult = mrOk then
    begin
      UnloadData(LinkColors, LinkIntervals[LinkTheme]);
      UpdateLegendMarkers(cLinks, LinkColors);
      ChangeTheme(MainForm.LegendTreeView, cLinks, LinkTheme);
      Result := true;
    end;
  finally
    Free;
  end;
end;

procedure ChangeTimePeriod(NewTimePeriod: Integer);

// Redraw the network map after a new time period is selected.

begin
  TimePeriod := NewTimePeriod;
  if (LinkTheme >= FirstLinkResultTheme) or
     (NodeTheme >= FirstNodeResultTheme) then
  begin
    if MainForm.QueryFrame.Visible then
      MainForm.QueryFrame.UpdateResults
    else
      MainForm.MapFrame.RedrawMap;
  end;
end;

function GetNodeColor(NodeIndex: Integer; var ColorIndex: Integer): TColor;

// Find the legend color associated with a given node's current theme value.

var
  Value: Single;
  K: Integer;
begin
  ColorIndex := 0;
  if MainForm.QueryFrame.Visible then
    Result := MainForm.QueryFrame.GetFilteredNodeColor(NodeIndex)
  else if NodeTheme <= 0 then
    Result := clGray
  else
  begin
    Value := GetCurrentThemeValue(cNodes, NodeIndex);
    if Value = MISSING then
      Result := clGray
    else
    begin
      for K := 1 to MAXLEVELS do
      begin
        if Value < NodeIntervals[NodeTheme].Values[K] then
        begin
          ColorIndex := K-1;
          break;
        end;
        ColorIndex := K;
      end;
      Result := NodeColors[ColorIndex];
    end;
  end;
end;

function GetLinkColor(LinkIndex: Integer; var ColorIndex: Integer): TColor;

// Find the legend color associated with a given link's current theme value.

var
  Value: Single;
  K: Integer;
begin
  ColorIndex := 0;
  if MainForm.QueryFrame.Visible then
    Result := MainForm.QueryFrame.GetFilteredLinkColor(LinkIndex)
  else if LinkTheme <= 0 then
    Result := clGray
  else
  begin
    Value := GetCurrentThemeValue(cLinks, LinkIndex);
    if Value = MISSING then
      Result := clGray
    else
    begin
      Value := Abs(Value);
      for K := 1 to MAXLEVELS do
      begin
        if Value < LinkIntervals[LinkTheme].Values[K] then
        begin
          ColorIndex := K-1;
          break;
        end;
        ColorIndex := K;
      end;
      Result := LinkColors[ColorIndex];
    end;
  end;
end;

function GetMinMaxValues(ObjType: Integer; var Vmin: Double;
  var Vmax: Double): Boolean;

// Find the range of values for an object's current theme.

var
  I, N: Integer;
  V: Double;
begin
  Vmax := -1.e50;
  Vmin := 1.e50;
  Result := false;
  if not Project.HasResults then
  begin
    if (ObjType = cNodes) and (NodeTheme >= FirstNodeResultTheme) then exit;
    if (ObjType = cLinks) and (LinkTheme >= FirstLinkResultTheme) then exit;
  end;
  N := Project.GetItemCount(ObjType);
  if N = 0 then exit;
  for I := 1 to N do
  begin
    V := GetCurrentThemeValue(ObjType, I);
    if V <> MISSING then
    begin
      if (ObjType = cNodes) then
      begin
        if GetNodeType(I) in [nReservoir, nTank] then continue;
        if NodeTheme in [ntBaseDemand, ntDemand] then V := Abs(V);
        if NodeTheme = ntPressure then V := Max(V, 0.01);
      end;
      if (ObjType = cLinks) then
      begin
        if LinkTheme in [ltFlow, ltVelocity, ltHeadloss] then V := Abs(V);
      end;
      Vmax := Max(Vmax, V);
      Vmin := Min(Vmin, V);
    end;
  end;
  if (Vmax < Vmin) then exit;
  Result := true;
end;

procedure SetInitialThemeIntervals(ThemeType: Integer;
  var Intervals: TLegendIntervals);
var
  I: Integer;
  Vmin: Double = 0;
  Vmax: Double = 0;
  Vinterval: Double;
begin
  if GetMinMaxValues(ThemeType, Vmin, Vmax) then
  begin
    Vinterval := (Vmax - Vmin) / (MAXLEVELS + 1);
    Utils.AutoScale(Vmin, Vmax, Vinterval);
    for I := 1 to MAXLEVELS do
    begin
      Intervals.Values[I] := Single(Vmin + I * Vinterval);
      Intervals.Labels[I] := FloatToStr(Intervals.Values[I]);
    end;
  end;
end;

procedure SetInitialTheme(ThemeType: Integer; aTheme: Integer);
var
  Intervals: TLegendIntervals;
begin
  if ThemeType = cNodes then
  begin
    NodeTheme := aTheme;
    Intervals := NodeIntervals[NodeTheme]
  end
  else if ThemeType = cLinks then
  begin
    LinkTheme := aTheme;
    Intervals := LinkIntervals[LinkTheme]
  end
  else exit;
  SetInitialThemeIntervals(ThemeType, Intervals);
  if ThemeType = cNodes then
    NodeIntervals[aTheme] := Intervals;
  if ThemeType = cLinks then
    LinkIntervals[aTheme] := Intervals;
  ChangeTheme(MainForm.LegendTreeView, ThemeType, aTheme);
end;

function  GetThemeUnits(ThemeType: Integer; aTheme: Integer): String;
begin
  Result := '';
  if ThemeType = cNodes then case aTheme of
    ntElevation,
    ntHead:
      if Project.GetUnitsSystem = usUS then Result := 'ft'
      else Result := 'm';
    ntBaseDemand,
    ntDemand:
      Result := Project.FlowUnitsStr[Project.FlowUnits];
    ntDmndDfct:
      Result := '%';
    ntEmittance, ntLeakage:
      Result := Project.FlowUnitsStr[Project.FlowUnits];
    ntPressure:
      if Project.GetUnitsSystem = usUS then Result := 'psi'
      else Result := 'm';
    else Result := results.GetQualUnits(aTheme - ntPressure - 1);
  end

  else if ThemeType = cLinks then case aTheme of
    ltDiameter:
      if Project.GetUnitsSystem = usUS then Result := 'in'
      else Result := 'mm';
    ltLength:
      if Project.GetUnitsSystem = usUS then Result := 'ft'
      else Result := 'm';
    ltRoughness:
      Result := '';
    ltFlow:
      Result := Project.FlowUnitsStr[Project.FlowUnits];
    ltVelocity:
      if Project.GetUnitsSystem = usUS then Result := 'ft/s'
      else Result := 'm/s';
    ltHeadloss:
      if Project.GetUnitsSystem = usUS then Result := 'ft/Kft'
      else Result := 'm/km';
    ltLeakage:
      Result := Project.FlowUnitsStr[Project.FlowUnits];
    else
      Result := results.GetQualUnits(aTheme - FirstLinkQualTheme);
  end;
end;

end.
