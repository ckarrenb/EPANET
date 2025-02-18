{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       mapframe
 Description:  a frame that displays the pipe network and
               handles user interaction with it
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}
unit mapframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, LCLtype,
  Graphics, Clipbrd, ExtDlgs, Types, Math, Dialogs,
  project, map, maphiliter, mapcoords, mapoptions;

const
  TICKDELAY    = 100;        //Delay before object can be moved

type

  TMapAction = (maSelecting = 1, maVertexing, maFenceLining, maPanning,
                maZooming, maDrawExtent, maAddingJunc, maAddingResv,
                maAddingTank, maAddingPipe, maAddingPump, maAddingValve,
                maAddingLabel);

  TCtrlPoint = Record
    Bitmap: TBitmap;
    Position: TDoublePoint;
    Visible: Boolean;
  end;

  { TMapFrame }

  TMapFrame = class(TFrame)
    MapBox: TPaintBox;
    HiliteTimer: TTimer;
    OpenPictureDialog1: TOpenPictureDialog;
    ResizeTimer: TTimer;
    procedure HiliteTimerTimer(Sender: TObject);
    procedure MapBoxChangeBounds(Sender: TObject);
    procedure MapBoxClick(Sender: TObject);
    procedure MapBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MapBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapBoxMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure MapBoxMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure MapBoxPaint(Sender: TObject);
    procedure MapBoxResize(Sender: TObject);
    procedure ResizeTimerTimer(Sender: TObject);
    procedure ShowHint(Sender: TObject; HintInfo:PHintInfo);

  private
    Hiliter: THiliter;
    Linking: Boolean;
    FenceLining: Boolean;
    Moving: Boolean;
    Node1: Integer;
    Label1: Integer;
    Point1, Point2: TPoint;
    Offset: TPoint;
    Vertices: array[0..project.MAX_VERTICES] of TPoint;
    NumVertices: Integer;
    SelectedVertex: Integer;
    SelectedObjType: Integer;
    SelectedObjIndex: Integer;
    OldTickCount: QWORD;        //Used to measure a small time delay

    procedure BeginFenceLining(X: Integer; Y: Integer);
    procedure EnableMainForm(State: Boolean);
    procedure EndLinking(const X: Integer; const Y: Integer);
    procedure MoveVertex(X: Integer; Y: Integer);
    function  SelectVertex(X: Integer; Y: Integer): Boolean;
    procedure ShowJunctions;
    procedure ShowVertex(I: Integer; J: Integer; C: TColor);
    procedure ShowVertices(I: Integer);
    function  StartLinking(const X: Integer; const Y: Integer): Boolean;
    procedure DrawCtrlPoints;

  public
    Map: TMap;
    MapAction: TMapAction;
    HasBaseMap: Boolean;
    BaseMapFile: String;
    CtrlPoint: array[1..3] of TCtrlPoint;

    procedure AddNode(NodeType: Integer);
    procedure AddLink(LinkType: Integer);
    procedure AddLabel;
    procedure AddVertex;

    procedure ChangeExtent(NewExtent: TDoubleRect);
    procedure ChangeMapLayer(MapLegend: TTreeView);
    procedure Clear;
    procedure Close;
    procedure CopyMap(FileName: String; IncludeLegend: Boolean);

    procedure DeleteVertex;
    procedure DeleteAllVertices;
    procedure DrawFullExtent;

    procedure EditMapOptions;
    procedure EnterSelectionMode;
    procedure EnterVertexingMode;
    procedure EnterFenceLiningMode;
    procedure FindBasemapLocation;

    function  GetBasemapSize: TSize;
    function  GetExtent: TDoubleRect;
    function  GetMapRect: TRect;
    procedure GetVertices(var X: array of Double; var Y: array of Double;
              var N: Integer);
    function  GetWebBasemapSource: Integer;
    procedure GoKeyDown(var Key: Word);

    function  HasWebBasemap: Boolean;
    procedure HiliteObject(const Objtype: Integer; const ObjIndex: Integer);
    procedure Init;
    procedure InitMapOptions;

    procedure LeaveFenceLiningMode;
    procedure LeaveVertexingMode;
    procedure LoadBasemapFromFile;
    procedure LoadBasemapFromWeb(MapSource: Integer; EpsgStr:String);

    procedure RedrawMap;
    procedure RedrawMapLabels;
    procedure ResizeMap;

    procedure SetBasemapBrightness(Brightness: Integer);
    procedure SetExtent(E: TDoubleRect);
    procedure ShowWebBasemap;

    procedure UnloadBasemap;
    procedure UnloadBasemapFromWeb;
    procedure ZoomIn(Dx: Integer; Dy: Integer);
    procedure ZoomOut(Dx: Integer; Dy: Integer);

  end;

implementation

{$R *.lfm}

uses
  main, projectbuilder, mapthemes, maplabel, webmapfinder, config, utils;

//------------------------------------------------------------------------------
//  MapFrame procedures
//------------------------------------------------------------------------------

procedure TMapFrame.Init;
var
  I: Integer;
begin
  Map := TMap.Create;
  Hiliter := THiliter.Create(HiliteTimer);
  MapBox.OnShowHint := @ShowHint;
  MapAction := maSelecting;
  Offset := Point(0, 0);
  Linking := false;
  for I := 1 to 3 do
  begin
    CtrlPoint[I].Bitmap := TBitmap.Create;
    MainForm.MarkerImageList.GetBitmap(I-1, CtrlPoint[I].Bitmap);
    CtrlPoint[I].Visible := False;
  end;
end;

procedure TMapFrame.Clear;
var
  I: Integer;
begin
  Hiliter.Hide;
  Map.Reset;
  Offset := Point(0, 0);
  SelectedObjType := -1;
  SelectedObjIndex := -1;
  HasBaseMap := false;
  BaseMapFile := '';
  for I := 1 to 3 do
    CtrlPoint[I].Visible := False;
  MapBox.Refresh;
end;

procedure TMapFrame.Close;
var
  I: Integer;
begin
  for I := 1 to 3 do
    CtrlPoint[I].Bitmap.Free;
  Hiliter.Free;
  Map.Free;
end;

procedure TMapFrame.DrawFullExtent;
begin
  Map.ZoomToExtent;
  RedrawMap;
  HiliteObject(SelectedObjType, SelectedObjIndex);
end;

procedure TMapFrame.ResizeTimerTimer(Sender: TObject);
begin
  ResizeTimer.Enabled := false;
  ResizeMap;
end;

procedure TMapFrame.ResizeMap;
begin
  if Assigned(Map) then
  try
    Map.Resize(Rect(0, 0, MapBox.ClientWidth, MapBox.ClientHeight));
   // Map.Rescale;
   // Map.ZoomToExtent;
   // Map.SetBasemapBounds;
    RedrawMap;
    HiliteObject(SelectedObjType, SelectedObjIndex);
  finally
  end;
end;

procedure TMapFrame.ChangeExtent(NewExtent: TDoubleRect);
var
  S1, S2: TScalingInfo;
begin
  S1 := Map.GetScalingInfo;
  Map.Extent := NewExtent;
  Map.Rescale;
  Map.SetBasemapBounds;
  S2 := Map.GetScalingInfo;
  MapCoords.DoScalingTransform(S1, S2);
  DrawFullExtent;
  project.HasChanged := True;
end;

procedure TMapFrame.HiliteTimerTimer(Sender: TObject);
begin
  if config.MapHiliter then Hiliter.Hilite;
end;

procedure TMapFrame.HiliteObject(const ObjType: Integer; const ObjIndex: Integer);
//  Highlight the currently selected object on the map

var
  RectSize: Integer;
  X: Double = 0;
  Y: Double = 0;
  P: TPoint = (X:0; Y:0);
  R: TRect;
begin
  // Turn off highlighter if no object selected
  if ObjIndex <= 0 then
  begin
    Hiliter.Hide;
    SelectedObjIndex := -1;
    exit;
  end;

  // Get the world coordinates of the selected object
  RectSize := 5;
  if ObjType = cNodes then
  begin
    project.GetNodeCoord(ObjIndex, X, Y);
    RectSize := Max(Map.Options.LinkSize, Map.Options.NodeSize) + 2;
  end
  else if ObjType = cLinks then
  begin
    project.GetLinkCoord(ObjIndex, X, Y);
    RectSize := Map.Options.LinkSize + 4;
  end
  else if ObjType = cLabels then P := Map.FindLabelPoint(ObjIndex)
  else
  begin
    SelectedObjType := -1;
    SelectedObjIndex := -1;
    exit;
  end;

  // Save the selected object's type and index (within the type)
  SelectedObjType := ObjType;
  SelectedObjIndex := ObjIndex;

  // Get the selected object's highlighted rectangle
  if ObjType = cLabels then
    R := TMapLabel(project.MapLabels.Objects[ObjIndex-1]).GetRect(P)
  else begin
    P := Map.WorldToScreen(X, Y);
    R := Rect(P.X - RectSize, P.Y - RectSize, P.X + RectSize, P.Y + RectSize);
  end;

  // Activate the highlighter
  Hiliter.Show(MapBox.Canvas, R);
end;

procedure TMapFrame.ShowHint(Sender: TObject; HintInfo:PHintInfo);
//  Display an object's ID and theme value when the mouse is over it.

var
  I, X, Y: Integer;
  V: Single;
  S: string;
begin
  // Do nothing if hints are turned off
  if config.MapHinting = False then exit;

  // Get mouse's position
  S := '';
  X := HintInfo^.CursorPos.X;
  Y := HintInfo^.CursorPos.Y;

  // Check if mouse is over a node
  I := Map.FindNodeHit(X, Y);
  if I > 0 then

  // Construct the string to display in the Hint panel
  begin
    S := project.GetItemTypeStr(cNodes, I-1) + project.GetItemID(cNodes, I-1);
    if mapthemes.NodeTheme > 0 then
    begin
      V := mapthemes.GetCurrentThemeValue(cNodes, I);
      if V = MISSING then S := S + Chr(13) + 'N/A'
      else S := S + Chr(13) + FloatToStrF(V, ffFixed, 7, config.DecimalPlaces);
    end;
  end
  else

  // Check if mouse is over a link
  begin
    I := Map.FindLinkHit(X, Y);
    if I > 0 then

    // Contruct the string to display in the Hint panel
    begin
      S := project.GetItemTypeStr(cLinks, I-1) + project.GetItemID(cLinks, I-1);
      if mapthemes.LinkTheme > 0 then
      begin
        V := mapthemes.GetCurrentThemeValue(cLinks, I);
        if V = MISSING then S := S + Chr(13) + 'N/A'
        else S := S + Chr(13) + FloatToStrF(V, ffFixed, 7, config.DecimalPlaces);
      end;
    end;
  end;

  // Display the Hint panel (will not appear if S is blank)
  HintInfo^.HintStr := S;
end;

procedure TMapFrame.EnableMainForm(State: Boolean);
begin
  MainForm.MainMenuFrame.Enabled := State;
  MainForm.ProjectFrame.Enabled := State;
  MainForm.LegendTreeView.Enabled := State;
  MainForm.MainMenuFrame.SpeedPanel1.Enabled := State;
  //MainForm.MainMenuFrame.SpeedPanel2.Enabled := State;
  if State then MainForm.HideHintPanel;
end;

procedure TMapFrame.AddNode(NodeType: Integer);
// Prepare the Map to begin adding a Node to it.

begin
  case NodeType of
    nJunction: MapAction := maAddingJunc;
    nReservoir: MapAction := maAddingResv;
    nTank: MapAction := maAddingTank;
  end;
  Hiliter.Hide;
  ShowJunctions;
  MapBox.Cursor := crCross;
  EnableMainForm(false);
  SetFocus;
end;

procedure TMapFrame.AddLink(LinkType: Integer);
begin
  case LinkType of
  lPipe: MapAction := maAddingPipe;
  lPump: MapAction := maAddingPump;
  lValve: MapAction := maAddingValve;
  end;
  Hiliter.Hide;
  ShowJunctions;
  MapBox.Cursor := crHandPoint;
  EnableMainForm(false);
  SetFocus;
end;

procedure TMapFrame.AddLabel;
begin
  MapAction := maAddingLabel;
  Hiliter.Hide;
  MapBox.Cursor := crCross;
  EnableMainForm(false);
  SetFocus;
end;

procedure TMapFrame.ZoomIn(Dx: Integer; Dy: Integer);
var
  TmpMapAction: TMapAction;
begin
  TmpMapAction := MapAction;
  MapAction := maZooming;
  Map.ZoomIn(Dx, Dy);
  RedrawMap;
  HiliteObject(SelectedObjType, SelectedObjIndex);
  MapAction := TmpMapAction;
end;

procedure TMapFrame.ZoomOut(Dx: Integer; Dy: Integer);
var
  TmpMapAction: TMapAction;
begin
  TmpMapAction := MapAction;
  MapAction := maZooming;
  Map.ZoomOut(Dx, Dy);
  RedrawMap;
  HiliteObject(SelectedObjType, SelectedObjIndex);
  MapAction := TmpMapAction;
end;

procedure TMapFrame.EnterSelectionMode;
begin
  EnableMainForm(true);
  Linking := false;
  FenceLining := false;
  Moving := false;
  Node1 := 0;
  Label1 := 0;
  MapAction := maSelecting;
  Offset := Point(0, 0);
  MapBox.Refresh;
  MapBox.Cursor := crDefault;
end;

procedure TMapFrame.EnterFenceLiningMode;
begin
  Hiliter.Hide;
  MapAction := maFenceLining;
  FenceLining := False;
  NumVertices := 0;
  EnableMainForm(false);
  MapBox.Cursor := crCross;
end;

procedure TMapFrame.BeginFenceLining(X: Integer; Y: Integer);
begin
  // If this is first fence point then begin a new fence line
  if not FenceLining then
  begin
    Vertices[0] := Point(X, Y);
    Vertices[1] := Point(X, Y);
    NumVertices := 2;
    FenceLining := True;
  end

  // Otherwise add a new vertex point to the fence line
  else if NumVertices < project.MAX_VERTICES then
  begin
    Inc(NumVertices);
    Vertices[NumVertices-1] := Point(X, Y);
  end;
end;

procedure TMapFrame.LeaveFenceLiningMode;
var
  I: Integer;
  WorldPoly: TPolygon;
begin
  if MapAction = maFenceLining then
  begin
    MainForm.HideHintPanel;
    Setlength(WorldPoly, 0);

    // User selected entire network
    if NumVertices = -1 then
      MainForm.ProjectFrame.GroupEdit(WorldPoly, NumVertices);

    // A valid fence line polygon was constructed
    if NumVertices > 3 then
    begin
      // Close up the polygon
      utils.DrawDottedLine(MapBox.Canvas, Vertices[NumVertices-1], Vertices[0]);
      Vertices[NumVertices-1] := Vertices[0];

      // Convert polygon from screen to world coordinates
      SetLength(WorldPoly, NumVertices);
      for I := 0 to NumVertices - 1 do
        WorldPoly[I] := Map.ScreenToWorld(Vertices[I].X, Vertices[I].Y);

      // Invoke the modal GroupEdit form
      MainForm.ProjectFrame.GroupEdit(WorldPoly, NumVertices);
    end;

    // Return map to Selection mode
    FenceLining := False;
    MapBox.Refresh;
    MainForm.MainMenuFrame.GroupEditBtn.Down := False;
    EnterSelectionMode;
  end;
end;

procedure TMapFrame.EnterVertexingMode;
begin
  Hiliter.Hide;
  MapAction := maVertexing;
  if SelectedObjType = cLinks then
  begin
    EnableMainForm(false);
    SelectedVertex := 1;
    ShowVertices(SelectedObjIndex);
  end;
  MapBox.Cursor := crHandPoint;
end;

procedure TMapFrame.LeaveVertexingMode;
begin
  if MapAction = maVertexing then
  begin
    MainForm.MainMenuFrame.EditVertexBtn.Down := False;
    if project.AutoLength and
      (project.GetLinkType(SelectedObjIndex) <= lPipe) then
      project.SetPipeLength(SelectedObjIndex);
    EnterSelectionMode;
  end;
end;

procedure TMapFrame.SetExtent(E: TDoubleRect);
begin
  Map.Extent := E;
end;

function TMapFrame.GetExtent: TDoubleRect;
begin
  Result := Map.Extent;
end;

function TMapFrame.GetMapRect: TRect;
begin
  Result := Map.MapRect;
end;

procedure TMapFrame.CopyMap(FileName: String; IncludeLegend: Boolean);
var
  H1, H2, W1, W2: Integer;
  R: TRect;
  TmpBitmap: TBitmap;
begin
  with MainForm do
  begin
    H1 := LegendTreeView.ClientHeight;
    H2 := MapFrame.MapBox.ClientHeight;
    W1 := LegendTreeView.ClientWidth;
    W2 := MapFrame.MapBox.ClientWidth;
  end;
  if not IncludeLegend then W1 := 0;
  TmpBitmap := TBitmap.Create;
  try
    TmpBitmap.SetSize(W1 + W2, H2);
    R := Rect(0, 0, W1 + W2, H2);
    with TmpBitmap.Canvas do
    begin
      Brush.Color := MainForm.LegendTreeView.Color;
      Brush.Style := bsSolid;
      FillRect(R);
    end;
    if IncludeLegend then
    begin
      R := Rect(2, 2, W1, H1);
      with MainForm do
      begin
        TmpBitmap.Canvas.CopyRect(R, LegendTreeView.Canvas, Rect(0, 0, W1, H1));
      end;
    end;
    R := Rect(W1, 0, W1 + W2, H2);
    TmpBitmap.Canvas.CopyRect(R, MapBox.Canvas, Rect(0, 0, W2, H2));
    if IncludeLegend then TmpBitmap.Canvas.Line(W1, 0, W1, H2);
    if Length(FileName) = 0 then
    begin
      Clipboard.Assign(TmpBitmap);
//    utils.MsgDlg('Map copied to Clipboard.', mtInformation, [mbOk], MainForm);
    end
    else
      TmpBitmap.SaveToFile(FileName);
  finally
    TmpBitmap.Free;
  end;
end;

procedure TMapFrame.RedrawMap;
begin
  Hiliter.Hide;
  Map.Redraw;
  if MainForm.GeoRefFrame.Visible then DrawCtrlPoints;
  MapBox.Refresh;
  if MapAction = maSelecting then
    HiliteObject(SelectedObjType, SelectedObjIndex);
end;

procedure TMapFrame.DrawCtrlPoints;
var
  I: Integer;
begin
  for I := Low(CtrlPoint) to High(CtrlPoint) do
  begin
    if CtrlPoint[I].Visible then
      Map.DrawBitmap(CtrlPoint[I].Bitmap, CtrlPoint[I].Position);
  end;
end;

procedure TMapFrame.RedrawMapLabels;
begin
  if Map.Options.ShowLabels then RedrawMap;
end;

procedure TMapFrame.GoKeyDown(var Key: Word);
begin
  if MapAction =  maVertexing then
  begin
    if (Key = VK_INSERT) or (Key = 187) then AddVertex
    else if (Key = VK_DELETE) or (Key = VK_BACK) then DeleteVertex
    else if Key = VK_ESCAPE then LeaveVertexingMode
    else ShowVertices(SelectedObjIndex);
  end
  else if MapAction = maFenceLining then
  begin
    if Key = VK_RETURN then
    begin
      if NumVertices = 0 then NumVertices := -1;
      LeaveFenceLiningMode;
    end
    else if Key = VK_ESCAPE then
    begin
      NumVertices := 0;
      LeaveFenceLiningMode;
    end;
  end
  else if MapAction in [maAddingJunc .. maAddingLabel] then
  begin
    if Key = VK_ESCAPE then EnterSelectionMode;
  end
  else if (Key = VK_RETURN) and (MapAction = maSelecting) then
  begin
    if (SelectedObjType = cLinks) and
      (MainForm.ProfileSelectorFrame.Visible) then
        MainForm.ProfileSelectorFrame.AddLink;
  end
  else if MainForm.ProjectFrame.TreeView1.Focused then
  begin
    if (Key = VK_DELETE) or (Key = VK_BACK) then
    with MainForm.MainMenuFrame do
      if ProjectDeleteBtn.Enabled then ProjectDeleteBtnClick(self);
  end;
end;

function TMapFrame.StartLinking(const X: Integer; const Y: Integer): Boolean;
var
  I: Integer;
begin
  I := Map.FindNodeHit(X, Y);
  if I > 0 then
  begin
    Node1 := I;
    Point1.x := X;
    Point1.y := Y;
    Point2 := Point1;
    NumVertices := 0;
    MapBox.Cursor := crCross;
    Result := true;
  end
  else Result := false;
end;

procedure TMapFrame.EndLinking(const X: Integer; const Y: Integer);
var
  Node2: Integer;
  I: Integer;
begin
  Node2 := Map.FindNodeHit(X, Y);
  if Node2 > 0 then
  begin
    MapBox.Cursor := crHandPoint;
    if Node1 <> Node2 then
    begin
      I :=  Ord(MapAction) - Ord(maAddingPipe) + 1;
      ProjectBuilder.AddLink(I, Node1, Node2);
      if I >= lPipe then AddLink(I);
    end;
    Linking := False;
  end
  else
  begin
    Inc(NumVertices);
    Vertices[NumVertices] := Point2;
    Point1 := Point2;
  end;
end;

procedure TMapFrame.ChangeMapLayer(MapLegend: TTreeView);
//  Responds to a user's choice to turn on/off the display of a specific
//  type of network element (e.g., junction nodes) on the network map.

var
  TreeNode: TTreeNode;
  IsSelected: Boolean;
begin
  // Find the selected Tree node
  TreeNode := MapLegend.Selected;
  if TreeNode = nil then exit;
  if TreeNode.StateIndex = -1 then exit;

  // Change the Checked/Unchecked state of the node
  TreeNode.StateIndex := 1 - TreeNode.StateIndex;
  IsSelected := (TreeNode.StateIndex = 1);
  MapLegend.Refresh;

  // Apply the new state to the network map's display options
  // (Note: Tree nodes representing display options were
  // assigned SelectedIndex values at design time.)
  case TreeNode.SelectedIndex of
  0: Map.Options.ShowNodes := IsSelected;
  1: Map.Options.ShowJunctions := IsSelected;
  2: Map.Options.ShowTanks := IsSelected;
  3: Map.Options.ShowLinks := IsSelected;
  4: Map.Options.ShowPumps := IsSelected;
  5: Map.Options.ShowValves := IsSelected;
  6: Map.Options.ShowLabels := IsSelected;
  7: begin
       Map.Options.ShowBackdrop := IsSelected;
       Map.Basemap.NeedsRedraw := IsSelected;
     end;
  end;

  // Redraw the network map
  MapLegend.Selected := nil;
  RedrawMap;
end;

procedure TMapFrame.ShowJunctions;
// Redraw the map showing nodes if their display was turned off.

var
  TreeNode : TTreeNode;
begin
  // Redraw map with nodes displayed
  if (Map.Options.ShowNodes = False) or
     (Map.Options.ShowJunctions = False) then
  begin
    Map.Options.ShowNodes := True;
    Map.Options.ShowJunctions := True;
    RedrawMap;
  end;

  // Update the main form's map layers tree view
  for TreeNode in MainForm.LegendTreeView.Items do
  begin
    if TreeNode.Text = 'Nodes' then
    begin
      if TreeNode.StateIndex = 0 then TreeNode.StateIndex := 1;
    end
    else if TreeNode.Text = 'Junctions' then
    begin
      if TreeNode.StateIndex = 0 then TreeNode.StateIndex := 1;
    end;
  end;
end;

procedure TMapFrame.InitMapOptions;
begin
  Map.Options := mapoptions.DefaultOptions;
end;

procedure TMapFrame.EditMapOptions;
begin
  if MapOptions.Edit(Map.Options) then
  begin
    RedrawMap;
  end;
end;

//------------------------------------------------------------------------------
//  MapBox Procedures
//------------------------------------------------------------------------------

procedure TMapFrame.MapBoxPaint(Sender: TObject);
begin
  if Assigned(Map) then
  begin
    if MapAction = maPanning then
    begin
      MapBox.Canvas.Brush.Color := Map.GetBackColor;
      MapBox.Canvas.FillRect(Rect(0, 0, ClientWidth, ClientHeight))
    end;
    MapBox.Canvas.Draw(Offset.X, Offset.Y, Map.Bitmap);
  end;
end;

procedure TMapFrame.MapBoxResize(Sender: TObject);
begin
  if not MainForm.ReportPanel.Visible then
    ResizeTimer.Enabled := true;
end;

procedure TMapFrame.MapBoxChangeBounds(Sender: TObject);
begin
  ResizeTimer.Enabled := False;
end;

procedure TMapFrame.MapBoxClick(Sender: TObject);
var
  I: Integer;
  W: TDoublePoint;
begin
  if MainForm.GeoRefFrame.Visible then
  begin
    W := Map.ScreenToWorld(Point1.X, Point1.Y);
    I := MainForm.GeoRefFrame.GetCtrlPointIndex(W);
    if I > 0 then
    begin
      CtrlPoint[I].Position := W;
      CtrlPoint[I].Visible := True;
      RedrawMap;
    end;
  end;
end;

procedure TMapFrame.MapBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  Moving := false;
  if MapAction = maVertexing then
  begin
    if SelectVertex(X, Y) and (Shift = [ssLeft, ssCtrl]) then
    begin
      Moving := true;
      Point1 := Point(X, Y);
      Point2 := Point1;
    end;
  end

  else if MapAction = maFenceLining then
  begin
    BeginFenceLining(X, Y);
    exit;
  end

  else if MapAction = maSelecting then
  begin
    if Shift = [ssRight] then
    begin
      EditMapOptions;
      exit;
    end;

    I := 0;
    I := Map.FindNodeHit(X, Y);
    if I > 0 then
    begin
      MainForm.ProjectFrame.SelectItem(cNodes, I-1);
      Node1 := I;
      Point1 := Point(X, Y);
      Point2 := Point1;
      if Shift = [ssLeft, ssCtrl] then
        Moving := true;
      OldTickCount := GetTickCount64;
    end

    else begin
      I := Map.FindLinkHit(X, Y);
      if I > 0 then
        MainForm.ProjectFrame.SelectItem(cLinks, I-1)
      else
      begin
        I := Map.FindLabelHit(X, Y);
        if I > 0 then
        begin
          MainForm.ProjectFrame.SelectItem(cLabels, I-1);
          Label1 := I;
          Point1 := Point(X, Y);
          Point2 := Point1;
          if Shift = [ssLeft, ssCtrl] then Moving := true;
          OldTickCount := GetTickCount64;
        end;
      end;
    end;
    if I = 0 then HiLiter.Hide;

    if not Moving then
    begin
      MapAction := maPanning;
      Point1 := Point(X, Y);
    end;
  end

  else if Shift = [ssRight] then
  begin
    EnterSelectionMode;
  end;
end;

procedure TMapFrame.MapBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  XY: TDoublePoint;
begin
  if Assigned(Map) then
  begin
    XY := Map.ScreenToWorld(X, Y);
    MainForm.UpdateXYStatus(XY.X, XY.Y);
  end;

  if Linking or Moving then
  begin
    Hiliter.Hide;
    if Moving then
    begin
      if (Shift <> [ssLeft, ssCtrl]) or
        (GetTickCount64 - OldTickCount < TICKDELAY) then exit;
      OldTickCount := 0;
    end;
    Utils.DrawDottedLine(MapBox.Canvas, Point1, Point2);
    Point2 := Point(X, Y);
    Utils.DrawDottedLine(MapBox.Canvas, Point1, Point2);
  end

  else if FenceLining then
  begin
    utils.DrawDottedLine(
      MapBox.Canvas, Vertices[NumVertices-2], Vertices[NumVertices-1]);
    Vertices[NumVertices-1] := Point(X,Y);
    utils.DrawDottedLine(
      MapBox.Canvas, Vertices[NumVertices-2], Vertices[NumVertices-1]);
  end

  else if MapAction = maPanning then
  begin
    if Shift = [ssLeft] then
    begin
      Hiliter.Hide;
      MapBox.Cursor := crSize;
      Offset := Point(X - Point1.X, Y - Point1.Y);
      MapBox.Refresh;
    end;
  end;
end;

procedure TMapFrame.MapBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  W: TDoublePoint;
  I: Integer;
begin
  if MapAction = maVertexing then
  begin
    if Moving then MoveVertex(X, Y)
  end
  else if MapAction = maPanning then
  begin
    MapAction := maSelecting;
    MapBox.Cursor := crDefault;
    if (Abs(Offset.X) > 2) or (Abs(Offset.Y) > 2) then
    begin
      Map.AdjustOffset(Offset.X, Offset.Y);
      Offset := Point(0, 0);  //Allows Map to be copied correctly into MapBox
      RedrawMap;
    end;
  end

  else if MapAction in [maAddingJunc .. maAddingTank] then
  begin
    W := Map.ScreenToWorld(X, Y);
    I := ord(MapAction) - ord(maAddingJunc);
    ProjectBuilder.AddNode(I, W.X, W.Y);
    if I >= nJunction then
    begin
      AddNode(I);
    end;
  end

  else if MapAction in [maAddingPipe .. maAddingValve] then
  begin
    if Linking = True then EndLinking(X,Y)
    else Linking := StartLinking(X,Y);
  end

  else if MapAction = maAddingLabel then
  begin
    P := ClientToScreen(Point(X,Y));
    W := Map.ScreenToWorld(X, Y);
    ProjectBuilder.AddLabel(P, W.X, W.Y);
  end

  else if Moving and (GetTickCount64 - OldTickCount > TICKDELAY) then
  begin
    OldTickCount := 0;
    W := Map.ScreenToWorld(X, Y);
    if Node1 > 0 then
    begin
      project.SetNodeCoord(Node1, W.X, W.Y);
      project.AdjustLinkLengths(Node1);
    end
    else if Label1 > 0 then
      project.SetLabelCoord(Label1, W.X, W.Y);
    project.HasChanged := True;
    RedrawMap;
    EnterSelectionMode;
  end;
end;

procedure TMapFrame.MapBoxMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if FenceLining then exit;
  ZoomOut(MousePos.x - (MapBox.ClientWidth div 2),
    MousePos.y - (MapBox.ClientHeight div 2));
end;

procedure TMapFrame.MapBoxMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if FenceLining then exit;
  ZoomIn(MousePos.x - (MapBox.ClientWidth div 2),
    MousePos.y - (MapBox.ClientHeight div 2));
end;

//------------------------------------------------------------------------------
//  Link Vertex Editing Procedures
//------------------------------------------------------------------------------

procedure TMapFrame.GetVertices(var X: array of Double; var Y: array of Double;
  var N: Integer);
var
  I: Integer;
  W: TDoublePoint;
begin
  N := NumVertices;
  for I := 1 to NumVertices do
  begin
    W := Map.ScreenToWorld(Vertices[I].X, Vertices[I].Y);
    X[I-1] := W.X;
    Y[I-1] := W.Y;
  end;
end;

procedure TMapFrame.ShowVertices(I: Integer);
var
  J: Integer;
begin
  MapBox.Canvas.Pen.Color := clBlack;
  NumVertices := project.GetVertexCount(I);
  if NumVertices > 0 then
    for J := 1 to NumVertices do ShowVertex(I, J, clWhite)
  else begin
    ShowVertex(I, 0, clWhite);
    SelectedVertex := 0;
  end;
  if Selectedvertex > 0 then ShowVertex(I, SelectedVertex, clBlack);
end;

procedure TMapFrame.ShowVertex(I: Integer; J: Integer; C: TColor);
var
  X: Double = 0;
  Y: Double = 0;
  P: TPoint;
  R: TRect;
  S: Integer;
begin
  S := 3 +  Map.Options.LinkSize;
  if J > 0 then project.GetVertexCoord(I, J, X, Y)
  else project.GetLinkCoord(I, X, Y);
  P := Map.WorldToScreen(X, Y);
  R := Rect(P.x-S, P.y-S, P.x+S, P.y+S);
  MapBox.Canvas.Brush.Color := C;
  MapBox.Canvas.Rectangle(R);
end;

function TMapFrame.SelectVertex(X: Integer; Y: Integer): Boolean;
var
  Vx: Double = 0;
  Vy: Double = 0;
  P: TPoint;
  R: TRect;
  I, J, S: Integer;
begin
  Result := false;
  I := SelectedObjIndex;
  if NumVertices = 0 then exit;
  S := 3 +  Map.Options.LinkSize;
  for J := 1 to NumVertices do
  begin
    project.GetVertexCoord(I, J, Vx, Vy);
    P := Map.WorldToScreen(Vx, Vy);
    R := Rect(P.x-S, P.y-S, P.x+S, P.y+S);
    if PtInRect(R, Point(X, Y)) then
    begin
      ShowVertex(I, SelectedVertex, clWhite);
      SelectedVertex := J;
      ShowVertex(I, SelectedVertex, clBlack);
      Result := true;
      exit;
    end;
  end;
end;

procedure TMapFrame.MoveVertex(X: Integer; Y: Integer);
var
  W: TDoublePoint;
begin
  if SelectedVertex = 0 then exit;
  W := Map.ScreenToWorld(X, Y);
  project.SetVertexCoord(SelectedObjIndex, SelectedVertex, W.X, W.Y);
  RedrawMap;
  ShowVertices(SelectedObjIndex);
end;

procedure TMapFrame.AddVertex;
var
  I, J, V: Integer;
  N1: Integer = 0;
  N2: Integer = 0;
  X: Double = 0;
  Y: Double = 0;
  X1: Double = 0;
  Y1: Double = 0;
  X2: Double = 0;
  Y2: Double = 0;
  Xv: array[0..project.MAX_VERTICES] of Double;
  Yv: array[0..project.MAX_VERTICES] of Double;
begin
  I := SelectedObjIndex;
  V := SelectedVertex;

  // Find location midway between selected vertex and next higher one
  if V = 0 then project.GetLinkCoord(I, X, Y)
  else begin
    project.GetVertexCoord(I, V, X1, Y1);
    if V < NumVertices then project.GetVertexCoord(I, V+1, X2, Y2)
    else begin
      project.GetLinkNodes(I, N1, N2);
      project.GetNodeCoord(N2, X2, Y2);
    end;
    X := (X1 + X2) / 2;
    Y := (Y1 + Y2) / 2;
  end;

  // Insert new vertex at that location
  Xv[0] := 0;
  Yv[0] := 0;
  for J := 1 to V do
    project.GetVertexCoord(I, J, Xv[J-1], Yv[J-1]);
  for J := NumVertices downto V + 1 do
    project.GetVertexCoord(I, J, Xv[J], Yv[J]);
  Inc(NumVertices);
  Xv[V] := X;
  Yv[V] := Y;
  project.SetVertexCoords(I, Xv, Yv, NumVertices);

  // Redraw vertex pts. with new vertex selected
  MapBox.Refresh;
  SelectedVertex := V + 1;
  ShowVertices(I);
end;

procedure TMapFrame.DeleteVertex;
var
  I, J, K: Integer;
  Xv: array[0..project.MAX_VERTICES] of Double;
  Yv: array[0..project.MAX_VERTICES] of Double;
begin
  if SelectedVertex = 0 then exit;
  I := SelectedObjIndex;
  Xv[0] := 0;
  Yv[0] := 0;
  K := 0;
  for J := 1 to NumVertices do
  begin
    if J <> SelectedVertex then
    begin
      project.GetVertexCoord(I, J, Xv[K], Yv[K]);
      Inc(K);
    end;
  end;
  Dec(NumVertices);
  project.SetVertexCoords(I, Xv, Yv, NumVertices);
  if SelectedVertex > 1 then Dec(SelectedVertex);
  RedrawMap;
  ShowVertices(I);
end;

procedure TMapFrame.DeleteAllVertices;
var
  Xv: array of Double;
  Yv: array of Double;
begin
  if SelectedVertex = 0 then exit;
  project.SetVertexCoords(SelectedObjIndex, Xv, Yv, 0);
  SelectedVertex := 0;
  RedrawMap;
  ShowVertices(SelectedObjIndex);
end;

//------------------------------------------------------------------------------
//  Basemap Procedures
//------------------------------------------------------------------------------

procedure TMapFrame.LoadBasemapFromFile;
begin
  with OpenPictureDialog1 do
  begin
    if Execute then
    begin
      if not Map.LoadBasemapFile(Filename)then
        utils.MsgDlg('Was not able to load the image file.',
          mtError, [mbOK], MainForm)
//       utils.MsgDlg('Was not able to load the image file.', mtError,
//          [mbOk], MainForm)
      else begin
        mapthemes.SetBaseMapVisible(true);
        Map.Options.ShowBackdrop := true;
        RedrawMap;
        HasBaseMap := true;
        BaseMapFile := Filename;
      end;
    end;
  end;
  MainForm.SetFocus;
end;

procedure TMapFrame.LoadBasemapFromWeb(MapSource: Integer; EpsgStr:String);
var
  NorthEast: TDoublePoint = (X: -60; Y: 52.5);
  SouthWest: TDoublePoint = (X: -130; Y: 17.5);
  ShowLocationFinder: Boolean = false;
  TmpExtent: TDoubleRect;
  Converted: Boolean;
begin
  // Change the source for an existing WebMap
  if MapSource < 0 then exit;
  if Map.Basemap.WebMap <> nil then
  begin
    Map.Basemap.WebMap.MapSource := MapSource;
    Map.Basemap.NeedsRedraw := True;
  end

  // Otherwise load a new WebMap with default extent (North America)
  else begin

    // If a network exists, replace default extent with network's
    if not project.IsEmpty then
    begin

      // Transform coordinates to EPSG 4326 if a different EPSG provided
      if Length(EpsgStr) > 0 then
      begin
        // Check that current map extent can be converted
        TmpExtent := Map.Extent;
        MainForm.Cursor := crHourGlass;
        Converted := mapcoords.DoProjectionTransform(EpsgStr, '4326', Map.Extent);
        MainForm.Cursor := crDefault;
        if not Converted then
        begin
          Map.Extent := TmpExtent;
          utils.MsgDlg(
          'Could not convert EPSG ' + EpsgStr + ' coordinates to lat/lon.' +
          sLineBreak + 'Please make sure you specify the correct projection.',
          mtInformation, [mbOk]);
          exit;
        end;

        // Save EPSG & map units so original coords. can be restored
        project.OldMapEPSG := StrToInt(EpsgStr);
        project.OldMapUnits := project.MapUnits;
      end;

      // Network must have lat/lon coords.
      if mapcoords.HasLatLonCoords(Map.Extent) then
      begin
        NorthEast := Map.Extent.UpperRight;
        SouthWest := Map.Extent.LowerLeft;
      end
      else begin
        utils.MsgDlg(
          'Map coordinates must be in decimal degrees.', mtInformation, [mbOk]);
        exit;
      end;
    end

    // If no network then ask that the location finder form be shown
    else ShowLocationFinder := true;

    // Create a Web Basemap
    Map.CreateWebBasemap(MapSource, NorthEast, SouthWest);
  end;

  // Apply the Webmap as a backdrop for the network map
  // (If the web basemap can't be loaded it will be set to nil.)
  ShowWebBasemap;
  if HasWebBasemap then
  begin
    project.MapUnits := muDegrees;
    if ShowLocationFinder then FindBasemapLocation;
  end;
end;

procedure TMapFrame.ShowWebBasemap;
begin
  mapthemes.SetBaseMapVisible(true);
  Map.Options.ShowBackdrop := true;
  HasBaseMap := true;
  RedrawMap;
  if not HasWebBasemap then
    UnloadBasemap;
end;

procedure TMapFrame.FindBasemapLocation;
var
  NorthEast: TDoublePoint;
  SouthWest: TDoublePoint;
begin
  if not project.IsEmpty then
    utils.MsgDlg('This feature is currently only available for empty networks.',
      mtInformation, [mbOk])

  else with TWebMapFinderForm.Create(MainForm) do
  try
    ShowModal;
    if ModalResult = mrOK then
    begin
      NorthEast.X := Lon + 0.125;
      NorthEast.Y := Lat + 0.125;
      SouthWest.X := Lon - 0.125;
      SouthWest.Y := Lat - 0.125;
      Map.CreateWebBasemap(Map.Basemap.WebMap.MapSource, NorthEast, SouthWest);
      ShowWebBasemap;
    end;
  finally
    Free;
  end;
end;

function TMapFrame.GetBasemapSize: TSize;
begin
  Result := Size(Map.Basemap.Picture.Width, Map.Basemap.Picture.Height);
end;

procedure TMapFrame.UnloadBasemap;
begin
  mapthemes.SetBaseMapVisible(false);
  if HasWebBasemap then UnloadBasemapFromWeb;
  Map.ClearBasemap;
  RedrawMap;
  HasBaseMap := false;
end;

procedure TMapFrame.UnloadBasemapFromWeb;
begin
  // Revert coord. projection to its original value
  if (project.OldMapEPSG > 0) and (project.OldMapEPSG <> 4326) then
  begin
    if mapcoords.DoProjectionTransform('4326', IntToStr(project.OldMapEPSG),
      Map.Extent) then
    begin
      project.MapEPSG:= project.OldMapEPSG;
      project.MapUnits := project.OldMapUnits;
    end;
  end;
end;

procedure TMapFrame.SetBasemapBrightness(Brightness: Integer);
begin
  Map.Basemap.Brightness := Brightness;
  RedrawMap;
end;

function  TMapFrame.HasWebBasemap: Boolean;
begin
  Result := (Map.Basemap.WebMap <> nil);
end;

function  TMapFrame.GetWebBasemapSource: Integer;
begin
  if Map.Basemap.WebMap <> nil then Result := Map.Basemap.WebMap.MapSource
  else Result := -1;
end;

end.

