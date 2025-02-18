{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       mapcoords
 Description:  utility functions for map coordinates
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit mapcoords;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math;

type
  TDoublePoint = record
    X: Double;
    Y: Double;
  end;

  TDoubleRect = record
    LowerLeft: TDoublePoint;
    UpperRight: TDoublePoint;
  end;

  TPolygon = array of TDoublePoint;

  TScalingInfo = record
    CW: TDoublePoint;  // world coordinates of map center
    CP: TPoint;        // pixel coordinates of map center
    WP: Double;        // world distance per pixel
  end;

function  DoublePoint(X, Y: Double): TDoublePoint;

function  DoubleRect(LowerLeft, UpperRight: TDoublePoint): TDoubleRect;

function  GetBounds(Bounds: TDoubleRect): TDoubleRect;

function  GetZoomLevel(NorthEast: TDoublePoint; SouthWest: TDoublePoint;
            MapRect: TRect): Integer;

function  HasLatLonCoords(MapExtent: TDoubleRect): Boolean;

procedure DoAffineTransform(FromRect, ToRect: TDoubleRect);

procedure DoScalingTransform(FromScaling, ToScaling: TScalingInfo);

function  DoProjectionTransform(FromProj, ToProj: String;
            var Bounds: TDoubleRect): Boolean;

function  FromWGS84ToWebMercator(LatLng: TDoublePoint): TDoublePoint;

function  ManhattanDistance(P1, P2: TDoublePoint): Double;

implementation

uses
  project, projtransform;

const
  ScalingTransform = 0;
  AffineTransform = 1;
  ProjectionTransform = 2;

var
  S1, S2: TScalingInfo;      // Used for scaling transform
  Ax, Ay, Bx, By: Double;    // Used for affine transform
  ProjTrans: TProjTransform; // Used for projection transform

function  DoublePoint(X, Y: Double): TDoublePoint;

//  Create a point from coordinates X, Y

begin
  Result.X := X;
  Result.Y := Y;
end;

function  DoubleRect(LowerLeft, UpperRight: TDoublePoint): TDoubleRect;

//  Create a rectangle from a pair of points

begin
  Result.LowerLeft := LowerLeft;
  Result.UpperRight := UpperRight;
end;

function GetBounds(Bounds: TDoubleRect): TDoubleRect;

//  Get the rectangle that bounds all map objects

var
  Xmin : Double = 1.e50;
  Ymin : Double = 1.e50;
  Xmax : Double = -1.e50;
  Ymax : Double = -1.e50;
  X : Double= 0;
  Y : Double= 0;
  Bufr: Double;
  I, NumNodes, NumLabels: Integer;
begin
  // Get number of nodes & labels
  NumNodes := project.GetItemCount(cNodes);
  NumLabels := project.GetItemCount(cLabels);

  // If no nodes and labels return the current bounding rectangle
  Result := Bounds;
  if (NumNodes = 0) and (NumLabels = 0) then exit;

  // Find min/max X,Y coords.
  for I := 1 to NumNodes do
  begin
    if project.GetNodeCoord(I, X, Y) then
    begin
      Xmin := Min(Xmin, X);
      Ymin := Min(Ymin, Y);
      Xmax := Max(Xmax, X);
      Ymax := Max(Ymax, Y);
    end;
  end;
  for I := 1 to NumLabels do
  begin
    if project.GetLabelCoord(I, X, Y) then
    begin
      Xmin := Min(Xmin, X);
      Ymin := Min(Ymin, Y);
      Xmax := Max(Xmax, X);
      Ymax := Max(Ymax, Y);
    end;
  end;

  // Expand bounds by a 5% buffer
  Bufr := 0.05 * (Xmax - Xmin);
  if Bufr = 0 then Bufr := 10;
  Xmin := Xmin - Bufr;
  Xmax := Xmax + Bufr;
  Bufr := 0.05 * (Ymax - Ymin);
  if Bufr = 0 then Bufr := 10;
  Ymin := Ymin - Bufr;
  Ymax := Ymax + Bufr;

  // Build a bounding rectangle from the min/max points
  Result.LowerLeft := DoublePoint(Xmin, Ymin);
  Result.UpperRight := DoublePoint(Xmax, Ymax);
end;

function  HasLatLonCoords(MapExtent: TDoubleRect): Boolean;

//  Determine if a map's bounding coordinates are lat/lon

var
  Delta: Double;

begin
  Result := false;
  with MapExtent do
  begin
    if Max(Abs(LowerLeft.X), Abs(UpperRight.X)) > 180 then exit;
    if Max(Abs(LowerLeft.Y), Abs(UpperRight.Y)) > 90 then exit;
    Delta := Abs(LowerLeft.X - UpperRight.X);
    if Delta < 1e-6 then exit;
    Delta := Abs(LowerLeft.Y - UpperRight.Y);
    if Delta < 1e-6 then exit;
  end;
  Result := true;
end;

function ApplyScalingTransform(X, Y: Double): TDoublePoint;
var
  P: TPoint;
  Z: Double;
begin
  Z := (X - S1.CW.X) / S1.WP;
  P.X := S1.CP.X + Round(Z);
  X := S2.CW.X + (P.X - S2.CP.X) * S2.WP;
  Z := (Y - S1.CW.Y) / S1.WP;
  P.Y := S1.CP.Y - Round(Z);
  Y := S2.CW.Y + (S2.CP.Y - P.Y) * S2.WP;
  Result.X := X;
  Result.Y := Y;
end;

function ApplyAffineTransform(X, Y: Double): TDoublePoint;
begin
  Result.X := Ax + Bx*X;
  Result.Y := Ay + By*Y;
end;

function ApplyProjectionTransform(var X, Y: Double): TDoublePoint;
begin
  ProjTrans.Transform(X,Y);
  Result.X := X;
  Result.Y := Y;
end;

function ApplyTransform(TransformType: Integer; X, Y: Double): TDoublePoint;
begin
  Result := DoublePoint(0,0);
  case TransformType of
    AffineTransform: Result := ApplyAffineTransform(X, Y);
    ScalingTransform: Result := ApplyScalingTransform(X, Y);
    ProjectionTransform: Result := ApplyProjectionTransform(X, Y);
  end;
end;

procedure TransformNodeCoords(TransformType: Integer);

//  Apply a transform to all network node coordinates

var
  I: Integer;
  X: Double = 0;
  Y: Double = 0;
  DP: TDoublePoint;
begin
  for I := 1 to project.GetItemCount(cNodes) do
  begin
    if project.GetNodeCoord(I, X, Y) then
    begin
      DP := ApplyTransform(TransformType, X, Y);
      project.SetNodeCoord(I, DP.X, DP.Y);
    end;
  end;
end;

procedure TransformVertexCoords(TransformType: Integer);

//  Apply a transform to all network link vertex coordinates

var
  I, J: Integer;
  X: Double = 0;
  Y: Double = 0;
  DP: TDoublePoint;
  Vx: array of Double;
  Vy: array of Double;
  Vcount, MaxVcount: Integer;
begin
  MaxVcount := 0;
  for I := 1 to project.GetItemCount(cLinks) do
  begin
    Vcount := project.GetVertexCount(I);
    if Vcount > 0 then
    begin
      if Vcount > MaxVcount then
      begin
        SetLength(Vx, Vcount);
        SetLength(Vy, Vcount);
        MaxVcount := Vcount;
      end;
      for J := 1 to Vcount do
      begin
        project.GetVertexCoord(I, J, X, Y);
        DP := ApplyTransform(TransformType, X, Y);
        Vx[J-1] := DP.X;
        Vy[J-1] := DP.Y;
      end;
      project.SetVertexCoords(I, Vx, Vy, Vcount);
    end;
  end;
  SetLength(Vx, 0);
  SetLength(Vy, 0);
end;

procedure TransformLabelCoords(TransformType: Integer);

//  Apply a transform to all map label coordinates

var
  I: Integer;
  X: Double = 0;
  Y: Double = 0;
  DP: TDoublePoint;
begin
  for I := 1 to project.GetItemCount(cLabels) do
  begin
    if project.GetLabelCoord(I, X, Y) then
    begin
      DP := ApplyTransform(TransformType, X, Y);
      Project.SetLabelCoord(I, DP.X, DP.Y);
    end;
  end;
end;

procedure DoAffineTransform(FromRect, ToRect: TDoubleRect);

//  Affine transform coordinates of all map objects from one
//  bounding rectangle to another

var
  LL1, LL2, UR1, UR2: TDoublePoint;
begin
  // Lower left coordinates of both rectangles
  LL1 := FromRect.LowerLeft;
  UR1 := FromRect.UpperRight;
  LL2 := ToRect.LowerLeft;
  UR2 := ToRect.UpperRight;

  // Affine transform coeffs. (Xto = Ax + Bx * Xfrom)
  Bx := (LL2.X - UR2.X) / (LL1.X - UR1.X);
  Ax := LL2.X - Bx * LL1.X;
  By := (LL2.Y - UR2.Y) / (LL1.Y - UR1.Y);
  Ay := LL2.Y - Bx * LL1.Y;

  // Apply affine transform to all network objects
  TransformNodeCoords(AffineTransform);
  TransformVertexCoords(AffineTransform);
  TransformLabelCoords(AffineTransform);
end;

procedure DoScalingTransform(FromScaling, ToScaling: TScalingInfo);

//  Transform all network coordinates from one scaling to another

begin
  // Assign scaling info to global variables S1 & S2 for convenience
  S1 := FromScaling;
  S2 := ToScaling;

  // Transform all node, link vertex & map label coordinates
  TransformNodeCoords(ScalingTransform);
  TransformVertexCoords(ScalingTransform);
  TransformLabelCoords(ScalingTransform);
end;

function DoProjectionTransform(FromProj, ToProj: String;
  var Bounds: TDoubleRect): Boolean;

// Transform all network coordinates from one CRS to another

begin
  // Create a Projection Transform object
  Result := False;
  ProjTrans := TProjTransform.Create;
  try
    if ProjTrans.SetProjections(FromProj, ToProj) then
    begin
      // Check that coords. of current bounding rectangle can be transformed
      ApplyProjectionTransform(Bounds.LowerLeft.X, Bounds.LowerLeft.Y);
      ApplyProjectionTransform(Bounds.UpperRight.X, Bounds.UpperRight.Y);
      if SameText(ToProj, '4326') then
      begin
        if not HasLatLonCoords(Bounds) then exit;
      end;

      // Transform coords. for all map objects
      TransformNodeCoords(ProjectionTransform);
      TransformVertexCoords(ProjectionTransform);
      TransformLabelCoords(ProjectionTransform);
      Result := True;
    end;
  finally
    ProjTrans.Free;
  end;
end;

function GetZoomLevel(NorthEast: TDoublePoint; SouthWest: TDoublePoint;
  MapRect: TRect): Integer;
 
//  Find zoom level for a tiled web map bounded by Northeast and
//  SouthWest lat/lon coordinates displayed in a MapRect screen window

const
  WORLD_DIM = 256;
  ZOOM_MAX = 21;

  function LatRad(Lat: Double): Double;
  var
    a, radX2: Double;
  begin
    a := Sin(Lat * PI / 180);
    radX2 := Ln((1 + a) / (1 - a)) / 2;
    Result := Max(Min(radX2, PI), -PI) / 2;
  end;

  function zoom(mapPx: Integer; worldPx: Integer; fraction: Double): Integer;
  begin
    Result := Floor(Ln(mapPx / worldPx / fraction) / Ln(2));
  end;

var
  latFraction, lonDiff, lonFraction: Double;
  latZoom, lonZoom: Integer;
begin
  latFraction := (LatRad(NorthEast.Y) - LatRad(SouthWest.Y)) / PI;
  latFraction := Abs(latFraction);
  lonDiff := NorthEast.X - SouthWest.X;
  if lonDiff < 0 then lonDiff := lonDiff + 360;
  lonFraction := lonDiff / 360;
  latZoom := zoom(MapRect.Height, WORLD_DIM, latFraction);
  lonZoom := zoom(MapRect.Width, WORLD_DIM, lonFraction);
  Result := Min(latZoom, lonZoom);
  Result := Min(Result, ZOOM_MAX);
end;

//  Convert from WGS84 projection to Web Mercator projection
function FromWGS84ToWebMercator(LatLng: TDoublePoint): TDoublePoint;
var
  A: Double;
begin
  if (Abs(LatLng.X) > 180) or (Abs(LatLng.Y) > 90)
  then Result := LatLng else
  begin
    Result.X := 6378137.0 * LatLng.X * 0.017453292519943295;
    A := Sin(LatLng.Y * 0.017453292519943295);
    Result.Y := 3189068.5 * Ln((1.0 + A) / (1.0 - A));
  end;
end;

//  Find the Manhattan distance between two points
function  ManhattanDistance(P1, P2: TDoublePoint): Double;
begin
  Result := Abs(P2.X - P1.X) + Abs(P2.Y - P1.Y);
end;

end.
