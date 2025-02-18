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

{
  This unit contains a TProjTransform class used to transform
  geospatial coordinates from one coordinate reference system
  (CRS) to another. It uses:
    1. the Proj.4 library whose Pascal API declarations are
       contained in proj.pas.
    2. the restclient.pas unit used to make a web request for
       the Proj.4 projection string for a given CRS EPSG code.

  Typical usage to transform coordinates X and Y from a
  coordinate system with EPSG code SrcEPSG to one with code DstEPSG:

  function Transform(SrcEPSG, DstEPSG: String; var X, Y: Double): Boolean;
  var
    ProjTrans: TProjTransform;
  begin
    Result := False;
    ProjTrans := TProjTransform.Create;
    try
      if not ProjTrans.SetProjections(SrcEPSG, DstEPSG) then exit;
      ProjTrans.Transform(X, Y);
      Result := True;
    finally
      ProjTrans.Free;
    end;
  end;
==================================================================}

unit projtransform;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, restclient, proj;

type
  TProjTransform = class(TObject)
    private
      SrcProj, DstProj: proj.ProjHandle;
      IsSrcLatLong, IsDstLatLong: Integer;
      function GetProjHandle(EPSGcode: String): proj.ProjHandle;
    public
      constructor Create;
      destructor Destroy; override;
      function SetProjections(SrcEPSG, DstEPSG: String): Boolean;
      function Transform(var X, Y: Double): Boolean;
  end;
implementation

constructor TProjTransform.Create;
begin
  inherited Create;
  SrcProj := 0;
  DstProj := 0;
  IsSrcLatLong := 0;
  IsDstLatLong := 0;
end;

destructor TProjTransform.Destroy;
begin
  if SrcProj <> 0 then proj.pj_free(SrcProj);
  if DstProj <> 0 then proj.pj_free(DstProj);
  inherited Destroy;
end;

function TProjTransform.GetProjHandle(EPSGcode: String): proj.ProjHandle;
var
  Url: String;
  ProjStr: String;
begin
  // Obtain the projection's string from its EPSG code
  ProjStr := '';
  Url := 'https://epsg.io/' + EPSGcode + '.proj4';
  try
    restclient.GetString(Url, ProjStr);
    Result := proj.pj_init_plus(PAnsiChar(ProjStr));
  except
    On E: Exception do
      Result := 0;
  end;
end;

function TProjTransform.SetProjections(SrcEPSG, DstEPSG: String): Boolean;
begin
  // Free current source & destination projection handles
  Result := False;
  if SrcProj <> 0 then proj.pj_free(SrcProj);
  if DstProj <> 0 then proj.pj_free(DstProj);

  // Create handle for source projection
  SrcProj := GetProjHandle(SrcEPSG);
  if SrcProj <> 0 then
    IsSrcLatLong := proj.pj_is_latlong(SrcProj)
  else exit;

  // Create handle for destination projection
  DstProj := GetProjHandle(DstEPSG);
  if DstProj <> 0 then
    IsDstLatLong := proj.pj_is_latlong(DstProj)
  else exit;
  Result := True;
end;

function TProjTransform.Transform(var X, Y: Double): Boolean;
var
  Z: Double = 0;
begin
  Result := False;
  if (SrcProj = 0) or (DstProj = 0) then exit;
  if IsSrcLatLong = 1 then
  begin
    X := X * DEG_TO_RAD;
    Y := Y * DEG_TO_RAD;
  end;
  if proj.pj_transform(SrcProj, DstProj, 1, 1, X, Y, Z) <> 0 then exit;
  if IsDstLatLong = 1 then
  begin
    X := X * RAD_TO_DEG;
    Y := Y * RAD_TO_DEG;
  end;
  Result := True;
end;

end.

