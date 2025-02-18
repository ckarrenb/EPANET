{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       maphiliter
 Description:  a class used to highlight a rectangle on a Canvas
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit maphiliter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, extctrls;

type
  THiliter = Class(TObject)
    Bitmap    : array [0..1] of TBitmap;
    Brect     : TRect;
    Hrect     : TRect;
    Hcanvas   : TCanvas;
    IsOn      : Boolean;
    Size      : Integer;
    Ibitmap   : Integer;
    Timer     : TTimer;

    procedure Show(C: TCanvas; R: TRect);
    procedure Hide;
    procedure Hilite;
    constructor Create(aTimer: TTimer);
    destructor  Destroy; override;

  end;

implementation

uses
  utils;

constructor THiliter.Create(aTimer: TTimer);
begin
  inherited Create;
  Bitmap[0] := TBitmap.Create;
  Bitmap[1] := TBitmap.Create;
  Timer := aTimer;
  Timer.Interval := 500;
  Timer.Enabled := false;
  Brect := Rect(0, 0, 0, 0);
  Hrect := Rect(0, 0, 0, 0);
  Hcanvas := nil;
  IsOn := false;
  Ibitmap := 0;
end;

destructor THiliter.Destroy;
begin
  Bitmap[0].Free;
  Bitmap[1].Free;
  inherited Destroy;
end;

procedure THiliter.Show(C: TCanvas; R: TRect);
var
  I: Integer;
begin
  if IsOn then Hide;
  Hcanvas := C;
  Hrect := R;
  for I := 0 to 1 do
    with Bitmap[I] do
    begin
      SetSize(R.Width, R.Height);
      Brect := Rect(0, 0, R.Width, R.Height);
      Canvas.CopyRect(Rect(0, 0, R.Width, R.Height), C, Hrect);
    end;
  utils.InvertRect(Bitmap[1].Canvas, Brect, clBlack);
  Ibitmap := 1;
  Timer.Enabled := true;
  Hilite;
  IsOn := true;
end;

procedure THiliter.Hide;
begin
  if IsOn then
  begin
    Timer.Enabled := false;
    Hcanvas.CopyRect(Hrect, Bitmap[0].Canvas, Brect);
    Hrect := Rect(0,0,0,0);
    IsOn := False;
    Ibitmap := 0;
  end;
end;

procedure THiliter.Hilite;
begin
  if Hcanvas <> nil then
  begin
    Hcanvas.CopyRect(Hrect, Bitmap[Ibitmap].Canvas, Brect);
    Ibitmap := 1 - Ibitmap;
  end;
end;

end.

