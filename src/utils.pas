{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       utils
 Description:  contains various utility functions
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Dialogs, Math, Graphics, Controls, ComCtrls,
  GraphType, IntfGraphics, LCLType, LCLProc,  LCLIntf, DateUtils, System.UItypes,
  FPImage, fphttpclient, mapcoords;

procedure AutoScale(var Zmin: Double; var Zmax: Double; var T: Double);
procedure BrightenBitmap(Bitmap: TBitmap; Brightness: Integer);
procedure DrawDottedLine(C: TCanvas; P1: TPoint; P2: TPoint);

function  FindTreeNode(aTreeView: TTreeView; Text: string): TTreeNode;
function  Float2Str(const X: Double; const N: Integer): string;
procedure GetTextSize(const aText: string; aFont: TFont; var H, W: Integer);
procedure GrayscaleBitmap(Bitmap: TBitmap);

function  HasInternetConnection: Boolean;
function  Haversine(X1, Y1, X2, Y2: Double): Double;
procedure InvertRect(C: TCanvas; const R: TRect; AColor: TColor);

function  MsgDlg(const Msg: string; DlgType: TMsgDlgType;
          Buttons: TMsgDlgButtons): Integer; overload;
function  MsgDlg(const Msg: string; DlgType: TMsgDlgType;
          Buttons: TMsgDlgButtons; F: TForm): Integer; overload;

function  PointInPolygon(const P: TDoublePoint; const Bounds: TDoubleRect;
          const Npts: Integer; Poly: TPolygon): Boolean;
function  PointOnLine(const P1: TPoint; const P2: TPoint;
          const P: TPoint; const Ptol: Integer): Boolean;
procedure ResizeControl(aControl:TControl;
          const ParentWidth, ParentHeight: Integer;
          const WidthRatio, HeightRatio: Integer;
          const WidthOffset, HeightOffset: Integer);

function  Str2Float(const S: string; var X: Single): Boolean; overload;
function  Str2Float(const S: string; var X: Double): Boolean; overload;
function  Str2Seconds(S: string): Integer;
procedure SwapListBoxLines(aListBox: TCustomListBox; Direction: Integer);
function  Time2Str(T: Integer): string;

implementation

function TaskDlg(const Title: String; const Msg: String; DlgType: TMsgDlgType;
          DlgButtons: TMsgDlgButtons; F: TForm): Integer; forward;

//  Find a nice scaling between Zmin and Zmax at intervals of T.
procedure AutoScale(var Zmin: Double; var Zmax: Double; var T: Double);
var
  m        : Integer;
  z        : Longint;
  d, z1, z2: Double;
begin
  z1 := Zmin;
  z2 := Zmax;
  try
    d := Abs(Zmax-Zmin);
    if (d = 0.0) and (Zmin = 0.0) then
    begin
      Zmin := -1.0;
      Zmax := 1.0;
      T := 1.0;
      Exit;
    end
    else if d < 0.01 then
    begin
      Zmin := Zmin - 0.5*Abs(Zmin);
      Zmax := Zmax + 0.5*Abs(Zmax);
    end;
    d := Abs(Zmax - Zmin);
    m := Trunc(Ln(d)/Ln(10.0));
    T := IntPower(10.,m);
    if T > 0.5*d then T := 0.2*T
    else if T > 0.2*d then T := 0.5*T;
    z := Trunc(Zmax/T) + 1;
    Zmax := z*T;
    z := Trunc(Zmin/T);
    if Zmin < 0 then z := z - 1;
    Zmin := z*T;
    if Zmin = Zmax then Zmax := Zmin + T;
    if Abs(Zmin-Zmax)/T > 10.0 then T := 2.0*T;
  except
    Zmin := z1;
    Zmax := z2;
    T := z2 - z1;
  end;
end;

// Brighten a bitmap image.
procedure BrightenBitmap(Bitmap: TBitmap; Brightness: Integer);
var
  X, Y: Integer;
  R, G, B: Byte;
  C: TColor;
  Fpc: TFPColor;
  IntfImage: TLazIntfImage;
begin
  IntfImage := Bitmap.CreateIntfImage;
  try
    for Y := 0 to IntfImage.Height-1 do
    begin
      for X := 0 to IntfImage.Width-1 do
      begin
        Fpc := IntfImage.Colors[X, Y];
        C := FPColorToTColor(Fpc);
        R := Red(C);
        G := Green(C);
        B := Blue(C);
        R := R + (Brightness*(255 - R) div 100);
        R := Min(255, R);
        G := G + (Brightness*(255 - G) div 100);
        G := Min(255, G);
        B := B + (Brightness*(255 - B) div 100);
        B := Min(255, B);
        C := RGBToColor(R, G, B);
        IntfImage.Colors[X, Y] := TColorToFPColor(C);
      end;
    end;
    Bitmap.LoadFromIntfImage(IntfImage);
  finally
    IntfImage.Free;
  end;
end;

// Draw a dotted line between points P1 and P2 on canvas C.
procedure DrawDottedLine(C: TCanvas; P1: TPoint; P2: TPoint);
begin
  C.Pen.Mode := pmNotXor; //pmXor;
  C.Pen.Style := psDot;
  C.Pen.Width := 2;
  C.MoveTo(P1.X, P1.Y);
  C.LineTo(P2.X, P2.Y);
  C.Pen.Width := 1;
  C.Pen.Style := psSolid;
  C.Pen.Mode := pmCopy;
end;

// Find the node of a TreeView with given text.
function FindTreeNode(aTreeView: TTreeView; Text: string): TTreeNode;
var
  TreeNode: TTreeNode;
begin
  Result := nil;
  TreeNode := aTreeView.Items[0];
  while TreeNode <> nil do
  begin
    if SameText(TreeNode.Text, Text) then
    begin
      Result := TreeNode;
      break;
    end;
    TreeNode := TreeNode.GetNext;
  end;
end;

// Convert a floating point number X to a string with N decimal places.
function Float2Str(const X: Double; const N: Integer): String;
begin
  Result := Format('%*.*f', [0, N, X]);
end;

// Find height and width of a string using a given font.
procedure GetTextSize(const aText: string; aFont: TFont; var H, W: Integer);
var
  bmp: Graphics.TBitmap; // Graphics.TBitmap, not Windows.TBitmap
begin
  bmp := Graphics.TBitmap.Create;
  try
    bmp.Canvas.Font.Assign(aFont);
    H := bmp.Canvas.TextHeight(aText);
    W := bmp.Canvas.TextWidth(aText);
  finally
    bmp.Free;
  end;
end;

//  Convert a bitmap to grayscale.
procedure GrayscaleBitmap(Bitmap: TBitmap);
var
  X, Y: Integer;
  R, G, B, MonoByte: Byte;
  C: TColor;
  Fpc: TFPColor;
  IntfImage: TLazIntfImage;
begin
  IntfImage := Bitmap.CreateIntfImage;
  try
    for Y := 0 to IntfImage.Height-1 do
    begin
      for X := 0 to IntfImage.Width-1 do
      begin
        Fpc := IntfImage.Colors[X, Y];
        C := FPColorToTColor(Fpc);
        R := Red(C);
        G := Green(C);
        B := Blue(C);
        MonoByte := Round(0.2125 * R + 0.7154 * G + 0.0721 * B);
        C := RGBToColor(MonoByte, MonoByte, MonoByte);
        IntfImage.Colors[X, Y] := TColorToFPColor(C);
      end;
    end;
    Bitmap.LoadFromIntfImage(IntfImage);
  finally
    IntfImage.Free;
  end;
end;

//  Check if an internet connection exists.
function HasInternetConnection: Boolean;
begin
  Result := False;
  try
    TFPHttpClient.SimpleGet('https://example.com');
    Result := True;
  except
  end;
end;

// Find distance in meters between two points on a spherical earth
// given their longitudes (X) and latitudes (Y) in decimal degrees.
function Haversine(X1, Y1, X2, Y2: Double): Double;
var
  P, Dy, Dx, SinDy, SinDx, A, C: Double;
begin
  P := PI / 180.;   //degrees to radians
  Dy := (Y2 - Y1) * P;
  Dx := (X2 - X1) * P;
  SinDy := Sin(Dy/2);
  SinDx := Sin(Dx/2);
  A := (SinDy*SinDy) + (Cos(Y1*P)*Cos(Y2*P)*(SinDx*SinDx));
  C := 2 * ArcTan2(Sqrt(A), Sqrt(1-A));
  Result := 6371 * 1000 * C;   //6371 = avg. radius of earth in km
end;

// Invert the colors of a rectangular region R on canvas C.
procedure InvertRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  X: integer;
  AM: TAntialiasingMode;
  TmpPen: TPen;
begin
  TmpPen:= TPen.Create;
  TmpPen.Assign(C.Pen);

  AM:= C.AntialiasingMode;

  X:= (R.Left+R.Right) div 2;
  C.Pen.Mode:= pmNotXor;
  C.Pen.Style:= psSolid;
  C.Pen.Color:= AColor;
  C.AntialiasingMode:= amOff;
  C.Pen.EndCap:= pecFlat;
  C.Pen.Width:= R.Right-R.Left;

  C.MoveTo(X, R.Top);
  C.LineTo(X, R.Bottom);

  C.Pen.Assign(TmpPen);
  C.AntialiasingMode:= AM;
  C.Rectangle(0, 0, 0, 0);
  TmpPen.Free;
end;

// Display a message dialog in center of currently active form.
function  MsgDlg(const Msg: string; DlgType: TMsgDlgType;
          Buttons: TMsgDlgButtons): Integer; overload;
begin
  //DlgType := mtCustom;
  //Result :=  MsgDlg(Msg, DlgType, Buttons, Screen.ActiveForm);
  Result := TaskDlg('', Msg, DlgType, Buttons, Screen.ActiveForm);
end;

//  Display a message dialog in center of a specific form.
function  MsgDlg(const Msg: string; DlgType: TMsgDlgType;
          Buttons: TMsgDlgButtons; F: TForm): Integer; overload;
//var
//  R: TRect;
begin
  //DlgType := mtCustom;
  Result := TaskDlg('', Msg, DlgType, Buttons, F);
{
  if not Assigned(F) then
    Result := MessageDlg(Msg, DlgType, Buttons, 0)
  else begin
    Result := 0;
    R := Rect(F.Left, F.Top, F.Left + F.Width, F.Top + F.Height);
    with CreateMessageDialog(Msg, DlgType, Buttons) do
    try
      Position := poDesigned;
      Left := R.Left + ((R.Right - R.Left) div 2) - (Width div 2);
      Top := R.Top + ((R.Bottom - R.Top) div 2) - (Height div 2);
      Result := ShowModal;
    finally
      Free;
    end;
  end;
}
end;

// Determine if point P is contained in polygon Poly with Npts number of
// vertices and bounding rectangle Bounds.
function  PointInPolygon(const P: TDoublePoint; const Bounds: TDoubleRect;
          const Npts: Integer; Poly: TPolygon): Boolean;
//
// Adapted from https://wrfranklin.org/Research/Short_Notes/pnpoly.html.
//
// TDoublePoint, TDoubleRect, and TPolygon are defined in mapcoods.pas.
//
var
  I, J: Integer;
  T: Double;
begin
  Result := False;
  if P.X < Bounds.LowerLeft.X then exit;
  if P.Y < Bounds.LowerLeft.Y then exit;
  if P.X > Bounds.UpperRight.X then exit;
  if P.Y > Bounds.UpperRight.Y then exit;

  I := 0;
  J := Npts - 1;
  while I < Npts do
  begin
    if (Poly[I].Y > P.Y) <> (Poly[J].Y > P.Y) then
    begin
      T := (Poly[J].X - Poly[I].X) * (P.Y - Poly[I].Y) /
           (Poly[J].Y - Poly[I].Y) + Poly[I].X;
      if P.X < T then Result := not Result;
    end;
    J := I;
    Inc(I);
  end;
end;

// Check if the point P is within Ptol distance of the line between P1 & P2.
function PointOnLine(const P1: TPoint; const P2: TPoint;
          const P: TPoint; const Ptol: Integer): Boolean;
var
  dx,  dy  : Integer;
  dx1, dy1 : Integer;
  a, b, c  : Integer;
begin
  Result := False;
  dx := P2.X - P1.X;
  dy := P2.Y - P1.Y;
  dx1 := P.X - P1.X;
  dy1 := P.Y - P1.Y;
  if (Abs(dx) > 0) and (Abs(dy) < Abs(dx)) then
  begin
    if (dx*dx1 >= 0) and (Abs(dx1) <= Abs(dx)) then
    begin
      a := (dy*dx1);
      b := (dx*dy1);
      c := Abs(dx*Ptol);
      if Abs(a-b) <= c then Result := True;
    end;
  end
  else if Abs(dy) > 0 then
  begin
    if (dy*dy1 >= 0) and (Abs(dy1) <= Abs(dy)) then
    begin
      a := (dx*dy1);
      b := (dy*dx1);
      c := Abs(dy*Ptol);
      if Abs(a-b) <= c then Result := True;
    end;
  end;
end;

// Resize a control to maintain a 2:1 width to height ratio
// (WidthRatio and HeightRatio are percentages)
procedure ResizeControl(aControl:TControl;
          const ParentWidth, ParentHeight: Integer;
          const WidthRatio, HeightRatio: Integer;
          const WidthOffset, HeightOffset: Integer);
var
  MinWidth, MinHeight: Integer;
begin
  // Adjust control's width and left position
  MinWidth := ParentWidth - WidthOffset;
  aControl.Width := (ParentWidth * WidthRatio) div 100;
  aControl.Width := Min(MinWidth, aControl.Width);
  aControl.Left := (ParentWidth - aControl.Width) div 2;

  // Adjust control's height and top position
  aControl.Top := HeightOffset;
  MinHeight := ParentHeight;
  aControl.Height := Min(MinHeight, (aControl.Width * HeightRatio) div 100);
end;

// Convert string S to number X returning False if conversion fails.
function  Str2Float(const S: String; var X: Single): Boolean; overload;
begin
  X := 0;
  Result := True;
  try
    X := StrToFloat(S);
  except
    On EConvertError do Result := False;
  end;
end;

function  Str2Float(const S: string; var X: Double): Boolean; overload;
begin
  X := 0;
  Result := True;
  try
    X := StrToFloat(S);
  except
    On EConvertError do Result := False;
  end;
end;

// Convert time in Hours:Mins:Secs to seconds
function  Str2Seconds(S: String): Integer;
var
  T: TDateTime;
begin
  try
    // If no ':' separator then string is a decimal number
    if (Pos(':', S) = 0) then
      Result := Round(StrToFloat(S) * 3600)
    else begin
      T := StrToTime(S);
      Result := (HourOf(T)*60*60) + (MinuteOf(T)*60) + SecondOf(T);
    end;
  except
    on EConvertError do Result := -1;
  end;
end;

// Exchange the position of the selected entry in a list box.
procedure SwapListBoxLines(aListBox: TCustomListBox; Direction: Integer);
var
  i, k: Integer;
begin
  if Direction < 0 then k := -1 else k := 1;
  i := aListBox.ItemIndex;
  if (k < 0) and (i <= 0) then exit;
  if (k > 0) and (i >= aListBox.Items.Count-1) then exit;
  aListBox.Items.Exchange(i, i+k);
  aListBox.Selected[i+k] := True;
  aListBox.ItemIndex := i + k;
end;

// Display a message in a custom Task Dialog form.
function TaskDlg(const Title: String; const Msg: String; DlgType: TMsgDlgType;
          DlgButtons: TMsgDlgButtons; F: TForm): Integer;
var
  TD: TTaskDialog;
begin
  TD := TTaskDialog.Create(F);
  try
    TD.Caption := 'EPANET';  //The dialog window caption
//    TD.Title := Msg;       //The large blue text
    TD.Text := Msg;          //The smaller black text
    TD.CommonButtons := [tcbOk];
    TD.DefaultButton := tcbOk;
    TD.Flags := [tfPositionRelativeToWindow];
    if DlgType = mtError then TD.MainIcon := tdiError
    else if DlgType = mtInformation then TD.MainIcon := tdiInformation
    else if DlgType = mtConfirmation then
    begin
      TD.MainIcon := tdiQuestion;
      TD.CommonButtons := [];
      if mbOK in DlgButtons then TD.CommonButtons := TD.CommonButtons + [tcbOk];
      if mbYes in DlgButtons then TD.CommonButtons := TD.CommonButtons + [tcbYes];
      if mbNo in DlgButtons then TD.CommonButtons := TD.CommonButtons + [tcbNo];
      if mbCancel in DlgButtons then TD.CommonButtons := TD.CommonButtons + [tcbCancel];
    end
    else TD.MainIcon := tdiNone;
    if TD.Execute then Result := TD.ModalResult;
  finally
    TD.Free;
  end;
end;

// Convert time in seconds to an Hours:Mins string.
function Time2Str(T: Integer): String;
var
  H: Integer;
  M: Integer;
  S: Integer;
begin
  H := T div 3600;
  M := (T - H * 3600) div 60;
  S := T - (H * 3600) - (M * 60);
  if S = 0 then
    Result := Format('%.2d:%.2d', [H,M])
  else
    Result := Format('%.2d:%.2d:%.2d', [H,M,S])
end;

end.

