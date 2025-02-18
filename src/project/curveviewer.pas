{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       curveviewer
 Description:  displays an EPANET Data Curve in a TChart component
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit curveviewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, TAGraph, TASeries, Math;

procedure PlotCurve(aChart: TChart; CurveType: Integer;
  X, Y: array of double; N: Integer);

implementation

uses
  project, utils;

const
  TINY: Double = 1.e-6;

  XLabel: array[0..5] of string =
    ('Depth', 'Flow', 'Flow', 'Flow', 'X', '% Open');

  YLabel: array[0..5] of string =
    ('Volume', 'Head', 'Efficiency', 'Head Loss', 'Y', '% Full Flow');

var
  DataSeries: TLineSeries;

function FindPumpCurveCoeffs(Q: array of Double; H: array of Double;
  var A: Double; var B: Double; var C: Double): Boolean;
var
  H4, H5: Double;
begin
  Result := false;
  if (H[0] < TINY) or (H[0] - H[1] < TINY) or (H[1] - H[2] < TINY) or
      (Q[1] < TINY) or (Q[2] - Q[1] < TINY) then exit;

  A  := H[0];
  H4 := H[0] - H[1];
  H5 := H[0] - H[2];
  C  := Ln(H5 / H4) / Ln(Q[2] / Q[1]);
  if (C <= 0.0) or (C > 20.0) then exit;
  B := -H4 / Q[1]**C;
  if B >= 0.0 then exit;
  Result := true;
end;

function AddPumpCurvePts(Q: array of Double; H: array of Double):
  Boolean;
var
  Dx : Double;
  A, B, C, Qmax, X, Y: Double;
  I  : Integer;
begin
  Result := true;
  A := 0; B := 0; C := 0;
  if not FindPumpCurveCoeffs(Q, H, A, B, C) then
  begin
    Utils.MsgDlg('Cannot compute a valid pump curve.', mtError, [mbOK]);
    Result := false;
  end
  else begin
    Qmax := (-A/B)**(1/C);
    Dx := Qmax / 25.;
    with DataSeries do
    begin
      ShowPoints := False;
      AddXY(0.0, A);
      X := 0.0;
      for I := 1 to 24 do
      begin
        X := X + Dx;
        Y := A + B * X ** C;
        AddXY(X, Y);
      end;
      AddXY(Qmax, 0.0);
    end;
  end;
end;

function Plot1PointPumpCurve(X0, Y0: Double): Boolean;
var
  H: array[0..2] of Double;
  Q: array[0..2] of Double;
begin
  Q[1] := X0;
  H[1] := Y0;
  Q[0] := 0;
  H[0] := 1.33334 * H[1];
  Q[2] := 2 * Q[1];
  H[2] := 0;
  Result := AddPumpCurvePts(Q, H);
end;

function Plot3PointPumpCurve(X, Y: array of Double): Boolean;
var
  H: array[0..2] of Double;
  Q: array[0..2] of Double;
  I: Integer;
begin
  for I := 0 to 2 do
  begin
    Q[I] := X[I];
    H[I] := Y[I];
  end;
  Result := AddPumpCurvePts(Q, H);
end;

procedure FindEfficCurveCoeffs(Qstar: Double; Estar: Double;
  var Qmax: Double; var A: Double; var B: Double; var C: Double);
var
  Denom, Qstar2: Double;
begin
  while (Qstar > 2.0/3.0 * Qmax) do
      Qmax := 1.55 * Qstar;
  Qstar2 := Qstar * Qstar;
  Denom := -Qstar2 * (Qmax - Qstar) * (Qmax - Qstar);
  A := (Estar * (2.0 * Qstar - Qmax)) / Denom;
  B := -Estar * (3.0 * Qstar2 - (Qmax * Qmax)) / denom;
  C := (Estar * Qstar * Qmax * (3.0 * Qstar - 2.0 * Qmax)) / denom;
end;

procedure AddEfficCurvePts(Qmax: Double; A: Double; B: Double;
  C: Double);
var
  X, Y, Dx: Double;
  I: Integer;
begin
  Dx := Qmax / 25.;
  with DataSeries do
  begin
    ShowPoints := False;
    AddXY(0.0, 0.0);
    X := 0.0;
    for I := 1 to 24 do
    begin
      X := X + Dx;
      Y := (C + X * (B + A * X)) * X;
      AddXY(X, Y);
    end;
    AddXY(Qmax, 0.0);
  end;
end;

function PlotEfficCurve(X, Y: array of Double; N: Integer): Boolean;
var
  Qstar, Estar, Qmax: Double;
  A: Double = 0;
  B: Double = 0;
  C: Double = 0;
begin
  Result := false;
  if N = 1 then
  begin
    if (X[0] = 0) and (Y[0] = 0) then exit;
    Qstar := X[0];
    Estar := Y[0];
    Qmax := 2 * Qstar;
  end
  else if N = 2 then
  begin
    if (X[0] = 0) and (Y[0] = 0) and
       (X[1] <> 0) and (Y[1] <> 0) then
    begin
      Qstar := X[1];
      Estar := Y[1];
      Qmax := 2 * Qstar;
    end
    else if (X[0] <> 0) and (Y[0] <> 0) and (Y[1] = 0) then
    begin
      Qstar := X[0];
      Estar := Y[0];
      Qmax := X[1];
    end
    else exit;
  end
  else if N = 3 then
  begin
    if (X[0] <> 0) or (Y[2] <> 0) then exit;
    Qstar := X[1];
    Estar := Y[1];
    Qmax := X[2];
  end
  else exit;
  FindEfficCurveCoeffs(Qstar, Estar, Qmax, A, B, C);
  AddEfficCurvePts(Qmax, A, B, C);
  Result := true;
end;

procedure PlotCurve(aChart: TChart; CurveType: Integer;
  X, Y: array of double; N: Integer);
var
  I: Integer;
  FunctionPlot: Boolean;
begin
  // Add points to the chart's LineSeries
  if not aChart.Series[0].ClassNameIs('TLineSeries') then exit;
  DataSeries := TLineSeries(aChart.Series[0]);
  with DataSeries do
  begin
    Clear;
    Active := false;
    BeginUpdate;
    FunctionPlot := false;

    // Plot functional pump curve
    if (CurveType = ctPump) and (N = 1) then
      FunctionPlot := Plot1PointPumpCurve(X[0], Y[0])
    else if (CurveType = ctPump) and (N = 3) and (X[0] = 0) then
      FunctionPlot := Plot3PointPumpCurve(X, Y)

    // Plot functional efficiency curve
    else if (CurveType = ctEffic) and (N <= 3) then
      FunctionPlot := PlotEfficCurve(X, Y, N);

    // Plot piece-wise linear curve
    if not FunctionPlot then for I := 0 to N - 1 do
        AddXY(X[I], Y[I], '');

    EndUpdate;
    Active := True;
  end;
end;

end.

