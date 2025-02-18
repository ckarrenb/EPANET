{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       energycalc
 Description:  calculates an energy balance for the project
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit energycalc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

const
  eInflows  = 1;
  ePumping  = 2;
  eTankOut  = 3;
  eDemands  = 4;
  eLeakage  = 5;
  eFriction = 6;
  eTankIn   = 7;
  eMinUse   = 8;

var
  Energy : array[eInflows..eMinUse] of Double;
  Preq : Single;
  PreqStr : String;
  Tsum : Integer;

  procedure Start;
  procedure Update(T: Integer; Dt: Integer);
  procedure Finish;

implementation

uses
  project, epanet2;

const
  SECperHR  = 3600;
  SECperDAY = 86400;
  MperFT = 0.3048;
  QperCFS : array[EN_CFS .. EN_CMS] of Double =
    (1.0,
    448.831,     // GPMperCFS
    0.64632,     // MGDperCFS
    0.5382,      // IMGDperCFS
    1.9837,      // AFDperCFS
    28.317,      // LPSperCFS
    1699.0,      // LPMperCFS
    2.4466,      // MLDperCFS
    101.94,      // CMHperCFS
    2446.6,      // CMDperCFS
    0.028317);   // CMSperCFS
var
  SpGrav : Single;

procedure Start;
var
  I, DmndModel: Integer;
  Pmin, Pexp, Punits: Single;
begin
  Tsum := 0;
  for I := eInflows to eMinuse do Energy[I] := 0;
  ENgetdemandmodel(DmndModel, Pmin, Preq, Pexp);
  ENgetoption(EN_PRESS_UNITS, Punits);
  ENgetoption(EN_SP_GRAVITY, SpGrav);
  PreqStr := Format('%.1f', [Preq]);
  case Round(Punits) of
  EN_PSI:
    begin
      Preq := Preq / 0.4333;      // Psi -> Feet
      PreqStr := PreqStr + ' psi';
    end;
  EN_KPA:
    begin
      Preq := Preq / 9.8066;      // Kpa -> Meters
      PreqStr := PreqStr + ' kpa';
    end;
  else PreqStr := PreqStr + ' m';
  end;
  Preq := Preq / SpGrav;
end;

procedure UpdateNodeEnergy(Dt: Integer);
var
  I, N : Integer;
  Qd, Ql, H, El : Single;
begin
  N := project.GetItemCount(cNodes);
  for I := 1 to N do
  begin
    ENgetnodevalue(I, EN_ELEVATION, El);
    ENgetnodevalue(I, EN_HEAD, H);
    ENgetnodevalue(I, EN_DEMAND, Qd);
    ENgetnodevalue(I, EN_LEAKAGEFLOW, Ql);
    Qd := Qd - Ql;

    case project.GetNodeType(I) of
    nJunction:
      begin
        if Qd > 0 then
        begin
          Energy[eDemands] := Energy[eDemands] + (Qd * H * Dt);
          Energy[eMinUse] := Energy[eMinUse] + (Qd * (El + Preq) * Dt);
        end
        else
          Energy[eInflows] := Energy[eInflows] - (Qd * H * Dt);
        if Ql > 0 then
          Energy[eLeakage] := Energy[eLeakage] + (Ql * H * Dt)
        else
          Energy[eInflows] := Energy[eInflows] - (Ql * H * Dt);
      end;

    nReservoir: // Qd < 0 for outflow, > 0 for inflow
      begin
        if Qd < 0 then
          Energy[eInflows] := Energy[eInflows] - (Qd * H * Dt)
        else
          Energy[eDemands] := Energy[eDemands] + (Qd * H * Dt);
      end;

    nTank: // Qd > 0 for inflow, < 0 for outflow
      if Qd > 0 then
        Energy[eTankIn] := Energy[eTankIn] + (Qd * H * Dt)
      else
        Energy[eTankOut] := Energy[eTankOut] - (Qd * H * Dt);
    end;
  end;
end;

procedure UpdateLinkEnergy(Dt: Integer);
var
  I, N, N1, N2 : Integer;
  Q : Single;
  H1, H2, P : Single;
begin
  N := project.GetItemCount(cLinks);
  for I := 1 to N do
  begin
    project.GetLinkNodes(I, N1, N2);
    ENgetlinkvalue(I, EN_FLOW, Q);
    ENgetnodevalue(N1, EN_HEAD, H1);
    ENgetnodevalue(N2, EN_HEAD, H2);
    P := Abs((H1 - H2) * Q) * Dt;
    if project.GetLinkType(I) = lPump then
      Energy[ePumping] := Energy[ePumping] + P
    else
      Energy[eFriction] := Energy[eFriction] + P;
  end;
end;

procedure Update(T: Integer; Dt: Integer);
begin
  if Dt = 0 then
  begin
    if T = 0 then Dt := SECperDAY else exit;
  end;
  UpdateNodeEnergy(Dt);
  UpdateLinkEnergy(Dt);
  Tsum := Tsum + Dt;
end;

procedure Finish;
var
  I : Integer;
  Ecf, Tcf : Double;
begin
  // Ecf converts H*Q from user units to (ft)*(gpm)/5310 = kw
  Ecf := QperCFS[EN_GPM] / QperCFS[project.FlowUnits] / 5310;
  if project.GetUnitsSystem = usSI then Ecf := Ecf / MperFt;

  // Tcf adjusts from kw-sec to kw-hr/day
  Tcf := SECperDAY / SECperHR / Tsum;

  // Energy used by each category in kwh/day
  for I := eInflows to eMinUse do
    Energy[I] := Ecf * Tcf * Energy[I];
end;

end.

