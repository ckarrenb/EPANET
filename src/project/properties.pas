{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       properties
 Description:  retrieves property values of a project's objects
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit properties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Dialogs;

{$I ..\timetype.txt}

const
  HydOptionsProps: array[1..13] of String =
    ('Flow Units', 'Head Loss Model', 'Sp. Gravity', 'Sp. Viscosity',
     'Maximum Trials', 'Accuracy', 'Head Tolerance', 'Flow Tolerance',
     'If Unbalanced', 'Status Reporting',
     'CHECKFREQ', 'MAXCHECK', 'DAMPLIMIT');

  DemandOptionsProps: array[1..8] of String =
    ('Demand Model', 'Default Pattern', 'Demand Multiplier',
     'Service Pressure', 'Minimum Pressure', 'Pressure Expon.',
     'Emitter Expon.', 'Emitter Backflow');

  QualOptionsProps: array[1..2] of String =
    ('Single-Species', 'Multi-Species');

  TimeOptionsProps: array[1..10] of String =
    ('Duration', 'Hydraulic Step', 'Quality Step', 'Pattern Step',
     'Pattern Start', 'Report Step', 'Report Start', 'Rule Step',
     'Clock Start', 'Statistic');

  EnergyOptionsProps: array[1..4] of String =
    ('Pump Efficiency (%)', 'Energy Price / kwh', 'Price Pattern',
     'Demand Charge');

  JunctionProps: array[1..16] of String =
    ('Junction ID', 'Description', 'Tag', 'Elevation', 'Base Demand',
     'Demand Pattern', 'Demand Categories', 'Emitter Coeff.',
     'Initial Quality', 'Source Quality', 'Actual Demand',
     'Demand Deficit', 'Emitter Demand', 'Leakage Demand',
     'Hydraulic Head', 'Pressure');

  FirstJuncResultIndex = 11;

  ReservoirProps: array[1..9] of String =
    ('Reservoir ID', 'Description', 'Tag', 'Elevation',
     'Elev. Pattern', 'Initial Quality', 'Source Quality',
     'Outflow Rate', 'Hydraulic Head');

  FirstResvResultIndex = 8;

  TankProps: array[1..19] of String =
     ('Tank ID', 'Description', 'Tag', 'Elevation', 'Initial Depth',
      'Minimum Depth', 'Maximum Depth', 'Diameter', 'Minimum Volume',
      'Volume Curve', 'Can Overflow', 'Mixing Model', 'Mixing Fraction',
      'Reaction Coeff.', 'Initial Quality', 'Source Quality',
      'Inflow Rate', 'Hydraulic Head', 'Water Depth');

  FirstTankResultIndex = 17;

  PipeProps: array[1..18] of String =
     ('Pipe ID', 'Start Node', 'End Node', 'Description', 'Tag', 'Length',
      'Diameter', 'Roughness', 'Loss Coeff.', 'Initial Status', 'Bulk Coeff.',
      'Wall Coeff.', 'Leak Area', 'Leak Expansion',
      'Flow', 'Velocity', 'Unit Head Loss', 'Leakage');

  FirstPipeResultIndex = 15;

  PumpProps: array[1..16] of String =
     ('Pump ID', 'Start Node', 'End Node', 'Description', 'Tag', 'Pump Curve',
      'Power', 'Speed', 'Speed Pattern', 'Initial Status', 'Effic. Curve',
      'Energy Price', 'Price Pattern', 'Flow', 'Velocity', 'Head Loss');

  FirstPumpResultIndex = 14;

  ValveProps: array[1..15] of String =
     ('Valve ID', 'Start Node', 'End Node', 'Description', 'Tag', 'Diameter',
      'Valve Type', 'Setting', 'Loss Coeff.', 'PCV Curve', 'GPV Curve',
      'Fixed Status', 'Flow', 'Velocity', 'Head Loss');

  FirstValveResultIndex = 13;

  LabelProps: array[1..4] of String =
     ('Text', 'Font', 'Rotation', 'Anchor Node');

procedure GetHydProps;
procedure GetDemandProps;
procedure GetQualProps;
procedure GetTimeProps;
procedure GetEnergyProps;
procedure GetJuncProps(Index: Integer);
procedure GetResvProps(Index: Integer);
procedure GetTankProps(Index: Integer);
procedure GetPipeProps(Index: Integer);
procedure GetPumpProps(Index: Integer);
procedure GetValveProps(Index: Integer);
procedure GetLabelProps(Item: Integer);

procedure PasteNodeProps(const Index: Integer; const NodeType: Integer);
procedure PasteLinkProps(const Index: Integer; const LinkType: Integer);

procedure AddNodeResults(Index: Integer);
procedure AddLinkResults(Index: Integer);

implementation

uses
  project, config, maplabel, mapthemes, utils, epanet2;

procedure GetHydProps;
var
  I: Integer = 0;
  X: Single = 0;
begin
  with Project.Properties do
  begin
    Clear;
    Add('');

    Epanet2.ENgetflowunits(I);
    Add(Project.FlowUnitsStr[I]);

    Epanet2.ENgetoption(EN_HEADLOSSFORM, X);
    Add(Project.HLossModelStr[Round(X)]);

    Epanet2.ENgetoption(EN_SP_GRAVITY, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetoption(EN_SP_VISCOS, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetoption(EN_TRIALS, X);
    Add(IntToStr(Round(X)));

    Epanet2.ENgetoption(EN_ACCURACY, X);
    Add(Float2Str(X, 8));

    Epanet2.ENgetoption(EN_HEADERROR, X);
    Add(Float2Str(X, 8));

    Epanet2.ENgetoption(EN_FLOWCHANGE, X);
    Add(Float2Str(X, 8));

    Epanet2.ENgetoption(EN_EXTRA_ITER, X);
    if (X < 0) then Add('Stop') else Add('Continue');

    //Add(Project.StatusRptStr[Project.StatusRptType]);
    Epanet2.ENgetoption(EN_STATUS_REPORT, X);
    Add(StatusRptStr[Round(X)]);

    Epanet2.ENgetoption(EN_CHECKFREQ, X);
    Add(IntToStr(Round(X)));

    Epanet2.ENgetoption(EN_MAXCHECK, X);
    Add(IntToStr(Round(X)));

    Epanet2.ENgetoption(EN_DAMPLIMIT, X);
    Add(Float2Str(X, 8));
  end;
end;

procedure GetQualProps;
var
  QualType: Integer = 0;
  ChemName: array[0..EN_MAXID] of AnsiChar;
  ChemUnits: array[0..EN_MAXID] of AnsiChar;
  TraceNodeIndex: Integer = 0;
  QualParam: AnsiString;
begin
  with Project.Properties do
  begin
    Clear;
    Add('');
    Epanet2.ENgetqualinfo(QualType, ChemName, ChemUnits, TraceNodeIndex);
    QualParam := Project.QualModelStr[QualType];
    if Length(MsxInpFile) > 0 then
    begin
      Add('No');
      Add('Yes');
    end
    else
    begin
      if QualType = 0 then
        Add('No')
      else
        Add(QualParam);
      Add('No');
    end;
  end;
end;

procedure GetDemandProps;
var
  X: Single = 0;
  DemandModel: Integer = 0;
  Pmin: Single = 0;
  Pmax: Single = 0;
  Pexp: Single = 0;
begin
  with Project.Properties do
  begin
    Clear;
    Add('');

    Epanet2.ENgetdemandmodel(DemandModel, Pmin, Pmax, Pexp);
    if Round(DemandModel) = 0 then Add('DDA') else Add('PDA');

    Epanet2.ENgetoption(EN_DEMANDPATTERN, X);
    Add(Project.GetID(cPatterns, Round(X)));

    Epanet2.ENgetoption(EN_DEMANDMULT, X);
    Add(Float2Str(X, 4));

    Add(Float2Str(Pmax, 4));
    Add(Float2Str(Pmin, 4));
    Add(Float2Str(Pexp, 4));

    Epanet2.ENgetoption(EN_EMITEXPON, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetoption(EN_EMITBACKFLOW, X);
    if Round(X) = 0 then Add('No') else Add('Yes');
  end;
end;

procedure GetTimeProps;
var
  I: Integer;
  T: TimeType = 0;
begin
  with Project.Properties do
  begin
    Clear;
    Add('');
    for I := EN_DURATION to EN_RULESTEP do
    begin
      Epanet2.ENgettimeparam(I, T);
      Add(Time2Str(T));
    end;
    Epanet2.ENgettimeparam(EN_STARTTIME, T);
    Add(Time2Str(T));
    Epanet2.ENgettimeparam(EN_STATISTIC, T);
    Project.Properties.Add(Project.StatisticStr[Round(T)]);
  end;
end;

procedure GetEnergyProps;
var
  X: Single = 0;
begin
  with Project.Properties do
  begin
    Clear;
    Add('');

    Epanet2.ENgetoption(EN_GLOBALEFFIC, X);
    Add(Float2Str(X, 2));

    Epanet2.ENgetoption(EN_GLOBALPRICE, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetoption(EN_GLOBALPATTERN, X);
    Add(Project.GetID(cPatterns, Round(X)));

    Epanet2.ENgetoption(EN_DEMANDCHARGE, X);
    Add(Float2Str(X, 4));
  end;
end;

procedure GetJuncProps(Index: Integer);
var
  I: Integer = 0;
  X: Single = 0;
begin
  with Project.Properties do
  begin
    Clear;
    Add('');

    Add(Project.GetID(cNodes, Index));
    Add(Project.GetComment(EN_NODE, Index));
    Add(Project.GetTag(EN_NODE, Index));

    Epanet2.ENgetnodevalue(Index, EN_ELEVATION, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_BASEDEMAND, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_PATTERN, X);
    Add(Project.GetID(cPatterns, Round(X)));

    Epanet2.ENgetnumdemands(Index, I);
    Add(IntToStr(I));

    Epanet2.ENgetnodevalue(Index, EN_EMITTER, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_INITQUAL, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_SOURCEQUAL, X);
    Add(Float2Str(X, 4));

    AddNodeResults(Index);
  end;
end;

procedure GetResvProps(Index: Integer);
var
  X: Single = 0;
begin
  with Project.Properties do
  begin
    Clear;
    Add('');

    Add(Project.GetID(cNodes, Index));
    Add(project.GetComment(EN_NODE, Index));
    Add(Project.GetTag(EN_NODE, Index));

    Epanet2.ENgetnodevalue(Index, EN_ELEVATION, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_PATTERN, X);
    Add(Project.GetID(cPatterns, Round(X)));

    Epanet2.ENgetnodevalue(Index, EN_INITQUAL, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_SOURCEQUAL, X);
    Add(Float2Str(X, 4));

    AddNodeResults(Index);
  end;
end;

procedure GetTankProps(Index: Integer);
var
  X: Single = 0;
begin
  with Project.Properties do
  begin
    Clear;
    Add('');
    Add(Project.GetID(cNodes, Index));
    Add(Project.GetComment(EN_NODE, Index));
    Add(Project.GetTag(EN_NODE, Index));

    Epanet2.ENgetnodevalue(Index, EN_ELEVATION, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_TANKLEVEL, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_MINLEVEL, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_MAXLEVEL, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_TANKDIAM, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_MINVOLUME, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_VOLCURVE, X);
    Add(Project.getID(cCurves, Round(X)));
    
    Epanet2.ENgetnodevalue(Index, EN_CANOVERFLOW, X);
    if (X = 1) then
      Add(Project.NoYesStr[1])
    else
      Add(Project.NoYesStr[0]);

    Epanet2.ENgetnodevalue(Index, EN_MIXMODEL, X);
    Add(Project.MixingModelStr[Round(X)]);

    Epanet2.ENgetnodevalue(Index, EN_MIXFRACTION, X);
    if X < 0 then X := 0;
    if X > 1 then X := 1;
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_TANK_KBULK, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_INITQUAL, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetnodevalue(Index, EN_SOURCEQUAL, X);
    Add(Float2Str(X, 4));

    AddNodeResults(Index);
  end;
end;

procedure AddNodeResults(Index: Integer);
//
//  Adds simulation results for a node to the list of property values
//  displayed in the ProjectFrame's PropertyEditor.
//
var
  I: Integer;
  J: Integer;
  X: Single;
begin
  // Get the type of node (nJunction, nReservoir, or nTank)
  J := project.GetNodeType(Index);

  // Loop through each result variable
  for I := mapthemes.FirstNodeResultTheme to
           mapthemes.FirstNodeQualTheme-1 do
  begin

    // Skip results that don't apply to non-Junction nodes
    if (J <> nJunction) and (I in [ntDmndDfct, ntEmittance, ntLeakage]) then
      continue;

    // Retrieve the result value
    X := mapthemes.GetNodeValue(Index, I, mapthemes.TimePeriod);

    // Add the value to the properties displayed in the Property Editor
    with project.Properties do
    begin
      if X = MISSING then Add('N/A') else
      begin
        // Convert Tank pressure value to a water depth in feet
        if (project.GetUnitsSystem = usUS) and  (J = nTank)
          and (I = ntPressure) then X := X / 0.4333

        // Convert Reservoir demand to an outflow
        else if (J = nReservoir) and (I = ntDemand) then X := -X;

        // Add the value as a string to the properties list
        Add(FloatToStrF(X, ffFixed, 7, config.DecimalPlaces));
      end;
    end;
  end;
end;

procedure GetPipeProps(Index: Integer);
var
  I: Integer = 0;
  J: Integer = 0;
  K: Integer;
  X: Single = 0;
begin
  with Project.Properties do
  begin
    Clear;
    Add('');

    Add(Project.GetID(cLinks, Index));
    Project.GetLinkNodes(Index, I, J);
    Add(Project.GetID(cNodes, I));
    Add(Project.GetID(cNodes, J));
    Add(Project.GetComment(EN_LINK, Index));
    Add(Project.GetTag(EN_LINK, Index));

    Epanet2.ENgetlinkvalue(Index, EN_LENGTH, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetlinkvalue(Index, EN_DIAMETER, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetlinkvalue(Index, EN_ROUGHNESS, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetlinkvalue(Index, EN_MINORLOSS, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetlinktype(Index, K);
    if K = EN_CVPIPE then
      K := 2  // index of CV in StatusStr
    else
    begin
      Epanet2.ENgetlinkvalue(Index, EN_INITSTATUS, X);
      K := Round(X);
    end;
    Add(Project.StatusStr[K]);

    Epanet2.ENgetlinkvalue(Index, EN_KBULK, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetlinkvalue(Index, EN_KWALL, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetlinkvalue(Index, EN_LEAK_AREA, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetlinkvalue(Index, EN_LEAK_EXPAN, X);
    Add(Float2Str(X, 4));

    AddLinkResults(Index);
  end;
end;

procedure GetPumpProps(Index: Integer);
var
  I: Integer = 0;
  J: Integer = 0;
  X: Single = 0;
begin
  with Project.Properties do
  begin
    Clear;
    Add('');

    Add(Project.GetID(cLinks, Index));
    Project.GetLinkNodes(Index, I, J);
    Add(Project.GetID(cNodes, I));
    Add(Project.GetID(cNodes, J));
    Add(Project.GetComment(EN_LINK, Index));
    Add(Project.GetTag(EN_LINK, Index));

    Epanet2.ENgetlinkvalue(Index, EN_PUMP_HCURVE, X);
    Add(Project.GetID(cCurves, Round(X)));

    Epanet2.ENgetlinkvalue(Index, EN_PUMP_POWER, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetlinkvalue(Index, EN_INITSETTING, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetlinkvalue(Index, EN_LINKPATTERN, X);
    Add(Project.GetID(cPatterns, Round(X)));

    Epanet2.ENgetlinkvalue(Index, EN_INITSTATUS, X);
    Add(Project.StatusStr[Round(X)]);

    Epanet2.ENgetlinkvalue(Index, EN_PUMP_ECURVE, X);
    Add(Project.GetID(cCurves, Round(X)));

    Epanet2.ENgetlinkvalue(Index, EN_PUMP_ECOST, X);
    Add(Float2Str(X, 4));

    Epanet2.ENgetlinkvalue(Index, EN_PUMP_EPAT, X);
    Add(Project.GetID(cPatterns, Round(X)));

    AddLinkResults(Index);
  end;
end;

procedure GetValveProps(Index: Integer);
var
  I: Integer = 0;
  J: Integer = 0;
  Status: Integer;
  X: Single = 0;
  Setting: Single = 0;
begin
  with Project.Properties do
  begin
    Clear;
    Add('');

    // Add valve ID, end nodes, comment & tag
    Add(Project.GetID(cLinks, Index));
    Project.GetLinkNodes(Index, I, J);
    Add(Project.GetID(cNodes, I));
    Add(Project.GetID(cNodes, J));
    Add(Project.GetComment(EN_LINK, Index));
    Add(Project.GetTag(EN_LINK, Index));

    // Add diameter
    Epanet2.ENgetlinkvalue(Index, EN_DIAMETER, X);
    Add(Float2Str(X, 4));

    // Add valve type
    Epanet2.ENgetlinktype(Index, I);
    Add(Project.ValveTypeStr[I-EN_PRV]);

    // Retrieve valve's setting and fixed status
    Epanet2.ENgetlinkvalue(Index, EN_INITSETTING, Setting);
    Epanet2.ENgetlinkvalue(Index, EN_INITSTATUS, X);
    Status := Round(X);

    // For GPV, Setting is index of head loss curve
    if I = EN_GPV then
    begin
      //Add(Project.GetID(cCurves, Round(Setting)));
      Add('');
      Status := 1;
    end
    // If setting is large negative number then status is fixed
    else
    begin
      if Setting = EN_MISSING then Setting := 0.0
      // Otherwise no fixed status
      else Status := 2;
      Add(Float2Str(Setting, 4));
    end;

    // Add minor loss coeff.
    Epanet2.ENgetlinkvalue(Index, EN_MINORLOSS, X);
    Add(Float2Str(X, 4));

    // Add PCV curve
    if I = EN_PCV then
    begin
      Epanet2.ENgetlinkvalue(Index, EN_PCV_CURVE, X);
      if X <= 0 then Add('')
      else Add(Project.GetID(cCurves, Round(X)));
    end
    else Add('');

    // Add GPV curve
    if I = EN_GPV then
    begin
      Epanet2.ENgetlinkvalue(Index, EN_GPV_CURVE, X);
      if X <= 0 then Add('')
      else Add(Project.GetID(cCurves, Round(X)));
    end
    else Add('');

    // Add valve fixed status
    Add(Project.ValveStatusStr[Status]);
    AddLinkResults(Index);
  end;
end;

procedure AddLinkResults(Index: Integer);
//
//  Adds simulation results for a link to the list of property values
//  displayed in the ProjectFrame's PropertyEditor.
//
var
  I: Integer;
  X: Single;
begin

  for I := MapThemes.FirstLinkResultTheme to
           MapThemes.FirstLinkQualTheme-1 do
  begin
    X := MapThemes.GetLinkValue(Index, I, MapThemes.TimePeriod);
    with Project.Properties do
    begin
      if X = MISSING then
        Add('N/A')
      else
        Add(FloatToStrF(X, ffFixed, 7, config.DecimalPlaces));
     end;
  end;
end;

procedure GetLabelProps(Item: Integer);
var
  MapLabel: TMapLabel;
begin
  MapLabel := TMapLabel(Project.MapLabels.Objects[Item]);
  with Project.Properties do
  begin
    Clear;
    Add('');
    Add(Project.MapLabels[Item]);
    Add('<Edit>');
    Add(IntToStr(MapLabel.Rotation));
    Add(MapLabel.AnchorNode);
  end;
end;

procedure PasteNodeProps(const Index: Integer; const NodeType: Integer);
var
  I: Integer;
  X: Single;
begin
  case NodeType of
  nJunction:
    begin
    if Utils.Str2Float(Project.CopiedProperties[4], X) then
      Epanet2.ENsetnodevalue(Index, EN_ELEVATION, X);
    if Utils.Str2Float(Project.CopiedProperties[5], X) then
      Epanet2.ENsetnodevalue(Index, EN_BASEDEMAND, X);
    I := Project.GetItemIndex(cPatterns, Project.CopiedProperties[6]);
    if I < 0 then I := 0;
    ENsetdemandpattern(Index, 1, I);
    if Utils.Str2Float(Project.CopiedProperties[8], X) then
      Epanet2.ENsetnodevalue(Index, EN_EMITTER, X);
    if Utils.Str2Float(Project.CopiedProperties[9], X) then
      Epanet2.ENsetnodevalue(Index, EN_INITQUAL, X);
    end;

  nReservoir:
    begin
    if Utils.Str2Float(Project.CopiedProperties[4], X) then
      Epanet2.ENsetnodevalue(Index, EN_ELEVATION, X);
    I := Project.GetItemIndex(cPatterns, Project.CopiedProperties[5]);
    if I < 0 then I := 0;
    ENsetnodevalue(Index, EN_PATTERN, I);
    if Utils.Str2Float(Project.CopiedProperties[6], X) then
      Epanet2.ENsetnodevalue(Index, EN_INITQUAL, X);
    end;

  nTank:
    begin
    if Utils.Str2Float(Project.CopiedProperties[4], X) then
      Epanet2.ENsetnodevalue(Index, EN_ELEVATION, X);
    if Utils.Str2Float(Project.CopiedProperties[5], X) then
      ENsetnodevalue(Index, EN_TANKLEVEL, X);
    if Utils.Str2Float(Project.CopiedProperties[6], X) then
      ENsetnodevalue(Index, EN_MINLEVEL, X);
    if Utils.Str2Float(Project.CopiedProperties[7], X) then
      ENsetnodevalue(Index, EN_MAXLEVEL, X);
    if Utils.Str2Float(Project.CopiedProperties[8], X) then
      ENsetnodevalue(Index, EN_DIAMETER, X);
    if Utils.Str2Float(Project.CopiedProperties[9], X) then
      ENsetnodevalue(Index, EN_MINVOLUME, X);
    I := Project.GetItemIndex(cCurves, Project.CopiedProperties[10]);
    if I < 0 then I := 0;
    ENsetlinkvalue(Index, EN_VOLCURVE, I);
    I := AnsiIndexText(Project.CopiedProperties[11], Project.MixingModelStr);
    if I < 0 then I := 0;
    ENsetnodevalue(Index, EN_MIXMODEL, I);
    if Utils.Str2Float(Project.CopiedProperties[12], X) then
      ENsetnodevalue(Index, EN_MIXFRACTION, X);
    if Utils.Str2Float(Project.CopiedProperties[13], X) then
      ENsetnodevalue(Index, EN_TANK_KBULK, X);
    if Utils.Str2Float(Project.CopiedProperties[14], X) then
      ENsetnodevalue(Index, EN_INITQUAL, X);
    end;
  end;
end;

procedure PasteLinkProps(const Index: Integer; const LinkType: Integer);
var
  I, J: Integer;
  X: Single;
begin
  case LinkType of
  lPipe:
    begin
    if Utils.Str2Float(Project.CopiedProperties[6], X) then
      ENsetlinkvalue(Index, EN_LENGTH, X);
    if Utils.Str2Float(Project.CopiedProperties[7], X) then
      ENsetlinkvalue(Index, EN_DIAMETER, X);
    if Utils.Str2Float(Project.CopiedProperties[8], X) then
      ENsetlinkvalue(Index, EN_ROUGHNESS, X);
    if Utils.Str2Float(Project.CopiedProperties[9], X) then
      ENsetlinkvalue(Index, EN_MINORLOSS, X);
    if Utils.Str2Float(Project.CopiedProperties[11], X) then
      ENsetlinkvalue(Index, EN_KBULK, X);
    if Utils.Str2Float(Project.CopiedProperties[12], X) then
      ENsetlinkvalue(Index, EN_KWALL, X);
    if Utils.Str2Float(Project.CopiedProperties[13], X) then
      ENsetlinkvalue(Index, EN_LEAK_AREA, X);
    if Utils.Str2Float(Project.CopiedProperties[14], X) then
      ENsetlinkvalue(Index, EN_LEAK_EXPAN, X);
    end;

  lPump:
    begin
    if Utils.Str2Float(Project.CopiedProperties[7], X) then
      ENsetlinkvalue(Index, EN_PUMP_POWER, X);
    if Utils.Str2Float(Project.CopiedProperties[8], X) then
      ENsetlinkvalue(Index, EN_INITSETTING, X);
    if Utils.Str2Float(Project.CopiedProperties[12], X) then
      ENsetlinkvalue(Index, EN_PUMP_ECOST, X);
    I := Project.GetItemIndex(cCurves, Project.CopiedProperties[6]);
    if I < 0 then I := 0;
    ENsetlinkvalue(Index, EN_PUMP_HCURVE, I);
    I := Project.GetItemIndex(cPatterns, Project.CopiedProperties[9]);
    if I < 1 then I := 0;
    ENsetlinkvalue(Index, EN_LINKPATTERN, I);
    I := Project.GetItemIndex(cCurves, Project.CopiedProperties[11]);
    if I < 0 then I := 0;
    ENsetlinkvalue(Index, EN_PUMP_ECURVE, I);
    I := Project.GetItemIndex(cCurves, Project.CopiedProperties[13]);
    if I < 0 then I := 0;
    ENsetlinkvalue(Index, EN_PUMP_EPAT, I);
    end;

  lValve:
    begin
    if Utils.Str2Float(Project.CopiedProperties[6], X) then
      ENsetlinkvalue(Index, EN_DIAMETER, X);
    I := EN_PRV + AnsiIndexText(Project.CopiedProperties[7], Project.ValveTypeStr);
    if I = EN_GPV then
    begin
      J := Project.GetItemIndex(cCurves, Project.CopiedProperties[8]);
      if J < 0 then J := 0;
      ENsetlinkvalue(Index, EN_INITSETTING, I);
    end
    else
    begin
      if Utils.Str2Float(Project.CopiedProperties[8], X) then
        ENsetlinkvalue(Index, EN_INITSETTING, X);
    end;
    if Utils.Str2Float(Project.CopiedProperties[9], X) then
      ENsetlinkvalue(Index, EN_MINORLOSS, X);
    end;
  end;
end;

end.

