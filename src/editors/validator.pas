{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       validator
 Description:  validates entries made in the main form's
               PropEditor control
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit validator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Dialogs;

{$I ..\timetype.txt}

var
  HasChanged: Boolean;
  IsValid: Boolean;

procedure ValidateOption(const Item: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
procedure ValidateNode(const Item: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
procedure ValidateLink(const Item: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
procedure ValidatePattern(const Item: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
procedure ValidateCurve(const Item: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
procedure ValidateLabel(const Item: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);

implementation

uses
  main, project, maplabel, mapthemes, utils, epanet2;

procedure ValidateHydraulOption(const Prop: Integer;
          const OldValue: String; var NewValue: String); forward;
procedure ValidateDemandOption(const Prop: Integer;
          const OldValue: String; var NewValue: String); forward;
procedure ValidateTimeOption(const Prop: Integer;
          const OldValue: String; var NewValue: String); forward;
procedure ValidateEnergyOption(const Prop: Integer;
          const OldValue: String; var NewValue: String); forward;

procedure ValidateJunc(const Index: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String); forward;
procedure ValidateResv(const Index: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String); forward;
procedure ValidateTank(const Index: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String); forward;

procedure ValidatePipe(Index: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String); forward;
procedure ValidatePump(Index: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String); forward;
procedure ValidateValve(Index: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String); forward;

procedure ShowError(const Msg: String; const OldValue: String; var NewValue: String);
begin
  utils.MsgDlg(Msg, mtError, [mbOK]);
  NewValue := OldValue;
  HasChanged := False;
  IsValid := False;
end;

procedure ValidateItemID(const Category: Integer; const Index: Integer;
          const OldValue: String; var NewValue: String);
var
  ErrMsg: String;
begin
  ErrMsg := project.GetIdError(Category, NewValue);
  if Length(ErrMsg) > 0 then ShowError(ErrMsg, OldValue, NewValue)
  else project.SetItemID(Category, Index, NewValue);
end;

procedure ValidateLinkNode(Index: Integer; Prop: Integer; const OldValue:
          String; var NewValue: String);

// Validates a new node assigned to one of the ends of a link
//   Index = the link's index
//   Prop = the index of the link property being changed
//          (2 for start node or 3 for end node)
//   OldValue = ID name of the current link's node
//   NewValue = new ID name for the link's node

var
  I: Integer = 0;
  J: Integer = 0;
  K: Integer;
begin
  // Check if new node exists
  K := Project.GetItemIndex(cNodes, NewValue);
  if K <= 0 then
    ShowError('Node ' + NewValue + ' does not exist.', OldValue, NewValue)
  else
  begin
    // Get indexes of link's current nodes
    project.GetLinkNodes(Index, I, J);

    // Assign new node to link if not same as link's other node
    if (Prop = 2) and (K <> J) then epanet2.ENsetlinknodes(Index, K, J) else
    if (Prop = 3) and (K <> I) then epanet2.ENsetlinknodes(Index, I, K) else

    // Otherwise swap the link's nodes with each other
    begin
      epanet2.ENsetlinknodes(Index, J, I);
      if Prop = 2 then Prop := 3 else Prop := 2;
      MainForm.ProjectFrame.PropEditor.Cells[1,Prop] := OldValue;
    end;

    // Redraw the network map
    MainForm.MapFrame.RedrawMap;
  end;
end;

procedure ValidateOption(const Item: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
begin
  case Item of
  oHydraul: ValidateHydraulOption(Prop, OldValue, NewValue);
  oDemands: ValidateDemandOption(Prop, OldValue, NewValue);
  oTimes:   ValidateTimeOption(Prop, OldValue, NewValue);
  oEnergy:  ValidateEnergyOption(Prop, OldValue, NewValue);
  end;
end;

procedure ValidateHydraulOption(const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  X: Single = 0;
  I: Integer;
begin
  // Numerical property indices in properties.HydOptionsProps
  if (Prop in [3, 4, 5, 6, 7, 8, 11, 12, 13]) then
  begin
    if utils.Str2Float(NewValue, X) = false then
    begin
      ShowError(NewValue + ' is not a valid number.', OldValue, NewValue);
      Exit;
    end;
  end;

  case Prop of
  1: {Flow Units}
    begin
      I := AnsiIndexText(NewValue, project.FlowUnitsStr);
      if I >= 0 then
      begin
        epanet2.ENsetflowunits(I);
        project.FlowUnits := I;
        MainForm.UpdateStatusBar(sbFlowUnits, NewValue);
      end;
    end;

  2: {Head Loss Model}
    begin
      I := AnsiIndexText(NewValue, project.HlossModelStr);
      if I >= 0 then
      begin
        epanet2.ENsetoption(EN_HEADLOSSFORM, I);
        MainForm.UpdateStatusBar(sbHeadLoss, NewValue);
      end;
    end;

  3: {Sp. Gravity}
    begin
      if X <= 0 then ShowError('Invalid numerical value.', OldValue, NewValue)
      else epanet2.ENsetoption(EN_SP_GRAVITY, X);
    end;

  4: {Sp. Viscosity}
    begin
      if X <= 0 then ShowError('Invalid numerical value.', OldValue, NewValue)
      else epanet2.ENsetoption(EN_SP_VISCOS, X);
    end;

  5: {Maximum Trials}
    begin
      if X <= 0 then ShowError('Invalid numerical value.', OldValue, NewValue)
      else epanet2.ENsetoption(EN_TRIALS, X);
    end;

  6: {Accuracy}
    begin
      if X <= 0 then ShowError('Invalid numerical value.', OldValue, NewValue)
      else epanet2.ENsetoption(EN_ACCURACY, X);
    end;

  7: {Head Tolerance}
    begin
      if X < 0 then ShowError('Invalid numerical value.', OldValue, NewValue)
      else epanet2.ENsetoption(EN_HEADERROR, X);
    end;

  8: {Flow Tolerance}
    begin
      if X < 0 then ShowError('Invalid numerical value.', OldValue, NewValue)
      else epanet2.ENsetoption(EN_FLOWCHANGE, X);
    end;

  9: {If Unbalanced}
    begin
      if sameText(NewValue, 'Stop') then X := -1 else X := 10;
      epanet2.ENsetoption(EN_EXTRA_ITER, X);
    end;

  10: {Status Reporting}
    begin
     I := AnsiIndexText(NewValue, project.StatusRptStr);
     if I >= 0 then
     begin
       epanet2.ENsetoption(EN_STATUS_REPORT, I);
     end;
    end;

  11: {CHECKFREQ}
    begin
      if X < 0 then ShowError('Invalid numerical value.', OldValue, NewValue)
      else epanet2.ENsetoption(EN_CHECKFREQ, X);
    end;

  12: {MAXCHECK}
    begin
      if X < 0 then ShowError('Invalid numerical value.', OldValue, NewValue)
      else epanet2.ENsetoption(EN_MAXCHECK, X);
    end;

  13: {DAMPLIMIT}
    begin
      if X < 0 then ShowError('Invalid numerical value.', OldValue, NewValue)
      else epanet2.ENsetoption(EN_DAMPLIMIT, X);
    end;
  end;
end;

procedure ValidateDemandOption(const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  X: Single = 0;
  I: Integer;
  Model: Integer = 0;
  Pmin: Single = 0;
  Preq: Single = 0;
  Pexp: Single = 0;
begin
  // Numerical property indices in properties.DemandOptionsProps
  if Prop in [3..7] then
  begin
    if utils.Str2Float(NewValue, X) = false then
    begin
      ShowError(NewValue + ' is not a valid number.', OldValue, NewValue);
      exit;
    end;
  end;

  epanet2.ENgetdemandmodel(Model, Pmin, Preq, Pexp);
  case Prop of
  1: {Demand Model}
    begin
      I := AnsiIndexText(NewValue, project.DemandModelStr);
      project.SetDemandModel(I);
    end;

  2: {Default Pattern}
    begin
      if Length(NewValue) = 0 then I := 0
      else I := project.GetItemIndex(cPatterns, NewValue);
      if I < 1
        then ShowError('Pattern ' + NewValue + ' does not exist.', OldValue, NewValue)
      else epanet2.ENsetoption(EN_DEMANDPATTERN, I);
    end;

  3: {Demand Multiplier}
    if X < 0 then ShowError('Invalid numerical value.', OldValue, NewValue)
    else epanet2.ENsetoption(EN_DEMANDMULT, X);

  4: {Service Pressure}
    if X < Pmin then ShowError(
      'Service pressure must be greater than minimum pressure.',
      OldValue, NewValue)
    else epanet2.ENsetdemandmodel(Model, Pmin, X, Pexp);

  5: {Minimum Pressure}
    if X > Preq then ShowError(
      'Minimum pressure must be less than service pressure.',
      OldValue, NewValue)
    else epanet2.ENsetdemandmodel(Model, X, Preq, Pexp);

  6: {Pressure Expon.}
    if X < 0 then ShowError('Invalid numerical value.', OldValue, NewValue)
    else epanet2.ENsetdemandmodel(Model, Pmin, Preq, X);

  7: {Emitter Expon.}
    if X <= 0 then ShowError('Invalid numerical value.', OldValue, NewValue)
    else epanet2.ENsetoption(EN_EMITEXPON, X);

  8: {Emitter Backflow}
    if SameText(NewValue, project.NoYesStr[0]) then
      epanet2.ENsetoption(EN_EMITBACKFLOW, 0)
    else
      epanet2.ENsetoption(EN_EMITBACKFLOW, 1);
  end;
end;

procedure ValidateTimeOption(const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  I: TimeType;
  T: TimeType;
  S: String;
begin
  if Prop = 10 then
  begin
    I := AnsiIndexText(NewValue, project.StatisticStr);
    if I >= 0 then epanet2.ENsettimeparam(EN_STATISTIC, I);
    exit;
  end;
  T := utils.Str2Seconds(NewValue);
  if (T < 0) then
  begin
    S := 'Invalid time value.';
    if Prop = 1 then S := S + ' Use decimal values for durations >= 24 hrs.';
    ShowError(S, OldValue, NewValue);
  end
  else if Prop <= 8 then epanet2.Ensettimeparam(Prop-1, T)
  else epanet2.ENsettimeparam(EN_STARTTIME, T);
end;

procedure ValidateEnergyOption(const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  X: Single = 0;
  I: Integer;
begin
  // Numerical property indices in properties.EnergyOptionsProps
  if (Prop in [1, 2, 4]) then
  begin
    if (not utils.Str2Float(NewValue, X)) or (X < 0) then
    begin
      ShowError(NewValue + ' is not a valid number.', OldValue, NewValue);
      Exit;
    end;
  end;

  case Prop of
  1: // Global pump efficiency
    if X > 100 then
       ShowError(NewValue + 'is not a valid efficiency value.', OldValue, NewValue)
     else epanet2.ENsetoption(EN_GLOBALEFFIC, X);

  2: // Global energy price
    epanet2.ENsetoption(EN_GLOBALPRICE, X);

  3: // Global energy price pattern
     begin
       if Length(NewValue) = 0 then I := 0
       else I := project.GetItemIndex(cPatterns, NewValue);
       if I < 1
         then ShowError('Pattern ' + NewValue + ' does not exist.', OldValue, NewValue)
       else epanet2.ENsetoption(EN_GLOBALPATTERN, I);
     end;

  4: // Peak energy demand charge
     epanet2.ENsetoption(EN_DEMANDCHARGE, X);
  end;
end;

procedure ValidateNode(const Item: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  NodeType: Integer;
  Index: Integer;
begin
  // Check for valid ID name
  Index := Item+1;
  if (Prop = 1) then
  begin
    ValidateItemID(cNodes, Index, OldValue, NewValue);
    if (OldValue <> NewValue) then
    begin
      MainForm.ProjectFrame.ShowItemID(Item);
      if MainForm.MapFrame.Map.Options.ShowNodeIDs then
        MainForm.MapFrame.RedrawMap;
    end;
  end

  // Save comment
  else if (Prop = 2) then
  begin
    epanet2.ENsetcomment(EN_NODE, Index, PAnsiChar(NewValue))
  end

  // Save tag
  else if (Prop = 3) then
  begin
    NewValue := ReplaceStr(NewValue, ' ', '_');
    NewValue := ReplaceStr(NewValue, ';', ':');
    epanet2.ENsettag(EN_NODE, Index, PAnsiChar(NewValue));
  end

  // Validate type-specific property
  else
  begin
    NodeType := Project.GetNodeType(Index);
    case NodeType of
      nJunction:  ValidateJunc(Index, Prop, OldValue, NewValue);
      nReservoir: ValidateResv(Index, Prop, OldValue, NewValue);
      nTank:      ValidateTank(Index, Prop, OldValue, NewValue);
    end;
  end;
end;

procedure ValidateLink(const Item: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  LinkType: Integer;
  Index: Integer;
begin
  Index := Item+1;
  // Check for valid ID name
  if (Prop = 1) then
  begin
    ValidateItemID(cLinks, Index, OldValue, NewValue);
    if (OldValue <> NewValue) then
    begin
      MainForm.ProjectFrame.ShowItemID(Item);
      if MainForm.MapFrame.Map.Options.ShowLinkIDs then
        MainForm.MapFrame.RedrawMap;
    end;
  end

  // Check for valid start/end node
  else if (Prop = 2) or (Prop = 3) then
    ValidateLinkNode(Index, Prop, OldValue, NewValue)

  // Save comment
  else if (Prop = 4) then epanet2.ENsetcomment(EN_LINK, Index, PAnsiChar(NewValue))

  // Save tag
   else if (Prop = 5) then
   begin
     NewValue := ReplaceStr(NewValue, ' ', '_');
     NewValue := ReplaceStr(NewValue, ';', ':');
     epanet2.ENsettag(EN_LINK, Index, PAnsiChar(NewValue));
   end

  // Validate type-specific property
  else
  begin
    LinkType := Project.GetLinkType(Index);
    case LinkType of
      lPipe:  ValidatePipe(Index, Prop, OldValue, NewValue);
      lPump:  ValidatePump(Index, Prop, OldValue, NewValue);
      lValve: ValidateValve(Index, Prop, OldValue, NewValue);
    end;
  end;
end;

procedure ValidateJunc(const Index: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  X: Single = 0;
  I: Integer;
begin
  // Numerical property indices in properties.JunctionProps
  if (Prop in [4, 5, 8, 9]) then
  begin
    if not utils.Str2Float(NewValue, X) then
    begin
      ShowError(NewValue + ' is not a valid number.', OldValue, NewValue);
      Exit;
    end;
  end;

  case Prop of
  4: begin
       epanet2.ENsetnodevalue(Index, EN_ELEVATION, X);
       if mapthemes.NodeTheme = ntElevation then
         MainForm.MapFrame.RedrawMap;
     end;
  5: begin
       epanet2.ENsetnodevalue(Index, EN_BASEDEMAND, X);
       if  mapthemes.NodeTheme = ntBaseDemand then
         MainForm.MapFrame.RedrawMap;
     end;
  6: begin
       if Length(NewValue) = 0 then I := 0 else
       begin
         I := project.GetItemIndex(cPatterns, NewValue);
         if I < 1 then
         begin
           ShowError('Pattern ' + NewValue + ' does not exist.',
             OldValue, NewValue);
           exit;
         end
       end;
       epanet2.ENsetdemandpattern(Index, 1, I);
     end;
  8: epanet2.ENsetnodevalue(Index, EN_EMITTER, X);
  9: epanet2.ENsetnodevalue(Index, EN_INITQUAL, X);
  10: begin
        epanet2.ENgetnodevalue(Index, EN_SOURCEQUAL, X);
        NewValue := utils.Float2Str(X, 4);
      end;
  end;
end;

procedure ValidateResv(const Index: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  X: Single = 0;
  I: Integer;
begin
  if (Prop in [4, 6]) and (not utils.Str2Float(NewValue, X)) then
  begin
    ShowError(NewValue + ' is not a valid number.', OldValue, NewValue);
    exit;
  end;

  case Prop of
  4: begin
      epanet2.ENsetnodevalue(Index, EN_ELEVATION, X);
      if mapthemes.NodeTheme = ntElevation then
        MainForm.MapFrame.RedrawMap;
     end;
  5: begin
       if Length(NewValue) = 0 then I := 0
       else I := project.GetItemIndex(cPatterns, NewValue);
       if I < 0
       then ShowError('Pattern ' + NewValue + ' does not exist.', OldValue, NewValue)
       else epanet2.ENsetnodevalue(Index, EN_PATTERN, I);
     end;
  6: epanet2.ENsetnodevalue(Index, EN_INITQUAL, X);
  7: begin
        epanet2.ENgetnodevalue(Index, EN_SOURCEQUAL, X);
        NewValue := FloatToStr(X);
      end;
  end;
end;

procedure ValidateTank(const Index: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  X: Single = 0;
  I: Integer;
begin
  // Numerical property indices in properties.TankProps
  if Prop in [4..9, 13..15] then
  begin
    if not utils.Str2Float(NewValue, X) then
    begin
      ShowError(NewValue + ' is not a valid number.', OldValue, NewValue);
      exit;
    end
    else if (Prop <> 14) and (X < 0) then
    begin
      ShowError('Value less than 0 not allowed.', OldValue, NewValue);
      exit;
    end;
  end;

  case Prop of
  4: begin
      epanet2.ENsetnodevalue(Index, EN_ELEVATION, X);
       if mapthemes.NodeTheme = ntElevation then
         MainForm.MapFrame.RedrawMap;
     end;
  5: begin
       I := epanet2.ENsetnodevalue(Index, EN_TANKLEVEL, X);
       if I > 0 then ShowError('Inconsistent tank levels.', OldValue, NewValue);
     end;
  6: begin
       I := epanet2.ENsetnodevalue(Index, EN_MINLEVEL, X);
       if I > 0 then ShowError('Inconsistent tank levels.', OldValue, NewValue);
     end;
  7: begin
       I := epanet2.ENsetnodevalue(Index, EN_MAXLEVEL, X);
       if I > 0 then ShowError('Inconsistent tank levels.', OldValue, NewValue);
     end;
  8: epanet2.ENsetnodevalue(Index, EN_TANKDIAM, X);
  9: epanet2.ENsetnodevalue(Index, EN_MINVOLUME, X);
  10: begin
        if Length(NewValue) = 0 then I := 0
        else I := project.GetItemIndex(cCurves, Newvalue);
        if I < 0
        then ShowError('Curve ' + NewValue + ' does not exist.', OldValue, NewValue)
        else epanet2.ENsetnodevalue(Index, EN_VOLCURVE, I);
      end;
  11: if SameText(NewValue, Project.NoYesStr[0]) then
        epanet2.ENsetnodevalue(Index, EN_CANOVERFLOW, 0)
      else
        epanet2.ENsetnodevalue(Index, EN_CANOVERFLOW, 1);
  12: begin
        I := AnsiIndexText(NewValue, project.MixingModelStr);
        if I < 0 then ShowError('Invalid mixing model.', OldValue, NewValue)
        else epanet2.ENsetnodevalue(Index, EN_MIXMODEL, I);
      end;
  13: epanet2.ENsetnodevalue(Index, EN_MIXFRACTION, X);
  14: epanet2.ENsetnodevalue(Index, EN_TANK_KBULK, X);
  15: epanet2.ENsetnodevalue(Index, EN_INITQUAL, X);
  16: begin
        epanet2.ENgetnodevalue(Index, EN_SOURCEQUAL, X);
        NewValue := FloatToStr(X);
      end;
  end;
end;

procedure ValidatePipe(Index: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  X: Single = 0;
begin
  // Numerical property indices in properties.PipeProps
  if Prop in [6, 7, 8, 9, 11, 12, 13, 14] then
  begin
    if not utils.Str2Float(NewValue, X) then
    begin
      ShowError(NewValue + ' is not a valid number.', OldValue, NewValue);
      exit;
    end
    else if (Prop in [6, 7, 8]) and (X <= 0) then
    begin
      ShowError('Value <= 0 not allowed.', OldValue, NewValue);
      exit;
    end
    else if (Prop in [9, 13, 14]) and (X < 0) then
    begin
      ShowError('Value < 0 not allowed.', OldValue, NewValue);
      exit;
    end;
  end;

  case Prop of
  6: begin
       epanet2.ENsetlinkvalue(Index, EN_LENGTH, X);
       if mapthemes.LinkTheme = ltLength then
          MainForm.MapFrame.RedrawMap;
     end;
  7: begin
       epanet2.ENsetlinkvalue(Index, EN_DIAMETER, X);
       if mapthemes.LinkTheme = ltDiameter then
         MainForm.MapFrame.RedrawMap;
     end;
  8: begin
      epanet2.ENsetlinkvalue(Index, EN_ROUGHNESS, X);
       if mapthemes.LinkTheme = ltRoughness then
         MainForm.MapFrame.RedrawMap;
     end;
  9: epanet2.ENsetlinkvalue(Index, EN_MINORLOSS, X);
  11: epanet2.ENsetlinkvalue(Index, EN_KBULK, X);
  12: epanet2.ENsetlinkvalue(Index, EN_KWALL, X);
  13: epanet2.ENsetlinkvalue(Index, EN_LEAK_AREA, X);
  14: epanet2.ENsetlinkvalue(Index, EN_LEAK_EXPAN, X);
  end;
  if Prop = 10 then
  begin
    if SameText(NewValue, 'CV')
    then epanet2.ENsetlinktype(Index, EN_CVPIPE, EN_UNCONDITIONAL)
    else begin
      epanet2.ENsetlinktype(Index, EN_PIPE, EN_UNCONDITIONAL);
      if SameText(NewValue, 'Open')
      then epanet2.ENsetlinkvalue(Index, EN_INITSTATUS, EN_OPEN)
      else if SameText(NewValue, 'Closed')
      then epanet2.ENsetlinkvalue(Index, EN_INITSTATUS, EN_CLOSED);
    end;
  end;
end;

procedure ValidatePump(Index: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  X: Single = 0;
  I: Integer;
begin
  // Numerical property indices in properties.PumpProps
  if Prop in [7, 8, 12] then
  begin
    if not utils.Str2Float(NewValue, X) then
    begin
      ShowError(NewValue + ' is not a valid number.', OldValue, NewValue);
      exit;
    end
    else if X < 0 then
    begin
      ShowError('Value < 0 not allowed.', OldValue, NewValue);
      exit;
    end;
  end;

  case Prop of
  7: epanet2.ENsetlinkvalue(Index, EN_PUMP_POWER, X);
  8: epanet2.ENsetlinkvalue(Index, EN_INITSETTING, X);
  12: epanet2.ENsetlinkvalue(Index, EN_PUMP_ECOST, X);

  6:  // Pump Curve
    begin
      if Length(NewValue) = 0 then I := 0
      else I := project.GetItemIndex(cCurves, NewValue);
      if I < 0
      then ShowError('Curve ' + NewValue + ' does not exist.', OldValue, NewValue)
      else epanet2.ENsetlinkvalue(Index, EN_PUMP_HCURVE, I);
    end;

  9:  // Speed Pattern
    begin
      if Length(NewValue) = 0 then I := 0
      else I := project.GetItemIndex(cPatterns, NewValue);
      if I < 1
      then ShowError('Pattern ' + NewValue + ' does not exist.', OldValue, NewValue)
      else epanet2.ENsetlinkvalue(Index, EN_LINKPATTERN, I);
    end;

  11:  // Efficiency Curve
    begin
      if Length(NewValue) = 0 then I := 0
      else I := project.GetItemIndex(cCurves, NewValue);
      if I < 0
      then ShowError('Curve ' + NewValue + ' does not exist.', OldValue, NewValue)
      else epanet2.ENsetlinkvalue(Index, EN_PUMP_ECURVE, I);
    end;

  13:  // Energy Cost Pattern
    begin
      if Length(NewValue) = 0 then I := 0
      else I := project.GetItemIndex(cPatterns, NewValue);
      if I < 1
      then ShowError('Pattern ' + NewValue + ' does not exist.', OldValue, NewValue)
      else epanet2.ENsetlinkvalue(Index, EN_PUMP_EPAT, I);
    end;
  end;

  if Prop = 10 then
  begin
    if SameText(NewValue, 'Open')
    then epanet2.ENsetlinkvalue(Index, EN_INITSTATUS, EN_OPEN)
    else if SameText(NewValue, 'Closed')
    then epanet2.ENsetlinkvalue(Index, EN_INITSTATUS, EN_CLOSED);
  end;
end;

procedure ValidateValve(Index: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  X: Single = 0;
  I: Integer = 0;
  J: Integer = 0;
  K: Integer;
  S: String;
begin
  // Check for valid numerical property
  ENgetlinktype(Index, J);
  S := Trim(NewValue);
  if Prop in [6, 8, 9] then
  begin
    if (not utils.Str2Float(S, X)) and (J <> EN_GPV) then
    begin
      ShowError(NewValue + ' is not a valid number.', OldValue, NewValue);
      exit;
    end
    else if X < 0 then
    begin
      ShowError('Value < 0 not allowed.', OldValue, NewValue);
      exit;
    end;
  end;

  case Prop of
  6: // Valve diameter
     begin
       epanet2.ENsetlinkvalue(Index, EN_DIAMETER, X);
       if mapthemes.LinkTheme = ltDiameter then
         MainForm.MapFrame.RedrawMap;
      end;

  7: // Valve type
     begin
       I := EN_PRV + AnsiIndexText(S, project.ValveTypeStr);
       begin
         epanet2.ENsetlinktype(Index, I, EN_UNCONDITIONAL);
         if I = EN_GPV then epanet2.ENsetlinkvalue(Index, EN_INITSETTING, 0);
       end;
     end;

  8:  // Valve setting
      begin
        if J <> EN_GPV then
          epanet2.ENsetlinkvalue(Index, EN_INITSETTING, X)
        else begin
          if Length(S) = 0 then I := 0
          else I := project.GetItemIndex(cCurves, S);
          if I < 0 then
            ShowError('Curve ' + S + ' does not exist.', OldValue, NewValue)
          else epanet2.ENsetlinkvalue(Index, EN_INITSETTING, I);
        end;
        with MainForm.ProjectFrame.PropEditor do
        begin
          EditorMode := False;
          Cells[1,11] := 'None';
          EditorMode := True;
        end;
      end;

  9: // Minor loss coeff.
     epanet2.ENsetlinkvalue(Index, EN_MINORLOSS, X);

  10: // PCV curve
      begin
        if Length(NewValue) = 0 then
          I := 0
        else
        begin
          I := project.GetItemIndex(cCurves, NewValue);
          if I < 1 then
            ShowError('Curve ' + NewValue + ' does not exist.', OldValue, NewValue)
        end;
        epanet2.ENsetlinkvalue(Index, EN_PCV_CURVE, I);
      end;

  11: // GPV curve
      begin
        if Length(NewValue) = 0 then
          I := 0
        else
        begin
          I := project.GetItemIndex(cCurves, NewValue);
          if I < 1 then
            ShowError('Curve ' + NewValue + ' does not exist.', OldValue, NewValue)
        end;
        epanet2.ENsetlinkvalue(Index, EN_GPV_CURVE, I);
      end;

  12: // Fixed Status
      begin
        epanet2.ENgetlinktype(Index, I);
        K := AnsiIndexText(S, project.ValveStatusStr);
        epanet2.ENsetlinkvalue(Index, EN_INITSTATUS, K);
        if ((K = EN_CLOSED) or (K = EN_OPEN))
        then with MainForm.ProjectFrame.PropEditor do
        begin
          EditorMode := False;
          Cells[1,8] := '0.0000';
          EditorMode := True;
        end;
      end;
  end;
end;

procedure ValidatePattern(const Item: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  Index: Integer;
begin
  Index := Item+1;
  if (Prop = 1) then ValidateItemID(cPatterns, Index, OldValue, NewValue)
  else if (Prop = 2) then epanet2.ENsetcomment(EN_PATTERN, Index, PAnsiChar(NewValue));
end;

procedure ValidateCurve(const Item: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  Index: Integer;
begin
  Index := Item+1;
  if (Prop = 1) then ValidateItemID(cCurves, Index, OldValue, NewValue)
  else if (Prop = 2) then epanet2.ENsetcomment(EN_CURVE, Index, PAnsiChar(NewValue))
  else if (Prop = 3) then
    epanet2.ENsetcurvetype(Index, AnsiIndexText(NewValue, project.CurveTypeStr));
end;

procedure ValidateLabel(const Item: Integer; const Prop: Integer;
          const OldValue: String; var NewValue: String);
var
  MapLabel: TMapLabel;
  Rotation: Integer;
begin
  MapLabel := TMapLabel(project.MapLabels.Objects[Item]);
  if Prop = 1 then
  begin
    project.SetItemID(cLabels, Item, NewValue);
    MainForm.MapFrame.RedrawMapLabels;
  end
  else if Prop = 3 then
  begin
    Rotation := StrToInt(NewValue);
    if (Rotation < 0) or (Rotation > 360) then
      ShowError('Rotation must be between 0 and 360.', OldValue, NewValue)
    else MapLabel.Rotation := Rotation;
  end
  else if Prop = 4 then
  begin
    if Length(NewValue) = 0 then
      Maplabel.AnchorNode:= NewValue
    else if project.GetItemIndex(cNodes, NewValue) <= 0 then
      ShowError('Node ' + NewValue + ' does not exist.', OldValue, NewValue)
    else
      Maplabel.AnchorNode:= NewValue;
  end;
end;

end.

