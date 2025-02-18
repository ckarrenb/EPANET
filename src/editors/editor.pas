{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       editor
 Description:  edits the properties of a project's objects
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}
{
 This unit works directly with the PropEditor control that appears on
 the main form's ProjectFrame.
}

unit editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, ValEdit, Controls;

var
  FirstResultRow: Integer;  // Row of the PropEditor where results are listed

procedure Edit(const Category: Integer; Item: Integer);
procedure EditTitleText;
procedure ButtonClick(const Category: Integer; const Item: Integer;
          const Prop: Integer);
function  Validate(const Category: Integer; const Item: Integer;
          const Prop: Integer; const OldValue: String; var NewValue: String): Boolean;
procedure PasteProperties(const Category: Integer; const ItemType: Integer;
          const Item: Integer);

implementation

uses
  main, project, projectframe, titleeditor, demandseditor, sourceeditor,
  controlseditor, ruleseditor, curveselector, patternselector, qualeditor,
  msxeditor, maplabel, mapthemes, properties, validator, epanet2;

procedure EditDemands(const Index: Integer); forward;
procedure EditSourceQuality(const Index: Integer; const Row: Integer); forward;
procedure EditSimpleControls; forward;
procedure EditRuleControls; forward;
procedure EditLabelFont(const Item: Integer); forward;

procedure EditOptions(const Item: Integer); forward;
procedure EditHydraulOptions; forward;
procedure EditDemandOptions; forward;
procedure EditQualityOptions; forward;
procedure EditSingleSpecieQuality; forward;
procedure EditMultiSpeciesQuality; forward;
procedure EditTimeOptions; forward;
procedure EditEnergyOptions; forward;

procedure EditNode(const Item: Integer); forward;
procedure EditJunction(const Index: Integer); forward;
procedure EditReservoir(const Index: Integer); forward;
procedure EditTank(const Index: Integer); forward;

procedure EditLink(const Item: Integer); forward;
procedure EditPipe(const Index: Integer); forward;
procedure EditPump(const Index: Integer); forward;
procedure EditValve(const Index: Integer); forward;

procedure EditControls(const Item: Integer); forward;
procedure EditLabel(const Item: Integer); forward;

procedure OptionButtonClick(const Item: Integer; const Prop: Integer); forward;
procedure NodeButtonClick(const Item: Integer; const Prop: Integer); forward;
procedure LinkButtonClick(const Item: Integer; const Prop: Integer); forward;
function  GetPatternSelection(PropIndex: Integer; PropValue: String): Integer; forward;
function  GetCurveSelection(PropIndex: Integer; PropValue: String): Integer; forward;

procedure ShowEditor(State: Boolean);
begin
  MainForm.ProjectFrame.PropertyPanel.Visible := State;
end;

procedure Edit(const Category: Integer; Item: Integer);
//
//  Loads properties of a selected network object into the PropEditor.
//
begin
  FirstResultRow := 0;
  case Category of
    cOptions:  EditOptions(Item);
    cNodes:    EditNode(Item);
    cLinks:    EditLink(Item);
    cControls: EditControls(Item);
    cLabels:   EditLabel(Item);
    else ShowEditor(false);
  end;
end;

procedure ButtonClick(const Category: Integer; const Item: Integer;
          const Prop: Integer);
//
// Selects a specialized editor to launch when an ellipsis button
// in the PropEditor is clicked.
//
begin
  case Category of
    cNodes:
      NodeButtonClick(Item, Prop);
    cLinks:
      LinkButtonClick(Item, Prop);
    cControls:
      if Prop = 1 then EditSimpleControls else EditRuleControls;
    cLabels:
      EditLabelFont(Item);
    cOptions:
      OptionButtonClick(Item, Prop);
  end;

  // Move to next row of Property Editor after done with specialized editor
  with MainForm.ProjectFrame.PropEditor do
  begin
    if Prop = RowCount -1 then Row := Prop-1 else Row := Prop+1;
  end;
end;

procedure OptionButtonClick(const Item: Integer; const Prop: Integer);
// Handles ellipsis button clicks when editing Option properties
var
  I: Integer;
  PropName, S: String;
begin
  PropName := MainForm.ProjectFrame.PropEditor.Cells[0,Prop];
  if PropName = 'Price Pattern' then
  begin
    S := MainForm.ProjectFrame.PropEditor.Cells[1,Prop];
    I := GetPatternSelection(Prop, S);
    if I >= 0 then ENsetoption(EN_GLOBALPATTERN, I);
  end
  else if PropName = 'Default Pattern' then
  begin
    S := MainForm.ProjectFrame.PropEditor.Cells[1,Prop];
    I := GetPatternSelection(Prop, S);
    if I >= 0 then ENsetoption(EN_DEMANDPATTERN, I);
  end
  else if Item = oQuality then
  begin
    if PropName = 'Single-Species' then EditSingleSpecieQuality;
    if PropName = 'Multi-Species' then  EditMultiSpeciesQuality;
    MainForm.UpdateStatusBar(sbQuality, Project.GetQualModelStr);
  end;
end;

procedure NodeButtonClick(const Item: Integer; const Prop: Integer);
// Handles ellipsis button clicks when editing Node properties
var
  I: Integer;
  PropName, S: String;
begin
  PropName := MainForm.ProjectFrame.PropEditor.Cells[0,Prop];

  if PropName = 'Demand Categories' then EditDemands(Item+1)

  else if PropName = 'Source Quality' then EditSourceQuality(Item+1, Prop)

  else if PropName = 'Volume Curve' then
  begin
    S := MainForm.ProjectFrame.PropEditor.Cells[1,Prop];
    I := GetCurveSelection(Prop, S);
    if I >= 0 then ENsetnodevalue(Item+1, EN_VOLCURVE, I);
  end

  else if (PropName = 'Demand Pattern') or (PropName = 'Elev. Pattern') then
  begin
    S := MainForm.ProjectFrame.PropEditor.Cells[1,Prop];
    I := GetPatternSelection(Prop, S);
    if I >= 0 then ENsetnodevalue(Item+1, EN_PATTERN, I);
  end;
end;

procedure LinkButtonClick(const Item: Integer; const Prop: Integer);
// Handles ellipsis button clicks when editing Link properties
var
  I: Integer;
  PropName, S: String;
begin
  PropName := MainForm.ProjectFrame.PropEditor.Cells[0,Prop];

  if PropName = 'Pump Curve' then
  begin
    S := MainForm.ProjectFrame.PropEditor.Cells[1,Prop];
    I := GetCurveSelection(Prop, S);
    if I >= 0 then ENsetlinkvalue(Item+1, EN_PUMP_HCURVE, I);
  end

  else if PropName = 'Speed Pattern' then
  begin
    S := MainForm.ProjectFrame.PropEditor.Cells[1,Prop];
    I := GetPatternSelection(Prop, S);
    if I >= 0 then ENsetlinkvalue(Item+1, EN_LINKPATTERN, I);
  end

  else if PropName = 'Effic. Curve' then
  begin
    S := MainForm.ProjectFrame.PropEditor.Cells[1,Prop];
    I := GetCurveSelection(Prop, S);
    if I >= 0 then ENsetlinkvalue(Item+1, EN_PUMP_ECURVE, I);
  end

  else if PropName = 'Price Pattern' then
  begin
    S := MainForm.ProjectFrame.PropEditor.Cells[1,Prop];
    I := GetPatternSelection(Prop, S);
    if I >= 0 then ENsetlinkvalue(Item+1, EN_PUMP_EPAT, I);
  end

  else if PropName = 'PCV Curve' then
  begin
    S := MainForm.ProjectFrame.PropEditor.Cells[1,Prop];
    I := GetCurveSelection(Prop, S);
    if I >= 0 then ENsetlinkvalue(Item+1, EN_PCV_CURVE, I);
  end

  else if PropName = 'GPV Curve' then
  begin
    S := MainForm.ProjectFrame.PropEditor.Cells[1,Prop];
    I := GetCurveSelection(Prop, S);
    if I >= 0 then ENsetlinkvalue(Item+1, EN_GPV_CURVE, I);
  end;

end;

function GetCurveSelection(PropIndex: Integer; PropValue: String): Integer;
var
  CurveSelectForm: TCurveSelectorForm;
begin
  Result := -1;
  CurveSelectForm := TCurveSelectorForm.Create(MainForm);
  try
    CurveSelectForm.Setup(PropValue);
    CurveSelectForm.ShowModal;
    if CurveSelectForm.ModalResult = mrOK then
    begin
      with MainForm.ProjectFrame.PropEditor do
        if Cells[1,PropIndex] <> CurveSelectForm.SelectedName then
        begin
          EditorMode := False;
          Cells[1,PropIndex] := CurveSelectForm.SelectedName;
          EditorMode := True;
          project.HasChanged := True;
        end;
      Result := CurveSelectForm.SelectedIndex;
    end;
  finally
    CurveSelectForm.Free;
  end;
end;

function GetPatternSelection(PropIndex: Integer; PropValue: String): Integer;
var
  PatSelectForm: TPatternSelectorForm;
begin
  Result := -1;
  PatSelectForm := TPatternSelectorForm.Create(MainForm);
  try
    PatSelectForm.Setup(PropValue);
    PatSelectForm.ShowModal;
    if PatSelectForm.ModalResult = mrOK then
    begin
      with MainForm.ProjectFrame.PropEditor do
        if Cells[1,PropIndex] <> PatSelectForm.SelectedName then
        begin
          EditorMode := False;
          Cells[1,PropIndex] := PatSelectForm.SelectedName;
          EditorMode := True;
          project.HasChanged := True;
        end;
      Result := PatSelectForm.SelectedIndex;
    end;
  finally
    PatSelectForm.Free;
  end;
end;

function Validate(const Category: Integer; const Item: Integer;
  const Prop: Integer; const OldValue: String; var NewValue: String): Boolean;
//
// Validates an edit made in the PropEditor
//
begin
  Result := True;
  if NewValue = OldValue then exit;
  validator.HasChanged := True;
  validator.IsValid := True;
  case Category of
    cOptions:  validator.ValidateOption(Item, Prop, OldValue, NewValue);
    cNodes:    validator.ValidateNode(Item, Prop, OldValue, NewValue);
    cLinks:    validator.ValidateLink(Item, Prop, OldValue, NewValue);
    cLabels:   validator.ValidateLabel(Item, Prop, OldValue, NewValue);
  end;
  if validator.HasChanged then
  begin
    project.HasChanged := True;
    if Category <> cLabels then project.UpdateResultsStatus;
  end;
  Result := validator.IsValid;
end;

procedure EditTitleText;
begin
  with TTitleEditorForm.Create(MainForm) do
  try
    ShowModal;
    if (ModalResult = mrOk) and HasChanged then project.HasChanged := True;
  finally
    Free;
  end;
end;

procedure EditDemands(const Index: Integer);
var
  Count: string = '';
  D1: string = '';
  P1: String = '';
begin
  with TDemandsEditorForm.Create(MainForm) do
  try
    LoadDemands(Index);
    ShowModal;
    if (ModalResult = mrOk) and hasChanged then
    begin
      project.HasChanged := True;
      project.UpdateResultsStatus;
    end;

    // After editing, update the cells of the PropEditor that
    // contain the primary demand, that demand's time pattern and
    // the total number of demands
    with MainForm.ProjectFrame.PropEditor do
    begin
      EditorMode := False;
      GetPrimaryDemandInfo(D1, P1, Count);
      Cells[1,5] := D1;
      Cells[1,6] := P1;
      Cells[1,7] := Count;
      EditorMode := True;
    end;
  finally
    Free;
  end;
end;

procedure EditSingleSpecieQuality;
var
  QualType: Integer = 0;
  ChemName: array[0..EN_MAXID] of AnsiChar;
  ChemUnits: array[0..EN_MAXID] of AnsiChar;
  TraceNodeIndex: Integer = 0;
begin
  with TQualEditorForm.Create(MainForm) do
  try
    ShowModal;
    if ModalResult = mrOK then
    begin
      epanet2.ENgetqualinfo(QualType, ChemName, ChemUnits, TraceNodeIndex);
      with MainForm.ProjectFrame.PropEditor do
      begin
        EditorMode := False;
        if QualType = 0 then Cells[1,1] := 'No'
        else Cells[1,1] := project.QualModelStr[QualType];
        Cells[1,2] := 'No';
        project.MsxInpFile := '';
        EditorMode := True;
        if HasChanged then
        begin
          project.HasChanged := True;
          project.UpdateResultsStatus;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

procedure EditMultiSpeciesQuality;
var
  S: String;
begin
  with TMsxEditorForm.Create(MainForm) do
  try
    ShowModal;
    with MainForm.ProjectFrame.PropEditor do
    begin
      EditorMode := False;
      if Length(project.MsxInpFile) > 0 then
      begin
        Cells[1,1] := 'No';
        Cells[1,2] := 'Yes';
        epanet2.ENsetqualtype(0, '', '', '');
      end
      else
        Cells[1,2] := 'No';
      EditorMode := True;
    end;
  finally
    Free;
  end;
end;

procedure EditSourceQuality(const Index: Integer; const Row: Integer);
begin
  with TSourceEditorForm.Create(MainForm) do
  try
    LoadSource(Index);
    ShowModal;
    if (ModalResult = mrOk) then
    begin
      if HasChanged then
      begin
        project.HasChanged := True;
        project.UpdateResultsStatus;
      end;

      // After editing, update the cell of the Property Editor that
      // displays the source's strength
      with MainForm.ProjectFrame.PropEditor do
      begin
        EditorMode := False;
        Cells[1,Row] := GetSourceStrength;
        EditorMode := True;
      end;
    end;
  finally
    Free;
  end;
end;

procedure EditOptions(const Item: Integer);
begin
  case Item of
  oHydraul: EditHydraulOptions;
  oDemands: EditDemandOptions;
  oQuality: EditQualityOptions;
  oTimes:   EditTimeOptions;
  oEnergy:  EditEnergyOptions;
  end;
end;

procedure EditHydraulOptions;
var
  I: Integer;
  OptionList: TStringList;
begin
  OptionList := TStringList.Create;
  try
    with MainForm.ProjectFrame.PropEditor do
    begin
      properties.GetHydProps;
      Clear;
      RowCount := 1;
      for I := 1 to High(HydOptionsProps) do
      begin
        Strings.AddPair(HydOptionsProps[I], project.Properties[I]);
      end;
      OptionList.AddStrings(project.FlowUnitsStr, true);
      with ItemProps['Flow Units'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;
      OptionList.AddStrings(project.HLossModelStr, true);
      with ItemProps['Head Loss Model'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;
      OptionList.Clear;
      OptionList.Add('Stop');
      OptionList.Add('Continue');
      with ItemProps['If Unbalanced'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;
      OptionList.AddStrings(project.StatusRptStr, true);
      with ItemProps['Status Reporting'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;
      Row := 1;
      Show;
    end;
  finally
    OptionList.Free;
  end;
end;

procedure EditQualityOptions;
var
  I: Integer;
begin
    with MainForm.ProjectFrame.PropEditor do
    begin
      properties.GetQualProps;
      Clear;
      RowCount := 1;
      for I := 1 to High(properties.QualOptionsProps) do
      begin
        Strings.AddPair(properties.QualOptionsProps[I], project.Properties[I]);
      end;
      with ItemProps['Single-Species'] do
      begin
        EditStyle := esEllipsis;
        ReadOnly := true;
      end;
      with ItemProps['Multi-Species'] do
      begin
        EditStyle := esEllipsis;
        ReadOnly := true;
      end;
      Row := 1;
    end;
end;

procedure EditDemandOptions;
var
  I: Integer;
  OptionList: TStringList;
begin
  OptionList := TStringList.Create;
  try
    with MainForm.ProjectFrame.PropEditor do
    begin
      properties.GetDemandProps;
      Clear;
      RowCount := 1;
      for I := 1 to High(properties.DemandOptionsProps) do
      begin
        Strings.AddPair(properties.DemandOptionsProps[I], project.Properties[I]);
      end;
      OptionList.Add('DDA');
      OptionList.Add('PDA');
      with ItemProps['Demand Model'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;
      with ItemProps['Default Pattern'] do
      begin
        EditStyle := esEllipsis;
        ReadOnly := true;
      end;
      OptionList.AddStrings(project.NoYesStr, true);
      with ItemProps['Emitter Backflow'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;
      Row := 1;
      Show;
   end;
  finally
    OptionList.Free;
  end;
end;

procedure EditTimeOptions;
var
  I: Integer;
  OptionList: TStringList;
begin
  OptionList := TStringList.Create;
  try
  with MainForm.ProjectFrame.PropEditor do
  begin
    properties.GetTimeProps;
    Clear;
    RowCount := 1;
    for I := 1 to High(properties.TimeOptionsProps) do
    begin
      Strings.AddPair(properties.TimeOptionsProps[I], project.Properties[I]);
    end;
    OptionList.AddStrings(project.StatisticStr, true);
    with ItemProps['Statistic'] do
    begin
      EditStyle := esPickList;
      PickList := OptionList;
      ReadOnly := true;
    end;
    Row := 1;
    Show;
  end;
  finally
    OptionList.Free;
  end;
end;

procedure EditEnergyOptions;
var
  I: Integer;
begin
  with MainForm.ProjectFrame.PropEditor do
  begin
    properties.GetEnergyProps;
    Clear;
    RowCount := 1;
    for I := 1 to High(properties.EnergyOptionsProps) do
    begin
      Strings.AddPair(properties.EnergyOptionsProps[I], project.Properties[I]);
    end;

    with ItemProps['Price Pattern'] do
    begin
      EditStyle := esEllipsis;
      ReadOnly := true;
    end;

    Row := 1;
    Show;
  end;
end;

procedure EditNode(const Item: Integer);
var
  NodeType: Integer;
  Index: Integer;
  CurrentRow: Integer;
begin
  MainForm.ProjectFrame.PropEditor.Hide;
  CurrentRow := MainForm.ProjectFrame.PreviousRow;
  Index := Item+1;
  NodeType := project.GetNodeType(Index);
  case NodeType of
    nJunction:  EditJunction(Index);
    nReservoir: EditReservoir(Index);
    nTank:      EditTank(Index);
  end;

  with MainForm.ProjectFrame.PropEditor do
  begin
    if RowCount >= CurrentRow then
      Row := CurrentRow
    else
      Row := 1;
    Show;
  end;

end;

procedure EditJunction(const Index: Integer);
var
  I: Integer;
begin
  FirstResultRow := properties.FirstJuncResultIndex;
  with MainForm.ProjectFrame.PropEditor do
  begin
    properties.GetJuncProps(Index);
    Clear;

    RowCount := 1;
    for I := 1 to High(properties.JunctionProps) do
    begin
      Strings.AddPair(properties.JunctionProps[I], project.Properties[I]);
    end;

    with ItemProps['Demand Pattern'] do
    begin
      EditStyle := esEllipsis;
      ReadOnly := true;
    end;

    with ItemProps['Demand Categories'] do
    begin
      EditStyle := esEllipsis;
      ReadOnly := true;
    end;

    with ItemProps['Source Quality'] do
    begin
      EditStyle := esEllipsis;
      ReadOnly := true;
    end;

    for I := FirstResultRow to High(properties.JunctionProps) do
      ItemProps[properties.JunctionProps[I]].ReadOnly := true;
  end;
end;

procedure EditReservoir(const Index: Integer);
var
  I: Integer;
begin
  FirstResultRow := properties.FirstResvResultIndex;
  with MainForm.ProjectFrame.PropEditor do
  begin
    properties.GetResvProps(Index);
    Clear;

    RowCount := 1;
    for I := 1 to High(properties.ReservoirProps) do
    begin
      Strings.AddPair(properties.ReservoirProps[I], project.Properties[I]);
    end;

    with ItemProps['Elev. Pattern'] do
    begin
      EditStyle := esEllipsis;
      ReadOnly := true;
    end;

    with ItemProps['Source Quality'] do
    begin
      EditStyle := esEllipsis;
      ReadOnly := true;
    end;

    for I := FirstResultRow to High(properties.ReservoirProps) do
      ItemProps[properties.ReservoirProps[I]].ReadOnly := true;
  end;
end;

procedure EditTank(const Index: Integer);
var
  I: Integer;
  OptionList: TStringList;
begin
  FirstResultRow := properties.FirstTankResultIndex;
  OptionList := TStringList.Create;
  try
    with MainForm.ProjectFrame.PropEditor do
    begin
      properties.GetTankProps(Index);
      Clear;
      RowCount := 1;
      for I := 1 to High(properties.TankProps) do
      begin
        Strings.AddPair(properties.TankProps[I], project.Properties[I]);
      end;

      with ItemProps['Volume Curve'] do
      begin
        EditStyle := esEllipsis;
        ReadOnly := true;
      end;

      OptionList.AddStrings(project.NoYesStr, true);
      with ItemProps['Can Overflow'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;

      OptionList.AddStrings(project.MixingModelStr, true);
      with ItemProps['Mixing Model'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;

      with ItemProps['Source Quality'] do
      begin
        EditStyle := esEllipsis;
        ReadOnly := true;
      end;

      for I := FirstResultRow to High(properties.TankProps) do
        ItemProps[properties.TankProps[I]].ReadOnly := true;;
    end;
  finally
    OptionList.Free;
  end;
end;

procedure EditLink(const Item: Integer);
var
  LinkType: Integer;
  Index: Integer;
  CurrentRow: Integer;
begin
  MainForm.ProjectFrame.PropEditor.Hide;
  CurrentRow := MainForm.ProjectFrame.PreviousRow;
  Index := Item+1;
  LinkType := project.GetLinkType(Index);
  case LinkType of
    lCVPipe,
    lPipe:  EditPipe(Index);
    lPump:  EditPump(Index);
    lValve: EditValve(Index);
  end;
  with MainForm.ProjectFrame.PropEditor do
  begin
    if RowCount >= CurrentRow then
      Row := CurrentRow
    else
      Row := 1;
    Show;
  end;
end;

procedure EditPipe(const Index: Integer);
var
  I: Integer;
  OptionList: TStringList;
begin
  FirstResultRow := properties.FirstPipeResultIndex;
  OptionList := TStringList.Create;
  try
    with MainForm.ProjectFrame.PropEditor do
    begin
      properties.GetPipeProps(Index);
      Clear;
      RowCount := 1;
      for I := 1 to High(properties.PipeProps) do
      begin
        Strings.AddPair(properties.PipeProps[I], project.Properties[I]);
      end;
      OptionList.AddStrings(project.StatusStr, true);
      with ItemProps['Initial Status'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;

      for I := FirstResultRow to High(properties.PipeProps) do
      begin
        ItemProps[properties.PipeProps[I]].ReadOnly := true;
      end;
    end;
  finally
    OptionList.Free;
  end;
end;

procedure EditPump(const Index: Integer);
var
  I: Integer;
  OptionList: TStringList;
begin
  FirstResultRow := properties.FirstPumpResultIndex;
  OptionList := TStringList.Create;
  try
    with MainForm.ProjectFrame.PropEditor do
    begin
      properties.GetPumpProps(Index);
      Clear;

      RowCount := 1;
      for I := 1 to High(properties.PumpProps) do
      begin
        Strings.AddPair(properties.PumpProps[I], project.Properties[I]);
      end;

      with ItemProps['Pump Curve'] do
      begin
        EditStyle := esEllipsis;
        ReadOnly := true;
      end;

      with ItemProps['Speed Pattern'] do
      begin
        EditStyle := esEllipsis;
        ReadOnly := true;
      end;

      OptionList.Add(project.StatusStr[0]);
      OptionList.Add(project.StatusStr[1]);
      with ItemProps['Initial Status'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;

      with ItemProps['Effic. Curve'] do
      begin
        EditStyle := esEllipsis;
        ReadOnly := true;
      end;

      with ItemProps['Price Pattern'] do
      begin
        EditStyle := esEllipsis;
        ReadOnly := true;
      end;

      for I := FirstResultRow to High(properties.PumpProps) do
      begin
        ItemProps[properties.PumpProps[I]].ReadOnly := true;
      end;
    end;
  finally
    OptionList.Free;
  end;
end;

procedure EditValve(const Index: Integer);
var
  I: Integer;
  OptionList: TStringList;
begin
  FirstResultRow := properties.FirstValveResultIndex;
  OptionList := TStringList.Create;
  try
    with MainForm.ProjectFrame.PropEditor do
    begin
      properties.GetValveProps(Index);
      Clear;

      RowCount := 1;
      for I := 1 to High(properties.ValveProps) do
      begin
        Strings.AddPair(properties.ValveProps[I], project.Properties[I]);
      end;

      OptionList.AddStrings(project.ValveTypeStr, true);
      with ItemProps['Valve Type'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;

      OptionList.AddStrings(project.ValveStatusStr, true);
      with ItemProps['Fixed Status'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;

      with ItemProps['PCV Curve'] do
      begin
        EditStyle := esEllipsis;
        ReadOnly := true;
      end;

      with ItemProps['GPV Curve'] do
      begin
        EditStyle := esEllipsis;
        ReadOnly := true;
      end;

      for I := FirstResultRow to High(properties.ValveProps) do
      begin
        ItemProps[properties.ValveProps[I]].ReadOnly := true;
      end;
    end;
  finally
    OptionList.Free;
  end;
end;

procedure EditControls(const Item: Integer);
begin
  case Item of
    0: EditSimpleControls;
    1: EditRuleControls;
  end;
end;

procedure EditSimpleControls;
begin
  with TControlsEditorForm.Create(MainForm) do
  try
    LoadControls;
    ShowModal;
    if (ModalResult = mrOk) and HasChanged then
    begin
      project.HasChanged := True;
      project.UpdateResultsStatus;
    end;
  finally
    Free;
  end;
end;

procedure EditRuleControls;
begin
  with TRulesEditorForm.Create(MainForm) do
  try
    LoadRules;
    ShowModal;
    if (ModalResult = mrOk) and HasChanged then
    begin
      project.HasChanged := True;
      project.UpdateResultsStatus;
    end;
  finally
    Free;
  end;
end;

procedure EditLabel(const Item: Integer);
var
  I: Integer;
begin
  with MainForm.ProjectFrame.PropEditor do
  begin
    properties.GetLabelProps(Item);
    Clear;
    RowCount := 1;
    for I := 1 to High(properties.LabelProps) do
    begin
      Strings.AddPair(properties.LabelProps[I], project.Properties[I]);
    end;
    with ItemProps['Font'] do
    begin
      EditStyle := esEllipsis;
      ReadOnly := true;
    end;
  end;
end;

procedure EditLabelFont(const Item: Integer);
var
  MapLabel : TMapLabel;
begin
  MapLabel := TMapLabel(project.MapLabels.Objects[Item]);
  if Maplabel <> nil then with MainForm.FontDialog1 do
  begin
    Font.Assign(MapLabel.Font);
    if Execute then
    begin
      if MapLabel.Font <> Font then project.HasChanged := True;
      MapLabel.Font.Assign(Font);
      MainForm.MapFrame.RedrawMapLabels;
    end;
  end;
end;

procedure PasteProperties(const Category: Integer; const ItemType: Integer;
  const Item: Integer);
begin
  if Category = cNodes then
  begin
    properties.PasteNodeProps(Item+1, ItemType);
    project.HasChanged := True;
    project.UpdateResultsStatus;
  end
  else if Category = cLinks then
  begin
    properties.PasteLinkProps(Item+1, ItemType);
    project.HasChanged := True;
    project.UpdateResultsStatus;
  end;
end;

end.

