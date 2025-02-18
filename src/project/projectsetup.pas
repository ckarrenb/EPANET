{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       projectsetup
 Description:  form unit that edits project default settings
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit projectsetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ValEdit, Grids, ExtCtrls, StrUtils, lclIntf;

const
  IDPrefixName: array[1..8] of string =
    ('Junctions', 'Reservoirs', 'Tanks', 'Pipes', 'Pumps', 'Valves',
     'Patterns', 'Curves');

  PropertyName: array[1..6] of string =
    ('Node Elevation', 'Tank Height', 'Tank Diameter',
     'Pipe Length', 'Pipe Diameter', 'Pipe Roughness');

  HydOptionName: array[1..7] of string =
    ('Flow Units', 'Head Loss Formula', 'Service Pressure',
    'Maximum Trials', 'Accuracy', 'Head Tolerance', 'Flow Tolerance');

  MapExtentsName: array[1..5] of string =
    ('Lower Left X', 'Lower Left Y', 'Upper Right X', 'Upper Right Y',
     'Map Units');

  HintLabels: array[0..3] of string =
    (' Hydraulic analysis options' ,
     ' ID prefixes for new objects',
     ' Properties for new objects',
     ' Map dimensions and units'
     );

type

  { TProjectSetupForm }

  TProjectSetupForm = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    TabControl1: TTabControl;
    ValueListEditor1: TValueListEditor;
    procedure OkBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure TabControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure ValueListEditor1PickListSelect(Sender: TObject);
    procedure ValueListEditor1PrepareCanvas(sender: TObject; aCol,
      aRow: Integer; aState: TGridDrawState);
    procedure ValueListEditor1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    TmpIDprefix: array[1..8] of string;
    TmpDefProps: array[1..6] of string;
    TmpDefOptions: array[1..7] of string;
    TmpMapExtents: array[1..5] of string;
    procedure EditIDPrefixes;
    procedure EditHydOptions;
    procedure EditProperties;
    procedure EditMapExtents;
    function  ValidateEditorValues: Boolean;
    procedure SaveEditorValues;
    procedure SetUnitSystemLabel(FlowUnits: string);
  public
    SaveDefaults: Boolean;

  end;

var
  ProjectSetupForm: TProjectSetupForm;

implementation

{$R *.lfm}

{ TProjectSetupForm }

uses
  main, project, config, utils, mapcoords;

var
  MapExtentRect: TDoubleRect;

procedure TProjectSetupForm.FormCreate(Sender: TObject);
begin
  Color := config.ThemeColor;
  Font.Size := config.FontSize;
  ValueListEditor1.FixedColor := config.ThemeColor;
  with TabControl1 do
  begin
    Tabs.Add('Options');
    Tabs.Add('ID Labels');
    Tabs.Add('Properties');
    if not MainForm.MapFrame.HasWebBasemap then Tabs.Add('Map');
  end;
end;

procedure TProjectSetupForm.FormShow(Sender: TObject);
begin
  MapExtentRect := MainForm.MapFrame.GetExtent;
  TmpMapExtents[1] := Utils.Float2Str(MapExtentRect.LowerLeft.X, 6);
  TmpMapExtents[2] := Utils.Float2Str(MapExtentRect.LowerLeft.Y, 6);
  TmpMapExtents[3] := Utils.Float2Str(MapExtentRect.UpperRight.X, 6);
  TmpMapExtents[4] := Utils.Float2Str(MapExtentRect.UpperRight.Y, 6);
  TmpMapExtents[5] := MapUnitsStr[Project.MapUnits];
  TmpIDprefix := Project.IDprefix;
  TmpDefProps := Project.DefProps;
  Project.GetDefHydOptions(TmpDefOptions);
  SetUnitSystemLabel(TmpDefOptions[1]);
  TabControl1.TabIndex := 0;
  Panel2.Caption := HintLabels[0];
  EditHydOptions;
  ValueListEditor1.Row := 1;
end;

procedure TProjectSetupForm.OkBtnClick(Sender: TObject);
//
//  Saves edited set of defaults to project.
//
begin
  ValueListEditor1.EditorMode := false;
  SaveEditorValues;
  if not ValidateEditorValues then exit;
  Project.SetDefHydOptions(TmpDefOptions);
  Project.IDprefix := TmpIDprefix;
  Project.DefProps := TmpDefProps;
  if TabControl1.Tabs.Count = 4 then
  begin
    Project.MapUnits := AnsiIndexStr(TmpMapExtents[5], Project.MapUnitsStr);
    MainForm.MapFrame.ChangeExtent(MapExtentRect);
    Project.HasChanged := True;
  end;
  SaveDefaults := CheckBox1.Checked;
  ModalResult := mrOk;
end;

procedure TProjectSetupForm.HelpBtnClick(Sender: TObject);
begin
 MainForm.ShowHelp('#project_setup');
end;

procedure TProjectSetupForm.TabControl1Change(Sender: TObject);
//
// Switches contents of the ValueListEditor when a new tab selected
//
begin
  case TabControl1.TabIndex of
  0: EditHydOptions;
  1: EditIDPrefixes;
  2: EditProperties;
  3: EditMapExtents;
  end;
  Panel2.Caption := HintLabels[TabControl1.TabIndex];
  ValueListEditor1.Row := 1;
end;

procedure TProjectSetupForm.TabControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
//
//  Saves contents of ValueListEditor when a new tab is selected
//
begin
  SaveEditorValues;
  AllowChange := true;
end;

procedure TProjectSetupForm.ValueListEditor1PickListSelect(Sender: TObject);
//
//  Changes the Units System label when a new value for Flow Units selected.
//
begin
  if TabControl1.TabIndex = 0 then with ValueListEditor1 do
    if Row = 1 then
      SetUnitSystemLabel(Cells[1, Row]);
end;

procedure TProjectSetupForm.ValueListEditor1PrepareCanvas(sender: TObject;
  aCol, aRow: Integer; aState: TGridDrawState);
begin
//  if (aRow > 0) and (aCol > 0) then
//    ValueListEditor1.Canvas.Brush.Color := clWindow; //clDefault;  //clWindow;
//  if (aRow > 0) and (aCol = 0) then
  if aRow = 0 then
    ValueListEditor1.Canvas.Brush.Color := config.ThemeColor;
end;

procedure TProjectSetupForm.ValueListEditor1SelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
//
//  Selects cell in column 1 when a cell in column 0 is selected.
//
begin
  if aCol = 0 then
  begin
    CanSelect := false;
    ValueListEditor1.Row := aRow;
    ValueListEditor1.Col := 1;
  end
  else CanSelect := true;
end;

procedure TProjectSetupForm.EditIDPrefixes;
//
//  Sets up the ValueListEditor to edit object ID prefixes.
//
var
  I: Integer;
begin
  with ValueListEditor1 do
  begin
    Clear;
    TitleCaptions[0] := 'Object Type';
    TitleCaptions[1] := 'ID Prefix';
    RowCount := 1;
    for I := 1 to 8 do
      InsertRow(IDPrefixName[I], TmpIDprefix[I], true);
    Show;
  end;
end;

procedure TProjectSetupForm.EditHydOptions;
//
//  Sets up the ValueListeditor to edit hydraulic options.
//
var
  I: Integer;
  OptionList: TStringList;
begin
  OptionList := TStringList.Create;
  try
    with ValueListEditor1 do
    begin
      Clear;
      TitleCaptions[0] := 'Hydraulic Option';
      TitleCaptions[1] := 'Value';
      RowCount := 1;
      for I := 1 to 7 do
        InsertRow(HydOptionName[I], TmpDefOptions[I], true);
      OptionList.AddStrings(Project.FlowUnitsStr, true);
      with ItemProps['Flow Units'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;
      OptionList.Clear;
      OptionList.AddStrings(Project.HLossModelStr, true);
      with ItemProps['Head Loss Formula'] do
      begin
        EditStyle := esPickList;
        PickList := OptionList;
        ReadOnly := true;
      end;
     Show;
   end;
   finally
     OptionList.Free;
   end;
end;

procedure TProjectSetupForm.EditProperties;
//
//  Sets up the ValueListEditor to edit node/link properties.
//
var
  I: Integer;
begin
 with ValueListEditor1 do
 begin
   Clear;
   TitleCaptions[0] := 'Object Property';
   TitleCaptions[1] := 'Value';
   RowCount := 1;
   for I := 1 to 6 do
     InsertRow(PropertyName[I], TmpDefProps[I], true);
   Show;
 end;
end;

procedure TProjectSetupForm.EditMapExtents;
//
//  Sets up the ValueListEditor to edit network map extents.
//
var
  I: Integer;
  OptionList: TStringList;
begin
 OptionList := TStringList.Create;
 try
   with ValueListEditor1 do
   begin
     Clear;
     TitleCaptions[0] := 'Map Property';
     TitleCaptions[1] := 'Value';
     RowCount := 1;
     for I := 1 to 5 do
       InsertRow(MapExtentsName[I], TmpMapExtents[I], true);
     OptionList.AddStrings(Project.MapUnitsStr, true);
     with ItemProps['Map Units'] do
     begin
       EditStyle := esPickList;
       PickList := OptionList;
       ReadOnly := true;
     end;
     Show;
   end;
 finally
   OptionList.Free;
 end;
end;

function TProjectSetupForm.ValidateEditorValues: Boolean;
var
  I: Integer;
  Tab: Integer;
  Row: Integer;
  X: Double;
  E: array[1..4] of Double;
  Msg: String = '';
begin
  Result := True;
  Tab := -1;
  Row := -1;
  for I := 1 to 6 do
  begin
    if not TryStrToFloat(TmpDefProps[I], X) then
    begin
      Msg := TmpDefProps[I] + ' is not a valid number.';
      Tab := 2;
      Row := I;
      Result := False;
      break;
    end;
  end;
  if Result = True then for I := 1 to 4 do
  begin
    if not TryStrToFloat(TmpMapExtents[I], E[I]) then
    begin
      Msg := TmpMapExtents[I] + ' is not a valid number.';
      Tab := 3;
      Row := I;
      Result := False;
      break;
    end;
  end;
  if Result = true then
  begin
    MapExtentRect.LowerLeft := DoublePoint(E[1], E[2]);
    MapExtentRect.UpperRight := DoublePoint(E[3], E[4]);
    if SameText(TmpMapExtents[5], Project.MapUnitsStr[muDegrees]) and
       (mapcoords.HasLatLonCoords(MapExtentRect) = false) then
      begin
        Msg := 'Map coordinates must be between -180 and 180 degrees.';
        Tab := 3;
        Row := 1;
        Result := False;
      end;
    end;

  if Result = true then for I := 3 to 7 do
  begin
    if not TryStrToFloat(TmpDefOptions[I], X) then
    begin
      Msg := TmpDefOptions[I] + ' is not a valid value.';
      Tab := 0;
      Row := I;
      Result := False;
      break;
    end;
  end;
  if Result = true then for I := 1 to 8 do
  begin
    if (Pos(' ', TmpIdPrefix[I]) > 0) or (Pos(';', TmpIdPrefix[I]) > 0) then
    begin
      Msg := 'ID labels cannot contain spaces or semi-colons.';
      Tab := 1;
      Row := I;
      Result := False;
      break;
    end;
  end;
  if Result = false then
  begin
    if TabControl1.TabIndex <> Tab then TabControl1.TabIndex := Tab;
    TabControl1Change(self);
    ValueListEditor1.Row := Row;
    Utils.MsgDlg(Msg, mtError, [mbOK]);
    ValueListEditor1.SetFocus;
  end;
end;

procedure TProjectSetupForm.SaveEditorValues;
//
//  Saves current values in the ValueListEditor to the TmpDefaults array.
//
var
  I: Integer;
begin
 with ValueListEditor1 do
   case TabControl1.TabIndex of
     1: for I := 1 to RowCount - 1 do
          TmpIDprefix[I] := Cells[1, I];
     2: for I := 1 to RowCount - 1 do
          TmpDefProps[I] := Cells[1, I];
     0: for I := 1 to RowCount - 1 do
          TmpDefOptions[I] := Cells[1, I];
     3: for I := 1 to RowCount - 1 do
          TmpMapExtents[I] := Cells[1, I];
   end;
end;

procedure TProjectSetupForm.SetUnitSystemLabel(FlowUnits: string);
//
//  Changes the Unit System label for new choice of flow units.
//
begin
  if AnsiIndexText(FlowUnits, Project.FlowUnitsStr) < 5 then
    Label1.Caption := 'Unit System: US'
  else
    Label1.Caption := 'Unit System: SI';
end;

end.

