{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       projectframe
 Description:  displays and edits the properties of project objects
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit projectframe;

{ The ProjectFrame, that appears in the left side panel of the MainForm
  serves as a project's object inspector/property editor. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, Grids,
  Dialogs, Graphics, ValEdit, LCLtype, LCLIntf, Buttons, project, mapcoords;

type

  { TProjectFrame }

  TProjectFrame = class(TFrame)
    EditTitleBtn: TSpeedButton;
    NextItemBtn: TSpeedButton;
    HintPanel: TPanel;
    ItemPanel: TPanel;
    ExplorerPanel: TPanel;
    PrevItemBtn: TSpeedButton;
    Notebook1: TNotebook;
    NotesBox: TGroupBox;
    NotesLabel: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    PropEditor: TValueListEditor;
    PropertyPanel: TPanel;
    PropertyPage: TPage;
    TitleBox: TGroupBox;
    TitleLabel: TLabel;
    TitlePage: TPage;
    TopPanel: TPanel;
    FrameSplitter: TSplitter;
    TreeView1: TTreeView;

    procedure EditTitleBtnClick(Sender: TObject);
    procedure GroupEdit(GroupPoly: TPolygon; NumPolyPts: Integer);
    procedure ItemBtnsClick(Sender: TObject);
    procedure PropEditorEditingDone(Sender: TObject);
    procedure PropEditorPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure PropEditorSelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
    procedure PropEditorButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure PropEditorValidateEntry(Sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: string);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Click(Sender: TObject);

  private
    procedure ShowHelpTopic;
    procedure ShowTitle;
    procedure UpdatePropEditor(aItem: Integer);
    procedure UpdateNodeResults(Index: Integer);
    procedure UpdateLinkResults(Index: Integer);
    procedure UpdateDeleteBtn;

  public
    CopiedCategory: Integer;
    CopiedType: Integer;
    CurrentCategory: Integer;
    PreviousRow: Integer;
    SelectedItem: array [0..project.cLabels] of Integer;
    ValidationNeeded: Boolean;

    procedure CopyItem;
    procedure DeleteItem;
    procedure Init;
    procedure InitSplit;
    procedure PasteItem;
    procedure PropEditorKeyPress(Key: Word);
    procedure SelectItem(aCategory: Integer; aItem: Integer);
    procedure ShowItemID(aItem: Integer);
    procedure UpdateItem(aCategory: Integer; aItem: Integer);
    procedure UpdateResultsDisplay;

  end;

implementation

{$R *.lfm}

uses
  main, editor, properties, groupeditor, mapthemes, config, utils;

const
  // Maps project object categories to their index in TreeView1
  TreeIndex: array[cTitle..cLabels] of Integer =
    (0, 1, 7, 8, 9, 12);

{ TProjectFrame }

procedure TProjectFrame.Init;
var
  I: Integer;
begin
  // Set component colors
  Color := config.ThemeColor;
  Panel2.Color := Color;
  PropEditor.FixedColor:= Color;
  PropEditor.Color := clWindow;

  // Set enabled state of treeview items
  for I := 0 to cLabels do
  begin
    SelectedItem[I] := -1;
    TreeView1.Items[TreeIndex[I]].Enabled := True;
  end;
  for I in [cNodes, cLinks, cLabels] do
  begin
    if project.GetItemCount(I) > 0 then SelectedItem[I] := 0
    else TreeView1.Items[TreeIndex[I]].Enabled := False;
  end;

  // Set initial selected data category
  CurrentCategory := cTitle;
  with TreeView1 do Select(Items[0]);
  SelectItem(cTitle, 0);
  PreviousRow := 0;
  CopiedCategory := 0;
  CopiedType := -1;
  ValidationNeeded := True;
  PropEditor.FastEditing := false;
end;

procedure TProjectFrame.InitSplit;
begin
  ExplorerPanel.Height := Panel1.ClientHeight div 2;
end;

{-------------------------------------------------------------------------------
  Project Explorer Events
-------------------------------------------------------------------------------}

procedure TProjectFrame.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  aCategory: Integer;
  aItem: Integer;
begin
  // Get data category from treeview's selected node
  if not Assigned(TreeView1.Selected) then exit;
  aCategory := TreeView1.Selected.SelectedIndex;

  // Expand tree if Analysis Options chosen
  if (aCategory = cOptions) then
  begin
    TreeView1.Selected.Expand(False);
    exit;
  end;

  // Title/Notes chosen
  if aCategory = cTitle then aItem := 0

  // Sub-category of Analysis Options selected
  else if (aCategory >= 10) and (aCategory < 20) then
  begin
    // Display name of sub-category
    ItemPanel.Caption := 'Analysis Options - ' + TreeView1.Selected.Text;
    // Convert sub-category to an options index
    aItem := aCategory - 10;
    aCategory := cOptions;
  end

  // Nodes, Links, or Map Labels selected
  else if aCategory in [cNodes, cLinks, cLabels] then
  begin
    aItem := SelectedItem[aCategory];
  end

  // Control Actions selected
  else if aCategory = cControls then
  begin
    TreeView1.Selected.Expand(False);
    exit;
  end

  // Sub-category of Control Actions selected
  else if aCategory > 20 then
  begin
    ItemPanel.Caption := '';
    SelectItem(cControls, -1);
    editor.Edit(cControls, aCategory - 21);
    exit;
  end

  else exit;

  // Setup the Property Editor page
  SelectItem(aCategory, aItem);
end;

procedure TProjectFrame.TreeView1Click(Sender: TObject);
begin
  TreeView1Change(Sender, TreeView1.Selected);
end;

procedure TProjectFrame.SelectItem(aCategory: Integer; aItem: Integer);
{
   Displays the properties of a selected object, given its category
   and index, in the lower panel of the ProjectFrame.
}
begin
  // Display the project's Title/Notes
  CurrentCategory := aCategory;
  if CurrentCategory = cTitle then
  begin
    ShowTitle;
    exit;
  end;

  // Make the PropertyPage of the lower panel visible
  Notebook1.PageIndex := 1;
  EditTitleBtn.Visible := False;
  PrevItemBtn.Visible := CurrentCategory in [cNodes, cLinks, cLabels];
  NextItemBtn.Visible := PrevItemBtn.Visible;
  MainForm.MainMenuFrame.UpdateProjectBtns;
  PreviousRow := PropEditor.Row;

  // This line is necessary to prevent an Out-of-Range exception
  // being thrown when the Property Editor is re-populated after
  // a new object has been selected while an in-place editor for
  // some property of the current object is still active.
  if PropertyPanel.Visible then PropEditor.Row := 0;

  // Update the category's selected item
  SelectedItem[CurrentCategory] := aItem;
  if CurrentCategory in [cNodes, cLinks, cLabels] then
    TreeView1.Selected := TreeView1.Items[TreeIndex[Currentcategory]];

  // Hide the Property Editor if the selected category has no items
  if aItem < 0 then
    PropertyPanel.Visible := false
  else
  begin
    TreeView1.Items[TreeIndex[CurrentCategory]].Enabled := True;
    // Need to reset goAlwaysShowEditor so that GridEditor properties
    // get displayed properly
    PropEditor.Options := PropEditor.Options - [goAlwaysShowEditor];
    UpdatePropEditor(aItem);
    UpdateDeleteBtn;
    MainForm.MapFrame.HiliteObject(CurrentCategory, aItem + 1);
    PropEditor.Options := PropEditor.Options + [goAlwaysShowEditor];
  end;

  with MainForm.TseriesSelectorFrame do
    if Visible then SetSelectedObjectProps;

end;

procedure TProjectFrame.UpdateDeleteBtn;
begin
  with MainForm.MainMenuFrame.ProjectDeleteBtn do
  begin
    if CurrentCategory in [cTitle, cOptions, cControls] then
      Enabled := False
    else if project.GetItemCount(CurrentCategory) = 0 then
      Enabled := False
    else
      Enabled := True;
  end;
end;

procedure TProjectFrame.DeleteItem;
var
  Nitems: Integer;
  aItem: Integer;
  Msg: string;
begin
  // Confirm item deletion
  aItem := SelectedItem[CurrentCategory];
  if config.ConfirmDeletions then
  begin
    Msg := 'Do you wish to delete ' +
           project.GetItemTypeStr(CurrentCategory, aItem) +
           project.GetItemID(CurrentCategory, aItem);
    if CurrentCategory = cNodes then
      Msg := Msg + ' and all of its connecting links';
    Msg := Msg + '?';
    if utils.MsgDlg(Msg, mtConfirmation, [mbYes, mbNo]) = mrNo then exit;
  end;

  // Delete item from project
  project.DeleteItem(CurrentCategory, aItem + 1);

  // Update Project Explorer if deleting a node results in no more links
  if (Currentcategory = cNodes) and (project.GetItemCount(cLinks) = 0) then
  begin
    SelectedItem[cLinks] := -1;
    TreeView1.Items[TreeIndex[cLinks]].Enabled := False;
  end;

  // Get remaining number of items for current category
  Nitems := project.GetItemCount(CurrentCategory);

  // No more items remain - disable category in Project Explorer
  if Nitems = 0 then
  begin
    SelectedItem[CurrentCategory] := -1;
    MainForm.MapFrame.HiliteObject(CurrentCategory, aItem);
    //PropertyPanel.Visible := False;
    TreeView1.Items[TreeIndex[CurrentCategory]].Enabled := False;
    TreeView1.Select(TreeView1.Items[0]);
  end

  // Shift selected item for the current object category
  else begin
    if aItem = Nitems then Dec(aItem);
    SelectItem(CurrentCategory, aItem);
  end;
  UpdateDeleteBtn;
end;

procedure TProjectFrame.CopyItem;
var
  I: Integer;
begin
  // Initialize info on item being copied
  CopiedCategory := 0;
  CopiedType := -1;

  // A node is being copied (junction, reservoir or tank)
  if CurrentCategory = cNodes then
  begin
    CopiedCategory := cNodes;
    CopiedType := Project.GetNodeType(SelectedItem[cNodes]+1);
  end

  // A link is being copied (pipe, pump or valve)
  else if CurrentCategory = cLinks then
  begin
    CopiedCategory := cLinks;
    CopiedType := Project.GetLinkType(SelectedItem[cLinks]+1);
  end

  // Can't copy other categories of objects
  else
    exit;

  // Save the properties of the item being copied
  Project.CopiedProperties.Clear;
  with PropEditor do
    for I := 0 to RowCount do
      Project.CopiedProperties.Add(Cells[1, I]);
end;

procedure TProjectFrame.PasteItem;
var
  Item: Integer;
begin
  Item := SelectedItem[CurrentCategory];
  if Project.CanPasteItem(Item, CopiedCategory, CopiedType) then
  begin
    Editor.PasteProperties(CurrentCategory, CopiedType, Item);
    UpdatePropEditor(Item);
  end
  else
    Utils.MsgDlg('Source and destination objects are not the same type.',
      mtInformation, [mbOK]);
end;

procedure TProjectFrame.GroupEdit(GroupPoly: TPolygon; NumPolyPts: Integer);
begin
  with TGroupEditorForm.Create(MainForm) do
  try
    Init(GroupPoly, NumPolyPts);
    ShowModal;
    if (ModalResult = mrOk) and HasChanged then
    begin
      Project.HasChanged := True;
      Project.UpdateResultsStatus;
      MainForm.MapFrame.RedrawMap;
    end;
  finally
    Free;
  end;
end;

procedure TProjectFrame.ItemBtnsClick(Sender: TObject);
var
  CurrentItem: Integer;
  MoveTo: Integer;
begin
  // Tag for NextItemBtn is +1, for PrevItemBtn is -1
  with Sender as TSpeedButton do MoveTo := Tag;

  // Change the current item depending on which button pressed
  CurrentItem := SelectedItem[CurrentCategory];
  if (MoveTo = 1) and
    (CurrentItem < project.GetItemCount(CurrentCategory) - 1) then
      Inc(CurrentItem)
  else if (MoveTo = -1) and (CurrentItem > 0) then Dec(CurrentItem)
  else exit;

  // Update the Property Editor and highlighted map object with new item
  SelectedItem[CurrentCategory] := CurrentItem;
  UpdatePropEditor(SelectedItem[CurrentCategory]);
  MainForm.MapFrame.HiliteObject(CurrentCategory, CurrentItem + 1);
end;

procedure TProjectFrame.EditTitleBtnClick(Sender: TObject);
begin
  editor.EditTitleText;
  ShowTitle;
end;

procedure TProjectFrame.PropEditorEditingDone(Sender: TObject);
begin
  //showmessage('Editing done');
end;

procedure TProjectFrame.ShowTitle;
begin
  ItemPanel.Caption:= '  Title / Notes';
  PrevItemBtn.Visible := False;
  NextItemBtn.Visible := False;
  EditTitleBtn.Visible := True;
  Notebook1.PageIndex := 0;
  TitleLabel.Caption := project.GetTitle(0);
  NotesLabel.Caption := project.GetTitle(1) + ' ' + project.GetTitle(2);
end;

procedure TProjectFrame.UpdatePropEditor(aItem: Integer);
begin
  ShowItemID(aItem);
  if PropEditor.Row > 0 then PreviousRow := PropEditor.Row;
  PropertyPanel.Visible := true;
  ValidationNeeded := False;
  Editor.Edit(CurrentCategory, aItem);
  ValidationNeeded := True;
end;

procedure TProjectFrame.UpdateItem(aCategory: Integer; aItem: Integer);
begin
  if (CurrentCategory = aCategory) and
     (SelectedItem[CurrentCategory] = aItem) then
    Editor.Edit(CurrentCategory, aItem);
end;

procedure TProjectFrame.ShowItemID(aItem: Integer);
begin
  if CurrentCategory in [cNodes, cLinks] then
    ItemPanel.Caption := ' ' + project.GetItemTypeStr(CurrentCategory, aItem) +
      ' ' + project.GetItemID(CurrentCategory, aItem)
  else if CurrentCategory = cLabels then ItemPanel.Caption := ' Map Labels';
end;

procedure TProjectFrame.UpdateResultsDisplay;
begin
  if CurrentCategory = cNodes then
    UpdateNodeResults(SelectedItem[CurrentCategory]+1)
  else if Currentcategory = cLinks then
    UpdateLinkResults(SelectedItem[CurrentCategory]+1);
end;

procedure TProjectFrame.UpdateNodeResults(Index: Integer);
var
  I: Integer;
  J: Integer;
  K: Integer;
  X: Single;
  S: String;
begin
  J := Project.GetNodeType(Index);
  case J of
  nJunction:  K := FirstJuncResultIndex;
  nReservoir: K := FirstResvResultIndex;
  nTank:      K := FirstTankResultIndex;
  end;

  PropEditor.EditorMode := False;
  for I := mapthemes.FirstNodeResultTheme to
           mapthemes.FirstNodeQualTheme-1 do
  begin
    if (J <> nJunction) and (I in [ntDmndDfct, ntEmittance, ntLeakage]) then
      continue;
    if (J = nReservoir) and (I = ntPressure) then continue;
    X := mapthemes.GetNodeValue(Index, I, mapthemes.TimePeriod);
    if X = MISSING then S := 'N/A' else
    begin
      if (project.GetUnitsSystem = usUS) and  (J = nTank) and (I = ntPressure) then
        X := X / 0.4333;
      if (J = nReservoir) and (I = ntDemand) then X := -X;
      S := FloatToStrF(X, ffFixed, 7, config.DecimalPlaces);
    end;
    PropEditor.Cells[1, K] := S;
    Inc(K);
  end;
  PropEditor.EditorMode := True;
end;

procedure TProjectFrame.UpdateLinkResults(Index: Integer);
var
  I: Integer;
  J: Integer;
  K: Integer;
  X: Single;
  S: String;
begin
  J := Project.GetLinkType(Index);
  case J of
  lPipe:  K := FirstPipeResultIndex;
  lPump:  K := FirstPumpResultIndex;
  lValve: K := FirstValveResultIndex;
  end;
  PropEditor.EditorMode := False;
  for I := mapthemes.FirstLinkResultTheme to
           mapthemes.FirstLinkQualTheme-1 do
  begin
    X := mapthemes.GetLinkValue(Index, I, mapthemes.TimePeriod);
    if X = MISSING then S := 'N/A'
    else S := FloatToStrF(X, ffFixed, 7, config.DecimalPlaces);
    PropEditor.Cells[1, K] := S;
    Inc(K);
  end;
  PropEditor.EditorMode := True;
end;

{-------------------------------------------------------------------------------
  Property Editor Events
-------------------------------------------------------------------------------}

procedure TProjectFrame.PropEditorPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if not (sender is TValueListEditor) then exit;

  // Header row color
  if (aRow = 0) then PropEditor.Canvas.Brush.Color := config.ThemeColor;

  // Color used for cells that display simulation results
  if (aCol = 1) then
  begin
    if (Editor.FirstResultRow > 0) and
      (aRow >= Editor.FirstResultRow) then
        PropEditor.Canvas.Brush.Color := clCream;  //$00E1FFFF;
  end;
end;

procedure TProjectFrame.PropEditorKeyPress(Key: Word);
//
//  Called by MainForm's OnKeyPress procedure
//
begin
  // Return key press activates an ellipsis button in the editor
  if Key = VK_RETURN then with PropEditor do
  begin
    if ItemProps[Cells[0,Row]].EditStyle = esEllipsis then
      PropEditorButtonClick(PropEditor, 1, Row);
  end

  // F1 key press brings up Help
  else if Key = VK_F1 then ShowHelpTopic;
end;

procedure TProjectFrame.PropEditorButtonClick(Sender: TObject; aCol,
  aRow: Integer);
begin
  Editor.ButtonClick(CurrentCategory, SelectedItem[CurrentCategory], aRow);
end;

procedure TProjectFrame.PropEditorSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  // Can't select cells that display simulation results
  if (Editor.FirstResultRow > 0) and
     (aRow >= Editor.FirstResultRow) then CanSelect := False

  // Can't select cells in column 0 that display property names
  else if aCol = 0 then
  begin
    CanSelect := false;
    PropEditor.Row := aRow;
    PropEditor.Col := 1;
  end

  else CanSelect := true;
end;

procedure TProjectFrame.PropEditorValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
begin
  if ValidationNeeded then
  begin
    if not Editor.Validate(CurrentCategory, SelectedItem[CurrentCategory], aRow,
      OldValue, NewValue) then PropEditor.SetFocus;

//    if not Editor.Validate(CurrentCategory, ItemsGrid.Row, aRow, OldValue, NewValue)
//    then PropEditor.SetFocus;
  end;
end;

procedure TProjectFrame.ShowHelpTopic;
var
  Topic: String;
begin
  Topic := '';

  case CurrentCategory of
  cOptions:
    case SelectedItem[cOptions] of
    oHydraul: Topic := '#hydraulic_options';
    oDemands: Topic := '#demand_options';
    oQuality: Topic := '#water_quality_options';
    oTimes:   Topic := '#time_options';
    oEnergy:  Topic := '#energy_options';
    end;
  cNodes:
    case Project.GetNodeType(SelectedItem[cNodes]+1) of
    nJunction:  Topic := '#junction_properties';
    nReservoir: Topic := '#reservoir_properties';
    nTank:      Topic := '#tank_properties';
    end;
  cLinks:
    case Project.GetLinkType(SelectedItem[cLinks]+1) of
    lPipe:  Topic := '#pipe_properties';
    lPump:  Topic := '#pump_properties';
    lValve: Topic := '#valve_properties';
    end;
  cLabels:
    Topic := '#label_properties';
  end;

  if Length(Topic) > 0 then MainForm.ShowHelp(Topic);
end;

end.

