{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       maplocator
 Description:  a frame that locates objects on the network map
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit maplocater;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, LCLtype, ExtCtrls, StdCtrls, Buttons;

type

  // Types of objects to locate (ftNode & ftLink) or to list
  TFindType = (ftNode, ftLink, ftTanks, ftReservoirs, ftSources,
               ftPumps, ftValves);

  { TMapLocaterFrame }

  TMapLocaterFrame = class(TFrame)
    SearchBtn: TBitBtn;
    CloseBtn: TSpeedButton;
    FindCbx: TComboBox;
    NameEdt: TEdit;
    ResultsLbl: TLabel;
    NamedLbl: TLabel;
    FindLbl: TLabel;
    ItemsLbx: TListBox;
    TopPanel: TPanel;
    procedure CloseBtnClick(Sender: TObject);
    procedure FindCbxChange(Sender: TObject);
    procedure NameEdtChange(Sender: TObject);
    procedure NameEdtKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ItemsLbxClick(Sender: TObject);
    procedure SearchBtnClick(Sender: TObject);
  private
    procedure FindObject(FindType: TFindType);
    procedure FindObjects(FindType:TFindType);
    procedure GetAdjacentObjects(FoundObjType, FoundObjIndex: Integer);
  public
    procedure Clear;
    procedure Show;
    procedure Hide;
  end;

implementation

{$R *.lfm}

uses
  main, project, config, utils;

const
  TXT_NO_SUCH_OBJECT = 'There is no such object in the network.';
  TXT_NO_OBJECTS = 'There are no objects of that type in the network.';
  ResultsLblCaption: array[0..6] of String =
    ('Adjacent Links', 'Adjacent Nodes', 'Tank Nodes', 'Reservoir Nodes',
     'WQ Source Nodes', 'Pump Links', 'Valve Links');

{ TMapLocaterFrame }

procedure TMapLocaterFrame.Clear;
begin
  NameEdt.Text := '';
  ItemsLbx.Clear;
end;

procedure TMapLocaterFrame.Show;
begin
  TopPanel.Color := config.ThemeColor;
  Visible := True;
  Clear;
end;

procedure TMapLocaterFrame.Hide;
begin
  Visible := False;
end;

procedure TMapLocaterFrame.SearchBtnClick(Sender: TObject);
var
  FindType: TFindType;
begin
  // Clear the found items list box
  ItemsLbx.Clear;

  // What type of object are we looking for
  FindType := TFindType(FindCbx.ItemIndex);

  // Looking for a named node or link
  if FindType in [ftNode, ftLink] then FindObject(FindType)

  // Looking for all nodes/links of a given type
  else FindObjects(FindType);
end;

procedure TMapLocaterFrame.FindObject(FindType: TFindType);
var
  ID    : String;
  FoundObjType: Integer;
  FoundObjIndex: Integer;
begin
  // Place map in Object Selection mode
  MainForm.MapFrame.EnterSelectionMode;

  // Search project for specified node/link ID
  ID := NameEdt.Text;
  ItemsLbx.Clear;
  if (FindType = ftNode) then
    FoundObjType := cNodes
  else
    FoundObjType := cLinks;
  FoundObjIndex := project.GetItemIndex(FoundObjType, ID);

  // If object exists then select it and list its adjacent objects
  if FoundObjIndex > 0 then
  begin
    GetAdjacentObjects(FoundObjType, FoundObjIndex);
    MainForm.ProjectFrame.SelectItem(FoundObjType,FoundObjIndex - 1)
  end

  // If not found then issue a message.
  else
    utils.MsgDlg(TXT_NO_SUCH_OBJECT, mtInformation, [mbOK], MainForm);

  // Return focus to the NameEdt control
  NameEdt.SetFocus;
  NameEdt.SelectAll;
end;

procedure TMapLocaterFrame.NameEdtChange(Sender: TObject);
begin
  ItemsLbx.Clear;
end;

procedure TMapLocaterFrame.CloseBtnClick(Sender: TObject);
begin
  Hide;
end;

procedure TMapLocaterFrame.FindCbxChange(Sender: TObject);

// Changes the contents of the frame depending on choice of objects to be found.

var
  EnableNameEdt: Boolean;
begin
  // NameEdt visible only when we want to locate a specific node or link
  EnableNameEdt := TFindType(FindCbx.ItemIndex) in [ftNode, ftLink];
  NamedLbl.Visible := EnableNameEdt;
  NameEdt.Clear;
  NameEdt.Enabled := EnableNameEdt;
  ResultsLbl.Caption := ResultsLblCaption[FindCbx.ItemIndex];
  ItemsLbx.Clear;
end;

procedure TMapLocaterFrame.NameEdtKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then SearchBtnClick(Sender);
end;

procedure TMapLocaterFrame.ItemsLbxClick(Sender: TObject);

// Highlight the object clicked on in the results listbox on the map.

var
  ID: String;
  ObjType: Integer;
  ObjIndex: Integer;
  FoundType: TFindType;
begin
  // Get ID of selected item
  if ItemsLbx.Items.Count = 0 then exit;
  if ItemsLbx.ItemIndex < 0 then exit;
  ID := ItemsLbx.Items[ItemsLbx.ItemIndex];

  // Set type of object to find on network map
  FoundType := TFindType(FindCbx.ItemIndex);
  if FoundType in [ftLink, ftTanks, ftReservoirs, ftSources] then
    ObjType := cNodes
  else ObjType := cLinks;

  // Get object's index in the network database
  ObjIndex := project.GetItemIndex(ObjType, ID);

  // If object exists then make it the current selected object which
  // will highlight it on the network map
  if ObjIndex > 0 then
    MainForm.ProjectFrame.SelectItem(ObjType, ObjIndex - 1);
end;

procedure TMapLocaterFrame.GetAdjacentObjects(FoundObjType, FoundObjIndex: Integer);
var
  I: Integer;
  Node1: Integer = 0;
  Node2: Integer = 0;
begin
  if FoundObjType = cLinks then
  begin
    project.GetLinkNodes(FoundObjIndex, Node1, Node2);
    ItemsLbx.Items.Add(project.GetID(cNodes, Node1));
    ItemsLbx.Items.Add(project.GetID(cNodes, Node2));
  end
  else if FoundObjType = cNodes then
  begin
    for I := 1 to project.GetItemCount(cLinks) do
    begin
      project.GetLinkNodes(I, Node1, Node2);
      if (Node1 = FoundObjIndex) or (Node2 = FoundObjIndex) then
        ItemsLbx.Items.Add(project.GetID(cLinks, I));
    end;
  end;
end;

procedure TMapLocaterFrame.FindObjects(FindType:TFindType);
var
  I, N, Imax, ObjClass: Integer;
  Found: Boolean;
begin
  // Which class of object are we looking for
  if FindType in [ftTanks .. ftSources] then
  begin
    ObjClass := cNodes;
    Imax := project.GetItemCount(cNodes);
  end else
  begin
    ObjClass := cLinks;
    Imax := project.GetItemCount(cLinks);
  end;

  // Loop through each object in the class (Nodes or Links)
  N := 0;
  for I := 1 to Imax do
  begin
    // Check if the object is of the type being searched for
    Found := False;
    case FindType of
      ftTanks:      if project.GetNodeType(I) = nTank then Found := True;
      ftReservoirs: if project.GetNodeType(I) = nReservoir then Found := True;
      ftSources:    if project.GetSourceQual(I) > 0 then Found := True;
      ftPumps:      if project.GetLinkType(I) = lPump then Found := True;
      ftValves:     if project.GetLinkType(I) = lValve then Found := True;
    end;

    // if it is then add its ID to the items in the ItemsLbx
    if Found then
    begin
      Inc(N);
      ItemsLbx.Items.Add(project.GetID(ObjClass, I));
    end;
  end;

  // Message posted if no items of type searched for are found
  if N = 0 then
    utils.MsgDlg(TXT_NO_OBJECTS, mtInformation, [mbOK], MainForm);
end;

end.

