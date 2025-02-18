{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       controlseditor
 Description:  a form that edits a project's set of simple controls
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit controlseditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, lclIntf,
  Buttons, ExtCtrls, LCLtype, Grids, Types;

type

  { TControlsEditorForm }

  TControlsEditorForm = class(TForm)
    AddBtn: TBitBtn;
    CancelBtn: TButton;
    ControlsGrid: TStringGrid;
    DeleteBtn: TBitBtn;
    DownBtn: TBitBtn;
    EditBtn: TBitBtn;
    OkBtn: TButton;
    HelpBtn: TButton;
    UpBtn: TBitBtn;
    procedure AddBtnClick(Sender: TObject);
    procedure ControlsGridCheckboxToggled(Sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure ControlsGridPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure DeleteBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
  private
    function ControlToStr(const I: Integer): String;
    function StrToControl(const sControl: String; var aType: Integer;
      var aLink: Integer; var aNode: Integer; var aSetting: Single;
      var aLevel: Single): Boolean;
    procedure SetButtonStates;
    function  GoEditControl(const sControl: String): String;
  public
    HasChanged: Boolean;
    procedure LoadControls;

  end;

var
  ControlsEditorForm: TControlsEditorForm;

implementation

{$R *.lfm}

uses
  main, controledit, epanet2, project, utils, config;

{ TControlsEditorForm }

procedure TControlsEditorForm.FormCreate(Sender: TObject);
begin
  Color := Config.ThemeColor;
  Font.Size := Config.FontSize;
  ControlsGrid.RowCount := 1;
end;

procedure TControlsEditorForm.LoadControls;
var
  Ncontrols: Integer;
  I: Integer;
  ControlStr: String;
  UseControl: Integer;
begin
  // Find number of controls currently in the project
  Ncontrols := 0;
  epanet2.ENgetcount(EN_CONTROLCOUNT, Ncontrols);

  // Add each control to the ControlsGrid
  ControlsGrid.RowCount := Ncontrols + 1;
  for I := 1 to Ncontrols do
  begin
    ControlStr := ControlToStr(I);
    UseControl := 1;
    epanet2.ENgetcontrolenabled(I, UseControl);
    ControlsGrid.Cells[0,I] := IntToStr(UseControl);
    ControlsGrid.Cells[1,I] := ControlStr;
  end;

  // Select the first control in the ControlsGrid
  if Ncontrols > 0 then ControlsGrid.Row := 1;
  SetButtonStates;
  HasChanged := False;
end;

procedure TControlsEditorForm.OkBtnClick(Sender: TObject);
var
  I: Integer;
  S: String;
  Ncontrols: Integer = 0;
  UseControl: Integer;
  aType: Integer;
  aLink: Integer;
  aNode: Integer;
  aSetting: Single;
  aLevel: Single;
  Index: Integer = 0;
begin
  // Remove all currrent controls from the project
  epanet2.ENgetcount(EN_CONTROLCOUNT, Ncontrols);
  for I := Ncontrols downto 1 do epanet2.ENdeletecontrol(I);

  // Add all control statements in the ControlsGrid to the project
  for I := 1 to ControlsGrid.RowCount-1 do
  begin
    S := ControlsGrid.Cells[1,I];
    StrToControl(S, aType, aLink, aNode, aSetting, aLevel);
    epanet2.ENaddcontrol(aType, aLink, aSetting, aNode, aLevel, Index);
    UseControl := 1;
    if SameText(ControlsGrid.Cells[0,I], '0') then UseControl := 0;
    epanet2.ENsetcontrolenabled(I, UseControl);
  end;
  ModalResult := mrOk;
end;

procedure TControlsEditorForm.AddBtnClick(Sender: TObject);
var
  NewControl: String;
  ControlIndex: Integer;
begin
  ControlIndex := ControlsGrid.RowCount;
  NewControl := GoEditControl('');
  if Length(NewControl) = 0 then exit;
  with ControlsGrid do
  begin
    RowCount := ControlIndex + 1;
    Cells[0,ControlIndex] := '1';
    Cells[1,ControlIndex] := NewControl;
  end;
  HasChanged := True;
end;

procedure TControlsEditorForm.ControlsGridCheckboxToggled(Sender: TObject;
  aCol, aRow: Integer; aState: TCheckboxState);
begin
  HasChanged := True;
end;

procedure TControlsEditorForm.ControlsGridPrepareCanvas(Sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if aRow > 0 then with Sender as TStringGrid do
  begin
    if gdSelected in aState then
    begin
      Canvas.Brush.Color := $00FFE8CD;
      Canvas.Font.Color := clBlack;
    end;
  end;
end;

procedure TControlsEditorForm.DeleteBtnClick(Sender: TObject);
var
  ControlIndex: Integer;
begin
  with ControlsGrid do
  begin
    if Row > 0 then
    begin
      ControlIndex := Row;
      DeleteRow(Row);
      if ControlIndex >= RowCount then ControlIndex := RowCount - 1;
      Row := ControlIndex;
      HasChanged := True;
    end;
  end;
  SetButtonStates;
end;

procedure TControlsEditorForm.UpBtnClick(Sender: TObject);
var
  I, K: Integer;
begin
  if (Sender as TBitBtn).Name = 'UpBtn' then K := -1 else K := 1;
  with ControlsGrid do
  begin
    I := Row;
    if (K = -1) and (I <= 1) then exit;
    if (K = 1) and (I >= RowCount-1) then exit;
    MoveColRow(False, I, I+K);
    HasChanged := True;
  end;
end;

procedure TControlsEditorForm.EditBtnClick(Sender: TObject);
var
  NewControl: String;
  ControlIndex: Integer;
begin
  with ControlsGrid do
  begin
    if RowCount = 1 then exit;
    ControlIndex := Row;
    NewControl := GoEditControl(Cells[1,Row]);
    if Length(NewControl) = 0 then exit;
    if not SameText(Cells[1,Row], NewControl) then
    begin
      Cells[1,ControlIndex] := NewControl;
      HasChanged := True;
    end;
  end;
end;

procedure TControlsEditorForm.HelpBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('#simple_controls_editor');
end;

procedure TControlsEditorForm.SetButtonStates;
var
  State: Boolean;
begin
  State := ControlsGrid.RowCount > 1;
  DeleteBtn.Enabled := State;
  EditBtn.Enabled := State;
  State := ControlsGrid.RowCount > 2;
  UpBtn.Enabled := State;
  DownBtn.Enabled := State;
end;

function TControlsEditorForm.ControlToStr(const I: Integer): String;
var
  aType: Integer;
  aLink: Integer;
  aNode: Integer;
  aSetting: Single;
  aLevel: Single;
  sLink: String;
  sNode: String;
  sSetting: String;
  aTime: Integer;
  LinkType: Integer;
begin
  // Get the control's properties
  epanet2.ENgetcontrol(I, aType, aLink, aSetting, aNode, aLevel);

  // The controlled link's ID
  sLink := project.GetID(cLinks, aLink);

  // The controlled link's setting
  epanet2.ENgetlinktype(aLink, LinkType);
  if aSetting = EN_SET_CLOSED then sSetting := 'CLOSED'
  else if aSetting = EN_SET_OPEN then sSetting := 'OPEN'
  else if (LinkType = EN_PIPE) or (LinkType = EN_PUMP) then
  begin
    if aSetting = 0 then sSetting := 'CLOSED'
    else if aSetting = 1 then sSetting := 'OPEN'
    else sSetting := utils.Float2Str(aSetting, 4);
  end
  else sSetting := utils.Float2Str(aSetting, 4);

  // The link ID & its setting portion of the control string
  Result := 'LINK ' + sLink + ' ' + sSetting;

  // The conditional elements of the control added its string format
  case aType of
  EN_LOWLEVEL:
    begin
    sNode := project.GetID(cNodes, aNode);
    Result := Result + ' IF NODE ' + sNode + ' BELOW ' + utils.Float2Str(aLevel, 4);
    end;
  EN_HILEVEL:
    begin
    sNode := project.GetID(cNodes, aNode);
    Result := Result + ' IF NODE ' + sNode + ' ABOVE ' + utils.Float2Str(aLevel, 4);
    end;
  EN_TIMER:
    begin
    aTime := Round(aLevel);
    Result := Result + ' AT TIME ' + utils.Time2Str(aTime);
    end;
  EN_TIMEOFDAY:
    begin
    aTime := Round(aLevel);
    Result := Result + ' AT CLOCKTIME ' + utils.Time2Str(aTime);
    end;
  end;
end;

function TControlsEditorForm.StrToControl(const sControl: String;
  var aType: Integer; var aLink: Integer; var aNode: Integer;
  var aSetting: Single; var aLevel: Single): Boolean;
var
  sList: TStringList;
begin
  Result := True;
  sList := TStringList.Create;
  try
    sList.DelimitedText := sControl;
    if sList.Count >= 6 then
    begin
      aNode := 0;
      aLink := project.GetItemIndex(cLinks, sList[1]);
      if aLink = 0 then Result := false;
      if SameText(sList[2], 'OPEN') then aSetting := EN_SET_OPEN
      else if SameText(sList[2], 'CLOSED') then aSetting := EN_SET_CLOSED
      else if not Str2Float(sList[2], aSetting) then Result := false;
      if SameText(sList[4], 'NODE') then
      begin
        aNode := project.GetItemIndex(cNodes, sList[5]);
        if aNode = 0 then Result := false;
        if SameText(sList[6], 'BELOW') then aType := EN_LOWLEVEL
        else if SameText(sList[6], 'ABOVE') then aType := EN_HILEVEL
        else Result := false;
        if not Str2Float(sList[7], aLevel) then Result := false;
      end
      else if SameText(sList[4], 'Time') then
      begin
        aType := EN_TIMER;
        aLevel := utils.Str2Seconds(sList[5]);
      end
      else if SameText(sList[4], 'ClockTime') then
      begin
        aType := EN_TIMEOFDAY;
        aLevel := utils.Str2Seconds(sList[5]);
      end
      else Result := false;
    end
    else Result := false;
  finally
    sList.Free;
  end;
end;

function TControlsEditorForm.GoEditControl(const sControl: String): String;
var
  CtrlEditor: TControlEditForm;
begin
  Result := '';
  Hide;
  CtrlEditor := TControlEditForm.Create(self);
  try
    CtrlEditor.SetControlProperties(sControl);
    CtrlEditor.ShowModal;
    if CtrlEditor.ModalResult = mrOK then
      Result := CtrlEditor.EditedControl;
  finally
    CtrlEditor.Free;
    Show;
  end;
end;

end.

