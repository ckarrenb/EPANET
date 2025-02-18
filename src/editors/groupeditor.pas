{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       groupeditor
 Description:  changes a property for a group of objects located
               within a designated area of the network
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit groupeditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, lclIntf,
  ExtCtrls, Math, mapcoords;

type

  { TGroupEditorForm }

  TGroupEditorForm = class(TForm)
    ActionCombo: TComboBox;
    CancelBtn: TButton;
    FilterCheckBox: TCheckBox;
    FilterParamCombo: TComboBox;
    FilterRelationCombo: TComboBox;
    FilterValueEdit: TEdit;
    JunctionsRadioBtn: TRadioButton;
    Label1: TLabel;
    Label2: TLabel;
    InRegionLabel: TLabel;
    OkBtn: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    ParamCombo: TComboBox;
    PipesRadioBtn: TRadioButton;
    ValueEdit: TEdit;
    procedure OkBtnClick(Sender: TObject);
    procedure FilterCheckBoxChange(Sender: TObject);
    procedure FilterParamComboChange(Sender: TObject);
    procedure ActionComboChange(Sender: TObject);
    procedure ParamComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JunctionsRadioBtnChange(Sender: TObject);
  private
    ObjType: Integer;
    FilterParam: Integer;
    FilterRelation: Integer;
    FilterValue: Single;
    FilterTag: String;
    ChangeAction: Integer;
    ChangeParam: Integer;
    ChangeValue: Single;
    ChangeTag: String;
    GroupPoly: TPolygon;
    NumPolyPts: Integer;
    GroupBounds: TDoubleRect;
    procedure SetParamChoices(aComboBox: TComboBox; Choices: String);
    function  GoChangeValues: Boolean;
    function  ObjValueChanged(I: Integer): Boolean;
    function  PassesLocationFilter(I: Integer): Boolean;
    function  PassesTextFilter(I: Integer): Boolean;
    function  PassesNumericalFilter(I: Integer): Boolean;
    function  GetNewValue(Index: Integer): Single;

  public
    HasChanged: Boolean;
    procedure Init(Poly: TPolygon; NumPts: Integer);

  end;

var
  GroupEditorForm: TGroupEditorForm;

implementation

{$R *.lfm}

uses
  main, project, config, utils, epanet2;

{ TGroupEditorForm }

const
  EN_TAG = -999;

  // EN_xxx are EPANET constants defined in the epanet2.pas file
  JuncParamCode: array[0 .. 5] of Integer =
    ( EN_TAG, EN_ELEVATION, EN_BASEDEMAND, EN_PATTERN, EN_EMITTER, EN_INITQUAL);
  JuncParamsTxt: String =
    'Tag'#13'Elevation'#13'Base Demand'#13'Demand Pattern'#13+
    'Emitter Coeff.'#13'Initial Quality';

  PipeParamCode: array[0..8] of Integer =
    ( EN_TAG, EN_DIAMETER, EN_LENGTH, EN_ROUGHNESS, EN_MINORLOSS,
      EN_KBULK, EN_KWALL, EN_LEAK_AREA, EN_LEAK_EXPAN);
  PipeParamsTxt: String =
    'Tag'#13'Diameter'#13'Length'#13'Roughness'#13'Loss Coeff.'#13 +
    'Bulk Coeff.'#13'Wall Coeff.'#13'Leak Area'#13'Leak Expansion';

  BELOW = 0;
  EQUAL = 1;
  ABOVE = 2;
  FilterRelationsTxt: String = 'Below'#13'Equal To'#13'Above';

  REPLACE = 0;
  MULTIPLY = 1;
  ADD = 2;
  ChangeActionsTxt: String = 'Replace'#13'Multiply'#13'Add To';

procedure TGroupEditorForm.FormCreate(Sender: TObject);
var
  Location: TPoint;
begin
  Color := config.ThemeColor;
  Font.Size := config.FontSize;
  FilterRelationCombo.Items.Text := FilterRelationsTxt;
  FilterRelationCombo.ItemIndex := EQUAL;
  ActionCombo.Items.Text := ChangeActionsTxt;
  ActionCombo.ItemIndex := REPLACE;
  JunctionsRadioBtnChange(Self);
  NumPolyPts := 0;
  HasChanged := False;

  Location := MainForm.LeftPanel.ClientOrigin;
  Left := Location.X;
  Top := Location.Y;
end;

procedure TGroupEditorForm.Init(Poly: TPolygon; NumPts: Integer);
var
  I: Integer;
  Bmin, Bmax: TDoublePoint;
begin
  GroupPoly := Poly;
  NumPolyPts := NumPts;

  // Find bounding rectangle of selection polygon
  if NumPts > 0 then
  begin
    Bmin.X := GroupPoly[0].X;
    Bmin.Y := GroupPoly[0].Y;
    Bmax.X := GroupPoly[0].X;
    Bmax.Y := GroupPoly[0].Y;
    for I := 1 to NumPts-1 do
    begin
      Bmin.X := Math.Min(Bmin.X, GroupPoly[I].X);
      Bmin.Y := Math.Min(Bmin.Y, GroupPoly[I].Y);
      Bmax.X := Math.Max(Bmax.X, GroupPoly[I].X);
      Bmax.Y := Math.Max(Bmax.Y, GroupPoly[I].Y);
    end;
    GroupBounds.LowerLeft := Bmin;
    GroupBounds.UpperRight := Bmax;
  end;
  InRegionLabel.Visible := (NumPts > 0);
end;

procedure TGroupEditorForm.OkBtnClick(Sender: TObject);
begin
  if JunctionsRadioBtn.Checked then ObjType := cNodes else ObjType := cLinks;

  FilterParam := -1;
  if FilterCheckBox.Checked then
  begin
    FilterParam := FilterParamCombo.ItemIndex;
    if ObjType = cNodes then
      FilterParam := JuncParamCode[FilterParam]
    else
      FilterParam := PipeParamCode[Filterparam];

    if FilterParam = EN_TAG then
      FilterTag := FilterValueEdit.Text
    else if (ObjType = cNodes) and (FilterParam = EN_PATTERN) then
    begin
      FilterValue := project.GetItemIndex(cPatterns, FilterValueEdit.Text);
      if FilterValue = 0 then
      begin
        utils.MsgDlg('Invalid Patten ID', mtError, [mbOk]);
        FilterValueEdit.SetFocus;
        exit;
      end;
    end else
    begin
      FilterRelation := FilterRelationCombo.ItemIndex;
      if not utils.Str2Float(FilterValueEdit.Text, FilterValue) then
      begin
        utils.MsgDlg('Invalid number.', mtError, [mbOk]);
        FilterValueEdit.SetFocus;
        exit;
      end;
    end;
  end;

  ChangeAction := ActionCombo.ItemIndex;
  ChangeParam := ParamCombo.ItemIndex;
  if ObjType = cNodes then
    ChangeParam := JuncParamCode[ChangeParam]
  else
    ChangeParam := PipeParamCode[ChangeParam];
  if (ObjType = cNodes) and (ChangeParam = EN_PATTERN) then
  begin
    ChangeValue := project.GetItemIndex(cPatterns, ValueEdit.Text);
    if ChangeValue = 0 then
    begin
      utils.MsgDlg('Invalid Patten ID', mtError, [mbOk]);
      ValueEdit.SetFocus;
      exit;
    end;
  end
  else if ChangeParam = EN_TAG then
  begin
    ChangeTag := ValueEdit.Text;
    if (Pos(' ', ChangeTag) > 0) or (Pos(';', ChangeTag) > 0) then
    begin
      utils.MsgDlg('Tags cannot contain spaces or semi-colons', mtError, [mbOk]);
      ValueEdit.SetFocus;
      exit;
    end;
  end
  else if not utils.Str2Float(ValueEdit.Text, ChangeValue) then
  begin
    utils.MsgDlg('Invalid number.', mtError, [mbOk]);
    ValueEdit.SetFocus;
    exit;
  end;

  if not GoChangeValues then ModalResult := mrOK;
end;

procedure TGroupEditorForm.FilterCheckBoxChange(Sender: TObject);
var
  WithEnabled: Boolean;
begin
  WithEnabled := FilterCheckBox.Checked;
  FilterParamCombo.Enabled := WithEnabled;
  FilterRelationCombo.Enabled := WithEnabled;
  FilterValueEdit.Enabled := WithEnabled;
  FilterParamComboChange(self);
end;

procedure TGroupEditorForm.FilterParamComboChange(Sender: TObject);
begin
  if SameText(FilterParamCombo.Text, 'Tag') or
     SameText(FilterParamCombo.Text, 'Demand Pattern') then
  begin
    FilterRelationCombo.ItemIndex := 1;
    FilterRelationCombo.Enabled := False;
  end else
    FilterRelationCombo.Enabled := True;
end;

procedure TGroupEditorForm.ActionComboChange(Sender: TObject);
begin
  if ActionCombo.ItemIndex = 0 then
    Label2.Caption := 'with'
  else
    Label2.Caption := 'by';
end;

procedure TGroupEditorForm.ParamComboChange(Sender: TObject);
begin
  if SameText(ParamCombo.Text, 'Tag') or
    SameText(ParamCombo.Text, 'Demand Pattern') then
  begin
    ActionCombo.ItemIndex := 0;
    ActionCombo.Enabled := False;
    Label2.Caption := 'with';
  end
  else ActionCombo.Enabled := True;
end;

procedure TGroupEditorForm.JunctionsRadioBtnChange(Sender: TObject);
begin
  if JunctionsRadioBtn.Checked then
  begin
    SetParamChoices(FilterParamCombo, JuncParamsTxt);
    SetParamChoices(ParamCombo, JuncParamsTxt);
  end
  else
  begin
    SetParamChoices(FilterParamCombo, PipeParamsTxt);
    SetParamChoices(ParamCombo, PipeParamsTxt);
    FilterRelationCombo.Enabled := True;
    ActionCombo.Enabled := True;
  end;
  FilterParamComboChange(self);
  ParamComboChange(self);
end;

procedure TGroupEditorForm.SetParamChoices(aComboBox: TComboBox; Choices: String);
begin
  with aComboBox do
  begin
    Clear;
    Items.Text := Choices;
    ItemIndex := 0;
  end;
end;

function TGroupEditorForm.GoChangeValues: Boolean;
var
  I: Integer;
  N: Integer = 0;
  Count: Integer = 0;
  Msg: String;
  Msg1: String = 'No objects match your criteria';
  Msg2: String = ' object(s) have been modified.';
  Msg3: String = 'Do you wish to make more edits?';
begin
  Result := True;
  if ObjType = cNodes then
    epanet2.ENgetcount(EN_NODECOUNT, N)
  else
    epanet2.ENgetcount(EN_LINKCOUNT, N);
  for I := 1 to N do
  begin
    if ObjValueChanged(I) then Inc(Count);
  end;

  if Count = 0 then Msg := Msg1 else
  begin
    Msg := IntToStr(Count) + Msg2;
    HasChanged := True;
  end;
  Msg := Msg + sLineBreak + sLineBreak + Msg3;
  if utils.MsgDlg(Msg, mtConfirmation, [mbYes, mbNo]) = mrNo then
    Result := False;
end;

function TGroupEditorForm.ObjValueChanged(I: Integer): Boolean;
var
  NodeType: Integer = 0;
  LinkType: Integer = 0;
  X: Single;
begin
  Result := False;
  if ObjType = cNodes then
  begin
    epanet2.ENgetnodetype(I, NodeType);
    if NodeType <> EN_JUNCTION then exit;
  end else
  begin
    epanet2.ENgetlinktype(I, LinkType);
    if LinkType <> EN_PIPE then exit;
  end;

  if (NumPolyPts > 0) and (not PassesLocationFilter(I)) then exit;

  if (FilterParam = EN_TAG) and (not PassesTextFilter(I)) then exit
  else if (FilterParam >= 0) and (not PassesNumericalFilter(I)) then exit;

  if ChangeParam = EN_TAG then
    epanet2.ENsettag(ObjType, I, PChar(ChangeTag))
  else begin
    X := GetNewValue(I);
    if ObjType = cNodes then
      epanet2.ENsetnodevalue(I, ChangeParam, X)
    else if ObjType = cLinks then
      epanet2.ENsetlinkvalue(I, ChangeParam, X);
  end;
  Result := True;
end;

function TGroupEditorForm.PassesLocationFilter(I: Integer): Boolean;
var
  N1: Integer = 0;
  N2: Integer = 0;
  Pt: TDoublePoint;
begin
  Result := False;
  if ObjType = cNodes then
  begin
    if not project.GetNodeCoord(I, Pt.X, Pt.Y) then exit;
    if not utils.PointInPolygon(Pt, GroupBounds, NumPolyPts, GroupPoly) then exit;
  end
  else if ObjType = cLinks then
  begin
    if not project.GetLinkNodes(I, N1, N2) then exit;
    if not project.GetNodeCoord(N1, Pt.X, Pt.Y) then exit;
    if not utils.PointInPolygon(Pt, GroupBounds, NumPolyPts, GroupPoly) then exit;
    if not project.GetNodeCoord(N2, Pt.X, Pt.Y) then exit;
    if not utils.PointInPolygon(Pt, GroupBounds, NumPolyPts, GroupPoly) then exit;
  end;
  Result := True;
end;

function TGroupEditorForm.PassesTextFilter(I: Integer): Boolean;
var
  R : Integer;
begin
  R := CompareStr(project.GetTag(ObjType, I), FilterTag);
  case FilterRelation of
  BELOW: Result := (R < 0);
  EQUAL: Result := (R = 0);
  ABOVE: Result := (R > 0);
  else Result := False;
  end;
end;

function TGroupEditorForm.PassesNumericalFilter(I: Integer): Boolean;
var
  X: Single;
begin
  if ObjType = cNodes then
    epanet2.ENgetnodevalue(I, FilterParam, X)
  else
    epanet2.ENgetlinkvalue(I, FilterParam, X);
  case FilterRelation of
  BELOW: Result := (X <= FilterValue);
  EQUAL: Result := (Abs(X - FilterValue) < 0.0001);
  ABOVE: Result := (X >= FilterValue);
  else Result := False;
  end;
end;

function TGroupEditorForm.GetNewValue(Index: Integer): Single;
var
  X: Single;
begin
  if ChangeAction = REPLACE then
    Result := ChangeValue
  else begin
    if ObjType = cNodes then
      epanet2.ENgetnodevalue(Index, ChangeParam, X)
    else
      epanet2.ENgetlinkvalue(Index, ChangeParam, X);
    if ChangeAction = MULTIPLY then
      Result := X * ChangeValue
    else if ChangeAction = ADD then
      Result := X + ChangeValue
    else
      Result := ChangeValue;
  end;
end;

end.

