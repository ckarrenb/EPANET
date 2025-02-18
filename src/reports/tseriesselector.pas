{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       tseriesselector
 Description:  A frame used to select network objects and parameters
               to display in a time series report.
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit tseriesselector;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ExtCtrls, Dialogs,
  timeseriesrpt;

type

  { TTseriesSelectorFrame }

  TTseriesSelectorFrame = class(TFrame)
    CancelBtn2: TButton;
    AddBtn: TBitBtn;
    TimeOfDayBox: TCheckBox;
    EditBtn: TBitBtn;
    DeleteBtn: TBitBtn;
    UpBtn: TBitBtn;
    DnBtn: TBitBtn;
    AcceptBtn: TButton;
    CloseBtn: TSpeedButton;
    ObjectTypeCombo: TComboBox;
    ParamCombo: TComboBox;
    ObjectNameEdit: TEdit;
    LegendLabelEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    AxisLeftBtn: TRadioButton;
    AxisRightBtn: TRadioButton;
    SeriesSelectPage: TPage;
    ViewBtn: TButton;
    CancelBtn1: TButton;
    Label1: TLabel;
    SeriesListBox: TListBox;
    Notebook1: TNotebook;
    SeriesListPage: TPage;
    TopPanel: TPanel;
    procedure AcceptBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure CancelBtn1Click(Sender: TObject);
    procedure CancelBtn2Click(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure ObjectTypeComboChange(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure DnBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure ViewBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
  private
    SeriesAction: Integer;
    TimeOfDayPlot: Boolean;
    HasChanged: Boolean;
    procedure SetActionButtons;
    procedure InitDataSeriesProps;
    procedure SetDataSeriesProps(I: Integer);
    function  GetDataSeriesProps(I: Integer): Boolean;
    function  GetDataSeriesStr(aSeries: TDataSeries): String;

  public
    procedure Init(DataSeries: array of TDataSeries; PlotTimeOfDay: Boolean);
    function  GetSelectedObjectDataSeries: TDataSeries;
    procedure SetSelectedObjectProps;

  end;

implementation

{$R *.lfm}

uses
  main, config, project, mapthemes, utils;

const
  Adding = 1;
  Editing = 2;

var
  TempDataSeries: array[0..timeseriesrpt.MaxSeries-1] of TDataSeries;

{ TTseriesSelectorFrame }


procedure TTseriesSelectorFrame.Init(DataSeries: array of TDataSeries;
  PlotTimeOfDay: Boolean);
var
  I: Integer;
  S: String;
begin
  // Clear the SeriesListBox
  TopPanel.Color := config.ThemeColor;
  HasChanged := False;
  SeriesListBox.Clear;
  SeriesListBox.ItemIndex := -1;

  // Initialize SeriesSelectPage
  InitDataSeriesProps;

  // Make local copy of the Time Series Report's data series
  for I := 0 to High(DataSeries) do
    TempDataSeries[I] := DataSeries[I];

  // Add results for currently selected object if TempDataSeries empty
  if TempDataSeries[0].ObjType < 0 then
    TempDataSeries[0] := GetSelectedObjectDataSeries;
  if TempDataSeries[0].ObjType >= 0 then HasChanged := True;

  // Add each data series short description to SeriesListBox
  for I := 0 to High(TempDataSeries) do
  begin
    if TempDataSeries[I].ObjType < 0 then break;
    S := GetDataSeriesStr(TempDataSeries[I]);
    SeriesListBox.Items.Add(S);
  end;
  if SeriesListBox.Count > 0 then SeriesListBox.ItemIndex := 0;

  // Set the status of the SeriesListBox action buttons
  SetActionButtons;

  // Set state of TimeOfDayBox
  TimeOfDayPlot := PlotTimeOfDay;
  TimeOfDayBox.Checked := PlotTimeOfDay;
end;

procedure TTseriesSelectorFrame.InitDataSeriesProps;
begin
  ObjectTypeCombo.ItemIndex := 0;
  ObjectNameEdit.Text := '';
  LegendLabelEdit.Text := '';
  ObjectTypeComboChange(Self);
  AxisLeftBtn.Checked := True;
end;

procedure TTseriesSelectorFrame.ObjectTypeComboChange(Sender: TObject);
var
  I: Integer;
begin
  ParamCombo.Clear;
  case ObjectTypeCombo.Itemindex of
  0: begin
       for I := FirstNodeResultTheme to NodeThemeCount - 1 do
         ParamCombo.Items.Add(MapThemes.NodeThemes[I].Name);
       ParamCombo.ItemIndex := ntPressure - FirstNodeResultTheme;
     end;
  1: begin
       for I := FirstLinkResultTheme to LinkThemeCount - 1 do
         ParamCombo.Items.Add(MapThemes.LinkThemes[I].Name);
       ParamCombo.ItemIndex := ltFlow - FirstLinkResultTheme;
     end;
  end;
end;

procedure TTseriesSelectorFrame.AddBtnClick(Sender: TObject);
begin
  SeriesAction := Adding;
  SetDataSeriesProps(-1);
  Notebook1.PageIndex := 1;
end;

procedure TTseriesSelectorFrame.CancelBtn1Click(Sender: TObject);
begin
  HasChanged := False;
  Visible := False;
  with MainForm.ReportFrame.Report as TTimeSeriesFrame do
    SetDataSeries(TempDataSeries, TimeOfDayPlot, HasChanged);
end;

procedure TTseriesSelectorFrame.CancelBtn2Click(Sender: TObject);
begin
  Notebook1.PageIndex := 0;
end;

procedure TTseriesSelectorFrame.CloseBtnClick(Sender: TObject);
begin
  CancelBtn1Click(Sender);
end;

procedure TTseriesSelectorFrame.DeleteBtnClick(Sender: TObject);
var
  I, J: Integer;
begin
  I := SeriesListBox.ItemIndex;
  if I >= 0 then
  begin
    SeriesListBox.DeleteSelected;
    for J := I+1 to MaxSeries-1 do
      TempDataSeries[J-1] := TempDataSeries[J];
    TempDataSeries[MaxSeries-1].ObjType:= -1;
  end;
  SetActionButtons;
end;

procedure TTseriesSelectorFrame.DnBtnClick(Sender: TObject);
var
  I, Max: Integer;
  TmpSeries: TDataSeries;
begin
  Max := SeriesListBox.Items.Count;
  if Max > 0 then begin
    Dec(Max);
    I := SeriesListBox.ItemIndex;
    if I < Max then begin
      SeriesListBox.Items.Exchange(I, I + 1);
      SeriesListBox.Selected[I+1]:= True;
      TmpSeries := TempDataSeries[I];
      TempDataSeries[I] := TempDataSeries[I+1];
      TempDataSeries[I+1] := TmpSeries;
      HasChanged := True;
    end;
  end;
end;

procedure TTseriesSelectorFrame.AcceptBtnClick(Sender: TObject);
begin
  if GetDataSeriesProps(SeriesListBox.ItemIndex) then
  begin
    HasChanged := True;
    Notebook1.PageIndex := 0;
    SetActionButtons;
  end;
end;

procedure TTseriesSelectorFrame.EditBtnClick(Sender: TObject);
begin
  SeriesAction := Editing;
  SetDataSeriesProps(SeriesListBox.ItemIndex);
  Notebook1.PageIndex := 1;
end;

procedure TTseriesSelectorFrame.ViewBtnClick(Sender: TObject);
begin
  Visible := False;
  if TimeOfDayBox.Checked <> TimeOfDayPlot then HasChanged := True;
  with MainForm.ReportFrame.Report as TTimeSeriesFrame do
    SetDataSeries(TempDataSeries, TimeOfDayBox.Checked, HasChanged);
end;

procedure TTseriesSelectorFrame.UpBtnClick(Sender: TObject);
var
  I: Integer;
  TmpSeries: TDataSeries;
begin
  I := SeriesListBox.ItemIndex;
  if I > 0 then begin
    SeriesListBox.Items.Exchange(I, I - 1);
    SeriesListBox.Selected[I-1]:= True;
    TmpSeries := TempDataSeries[I];
    TempDataSeries[I] := TempDataSeries[I-1];
    TempDataSeries[I-1] := TmpSeries;
    HasChanged := True;
  end;
end;

function TTseriesSelectorFrame.GetSelectedObjectDataSeries: TDataSeries;
//
// Gets data series properties for the project's currently selected node or link.
//
var
  Index: Integer;
  Param: Integer;
begin
  // Default result is an empty data series
  Result.ObjType := -1;
  with MainForm.ProjectFrame do
  begin

    // The project's currently selected object is a node
    if CurrentCategory = cNodes then
    begin
      Result.ObjType := cNodes;
      Index := SelectedItem[cNodes] + 1; //Indexes are 1-based
      Result.ObjIndex := Index;
      Param := mapthemes.NodeTheme;
      if Param < FirstNodeResultTheme then Param := ntPressure;
      Result.ObjParam := Param;
    end

    // The project's currently selected object is a link
    else if CurrentCategory = cLinks then
    begin
      Result.ObjType := cLinks;
      Index := SelectedItem[cLinks] + 1; //Indexes are 1-based
      Result.ObjIndex := Index;
      Param := mapthemes.LinkTheme;
      if Param < FirstLinkResultTheme then Param := ltFlow;
      Result.ObjParam := Param;
    end

    // Currently selected object is neither a node nor a link
    else exit;
  end;

  // Default plot y-axis & legend title
  Result.PlotAxis := 0;
  Result.Title:= '';   //GetDataSeriesStr(Result);
end;

function TTseriesSelectorFrame.GetDataSeriesProps(I: Integer): Boolean;
//
//  Transfer the entries on the SeriesSelectPage into a Data Series object.
//
var
  ObjID, ItemString: String;
  aSeries: TDataSeries;
begin
  Result := True;
  with aSeries do
  begin
    ObjParam := ParamCombo.ItemIndex;
    if ObjectTypeCombo.ItemIndex = 0 then
    begin
      ObjType := cNodes;
      ObjParam := MapThemes.FirstNodeResultTheme + ObjParam;
    end else
    begin
      ObjType := cLinks;
      ObjParam := MapThemes.FirstLinkResultTheme + ObjParam;
    end;
    ObjID := ObjectNameEdit.Text;
    ObjIndex := Project.GetItemIndex(ObjType, ObjID);
    if ObjIndex = 0 then
    begin
      utils.MsgDlg('There is no such object in the project.', mtError, [mbOK]);
      Result := False;
      exit;
    end;
  end;

  ItemString := GetDataSeriesStr(aSeries);
  if Length(LegendLabelEdit.Text) = 0 then
    aSeries.Title := ItemString
  else
    aSeries.Title:= LegendLabelEdit.Text;
  if AxisLeftBtn.Checked then
    aSeries.PlotAxis:= 0
  else
    aSeries.PlotAxis := 1;

  if SeriesAction = Editing then
  begin
    SeriesListBox.Items[I] := ItemString;
    SeriesListBox.ItemIndex := I;
  end else
  begin
    SeriesListBox.Items.Add(ItemString);
    SeriesListBox.ItemIndex := SeriesListBox.Count - 1;
    I := SeriesListBox.ItemIndex;
  end;
  TempDataSeries[I] := aSeries;
end;

procedure TTseriesSelectorFrame.SetDataSeriesProps(I: Integer);
var
  ObjType, ObjIndex, ObjParam: Integer;
begin
  if I < 0 then
  begin
    ObjectNameEdit.Text := '';
    LegendLabelEdit.Text := '';
    exit;
  end;

  ObjType := TempDataSeries[I].ObjType;
  if ObjType = cNodes then ObjectTypeCombo.ItemIndex := 0;
  if ObjType = cLinks then ObjectTypeCombo.ItemIndex := 1;

  ObjIndex := TempDataSeries[I].ObjIndex;
  ObjectNameEdit.Text := Project.GetID(ObjType, ObjIndex);
  LegendLabelEdit.Text := TempDataSeries[I].Title;

  ObjectTypeComboChange(Self);
  ObjParam := TempDataSeries[I].ObjParam;
  if ObjType = cNodes then
    ParamCombo.ItemIndex := ObjParam - MapThemes.FirstNodeResultTheme;
  if ObjType = cLinks then
    ParamCombo.ItemIndex := ObjParam - MapThemes.FirstLinkResultTheme;

  if TempDataSeries[I].PlotAxis = 0 then
    AxisLeftBtn.Checked := True
  else
    AxisRightBtn.Checked := True;
end;

procedure TTseriesSelectorFrame.SetSelectedObjectProps;
var
  ObjType, Index: Integer;
begin
  if Notebook1.PageIndex <> 1 then exit;
  with MainForm.ProjectFrame do
  begin
    if CurrentCategory = cNodes then
    begin
      ObjType := cNodes;
      Index := SelectedItem[cNodes] + 1; //Indexes are 1-based
      ObjectNameEdit.Text := project.GetID(cNodes, Index);
      if ObjectTypeCombo.ItemIndex <> 0 then
      begin
        ObjectTypeCombo.ItemIndex := 0;
        ObjectTypeComboChange(self);
      end;
    end
    else if CurrentCategory = cLinks then
    begin
      ObjType := cLinks;
      Index := SelectedItem[cLinks] + 1; //Indexes are 1-based
      ObjectNameEdit.Text := project.GetID(cLinks, Index);
      if ObjectTypeCombo.ItemIndex <> 1 then
      begin
        ObjectTypeCombo.ItemIndex := 1;
        ObjectTypeComboChange(self);
      end;
    end
    else exit;
  end;
end;

function TTseriesSelectorFrame.GetDataSeriesStr(aSeries:TDataSeries): String;
//
//  Builds a descriptive string of a given data series
//  (e.g., Junction J29 Pressure)
//
begin
  Result := '';
  with aSeries do
  begin
    // Check for valid object type
    if ObjType < 0 then exit;

    // Use functions from the TimeSeriesFrame report to build data series string
    with MainForm.ReportFrame.Report as TTimeSeriesFrame do
    begin
      Result := GetObjStr(ObjType, ObjIndex-1);
      Result := Result + ' ' + GetParamStr(ObjType, ObjParam);
    end;
  end;
end;

procedure TTseriesSelectorFrame.SetActionButtons;
var
  N: Integer;
begin
  N := SeriesListBox.Count;
  AddBtn.Enabled := N < 6;
  EditBtn.Enabled := N > 0;
  DeleteBtn.Enabled := N > 0;
  UpBtn.Enabled := N > 1;
  DnBtn.Enabled := (N > 1) and (N < 6);
end;

end.

