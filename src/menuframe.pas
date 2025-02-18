{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       menuframe
 Description:  a frame containing EPANET's main menu panel
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}
{
EPANET's main menu panel consists of a MenuPanel containing the
label of each menu item and a Notebook with a page for each item.
Each page, except the File item, contains a Toolbar with ToolButtons
for each menu sub-item. There is also a panel to the right of the
Panel3 containing a ToolBar with speed buttons for commonly used
commands.
}

unit menuframe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  Menus, LCLintf, LCLtype, Dialogs, Graphics;

type

  { TMainMenuFrame }

  TMainMenuFrame = class(TFrame)
    AddJuncBtn: TToolButton;
    AddJuncItem: TMenuItem;
    AddLabelBtn: TToolButton;
    AddLabelItem: TMenuItem;
    AddPipeBtn: TToolButton;
    AddPipeItem: TMenuItem;
    AddPumpBtn: TToolButton;
    AddPumpItem: TMenuItem;
    AddResvBtn: TToolButton;
    AddResvItem: TMenuItem;
    AddTankBtn: TToolButton;
    AddTankItem: TMenuItem;
    AddValveBtn: TToolButton;
    AddValveItem: TMenuItem;
    AnimationTimer: TTimer;
    BasemapGeorefItem: TMenuItem;
    BasemapGrayscaleItem: TMenuItem;
    BasemapLightenItem: TMenuItem;
    BasemapLoadItem: TMenuItem;
    BasemapMenu: TPopupMenu;
    BasemapUnloadItem: TMenuItem;
    EditCopyBtn: TToolButton;
    EditPasteBtn: TToolButton;
    EditReverseBtn: TToolButton;
    EditSep1: TToolButton;
    EditSep2: TToolButton;
    EditToolBar: TToolBar;
    EditUndoBtn: TToolButton;
    EditVertexBtn: TToolButton;
    ExportMapToClipboard: TMenuItem;
    ExportMapToFile: TMenuItem;
    FilePanel: TPanel;
    GroupEditBtn: TToolButton;
    HelpAboutBtn: TToolButton;
    HelpErrorsBtn: TToolButton;
    HelpSep1: TToolButton;
    HelpToolBar: TToolBar;
    HelpTopicsBtn: TToolButton;
    HelpTutorialBtn: TToolButton;
    HelpUnitsBtn: TToolButton;
    MapBasemapBtn: TToolButton;
    MapCopyMapBtn: TToolButton;
    MapExtentsBtn: TToolButton;
    MapOptionsBtn: TToolButton;
    MapQueryBtn: TToolButton;
    MapSep1: TToolButton;
    MapSep3: TToolButton;
    BasemapAlignItem: TMenuItem;
    MapToolBar: TToolBar;
    MapZoomInBtn: TToolButton;
    MapZoomOutBtn: TToolButton;
    MenuPanel2: TPanel;
    MenuPanel3: TPanel;
    MenuPanel4: TPanel;
    MenuPanel5: TPanel;
    MenuPanel6: TPanel;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    Page4: TPage;
    Page5: TPage;
    Page6: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    MenuPanel1: TPanel;
    ProjectAddBtn: TToolButton;
    ProjectAnalyzeBtn: TToolButton;
    ProjectDeleteBtn: TToolButton;
    ProjectDetailsBtn: TToolButton;
    ProjectFindBtn: TToolButton;
    ProjectReportBtn: TToolButton;
    ProjectSep1: TToolButton;
    ProjectSep2: TToolButton;
    ProjectSetupBtn: TToolButton;
    ProjectSummaryBtn: TToolButton;
    ProjectToolBar: TToolBar;
    RptProfileItem: TMenuItem;
    ObjectMenu: TPopupMenu;
    ReportMenu: TPopupMenu;
    RptCalibItem: TMenuItem;
    RptEnergyItem: TMenuItem;
    RptNetLinksItem: TMenuItem;
    RptNetNodesItem: TMenuItem;
    RptRangeItem: TMenuItem;
    RptPumpItem: TMenuItem;
    RptStatusItem: TMenuItem;
    RptSysFlowItem: TMenuItem;
    RptTseriesItem: TMenuItem;
    Separator1: TMenuItem;
    Separator11: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Separator6: TMenuItem;
    Separator7: TMenuItem;
    Separator8: TMenuItem;
    SpeedBtn1: TToolButton;
    SpeedBtn2: TToolButton;
    SpeedBtn3: TToolButton;
    SpeedBtn4: TToolButton;
    SpeedBtn5: TToolButton;
    SpeedBtn6: TToolButton;
    SpeedBtn7: TToolButton;
    SpeedBtn8: TToolButton;
    SpeedBtn9: TToolButton;
    SpeedPanel1: TPanel;
    SpeedPanel2: TPanel;
    SpeedToolBar1: TToolBar;
    SpeedToolBar2: TToolBar;
    ViewAnimateBtn: TSpeedButton;
    ViewBevel1: TBevel;
    ViewBevel2: TBevel;
    ViewLinkCombo: TComboBox;
    ViewLinkLbl: TLabel;
    ViewLinkLegendBtn: TSpeedButton;
    ViewNodeCombo: TComboBox;
    ViewNodeLbl: TLabel;
    ViewNodeLegendBtn: TSpeedButton;
    ViewPanel: TPanel;
    ViewTimePanel: TPanel;
    ViewTrackBar: TTrackBar;
    procedure AddLabelItemClick(Sender: TObject);
    procedure AddLinkItemClick(Sender: TObject);
    procedure AddNodeItemClick(Sender: TObject);
    procedure AnimationTimerTimer(Sender: TObject);
    procedure BasemapGeorefItemClick(Sender: TObject);
    procedure BasemapGrayscaleItemClick(Sender: TObject);
    procedure BasemapLightenItemClick(Sender: TObject);
    procedure BasemapLoadItemClick(Sender: TObject);
    procedure BasemapLocateItemClick(Sender: TObject);
    procedure MenuMeasureItem(Sender: TObject; ACanvas: TCanvas;
      var AWidth, AHeight: Integer);
    procedure BasemapMenuPopup(Sender: TObject);
    procedure BasemapUnloadItemClick(Sender: TObject);
    procedure EditCopyBtnClick(Sender: TObject);
    procedure EditPasteBtnClick(Sender: TObject);
    procedure EditReverseBtnClick(Sender: TObject);
    procedure EditVertexBtnClick(Sender: TObject);
    procedure GroupEditBtnClick(Sender: TObject);
    procedure HelpAboutBtnClick(Sender: TObject);
    procedure HelpErrorsBtnClick(Sender: TObject);
    procedure HelpTopicsBtnClick(Sender: TObject);
    procedure HelpTutorialBtnClick(Sender: TObject);
    procedure HelpUnitsBtnClick(Sender: TObject);
    procedure MapCopyMapBtnClick(Sender: TObject);
    procedure MapExtentsBtnClick(Sender: TObject);
    procedure MapOptionsBtnClick(Sender: TObject);
    procedure MapQueryBtnClick(Sender: TObject);
    procedure MapZoomInBtnClick(Sender: TObject);
    procedure MapZoomOutBtnClick(Sender: TObject);
    procedure MenuPanel1Click(Sender: TObject);
    procedure MenuPanel1MouseEnter(Sender: TObject);
    procedure MenuPanel1MouseLeave(Sender: TObject);
    procedure MenuPanelClick(Sender: TObject);
    procedure ProjectAnalyzeBtnClick(Sender: TObject);
    procedure ProjectDeleteBtnClick(Sender: TObject);
    procedure ProjectDetailsBtnClick(Sender: TObject);
    procedure ProjectFindBtnClick(Sender: TObject);
    procedure ProjectSetupBtnClick(Sender: TObject);
    procedure ProjectSummaryBtnClick(Sender: TObject);
    procedure ReportMenuPopup(Sender: TObject);
    procedure RptCalibItemClick(Sender: TObject);
    procedure RptEnergyItemClick(Sender: TObject);
    procedure RptNetLinksItemClick(Sender: TObject);
    procedure RptNetNodesItemClick(Sender: TObject);
    procedure RptRangeItemClick(Sender: TObject);
    procedure RptProfileItemClick(Sender: TObject);
    procedure RptPumpItemClick(Sender: TObject);
    procedure RptStatusItemClick(Sender: TObject);
    procedure RptSysFlowItemClick(Sender: TObject);
    procedure RptTseriesItemClick(Sender: TObject);
    procedure SpeedBtn1Click(Sender: TObject);
    procedure ViewAnimateBtnClick(Sender: TObject);
    procedure ViewLinkComboChange(Sender: TObject);
    procedure ViewLinkLegendBtnClick(Sender: TObject);
    procedure ViewNodeComboChange(Sender: TObject);
    procedure ViewNodeLegendBtnClick(Sender: TObject);
    procedure ViewTrackBarChange(Sender: TObject);
    procedure ViewTrackBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewTrackBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    MenuPanel: TPanel;
    procedure LoadImageFile;

  public
    procedure Init;
    procedure InitMapThemes;
    procedure InitViewTimeTrackBar(const N: Integer);
    procedure Reset;
    procedure SelectProjectMenu;
    procedure ResetMapThemes;
    procedure SetIconFamily(NewIconFamily: String);
    procedure SetColorTheme;
    procedure UpdateProjectBtns;
  end;

implementation

{$R *.lfm}

uses
  main, filemenu, basemapmenu, project, projectsummary, projectviewer, config,
  mapthemes, simulator, results, reportframe, utils, about;

const
  TabCaptions: array[0..5] of String =
    ('   File   ','   Edit   ','   View   ','   Map   ', '   Project   ','   Help   ');

  ShapeLinkTitle: String  = 'Shaping a Link';
  ShapeLinkContent: String =
    'Left-click on a vertex to select it.' + sLineBreak + ' ' + sLineBreak +
    'Drag it to a new location with the Ctrl key pressed.' + sLineBreak +
    ' ' + sLineBreak +
    'Press Insert to add a new vertex.' + sLineBreak +
    'Press Delete to delete a vertex.' + sLineBreak +
    'Press Escape to quit editing.';

  GroupEditTitle: String = 'Group Editing';
  GroupEditContent: String =
    'Press Enter to select the entire' + sLineBreak +
    'network or draw a polygon' + sLineBreak +
    'that encloses the objects' + sLineBreak +
    'to be edited.' + sLineBreak + sLineBreak +
    'Left-click at each polygon vertex.' + sLineBreak +
    'Press Enter to close the polygon.' + sLineBreak +
    'Press Escape or right-click to cancel.';

  AddNodeTitle: String = 'To Add a Node:';
  AddNodeContent: String =
    'Move the pointer to the location of the new node and left-click. ' +
    sLineBreak + ' ' + sLineBreak + 'Press Escape or right-click to cancel.';

  AddLinkTitle: String = 'To Add a Link:';
  AddLinkContent: String =
    'Left-click on the start node of the link, move the pointer to the ' +
    'end node and left-click again.' + sLineBreak + ' ' + sLineBreak +
    'Left-click at intermediate points to shape the link. ' + sLineBreak +
    ' ' + sLineBreak + 'Press Escape or right-click to cancel.';

  AddLabelTitle: String = 'To Add a Label:';
  AddLabelContent: String =
    'Left-click at the label''s location.' + sLineBreak + ' ' + sLineBreak +
    'Type in the label''s text and press Enter.' + sLineBreak + ' ' + sLineBreak +
    'Press Escape to cancel.';

  ResultsCurrentTxt: String = 'Results are Current';

  NoResultsTxt: String = 'No Results';

  NoPumpsTxt: String = 'There are no pumps to report on.';

{ TMainMenuFrame }

procedure TMainMenuFrame.Init;
var
  I: Integer;
begin
  // MainMenu bar only used for MacOS
  MainForm.Menu := nil;
  Font.Size := config.FontSize;
  MenuPanel := MenuPanel5;
  MenuPanel.Color := Notebook1.Color;
  Notebook1.PageIndex := MenuPanel.Tag;
  MapThemes.InitThemes(MainForm.LegendTreeView);
  MapThemes.InitColors;
  for I := 0 to High(MapThemes.NodeThemes) do
    ViewNodeCombo.Items.Add(MapThemes.NodeThemes[I].Name);
  for I := 0 to High(MapThemes.LinkThemes) do
    ViewLinkCombo.Items.Add(MapThemes.LinkThemes[I].Name);
  ViewNodeCombo.ItemIndex := 0;
  ViewLinkCombo.ItemIndex := 0;
  ViewNodeLegendBtn.Enabled := false;
  ViewLinkLegendBtn.Enabled := false;
end;

procedure TMainMenuFrame.Reset;
var
  I: Integer;
begin
  Color := config.ThemeColor;
  BasemapGrayscaleItem.Checked := False;
  BasemapLightenItem.Checked := False;
  InitViewTimeTrackBar(0);
  MapThemes.InitThemes(MainForm.LegendTreeView);
  ViewNodeCombo.Items.Clear;
  ViewLinkCombo.Items.Clear;
  for I := 0 to MapThemes.NodeThemeCount - 1 do
    ViewNodeCombo.Items.Add(MapThemes.NodeThemes[I].Name);
  for I := 0 to MapThemes.LinkThemeCount - 1 do
    ViewLinkCombo.Items.Add(MapThemes.LinkThemes[I].Name);
  ViewNodeCombo.ItemIndex := 1;
  ViewNodeComboChange(self);
  ViewLinkCombo.ItemIndex := 1;
  ViewLinkComboChange(self);
  ViewAnimateBtn.Down := false;
  ViewAnimateBtn.Enabled := false;
  ProjectDeleteBtn.Enabled := false;
  AnimationTimer.Enabled := false;
  EditPasteBtn.Enabled := false;
  MenuPanelClick(MenuPanel5);
end;

procedure TMainMenuFrame.MenuPanel1Click(Sender: TObject);
var
  P: TPoint;
begin
  MenuPanel.Color := config.ThemeColor;
  MenuPanel := MenuPanel1;
  Notebook1.PageIndex := 0;
  P := ClientToScreen(Point(0, 0));
  FileMenuForm.Left := P.X;
  FileMenuForm.Top := P.Y;
  FileMenuForm.SetIconType(IconFamily);
  FileMenuForm.Show;
end;

procedure TMainMenuFrame.MenuPanel1MouseEnter(Sender: TObject);
var
  theMenuPanel: TPanel;
begin
  with Sender As TPanel do theMenuPanel := TPanel(Sender);
  if theMenuPanel <> MenuPanel then
    theMenuPanel.Color := clGradientActiveCaption;
end;

procedure TMainMenuFrame.MenuPanel1MouseLeave(Sender: TObject);
var
  theMenuPanel: TPanel;
begin
  with Sender As TPanel do theMenuPanel := TPanel(Sender);
  if theMenuPanel <> MenuPanel then
    theMenuPanel.Color := config.ThemeColor;
end;

procedure TMainMenuFrame.MenuPanelClick(Sender: TObject);
var
  NewMenuPanel: TPanel;
begin
  with Sender As TPanel do NewMenuPanel := TPanel(Sender);
  if NewMenuPanel <> MenuPanel then
  begin
    MenuPanel.Color := config.ThemeColor;
    MenuPanel := NewMenuPanel;
    MenuPanel.Color := Notebook1.Color;
    Notebook1.PageIndex := MenuPanel.Tag;
  end;
end;

procedure TMainMenuFrame.SelectProjectMenu;
begin
  MenuPanelClick(MenuPanel5);
end;

procedure TMainMenuFrame.MapCopyMapBtnClick(Sender: TObject);
begin
  MainForm.HideLeftPanelFrames;
  MainForm.ExporterFrame.TopPanel.Color := config.ThemeColor;
  MainForm.ExporterFrame.Show;
end;

procedure TMainMenuFrame.MapExtentsBtnClick(Sender: TObject);
begin
  MainForm.MapFrame.DrawFullExtent;
end;

procedure TMainMenuFrame.MapOptionsBtnClick(Sender: TObject);
begin
  MainForm.MapFrame.EditMapOptions;
end;

procedure TMainMenuFrame.MapQueryBtnClick(Sender: TObject);
begin
  MainForm.HideLeftpanelFrames;
  MainForm.QueryFrame.Show;
end;

procedure TMainMenuFrame.MapZoomInBtnClick(Sender: TObject);
begin
  MainForm.MapFrame.ZoomIn(0, 0)
end;

procedure TMainMenuFrame.MapZoomOutBtnClick(Sender: TObject);
begin
  MainForm.MapFrame.ZoomOut(0, 0);
end;

procedure TMainMenuFrame.ProjectAnalyzeBtnClick(Sender: TObject);
var
  SF: TSimulationForm;
begin
  ViewAnimateBtn.Down := false;
  ViewAnimateBtn.Enabled := false;
  AnimationTimer.Enabled := false;
  SF := TSimulationForm.Create(self);
  try
    SF.ShowModal;
    mapthemes.ResetThemes;
    if project.HasResults then
    begin
      project.ResultsStatus := rsUpToDate;
      MainForm.UpdateStatusBar(sbResults, ResultsCurrentTxt);
      InitViewTimeTrackBar(results.Nperiods);
      ViewAnimateBtn.Enabled := (results.Nperiods > 1);
      mapthemes.ChangeTimePeriod(0);
      MainForm.ReportFrame.RefreshReport;
    end
    else begin
      InitViewTimeTrackBar(0);
      project.ResultsStatus := rsNotAvailable;
      MainForm.UpdateStatusBar(sbResults, NoResultsTxt);
    end;
    MainForm.ProjectFrame.UpdateResultsDisplay;
    if project.RunStatus in [rsFailed, rsError, rsWarning] then
      with MainForm.ReportFrame do ShowReport(rtStatus);
  finally
    SF.Free;
  end;
end;

procedure TMainMenuFrame.ProjectDeleteBtnClick(Sender: TObject);
begin
  MainForm.ProjectFrame.DeleteItem;
  MainForm.MapFrame.RedrawMap;
end;

procedure TMainMenuFrame.ProjectDetailsBtnClick(Sender: TObject);
var
  TmpHasChanged: Boolean;
  ProjectViewer: TProjectViewerForm;
begin
  TmpHasChanged := Project.HasChanged;
  ProjectViewer := TProjectViewerForm.Create(self);
  try
    ProjectViewer.ShowModal;
  finally
    ProjectViewer.Free;
  end;
  Project.HasChanged := TmpHasChanged;
end;

procedure TMainMenuFrame.ProjectFindBtnClick(Sender: TObject);
begin
  MainForm.HideLeftPanelFrames;
  MainForm.LocaterFrame.Show;
end;

procedure TMainMenuFrame.ProjectSetupBtnClick(Sender: TObject);
begin
  MainForm.ProjectSetup;
end;

procedure TMainMenuFrame.ProjectSummaryBtnClick(Sender: TObject);
var
  ProjectSummarizer: TSummaryForm;
begin
  ProjectSummarizer := TSummaryForm.Create(self);
  try
    ProjectSummarizer.ShowModal;
  finally
    ProjectSummarizer.Free;
  end;
end;

procedure TMainMenuFrame.ReportMenuPopup(Sender: TObject);
var
  CanEnable: Boolean;
begin
  CanEnable := project.HasResults;
  RptPumpItem.Enabled := CanEnable;
  RptRangeItem.Enabled := CanEnable;
  RptEnergyItem.Enabled := CanEnable;
  RptTseriesItem.Enabled := CanEnable;
  RptProfileItem.Enabled := CanEnable;
  RptSysFlowItem.Enabled := CanEnable;
  RptNetNodesItem.Enabled := CanEnable;
  RptNetLinksItem.Enabled := CanEnable;
  RptCalibItem.Enabled := CanEnable;
end;

procedure TMainMenuFrame.RptCalibItemClick(Sender: TObject);
begin
  MainForm.ReportFrame.ShowReport(rtCalib);
end;

procedure TMainMenuFrame.RptEnergyItemClick(Sender: TObject);
begin
  MainForm.ReportFrame.ShowReport(rtEnergy);
end;

procedure TMainMenuFrame.RptNetLinksItemClick(Sender: TObject);
begin
  MainForm.ReportFrame.ShowReport(rtLinks);
end;

procedure TMainMenuFrame.RptNetNodesItemClick(Sender: TObject);
begin
  MainForm.ReportFrame.ShowReport(rtNodes);
end;

procedure TMainMenuFrame.RptRangeItemClick(Sender: TObject);
begin
  MainForm.ReportFrame.ShowReport(rtRange);
end;

procedure TMainMenuFrame.RptProfileItemClick(Sender: TObject);
begin
  MainForm.ReportFrame.ShowReport(rtProfile);
end;

procedure TMainMenuFrame.RptPumpItemClick(Sender: TObject);
begin
 if Project.GetPumpCount = 0 then
    utils.MsgDlg(NoPumpsTxt, mtInformation, [mbOk], MainForm)
  else
    MainForm.ReportFrame.ShowReport(rtPumping);
end;

procedure TMainMenuFrame.RptStatusItemClick(Sender: TObject);
begin
  MainForm.ReportFrame.ShowReport(rtStatus);
end;

procedure TMainMenuFrame.RptSysFlowItemClick(Sender: TObject);
begin
 MainForm.ReportFrame.ShowReport(rtSysFlow);
end;

procedure TMainMenuFrame.RptTseriesItemClick(Sender: TObject);
begin
  MainForm.ReportFrame.ShowReport(rtTimeSeries);
end;

procedure TMainMenuFrame.SpeedBtn1Click(Sender: TObject);
// This event handler is shared by all buttons on SpeedToolbar1
begin
  with Sender as TToolButton do
  case Tag of
  1: MainForm.FileNew(True);
  2: MainForm.FileOpen;
  3: MainForm.FileSave;
  4: MapZoomInBtnClick(Sender);
  5: MapZoomOutBtnClick(Sender);
  6: MapExtentsBtnClick(Sender);
  7: ProjectAnalyzeBtnClick(Sender);
  // SpeedBtn8 has a dropdown menu attached to it
  9: HelpTopicsBtnClick(Sender);
  end;
end;

procedure TMainMenuFrame.ViewAnimateBtnClick(Sender: TObject);
begin
  AnimationTimer.Enabled := ViewAnimateBtn.Down
end;

procedure TMainMenuFrame.ViewLinkComboChange(Sender: TObject);
begin
  MapThemes.ChangeTheme(MainForm.LegendTreeView, cLinks, ViewLinkCombo.ItemIndex);
  MainForm.MapFrame.RedrawMap;
  ViewLinkLegendBtn.Enabled := (ViewLinkCombo.ItemIndex > 0);
end;

procedure TMainMenuFrame.ViewLinkLegendBtnClick(Sender: TObject);
begin
  if MapThemes.EditLinkLegend then
  begin
    MainForm.LegendTreeView.Refresh;
    MainForm.MapFrame.RedrawMap;
  end;
end;

procedure TMainMenuFrame.ViewNodeComboChange(Sender: TObject);
begin
  MapThemes.ChangeTheme(MainForm.LegendTreeView, cNodes, ViewNodeCombo.ItemIndex);
  MainForm.MapFrame.RedrawMap;
  ViewNodeLegendBtn.Enabled := (ViewNodeCombo.ItemIndex > 0);
end;

procedure TMainMenuFrame.ViewNodeLegendBtnClick(Sender: TObject);
begin
  if MapThemes.EditNodeLegend then
  begin
    MainForm.LegendTreeView.Refresh;
    MainForm.MapFrame.RedrawMap;
  end;
end;

procedure TMainMenuFrame.ViewTrackBarChange(Sender: TObject);
begin
  if Project.HasResults then
  begin
     ViewTimePanel.Caption := 'Time: ' +
       results.GetTimeStr(ViewTrackBar.Position) + ' hrs';
     if ViewTrackBar.Tag = 1 then
     begin
       MapThemes.ChangeTimePeriod(ViewTrackBar.Position);
       MainForm.ProjectFrame.UpdateResultsDisplay;
       MainForm.ReportFrame.ChangeTimePeriod;
     end;
  end;
end;

procedure TMainMenuFrame.ViewTrackBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ViewTrackBar.Tag := 0;
end;

procedure TMainMenuFrame.ViewTrackBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ViewTrackBar.Tag := 1;
  ViewTrackBarChange(Sender);
end;

procedure TMainMenuFrame.InitViewTimeTrackBar(const N: Integer);
// N = number of time periods in simulation
var
  I: Integer;
begin
  mapthemes.TimePeriod := 0;
  ViewTrackBar.Position:=0;
  ViewTrackBar.Visible := False;

  ViewTimePanel.Caption := '';
  ViewAnimateBtn.Visible := false;
  if N > 0 then
  begin
    I := project.GetStatisticsType;
    if I = 0 then
    begin
      if N > 1 then
      begin
        ViewTrackBar.Visible := True;
        ViewTrackBar.Max := N-1;
        ViewAnimateBtn.Visible := (N>1);
        ViewTimePanel.Caption := 'Time: ' +
          results.GetTimeStr(0) + ' hrs';
      end;
    end
    else
      ViewTimePanel.Caption := 'Themes are ' + Project.StatisticStr[I];
  end;
end;

procedure TMainMenuFrame.EditCopyBtnClick(Sender: TObject);
begin
  MainForm.ProjectFrame.CopyItem;
  EditPasteBtn.Enabled := True;
end;

procedure TMainMenuFrame.AnimationTimerTimer(Sender: TObject);
begin
  with ViewTrackBar do
    if Position = Max then Position := 0 else Position := Position + 1;
  MapThemes.ChangeTimePeriod(ViewTrackBar.Position);
end;

procedure TMainMenuFrame.AddNodeItemClick(Sender: TObject);
var
  NodeType: Integer;
begin
  NodeType := -1;
  if Sender is TMenuItem then
    NodeType := TMenuItem(Sender).Tag
  else if Sender is TToolButton then
    NodeType := TToolButton(Sender).Tag;
  if NodeType >= 0 then
  begin
    if config.ShowNotifiers then
      MainForm.ShowHintPanel(AddNodeTitle, AddNodeContent);
    MainForm.MapFrame.AddNode(NodeType);
  end;
end;

procedure TMainMenuFrame.AddLinkItemClick(Sender: TObject);
var
  LinkType: Integer;
begin
  LinkType := -1;
  if Sender is TMenuItem then
    LinkType := TMenuItem(Sender).Tag
  else if Sender is TToolButton then
    LinkType := TToolButton(Sender).Tag;
  if LinkType >= 0 then
  begin
    if config.ShowNotifiers then
      MainForm.ShowHintPanel(AddLinkTitle, AddLinkContent);
    MainForm.MapFrame.AddLink(LinkType);
  end;
end;

procedure TMainMenuFrame.AddLabelItemClick(Sender: TObject);
begin
  if config.ShowNotifiers then
    MainForm.ShowHintPanel(AddLabelTitle, AddLabelContent);
  MainForm.MapFrame.Addlabel;
end;

procedure TMainMenuFrame.BasemapGeorefItemClick(Sender: TObject);
begin
  MainForm.HideLeftpanelFrames;
  MainForm.GeoRefFrame.Show;
end;

procedure TMainMenuFrame.BasemapGrayscaleItemClick(Sender: TObject);
begin
  MainForm.MapFrame.Map.Basemap.Grayscale := BasemapGrayscaleItem.Checked;
  MainForm.MapFrame.RedrawMap;
end;

procedure TMainMenuFrame.BasemapLightenItemClick(Sender: TObject);
begin
  with BasemapLightenItem do
  begin
   if Checked then MainForm.MapFrame.SetBasemapBrightness(50)
   else MainForm.MapFrame.SetBasemapBrightness(0);
  end;
end;

procedure TMainMenuFrame.BasemapLoadItemClick(Sender: TObject);
var
  BMF: TBasemapMenuForm;
begin
  BMF := TBasemapMenuForm.Create(MainForm);
  try
    BMF.ShowModal;
    BMF.Hide;
    if BMF.MapSelection = 0 then LoadImageFile
    else if BMF.MapSelection > 0 then
      MainForm.MapFrame.LoadBasemapFromWeb(BMF.MapSelection, BMF.GetEpsg);
  finally
    BMF.Free;
  end;
end;

procedure TMainMenuFrame.BasemapLocateItemClick(Sender: TObject);
begin
  MainForm.MapFrame.FindBasemapLocation;
end;

procedure TMainMenuFrame.MenuMeasureItem(Sender: TObject;
  ACanvas: TCanvas; var AWidth, AHeight: Integer);
begin
  with Sender as TMenuItem do
  begin
    // Distinguish between an item with text and a separator line
    if Tag = -1 then aHeight := 8 else aHeight := 28;
  end;
end;

procedure TMainMenuFrame.BasemapMenuPopup(Sender: TObject);
begin
  with MainForm.MapFrame do
  begin
    BasemapUnloadItem.Enabled := HasBaseMap;
    BasemapGeorefItem.Enabled := (Length(BaseMapFile) > 0);
    BasemapAlignItem.Enabled := BasemapGeorefItem.Enabled;
    BasemapLightenItem.Enabled := HasBaseMap;
    BasemapGrayscaleItem.Enabled := HasBaseMap;
  end;
end;

procedure TMainMenuFrame.BasemapUnloadItemClick(Sender: TObject);
begin
  MainForm.MapFrame.UnloadBasemap;
  BasemapLightenItem.Checked := False;
  BasemapGrayscaleItem.Checked := False;
end;

procedure TMainMenuFrame.EditPasteBtnClick(Sender: TObject);
begin
  MainForm.ProjectFrame.PasteItem;
end;

procedure TMainMenuFrame.EditReverseBtnClick(Sender: TObject);
var
  Item: Integer;
  Category: Integer;
begin
  Category := MainForm.ProjectFrame.CurrentCategory;
  Item := MainForm.ProjectFrame.SelectedItem[Category];
  Project.ReverseLinkNodes(Item+1);
  MainForm.ProjectFrame.UpdateItem(Category, Item);
  MainForm.MapFrame.RedrawMap;
end;

procedure TMainMenuFrame.EditVertexBtnClick(Sender: TObject);
begin
  EditVertexBtn.Down := True;
  if config.ShowNotifiers then
    MainForm.ShowHintPanel(ShapeLinkTitle, ShapeLinkContent);
  MainForm.MapFrame.EnterVertexingMode;
end;

procedure TMainMenuFrame.GroupEditBtnClick(Sender: TObject);
begin
  GroupEditBtn.Down := True;
  if config.ShowNotifiers then
    MainForm.ShowHintPanel(GroupEditTitle, GroupEditContent);
  MainForm.MapFrame.EnterFenceLiningMode;
end;

procedure TMainMenuFrame.HelpAboutBtnClick(Sender: TObject);
var
  AboutForm: TAboutForm;
begin
  AboutForm := TAboutForm.Create(self);
  try
    AboutForm.ShowModal;
  finally
    AboutForm.Free;
  end;
end;

procedure TMainMenuFrame.HelpErrorsBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('#error_codes');
end;

procedure TMainMenuFrame.HelpTopicsBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('');
end;

procedure TMainMenuFrame.HelpTutorialBtnClick(Sender: TObject);
var
  TutorialFile: String;
begin
  TutorialFile := 'file:///' + ExtractFilePath(Application.ExeName) + 'tutorial.html';
  OpenUrl(TutorialFile);
end;

procedure TMainMenuFrame.HelpUnitsBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('#measurement_units');
end;

procedure TMainMenuFrame.LoadImageFile;
begin
  MainForm.MapFrame.LoadBasemapFromFile;
{  if MainForm.MapFrame.HasBaseMap then
  begin
    MainForm.QueryFrame.Hide;
    MainForm.LocaterFrame.Hide;
    MainForm.ExporterFrame.Hide;
    MainForm.GeoRefFrame.Show;
  end; }
end;

procedure TMainMenuFrame.SetIconFamily(NewIconFamily: String);
var
  IconImageList: TImageList;
begin
  if SameText(IconFamily, 'Material') then
    IconImageList := MainForm.MaterialImageList
  else if SameText(IconFamily, 'Office' ) then
    IconImageList := MainForm.OfficeImageList
  else exit;
  IconFamily := NewIconFamily;
  EditToolbar.Images := IconImageList;
  ViewNodeLegendBtn.Images := IconImageList;
  ViewLinkLegendBtn.Images := IconImageList;
  ViewAnimateBtn.Images := IconImageList;
  MapToolbar.Images := IconImageList;
  ProjectToolbar.Images := IconImageList;
  HelpToolbar.Images := IconImageList;
  SpeedToolbar1.Images := IconImageList;
end;

procedure TMainMenuFrame.SetColorTheme;
var
  I: Integer;
begin
  Color := config.ThemeColor;
  for I := 1 to 6 do
    with FindComponent('MenuPanel' + IntToStr(I)) as TPanel do
      Color := config.ThemeColor;
  MenuPanel.Color := Notebook1.Color;
end;

procedure TMainMenuFrame.ResetMapThemes;
var
  I, J: Integer;
begin
  J := ViewNodeCombo.ItemIndex;
  if J >= NodeThemeCount then J := 0;
  ViewNodeCombo.Items.Clear;
  for I := 0 to NodeThemeCount - 1 do
    ViewNodeCombo.Items.Add(mapthemes.NodeThemes[I].Name);
  ViewNodeCombo.ItemIndex := J;

  J := ViewLinkCombo.ItemIndex;
  if J >= LinkThemeCount then J := 0;
  ViewLinkCombo.Items.Clear;
  for I := 0 to LinkThemeCount - 1 do
    ViewLinkCombo.Items.Add(mapthemes.LinkThemes[I].Name);
  ViewLinkCombo.ItemIndex := J;

  mapthemes.ChangeTheme(MainForm.LegendTreeView, cNodes, ViewNodeCombo.ItemIndex);
  ViewNodeLegendBtn.Enabled := (ViewNodeCombo.ItemIndex > 0);
  mapthemes.ChangeTheme(MainForm.LegendTreeView, cLinks, ViewLinkCombo.ItemIndex);
  ViewLinkLegendBtn.Enabled := (ViewLinkCombo.ItemIndex > 0);
end;

procedure TMainMenuFrame.InitMapThemes;
begin
  ViewNodeCombo.ItemIndex := ntElevation;
  ViewNodeLegendBtn.Enabled := true;
  ViewLinkCombo.ItemIndex := ltDiameter;
  ViewLinkLegendBtn.Enabled := true;
  mapthemes.SetInitialTheme(cNodes, ntElevation);
  mapthemes.SetInitialTheme(cLinks, ltDiameter);
  SelectProjectMenu;
end;

procedure TMainMenuFrame.UpdateProjectBtns;
var
  BtnEnabled: Boolean;
begin
  BtnEnabled := MainForm.ProjectFrame.CurrentCategory in [cNodes, cLinks];
  EditCopyBtn.Enabled := BtnEnabled;
  EditPasteBtn.Enabled := BtnEnabled and
    (MainForm.ProjectFrame.CurrentCategory = MainForm.ProjectFrame.CopiedCategory);

  BtnEnabled := (MainForm.ProjectFrame.CurrentCategory = cLinks);
  EditVertexBtn.Enabled := BtnEnabled;
  EditReverseBtn.Enabled := BtnEnabled;
end;
end.

