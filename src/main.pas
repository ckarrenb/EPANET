{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       main
 Description:  main form of a graphical user interface for the
               EPANET water distribution system analysis engine
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 01/03/2025
=====================================================================}
{
 The application's main form consists of several panels as shown
 below that are populated with different Frame components:
 ______________________________________________________________
 |                        MenuPanel                           |
 |____________________________________________________________|
 |               |                             |              |
 |  HintPanel    |                             |              |
 |_______________|         MapPanel            | ProjectPanel |
 |               |                             |              |
 |               |                             |              |
 |LegendTreeView |_____________________________|              |
 |               |                             |              |
 |               |         ReportPanel         |              |
 |_______________|_____________________________|______________|
 |_________________________StatusPanel _______________________|

 MenuPanel - contains the MainMenuFrame used to select various program actions.

 ProjectPanel - contains the ProjectFrame used to navigate through an
 EPANET project's database and edit its properties.

 MapPanel - contains a MapFrame that displays a map of the EPANET pipe
 network being analyzed and handles user interaction with the map.

 LeftPanel - contains a HintPanel and a LegendTreeView.

 HintPanel - shares space with several other pop-up panels that are normally
 hidden and are used to display progam instructions or implement map operations.

 LegendTreeView - shows the symbology used to colorize the themes displayed on
 the network map.

 ReportPanel - a normally hidden panel that houses a ReportFrame used to
 display different types of simulation results.

 StatusPanel - contains a StatusBarFrame that displays key project properties.
}

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Menus, StdCtrls, Buttons, LCLIntf, LCLtype, ExtDlgs, fileutil,
  mrumanager, ImgList, Themes,
  projectframe, mapframe, menuframe, reportframe, statusframe, mapgeoref,
  maplocater, mapquery, mapexporter, tseriesselector, profileselector;

type

  // StatusBar sections
  TStatusBarIndex = (sbAutoLength = 1, sbFlowUnits, sbHeadLoss, sbDemands,
                     sbQuality, sbResults, sbXY);

  { TMainForm }

  TMainForm = class(TForm)
    EditingImageList: TImageList;
    MarkerImageList: TImageList;
    MaterialImageList: TImageList;
    OfficeImageList: TImageList;
    StatusPanel: TPanel;
    PopupImageList: TImageList;
    HintTitleLabel: TLabel;
    HintTextLabel: TLabel;
    LegendTreeView: TTreeView;
    MainPanel: TPanel;
    LegendTitlePanel: TPanel;
    MenuPanel: TPanel;
    LeftSpacerPanel: TPanel;
    WindowImageList: TImageList;
    ReportPanel: TPanel;
    MapPanel: TPanel;
    Leftpanel: TPanel;
    HintPanel: TPanel;
    ProjectPanel: TPanel;
    ReportSplitter: TSplitter;
    Splitter2: TSplitter;
    FontDialog1: TFontDialog;
    LegendImageList: TImageList;
    MRUMenuMgr: TMRUMenuManager;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SavePictureDialog1: TSavePictureDialog;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PopupImageListGetWidthForPPI(Sender: TCustomImageList; AImageWidth,
      APPI: Integer; var AResultWidth: Integer);
    procedure LegendTreeViewSelectionChanged(Sender: TObject);
    procedure MRUMenuMgrRecentFile(Sender: TObject; const AFileName: String);
    procedure StatusPanelPaint(Sender: TObject);

  private
    IsActivated: Boolean;
    AppHelpFile: String;
    procedure SetFormPosition;
    procedure SetMruFiles;
    procedure StartNewProject;
    procedure CreateFrames;
    procedure OpenFile(Filename: String);
    procedure SaveFile(Filename: String);
    procedure InitFormContents(Filename: String);
    procedure InitStatusBar;
    function  SaveFileDlg: Integer;
    procedure ShowWelcomePage;

  public
    MainMenuFrame: TMainMenuFrame;
    MapFrame : TMapFrame;
    GeoRefFrame: TGeoRefFrame;
    LocaterFrame: TMapLocaterFrame;
    QueryFrame: TMapQueryFrame;
    ExporterFrame: TMapExporterFrame;
    ProjectFrame: TProjectFrame;
    ReportFrame: TReportFrame;
    StatusBarFrame: TStatusBarFrame;
    ProfileSelectorFrame: TProfileSelectorFrame;
    TseriesSelectorFrame: TTseriesSelectorFrame;
    AppIniFile: String;
    procedure ExportMapToFile(IncludeLegend: Boolean);
    procedure FileConfigure;
    procedure FileImport(FileType: String);
    procedure FileNew(ShowSetupForm: Boolean);
    procedure FileOpen;
    procedure FileQuit;
    procedure FileSave;
    procedure FileSaveAs;
    procedure HideHintPanel;
    procedure HideLeftPanelFrames;
    procedure ProjectSetup;
    procedure ShowHelp(Topic: String);
    procedure ShowHintPanel(Title: String; Content: String);
    procedure UpdateStatusBar(Index: TStatusBarIndex; S: String);
    procedure UpdateXYStatus(const X: Double; const Y: Double);

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  project, projectloader, projectbuilder, projectsetup, welcome,
  config, inifile, utils;

{ TmainForm }

//------------------------------------------------------------------------------
//  Form Procedures
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  AppIniDir: String;
begin
  IsActivated := False;
  project.Open;
  MainPanel.Align := alClient;
  MapPanel.Align := alClient;
  CreateFrames;
  Application.HintColor := $00E0FFFF;  //clInactiveBorder;  //clWhite;
  Screen.HintFont.Color := clBlack;
  Application.ShowButtonGlyphs := sbgNever;

  AppHelpFile := 'file:///' + ExtractFilePath(Application.ExeName) + 'manual.html';
  AppIniFile := '';
  AppIniDir := GetAppConfigDir(false);
  if ForceDirectories(AppIniDir) then
  begin
    AppIniFile := AppIniDir + ApplicationName + '.ini';
    config.ReadPreferences(AppIniFile);
  end;

  Color := config.ThemeColor;
  Font.Size := config.FontSize;
  MainMenuFrame.SetIconFamily(config.IconFamily);
  MainMenuFrame.SpeedPanel1.Visible := config.ShowSpeedBar;
//MainMenuFrame.SpeedPanel2.Visible := config.ShowSpeedBar;
  SetMruFiles;
  SetFormPosition;
end;

procedure TMainForm.SetMruFiles;
var
  I: Integer;
begin
  MruMenuMgr.IniFileName := AppIniFile;
  MruMenuMgr.IniSection := 'MRU_FILES';
  for I := MruMenuMgr.Recent.Count - 1 downto 0 do
    if not FileExists(MruMenuMgr.Recent[I]) then MruMenuMgr.Recent.Delete(I);
end;

procedure TMainForm.SetFormPosition;
var
  H, W, L, R, T, B: LongInt;
  DPI: LongInt;
begin
  inifile.ReadFormPosition(AppIniFile);
  H := BoundsRect.Height;
  W := BoundsRect.Width;

  // Scale form's size to screen's dots per inch
  DPI := Screen.PixelsPerInch;
  H := (96 * H) div DPI;
  W := (96 * W) div DPI;
  L := BoundsRect.Left;
  R := L + W;
  T := BoundsRect.Top;
  B := T + H;

  if L < Screen.DesktopLeft then
  begin
    L := Screen.DesktopLeft;
    R := Screen.DesktopLeft + W;
  end;
  if R > Screen.DesktopLeft + Screen.DesktopWidth then
  begin
    L := Screen.DesktopLeft + Screen.DesktopWidth - W;
    R := Screen.DesktopLeft + Screen.DesktopWidth;
  end;
  if T < Screen.DesktopTop then
  begin
    T := Screen.DesktopTop;
    B := Screen.DesktopTop + H;
  end;
  if B > (Screen.DesktopTop + Screen.DesktopHeight) then
  begin
    T := Screen.DesktopTop + Screen.DesktopHeight - H;
    B := Screen.DesktopTop + Screen.DesktopHeight;
  end;
  W := R - L;
  H := B - T;
  SetBounds(L, T, W, H);
end;

procedure TMainForm.CreateFrames;
begin
  if not Assigned(MainmenuFrame) then
  begin
    MainMenuFrame := TMainMenuFrame.Create(self);
    MainMenuFrame.Parent := MenuPanel;
    MainMenuFrame.Align := alClient;
    MainMenuFrame.Init;
  end;
  if not Assigned(MapFrame) then
  begin
    MapFrame := TMapFrame.Create(self);
    MapFrame.Parent := MapPanel;
    MapFrame.Align := alClient;
    MapFrame.Init;
  end;
  if not Assigned(ProjectFrame) then
  begin
    ProjectFrame := TProjectFrame.Create(self);
    ProjectFrame.Parent := ProjectPanel;
    ProjectFrame.Align := alClient;
  end;
  if not Assigned(ReportFrame) then
  begin
    ReportFrame := TReportFrame.Create(self);
    ReportFrame.Parent := ReportPanel;
    ReportFrame.Align := alClient;
    ReportFrame.Init;
    ReportPanel.Visible := False;
  end;
  if not Assigned(StatusBarFrame) then
  begin
    StatusBarFrame := TStatusBarFrame.Create(self);
    StatusBarFrame.Parent := StatusPanel;
    StatusBarFrame.Align := alClient;
    StatusBarFrame.BorderSpacing.Right := 24;
  end;
  if not Assigned(GeoRefFrame) then
  begin
    GeoRefFrame := TGeoRefFrame.Create(self);
    GeoRefFrame.Parent := LeftPanel;
    GeoRefFrame.Align := alTop;
    GeoRefFrame.Visible := False;
  end;
  if not Assigned(LocaterFrame) then
  begin
    LocaterFrame := TMapLocaterFrame.Create(self);
    LocaterFrame.Parent := LeftPanel;
    LocaterFrame.Align := alTop;
    LocaterFrame.Visible := False;
  end;
  if not Assigned(QueryFrame) then
  begin
    QueryFrame := TMapQueryFrame.Create(self);
    QueryFrame.Parent := LeftPanel;
    QueryFrame.Align := alTop;
    QueryFrame.Init;
    QueryFrame.Visible := False;
  end;
  if not Assigned(ExporterFrame) then
  begin
    ExporterFrame := TMapExporterFrame.Create(self);
    ExporterFrame.Parent := LeftPanel;
    ExporterFrame.Align := alTop;
    ExporterFrame.Visible := False;
  end;
  if not Assigned(ProfileSelectorFrame) then
  begin
    ProfileSelectorFrame := TProfileSelectorFrame.Create(self);
    ProfileSelectorFrame.Parent := LeftPanel;
    ProfileSelectorFrame.Align := alTop;
    ProfileSelectorFrame.Visible := False;
  end;
  if not Assigned(TseriesSelectorFrame) then
  begin
    TseriesSelectorFrame := TTseriesSelectorFrame.Create(self);
    TseriesSelectorFrame.Parent := LeftPanel;
    TseriesSelectorFrame.Align := alTop;
    TseriesSelectorFrame.Visible := False;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin

end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // Initialize split between Project Explorer & Property Editor in ProjectFrame
  ProjectFrame.InitSplit;
  MapFrame.ResizeMap;
end;

procedure TMainForm.PopupImageListGetWidthForPPI(Sender: TCustomImageList;
  AImageWidth, APPI: Integer; var AResultWidth: Integer);
begin
  AResultWidth := AImageWidth * APPI div 96;
end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  Filename: String;
begin
  if IsActivated then exit;
  IsActivated := True;
  StartNewProject;
  if config.ShowWelcomePage then ShowWelcomePage
  else if config.OpenLastFile then
  begin
    if MRUMenuMgr.Recent.Count > 0 then
    begin
      Filename := MRUMenuMgr.Recent[0];
      if FileExists(FileName) then OpenFile(Filename);
    end;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ReportFrame.Close;
  project.Close;
  MapFrame.Close;
  if Length(AppIniFile) > 0 then
  begin
    MruMenuMgr.SaveRecentFilesToIni(AppIniFile, 'MRU_FILES');
    config.SavePreferences(AppIniFile);
    iniFile.SaveFormPosition(AppIniFile);
  end;
  CloseAction := caFree;
  Self.Show;  //For MacOS
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SetFocus;
  if SaveFileDlg = mrCancel then
    CanClose := False
  else
    CanClose := True;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ProjectFrame.PropEditor.Focused then ProjectFrame.PropEditorKeyPress(Key)
  else MapFrame.GoKeyDown(Key);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  ReportPanel.Height := MainPanel.Height div 2;
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
begin
  if SaveFileDlg = mrCancel then Exit;
  OpenFile(FileNames[0]);
end;

procedure TMainForm.MRUMenuMgrRecentFile(Sender: TObject;
  const AFileName: String);
begin
  if SaveFileDlg = mrCancel then Exit;
  if not FileExists(AFileName) then
    utils.MsgDlg('Input file no longer exists.', mtInformation, [mbOK], self)
  else
    OpenFile(AFileName);
end;

// Paint a resizing gripper symbol on the StatusPanel.
procedure TMainForm.StatusPanelPaint(Sender: TObject);
const
  GRIP_SIZE = 20;
var
  R: TRect;
  details: TThemedElementDetails;
begin
  R := Rect(StatusPanel.Width-GRIP_SIZE, StatusPanel.Height-GRIP_SIZE,
    StatusPanel.Width, StatusPanel.Height);
  details := ThemeServices.GetElementDetails(tsGripper);
  ThemeServices.DrawElement(StatusPanel.Canvas.Handle, details, R);
end;

procedure TMainForm.LegendTreeViewSelectionChanged(Sender: TObject);
begin
  MapFrame.ChangeMapLayer(LegendTreeView);
end;

procedure TMainForm.ShowHintPanel(Title: String; Content: String);
begin
  HideLeftPanelFrames;
  HintTitleLabel.Caption:= Title;
  HintTextLabel.Caption := Content;
  HintPanel.Visible := True;
end;

procedure TMainForm.HideHintPanel;
begin
  HintPanel.Visible := False;
end;

procedure TMainForm.HideLeftPanelFrames;
begin
  GeoRefFrame.Hide;
  LocaterFrame.Hide;
  QueryFrame.CloseBtnClick(self);
  ExporterFrame.Hide;
  TseriesSelectorFrame.Hide;
end;

//------------------------------------------------------------------------------
//  File Menu Procedures
//------------------------------------------------------------------------------
procedure TMainForm.FileImport(FileType: String);
begin
  if SameText(FileType, 'shp') then projectBuilder.ImportShapeFile;
  if SameText(FileType, 'dxf') then projectBuilder.ImportDxfFile;
end;

procedure TMainForm.FileNew(ShowSetupForm: Boolean);
begin
  if SaveFileDlg = mrCancel then Exit;
  StartNewProject;
  if ShowSetupForm then MainMenuFrame.ProjectSetupBtnClick(self);
end;

procedure TMainForm.FileOpen;
begin
  if SaveFileDlg = mrCancel then Exit;
  with OpenDialog1 do
  begin
    Filter := 'EPANET INP Files|*.inp|All Files|*.*';
    Filename := '*.inp';
    if Execute then OpenFile(Filename);
  end;
end;

procedure TMainForm.FileQuit;
begin
  Close;
end;

procedure TMainForm.FileSaveAs;
begin
  with SaveDialog1 do
  begin
    Filter := 'EPANET INP Files|*.inp|All Files|*.*';
    if project.InpFile.Length > 0 then
    begin
      InitialDir := ExtractFileDir(project.InpFile);
      FileName := ExtractFileName(project.InpFile);
    end
    else FileName := '*.inp';
    if Execute then SaveFile(FileName);
  end;
end;

procedure TMainForm.SaveFile(FileName: String);
begin
  if project.Save(FileName) then
  begin
    inifile.WriteProjectDefaults(ChangeFileExt(FileName, '.ini'),
      MapFrame.GetWebBasemapSource);
    project.InpFile := FileName;
    Self.Caption := 'EPANET - ' + ChangeFileExt(ExtractFileName(FileName), '');
    MruMenuMgr.AddToRecent(FileName);
  end;
end;

procedure TMainForm.FileSave;
begin
  if project.InpFile.Length > 0 then
  begin
    if project.Save(project.InpFile) then
      inifile.WriteProjectDefaults(ChangeFileExt(Project.InpFile, '.ini'),
        MapFrame.GetWebBasemapSource);
  end
  else FileSaveAs;
end;

function TMainForm.SaveFileDlg: Integer;
begin
  Result := mrNo;
  if project.HasChanged then
  begin
    Result := utils.MsgDlg('Save changes made to current project?',
      mtConfirmation, [mbYes, mbNo, mbCancel], Self);
    if Result = mrYes then FileSave;
  end
  else if Length(project.InpFile) > 0 then
    inifile.WriteProjectMapOptions(ChangeFileExt(project.InpFile, '.ini'),
        MapFrame.GetWebBasemapSource);
end;

procedure TMainForm.FileConfigure;
var
  ClearFileList: Boolean;
  OldColor: TColor;
begin
  OldColor := Color;
  ClearFileList := False;
  config.EditPreferences(ClearFileList);
  if ClearFileList then MRUMenuMgr.Recent.Clear;
  if Color <> OldColor then
  begin
    GeoRefFrame.TopPanel.Color := Color;
    LocaterFrame.TopPanel.Color := Color;
    QueryFrame.TopPanel.Color := Color;
    ExporterFrame.TopPanel.Color := Color;
    ProfileSelectorFrame.TopPanel.Color := Color;
    TseriesSelectorFrame.TopPanel.Color := Color;
  end;
//MainMenuFrame.SpeedPanel1.Visible := config.ShowSpeedBar;
//MainmenuFrame.SpeedPanel2.Visible := config.ShowSpeedBar;
end;

procedure TMainForm.ExportMapToFile(IncludeLegend: Boolean);
begin
  with SavePictureDialog1 do
  begin
    if project.InpFile.Length > 0 then
    begin
      InitialDir := ExtractFileDir(project.InpFile);
      FileName := ChangeFileExt(ExtractFileName(project.InpFile), '.png');
    end;
    if Execute then MapFrame.CopyMap(FileName, IncludeLegend);
  end;
end;

procedure TMainForm.ShowWelcomePage;
var
  StartupAction : Integer;
  StartupFile : String;
  WelcomeForm: TWelcomeForm;
begin
  WelcomeForm := TWelcomeForm.Create(Application);
  try
    WelcomeForm.ShowStartPageCB.Checked := config.ShowWelcomePage;
    WelcomeForm.ShowModal;
    WelcomeForm.Hide;
    StartupAction := WelcomeForm.SelectedAction;
    StartupFile := WelcomeForm.SelectedFile;
    config.ShowWelcomePage := WelcomeForm.ShowStartPageCB.Checked;
  finally
    WelcomeForm.Free;
  end;
  Show;
  case StartupAction of
    saShowTutorial: MainMenuFrame.HelpTutorialBtnClick(self);
    saShowUserGuide: MainMenuFrame.HelpTopicsBtnClick(self);
    saNewProject: MainMenuFrame.ProjectSetupBtnClick(self);
    saOpenProject: FileOpen;
    saLoadRecent: MRUMenuMgrRecentFile(self, StartupFile);
  end;
end;

//------------------------------------------------------------------------------
//  Status Panel Procedures
//------------------------------------------------------------------------------

procedure TMainForm.InitStatusBar;
begin
  StatusBarFrame.SetText(Ord(sbAutoLength), 'Auto-Length: Off');
  StatusBarFrame.SetText(Ord(sbFlowUnits), 'FlowUnits: ' + project.DefOptions[1]);
  StatusBarFrame.SetText(Ord(sbHeadLoss), 'Head Loss: ' + project.DefOptions[2]);
  StatusBarFrame.SetText(Ord(sbDemands), 'Demands: DDA');
  StatusBarFrame.SetText(Ord(sbQuality), 'Quality: None');
  StatusBarFrame.SetText(Ord(sbResults), 'No Results');
  StatusBarFrame.SetText(Ord(sbXY), '   X,Y:');
end;

procedure TMainForm.UpdateStatusBar(Index: TStatusBarIndex; S: String);
var
  Txt: String;
begin
  case Index of
    sbAutoLength:
      if project.AutoLength then
        Txt := 'Auto-Length: On'
      else
        Txt := 'Auto-Length: Off';
    sbFlowUnits:
      Txt := 'Flow Units: ' + S;
    sbHeadLoss:
      Txt := 'Head Loss: ' + S;
    sbDemands:
      Txt := 'Demands: ' + S;
    sbQuality:
      Txt := 'Quality: ' + S;
    sbResults:
      Txt := S;
    else exit;
  end;
  StatusBarFrame.SetText(Ord(Index), Txt);
end;

procedure TMainForm.UpdateXYStatus(const X: Double; const Y: Double);
var
  Lat, Lon: String;
  S: String;
begin
  if project.MapUnits = muDegrees then
  begin
    if Y < 0 then Lat := Format('   %.6f°',[-Y]) + ' S, '
    else          Lat := Format('   %.6f°',[Y]) + ' N, ';
    if X < 0 then Lon := Format('   %.6f°',[-X]) + ' W'
    else          Lon := Format('   %.6f°',[X]) + ' E';
    StatusBarFrame.SetText(Ord(sbXY), Lat + Lon);
  end
  else
  begin
    S := Format('   X, Y: %.6f, %.6f ', [X, Y]);
    if project.MapUnits = muFeet then S := S + 'ft'
    else if project.MapUnits = muMeters then S := S + 'm';
    StatusBarFrame.SetText(Ord(sbXY), S);
  end;
end;

procedure TMainForm.StartNewProject;
var
  TreeNode: TTreeNode;
begin
  ReportPanel.Height := 0;
  ReportFrame.CloseReport;
  HideLeftPanelFrames;
  project.Clear;
  MapFrame.InitMapOptions;
  MapPanel.Color:= MapFrame.Map.Options.BackColor;
  MapFrame.Clear;
  TreeNode := utils.FindTreeNode(LegendTreeView, 'Basemap');
  if TreeNode <> nil then TreeNode.Visible := False;
  project.Init;
  ProjectFrame.Init;
  InitStatusBar;
  inifile.ReadAppDefaults(AppIniFile);
  project.HasChanged := False;
  Caption := 'EPANET';
  MainMenuFrame.Reset;
end;

procedure TMainForm.ProjectSetup;
var
  SetupForm: TProjectSetupForm;
begin
  SetupForm := TProjectSetupForm.Create(self);
  try
    SetupForm.ShowModal;
    if SetupForm.ModalResult = mrOK then
    begin
      ProjectFrame.UpdateItem(cOptions, oHydraul);
      if SetupForm.SaveDefaults then
        inifile.WriteAppDefaults(AppIniFile);
    end;
  finally
    SetupForm.Free;
  end;
end;

procedure TMainForm.OpenFile(FileName: String);
var
  Result: Integer;
  LoaderForm: TProjectLoaderForm;
begin
  StartNewProject;
  if config.BackupFile then
    CopyFile(FileName, FileName + '.bak');

  LoaderForm := TProjectLoaderForm.Create(self);
  try
    HintTitleLabel.Caption := 'Loading project file ...';
    LoaderForm.InpFileName := FileName;
    LoaderForm.ShowModal;
    Result := LoaderForm.LoaderResult;
  finally
    LoaderForm.Free;
  end;

  if (Result = 0) or (Result = 200) then
  begin
    InitFormContents(FileName);
    if Result = 200 then
    begin
      ReportFrame.ShowReport(rtStatus);
      utils.MsgDlg(
        'There were errors found in the project file that ' +
        'may have prevented all data from being loaded. ' +
        'Please check the Status Report for details.', mtInformation, [mbOk], self);
    end;
  end else
  begin
    Caption := 'EPANET';
    ReportFrame.ShowReport(rtStatus);
    utils.MsgDlg(
      'Could not load project file.', mtError, [mbOk], self);
  end;
end;

procedure TMainForm.InitFormContents(FileName: String);
var
  WebMapSource: Integer;
begin
  Caption := 'EPANET - ' + ChangeFileExt(ExtractFileName(FileName), '');
  WebMapSource := -1;
  inifile.ReadProjectDefaults(ChangeFileExt(FileName, '.ini'), WebMapSource);
  if (WebMapSource >= 0) and
    (not utils.HasInternetConnection()) then
      WebMapSource := -1;

  MapPanel.Color := MapFrame.Map.Options.BackColor;
  ProjectFrame.Init;

  UpdateStatusBar(sbFlowUnits, project.FlowUnitsStr[project.FlowUnits]);
  UpdateStatusBar(sbHeadLoss, project.GetHlossModelStr);
  UpdateStatusBar(sbDemands, project.GetDemandModelStr);
  UpdateStatusBar(sbQuality, project.GetQualModelStr);
  UpdateStatusBar(sbResults, 'No Results');
  UpdateStatusBar(sbXY, '');

  MainMenuFrame.InitMapThemes;
  MruMenuMgr.AddToRecent(FileName);
  MapFrame.LoadBasemapFromWeb(WebMapSource, '');
  MapFrame.DrawFullExtent;
end;

procedure TMainForm.ShowHelp(Topic: String);
begin
  OpenUrl(AppHelpFile + Topic);
end;

end.

