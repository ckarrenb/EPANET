{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       welcome
 Description:  a form presenting a welcome screen to EPANET users
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit welcome;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, LCLtype, LazFileUtils;

type

  { TWelcomeForm }

  TWelcomeForm = class(TForm)
    DevelopBtn1: TSpeedButton;
    DevelopBtn2: TSpeedButton;
    GetStartedBtn1: TSpeedButton;
    GetStartedBtn2: TSpeedButton;
    Image1: TImage;
    ImageList1: TImageList;
    ImageList2: TImageList;
    NoRecentProjectsLbl: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel2: TPanel;
    RecentFileBtn0: TSpeedButton;
    RecentFileBtn1: TSpeedButton;
    RecentFileBtn2: TSpeedButton;
    RecentFileBtn3: TSpeedButton;
    RecentFileBtn4: TSpeedButton;
    RecentFileBtn5: TSpeedButton;
    RecentFileBtn6: TSpeedButton;
    RecentFileBtn7: TSpeedButton;
    SampleProjectBtn0: TSpeedButton;
    SampleProjectBtn1: TSpeedButton;
    SampleProjectBtn2: TSpeedButton;
    SampleProjectBtn3: TSpeedButton;
    SampleProjectBtn4: TSpeedButton;
    SampleProjectBtn5: TSpeedButton;
    SampleProjectBtn6: TSpeedButton;
    ShowStartPageCB: TCheckBox;
    Panel1: TPanel;
    procedure DevelopBtn1Click(Sender: TObject);
    procedure DevelopBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GetStartedBtn1Click(Sender: TObject);
    procedure GetStartedBtn2Click(Sender: TObject);
    procedure RecentFileBtnClick(Sender: TObject);
    procedure SampleProjectBtn0Click(Sender: TObject);
  private
    RecentFileCount: Integer;
    RecentFileNames: array [0..7] of String;
    SampleFileNames: array [0..2] of String;
    procedure LoadRecentProjects;
    procedure LoadSampleProjects;
  public
    SelectedFile: String;
    SelectedAction: Integer;
  end;

const
  // Startup actions
  saNoAction       = 1;
  saShowTutorial   = 2;
  saShowUserGuide  = 3;
  saNewProject     = 4;
  saOpenProject    = 5;
  saLoadSample     = 6;
  saLoadRecent     = 7;

var
  WelcomeForm: TWelcomeForm;

implementation

{$R *.lfm}

uses
  main;

{ TWelcomeForm }

procedure TWelcomeForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Color := MainForm.Color;  //$00F9F4F0;
  Font.Size := 9;
  NoRecentProjectsLbl.Left := Label4.Left;
  NoRecentProjectsLbl.Top := RecentFileBtn0.Top;
  NoRecentProjectsLbl.Visible := False;
  LoadRecentProjects;
  SelectedAction := saNoAction;
end;

procedure TWelcomeForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ModalResult := mrOK;
  Hide;
end;

procedure TWelcomeForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrOK;
end;

procedure TWelcomeForm.GetStartedBtn1Click(Sender: TObject);
begin
  SelectedAction := saShowTutorial;
  ModalResult := mrOK;
end;

procedure TWelcomeForm.GetStartedBtn2Click(Sender: TObject);
begin
  SelectedAction := saShowUserGuide;
  ModalResult := mrOK;
end;

procedure TWelcomeForm.RecentFileBtnClick(Sender: TObject);
begin
 with Sender as TSpeedButton do
  begin
    SelectedFile := RecentFileNames[Tag];
  end;
  SelectedAction := saLoadRecent;
  ModalResult := mrOK;
end;

procedure TWelcomeForm.SampleProjectBtn0Click(Sender: TObject);
begin
  ShowMessage('This feature has not yet been implemented.');
end;

procedure TWelcomeForm.DevelopBtn1Click(Sender: TObject);
begin
  SelectedAction := saNewProject;
  ModalResult := mrOK;
end;

procedure TWelcomeForm.DevelopBtn2Click(Sender: TObject);
begin
  SelectedAction := saOpenProject;
  ModalResult := mrOK;
end;

procedure TWelcomeForm.LoadSampleProjects;
//
//  Loads names of sample projects into the SamplesListView.
//
begin

end;

procedure TWelcomeForm.LoadRecentProjects;
var
  I, J: Integer;
  S: String;
  SpeedBtn: TSpeedButton;
begin
  J := 0;
  RecentFileCount := 0;
  for I := 0 to MainForm.MRUMenuMgr.Recent.Count - 1 do
  begin
    S := MainForm.MRUMenuMgr.Recent[I];
    if Length(S) = 0 then break;
    if  not FileExists(S) then continue;
    SpeedBtn := Self.FindComponent('RecentFileBtn' + IntToStr(J)) as TSpeedButton;
    SpeedBtn.Caption := ExtractFilename(S);
    SpeedBtn.Visible := True;
    SpeedBtn.ShowHint := True;
    SpeedBtn.Hint := S;
    RecentFileNames[J] := S;
    Inc(RecentFileCount);
    if J = High(RecentFileNames) then break;
    Inc(J);
  end;
  NoRecentProjectsLbl.Visible :=  (RecentFileCount = 0);
end;


end.

