{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       basemapmenu
 Description:  a form with a visual menu of basemap sources
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit basemapmenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons,
  StdCtrls, ExtCtrls, LCLtype, LCLintf, HtmlView, HtmlGlobals;

type

  { TBasemapMenuForm }

  TBasemapMenuForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    EpsgEdit: TEdit;
    EpsgHelpViewer: THtmlViewer;
    ImageFileBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    WebMapPanel: TPanel;
    SpeedButton1: TSpeedButton;
    WebMapBox: TGroupBox;
    ImageList1: TImageList;
    procedure BitBtn1Click(Sender: TObject);
    procedure EpsgEditChange(Sender: TObject);
    procedure EpsgHelpViewerHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
  private

  public
    MapSelection: Integer;
    function GetEpsg: String;
  end;

var
  BasemapMenuForm: TBasemapMenuForm;

implementation

{$R *.lfm}

uses
  project, config, utils;

const
  Msg1: String =
    'Internet maps are only available when ' +
    'map coordinate units are degrees.';
  Msg2: String =
    'You are not connected to the internet.';
  EpsgHelp: String =
    '<p><b>CRS EPSG</b></p>' +
    '<p>Basemaps provided by the internet use geographic coordinates ' +
    '(latitude, longitude). If your project uses some other coordinate ' +
    'reference system, such as State Plane or UTM, then its EPSG code ' +
    'must be provided so that the network map can be displayed correctly ' +
    'over the basemap.</p>' +
    '<p>EPSG codes for different Coordinate Reference Systems can be found ' +
    '<a href="https://spatialreference.org/">here</a>.</p>';

{ TBasemapMenuForm }

procedure TBasemapMenuForm.FormCreate(Sender: TObject);
begin
  // Begin with web map selection panel disabled
  Font.Size := config.FontSize;
  MapSelection := -1;
  WebMapPanel.Enabled := False;

  // Inform user if there is no internet connection
  if not utils.HasInternetConnection() then
  begin
    Notebook1.PageIndex := 1;
  end

  // If project is empty or uses lat-lon coordinates then enable the web map
  // selection panel and use the Google Maps geographic CRS of 4326
  else if project.IsEmpty or (project.MapUnits = muDegrees) then
  begin
    WebMapPanel.Enabled := True;
    EpsgEdit.Enabled := False;
    EpsgEdit.Text := '4326';
  end

  else if project.OldMapEPSG > 0 then
  begin
    WebMapPanel.Enabled := True;
    EpsgEdit.Enabled := True;
    EpsgEdit.Text := IntToStr(project.OldMapEPSG);
  end

  // Otherwise allow user to specify a CRS EPSG code (web map selection panel
  // will be enabled once user enters a code)
  else begin
    EpsgEdit.Enabled := True;
    EpsgEdit.Text := '';
  end;

  // Set up the contents of the help viewer
  EpsgHelpViewer.DefFontSize := config.FontSize;
  EpsgHelpViewer.DefBackground := $00E0FFFF;
  EpsgHelpViewer.Top := 208;
  EpsgHelpViewer.Height := 252;
  EpsgHelpViewer.LoadFromString(EpsgHelp);
  EpsgHelpViewer.Visible := False;
end;

procedure TBasemapMenuForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    if EpsgHelpViewer.Visible then EpsgHelpViewer.Visible := False
    else ModalResult := mrOK;
  end;
end;

procedure TBasemapMenuForm.SpeedButton1Click(Sender: TObject);
begin
  EpsgHelpViewer.Visible := True;
end;

procedure TBasemapMenuForm.BitBtn1Click(Sender: TObject);
begin
  with Sender as TBitBtn do MapSelection := Tag;
  ModalResult := mrOK;
end;

function TBasemapMenuForm.GetEpsg: String;
begin
  Result := EpsgEdit.Text;
end;

procedure TBasemapMenuForm.EpsgEditChange(Sender: TObject);
begin
  WebMapPanel.Enabled := Length(EpsgEdit.Text) > 0;
end;

procedure TBasemapMenuForm.EpsgHelpViewerHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: Boolean);
begin
  OpenUrl('https://spatialreference.org/');
end;

end.

