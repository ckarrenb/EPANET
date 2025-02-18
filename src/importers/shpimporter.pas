{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       shpimporter
 Description:  a wizard dialog form used to import a pipe network
               from a shapefile
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit shpimporter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ComCtrls, Grids, SpinEx, FileCtrl;

type

  { TShpImporterForm }

  TShpImporterForm = class(TForm)
    AddToProjectCB: TCheckBox;
    ClearLinksBtn: TBitBtn;
    ClearNodesBtn: TBitBtn;
    ComputeLengthsCB: TCheckBox;
    EpsgEdit: TEdit;
    FeetRB: TRadioButton;
    BackBtn: TBitBtn;
    Image1: TImage;
    ImportBtn: TBitBtn;
    Label1: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    IntroLabel: TLabel;
    ViewNodeAttribLabel: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    PrjFileLabel: TLabel;
    LinksDataGrid: TStringGrid;
    LinksFileBtn: TBitBtn;
    LinksFileEdit: TEdit;
    MetersRB: TRadioButton;
    NextBtn: TBitBtn;
    CancelBtn: TBitBtn;
    NodesDataGrid: TStringGrid;
    NodesFileBtn: TBitBtn;
    NodesFileEdit: TEdit;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    BtnPanel: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel9: TPanel;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    SnapTolEdit: TFloatSpinEditEx;
    LinksTabSheet: TTabSheet;
    NodesTabSheet: TTabSheet;
    OptionsTabSheet: TTabSheet;
    TabSheet4: TTabSheet;
    UnitsCombo: TComboBox;
    ViewLinkAttribLabel: TLabel;
    procedure ImportBtnClick(Sender: TObject);
    procedure BackBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ClearLinksBtnClick(Sender: TObject);
    procedure ClearNodesBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure PrjFileLabelClick(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FileBtnClick(Sender: TObject);
    procedure ViewLinkAttribLabelClick(Sender: TObject);
    procedure ViewNodeAttribLabelClick(Sender: TObject);
  private
    NodeFile:   String;      // Name of node shape file
    LinkFile:   String;      // Name of link shape file
    HasChanged: Boolean;     // True if new data loaded
    Bitmap:     TBitMap;     // Bitmap used to preview network
    function  LoadDbfFields(Fname: String; aGrid: TStringGrid): Boolean;
    procedure ClearDataGrid(aGrid: TStringGrid);
    procedure SetButtonStates;
    function  ReadPrjFile: Boolean;
    function  ReadEpsg(Prj: String): String;
    function  ReadUnits(Prj: String): String;
  public

  end;

var
  ShpImporterForm: TShpImporterForm;

implementation

{$R *.lfm}

uses
  main, config, project, utils, shpviewer, shploader, shpapi;

{ TShpImporterForm }

const

 Intro: String =
   'The following pages will step you' + LineEnding + ' ' + LineEnding +
   'through the process of importing' +  LineEnding + ' ' + LineEnding +
   'georeferenced node and link data' +  LineEnding + ' ' + LineEnding +
   'from GIS shapefiles into EPANET.';

  LinkProps: array[0..9] of String =
  ('Link Property', 'Link ID', 'Link Type', 'Start Node', 'End Node',
   'Description', 'Tag', 'Length', 'Diameter', 'Roughness');

  NodeProps: array[0..6] of String =
  ('Node Property', 'Node ID', 'Node Type', 'Description', 'Tag',
   'Elevation', 'Base Demand');

procedure TShpImporterForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Color := config.ThemeColor;
  Font.Size := config.FontSize;
  ViewLinkAttribLabel.Font.Size := config.FontSize;
  ViewNodeAttribLabel.Font.Size := config.FontSize;
  PrjFileLabel.Font.Size := config.FontSize;
  IntroLabel.Caption := Intro;
  for I := Low(LinkProps) to High(LinkProps) do
    LinksDataGrid.Cells[0,I] := LinkProps[I];
  LinksDataGrid.FixedColor := Color;
  for I := Low(NodeProps) to High(NodeProps) do
    NodesDataGrid.Cells[0,I] := NodeProps[I];
  NodesDatagrid.FixedColor := Color;
  BackBtn.Visible := False;
  ImportBtn.Left := NextBtn.Left;
  ImportBtn.Visible := False;
  UnitsCombo.ItemIndex := 0;
  Bitmap := TBitmap.Create;
  Bitmap.PixelFormat := pf24Bit;
  Bitmap.Canvas.Brush.Color := clWhite;
  Bitmap.Canvas.Brush.Style := bsSolid;
  Notebook1.PageIndex := 0;
  PageControl1.ActivePageIndex := 0;
end;

procedure TShpImporterForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Bitmap.Free;
end;

procedure TShpImporterForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TShpImporterForm.ClearLinksBtnClick(Sender: TObject);
begin
  LinkFile := '';
  LinksFileEdit.Text := '';
  ClearDataGrid(LinksDataGrid);
  ViewLinkAttribLabel.Visible := False;
end;

procedure TShpImporterForm.ClearNodesBtnClick(Sender: TObject);
begin
  NodeFile := '';
  NodesFileEdit.Text := '';
  ClearDataGrid(NodesDataGrid);
  ViewNodeAttribLabel.Visible := False;
end;

procedure TShpImporterForm.BackBtnClick(Sender: TObject);
begin
  with PageControl1 do
  begin
    if ActivePageIndex > 0 then ActivePageIndex := ActivePageIndex - 1;
  end;
  SetButtonStates;
end;

procedure TShpImporterForm.NextBtnClick(Sender: TObject);
begin
  if Notebook1.PageIndex = 0 then Notebook1.PageIndex := 1
  else with PageControl1 do
  begin
    if ActivePageIndex < 3 then ActivePageIndex := ActivePageIndex + 1;
  end;
  SetButtonStates;
end;

procedure TShpImporterForm.ImportBtnClick(Sender: TObject);
// Import link and node data from a shapefile into an EPANET project.

var
  ShpOptions: shploader.TShpOptions;
  R, Epsg, Code: Integer;
  S: String;
begin
  if not AddToProjectCB.Checked then
  begin
    MainForm.FileNew(False);
    Epsg := 0;
    Val(EpsgEdit.Text, Epsg, Code);
    project.MapEPSG := Epsg;
    project.OldMapEPSG := Epsg;
  end;
  
  with ShpOptions do
  begin
    NodeFileName := NodeFile;
    LinkFileName := LinkFile;
    CoordUnits := UnitsCombo.ItemIndex;
    SnapTol := SnapTolEdit.Value;
    if FeetRB.Checked then SnapUnits := 1 else SnapUnits := 2;
    ComputeLengths := ComputeLengthsCB.Checked;
    
    with LinksDataGrid do
    begin
      for R := 1 to RowCount-1 do
      begin
        S := Cells[1, R];
        LinkAttribs[R] := Columns[0].PickList.IndexOf(S) - 1;
        LinkUnits[R] := Cells[2,R];
      end;
    end;
    
    with NodesDataGrid do
    begin
      for R := 1 to RowCount-1 do
      begin
        S := Cells[1, R];
        NodeAttribs[R] := Columns[0].PickList.IndexOf(S) - 1;
        NodeUnits[R] := Cells[2,R];
      end;
    end;
  end;
  
  shploader.LoadShapeFile(ShpOptions);
  Hide;
  ModalResult := mrOK;
end;

procedure TShpImporterForm.PrjFileLabelClick(Sender: TObject);
var
  S: String;
begin
  if ReadPrjFile then exit;
  if QuestionDlg('EPANET', 'No projection data were found.'+LineEnding+
    'Would you like to search yourself?', mtInformation,
      [mrYes, mrNo],0) = mrNo then exit;

  with MainForm.OpenDialog1 do
  begin
    S := Title;
    Title := 'Select a Projection File';
    Filter := 'Projection Files|*.prj';
    Filename := '*.prj';
    if Execute then
    begin
      with TShpViewerForm.Create(self) do
      try
        ViewPrjFile(Filename);
        ShowModal;
      finally
        Free;
      end;
    end;
    Title := S;
  end;
end;

procedure TShpImporterForm.FileBtnClick(Sender: TObject);
// Load either a Links shapefile or Nodes shapefile into their
// respective tabsheets.

var
  Fname: String = '';
begin
  with MainForm.OpenDialog1 do
  begin
    Filter := 'Shape Files|*.shp';
    Filename := '*.shp';
    if Execute then Fname := Filename else exit;
   end;

  if PageControl1.ActivePage = LinksTabSheet then
  begin
    if LoadDbfFields(Fname, LinksDataGrid) then
    begin
      LinksFileEdit.Text := MinimizeName(Fname, Canvas, LinksFileEdit.Width);
      if not SameText(Fname, LinkFile) then HasChanged := True;
      LinkFile := Fname;
    end
    else LinksFileEdit.Text := '';
  end;

  if PageControl1.ActivePage = NodesTabSheet then
  begin
    if LoadDbfFields(Fname, NodesDataGrid) then
    begin
      NodesFileEdit.Text := MinimizeName(Fname, Canvas, NodesFileEdit.Width);
      if not SameText(Fname, NodeFile) then HasChanged := True;
      NodeFile := Fname;
    end
    else NodesFileEdit.Text := '';
  end;
end;

procedure TShpImporterForm.ViewLinkAttribLabelClick(Sender: TObject);
begin
  with TShpViewerForm.Create(self) do
  try
    if ViewDbfFile(LinkFile) then ShowModal;
  finally
    Free;
  end;
end;

procedure TShpImporterForm.ViewNodeAttribLabelClick(Sender: TObject);
begin
  with TShpViewerForm.Create(self) do
  try
    if ViewDbfFile(NodeFile) then ShowModal;
  finally
    Free;
  end;
end;

procedure TShpImporterForm.PageControl1Change(Sender: TObject);
begin
  SetButtonStates;
end;

procedure TShpImporterForm.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, Bitmap);
end;

procedure TShpImporterForm.SetButtonStates;
begin
  ImportBtn.Visible := False;
  BackBtn.Visible := True;
  NextBtn.Visible := True;
  TabSheet4.TabVisible := False;
  if PageControl1.ActivePageIndex = 0 then BackBtn.Visible := False;
  if PageControl1.ActivePageIndex = 3 then
  begin
    NextBtn.Visible := False;
    ImportBtn.Visible := True;
    TabSheet4.TabVisible := True;
    if HasChanged then
    begin
      Bitmap.SetSize(PaintBox1.Width, PaintBox1.Height);
      Bitmap.Canvas.Rectangle(0, 0, Bitmap.Width, Bitmap.Height);
      if shpviewer.ViewShpFile(LinkFile, NodeFile, Bitmap) = True then
        HasChanged := False;
      PaintBox1.Refresh;
    end;
  end;
end;

function TShpImporterForm.LoadDbfFields(Fname: String; aGrid: TStringGrid): Boolean;
// Load the shapfile's attribute names into a StringGrid.

var
  ShapeType: Integer;
  MinBound: array [0..3] of Double;
  MaxBound: array [0..3] of Double;
  FieldName: array[0..XBASE_FLDNAME_LEN_READ] of Char;
  Count, FieldWidth, FieldDecimals: Integer;
  Shp: SHPHandle;
  Dbf: DBFHandle;
  I: Integer;
begin
  Result := False;
  Shp := nil;
  Dbf := nil;

  try
    // Clear list of imported data fields
    aGrid.Columns[0].PickList.Clear;
    aGrid.Columns[0].PickList.Add('');

    // Open the shape file
    Shp := SHPOpen(PAnsiChar(Fname), 'rb');
    if Shp = Nil then
    begin
      utils.MsgDlg('File is not a valid shape file.', mtError, [mbOk]);
      exit;
    end;
    shpapi.SHPGetInfo(Shp, Count, ShapeType, MinBound, MaxBound);

    // Check that shape file is of the correct type
    if PageControl1.ActivePage = LinksTabSheet then
    begin
      if ShapeType <> SHPT_ARC then
      begin
        utils.MsgDlg('File does not contain link data.', mtError, [mbOk]);
        exit;
      end;
    end
    else
    begin
      if ShapeType <> SHPT_POINT then
      begin
        utils.MsgDlg('File does not contain node data.', mtError, [mbOk]);
        exit;
      end;
    end;

    // Open the shape file's corresponding dBase file
    Fname := ChangeFileExt(Fname, '.dbf');
    Dbf := shpapi.DBFOpen(PAnsiChar(Fname), 'rb');

    // Load data fields into grid's first column
    if Dbf <> Nil then
    begin
      Count := shpapi.DBFGetFieldCount(Dbf);
      for I := 0 to Count-1 do
      begin
        shpapi.DBFGetFieldInfo(Dbf, I, FieldName, FieldWidth, FieldDecimals);
        aGrid.Columns[0].PickList.Add(FieldName);
      end;
      if PageControl1.ActivePage = LinksTabSheet then
        ViewLinkAttribLabel.Visible := True
      else
        ViewNodeAttribLabel.Visible:= True;
    end;
    Result := True;

  // Close the shape and Dbase files
  finally
    shpapi.DBFClose(Dbf);
    shpapi.SHPClose(Shp);
  end;
end;

procedure TShpImporterForm.ClearDataGrid(aGrid: TStringGrid);
var
  I, J: Integer;
begin
  with aGrid do
  begin
    for I := 1 to ColCount-1 do
    begin
      for J := 1 to RowCount-1 do Cells[I,J] := '';
    end;
  end;
end;

function TShpImporterForm.ReadPrjFile: Boolean;
// Read the EPSG code and coordinate units from a .prj file.

var
  Fname: String;
  F: TextFile;
  S: String;
  Units: String;
  Epsg: String;
begin
  Result := False;
  Fname := ChangeFileExt(LinkFile, '.prj');
  if not FileExists(Fname) then
    Fname := ChangeFileExt(NodeFile, '.prj');
  if not FileExists(Fname) then exit;
  S := '';
  AssignFile(F, Fname);
  Reset(F);
  while not Eof(F) do
  begin
    Readln(F, S);
  end;
  CloseFile(F);
  S := UpperCase(S);
  Units := ReadUnits(S);
  Epsg := ReadEpsg(S);
  if (Length(Units) > 0) or (Length(Epsg) > 0) then
  begin
    S := 'The following projection data were found:' + LineEnding;
    S := S + 'EPSG: ' + Epsg + LineEnding + 'Units: ' + Units;
    if QuestionDlg('EPANET', S, mtInformation,
      [mrOK, 'Accept', mrCancel], 0) = mrOK then
    begin
      UnitsCombo.ItemIndex := UnitsCombo.Items.IndexOf(Units);
      EpsgEdit.Text := Epsg;
      Result := True;
    end
    else exit;
  end;
end;

function TShpImporterForm.ReadEpsg(Prj: String): String;
// Parse an EPSG code from string Prj.

const
  EpsgSubStr: String = 'AUTHORITY["EPSG","';
  GeoSubStr: String = 'GEOGCS';
  Wgs84SubStr: String = 'WGS_1984';
var
  N1, N2: Integer;
begin
  Result := '';
  if Length(Prj) = 0 then exit;
  if (Pos(GeoSubStr, Prj) > 0) and (Pos(Wgs84SubStr, Prj) > 0) then
    Result := '4326'
  else begin
    N1 := Prj.LastIndexOf(EpsgSubStr);
    if (N1 > 0) then
    begin
      N1 := N1 + Length(EpsgSubStr) + 1;
      N2 := LastDelimiter('"', Prj);
      Result := Copy(Prj, N1, N2 - N1);
    end;
  end;
end;

function TShpImporterForm.ReadUnits(Prj: String): String;
// Parse the units of the shapefile coordinates from string Prj

const
  UnitSubStr: String = ',UNIT["';
var
  N1, N2: Integer;
  S: String;
begin
  Result := 'Unknown';
  if Length(Prj) = 0 then exit;
  N1 := Prj.LastIndexOf(UnitSubStr);
  if N1 > 0 then
  begin
    N1 := N1 + Length(UnitSubStr) + 1;
    N2 := Pos('"', Prj, N1);
    if N2 <= N1 then exit;
    S := Upcase(Copy(Prj, N1, N2-N1));
    if Pos('DEG',S) > 0 then Result := 'Degrees'
    else if Pos('FEET', S) > 0 then Result := 'Feet'
    else if Pos('FOOT', S) > 0 then Result := 'Feet'
    else if Pos('MET', S) > 0 then Result := 'Meters';
  end;
end;

end.
