{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       msxeditor
 Description:  a form used to edit EPANET-MSX input data
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}
{
 The MsxEditorForm consists of a MenuPanel used to select a
 category of MSX input to edit, a MainPanel that contains a
 Notebook with pages used to edit each MSX data category, a
 normally hidden SymbolsPanel used to list reserved variable
 names that can be used in reaction expressions, and a
 ButtonPanel that contains various action buttons.
 ________________________________________________________
 |            |                          |              |
 |  MenuPanel |         MainPanel        | SymbolsPanel |
 |            |                          |              |
 |            |                          |              |
 |            |                          |              |
 |____________|__________________________|______________|
 |                     ButtonPanel                      |
 |______________________________________________________|

Main Panel Notebook Pages:
  Overview: brief description of the editor and its commands
  Options: MSX analysis options
  Species: names species to analyze in StringGrid1
  Pipes:   edits pipe reaction expressions in StringGrid2
  Tanks:   edits tank reaction expressions in StringGrid3
  Terms:   edits terms used in reaction expressions in StringGrid4
  Coeffs:  edits coefficients used in terms & reactions in StringGrid5
  Params:  assigns coefficient values to specific pipes in StringGrid6
  Quality: assigns initial quality to nodes in StrinGrid7
  Sources: assigns WQ sources to nodes in StringGrid8
}

unit msxeditor;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Menus, ComCtrls, Buttons, HtmlView, SpinEx, StrUtils, lclintf,
  Clipbrd, HTMLUn2, HtmlGlobals, LCSVUtils;

type

  { TMsxEditorForm }

  TMsxEditorForm = class(TForm)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    CopyBtn: TToolButton;
    CutBtn: TToolButton;
    TitleEdit: TEdit;
    FloatSpinEditEx1: TFloatSpinEditEx;
    FloatSpinEditEx2: TFloatSpinEditEx;
    FloatSpinEditEx3: TFloatSpinEditEx;
    FloatSpinEditEx4: TFloatSpinEditEx;
    FloatSpinEditEx5: TFloatSpinEditEx;
    HelpBtn: TButton;
    HtmlViewer1b: THtmlViewer;
    HtmlViewer1a: THtmlViewer;
    HtmlViewer2: THtmlViewer;
    OfficeImageList: TImageList;
    MaterialImageList: TImageList;
    InsertBtn: TToolButton;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MoveDnBtn: TToolButton;
    MoveUpBtn: TToolButton;
    Panel3: TPanel;
    PasteBtn: TToolButton;
    OkBtn: TButton;
    CancelBtn: TButton;
    LoadBtn: TButton;
    InstructPanel: TPanel;
    IntroPanel: TPanel;
    InstructTextPanel: TPanel;
    RemoveBtn: TToolButton;
    SaveBtn: TButton;
    ClearBtn: TButton;
    Coeffs: TPage;
    Image1: TImage;
    ImageList1: TImageList;
    SaveDialog1: TSaveDialog;
    SectionListBox: TListBox;
    Notebook1: TNotebook;
    OpenDialog1: TOpenDialog;
    Options: TPage;
    Overview: TPage;
    MenuPanel: TPanel;
    Panel2: TPanel;
    ButtonPanel: TPanel;
    MainPanel: TPanel;
    SymbolsPanel: TPanel;
    TitlePanel: TPanel;
    ImagePanel: TPanel;
    Params: TPage;
    Pipes: TPage;
    Quality: TPage;
    Sources: TPage;
    Species: TPage;
    StringGrid1: TStringGrid;
    StringGrid5: TStringGrid;
    StringGrid4: TStringGrid;
    StringGrid2: TStringGrid;
    StringGrid3: TStringGrid;
    StringGrid8: TStringGrid;
    StringGrid7: TStringGrid;
    StringGrid6: TStringGrid;
    Tanks: TPage;
    Terms: TPage;
    EditingToolBar: TToolBar;
    SymbolsBtn: TToolButton;
    procedure ClearBtnClick(Sender: TObject);
    procedure CopyBtnClick(Sender: TObject);
    procedure CutBtnClick(Sender: TObject);
    procedure TitleEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure HtmlViewer1bHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: Boolean);
    procedure InsertBtnClick(Sender: TObject);
    procedure MoveDnBtnClick(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PasteBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SectionListBoxClick(Sender: TObject);
    procedure StringGrid1ColRowInserted(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure StringGrid1SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure StringGrid5SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure StringGrid2SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure StringGrid3SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure StringGrid8EditButtonClick(Sender: TObject);
    procedure StringGrid8SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure StringGrid7SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure StringGrid6SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure SymbolsBtnClick(Sender: TObject);
  private
    Comment: String;
    HasChanged: Boolean;
    MsxFile: String;
    FocusedGrid: TStringGrid;
    procedure ClearAll;
    procedure SelectionSetText(TheText: String);
    procedure ReadMsxFile(Filename: String);

  public

  end;

var
  MsxEditorForm: TMsxEditorForm;

implementation

{$R *.lfm}

uses
  project, config, msxfileprocs, patternselector, utils, epanet2;

{$I msxtext.txt}

const

  // Default choices for ComboBox1 to ComboBox5
  DefListOptions: array[1..5] of Integer =
    (0, 2, 0, 0, 0);

  // Default values for FloatSpinEditEx1 to FloatSpinEditEx5
  DefValueOptions: array[1..5] of Double =
    (300, 0.01000, 0.01000, 5000, 1000);

  SaveMsg = 'Please save your newly created MSX model.';
  ShowSymbolsMsg = 'Show Reserved Symbols Panel';
  HideSymbolsMsg = 'Hide Reserved Symbols Panel';
  ShowSymbolsImageIndex = 7;
  HideSymbolsImageIndex = 8;

{ TMsxEditorForm }

procedure TMsxEditorForm.FormCreate(Sender: TObject);
begin
  Color := config.ThemeColor;
  TitlePanel.Color := Color;
//  IntroPanel.Color := $00E0FFFF; //  clInfoBk;  //Color;
  Font.Size := config.FontSize;

  // HtmlViewers used to display an introduction to the editor
  HtmlViewer1a.DefFontColor := clBlack;
  HtmlViewer1b.DefFontColor := clBlack;
  HtmlViewer1a.DefFontSize := Font.Size;
  HtmlViewer1b.DefFontSize := Font.Size;
  HtmlViewer1a.DefBackground := clCream;  //$00E0FFFF;
  HtmlViewer1b.DefBackground := clCream;  //$00E0FFFF;
  HtmlViewer1a.LoadFromString(IntroA);
  HtmlViewer1b.LoadFromString(IntroB);

  // HtmlViewer2 displays reserved variable names in the SymbolsPanel
  HtmlViewer2.DefFontSize := Font.Size;
  HtmlViewer2.LoadFromString(Symbols);
  SymbolsPanel.Visible := False;

  SectionListBox.ItemIndex := 0;
  Notebook1.PageIndex := 0;
  InstructPanel.Visible := False;

  // Initialize the editing toolbar used with StringGrids
  if SameText(config.IconFamily, 'Material') then
    EditingToolBar.Images := MaterialImageList
  else
    EditingToolBar.Images := OfficeImageList;
  EditingToolBar.Visible := False;
  Clipboard.Clear;
  Clipboard.asText := '';

  // Clear all data entry fields
  ClearAll;
  HasChanged := False;
  FocusedGrid := nil;
end;

procedure TMsxEditorForm.FormShow(Sender: TObject);

//  Load the current MSX file into the editor

begin
  MsxFile := project.MsxInpFile;
  if Length(MsxFile) > 0 then
  begin
    ReadMsxFile(MsxFile);
  end;
end;

procedure TMsxEditorForm.HtmlViewer1bHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: Boolean);
begin
  OpenUrl('https://epanetmsx2manual.readthedocs.io/en/latest/1_introduction.html');
end;

procedure TMsxEditorForm.TitleEditChange(Sender: TObject);

// OnChange handler shared by all input controls on the Options page

begin
  HasChanged := True;
end;

//------------------------------------------------------------------------------
//  ButtonPanel Procedures
//------------------------------------------------------------------------------

procedure TMsxEditorForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMsxEditorForm.LoadBtnClick(Sender: TObject);
var
  R: Integer;
  NewFileName: String;
begin
  // Get the name of the new MSX file to load
  NewFileName := '';
  with OpenDialog1 do
  begin
    Filter := 'MSX Files|*.msx';
    FileName := '*.msx';
    if Execute then NewFileName := FileName;
  end;
  if Length(NewFileName) = 0 then exit;

  // Ask if all existing editor contents should be replaced
  if HasChanged or (Length(MsxFile) > 0 )then
  begin
    R := utils.MsgDlg(
      'All current MSX data will be replaced by the contents of ' +
        ExtractFileName(NewFileName), mtConfirmation, [mbOK, mbCancel]);
    if R <> mrOK then exit;
  end;

  // Clear the editor's contents and load the new MSX file
  ClearAll;
  MsxFile := NewFileName;
  ReadMsxFile(MsxFile);
  HasChanged := True;
end;

procedure TMsxEditorForm.ReadMsxFile(Filename: String);
begin
  msxfileprocs.ReadMsxFile(self, Filename);
  SectionListBox.ItemIndex := 1;
  SectionListBoxClick(self);
end;

procedure TMsxEditorForm.SaveBtnClick(Sender: TObject);

//  Save the editor's contents to file

var
  CurrentMsxFile: String;
  CurrentMsxDir: String;
begin
  CurrentMsxFile := MsxFile;
  CurrentMsxDir := ExtractFileDir(CurrentMsxFile);
  if Length(CurrentMsxDir) = 0 then
    CurrentMsxDir := ExtractFileDir(project.InpFile);
  with SaveDialog1 do
  begin
    if Length(CurrentMsxFile) > 0 then
      FileName := CurrentMsxFile
    else
      FileName := '*.msx';
    if Length(CurrentMsxDir) > 0 then
      InitialDir := CurrentMsxDir;
    Filter := 'EPANET MSX Files|*.msx|All Files|*.*';
    if Execute then
    begin
      msxfileprocs.WriteMsxFile(self, FileName);
      MsxFile := FileName;
      HasChanged := False;
    end;
  end;
end;

procedure TMsxEditorForm.OkBtnClick(Sender: TObject);
begin
  if HasChanged then
  begin
    if Length(MsxFile) = 0 then
    begin
      if utils.MsgDlg(SaveMsg, mtInformation, [mbOK, mbCancel], self) = mrOk then
        SaveBtnClick(Sender)
      else exit;
    end
    else msxfileprocs.WriteMsxFile(self, MsxFile);
  end;
  if HasChanged or (not SameText(MsxFile, project.MsxInpFile)) then
    project.HasChanged := True;
  project.MsxInpFile := MsxFile;
  project.UpdateResultsStatus;
  ModalResult := mrOK;
end;

procedure TMsxEditorForm.HelpBtnClick(Sender: TObject);

const
  HelpTopics: array[1..9] of String =
    ('#options','#species','#pipes','#tanks','#terms','#coefficients',
     '#parameters','#quality','#sources');

var
  I: Integer;
  Url: String = 'https://epanetmsx2manual.readthedocs.io/en/latest/';
begin
  I := SectionListBox.ItemIndex;
  if I = 0 then
    Url := Url + '1_introduction.html'
  else
    Url := Url + '4_inputformat.html' + HelpTopics[I];
  OpenUrl(Url);
end;

procedure TMsxEditorForm.ClearBtnClick(Sender: TObject);
var
  R: Integer;
begin
  if HasChanged or (Length(MsxFile) > 0 )then
  begin
    R := utils.MsgDlg('This will remove all current MSX data from the project.',
      mtConfirmation, [mbOK, mbCancel]);
    if R = mrOK then ClearAll;
  end;
end;

procedure TMsxEditorForm.ClearAll;
var
  I, C: Integer;
begin
  // Initialize the StringGrid editors
  for I := 1 to 8 do
  begin
    with FindComponent('StringGrid' + IntToStr(I)) as TStringGrid do
    begin
      FastEditing := False;
      RowCount := 1;  // The header row
    end;
  end;

  // Initialize contents of Notebook1's Options page
  TitleEdit.Text := '';
  for I := 1 to 5 do
  begin
    with FindComponent('ComboBox' + IntToStr(I)) as TComboBox do
      ItemIndex := DefListOptions[I];
  end;
  for I := 1 to 5 do
  begin
    with FindComponent('FloatSpinEditEx' + IntToStr(I)) as TFloatSpinEditEx do
      Value := DefValueOptions[I];
  end;
  MsxFile := '';
  HasChanged := False;
end;

//------------------------------------------------------------------------------
//  StringGrid Procedures
//------------------------------------------------------------------------------

procedure TMsxEditorForm.SectionListBoxClick(Sender: TObject);

// Show the editing page for a data section selected from the SectionListBox.

var
  I: Integer;
begin
  I := SectionListBox.ItemIndex;
  Notebook1.PageIndex := I;
  if I > 0 then
  begin
    InstructPanel.Visible := True;
    InstructTextPanel.Caption := Instructs[I];
  end
  else InstructPanel.Visible := False;
  if I > 1 then
  begin
    EditingToolBar.Visible := True;
    FocusedGrid := FindComponent('StringGrid' + IntToStr(I-1)) as TStringGrid;
  end else
  begin
    EditingToolBar.Visible := False;
    FocusedGrid := nil;
  end;
  SymbolsPanel.Visible := False;
  SymbolsBtn.Visible := (I in [3,4,5]);
  SymbolsBtn.Hint := ShowSymbolsMsg;
  SymbolsBtn.ImageIndex := ShowSymbolsImageIndex;
end;

procedure TMsxEditorForm.StringGrid1ColRowInserted(Sender: TObject; IsColumn: Boolean;
  sIndex, tIndex: Integer);

// Initialize the selection in a PickList column when a new row is inserted
// into a StringGrid -- shared handler for all of the editor's StringGrids.

var
  C, R: Integer;
begin
  with Sender as TStringGrid do
  begin
    R := RowCount - 1;
    for C := 0 to ColCount-1 do
    begin
      if Columns[C].PickList.Count > 0 then
        Cells[C,R] := Columns[C].PickList[0];
    end;
  end;
end;

procedure TMsxEditorForm.StringGrid1SetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  HasChanged := True;
end;

procedure TMsxEditorForm.StringGrid1SelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if (aCol = 0) or (aCol = 2) then
  begin
    Editor := StringGrid1.EditorByStyle(cbsPickList);
    if Editor is TCustomComboBox then
      with Editor as TCustomComboBox do Style := csDropDownList;
  end;
end;

procedure TMsxEditorForm.StringGrid2SelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if aCol = 0 then
  begin
    Editor := StringGrid2.EditorByStyle(cbsPickList);
    if Editor is TCustomComboBox then
      with Editor as TCustomComboBox do Style := csDropDownList;
  end;
end;

procedure TMsxEditorForm.StringGrid3SelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if aCol = 0 then
  begin
    Editor := StringGrid3.EditorByStyle(cbsPickList);
    if Editor is TCustomComboBox then
      with Editor as TCustomComboBox do Style := csDropDownList;
  end;
end;

procedure TMsxEditorForm.StringGrid5SelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if aCol = 0 then
  begin
    Editor := StringGrid5.EditorByStyle(cbsPickList);
    if Editor is TCustomComboBox then
      with Editor as TCustomComboBox do Style := csDropDownList;
  end;
end;

procedure TMsxEditorForm.StringGrid6SelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if aCol = 0 then
  begin
    Editor := StringGrid6.EditorByStyle(cbsPickList);
    if Editor is TCustomComboBox then
      with Editor as TCustomComboBox do Style := csDropDownList;
  end;
end;

procedure TMsxEditorForm.StringGrid7SelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if aCol = 0 then
  begin
    Editor := StringGrid7.EditorByStyle(cbsPickList);
    if Editor is TCustomComboBox then
      with Editor as TCustomComboBox do Style := csDropDownList;
  end;
end;

procedure TMsxEditorForm.StringGrid8SelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if aCol = 0 then
  begin
    Editor := StringGrid8.EditorByStyle(cbsPickList);
    if Editor is TCustomComboBox then
      with Editor as TCustomComboBox do Style := csDropDownList;
  end;
end;

procedure TMsxEditorForm.StringGrid8EditButtonClick(Sender: TObject);
var
  S: String;
  PatSelector: TPatternSelectorForm;
begin
  with StringGrid8 do S := Cells[Col,Row];
  PatSelector := TPatternSelectorForm.Create(self);
  try
    PatSelector.Setup(S);
    PatSelector.ShowModal;
    if PatSelector.ModalResult = mrOK then
    begin
      with StringGrid8 do Cells[Col,Row] := PatSelector.SelectedName;
      if not SameText(PatSelector.SelectedName, S) then HasChanged := True;
    end;
  finally
    PatSelector.Free;
  end;
end;

//------------------------------------------------------------------------------
// EditingToolbar Procedures
//------------------------------------------------------------------------------

procedure TMsxEditorForm.CopyBtnClick(Sender: TObject);
begin
  if Assigned(FocusedGrid) then with FocusedGrid do
  begin
    CopyToClipboard(True);
  end;
end;

procedure TMsxEditorForm.CutBtnClick(Sender: TObject);
begin
  if Assigned(FocusedGrid) then with FocusedGrid do
  begin
    CopyToClipboard(True);
    Clean(TRect(Selection), [gzNormal]);
  end;
end;

procedure TMsxEditorForm.PasteBtnClick(Sender: TObject);
begin
  if FocusedGrid <> nil then with FocusedGrid do
  begin
     SelectionSetText(Clipboard.AsText);
  end;
end;

procedure TMsxEditorForm.SelectionSetText(TheText: String);

// Used to paste text from the clipboard into the StringGrid that has focus

var
  StartCol,StartRow: Integer;
  Stream: TStringStream;

  ///// Nested procedure ////
  procedure LoadTSV(Fields: TStringList);
  var
    i, aCol, aRow: Integer;
    NewValue: string;
  begin
    if StartRow < FocusedGrid.RowCount then
    begin
      aRow := StartRow;
      for i := 0 to Fields.Count-1 do
      begin
        aCol := StartCol + i;
        if (aCol < FocusedGrid.ColCount) then
        begin
          NewValue := Fields[i];
          FocusedGrid.Cells[aCol, aRow] := NewValue;
        end;
      end;
      inc(StartRow);
    end;
  end;
  //////////////////////////

begin
  Stream := TStringStream.Create(TheText);
  try
    StartCol := FocusedGrid.Selection.left;
    StartRow := FocusedGrid.Selection.Top;
    LCSVUtils.LoadFromCSVStream(Stream, @LoadTSV, #9);
  finally
    Stream.Free;
  end;
end;

procedure TMsxEditorForm.InsertBtnClick(Sender: TObject);
var
  S: array of String;
begin
  if Assigned(FocusedGrid) then with FocusedGrid do
  begin
    RowCount := RowCount + 1;
  end;
end;

procedure TMsxEditorForm.RemoveBtnClick(Sender: TObject);
begin
  if FocusedGrid <> nil then with FocusedGrid do
  begin
    if RowCount > 1 then DeleteRow(Selection.Top);
  end;
end;

procedure TMsxEditorForm.MoveDnBtnClick(Sender: TObject);
begin
  if Assigned(FocusedGrid) then with FocusedGrid do
  begin
    MoveColRow(False, Row, Row+1);
  end;
end;

procedure TMsxEditorForm.MoveUpBtnClick(Sender: TObject);
begin
  if Assigned(FocusedGrid) then with FocusedGrid do
  begin
    MoveColRow(False, Row, Row-1);
  end;
end;

procedure TMsxEditorForm.SymbolsBtnClick(Sender: TObject);
begin
  if SymbolsBtn.ImageIndex = ShowSymbolsImageIndex then
  begin
    SymbolsPanel.Visible := True;
    SymbolsBtn.ImageIndex := HideSymbolsImageIndex;
    SymbolsBtn.Hint := HideSymbolsMsg;
  end else
  begin
    SymbolsPanel.Visible := False;
    SymbolsBtn.ImageIndex := ShowSymbolsImageIndex;
    SymbolsBtn.Hint := ShowSymbolsMsg;
  end;
end;

end.

