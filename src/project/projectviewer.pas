{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       projectviewer
 Description:  a form that displays all project data
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit projectviewer;

{ Displays all project data in a read-only table, one section at a time. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Grids, LCLType;

type

  { TProjectViewerForm }

  TProjectViewerForm = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    StringGrid1: TStringGrid;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { private declarations }
    procedure RefreshGrid;
    procedure SetGridColCount;
    function  StartOfDataSection(SectionName: String): Integer;
    function  LinesInDataSection(StartLine: Integer): Integer;
    procedure FillGridRow(Row: Integer; Value: String);
  public
    { public declarations }
  end;

var
  ProjectViewerForm: TProjectViewerForm;

implementation

{$R *.lfm}

uses
  project, config;

const
  Sections: array[0..22] of String =
    ('[TITLE]',    '[JUNCTIONS]', '[RESERVOIRS]', '[TANKS]',    '[PIPES]',
     '[PUMPS]',    '[VALVES]',    '[TAGS]',       '[DEMANDS]',  '[EMITTERS]',
     '[LEAKAGE]',  '[STATUS]',   '[PATTERNS]',  '[CURVES]',     '[QUALITY]',
     '[SOURCES]',  '[MIXING]',   '[CONTROLS]',  '[RULES]',      '[REACTIONS]',
     '[ENERGY]',   '[TIMES]',    '[OPTIONS]');

  Headings: array[0..22] of String =
    ('',
     'ID'#9'Elev'#9'Demand',
     'ID'#9'Head'#9'Pattern',
     'ID'#9'Elev'#9'InitLvl'#9'MinLvl'#9'MaxLvl'#9'Diam'#9'MinVol'#9'Curve'#9'Overflow',
     'ID'#9'Node1'#9'Node2'#9'Length'#9'Diam'#9'Roughness'#9'Mloss'#9'Status',
     'ID'#9'Node1'#9'Node2',
     'ID'#9'Node1'#9'Node2'#9'Diam'#9'Type'#9'Setting'#9'Mloss',
     'Object'#9'ID'#9'Tag',
     'Junction'#9'Demand'#9'Pattern',
     'Junction'#9'Coeff',
     'Pipe'#9'Area'#9'Expansion',
     'Link'#9'Status',
     'ID'#9'Factors',
     'ID'#9'X-Value'#9'Y-Value',
     'Node'#9'Quality',
     'Node'#9'Type'#9'Strength'#9'Pattern',
     'Tank'#9'Model'#9'MixFrac',
     '', '', '', '', '', '');


var
  S: TStringList;
  Line: TStringList;
  Section: Integer;

{ TProjectViewerForm }

procedure TProjectViewerForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Color := Config.ThemeColor;
  StringGrid1.AlternateColor := config.AlternateColor;

  // S stores the project's data in input file format
  S := TStringList.Create;
  Line := TStringList.Create;
  Line.Delimiter := ' ';
  for I := 0 to High(Sections) do
    ListBox1.Items.Add(Sections[I]);
end;

procedure TProjectViewerForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
//  Form's OnKeyDown handler.
//-----------------------------------------------------------------------------
begin
  if Key = VK_ESCAPE then ModalResult := mrOK;
end;

procedure TProjectViewerForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
begin
  // Export the project's data to string list S
  Project.Save(Project.AuxFile);
  S.LoadFromFile(Project.AuxFile);
  SysUtils.DeleteFile(AuxFile);

  // Initialize the current data section and list box selection
  Section := -1;
  ListBox1.ItemIndex := 0;

  // Force the list box to display data for the first section
  ListBox1Click(Self);
  ListBox1.SetFocus;
end;

procedure TProjectViewerForm.ListBox1Click(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the data sections list box.
//-----------------------------------------------------------------------------
begin
  with ListBox1 do
  begin
    // Display selected section of project data in the string grid
    if ItemIndex <> Section then
    begin
      Section := ItemIndex;
      RefreshGrid;
      StringGrid1.Row := 1;
      StringGrid1.Col := 0;
    end;
  end;
end;

procedure TProjectViewerForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
//-----------------------------------------------------------------------------
//  Form's OnClose handler.
//-----------------------------------------------------------------------------
begin
  Line.Free;
  S.Free;
end;

procedure TProjectViewerForm.RefreshGrid;
//-----------------------------------------------------------------------------
//  Displays project data from the current section in the string grid.
//-----------------------------------------------------------------------------
var
  I, K, N: Integer;
begin
  SetGridColCount;
  I := StartOfDataSection(Listbox1.Items[Section]);
  N := LinesInDataSection(I);
  StringGrid1.RowCount := StringGrid1.FixedRows + N;
  FillGridRow(0, Headings[Section]);
  if N > 0 then
  begin
    N := 0;
    for K := I+1 to S.Count-1 do
    begin
      // Stop when next section encountered
      if AnsiLeftStr(S[K], 1) = '[' then break;
      if Length(S[K]) = 0 then continue;
      if AnsiLeftStr(S[K], 2) = ';;' then continue;
      Inc(N);
      if (StringGrid1.ColCount = 1) or
         // Place comment in first column
         (AnsiLeftStr(S[K], 1) = ';')  then
          StringGrid1.Cells[0, N] := S[K]
      else FillGridRow(N, S[K]);
    end;
  end;
end;

procedure TProjectViewerForm.SetGridColCount;
begin
  with StringGrid1 do
  begin
    Clear;
    if (Section = 0) or (Section > 13) then
    begin
      ColCount := 1;
      Options := Options - [goHorzLine];
    end else
    begin
      ColCount := 9;
      Options := Options + [goHorzLine];
    end;
  end;
end;

function TProjectViewerForm.StartOfDataSection(SectionName: String): Integer;
var
  J: Integer;
begin
  Result := -1;
  for J := 0 to S.Count-1 do
  begin
    if AnsiStartsStr(SectionName, S[J]) then
    begin
      Result := J;
      break;
    end;
  end;
end;

function TProjectViewerForm.LinesInDataSection(StartLine: Integer): Integer;
var
  K: Integer;
begin
  Result := 0;
  if (StartLine >= 0) then
  begin
    for K := StartLine+1 to S.Count-1 do
    begin
      if AnsiLeftStr(S[K], 2) = ';;' then continue;
      if Length(S[K]) = 0 then continue;
      if AnsiLeftStr(S[K], 1) = '[' then break;
      Inc(Result);
    end;
  end;
end;

procedure TProjectViewerForm.FillGridRow(Row: Integer; Value: String);
var
  Col : Integer;
begin
  Line.DelimitedText := Value;
  for Col := 0 to StringGrid1.ColCount-1 do
    if Col < Line.Count then
      StringGrid1.Cells[Col, Row] := Line[Col];
end;

end.

