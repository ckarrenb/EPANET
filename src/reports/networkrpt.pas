{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       networkrpt
 Description:  A frame that displays a table of computed results
               for all network nodes or links
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit networkrpt;

{  This file defines a Frame that displays simulation results for all
   network nodes or links in a table that can be sorted and filtered.

   A TNotebook has a TablePage to display the results in a TDrawGrid
   and a FilterPage to define filters to limit the results shown in
   the TablePage.
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Grids, ComCtrls, Menus, Types, LCLtype, Clipbrd, Fgl;

type
TFilter = record
  Param: Integer;
  Relation: Integer;
  Value: Single;
  Text: string;
end;

  TIntegerList = specialize TFPGList<Integer>;  // an integer list

  { TNetworkRptFrame }

  TNetworkRptFrame = class(TFrame)
    ParamCheckGroup: TCheckGroup;
    DrawGrid1: TDrawGrid;
    ExportToClipboard: TMenuItem;
    ExportToFile: TMenuItem;
    FilterPage: TPage;
    FiltersAcceptBtn: TButton;
    FiltersAddBtn: TButton;
    FiltersCancelBtn: TButton;
    FiltersDeleteBtn: TButton;
    FiltersListBox: TListBox;
    GroupBox1: TGroupBox;
    MenuSave: TMenuItem;
    MnuFilters: TMenuItem;
    MenuCopy: TMenuItem;
    Notebook1: TNotebook;
    BottomPanel: TPanel;
    ParamComboBox: TComboBox;
    ParamValueEdit: TEdit;
    PopupMenu1: TPopupMenu;
    RelationComboBox: TComboBox;
    TablePage: TPage;
    procedure CloseBtnClick(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure DrawGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure FiltersAcceptBtnClick(Sender: TObject);
    procedure FiltersAddBtnClick(Sender: TObject);
    procedure FiltersCancelBtnClick(Sender: TObject);
    procedure FiltersDeleteBtnClick(Sender: TObject);
    procedure FiltersListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure MenuCopyClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MnuFiltersClick(Sender: TObject);

  private
    procedure SetupTable;
    function  GetTableCellValue(C: LongInt; R: LongInt): String;
    function  SetFilter(I: Integer; X: Single): String;
    function  Filtered(Index: Integer): Boolean;
    procedure GetDrawGridContents(Slist: TStringList);
    procedure RefreshGrid;

  public
    procedure InitReport(aReportType: Integer);
    procedure CloseReport;
    procedure ClearReport;
    procedure RefreshReport;
    procedure ShowPopupMenu;

  end;

implementation

{$R *.lfm}

uses
  project, main, mapthemes, results, config, utils;

const
  rtBelow = 0;
  rtEqual = 1;
  reAbove = 2;

  SortCaption = 'Click a column header to sort it.';

var
  TimePeriod: Integer;      // Time period being viewed
  TableType: Integer;       // Either Nodes or Links
  IndexList: TIntegerList;  // List of table indices for nodes/links
  SortIndex: Integer;       // Index of column to sort on
  SortOrder: TSortOrder;    // Either ascending or descending
  NumFilters: Integer;      // Number of table filter conditions
  NumTmpFilters: Integer;   // Number of temporary filter conditions
  Filters: array[0..4] of TFilter;    // Table filter conditions
  TmpFilters: array[0..4] of TFilter; // Temporary filter conditions

function Compare(const Index1: LongInt; const Index2: LongInt): LongInt;
//
//  Comparison function used for sorting items in the TablePage.
//
var
  X1, X2: Single;
begin
  Result := 0;
  if TableType = cNodes then
  begin
    X1 := MapThemes.GetNodeValue(Index1, SortIndex, TimePeriod);
    X2 := MapThemes.GetNodeValue(Index2, SortIndex, TimePeriod);
  end
  else begin
    X1 := MapThemes.GetLinkValue(Index1, SortIndex, TimePeriod);
    X2 := MapThemes.GetLinkValue(Index2, SortIndex, TimePeriod);
  end;
  if X1 < X2 then Result := -1
  else if X1 > X2 then Result := 1;
  if SortOrder = soDescending then Result := -Result;
end;

procedure TNetworkRptFrame.InitReport(aReportType: Integer);
//
//  Initializes the report frame.
//
begin
  DrawGrid1.AlternateColor := config.AlternateColor;
  TableType := cNodes;
  if aReportType = cLinks then TableType := cLinks;
  TimePeriod := mapthemes.TimePeriod;
  SortIndex := 0;
  NumFilters := 0;
  Notebook1.PageIndex := 0;
  IndexList := TIntegerList.Create;
//  SetupTable;
end;

procedure TNetworkRptFrame.CloseReport;
//
//  Closes the report frame.
//
begin
  ClearReport;
  IndexList.Free;
end;

procedure TNetworkRptFrame.ClearReport;
begin
  DrawGrid1.Clear;
  ParamCheckGroup.Items.Clear;
  ParamComboBox.Items.Clear;
  FiltersListBox.Items.Clear;
  IndexList.Clear;
  SortIndex := 0;
  NumFilters := 0;
end;

procedure TNetworkRptFrame.RefreshReport;
//
//  Refreshes the contents of the report frame.
//
var
  I: Integer;
  S: String;
begin
  // Setup the grid to display result themes in columns
  ClearReport;
  SetupTable;

  // Add a header row to the grid
  DrawGrid1.RowCount := 1;
  DrawGrid1.RowHeights[0] := 2 * DrawGrid1.DefaultRowHeight;

  // Set time period to display
  TimePeriod := mapthemes.TimePeriod;

  // Set caption of report's top panel
  if TableType = cNodes then S := 'Node Results'
  else S := 'Link Results';
  if results.Nperiods > 1 then
    S := S + ' at ' + results.GetTimeStr(TimePeriod) + ' hrs';
  MainForm.ReportFrame.TopPanel.Caption := S;

  // Display network results at specified time period
  RefreshGrid;
end;

procedure TNetworkRptFrame.RefreshGrid;
//
//  Refreshes the contents of the table grid.
//
var
  I: Integer;
  S: String;
begin
  // Set visibility of grid columns & their header height
  for I := 0 to DrawGrid1.Columns.Count - 1 do
    DrawGrid1.Columns[I].Visible := ParamCheckGroup.Checked[I];

  // Add filtered results to the grid
  IndexList.Clear;
  DrawGrid1.BeginUpdate;
  for I := 1 to project.GetItemCount(TableType) do
    if Filtered(I) then IndexList.Add(I);
  DrawGrid1.RowCount := IndexList.Count + 1;

  // Sort the grid if called for
  if (SortIndex > 0) then IndexList.Sort(@Compare);
  DrawGrid1.EndUpdate(true);

  // Display number of table entries
  if NumFilters = 0 then S := ' Unfiltered: '
  else S := ' Filtered: ';
  BottomPanel.Caption := S + IntToStr(DrawGrid1.RowCount - 1) + ' items.  ' +
    SortCaption;
end;

procedure TNetworkRptFrame.SetupTable;
//
//  Set up the TablePage to display either nodes or links.
//
var
  I, ThemeCount: Integer;
  ParamName, ParamStr: string;
begin
  // Add a column for each theme to the table
  DrawGrid1.Columns.Clear;
  if TableType = cNodes then ThemeCount := NodeThemeCount - 1
  else ThemeCount := LinkThemeCount - 1;
  for I := 1 to ThemeCount do DrawGrid1.Columns.Add;

  // Assign header names to each of the  columns
  for I := 1 to ThemeCount do
  begin
    if TableType = cNodes then
    begin
      ParamName := mapthemes.NodeThemes[I].Name;
      ParamStr := ParamName+ #10 + mapthemes.GetThemeUnits(cNodes, I)
    end else
    begin
      ParamName := mapthemes.LinkThemes[I].Name;
      ParamStr := ParamName + #10 + mapthemes.GetThemeUnits(cLinks, I);
    end;
    ParamComboBox.Items.Add(ParamName);
    ParamCheckGroup.Items.Add(ParamName);
    DrawGrid1.Columns[I-1].Title.Caption := ParamStr;
  end;
  ParamComboBox.ItemIndex := 0;

  // Select which parameters to display initially
  for I := 0 to ThemeCount - 1 do
  begin
    ParamCheckGroup.Checked[I] := True;
    if (TableType = cNodes) and
      (I+1 in [ntElevation, ntBaseDemand, ntEmittance, ntLeakage]) then
        ParamCheckGroup.Checked[I] := False
    else if (TableType = cLinks) and
      (I+1 in [ltDiameter, ltLength, ltRoughness]) then
        ParamCheckGroup.Checked[I] := False;
  end;
end;

procedure TNetworkRptFrame.DrawGrid1PrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
//
//  Set up the format used to display text in the TabelPage's DrawGrid.
//
var
  MyTextStyle: TTextStyle;
begin
  MyTextStyle := DrawGrid1.Canvas.TextStyle;
  if aRow = 0 then
  begin
    MyTextStyle.SingleLine := false;
    MyTextStyle.Alignment := taCenter;
    DrawGrid1.Canvas.TextStyle := MyTextStyle;
  end
  else if aCol > 0 then
  begin
    MyTextStyle.Alignment := taRightJustify;
    DrawGrid1.Canvas.TextStyle := MyTextStyle;
  end;
end;

procedure TNetworkRptFrame.FiltersAcceptBtnClick(Sender: TObject);
//
//  OnClick handler for the Accept button on the Filters dialog page.
//
//  Transfer the filters defined on the FilterPage to the actual filters
//  used to display results on the TablePage.
//
var
  I: Integer;
begin
  NumFilters := NumTmpFilters;
  for I := 0 to NumFilters - 1 do
    Filters[I] := TmpFilters[I];
  NoteBook1.PageIndex := 0;
  RefreshGrid;
end;

procedure TNetworkRptFrame.FiltersAddBtnClick(Sender: TObject);
//
//  OnClick handler for the Add button on the Filters dialog page.
//
//  Add the filter entered into the FilterPage's controls to the list
//  of filters.
//
var
  S: string;
  X: Single = 0;
begin
  if Utils.Str2Float(ParamValueEdit.Text, X) then
  begin
    S := SetFilter(NumTmpFilters, X);
    FiltersListBox.Items.Add(S);
    FiltersListBox.ItemIndex := FiltersListBox.Count - 1;
    FiltersAddBtn.Enabled := FiltersListBox.Count < Length(Filters);
    FiltersDeleteBtn.Enabled := true;
    Inc(NumTmpFilters);
    ParamComboBox.SetFocus;
  end else
    Utils.MsgDlg(ParamValueEdit.Text + ' is not a valid number.', mtError, [mbOk]);
end;

procedure TNetworkRptFrame.FiltersCancelBtnClick(Sender: TObject);
//
//  OnClick handler for the Cancel button on the FilterPage.
//
//  Exit the FilterPage and return to the TablePage.
//
begin
  Notebook1.PageIndex := 0;
end;

procedure TNetworkRptFrame.FiltersDeleteBtnClick(Sender: TObject);
//
//  OnClick handler the the Delete button on the FilterPage.
//
//  Remove the selected filter from the filters list.
//
  var
    I, J: Integer;
  begin
    I := FiltersListBox.ItemIndex;
    if I < NumTmpFilters - 1 then
      for J := I to NumTmpFilters - 2 do
        TmpFilters[J] := TmpFilters[J+1];
    FiltersListBox.Items.Delete(I);
    Dec(NumTmpFilters);
    if I > 0 then
      Dec(I);
    if FiltersListBox.Count = 0 then
    begin
      FiltersDeleteBtn.Enabled := false;
    end
    else begin
      FiltersListBox.ItemIndex := I;
    end;
end;

procedure TNetworkRptFrame.FiltersListBoxSelectionChange(Sender: TObject;
  User: boolean);
//
//  OnSelectionChange handler for the FilterPage's list box.
//
//  Place the selected filter's parameters into the page's editing controls.
//
var
  I: Integer;
begin
  I := FiltersListBox.ItemIndex;
  ParamComboBox.ItemIndex := TmpFilters[I].Param ;
  RelationComboBox.ItemIndex := TmpFilters[I].Relation;
  ParamValueEdit.Text := FloatToStr(TmpFilters[I].Value);
end;

procedure TNetworkRptFrame.ShowPopupMenu;
//
//  Display a popup menu with choices to set table filters or export the
//  table.
//
var
  P : TPoint;
begin
  P := Self.ClientToScreen(Point(0, 0));
  PopupMenu1.PopUp(P.x,P.y);
end;

procedure TNetworkRptFrame.MenuCopyClick(Sender: TObject);
//
//  OnClick handler for the menu item to copy the TablePage to the clipboard.
var
  Slist: TStringList;
begin
  Slist := TStringList.Create;
  try
    GetDrawGridContents(Slist);
    Clipboard.AsText := Slist.Text;
  finally
    Slist.Free;
  end;
end;

procedure TNetworkRptFrame.MenuSaveClick(Sender: TObject);
var
  Slist: TStringList;
begin
  with MainForm.SaveDialog1 do begin
    FileName := '*.txt';
    Filter := 'Text File|*.txt|All Files|*.*';
    DefaultExt := '*.txt';
    if Execute then
    begin
      Slist := TStringList.Create;
      try
        GetDrawGridContents(Slist);
        Slist.SaveToFile(FileName);
      finally
        Slist.Free;
      end;
    end;
  end;
end;

procedure TNetworkRptFrame.MnuFiltersClick(Sender: TObject);
//
//  OnClick hander for the menu item to set filters for the TabelPage.
//
//  Display the FilterPage of the frame's notebook.
//
var
  I: Integer;
  EnableBtns: Boolean = false;
begin
  FilterPage.Color:= config.ThemeColor;
  FiltersListBox.Clear;
  for I := 0 to High(TmpFilters) do
  begin
    TmpFilters[I].Param := -1;
    TmpFilters[I].Text := '';
  end;
  for I := 0 to NumFilters - 1 do
  begin
    FiltersListBox.Items.Add(Filters[I].Text);
    TmpFilters[I] := Filters[I];
  end;
  NumTmpFilters := NumFilters;

  if NumFilters > 0 then
  begin
    FiltersListBox.ItemIndex := 0;
    EnableBtns := true;
  end;
  FiltersDeleteBtn.Enabled := EnableBtns;
  if NumFilters > 0 then
     FiltersListBoxSelectionChange(Sender, false)
  else begin
    ParamComboBox.ItemIndex := 0;
    RelationComboBox.ItemIndex := 0;
    ParamValueEdit.Text := '';
  end;
  Notebook1.PageIndex := 1;
  ParamCheckGroup.SetFocus;
end;

procedure TNetworkRptFrame.DrawGrid1HeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
//
//  OnClick handler for the headers appearing on the TabelPage's DrawGrid.
//
//  Use the header click to indicate what parameter to sort the results by.
//
begin
  if IsColumn then
  begin
    SortIndex := Index;
    SortOrder := DrawGrid1.SortOrder;
    RefreshGrid;
  end;
end;

procedure TNetworkRptFrame.DrawGrid1DrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
//
//  OnDrawGridCell handler for the TablePage's DrawGrid.
//
//  Obtain the value to display in a cell of the DrawGrid.
//
var
  S: String;
  H: Integer;
begin
  S := GetTableCellValue(aCol, aRow);
  with Sender as TDrawGrid do
  begin
    if (aRow = 0) and (aCol = 0) then
    begin
      H := (aRect.Height - Canvas.TextHeight(S)) div 2;
      Canvas.TextRect(aRect, aRect.Left+2, aRect.Top + H, S)
    end
    else
      Canvas.TextRect(aRect, aRect.Left+2, aRect.Top+2, S);
  end;
end;

procedure TNetworkRptFrame.CloseBtnClick(Sender: TObject);
//
//  OnClick handler for the TopPanel's Close button.
//
begin
  MainForm.ReportFrame.CloseReport;
end;

function TNetworkRptFrame.GetTableCellValue(C: LongInt; R: LongInt): String;
//
//  Get the value of the parameter to display in a cell of the table.
//
var
  X: Single;
  Index: Integer = 0;
begin
  Result := '';
  if R >= 1 then Index := IndexList[R-1];
  if C = 0 then
  begin
    if TableType = cNodes then
    begin
      if R = 0 then
        Result := 'Node'
      else
        Result := Project.GetID(cNodes, Index);
    end
    else begin
      if R = 0 then  Result := 'Link'
      else Result := Project.GetID(cLinks, Index);
    end;
  end
  else if R > 0 then
  begin
    if TableType = cNodes then
      X := MapThemes.GetNodeValue(Index, C, TimePeriod)
    else
      X := MapThemes.GetLinkValue(Index, C, TimePeriod);
    if X = MISSING then Result := 'N/A  '
    else
      Result := FloatToStrF(X, ffFixed, 7, config.DecimalPlaces) + '  ';
  end;
end;

function TNetworkRptFrame.SetFilter(I: Integer; X: Single): String;
//
//  Transfer the entries in the FilterPage's editing controls to
//  a string representation of a filter.
//
begin
  Result := ParamComboBox.Text + ' '  + RelationComboBox.Text + ' ' +
            ParamValueEdit.Text;
  TmpFilters[I].Param:= ParamComboBox.ItemIndex;
  TmpFilters[I].Relation:= RelationComboBox.ItemIndex;
  TmpFilters[I].Value:= X;
  TmpFilters[I].Text := Result;
end;

function TNetworkRptFrame.Filtered(Index: Integer): Boolean;
//
//  Determine if the result for a given node or link meets the
//  filtering crieria or not.
//
var
  I: Integer;
  C: Integer;
  X, Y: Single;
begin
  if NumFilters = 0 then
    Result := true
  else
    Result := false;
  for I := 0 to NumFilters - 1 do
  begin
    C := Filters[I].Param + 1;
    Y := Filters[I].Value;
    if TableType = cNodes then
      X := MapThemes.GetNodeValue(Index, C, TimePeriod)
    else
    begin
      X := MapThemes.GetLinkValue(Index, C, TimePeriod);
      if C = ltFlow then X := Abs(X);
    end;
    case Filters[I].Relation of
      rtBelow: if X > Y then exit;
      rtEqual: if Abs(X - Y) > 0.001 then exit;
      reAbove: if X < Y then exit;
    end;
  end;
  Result := true;
end;

procedure TNetworkRptFrame.GetDrawGridContents(Slist: TStringList);
//
//  Transfer the contents of the TablePage's DrawGrid to a StringList.
//
var
  I, R: Integer;
  S, ColName: string;
begin
  with DrawGrid1 do
  begin
    // Add title lines to the Slist
    S := Project.GetTitle(0);
    Slist.Add(S);
    if TableType = cNodes then S := 'Network Nodes Report'
    else S := 'Network Links Report';
    S := S + ' at ' + Results.GetTimeStr(TimePeriod) + ' hrs';
    Slist.Add(S);
    Slist.Add('');

    // Add first line of each visible column
    S := '                    ';
    for I := 0 to Columns.Count - 1 do
    begin
      if not Columns[I].Visible then continue;
      if TableType = cNodes then
        ColName := mapthemes.NodeThemes[I+1].Name
      else
        ColName := mapthemes.LinkThemes[I+1].Name;
      S := S + Format('%20s', [ColName]);
    end;
    Slist.Add(S);

    // Add second line of each visible column
    S := Format('%-20s', [GetTableCellValue(0, 0)]);
    for I := 0 to Columns.Count - 1 do
    begin
      if not Columns[I].Visible then continue;
      S := S + Format('%20s', [mapthemes.GetThemeUnits(TableType, I+1)]);
    end;
    Slist.Add(S);

    // Add contents of each row
    for R := 1 to RowCount - 1 do
    begin
      S := Format('%-22s', [GetTableCellValue(0, R)]);
      for I := 1 to ColCount-1 do
        S := S + Format('%20s', [GetTableCellValue(I, R)]);
      Slist.Add(S);
    end;
  end;
end;

end.

