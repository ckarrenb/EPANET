{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       pumpingrpt
 Description:  a frame that displays a pumping report
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit pumpingrpt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Grids, Buttons,
  Graphics, Menus, Clipbrd, Math;

const
  ColHeading: array[0..6] of String =
    ('Pump', '% Utilized', 'Efficiency', '', 'Avg. Kw', 'Peak Kw', 'Cost/day');
  TXT_DMND_CHARGE = 'Demand Charge: ';
  TXT_TOTAL_COST = 'Total Cost: ';
  TXT_KW_HRS_PER_M3 = 'Kw-hrs/m3';
  TXT_KW_HRS_PER_MGAL = 'Kw-hrs/Mgal';

type

  { TPumpingRptFrame }

  TPumpingRptFrame = class(TFrame)
    ExportMenu: TPopupMenu;
    Label1: TLabel;
    MnuCopy: TMenuItem;
    MnuSave: TMenuItem;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    procedure CloseBtnClick(Sender: TObject);
    procedure MnuCopyClick(Sender: TObject);
    procedure StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    TotalCost: Single;
    DmndCharge: Single;
    procedure RefreshTable;
    procedure GetReportContents(Slist: TStringList);

  public
    procedure InitReport;
    procedure CloseReport;
    procedure ClearReport;
    procedure RefreshReport;
    procedure ShowPopupMenu;

  end;

implementation

{$R *.lfm}

uses
  main, project, config, results, utils;

procedure TPumpingRptFrame.StringGrid1CompareCells(Sender: TObject; ACol, ARow,
  BCol, BRow: Integer; var Result: integer);
var
  F1, F2: Extended;
begin
  Result := 0;
  with StringGrid1 do
  begin
    if Acol = 0 then Result := CompareText(Cells[ACol, ARow], Cells[BCol, BRow])
    else
      if TryStrToFloat(StringGrid1.Cells[ACol, ARow], F1) and
           TryStrToFloat(StringGrid1.Cells[BCol, BRow], F2) then
        Result := Math.CompareValue(F1, F2);
    if SortOrder = soDescending then Result := -Result;
  end;
end;

procedure TPumpingRptFrame.CloseBtnClick(Sender: TObject);
begin
   MainForm.ReportFrame.CloseReport;
end;

procedure TPumpingRptFrame.ShowPopupMenu;
var
  P : TPoint;
begin
  P := Self.ClientToScreen(Point(0, 0));
  ExportMenu.PopUp(P.x,P.y);
end;

procedure TPumpingRptFrame.MnuCopyClick(Sender: TObject);
var
  Slist: TStringList;
begin
  Slist := TStringList.Create;
  try
    GetReportContents(Slist);
    Clipboard.AsText := Slist.Text;
  finally
    Slist.Free;
  end;
end;

procedure TPumpingRptFrame.StringGrid1PrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  MyTextStyle: TTextStyle;
begin
  if aRow = 0 then
  begin
    MyTextStyle := StringGrid1.Canvas.TextStyle;
    if aCol > 0 then MyTextStyle.Alignment := taCenter;
    MyTextStyle.SystemFont := true;
    StringGrid1.Canvas.TextStyle := MyTextStyle;
  end
  else if aCol > 0 then
  begin
    MyTextStyle := StringGrid1.Canvas.TextStyle;
    MyTextStyle.Alignment := taRightJustify;
    StringGrid1.Canvas.TextStyle := MyTextStyle;
  end;
end;

procedure TPumpingRptFrame.InitReport;
var
  I: Integer;
begin
  with StringGrid1 do
  begin
    AlternateColor := config.AlternateColor;
    ColWidths[0] := 128;
    for I := 0 to ColCount - 1 do
      Cells[I,0] := ColHeading[I];
  end;
end;

procedure TPumpingRptFrame.ClearReport;
begin

end;

procedure TPumpingRptFrame.CloseReport;
begin
  StringGrid1.Clear;
end;

procedure TPumpingRptFrame.RefreshReport;
var
  KwHrsPerFlow: string;
begin
  if Project.GetUnitsSystem = 0 then
    KwHrsPerFlow := TXT_KW_HRS_PER_MGAL
  else
     KwHrsPerFlow := TXT_KW_HRS_PER_M3;
  StringGrid1.Cells[3, 0] := KwHrsPerFlow;
  RefreshTable;
end;

procedure TPumpingRptFrame.RefreshTable;
var
  I, J, K, L, N: Integer;
  X: array[0..5] of Single;  // Holds a pump's energy usage results
begin
  TotalCost := 0;
  DmndCharge := 0;
  J := 0;
  N := Project.GetPumpCount;
  StringGrid1.RowCount := N + 1;
  for I := 0 to 5 do X[I] := 0;

  // Examine each link
  for I := 1 to Project.GetItemCount(cLinks) do
  begin

    // Skip over non-pump links
    if Project.GetLinkType(I) <> lPump then continue;

    // Column 0 contains pump ID
    Inc(J);
    StringGrid1.Cells[0, J] := Project.GetID(cLinks, I);

    // Place energy usage results into columns 1 to 6
    L := Project.GetResultIndex(cLinks, I);
    if Results.GetPumpEnergy(L, X) then
    begin
      TotalCost := TotalCost + X[5];
      for K := 1 to 6 do
        StringGrid1.Cells[K, J] := Utils.Float2Str(X[K-1], 2);
    end
    else for K := 1 to 6 do
    begin
      StringGrid1.Cells[K, J] := 'N/A';
    end;
  end;
  DmndCharge := Results.GetPumpDemandCharge;
  Panel1.Caption := '  ' + TXT_TOTAL_COST + '  ' + Utils.Float2Str(TotalCost, 2) +
                    '  ' + TXT_DMND_CHARGE + '  ' + Utils.Float2Str(DmndCharge, 2);
end;

procedure TPumpingRptFrame.GetReportContents(Slist: TStringList);
//
//  Transfer the contents of the report to a StringList.
//
var
  I, J: Integer;
  S: string;
begin
  with StringGrid1 do
  begin
    S := Project.GetTitle(0);
    Slist.Add(S);
    S := 'Pumping Report';
    Slist.Add(S);
    Slist.Add('');
    for I := 0 to StringGrid1.RowCount - 1 do
    begin
      S := Format('%-20s', [Cells[0, I]]);
      for J := 1 to ColCount-1 do
        S := S + Format('%20s', [Cells[J, I]]);
      Slist.Add(S);
    end;
  end;
  Slist.Add('');
  Slist.Add(TXT_TOTAL_COST + Format('%.2f', [TotalCost]));
  Slist.Add(TXT_DMND_CHARGE + Format('%.2f', [DmndCharge]));
end;

end.

