{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       statusrpt
 Description:  a frame that displays a status report
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit statusrpt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, StrUtils,
  Clipbrd, Menus;

type

  { TStatusRptFrame }

  TStatusRptFrame = class(TFrame)
    Memo1: TMemo;
    MnuSave: TMenuItem;
    MnuCopy: TMenuItem;
    ExportMenu: TPopupMenu;
    procedure CloseBtnClick(Sender: TObject);
    procedure MnuCopyClick(Sender: TObject);
    procedure MnuSaveClick(Sender: TObject);
  private
    function FindText(Txt: String; StartPos: SizeUint): Integer;

  public
    procedure ClearReport;
    procedure RefreshReport;
    procedure ShowPopupMenu;

  end;

implementation

{$R *.lfm}

uses
  project, main;

procedure TStatusRptFrame.ClearReport;
begin
  Memo1.Clear;
end;

procedure TStatusRptFrame.RefreshReport;
begin
  with Memo1 do
  begin
    Clear;
    if FileExists(Project.AuxFile) then
    begin
      Lines.LoadFromFile(Project.AuxFile);
      if Project.RunStatus = rsWarning then FindText('WARNING:', 1)
      else Memo1.SelStart := 0;
    end;
  end;
end;

procedure TStatusRptFrame.CloseBtnClick(Sender: TObject);
begin
  MainForm.ReportFrame.CloseReport;
end;

procedure TStatusRptFrame.ShowPopupMenu;
var
  P : TPoint;
begin
  P := Self.ClientToScreen(Point(0, 0));
  ExportMenu.PopUp(P.x,P.y);
end;

procedure TStatusRptFrame.MnuCopyClick(Sender: TObject);
begin
  Memo1.SelectAll;
  Memo1.CopyToClipboard;
  Memo1.SelLength := 0;
end;

procedure TStatusRptFrame.MnuSaveClick(Sender: TObject);
begin
  with MainForm.SaveDialog1 do
  begin
    FileName := '*.txt';
    Filter := 'Text File|*.txt|All Files|*.*';
    DefaultExt := '*.txt';
    if Execute then Memo1.Lines.SaveToFile(FileName);
  end;
end;

function TStatusRptFrame.FindText(Txt: String; StartPos: SizeUint): Integer;
begin
  Result := PosEx(Txt, Memo1.Text, StartPos);
  if Result > 0 then
  begin
    Memo1.SelStart := Result - 1;
    Memo1.SelLength := Length(Txt);
//    Memo1.SetFocus;
  end;
end;

end.

