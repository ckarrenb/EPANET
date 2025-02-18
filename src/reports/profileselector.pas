{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       profileselector
 Description:  A frame used to select network links to include in
               a hydraulic profile plot
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit profileselector;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Dialogs;

type

  { TProfileSelectorFrame }

  TProfileSelectorFrame = class(TFrame)
    AddBtn: TBitBtn;
    CancelBtn: TButton;
    CloseBtn: TSpeedButton;
    DeleteBtn: TBitBtn;
    DnBtn: TBitBtn;
    ClearBtn: TBitBtn;
    LoadBtn: TBitBtn;
    SaveBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    TopPanel: TPanel;
    UpBtn: TBitBtn;
    ViewBtn: TButton;
    procedure AddBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure DnBtnClick(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure UpBtnClick(Sender: TObject);
    procedure ViewBtnClick(Sender: TObject);
  private
    procedure SetActionButtons;

  public
    procedure Init(LinksList: TStringList);
    procedure AddLink;

  end;

implementation

{$R *.lfm}

uses
  main, project, config, utils, hydprofilerpt;

{ TProfileSelectorFrame }

procedure TProfileSelectorFrame.Init(LinksList: TStringList);
begin
  TopPanel.Color := config.ThemeColor;
  ListBox1.Clear;
  ListBox1.Items.Assign(LinksList);
  if ListBox1.Count > 1 then ListBox1.ItemIndex := 0;
  SetActionButtons;
end;

procedure TProfileSelectorFrame.AddLink;
begin
  AddBtnClick(self);
end;

procedure TProfileSelectorFrame.CloseBtnClick(Sender: TObject);
begin
  CancelBtnClick(Sender);
end;

procedure TProfileSelectorFrame.AddBtnClick(Sender: TObject);
var
  I, J, N1, N2: Integer;
  S: String;
begin
  I := MainForm.ProjectFrame.CurrentCategory;
  if I <> cLinks then
  begin
    utils.MsgDlg('Selected object must be a Link.', mtInformation, [mbOK], MainForm);
    exit;
  end;
  J := MainForm.ProjectFrame.SelectedItem[I];
  S := Project.GetItemId(I, J);

  if ListBox1.Items.IndexOf(S) >=0 then
  begin
    utils.MsgDlg('Selected object is already in profile path.',
      mtInformation, [mbOK], MainForm);
    exit;
  end;

  ListBox1.Items.Add(S);
  ListBox1.ItemIndex := ListBox1.Count - 1;
  SetActionButtons;
end;

procedure TProfileSelectorFrame.CancelBtnClick(Sender: TObject);
begin
  ListBox1.Clear;
  Visible := False;
end;

procedure TProfileSelectorFrame.ClearBtnClick(Sender: TObject);
begin
  ListBox1.Clear;
  SetActionButtons;
end;

procedure TProfileSelectorFrame.DeleteBtnClick(Sender: TObject);
var
  I: Integer;
begin
  I := ListBox1.ItemIndex;
  ListBox1.Items.Delete(I);
  SetActionButtons;
end;

procedure TProfileSelectorFrame.DnBtnClick(Sender: TObject);
var
  I, Max: Integer;
begin
  Max := ListBox1.Items.Count;
  if Max > 0 then
  begin
    Dec(Max);
    I := ListBox1.ItemIndex;
    if I < Max then
    begin
      ListBox1.Items.Exchange(I, I + 1);
      ListBox1.Selected[I+1]:= True;
    end;
  end;
end;

procedure TProfileSelectorFrame.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  SetActionButtons;
end;

procedure TProfileSelectorFrame.UpBtnClick(Sender: TObject);
var
  I: Integer;
begin
  I := ListBox1.ItemIndex;
  if I > 0 then begin
    ListBox1.Items.Exchange(I, I - 1);
    ListBox1.Selected[I-1]:= True;
  end;
end;

procedure TProfileSelectorFrame.ViewBtnClick(Sender: TObject);
begin
  Visible := False;
  with MainForm.ReportFrame.Report as THydProfileFrame do
    SetProfileLinks(ListBox1.Items);

end;

procedure TProfileSelectorFrame.SetActionButtons;
var
  N: Integer;
begin
  N := ListBox1.Count;
  ClearBtn.Enabled := N > 0;
  DeleteBtn.Enabled := N > 0;
  UpBtn.Enabled := (N > 1) and (ListBox1.ItemIndex > 0);
  DnBtn.Enabled := (N > 1) and (ListBox1.ItemIndex < N-1);
end;

end.

