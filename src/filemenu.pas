{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       filemenu
 Description:  a form that presents file-related menu choices
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

{
The form consists of a left and right panel. The left panel contains
a MenuListBox that lists menu options. The right panel contains a
Notebook control with two pages. Page1 contains a FilesListBox that
displays a list of most recently used project files. Page2 lists
import options and is only displayed when the Import option is
selected from the MenuListBox.
}

unit filemenu;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLtype, ComCtrls, Menus, Types;

type

  { TFileMenuForm }

  TFileMenuForm = class(TForm)
    Bevel1: TBevel;
    OfficeImageList: TImageList;
    MaterialImageList: TImageList;
    ImportImageList: TImageList;
    MenuListBox: TListBox;
    FilesListBox: TListBox;
    ImportListBox: TListBox;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure MenuListBoxClick(Sender: TObject);
    procedure MenuListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure MenuListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure MenuListBoxMouseEnter(Sender: TObject);
    procedure MenuListBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MenuListBoxShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure FilesListBoxClick(Sender: TObject);
    procedure FilesListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FilesListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FilesListBoxMouseEnter(Sender: TObject);
    procedure FilesListBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FilesListBoxShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure ImportListBoxClick(Sender: TObject);
    procedure ImportListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ImportListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure ImportListBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel1Click(Sender: TObject);

  private
    IconImageList: TImageList;
    procedure HideForm;
    procedure ShowRecentProjects;
    procedure ShowImportMenu;

  public
    procedure SetIconType(IconType: String);
  end;

var
  FileMenuForm: TFileMenuForm;

implementation

{$R *.lfm}

uses
  main, config, utils;

const
  MenuHints: array[0..7] of String =
    ('',
     #13'  Start a new project. '#13,
     #13'  Open a previously saved project.  '#13,
     #13'  Save the project under its current name.  '#13,
     #13'  Save the project under a new name.  '#13,
     #13'  Import project data in other formats.  '#13,
     #13'  Configure program settings.  '#13,
     #13'  Exit the program.  '#13);

{ TFileMenuForm }

procedure TFileMenuForm.FormCreate(Sender: TObject);
begin
  IconImageList := MaterialImageList;
  Color := config.ThemeColor;
  Font.Size := config.FontSize;
end;

procedure TFileMenuForm.FormShow(Sender: TObject);
var
  I: Integer;
begin
  Color := MainForm.Color;
  FilesListBox.Clear;
  if MainForm.MRUMenuMgr.Recent.Count > 0 then
  begin
    FilesListBox.Enabled := True;
    for I := 0 to MainForm.MRUMenuMgr.Recent.Count-1 do
      FilesListBox.Items.Add(Format('%0:d.  %1:s',
        [I+1, ExtractFileName(MainForm.MRUMenuMgr.Recent[I])]));
    FilesListBox.ItemIndex := 0;
  end else FilesListBox.Enabled := False;
  ImportListBox.ItemIndex := 0;
  MenuListBox.ItemIndex := 0;
  MenuListBox.SetFocus;
end;

procedure TFileMenuForm.MenuListBoxClick(Sender: TObject);
begin
  if MenuListBox.ItemIndex < 0 then exit;

  // Import option selected - show the import menu
  if SameText(MenuListBox.GetSelectedText, 'Import') then
  begin
    ShowImportMenu;
    exit;
  end;

  // Return focus to MainForm
  HideForm;
  MainForm.SetFocus;

  // Implement the selected option
  case MenuListBox.ItemIndex of
  0: HideForm;
  1: MainForm.FileNew(True);
  2: MainForm.FileOpen;
  3: MainForm.FileSave;
  4: MainForm.FileSaveAs;
  6: MainForm.FileConfigure;
  7: MainForm.FileQuit;
  end;
end;

procedure TFileMenuForm.MenuListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
//
//  Draw an item (with icon image) on the Canvas of the MenuListBox
//
var
  BrushColor: TColor;
  theCanvas: TCanvas;
  Yoffset: Integer;
  S: String;
begin
  BrushColor := MenuListBox.Color;
  if (Control = ActiveControl) and (odSelected in State) then
    BrushColor := clGradientActiveCaption;
  theCanvas := (Control as TListBox).Canvas;
  theCanvas.Brush.Color := BrushColor;
  theCanvas.FillRect(ARect);
  Yoffset := (ARect.Height - IconImageList.Height) div 2;
  IconImageList.Draw(theCanvas, ARect.Left + 8, ARect.Top+Yoffset, Index);
  theCanvas.Font.Color := clBlack;
  S := MenuListBox.Items[Index];
  Yoffset := (ARect.Height - theCanvas.TextHeight(S)) div 2;
  theCanvas.TextOut(ARect.Left+50, ARect.Top+Yoffset, S);
  Yoffset := (ARect.Height - theCanvas.TextHeight('>')) div 2;
end;

procedure TFileMenuForm.MenuListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then MenuListBoxClick(Sender);
  if (Key = VK_RIGHT) and
    (SameText(MenuListBox.GetSelectedText, 'Import')) then ShowImportMenu;
end;

procedure TFileMenuForm.MenuListBoxMouseEnter(Sender: TObject);
begin
  ShowRecentProjects;
  MenuListBox.SetFocus;
end;

procedure TFileMenuForm.MenuListBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  Index := MenuListBox.GetIndexAtXY(X, Y);
  MenuListBox.ItemIndex := Index;
end;

procedure TFileMenuForm.MenuListBoxShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  Index: Integer;
begin
  if (HintInfo^.HintControl = MenuListBox) and
     (MenuListBox.ItemAtPos(HintInfo^.CursorPos, True) > -1) then
  begin
    Index := MenuListBox.ItemAtPos(HintInfo^.CursorPos, True);
    HintInfo^.HintStr := MenuHints[Index];
    HintInfo^.CursorRect := MenuListBox.ClientRect;
  end;
end;

procedure TFileMenuForm.FilesListBoxClick(Sender: TObject);
var
  FileName: String;
begin
  if FilesListBox.ItemIndex < 0 then exit;
  Hide;
  MainForm.SetFocus;
  FileName := MainForm.MRUMenuMgr.Recent[FilesListBox.ItemIndex];
  MainForm.MRUMenuMgrRecentFile(Sender, FileName);
end;

procedure TFileMenuForm.FilesListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  BrushColor: TColor;
  C: TCanvas;
begin
  BrushColor := FilesListBox.Color;
  if (Control = ActiveControl) and (odSelected in State) then
    BrushColor := $00FFE8CD; //clGradientActiveCaption;
  if Control is TListBox then
  begin
    C := (Control As TListBox).Canvas;
    C.Brush.Color := BrushColor;
    C.FillRect(ARect);
    C.Font.Color := clBlack;
    C.TextOut(ARect.Left+2, ARect.Top+4, FilesListBox.Items[Index]);
  end;
end;

procedure TFileMenuForm.FilesListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_LEFT then MenuListBox.SetFocus;
  if Key = VK_RETURN then FilesListBoxClick(Sender);
end;

procedure TFileMenuForm.FilesListBoxMouseEnter(Sender: TObject);
begin
  FilesListBox.SetFocus;
end;

procedure TFileMenuForm.FilesListBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  Index := FilesListBox.GetIndexAtXY(X, Y);
  FilesListBox.ItemIndex := Index;
end;

procedure TFileMenuForm.FilesListBoxShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  Index: Integer;
begin
  if (HintInfo^.HintControl = FilesListBox) and
     (FilesListBox.ItemAtPos(HintInfo^.CursorPos, True) > -1) then
  begin
    Index := FilesListBox.ItemAtPos(HintInfo^.CursorPos, True);
    HintInfo^.HintStr := MainForm.MRUMenuMgr.Recent[Index];
    HintInfo^.CursorRect := FilesListBox.ItemRect(Index);
  end;
end;

procedure TFileMenuForm.ImportListBoxClick(Sender: TObject);
begin
  if ImportListBox.ItemIndex = 2 then
  begin
    utils.MsgDlg('This feature is not yet available', mtInformation, [mbOk]);
    exit;
  end;
  HideForm;
  if ImportListBox.ItemIndex = 0 then MainForm.FileImport('shp');
  if ImportListBox.ItemIndex = 1 then MainForm.FileImport('dxf');
end;

procedure TFileMenuForm.ImportListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  BrushColor: TColor;
  C: TCanvas;
begin
  BrushColor := ImportListBox.Color;
  if (Control = ActiveControl) and (odSelected in State) then
    BrushColor := $00FFE8CD;  //clGradientActiveCaption;
  if Control is TListBox then
  begin
    C := (Control as TListBox).Canvas;
    C.Brush.Color := BrushColor;
    C.FillRect(ARect);
    ImportImageList.Draw(C, ARect.Left + 8, ARect.Top+16, Index);
    C.Font.Color := clBlack;
    C.TextOut(ARect.Left+48, ARect.Top+24, ImportListBox.Items[Index]);
  end;
end;

procedure TFileMenuForm.ImportListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_LEFT then ShowRecentProjects;
  if Key = VK_RETURN then ImportListBoxClick(Sender);
end;

procedure TFileMenuForm.ImportListBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  Index := ImportListBox.GetIndexAtXY(X, Y);
  ImportListBox.ItemIndex := Index;
end;

procedure TFileMenuForm.Panel1Click(Sender: TObject);
begin
  HideForm;
end;

procedure TFileMenuForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    if Notebook1.PageIndex = 1 then
      ShowRecentProjects
    else
      HideForm;
  end;
end;

procedure TFileMenuForm.FormDeactivate(Sender: TObject);
begin
  HideForm;
end;

procedure TFilemenuForm.HideForm;
begin
  MainForm.MainMenuFrame.SelectProjectMenu;
  Hide;
end;

procedure TFilemenuForm.ShowRecentProjects;
begin
  Panel4.Caption := ' Recent Projects';
  Notebook1.PageIndex := 0;
  MenuListBox.SetFocus;
end;

procedure TFilemenuForm.ShowImportMenu;
begin
  Panel4.Caption := ' Import Network Data From';
  Notebook1.PageIndex := 1;
  ImportListBox.SetFocus;
end;

procedure TFileMenuForm.SetIconType(IconType: String);
begin
  if SameText(IconType, 'Material') then IconImageList := MaterialImageList;
  if SameText(IconType, 'Office') then IconImageList := OfficeImageList;
end;

end.

