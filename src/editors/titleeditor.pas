{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       titleeditor
 Description:  a dialog form that edits a project's title
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit titleeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TTitleEditorForm }

  TTitleEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    HasChanged: Boolean;

  end;

var
  TitleEditorForm: TTitleEditorForm;

implementation

{$R *.lfm}

uses
  project, config;

{ TTitleEditorForm }

procedure TTitleEditorForm.FormCreate(Sender: TObject);
begin
  Color := config.ThemeColor;
  Font.Size := config.FontSize;
end;

procedure TTitleEditorForm.FormShow(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to 3 do
  begin
    with findComponent('Edit' + IntToStr(I)) as TEdit do
    begin
      Text := project.GetTitle(I-1);
    end;
  end;
  HasChanged := False;
end;

procedure TTitleEditorForm.Button1Click(Sender: TObject);
var
  I: Integer;
  Lines: array[1..3] of String;
begin
  for I := 1 to 3 do
  begin
    with findComponent('Edit' + IntToStr(I)) as TEdit do
    begin
      Lines[I] := Text;
    end;
  end;
  Project.SetTitle(Lines[1], Lines[2], Lines[3]);
end;

procedure TTitleEditorForm.Edit1Change(Sender: TObject);
begin
  HasChanged := True;
end;

end.

