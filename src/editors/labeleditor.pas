{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       labeleditor
 Description:  a borderless form for entering a map label's text
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit labeleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TLabelEditorForm }

  TLabelEditorForm = class(TForm)
    Edit1: TEdit;
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

//var
//  LabelEditorForm: TLabelEditorForm;

implementation

{$R *.lfm}

{ TLabelEditorForm }

procedure TLabelEditorForm.FormCreate(Sender: TObject);
begin
  Edit1.Left := 0;
  Edit1.Top := 0;
  ClientWidth := Edit1.Width;
  ClientHeight := Edit1.Height;
end;

procedure TLabelEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TLabelEditorForm.Edit1KeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    ModalResult := mrOK;
  end;
  if Key = #27 then
  begin
    Key := #0;
    ModalResult := mrCancel;
  end;
end;

end.

