{====================================================================
 Project:      EPANET
 Version:      2.3
 Module:       mapexporter
 Description:  a frame that exports the pipe network map to the
               clipboard or to a file
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit mapexporter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Buttons;

type

  { TMapExporterFrame }

  TMapExporterFrame = class(TFrame)
    ExportBtn: TBitBtn;
    IncludeLgndCB: TCheckBox;
    CloseBtn: TSpeedButton;
    Label1: TLabel;
    ToClipbrdRB: TRadioButton;
    ToFileRB: TRadioButton;
    TopPanel: TPanel;
    procedure CloseBtnClick(Sender: TObject);
    procedure ExportBtnClick(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

uses
  main;

{ TMapExporterFrame }

procedure TMapExporterFrame.ExportBtnClick(Sender: TObject);
var
  IncludeLgnd: Boolean;
begin
  IncludeLgnd := IncludeLgndCB.Checked;
  if ToFileRB.Checked then MainForm.ExportMapToFile(IncludeLgnd)
      else MainForm.MapFrame.CopyMap('', IncludeLgnd);
  Hide;
end;

procedure TMapExporterFrame.CloseBtnClick(Sender: TObject);
begin
  Visible := False;
end;

end.

