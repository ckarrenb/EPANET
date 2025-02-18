{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       themerange
 Description:  a dialog form that sets the range of a map theme
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit themerange;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SpinEx;

type

  { TThemeRangeForm }

  TThemeRangeForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    MinEdit: TFloatSpinEditEx;
    Label2: TLabel;
    Label3: TLabel;
    MaxEdit: TFloatSpinEditEx;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure SetRange(Vmin, Vmax: Double);
    procedure GetRange(var Vmin, Vmax: Double);

  end;

var
  ThemeRangeForm: TThemeRangeForm;

implementation

{$R *.lfm}

uses
  config, utils;

{ TThemeRangeForm }

procedure TThemeRangeForm.FormCreate(Sender: TObject);
begin
  Color := config.ThemeColor;
  Font.Size := config.FontSize;
end;

procedure TThemeRangeForm.Button1Click(Sender: TObject);
begin
  if MinEdit.Value >= MaxEdit.Value then
  begin
    utils.MsgDlg('Range values are invalid.', mtError, [mbOk], self);
    exit;
  end
  else ModalResult := mrOK;
end;

procedure TThemeRangeForm.SetRange(Vmin, Vmax: Double);
begin
  MaxEdit.Value := Vmax;
  MinEdit.Value := Vmin;
end;

procedure TThemeRangeForm.GetRange(var Vmin, Vmax: Double);
begin
  Vmin := MinEdit.Value;
  Vmax := MaxEdit.Value;
end;

end.

