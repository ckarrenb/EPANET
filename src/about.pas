{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       about
 Description:  'About EPANET' form
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLtype, StdCtrls, LclIntf;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Image1: TImage;
    Image2: TImage;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Label4Click(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

uses
  config;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Color := Config.ThemeColor;
  Font.Size := config.FontSize;
  Label4.Left := Label3.Left + Label3.Width + 4;
end;

procedure TAboutForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TAboutForm.Label4Click(Sender: TObject);
begin
  OpenUrl('https://icons8.com');
end;


end.

