{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       statusframe
 Description:  a frame EPANET's status panel
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit statusframe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls;

type

  { TStatusBarFrame }

  TStatusBarFrame = class(TFrame)
    Panel1: TPanel;
    Panel5: TPanel;
    Panel4: TPanel;
    Panel3: TPanel;
    Panel2: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
  private

  public
    procedure SetText(PanelIndex: Integer; Txt: String);

  end;

implementation

{$R *.lfm}

  procedure TStatusBarFrame.SetText(PanelIndex: Integer; Txt: String);
  begin
    with FindComponent('Panel' + IntToStr(PanelIndex)) as TPanel do
      Caption := Txt;
  end;

end.

