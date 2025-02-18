{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       projectloader
 Description:  form with progress bar that executes loading an EPANET
               input file in a separate thread
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit projectloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls;

type
  TFileLoader = class(TThread)
  private
    FileName: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Fname: string);
    property ReturnValue;
  end;

  { TProjectLoaderForm }

  TProjectLoaderForm = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    procedure FormShow(Sender: TObject);
  private

  public
    InpFileName: string;
    LoaderResult: Integer;
    procedure FileLoaderTerminated(Sender: TObject);
  end;

var
  ProjectLoaderForm: TProjectLoaderForm;

implementation

{$R *.lfm}

uses
  project;

{ TProjectLoaderForm }

procedure TProjectLoaderForm.FormShow(Sender: TObject);
var
  FileLoader : TFileLoader;
begin
  FileLoader := TFileLoader.Create(InpFileName);
  FileLoader.OnTerminate := @FileLoaderTerminated;
  FileLoader.Start;
end;

procedure TProjectLoaderForm.FileLoaderTerminated(Sender: TObject);
begin
  LoaderResult := TFileLoader(Sender).ReturnValue;
  ModalResult := mrOK;
end;  

constructor TFileLoader.Create(Fname: string);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FileName := Fname;
end;

procedure TFileLoader.Execute;
begin
  ReturnValue := Project.Load(FileName);
end;

end.

