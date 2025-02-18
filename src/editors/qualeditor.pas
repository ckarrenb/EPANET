{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       qualeditor
 Description:  a dialog form that edits single species Water
               Quality options
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit qualeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LCLIntf, SpinEx;

type

  { TQualEditorForm }

  TQualEditorForm = class(TForm)
    QualTolEdit: TFloatSpinEditEx;
    QualNameEdit: TEdit;
    BulkOrderEdit: TFloatSpinEditEx;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    MainPanel: TPanel;
    ChemOptionsPanel: TPanel;
    GeneralOptionsPanel: TPanel;
    BtnPanel: TPanel;
    ConcenLimitEdit: TFloatSpinEditEx;
    DiffusEdit: TFloatSpinEditEx;
    TankOrderEdit: TFloatSpinEditEx;
    QualTypeCombo: TComboBox;
    UnitsCombo: TComboBox;
    UnitsLabel: TLabel;
    WallOrderCombo: TComboBox;
    procedure QualNameEditChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure QualTypeComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function EntriesAreValid: Boolean;
    procedure SetQualOptions;

  public
    HasChanged: Boolean;

  end;

var
  QualEditorForm: TQualEditorForm;

implementation

{$R *.lfm}

uses
  main, project, utils, config, epanet2;

const
  ChemHint: String = 'Optional name of the chemical to analyze.';
  TraceHint: String = 'ID name of the node to trace from.';

var
  QualType: Integer;
  ChemName: array[0..EN_MAXID] of AnsiChar;
  QualUnits: array[0..EN_MAXID] of AnsiChar;
  TraceNode: array[0..EN_MAXID] of AnsiChar;
  TraceNodeIndex: Integer = 0;

{ TQualEditorForm }

procedure TQualEditorForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Color := config.ThemeColor;
  Font.Size := config.FontSize;
  for I := 0 to High(project.QualModelStr) do
    QualTypeCombo.Items.Add(project.QualModelStr[I]);
  QualNameEdit.MaxLength := EN_MAXID;
  UnitsLabel.Left := UnitsCombo.Left;
  UnitsLabel.Top := Label4.Top;
end;

procedure TQualEditorForm.FormShow(Sender: TObject);
var
  I: Integer;
  X: Single = 0;
begin
  epanet2.ENgetqualinfo(QualType, ChemName, QualUnits, TraceNodeIndex);
  QualTypeCombo.ItemIndex := QualType;

  if QualType = Project.qtChem then
  begin
    QualNameEdit.Text := ChemName;
  end
  else ChemName := '';

  TraceNode := '';
  if TraceNodeIndex > 0 then
    epanet2.ENgetnodeID(TraceNodeIndex, TraceNode);

  epanet2.ENgetoption(EN_TOLERANCE, X);
  QualTolEdit.Value := X;

  epanet2.ENgetoption(EN_BULKORDER, X);
  BulkOrderEdit.Value := X;

  epanet2.ENgetoption(EN_TANKORDER, X);
  TankOrderEdit.Value := X;

  epanet2.ENgetoption(EN_WALLORDER, X);
  I := Round(X);
  WallOrderCombo.ItemIndex := I;

  epanet2.ENgetoption(EN_CONCENLIMIT, X);
  ConcenLimitEdit.Value := X;

  epanet2.ENgetoption(EN_SP_DIFFUS, X);
  DiffusEdit.Value := X;

  QualTypeComboChange(Sender);
  QualTypeCombo.SetFocus;
  HasChanged := False;
end;

procedure TQualEditorForm.OkBtnClick(Sender: TObject);
begin
  if EntriesAreValid then
  begin
    SetQualOptions;
    ModalResult := mrOk;
  end;
end;

procedure TQualEditorForm.QualNameEditChange(Sender: TObject);
// OnChange handler shared by all edit controls
begin
  HasChanged := True;
end;

procedure TQualEditorForm.QualTypeComboChange(Sender: TObject);
begin
  Label2.Caption := 'Constituent Name';
  Label2.Hint := '';
  QualNameEdit.Enabled := False;
  QualTolEdit.Enabled := True;
  if QualTypeCombo.ItemIndex = qtChem then
  begin
    ChemOptionsPanel.Visible := True;
    UnitsLabel.Visible := false;
    UnitsCombo.Visible := true;
  end
  else
  begin
    ChemOptionsPanel.Visible := False;
    UnitsLabel.Visible := true;
    UnitsCombo.Visible := false;
  end;
  case QualTypeCombo.ItemIndex of
  qtNone:
    begin
      QualNameEdit.Text := '';
      QualTolEdit.Enabled := False;
      UnitsLabel.Caption := '';
    end;
  qtChem:
    begin
      QualNameEdit.Text := ChemName;
      QualNameEdit.Enabled := True;
      Label2.Hint := ChemHint;
     end;
  qtAge:
    begin
      QualNameEdit.Text := 'Age';
      UnitsLabel.Caption := 'Hours';
    end;
  qtTrace:
    begin
      Label2.Caption := 'Node to Trace From';
      Label2.Hint := TraceHint;
      QualNameEdit.Enabled := True;
      QualNameEdit.Text := TraceNode;
      UnitsLabel.Caption := 'Percent';
    end;
  end;
  HasChanged := True;
end;

function TQualEditorForm.EntriesAreValid: Boolean;
var
  I: Integer;
  X: Single;
begin
  Result := False;
  QualType := QualTypeCombo.ItemIndex;

  TraceNode := '';
  if QualType = qtTrace then
  begin
    TraceNode := QualNameEdit.Text;
    if epanet2.ENgetnodeindex(TraceNode, TraceNodeIndex) > 0 then
    begin
      utils.MsgDlg('Tracing source node does not exist.', mtError, [mbOK]);
      QualNameEdit.SetFocus;
      exit;
    end;
  end;
  Result := True;
end;

procedure TQualEditorForm.SetQualOptions;
var
  X: Single;
  ChemStr: String;
begin
  ChemStr := '';
  QualUnits := '';
  if QualType = qtChem then
  begin
    ChemStr := Trim(QualNameEdit.Text);
    if Length(ChemStr) = 0 then ChemStr := 'Chemical';
    QualUnits := UnitsCombo.Text;
  end;
  if epanet2.ENsetqualtype(QualType, PChar(ChemStr), PChar(QualUnits),
    PChar(TraceNode)) > 0 then
  begin
    utils.MsgDlg('Was not able to set water quality options.',
      mtInformation, [mbOk]);
    exit;
  end;
  X := QualTolEdit.Value;
  epanet2.ENsetoption(EN_TOLERANCE, X);

  if QualType = qtChem then
  begin
    X := BulkOrderEdit.Value;
    epanet2.ENsetoption(EN_BULKORDER, X);

    X := TankOrderEdit.Value;
    epanet2.ENsetoption(EN_TANKORDER, X);

    X := WallOrderCombo.ItemIndex;
    epanet2.ENsetoption(EN_WALLORDER, X);

    X := ConcenLimitEdit.Value;
    epanet2.ENsetoption(EN_CONCENLIMIT, X);

    X := DiffusEdit.Value;
    epanet2.ENsetoption(EN_SP_DIFFUS, X);
  end;
end;

procedure TQualEditorForm.HelpBtnClick(Sender: TObject);
begin
  MainForm.ShowHelp('#single_species_quality');
end;

end.

