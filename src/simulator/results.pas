{====================================================================
 Project:      EPANET Graphical User Interface
 Version:      2.3
 Module:       results
 Description:  retrieves the hydraulic and water quality results
               of a simulation that were saved to file
 Authors:      see AUTHORS
 Copyright:    see AUTHORS
 License:      see LICENSE
 Last Updated: 02/16/2025
=====================================================================}

unit results;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, project,     Dialogs;

var
  QualFlag  : Integer;
  TraceNode : Integer;
  FlowFlag  : Integer;
  Nperiods  : Integer;
  Rstart    : Integer;
  Rstep     : Integer;
  Duration  : Integer;

  QualName  : String;
  QualUnits : String;

function  OpenOutFile(const Fname: String): TRunStatus;
function  OpenMsxOutFile(const Fname: String): TRunStatus;
function  GetNodeID(const I: Integer): String;
function  GetLinkID(const I: Integer): String;
function  GetNodeValue(const I: Integer; const V: Integer; const T: Integer): Single;
function  GetDmndDfctValue(const I: Integer; const T: Integer): Single;
function  GetEmitterFlowValue(const I: Integer; const T: Integer): Single;
function  GetNodeMsxValue(const I: Integer; const V: Integer; const T: Integer): Single;
function  GetLinkValue(const I: Integer; const V: Integer; const T: Integer): Single;
function  GetNodeLeakageValue(const I: Integer; const T: Integer): Single;
function  GetLinkLeakageValue(const I: Integer; const T: Integer): Single;
function  GetLinkMsxValue(const I: Integer; const V: Integer; const T: Integer): Single;
function  GetPumpEnergy(const I: Integer; var PumpEnergy: array of Single): Boolean;
function  GetPumpDemandCharge: Single;
function  GetTimeStr(const Period: Integer): String;
function  GetQualCount: Integer;
function  GetQualName(const I: Integer): String;
function  GetQualUnits(const I: Integer): String;
procedure CloseOutFile;
procedure GetDataOffsets;
procedure GetMsxSpecies;
procedure SetQualName;

implementation

uses
  utils, epanet2;

const

//****************** EXTREMELY IMPORTANT CONSTANTS ******************
//
// These constants allow one to correctly read the results of a
// simulation saved to a binary output file by the network solver
// EPANET2.DLL.
  MagicNumber = 516114521; //File signature
  Version     = 20012;     //Solver version number
  RECORDSIZE  = 4;         //Byte size of each record
  IDSIZE      = 32;        //Size of ID strings
  NUM_NODE_VARS = 4;       //Num. of node variables reported on
  NUM_LINK_VARS = 8;       //Num. of link variables reported on

// These are the numbers of additional demand/leakage variables
// saved to the simulator's binary Dmnd file.
  NUM_NODE_AUX_VARS = 3;   //Num. of auxilary node variables reported
  NUM_LINK_AUX_VARS = 1;   //Num. of auxilary link variables reported
//*******************************************************************

var
  Fout       : TFileStream;
  Fdmnd      : TFileStream;
  Fmsx       : TFileStream;
  Offset1    : Int64;       //File position where ID names begin
  Offset2    : Int64;       //File position where pump energy results begin
  Offset3    : Int64;       //File position where dynamic results begin
  MsxOffset  : Int64;
  BlockSize1 : Int64;
  BlockSize2 : Int64;
  BlockSize3 : Int64;
  BlockSize4 : Int64;
  Nlinks     : Integer;
  Npipes     : Integer;
  Npumps     : Integer;
  Nnodes     : Integer;
  MsxCount   : Integer;
  MsxSpecies : TStringList;
  MsxUnits   : TStringList;

function OpenOutFile(const Fname: String): TRunStatus;
var
  mfirst : Integer = 0;
  mlast  : Integer = 0;
  v      : Integer = 0;
  w      : Integer = 0;
begin
  // Initialize single species water quality parameters
  QualName := '';
  QualUnits := '';

  // Open binary output file & check for minimum size
  Result := rsError;
  Fout := TFileStream.Create(fname, fmOpenRead);
  if (Fout = nil) or (Fout.Size/RECORDSIZE < 21) then
  begin
    Result := rsError;
    CloseOutFile;
    exit;
  end;

  // Read # time periods, warning flag & magic number at end of file
  Fout.Seek(-3*RECORDSIZE, soEnd);
  Fout.Read(Nperiods, RECORDSIZE);
  Fout.Read(w, RECORDSIZE);
  Fout.Read(mlast, RECORDSIZE);

  // Read magic number & version number from start of file
  Fout.Seek(0, soBeginning);
  Fout.Read(mfirst, RECORDSIZE);
  Fout.Read(v, RECORDSIZE);

  // Check if EPANET run was completed
  if mlast <> MagicNumber then Result := rsError

  // Ckeck if results saved for 1 or more time periods
  else if Nperiods <= 0 then Result := rsError

  // Check if correct version of EPANET was used
  else if (mfirst <> MagicNumber) or (v <> Version)
  then Result := rsWrongVersion

  // Check if warning messages were generated
  else if w <> 0 then Result := rsWarning
  else Result := rsSuccess;

  // Close file if run was unsuccessful
  if Result in [rsFailed, rsWrongVersion, rsError] then CloseOutFile

  // Otherwise process file and open the demand deficit file
  else begin
    SetQualName;
    GetDataOffsets;
    project.OutFileOpened := true;
    Fdmnd := TFileStream.Create(project.DmndFile, fmOpenRead);
    project.DmndFileOpened := (Fdmnd <> nil) and (Fdmnd.Size > 0);
  end;
end;

procedure GetDataOffsets;
var
  Ntanks        : Integer = 0;
  Nvalves       : Integer = 0;
  dummy1        : Integer;
  dummy2        : Integer;
begin
  // Read number of network components
  Fout.Read(Nnodes, RECORDSIZE);
  Fout.Read(Ntanks, RECORDSIZE);
  Fout.Read(Nlinks, RECORDSIZE);
  Fout.Read(Npumps, RECORDSIZE);
  Fout.Read(Nvalves, RECORDSIZE);
  Npipes := Nlinks - Npumps - Nvalves;

  // Read other network data
  Fout.Read(QualFlag, RECORDSIZE);
  Fout.Read(TraceNode, RECORDSIZE);
  Fout.Read(FlowFlag, RECORDSIZE);
  Fout.Read(dummy1, RECORDSIZE);
  Fout.Read(dummy2,  RECORDSIZE);
  Fout.Read(Rstart, RECORDSIZE);
  Fout.Read(Rstep, RECORDSIZE);
  Fout.Read(Duration, RECORDSIZE);

  // File offset to where object ID names begin
  Offset1 := 15*RECORDSIZE           //Integer parameters
             + 3*80                  //Title lines
             + 2*260                 //File names
             + 2*IDSIZE;             //WQ parameter & units

  // File offset to where energy usage results begin
  Offset2 := Offset1 +
             + Nnodes*IDSIZE         //Node ID labels
             + Nlinks*IDSIZE         //Link ID labels
             + 3*Nlinks*RECORDSIZE   //Link end nodes & types
             + 2*Ntanks*RECORDSIZE   //Tank node indexes & x-areas
             + Nnodes*RECORDSIZE     //Node elevations
             + 2*Nlinks*RECORDSIZE;  //Link lengths & diameters

  // File offset to where network results for each time period begin
  Offset3 := Offset2
             + (7*Npumps+1)*RECORDSIZE; //Pump energy usage
  BlockSize1 := RECORDSIZE * (Nnodes * NUM_NODE_VARS + Nlinks * NUM_LINK_VARS);
  BlockSize2 := RECORDSIZE * Nnodes * NUM_NODE_VARS;
  BlockSize3 := RECORDSIZE * (Nnodes * NUM_NODE_AUX_VARS + Nlinks * NUM_LINK_AUX_VARS);
  BlockSize4 := RECORDSIZE * Nnodes * NUM_NODE_AUX_VARS;
end;

function  OpenMsxOutFile(const Fname: String): TRunStatus;
var
  mfirst : Longint = 0;
  mlast  : Longint = 0;
  np     : Longint = 0;
  v      : Longint = 0;
  e      : Longint = 0;
  offset : Longint = 0;
begin
  // Open binary output file & check for minimum size
  Result := rsSuccess;
  MsxCount := 0;
  if not Assigned(MsxSpecies) then MsxSpecies := TStringList.Create;
  if not Assigned(MsxUnits) then MsxUnits := TStringList.Create;
  Fmsx := TFileStream.Create(Fname, fmOpenRead);
  if (Fmsx = nil) or (Fmsx.Size/RECORDSIZE < 21) then
  begin
    Result := rsError;
    CloseOutFile;
    exit;
  end;

  // Check for records at end of file
  Fmsx.Seek(-4*RecordSize, soEnd);
  Fmsx.Read(offset, sizeOf(offset));
  MsxOffset := offset;
  Fmsx.Read(np, SizeOf(np));
  Fmsx.Read(e, SizeOf(e));
  Fmsx.Read(mlast, SizeOf(mlast));

  // Read magic number & version number from start of file
  Fmsx.Seek(0, soBeginning);
  Fmsx.Read(mfirst, SizeOf(mfirst));
  Fmsx.Read(v, SizeOf(v));

  // Check if MSX run was completed
  if mlast <> MagicNumber then
  begin
    Result := rsError;
  end

  // Ckeck if number time periods matches Epanet result
  else if np <> Nperiods then
  begin
    Result := rsError;
  end

  // Check if file has correct magic number
  else if (mfirst <> MagicNumber) then
  begin
    Result := rsWrongVersion;
  end

  // Check if error messages were generated
  else if e <> 0 then
  begin
    Result := rsError;
  end;

  // Close file if run was unsuccessful
  if Result in [rsFailed, rsWrongVersion, rsError] then CloseOutFile
  else begin
    project.MsxFileOpened := true;
    GetMsxSpecies;
  end;
end;

procedure GetMsxSpecies;
var
  n   : Integer;
  len : Integer;
  S   : String;
  Buf : array[0..1024] of Char;
begin
  // Continue reading from MSX output file
  Fmsx.Read(n, Sizeof(n));  // # nodes
  Fmsx.Read(n, Sizeof(n));  // # links
  Fmsx.Read(n, Sizeof(n));  // # species
  QualFlag := n;
  MsxCount := n;
  Fmsx.Read(n, Sizeof(n));  // report step

  // Read name of each specie
  for n := 1 to MsxCount do
  begin
    Fmsx.Read(len, SizeOf(len));  //read #chars in name
    Fmsx.Read(buf, len);          //read name into buffer
    SetString(s, buf, len);       //convert buffer to string
    MsxSpecies.Add(s);            //add name to list
    Fmsx.Read(buf, 16);           //read units into buffer (fixed at 16 chars)
    SetString(s, buf, 16);        //convert buffer to string
    s := Trim(s);                 //strip off null chars
    MsxUnits.Add(s);              //add units to list
  end;
end;

function  GetNodeID(const I: Integer): String;
var
  ID: array[0..IDSIZE-1] of Char;
  P: Int64;
begin
  ID[0] := char(0);
  P := Offset1 + (I - 1) * IDSIZE;
  Fout.Seek(P, soBeginning);
  Fout.Read(ID, IDSIZE);
  Result := String(ID);
  Result := Trim(Result);
end;

function  GetLinkID(const I: Integer): String;
var
  ID: array[0..IDSIZE-1] of Char;
  P: Int64;
begin
  ID[0] := char(0);
  P := Offset1 + (Nnodes + I - 1) * IDSIZE;
  Fout.Seek(P, soBeginning);
  Fout.Read(ID, IDSIZE);
  Result := String(ID);
  Result := Trim(Result);
end;

function  GetNodeValue(const I: Integer; const V: Integer; const T: Integer): Single;
// I = node index (1 to Nnodes)
// V = index of variable in output file
// T = time period (0 to Nperiods - 1)
var
  P: Int64;
begin
  Result := 0;
  P := Offset3 + T * BlockSize1 + (V * Nnodes + (I-1)) * RECORDSIZE;
  Fout.Seek(P, soBeginning);
  Fout.Read(Result, RECORDSIZE);
end;

function  GetDmndDfctValue(const I: Integer; const T: Integer): Single;
var
  P: Int64;
begin
  Result := 0;
  if Fdmnd = nil then exit;
  P := (T * BlockSize3) + ((I - 1) * RECORDSIZE);
  Fdmnd.Seek(P, soBeginning);
  Fdmnd.Read(Result, RECORDSIZE);
end;

function  GetEmitterFlowValue(const I: Integer; const T: Integer): Single;
var
  P: Int64;
begin
  Result := 0;
  if Fdmnd = nil then exit;
  P := (T * BlockSize3) + ((Nnodes + I - 1) * RECORDSIZE);
  Fdmnd.Seek(P, soBeginning);
  Fdmnd.Read(Result, RECORDSIZE);
end;

function  GetNodeLeakageValue(const I: Integer; const T: Integer): Single;
var
  P: Int64;
begin
  Result := 0;
  if Fdmnd = nil then exit;
  P := (T * BlockSize3) + ((2*Nnodes + I - 1) * RECORDSIZE);
  Fdmnd.Seek(P, soBeginning);
  Fdmnd.Read(Result, RECORDSIZE);
end;

function  GetLinkLeakageValue(const I: Integer; const T: Integer): Single;
var
  P: Int64;
begin
  Result := 0;
  if Fdmnd = nil then exit;
  P := (T * BlockSize3) + BlockSize4 + ((I - 1) * RECORDSIZE);
  Fdmnd.Seek(P, soBeginning);
  Fdmnd.Read(Result, RECORDSIZE);
end;

function  GetNodeMsxValue(const I: Integer; const V: Integer; const T: Integer): Single;
// I = node index (1 to Nnodes)
// V = index of variable in MSX output file (0 to MsxCount - 1)
// T = time period (0 to Nperiods - 1)
var
  P: Int64;
begin
  Result := 0;
  P := MsxOffset +                               //Start of MSX results
       T*RECORDSIZE*(Nnodes + Nlinks)*MsxCount + //Results from prior periods
       V*Nnodes*RECORDSIZE +                     //Results for prior species
       (I-1)*RECORDSIZE;                         //Results for prior nodes
  Fmsx.Seek(P, soBeginning);
  Fmsx.Read(Result, SizeOf(Single));
end;

function  GetLinkValue(const I: Integer; const V: Integer; const T: Integer): Single;
// I = link index (1 to Nlinks)
// V = variable index (0 to NUM_LINK_VARS -1)
// T = time period (0 to Nperiods - 1)
var
  P: Int64;
begin
  Result := 0;
  P := Offset3 + T * BlockSize1 + BlockSize2 + (V * Nlinks + (I-1)) * RECORDSIZE;
  Fout.Seek(P, soBeginning);
  Fout.Read(Result, RECORDSIZE);
end;

function  GetLinkMsxValue(const I: Integer; const V: Integer; const T: Integer): Single;
// I = node index (1 to Nnodes)
// V = index of variable in MSX output file (0 to MsxCount - 1)
// T = time period (0 to Nperiods - 1)
var
  P: Int64;
begin
  Result := 0;
  P := MsxOffset +                               //Start of MSX results
       T*RECORDSIZE*(Nnodes + Nlinks)*MsxCount + //Results from prior periods
       Nnodes*MsxCount*RECORDSIZE +              //Results for nodes
       V*Nlinks*RECORDSIZE +                     //Results for prior species
       (I-1)*RECORDSIZE;                         //Results for prior links
  Fmsx.Seek(P, soBeginning);
  Fmsx.Read(Result, SizeOf(Single));
end;

function GetPumpEnergy(const I: Integer; var PumpEnergy: array of Single): Boolean;
// I = link index (1 to Nlinks)
// PumpEnergy = array of 6 energy usage statistics
var
  J: Integer;
  K: Integer = 0;
begin
  Result := true;
  Fout.Seek(Offset2, soBeginning);
  for J := 1 to Npumps do
  begin
    Fout.Read(K, RECORDSIZE);
    Fout.Read(PumpEnergy, 6 * RECORDSIZE);
    if K = I then exit;
  end;
  Result := false;
end;

function  GetPumpDemandCharge: Single;
var
  P: Int64;
begin
  Result := 0;
  P := Offset2 + (7 * Npumps) * RECORDSIZE;
  Fout.Seek(P, soBeginning);
  Fout.Read(Result, RECORDSIZE);
end;

function  GetTimeStr(const Period: Integer): String;
var
  Seconds: Integer;
begin
  Seconds := Period * Rstep + Rstart;
  Result := utils.Time2Str(Seconds);
end;

function  GetQualCount: Integer;
begin
  Result := 0;
  if MsxFlag then Result := MsxCount
  else if QualFlag > 0 then Result := 1;
end;

function  GetQualName(const I: Integer): String;
begin
  Result := '';
  if MsxFlag then
  begin
    if project.MsxFileOpened then
      Result := MsxSpecies[I];
  end
  else if project.OutFileOpened then Result := QualName;
end;

function GetQualUnits(const I: Integer): String;
begin
  Result := '';
  if MsxFlag then
  begin
    if project.MsxFileOpened then
      Result := MsxUnits[I];
  end
  else if project.OutFileOpened then Result := QualUnits;
end;

procedure SetQualName;
var
  QualType: Integer;
  ChemName: array[0..EN_MAXID] of AnsiChar;
  Units: array[0..EN_MAXID] of AnsiChar;
  TraceNodeIndex: Integer = 0;
begin
  QualType := project.qtNone;
  if epanet2.ENgetqualinfo(QualType, ChemName, Units, TraceNodeIndex) > 0 then
    exit;
  if QualType = project.qtChem then
  begin
    QualName := String(ChemName);
    QualUnits := String(Units);
  end
  else if QualType = project.qtAge then
  begin
    QualName := 'Water Age';
    QualUnits := 'Hours';
  end
  else if QualType = project.qtTrace then
  begin
    QualName := 'Trace ';
    if TraceNodeIndex > 0 then
      QualName := QualName + project.GetID(cNodes, TraceNodeIndex);
    QualUnits := '%';
  end;
end;

procedure CloseOutFile;
//----------------------------------------------------
// Closes binary output results file.
//----------------------------------------------------
begin
  FreeAndNil(Fout);
  FreeAndNil(Fdmnd);
  FreeAndNil(Fmsx);
  FreeAndNil(MsxSpecies);
  FreeAndNil(MsxUnits);
  project.OutFileOpened := false;
  project.DmndFileOpened := false;
  project.MsxFileOpened := false;
end;

end.

