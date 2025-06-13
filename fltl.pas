{$I STDEFINE.INC}

unit FlTl;

interface

type
  TDrvTypeNew = ( dtnFloppy, dtnHDD, dtnInvalid,
    dtnCDRom, dtnLAN, dtnUnknown, dtnOptical
    , dtnProgram, dtRamDisk, dtnSubst);

function GetBytesPerCluster(Path: PChar): LongInt;

function GetFileAges(S: String; var Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
function SetFileAges(S: String; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;

procedure GetSerFileSys(Drive: Char; var SerialNo: Longint;
  var VolLab, FileSys: String);

function GetFSString(Drive: Char): String; {AK155}
function GetShare(Drive: Char): String; {AK155}
function GetSubst(Drive: Char): String; {AK155}
function GetDriveTypeNew(Drive: Char): TDrvTypeNew; {JO} {<fltl.001>}

implementation

uses
  vp2fp,
  VpSysLow, VPUtils, Strings, advance1;

var
  DiskInfo: record
    InfoLevel: SmallWord;
    SerialNo: LongInt;
    VolumeLabel: array[0..10] of Char;
    FileSystem: array[0..7] of Char;
  end;

function GetErrorText(ErrCode: Integer; var Msg: String): Boolean;
  inline;
  begin
  Result := False;
  end;

function GetBytesPerCluster(Path: PChar): LongInt;
begin
  // fixme: porting stub
  Result := 512;
end;

{JO}
type
  TDateTimeRec = record
    FTime,FDate: SmallWord;
  end;

function SetResult(Success: Boolean): Longint;
begin
  SetResult := 0;
  if not Success then
    SetResult := 1;
end;
{/JO}

function GetFileAges(S: String; var Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
  begin
    // fixme: porting stub
    Result := 0;
  end;

function SetFileAges(S: String; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
  begin
    // fixme: porting stub
    Result := 0;
  end;

procedure GetSerFileSys(Drive: Char; var SerialNo: Longint;
    var VolLab, FileSys: String);
  begin
    // fixme: porting stub
  end;

function GetFSString(Drive: Char): String; {AK155}
  begin
    // fixme: porting stub
    Result := '';
  end;

function GetShare(Drive: Char): String; {AK155}
  begin
    // fixme: porting stub
    Result := '';
  end;

function GetSubst(Drive: Char): string; {AK155}
  begin
    // fixme: porting stub
    Result := '';
  end;

function GetDriveTypeNew(Drive: Char): TDrvTypeNew;
  begin
    // fixme: porting stub
    Result := dtnHDD;
  end;

begin
end.

