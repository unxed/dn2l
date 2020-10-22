{ Dpmi32-specific file tools unit by J.Osadtchiy (JO), A.Kozlov (Cat)}
{ Optimise-}

{$I STDEFINE.INC}

{Cat
   04/12/2001 - в WinNT поддерживается копирование Security Attributes
}

unit FlTl;
interface

type
  TDrvTypeNew = ( dtnFloppy, dtnHDD, dtnInvalid,
    dtnCDRom, dtnLAN, dtnUnknown, dtnOptical
    , dtnProgram, dtRamDisk, dtnSubst);

function GetBytesPerCluster(Path: PChar): LongInt;

function GetFileAges(S: String; var Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
  {JO: возвращает время и дату последней модификации (Age_LWr),                   }
  {    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
  {    файла по хэндлу файла (Handle), принимает значение кода ошибки             }
function SetFileAges(S: String; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
  {JO: устанавливает время и дату последней модификации (Age_LWr),                }
  {    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
  {    файла по хэндлу файла (Handle), принимает значение кода ошибки             }

procedure GetSerFileSys(Drive: Char; var SerialNo: Longint;
  var VolLab, FileSys: String);

function GetFSString(Drive: Char): String; {AK155}
function GetShare(Drive: Char): String; {AK155}
function GetSubst(Drive: Char): String; {AK155}
function GetDriveTypeNew(Drive: Char): TDrvTypeNew; {JO} {<fltl.001>}

function GetErrorText(ErrCode: Integer; var Msg: String): Boolean;
  inline;
  begin
  Result := False;
  end;

implementation

uses
  dpmi32df, dpmi32, Dos, VpSysLow, VPUtils, Strings, advance1;

var
  DiskInfo: record
    InfoLevel: SmallWord;
    SerialNo: LongInt;
    VolumeLabel: array[0..10] of Char;
    FileSystem: array[0..7] of Char;
  end;

function GetBytesPerCluster(Path: PChar): LongInt;
var
  regs: real_mode_call_structure_typ;
begin
  fillchar(Mem[segdossyslow32],
           SizeOf(DriveData) + SizeOf(Path) + 1{#0}, #0);
  move(Path^, Mem[segdossyslow32 + SizeOf(DriveData)], SizeOf(Path));
  init_register(regs);
  regs.ax_ := $7303;
  regs.ds_ := segdossyslow16;
  regs.dx_ := SizeOf(DriveData); //ds:dx=path
  regs.es_ := segdossyslow16;
//  regs.di_ := 0;
  regs.cx_ := SizeOf(DriveData);
  intr_realmode(Regs, $21);
  move(Mem[segdossyslow32], DriveData.RecSize, SizeOf(DriveData));
  if DriveData.RecSize <> 0
  then begin
       Result := DriveData.SectorsPerCluster * DriveData.BytesPerSector;
       end
  else begin
       init_register(regs);
       regs.ah_ := $36;
       regs.dl_ := Byte(UpCase(Path[0])) - Byte('A') + 1;
       intr_realmode(Regs, $21);
       result := regs.ax_ * 512;
       end;
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
  var
    f: File;
    SaveMode: Longint;
  begin
  SaveMode := FileMode;
  FileMode := 0;
  Assign(f, S);
  Reset(f, 1);
  GetFTime(f, Age_LWr);
{ Тут неплохо бы вставить чтение Age_Cr и Age_LAc через Win95 LFN API}
  Age_Cr := 0;
  Age_LAc := 0;
  Close(f);
  FileMode := SaveMode;
  Result := 0;
  end;

function SetFileAges(S: String; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
  var
    f: File;
    SaveMode: Longint;
    i: Longint;
  begin
  SaveMode := FileMode;
  FileMode := open_access_WriteOnly + open_share_DenyReadWrite;
  Assign(f, S);
  Reset(f, 1);
  SetFTime(f, Age_LWr);
{ Тут неплохо бы вставить запись Age_Cr и Age_LAc через Win95 LFN API}
  Close(f);
  FileMode := SaveMode;
  Result := 0;
  end;

procedure GetSerFileSys(Drive: Char; var SerialNo: Longint;
    var VolLab, FileSys: String);
  begin
  FileSys := GetFSString(Drive);
  SerialNo := DiskInfo.SerialNo;
  VolLab := StrPas(DiskInfo.VolumeLabel);
  DelRight(VolLab);
  if (VolLab = '') or (VolLab = 'NO NAME')
    then VolLab := SysGetVolumeLabel(Drive);
  end;

function GetFSString(Drive: Char): String; {AK155}
  var
    Regs: real_mode_call_structure_typ;
    i: Integer;
  begin
  fillchar(Mem[segdossyslow32], SizeOf(DiskInfo), #0);
  init_register(Regs);
  with Regs do
    begin
    AX_ := $6900;
    BX_ := Byte(Drive) - Byte('A') + 1;
    DS_ := segdossyslow16;
//    DX_ := 0;  {DS:DX -> buffer for returned info}
    end;
  intr_realmode(Regs, $21);
  //we don't check errors for Novell compatibility
  //Mem[segdossyslow32] filled with zeros for catching errors
  move(Mem[segdossyslow32], DiskInfo.InfoLevel, SizeOf(DiskInfo));
  {В ДОС-сессиях OS/2 и WinNT поле FS дополняется #0, а в голом ДОС
  (PC DOS 7, MS DOS 7.*) - пробелами. }
  Result := fDelRight(fReplace(#0, ' ', StrPas(DiskInfo.FileSystem)));
  end;

function GetShare(Drive: Char): String; {AK155}
  var
    Regs: real_mode_call_structure_typ;
  const
    Buf: array[0..4] of char = 'C:\'#0#0;
  begin
  Buf[0] := Drive;
  move(Buf, Mem[segdossyslow32], 5);
  with Regs do
    begin
    AH_ := $60; // CANONICALIZE FILENAME OR PATH
    DS_ := segdossyslow16; SI_ := 0; // исходное имя
    ES_ := segdossyslow16; DI_ := 4; // результат
    intr_realmode(Regs, $21);
    Result := StrPas(@Mem[segdossyslow32+4]);
    if (Length(Result) <> 0) and (Result[1] = Drive) then
      Result := '';
    end;
  end;

function GetSubst(Drive: Char): string; {AK155}
  begin
  Result := GetShare(Drive);
  end;

function GetDriveTypeNew(Drive: Char): TDrvTypeNew;
  var
    Regs: real_mode_call_structure_typ;
  begin
    Result := dtnInvalid;

    init_register(regs);
    regs.ax_ := $4408;
    regs.bl_ := Byte(Drive) - $40;
    intr_realmode(regs, $21);
    if (regs.flags_ and fCarry = 0) and (regs.ax_ = 0) then
      begin
        Result := dtnFloppy;
        Exit;
      end;

    init_register(regs);
    regs.ax_ := $150b;
    regs.cx_ := Byte(Drive) - $41;
    intr_realmode(regs, $2f);
    if (regs.bx_ = $ADAD) and (regs.ax_ <> 0) then
      begin
        Result := dtnCDRom;
        Exit;
      end;

    init_register(regs);
    regs.ax_ := $4409;
    regs.bl_ := Byte(Drive) - $40;
    intr_realmode(regs, $21);
    if (regs.flags_ and fCarry = 0) then
      begin
      if (regs.dh_ and $10) <> 0 then
        begin
        if GetShare(Drive) = '' then
          Result := dtnProgram
        else
          Result := dtnLAN;
        end
      else if (regs.dh_ and $1) <> 0 then
        Result := dtnSubst
      else
        Result := dtnHDD;
      end;

  end;

begin
end.

