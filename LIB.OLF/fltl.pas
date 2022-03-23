{ OS/2-specific file tools unit by J.Osadtchiy (JO), A.Korop (AK155)}
{ Optimise-}

{$I STDEFINE.INC}
unit FlTl;

interface

type
{<fltl.001>}
  TDrvTypeNew = ( dtnFloppy, dtnHDD, dtnInvalid,
    dtnCDRom, dtnLAN, dtnUnknown, dtnOptical
    , dtnProgram, dtRamDisk, dtnSubst);

const

  ageLastWrite = 1;
  ageCreation = 2;
  ageLastAccess = 3;
  ageAll = 4;

function GetFileAge(S: String; AgeType: Byte): LongInt;
{JO: возвращает время и дату файла или каталога по полному пути (S); }
{    тип времени и даты задаётся в AgeType и может принимать         }
{    значение трёх стандартных переменных  ageLastWrite,             }
{    ageCreation и ageLastAccess                                     }

function SetFileAge(S: String; Age: LongInt; AgeType: Byte): LongInt;
{JO: устанавливает время и дату файла или каталога по полному пути (S);}
{    тип времени и даты задаётся в AgeType и может принимать           }
{    значение трёх стандартных переменных  ageLastWrite,               }
{    ageCreation и ageLastAccess                                       }

function GetFileAges(S: String; var Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
{JO: возвращает время и дату последней модификации (Age_LWr),                   }
{    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
{    файла или каталога по полному пути (S), принимает значение кода ошибки     }

function SetFileAges(S: String; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
{JO: устанавливает время и дату последней модификации (Age_LWr),                }
{    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
{    файла или каталога по полному пути (S), принимает значение кода ошибки     }

procedure GetSerFileSys(Drive: Char; var SerialNo: Longint;
  var VolLab, FileSys: String);

function SetVolume(DriveNum: LongInt; VolLab: String): LongInt;
function GetBytesPerCluster(Path: PChar): LongInt;

procedure CopyEAs(FFromName, FToName: String);

function GetEAString(const Filename, Name: String; var Value: String;
     Silent: Boolean): Integer;
(*
function SetEAString(const Filename, Name, Value: string): Integer;
*)
procedure SetEALongname(Filename: String);
function GetFSString(Drive: Char): String; {AK155}
function GetShare(Drive: Char): String; {AK155}
function GetSubst(Drive: Char): string; {AK155}
function GetDriveTypeNew(Drive: Char): TDrvTypeNew; {JO} {<fltl.001>}
//JO: Функция, которую следует использовать в исходниках DN/2
//    вместо неудачной штатной для VP RTL функции SysGetDriveType,
//    в отличие от которой данная функция определяет тип диска
//    никогда к нему не обращаясь - используя DosDevIOCtl и не
//    делая никаких проверок файловой системы

function GetErrorText(ErrCode: Integer; var Msg: String): Boolean;
  {` Дать текст расшифровки кода ошибки `}

implementation

uses
  Os2Def, Os2Base, Strings
  {для CopyEAs}, Objects, Messages, EAOper, Advance1, Commands, DNApp
  ;

var
{&stdcall+}
  Net32UseGetInfo: function(pszServername: PChar; netname: PChar;
     ulLevel: ULONG;
     var buf; ulBuflen: ULONG; var pulBytesAvail: ULONG
     ): APIRet;
{&stdcall-}

  hNetapi32: hModule;

(*
function GetFileAge(S: String): longint;
 begin
  GetFileAge := FileAge (S);
 end;
*)

function GetFileAge(S: String; AgeType: Byte): LongInt;
  var
    fsts3ConfigInfo: FileStatus3;
    ulBufSize: LongInt;
    rc: LongInt;
    PS: PChar;
    PSArr: array[0..255] of Char;
  begin
  ulBufSize := SizeOf(FileStatus3);
  PS := PSArr;
  PS := StrPCopy(PS, S);
  rc := DosQueryPathInfo(
      PS,
      fil_Standard,
      fsts3ConfigInfo,
      ulBufSize);

  if rc > 0 then
    GetFileAge := 0
  else
    case AgeType of
      ageLastWrite:
        GetFileAge := (fsts3ConfigInfo.fdateLastWrite shl 16)
          +fsts3ConfigInfo.ftimeLastWrite;
      ageCreation:
        GetFileAge := (fsts3ConfigInfo.fdateCreation shl 16)
          +fsts3ConfigInfo.ftimeCreation;
      ageLastAccess:
        GetFileAge := (fsts3ConfigInfo.fdateLastAccess shl 16)
          +fsts3ConfigInfo.ftimeLastAccess;
      else {case}
        GetFileAge := 0;
    end {case};
  end { GetFileAge };

function SetFileAge(S: String; Age: LongInt; AgeType: Byte): LongInt;
  var
    fsts3ConfigInfo: FileStatus3;
    ulBufSize: LongInt;
    rc: LongInt;
    PS: PChar;
    PSArr: array[0..255] of Char;
  begin
  ulBufSize := SizeOf(FileStatus3);
  PS := PSArr;
  PS := StrPCopy(PS, S);
  rc := DosQueryPathInfo(
      PS,
      fil_Standard,
      fsts3ConfigInfo,
      ulBufSize);

  case AgeType of
    ageLastWrite:
      begin
      fsts3ConfigInfo.fdateLastWrite := Age shr 16;
      fsts3ConfigInfo.ftimeLastWrite := Age and $FFFF;
      end;
    ageCreation:
      begin
      fsts3ConfigInfo.fdateCreation := Age shr 16;
      fsts3ConfigInfo.ftimeCreation := Age and $FFFF;
      end;
    ageLastAccess:
      begin
      fsts3ConfigInfo.fdateLastAccess := Age shr 16;
      fsts3ConfigInfo.ftimeLastAccess := Age and $FFFF;
      end;
    ageAll:
      begin
      fsts3ConfigInfo.fdateLastWrite := Age shr 16;
      fsts3ConfigInfo.ftimeLastWrite := Age and $FFFF;
      fsts3ConfigInfo.fdateCreation := Age shr 16;
      fsts3ConfigInfo.ftimeCreation := Age and $FFFF;
      fsts3ConfigInfo.fdateLastAccess := Age shr 16;
      fsts3ConfigInfo.ftimeLastAccess := Age and $FFFF;
      end;
    else {case}
      begin
      SetFileAge := 1;
      Exit;
      end;
  end {case};

  {ulBufSize := sizeof(FileStatus3);}
  rc := DosSetPathInfo(
      PS,
      fil_Standard,
      fsts3ConfigInfo,
      ulBufSize,
      0);
  SetFileAge := rc;
  end { SetFileAge };

function GetFileAges(S: String; var Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
  var
    fsts3ConfigInfo: FileStatus3;
    ulBufSize: LongInt;
    rc: LongInt;
    PS: PChar;
    PSArr: array[0..255] of Char;
  begin
  ulBufSize := SizeOf(FileStatus3);
  PS := PSArr;
  PS := StrPCopy(PS, S);
  rc := DosQueryPathInfo(
      PS,
      fil_Standard,
      fsts3ConfigInfo,
      ulBufSize);
  if rc > 0 then
    begin
    Age_LWr := 0;
    Age_Cr := 0;
    Age_LAc := 0;
    end
  else
    begin
    Age_LWr := (fsts3ConfigInfo.fdateLastWrite shl 16)
      +fsts3ConfigInfo.ftimeLastWrite;
    Age_Cr := (fsts3ConfigInfo.fdateCreation shl 16)
      +fsts3ConfigInfo.ftimeCreation;
    Age_LAc := (fsts3ConfigInfo.fdateLastAccess shl 16)
      +fsts3ConfigInfo.ftimeLastAccess;
    end;
  GetFileAges := rc;
  end { GetFileAges };


function SetFileAges(S: String; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
  var
    fsts3ConfigInfo: FileStatus3;
    ulBufSize: LongInt;
    rc: LongInt;
    PS: PChar;
    PSArr: array[0..255] of Char;
  begin
  ulBufSize := SizeOf(FileStatus3);
  PS := PSArr;
  PS := StrPCopy(PS, S);
  rc := DosQueryPathInfo(
      PS,
      fil_Standard,
      fsts3ConfigInfo,
      ulBufSize);

  fsts3ConfigInfo.fdateLastWrite := Age_LWr shr 16;
  fsts3ConfigInfo.ftimeLastWrite := Age_LWr and $FFFF;
  fsts3ConfigInfo.fdateCreation := Age_Cr shr 16;
  fsts3ConfigInfo.ftimeCreation := Age_Cr and $FFFF;
  fsts3ConfigInfo.fdateLastAccess := Age_LAc shr 16;
  fsts3ConfigInfo.ftimeLastAccess := Age_LAc and $FFFF;

  {ulBufSize := sizeof(FileStatus3);}
  rc := DosSetPathInfo(
      PS,
      fil_Standard,
      fsts3ConfigInfo,
      ulBufSize,
      0);
  SetFileAges := rc;
  end { SetFileAges };

procedure GetSerFileSys(Drive: Char; var SerialNo: Longint;
  var VolLab, FileSys: String);
  type
    fsInfoBuf = record
      ulVolser: LongInt {ULong}; // Volume serial number
      Vol: VolumeLabel; // Volume label
      end;
  var
    VolumeInfo: fsInfoBuf; // File system info buffer
    rc: LongInt {ApiRet};

  begin
  FileSys := GetFSString(Drive);
  rc := DosQueryFSInfo(Byte(Drive) - (Byte('A')-1),
    fsil_VolSer, VolumeInfo, SizeOf(fsInfoBuf));
  if rc = 0 then
    begin
    SerialNo := VolumeInfo.ulVolser;
    VolLab := VolumeInfo.Vol;
    end
  else
    begin
    SerialNo := 0;
    VolLab := '';
    end;
  end { GetSerFileSys };

function SetVolume(DriveNum: LongInt; VolLab: String): LongInt;
  var
    FSInfoBuf: VolumeLabel; // File system info buffer
    rc: LongInt;
  begin
  FSInfoBuf := VolLab;
  rc := DosSetFSInfo(DriveNum, fsil_VolSer, FSInfoBuf,
       SizeOf(VolumeLabel));
  SetVolume := rc;
  end;

function GetBytesPerCluster(Path: PChar): LongInt;
  var
    DriveNum: Longint;
    aulFSInfoBuf: FsAllocate;
    rc: LongInt;
  begin
  DriveNum := Byte(Upcase(Path^)) - (Byte('A') - 1);
  rc := DosQueryFSInfo(DriveNum, fsil_Alloc, aulFSInfoBuf,
         SizeOf(FsAllocate));
  if rc = 0 then
    GetBytesPerCluster := aulFSInfoBuf.cSectorUnit*aulFSInfoBuf.cbSector
  else
    GetBytesPerCluster := 0;
  end;

procedure CopyEAs(FFromName, FToName: String);
  var
    coll: PStringCollection;
    ulrc, ulEASize: Cardinal;
    i: LongInt;
    PszName: PChar;
    params: array[0..1] of Pointer;
    ea: Pointer;

  begin
  coll := New(PStringCollection, Init(5, 10, False));
  ulrc := EnumEAs(FFromName, coll);
  if ulrc = 0 then
    begin
    if  (coll^.Count > 0) then
      {JO: см. комментарий от 30-07-2002 к EAOper.EnumEAs }
      for i := coll^.Count-1 downto 0 do
        begin
        PszName := PChar(coll^.At(i));
        Inc(PszName);
        {JO почему возникает необходимость так извращаться - непонятно, но надо}
        {если брать просто PString(coll^.At(i)) - падает}
        ulrc := RetrieveEA(FFromName, PszName, ea, ulEASize, False);
        if ulrc = 0 then
          begin
          ulrc := StoreEA(FToName, PszName, ea, ulEASize);
          if  (ulrc <> 0)
            and (ulrc <> 282)
            { Destination file system does not support EAs }
            and (ulrc <> 283)
            { Destination file system does not support EAs }
            { and the source file's EAs contain a need EA  }
            then
            begin
            params[0] := coll^.At(i);
            params[1] := Pointer(ulrc);
            MessageBox
              (#3+GetString(dl_Failed_to_store_EA)+' "%s"'+^M^C'(RC=%d)'
              , @params,
              mfError or mfOKButton);
            end;
          if ulrc = 283 then
            MessageBox(GetString(dl_Critical_EA_Copy_Fail)+FFromName,
               nil, mfOKButton);
          FreeMem(ea);
          end
        else
          begin
          params[0] := coll^.At(i);
          params[1] := Pointer(ulrc);
          MessageBox
            (#3+GetString(dl_Failed_to_retrieve_EA)+' "%s"'+^M^C'(RC=%d)'
            , @params,
            mfError or mfOKButton);
          end;
        end;
    end
  else if ulrc <> 124 then
    {JO: ошибка 124 - не предусмотренный для данного      }
    {    устройства уровень получения/задания информации  }
    MessageBox(#3+GetString(dl_Failed_to_enumerate_EA)+^M^C'(RC=%d)',
      @ulrc, mfError or mfOKButton);
  Dispose(coll, Done);
  end { CopyEAs };

function GetEAString(const Filename, Name: String; var Value: String;
     Silent: Boolean): Integer;
  var
    ea: Pointer;
    ulEASize, ulSize: Cardinal;
    szName: array[0..255] of Char;
    pszValue: PChar;
  begin
  Value := '';
  Result := RetrieveEA(Filename, StrPCopy(szName, Name), ea, ulEASize,
       Silent);
  if  (Result = 0) and (ea <> nil) then
    begin
    ulSize := RetrieveStringSize(ea);
    GetMem(pszValue, Succ(ulSize));
    Value := StrPas(RetrieveString(ea, pszValue));
    FreeMem(pszValue);
    FreeMem(ea);
    end;
  end;

function SetEAString(const Filename, Name, Value: String): Integer;
  var
    ea: Pointer;
    szValue, szName: array[0..255] of Char;
    ulEASize: Cardinal;
  begin
  ea := BuildEAFromString(StrPCopy(szValue, Value), ulEASize);
  SetEAString := StoreEA(Filename, StrPCopy(szName, Name), ea, ulEASize);
  FreeMem(ea);
  end;

procedure SetEALongname(Filename: String);
  var
    LNValue: String;
    Result_: Integer;
  begin
  Result_ := GetEAString(Filename, '.LONGNAME', LNValue, True);
  if Result_ = 0 then
    begin
    if BigInputBox(GetString(dlEditEALongname),
         GetString(dl_EALongname), LNValue, 255, hsEditEALongname)
       <> cmOK
    then
      Exit;
    Result_ := SetEAString(Filename, '.LONGNAME', LNValue);
    if Result_ <> 0 then
      MessageBox
        (#3'Failed to write .LONGNAME extended attribute'+^M^C'(RC=%d)',
        @Result_, mfError or mfOKButton);
    end
  else if Result_ <> 48 then
    {JO: ошибку 48, которая в оси зарезервирована      }
    {    мы используем, когда DosQueryPathInfo выдаёт  }
    {    fst4.cbList (величина списка EA) равным нулю, }
    {    что строго говоря не ошибка, но является      }
    {    поводом прервать дальнейшую работу с EA , т.к.}
    {    наблюдается на дисках, не поддерживающих EA   }
    MessageBox
      (#3'Failed to read .LONGNAME extended attribute'+^M^C'(RC=%d)',
       @Result_, mfError or mfOKButton)
  else
    MessageBox(GetString(dlOperationNotValidForDdrive), nil, mfOKButton);
  end { SetEALongname };

{AK155}
function GetFSString(Drive: Char): String;
  var
    BufLen: Word;
    FSQb: pFSQBuffer2;
    DrvName: String[3];
    Ordinal: SmallWord;
    name: PChar;
    rc: Word;
    {DiskSize  : Word;}
  begin
  GetFSString := '';
  BufLen := 100;
  GetMem(FSQb, BufLen);
  DrvName := Drive+':'#0;
  Ordinal := 0;
  rc := DosQueryFSAttach(@DrvName[1], Ordinal, fsail_QueryName, FSQb,
       BufLen);
  if rc = 0 then
    with FSQb^ do
      begin
      name := szName+cbName+1;
      GetFSString := StrPas(name);
      end;

  FreeMem(FSQb, 100);
  end { GetFSString };
{/AK155}

const
  DEVLEN = 8;
type
  use_info_0 = record
    ui0_local: array [0..DEVLEN] of char;
    ui0_pad_1: char;
    ui0_remote: array [0..17] of byte;
    end;

function GetShare(Drive: Char): string;
  const
    DrivePath: array[0..2] of char = 'C:'#0;
  var
    buf: use_info_0;
    pulBytesAvail: ULONG;
  begin
  Result := '';
  if @Net32UseGetInfo <> nil then
    begin
    DrivePath[0] := Drive;
    Net32UseGetInfo(nil, @DrivePath, 0,
                 buf, SizeOf(buf), pulBytesAvail);
    if pulBytesAvail <> 0 then
      Result := StrPas(PChar(@buf.ui0_remote[SizeOf(buf)+4-pulBytesAvail]));
    end;
  end;

function GetSubst(Drive: Char): string; {AK155}
  begin
  Result := '';
  end;

{JO}
function GetDriveTypeNew(Drive: Char): TDrvTypeNew; {<fltl.001>}
var
  RC : Word;

  ParmRec     : record  // Input parameter record
    ComInfo   : Byte;
    DrvUnit   : Byte;
  end;

  ParmLen     : ULong;  // Parameter length in bytes

  DataRec     : record
    BytesPerSector    : UShort; // BIOS Parameter Block (BPB) structure
    SectorsPerCluster : Byte;
    ReservedSectors   : UShort;
    NumberOfFATs      : Byte;
    RootDirEntries    : UShort;
    TotalSectors      : UShort;
    MediaDescriptor   : Byte;
    SectorsPerFAT     : UShort;
    SectorsPerTrack   : UShort;
    NumberOfHeads     : UShort;
    HiddenSectors     : ULong;
    LargeTotalSectors : ULong;  // end of BPB structure
    Reserved : array [0..5] of Byte;
    CylNum    : UShort;
    DevType   : Byte;
    DevAttr   : UShort;
  end;

  DataLen     : ULong;  // Data length in bytes

begin
  Result := dtnInvalid;

  ParmLen := SizeOf(ParmRec);
  with ParmRec do
  begin
    ComInfo := 0;
    DrvUnit :=  Ord(Drive) - Ord('A');
  end;

  FillChar(DataRec, SizeOf(DataRec), 0);
  DataLen := SizeOf(DataRec);

  RC := DosDevIOCtl(
    -1,
    IOCTL_DISK,
    DSK_GETDEVICEPARAMS,
    @ParmRec,
    ParmLen,
    @ParmLen,

    @DataRec,
    Datalen,
    @DataLen);

  if RC = 50 then  // Network request not supported
    begin
    if GetShare(Drive) = '' then Result := dtnProgram
      else Result := dtnLAN;
    Exit;
    end
  else
  if RC > 0 then Exit
  else
    with DataRec do
      begin
      if ((DevAttr and 1) = 0) and (DevType <> 5) then
//JO: It seems that DosDevIOCtl returns 65535 value for the number
//    of cylinders if the device is CD.
//    Checked for Warp 3 with f/p 44 (internal revision 8.268)
//    and for Warp 4 with f/p 15 (internal revision 14.062_W4)
        if ((DevType = 7) and (CylNum = 65535)
             and (BytesPerSector = 2048)) then  // 2048 is a standard for CD
          begin
          Result := dtnCDRom;
          Exit;
          end
//JO: Note: all CD-RW also have type 8 ("Optical") when IBM UDF support
//          installed (OS2CDROM.DMD seems to be responsible),
//          but CD still have "Cylinders number" 65535,
//          whereas MO has more intelligent cylinders number.
        else if (DevType = 8) then
          begin
            if ((CylNum = 65535)
             and (BytesPerSector = 2048)) then  // 2048 is a standard for CD
              Result := dtnCDRom   {<vpsysos2.001>}
            else
              Result := dtnOptical;
          end
        else
          begin
          Result := dtnFloppy;
          Exit;
          end
      else Result := dtnHDD;
      end;
end;
{/JO}

function GetErrorText(ErrCode: Integer; var Msg: String): Boolean;
  var
    Len: Integer;
    RC: Integer;
  begin
  Result := False;
  RC := DosGetMessage(nil, 0, @Msg[1], 255, ErrCode, 'OSO001.MSG', Len);
  if RC <> 0 then
    Exit;
  while (Len <> 0) and (Msg[Len] in [#10, #13]) do
    Dec(Len);
  SetLength(Msg, Len);
  Result := Len <> 0;
  end;

begin
DosLoadModule(nil, 0, 'NETAPI32.DLL', hNetapi32);
if hNetapi32 <> 0 then
  DosQueryProcAddr(
    hNetapi32, {DLL module handle}
    0, {function ordinal value}
    'Net32UseGetInfo', {function name}
    @Net32UseGetInfo {address of function pointer}
    );
end.
