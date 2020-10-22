{ Win32-specific file tools unit by J.Osadtchiy (JO), A.Kozlov (Cat)}
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
procedure CopyEAs(FFromName, FToName: String);
{$IFDEF WIN32}
procedure CopySAs(FFromName, FToName: String);
{$ENDIF}

function GetFileAges(S: ShortString;
     var Age_LWr, Age_Cr, Age_LAc: LongInt): LongInt;
{JO: возвращает время и дату последней модификации (Age_LWr),                   }
{    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
{    файла или каталога по полному пути (S), принимает значение кода ошибки     }

function SetFileAges(S: ShortString; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
{JO: устанавливает время и дату последней модификации (Age_LWr),                }
{    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
{    файла или каталога по полному пути (S), принимает значение кода ошибки     }

procedure GetSerFileSys(Drive: Char; var SerialNo: Longint;
  var VolLab, FileSys: String);

function GetFSString(Dr: Char): String; {JO}
function GetShare(Drive: Char): string; {AK155}
function GetSubst(Drive: Char): string; {AK155}
function GetDriveTypeNew(Drive: Char): TDrvTypeNew; {JO} {<fltl.001>}
//JO: Функция, которую следует использовать в исходниках DN/2
//    вместо неудачной штатной для VP RTL функции SysGetDriveType,
//    в отличие от которой данная функция определяет тип диска
//    никогда к нему не обращаясь - используя DosDevIOCtl и не
//    делая никаких проверок файловой системы

function GetErrorText(ErrCode: Integer; var Msg: String): Boolean;

implementation

uses
  Windows, Strings, Lfn
  ;

function GetBytesPerCluster(Path: PChar): LongInt;
  var
    RootPath: array[0..3] of Char;
    RootPtr: PChar;
    SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters: DWord;
    i: Longint;
    c: Char;
  begin
(*  i := GetRootStart+1;
  c :=
  Buffer^[i] := #0; // получается что-то вроде \\server\share\ или C:\
  RootPtr := nil;
  if Drive > 0 then
    begin
    RootPath[0] := Char(Drive+(Ord('A')-1));
    RootPath[1] := ':';
    RootPath[2] := '\';
    RootPath[3] := #0;
    RootPtr := RootPath;
    end;
*)
  if GetDiskFreeSpace(Path, SectorsPerCluster, BytesPerSector,
       FreeClusters, TotalClusters)
  then
    GetBytesPerCluster := SectorsPerCluster*BytesPerSector
  else
    GetBytesPerCluster := 0;
  end;

{Cat}
type
  PFullEAInformation = ^TFullEAInformation;
  TFullEAInformation = packed record
    NextEntryOffset: DWord;
    Flags: Byte;
    EaNameLength: Byte;
    EaValueLength: Word;
    EaName: array[0..0] of Char;
    end;

  {*** copy extended attributes ***}
procedure CopyEAs(FFromName, FToName: String);
  var
    Size: DWord;
    Ptr: Pointer;
    hFileSource: Handle;
    StreamId: TWin32StreamId;
    StreamSize: DWord;
    dwBytesWritten: DWord;
    lpContext: Pointer;
    sl, sh: DWord;
  begin
  FFromName := FFromName+#0;
  FToName := FToName+#0;

  (*** пока не работает
  hFileSource := CreateFile(@FFromName[1], FILE_READ_EA,
                            FILE_SHARE_READ or FILE_SHARE_WRITE,
                            nil, OPEN_EXISTING, 0, 0);
  if hFileSource <> INVALID_HANDLE_VALUE then
    begin
      Size := 0;
      lpContext := nil;
      StreamSize := SizeOf(TWin32StreamId) - SizeOf(WChar);
      while BackupRead(hFileSource, @StreamId,
                       StreamSize, dwBytesWritten,
                       False, False, lpContext) do
        begin
          {no more Stream IDs?}
          if dwBytesWritten = 0 then
            Break;

          {skip StreamName}
          if StreamId.dwStreamNameSize <> 0 then
            begin
              GetMem(Ptr, StreamId.dwStreamNameSize);
              if Ptr = nil then
                Break;
              if not BackupRead(hFileSource, Ptr,
                                StreamId.dwStreamNameSize, dwBytesWritten,
                                False, False, lpContext) then
                begin
                  FreeMem(Ptr);
                  Break;
                end;
              FreeMem(Ptr);
            end;

          {is it EA stream?}
          if StreamId.dwStreamId = BACKUP_EA_DATA then
            begin
              GetMem(Ptr, StreamId.Size.LowPart);
              if Ptr = nil then
                Break;
              if not BackupRead(hFileSource, Ptr,
                                StreamId.Size.LowPart, dwBytesWritten,
                                False, False, lpContext) then {EA read error}
                begin
                  FreeMem(Ptr);
                  Break;
                end;
              Size := StreamId.Size.LowPart;
              Break;
            end;

          {skip current stream}
          if not BackupSeek(hFileSource,
                            StreamId.Size.LowPart, StreamId.Size.HighPart,
                            sl, sh, lpContext) then
            Break;
        end;

      {free context}
      BackupRead(hFileSource, nil,
                 0, dwBytesWritten,
                 True, False, lpContext);
      CloseHandle(hFileSource);

      {write EAs}
      if Size > 0 then
        begin
          hFileSource := CreateFile(@FToName[1], FILE_WRITE_EA,
                                    FILE_SHARE_READ or FILE_SHARE_WRITE,
                                    nil, OPEN_EXISTING, 0, 0);
          if hFileSource <> INVALID_HANDLE_VALUE then
            begin
              lpContext := nil;
              StreamSize := SizeOf(TWin32StreamId) - SizeOf(WChar);
              StreamId.dwStreamId := BACKUP_EA_DATA;
              StreamId.dwStreamAttributes := 0;
              StreamId.Size.HighPart := 0;
              StreamId.Size.LowPart := Size;
              StreamId.dwStreamNameSize := 0;

              if BackupWrite(hFileSource, @StreamId,
                             StreamSize, dwBytesWritten,
                             False, False, lpContext) then
                if BackupWrite(hFileSource, Ptr,
                               Size, dwBytesWritten,
                               False, False, lpContext) then
                  {success!};

              {free context}
              BackupRead(hFileSource, nil,
                         0, dwBytesWritten,
                         True, False, lpContext);
              CloseHandle(hFileSource);
            end;

          FreeMem(Ptr);
        end;
    end;
  ***)
  end { CopyEAs };

{*** copy security attributes ***}
procedure CopySAs(FFromName, FToName: String);
  var
    Size: DWord;
    Ptr: Pointer;
  begin
  FFromName := FFromName+#0;
  FToName := FToName+#0;

  if not GetFileSecurity(@FFromName[1], DACL_Security_Information, nil,
       0, Size)
  then
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then
      begin
      GetMem(Ptr, Size);
      if GetFileSecurity(@FFromName[1], DACL_Security_Information, Ptr,
           Size, Size)
      then
        SetFileSecurity(@FToName[1], DACL_Security_Information, Ptr);
      FreeMem(Ptr, Size);
      end;
  end;
{/Cat}

{JO}
type
  TDateTimeRec = record
    FTime, FDate: SmallWord;
    end;

function SetResult(Success: Boolean): LongInt;
  begin
  SetResult := 0;
  if not Success then
    SetResult := GetLastError;
  end;

function GetFileAges(S: ShortString;
     var Age_LWr, Age_Cr, Age_LAc: LongInt): LongInt;
  var
    LocalFileTime_LWr,
    LocalFileTime_Cr,
    LocalFileTime_LAc: TFileTime;
    FindData: TWin32FindData;
    SH: THandle;
  begin
  S[Length(S)+1] := #0;
  SH := FindFirstFile(@S[1], FindData);
  if SH = invalid_Handle_Value then
    begin
    Result := GetLastError;
    Exit;
    end;
  FindClose(SH);
  with FindData do
    Result := SetResult(
        FileTimeToLocalFileTime(ftLastWriteTime, LocalFileTime_LWr) and
        FileTimeToDosDateTime(LocalFileTime_LWr,
           TDateTimeRec(Age_LWr).FDate, TDateTimeRec(Age_LWr).FTime) and
        FileTimeToLocalFileTime(ftCreationTime, LocalFileTime_Cr) and
        FileTimeToDosDateTime(LocalFileTime_Cr,
           TDateTimeRec(Age_Cr).FDate, TDateTimeRec(Age_Cr).FTime) and
        FileTimeToLocalFileTime(ftLastAccessTime, LocalFileTime_LAc));

  if not FileTimeToDosDateTime(LocalFileTime_LAc,
           TDateTimeRec(Age_LAc).FDate, TDateTimeRec(Age_LAc).FTime)
  then
    begin // файловая система не поддержвает время последнего доступа
    TDateTimeRec(Age_LAc).FDate := 0;
    TDateTimeRec(Age_LAc).FTime := 0;
    end;
  end { GetFileAges };

function SetFileAges(S: ShortString; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
  var
    Handle: LongInt;
    LocalFileTime_LWr, FileTime_LWr,
    LocalFileTime_Cr, FileTime_Cr,
    LocalFileTime_LAc, FileTime_LAc: TFileTime;
  begin
  S[Length(S)+1] := #0;
  Handle := CreateFile(@S[1], GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE,
      nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if Handle = invalid_Handle_Value then
    Result := 1
  else
    SetFileAges := SetResult(
        DosDateTimeToFileTime(TDateTimeRec(Age_LWr).FDate,
             TDateTimeRec(Age_LWr).FTime, LocalFileTime_LWr) and
        LocalFileTimeToFileTime(LocalFileTime_LWr, FileTime_LWr) and
        DosDateTimeToFileTime(TDateTimeRec(Age_Cr).FDate,
             TDateTimeRec(Age_Cr).FTime, LocalFileTime_Cr) and
        LocalFileTimeToFileTime(LocalFileTime_Cr, FileTime_Cr) and
        DosDateTimeToFileTime(TDateTimeRec(Age_LAc).FDate,
             TDateTimeRec(Age_LAc).FTime, LocalFileTime_LAc) and
        LocalFileTimeToFileTime(LocalFileTime_LAc, FileTime_LAc) and
        SetFileTime(Handle, @FileTime_Cr, @FileTime_LAc, @FileTime_LWr));
  CloseHandle(Handle);
  end { SetFileAges };

var
  VolName: array[0..11] of Char;
  SerialNumber: Longint;

procedure GetSerFileSys(Drive: Char; var SerialNo: Longint;
    var VolLab, FileSys: String);
  begin
  FileSys := GetFSString(Drive);
  SerialNo := SerialNumber;
  VolLab := StrPas(VolName);
  end;

function GetFSString(Dr: Char): String;
  const
    Root: array[0..4] of Char = 'C:\'#0;
  var
    FSName: array[0..255] of Char;
    MaxLength: LongInt;
    FSFlags: LongInt;
  begin
  GetFSString := '';
  Root[0] := Dr;
  if GetVolumeInformation(Root, @VolName, 12, @SerialNumber,
    MaxLength, FSFlags, FSName, SizeOf(FSName))
  then
    GetFSString := StrPas(FSName)
  else
    begin
    SerialNumber := 0;
    VolName[0] := #0;
    end;
  end;

{/JO}

  const
    LocDrive: array[0..2] of char = 'C:'#0;

function GetShare(Drive: Char): string;
  type
    PREMOTE_NAME_INFO = ^TREMOTE_NAME_INFO ;
    TREMOTE_NAME_INFO = record
      lpUniversalName, lpConnectionName, lpRemainingPath: PChar;
      end;
  var
    Buf: array[0..500] of Char;
    RC: Longint;
    lNetPath: DWORD;
  begin
  Result := '';
  LocDrive[0] := Drive;
  lNetPath := SizeOf(Buf);
  RC := WNetGetUniversalName(
    @LocDrive,  // path for network resource
    REMOTE_NAME_INFO_LEVEL,    // level of information
    @Buf,      // name buffer
    lNetPath  // size of buffer
  );

  if RC = 0 then
    Result := StrPas(PREMOTE_NAME_INFO(@Buf)^.lpConnectionName);
  end;

function GetSubst(Drive: Char): string; {AK155}
  const
    Root: Array[0..3] of char = 'C:'#0;
  begin
  Root[0] := Drive;
  SetLength(Result, QueryDosDevice(Root, @Result[1], 255));
  if Copy(Result, 1, 4) = '\??\' then
    Result := Copy(Result, 5, 255)
  else
    Result := '';
  end;

function GetDriveTypeNew(Drive: Char): TDrvTypeNew; {<fltl.001>}
const
  Root: Array[0..4] of char = 'C:\'#0;

begin
  Root[0] := Drive;
  Result := dtnInvalid;
  case GetDriveType(Root) of
    Drive_Fixed:
      begin
      if GetSubst(Drive) <> '' then
        Result := dtnSubst
      else
        Result := dtnHDD;
      end;
    Drive_Removable:
      Result := dtnFloppy;
    Drive_CDRom:
      Result := dtnCDROM;
    Drive_Remote:
      begin
      if GetShare(Drive) = '' then
             Result := dtnProgram
        else Result := dtnLAN;
      end;
    0, 1:
      Result := dtnInvalid;
    drive_RamDisk:
      Result := dtRamDisk

    else
      Result := dtnUnknown;
  end;
end;

function GetErrorText(ErrCode: Integer; var Msg: String): Boolean;
  var
    Len: Integer;
    RC: Integer;
  begin
  Result := False;
  Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
      FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, ErrCode, 0, @Msg[1], 255, nil);
  SetLength(Msg, Len);
  Result := Len <> 0;
  if Result then
    AnsiToOemBuff(@Msg[1], @Msg[1], Length(Msg));
  end;

begin
end.
