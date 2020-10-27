// |---------------------------------------------------------|
// |                                                         |
// |     Virtual Pascal Runtime Library.  Version 2.1.       |
// |     System Interface Layer for Linux                    |
// |     ----------------------------------------------------|
// |     Copyright (C) 1995-2003 vpascal.com                 |
// |                                                         |
// |---------------------------------------------------------|

{   $Define KbdDebug} // to show unknown key/mouse events

var
  // Thread handle of keyboard thread
  KbdThreadID: LongInt = 0;

function IntToStr(I: Integer): string;
begin
  Str(I, Result);
end;

procedure TrmDone;    forward;
procedure SuspendTrm; forward;
procedure ResumeTrm;  forward;

//--------------[ DATE/TIME CONVERSION FUNCTIONS ]--------------------

const
  // The number of seconds in a day.
  SecsPerDay  = 24 * 60 * 60;

  // The number of days from (assumed) date 31-Dec-0000 to UTC base
  // day 01-Jan-1970.
  UTCBaseDay  = 719163;

  // The number of days that have passed since 01-Jan to the beginning
  // of a given month. Two variants, one for non-leap years, the other
  // for leap years.
  DaysPassed: array[False..True, 1..13] of Integer =
    ((0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
     (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366));

type
  // A record holding all the fields needed for a date/time
  // conversion.
  TDateTime = record
    Year,Month,Day,DayOfWeek,Hour,Min,Sec: LongInt;
  end;

function CP866ToUtf8(var Text: string): boolean;
var
    InString, OutString, b: string;
    i, Len: integer;
    c: byte;
    a: Char;
begin
    Result := true;
    InString:=Text; OutString:='';
    Len:=Length(InString);
    try
        for i:=1 to Len do
        begin
            a:=InString[i];
            c:=Ord(a);
            if (c < 128) then
                b := a
            else
            case c of
                (*
                <?php
                $cnt = 0; for ($i = 128; $i<256; $i++) {
                    $c = chr($i); $out = iconv("CP866", "UTF-8", $c);
                    echo "$i: b := "; $array = str_split($out);
                    foreach ($array as $char) { echo "#" . ord($char); } echo "; ";
                    $cnt++; if ($cnt > 3) { $cnt = 0; echo "\n"; }
                }
                *)
                128: b := #208#144; 129: b := #208#145; 130: b := #208#146; 131: b := #208#147;
                132: b := #208#148; 133: b := #208#149; 134: b := #208#150; 135: b := #208#151;
                136: b := #208#152; 137: b := #208#153; 138: b := #208#154; 139: b := #208#155;
                140: b := #208#156; 141: b := #208#157; 142: b := #208#158; 143: b := #208#159;
                144: b := #208#160; 145: b := #208#161; 146: b := #208#162; 147: b := #208#163;
                148: b := #208#164; 149: b := #208#165; 150: b := #208#166; 151: b := #208#167;
                152: b := #208#168; 153: b := #208#169; 154: b := #208#170; 155: b := #208#171;
                156: b := #208#172; 157: b := #208#173; 158: b := #208#174; 159: b := #208#175;
                160: b := #208#176; 161: b := #208#177; 162: b := #208#178; 163: b := #208#179;
                164: b := #208#180; 165: b := #208#181; 166: b := #208#182; 167: b := #208#183;
                168: b := #208#184; 169: b := #208#185; 170: b := #208#186; 171: b := #208#187;
                172: b := #208#188; 173: b := #208#189; 174: b := #208#190; 175: b := #208#191;
                176: b := #226#150#145; 177: b := #226#150#146; 178: b := #226#150#147; 179: b := #226#148#130;
                180: b := #226#148#164; 181: b := #226#149#161; 182: b := #226#149#162; 183: b := #226#149#150;
                184: b := #226#149#149; 185: b := #226#149#163; 186: b := #226#149#145; 187: b := #226#149#151;
                188: b := #226#149#157; 189: b := #226#149#156; 190: b := #226#149#155; 191: b := #226#148#144;
                192: b := #226#148#148; 193: b := #226#148#180; 194: b := #226#148#172; 195: b := #226#148#156;
                196: b := #226#148#128; 197: b := #226#148#188; 198: b := #226#149#158; 199: b := #226#149#159;
                200: b := #226#149#154; 201: b := #226#149#148; 202: b := #226#149#169; 203: b := #226#149#166;
                204: b := #226#149#160; 205: b := #226#149#144; 206: b := #226#149#172; 207: b := #226#149#167;
                208: b := #226#149#168; 209: b := #226#149#164; 210: b := #226#149#165; 211: b := #226#149#153;
                212: b := #226#149#152; 213: b := #226#149#146; 214: b := #226#149#147; 215: b := #226#149#171;
                216: b := #226#149#170; 217: b := #226#148#152; 218: b := #226#148#140; 219: b := #226#150#136;
                220: b := #226#150#132; 221: b := #226#150#140; 222: b := #226#150#144; 223: b := #226#150#128;
                224: b := #209#128; 225: b := #209#129; 226: b := #209#130; 227: b := #209#131;
                228: b := #209#132; 229: b := #209#133; 230: b := #209#134; 231: b := #209#135;
                232: b := #209#136; 233: b := #209#137; 234: b := #209#138; 235: b := #209#139;
                236: b := #209#140; 237: b := #209#141; 238: b := #209#142; 239: b := #209#143;
                240: b := #208#129; 241: b := #209#145; 242: b := #208#132; 243: b := #209#148;
                244: b := #208#135; 245: b := #209#151; 246: b := #208#142; 247: b := #209#158;
                248: b := #194#176; 249: b := #226#136#153; 250: b := #194#183; 251: b := #226#136#154;
                252: b := #226#132#150; 253: b := #194#164; 254: b := #226#150#160; 255: b := #194#160;
            end;
            OutString:=OutString+b
        end;
        Text:=OutString;
        //WriteLn('<: ' + InString);
        //WriteLn('>: ' + OutString);
    except
        Result:=false;
    end;
end;

// Packs a TDateTime record to a single UTC date/time value. No
// timezone adjustment is performed.
function PackUTCTime(DateTime: TDateTime): LongInt;
var
  Date, Time: LongInt;
begin
  with DateTime do
  begin
    if Month > 2 then
      Dec(Month, 3)
    else
      begin
        Inc (Month, 9);
        Dec (Year);
      end;

    Date := (146097 * (Year div 100)) shr 2
          + (1461 * (Year mod 100)) shr 2
          + (153 * Month + 2) div 5 + Day - 306;

    Time := (Hour * 60 + Min) * 60 + Sec;

    Result := (Date - UTCBaseDay) * SecsPerDay + Time;
  end;
end;

// Unpacks a UTC date/time value to a TDateTime record. No timezone
// adjustment is performed.
function UnpackUTCTime(Value: LongInt): TDateTime;
const
  Days400 = 146097;
  Days100 = 36524;
  Days4   = 1461;
var
  Count, DayNum: LongInt;
  LeapYear: Boolean;
begin
  with Result do
  begin
    DayNum := Value div SecsPerDay + UTCBaseDay;

    DayOfWeek := DayNum mod 7;

    Year := 1;

    while DayNum > Days400 do
    begin
      Inc(Year, 400);
      Dec(DayNum, Days400);
    end;

    Count := 0;
    while (DayNum > Days100) and (Count < 3) do
    begin
      Inc(Year, 100);
      Dec(DayNum, Days100);
      Inc(Count);
    end;

    while DayNum > Days4 do
    begin
      Inc(Year, 4);
      Dec(DayNum, Days4);
    end;

    Count := 0;
    while (DayNum > 365) and (Count < 3) do
    begin
      Inc(Year);
      Dec(DayNum, 365);
      Inc(Count);
    end;

    LeapYear := (Year mod 4 = 0) and not (Year mod 100 = 0) or (Year mod 400 = 0);

    Month := 0;
    while DaysPassed[LeapYear, Month + 1] < DayNum do Inc(Month);

    Day := DayNum - DaysPassed[LeapYear, Month];

    Sec := Value mod SecsPerDay;
    Min := Sec div 60;
    Sec := Sec mod 60;
    Hour := Min div 60;
    Min := Min mod 60;
  end;
end;

// Packs a TDateTime record to a DOS time value. Taken from the DOS
// unit.
procedure PackDosTime(var T: TDateTime; var P: Longint);
var
  FDateTime: TDateTimeRec absolute P;
begin
  with T,FDateTime do
  begin
    FDate := (Year - 1980) shl 9 + Month shl 5 + Day;
    FTime := Hour shl 11 + Min shl 5 + (Sec div 2);
  end;
end;

// Unpacks a DOS time value to a TDateTime record. Taken from the DOS
// unit.
procedure UnpackDosTime(P: Longint; var T: TDateTime);
var
  FDateTime: TDateTimeRec absolute P;
begin
  with T,FDateTime do
  begin
    Year  := (FDate and $FE00) shr 9 + 1980;
    Month := (FDate and $01E0) shr 5;
    Day   := (FDate and $001F);
    Hour  := (FTime and $F800) shr 11;
    Min   := (FTime and $07E0) shr 5;
    Sec   := (FTime and $001F) * 2;
  end;
end;

//--------------[ FILENAME CONVERSION FUNCTIONS ]---------------------


type
  // A buffer for file names. TFileNameBuf
  TFileNameBuf = array[0..511] of Char;

// Converts file name given given in Source according to the source
// and destination file systems given in SourceFS and DestFS. The
// result is written to the Dest buffer, and Dest is returned. In
// case no conversion is necessary, the function returns Source
// and the Dest buffer stays unchanged.
function SysConvertFileName(Dest, Source: PChar; DestFS, SourceFS: TFileSystem): PChar;
var
  SourceChar, DestChar: Char;
  P: PChar;
begin
  if DestFS = SourceFS then
    begin
      Result := Source;
      Exit;
    end;

  if DestFS = fsUnix then
    begin
      if (Source[0] <> #0) and (Source[1] = ':') then
        Inc(Source, 2);

      SourceChar := '\';
      DestChar := '/';
    end
  else
    begin
      SourceChar := '/';
      DestChar := '\';
    end;

  StrCopy(Dest, Source);

  if SourceFS = fsDosUpper then
    StrUpper(Dest)
  else if SourceFS = fsDosLower then
    StrLower(Dest);

  P := StrScan(Dest, SourceChar);
  while P <> nil do
    begin
      P^ := DestChar;
      P := StrScan(P, SourceChar);
    end;

  Result := Dest;
end;

// Checks whether a file name is valid for the given file system.
function SysIsValidFileName(FileName: PChar; FileSystem: TFileSystem): Boolean;
var
  P: PChar;
begin
  Result := False;

  P := FileName;
  while P[0] <> #0 do
    begin
      case P[0] of
        '\', ':': if FileSystem = fsUnix then Exit;
        '/'     : if FileSystem <> fsUnix then Exit;
        'a'..'z': if (FileSystem = fsDosUpper) and (P[1] <> ':') then Exit;
        'A'..'Z': if (FileSystem = fsDosLower) and (P[1] <> ':') then Exit;
      end;
      Inc(P);
    end;

  Result := True;
end;

//--------------[ OTHER HELPER FUNCTIONS ]----------------------------

procedure Unimplemented(const S: string);
  var
    errormsg:array[0..200] of char;
  begin
    StrCopy(errormsg,'Fatal error: Function "');
    StrPCopy(StrEnd(errormsg),S);
    StrCat(errormsg,'" not implemented yet.');
    SysDisplayConsoleError(false,'VpSysLnx.pas',errormsg);
    LnxExit(255);
  end;

procedure Internal_Error(const S: string);
  var
    errormsg:array[0..200] of char;
  begin
    StrPCopy(errormsg,S);
    SysDisplayConsoleError(false,'Internal error in VpSysLnx.pas',errormsg);
    LnxExit(255);
  end;


//--------------[ BASIC FILE FUNCTIONS ]------------------------------

// Please refer to the online help for VpSysLow for details

function SysFileStdIn: Longint;
begin
  Result := STDIN_FILENO;
end;

function SysFileStdOut: Longint;
begin
  Result := STDOUT_FILENO;
end;

function SysFileStdErr: Longint;
begin
  Result := STDERR_FILENO;
end;

function SysFileOpen_Create(Open: Boolean;FileName: PChar; Mode,Attr,Action: Longint; var Handle: Longint): Longint;
var
  Buffer: TFileNameBuf;
  LnxMode, LnxAttr: Longint;
begin
  FileName := SysConvertFileName(Buffer, FileName, fsUnix, FileSystem);

  if Open then
    begin
      case Action of
        Open_FailIfNew:          LnxMode := 0;
        Open_CreateIfNew:        LnxMode := O_CREAT;
        Open_TruncateIfExists:   LnxMode := O_TRUNC;
      end;
    end
  else
    begin
      case Action of
        Create_FailIfExists:     LnxMode := O_CREAT or O_EXCL;
        Create_TruncateIfExists: LnxMode := O_CREAT or O_TRUNC;
      end;
    end;

  LnxMode := LnxMode or Mode and O_ACCMODE;
  LnxAttr := S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;

  if Attr and 1 = 1 then
    LnxAttr := LnxAttr and not (S_IWUSR or S_IWGRP or S_IWOTH);

  Result := LnxOpen(FileName, LnxMode, LnxAttr);
  if Result < 0 then
    Result := -Result
  else
    begin
      Handle := Result;
      Result := 0;
    end;
end;

function SysFileOpen(FileName: PChar; Mode: Longint; var Handle: Longint): Longint;
var
  Buffer: TFileNameBuf;
  LnxMode: Longint;
begin
  FileName := SysConvertFileName(Buffer, FileName, fsUnix, FileSystem);

  LnxMode := Mode and O_ACCMODE;

  Result := LnxOpen(FileName, LnxMode, 0);
  if Result < 0 then
    Result := -Result
  else
    begin
      Handle := Result;
      Result := 0;
    end;
end;

function SysFileCreate(FileName: PChar; Mode,Attr: Longint; var Handle: Longint): Longint;
var
  Buffer: TFileNameBuf;
  LnxMode, LnxAttr: Longint;
begin
  FileName := SysConvertFileName(Buffer, FileName, fsUnix, FileSystem);

  LnxMode := Mode and O_ACCMODE;
  LnxAttr := S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;

  if Attr and 1 = 1 then
    LnxAttr := LnxAttr and not (S_IWUSR or S_IWGRP or S_IWOTH);

  Result := LnxCreat(FileName, LnxMode or LnxAttr);
  if Result < 0 then
    Result := -Result
  else
    begin
      Handle := Result;
      Result := 0;
    end;
end;

function SysFileCopy(_Old, _New: PChar; _Overwrite: Boolean): Boolean;
var
  Attr, Src, Dst, Error, Actual: Longint;
  Buffer: array[0..1023] of Char;
begin
  Result := False;

  SysGetFileAttr(_Old, Attr);

  Error := SysFileOpen(_Old, Open_Access_ReadOnly, Src);
  if Error = 0 then
    begin
      if _Overwrite then
        Error := SysFileCreate(_New, Open_Access_ReadWrite, Attr, Dst)
      else
        Error := SysFileOpen_Create(False, _New, Open_Access_ReadWrite, Attr, 0, Dst);

      if Error = 0 then
        begin
          Actual := 1;
          while (Error = 0) and (Actual > 0) do
            begin
              Error := SysFileRead(Src, Buffer, SizeOf(Buffer), Actual);
              if Error = 0 then
                Error := SysFileWrite(Dst, Buffer, Actual, Actual);
            end;

          Result := SysFileClose(Dst) = 0;
        end
      else
        SysFileClose(Src);
    end;
end;

function SysFileSeek(Handle, Distance, Method: Longint; var Actual: Longint): Longint;
begin
  Result := LnxLSeek(Handle, Distance, Method);
  if Result < 0 then
    Result := -Result
  else
    begin
      Actual := Result;
      Result := 0;
    end;
end;

function SysFileRead(Handle: Longint; var Buffer; Count: Longint; var Actual: Longint): Longint;
begin
  Result := LnxRead(Handle, Buffer, Count);
  if Result < 0 then
    Result := -Result
  else
    begin
      Actual := Result;
      Result := 0;
    end;
end;

function SysFileWrite(Handle: Longint; const Buffer; Count: Longint; var Actual: Longint): Longint;
begin
  Result := LnxWrite(Handle, Buffer, Count);
  if Result < 0 then
    Result := -Result
  else
    begin
      Actual := Result;
      Result := 0;
    end;
end;

function SysFileSetSize(Handle,NewSize: Longint): Longint;
begin
  Result := -LnxFTruncate(Handle, NewSize);

end;

function SysFileClose(Handle: Longint): Longint;
begin
  Result := -LnxClose(Handle);
end;

function SysFileFlushBuffers(Handle: Longint): Longint;
begin
  Result := -LnxFSync(Handle);
end;

function SysFileDelete(FileName: PChar): Longint;
var
  Buffer: TFileNameBuf;
begin
  FileName := SysConvertFileName(@Buffer, FileName, fsUnix, FileSystem);

  Result := -LnxUnlink(FileName);
end;

function SysFileMove(OldName,NewName: PChar): Longint;
var
  OldBuffer, NewBuffer: TFileNameBuf;
begin
  OldName := SysConvertFileName(@OldBuffer, OldName, fsUnix, FileSystem);
  NewName := SysConvertFileName(@NewBuffer, NewName, fsUnix, FileSystem);

  Result := -LnxRename(OldName, NewName);
end;

function SysFileIsDevice(Handle: Longint): Longint;
var
  Stat: TStat;
begin
  Result := -LnxFStat(Handle, Stat);
    if Result = 0 then
    begin
      if Stat.{st_rdev}st_Mode and S_IFCHR <> 0 then
        Result := 1
      else if Stat.{st_rdev}st_Mode and S_IFIFO <> 0 then
        Result := 2;
    end;
end;

// Retrieve current directory via the proc file system
function GetCwdViaProc(Buffer: PChar): Longint;
begin
  Result := LnxReadLink('/proc/self/cwd', Buffer, SizeOf(TFileNameBuf) - 1);
  if Result > 0 then
    Buffer[Result] := #0;
end;

// Retrieve the current directory through FS
function GetCwdViaFS(Path: PChar): Longint;
var
  Root, This, RootDev, ThisDev: Longint;
  Temp, TempDev, Find, FindDev, Handle, Count: LongInt;
  Stat: TStat;
  DirEnt: TDirEnt;
  Name, Buffer: TFileNameBuf;
  MountPoint: Boolean;
  NameBeg: PChar;
begin
  Result := -1;

  // Get INode of root directory
  LnxStat('/', Stat);
  Root := Stat.st_Ino;
  RootDev := Stat.st_Dev;

  // Get INode of current directory
  LnxStat('.', Stat);

  This := Stat.st_Ino;
  ThisDev := Stat.st_Dev;

  Find := This;
  FindDev := ThisDev;

  // Initialze the buffers
  StrCopy(Path, '');
  StrCopy(@Name, '..');
  StrCopy(@Buffer, '/');

  { As long as the current directory is not the root  }
  { directory, we go one directory upwards and search }
  { for an entry whose INode is equal to the one of   }
  { our current directory.                            }
  while (This <> Root) or (ThisDev <> RootDev) do
    begin
      if SysFileOpen(@Name, OPEN_ACCESS_READONLY, Handle) = 0 then
        begin
          // Get stats of parent directory
          LnxFStat(Handle, Stat);
          Temp := Stat.st_Ino;
          TempDev := Stat.st_Dev;

          MountPoint := TempDev <> ThisDev;

          // Find INode of this directory in parent directory
          while LnxReadDir(Handle, DirEnt, 1) = 1 do
            begin
              if DirEnt.d_Name[0] = '.' then
                if (DirEnt.d_Name[1] = #0) or ((DirEnt.d_Name[1] = '.') and (DirEnt.d_Name[2] = #0)) then
                  Continue;

              if MountPoint or (DirEnt.d_Ino = This) then
                begin
                  if MountPoint then
                    begin
                      NameBeg := StrECopy(StrECopy(@Buffer[1], @Name), '/');
                      StrCopy(StrECopy(NameBeg, @DirEnt.d_Name), Path);

                      if LnxStat(@Buffer, Stat) <> 0 then Continue;
                      if (Stat.st_INo <> Find) or (Stat.st_Dev <> FindDev) then Continue;

                      StrCopy(@Buffer[1], NameBeg);
                    end
                  else
                    StrCopy(StrECopy(@Buffer[1], @DirEnt.d_Name), Path);

                  StrCopy(Path, @Buffer);

                  This := Temp;
                  ThisDev := TempDev;

                  Break;
                end;
              end;
          SysFileClose(Handle);

          if ThisDev <> TempDev then
            begin
              // Not found
              StrCopy(Path, '');
              Exit;
            end;
        end
      else
        begin
          // File could not be opened
          StrCopy(Path, '');
          Exit;
        end;

      StrCat(@Name, '/..');
    end; // While

  if StrLen(Path) = 0 then
    StrCopy(Path, '/');

  Result := StrLen(Path);
end;

function SysDirGetCurrent(Drive: Longint; Path: PChar): Longint;
var
  Buffer: TFileNameBuf;
begin
  Buffer[0] := 'c';
  Buffer[1] := ':';

  if (Drive <> 0) and (Drive <> 3) then
  begin
    Result := -1;
    Exit;
  end;

  if GetCwdViaProc(@Buffer[2]) < 1 then
    if GetCwdViaFs(@Buffer[2]) < 1 then
    begin
      Result := -1;
      Exit;
    end;

  if FileSystem = fsUnix then
    begin
      if SysConvertFileName(Path, @Buffer[2], FileSystem, fsUnix) <> Path then
        StrCopy(Path, @Buffer[2]);
    end
  else
    begin
      if SysConvertFileName(Path, @Buffer, FileSystem, fsUnix) <> Path then
        StrCopy(Path, @Buffer);
    end;

  Result := 0;
end;

function SysDirSetCurrent(Path: PChar): Longint;
var
  Buffer: TFileNameBuf;
begin
  Path := SysConvertFileName(@Buffer, Path, fsUnix, FileSystem);

  Result := LnxChDir(Path);
end;

function SysDirCreate(Path: PChar): Longint;
var
  Buffer: TFileNameBuf;
begin
  Path := SysConvertFileName(@Buffer, Path, fsUnix, FileSystem);

  Result := LnxMkDir(Path, S_IRWXU or S_IRWXG or S_IRWXO);
end;

function SysDirDelete(Path: PChar): Longint;
var
  Buffer: TFileNameBuf;
begin
  Path := SysConvertFileName(@Buffer, Path, fsUnix, FileSystem);

  Result := LnxRmDir(Path);
end;

//--------------[ MEMORY MANAGEMENT ]---------------------------------

// Memory management stuff. Since the Linux munmap call needs to
// know the size of the block to be disposed, but Virtual Pascal
// doesn't pass it to the functions (OS/2 and NT don't need this),
// we have to store the size of each kernel-allocated memory block
// in a special list. This is quite some unnecessary overhead in
// memory management, but at least it should work.

type
  (**
   * An entry of the memory block list.
  *)

  PMemBlock = ^TMemBlock;
  TMemBlock = record
    FAddress: Pointer;
    FSize:    LongInt;
  end;

  (*
   * The list of memory blocks.
  *)

  PMemBlockList = ^TMemBlockList;
  TMemBlockList = array[0..MaxInt div SizeOf(TMemBlock) - 1] of TMemBlock;

var
  (**
   * Points to the list of currently
   * allocated memory blocks.
  *)

  MemBlockList:  PMemBlockList = nil;

  (**
   * Holds the number of currently
   * allocated memory blocks.
  *)

  MemBlockCount: LongInt = 0;

  (**
   * Holds the current size of the
   * memory block list.
  *)

  MemBlockLimit: LongInt = 0;

const
  (**
   * The growth of the memory block
   * list. 512 * 8 bytes is exactly
   * one page.
  *)

  MemBlockDelta = 4096 div SizeOf(TMemBlock);

(**
 * Adds a block to the list of currrently
 * allocated memory blocks.
  *)

procedure SysMemAddBlock(Address: Pointer; Size: LongInt);
var
  TmpList: Pointer;
  TmpLimit: LongInt;
begin
  if MemBlockCount = MemBlockLimit then
  begin
    TmpLimit := MemBlockLimit + MemBlockDelta;
    TmpList := LnxMMap(nil, TmpLimit * SizeOf(TMemBlock), HeapAllocFlags, MAP_ANON or MAP_COPY, 0, 0);

    if (LongInt(TmpList) >= -4095) and (LongInt(TmpList) <= 0) then
      Internal_Error('SysMemAddBlock: mmap failed.');

    if MemBlockLimit <> 0 then
    begin
      Move(MemBlockList^, TmpList^, MemBlockLimit * SizeOf(TMemBlock));
      if LnxMUnmap(MemBlockList, MemBlockLimit * SizeOf(TMemBlock)) <> 0 then
        Internal_Error('SysMemAddBlock: munmap failed.');
    end;

    MemBlockList := TmpList;
    MemBlockLimit := TmpLimit;
  end;

  with MemBlockList^[MemBlockCount] do
  begin
    FAddress := Address;
    FSize := Size;
  end;

  // Write('AddBlock(', MemBlockCount, ', ', LongInt(Address), ', ', Size, ')', #10);

  Inc(MemBlockCount);
//  Write(MemBlockCount, ' ');
end;

(**
 * Deletes a block from the list of currrently
 * allocated memory blocks. Returns its size.
  *)

function SysMemDeleteBlock(Address: Pointer): LongInt;
var
  I: LongInt;
begin
  I := MemBlockCount - 1;

  while (I <> -1) and (MemBlockList^[I].FAddress <> Address) do
    Dec(I);

  if I <> - 1 then
    begin
      Result := MemBlockList^[I].FSize;
      Move(MemBlockList^[I + 1], MemBlockList^[I], (MemBlockCount - I - 1) * SizeOf(TMemBlock));
      Dec(MemBlockCount);
    end
  else
    Internal_Error('SysMemDeleteBlock: block '+IntToStr(LongInt(Address))+' not found.');
end;

function SysMemAvail: Longint;
var
  Info: TSysInfo;
begin
  LnxSysInfo(Info);
  Result := Info.FreeRam + Info.FreeSwap;
end;

// by unxed, untested
function PhysMemAvail: Longint;
begin
  Result := SysMemAvail;
end;

function SysMemAlloc(Size,Flags: Longint; var MemPtr: Pointer): Longint;
begin
  Result := LongInt(LnxMMap(nil, Size, Flags, MAP_ANON or MAP_COPY, 0, 0));
  if (Result < -4095) or (Result > 0) then
  begin
    MemPtr := Pointer(Result);
    SysMemAddBlock(MemPtr, Size);
    Result := 0;
  end
  else
  begin
    Result := -Result;
    MemPtr := nil;
  end;
end;

function SysMemFree(MemPtr: Pointer): Longint;
begin
  Result := -LnxMUnmap(MemPtr, SysMemDeleteBlock(MemPtr));
end;

function SysSysMsCount: Longint;
var
  TimeVal: TTimeVal;
  TimeZone: TTimeZone;
begin
  if LnxGetTimeOfDay(TimeVal, TimeZone) = 0 then
    Result := TimeVal.tv_Sec*1000 + TimeVal.tv_USec div 1000
  else
    Result := 0;
end;

procedure SysSysSelToFlat(var P: Pointer);
begin
  // Nothing to do.
end;

procedure SysSysFlatToSel(var P: Pointer);
begin
  // Nothing to do.
end;

function SysCtrlSelfAppType: Longint;
begin
  // Hardcoded: Text mode
  Result := 2;
end;

//--------------[ THREAD MANAGEMENT ]---------------------------------

// Since Linux does not have thread IDs starting from 1, but
// assigns each thread a unique process ID instead, we need
// a mapping between TIDs and PIDs.

const
  MaxThread    = 256; // Maximum number of threads

  tsRunning    =   0; // Thread is up and running
  tsSuspended  =   1; // Thread has been suspended
  tsTerminated =   2; // Thread has (been) terminated

type
  PThreadInfo = ^TThreadInfo;
  TThreadInfo = record    // Thread information structure
    ExceptChain: Pointer; // Head of exception registration chain
    Stack:       Pointer; // Lower limit of stack
    StackLimit:  Pointer; // Upper limit of stack
    Handle:      LongInt; // One-based thread handle
    ThreadPid:   LongInt; // PID of thread itself
    ProcessPid:  LongInt; // PID of process to which thread belongs
    State:       LongInt; // State of thread
    TibSelector: LongInt; // Selector pointing to thread information block
  end;

var
  Threads: array[1..MaxThread] of PThreadInfo; // List of threads

  MainThread: TThreadInfo = ( // Thread info block for main thread
    ExceptChain: nil;
    Stack:       Pointer($C0000000);
    StackLimit:  Pointer($C0000000);
    Handle:      1;
    ThreadPid:   0;
    ProcessPid:  0;
    State:       tsRunning;
    TibSelector: 0
  );

  ThreadSemaphore: LongInt = 0; // Thread info list access semaphore

function SysGetProcessId: Longint;
begin
  Result := MainThread.ProcessPid;
end;

(**
 * Adds a thread, returns the info block or -1, if no more threads can
 * be created.
  *)

function AddThreadInfo(StackSize, Flags: LongInt): PThreadInfo;
var
  Index: LongInt;
begin
  Index := 1;

  while Index <= MaxThread do
  begin
    if Threads[Index] = nil then
    begin
      New(Result);
      Threads[Index] := Result;

      with Result do
      begin
        ExceptChain := nil;
        if StackSize <> 0 then
        begin
          GetMem(Stack, StackSize);
          StackLimit := Stack;
          Inc(LongInt(StackLimit), StackSize);
        end;
        Handle := Index;
        ThreadPid := 0;
        ProcessPid := LnxGetPid;
        State := Flags and 1;
        TibSelector := 0;
      end;

      Exit;
    end;
    Inc(Index);
  end;

  Result := nil;
end;

(**
 * Removes a thread.
  *)

procedure RemoveThreadInfo(Thread: PThreadInfo);
var
  Handle: LongInt;
begin
  if Thread.Stack <> nil then FreeMem(Thread.Stack);
  Handle := Thread.Handle;
  Dispose(Threads[Handle]);
  Threads[Handle] := nil;
end;

function GetThread(Handle: LongInt): PThreadInfo;
begin
  SysSysWaitSem(ThreadSemaphore);

  if (Handle < 1) or (Handle > MaxThread) or (Threads[Handle] = nil) then
    Result := nil
  else
    Result := Threads[Handle];

  ThreadSemaphore := 0;
end;

// State signal handler
procedure HandleStateSignal(SigNum: LongInt); cdecl; {&Frame-}
asm
            mov     eax, fs:[0].TThreadInfo.State
            cmp     eax, tsRunning
            je      @@RET

            cmp     eax, tsTerminated
            jne     @not_tsTerminated

            // is this possible ?
            push -1                             // for ExitCode
            call SysCtrlExitThread

          @not_tsTerminated:
            push    0
            call    LnxSigSuspend
            pop     eax                         // Cdecl

          @@RET:
end;

// Child signal handler
procedure HandleChildSignal(SigNum: LongInt); Cdecl;
var
  I: LongInt;
begin
  // Make sure all child signals go to the main thread.
  if GetThreadID <> 1 then
    begin
      LnxKill(MainThread.ProcessPid, SIGCHLD);
      Exit;
    end;

  // Walk the thread list and remove the blocks
  // of all terminated threads.
  SysSysWaitSem(ThreadSemaphore);

  for I := 2 to MaxThread do
    if Assigned(Threads[I]) then
      if Threads[I].State = tsTerminated then
        RemoveThreadInfo(Threads[I]);

  ThreadSemaphore := 0;
end;

function SysCtrlGetTlsMapMem: Pointer;
begin
  // Implementation using normal memory, for the time being.
  // Shared memory  will have to be used later, in order for DLLs
  // to work with TLS.
  SysMemAlloc(SharedMemSize, $06, Result);
  FillChar(Result^, SharedMemSize, $FF);
  FillChar(Result^, SizeOf(TSharedMem), 0);
  System.GetMemoryManager( PSharedMem(Result)^.TlsMemMgr );
end;

function SysCtrlKillThread(Handle: Longint): Longint;
var
  Thread: PThreadInfo;
begin
  Thread := GetThread(Handle);
  if Thread <> nil then
    begin
      Thread.State := tsTerminated;
      Result := -LnxKill(Thread.ThreadPid, SIGUSR1);
    end
  else
    Result := ESRCH;
end;

function SysCtrlSuspendThread(Handle: Longint): Longint;
var
  Thread: PThreadInfo;
begin
  Thread := GetThread(Handle);
  if Thread <> nil then
    begin
      Thread.State := tsSuspended;
      Result := -LnxKill(Thread.ThreadPid, SIGUSR1);
    end
  else
    Result := ESRCH;
end;

function SysCtrlResumeThread(Handle: Longint): Longint;
var
  Thread: PThreadInfo;
begin
  Thread := GetThread(Handle);
  if Thread <> nil then
    begin
      Thread.State := tsRunning;
      Result := -LnxKill(Thread.ThreadPid, SIGUSR1);
    end
  else
    Result := ESRCH;
end;

procedure SysCtrlExitThread(ExitCode: Longint);
begin
  asm
    mov fs:[0].TThreadInfo.State, tsTerminated
  end;

  // If the main thread terminates, this is also
  // the termiantion of the whole process.
  if GetThreadID = 1 then
    SysCtrlExitProcess(ExitCode);

  LnxExit(ExitCode);
end;

procedure SysCtrlExitProcess(ExitCode: Longint);
var
  I, J: LongInt;
begin
  I := GetThreadID;

  // Kill all threads except the current one.
  for J := 1 to MaxThread do
    if (I <> J) and (Threads[J] <> nil) then
      KillThread(J);

  TrmDone;

  LnxExit(ExitCode);
end;

// Creates a new selector in the process' local descriptor table
// and returns it. * If the result is zero, something went wrong.
function GetNewSelector(Index: LongInt; Address: Pointer; Size: LongInt): Word;
var
  LDT: TModifyLDT;
begin
  LDT.Index := Index;
  LDT.Base  := Address;
  LDT.Limit := Size - 1;
  LDT.Flags := 64;                           // 64: Segment is usable
  if LnxModifyLDT(1, LDT, SizeOf(LDT)) = 0 then
    Result := Index shl 3 or 7               // 7: LDT entry, user priveleges
  else
    Result := 0;
end;

function SysGetThreadId: Longint; assembler;
{&Frame-} {&Uses None}
asm
    mov     eax, fs:[0].TThreadInfo.ThreadPid
end;

function SysCtrlCreateThread(Attrs: Pointer; StackSize: Longint; Func,Param: Pointer; Flags: Longint; var Tid: Longint): Longint;
const
  CloneFlags = CLONE_VM or CLONE_FS or CLONE_FILES or SIGCHLD;
var
  Thread: PThreadInfo;
begin
  SysSysWaitSem(ThreadSemaphore);

  // Try to get a new thread handle
  Thread := AddThreadInfo(StackSize, Flags and 1);
  if Thread = nil then
  begin
    Result := -1;
    ThreadSemaphore := 0;
    Exit;
  end;

  // Create thread
  {&Alters eax,ebx,ecx,edx}
  asm
    mov edx, Thread;
    mov ecx, [edx].TThreadInfo.StackLimit;
    // "push" Param
    mov eax, Param;
    sub ecx, 4;
    mov [ecx], eax;
    // "push" Func
    mov eax, Func;
    sub ecx, 4;
    mov [ecx], eax;

    // "push" local Descriptor  [for GetNewSelector]
    mov eax, [edx].TThreadInfo.Handle // LDT entry = thread handle
    sub ecx, 4;
    mov DWORD [ecx], eax;
    // "push" @Thread           [for GetNewSelector]
    sub ecx, 4;
    mov [ecx], edx;
    // "push" SizeOf(Thread)    [for GetNewSelector]
    sub ecx, 4;
    mov DWORD [ecx], TYPE TThreadInfo;

    // Create the new thread
    mov eax, 120;
    mov ebx, CloneFlags;
    int $80;

    // Both threads land here. Check whether we deal with
    // the parent (EAX=new PID) or the child (EAX=0).
    or eax, eax;
    jnz @Parent;

    // Create FS selector for new thread. The arguments
    // are already on the stack.
    call GetNewSelector;
    mov fs, ax;
    mov fs:[0].TThreadInfo.TibSelector, fs;

    // Let the thread wait until the parent has
    // finished the initialization of the thread
    // control block.
    push OFFSET ThreadSemaphore
    call SysSysWaitSem
    btr ThreadSemaphore, 0

    // Call handle state signal to hold back
    // those threads that shall be created in
    // suspended state. Clean up stack, since
    // this is a C function.
    push SIGUSR1;
    call HandleStateSignal;
    pop eax;

    // Call real thread function for child thread.
    jmp [esp].longint

  @Parent:

    // Store the new PID
    mov [edx].TThreadInfo.ThreadPid, eax;
  end;

  if Thread.ThreadPid < 1 then
    begin
      Result := -Thread.ThreadPid;
      RemoveThreadInfo(Thread);
      ThreadSemaphore := 0;
      Exit;
    end;

  Tid := Thread.Handle;
  Result := 0;

  ThreadSemaphore := 0;
end;

function SysCtrlGetModuleName(Handle: Longint; Buffer: PChar): Longint;
var
  S: String;
begin
  if Handle = 0 then
    begin
      SysCmdlnParam(0, S);
      StrPCopy(Buffer, S);
    end
  else
    Unimplemented('SysCtrlGetModuleName');
end;

procedure SysCtrlEnterCritSec;
begin
  Unimplemented('SysCtrlEnterCritSec');
end;

procedure SysCtrlLeaveCritSec;
begin
  Unimplemented('SysCtrlLeaveCritSec');
end;

//--------------[ ENVIRONMENT ]---------------------------------------

type
  TPCharArray = array[0..1023] of PChar;
  PPCharArray = ^TPCharArray;

var
  Env:  PPCharArray;
  Argv: PPCharArray;
  Argc: LongInt;

function SysCmdln: PChar;
begin
  Result := Argv^[0];
end;

function SysCmdlnCount: Longint;
begin
  Result := Argc - 1;
end;

procedure SysCmdlnParam(Index: Longint; var Param: ShortString);
var
  Buffer1, Buffer2: TFileNameBuf;
  P: PChar;
  L: LongInt;
begin
  if (Index < 0) or (Index >= Argc) then
    Param := ''
  else
    if Index = 0 then
      begin
        L := LnxReadLink('/proc/self/exe', @Buffer1, SizeOf(Buffer1) - 1);
        if L > 0 then
          begin
            Buffer1[L] := #0;
            P := @Buffer1;
          end
        else
          P := Argv^[0];

        Param := StrPas(SysConvertFileName(@Buffer2, P, FileSystem, fsUnix));
      end
    else
      Param := StrPas(Argv^[Index]);
end;

function SysGetEnvironment: PChar;
begin
  Result := Env^[0];
end;

procedure SysFreeEnvironment(_Env: PChar);
begin
  // Nothing; the environment does not need freeing
end;

function SysGetEnvString(EnvVar, Default: PChar): PChar;
var
  P: PChar;
  L: Word;
begin
  L := StrLen(EnvVar);
  P := SysGetEnvironment;
  while P^ <> #0 do
    begin
      if (StrLIComp(P, EnvVar, L) = 0) and (P[L] = '=') then
        begin
          Result := P + L + 1;
          Exit;
        end;
      Inc(P, StrLen(P) + 1);
    end;
  Result := Default;
end;

function SysOsVersion: Longint;
var
  Handle, Actual, Error, Dot, VerLo, VerHi: LongInt;
  Buffer: ShortString;
begin
  Result := 0;

  if SysFileOpen('/proc/version', OPEN_ACCESS_READONLY, Handle) = 0 then
    begin
      Error := SysFileRead(Handle, Buffer[1], 255, Actual);
      SysFileClose(Handle);

      if Error = 0 then
        begin
          SetLength(Buffer, Actual);

          Dot := Pos('version ', Buffer);
          Delete(Buffer, 1, Dot + 7);
          Dot := Pos('.', Buffer + '.');
          Val(Copy(Buffer, 1, Dot - 1), VerHi, Error);
          Delete(Buffer, 1, Dot);
          Dot := Pos('.', Buffer + '.');
          Val(Copy(Buffer, 1, Dot - 1), VerLo, Error);
          Delete(Buffer, 1, Dot);

          Result := VerLo shl 8 + VerHi;
        end;
    end;
end;

function SysPlatformID: Longint;
begin
  Result := -3;
end;

procedure SysGetDateTime(Year,Month,Day,DayOfWeek,Hour,Minute,Second,MSec: PLongint);
var
  TimeVal: TTimeVal;
  TimeZone: TTimeZone;
  DateTime: TDateTime;
begin
  LnxGetTimeOfDay(TimeVal, TimeZone);
  DateTime := UnpackUTCTime(TimeVal.tv_Sec - TimeZone.tz_MinutesWest * 60);

  if Year <> nil then Year^ := DateTime.Year;
  if Month <> nil then Month^ := DateTime.Month;
  if Day <> nil then Day^ := DateTime.Day;
  if DayOfWeek <> nil then DayOfWeek^ := DateTime.DayOfWeek;
  if Hour <> nil then Hour^ := DateTime.Hour;
  if Minute <> nil then Minute^ := DateTime.Min;
  if Second <> nil then Second^ := DateTime.Sec;
  if MSec <> nil then MSec^ := TimeVal.tv_USec div 1000;
end;

procedure SysSetDateTime(Year,Month,Day,Hour,Minute,Second,MSec: PLongint);
var
  TimeVal: TTimeVal;
  TimeZone: TTimeZone;
  DateTime: TDateTime;
begin
  LnxGetTimeOfDay(TimeVal, TimeZone);
  DateTime := UnpackUTCTime(TimeVal.tv_Sec - TimeZone.tz_MinutesWest * 60);

  if Year <> nil then DateTime.Year := Year^;
  if Month <> nil then DateTime.Month := Month^;
  if Day <> nil then DateTime.Day := Day^;
  if Hour <> nil then DateTime.Hour := Hour^;
  if Minute <> nil then DateTime.Min := Minute^;
  if Second <> nil then DateTime.Sec := Second^;
  if MSec <> nil then TimeVal.tv_USec := 1000 * MSec^;

  TimeVal.tv_Sec := PackUTCTime(DateTime);
  Inc(TimeVal.tv_Sec, TimeZone.tz_MinutesWest * 60);

  LnxSetTimeOfDay(TimeVal, TimeZone);
end;

//--------------[ DISK FUNCTIONS ]------------------------------------

const
  VerifyFlag: Boolean = False;

function SysVerify(SetValue: Boolean; Value: Boolean): Boolean;
begin
  Result := VerifyFlag;
  if SetValue then
    VerifyFlag := Value;
end;

function SysDiskFreeLong(Drive: Byte): TQuad;
var
  Buffer: TStatFS;
begin
  if (Drive <> 0) and (Drive <> 3) then
    begin
      Result := -1;
      Exit;
    end;

  if LnxStatFS('/', Buffer) = 0 then
    Result := 1.0 * Buffer.f_BSize * Buffer.f_BAvail
  else
    Result := -1;
end;

function SysDiskSizeLong(Drive: Byte): TQuad;
var
  Buffer: TStatFS;
begin
  if (Drive <> 0) and (Drive <> 3) then
    begin
      Result := -1;
      Exit;
    end;

  if LnxStatFS('/', Buffer) = 0 then
    Result := 1.0 * Buffer.f_BSize * Buffer.f_Blocks
  else
    Result := -1;
end;

// by unxed, untested
function SysDiskFreeLongX(Path: PChar): TQuad; {Cat}
var
  Buffer: TStatFS;
begin
  if LnxStatFS(Path, Buffer) = 0 then
    Result := 1.0 * Buffer.f_BSize * Buffer.f_BAvail
  else
    Result := -1;
end;

// by unxed, untested
function SysDiskSizeLongX(Path: PChar): TQuad; {Cat}
var
  Buffer: TStatFS;
begin
  if LnxStatFS(Path, Buffer) = 0 then
    Result := 1.0 * Buffer.f_BSize * Buffer.f_Blocks
  else
    Result := -1;
end;

function SysGetFileAttr(FileName: PChar; var Attr: Longint): Longint;
var
  Stat: TStat;
  Buffer: TFileNameBuf;
begin
  FileName := SysConvertFileName(@Buffer, FileName, fsUnix, FileSystem);

  Result := -LnxStat(FileName, Stat);
  Attr := 0;
  if Stat.st_Mode and S_IFDIR <> 0 then Attr := Attr or $10;
  if Stat.st_Mode and S_IWUSR = 0 then Attr := Attr or $01;
  if FileName[0] = '.' then Attr := Attr or $02;
end;

function SysSetFileAttr(FileName: PChar; Attr: Longint): Longint;
var
  Stat: TStat;
  Buffer: TFileNameBuf;
begin
  FileName := SysConvertFileName(@Buffer, FileName, fsUnix, FileSystem);

  Result := -LnxStat(FileName, Stat);
  if Result <> 0 then Exit;

  if Attr and $10 <> 0 then
    Stat.st_Mode := Stat.st_Mode or S_IFDIR
  else
    Stat.st_Mode := Stat.st_Mode and not S_IFDIR;

  if Attr and $01 = 0 then
    Stat.st_Mode := Stat.st_Mode or S_IWUSR
  else
    Stat.st_Mode := Stat.st_Mode and not S_IWUSR;

  Result := -LnxChMod(FileName, Stat.st_Mode)
end;

function SysGetFileTime(Handle: Longint; var Time: Longint): Longint;
var
  Stat: TStat;
  TimeVal: TTimeVal;
  TimeZone: TTimeZone;
  DateTime: TDateTime;
begin
  Result := -LnxFStat(Handle, Stat);

  LnxGetTimeOfDay(TimeVal, TimeZone);
  DateTime := UnpackUTCTime(Stat.st_Mtime - TimeZone.tz_MinutesWest * 60);
  PackDosTime(DateTime, Time);
end;

function SysSetFileTime(Handle: Longint; Time: Longint): Longint;
var
  Stat: TStat;
  TimeVal: TTimeVal;
  TimeZone: TTimeZone;
  DateTime: TDateTime;
  Buf: TUTimBuf;
  FileName: string;
begin
  LnxFStat(Handle, Stat);
  LnxGetTimeOfDay(TimeVal, TimeZone);
  UnpackDosTime(Time, DateTime);
  Buf.modtime := PackUTCTime(DateTime);
  Inc(Buf.modtime, TimeZone.tz_MinutesWest * 60);
  buf.actime := Stat.st_ATime;

  Str(Handle, FileName);
  FileName := '/proc/self/fd/' + FileName + #0;

  Result := -LnxUTime(@FileName[1], Buf);
end;

// Compare a string with a pattern.  The pattern can contain any
// combination of * and ? characters.  The string can be up to 253
// characters in length, and the pattern up to 252.
// The text must not contain ascii 0 characters.

function MatchStr(Pat, Txt: string): Boolean;
var
  SubLen, ComPos, NextStar, SubPos: LongInt;
begin
  // First make sure that the pattern doesn't start with *, and always
  // ends with *.  Change the text accordingly.
  Pat := #0 + Pat + #0 + '*';
  Txt := #0 + Txt    + #0;

  Result := True;

  while (Pat <> '') and Result do
    begin
      // Look for the first *.  At least 1 character before this will be
      // a normal character, i.e. neither ? nor *
      NextStar := Pos('*', Pat);

      SubLen := NextStar - 1;

      // Ignore double-*
      while (NextStar < Length(Pat)) and (Pat[NextStar + 1] = '*') do
        Inc(NextStar);

      SubPos := 0;

      repeat
        Inc(SubPos);
        Result := True;
        ComPos := 0;
        while (ComPos < SubLen) and Result do
          begin
            if (Txt[SubPos + ComPos] <> Pat[ComPos + 1]) and
               (Pat[ComPos + 1] <> '?') then
              Result := False;

            Inc(ComPos);
          end;
      until (SubPos + SubLen > Length(Txt)) or Result;

      // When a match is found, cut a piece off the text and continue.
      if Result then
        begin
          Delete(Txt, 1, SubPos + SubLen - 1);
          Delete(Pat, 1, NextStar);
        end;
    end;
end;

function DoFindFile(var F: TOSSearchRec; IsPChar: Boolean): Longint;
var
  Buffer: TDirEnt;
  Stat: TStat;
  FileName: TFileNameBuf;
  DateTime: TDateTime;
  TimeVal: TTimeVal;
  TimeZone: TTimeZone;
  Ok: Boolean;
  I: LongInt;
begin
  repeat
    Result := -LnxReadDir(F.Handle, Buffer, 1);

    case Result of
      -1: Result := 0;

       0: begin
            Result := 254;
            Exit;
          end;

      else
        Exit;
    end;

    if IsPChar then
      StrCopy(@F.Name,@Buffer.d_Name)
    else
      F.Name := StrPas(@Buffer.d_Name);
    StrCopy(@FileName, @F.FindDir);
    StrCopy(StrEnd(@FileName), '/');
    StrCopy(StrEnd(@FileName), @Buffer.d_Name);

    LnxStat(@FileName, Stat);
    F.Size := Stat.st_Size;
    F.Attr := 0;

    if Stat.st_Mode and S_IFDIR <> 0 then F.Attr := F.Attr or $10; // directory
    if Stat.st_Mode and S_IWUSR =  0 then F.Attr := F.Attr or $01; // read only

    LnxGetTimeOfDay(TimeVal, TimeZone);
    DateTime := UnpackUTCTime(Stat.st_Mtime - TimeZone.tz_MinutesWest * 60);
    PackDosTime(DateTime, F.Time);

    Ok := (Lo(F.FindAttr) and F.Attr = F.Attr)
      and (Hi(F.FindAttr) and F.Attr = Hi(F.FindAttr))
      and (MatchStr(F.FindName, F.Name) or (F.FindName = '*') or (F.FindName = '*.*'))
      and SysIsValidFileName(@Buffer.d_Name, FileSystem);

  until Ok;
end;

procedure Log(const s: String);
var
  fLog: Text;
begin
  assign(fLog, '/tmp/vp');
  {$I-}
  append(fLog);
  {$I+}
  if IOResult <> 0 then
    Rewrite(fLog);
  Writeln(fLog, s);
  Close(fLog);
end;

function SysFindFirst(Path: PChar; Attr: Longint; var F: TOSSearchRec; IsPChar: Boolean): Longint;
var
  P, Q: LongInt;
  Buffer: TFileNameBuf;
begin
  Path := SysConvertFileName(@Buffer, Path, fsUnix, FileSystem);

  Q := StrLen(Path);
  P := Q;
  while (P > -1) and (Path[P] <> '/') do
    Dec(P);

  if Q-P-1 > 0 then
    SetString(F.FindName, @Path[P + 1], Q - P - 1)
  else
    F.FindName := '*';

  if P <> -1 then
    begin
      if (Path[P] = '/') and (P > 0) then Dec(P);
      Move(Path[0], F.FindDir, P + 1);
      F.FindDir[P + 1] := #0;
    end
  else
    begin
      F.FindDir[0] := '.';
      F.FindDir[1] := #0;
    end;

  F.FindAttr := Attr;
  Result := SysFileOpen(@F.FindDir, OPEN_ACCESS_READONLY, F.Handle);
  if Result = 0 then
    Result := DoFindFile(F,IsPChar);
end;

function SysFindNext(var F: TOSSearchRec; IsPChar: Boolean): Longint;
begin
  Result := DoFindFile(F,IsPChar);
end;

function SysFindClose(var F: TOSSearchRec): Longint;
begin
  Result := SysFileClose(F.Handle);
end;

// Check if file exists; if it does, update FileName parameter
// to include correct case of existing file
function SysFileAsOS(FileName: PChar): Boolean;
var
  Buffer: TFileNameBuf;
begin
  FileName := SysConvertFileName(@Buffer, FileName, fsUnix, FileSystem);

  Result := (LnxAccess(@FileName, F_OK) = 0);
end;

function SysFileSearch(Dest,Name,List: PChar): PChar;
var
  I, P, L: Integer;
  Buffer, NameBuffer, ListBuffer: TFileNameBuf;
begin
  Name := SysConvertFileName(@NameBuffer, Name, fsUnix, FileSystem);
  List := SysConvertFileName(@ListBuffer, List, fsUnix, FileSystem);

  Result := Dest;
  StrCopy(Buffer, Name);
  P := 0;
  L := StrLen(List);
  while True do
    begin
      if LnxAccess(@Buffer, F_OK) = 0 then
        begin
          if SysConvertFileName(@NameBuffer, @Buffer, FileSystem, fsUnix) = @Buffer then
            begin
              StrCopy(@NameBuffer, @Buffer);
              SysFileExpand(Dest, @NameBuffer);
            end
          else
            SysFileExpand(Dest, @NameBuffer);

          Exit;
        end;

      while (P < L) and (List[P] in [':', ';']) do
        Inc(P);
      if P >= L then
        Break;
      I := P;
      while (P < L) and not (List[P] in [':', ';']) do
        Inc(P);
      StrLCopy(Buffer, List + I, P - I);
      if not (List[P-1] = '/') then
        StrLCat(Buffer, '/', 259);
      StrLCat(Buffer, Name, 259);
    end;
  Dest^ := #0;
end;

function SysFileExpand(Dest,Name: PChar): PChar;
  var
    temp        :array[0..260] of char;
    Source,
    Work,
    DeleteLimit :PChar;
    PathSep     :Char;
  begin
    // process '~'
    Source:=Name;
    Work:=@temp;

    if (Source[0]='~') then
      begin
        Inc(Source,1);
        Work:=StrEnd(StrCopy(Work,SysGetEnvString('HOME','.')));
      end;

    StrCopy(Work,Source);


    // insert base dir if needed
    Source:=@temp;
    Work:=Dest;

    if FileSystem <> fsUnix then
      begin
        Work:=StrEnd(StrCopy(Work,'c:'));
        PathSep:='\';
      end
    else
      PathSep:='/';


    if (FileSystem <> fsUnix)
    and (Source[0] in ['A'..'Z','a'..'z'])
    and (Source[1]=':') then
      begin
        Inc(Source,Length('c:'))
      end;

    DeleteLimit:=Work;

    if not (Source[0] in ['\','/']) then
      begin
        if FileSystem <> fsUnix then
          Dec(Work,Length('c:'));
        SysDirGetCurrent(0,Work);
        if not ((Work[0] in ['\','/']) and (Work[1]=#0)) then
          Work:=StrEnd(Work);
        Work[0]:=PathSep;
        Inc(Work);
      end;

    repeat
      case Source[0] of
        #0:
          Break;

        '\','/':
          begin
            Work[0]:=PathSep;
            Inc(Source);
            Inc(Work);
          end;

        '.':
          begin

            if Work<=DeleteLimit then
              begin
                Work[0]:='.';
                Inc(Source);
                Inc(Work);
                Continue;
              end;

            Dec(Work);              // step back - to PathSep ?

            if (Work[0]=PathSep) and (Source[0]='.') and (Source[1] in ['\','/',#0]) then
              begin                 // 'aa/./aa' 'aa/.'
                                    // no PathSep
                Inc(Source);        // Ignore '.'
                Continue;
              end
            else
            if (Work[0]=PathSep) and (Source[0]='.') and (Source[1]='.') and (Source[2] in ['\','/',#0]) then
              begin                 // 'aa/../aa' 'aa/..'
                                    // preceeding PathSep is removed
                Inc(Source,2);      // Ignore '..'

                while Work>DeleteLimit do   // remove path component...
                  begin
                    Dec(Work);
                    if Work[0]=PathSep then
                      Break;
                  end;

                Continue;
              end;

            Inc(Work);              // keep previous char

            Work[0]:='.';
            Inc(Source);
            Inc(Work);

        end; (* '.' *)

      else
        Work[0]:=Source[0];
        Inc(Source);
        Inc(Work);
      end; (* case Source[0] *)

    until false;

    Work[0]:=#0;

    Result:=Dest;

    case FileSystem of
      fsDosUpper:StrUpper(Result);
      fsDosLower:StrLower(Result);
    end;
  end;

threadvar
  ExecProcID: LongInt;
  ExecResult: LongInt;
  ExecAsync:  Boolean;

function SysExecute(Path,CmdLine,Env: PChar; Async: Boolean; PID: PLongint; StdIn,StdOut,StdErr: Longint): Longint;
  procedure MakeArgList(Source: PChar; var Dest: TPCharArray);
  var
    I, J, K: LongInt;
    SQ, DQ: Boolean;
  begin
    I := 0;
    K := 0;

    SQ := False;
    DQ := False;

    while Source[I] <> #0 do
      begin
        J := I;
        while True do
          begin
            case Source[J] of
              '"':  if not SQ then DQ := not DQ;
              '''': if not DQ then SQ := not SQ;
              ' ':  if not (SQ or DQ) then Break;
              #0:   Break;
            end;

            Inc(J);
          end;

        if J > I then
          begin
            Source[J] := #0;
            Dest[K] := @Source[I];
            Inc(K);
          end;

        I := J;
        Inc(I);
      end;

    Dest[K] := nil;
  end;

  procedure MakeEnvList(Source: PChar; var Dest: TPCharArray);
  var
    I, J, K: LongInt;
  begin
    I := 0;
    K := 0;

    while Source[I] <> #0 do
      begin
        J := I;
        while Source[J] <> #0 do Inc(J);

        if J > I then
          begin
            // WriteLn('>', Source + I, '<');
            Dest[K] := @Source[I];
            Inc(K);
          end;

        I := J;
        Inc(I);
      end;

    Dest[K] := nil;
  end;

var
  Buffer: TFileNameBuf;
  ArgBuf: array[0..1023] of Char;
  ArgLst, EnvLst: TPCharArray;
  P: PChar;
  I: Longint;
  StdHandles,
  NewHandles,
  OldHandles: array[0..2] of Longint;
  // used only if not Async
  ExecErrorPipe: TPipe;
  ExecError: Longint;
  fLog: Text;

begin // SysExecute
  // ugly: address thread variable from child thread
  with ExecErrorPipe do
    begin
      RdFile:=-1;
      WrFile:=-1;
      if not Async then
        LnxPipe(ExecErrorPipe);
    end;

  // renumber console handles
  StdHandles[0] := StdIn;
  StdHandles[1] := StdOut;
  StdHandles[2] := StdErr;

  for I := 0 to 2 do
    if StdHandles[I] <> -1 then
      begin
        NewHandles[I] := I;
        OldHandles[I]:=LnxDup(NewHandles[I]);
        LnxDup2(StdHandles[I], NewHandles[I]);
      end;

  Path := SysConvertFileName(@Buffer, Path, fsUnix, FileSystem);

  P := StrECopy(@ArgBuf, Path);
  P^ := #0;
  Inc(P);
  StrCopy(P, CmdLine);

  MakeArgList(ArgBuf, ArgLst);

  if Env <> nil then
    MakeEnvList(Env, EnvLst);

  ExecProcID := 0;
  ExecProcID := LnxFork;
  if ExecProcID = 0 then
    begin
      // This is what happens in the child process after the fork
      //   Linux limitation: we can not report an error from async
      //   SysExecute, Dos.DosError will allways be 0,
      //   must check DosExitCode...
      i := 0;
{      while arglst[i] <> nil do
      begin
        Log(strpas(ArgLst[i]));
        inc(i);
      end;
}
      if Env <> nil then
        ExecError := LnxExecve(Path, @ArgLst, @EnvLst)
      else
        ExecError := LnxExecve(Path, @ArgLst, VpSysLow.Env);

      if ExecErrorPipe.WrFile<>-1 then
        LnxWrite(ExecErrorPipe.WrFile,ExecError,SizeOf(ExecError));

      // terminate "Process" if LnxExecve returned
      LnxExit(ExecError);
    end
  else
    begin
      // restore console handles
      for I := 0 to 2 do
        if StdHandles[I] <> -1 then
          begin
            LnxDup2(OldHandles[I], NewHandles[I]);
            SysFileClose(OldHandles[I]);
          end;

      // This is what happens in the parent process after the fork
      if ExecProcID < 0 then
        begin // error
          Result := -ExecProcID;
          Exit;
        end;

      ExecAsync := Async;

      if Async then
        begin
          Result := 0; // Success
          PID^ := ExecProcId;
        end
      else
        begin
          // disable keyboard+mouse input
          SuspendTrm;

          // wait for return code
          repeat
            Result:=LnxWaitPID(ExecProcID, ExecResult, 0);
          until (Result=ExecProcID)     // PID
             or (Result=-ECHILD);       // or does not exist

          // correct PID exited -> no error
          if Result=ExecProcID then
            Result:=0;

          // report error caused by LnxExecve
          if ExecErrorPipe.RdFile<>-1 then
            with ExecErrorPipe do
              begin
                LnxClose(WrFile);
                ExecError:=0;
                LnxReadUnsafe(RdFile,ExecError,SizeOf(ExecError));
                if ExecError<>0 then
                  Result:=ExecError;
                LnxClose(RdFile);
              end;

          // continue keyboard+mouse input
          ResumeTrm;

        end;

      // store PID if successful
      if (Result=0) and Assigned(PID) then
        PID^ := ExecProcID;
    end;
end; // SysExecute

function SysExitCode: Longint;
var
  rc:Longint;
begin
  if ExecAsync then
    repeat
      rc:=LnxWaitPID(ExecProcID, ExecResult, 0);
      if rc=-ECHILD then
        begin
          Result:=-1;
          Exit;
        end;
    until (rc=ExecProcID);  // PID

  // rotate important bits to byte 0...
  Result := (ExecResult shr 8) or (Lo(ExecResult) shl 24)
end;

//--------------[ STRING HANDLING ]-----------------------------------

type
  TCharCaseTable = array[0..255] of Char;
var
  UpperCaseTable: TCharCaseTable;
  LowerCaseTable: TCharCaseTable;
  AnsiUpperCaseTable: TCharCaseTable;
  AnsiLowerCaseTable: TCharCaseTable;
  WeightTable: TCharCaseTable;

const
  CaseTablesInitialized: Boolean = False;

procedure InitCaseTables;
var
  I,J: Integer;
begin
  for I := 0 to 255 do
    begin
      UpperCaseTable[I] := Chr(I);
      LowerCaseTable[I] := Chr(I);
      AnsiUpperCaseTable[I] := Chr(I);
      AnsiLowerCaseTable[I] := Chr(I);
      if I in [Ord('A')..Ord('Z')] then
        LowerCaseTable[I] := Chr(I + (Ord('a')-Ord('A')));
      if I in [Ord('a')..Ord('z')] then
        UpperCaseTable[I] := Chr(I - (Ord('a')-Ord('A')));
    end;
  SysGetCaseMap(SizeOf(AnsiUpperCaseTable), AnsiUpperCaseTable);
  for I := 0 to 255 do
    begin
      J := Ord(AnsiUpperCaseTable[I]);
      if (J <> I) {and (AnsiLowerCaseTable[J] <> chr(J))} then
        AnsiLowerCaseTable[J] := Chr(I);
    end;
  SysGetWeightTable(SizeOf(WeightTable), WeightTable);
  CaseTablesInitialized := True;
end;

procedure ConvertCase(S1,S2: PChar; Count: Integer; var Table: TCharCaseTable); {&USES esi,edi} {&FRAME-}
asm
                cmp     CaseTablesInitialized,false
                jne     @@1
                Call    InitCaseTables
              @@1:
                xor     eax,eax
                mov     esi,S1
                mov     edi,S2
                mov     ecx,Count
                mov     edx,Table
                jecxz   @@3
              @@2:
                dec     ecx
                mov     al,[esi+ecx]
                mov     al,[edx+eax]
                mov     [edi+ecx],al
                jnz     @@2
              @@3:
end;

procedure SysChangeCase(Source, Dest: PChar; Len: Longint; NewCase: TCharCase);
begin
  case NewCase of
    ccLower:     ConvertCase(Source, Dest, Len, LowerCaseTable);
    ccUpper:     ConvertCase(Source, Dest, Len, UpperCaseTable);
    ccAnsiLower: ConvertCase(Source, Dest, Len, AnsiLowerCaseTable);
    ccAnsiUpper: ConvertCase(Source, Dest, Len, AnsiUpperCaseTable);
  end;
end;

function SysLowerCase(s: PChar): PChar;
begin
  ConvertCase(s, s, strlen(s), AnsiLowerCaseTable);
  Result := s;
end;

function SysUpperCase(s: PChar): PChar;
begin
  ConvertCase(s, s, strlen(s), AnsiUpperCaseTable);
  Result := s;
end;

function MemComp(P1,P2: Pointer; L1,L2: Integer; T1,T2: PChar): Integer; {&USES ebx,esi,edi,ebp} {&FRAME-}
asm
                cmp     CaseTablesInitialized,false
                jne     @@0
                Call    InitCaseTables
              @@0:
                mov     ecx,L1
                mov     eax,L2
                mov     esi,P1
                mov     edi,P2
                cmp     ecx,eax
                jbe     @@1
                mov     ecx,eax
              @@1:
                mov     ebx,T1
                mov     ebp,T2
                xor     eax,eax
                xor     edx,edx
                test    ecx,ecx
                jz      @@5
              @@2:
                mov     al,[esi]
                mov     dl,[edi]
                inc     esi
                inc     edi
                test    ebp,ebp
                mov     al,[ebx+eax]    // Table1
                mov     dl,[ebx+edx]
                jz      @@3
                mov     al,[ebp+eax]    // Table2
                mov     dl,[ebp+edx]
              @@3:
                cmp     al,dl
                jne     @@RET
                dec     ecx
                jnz     @@2
              @@5:
                mov     eax,L1
                mov     edx,L2
              @@RET:
                sub     eax,edx
end;

function SysCompareStrings(s1, s2: PChar; l1, l2: Longint; IgnoreCase: Boolean): Longint;
begin
  if IgnoreCase then
    Result := MemComp(s1, s2, l1, l2, @WeightTable, nil)
  else
    Result := MemComp(s1, s2, l1, l2, @AnsiUpperCaseTable, @WeightTable);
end;

procedure SysGetCaseMap(TblLen: Longint; Tbl: PChar );
var
  I: LongInt;
begin
  for I := 0 to TblLen - 1 do
    Tbl[I] := UpCase(Tbl[I]);
end;

procedure SysGetWeightTable(TblLen: Longint; WeightTable: PChar);
var
  I: LongInt;
begin
  for I := 0 to TblLen - 1 do
    WeightTable[I] := Chr(I);
end;

function SysGetCodePage: Longint;
begin
  Result := 1004; // ISO-Latin-1
end;


function SysFileIncHandleCount(Count: Longint): Longint;
begin
  Result := 0;
end;

//--------------[ SCREEN AND KEYBOARD ]-------------------------------

var
  // Terminal in/out handle
  TrmHandle: LongInt = -1;

  // Saved terminal attributes
  TrmSaveAttr: TTermios;

  TermName : PChar = nil;

function TrmRead(var Buffer; Count: Integer): Integer;
begin
  Result := LnxRead(TrmHandle, Buffer, Count);
end;

procedure TrmWriteString(const s:String);
  var
    rc: Longint;
  begin
    if s<>'' then
      repeat
        rc:=LnxWrite(TrmHandle, s[1], Length(s));
      until (rc=Length(s)) or (rc<0);
  end;

function IsTermType(const CompName: PChar): boolean;
  begin
    Result := (StrPos(TermName,CompName)=TermName);
  end;

procedure TrmInit;
var
  Attr: TTermios;
begin
  // Get terminal name
  TermName := SysGetEnvString('TERM', 'unknown');

  // If already initialized, return immediately
  if TrmHandle <> -1 then Exit;

  // Open device
  TrmHandle := LnxOpen('/dev/tty', O_RDWR, 0);

  // Get terminal information and store it
  LnxIoCtl(TrmHandle, TCGETS, @Attr);
  TrmSaveAttr := Attr;

  // Change some flags
  with Attr do
  begin
    // ECHO  =echo input chars
    // ICANON=speacial EOF,EOF,EOF2.. chars
    // IEXTEN=implementation defined input processing
    c_lflag := c_lflag and not (ECHO  or ICANON or IEXTEN);
    // INPCK =enable input parity checking
    // ISTRIP=strip bit 7
    // IXON  =XON/XOFF flow control
    c_iflag := c_iflag and not (INPCK or ISTRIP or IXON);
    // CSIZE =character size mask
    // PARENB=parity for in+out
    c_cflag := c_cflag and not (CSIZE or PARENB);
    // CS8   =8 bit transmission
    c_cflag := c_cflag or CS8;
    // OPOST =output postprocessing
    c_oflag := c_oflag and not (OPOST);
    c_cc[VMIN]  := 1;
    c_cc[VTIME] := 0;
  end;

  // Activate the new terminal settings
  LnxIoCtl(TrmHandle, TCSETS, @Attr);

  // Enter XMIT mode
  TrmWriteString(#27'[?1h'); // #27'='; // #27'[?7l';
end;

procedure TrmDone;
begin
  // Reset old terminal settings
  if TrmHandle <> -1 then
    begin
      LnxIoCtl(TrmHandle, TCSETS, @TrmSaveAttr);

      // Enter LOCAL mode
      TrmWriteString(#27'[?1l'); // #27'>'; // #27'[?7h';

      // Reset all attributes, activate normal character set
      TrmWriteString(#27'[0m'#27'(B');

      // Free terminal handle
      LnxClose(TrmHandle);
      TrmHandle := -1;

      if KbdThreadID<>GetThreadID then
        begin
          SysCtrlKillThread(KbdThreadID);
          KbdThreadID:=0
        end;
    end;
end;

var
  ResumeAttr: TTermios;

procedure SuspendTrm;
  begin
    if TrmHandle <> -1 then
      begin
        LnxIoCtl(TrmHandle, TCGETS, @ResumeAttr);
        LnxIoCtl(TrmHandle, TCSETS, @TrmSaveAttr);

        // Enter LOCAL mode
        TrmWriteString(#27'[?1l'); // #27'>'; // #27'[?7h';

        // Reset all attributes, activate normal character set
        TrmWriteString(#27'[0m'#27'(B');

        SysCtrlSuspendThread(KbdThreadID);
      end;
  end;

procedure ResumeTrm;
  begin
    if TrmHandle <> -1 then
      begin
        LnxIoCtl(TrmHandle, TCSETS, @ResumeAttr);

        // Enter XMIT mode
        TrmWriteString(#27'[?1h'); // #27'='; // #27'[?7l';

        SysCtrlResumeThread(KbdThreadID);
      end;
  end;

const
  { Video modes }
  MON1          = $FE;          { Monochrome, ASCII chars only }
  MON2          = $FD;          { Monochrome, graphics chars   }
  COL1          = $FC;          { Color, ASCII chars only      }
  COL2          = $FB;          { Color, graphics chars        }

type
  // A single cell on the screen
  TScrCell = packed record
    Chr: Char; // Character
    Att: Byte; // Attribute
  end;

  // A buffer for the whole screen
  TScrBuffer = array[0..$8000] of TScrCell;
  PScrBuffer = ^TScrBuffer;

var
  // Current screen mode
  ScrMode: Integer;

  // Screen buffer
  ScrBuffer: PScrBuffer = nil;

  // Screen size and coordinates
  ScrWidth, ScrHeight, ScrSize, ScrRow, ScrColumn: Integer;

  // Cursor position arrived in keyboard input stream
  waitsemaphore_for_cursor_position: Longint;

  // True if Cursor is visible
  ScrCursor: Boolean = True;
  ScrCursorScan: Integer = 6; (* Range 0..7 *)

  // Graphics character table
  ScrGraphs: array[#00..#31] of Char;

const
  // --- Table for mapping 'ESC <0..9>' to scancodes --------------------
  KbdScanCtlNum: array['0'..'9'] of SmallWord =
  // 0      1      2      3      4      5      6      7      8      9
    ($8100, $7800, $7900, $7A00, $7B00, $7C00, $7D00, $7E00, $7F00, $8000);

  // --- Table for mapping 'ESC <0..9>' to scancodes --------------------
  KbdScanAltNum: array['0'..'9'] of SmallWord =
  // 0      1      2      3      4      5      6      7      8      9
    ($8100, $7800, $7900, $7A00, $7B00, $7C00, $7D00, $7E00, $7F00, $8000);

  // --- Table for mapping 'ESC <A..Z>' to scancodes --------------------
  KbdScanAltChr: array['A'..'Z'] of SmallWord =
  // A      B      C      D      E      F      G      H      I      J
    ($1E00, $3000, $2E00, $2000, $1200, $2100, $2200, $2300, $1700, $2400,
  // K      L      M      N      O      P      Q      R      S      T
     $2500, $2600, $3200, $3100, $1800, $1900, $1000, $1300, $1F00, $1400,
  // U      V      W      X      Y      Z
     $1600, $2F00, $1100, $2D00, $1500, $2C00);

  // --- Table for mapping 'ESC O <A..Z>' to scancodes ------------------
  KbdScanNrmFn1: array['A'..'Z'] of SmallWord =
  // UP     DOWN   RIGHT  LEFT   -----  END    NUM5   HOME   -----  -----
    ($4800, $5000, $4D00, $4B00, $0000, $4F00, $4c00, $4700, $0000, $0000,
  // -----  -----  ENTER  -----  -----  F1     F2     F3     F4     -----
     $0000, $0000, $1C0D, $0000, $0000, $3B00, $3C00, $3D00, $3E00, $0000,
  // -----  -----  -----  -----  -----  -----
     $0000, $0000, $0000, $0000, $0000, $0000);

  KbdScanSftFn1: array['A'..'Z'] of SmallWord =
  // UP     DOWN   RIGHT  LEFT   -----  END    NUM5   HOME   -----  -----
    ($4800, $5000, $4D00, $4B00, $0000, $4F00, $4c35, $4700, $0000, $0000,
  // -----  -----  ENTER  -----  -----  F1     F2     F3     F4     -----
     $0000, $0000, $1C0D, $0000, $0000, $5400, $5500, $5600, $5700, $0000,
  // -----  -----  -----  -----  -----  -----
     $0000, $0000, $0000, $0000, $0000, $0000);

  KbdScanCtlFn1: array['A'..'Z'] of SmallWord =
  // UP     DOWN   RIGHT  LEFT   -----  END    NUM5   HOME   -----  -----
    ($8D00, $9100, $7400, $7300, $0000, $7500, $8f00, $7700, $0000, $0000,
  // -----  -----  ENTER  -----  -----  F1     F2     F3     F4     -----
     $0000, $0000, $1C0A, $0000, $0000, $5E00, $5F00, $6000, $6100, $0000,
  // -----  -----  -----  -----  -----  -----
     $0000, $0000, $0000, $0000, $0000, $0000);

  // --- Table for mapping 'ESC ESC O <A..Z>' to scancodes --------------
  KbdScanAltFn1: array['A'..'Z'] of SmallWord =
  // UP     DOWN   RIGHT  LEFT   -----  END    NUM5   HOME   -----  -----
    ($9800, $A000, $9D00, $9B00, $0000, $9F00, $0000, $9700, $0000, $0000,
  // -----  -----  ENTER  -----  -----  F1     F2     F3     F4     -----
     $0000, $0000, $1C00, $0000, $0000, $6800, $6900, $6A00, $6B00, $0000,
  // -----  -----  -----  -----  -----  -----
     $0000, $0000, $0000, $0000, $0000, $0000);

  // --- Table for mapping 'ESC O <a..z>' to scancodes ------------------
  KbdScanNrmFn2: array['a'..'z'] of SmallWord =
  // -----  -----  -----  -----  -----  -----  -----  -----  -----  -----
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
  // -----  -----  -----  -----  -----  -----  -----  -----  -----  F5
     $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $3F00,
  // F6     F7     F8     F9     F10    -----
     $4000, $4100, $4200, $4300, $4400, $0000);

  // --- Table for mapping 'ESC O <a..z>' to scancodes ------------------
  KbdScanSftFn2: array['a'..'z'] of SmallWord =
  // -----  -----  -----  -----  -----  -----  -----  -----  -----  -----
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
  // -----  -----  -----  -----  -----  -----  -----  -----  -----  F5
     $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $5800,
  // F6     F7     F8     F9     F10    -----
     $5900, $5A00, $5B00, $5C00, $5D00, $0000);

  // --- Table for mapping 'ESC O <a..z>' to scancodes ------------------
  KbdScanCtlFn2: array['a'..'z'] of SmallWord =
  // -----  -----  -----  -----  -----  -----  -----  -----  -----  -----
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
  // -----  -----  -----  -----  -----  -----  -----  -----  -----  F5
     $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $6200,
  // F6     F7     F8     F9     F10    -----
     $6300, $6400, $6500, $6600, $6700, $0000);

  // --- Table for mapping 'ESC ESC O <a..z>' to scancodes --------------
  KbdScanAltFn2: array['a'..'z'] of SmallWord =
  // -----  -----  -----  -----  -----  -----  -----  -----  -----  -----
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
  // -----  -----  -----  -----  -----  -----  -----  -----  -----  F5
     $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $6C00,
  // F6     F7     F8     F9     F10    -----
     $6D00, $6E00, $6F00, $7000, $7100, $0000);

  // --- Table for mapping 'ESC [ <1..26> ~' to scancodes ---------------
  KbdScanNrmFn3: array[1..26] of SmallWord =
  // HOME   INS    DEL    END    PGUP   PGDN   -----  -----  -----  -----
    ($4700, $5200, $5300, $4F00, $4900, $5100, $0000, $0000, $0000, $0000,
  // F1     F2     F3     F4     F5     -----  F6     F7     F8     F9
     $3B00, $3C00, $3D00, $3E00, $3F00, $0000, $4000, $4100, $4200, $4300,
  // F10    -----  F11    F12    -----  -----
     $4400, $0000, $8500, $8600, $0000, $0000);

  // --- Table for mapping 'ESC [ <1..26> ~' to scancodes ---------------
  KbdScanSftFn3: array[1..26] of SmallWord =
  // HOME   INS    DEL    END    PGUP   PGDN   -----  -----  -----  -----
    ($4700, $0500, $0700, $4F00, $4900, $5100, $0000, $0000, $0000, $0000,
  // F1     F2     F3     F4     F5     -----  F6     F7     F8     F9
     $5400, $5500, $5600, $5700, $5800, $0000, $5900, $5A00, $5B00, $5C00,
  // F5D    -----  F11    F12    -----  -----
     $4400, $0000, $8700, $8800, $0000, $0000);

  // --- Table for mapping 'ESC [ <1..26> ~' to scancodes ---------------
  KbdScanCtlFn3: array[1..26] of SmallWord =
  // HOME   INS    DEL    END    PGUP   PGDN   -----  -----  -----  -----
    ($7700, $0400, $0600, $7500, $8400, $7600, $0000, $0000, $0000, $0000,
  // F1     F2     F3     F4     F5     -----  F6     F7     F8     F9
     $5E00, $5F00, $6000, $6100, $6200, $0000, $6300, $6500, $6600, $6700,
  // F10    -----  F11    F12    -----  -----
     $6800, $0000, $8900, $9000, $0000, $0000);

  // --- Table for mapping 'ESC ESC [ <1..26> ~' to scancodes -----------
  KbdScanAltFn3: array[1..26] of SmallWord =
  // HOME   INS    DEL    END    PGUP   PGDN   -----  -----  -----  -----
    ($9700, $A200, $A300, $9F00, $9900, $A100, $0000, $0000, $0000, $0000,
  // F1     F2     F3     F4     F5     -----  F6     F7     F8     F9
     $6800, $6900, $6A00, $6B00, $6C00, $0000, $6D00, $6E00, $6F00, $7000,
  // F10    -----  F11    F12    -----  -----
     $7100, $0000, $8B00, $8C00, $0000, $0000);

  // --- Table for mapping 'ESC [ [ <A..E>' to scancodes ----------------
  KbdScanNrmFn5: array['A'..'E'] of SmallWord =
  // F1     F2     F3     F4     F5
    ($3B00, $3C00, $3D00, $3E00, $3F00);

  // --- Table for mapping 'ESC [ [ <A..E>' to scancodes ----------------
  KbdScanSftFn5: array['A'..'E'] of SmallWord =
  // F1     F2     F3     F4     F5
    ($5400, $5500, $5600, $5700, $5800);

  // --- Table for mapping 'ESC [ [ <A..E>' to scancodes ----------------
  KbdScanCtlFn5: array['A'..'E'] of SmallWord =
  // F1     F2     F3     F4     F5
    ($5E00, $5F00, $6000, $6100, $6200);

  // --- Table for mapping 'ESC ESC [ [ <A..E>' to scancodes ------------
  KbdScanAltFn5: array['A'..'E'] of SmallWord =
  // F1     F2     F3     F4     F5
    ($6800, $6900, $6A00, $6B00, $6C00);

var
  // Thread handle of keyboard thread
  //KbdThreadID: LongInt = 0;

  // Keyboard buffer
  KbdBuffer: TPipe = (RdFile:-1;WrFile:-1);

  // Number of characters in the keyboard buffer
  KbdBufferCount: LongInt = 0;

  // Semaphore for accessing the keyboard buffer counter
  KbdBufferMutex: LongInt = 0;

  // Next keyboard event to be read from keyboard - needed for TV
  KbdNextKey: TSysKeyEvent = (skeKeyCode: 0; skeShiftState: 0);

  // Mouse event buffer
  MouseBuffer: TPipe = (RdFile:-1;WrFile:-1);

  // Number of events in mouse buffer
  MouseBufferCount: Longint = 0;

  // Semaphore for accessing the mouse buffer counter
  MouseBufferMutex: Longint = 0;

  // for remembering last X.Y
  MouseLastPos: TSysPoint;

// Keyboard worker thread function for use in terminal
function KbdTerminalThread(Args: Pointer): LongInt;
  var
    Buffer: array[0..15] of Char;
    Key: TSysKeyEvent;
    in_queue, work_pos, readc: Word;

  // Decode 'ESC <single character>'
  procedure DecodeEscChr(C: Char);
    begin
      // ALT and a normal key
      Key.skeShiftState := Key.skeShiftState or 8; // ALT

      case C of
        #$0a:
          Key.skeKeyCode := $1c0a; // Alt+Enter

        ' ':
          Key.skeKeyCode := $3920; // Alt+Space

        '0'..'9':
          Key.skeKeyCode := KbdScanAltNum[C];

        'A'..'Z':
          Key.skeKeyCode := KbdScanAltChr[C];

        'a'..'z':
          Key.skeKeyCode := KbdScanAltChr[UpCase(C)];

      end;

    end; (* DecodeEscChr *)


  // Decode 'ESC <character sequence>'
  procedure DecodeEscSeq;

    var
      N1,N2: Integer;
      P: PChar;
      mouse_state: TSysMouseEvent;
      rc: Longint;

    const
      convert_mouse_button:array[0..3] of byte=
        (1 shl 0,  // button 1 .. L
         1 shl 2,  // button 2 .. M
         1 shl 1,  // button 3 .. R
         0      ); // release

    procedure unknown_sequence;
      {$IfDef KbdDebug}
      var
        z:longint;
        u:string;
      {$EndIf}
      begin
        {$IfDef KbdDebug}
        u:='<Unknown Input Escape Sequence :';
        z:=Low(Buffer);
        while z<in_queue do
          begin
            if Buffer[z] in [#$20..Pred('>'),Succ('>')..#$7e] then
              u:=u+Buffer[z]
            else
              u:=u+'#'+IntToStr(Ord(Buffer[z]));
            Inc(z);
          end;
        u:=u+'>';
        LnxDebug(u);
        SysCtrlSleep(1000);
        {$EndIf}

        // invalid .. eat all
        P:=@Buffer[in_queue];

      end; (* unknown_sequence *)

    function ReadNumber:Word;
      begin
        Result:=0;
        while (P^ in ['0'..'9']) and (Result<=999) do
          begin
            Result := 10*Result + Ord(P^)-Ord('0');
            Inc(P);
          end;
      end;

    procedure SetShiftState(Y:Word);
      begin
        // -                      0
        // 2 : Shift              1    Bit0
        // 3 : Alt                2    Bit1
        // 4 : Alt+Shift          3
        // 5 : Ctrl               4    Bit2
        // 6 : Ctrl+Shift         5
        // 7 : Ctrl+Alt           6
        // 8 : Ctrl+Alt+Shift     7
        if Y<2 then Y:=0 else Dec(Y);

        with Key do
          begin
            if (Y and 1)=1 then // +Shift
              skeShiftState:=skeShiftState or (1 shl 0);
            if (Y and 2)=2 then // +Alt
              skeShiftState:=skeShiftState or (1 shl 3);
            if (Y and 4)=4 then // +Ctrl
              skeShiftState:=skeShiftState or (1 shl 2);
          end;
      end; (* ReadShiftState *)

    begin (* DecodeEscSeq *)

      P:=@Buffer[work_pos];

      if P^ = #27 then
        begin
          Key.skeShiftState := Key.skeShiftState or 8; // ALT
          Inc(P);
        end;

      case P^ of
        '[':
          begin
            Inc(P); // '['

            case P^ of
              'M': // Mouse event = Esc '[' 'M' (button-1) x y
                begin
                  Inc(P);

                  with mouse_state do
                    begin
                      smeTime:=SysSysMsCount;
                      with smePos do
                        begin
                          X:=Ord(P[+1])-($20+1);
                          if X<0 then X:=0 else if X>=ScrWidth  then X:=Pred(ScrWidth );
                          Y:=Ord(P[+2])-($20+1);
                          if Y<0 then Y:=0 else if Y>=ScrHeight then Y:=Pred(ScrHeight);
                        end;
                      MouseLastPos:=smePos;
                      smeButtons:=convert_mouse_button[Ord(P[+0]) and 3];
                    end;

                  if MouseBuffer.WrFile<>-1 then
                    begin
                      SysSysWaitSem(MouseBufferMutex);
                      repeat
                        rc:=LnxWrite(MouseBuffer.WrFile, mouse_state, SizeOf(mouse_state));
                      until (rc=SizeOf(mouse_state)) or (rc<0);
                      Inc(MouseBufferCount);
                      MouseBufferMutex := 0;
                    end;

                  Inc(P,3);
                end; (* Mouse *)

              '0'..'9': // Esc '[' number '~'
                        // Esc '[' number ';' number '~' (XTerm)
                begin
                  N1 := ReadNumber;
                  N2 := 0;
                  if P^=';' then
                    begin
                      Inc(P);
                      N2 := ReadNumber;
                    end;


                  if (P^='R') then
                    begin
                      Inc(P);
                      if (N2>=1) and (N2<=ScrWidth ) then ScrColumn:=N2-1;
                      if (N1>=1) and (N1<=ScrHeight) then ScrRow   :=N1-1;
                      waitsemaphore_for_cursor_position:=0;
                    end

                  else
                  if (P^='~') and (N1>=1) and (N1<=26) then
                    begin
                      Inc(P); // '~'
                      SetShiftState(N2);
                      if Key.skeShiftState and 8 = 8 then
                        Key.skeKeyCode := KbdScanAltFn3[N1]
                      else if Key.skeShiftState and 4 = 4 then
                        Key.skeKeyCode := KbdScanCtlFn3[N1]
                      else if Key.skeShiftState and 2 = 2 then
                        Key.skeKeyCode := KbdScanSftFn3[N1]
                      else
                        Key.skeKeyCode := KbdScanNrmFn3[N1];
                    end
                  else
                    // invalid .. eat all
                    unknown_sequence;
                end; (* Esc '[' number '~' *)

              'A'..'D','G': (* Esc '[' 'A' *)
                begin
                  if Key.skeShiftState and 8 = 8 then
                    Key.skeKeyCode := KbdScanAltFn1[P^]
                  else if Key.skeShiftState and 4 = 4 then
                    Key.skeKeyCode := KbdScanCtlFn1[P^]
                  else if Key.skeShiftState and 2 = 2 then
                    Key.skeKeyCode := KbdScanSftFn1[P^]
                  else
                    Key.skeKeyCode := KbdScanNrmFn1[P^];
                  Inc(P);
                end;

              'P': (* Esc '[' 'P' *)
                begin
                  Inc(P);
                  Key.skeKeyCode := 0; (* Pause *)
                end;

              '[': (* Esc '[' '[' 'A' *)
                begin
                  Inc(P);
                  if P^ in ['A'..'E'] then
                    begin
                      if Key.skeShiftState and 8 = 8 then
                        Key.skeKeyCode := KbdScanAltFn5[P^]
                      else if Key.skeShiftState and 4 = 4 then
                        Key.skeKeyCode := KbdScanCtlFn5[P^]
                      else if Key.skeShiftState and 2 = 2 then
                        Key.skeKeyCode := KbdScanSftFn5[P^]
                      else
                        Key.skeKeyCode := KbdScanNrmFn5[P^];
                      Inc(P);
                    end
                  else
                    unknown_sequence;
                end; (* Esc '[' '[' *)

            else
              unknown_sequence;
            end; (* case P^ *)

          end; (* Esc '[' *)

        'O':   (* Esc 'O' *)
          begin
            Inc(P); // 'O'

            if P^ in ['0'..'9'] then // XTerm: Ctrl+F1 = Esc'O5P'
              SetShiftState(ReadNumber);

            case P^ of
              'A'..'Z': (* Esc 'O' 'A' *)
                begin
                  if Key.skeShiftState and 8 = 8 then
                    Key.skeKeyCode := KbdScanAltFn1[P^]
                  else if Key.skeShiftState and 4 = 4 then
                    Key.skeKeyCode := KbdScanCtlFn1[P^]
                  else if Key.skeShiftState and 2 = 2 then
                    Key.skeKeyCode := KbdScanSftFn1[P^]
                  else
                    Key.skeKeyCode := KbdScanNrmFn1[P^];
                  Inc(P);
                end;

              'a'..'z': (* Esc 'O' 'a' *)
                begin
                  if Key.skeShiftState and 8 = 8 then
                    Key.skeKeyCode := KbdScanAltFn2[P^]
                  else if Key.skeShiftState and 4 = 4 then
                    Key.skeKeyCode := KbdScanCtlFn2[P^]
                  else if Key.skeShiftState and 2 = 2 then
                    Key.skeKeyCode := KbdScanSftFn2[P^]
                  else
                    Key.skeKeyCode := KbdScanNrmFn2[P^];
                  Inc(P);
                end;

              else
                unknown_sequence;
              end;

          end; (* Esc 'O' *)
      else
        unknown_sequence;
      end; (* case *)

      work_pos:=P-@Buffer;
    end; (* DecodeEscSeq *)

begin
  in_queue:=0;
  work_pos:=0;
  while True do
    begin

      if in_queue>work_pos then
        begin
          // move to begin of buffer
          Move(Buffer[work_pos],Buffer[0],in_queue-work_pos);
          work_pos:=0;
          FillChar(Buffer[in_queue],SizeOf(Buffer)-in_queue,#0);
        end
      else
        begin
          in_queue:=0;
          work_pos:=0;
          // load first char
          repeat
          until TrmRead(Buffer[in_queue], 1)=1;
          Inc(in_queue);
          Buffer[in_queue]:=#0;
        end;


      Key.skeKeyCode := 0;
      Key.skeShiftState := SysTVGetShiftState;

      // Decode key
      case Buffer[0] of
        #27:
          begin
            // ALT simulation via ESC

            Inc(work_pos);

            if in_queue<14 then
              begin
                readc:=TrmRead(Buffer[in_queue],14-in_queue);
                if readc>0 then
                  Inc(in_queue,readc);
              end;

            Buffer[in_queue]:=#0;


            // Esc
            if Buffer[work_pos]=#0 then
              Key.skeKeyCode := $011B  // ESC
            else
              begin
                // Esc Esc
                if (Buffer[work_pos]=#27) and (Buffer[work_pos+1]=#0) then
                  begin
                    Key.skeKeyCode := $011B; // ESC ESC means ESC itself
                    Inc(work_pos);
                  end

                // Esc 'A'
                else
                if (Buffer[work_pos] in [#$0a,' ','0'..'9','A'..'N','P'..'Z','a'..'z'])
                or (Buffer[work_pos+1]=#0) then
                  begin
                    DecodeEscChr(Buffer[work_pos]);
                    Inc(work_pos);
                  end

                // Esc '[' ...
                // Esc 'O' ...
                else
                  DecodeEscSeq;
              end;
          end (* #27 *)


      else
        Key.skeKeyCode := Ord(Buffer[work_pos]);
        Inc(work_pos);
        if Key.skeKeyCode in [1..27,$7f] then
          case Key.skeKeyCode of
            $7f: Key.skeKeyCode := $0E08; // Backspace
            $09:
              if Key.skeShiftState and 8 = 8 then
                Key.skeKeyCode := $a500   // Alt+Tab (??)
              else if Key.skeShiftState and 4 = 4 then
                Key.skeKeyCode := $9400   // Ctrl+Tab (??)
              else if Key.skeShiftState and 2 = 2 then
                Key.skeKeyCode := $0F00   // Shift+TAB
              else
                Key.skeKeyCode := $0F09;  // TAB

            $0A:
              if Key.skeShiftState and 4 = 4 then
                Key.skeKeyCode := $1c0a   // Ctrl+Enter (??)
              else
                Key.skeKeyCode := $1C0D;  // CR (instead of LF)

          else
            Key.skeShiftState := Key.skeShiftState or 4 // Ctrl
          end;
      end;

      if Key.skeKeyCode <> 0 then
        begin
          repeat
          until LnxWrite(KbdBuffer.WrFile, Key, SizeOf(Key))=SizeOf(Key);
          SysSysWaitSem(KbdBufferMutex);
          Inc(KbdBufferCount);
          KbdBufferMutex := 0;
        end;
    end; (* while true *)

  end; (* KbdTerminalThread *)


function SysKeyPressed: Boolean;
var
  C: Char;
begin
  Result := SysPeekKey(C);
end;

function SysPeekKey(Var Ch: Char): Boolean;
begin
  if KbdNextKey.skeKeyCode = 0 then
    begin
      if KbdBufferCount <> 0 then
        begin
          repeat
          until LnxRead(KbdBuffer.RdFile, KbdNextKey, SizeOf(KbdNextKey))=SizeOf(KbdNextKey);
          Ch := Chr(Lo(KbdNextKey.skeKeyCode));
          SysSysWaitSem(KbdBufferMutex);
          Dec(KbdBufferCount);
          KbdBufferMutex := 0;
          Result := True;
        end
      else
        Result := False;
    end
  else
    begin
      Ch := Chr(Lo(KbdNextKey.skeKeyCode));
      Result := True;
    end;
end;

function SysReadKey: Char;
begin
  if KbdNextKey.skeKeyCode = 0 then
    begin
      repeat
      until LnxRead(KbdBuffer.RdFile, KbdNextKey, SizeOf(KbdNextKey))=SizeOf(KbdNextKey);
      Result := Chr(Lo(KbdNextKey.skeKeyCode));
      SysSysWaitSem(KbdBufferMutex);
      Dec(KbdBufferCount);
      KbdBufferMutex := 0;
    end
  else
    Result := Chr(Lo(KbdNextKey.skeKeyCode));

  if Result = #0 then
    KbdNextKey.skeKeyCode := KbdNextKey.skeKeyCode shr 8
  else
    KbdNextKey.skeKeyCode := 0;
end;

procedure SysFlushKeyBuf;
var
  I: Integer;
begin
  SysSysWaitSem(KbdBufferMutex);

  for I := 0 to KbdBufferCount - 1 do
    LnxRead(KbdBuffer.RdFile, KbdNextKey, SizeOf(KbdNextKey));

  KbdNextKey.skeKeyCode := 0;
  KbdBufferMutex := 0;
end;

procedure SysWrtCharStrAtt(CharStr: Pointer; Len, X, Y: SmallWord; var Attr: Byte);
var
  Src: PChar;
  Dst, I: LongInt;
begin
  Src := CharStr;
  Dst := Y * ScrWidth + X;

  for I := 0 to Len - 1 do
    begin
      ScrBuffer^[Dst + I].Chr := Src[I];
      ScrBuffer^[Dst + I].Att := Attr;
    end;

  SysTVShowBuf(Dst * 2, Len * 2);
end;

function SysReadAttributesAt(x,y: SmallWord): Byte;
begin
  Result := ScrBuffer^[Y * ScrWidth + X].Att;
end;

function SysReadCharAt(x,y: SmallWord): Char;
begin
  Result := ScrBuffer^[Y * ScrWidth + X].Chr;
end;

procedure SysScrollUp(X1,Y1,X2,Y2,Lines,Cell: SmallWord);
var
  I, J, Src, Dst, Len: Integer;
  Cell2: TScrCell absolute Cell;
begin
  if Lines > Y2 - Y1 + 1 then Lines := Y2 - Y1 + 1;

  // FullScreen
  if (Lines = 1) and (X1 = 0) and (Y1 = 0) and (X2 = ScrWidth - 1) and (Y2 = ScrHeight - 1) then
    begin
      // goto lower left corner and send IND
      TrmWriteString(#27'[s'                            // save cursor position
                    +#27'['+IntToStr(ScrHeight)+';1H'   // X:=1 Y=25
                    +#27'D'                             // IND
                    +#27'[u');                          // restore cursor position

      // skip upper screen portion
      Y1:=Y2;
    end;

  Src := ScrWidth * (Y1 + Lines) + X1;
  Dst := ScrWidth * Y1 + X1;
  Len := X2 - X1 + 1;

  for I := Y1 to Y2 - Lines do
    begin
      Move(ScrBuffer^[Src], ScrBuffer^[Dst], Len * 2);
      SysTVShowBuf(Dst * 2, Len * 2);

      Inc(Src, ScrWidth);
      Inc(Dst, ScrWidth);
    end;

  for I := 1 to Lines do
    begin
      for J := 0 to Len - 1 do
        ScrBuffer^[Dst + J] := Cell2;

      SysTVShowBuf(Dst * 2, Len * 2);

      Inc(Src, ScrWidth);
      Inc(Dst, ScrWidth);
    end;

end;

procedure SysScrollDn(X1,Y1,X2,Y2,Lines,Cell: SmallWord);
var
  I, J, Src, Dst, Len: Integer;
  Cell2: TScrCell absolute Cell;
begin
  if Lines > Y2 - Y1 + 1 then Lines := Y2 - Y1 + 1;

  Src := ScrWidth * (Y2 - Lines) + X1;
  Dst := ScrWidth * Y2 + X1;
  Len := X2 - X1 + 1;

  for I := Y1 to Y2 - Lines do
    begin
      Move(ScrBuffer^[Src], ScrBuffer^[Dst], Len * 2);
      SysTVShowBuf(Dst * 2, Len * 2);

      Dec(Src, ScrWidth);
      Dec(Dst, ScrWidth);
    end;

  for I := 1 to Lines do
    begin
      for J := 0 to Len - 1 do
        ScrBuffer^[Dst + J] := Cell2;

      SysTVShowBuf(Dst * 2, Len * 2);

      Dec(Src, ScrWidth);
      Dec(Dst, ScrWidth);
    end;
end;

procedure SysGetCurPos(var X,Y: SmallWord);
begin
  X := ScrColumn;
  Y := ScrRow;
end;

function SysTVDetectMouse: Longint;
begin
  //Result := 0;
  // Should ask terminfo "BT" ?
  //if ? in [] then
  Result := 3; // X10/X11 protocol supports 3 buttons
end;

procedure SysTVInitMouse(var X,Y: Integer);
begin
  SysTVKbdInit;

  if MouseBuffer.WrFile=-1 then
    LnxPipe(MouseBuffer);

  with MouseLastPos do
    begin
      X:=0;Y:=0;
    end;
  X:=0;Y:=0;
  TrmWriteString({#27'[?9h'}    // X10 Mouse reporting on
                +#27'[?1000h'); // X11 Mouse reporting on
end;

procedure SysTVDoneMouse(Close: Boolean);
begin
  TrmWriteString(#27'[?1000l'   // X11 Mouse reporting off
                {+#27'[?9l'});  // X10 Mouse reporting off

  // flush buffer ?
  if Close then
    begin
      LnxClose(MouseBuffer.WrFile);
      LnxClose(MouseBuffer.RdFile);
    end;
end;


procedure SysTVShowMouse;
begin
    // paint esc code does not exist ?
end;

procedure SysTVHideMouse;
begin
    // hide esc code does not exist ?
end;

procedure SysTVUpdateMouseWhere(var X,Y: Integer);
begin
  X := MouseLastPos.X;
  Y := MouseLastPos.Y;
end;

function SysTVGetMouseEvent(var Event: TSysMouseEvent): Boolean;
begin
  SysSysWaitSem(MouseBufferMutex);

  if MouseBufferCount=0 then
    Result:=false
  else
    begin
      Result:=true;
      repeat
      until LnxRead(MouseBuffer.RdFile, Event, SizeOf(Event))=SizeOf(Event);
      Dec(MouseBufferCount);
    end;

  MouseBufferMutex := 0;
end;

procedure SysTVKbdInit;
begin
  if KbdThreadID<>0 then Exit;

  // Get a pipe for the keyboard buffer
  LnxPipe(KbdBuffer);

  // Start keyboard converter thread
  // must use System unit for exception handler !
  BeginThread(nil, 8*1024, KbdTerminalThread, nil, 0, KbdThreadID);
end;

function SysTVGetKeyEvent(var Event: TSysKeyEvent): Boolean;
begin
  if KbdNextKey.skeKeyCode = 0 then
    begin
      SysSysWaitSem(KbdBufferMutex);

      if KbdBufferCount <> 0 then
        begin
          repeat
          until LnxRead(KbdBuffer.RdFile, Event, SizeOf(KbdNextKey))=SizeOf(KbdNextKey);
          Dec(KbdBufferCount);
          Result := True;
        end
      else
        Result := False;

      KbdBufferMutex := 0;
    end
  else
    begin
      Event := KbdNextKey;
      KbdNextKey.skeKeyCode := 0;
      Result := True;
    end;
end;

function SysTVPeekKeyEvent(var Event: TSysKeyEvent): Boolean;
begin
  if KbdNextKey.skeKeyCode = 0 then
    begin
      SysSysWaitSem(KbdBufferMutex);

      if KbdBufferCount <> 0 then
        begin
          repeat
          until LnxRead(KbdBuffer.RdFile, KbdNextKey, SizeOf(KbdNextKey))=SizeOf(KbdNextKey);
          Event := KbdNextKey;
          Dec(KbdBufferCount);
          Result := True;
        end
      else
        Result := False;

      KbdBufferMutex := 0;
    end
  else
    begin
      Event := KbdNextKey;
      Result := True;
    end;
end;

function SysTVGetShiftState: Byte;
var
  B: Longint;
begin
  B := 6;
  if LnxIoCtl(TrmHandle, TIOCLINUX, @B) < 0 then
    begin
      // access prevented by mc or X
      B := 0;
      // maybe use KbdNextKey.skeShiftState ?
    end;

  // Linux              VpSysLow
  // Bit0: L,R Shift    Bit1: L Shift
  // Bit1: AltGr        Bit3: either Alt
  // Bit2: Ctrl         Bit2: Ctrl
  // Bit3: Alt          Bit3: either Alt

  Result:=((B and 1) shl 1)                             // Shift
       or (B and 12);                                   // Ctrl,Alt
  if (B and 2)=2 then Result:=Result or (1 shl 3);      // AltGr
end;

procedure SysTVSetCurPos(X,Y: Integer);
var
  S: string;
begin
  ScrColumn := X;
  ScrRow := Y;
  S := #27'[' + IntToStr(Y + 1) + ';' + IntToStr(X + 1) + 'H';
  TrmWriteString(S);
end;

procedure SysTVSetCurType(Y1,Y2: Integer; Show: Boolean);
begin

  if Show then
    begin
      TrmWriteString(#27'[?25h');     // Set Mode: Cursor visble

      if Y1<=0 then Y1:=(Abs(Y1)*7) div 100;
      if Y1>7 then Y1:=7;

      // 8:=0%(full block)..1:=100%(invisible)
      // Y1=0....................7

      if IsTermType('linux') then
        TrmWriteString(#27'[?'+IntToStr(8-Y1)+'c'); (* VGA-SOFT.TXT *)

      ScrCursorScan:=Y1;

    end
  else
    TrmWriteString(#27'[?25l');       // Reset Mode: Cursor invisble

  ScrCursor:=Show;
end;

procedure SysTVGetCurType(var Y1,Y2: Integer; var Visible: Boolean);
begin
  // emulates 8 Scanlines..
  Y1 := ScrCursorScan;
  Y2 := 7;
  Visible := ScrCursor;
end;

function ColourSequence16(Attr,LastAttr:word):string;
const
  // Color table
  ScrPalette: array[0..7] of Char = '04261537';

begin
  if Attr=LastAttr then
    Result:=''
  else
    begin
      Result:=#27'[';

      if ((Attr xor LastAttr) and $80)<>0 then
        if Attr and $80=$80 then
          // + blink
          Result:=Result+'5;'
        else
          // - blink
          Result:=Result+'25;';

      if ((Attr xor LastAttr) and $08)<>0 then
        if (Attr and $08)=$08 then
          // + bold
          Result:=Result+'1;'
        else
          // - bold
          Result:=Result+'22;';

      // + foreground
      if ((Attr xor LastAttr) and $07)<>0 then
        Result:=Result+'3'+ScrPalette[Attr and $07]+';';

      // + background
      if ((Attr xor LastAttr) and $70)<>0 then
        Result:=Result+'4'+ScrPalette[(Attr and $70) shr 4]+';';

      Result[Length(Result)]:='m';
    end;

end;

function ColourSequenceMono(Attr,LastAttr:word):string;
(*  const
  MonoAttr:array[boolean,boolean] of string[10]=
    (('30;40;';       // black on black
      '37;40;');      // white on black
     ('30;47;';       // black on white
      '37;47;'));     // white on white*)

begin
  if Attr=LastAttr then
    Result:=''
  else
    begin
      Result:=#27'[';

      if ((Attr xor LastAttr) and $80)<>0 then
        if Attr and $80=$80 then
          // + blink
          Result:=Result+'5;'
        else
          // - blink
          Result:=Result+'25;';

      if ((Attr xor LastAttr) and $08)<>0 then
        if (Attr and $08)=$08 then
          // + bold
          Result:=Result+'1;'
        else
          // - bold
          Result:=Result+'22;';

      // underline bit changed
      if ((Attr and $07)=$01)<>((LastAttr and $07)=$01) then
        if (Attr and $07)=$01 then
          // + underline
          Result:=Result+'4;'
        else
          // - underline
          Result:=Result+'24;';

      // invisible black/normal/inverse/invisible white
      if ((Attr xor LastAttr) and $77)<>0 then
        begin
          // Result:=Result+MonoAttr[(Attr and $07)=0,(Attr and $70)=0];
          if (Attr and $07)=0 then
            Result:=Result+'30;'
          else
            Result:=Result+'37;';

          if (Attr and $70)=0 then
            Result:=Result+'40;'
          else
            Result:=Result+'47;'

        end;

      Result[Length(Result)]:='m';
    end;

end;

var
  ColourSequence:function(Attr,LastAttr:word):string=ColourSequence16;

procedure SysTVShowBuf(Pos,Size: Integer);
var
  Attr, LastAttr: Byte;
  Mode: Boolean;
  Reset_Wrap,
  Ctrl, Data: string;
  J, X, Y, X_STOP, Y_STOP, XLimit: Integer;
  NextLimit: Integer;
  NeedGotoXY: Boolean;
  Temp: string;
begin
  if Odd(Pos) then
    begin
      Dec(Pos);
      Inc(Size);
    end;

  if Odd(Size) then
    Inc(Size);

  Pos:=Pos shr 1;
  Size:=Size shr 1;

  if Size<=0 then Exit;
  if Pos+Size>ScrWidth*ScrHeight then
    Size:=ScrWidth*ScrHeight-Pos;

  if ScrCursor then
    TrmWriteString(#27'[?25l'#27'7')  (* hide cursor, save state *)
  else
    TrmWriteString(          #27'7'); (*              save state *)

  Y := Pos div ScrWidth;
  X := Pos mod ScrWidth;

  Y_STOP := (Pos+Size) div ScrWidth;
  X_STOP := (Pos+Size) mod ScrWidth;

  Mode := False;
  Data := '';
  Reset_Wrap := '';

  J := Pos;
  NeedGotoXY := True;

  repeat
    if (X=X_STOP) and (Y=Y_STOP) then Break;

    if NeedGotoXY then
      begin

        if Data<>'' then
          begin
            TrmWriteString(Data);
            Data:='';
          end;

        NeedGotoXY := False;
        // move Corsor to upper left start position
        Ctrl := #27'[' + IntToStr(Y + 1) + ';' + IntToStr(X + 1) + 'H';
        TrmWriteString(Ctrl);
        LastAttr := ScrBuffer^[J].Att xor $ff;
      end;

    if Y=Y_STOP then
      XLimit := X_STOP
    else
      XLimit := ScrWidth;

    while X < XLimit do
      begin

        with ScrBuffer^[J] do
          begin

            (*** Attribute *****************************************)
            Attr := Att;

            if Attr<>LastAttr then
              begin
                TrmWriteString(Data);
                Data:='';

                TrmWriteString(ColourSequence(Attr,LastAttr));

                LastAttr:=Attr;
              end; (* Attr<>LastAttr *)


            if J = NextLimit - 1 then
              if J =ScrWidth*ScrHeight-1 then
                begin
                  Data := Data + #27'[7l'; (* autowrap off *)
                  Reset_Wrap := #27'[7h';  (* autowrap on  *)
                end;

            (*** Character *****************************************)
            case Chr of
              #1..#6:
                begin
                  if Mode then
                    begin
                      Data := Data + #27'(B'; (* select ISO-8859-1 *)
                      Mode := False;
                    end;

                  Data := Data + ScrGraphs[Chr];
                end;

              #7..#31:
                begin
                  if not (Mode or (ScrMode in [MON1,COL1])) then
                    begin
                      Data := Data + #27'(0'; (* select vt100 graphics *)
                      Mode := True;
                    end;

                  Data := Data + ScrGraphs[Chr];
                end;

              #0, #$7f..#$9f:
                Data := Data + ' ';

            else

              if Mode then
                begin
                  Data := Data + #27'(B';   (* select ISO-8859-1 *)
                  Mode := False;
                end;

              Temp := Chr;
              CP866ToUtf8(Temp);
              Data := Data + Temp;

            end; (* case Chr *)


            if Length(Data) > 127 then
              begin
                TrmWriteString(Data);
                Data := '';
              end;

          end; (* with ScrBuffer^[J] *)

        Inc(J);
        Inc(X);

      end; (* while J < NextLimit *)

    (* next line reached ? *)
    if X=ScrWidth then
      begin
        X:=0;
        Inc(Y);
      end;

    (* optimize blank line ends *)
    if (Length(Data)>3) and (X=0) then
      if (Data[Length(Data)]=' ') and (Data[Length(Data)-1]=' ') and (Data[Length(Data)-2]=' ') then
        begin

          while (Data<>'') and (Data[Length(Data)]=' ') do
            Dec(Data[0]);

          Data := Data + #27'[K'; (* erase up to end of line *)

          NeedGotoXY:=True;
        end;

  until false; (* main lines loop *)


  if Mode then
    Data := Data + #27'(B';         (* select ISO-8859-1 *)

  TrmWriteString(Data);

  if ScrCursor then
    TrmWriteString(#27'[?25h'#27'8')  (* show cursor, restore state *)
  else
    TrmWriteString(          #27'8'); (*              restore state *)

  if Reset_Wrap<>'' then
    TrmWriteString(Reset_Wrap);
end;

procedure SysTVClrScr;
const
  Cell: TScrCell=(Chr:' ';Att:$07);
var
  I: LongInt;
begin
  for I := 0 to ScrSize div 2 - 1 do
    ScrBuffer^[I] := Cell;

  SysTVSetCurPos(0, 0);

  // default colour ; erase screen
  TrmWriteString(#27'[0m'+ColourSequence($07,not $07)+#27'[2J');
end;

function SysTVGetScrMode(_Size: PSysPoint; _Align: Boolean): Integer;
var
  Size: TWinSize;
begin
  if _Size <> nil then
    begin
      if LnxIoCtl(TrmHandle, TIOCGWINSZ, @Size) = 0 then
        begin
          _Size^.X := Size.ws_Col;
          _Size^.Y := Size.ws_Row;
        end;
//      _Size^.X := ScrWidth;
//      _Size^.Y := ScrHeight;
    end;

  Result := ScrMode;
end;

procedure SysTVSetScrMode(Mode: Integer);
var
  Size: TWinSize;
begin
  case Mode of
    COL1,COL2: ColourSequence:=ColourSequence16;
    MON1,MON2: ColourSequence:=ColourSequenceMono;
  else
    ColourSequence:=ColourSequence16;
  end;

  // Set mapping of graphics characters
  case Mode of
    MON1, COL1: ScrGraphs := #032#094#086#060#062#043#045#079
                          +  #032#032#091#093#035#061#032#043
                          +  #043#043#043#045#124#043#043#043
                          +  #043#043#043#043#043#043#045#124;

    MON2, COL2: ScrGraphs := #032#094#086#060#062#043#177#096
                          +  #032#048#048#048#048#104#097#108
                          +  #107#106#109#113#120#118#119#117
                          +  #116#110#108#107#106#109#113#120;
  end;

  ScrMode := Mode;
end;

function SysTVGetSrcBuf: Pointer;
begin
  Result := ScrBuffer;
end;

function StrToIntDef(const S: string; Default: Integer): Integer;
var
  Error: LongInt;
begin
  Val(S, Result, Error);
  if Error <> 0 then
    Result := Default;
end;

procedure SysTVInitCursor;
var
  Term: string;
  Size: TWinSize;
begin
  // Initialize terminal
  TrmInit;

  // Get window size, calculate usable screen, get buffer
  if LnxIoCtl(TrmHandle, TIOCGWINSZ, @Size) = 0 then
    begin
      ScrWidth  := Size.ws_Col;
      ScrHeight := Size.ws_Row;
    end;

  ScrSize := ScrWidth * ScrHeight * 2;
  if ScrSize > SizeOf(TScrBuffer) then
    begin
      ScrHeight := SizeOf(TScrBuffer) div (2 * ScrWidth);
      ScrSize   := ScrWidth * ScrHeight * 2;
    end;

  if assigned(ScrBuffer) then
    FreeMem(ScrBuffer);
  GetMem(ScrBuffer, ScrSize);

  // Try to default to a reasonable video mode
  if IsTermType('xterm') or IsTermType('linux') or IsTermType('rxvt')
  or (SysGetEnvString('COLORTERM', nil)<>nil) then
    SysTVSetScrMode(COL2)
  else if IsTermType('vt100') then
    SysTVSetScrMode(MON2)
  else
    SysTVSetScrMode(MON1);

  // Clear the screen
  SysTVClrScr;
end;

procedure SysTvDoneCursor;
begin
  TrmDone;
end;

procedure SysCtrlSleep(Delay: Integer);
var
  Req, Rem: TTimeSpec;
  Result: LongInt;
begin
  Req.tv_Sec := Delay div 1000;
  Req.tv_NSec := (Delay mod 1000) * 1000000;
  repeat
    Result := -LnxNanoSleep(Req, Rem);
    Req := Rem;
  until Result <> EAGAIN;
end;

function SysGetValidDrives: Longint;
begin
  Result := 0; // 000..000100 -- drive C: only
end;

procedure SysDisableHardErrors;
begin
  // nop
end;

function SysKillProcess(Process: Longint): Longint;
begin
  Result := -LnxKill(Process, SIGKILL);
end;

function SysAllocSharedMemory(var _Base: Pointer; _Name: pChar; _Size: Longint): Longint;
begin
  Unimplemented('SysAllocSharedMemory');
end;

function SysAccessSharedMemory(var _Base: Pointer; _Name: pChar): Longint;
begin
  Unimplemented('SysAccessSharedMemory');
end;

procedure SysFreeSharedMemory(_Base: Pointer; _Handle: Longint);
begin
  Unimplemented('SysFreeSharedMemory');
end;

function SysAllocSharedMem(Size: Longint; var MemPtr: Pointer): Longint;
begin
  // Unimplemented('SysAllocSharedMem');
  Result := -1; // Unsupported
end;

function SysGiveSharedMem(MemPtr: Pointer): Longint;
begin
  Unimplemented('SysGiveSharedMem');
end;

function SysPipeCreate(var ReadHandle,WriteHandle: Longint; Size: Longint): Longint;
var
  p:TPipe;
begin
  Result:=LnxPipe(p); // buffersize is ignored
  if Result=0 then
    begin
      ReadHandle :=p.RdFile;
      WriteHandle:=p.WrFile;
    end;
end;

function SysPipePeek(Pipe: Longint; Buffer: Pointer; BufSize: Longint; var BytesRead: Longint; var IsClosing: Boolean): Longint;
var
  Poll: tPollFd;
begin
  Poll.FD := Pipe;
  Poll.Events := POLLIN or POLLPRI or POLLERR or POLLHUP or POLLNVAL;
  if LnxPoll(@Poll, 1, 100) = 0 then
    Result := 0 // No structures changed: return 0
  else
    begin
      if Poll.rEvents and (POLLIN or POLLPRI) <> 0 then
        BytesRead := 1 // We haven't really read anything, but there is data
      else
        IsClosing := True; // Everything else is an error
      Result := 0;
    end;
end;

function SysPipeClose(Pipe: Longint): Longint;
begin
  Result:=SysFileClose(Pipe);
end;

function SysLoadResourceString(ID: Longint; Buffer: PChar; BufSize: Longint): PChar;
var
  p: PLinux_Resource_String;
  Len: Longint;
begin
  Buffer^ := #0;
  p := LnxGetResourceStringAddress(ID);
  if assigned(p) then
    with p^ do
      begin
        Len := fStrLength;
        if Len > BufSize then
          Len := BufSize;

        StrLCopy(Buffer, fStr, Len);
      end;
  Result := Buffer;
end;

function SysFileUNCExpand(Dest,Name: PChar): PChar;
begin
  Unimplemented('SysFileUNCExpand');
end;

function SysGetSystemError(Code: Longint; Buffer: PChar; BufSize: Longint;var MsgLen: Longint): PChar;
begin
  Result := SysLoadResourceString(LinuxErrorStringResourceBase + Code, Buffer, BufSize);
  MsgLen := StrLen(Buffer);
end;

type
  (* country/locale settings
   used by SysGetCurrencyFormat/SysGetDateFormat/SysGetTimeFormat

   /usr/share/locale has some files, but incomplete *)
  Pcountry_info         =^Tcountry_info;
  Tcountry_info         =
    record
      initialized       :boolean;
      (* Currency *)
      CuString          :array[0..20] of char;
      CuFormat          :byte;
      CuNegFormat       :byte;
      CuDecimals        :byte;
      CuThousandSep     :char;
      CuDecimalSep      :char;
      (* Date Format *)
      DaDateSeparator   :char;
      DaShortDateFormat :array[0..40] of char;
      DaLongDateFormat  :array[0..40] of char;
      (* Time Format *)
      TiTimeSeparator   :char;
      TiTimeAMString    :array[0..20] of char;
      TiTimePMString    :array[0..20] of char;
      TiShortTimeFormat :array[0..40] of char;
      TiLongTimeFormat  :array[0..40] of char;
    end;


var
  CountryInfo           :Tcountry_info=
    (
      initialized       :false;
      (* Currency *)
      CuString          :'$';
      CuFormat          :0;
      CuNegFormat       :0;
      CuDecimals        :2;
      CuThousandSep     :',';
      CuDecimalSep      :'.';
      (* Date Format *)
      DaDateSeparator   :'/';
      DaShortDateFormat :'mm/dd/yy';
      DaLongDateFormat  :'mmmm d, yyyy';
      (* Time Format *)
      TiTimeSeparator   :':';
      TiTimeAMString    :'am';
      TiTimePMString    :'pm';
      TiShortTimeFormat :'hh:mm';
      TiLongTimeFormat  :'hh:mm:ss'
    );

// exapmle: country_info=_EUR_3_8_2_._,_._yyyy/mm/dd_dddd, d. mmmm. yyyy_.___hh.mm_hh.mm.ss_
function retrieve_country_info:Pcountry_info;
var
  i                   :PChar;
  sep                 :char;

  procedure ReadChar(c:PChar;n:word);
  begin

    // copy
    while (i^<>#0) and (i^<>sep) and (n>0) do
      begin
        c^:=i^;
        Inc(c);
        Inc(i);
        Dec(n);
      end;

    // search sep
    while (i^<>#0) and (i^<>sep) do
      Inc(i);

    // skip sep
    if i^=sep then
      Inc(i);

    // zero terminate
    if n>0 then
      c^:=#0;

  end;

  procedure ReadByte(var b:byte;const min,max:byte);
  var
    z         :array[0..5] of char;
    k         :integer;
    t         :byte;
  begin
    ReadChar(@z,5);
    Val(z,t,k);
    if (k=0) and (t>=min) and (t<=max) then
      b:=t;
  end;

begin

  with CountryInfo do
    begin
      Result:=@CountryInfo;
      if initialized then
        Exit;

      initialized:=true;
      i:=SysGetEnvString('country_info','');
      if StrLen(i)=0 then Exit;

      sep:=i^;
      Inc(i);

      ReadChar(@CuString,20);
      ReadByte(CuFormat,0,3);
      ReadByte(CuNegFormat,0,15);
      ReadByte(CuDecimals,0,100);
      ReadChar(@CuThousandSep,1);
      ReadChar(@CuDecimalSep,1);

      ReadChar(@DaDateSeparator,1);
      ReadChar(@DaShortDateFormat,40);
      ReadChar(@DaLongDateFormat,40);

      ReadChar(@TiTimeSeparator,1);
      ReadChar(@TiTimeAMString,20);
      ReadChar(@TiTimePMString,20);
      ReadChar(@TiShortTimeFormat,40);
      ReadChar(@TiLongTimeFormat,40);

    end;

end; (* retrieve_country_info *)

procedure SysGetCurrencyFormat(CString: PChar; var CFormat, CNegFormat, CDecimals: Byte; var CThousandSep, CDecimalSep: Char);
begin
  with retrieve_country_info do
    begin
      StrCopy(CString,CuString);
      CFormat := CuFormat;
      CNegFormat := CuNegFormat;
      CThousandSep := CuThousandSep;
      CDecimalSep := CuDecimalSep;
      CDecimals := CuDecimals;
    end;
end;

procedure SysGetDateFormat(var DateSeparator: Char; ShortDateFormat,LongDateFormat: PChar);
begin
  with retrieve_country_info do
    begin
      DateSeparator := DaDateSeparator;
      StrCopy(ShortDateFormat, DaShortDateFormat);
      StrCopy(LongDateFormat, DaLongDateFormat);
    end;
end;

procedure SysGetTimeFormat(var TimeSeparator: Char; TimeAMString,TimePMString,ShortTimeFormat,LongTimeFormat: PChar);
begin
  with retrieve_country_info do
    begin
      TimeSeparator := TiTimeSeparator;
      StrCopy(TimeAmString, TiTimeAMString);
      StrCopy(TimePmString, TiTimePMString);
      StrCopy(ShortTimeFormat, TiShortTimeFormat);
      StrCopy(LongTimeFormat, TiLongTimeFormat);
    end;
end;

function SysGetModuleName(var Address: Pointer; Buffer: PChar; BufSize: Longint): PChar;
var
  ModuleName, Temp: PChar;
begin
  StrCopy(Buffer, 'UNKNOWN');
  Result := Buffer;
//
  ModuleName := Argv^[0];
  Temp := StrRScan(ModuleName, '/');
  if Temp = nil then Temp := ModuleName else Temp := Temp + 1;
  StrLCopy(Buffer,  Temp, BufSize - 1);
  Result := Buffer;

end;

procedure SysDisplayConsoleError(PopupErrors: Boolean; Title, Msg: PChar);
var
  TitleLen: Longint;

function WriteToConsole(s:PChar): Longint;
  begin
    SysFileWrite(SysFileStdErr, s^, StrLen(s), Result);
  end;

const
  underline:PChar='=';
  linebreak:PChar=#13#10;

begin
  TitleLen:=WriteToConsole(Title);
  WriteToConsole(linebreak);
  while TitleLen>0 do
    begin
      WriteToConsole(underline);
      Dec(TitleLen)
    end;
  WriteToConsole(linebreak);

  WriteToConsole(Msg);
  WriteToConsole(linebreak);
end;

procedure SysMessageBox(_Msg, _Title: PChar; _Error: Boolean);
begin
  SysDisplayConsoleError(true,_Title,_Msg);
  if _Error then
    SysBeep;

  SysReadKey;
end;

procedure SysDisplayGUIError(Title, Msg: PChar);
begin
  SysMessageBox(Msg,Title,true);
end;

procedure SysBeep;
var
  Count: Longint;
const
  Bell:Char=#7;
begin
  SysFileWrite(SysFileStdErr,bell, 1, Count);
end;

procedure SysBeepEx(Freq,Dur: Longint);
var
  period,rc:Longint;
begin
  if Freq<=0 then Exit;

  // Sound...
  period:=1193180 div Freq;
  if LnxIoCtl(TrmHandle, KIOCSOUND, Ptr(period))<>0 then
    begin
      // broken terminal
      SysBeep;
      Exit;
    end;

  // Delay
  SysCtrlSleep(Dur);

  // NoSound
  LnxIoCtl(TrmHandle, KIOCSOUND, nil);
end;

function SysGetVolumeLabel(Drive: Char): ShortString;
begin
  Result := '';
end;

function SysSetVolumeLabel(Drive: Char; _Label: ShortString): Boolean;
begin
  Result := False;
end;

function SysGetForegroundProcessId: Longint;
begin
  Unimplemented('SysGetForegroundProcessId');
end;

function SysGetBootDrive: Char;
begin
  if FileSystem = fsDosUpper then Result := 'C' else Result := 'c';
end;

function SysGetDriveType(Drive: Char): TDriveType;
var
  StatFS: TStatFS;
begin
  if (Drive <> 'C') and (Drive <> 'c') then
  begin
    Result := dtInvalid;
    Exit;
  end;

  LnxStatFS('/', StatFS);

  case StatFS.f_Type of
    $00004d44:
      Result:=dtHDFAT;
    $F995E849:
      Result:=dtHDHPFS;
    $0000EF51, // old
    $0000EF53:
      Result:=dtHDEXT2;
    $00006960: // ISO
      Result:=dtCDROM;
  else
      Result:=dtInvalid;
  end;
end;

function SysGetVideoModeInfo( Var Cols, Rows, Colours : Word ): Boolean;
begin
  if ScrWidth=0 then
    begin
      Cols := 24;
      Rows := 80;
      Colours:=1;
      SysGetVideoModeInfo := false;
      Exit;
    end;

  Cols := ScrWidth;
  Rows := ScrHeight;
  if ScrMode in [COL1,COL2] then
    Colours := 8
  else
    Colours := 1;

  Result := True;
end;

function SysGetVisibleLines( var Top, Bottom: Longint ): Boolean;
var
  Cols, Rows, Colours: Word;
begin
  if SysGetVideoModeInfo( Cols, Rows, Colours ) then
  begin
    Result := True;
    Top := 1;
    Bottom := Rows;
  end
  else
    Result := False;
end;

function SysSetVideoMode( Cols, Rows: Word ): Boolean;
begin
  //Unimplemented('SysSetVideoMode');

  // possible way:
  //   cd /usr/lib/kbd/consolefonts
  //   consolechars -f cp850-8x8
  // but then you see a lot of broken things...

  // simple solution: return true if desired mode=current mode...
  SysSetVideoMode:=(Cols=ScrWidth) and (Rows=ScrHeight);
end;

//--------------[ SEMPAHORE FUNCTIONS ]-------------------------------

type
  sem_types             =(sem_mutex,sem_event);

  Tnamed_sem_table      =array[1..10] of
    record
      used              :boolean;
      change_sem        :integer;
      sem_name          :array[0..80] of char;
      sem_type          :sem_types;
      counter           :integer;
      owner             :word;
      open_count        :word;
    end;

var
  named_sem_table       :^Tnamed_sem_table=nil;

const
  sem_handle_sign       =Ord('S') shl 24 +Ord('E') shl 16;
  named_sem_mutex       :longint=0; (* protect table *)
  empty_pchar           :char=#0;

procedure init_named_sem_table;
begin
  if not Assigned(named_sem_table) then
    begin
      // can not us New here - New uses semaphores
      // (New(named_sem_table);)
      SysMemAlloc(SizeOf(named_sem_table^),HeapAllocFlags,Pointer(named_sem_table));
      FillChar(named_sem_table^,SizeOf(named_sem_table^),0);
    end;
end;

(* search mutex name in table and return index or 0 if not found *)
function search_named_sem(const _Name:PChar):word;
var
  i:word;
begin
  for i:=Low(named_sem_table^) to High(named_sem_table^) do
    with named_sem_table^[i] do
      if used and (StrComp(_Name,sem_name)=0) then
        begin
          search_named_sem:=i;
          Exit;
        end;
  search_named_sem:=0;
end;

(* search free index in named_sem_table or return 0 *)
function search_named_sem_free:word;
var
  i:word;
begin
  for i:=Low(named_sem_table^) to High(named_sem_table^) do
    if not named_sem_table^[i].used then
      begin
        search_named_sem_free:=i;
        Exit;
      end;
  search_named_sem_free:=0;
end;

procedure check_invalid_sem_handle(_Handle: TSemHandle;_sem_type:sem_types);
begin
  if  ((_Handle and $ffff0000)=sem_handle_sign) then
    begin
      _Handle:=_Handle and $0000ffff;
      if  (_Handle>=Low (named_sem_table^))
      and (_Handle<=High(named_sem_table^)) then
        if  (named_sem_table^[_Handle].used)
        and (named_sem_table^[_Handle].sem_type=_sem_type) then
          Exit;
    end;

  (*$IFDEF DEBUG*)
  LnxDebug('VpSysLnx.pas: invalid semaphore handle ('+IntToStr(_Handle)+')');
  (*$ENDIF*)
  RunError(204{RTE_Invalid_Pointer});
end;

function SemCreateEvent(_Name: pChar; _Shared, _State: Boolean): TSemHandle;
begin
  Unimplemented('SemCreateEvent');
end;

function SemAccessEvent(_Name: PChar): TSemHandle;
begin
  Unimplemented('SemAccessEvent');
end;

function SemPostEvent(_Handle: TSemhandle): Boolean;
begin
  Unimplemented('SemPostEvent');
end;

function SemResetEvent(_Handle: TSemhandle; var _PostCount: Longint): Boolean;
begin
  Unimplemented('SemResetEvent');
end;

function SemWaitEvent(_Handle: TSemHandle; _TimeOut: Longint): Boolean;
begin
  Unimplemented('SemWaitEvent');
end;

procedure SemCloseEvent(_Handle: TSemHandle);
begin
  Unimplemented('SemCloseEvent');
end;

function SemCreateMutex(_Name: PChar; _Shared, _State: Boolean): TSemHandle;
var
  i                   :word;
begin

  SysSysWaitSem(named_sem_mutex);
  init_named_sem_table;

  SemCreateMutex:=-1;

  if _Name=nil then
    _Name:=@empty_pchar;

  i:=StrLen(_Name);

  if (i<SizeOf(named_sem_table^[1].sem_name)) and (i<>0) then
    i:=search_named_sem(_Name); (* already exist ? *)

  (* 0: does not exist or _Name='' *)
  if i=0 then
    begin
      i:=search_named_sem_free;
      if i<>0 then
        with named_sem_table^[i] do
          begin
            change_sem:=1;
            used:=true;
            FillChar(sem_name,SizeOf(sem_name),0);
            StrCopy(sem_name,_Name); (* assume short name *)
            sem_type:=sem_mutex;
            counter:=Ord(_State);
            owner:=GetThreadID;
            open_count:=1;
            SemCreateMutex:=sem_handle_sign+i;
            change_sem:=0;
          end;
    end;

  named_sem_mutex:=0;

end;

function SemRequestMutex(_Handle: TSemHandle; _TimeOut: Longint): Boolean;
var
  i:word;
begin

  if _TimeOut<>-1 then
    RunError(201{RTE_Range_Check});

  SysSysWaitSem(named_sem_mutex);
  init_named_sem_table;

  check_invalid_sem_handle(_Handle,sem_mutex);

  named_sem_mutex:=0;
  SemRequestMutex:=false;

  with named_sem_table^[_Handle and $0000ffff] do

    repeat
      SysSysWaitSem(change_sem);

      if (counter=0) or (owner=GetThreadID) then
        begin
          Inc(counter);
          owner:=GetThreadID;
          change_sem:=0;
          SemRequestMutex:=true;
          Break;
        end;

      change_sem:=0;
      SysCtrlSleep(31);
    until false;

end;

function SemAccessMutex(_Name: PChar): TSemHandle;
var
  i:word;
begin
  init_named_sem_table;

  if (_Name=nil) or (StrLen(_Name)=0) then
    RunError(20{RTE_Invalid_Pointer});

  i:=search_named_sem(_Name); (* already exist ? *)
  if i=0 then
    SemAccessMutex:=-1
  else
    with named_sem_table^[i] do
      begin
        SysSysWaitSem(change_sem);
        Inc(open_count);
        SemAccessMutex:=sem_handle_sign+i;
        change_sem:=0;
      end;

end;

function SemReleaseMutex(_Handle: TSemHandle): Boolean;
begin

  SysSysWaitSem(named_sem_mutex);
  init_named_sem_table;
  check_invalid_sem_handle(_Handle,sem_mutex);
  named_sem_mutex:=0;

  with named_sem_table^[_Handle and $0000ffff] do
    begin
      SysSysWaitSem(change_sem);
      if counter>0 then
        begin
          Dec(counter);
          SemReleaseMutex:=true;
        end
      else
        SemReleaseMutex:=false;

      change_sem:=0;
    end;

end;

procedure SemCloseMutex(_Handle: TSemHandle);
begin

  SysSysWaitSem(named_sem_mutex);
  init_named_sem_table;
  check_invalid_sem_handle(_Handle,sem_mutex);
  named_sem_mutex:=0;

  with named_sem_table^[_Handle and $0000ffff] do
    begin
      SysSysWaitSem(change_sem);
      Dec(open_count);
      //if open_count=0 then
      //  used:=false;
      change_sem:=0;
    end;

end;

function SysMemInfo(_Base: Pointer; _Size: Longint; var _Flags: Longint): Boolean;
begin
  // Doesn't seem to be supported. Could be emulated by storing the
  // access flags in the list of allocated mmap memory blocks and
  // getting the flags from this list.
  _Flags := sysmem_read or sysmem_execute;
  Result := False;
end;

function SysSetMemProtection(_Base: Pointer; _Size: Longint; _Flags: Longint): Boolean;
begin
  Result := (LnxMProtect(_Base, _Size, _Flags) = 0);
end;

function SysClipCanPaste: Boolean;
begin
  Result := False;
end;

function SysClipCopy(P: PChar; Size: Longint): Boolean;
begin
  Result := False;
end;

function SysClipPaste(var Size: Integer): Pointer;
begin
  Result := nil;
end;

// Retrieve various system settings, bitmapped:
// 0: Enhanced keyboard installed

function SysGetSystemSettings: Longint;
begin
  Result := 1;
end;

type
  PFpReg = ^TFpReg;
  TFpReg = record
    losig:   LongInt;
    hisig:   LongInt;
    signexp: SmallWord;
  end;

  PContextRecord = ^TContextRecord;
  TContextRecord = record
    ContextFlags: LongInt;
    ctx_env:    array [0..6] of LongInt;
    ctx_stack:  array [0..7] of TFpReg;
    ctx_SegGs:  LongInt;
    ctx_SegFs:  LongInt;
    ctx_SegEs:  LongInt;
    ctx_SegDs:  LongInt;
    ctx_RegEdi: LongInt;
    ctx_RegEsi: LongInt;
    ctx_RegEax: LongInt;
    ctx_RegEbx: LongInt;
    ctx_RegEcx: LongInt;
    ctx_RegEdx: LongInt;
    ctx_RegEbp: LongInt;
    ctx_RegEip: LongInt;
    ctx_SegCs:  LongInt;
    ctx_EFlags: LongInt;
    ctx_RegEsp: LongInt;
    ctx_SegSs:  LongInt;
  end;

  PExcFrame = ^TExcFrame;
  TExcFrame = record
    Next: PExcFrame;
    Desc: Pointer;
  end;

const // os2base.pas
  context_Control               = $00000001;    { SS:ESP, CS:EIP, EFLAGS, EBP  }
  context_Integer               = $00000002;    { EAX, EBX, ECX, EDX, ESI, EDI }
  context_Segments              = $00000004;    { DS, ES, FS, GS               }
  context_Floating_Point        = $00000008;    { numeric coprocessor state    }

  XCPT_CONTINUE_SEARCH          = $00000000;  // exception not handled
  XCPT_CONTINUE_EXECUTION       = $FFFFFFFF;  // exception handled

  xcpt_Ctrl_Break_message       : array[1..4] of char = '^C'#13#10;
  xcpt_unhandled_message        : string[21]          = 'unhandled exception !';

var
  SIGINT_processing             : boolean = false;

// StdCall: Last to first, Stack cleanup
function SysRaiseExceptionC(Xcpt: PXcptReportRecord;Context:PContextRecord): LongInt; StdCall; assembler;
  {&Frame-} {&Uses ebx,ecx,edx}
asm
    mov ebx,Context
    mov ecx,fs:[0]                      // First exception frame
    mov edx,Xcpt                        // Xcpt

    // report to debugger
    push 2                              // ArgCount
    push edx                            // exception report             // P1
    push ebx                            // context                      // P2
    push [edx].longint                  // exception code
    Call RaiseNotification

    cmp [edx].longint,xcpt_Ctrl_Break
    jne @not_SIGINT

    cmp [SIGINT_processing],true
    je @RET

    mov [SIGINT_processing],true

  @not_SIGINT:

  @LOOP:
    // not handled by System unit exception to runtime mapper ?
    cmp ecx,-1
    je @no_handler
    jecxz @no_handler

    pushad
                                        // Cdecl+
      push 0                            // unused
        push ebx                        // Context
          push ecx                      // Registration
            push edx                    // Report
              call [ecx].TExcFrame.Desc
      add esp,4*4
      or eax,eax                        // XCPT_CONTINUE_SEARCH ?

    popad
    jnz @RET

    mov ecx, [ecx].TExcFrame.Next // Get previous frame
    jmp @LOOP


  @no_handler:
    // xcpt_Ctrl_Break is not handled by System._ExceptionHandler
    cmp [edx].longint,xcpt_Ctrl_Break
    jne @other_unhandled

    push type   xcpt_Ctrl_Break_message
    push offset xcpt_Ctrl_Break_message
    push STDERR_FILENO
    call LnxWrite
    // no stack cleanup needed here

    push 255
    call _Halt

  @other_unhandled:
    push offset xcpt_unhandled_message
    call Internal_Error

  @RET:
    xor     eax, eax
end;


// you have to assign eax,eip,esp yourself
procedure GetContextRecord(Context: PContextRecord); assembler;
  {&Frame-} {&Uses eax}
asm
    mov     eax, [Context]
    mov     [eax].TContextRecord.ContextFlags,context_Control or context_Integer or context_Segments

    // not processesed: ctx_env and ctx_stack

    mov     [eax].TContextRecord.ctx_SegGs, Gs
    mov     [eax].TContextRecord.ctx_SegFs, Fs
    mov     [eax].TContextRecord.ctx_SegEs, Es
    mov     [eax].TContextRecord.ctx_SegDs, Ds

    // TContextRecord.ctx_RegEax
    mov     [eax].TContextRecord.ctx_RegEbx, ebx
    mov     [eax].TContextRecord.ctx_RegEcx, ecx
    mov     [eax].TContextRecord.ctx_RegEdx, edx
    mov     [eax].TContextRecord.ctx_RegEsi, esi
    mov     [eax].TContextRecord.ctx_RegEdi, edi
    mov     [eax].TContextRecord.ctx_RegEbp, ebp

    // eip must be assigned by caller
    // TContextRecord.ctx_RegEip

    mov     [eax].TContextRecord.ctx_SegCs, cs

    pushfd
    pop     [eax].TContextRecord.ctx_EFlags

    // esp must be assigned by caller
    // TContextRecord.ctx_RegEsp

    mov     [eax].TContextRecord.ctx_SegSs, ss

end;


function SysRaiseException(Xcpt: PXcptReportRecord): LongInt; StdCall; orgname; assembler;
  {&Frame-} {&Uses ebx}
var
  Context:TContextRecord;
asm
    // fill context record
    mov [Context.ctx_RegEax],eax
    lea eax,[Context]
    push eax
    call GetContextRecord

    // get eip after "call SysRaiseException"
    mov ebx,[esp+@Locals+@Uses]
    mov Context.ctx_RegEip,ebx
    // get esp after "call SysRaiseException"
    lea ebx,[esp+@Locals+@Uses+4+@Params]
    mov Context.ctx_RegEsp,ebx


    // Result:=SysRaiseExceptionC(Xcpt,Context);
    mov ebx,Xcpt

    push eax                    // Context
    push ebx                    // Report
    call SysRaiseExceptionC

end;

function SysUnwindException(Handler: PExcFrame; TargetIP: Pointer; Xcpt: PXcptReportRecord): LongInt; stdcall; orgname; assembler;
  {&Frame-}{&uses ebx,ecx,edx}
var
  Context:TContextRecord;
asm
    // fill context record (in case of invalid unwind)
    mov [Context.ctx_RegEax],eax
    lea eax,[Context]
    push eax
    call GetContextRecord
    // fill context record

    // get eip after "call SysUnwindException"
    mov ebx,[esp+@Locals+@Uses]
    mov Context.ctx_RegEip,ebx
    // get esp after "call SysUnwindException"
    lea ebx,[esp+@Locals+@Uses+4+@Params]
    mov Context.ctx_RegEsp,ebx


    mov ebx, TargetIP                   // Get TargetIP
    mov [esp+@locals+@uses], ebx;       // And store it as return address

                                        // eax=context
    mov ebx,Handler                     // ebx=target Handler
    mov ecx,fs:[0]                      // ecx=First exception frame
    mov edx,Xcpt                        // edx=Report

    or [edx].TXcptReportRecord.fHandlerFlags,$02 // eh_Unwinding

    // should System.RaiseNotification called here for debugging ?

  @LOOP:
    cmp ecx,ebx                         // Target handler reached ?
    je @RET                             // If so, return

    pushad

      push 0                            // unused
        push eax                        // Context
          push ecx                      // Registration
            push edx                    // Report
              call [ecx].TExcFrame.Desc // Call handler

      add esp, 4*4                      // Cleanup stack

    popad

    mov ecx,[ecx].TExcFrame.Next        // Get previous frame
    mov fs:[0],ecx                      // Remove current frame
    jmp @LOOP

  @RET:
    cmp [edx].longint,xcpt_Ctrl_Break
    jne @not_SIGINT

    mov [SIGINT_processing],false

  @not_SIGINT:
    xor eax,eax
end;

// Except signal handler
procedure HandleExceptSignal(SigNum: LongInt; _Context: LongInt); Cdecl;
var
  SigContext: TSigContext absolute _Context;
  Report:  TXcptReportRecord;
  Context: TContextRecord;

begin

  if (SigNum=SIGINT) and (GetThreadId<>1) then
    begin
      LnxKill(MainThread.ProcessPid,SIGINT);
      Exit;
    end;

  with Report do
    begin

      FillChar(Report,SizeOf(Report),0);

      { Linux exception code are $C00xyyzz, with...

         x: Signal number, see SIG* constants in Linux.pas for details
        yy: i386 Trap code (for signals which are caused by a trap)
        zz: Lower 7 bit of coprocessor status (for signals which are
            caused by a floating point fault)                        }

      ExceptionNum := $C0000000 or (SigNum shl 16);

      if SigNum in [SIGBUS, SIGFPE, SIGHUP, SIGSEGV, SIGTERM, SIGTRAP] then
        ExceptionNum := ExceptionNum or (SigContext.TrapNo shl 8);

      case ExceptionNum of
        xcpt_Float_Generic:
          ExceptionNum := ExceptionNum or (SigContext.FpState.Status and $7F);

        xcpt_In_Page_Error, xcpt_Access_Violation:
        begin
          cParameters := 2;
          ExceptionInfo[0] := SigContext.err { and $02 - fixed in SysUtils };
          ExceptionInfo[1] := SigContext.cr2;
        end;
      end;

      ExceptionNum := ExceptionNum and $F0FFFFFF;

      fHandlerFlags := 1; // cNonContinuable

      // NestedXcptReportRecord := nil;

      ExceptionAddress := Pointer(SigContext.Eip);

    end; (* Report *)


  with Context, SigContext do
    begin

      FillChar(Context,SizeOf(Context),0);

      ContextFlags := context_Control or context_Integer or context_Segments;

      if Assigned(fpstate) then
        begin
          Move(fpstate^.cw,ctx_env,SizeOf(ctx_env));
          Move(fpstate^._st,ctx_stack,SizeOf(ctx_stack));
          ContextFlags :=ContextFlags or context_Floating_Point;
        end;

      ctx_SegSs  := Ss.r32;
      ctx_SegGs  := Gs.r32;
      ctx_SegFs  := Fs.r32;
      ctx_SegEs  := Es.r32;
      ctx_SegDs  := Ds.r32;
      ctx_SegCs  := Cs.r32;

      ctx_RegEdi := Edi;
      ctx_RegEsi := Esi;
      ctx_RegEdx := Edx;
      ctx_RegEcx := Ecx;
      ctx_RegEbx := Ebx;
      ctx_RegEax := Eax;

      ctx_RegEbp := Ebp;
      ctx_RegEsp := {Esp;}esp_at_signal;
      ctx_RegEip := Eip;

      ctx_EFlags := EFlags;
    end; (* Context, SigContext *)


  SysRaiseExceptionC(@Report,@Context);

end;

procedure SetSignalHandlers;
const
  OtherSignals: array[1..21] of LongInt =
    (SIGABRT, SIGALRM, SIGBUS,  SIGFPE,    SIGHUP,    SIGILL,  SIGINT,
     SIGIO,   SIGIOT,  SIGKILL, SIGPIPE,   SIGPOLL,   SIGPWR,  SIGQUIT,
     SIGSEGV, SIGTERM, SIGTRAP, SIGUSR2,   SIGVTALRM, SIGXCPU, SIGXFSZ);
var
  Act, Old: TSigAction;
  I: LongInt;
begin
  FillChar(Act, SizeOf(Act), 0);

  // Set handler for SIGUSR1 - needed for
  // supending and restarting threads
  Act.sa_Handler := @HandleStateSignal;
  LnxSigAction(SIGUSR1, Act, Old);

  // Set handler for SIGCHLD - needed for
  // notifying the main thread when a
  // child thread terminates.
  Act.sa_Handler := @HandleChildSignal;
  LnxSigAction(SIGCHLD, Act, Old);

  // Set other handlers - needed for
  // mapping of signals to exceptions.
  for I := Low(OtherSignals) to High(OtherSignals) do
  begin
    // uncomment this if you want to use INT3 in a debugger:
    //if I=SIGTRAP then Continue;
    Act.sa_Handler := @HandleExceptSignal;
    Act.sa_Flags := SA_NODEFER;
    LnxSigAction(OtherSignals[I], Act, Old);
  end;
end;


var
  PrevXcptProc: Pointer= Ptr(-1);

function SignalHandler(Report:       PXcptReportRecord;
                       Registration: PExcFrame;
                       Context:      PContextRecord;
                       P:            Pointer): longint; StdCall;
begin
  Result := xcpt_Continue_Search;

  if Report^.ExceptionNum = xcpt_Ctrl_Break then
    begin
      if Assigned(CtrlBreakHandler) then
        if CtrlBreakHandler then
          Result := xcpt_Continue_Execution;
    end;

  XcptProc := PrevXcptProc;
end;


procedure SysCtrlSetCBreakHandler;
begin
  if PrevXcptProc=Ptr(-1) then
    begin
      PrevXcptProc := XcptProc;
      XcptProc := @SignalHandler
    end;
end;

// In Windows, the first 64k of address space for a process are set up
// to be inaccessible.  This helps in debugging as dereferenced nil
// (and small invalid) pointers cause an access violation.

// Attempt to do the same in Linux (if possible)

var
  GotLowMemory: Boolean = False;

procedure ReserveLowMemory;
var
  P: Pointer;
begin
  if IsDll then
    Exit;

  if not GotLowMemory then
    begin
      P := LnxMmap(nil, $FFFF, PROT_NONE, MAP_ANON or MAP_PRIVATE or MAP_FIXED, 0, 0);
      GotLowMemory := P = nil;
      if (Integer(P) <> -1) and (P <> nil) then
        LnxMUnMap(P, $FFFF);
    end;
end;

procedure FreeLowMemory;
begin
  if GotLowMemory then
    begin
      LnxMUnMap(nil, $FFFF);
      GotLowMemory := False;
    end;
end;

procedure SysLowInitPreTLS; assembler; {&Uses All} {&Frame-}
asm
    // Adjust stack bottom
    sub     MainThread.Stack, eax

    // Get process ID
    call    LnxGetPid
    mov     MainThread.ThreadPid, eax
    mov     MainThread.ProcessPid, eax

    // Create FS selector for main thread
    push    1 // LDT entry #1
    push    OFFSET MainThread
    push    TYPE MainThread
    call    GetNewSelector
    mov     fs, ax
    mov     MainThread.TibSelector, fs

    // Clear exception handler chain
    xor     eax, eax
    mov     MainThread.ExceptChain, eax

    // Initialize thread info table
    mov     edi, OFFSET Threads
    mov     DWORD [edi], OFFSET MainThread
    add     edi, 4
    mov     ecx, TYPE Threads / 4 - 4
    repnz   stosw

    // Reserve low memory if possible
    call    ReserveLowMemory

    // Get argument values
    mov ebx, esp
    add ebx, @uses+32
    mov Argv, ebx

    // Get argument count
    mov ebx, [esp+28+@uses]
    mov Argc, ebx

    // Get environment strings
    shl ebx, 2
    add ebx, esp
    add ebx, 36+@uses;
    mov Env, ebx;
    mov Environment, ebx;

    // Set needed signal handlers
    call SetSignalHandlers;
end;

procedure SysLowInitPostTLS;
begin
  // Nothing
end;

procedure SysLowInit;
begin
end;
