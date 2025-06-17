unit vp2fp;

interface

uses sysutils;

type
  TFileSize  = Longint;
  SmallWord  = System.Word;
  i32 = LongInt;
  lFile = File;

  TXlat = array[Char] of Char;

const
  stOK = 0; { No error }

  open_access_ReadOnly          = fmOpenRead;
  open_access_WriteOnly         = fmOpenWrite;
  open_access_ReadWrite         = fmOpenReadWrite;
  open_share_DenyReadWrite      = fmShareExclusive;
  open_share_DenyWrite          = fmShareDenyWrite;
  open_share_DenyRead           = fmShareDenyRead;
  open_share_DenyNone           = fmShareDenyNone;
  // fmShareCompat ?

// from fp rtl
const
   { TCStream seek origins }
   soFromBeginning = 0;
   soFromCurrent = 1;
   soFromEnd = 2;

function MemAvail: Longint;
function MaxAvail: Longint;

function SysFileOpen(FileName: PChar; Mode: Longint; var Handle: Longint): Longint;
function SysFileCreate(FileName: PChar; Mode,Attr: Longint; var Handle: Longint): Longint;
function SysFileSeek(Handle, Distance, Method: Longint; var Actual: Longint): Longint;
function SysFileWrite(Handle: Longint; const Buffer; Count: Longint; var Actual: Longint): Longint;
function SysFileClose(Handle: Longint): Longint;

function SysFileRead(Handle: Longint; var Buffer; Count: Longint; var Actual: Longint): Longint;
function SysFileSetSize(Handle: Longint; NewSize: TFileSize): Longint;

function lFExpand(const Path: string): string;
procedure lTrueName(const Name: String; var S: String);

procedure ClrIO;

function SysDiskSizeLongX(const fn: PChar): Int64;

procedure SysTVKbdDone;
procedure SysTVKbdInit;

function lfGetShortFileName(S: String): String;

function SysGetValidDrives: Longint;

function SysFileIsDevice(hFile: LongInt) : Byte;

function QueryABCSort(CP: Word; var ABCSortXlat: TXLat): Boolean;

implementation

// https://www.freepascal.org/docs-html/current/prog/progse37.html
function MemAvail: Longint;
begin
    Result := High(LongInt);
end;

function MaxAvail: Longint;
begin
    Result := High(LongInt);
end;

function SysFileOpen(FileName: PChar; Mode: Longint; var Handle: Longint): Longint;
var
 h: Longint;
begin
    h := FileOpen(FileName, Mode);
    Handle := h;
    Result := h;
end;

function SysFileCreate(FileName: PChar; Mode,Attr: Longint; var Handle: Longint): Longint;
var
 h: THandle;
begin
    h := FileCreate(FileName, Mode);
    Handle := Longint(h);
    Result := Longint(h);
end;

function SysFileSeek(Handle, Distance, Method: Longint; var Actual: Longint): Longint;
var
 a: longint;
begin
    a := FileSeek(THandle(Handle), Distance, Method);
    Actual := a;
    Result := a;
end;

function SysFileWrite(Handle: Longint; const Buffer; Count: Longint; var Actual: Longint): Longint;
var
    res: Longint;
begin
    res := FileWrite(Handle, Buffer, Count);
    Actual := res;
    Result := res;
end;

function SysFileClose(Handle: Longint): Longint;
begin
    FileClose(THandle(Handle));
end;

function SysFileRead(Handle: Longint; var Buffer; Count: Longint; var Actual: Longint): Longint;
var
    res: Longint;
begin
    res := FileRead(Handle, Buffer, Count);
    Actual := res;
    Result := res;
    // fixme: porting stub
end;

function SysFileSetSize(Handle: Longint; NewSize: TFileSize): Longint;
var
    res: Boolean;
begin
    res := FileTruncate(Handle, NewSize);
    Result := Longint(res);
    // fixme: porting stub
end;

function lFExpand(const Path: string): string;
begin
  lFExpand := ExpandFileName(Path);
end;

procedure lTrueName(const Name: String; var S: String);
begin
  S := Name;
end;

procedure ClrIO;
begin
//  while KeyPressed do
//    ReadKey;
end;

function SysDiskSizeLongX(const fn: PChar): Int64;
var
  Path: string;
begin
  Path := string(fn);  // преобразуем PChar в string

  {$ifdef linux}
  // Это падает на FreeBSD 12 x64
  Exit(SysUtils.DiskFree(SysUtils.AddDisk(ExtractFileDir(Path))));
  {$endif}

  {$ifdef windows}
  Exit(SysUtils.DiskFree(SysUtils.GetDriveIDFromLetter(ExtractFileDrive(Path))));
  {$endif}

  // Не удалось определить
  Exit(-1);
end;

procedure SysTVKbdDone;
begin
end;

procedure SysTVKbdInit;
begin
end;

function lfGetShortFileName(S: String): String;
begin
  Exit(S);
end;

function SysGetValidDrives: Longint;
begin
  Result := 256;
end;

function SysFileIsDevice(hFile: LongInt) : Byte;
begin
  Exit(0);
end;

function QueryABCSort(CP: Word; var ABCSortXlat: TXLat): Boolean;
begin
  Result := False; //!! ���� �� ॠ�������� (04.09.2005)
end;

end.
