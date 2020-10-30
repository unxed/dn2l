{$I STDEFINE.INC}
{* Terms of distribution of this code: Public domain. *}
{* unxed, 2020 *}
unit dnsys;

// shims for system functions needed to build DN under FPC

interface

uses sysutils;

const
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

end.