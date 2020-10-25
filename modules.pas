unit Modules;
(******

LoadModule & GetProcAddress
Written by Cat 2:5030/1326.13
(for use in DN/2)

******)

{$I STDEFINE.INC}
{&Delphi+}

interface

type
  PPointer = ^Pointer;

  { обёртки для вызова системных функций }
function LoadModule(ModuleName: PChar; var LibHandle: Integer): Boolean;
procedure FreeModule(LibHandle: Integer);
function GetProcAddress(LibHandle: Integer; ProcName: PChar;
     var ProcAddr: Pointer): Boolean;

{ сначала пытается загрузить DLL из каталога ДН-а, если не удаётся, }
{ то грузит из подкаталога, имеющего то же имя, что и модуль        }
{ т.е. если передана строка PLUGIN, то делается попытка загрузки:   }
{    C:\DN\PLUGIN.DLL           }
{    C:\DN\PLUGIN\PLUGIN.DLL    }
function LoadPluginModule(const ModuleName: String;
     var LibHandle: Integer): Boolean;
function LoadPluginModuleAndGetProcAddress(const ModuleName: String;
     var LibHandle: Integer; ProcNames: array of PChar;
     ProcAddrs: array of PPointer): Boolean;

implementation

{$IFDEF OS2}
uses
  Os2Def, Os2Base,
  Advance
  ;

function LoadModule(ModuleName: PChar; var LibHandle: Integer): Boolean;
  begin
  Result := (DosLoadModule(nil, 0, ModuleName, LibHandle) = 0);
  if not Result then
    LibHandle := 0;
  end;

procedure FreeModule(LibHandle: Integer);
  begin
  if LibHandle <> 0 then
    begin
    DosFreeModule(LibHandle);
    LibHandle := 0;
    end;
  end;

function GetProcAddress(LibHandle: Integer; ProcName: PChar;
     var ProcAddr: Pointer): Boolean;
  begin
  Result := (DosQueryProcAddr(LibHandle, 0, ProcName, ProcAddr) = 0);
  if not Result then
    ProcAddr := nil;
  end;
{$ENDIF}

{$IFDEF Win32}
uses
  Windows,
  Advance
  ;

function LoadModule(ModuleName: PChar; var LibHandle: Integer): Boolean;
  begin
  LibHandle := Windows.LoadLibrary(ModuleName);
  Result := (LibHandle > HINSTANCE_ERROR);
  if not Result then
    LibHandle := 0;
  end;

procedure FreeModule(LibHandle: Handle);
  begin
  if LibHandle <> 0 then
    begin
    Windows.FreeLibrary(LibHandle);
    LibHandle := 0;
    end;
  end;

function GetProcAddress(LibHandle: Handle; ProcName: PChar;
     var ProcAddr: Pointer): Boolean;
  begin
  ProcAddr := Windows.GetProcAddress(LibHandle, ProcName);
  Result := (ProcAddr <> nil);
  end;
{$ENDIF}

function LoadPluginModule(const ModuleName: String;
     var LibHandle: Integer): Boolean;
  var
    S: String;
  begin
  S := SourceDir+ModuleName+'.DLL'#0;
  Result := LoadModule(@S[1], LibHandle);
  if not Result then
    begin
    S := SourceDir+ModuleName+'/'+ModuleName+'.DLL'#0; // slash change by unxed
    Result := LoadModule(@S[1], LibHandle);
    end;
  end;

function LoadPluginModuleAndGetProcAddress(const ModuleName: String;
     var LibHandle: Integer; ProcNames: array of PChar;
     ProcAddrs: array of PPointer): Boolean;
  var
    I: Integer;
  begin
  Result := LoadPluginModule(ModuleName, LibHandle);
  if Result then
    for I := Low(ProcAddrs) to High(ProcAddrs) do
      Result := Result and GetProcAddress(LibHandle, ProcNames[I],
           ProcAddrs[I]^)
  else
    for I := Low(ProcAddrs) to High(ProcAddrs) do
      ProcAddrs[I]^:= nil;

  if not Result then
    LibHandle := 0;
  end;

end.
