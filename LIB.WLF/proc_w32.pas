unit Proc_W32;
(******

Task List plugin
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{JO: 20-10-2002 - исправлено попадание kernel32.dll в список процессов под w9x }

{&Delphi+}
{&Use32+}

interface

uses
  Defines, Objects2, Collect
  ;

type
  PProcessItem = ^TProcessItem;
  TProcessItem = object(TObject)
    Name: ShortString;
    Pid: LongInt;
    function GetString: String;
    end;

  PProcessCollection = ^TProcessCollection;
  TProcessCollection = object(TSortedCollection)
    function Compare(P1, P2: Pointer): Integer; virtual;
    end;

function GetProcessList: PProcessCollection;
function ProcessKill(Pid: LongInt): LongInt;
function ProcessSwitch(Pid: LongInt): LongInt;

implementation

uses
  VpSysLow, Windows,
  DnIni, Advance1, Advance2
  ;

function TProcessItem.GetString: String;

  function Hex8(A: LongInt): Str8;
    const
      Hex: array[0..15] of Char =
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B',
         'C', 'D', 'E', 'F');
    begin
    Result := Hex[(A shr 28) and $F]+
      Hex[(A shr 24) and $F]+
      Hex[(A shr 20) and $F]+
      Hex[(A shr 16) and $F]+
      Hex[(A shr 12) and $F]+
      Hex[(A shr 08) and $F]+
      Hex[(A shr 04) and $F]+
      Hex[(A shr 00) and $F];
    end;

  begin
  if ShowExePaths then
    Result := Name
  else
    Result := GetName(Name);
  GetString := Hex8(Pid)+'  '+Result;
  end { TProcessItem.GetString: };

function TProcessCollection.Compare(P1, P2: Pointer): Integer;
  begin
  Compare := PProcessItem(P1)^.Pid-PProcessItem(P2)^.Pid;
  end;

procedure FillWin95ProcessCollection(Collection: PProcessCollection);
  type
    PProcessEntry32 = ^TProcessEntry32;
    TProcessEntry32 = packed record
      dwSize: LongInt;
      cntUsage: LongInt;
      th32ProcessID: LongInt;
      th32DefaultHeapID: LongInt;
      th32ModuleID: LongInt;
      cntThreads: LongInt;
      th32ParentProcessID: LongInt;
      pcPriClassBase: LongInt;
      dwFlags: LongInt;
      szExeFile: array[0..MAX_PATH-1] of Char;
      end;
  var
    hKernel: Handle;
    CreateToolhelp32Snapshot: function (dwFlags, th32ProcessID: LongInt)
    : Handle stdcall;
    Process32First: function (hSnapshot: Handle; lppe: PProcessEntry32)
    : Boolean stdcall;
    Process32Next: function (hSnapshot: Handle; lppe: PProcessEntry32)
    : Boolean stdcall;
    hSnapshot: Handle;
    Entry: TProcessEntry32;
    P: PProcessItem;
    I: Byte;
  begin { FillWin95ProcessCollection }
  hKernel := GetModuleHandle('kernel32.dll');
  if hKernel = 0 then
    Exit;
  @CreateToolhelp32Snapshot := GetProcAddress(hKernel,
       'CreateToolhelp32Snapshot');
  @Process32First := GetProcAddress(hKernel, 'Process32First');
  @Process32Next := GetProcAddress(hKernel, 'Process32Next');
  if  (@CreateToolhelp32Snapshot = nil) or (@Process32First = nil)
       or (@Process32Next = nil)
  then
    Exit;

  hSnapshot := CreateToolhelp32Snapshot(2 {TH32CS_SNAPPROCESS}, 0);
  if hSnapshot = invalid_Handle_Value then
    Exit;

  Entry.dwSize := SizeOf(Entry);
  if not Process32First(hSnapshot, @Entry) then
    Exit;

  {JO: нам совсем не нужно, чтобы kernel32.dll попадал в список процессов, }
  {    а посему repeat ... until здесь использовать не надо                }
  {repeat}
  while Process32Next(hSnapshot, @Entry) do
    begin
    P := New(PProcessItem, Init);
    if Entry.dwSize > SizeOf(Entry)-SizeOf(Entry.szExeFile) then
      with Entry do
        begin
        P^.Name := '';
        for I := 0 to 254 do
          if szExeFile[I] = #0 then
            Break
          else
            P^.Name := P^.Name+szExeFile[I];
        end
    else
      P^.Name := '?'#0;
    P^.Pid := Entry.th32ProcessID;
    if not (FilteredList and (Pos('#'+UpStrg(GetName(P^.Name))+'#',
            '#'+UpStrg(UserTaskFilter)+'#') > 0))
    then
      Collection^.Insert(P)
    else
      Dispose(P, Done);
    Entry.dwSize := SizeOf(Entry);
    end;
  {until not Process32Next(hSnapshot, @Entry);}

  CloseHandle(hSnapshot);
  end { FillWin95ProcessCollection };

procedure FillWinNTProcessCollection(Collection: PProcessCollection);
  type
    PWideChar = ^WideChar;

    PThreadInfo = ^TThreadInfo;
    TThreadInfo = packed record
      ftCreationTime: TFileTime;
      dwUnknown1: LongInt;
      dwStartAddress: LongInt;
      dwOwningPID: LongInt;
      dwThreadID: LongInt;
      dwCurrentPriority: LongInt;
      dwBasePriority: LongInt;
      dwContextSwitches: LongInt;
      dwThreadState: LongInt;
      dwUnknown2: LongInt;
      dwUnknown3: LongInt;
      dwUnknown4: LongInt;
      dwUnknown5: LongInt;
      dwUnknown6: LongInt;
      dwUnknown7: LongInt;
      end;

    PProcessInfo = ^TProcessInfo;
    TProcessInfo = packed record
      dwOffset: LongInt;
      dwThreadCount: LongInt;
      dwUnkown1: array[1..6] of LongInt;
      ftCreationTime: TDateTime;
      dwUnkown2: LongInt;
      dwUnkown3: LongInt;
      dwUnkown4: LongInt;
      dwUnkown5: LongInt;
      dwUnkown6: LongInt;
      pszProcessName: PWideChar;
      dwBasePriority: LongInt;
      dwProcessID: LongInt;
      dwParentProcessID: LongInt;
      dwHandleCount: LongInt;
      dwUnkown7: LongInt;
      dwUnkown8: LongInt;
      dwVirtualBytesPeak: LongInt;
      dwVirtualBytes: LongInt;
      dwPageFaults: LongInt;
      dwWorkingSetPeak: LongInt;
      dwWorkingSet: LongInt;
      dwUnkown9: LongInt;
      dwPagedPool: LongInt;
      dwUnkown10: LongInt;
      dwNonPagedPool: LongInt;
      dwPageFileBytesPeak: LongInt;
      dwPageFileBytes: LongInt;
      dwPrivateBytes: LongInt;
      dwUnkown11: LongInt;
      dwUnkown12: LongInt;
      dwUnkown13: LongInt;
      dwUnkown14: LongInt;
      ati: array[1..1] of TThreadInfo;
      end;
  var
    hNTDll: Handle;
    NtQuerySystemInformation: function (Nmb: Integer; Ptr: Pointer;
       Size1, Size2: Integer): LongInt stdcall;
    Buf: array[1..1024*512] of Byte;
    PInfo: PProcessInfo;
    P: PProcessItem;
    I: Byte;
  begin { FillWinNTProcessCollection }
  hNTDll := LoadLibrary('ntdll.dll');
  @NtQuerySystemInformation := GetProcAddress(hNTDll,
       'NtQuerySystemInformation');
  if @NtQuerySystemInformation <> nil then
    begin
    NtQuerySystemInformation(5, @Buf, SizeOf(Buf), 0);
    PInfo := @Buf;
    repeat
      P := New(PProcessItem, Init);
      WideCharToMultiByte(CP_OEMCP, 0, Pointer(PInfo^.pszProcessName),
         -1, @P^.Name[1], 255, nil, nil);
      for I := 1 to 255 do
        if P^.Name[I] = #0 then
          Break;
      P^.Name[0] := Char(I);
      P^.Pid := PInfo^.dwProcessID;
      if not (FilteredList and (Pos('#'+UpStrg(GetName(P^.Name))+'#',
              '#'+UpStrg(UserTaskFilter)+'#') > 0))
      then
        Collection^.Insert(P)
      else
        Dispose(P, Done);
      if PInfo^.dwOffset = 0 then
        Break;
      PInfo := PProcessInfo(PChar(PInfo)+PInfo^.dwOffset);
    until False;
    end;
  FreeLibrary(hNTDll);
  end { FillWinNTProcessCollection };

function GetProcessList: PProcessCollection;
  begin
  Result := New(PProcessCollection, Init(32, 16));
  if Result <> nil then
    if SysPlatformId = VER_PLATFORM_WIN32_NT then
      FillWinNTProcessCollection(Result)
    else
      FillWin95ProcessCollection(Result);
  end;

function ProcessKill(Pid: LongInt): LongInt;
  begin
  if TerminateProcess(OpenProcess(1, True, Pid), 1) then
    Result := 0
  else
    Result := 1;
  end;

function ProcessSwitch(Pid: LongInt): LongInt;
  begin
  Result := 0;
  end;

end.
