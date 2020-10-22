unit Proc_Os2;
(******

Task List plugin
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{JO: 20-10-2002 добавлено определение типа сессии, показ заголовка окна    }
{    (требует PM) и возможность переключения на окно процесса (требует PM) }

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
    SessID: LongInt;
    PType: LongInt; {JO}
    PTitle: String; {JO}
    function GetString: String;
    end;

  PProcessCollection = ^TProcessCollection;
  TProcessCollection = object(TSortedCollection)
    function Compare(P1, P2: Pointer): Integer; virtual;
    end;

function GetProcessList: PProcessCollection;
function ProcessKill(Pid: LongInt): LongInt;
function ProcessSwitch(Pid: LongInt): LongInt;
function GetCurPid: LongInt;

implementation

uses
  Os2Def, Os2Base,
  Dn2PmApi, DnIni, Advance1, Advance2
  ;

function GetCurPid: LongInt;
  var
    TB: PTib;
    PB: PPib;
  begin
  DosGetInfoBlocks(TB, PB);
  Result := PB.Pib_ulPid;
  end;

function TProcessItem.GetString: String;

  function Hex4(A: Word): Str4;
    const
      Hex: array[0..15] of Char =
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B',
         'C', 'D', 'E', 'F');
    begin
    Result := Hex[(A shr 12) and $F]+
      Hex[(A shr 08) and $F]+
      Hex[(A shr 04) and $F]+
      Hex[(A shr 00) and $F];
    if Result[1] = '0' then
      begin
      Result[1] := ' ';
      if Result[2] = '0' then
        begin
        Result[2] := ' ';
        if Result[3] = '0' then
          Result[3] := ' ';
        end;
      end;
    end;

  begin { TProcessItem.GetString: }
  case PType of
    0:
      Result := 'FS ';
    1:
      Result := 'VDM';
    2:
      Result := 'VIO';
    3:
      Result := 'PM ';
    4:
      Result := 'Det';
    else {case}
      Result := '  ?';
  end {case};

  Result := Hex4(Pid)+'  '+Result+'  ';

  if ShowExePaths then
    Result := Result+Name
  else
    Result := Result+GetName(Name);

  if PTitle <> '' then
    begin
    if ShowExePaths then
      Result := Result+'  '
    else
      while Length(Result) < 24 do
        Result := Result+' ';
    Result := Result+'  ('+PTitle+')';
    end;

  if Pid = GetCurPid then
    Result := '>'+Result
  else
    Result := ' '+Result;
  end { TProcessItem.GetString: };

function TProcessCollection.Compare(P1, P2: Pointer): Integer;
  begin
  Compare := PProcessItem(P1)^.Pid-PProcessItem(P2)^.Pid;
  end;

function GetProcessList: PProcessCollection;
  const
    ModuleInfoSize = 1024*1024;
  type
    PByteArray = ^TByteArray;
    TByteArray = array[0..1023] of Byte;
    PPointerArray = ^TPointerArray;
    TPointerArray = array[0..1023] of Pointer;
    PThreadInfo = ^TThreadInfo;
    TThreadInfo = record
      RecType: ULong;
      Tid: UShort;
      Slot: UShort;
      SleepId: ULong;
      Priority: ULong;
      SysTime: ULong;
      UserTime: ULong;
      State: Byte;
      Padding: array[1..3] of Byte;
      end;
    PProcessInfo = ^TProcessInfo;
    TProcessInfo = record
      RecType: ULong;
      Threads: PThreadInfo;
      Pid: UShort;
      PPid: UShort;
      ProcType: ULong;
      State: ULong;
      SessID: ULong;
      ModHnd: UShort;
      ThreadCnt: UShort;
      Sem32Cnt: ULong;
      Sem32s: Pointer;
      Sem16Cnt: UShort;
      DllCnt: UShort;
      ShrMemCnt: UShort;
      FdsCnt: UShort;
      Sem16s: Pointer;
      Dlls: Pointer;
      ShrMems: Pointer;
      Fds: Pointer;
      end;
    PModuleInfo = ^TModuleInfo;
    TModuleInfo = record
      Next: PModuleInfo;
      ModHnd: UShort;
      ModType: UShort;
      RefCnt: ULong;
      SegCnt: ULong;
      Reserved: Pointer;
      Name: PChar;
      ModRef: UShort;
      end;
  var
    LibHandle: hModule;
    Dos32QuerySysState: function (func, arg1, Pid, _res_: ULong;
       Buf: Pointer; BufSz: ULong): ApiRet cdecl;
    ModuleInfo: PChar;
    Process: PProcessInfo;
    P: PProcessItem;
    I, J: Word;
  begin { GetProcessList: }
  Result := New(PProcessCollection, Init(32, 16));
  if Result <> nil then
    with Result^ do
      if DosLoadModule(nil, 0, 'DOSCALLS', LibHandle) = 0 then
        if DosQueryProcAddr(LibHandle, 368, nil, @Dos32QuerySysState)
           = 0
        then
          begin
          GetMem(ModuleInfo, ModuleInfoSize);
          {FillChar(ModuleInfo^,ModuleInfoSize, 0);}
          if Dos32QuerySysState(
              $00000001, // process data
              0, // reserved
              0, // all processes
              0, // reserved
              ModuleInfo,
              ModuleInfoSize) = 0
          then

            begin
            Process := PPointerArray(ModuleInfo)^[1];
            while Process^.RecType = 1 do
              begin
              P := New(PProcessItem, Init);
              if DosQueryModuleName(Process^.ModHnd, 255, @P^.Name[1])
                 <> 0
              then
                P^.Name := '?'#0;
              P^.Name[255] := #0;
              for I := 1 to 255 do
                if P^.Name[I] = #0 then
                  begin
                  SetLength(P^.Name, I-1);
                  Break;
                  end;
              P^.Pid := Process^.Pid;
              P^.SessID := Process^.SessID;
              P^.PType := Process^.ProcType;
              P^.PTitle := '';
              if Process^.SessID <> 0 then
                DN_WinQueryTaskTitle(Process^.SessID, P^.PTitle);
              if not (FilteredList and ((Process^.SessID in [0, 1])
                    or (Process^.ProcType = 4)
                    or (Pos('#'+UpStrg(GetName(P^.Name))+'#',
                        '#'+UpStrg(UserTaskFilter)+'#') > 0)))
              then
                Insert(P)
              else
                Dispose(P, Done);
              Process := PProcessInfo
                  (@PByteArray(Process^.Threads)^[SizeOf(TThreadInfo)*
                  Process^.ThreadCnt]);
              end;
            end;

          FreeMem(ModuleInfo, ModuleInfoSize);
          DosFreeModule(LibHandle);
          end;
  I := 1;
  while FilteredList and (I < Result^.Count) do
    begin
    for J := I-1 downto 0 do
      if PProcessItem(Result^.At(J))^.SessID =
        PProcessItem(Result^.At(I))^.SessID
      then
        begin
        Result^.AtFree(J);
        Dec(I);
        end;
    Inc(I);
    end;
  end { GetProcessList: };

function ProcessKill(Pid: LongInt): LongInt;
  begin
  Result := DosKillProcess(dkp_Process, Pid);
  end;

function ProcessSwitch(Pid: LongInt): LongInt;
  begin
  Result := DN_WinSwitchToProgram(Pid);
  end;

end.
