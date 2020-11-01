unit Plugin;
(******

Interface with plugins
Written by Cat 2:5030/1326.13
(for use in DN/2 Plugin Version)

******)

{$I STDEFINE.INC}
{&Delphi-}
{&Use32+}

{Cat
   05-10-2001 - начато добавление плагинов к ДН-у
   19-10-2001 - плагины типа EventCatcher
   21-10-2001 - плагины используют индексы в файлах ресурсов (Lng и Dlg)
   27-10-2001 - плагины используют индекс в файле помощи (Hlp)
   17-01-2002 - решил, что не следует изменять языковой файл и файл
                ресурсов (за исключением добавления/удаления строк меню),
                поэтому индексы в Lng, Dlg, Hlp файлах более не используются,
                а ресурсы плагинов теперь будут содержаться в файлах *.REZ,
                к которым Plugin Manager не будет иметь никакого отношения
                Версия плагинов EventCatcher - 2.0
   23-01-2002 - плагины типа ArchiveViewer
   28-01-2002 - плагины могут регистрировать объекты для операций с потоками
   10-05-2002 - вся структура плагинов переделана заново, без использования
                DN2CAT.DLL
   10-09-2002 - плагины типа RuntimePatch
   03-11-2002 - плагины типа DrivePanel
}

{Cat:warn при выходе из программы хорошо было бы освобождать библиотеки }
{ модулей RuntimePatch, а пока надеемся, что система всё сделает за нас }

{$IFNDEF PLUGIN}
{$ERROR This unit is for use only in plugin version of DN/2!}
{$ENDIF}

interface

uses
  Defines, Modules, Objects2, Drivers, Menus, Archiver
  ;

(*** RUNTIME PATCH ***)

type
  {&Cdecl+}
  TRuntimePatch = function (const PluginName: ShortString;
     DNFuncs, DNMethods: Pointer; var finalization: Pointer): Boolean;
  {&Cdecl-}

  (*** EVENT CATCHER ***)

type
  {&Cdecl+}
  THandleCommandProc = procedure (Command, ObjType: SmallWord;
     const PluginName: ShortString; DNFuncs, DNMethods: Pointer;
     var finalization: Pointer);
  {&Cdecl-}
  PEventCatcherInfo = ^TEventCatcherInfo;
  TEventCatcherInfo = packed record
    FirstCatchedCommand: Word;
    LastCatchedCommand: Word;
    {FirstLngIndex: Word;}
    {LastLngIndex: Word;}
    {FirstDlgIndex: Word;}
    {LastDlgIndex: Word;}
    {FirstHlpIndex: Word;}
    {LastHlpIndex: Word;}
    FirstObjType: Word;
    LastObjType: Word;
    PluginPath: String[8];
    Reserved: packed array[0..2] of Byte;
    LibHandle: Integer;
    Entry: THandleCommandProc;
    end;
  PEventCatcherArray = ^TEventCatcherArray;
  TEventCatcherArray = packed array[1..1] of TEventCatcherInfo;

procedure CatchersHandleCommand(Command: Word);

(*** DRIVE PANELS ***)

const
  MaxDrivePanelsInfo = 32;

type
  PIntegerArray = ^TIntegerArray;
  TIntegerArray = packed array[0..0] of Integer;

  PPCharArray = ^TPCharArray;
  TPCharArray = packed array[0..0] of PChar;

  PMenuStringsRet = ^TMenuStringsRet;
  TMenuStringsRet = packed record
    Reserved0: Integer;
    Count: Byte;
    Cacheable: Boolean;
    Reserved1: SmallWord;
    Strings1: PPCharArray;
    Strings2: PPCharArray;
    Keys: PIntegerArray;
    Reserved2: Integer;
    Reserved3: Integer;
    Reserved4: Integer;
    end;

  {&Cdecl+}
  TGetMenuStringsProc = function (Command, ObjType: SmallWord;
     const PluginName: ShortString; DNFuncs, DNMethods: Pointer)
  : PMenuStringsRet;
  TCreateDriveObjectProc = function (Command, ObjType: SmallWord;
     const PluginName: ShortString; DNFuncs, DNMethods: Pointer;
     AOwner: Pointer): Pointer;
  TRegisterDriveObjectProc = function (Command, ObjType: SmallWord;
     const PluginName: ShortString; DNFuncs, DNMethods: Pointer): Integer;
  {&Cdecl-}

  PDrivePanelsInfo = ^TDrivePanelsInfo;
  TDrivePanelsInfo = packed record
    PluginPath: PString;
    LibHandle: Integer;
    CreateDriveObject: TCreateDriveObjectProc;
    RegisterDriveObject: TRegisterDriveObjectProc;
    ObjType: Word;
    MenuString1: PString;
    MenuString2: PString;
    MenuKey: Integer;
    end;

  PDrivePanelsInfoArray = ^TDrivePanelsInfoArray;
  TDrivePanelsInfoArray = packed array[1..MaxDrivePanelsInfo] of
   PDrivePanelsInfo;

  PDrivePanelsInfo2 = ^TDrivePanelsInfo2;
  TDrivePanelsInfo2 = packed record
    PluginPath: PString;
    LibHandle: Integer;
    CreateDriveObject: TCreateDriveObjectProc;
    RegisterDriveObject: TRegisterDriveObjectProc;
    ObjType: Word;
    MenuStrings: PMenuStringsRet;
    end;

  PDrivePanelsInfoArray2 = ^TDrivePanelsInfoArray2;
  TDrivePanelsInfoArray2 = packed array[1..MaxDrivePanelsInfo] of
   PDrivePanelsInfo2;

function CreateDriveMenus(var Items: PMenuItem; var MaxL: Integer)
  : Integer;
function CreateDriveObject(I: Integer; AOwner: Pointer)
  : Pointer;

(*** EDITOR EVENT HOOKS ***)

{ помимо обычных отлавливаются следующие специальные события: }
{   65001 - начало перерисовки                                }
{   65002 - хотим получить данные подсветки                   }
{   65003 - после загрузки файла                              }
{   65004 - перед сохранением файла                           }
{   65005 - перед "сохранением как"                           }
{ в случае 65002 поле InfoPtr содержит указатель на структуру }
{ TFillColorsData                                             }

type
  PFillColorsData = ^TFillColorsData;
  TFillColorsData = record
    DrawBuffer: Pointer;
    StrNum, StartPos, EndPos: LongInt;
    end;

type
  TEditorEventHook = function (var Event: TEvent; Editor: Pointer)
  : Boolean;

function SetEditorEventHook(EditorEventHook: TEditorEventHook): Boolean;
procedure RemoveEditorEventHook(EditorEventHook: TEditorEventHook);
function ProcessEditorEventHook(var Event: TEvent; Editor: Pointer)
  : Boolean;

(*** ARCHIVE VIEWER ***)

const
  ArcFirst = 100;
  arcLast = 249;

type
  {&Cdecl+}
  TFormatsCountProc = function : Word;
  TArchiveSignProc = function (Id: Word): TStr4;
  TCreateArchiveObjectProc = function (Id: Word): PARJArchive;
  TDetectCreateArchiveObjectProc = function : PARJArchive;
  {&Cdecl-}
  PArchiveViewerInfo = ^TArchiveViewerInfo;
  TArchiveViewerInfo = packed record
    FirstTag: Byte;
    PluginPath: String[8];
    Reserved: SmallWord;
    LibHandle: Integer;
    FormatsCount: TFormatsCountProc;
    ArchiveSign: TArchiveSignProc;
    CreateArchiveObject: TCreateArchiveObjectProc;
    DetectCreateArchiveObject: TDetectCreateArchiveObjectProc;
    end;
  PArchiveViewerArray = ^TArchiveViewerArray;
  TArchiveViewerArray = packed array[ArcFirst-1..arcLast+1] of
   PArchiveViewerInfo;

function DetectCreateArchiveObject: PARJArchive;
function GetArchiveTagBySign(Sign: TStr4): Byte;
function GetArchiveByTag(ID: Byte): PARJArchive;

{Cat: порядок переменных не менять, иначе будут проблемы с плагинами}

var
  EventCatchers: PEventCatcherArray;
  EventCatchersCount: Integer;
  ArchiveViewers: TArchiveViewerArray;
  DrivePanelsInfo: TDrivePanelsInfoArray;
  DrivePanelsInfoCount: Integer;
  DrivePanelsInfo2: TDrivePanelsInfoArray2;
  DrivePanelsInfo2Count: Integer;

const
  ProcNamesArchiveViewer: array[0..3] of PChar =
    ('FormatsCount',
    'ArchiveSign',
    'CreateArchiveObject',
    'DetectCreateArchiveObject');
  ProcNamesEventCatcher: array[0..0] of PChar =
    ('CatchCommand');
  ProcNamesDrivePanels: array[0..2] of PChar =
    ('GetMenuStrings',
    'CreateDriveObject',
    'RegisterDriveObject');

procedure PluginRegisterObject(ObjType: Word);

implementation

uses
  Dos, Strings, Commands,
  ObjType, Messages, DNApp, Advance, Advance1, Advance2, Lfnvp, DNFuncs
  ;

const
  s_Cannot_load_module = 'Cannot load module ';
  s_Error_reading_file_PLUGINS_CFG = 'Error reading file PLUGINS.CFG';
  s_Error_reading_file_PLUGINS2_CFG = 'Error reading file PLUGINS2.CFG';
  s_Error_writing_file_PLUGINS2_CFG = 'Error writing file PLUGINS2.CFG';

  {$IFNDEF DNPRG}
  {$I Version.Inc}
  {$ENDIF}

function StrPas(P: PChar): String;
  begin
  if P = nil then
    StrPas := ''
  else
    StrPas := Strings.StrPas(P);
  end;

(*** RUNTIME PATCH ***)

procedure ApllyRuntimePatches;
  var
    PlugDir: String;
    SR: lSearchRec;
    LibHandle: Integer;
    RuntimePatch: TRuntimePatch;
    finalization: Pointer;

  procedure ApplyRuntimePatch(const FullPath: String);
    begin
    if LoadModule(@FullPath[1], LibHandle)
      and GetProcAddress(LibHandle, 'RuntimePatch', @RuntimePatch)
    then
      begin
      if RuntimePatch(FullPath, @DNFunctions, @DNMethods, finalization)
      then
        FreeModule(LibHandle)
      else if finalization <> nil then
        AddExitProc(finalization);
      end
    else
      begin
      Writeln(s_Cannot_load_module, FullPath);
      FreeModule(LibHandle);
      end;
    end;

  begin { ApllyRuntimePatches }
  PlugDir := SourceDir+'PLUG_X\';

  { сначала запускаем все плагины из подкаталога PLUG_X }
  lFindFirst(PlugDir+'*.DLL', AnyFileDir-Directory, SR);
  while DosError = 0 do
    begin
    if  (SR.SR.Attr and (Directory or Hidden)) = 0 then
      ApplyRuntimePatch(PlugDir+SR.FullName+#0);
    lFindNext(SR);
    end;
  lFindClose(SR);

  { теперь попробуем запустить X.DLL из каталога DN-а }
  if ExistFile(SourceDir+'X.DLL') then
    ApplyRuntimePatch(SourceDir+'X.DLL'#0);
  end { ApllyRuntimePatches };

(*** EVENT CATCHER ***)

procedure DoneDrivePanels(NeedWriteConfig: Boolean);forward;

procedure DonePlugins;
  var
    I: Integer;
  begin
  for I := 1 to EventCatchersCount do
    with EventCatchers^[I] do
      FreeModule(LibHandle);
  FreeMem(EventCatchers, EventCatchersCount*SizeOf(TEventCatcherInfo));

  for I := arcLast downto ArcFirst do
    if ArchiveViewers[I] <> nil then
      if I = ArchiveViewers[I]^.FirstTag then
        Dispose(ArchiveViewers[I]);

  DoneDrivePanels(True);
  end;

procedure InitPlugins;
  label
    Plugins2;
  var
    F: file;
    I, J, K: Integer;
    ArchiveViewersCount: Integer;
    FullPath: String;

    {&Delphi+}
  function ReadStr: String;
    var
      Len: Byte;
    begin
    BlockRead(F, Len, SizeOf(Len));
    SetLength(Result, Len);
    BlockRead(F, Result[1], Len);
    end;
  {&Delphi-}

  const
    Initialized: Boolean = False;
    PLUGINS_CFG: array[0..31] of Char =
    #$01#$00#$00#$00#$00#$7D#$00#$00#$00#$7D#$00#$00#$FF#$FF#$00#$00#$FF#$FF#$00#$00#$07#$50#$4C#$55#$47#$4D#$41#$4E#$00#$00#$00#$00
    ;
  begin { InitPlugins }
  if Initialized then
    Exit;
  Initialized := True;

  DNFunctions.DN2Version := VersionWord;

  ApllyRuntimePatches;

  FullPath := SourceDir+'PLUGINS.CFG';

  if not ExistFile(FullPath) then
    begin
    Assign(F, FullPath);
    Rewrite(F, 1);
    BlockWrite(F, PLUGINS_CFG, SizeOf(PLUGINS_CFG));
    Close(F);
    if IOResult = 0 then
      ;
    end;

  FileMode := $40;

  Assign(F, FullPath);
  Reset(F, 1);
  BlockRead(F, EventCatchersCount, SizeOf(EventCatchersCount));
  if  (IOResult <> 0) or (EventCatchersCount < 0)
       or (EventCatchersCount > 60000)
  then
    begin
    Close(F);
    if IOResult = 0 then
      ;
    EventCatchers := nil;
    Writeln(s_Error_reading_file_PLUGINS_CFG);
    goto Plugins2;
    end;

  GetMem(EventCatchers, EventCatchersCount*SizeOf(TEventCatcherInfo));

  for I := 1 to EventCatchersCount do
    with EventCatchers^[I] do
      begin
      BlockRead(F, FirstCatchedCommand, SizeOf(FirstCatchedCommand));
      BlockRead(F, LastCatchedCommand, SizeOf(LastCatchedCommand));
      {BlockRead(F, FirstLngIndex, SizeOf(FirstLngIndex));}
      {BlockRead(F, LastLngIndex, SizeOf(LastLngIndex));}
      {BlockRead(F, FirstDlgIndex, SizeOf(FirstDlgIndex));}
      {BlockRead(F, LastDlgIndex, SizeOf(LastDlgIndex));}
      {BlockRead(F, FirstHlpIndex, SizeOf(FirstHlpIndex));}
      {BlockRead(F, LastHlpIndex, SizeOf(LastHlpIndex));}
      BlockRead(F, FirstObjType, SizeOf(FirstObjType));
      BlockRead(F, LastObjType, SizeOf(LastObjType));
      PluginPath := Copy(ReadStr, 1, 8);
      LibHandle := 0;
      @Entry := nil;
      end;

  BlockRead(F, ArchiveViewersCount, SizeOf(ArchiveViewersCount));
  FillChar(ArchiveViewers, SizeOf(ArchiveViewers), #0);
  if IOResult <> 0 then
    begin
    Close(F);
    if IOResult = 0 then
      ;
    FreeMem(EventCatchers, EventCatchersCount*SizeOf(TEventCatcherInfo));
    EventCatchers := nil;
    Writeln(s_Error_reading_file_PLUGINS_CFG);
    goto Plugins2;
    end;

  J := ArcFirst;
  for I := 1 to ArchiveViewersCount do
    begin
    New(ArchiveViewers[J]);
    with ArchiveViewers[J] do
      begin
      FirstTag := J;
      PluginPath := Copy(ReadStr, 1, 8);
      if IOResult <> 0 then
        begin
        Close(F);
        if IOResult = 0 then
          ;
        FreeMem(EventCatchers,
             EventCatchersCount*SizeOf(TEventCatcherInfo));
        EventCatchers := nil;
        Dispose(ArchiveViewers[J]);
        ArchiveViewers[J] := nil;
        Writeln(s_Error_reading_file_PLUGINS_CFG);
        goto Plugins2;
        end;
      if not LoadPluginModuleAndGetProcAddress(PluginPath, LibHandle,
           ProcNamesArchiveViewer,
          [@@FormatsCount, @@ArchiveSign, @@CreateArchiveObject,
           @@DetectCreateArchiveObject])
      then
        begin
        Writeln(s_Cannot_load_module, PluginPath);
        Dispose(ArchiveViewers[J]);
        ArchiveViewers[J] := nil;
        Continue;
        end;
      end;
    for K := 1 to ArchiveViewers[J]^.FormatsCount do
      begin
      Inc(J);
      if J > arcLast then
        Break;
      ArchiveViewers[J] := ArchiveViewers[J-1];
      end;
    end;

  Close(F);
  if IOResult = 0 then
    AddExitProc(DonePlugins)
  else
    begin
    FreeMem(EventCatchers, EventCatchersCount*SizeOf(TEventCatcherInfo));
    EventCatchers := nil;
    Writeln(s_Error_reading_file_PLUGINS_CFG);
    end;

Plugins2:

  FullPath := SourceDir+'PLUGINS2.CFG';

  DrivePanelsInfoCount := 0;
  if ExistFile(FullPath) then
    begin
    Assign(F, FullPath);
    Reset(F, 1);
    while not Eof(F) do
      begin
      Inc(DrivePanelsInfoCount);
      if DrivePanelsInfoCount > MaxDrivePanelsInfo then
        Break;
      New(DrivePanelsInfo[DrivePanelsInfoCount]);
      with DrivePanelsInfo[DrivePanelsInfoCount]^ do
        begin
        PluginPath := NewStr(ReadStr);
        LibHandle := 0;
        MenuString1 := NewStr(ReadStr);
        MenuString2 := NewStr(ReadStr);
        BlockRead(F, MenuKey, SizeOf(MenuKey));
        BlockRead(F, ObjType, SizeOf(ObjType));

        if IOResult <> 0 then
          begin
          DoneDrivePanels(False);
          Writeln(s_Error_reading_file_PLUGINS2_CFG);
          Break;
          end;
        end;
      end;
    Close(F);
    end;
  end { InitPlugins };

procedure CatchersHandleCommand(Command: Word);
  var
    I: Integer;
    finalization: Pointer;
  begin
  if EventCatchers <> nil then
    for I := 1 to EventCatchersCount do
      with EventCatchers^[I] do
        if  (Command >= FirstCatchedCommand)
             and (Command <= LastCatchedCommand)
        then
          begin
          if Assigned(Entry) then
            begin
            Entry(Command-FirstCatchedCommand, FirstObjType, PluginPath,
               @DNFunctions, @DNMethods, finalization);
            if finalization <> nil then
              AddExitProc(finalization);
            end
          else
            begin
            if  (LibHandle <> 0)
              or not LoadPluginModuleAndGetProcAddress(PluginPath,
                 LibHandle, ProcNamesEventCatcher,
                [@@Entry])
            then
              begin
              MessageBox(GetString(dlCantLoad)+PluginPath, nil,
                 mfError+mfOKButton);
              Exit;
              end;
            Entry(Command-FirstCatchedCommand, FirstObjType, PluginPath,
               @DNFunctions, @DNMethods, finalization);
            if finalization <> nil then
              AddExitProc(finalization);
            end;
          Exit;
          end;
  end { CatchersHandleCommand };

procedure CatchersRegisterObject(ObjType: Word);
  var
    I: Integer;
    finalization: Pointer;
  begin
  if  (ObjType >= otPlugins) and (ObjType <= otPluginsEnd) then
    if EventCatchers <> nil then
      for I := 1 to EventCatchersCount do
        with EventCatchers^[I] do
          if  (ObjType >= FirstObjType) and (ObjType <= LastObjType)
          then
            begin
            if Assigned(Entry) then
              begin
              Entry($FFFF, FirstObjType, PluginPath, @DNFunctions,
                 @DNMethods, finalization);
              if finalization <> nil then
                AddExitProc(finalization);
              end
            else
              begin
              if  (LibHandle <> 0)
                or not LoadPluginModuleAndGetProcAddress(PluginPath,
                   LibHandle, ProcNamesEventCatcher,
                  [@@Entry])
              then
                begin
                MessageBox(GetString(dlCantLoad)+PluginPath, nil,
                   mfError+mfOKButton);
                Exit;
                end;
              Entry($FFFF, FirstObjType, PluginPath, @DNFunctions,
                 @DNMethods, finalization);
              if finalization <> nil then
                AddExitProc(finalization);
              end;
            Exit;
            end;
  end { CatchersRegisterObject };

(*** DRIVE PANELS ***)

procedure DoneDrivePanels(NeedWriteConfig: Boolean);
  var
    I: Integer;

  procedure WriteConfig;
    var
      I, J, K: Integer;
      F: file;

    procedure WriteStr(S: String);
      begin
      BlockWrite(F, S, 1+Length(S));
      end;

    begin
    //  Writeln('Drive Panel Plugins: ',DrivePanelsInfoCount,' old, ',DrivePanelsInfo2Count,' new');
    Assign(F, SourceDir+'PLUGINS2.CFG');
    Rewrite(F, 1);
    for I := 1 to DrivePanelsInfoCount do
      with DrivePanelsInfo[I]^ do
        begin
        WriteStr(PluginPath^);
        WriteStr(CnvString(MenuString1));
        WriteStr(CnvString(MenuString2));
        BlockWrite(F, MenuKey, SizeOf(MenuKey));
        BlockWrite(F, ObjType, SizeOf(ObjType));
        end;
    for I := 1 to DrivePanelsInfo2Count do
      with DrivePanelsInfo2[I]^, MenuStrings^ do
        for J := 0 to Count-1 do
          begin
          WriteStr(PluginPath^);
          WriteStr(StrPas(Strings1^[J]));
          WriteStr(StrPas(Strings2^[J]));
          BlockWrite(F, Keys^[J], SizeOf(Integer));
          K := ObjType+J;
          BlockWrite(F, K, SizeOf(Integer));
          end;
    Close(F);
    if IOResult <> 0 then
      Writeln(s_Error_writing_file_PLUGINS2_CFG);
    end { WriteConfig };

  begin { DoneDrivePanels }
  if NeedWriteConfig and (DrivePanelsInfo2Count > 0) then
    WriteConfig;

  for I := 1 to DrivePanelsInfoCount do
    with DrivePanelsInfo[I]^ do
      begin
      DisposeStr(PluginPath);
      DisposeStr(MenuString1);
      DisposeStr(MenuString2);

      FreeModule(LibHandle);

      Dispose(DrivePanelsInfo[I]);
      DrivePanelsInfo[I] := nil;
      end;

  for I := 1 to DrivePanelsInfo2Count do
    with DrivePanelsInfo2[I]^ do
      begin
      FreeModule(LibHandle);

      Dispose(DrivePanelsInfo2[I]);
      DrivePanelsInfo2[I] := nil;
      end;

  DrivePanelsInfoCount := 0;
  DrivePanelsInfo2Count := 0;
  end { DoneDrivePanels };

{&Delphi+}
function CreateDriveMenus(var Items: PMenuItem; var MaxL: Integer)
  : Integer;
  var
    I, J: Integer;
    S1, S2: String;

  procedure LoadLibs;
    var
      SR: lSearchRec;
      S: String;

    procedure LoadLib(const APluginPath: String);
      var
        GetMenuStrings: TGetMenuStringsProc;
      begin
      Inc(DrivePanelsInfo2Count);
      if DrivePanelsInfo2Count <= MaxDrivePanelsInfo then
        begin
        New(DrivePanelsInfo2[DrivePanelsInfo2Count]);
        with DrivePanelsInfo2[DrivePanelsInfo2Count]^ do
          begin
          PluginPath := NewStr(APluginPath);
          if LoadPluginModuleAndGetProcAddress(APluginPath, LibHandle,
               ProcNamesDrivePanels,
              [@@GetMenuStrings, @@CreateDriveObject,
               @@RegisterDriveObject])
          then
            MenuStrings := GetMenuStrings(0, 0, APluginPath,
                 @DNFunctions, @DNMethods)
          else
            begin
            FreeModule(LibHandle);
            MenuStrings := nil;
            end;
          end;
        end;
      end { LoadLib };

    function Ok: Boolean;
      var
        I: Integer;
      begin
      if  (SR.SR.Attr and (Directory or Hidden)) = 0 then
        begin
        Result := True;
        S := SR.FullName;
        Dec(S[0], 4);
        for I := 1 to DrivePanelsInfoCount do
          if DrivePanelsInfo[I]^.PluginPath^ = S then
            begin
            Result := False;
            Exit;
            end;
        for I := 1 to DrivePanelsInfo2Count do
          if DrivePanelsInfo2[I]^.PluginPath^ = S then
            begin
            Result := False;
            Exit;
            end;
        end
      else
        Result := False;
      end { Ok: };

    begin { LoadLibs }
    lFindFirst(SourceDir+'P_*.DLL', AnyFileDir-Directory, SR);
    while DosError = 0 do
      begin
      if Ok then
        LoadLib(S);
      lFindNext(SR);
      end;
    lFindClose(SR);
    end { LoadLibs };

  begin { CreateDriveMenus }
  Result := 0;
  LoadLibs;

  for I := DrivePanelsInfoCount downto 1 do
    with DrivePanelsInfo[I]^ do
      begin
      Items := NewItem(CnvString(MenuString1), CnvString(MenuString2),
           MenuKey, 65000+I, 0, Items);
      MaxL := Max(CStrLen(MenuString1^), MaxL);
      Inc(Result);
      end;

  for I := DrivePanelsInfo2Count downto 1 do
    with DrivePanelsInfo2[I]^, MenuStrings^ do
      if MenuStrings <> nil then
        begin
        for J := Count-1 downto 0 do
          begin
          S1 := StrPas(Strings1^[J]);
          S2 := StrPas(Strings2^[J]);
          Items := NewItem(S1, S2, Keys^[J], 65000+MaxDrivePanelsInfo+I,
               0, Items);
          MaxL := Max(CStrLen(S1), MaxL);
          Inc(Result);
          end;
        end;
  end { CreateDriveMenus };

function CreateDriveObject(I: Integer; AOwner: Pointer)
  : Pointer;
  type
    PDrivePanelsInfoShort = ^TDrivePanelsInfoShort;
    TDrivePanelsInfoShort = record
      PluginPath: PString;
      LibHandle: Integer;
      CreateDriveObject: TCreateDriveObjectProc;
      RegisterDriveObject: TRegisterDriveObjectProc;
      ObjType: Word;
      end;
  var
    P: PDrivePanelsInfoShort;
    GetMenuStrings: TGetMenuStringsProc;
  begin
  Result := nil;

  if I <= MaxDrivePanelsInfo then
    P := PDrivePanelsInfoShort(DrivePanelsInfo[I])
  else
    P := PDrivePanelsInfoShort(DrivePanelsInfo2[I-MaxDrivePanelsInfo]);

  if P <> nil then
    with P^ do
      begin
      if  (LibHandle = 0)
        and not LoadPluginModuleAndGetProcAddress(PluginPath^,
           LibHandle, ProcNamesDrivePanels,
          [@@GetMenuStrings, @@CreateDriveObject,
           @@RegisterDriveObject])
      then
        begin
        MessageBox(GetString(dlCantLoad)+PluginPath^, nil,
           mfError+mfOKButton);
        FreeModule(LibHandle);
        end
      else
        begin
        ObjType := RegisterDriveObject($FFFF, 0, PluginPath^,
             @DNFunctions, @DNMethods);
        Result := CreateDriveObject(0, 0, PluginPath^, @DNFunctions,
             @DNMethods, AOwner);
        end;
      end;
  end { CreateDriveObject };

procedure DrivePanelsRegisterObject(AObjType: Word);
  var
    I: Integer;
    GetMenuStrings: TGetMenuStringsProc;
  begin
  for I := 1 to DrivePanelsInfoCount do
    with DrivePanelsInfo[I]^ do
      if ObjType = AObjType then
        if  (LibHandle = 0)
          and not LoadPluginModuleAndGetProcAddress(PluginPath^,
             LibHandle, ProcNamesDrivePanels,
            [@@GetMenuStrings, @@CreateDriveObject,
             @@RegisterDriveObject])
        then
          begin
          MessageBox(GetString(dlCantLoad)+PluginPath^, nil,
             mfError+mfOKButton);
          FreeModule(LibHandle);
          end
        else
          RegisterDriveObject($FFFF, 0, PluginPath^, @DNFunctions,
             @DNMethods);
  end { DrivePanelsRegisterObject };
{&Delphi-}

(*** EDITOR EVENT HOOKS ***)

{&Delphi+}
const
  MaxEditorEventHookCount = 32;

var
  EditorEventHookArray: array[1..MaxEditorEventHookCount] of Pointer
  {TEditorEventHook};
  EditorEventHookCount: Integer;

function SetEditorEventHook(EditorEventHook: TEditorEventHook): Boolean;
  begin
  if EditorEventHookCount < MaxEditorEventHookCount then
    begin
    Result := True;
    Inc(EditorEventHookCount);
    EditorEventHookArray[EditorEventHookCount] := @EditorEventHook;
    end
  else
    Result := False;
  end;

procedure RemoveEditorEventHook(EditorEventHook: TEditorEventHook);
  var
    I: Integer;
  begin
  for I := 1 to EditorEventHookCount do
    if EditorEventHookArray[I] = @EditorEventHook then
      begin
      for I := I+1 to EditorEventHookCount do
        EditorEventHookArray[I-1] := EditorEventHookArray[I];
      Dec(EditorEventHookCount);
      Exit;
      end;
  end;

function ProcessEditorEventHook(var Event: TEvent; Editor: Pointer)
  : Boolean;
  var
    I: Integer;
  begin
  for I := 1 to EditorEventHookCount do
    if TEditorEventHook(EditorEventHookArray[I])(Event, Editor) then
      begin
      Result := True;
      Exit;
      end;
  Result := False;
  end;
{&Delphi-}

(*** ARCHIVE VIEWER ***)

{&Delphi+}
function DetectCreateArchiveObject: PARJArchive;
  var
    J: Integer;
  begin
  for J := ArcFirst to arcLast do
    if ArchiveViewers[J] <> nil then
      begin
      Result := ArchiveViewers[J]^.DetectCreateArchiveObject;
      if Result <> nil then
        Exit;
      end;
  Result := nil;
  end;

function GetArchiveTagBySign(Sign: TStr4): Byte;
  var
    J: Integer;
  begin
  for J := ArcFirst to arcLast do
    if ArchiveViewers[J] <> nil then
      with ArchiveViewers[J] do
        if ArchiveSign(J-FirstTag) = Sign then
          begin
          Result := J;
          Exit;
          end;
  Result := arcUNK;
  end;

function GetArchiveByTag(ID: Byte): PARJArchive;
  begin
  if ArchiveViewers[ID] = nil then
    Result := nil
  else
    with ArchiveViewers[ID] do
      Result := CreateArchiveObject(ID-FirstTag);
  end;
{&Delphi-}

procedure PluginRegisterObject(ObjType: Word);
  begin
  CatchersRegisterObject(ObjType);
  DrivePanelsRegisterObject(ObjType);
  end;

begin
InitPlugins
end.
