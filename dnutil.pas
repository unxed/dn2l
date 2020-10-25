{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08
//  Based on Dos Navigator (C) 1991-99 RIT Research Labs
//
//  This programs is free for commercial and non-commercial use as long as
//  the following conditions are aheared to.
//
//  Copyright remains RIT Research Labs, and as such any Copyright notices
//  in the code are not to be removed. If this package is used in a
//  product, RIT Research Labs should be given attribution as the RIT Research
//  Labs of the parts of the library used. This can be in the form of a textual
//  message at program startup or in documentation (online or textual)
//  provided with the package.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//  1. Redistributions of source code must retain the copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. All advertising materials mentioning features or use of this software
//     must display the following acknowledgement:
//     "Based on Dos Navigator by RIT Research Labs."
//
//  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS
//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
//  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  The licence and distribution terms for any publically available
//  version or derivative of this code cannot be changed. i.e. this code
//  cannot simply be copied and put under another distribution licence
//  (including the GNU Public Licence).
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{JO: этот дефайн позволяет при загрузке новой палитры принудительно сделать }
{    цвета диалога сообщений об ошибках стандартными                        }
{.$DEFINE ForceErrPal}

unit DNUtil;

interface

uses
  Defines, Objects2,
   {SBlocks,}Drivers, Streams,
  Views, Scroller, DNApp,
  Dialogs, Gauges,
  Commands, Tree,
  FilesCol, UserMenu,
  HelpFile, xTime
  ;

const
 {$IFNDEF DPMI32}
  RestartOnExit: Boolean = False; {Restart after exit}
 {$ENDIF}
  {$IFDEF TrashCan}TrashCan: PTrashCan = nil; {$ENDIF}
  HelpWnd: PHelpWindow = nil;
  HelpInUse: Boolean = False;
  RunMenu: Boolean = False;
    {`Признак автовыпадения меню ([X] Auto run User Menu)`}
  IdleCounter: Word = 0;
  StartTicks: Word = 0;
  NullStr: Byte = 0;

var
{$IFDEF DPMI32}
  LoaderSeg: SmallWord;
  CommandOfs: SmallWord;
{$ENDIF}
  RunFirst: Boolean;
const
  Virgin: Boolean = False; {JO}
  NoTempDir: Boolean = False; {JO}

type
  PDNApplication = ^TDNApplication;
  TDNApplication = object(TApplication)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    IdleClick: TEventTimer;
    IdleEvt: TEvent;
    TreeReader: PTreeReader;

    Pk1, Pk2, Pk3, Pk4: PView;

    constructor Init;
    destructor Done; virtual;
    procedure InitMenuBar; virtual;
    procedure InitCommandLine; virtual;
    procedure InitDesktop; virtual;
    procedure InitStatusLine; virtual;
    procedure ViewFile(AltExt, NoExtFile: Boolean; FileName: String);
    {AK155}
    procedure AddFormat;
    procedure EditFile(Intern: Boolean; FileName: String);
    {DataCompBoy}
    procedure OutOfMemory; virtual;
    procedure RetrieveDesktop(const FileName: String; LS: PStream;
         LoadColors: Boolean); {DataCompBoy}
    procedure SaveDesktop(const FileName: String); {DataCompBoy}
    procedure LoadDesktop(var S: TStream);
    procedure StoreDesktop(var S: TStream);
    procedure ChgColors;
    procedure EventError(var Event: TEvent); virtual;
    procedure HandleCommand(var Event: TEvent);
  private
    procedure ProcessTempFile(TFStr: String);
    end;

function CheckExit: Boolean;
function PresentFile(Name: String): PStream; {DataCompBoy}
procedure WriteConfig;
procedure ShowTimeInfo;
procedure LoadPalFromFile(const FN: String); {DataCompBoy}
{$IFDEF DPMI32}
procedure w95QuitInit; {Gimly}
function w95QuitCheck: Boolean; {Gimly}
procedure w95QuitCancel;
{$ENDIF}
procedure ClearSelection(AFP: Pointer {PFilePanelRoot};
    FC: Pointer {PFilesCollection - файлы к разотметке });

const
  w95locked: Boolean = False; {Gimly}

const
// Идентификаторы блоков dn.cfg, соответствующих структурам данных
// различных диалогов настройки. При изменении размера или структуры
// таких блоков желательно давать им новый идентификатор во избежание
// неправильной интерпретации старых версий dn.cfg. Новые идентификаторы
// во избежание путаницы следует помещать в конец данного списка,
// а неиспользуемые - оставлять закомментированными на старом месте
  cfgMouseData = 3;
  cfgInterfaceData = 4;
  cfgSaversData = 5;
  cfgSystemColors = 6;
//  cfgPanelDefaults = 7;
  cfgTetrisRec = 9;
//  cfgOldCustomMasks = 10; {DataCompBoy}
  cfgPrinterSetup = 11;
  cfgColumnDefaults = 12;
  {cfgSavers              = 13; Obsolete}
  cfgCountryInfo = 14;
  cfgConfirms = 15;
  cfgTermDefaults = 16;
//  cfgDirsToChange = 17;
  cfgFindDlgPos = 20;
  cfgCDParams = 21;
  cfgUUEData = 22;
  cfgVGAPalette = 23;
  cfgBlink = 24;
  cfgFFindOptions = 26;
//  cfgOldFMSetup = 27; {DataCompBoy}
  cfgDriveInfoData = 28;
//  cfgMakeListFile = 29;
  cfgOldEditorDefaults = 30; {DataCompBoy}
  cfgStartupData = 31;
  cfgSystemData = 32;
//  cfgINIdata = 33;
//  cfgIgnoreOldFiles = 34;
  cfgExtractOptions = 35;
  {New Record Versions}
  cfgOldSystemData = 36; {DataCompBoy}
  cfgNewSaversData = 37;
//  cfgColumnsDefaultsDisk = 38;
//  cfgColumnsDefaultsFind = 39;
//  cfgColumnsDefaultsTemp = 40;
//  cfgColumnsDefaultsArch = 41;
  {    cfgColumnsDefaultsArvd = 42; Do not use 42 - old version!!!}
//  cfgNewFMSetup = 43; {DataCompBoy}
//  cfgOldArcCustomMasks = 44; {DataCompBoy}
//  cfgColumnsDefaultsArvd = 45; {DataCompBoy}
//  cfgShowScrollBar = 47; {DataCompBoy}
  cfgNewStartupData = 48; {DataCompBoy}
  cfgDefaultArchiver = 49; {DataCompBoy}
  cfgDefaultArchiverMode = 50; {DataCompBoy}
  cfgOld2SystemData = 51; {DataCompBoy}
//  cfgOldCustomMasks2 = 52; {DataCompBoy}
//  cfgCustomMasks = 53; {DataCompBoy}
//  cfgCustomMasks2 = 54; {DataCompBoy}
//  cfgArcCustomMasks = 55; {DataCompBoy}
  cfgFMSetup = 56; {DataCompBoy}
  {                      = 57; !DO NOT USE!}
  cfgNewSystemData = 58; {DataCompBoy}
  cfgEditorDefaults = 59; {DataCompBoy}
//  cfgNewPanelDefaults = 60; {DataCompBoy}
  cfgSavers = 61; {DataCompBoy}
  cfgChangeCaseOptions = 62; {DataCompBoy}
  cfgINIcrc = 63; {DataCompBoy}
  cfgAppPalette = 64; {JO}
  cfgComareDirsOptions = 65; {JO}
  cfgPanSetupPreset = 66;
  cfgSortCurPanTypeOnly = 67; {JO}
  cfgFullMenuPanelSetup = 68; {JO}
  cfgCalcFormat = 69; {AK155}

  dlAbout = #13#3'DN/2 Open Source'+
  #13#3'Version %s, %s'+
  #13#3'http://www.dnosp.ru/'#13+
  #13#3'Based on: '+
  'Dos Navigator Open Source 1.51.08'#13+
  {$IFDEF LITE}+#3'Light version'#13+ {$ENDIF}
  +#3'http://www.dnosp.ru'+
  +#13+
  +#13#3'Thanks to fPrint (UK) Ltd for copy of VP'+
  +#13#3'http://www.vpascal.com'#13
  +#13#13
  {#3'Based on Dos Navigator' +
     +#13#3'Copyright (C) 1991-99 RIT Research Labs'#13#13};

type
  PDataSaver = ^TDataSaver;
  TDataSaver = object(TView)
    constructor Init;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    destructor Done; virtual;
    end;

const
  DataSaver: PDataSaver = nil;

procedure ExecDNAutoexec;
{$IFDEF DPMI32}
procedure SaveDsk;
{$ENDIF}

procedure GetFromClip(var S: String);
  {` Взять строку из буфера обмена. Если в буфере пусто - будет '' `}
procedure GetFromClipLong(var S: LongString);
  {` Взять строку из буфера обмена. Если в буфере пусто - будет '' `}
procedure PutInClip(const S: String);
  {` Положить строку в буфера обмена и в его историю `}
procedure PutInClipLong(const S: LongString);
  {` Положить строку в буфера обмена и в его историю `}

implementation

uses
  Dos, Lfnvp, HelpKern, Menus, FileCopy, VPUtils,
  DNHelp, DnIni, DnInip, Histries,
  VideoMan, Memory, DblWnd, Messages, HistList, FileFind,
  {$ifdef modem}
  {$IFDEF LINK}NavyLink, {$ENDIF}
  Terminal, ScrollBk,
  apPort, apUART,
  modemio,
  {$IFDEF PHONES}uDialer, {$ENDIF}
  {$endif}
  {$IFDEF PHONES}Phones, {$ENDIF}
  ASCIITab,
  {$IFDEF Game}Tetris, {$ENDIF}
  {$IFDEF Calendar}Calendar, {$ENDIF} {JO}
  {$IFDEF SpreadSheet}Calc, CellsCol, {$ENDIF}
  {$IFDEF DBView}DBView, DBWatch, {$ENDIF}
  {$IFNDEF MINARCH} {$IFNDEF OS2}ArchRead, {$ENDIF} {$ENDIF}
  {$IFDEF ARVID}Arvid, {$ENDIF}
  {$IFDEF Printer}PrintMan, {$ENDIF}
  {$IFDEF Tasklist}
  TaskLst,
  {$ENDIF}
  CCalc, Collect, {-$VIV}
  DnExec,
  Setups, RegAll, XDblWnd,
  Idlers, FlPanelX, {WinClp, // commented by unxed}
  Drives, Archiver, ArchSet,
  ArcView, FViewer, CmdLine, FBB, DNStdDlg,
  Colors, Microed, ed2, Editor, EdWin,
  Advance, Advance1, Advance2, Advance3, Advance4, Advance7,
  ColorSel, Eraser,  DiskInfo
  , FileType, PDSetup, UKeyMap
  , Startup, Startupp
  ;

{ Load and Store Palette routines }

procedure LoadIndexes(var S: TStream);
  var
    ColorSize: Byte;
  begin
  S.Read(ColorSize, SizeOf(ColorSize));
  if ColorSize > 0 then
    begin
    if ColorIndexes <> nil then
      FreeMem(ColorIndexes, 2+ColorIndexes^.ColorSize);
    GetMem(ColorIndexes, ColorSize);
    S.Read(ColorIndexes^, ColorSize);
    ColorIndexes^.ColorSize := (ColorSize-2);
    end;
  end;

procedure StoreIndexes(var S: TStream);
  var
    ColorSize: Byte;
  begin
  if ColorIndexes <> nil then
    ColorSize := 2+ColorIndexes^.ColorSize
  else
    ColorSize := 0;
  S.Write(ColorSize, SizeOf(ColorSize));
  if ColorSize > 0 then
    S.Write(ColorIndexes^, ColorSize);
  end;

constructor TDataSaver.Init;
  var
    R: TRect;
  begin
  R.Assign(0, 0, 0, 0);
  inherited Init(R);
  SetState(sfVisible, False);
  Options := Options and not ofSelectable;
  EventMask := 0;
  DataSaver := @Self;
  end;

const
  dskViewerFind = 2;
  dskEditorFind = 3;
  dskHideCmdLine = 4;
  dskViewerBounds = 5;
  dskTempContents = 6;
  dskTempContents2 = 7;

  {Cat:warn потенциально глюкавое место: нельзя читать в память больше, чем
          туда может поместиться; кроме того, нежелательно одной операцией
          читать несколько подряд расположенных переменных - это у нас в
          исходнике они подряд, а компилятор может думать иначе}

constructor TDataSaver.Load;
  var
    D, L: AWord;
    Q, Q2: LongInt;
  begin
  if DataSaver <> nil then
    Dispose(DataSaver, Done);
  DataSaver := nil;
  inherited Load(S);
  DataSaver := @Self;
  repeat
    S.Read(D, SizeOf(D));
    if D = 0 then
      Break;
    S.Read(L, SizeOf(L));
    case D of
      dskViewerBounds:
        S.Read(LastViewerBounds, L);
      dskViewerFind:
        S.Read(FViewer.SearchString, L);
      dskEditorFind:
        S.Read(Microed.SearchData, L);
      dskHideCmdLine:
        begin
        S.Read(HideCommandLine, L);
        if  (CommandLine <> nil)
             and (CommandLine^.GetState(sfVisible) and HideCommandLine)
        then
          ToggleCommandLine(not HideCommandLine);
        end;
      dskTempContents2:
        if TempFiles = nil then
          begin
          TempDirs := PStringCollection(S.Get);
          if TempDirs = nil then
            Continue;
          S.Read(Q, SizeOf(Q));
          if Q >= 0 then
            begin
            TempFiles := New(PFilesCollection, Init(Q+1, $10));
            TempFiles^.SortMode := psmLongName;
            TempFiles^.Duplicates := False;
            {TempFiles^.Owner := @Self;}
            for Q2 := 0 to Q do
              TempFiles^.AtInsert(Q2, LoadFileRecOwn(S, TempDirs));
            end;
          end
        else
          S.Seek(S.GetPos+L);
      else {case}
        S.Seek(S.GetPos+L);
    end {case};
  until D = 0;
  end { TDataSaver.Load };

procedure TDataSaver.Store;

  var
    D: AWord;
    I, Q, Q2, SPos{!!s}: LongInt;

  procedure StoreBlock(I: AWord; var B; Sz: AWord);
    begin
    S.Write(I, SizeOf(I));
    S.Write(Sz, SizeOf(Sz));
    S.Write(B, Sz);
    end;

  procedure MarkP(Blk: AWord);
    begin
    S.Write(Blk, SizeOf(Blk));
    S.Write(Blk, SizeOf(Blk));
    SPos := i32(S.GetPos);
    end;

  procedure UnMark;
    begin
    i := i32(S.GetPos)-SPos;
    S.Seek(SPos-SizeOf(AWord));
    S.Write(i, SizeOf(AWord));
    S.Seek(S.GetSize);
    end;

  begin { TDataSaver.Store }
  inherited Store(S);
  HideCommandLine := (CommandLine <> nil)
       and not CommandLine^.GetState(sfVisible);
  StoreBlock(dskViewerFind, FViewer.SearchString,
       SizeOf(FViewer.SearchString));
  StoreBlock(dskEditorFind, Microed.SearchData,
     SizeOf(Microed.SearchData));
  StoreBlock(dskViewerBounds, LastViewerBounds,
     SizeOf(LastViewerBounds)+SizeOf(TPoint)*2);
  StoreBlock(dskHideCmdLine, HideCommandLine, SizeOf(HideCommandLine));
  if  (TempFiles <> nil) and (TempFiles^.Count <> 0) then
    begin
    MarkP(dskTempContents2);
    S.Put(TempDirs);
    if TempDirs <> nil then
      begin
      Q := TempFiles^.Count-1;
      S.Write(Q, SizeOf(Q));
      for Q2 := 0 to Q do
        StoreFileRecOwn(S, TempFiles^.At(Q2), TempDirs);
      UnMark;
      end;
    end;
  D := 0;
  S.Write(D, SizeOf(D));
  end { TDataSaver.Store };

destructor TDataSaver.Done;
  begin
  DataSaver := nil;
  inherited Done;
  end;

{-DataCompBoy-}
function PresentFile(Name: String): PStream;
  var
    S: PStream;
  begin
  if ExistFile(Name) then
    begin
    S := New(PBufStream, Init(Name, stOpenRead, 2048));
    if S^.Status <> stOK then
      begin
      Dispose(S, Done);
      S := nil;
      end
    end
  else
    S := nil;
  PresentFile := S;
  end;
{-DataCompBoy-}

function CheckExit: Boolean;
  var
    Event: TEvent;
  function FindQuit(P: PView): Boolean;
    begin
    P^.HandleEvent(Event);
    FindQuit := (Event.What = evNothing) or not P^.Valid(cmQuit);
    end;
  begin
  { if FormatWindow <> nil then begin CheckExit := False; Exit; end;}
  Event.What := evCommand;
  Event.Command := cmQuit;
  CheckExit := Desktop^.FirstThat(@FindQuit) = nil;
  end;

{-DataCompBoy-}
procedure ClearSwap;
  var
    DT: Dos.DateTime;
    L: LongInt;
    SR: lSearchRec;

  procedure ClearFiles(const FileSpec: String);
    begin
    lFindFirst(SwpDir+FileSpec, Archive, SR);
    while DosError = 0 do
      begin
      if SR.SR.Time < l then
        EraseFile(SR.FullName);
      ClrIO;
      lFindNext(SR);
      end;
    lFindClose(SR);
    end;

  begin
  FillChar(DT, SizeOf(DT), 0);
  GetDate(DT.Year, DT.Month, DT.Day, Word(L));

  PackTime(DT, L);
  SetFileAttr(SwpDir+'DN.FLG', 0);
  ClrIO;
  ClearFiles('DN*.SWP');
  ClearFiles('$DN*.*');
  ClearFiles('$$DN*.*');
  ClearFiles('$$$DN*.*');
  end { ClearSwap };
{-DataCompBoy-}

{-DataCompBoy-}
constructor TDNApplication.Init;
  var
    R: TRect;
    I: Integer;
    C: Char absolute I { let's so };
    FileName: String;
    LoadStream: PStream;
    Event: TEvent;

  var
    flj: Boolean;
  begin
  for C := 'A' to 'Z' do
    DrvTrees[C].C := nil;
  LastViewerBounds.Assign(0, 0, 0, 0);

  if RunFirst then
    ClearSwap;

  TApplication.Init;

  LoadHistories;

  GetExtent(R);
  if ShowSeconds then
    R.A.X := R.B.X-10
  else
    R.A.X := R.B.X-7;
  R.B.Y := R.A.Y+1;
  Clock := New(PClockView, Init(R));
  if InterfaceData.Options and ouiClock = 0 then
    Clock^.Hide;
  PClockView(Clock)^.Update;

  Desktop^.GetExtent(R);
  Dec(R.B.Y);
  Dec(R.B.X);
  R.A.Y := R.B.Y-3;
  R.A.X := R.B.X-5;
  {$IFDEF TrashCan}
  TrashCan := New(PTrashCan, Init(R));
  TrashCan^.ImVisible := False;
  TrashCan^.Hide;
  Desktop^.Insert(TrashCan);
  {$ENDIF}
  if HideCommandLine then
    CommandLine^.Hide;
  (*
  If RunFirst then
   begin
    OldArchiveName:='';
    NewArchiveName:='';
   end;
*)
  LoadStream := PresentFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
  if LoadStream = nil then
    LoadStream := PresentFile(SourceDir+'DN'+GetEnv('DNDSK')+'.DSK');
  if LoadStream <> nil then
    RetrieveDesktop('', LoadStream, True);
  InitDrivers;

  {-$VOL begin}
  if not RunFirst or cbAutoSave then
    begin
    ClipBoardStream := GetMeMemoStream;
    LoadStream := PresentFile(SourceDir+'CLIPBOAR'+'.DN');
    if  (LoadStream <> nil) and (ClipBoardStream <> nil) then
      begin
      LoadStream^.Seek(0);
      ClipBoardStream^.CopyFrom(LoadStream^,
        i32(LoadStream^.GetSize){!!s-});
// fixme: commented by unxed
//      if SystemData.Options and ossUseSysClip <> 0 then
//        SyncClipOut {(false)};
      end;
    FreeObject(LoadStream);
    end;
  {-$VOL end}

  Insert(Clock);

  flj := False;
  if RunFirst then
    for I := 1 to ParamCount do
      begin
      if flj then
        FileName := FileName+ParamStr(I)
      else
        FileName := ParamStr(I);
      if Pos('"', FileName) <> 0 then
        flj := not flj;
      if  (FileName[1] <> '/') and not flj then
        EditFile(True, DelSquashes(FileName));
      end;

  if RunMenu then
    begin
    Event.What := evCommand;
    Event.Command := cmUserMenu;
    PutEvent(Event);
    end;
  if ExistFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP') then
    EraseByName(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
  end { TDNApplication.Init };
{-DataCompBoy-}

procedure TDNApplication.InitCommandLine;
  var
    R: TRect;
    HideCL: Boolean;
  begin
  HideCL := (InterfaceData.Options and ouiHideCmdline <> 0)
     or HideCommandLine;

  R.Assign(0, 0, 0, 0);
  TreeReader := New(PTreeReader, Init(R));
  Insert(TreeReader);
  TreeReader^.Hide;

  GetExtent(R);

  R.A.Y := R.B.Y-1-Byte(InterfaceData.Options and ouiHideStatus = 0);
  R.B.Y := R.A.Y+1-Byte(HideCL);
  CommandLine := New(PCommandLine, Init(R));
  Insert(CommandLine);
  if not HideCL then
    ActivateView(CommandLine);
  end;

{-DataCompBoy-}
procedure SaveRealDsk;
  begin
  PDNApplication(Application)^.SaveDesktop
    (SourceDir+'DN'+GetEnv('DNDSK')+'.DSK');
  end;
{-DataCompBoy-}

procedure SaveDsk;
  begin
  ClrIO;
  if not TottalExit
  then
    begin
   {$IFDEF DPMI32}
    if StartDir <> '' then
      StartDir := '<' + StartDir; {помечаем, что StartDir нужно сохpанить}
   {$ENDIF}
    PDNApplication(Application)^.SaveDesktop
      (SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
    end;
  end;

{-DataCompBoy-}
destructor TDNApplication.Done;
  var
    B: Word;
    SaveStream: PStream;
    INItime, INIsize: LongInt;
  begin
  {-$VOL begin} {if CBAutoSave added by piwamoto}
  if cbAutoSave then
    begin
    SaveStream := New(PBufStream, Init(SourceDir+'CLIPBOAR'+'.DN',
           stCreate, 2048));
    if  (SaveStream <> nil) and (SaveStream^.Status = stOK)
         and (ClipBoardStream <> nil)
    then
      begin
      ClipBoardStream^.Seek(0);
      SaveStream^.CopyFrom(ClipBoardStream^,
        i32(ClipBoardStream^.GetSize){!!s});
      end;
    Dispose(SaveStream, Done);
    SaveStream := nil;
    end;
  if ClipBoard <> nil then
    Dispose(ClipBoard, Done);
  ClipBoard := nil;
  if ConfigModified then
    WriteConfig;
{JO}
  if ProbeINI(INItime, INIsize) then
    WriteIniCache(INItime, INIsize);
{/JO}
  if  (StartupData.Unload and osuAutosave <> 0)
  then
    SaveRealDsk
  {$IFDEF DPMI32}
  else
    SaveDsk
  {$ENDIF}
    ;
  {AK155 SaveHistories;}
  {все запоминалось по ходу дела, притом с чтением,}
  {а теперь можно только напортить}
  SaveHistories;
  {JO: пока вернул назад, а то история совсем не запоминается}
  HideCommandLine := (CommandLine <> nil)
       and not CommandLine^.GetState(sfVisible);
  B := $8000 or (Byte(HideCommandLine));
  inherited Done;
  {$IFDEF Modem}
  SetBlink(StartupData.Unload and osuBlinking <> 0);
  DeallocateScrollBk;
  if StartupData.Unload and osuRestorePal <> 0 then
    ResetVGApalette(True);
  if COMPort <> nil then
    begin
    B := COMPort^.GetBaseAddr;
    if not TottalExit then
      begin
      COMPort^.ptOptionsOff(ptDropModemOnClose);
      COMPort^.ptOptionsOff(ptRestoreOnClose);
      end;
    Dispose(COMPort, Done);
    SetFIFOBuffering(B, False, 0);
    end;
  {$ENDIF}
  DoneDrivers;
  end { TDNApplication.Done };
{-DataCompBoy-}

procedure TDNApplication.EventError;
  begin
  {$IFDEF SS}
  if  (Event.What = evMouseMove) and (Event.Where.Y = 0)
       and (Event.Where.X = Size.X-1)
    and (SSaver = nil) and SaversData.Mouse
  then
    InsertIdler;
  {$ENDIF}
  end;

procedure TDNApplication.ChgColors;
  begin
  ChangeColors;
  WriteConfig;
  end;

{JO}
procedure WriteHighlite;
  var
    F: lText;
  begin
  lAssignText(F, SourceDir+'dnhgl.grp');
  lRewriteText(F);
  Writeln(F.T, CustomMask1);
  Writeln(F.T, CustomMask2);
  Writeln(F.T, CustomMask3);
  Writeln(F.T, CustomMask4);
  Writeln(F.T, CustomMask5);
  Writeln(F.T, CustomMask6);
  Writeln(F.T, CustomMask7);
  Writeln(F.T, CustomMask8);
  Writeln(F.T, CustomMask9);
  Writeln(F.T, CustomMask10);
  Writeln(F.T, Archives);
  Close(F.T);
  end;
{JO}

{-DataCompBoy-}
procedure WriteConfig;
  {$IFDEF DNPRG}
  var
    S: TBufStream;
    I: AWord;
    SPos: LongInt;

  procedure StoreBlock(I: AWord; var B; Sz: AWord);
    begin
    S.Write(I, SizeOf(I));
    S.Write(Sz, SizeOf(Sz));
    S.Write(B, Sz);
    end;

  procedure MarkP(Blk: AWord);
    begin
    S.Write(Blk, SizeOf(Blk));
    S.Write(Blk, SizeOf(Blk));
    SPos := i32(S.GetPos);
    end;

  procedure UnMark;
    begin
    i := i32(S.GetPos)-SPos;
    S.Seek(SPos-SizeOf(AWord));
    S.Write(i, SizeOf(AWord));
    S.Seek(S.GetSize);
    end;
  {$ENDIF} {DNPRG}

  begin { WriteConfig }
  {$IFDEF DNPRG}
  ConfigModified := False;
  S.Init(SourceDir+'DN'+GetEnv('DNCFG')+'.CFG', stCreate, 16384);
  if S.Status <> stOK then
    begin
    Msg(erCantOpenConfig, nil, mfError+mfOKButton);
    S.Done;
    Exit;
    end;
  S.Write(ConfigSigns[NumSupportedConfigs].Sign[1],
     ConfigSigns[NumSupportedConfigs].SignLen);
  if ConfigSigns[NumSupportedConfigs].HavVer then
    S.Write(VersionWord, SizeOf(VersionWord));
  StoreBlock(cfgNewSystemData, SystemData, SizeOf(SystemData));
  StoreBlock(cfgNewStartupData, StartupData, SizeOf(StartupData));
  StoreBlock(cfgMouseData, MouseData, SizeOf(MouseData));
  StoreBlock(cfgInterfaceData, InterfaceData, SizeOf(InterfaceData));
  {$IFDEF SS}
  StoreBlock(cfgNewSaversData, SaversData.Time,
       SizeOf(SaversData)-SizeOf(SaversData.Selected)*2);
  MarkP(cfgSavers);
  S.Put(SaversData.Selected.List);
  UnMark;
  {$ENDIF}
  StoreBlock(cfgSystemColors, SystemColors, SizeOf(SystemColors));
  StoreBlock(cfgEditorDefaults, EditorDefaults, SizeOf(EditorDefaults));
  StoreBlock(cfgTetrisRec, TetrisRec, SizeOf(TetrisRec));
  {$IFDEF PRINTMANAGER}
  StoreBlock(cfgPrinterSetup, RPrinterSetup, SizeOf(RPrinterSetup));
  {$ENDIF}
  StoreBlock(cfgPanSetupPreset, PanSetupPreset, SizeOf(PanSetupPreset));
  StoreBlock(cfgCountryInfo, CountryInfo, SizeOf(CountryInfo));
  StoreBlock(cfgConfirms, Confirms, SizeOf(Confirms));
  StoreBlock(cfgUUEData, UUDecodeOptions,
       SizeOf(UUDecodeOptions)+SizeOf(TUUEncodeData));
  StoreBlock(cfgTermDefaults, TerminalDefaults, SizeOf(TerminalDefaults));
  StoreBlock(cfgFMSetup, Startup.FMSetup, SizeOf(Startup.FMSetup));
  StoreBlock(cfgVGAPalette, VGA_palette, SizeOf(VGA_palette));
  StoreBlock(cfgBlink, CurrentBlink, SizeOf(CurrentBlink));
  StoreBlock(cfgFFindOptions, FileFind.FindRec.Options, SizeOf(Word)*2);
  StoreBlock(cfgDriveInfoData, DriveInfoData, SizeOf(DriveInfoData));
  StoreBlock(cfgExtractOptions, UnarchiveOpt, SizeOf(UnarchiveOpt));
  {JO}
  StoreBlock(cfgDefaultArchiver, DefaultArchiver, SizeOf(DefaultArchiver));
  StoreBlock(cfgDefaultArchiverMode, DefaultArcMode,
       SizeOf(DefaultArcMode));
  StoreBlock(cfgExtractOptions, UnarchiveOpt, SizeOf(UnarchiveOpt));
  {JO: два раза???}
  StoreBlock(cfgChangeCaseOptions, ChangeNamesCaseOptions,
       SizeOf(ChangeNamesCaseOptions));
  StoreBlock(cfgAppPalette, appPalette, SizeOf(appPalette));
  StoreBlock(cfgComareDirsOptions, ComareDirsOptions,
       SizeOf(ComareDirsOptions));
  StoreBlock(cfgSortCurPanTypeOnly, SortCurPanTypeOnly,
       SizeOf(SortCurPanTypeOnly));
  StoreBlock(cfgFullMenuPanelSetup, FullMenuPanelSetup,
       SizeOf(FullMenuPanelSetup));
  StoreBlock(cfgCalcFormat, CalcFormat,
       SizeOf(CalcFormat));

  StoreBlock(0, I, 0);
  S.Done;
  WriteHighlite; {JO}
  {$ENDIF} {DNPRG}
  end { WriteConfig };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDNApplication.LoadDesktop(var S: TStream);
  var
    P: PView;
    PP: PView;
    Pal: PString;
    SaveState: AWord;
  begin
  if Desktop^.Valid(cmClose) then
    begin
    Desktop^.Clear;
    repeat
      P := PView(S.Get);

      {Cat}
      if S.Status <> stOK then
        begin
        ErrMsg(erCantReadDesktop);
        Break;
        end;
      {/Cat}

      if P <> nil then
        begin
        if DataSaver <> nil then
          begin
          Dispose(DataSaver, Done);
          DataSaver := nil;
          Continue
          end;
        P := ValidView(P);
        if P = nil then
          Continue;
        SaveState := P^.State;
        P^.Hide;
        Desktop^.InsertView(P, Desktop^.Last);
        if SaveState and sfVisible <> 0 then
          P^.Show;
        end;
    until { P = nil;}S.GetPos = S.GetSize;
    {AK155: P = nil бывает, например, при невозможности открыть
      запомненный просмотр; это не повод терять все остальные настройки }
    end;
  P := Desktop^.Current;
  if P <> nil then
    P^.SetState(sfActive, True);
  end { TDNApplication.LoadDesktop };
{-DataCompBoy-}

procedure TDNApplication.StoreDesktop(var S: TStream);
  var
    Pal: PString;

  procedure WriteView(P: PView);
    type
      TLongInt = array[0..10] of LongInt;
      PLongInt = ^TLongInt;
    var
      l: array[0..5] of LongInt;
    begin
    if  (P <> Desktop^.Last)
      {$IFDEF TrashCan} and (P <> PView(TrashCan)) {$ENDIF}
      and (P <> PView(Desktop^.Background))
      and (TypeOf(P^) <> TypeOf(TWriteWin))
      and (TypeOf(P^) <> TypeOf(THelpWindow))
    then
      S.Put(P);
    end;

  begin
  if DataSaver = nil then
    New(DataSaver, Init);
  if DataSaver <> nil then
    begin
    S.Put(DataSaver);
    Dispose(DataSaver, Done);
    DataSaver := nil;
    end;
  Desktop^.ForEach(@WriteView);
  S.Put(nil);
  end { TDNApplication.StoreDesktop };

function CacheLngId: String;
  begin
  {$IFDEF DNPRG}CacheLngId := VersionName+LngId+VersionDate+'123';
  {$ENDIF}
  end;

procedure TDNApplication.ProcessTempFile(TFStr: String);
  var
    F: lFile;
    Q: String;
    QQ: Integer;
    CC: Word;
    Event: TEvent;

  function GetGLUKName(s: String): String;
    begin
    GetGLUKName := GetPath(s)+Copy(GetName(s), 1, 12);
    end;

  function GetfURZ2(const s: String): String;
    var
      a, aa, aaa: String;
    begin
    lFSplit(s, a, aa, aaa);
    aa := aa+aaa;
    GetfURZ2 := a+Copy(aa, 1, 8)+'.'+Copy(aaa, 9, 3);
    end;

  begin
  if PosChar(TFStr[1], '+-=<>|[!') = 0 then
    begin
    lAssignFile(F, TFStr);
    lSetFAttr(F, $20);
    lEraseFile(F);
    end
  else
    begin
    CC := 0;
    TempFile := Copy(TFStr, 2, MaxStringLength);
    QQ := Pos('|', TempFile);
    Dec(QQ);
    if QQ > 0 then
      Q := Copy(TempFile, QQ+2, MaxStringLength)
    else
      Q := '';
    QQ := QQ and 255; {BP bugfix by piwamoto}
    if not ExistFile(TempFile) then
      TempFile := GetfURZ(Copy(TFStr, 2, QQ));
    if not ExistFile(TempFile) then
      TempFile := GetfURZ2(Copy(TFStr, 2, QQ));
    if not ExistFile(TempFile) then
      TempFile := GetGLUKName(Copy(TFStr, 2, QQ));
    if not ExistFile(TempFile) then
      TempFile := Copy(TFStr, 2, QQ);
    case TFStr[1] of
      '-':
        CC := cmIntFileView;
      '=':
        CC := cmDBFView;
      '>':
        CC := cmWKZView;
      '<':
        CC := cmTextView;
      '|':
        CC := cmHexView;
      '[':
        CC := cmReadArchive;
      '!':
        CC := cmViewFilter;
      else {case}
        CC := cmFileView;
    end {case};
    if CC <> 0 then
      begin
      if Q <> '' then
        TempFile := TempFile+'|'+Q;
      Event.What := evCommand;
      Event.Command := CC;
      Event.InfoPtr := @TempFile;
      PutEvent(Event);
      end;
{AK155 27-10-2004 Убрал очистку RunMenu, поскольку единственный
её результат - это глюк, заключающийся в том, что автовыпадение
меню перестаёт работать после просмотра файла из архива
    RunMenu := False;
}
    end;
  end;

{-DataCompBoy-}
procedure TDNApplication.RetrieveDesktop;
  var
    S: PStream;
    Sign: String[MaxSignLen];
    B, BB: Boolean;
    SM: Word;
    R: TRect;
    SaveBounds: TRect;
    PS: PString;
    OldAppSize, OldDskSize: TPoint;
   {$IFDEF DPMI32}
    PJ: PString;
    FCT: PFilesCollection;
    FRT: PFileRec;
    OldConfirms: Word;
    PV: PView;
    TempExtrDir, SCurDir, Str1: String;
    ForceMod: Boolean;
   {$ENDIF}

  label
    Err;
  begin { TDNApplication.RetrieveDesktop }
  HideCommandLine := (CommandLine <> nil)
         and (not CommandLine^.GetState(sfVisible));
  SetState(sfActive, True);
  if LS = nil
  then
    S := New(PBufStream, Init(FileName, stOpenRead, 4096))
  else
    S := LS;
  if not Desktop^.Valid(cmClose) then
    Exit;
  if LowMemory
  then
    OutOfMemory
  else if (S^.Status <> stOK) or (S^.GetSize < SizeOf(DskSign))
  then
Err:
    ErrMsg(erCantReadDesktop)
  else
    begin
    S^.Read(Sign[1], DskSign.SignLen);
    Sign[0] := Char(DskSign.SignLen);
    if Sign <> DskSign.Sign then
      goto Err;
    PS := S^.ReadStr;
    if PS <> nil then
      lChDir(PS^);
    DisposeStr(PS);
{$IFDEF DPMI32}
    PJ := S^.ReadStr;
    if  (PJ <> nil) and (PJ^ <> '') then
      ProcessTempFile(PJ^);
    DisposeStr(PJ);

//JO: 31-05-2006 - процедура перекидывания файлов из временного
//                 подкаталога после запуска архиватора
    PJ := S^.ReadStr;
    if  (PJ <> nil) and (PJ^ <> '') and (Pos('|', PJ^) > 0)  then
      begin
      ForceMod := False;
      if PJ^[1] = '<' then
        begin
        ForceMod := True;
        TempExtrDir := Copy(PJ^, 2, MaxStringLength);
        end
      else
        TempExtrDir := PJ^;
      SCurDir := Copy(TempExtrDir, Pos('|', TempExtrDir)+1,
                      MaxStringLength);
      TempExtrDir := Copy(TempExtrDir, 1, Pos('|', TempExtrDir)-1);
    { перекидываем файлы из временного подкаталога в каталог назначения}
      PV := New(PUserWindow, Init);
      Desktop^.Insert(PV);
      SetLength(TempExtrDir, Length(TempExtrDir)-1);
      Str1 := GetPath(TempExtrDir);
      CopyDirContent(TempExtrDir+'\'+SCurDir, Str1, True, ForceMod);
      PV^.Free;
    { удаляем временный каталог со всем, что в нём осталось}
      FRT := NewFileRec(GetName(TempExtrDir),
        GetName(TempExtrDir),
        0, 0, 0, 0, Directory,
        @Str1);
      New(FCT, Init(1, 1));
      FCT^.AtInsert(0, FRT);
      OldConfirms := Confirms;
      Confirms := 0;
      lGetDir(0, DirToChange);
      LFNvp.lChDir(Str1);
      if ActiveDir[2] = ':' then {освобождаем каталог}
        ChDir(Copy(ActiveDir, 1, 2) + '\');
      Eraser.EraseFiles(FCT);
      LFNvp.lChDir(DirToChange);
      DirToChange := '';
      Confirms := OldConfirms;
      FCT^.DeleteAll;
      Dispose(FCT, Done);
      end;
    DisposeStr(PJ);

//JO: 8-06-2006 - восстанавливаем стаpтовый каталог
    PJ := S^.ReadStr;
    if  (PJ <> nil) and (PJ^ <> '') then
      begin
      StartDir := PJ^;
      ChDir(StartDir);
      end;
    DisposeStr(PJ);
{$ELSE} {DPMI32}
    S^.ReadStrV(FreeStr);
    S^.ReadStrV(FreeStr);
    S^.ReadStrV(FreeStr);
{$ENDIF}
    S^.Read(SM, SizeOf(SM));
    S^.Read(OldAppSize, SizeOf(Size));
    S^.Read(OldDskSize, SizeOf(Size));
    LoadIndexes(S^);
    GetExtent(SaveBounds);
    Desktop^.Clear;
    BB := not (OldDskSize.Equals(Desktop^.Size));
    if BB then
      begin
      Desktop^.Hide;
      R.A := Desktop^.Origin;
      R.B.X := R.A.X+OldDskSize.X;
      R.B.Y := R.A.Y+OldDskSize.Y;
      Desktop^.ChangeBounds(R);
      end;
    {LoadDesktop(S^);} {Cat: перенёс в самый конец}
    S^.Read(TempBounds, SizeOf(TempBounds));
    {S^.Read(ArcBounds, SizeOf(TempBounds));}
    {$IFDEF TrashCan}S^.Read(TrashCan^.ImVisible, 1); {$ENDIF}
    KeyMacroses := PCollection(S^.Get);
    S^.Read(R, SizeOf(R));
    if not ShowSeconds then
      if R.A.X > (ScreenWidth shr 1) then
        Inc(R.A.X, 3)
      else
        Dec(R.B.X, 3);
    Clock^.Locate(R);
    {S^.Read(ArcFlags, 4);}
    {$IFDEF TrashCan}
    if TrashCan^.ImVisible then
      begin
      TrashCan^.Show;
      S^.Read(R, SizeOf(R));
      TrashCan^.Locate(R)
      end;
    {$ENDIF}
    if PreserveMenuPositions then
      LoadMenuDefaults(MenuBar^.Menu, S^);
    LoadDesktop(S^);
    // JO: нижележащий кусок должен _обязательно_ быть _после_ LoadDesktop,
    //     иначе размер панелей не подстраивается под видеорежим
    if BB then
      begin
      R.Assign(0, Byte(InterfaceData.Options and ouiHideMenu = 0),
         Size.X, Size.Y
        -Byte(InterfaceData.Options and ouiHideStatus = 0)
        -Byte(not HideCommandLine));
      Desktop^.ChangeBounds(R);
      Desktop^.Show;
      end;
    end;
  Desktop^.Redraw;
  GetBounds(R);
  if  (Clock^.Size.X <= 0) or (Clock^.Size.Y <= 0)
       or not R.Contains(Clock^.Origin)
  then
    begin
    if ShowSeconds then
      R.Assign(Size.X-10, 0, Size.X, 1)
    else
      R.Assign(Size.X-7, 0, Size.X, 1);
    Clock^.Locate(R);
    end;
  Dispose(S, Done);
  ActivateView(Desktop^.Current);
  GlobalMessage(evCommand, cmRereadForced, nil);
  end { TDNApplication.RetrieveDesktop };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDNApplication.SaveDesktop(const FileName: String);
  var
    S: PStream;
    F: lFile;
    B: Boolean;
    PP: Pointer;
    R: TRect;

  begin { TDNApplication.SaveDesktop }
  ClrIO;
  S := New(PBufStream, Init(FileName, stCreate, 2048));
  if not LowMemory and (S^.Status = stOK) then
    begin
    S^.Write(DskSign.Sign[1], DskSign.SignLen);
    S^.WriteStr(@DirToChange);
   {$IFDEF DPMI32}
    S^.WriteStr(@TempFile);
    S^.WriteStr(@DirToMoveContent); //каталог назначения пpи pазаpхивиpовании
                                    //чеpез вpеменный подкаталог
    if StartDir[1] = '<' then
      begin
      System.Delete(StartDir, 1, 1);
      S^.WriteStr(@StartDir); //каталог, из котоpого DN/2 был запущен
      end
    else
      S^.WriteStr(@NullStr);
   {$ELSE}
    S^.WriteStr(@NullStr);
    S^.WriteStr(@NullStr);
    S^.WriteStr(@NullStr);
   {$ENDIF}
    S^.Write(ScreenMode, SizeOf(Word));
    S^.Write(Size, SizeOf(Size));
    S^.Write(Desktop^.Size, SizeOf(Size));
    StoreIndexes(S^);
    {StoreDesktop(S^);} {Cat: перенёс в самый конец}
    S^.Write(TempBounds, SizeOf(TempBounds));
    {$IFDEF TrashCan}S^.Write(TrashCan^.ImVisible, 1); {$ENDIF}
    S^.Put(KeyMacroses);
    Clock^.GetBounds(R);
    if R.A.X > (ScreenWidth shr 1) then
      R.A.X := R.B.X-10
    else
      R.B.X := R.A.X+10;
    S^.Write(R, SizeOf(R));
    {$IFDEF TrashCan}
    TrashCan^.GetBounds(R);
    if TrashCan^.ImVisible then
      S^.Write(R, SizeOf(R));
    {$ENDIF}
    if PreserveMenuPositions then
      StoreMenuDefaults(MenuBar^.Menu, S^);
    StoreDesktop(S^);
    if S^.Status <> stOK then
      begin
      PP := @FileName;
      Msg(erCantCreateFile, @PP, mfOKButton+mfError);
      //    MessageBox('S^.Status    = '+ItoS(S^.Status)+#13+
      //               'S^.ErrorInfo = '+ItoS(S^.ErrorInfo), nil, mfOkButton);
      Dispose(S, Done);
      lAssignFile(F, FileName);
      lEraseFile(F);
      Exit;
      end;
    end;
  Dispose(S, Done);
  end { TDNApplication.SaveDesktop };
{-DataCompBoy-}

procedure TDNApplication.OutOfMemory;
  begin
  Msg(erNotEnoughMemory, nil, mfError+mfOKButton);
  end;

const
  ExtFileName: array[Boolean] of String[10] =
    ('dn.vwr', 'dnalt.vwr');

  {-DataCompBoy-} {AK155}
procedure TDNApplication.ViewFile(AltExt, NoExtFile: Boolean;
     FileName: String); {AK155}
  var
    W: PWindow;
    R: TRect;
    fr: PFileRec;
    up: tUserParams;
    P: PViewRecord;
    I, J: Integer;
    PS: PString;
    q: Integer;
    RN: String;

  function SpecialIntView: Boolean;
    var
      FileIsDBF: Boolean;
      XT: String;
    label
      db;
    begin
    SpecialIntView := False;
    XT := ' '+RN;
    UpStr(XT);
    PS := @XT;
    if  (InterfaceData.Options and ouiTrackViewers <> 0) and
        (ViewHistory <> nil)
    then
      i := ViewHistory^.IndexOf(@PS)
    else
      i := -1;
    if i >= 0 then
      begin
      P := ViewHistory^.At(i);
      Q := P^.fViewMode;
      if Q and vmInternal = vmInternal then
        Q := -1;
      end
    else
      Q := -1;
    XT := UpStrg(GetExt(FileName));
    {$IFDEF DBView}
    if  (XT = '.DBF') or (Q = vmDB) then
      begin
      {--- start -------- Eugeny Zvyagintzev ---------}
      {Use history if possible}
      XT := ' '+RN;
      UpStr(XT);
      PS := @XT;
      Q := -1;
      if  (InterfaceData.Options and ouiTrackViewers <> 0) and
          (ViewHistory <> nil)
      then
        i := ViewHistory^.IndexOf(@PS)
      else
        i := -1;
      if i >= 0 then
        begin
        P := ViewHistory^.At(i);
        Q := P^.fViewMode;
        end;
      FileIsDBF := False;
      W := New(PDBWindow, Init(RN+'|'+FileName, FileIsDBF));
      {If file is not valid DBF and NOT empty DBF file then use internal viewer}
      if  (W = nil) and (FileIsDBF = False) then
        Exit; { Flash }
      {--- finish -------- Eugeny Zvyagintzev ---------}
      if  (Q = vmDB) and (W <> nil) then
        with P^, W^ do
          begin
          R.Assign(fOrigin.X, fOrigin.Y, fOrigin.X+fSize.X,
             fOrigin.Y+fSize.Y);
          AdjustToDesktopSize(R, fDeskSize);
          Locate(R);
          with PDBWindow(W)^.P^ do
            begin
            Delta := fdDelta;
            Pos := fdPos;
            XCoder.FromHistory(fKeyMap, fToAscii, fCodeTag);
            end;
          end;
      goto db;
      end;
    {$ENDIF}

    {$IFDEF SpreadSheet}
    if  (XT = '.WKZ') or (Q = vmSpread) or (Q = vmSpreadSL) then
      begin
      W := New(PCalcWindow, Init(R, FileName));
      if  ( (Q = vmSpread) or (Q = vmSpreadSL)) and (W <> nil) then
        with P^, W^ do
          begin
          R.Assign(fOrigin.X, fOrigin.Y, fOrigin.X+fSize.X,
             fOrigin.Y+fSize.Y);
          AdjustToDesktopSize(R, fDeskSize);
          Locate(R);
          with PCalcWindow(W)^.CalcView^ do
            begin
            Delta := fsDelta;
            Cur := fsCur;
            Mark := fsMark;
            CurrentCalc := fsCurrentCalc;
            SearchPos := fsSearchPos;
            ErrorCell := fsErrorCell;
            HScroll^.Value := Delta.X; {AK155}
            VScroll^.Value := Delta.Y; {AK155}
            ShowSeparators := Q = vmSpreadSL; {AK155}
            end;
          end;
      goto db;
      end;
    {$ENDIF}

    SpecialIntView := ArcViewer(FileName, RN);
    Exit;

db:
    if  (W <> nil) and not W^.Valid(0) then
      begin
      Dispose(W, Done);
      Exit;
      end;
    {    StoreViewInfo(W);  AK155: сохранять бессмысленно (еще нечего)
        и излишне (сохранит Done) }
    InsertWindow(W);
    SpecialIntView := True;
    end { SpecialIntView: };

  procedure TextView;
    var
      S: String;
      V: PFileViewer;
    begin
    W := New(PFileWindow, Init(FileName, RN, EditorDefaults.ViOpt and 1 <>
           0));
    if W = nil then
      Exit;
    if not (W^.Valid(cmValid)) then
      W^.Free
    else
      begin
      V := PFileViewer(W^.Current);
      S := V^.VFileName;
      System.Insert(' ', S, 1);
      UpStr(S);
      PS := @S;
      if  (InterfaceData.Options and ouiTrackViewers <> 0)
           and (ViewHistory <> nil)
      then
        i := ViewHistory^.IndexOf(@PS)
      else
        i := -1;
      if i >= 0 then
        if  (PViewRecord(ViewHistory^.At(i))^.fViewMode and not
             vmInternal) > vmAsm
        then
          i := -1;
      if i >= 0 then
        begin
        P := ViewHistory^.At(i);
        R.Assign(P^.fOrigin.X, P^.fOrigin.Y,
          P^.fOrigin.X+P^.fSize.X,
          P^.fOrigin.Y+P^.fSize.Y);
        AdjustToDesktopSize(R, P^.fDeskSize);
        W^.Locate(R);
        with V^, P^ do
          begin
          if (fPos > FileSize) or { Flash }
             (InterfaceData.Options and ouiStoreViewerPosition = 0)
          then
            begin
            fPos := 0;
            fBufPos := 0;
            end;
          Seek({$ifndef LargeFileSupport} Round {$endif}(fPos));
          XCoder.FromHistory(fKeyMap, fToAscii, fCodeTag);
          ViewMode := fViewMode and not vmInternal;
          Filter := fFilter;
          HexEdit := fHexEdit;
          Wrap := fWrap;
          XDelta := fXDelta;
          HexPos := fHexPos;
          Cur := fCur;
          MarkPos := fMarks;
          Loaded := True;
          MakeLines;
          end;
        end;
      InsertWindow(W);
      end;
    end { TextView };

  begin { TDNApplication.ViewFile }
  I := PosChar('|', FileName);
  if I > 0 then
    begin
    RN := Copy(FileName, I+1, MaxStringLength);
    SetLength(FileName, I-1);
    if  (ActivePanel <> nil)
           and (Copy(PFilePanelRoot(ActivePanel)^.DirectoryName, 1, 4) =
         'UC2:')
    then
      begin
      J := PosChar(';', FileName);
      if J > 0 then
        SetLength(FileName, J-1);
      {AK155 удалить номер версии файла в uc2-архиве }
      end;
    end;
  FileName := lFExpand(FileName);
  if I <= 0 then
    RN := FileName;
  fr := CreateFileRec(FileName);
  FillChar(up, SizeOf(up), 0);
  up.Active := fr;
  Desktop^.GetExtent(R);
  Abort := False;
  if  (ReuseViewers > 0)
    and (Message(Desktop, evBroadcast, cmFindView, @FileName) <> nil)
    and ((ReuseViewers = 2) or
        ( (ReuseViewers = 1) and
          (MessageBox(GetString(dlOpenNewWindow), nil,
             mfYesButton+mfNoButton) <> cmYes)
        )
      )
  then
    begin
    TempFile := '';
    DisposeStr(fr^.Owner);
    DelFileRec(fr);
    Exit;
    end;

  { Логика выбора просмотра такая:
  по F3 (not AltExt) - согласно dn.vwr, затем специальный внутренний, затем текстовый;
  по Alt-F3 (AltExt) - согласно dnalt.vwr, затем текстовый;
  по Alt-Shift-F3 (это вообще не здесь) - безусловно текстовый. }

  if not NoExtFile and
    ExecExtFile(ExtFileName[AltExt], @up, dlLoadingViewer)
  then
    Exit;
  if not AltExt and SpecialIntView then
    Exit;
  TextView;
  DisposeStr(fr^.Owner);
  DelFileRec(fr); {Cat}
  end { TDNApplication.ViewFile };
{-DataCompBoy-} {AK155}

{-DataCompBoy-}
procedure TDNApplication.EditFile;
  var
    W: PWindow;
    R: TRect;
    PS: PString;
    V: PFileEditor;
    I: Integer;
    P: PEditRecord;
    fr: PFileRec;
    up: tUserParams;
    s: String;

  function Edit: Boolean;
    begin
    Edit := ExecExtFile('DN.EDT', @up, dlLoadingEditor);
    end;

  label ex;
  begin
  {JO: редактирование найденных в архиве файлов из панели поиска не предусмотрено}
  if PathFoundInArc(FileName) then
    Exit;
  {/JO}
  FileName := lFExpand(FileName);
  fr := CreateFileRec(FileName);
  FillChar(up, SizeOf(up), 0);
  up.Active := fr;
  if  (Intern xor (SystemData.Options and ossEditor = 0)) or
    not Edit
  then
    begin
    if  (ReuseEditors > 0)
      and (Message(Desktop, evBroadcast, cmFindEdit, @FileName) <> nil)
      and ((ReuseEditors = 2) or
          ( (ReuseEditors = 1) and
            (MessageBox(GetString(dlOpenNewWindow), nil,
               mfYesButton+mfNoButton) <> cmYes)
          )
        )
    then
      begin
      TempFile := '';
      DisposeStr(fr^.Owner);
      DelFileRec(fr);
      Exit;
      end;
    if  (InterfaceData.Options and ouiStoreEditorPosition <> 0) then
      begin
      if not TempBounds.Empty then
        R := TempBounds
      else
        begin
        Desktop^.GetExtent(R);
        LastEditDeskSize := Desktop^.Size
        end;
      AdjustToDesktopSize(R, LastEditDeskSize);
      end
    else
      Desktop^.GetExtent(R);
    W := New(PEditWindow, Init(R, FileName));
    if  (W <> nil) and (W^.Valid(cmValid)) then
      begin
      V := PEditWindow(W)^.Intern;
      FreeStr := V^.EditName;
      System.Insert(' ', FreeStr, 1);
      UpStr(FreeStr);
      PS := @FreeStr;
      if  (InterfaceData.Options and ouiTrackEditors <> 0)
           and (EditHistory <> nil)
      then
        I := EditHistory^.IndexOf(@PS)
      else
        I := -1;
      if I >= 0 then
        begin
        P := EditHistory^.At(I);
        R.Assign(P^.fOrigin.X, P^.fOrigin.Y, P^.fOrigin.X+P^.fSize.X,
           P^.fOrigin.Y+P^.fSize.Y);
        AdjustToDesktopSize(R, P^.fDeskSize);
        W^.Locate(R);
        with V^, P^ do
          begin
          if (InterfaceData.Options and ouiStoreEditorPosition <> 0)
          then
            begin
            Mark.A := fBlockStart;
            Mark.B := fBlockEnd;
            ScrollTo(fDelta.X, fDelta.Y);
            Pos := fPos;
            end;
          MarkPos := fMarks;
          BlockVisible := fBlockVisible;
          VertBlock := fVerticalBlock;
          EdOpt.HiLite := fHighlight;
          EdOpt.HiliteColumn := fHiliteColumn;
          EdOpt.HiliteLine := fHiliteLine;
          EdOpt.AutoIndent := fAutoIndent;
          EdOpt.AutoJustify := fAutoJustify;
          EdOpt.AutoBrackets := fAutoBrackets;
          EdOpt.LeftSide := fLeftSide;
          EdOpt.RightSide := fRightSide;
          EdOpt.InSide := fInSide;
          InsertMode := fInsMode;
          if fKeyMap <= MaxKeyMap then
            KeyMap := fKeyMap; {-$VIV}
          { Flash >>> }
          EdOpt.BackIndent := fBackIndent;
          EdOpt.AutoWrap := fAutoWrap;
          OptimalFill := fOptimalFill;
          TabReplace := fTabReplace;
          EdOpt.SmartTab := fSmartTab;
          { Flash <<< }
          WorkString := GetLine(fDelta.Y); {-$VIV 11.05.99}
          end;
        end { else StoreEditInfo(W)}; { Commented by Flash 21-12-2002 }
      InsertWindow(W);
      end;
    end;
ex:
  DisposeStr(fr^.Owner);
  DelFileRec(fr);
  end { TDNApplication.EditFile };
{-DataCompBoy-}

procedure TDNApplication.AddFormat;
  begin
  { if AddFormatDialog <> cmOK then Exit;
 New(FormatWindow,Init);
 DeskTop^.Insert(FormatWindow);}
  end;

procedure TDNApplication.InitMenuBar;
  var
    R: TRect;
  begin
  GetExtent(R);
  R.B.Y := R.A.Y;
  MenuBar := PMenuBar(LoadResource(dlgMainMenu));
  if MenuBar = nil then
    FatalError('Invalid resource file.');
  MenuBar^.Menu^.Items^.Name^:= '~'+Char(SystemMenuChar)+'~';
  MenuBar^.Locate(R);
  MenuBar^.Options := MenuBar^.Options and (not ofPreProcess)
     or ofPostProcess;
  end;

procedure TDNApplication.InitStatusLine;
  var
    R: TRect;
  begin
  GetExtent(R);
  R.A.Y := R.B.Y;
  R.Move(0, -1);
  StatusLine := PStatusLine(LoadResource(dlgStatusLine));
  StatusLine^.Locate(R);
  end;

procedure TDNApplication.InitDesktop;
  var
    R: TRect;
    WS: Word;
  begin
  WS := 0;
  HideCommandLine := (WS and cdnHideCmdLine <> 0);
  DisableCommands(DblWndCommands);
  GetExtent(R);
  if InterfaceData.Options and ouiHideMenu = 0 then
    Inc(R.A.Y);
  if InterfaceData.Options and ouiHideStatus = 0 then
    Dec(R.B.Y);
  if  (InterfaceData.Options and ouiHideCmdline = 0)
     and not HideCommandLine
  then
    Dec(R.B.Y);
  New(Desktop, Init(R));
  end;

{-DataCompBoy-}
procedure LoadPalFromFile(const FN: String);
  var
    P: LongInt;
    LoadPalette, LoadVGAPalette: Boolean;
    St: String;
    S: TDOSStream;
    Pal: PString;
  begin
  LoadPalette := False;
  LoadVGAPalette := False;
  S.Init(FN, stOpenRead);
  if S.Status = 0 then
    begin
    Pal := S.ReadStr;
    if Pal <> nil then
      begin
      St := Pal^;
      DisposeStr(Pal);
      while Length(St) < Length(CColor) do
        St := St+#$3F;
      Application^.GetPalette^:= Copy(St, 1, Length(CColor));
      LoadPalette := True;
      end;
    LoadIndexes(S);
    P := 0;
    S.Read(P, SizeOf(P));
    if P = $50414756 then
      { VGAP }
      begin
      S.Read(VGA_palette, SizeOf(VGA_palette));
      S.Read(P, SizeOf(P));
      if P = $4B4E4C42 then
        { BLNK }
        begin
        S.Read(CurrentBlink, SizeOf(CurrentBlink));
        SetBlink(CurrentBlink);
        end;
      LoadVGAPalette := True;
      if StartupData.Load and osuResetPalette <> 0 then
        SetPalette(VGA_palette);

      end;
    { else
      if Msg(dlRestoreVGAPalette, nil, mfYesNoConfirm) = cmYes then
          ResetVGAPalette( True ) ;}
    end;
  S.Done;
  WriteConfig;

  if LoadPalette then
    begin
    DoneMemory;
    Application^.Redraw;
    end;
  end { LoadPalFromFile };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDNApplication.HandleCommand;

  var
    R: TRect;
    P, P1: PView;
    ST: String;
    FileIsDBF: Boolean;
    HFile: PHelpFile;
    HelpStrm: PDosStream;
  procedure OpenWindow(ChDrive: Boolean);
    var
      S: String;
    begin
    Desktop^.GetExtent(R);
    S[1] := #0;
    if ChDrive then
      begin
      S := SelectDrive(R.A.X+(R.B.X-R.A.X) div 2, R.A.Y, #0, False);
      if S = '' then
        Exit;
      S[1] := Char(Byte(S[1])-64);
      end;
    {$IFDEF TrashCan}
    if TrashCan^.ImVisible then
      Desktop^.Delete(TrashCan);
    {$ENDIF}
    {P := nil;}
    P := New(PXDoubleWindow, Init(R, 0, Byte(S[1])));
    if Abort then
      Dispose(P, Done)
    else
      InsertWindow(PWindow(P));
    {$IFDEF TrashCan}
    if TrashCan^.ImVisible then
      Desktop^.Insert(TrashCan);
    {$ENDIF}
    end { OpenWindow };

  procedure OpenTreeWindow;
    var
      S: String;
    begin
    Desktop^.GetExtent(R);
    S := ChangeDir(GetString(dlSelectDirectory), 0);
    if S = '' then
      Exit;
    ClrIO;
    lChDir(S);
    ClrIO;
    S[1] := Char(Byte(S[1])-64);
    P := New(PXDoubleWindow, Init(R, 0, Byte(S[1])));
    Message(P, evCommand, cmDirTree, nil);
    if Abort then
      Dispose(P, Done)
    else
      InsertWindow(PWindow(P));
    end;

  function NewVideoMenu(Items: PMenuItem): PMenu;
    var
      M: PMenu;
    begin
    M := NewMenu(Items);
    while Items <> nil do
      begin
      if Items^.Command-cmSwitch = ScreenMode then
        begin
        M^.Default := Items;
        Break;
        end;
      Items := Items^.Next;
      end;
    NewVideoMenu := M;
    end;

  procedure SelectVideoModeDialog;
    var
      R: record
        VM: Integer;
        Cols, Rows: String[3];
        end;
      W: Word;
      D: PDialog;
    begin
    case ScreenMode of
      sm80x30:
        R.VM := 1;
      sm80x34:
        R.VM := 2;
      sm80x43:
        R.VM := 3;
      sm80x50:
        R.VM := 4;
      sm80x60:
        R.VM := 5;
      $140A..$FFFE:
        R.VM := 6;
      else {case}
        R.VM := 0;
    end {case};
    R.Cols := ItoS(ScreenWidth);
    R.Rows := ItoS(ScreenHeight);

    D := PDialog(LoadResource(dlgChScreenMode));
    D := PDialog(Application^.ValidView(D));
    if D = nil then
      Exit;
    D^.SetData(R);
    if opSys = opWin then
      PRadioButtons(D^.Last^.Prev)^.EnableMask := $19;
    W := Desktop^.ExecView(D);
    if W <> cmCancel then
      D^.GetData(R);
    Dispose(D, Done);
    if W <> cmOK then
      Exit;

    case R.VM of
      0:
        W := sm80x25;
      1:
        W := sm80x30;
      2:
        W := sm80x34;
      3:
        W := sm80x43;
      4:
        W := sm80x50;
      5:
        W := sm80x60;
      6:
        W := StoI(R.Cols)*256+StoI(R.Rows);
    end {case};
    {PZ begin 2000.06.29}
    SetScrMode(W);
    if StoreVideoMode <> 0 then
      begin
      case StoreVideoMode of
        1:
          smSVGALo := W;
        2:
          smSVGAHi := W;
        else {case}
          StoreVideoMode := 0;
      end {case};
      SystemData.Mode1 := ItoS(smSVGALo);
      SystemData.Mode2 := ItoS(smSVGAHi);
      Message(Application, evCommand, cmUpdateConfig, nil);
      end;
    {PZ end}
    end { SelectVideoModeDialog };

  procedure MessageBoxAbout;
    var
      D: array[1..2] of PString;
    begin
    {$IFDEF DNPRG}
    D[1] := NewStr(VersionName);
    D[2] := NewStr(#13+#3+'Compiled '+VersionDate);
    MessageBox2(dlAbout, ^C'Based on Dos Navigator'#13+
      +^C'Copyright (C) 1991-99 RIT Research Labs'#13#13+
      +^C'This product is a FREEWARE', @D, nil, mfAbout+mfOKButton);
    DisposeStr(D[1]);
    DisposeStr(D[2]);
    {$ENDIF}
    end;

  {$IFDEF SpreadSheet}
  {-DataCompBoy-}
  procedure LoadSheet(const SheetName: String);
    begin
    Desktop^.GetExtent(R);
    Desktop^.Insert(ValidView(New(PCalcWindow, Init(R, SheetName))));
    end;
  {-DataCompBoy-}

  {-DataCompBoy-}
  procedure OpenSheet;
    var
      FN: String;
    begin
    if GetFileName(FN, '*.WKZ', GetString(dlOpenFile),
           GetString(dlOpenFileName),
        fdOpenButton) = cmFileOpen
    then
      LoadSheet(FN);
    end;
  {-DataCompBoy-}
  {$ENDIF}

  var
    S: TDOSStream;
    FN: String;
    Pal: PString;
    W: Word;
    {WW: Word;
     PW: PWindow;}
    vId: LongInt;

  procedure StoreColors;
    begin
    FN := GetFileNameDialog(SourceDir+'COLORS\*.PAL',
           GetString(dlStoreColorPal), GetString(dlFileName),
        fdOKButton+fdHelpButton, hsColors);
    if FN = '' then
      Exit;
    S.Init(FN, stCreate);
    if S.Status = 0 then
      begin
      Pal := PString(GetPalette);
      S.WriteStr(Pal);
      StoreIndexes(S);
      vId := $50414756; { VGAP }
      S.Write(vId, SizeOf(vId));
      S.Write(VGA_palette, SizeOf(VGA_palette));
      vId := $4B4E4C42; { BLNK }
      S.Write(vId, SizeOf(vId));
      S.Write(CurrentBlink, SizeOf(CurrentBlink));
      end;
    {else PlaySound(1559, 30);}
    S.Done;
    if not Startup.AutoRefreshPanels then
      RereadDirectory(GetPath(FN));
    end { StoreColors };

  { procedure LoadColors;
 begin
  asm int 3 end;
  FN := GetFileNameDialog(SourceDir+'COLORS\*.PAL', GetString(dlLoadColorPal), GetString(dlFileName),
                          fdOKButton + fdHelpButton, hsColors);
  if FN = '' then Exit;
  LoadPalFromFile(FN);
 end;}

  procedure LoadColors; {JO}
    var
      More: Boolean;
      None: Boolean;
    begin
    More := True;
    None := False;
    FN := GetFileNameMenu(SourceDir+'COLORS\', '*.PAL', '', True, More,
         None);
    if More then
      FN := GetFileNameDialog(SourceDir+'COLORS\*.PAL',
             GetString(dlLoadColorPal), GetString(dlFileName),
          fdOKButton+fdHelpButton, hsColors);
    if FN = '' then
      Exit;
    LoadPalFromFile(FN);
    end; {JO}

  function TryRunSession(S: PString): Boolean;
    var
      B: Boolean;
      ST: SessionType;
    begin
    TryRunSession := True;
    DelLeft(S^);
    DelRight(S^);
    if S^ = '' then
      Exit;
    if S^ = '' then
      Exit;
    TryRunSession := False;
    if not (OS2exec or Win32exec) then
      Exit;
    case S^[1] of
      '>':
        begin
        B := False;
        ST := stOS2FullScreen
        end;
      '<':
        begin
        B := True;
        ST := stOS2FullScreen
        end;
      ']':
        begin
        B := False;
        ST := stOS2Windowed
        end;
      '[':
        begin
        B := True;
        ST := stOS2Windowed
        end;
      else {case}
        if  (CmdLine.Str <> '')
             and (PCommandLine(CommandLine)^.LineType in [ltWindow,
             ltFullScreen])
        then
          begin
          S^:= ' '+S^;
          B := ShiftState and 3 <> 0;
          if PCommandLine(CommandLine)^.LineType = ltWindow then
            ST := stOS2Windowed
          else
            ST := stOS2FullScreen
          end
        else
          Exit;
    end {case};
    TryRunSession := True;
    RunSession(Copy(S^, 2, MaxStringLength), B, ST);
    CmdLine.StrModified := True;
    Message(CommandLine, evKeyDown, kbDown, nil);
    ClearEvent(Event);
    end { TryRunSession };

  function ExecCommandLine: Boolean;
    var
      S, {$IFDEF RecodeWhenDraw}S1, {$ENDIF}SD: String; {AK155}
      up: tUserParams;
      i: Integer;
    begin
    ExecCommandLine := False;
    {S := '';}
    CommandLine^.GetData(S);

    if  (DelSpaces(S) = '') then
      begin
      SD := '';
      CommandLine^.SetData(SD);
      CommandLine^.DrawView; {JO}
      Exit;
      end;
    ExecCommandLine := True;
      (* if HandleChDirCommand then *)
      begin
      SD := S; {AK155}
      DelLeft(S);
      DelRight(S);
      if S[2] = ':' then
        S[1] := UpCase(S[1]); {/Cat}
      if  (Length(S) = 2) and (S[2] = ':') then
        begin
        if S[1] = '*' then
          S := cTEMP_
        else if not ValidDrive(S[1])  then
          Exit;
        Message(Desktop, evBroadcast, cmChangeDrv, @S);
        SD := '';
        CommandLine^.SetData(SD);
        CommandLine^.DrawView; {JO}
        Exit;
        end;
      if  (S[1] in ['c', 'C']) and (S[2] in ['d', 'D']) and (S[3] = ' ')
      then
        begin
        System.Delete(S, 1, 3); {DelFC(S); DelFC(S);}
        DelLeft(S);
        S := DelSquashes(S);
        S := lFExpand(S);
        repeat
          SD := S;
          if S[Length(S)] = '.' then
            SetLength(S, Length(S)-1);
          MakeNoSlash(S);
        until SD = S;
        if PathExist(S) then
          begin
          S[1] := UpCase(S[1]); {Cat}
            {AK155 Под NT назначение маленькой буквы проходит буквально,
              после чего в комстроке тоже индицируется маленькая буква,
              что смешно, и начинает глючить перемещение по полосе дисков,
              что уже не смешно. }
          Message(ActivePanel, evCommand, cmChangeDirectory, @S);
          end;
        SD := '';
        CommandLine^.SetData(SD);
        CommandLine^.DrawView; {JO}
        Exit;
        end;
      S := SD; {AK155 CommandLine^.GetData(S);}
      end;
    {$IFDEF DPMI32} {AK155}
    if not CheckExit then
      begin
      ClearEvent(Event);
      Exit
      end;
    {$ENDIF}
    if TryRunSession(@S) then
      Exit;
    {AK155 очистка комстроки нужна всегда, поэтому она вынесена в общую         }
    {      часть. Очищать лучше перед выполнением, а не после, так как в        }
    {      ExecString она запоминается и восстанавливается. Аналогичная вставка }
    {      сделана в dnexec.ExecFile.RunCommand                                 }
    {JO:  перенёс очистку комстроки после TryRunSession, так как иначе оно      }
    {     не срабатывает, и при запуске набранного в коммандлайне не            }
    {     получается запустить в отдельной сессии                               }
    SD := '';
    CommandLine^.SetData(SD);
    CommandLine^.DrawView;
    {$IFDEF RecodeWhenDraw}
    S1 := OemToCharStr(S);
    ExecString(S1, #13+CharToOemStr(ActiveDir)+'>'+S);
    {$ELSE}
    ExecString(S, #13+ActiveDir+'>'+S);
    {$ENDIF}
    {AK155     S:='';CommandLine^.SetData(S);CommandLine^.DrawView;}
    end { ExecCommandLine: };

  procedure ExecDOSCmd;
    var
      Nm: String;
      Xt: String;
    begin
    ST := '';
    if (ActivePanel <> nil) then
      with ActivePanel^ do
        begin
        if Files^.Count <> 0 then
          ST := PFileRec(Files^.At(ScrollBar^.Value))^.FlName[True];
        end;
    if InputBox(GetString(dlExecDOScmdHdr), GetString(dlExecDOScmdLine),
         ST, 128, hsExecDOSCmd) <> cmOK
    then
      Exit;
    if not TryRunSession(@ST) {$IFDEF DPMI32} and CheckExit {$ENDIF} {JO}
      then
      ExecString(St, #13#10+ActiveDir+'>'+ST);
    end { ExecDOSCmd };

  procedure GetUserScreen;
    var
      P: PView;
      PP: PView;
    begin
    PP := nil;
    P := ViewPresent(cmShowOutput, @PP);
    if PP <> nil then
      PP^.Select
    else
      Desktop^.Insert(New(PUserWindow, Init));
    end;

  procedure Rebound;
    begin
    GetExtent(R);
    if InterfaceData.Options and ouiHideMenu = 0 then
      Inc(R.A.Y);
    if SystemData.Options and ouiHideStatus = 0 then
      Dec(R.B.Y);
    Dec(R.B.Y);
    Desktop^.Locate(R);
    R.A.Y := R.B.Y;
    Inc(R.B.Y);
    CommandLine^.Locate(R);
    end;

  procedure RSearchAdvance;
    begin
    if ExecResource(dlgAdvanceSearch, AdvanceSearchData) = cmOK then
      begin
      PView(Desktop^.Current {Event.InfoPtr})^.GetData(FindRec);
      FindRec.Options := FindRec.Options or ffoAdvanced;
      PView(Desktop^.Current {Event.InfoPtr})^.SetData(FindRec);
      end;
    ClearEvent(Event);
    end;

  procedure DoQuickChange;
    var
      R: TRect;
      P: PMenuBox;
      Menu: PMenu;
      Items: PMenuItem;
      C: Char;
      N, Q, J: Integer;
    begin
    Items := nil;
    Q := 0;
    J := 0;
    for N := 8 downto 0 do
      if DirsToChange[N] <> nil then
        begin
        FreeStr := '~'+ItoS(N+1)+'~ '+CutH(DirsToChange[N]^, 40);
        Items := NewItem(FreeStr, 'Alt-'+ItoS(N+1), kbAlt1,
             cmQuickChange1+N,
            hcNoContext, Items);
        J := Max(CStrLen(FreeStr), J);
        Inc(Q);
        end;
    if Items = nil then
      begin
      Msg(erNoQuickDirs, nil, mfWarning+mfCancelButton);
      Exit;
      end;
    R.Assign(Application^.Size.X div 2-J div 2,
      Application^.Size.Y div 2-Q div 2,
      Application^.Size.X div 2+J div 2+J mod 2,
      Application^.Size.Y div 2+Q div 2+Q mod 2);
    Menu := NewMenu(Items);
    P := New(PMenuBox, Init(R, Menu, nil));
    P^.Options := P^.Options or ofCentered;
    P^.HelpCtx := hcQuickDirs;

    N := Application^.ExecView(P);
    Dispose(P, Done);
    DisposeMenu(Menu);
    if N >= cmQuickChange1 then
      Message(Desktop, evCommand, N, nil);
    end { DoQuickChange };

  procedure ExecTree;
    type
      TGR = record
        S: String;
        Dummy: array[0..32767] of Byte;
        end;
    var
      GR: ^TGR;
    begin
    New(GR);
    Desktop^.Current^.GetData(GR^);
    {GR^.S := lFExpand(GR^.S);}
    GR^.S := ChangeDir(GetString(dlChooseDir), 0);
    if GR^.S <> '' then
      Desktop^.Current^.SetData(GR^.S);
    Dispose(GR);
    ClearEvent(Event);
    end;

  procedure SearchAdvance;
    begin
    RSearchAdvance
    end;

  procedure ToggleCmdLine;
    var
      R: TRect;
      B: Boolean;
    begin
    if CommandLine = nil then
      Exit;
    B := (not CommandLine^.GetState(sfVisible)) and (Str <> '');
    GetExtent(R);
    if InterfaceData.Options and ouiHideMenu = 0 then
      Inc(R.A.Y);
    if InterfaceData.Options and ouiHideStatus = 0 then
      Dec(R.B.Y);
    if B then
      Dec(R.B.Y);
    Desktop^.Locate(R);
    R.A.Y := R.B.Y;
    R.B.Y := R.A.Y+Byte(B);
    CommandLine^.Locate(R);
    CommandLine^.SetState(sfVisible, B);
    end;

  procedure CE;
    begin
    ClearEvent(Event)
    end;

  procedure DoExecFile;
    begin
    if
      not TryRunSession(PString(Event.InfoPtr))
       {$IFDEF DPMI32} and CheckExit {$ENDIF} {JO}
      and
      not ExecCommandLine
    then
      ExecFile(PString(Event.InfoPtr)^);
    CE;
    end;

  procedure DoExecString;
    begin
    if not TryRunSession(PString(Event.InfoPtr))
       {$IFDEF DPMI32} and CheckExit {$ENDIF} {JO}
      then
      ExecString(PString(Event.InfoPtr)^, '');
    CE;
    end;

  procedure ChLngId;
    var
      S: String;
      SS: String[9];
      L: TStringCollection;
      SR: lSearchRec;
      Current, Default: PMenuItem;
      Menu: PMenu;
      CurIdx, i: Integer;
      HMB: PMenuBox;
      R: TRect;
      V: PView;
    function LngMixCase(P: String): String;
      var
        i: Integer;
      begin
      for i := 1 to Length(P) do
        if  (i = 1) or not (P[i-1] in ['a'..'z',
            'A'..'Z'])
        then
          P[i] := UpCase(P[i])
        else if P[i] in ['A'..'Z']
        then
          P[i] := Chr(Ord(P[i])+Ord('a')-Ord('A'));
      LngMixCase := P
      end;
    begin { ChLngId }
    L.Init(5, 5, False);
    S := SourceDir;
    MakeSlash(S);
    lFindFirst(S+'*.LNG', AnyFileDir, SR);
    while DosError = 0 do
      begin
      if  ( (SR.SR.Attr and Directory) = 0) and (SR.FullSize > 0) then
        begin
        SS[0] := #0;
        while SR.FullName[Byte(SS[0])+1] <> '.'
        do
          SS := SS+SR.FullName[Byte(SS[0])+1];
        SS := LngMixCase(SS);
        if ValidLngId(SS, False) then
          L.Insert(NewStr(SS))
        end;
      lFindNext(SR)
      end;
    lFindClose(SR);
    S := StartupDir;
    MakeSlash(S);
    lFindFirst(S+'*.LNG', AnyFileDir, SR);
    while DosError = 0 do
      begin
      if  ( (SR.SR.Attr and Directory) = 0) and (SR.FullSize > 0) then
        begin
        SS[0] := #0;
        while SR.FullName[Byte(SS[0])+1] <> '.'
        do
          SS := SS+SR.FullName[Byte(SS[0])+1];
        SS := LngMixCase(SS);
        if ValidLngId(SS, False) and (L.IndexOf(@SS) = -1) then
          L.Insert(NewStr(SS));
        end;
      lFindNext(SR)
      end;
    lFindClose(SR);
    S := LngMixCase(LngId);
    CurIdx := L.IndexOf(@S);
    if CurIdx = -1 then
      begin
      S := 'ENGLISH';
      CurIdx := L.IndexOf(@S)
      end;
    if CurIdx = -1 then
      CurIdx := 0;
    if L.Count > 0 then
      begin
      if ShowLanguageMenu then
        begin
        Current := nil;
        for i := L.Count-1 downto 0 do
          begin
          if i = 9 then
            S := '~0~ '
          else if i < 9 then
            S := '~'+Chr(Ord('1')+i)+'~ '
          else
            S := '  ';
          Current := NewItem(S+CutH(PString(L.At(i))^, 40), '', kbNoKey,
              cmCancel+1+i, hcChLngId, Current);
          if i = CurIdx then
            Default := Current
          end;
        Menu := NewMenu(Current);
        Menu^.Default := Default;
        Desktop^.GetExtent(R);
        R.A.X := ((R.A.X+R.B.X) div 2)-8;
        R.B.X := R.A.X+16;
        R.A.Y := ((R.A.Y+R.B.Y-L.Count) div 2)-1;
        R.B.Y := R.A.Y+L.Count+2;
        if R.A.Y < Desktop^.Origin.Y+1 then
          R.A.Y := Desktop^.Origin.Y+1;
        if R.B.Y > Desktop^.Origin.Y+Desktop^.Size.Y-1
        then
          R.B.Y := Desktop^.Origin.Y+Desktop^.Size.Y-1;
        New(HMB, Init(R, Menu, nil));
        CurIdx := Desktop^.ExecView(HMB)-cmCancel-1;
        Dispose(HMB, Done);
        DisposeMenu(Menu)
        end
      else
        CurIdx := (CurIdx+1) mod L.Count;
      if  (CurIdx >= 0) and (PString(L.At(CurIdx))^ <>
          LngMixCase(ActiveLanguage)) and CheckExit
      then
        begin
        ActiveLanguage := PString(L.At(CurIdx))^;
        if ActiveLanguage = LngMixCase(GetEnv('DNLNG'))
        then
          ActiveLanguage := '';
        SaveDnIniSettings(@ActiveLanguage);
        DoneIniEngine;
       {$IFNDEF DPMI32}
        RestartOnExit := True;
       {$ELSE}
        ExecString('', '');
       {$ENDIF}
        end;
      end;
    L.FreeAll;
    L.Done;
   {$IFNDEF DPMI32}
    if RestartOnExit then
      begin
      SaveDsk;
      Message(Application, evCommand, cmQuit, nil);
      end;
   {$ENDIF}
    end { ChLngId };

  begin { TDNApplication.HandleCommand }
  case Event.Command of
    cmFirstTimePanel:
      OpenWindow(False);
    cmSearchAdvance:
      SearchAdvance;
    cmListOfDirs:
      DoQuickChange;
    {$IFDEF Printer}
    cmFilePrint:
      PrintFile(PString(Event.InfoPtr)^);
    {$ENDIF}
    {$IFDEF PrintManager}
    cmSetupPrinter:
      SetupPrinter;
    {$ENDIF}
    cmOpenSmartpad:
      OpenSmartpad;
    cmOpenClipBoard:
      OpenClipBoard; {-$VOL}
    cmExecGrabber:
      ScreenGrabber(True);
    {$IFDEF MODEM}
    cmTerminalDefaults:
      if TerminalSetup and (Term <> nil) then
        Move(TerminalDefaults, Term^.Emulator, 4);
    {$ENDIF}
    cmSetupConfirmation:
      ConfirmSetup;
    cmCountrySetup:
      SetupCountryInfo;
    cmHighlightGroups:
      SetHighlightGroups;
    {$IFDEF DBView}
    cmDBFView:
      InsertWindow(New(PDBWindow, Init(CnvString(Event.InfoPtr),
             FileIsDBF)));
    {$ENDIF}
    {$IFDEF SpreadSheet}
    cmNewTable:
      LoadSheet('');
    cmSheetLoad:
      OpenSheet;
    cmWKZView:
      begin
      Desktop^.GetExtent(R);
      InsertWindow(New(PCalcWindow, Init(R, CnvString(Event.InfoPtr))));
      end;
    {$ENDIF}
    {$IFDEF PHONES}
    cmPhoneBook:
      PhoneBook(False);
    {$ENDIF}
    {$IFDEF Modem}
    {$IFDEF LINK}
    cmNavyLink:
      StartLink;
    {$ENDIF}
    {$IFDEF PHONES}
    cmUndial:
      PhoneBook(True);
    {$ENDIF}
    cmSetupModem:
      SetupModem;
    cmTerminal:
      OpenTerminal;
    cmHangUp:
      if COMPort <> nil then
        begin
        P := WriteMsg(GetString(dlDisconnect));
        HangUp;
        {$IFDEF PHONES}
        if Dialer <> nil then
          Dialer^.Free;
        {$ENDIF}
        if Term <> nil then
          Term^.Owner^.Redraw;
        if P <> nil then
          P^.Free;
        end;
    {$ENDIF}
    cmTextView, cmHexView:
      begin
      ST := CnvString(Event.InfoPtr);
      W := PosChar('|', ST);
      if W > 0
      then
        InsertWindow(New(PFileWindow,
            Init(Copy(ST, 1, W-1),
              Copy(ST, W+1, MaxStringLength),
              Event.Command = cmHexView)))
      else
        InsertWindow(New(PFileWindow,
            Init(ST, ST,
              Event.Command = cmHexView)))
      end;
    {$IFDEF MODEM}
    cmAdvancePortSetup:
      if ExecResource(dlgAdvancedCOMSetup, AdvModemData) = cmOK then
        begin
        StoreModemSetup;
        end;
    {$ENDIF}
    cmSystemSetup:
      SystemSetup;
    cmRSearchAdvance:
      RSearchAdvance;
    cmInterfaceSetup:
      InterfaceSetup;
    cmStartup:
      StartupSetup;
    cmSetupMouse:
      MouseSetup;
    {$IFDEF SS}
    cmSaversSetup:
      SaversSetup;
    {$ENDIF}
    {$IFDEF TrashCan}
    cmHideShowTools:
      if TrashCan^.ImVisible
      then
        begin
        TrashCan^.Hide;
        TrashCan^.ImVisible := False;
        end
      else
        begin
        TrashCan^.Show;
        TrashCan^.MakeFirst;
        TrashCan^.ImVisible := True;
        end;
    {$ENDIF}
    cmEditHistory:
      EditHistoryMenu;
    cmViewHistory:
      ViewHistoryMenu;
    {$IFDEF OS2}
    cmSetVolumeLabel:
      SetVLabel;
    {$ENDIF}
    cmXEditFile:
      OpenEditor;
    cmWindowManager:
      WindowManager;
    cmUserMenu, cmGlobalUserMenu:
      if  (ExecUserMenu(Event.Command = cmGlobalUserMenu)) then
        begin
        ST := FreeStr;
        {$IFDEF DPMI32}
        if CheckExit then
          {$ENDIF} {JO}
          begin
          ST := SwpDir+'$DN'+ItoS(DNNumber)+'$'+CmdExt+' '+ST;
          ExecString(ST, '');
          if RunMenu then
            begin
            Event.What := evCommand;
            Event.Command := cmUserMenu;
            PutEvent(Event);
            end;
          end;
        end;
    {$IFNDEF MINARCH}
    {$IFNDEF OS2}
    cmReadArchive:
      begin
      ReadArcList;
      end;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF SS}
    cmScreenRest:
      InsertIdler;
    {$ENDIF}
    cmChScreenMode:
      SelectVideoModeDialog;
    cmASCIITable:
      ASCIITable;
    cmAbout:
      begin
      ClearEvent(Event);
      MessageBoxAbout;
      Exit;
      end;
    cmCalculator:
      InsertCalc;
    {$IFDEF Calendar}
    cmCalendar:
      InsertCalendar; {JO}
    {$ENDIF}
    {$IFDEF Tasklist}
    cmTasklist:
      InsertTaskList; {JO}
    {$ENDIF}
    cmRefresh:
      Redraw;
    cmClearDesktop:
      GlobalMessage(evCommand, cmClose, nil);
    cmFileView:
      ViewFile(False, False, CnvString(Event.InfoPtr));
    cmFileEdit:
      EditFile(True, CnvString(Event.InfoPtr));
    {cmViewFilter: ViewFile(true, true, CnvString(Event.InfoPtr));}
    cmViewFilter:
      ViewFile(False, True, CnvString(Event.InfoPtr));
    {JO для распознания фильтрованных файлов как архивов}
    cmIntFileView:
      ViewFile(True, False, CnvString(Event.InfoPtr));
    cmIntFileEdit:
      EditFile(False, CnvString(Event.InfoPtr));
    cmEditQuickRun:
      EditFile(True, Copy(SourceDir, 1, Byte(ShiftState and 3 =
           0)*MaxStringLength)+'dn.xrn');
    cmExtFileEdit:
      EditFile(True, Copy(SourceDir, 1, Byte(ShiftState and 3 =
           0)*MaxStringLength)+'dn.ext');
    cmMenuFileEdit:
      EditFile(True, SourceDir+'dn.mnu');
    cmLocalMenuFileEdit:
      EditFile(True, 'dn.mnu');
    cmEditHGL:
      EditFile(True, SourceDir+'dn.hgl');
    cmEditSPF:
      EditFile(True, SourceDir+'dn.spf');
    cmEditINI:
      EditFile(True, SourceDir+'dn.ini');
    cmExternalViewers:
      EditFile(True, Copy(SourceDir, 1, Byte(ShiftState and 3 =
           0)*MaxStringLength)+'dn.vwr');
    cmExternalEditors:
      EditFile(True, Copy(SourceDir, 1, Byte(ShiftState and 3 =
           0)*MaxStringLength)+'dn.edt');
    cmShowUserScreen:
      ShowUserScreen;
    {$IFDEF DPMI32}
    {-DataCompBoy-}
    cmRestart:
      if not CheckExit
      then
        begin
        ClearEvent(Event);
        Exit
        end
      else
        begin
        ExecString('', '');
        end;
    {-DataCompBoy-}
    {$ENDIF}
    cmCreatePanel:
      OpenWindow(True);
    cmCreateTree:
      OpenTreeWindow;
    cmFormatDisk:
      AddFormat;
    cmShowOutput:
      GetUserScreen;
    cmHistoryList:
      CmdHistory;
    cmLoadDesk:
      RetrieveDesktop(SourceDir+'DN'+GetEnv('DNDSK')+'.DSK', nil, True);
    cmRetrieveSwp:
      ProcessTempFile(TempFileSWP);
    cmSaveDesk:
      SaveRealDsk;
    cmExecFile:
      DoExecFile;
    cmExecString:
      DoExecString;
    cmExecCommandLine:
      if ExecCommandLine then
        CE;
    cmShowTimeInfo:
      ShowTimeInfo; {DataCompBoy}
    cmChangeColors:
      ChgColors;
    cmChangeUserMode1:
      if ScreenMode <> smSVGALo then
        SetScrMode(smSVGALo)
      else
        SetScrMode(sm80x25);
    cmChangeUserMode2:
      if ScreenMode <> smSVGAHi then
        SetScrMode(smSVGAHi)
      else
        SetScrMode(sm80x25);
    cmChangeMode:
      begin
      case ScreenMode of
        3, sm80x25:
          if VideoType = vtEGA then
            SetScrMode(sm80x43)
          else
            SetScrMode(sm80x50);
        else {case}
          SetScrMode(sm80x25);
      end {case};
      end;

    cmLoadColors:
      LoadColors;
    cmSetupArchive:
      SetupArchive(DefaultArchiver);
    cmUpdateArcFile:
      UpdateARH(nil);
    cmLoConfigArchiver..cmHiConfigArchiver:
      SetupArchive(Event.Command);
    cmFMSetup:
      DoFMSetup;
    cmDriveInfoSetup:
      DriveInfoSetup;
    cmExecuteDOScmd:
      ExecDOSCmd;
    cmEditorDefaults:
      SetupEditorDefaults;
    cmStoreColors:
      StoreColors;
    {$IFDEF Game}
    cmGame:
      if Game = nil then
        InsertWindow(New(PGameWindow, Init))
      else
        Game^.Owner^.Select;
    {$ENDIF}
    {From  GetEvent}

    cmHideCmdLine:
      ToggleCmdLine;

    cmGetTeam:
      if Desktop^.TopView^.HelpCtx = hcAboutDialog then
        begin
        Desktop^.TopView^.GetExtent(R);
        R.Grow(-1, -2);
        Dec(R.B.Y, 2);
        New(TeamView, Init(R));
        PGroup(Desktop^.TopView)^.Insert(TeamView);
        Desktop^.TopView^.HelpCtx := hcTeam;
        end;
    cmQuit:
      begin
      if Confirms and cfExitConfirm <> 0 then
        W := Msg(dlQueryExit, nil, mfYesNoConfirm)
      else
        W := cmYes;
      if  (W <> cmYes) or not CheckExit then
        begin
        {$IFDEF DPMI32}
        if w95locked then
          w95QuitCancel; {Gimly}
        {$ENDIF}
        ClearEvent(Event);
        end;
      end;
    cmGetCmpNfo:
      begin
      ClearEvent(Event);
      Event.InfoPtr := WriteMsg(GetString(dlComparing));
      end;
    cmNewStrColl:
      begin
      ClearEvent(Event);
      Event.InfoPtr := New(PStringCollection, Init($80, $40, False));
      end;
    cmHelp:
      begin
      ClearEvent(Event);
      P1:=DeskTop^.Current;
      if P1 = HelpWnd then
        Exit;
        { Такое может быть только при работе граббера на окне хелпа,
         так как иначе окно хелпа (если оно есть, и Current)
         захватит cmHelp и выдаст Help on Help, а сюда управление
         просто не попадёт }
      if  (not HelpInUse) or (HelpWnd = nil) then
        begin { создаём окно хелпа }
        HelpStrm := New(PDosStream,
            Init(SourceDir+HelpLngId+'.HLP', stOpenRead {stOpenPacked})
            );
        if (HelpStrm^.Status <> stOK) and (SourceDir<> StartupDir)
        then
          begin
          Dispose(HelpStrm, Done);
          HelpStrm := New(PDosStream,
              Init(StartupDir+HelpLngId+'.HLP', stOpenRead {stOpenPacked})
              );
          end;
        if HelpStrm^.Status <> stOK then
          begin
          Dispose(HelpStrm, Done);
          Msg(erCantOpenHelp, nil, mfError+mfOKButton);
          Exit;
          end;
        HFile := New(PHelpFile, Init(HelpStrm));
        HelpWnd := PHelpWindow(ValidView(New(PHelpWindow,
                     Init(HFile, GetHelpCtx))));
        if HelpWnd = nil then
          begin
          Dispose(HFile, Done);
          exit;
          end;
        end
      else
        begin { используем существующее окно хелпа }
        Lock;
        HelpWnd^.GotoContext(GetHelpCtx);
        Desktop^.Delete(HelpWnd); { это может изменить Current}
        Desktop^.SetCurrent(P1, EnterSelect);
        Unlock;
        end;

      if (ModalCount <> 0) or MenuActive or
         Desktop^.Current^.GetState(sfDragging)
      then
        begin { исполняем и уничтожаем модальный хелп }
        ExecView(HelpWnd);
        HelpWnd^.Free;
        end
      else
        begin { вставляем немодальный хелп-долгожитель }
        Desktop^.Insert(HelpWnd);
        HelpInUse := True;
        end;
      end;
    cmTree:
      ExecTree;
    cmClearData:
      begin
      FillChar(FreeStr, SizeOf(FreeStr), 0);
      PView(Event.InfoPtr)^.Owner^.SetData(FreeStr);
      ClearEvent(Event);
      end;
    cmChLngId:
      ChLngId;
  end {case};
  end { TDNApplication.HandleCommand };
{-DataCompBoy-}

procedure ShowTimeInfo;
  var
    S: String;
  begin
  S := SecToStr(DDTimer div 1000) + '.' + Dec2(DDTimer mod 1000);
  DDTimer := 0; {JO}
  MessageBox(GetString(dlElapsedTime)+S, nil, mfOKButton+mfInformation);
  end;

{$IFDEF DPMI32}
{Gimly}
const
  w95QuitEnabled: Byte = 1;

procedure w95QuitInit;
  assembler;
asm
   MOV AL, OpSys {Check Win95}
   AND AL, opWin
   JNZ @@EnableEvent

   MOV AL, OpSys {Check WinNT and W2K}
   AND AL, opWNT
   JNZ @@EnableEvent

   XOR AL, AL
   MOV w95QuitEnabled, AL
   JMP @@E

@@EnableEvent:
{ Enablind close event handling... }
   mov ax, 168fh
   mov dx, 1
   int 2fh

@@E:
end;

function w95QuitCheck: Boolean;
  assembler;
asm
   MOV AL, w95QuitEnabled
   OR  AL, AL
   JZ  @@No

{Checking DN.INI setting...}
   MOV AL, Byte(SmartWindowsBoxClose)
   OR  AL, AL
   JZ  @@No

{ Checking the close button }
   MOV AX, 168Fh
   MOV CX, AX
   MOV DX, 100h
   INT 2fh
   CMP AX, CX
   JZ  @@No

{ Pressed - preventing the Windows message from appearing }
   MOV AX, 168Fh
   MOV DX, 200h
   INT 2fh

   MOV AL, 1
   MOV w95locked, AL

{ Just disabling close ;) }
   MOV AX, 168Fh
   MOV DX, 0h
   INT 2fh

   MOV AX, 1 { "X" button pressed }
   JMP @@E
@@No:
   XOR AX, AX
@@E:;
end;

procedure w95QuitCancel;
  begin
  asm
{ Enabling the cancel button ;) }
   mov ax, 168Fh
   mov dx, 1
   int 2fh

{ Cancelling close }
   mov ax, 168Fh
   mov dx, 300h
   int 2fh
  end;
  w95locked := False;
  end;
{Gimly}
{$ENDIF}

procedure ExecDNAutoexec;
  var
    I: Integer;
  begin
  if RunFirst then
    begin
    I := FindParam('/E');
    if I > 0 then
      ExecString(Copy(ParamStr(I), 3, MaxStringLength), '');
    end;
  end;

procedure ClearSelection(AFP: Pointer; FC: Pointer);

  procedure UnSelect(P: PFileRec);
    stdcall;
    begin
    if P^.Selected then
      Dec(PFilePanelRoot(AFP)^.SelNum);
    P^.Selected := False;
    end;

  begin
  PFilesCollection(FC)^.ForEach(@UnSelect);
  with PFilePanelRoot(AFP)^ do
    begin
    DrawView;
    if InfoView <> nil then
      InfoView^.DrawView;
    end;
  end;

{Cat: переписал для поддержки длинных строк в Clipboard-е}
procedure GetFromClip;
  begin
// fixme: commented by unxed
//  if SystemData.Options and ossUseSysClip <> 0 then
//    SyncClipOut {(true)};
  if  (Microed.ClipBoard = nil) or (Microed.ClipBoard^.At(0) = nil)
  then
    S := ''
  else
    S := PLongString(Microed.ClipBoard^.At(0))^;
  end;

procedure GetFromClipLong(var S: LongString);
  begin
// fixme: commented by unxed
//  if SystemData.Options and ossUseSysClip <> 0 then
//    SyncClipOut {(true)};
  if  (Microed.ClipBoard = nil) or (Microed.ClipBoard^.At(0) = nil)
  then
    S := ''
  else
    S := PLongString(Microed.ClipBoard^.At(0))^;
  end;

procedure PutInClip(const S: String);
  begin
  if Microed.ClipBoard <> nil then
    Dispose(Microed.ClipBoard, Done);
  Microed.ClipBoard := New(PLineCollection, Init(1, 1, True));
  Microed.ClipBoard^.Insert(NewLongStr(S));
// fixme: commented by unxed
//  if SystemData.Options and ossUseSysClip <> 0 then
//    SyncClipIn;
  if ClipBoardStream <> nil then
    ClipBoardStream^.Seek(Positive(ClipBoardStream^.GetPos-4));
// fixme: commented by unxed
//  CopyLines2Stream(Microed.ClipBoard, ClipBoardStream);
  end;

procedure PutInClipLong(const S: LongString);
  begin
  if Microed.ClipBoard <> nil then
    Dispose(Microed.ClipBoard, Done);
  Microed.ClipBoard := New(PLineCollection, Init(1, 1, True));
  Microed.ClipBoard^.Insert(NewLongStr(S));
// fixme: commented by unxed
//  if SystemData.Options and ossUseSysClip <> 0 then
//    SyncClipIn;
  if ClipBoardStream <> nil then
    ClipBoardStream^.Seek(Positive(ClipBoardStream^.GetPos-4));
// fixme: commented by unxed
//  CopyLines2Stream(Microed.ClipBoard, ClipBoardStream);
  end;
{/Cat}

end.

