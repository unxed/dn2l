unit DNFuncs;
(******

Interface with plugins - functions & objects to export
Written by Cat 2:5030/1326.13
(for use in DN/2 Plugin Version)

******)

{&Delphi+}
{&Use32+}
{$T-}

interface

uses
  Defines, Dos, RTPatch, Advance, Advance1, Advance7, Commands,
   UKeyMap,
  xTime, Objects2, Streams, Drivers, RegExp, Collect, FilesCol, FLTools,
   Lfn,
  Views, Menus, Scroller, Dialogs, Gauge, Messages, DNApp, Filelst,
   DiskImg,
  DNUtil, UniWin, Editor, EdWin, FViewer, Calculat, FlPanelX, XDblWnd,
  Drives, FileFind, ArcView, Arvid,
  Plugin, PlugRez
  , DblWnd
  , FileType
  ;

type
  PSomeObjects1 = ^TSomeObjects1;
  TSomeObjects1 = packed record
    Application: Pointer;
    Desktop: Pointer;
    StatusLine: Pointer;
    MenuBar: Pointer;
    CommandLine: Pointer;
    ResourceStream: Pointer;
    LngStream: Pointer;
    end;

  PSomeObjects2 = ^TSomeObjects2;
  TSomeObjects2 = packed record
    StartupDir: String;
    SourceDir: String;
    TempDir: String;
    TempFile: String;
    TempFileSWP: String;
    LngFile: String;
    SwpDir: String;
    end;

  PSomeObjects3 = ^TSomeObjects3;
  TSomeObjects3 = packed record
    EventCatchers: PEventCatcherArray;
    EventCatchersCount: Integer;
    ArchiveViewers: TArchiveViewerArray;
    end;

  PSimpleHooks = ^TSimpleHooks;
  TSimpleHooks = packed record
    SetEditorEventHook: function (EditorEventHook: TEditorEventHook)
    : Boolean;
    RemoveEditorEventHook: procedure (EditorEventHook: TEditorEventHook);
    end;

  PSpecialFunctions = ^TSpecialFunctions;
  TSpecialFunctions = packed record
    RuntimePatch: function (OldFunc, NewFunc: Pointer): Boolean;
    end;

  {&AlignRec+}
  PSystemVars = ^TSystemVars;
  TSystemVars = record
    ExitCode: LongInt;
    ErrorAddr: Pointer;
    ExceptionNo: LongInt;
    TlsSharedMem: Pointer;
    TlsSharedMemSize: LongInt;
    DebugHook: Boolean;
    IsConsole: Boolean;
    IsMultiThread: Boolean;
    ExitProc: Pointer;
    XcptProc: Pointer;
    ExceptProc: Pointer;
    ErrorProc: Pointer;
    SmHeapList: Pointer;
    LgHeapList: Pointer;
    HeapError: Pointer;
    Environment: Pointer;
    ExceptClsProc: Pointer;
    ExceptObjProc: Pointer;
    ExceptionClass: TClass;
    CmdLine: PChar;
    ModuleHandle: LongInt;
    RandSeed: LongInt;
    AllocMemCount: LongInt;
    AllocMemSize: LongInt;
    SmHeapBlock: LongInt;
    LgHeapBlock: LongInt;
    HeapLimit: LongInt;
    HeapAllocFlags: LongInt;
    HeapSemaphore: LongInt;
    Test8086: Byte;
    Test8087: Byte;
    end;
  {&AlignRec-}

  PDNFunctions = ^TDNFunctions;
  TDNFunctions = packed record
    DN2Version: Integer;
    APIVersion: Integer;
    Reserved1: Integer;
    SystemVars: PSystemVars;

    SomeObjects1: PSomeObjects1;
    SomeObjects2: PSomeObjects2;
    SomeObjects3: PSomeObjects3;
    Reserved2: Integer;
    Reserved3: Integer;

    SpecialFunctions: PSpecialFunctions;
    TryExcept: function (Proc: TProcedure): Pointer;
    SimpleHooks: PSimpleHooks;

    MemoryManager: TMemoryManager;

    TinySlice: procedure;

    Evalue: function (const S: String; CCV: Pointer): CReal;
    EvalueError: ^Boolean;

    DosError: function : Integer;

    lFindFirst: procedure (const Path: String; Attr: Word;
       var R: lSearchRec);
    lFindNext: procedure (var R: lSearchRec);
    lFindClose: procedure (var R: lSearchRec);

    CopyFileRec: function (fr: PFileRec): PFileRec;
    CreateFileRec: function (Name: String): PFileRec;
    NewFileRec: function (const {$IFNDEF OS2}Lfn, {$ENDIF}Name: String;
       Size: Comp; Date, CreationDate, LastAccDate: LongInt; Attr: Word;
       AOwner: PString): PFileRec;
    DelFileRec: procedure (var fr: PFileRec);
    LoadFileRec: function (var S: TStream): PFileRec;
    StoreFileRec: procedure (var S: TStream; fr: PFileRec);
    LoadFileRecOwn: function (var S: TStream; Dirs: PCollection): PFileRec;
    StoreFileRecOwn: procedure (var S: TStream; fr: PFileRec;
       Dirs: PCollection);

    GetActivePanel: function : Pointer;
    GetPassivePanel: function : Pointer;
    GetSelection: function (P: PFilePanelRoot; Single: Boolean)
    : PFilesCollection;
    ClearSelection: procedure (AFP: Pointer; FC: Pointer);

    NewStr: function (const S: String): PString;
    NewLongStr: function (const S: LongString): PLongString;
    DisposeStr: procedure (var P: PString);
    DisposeLongStr: procedure (var P: PLongString);
    CnvString: function (P: PString): String;
    CnvLongString: function (P: PLongString): LongString;

    UpStr: procedure (var S: String);
    LowStr: procedure (var S: String);

    FormatStr: procedure (var Result: String; const Format: String;
       var Params);
    MoveColor: procedure (var Buf; Num: Word; Attr: Byte);
    MoveBuf: procedure (var Dest; var Source; Attr: Byte; Count: Word);
    MoveChar: procedure (var Dest; C: Char; Attr: Byte; Count: Word);
    MoveCStr: procedure (var Dest; const Str: String; Attrs: Word);
    MoveStr: procedure (var Dest; const Str: String; Attr: Byte);
    CStrLen: function (const S: String): Integer;

    ExecView: function (P: PView): Integer;
    Reserved4: Integer;

    GetString: function (Index: TStrIdx): String;
    ExecResource: function (Key: TDlgIdx; var Data): Word;
    LoadResource: function (Key: TDlgIdx): PObject;

    OpenRez: function (const PluginName: String): LongInt;
    OpenRezX: function (const PluginName: String): LongInt;
    CloseRez: procedure (RezId: LongInt);
    GetRezString: function (RezId: LongInt; ItemId: SmallWord): String;
    GetRezObject: function (RezId: LongInt; ItemId: SmallWord): PObject;

    NewSItem: function (const Str: String; ANext: PSItem): PSItem;

    NewItem: function (Name, Param: TMenuStr; KeyCode: Word;
       Command: Word; AHelpCtx: Word; Next: PMenuItem): PMenuItem;
    NewLine: function (Next: PMenuItem): PMenuItem;
    NewSubMenu: function (Name: TMenuStr; AHelpCtx: Word;
       SubMenu: PMenu; Next: PMenuItem): PMenuItem;
    NewMenu: function (Items: PMenuItem): PMenu;
    DisposeMenu: procedure (Menu: PMenu);
    ExecAndDisposeMenu: function (Menu: PMenu): Integer;
    StoreMenuDefaults: procedure (Menu: PMenu; var S: TStream);
    LoadMenuDefaults: procedure (Menu: PMenu; var S: TStream);
    LookUpMenu: function (Menu: PMenu; idCheckItem: Word; Flags: Word)
    : PMenuItem;
    MenuIndexOf: function (Menu: PMenu; idCheckItem: PMenuItem): Word;

    NewStatusDef: function (AMin, AMax: Word; AItems: PStatusItem;
       ANext: PStatusDef): PStatusDef;
    NewStatusKey: function (const AText: String; AKeyCode: Word;
       ACommand: Word; ANext: PStatusItem): PStatusItem;

    LngId: function : String;
    HelpLngId: function : String;

    RegisterType: procedure (var S: TStreamRec);
    ReRegisterType: procedure (var S: TStreamRec);

    Message: function (Receiver: PView; What, Command: Word;
       InfoPtr: Pointer): Pointer;
    MessageL: function (Receiver: PView; What, Command: Word;
       InfoLng: LongInt): Pointer;

    RegisterToPrior: procedure (P: PView);
    RegisterToBackground: procedure (P: PView);
    Deregister: procedure (P: PView);
    UpdateAll: procedure (All: Boolean);

    GetWinNumber: function : AInt;

    MessageBox: function (Msg: String; Params: Pointer; AOptions: Word)
    : Word;
    MessageBox2: function (Msg1, Msg2: String;
       Params1, Params2: Pointer; AOptions: Word): Word;
    MessageBoxRect: function (var R: TRect; Msg: String;
       Params: Pointer; AOptions: Word): Word;
    MessageBox2Rect: function (var R: TRect; Msg1, Msg2: String;
       Lines1: Word; Params1, Params2: Pointer; AOptions: Word): Word;
    InputBox: function (Title: String; ALabel: String; var S: String;
       Limit: Word; HistoryId: Word): Word;
    BigInputBox: function (Title: String; ALabel: String; var S: String;
       Limit: Word; HistoryId: Word): Word;
    InputBoxRect: function (var Bounds: TRect; Title: String;
       ALabel: String; var S: String; Limit: Word; HistoryId: Word): Word;

    GetFileNameDialog: function (Mask, Title, Name: String;
       Buttons, HistoryId: Word): String;
    GetFileNameMenu: function (Path, Mask, Default: String;
       PutNumbers: Boolean; var More, None: Boolean): String;

    Reserved5: Integer;
    Reserved6: Integer;

    UpdateWriteView: procedure (P: Pointer);
    GlobalMessage: function (What, Command: Word; InfoPtr: Pointer)
    : Pointer;
    GlobalMessageL: function (What, Command: Word; InfoLng: LongInt)
    : Pointer;
    GlobalEvent: procedure (What, Command: Word; InfoPtr: Pointer);
    ViewPresent: function (Command: Word; InfoPtr: Pointer): PView;
    WriteMsg: function (Text: String): PView;
    ForceWriteShow: procedure (P: Pointer);
    ToggleCommandLine: procedure (OnOff: Boolean);
    AdjustToDesktopSize: procedure (var R: TRect; OldDeskSize: TPoint);

    Reserved7: Integer;
    Reserved8: Integer;

    HistoryAdd: procedure (Id: Byte; const Str: String);
    HistoryCount: function (Id: Byte): Word;
    HistoryStr: function (Id: Byte; Index: Integer): String;
    DeleteHistoryStr: procedure (Id: Byte; Index: Integer);

    Reserved9: Integer;
    Reserved10: Integer;

    GetMouseEvent: procedure (var Event: TEvent);
    GetKeyEvent: procedure (var Event: TEvent);

    DispWhileViewEvents: procedure (InfoView: PWhileView;
       var CancelParam: Boolean);

    Reserved11: Integer;

    SetTitle: procedure (Text: String);

    SetWinClip: function (PC: PLineCollection): Boolean;
    GetWinClip: function (var PCL: PLineCollection): Boolean;
    GetWinClipSize: function : Boolean;
    SyncClipIn: procedure;
    SyncClipOut: procedure;
    CopyLines2Stream: procedure (PC: PCollection; var PCS: PStream);
    CopyStream2Lines: procedure (PCS: PStream; var PC: PCollection);

    NewTimer: procedure (var ET: TEventTimer; Tics: LongInt);
    TimerExpired: function (ET: TEventTimer): Boolean;
    ElapsedTime: function (ET: TEventTimer): LongInt;

    GetPossibleDizOwner: function (n: Integer): String;
    GetDizPath: function (const Path: String; PreferedName: String): String;
    ExportDiz: procedure (const OldName: PFlName; const NewLongName: string;
                          var NewDiz: PDiz; TargetPath: string);
    DeleteDiz: procedure (FR: PFileRec);
    GetDiz: procedure (FR: PFileRec);
    SetDescription: procedure (PF: PFileRec; DizOwner: String);
    DizFirstLine: function (DIZ: PDiz): String;
    OpenFileList: function (const AConatainerPath: string): Boolean;
{   ReadFileList: procedure (ProcessDizName: TDizNameProc;
              ProcessDizLine: TDizLineProc; ProcessDizEnd: TDizEndProc);}

    Reserved12: Integer;
    Reserved13: Integer;

    SelectFiles: function (AFP: Pointer; Select, XORs: Boolean): Boolean;
    InvertSelection: procedure (AFP: Pointer; dr: Boolean);
    DragMover: procedure (AP: Pointer; Text: String; AFC, AC: Pointer);
    CM_AdvancedFilter: procedure (AFP: Pointer);
    CM_ArchiveFiles: procedure (AFP: Pointer);
    {CM_Branch: procedure(AFP: Pointer);}
    CM_ChangeDirectory: function (AFP: Pointer): String;
    CM_ChangeCase: procedure (AFP: Pointer);
    CM_CompareDirs: procedure (AFP, IP: Pointer);
    CM_CopyFiles: procedure (AFP: Pointer; MoveMode, Single: Boolean);
    CM_CopyTemp: procedure (AFP: Pointer);
    CM_DragDropper: procedure (AFP: Pointer; CurPos: Integer; EV: Pointer);
    CM_Dropped: procedure (AFP, EI: Pointer);
    CM_EraseFiles: procedure (AFP: Pointer; Single: Boolean);
    CM_LongCopy: procedure (AFP: Pointer);
    CM_MakeDir: procedure (AFP: Pointer);
    CM_MakeList: procedure (AFP: Pointer);
    CM_RenameSingleL: procedure (AFP, PEV: Pointer);
    CM_RenameSingleDialog: procedure (AFP, PEV: Pointer);
    CM_SelectColumn: procedure (AFP: Pointer);
    CM_SetAttributes: procedure (AFP: Pointer; Single: Boolean;
       CurPos: Integer);
    CM_SetShowParms: procedure (AFP: Pointer);
    CM_SortBy: procedure (AFP: Pointer);
    CM_ToggleLongNames: procedure (AFP: Pointer);
    CM_ToggleShowMode: procedure (AFP: Pointer);
    CM_ToggleDescriptions: procedure (AFP: Pointer);

    Reserved14: Integer;
    Reserved15: Integer;

    ExecString: procedure (const S: AnsiString; const WS: String);
    SearchExt: function (FileRec: PFileRec; var HS: String): Boolean;
    ExecExtFile: function (const ExtFName: String;
       UserParams: PUserParams; SIdx: TStrIdx): Boolean;
    ExecFile: procedure (const FileName: String);
    AnsiExec: procedure (const Path: String; const ComLine: AnsiString);

    Reserved16: Integer;
    Reserved17: Integer;

    SelectDrive: function (X, Y: Integer; Default: Char;
       IncludeTemp: Boolean): String;
    GetFileType: function (const S: String; Attr: Byte): Integer;

    SearchFileStr: function (F: PStream; var XLAT: TXlat;
       const What: String; Pos: LongInt;
      CaseSensitive, Display, WholeWords, Back, AllCP, IsRegExp: Boolean)
    : LongInt;
    MakeListFile: procedure (APP: Pointer; Files: PCollection);
    ASCIITable: procedure;
    InsertCalendar: procedure;
    InsertCalc: procedure;
    ChangeColors: procedure;
    WindowManager: procedure;
    SetHighlightGroups: procedure;
    UnpackDiskImages: procedure (AOwner: Pointer; Files: PFilesCollection);
    end;

function TryExcept(Proc: TProcedure): Pointer;
function DosError: Integer;
function ExecView(P: PView): Integer;
function ExecAndDisposeMenu(Menu: PMenu): Integer;
function NewStatusDef(AMin, AMax: Word; AItems: PStatusItem;
     ANext: PStatusDef): PStatusDef;
function GetActivePanel: Pointer;
function GetPassivePanel: Pointer;
function OpenRezX(const PluginName: String): LongInt;

const
  SimpleHooks: TSimpleHooks =
    (
    SetEditorEventHook: Plugin.SetEditorEventHook;
    RemoveEditorEventHook: Plugin.RemoveEditorEventHook
    );

  SpecialFunctions: TSpecialFunctions =
    (
    RuntimePatch: RTPatch.RuntimePatch
    );

  DNFunctions: TDNFunctions =
    (
    DN2Version: 0;
    APIVersion: 4;
    Reserved1: 0;
    SystemVars: PSystemVars(@System.ExitCode);

    SomeObjects1: PSomeObjects1(@DNApp.Application);
    SomeObjects2: PSomeObjects2(@Advance.StartupDir);
    SomeObjects3: PSomeObjects3(@Plugin.EventCatchers);
    Reserved2: 0;
    Reserved3: 0;
    SpecialFunctions: @SpecialFunctions;
    TryExcept: TryExcept;
    SimpleHooks: @SimpleHooks;

    MemoryManager: (
      GetMem: SysGetMem;
      FreeMem: SysFreeMem;
      ReallocMem: SysReallocMem
      );

    TinySlice: Advance.TinySlice;

    Evalue: Calculat.Evalue;
    EvalueError: @Calculat.EvalueError;

    DosError: DosError;

    lFindFirst: Lfn.lFindFirst;
    lFindNext: Lfn.lFindNext;
    lFindClose: Lfn.lFindClose;

    CopyFileRec: FilesCol.CopyFileRec;
    CreateFileRec: FilesCol.CreateFileRec;
    NewFileRec: FilesCol.NewFileRec;
    DelFileRec: FilesCol.DelFileRec;
    LoadFileRec: FilesCol.LoadFileRec;
    StoreFileRec: FilesCol.StoreFileRec;
    LoadFileRecOwn: FilesCol.LoadFileRecOwn;
    StoreFileRecOwn: FilesCol.StoreFileRecOwn;

    GetActivePanel: GetActivePanel;
    GetPassivePanel: GetPassivePanel;
    GetSelection: FLTools.GetSelection;
    ClearSelection: ClearSelection;

    NewStr: Advance1.NewStr;
    NewLongStr: Advance1.NewLongStr;
    DisposeStr: Advance1.DisposeStr;
    DisposeLongStr: Advance1.DisposeLongStr;
    CnvString: Advance1.CnvString;
    CnvLongString: Advance1.CnvLongString;

    UpStr: Advance1.UpStr;
    LowStr: Advance1.LowStr;

    FormatStr: Drivers.FormatStr;
    MoveColor: Drivers.MoveColor;
    MoveBuf: Drivers.MoveBuf;
    MoveChar: Drivers.MoveChar;
    MoveCStr: Drivers.MoveCStr;
    MoveStr: Drivers.MoveStr;
    CStrLen: Drivers.CStrLen;

    ExecView: ExecView;
    Reserved4: 0;

    GetString: DNApp.GetString;
    ExecResource: DNApp.ExecResource;
    LoadResource: DNApp.LoadResource;

    OpenRez: PlugRez.OpenRez;
    OpenRezX: OpenRezX;
    CloseRez: PlugRez.CloseRez;
    GetRezString: PlugRez.GetRezString;
    GetRezObject: PlugRez.GetRezObject;

    NewSItem: Dialogs.NewSItem;

    NewItem: Menus.NewItem;
    NewLine: Menus.NewLine;
    NewSubMenu: Menus.NewSubMenu;
    NewMenu: Menus.NewMenu;
    DisposeMenu: Menus.DisposeMenu;
    ExecAndDisposeMenu: ExecAndDisposeMenu;
    StoreMenuDefaults: Menus.StoreMenuDefaults;
    LoadMenuDefaults: Menus.LoadMenuDefaults;
    LookUpMenu: Menus.LookUpMenu;
    MenuIndexOf: Menus.MenuIndexOf;

    NewStatusDef: NewStatusDef;
    NewStatusKey: Menus.NewStatusKey;

    LngId: Advance7.LngId;
    HelpLngId: Advance7.HelpLngId;

    RegisterType: Streams.RegisterType;
    ReRegisterType: Streams.ReRegisterType;

    Message: Views.Message;
    MessageL: Views.MessageL;

    RegisterToPrior: Views.RegisterToPrior;
    RegisterToBackground: Views.RegisterToBackground;
    Deregister: Views.Deregister;
    UpdateAll: Views.UpdateAll;

    GetWinNumber: Views.GetNum;

    MessageBox: Messages.MessageBox;
    MessageBox2: Messages.MessageBox2;
    MessageBoxRect: Messages.MessageBoxRect;
    MessageBox2Rect: Messages.MessageBox2Rect;
    InputBox: Messages.InputBox;
    BigInputBox: Messages.BigInputBox;
    InputBoxRect: Messages.InputBoxRect;

    GetFileNameDialog: DNStdDlg.GetFileNameDialog;
    GetFileNameMenu: DNStdDlg.GetFileNameMenu;

    Reserved5: 0;
    Reserved6: 0;

    UpdateWriteView: DNApp.UpdateWriteView;
    GlobalMessage: DNApp.GlobalMessage;
    GlobalMessageL: DNApp.GlobalMessageL;
    GlobalEvent: DNApp.GlobalEvent;
    ViewPresent: DNApp.ViewPresent;
    WriteMsg: DNApp.WriteMsg;
    ForceWriteShow: DNApp.ForceWriteShow;
    ToggleCommandLine: DNApp.ToggleCommandLine;
    AdjustToDesktopSize: DNApp.AdjustToDesktopSize;

    Reserved7: 0;
    Reserved8: 0;

    HistoryAdd: HistList.HistoryAdd;
    HistoryCount: HistList.HistoryCount;
    HistoryStr: HistList.HistoryStr;
    DeleteHistoryStr: HistList.DeleteHistoryStr;

    Reserved9: 0;
    Reserved10: 0;

    GetMouseEvent: Drivers.GetMouseEvent;
    GetKeyEvent: Drivers.GetKeyEvent;

    DispWhileViewEvents: Gauge.DispatchEvents;

    Reserved11: 0;

    SetTitle: TitleSet.SetTitle;

    SetWinClip: WinClp.SetWinClip;
    GetWinClip: WinClp.GetWinClip;
    GetWinClipSize: WinClp.GetWinClipSize;
    SyncClipIn: WinClp.SyncClipIn;
    SyncClipOut: WinClp.SyncClipOut;
    CopyLines2Stream: WinClp.CopyLines2Stream;
    CopyStream2Lines: WinClp.CopyStream2Lines;

    NewTimer: xTime.NewTimer;
    TimerExpired: xTime.TimerExpired;
    ElapsedTime: xTime.ElapsedTime;

    GetPossibleDizOwner: Filediz.GetPossibleDizOwner;
    GetDizPath: Filediz.GetDizPath;
    ExportDiz: Filediz.ExportDiz;
    DeleteDiz: Filediz.DeleteDiz;
    GetDiz: Filediz.GetDiz;
    SetDescription: Filediz.SetDescription;
    DizFirstLine: Filediz.DizFirstLine;
    OpenFileList: Filediz.OpenFileList;
  { ReadFileList: Filediz.ReadFileList; }

    Reserved12: 0;
    Reserved13: 0;

    SelectFiles: FLTools.SelectFiles;
    InvertSelection: FLTools.InvertSelection;
    DragMover: FLTools.DragMover;
    CM_AdvancedFilter: FLTools.CM_AdvancedFilter;
    CM_ArchiveFiles: FLTools.CM_ArchiveFiles;
    {CM_Branch:             FlTools.CM_Branch;}
    CM_ChangeDirectory: FLTools.CM_ChangeDirectory;
    CM_ChangeCase: FLTools.CM_ChangeCase;
    CM_CompareDirs: FLTools.CM_CompareDirs;
    CM_CopyFiles: FLTools.CM_CopyFiles;
    CM_CopyTemp: FLTools.CM_CopyTemp;
    CM_DragDropper: FLTools.CM_DragDropper;
    CM_Dropped: FLTools.CM_Dropped;
    CM_EraseFiles: FLTools.CM_EraseFiles;
    CM_LongCopy: FLTools.CM_LongCopy;
    CM_MakeDir: FLTools.CM_MakeDir;
    CM_MakeList: FLTools.CM_MakeList;
    CM_RenameSingleL: FLTools.CM_RenameSingleL;
    CM_RenameSingleDialog: FLTools.CM_RenameSingleDialog;
    CM_SelectColumn: FLTools.CM_SelectColumn;
    CM_SetAttributes: FLTools.CM_SetAttributes;
    CM_SetShowParms: FLTools.CM_SetShowParms;
    CM_SortBy: FLTools.CM_SortBy;
    CM_ToggleLongNames: FLTools.CM_ToggleLongNames;
    CM_ToggleShowMode: FLTools.CM_ToggleShowMode;
    CM_ToggleDescriptions: FLTools.CM_ToggleDescriptions;

    Reserved14: 0;
    Reserved15: 0;

    ExecString: DnExec.ExecString;
    SearchExt: DnExec.SearchExt;
    ExecExtFile: DnExec.ExecExtFile;
    ExecFile: DnExec.ExecFile;
    AnsiExec: DnExec.AnsiExec;

    Reserved16: 0;
    Reserved17: 0;

    SelectDrive: FilesCol.SelectDrive;
    GetFileType: FileType.GetFileType;

    SearchFileStr: FViewer.SearchFileStr;
    MakeListFile: Filelst.MakeListFile;
    ASCIITable: ASCIITab.ASCIITable;
    InsertCalendar: Calendar.InsertCalendar;
    InsertCalc: CCalc.InsertCalc;
    ChangeColors: Colors.ChangeColors;
    WindowManager: Colors.WindowManager;
    SetHighlightGroups: Colors.SetHighlightGroups;
    UnpackDiskImages: DiskImg.UnpackDiskImages
    );

type
  PTEmptyObject = ^TTEmptyObject;
  TTEmptyObject = packed record
    VMT: Pointer;
    Init: Pointer;
    Free: Pointer;
    end;

  PTObject = ^TTObject;
  TTObject = packed record
    VMT: Pointer;
    Init: Pointer;
    end;

  PTRegExp = ^TTRegExp;
  TTRegExp = packed record
    VMT: Pointer;
    Init: Pointer;
    Reset: Pointer;
    CompileString: Pointer;
    CompileStr: Pointer;
    Compile: Pointer;
    Execute: Pointer;
    SubstituteString: Pointer;
    SubstituteStr: Pointer;
    Substitute: Pointer;
    end;

  PTStream = ^TTStream;
  TTStream = packed record
    VMT: Pointer;
    CopyFrom: Pointer;
    Get: Pointer;
    Put: Pointer;
    ReadStr: Pointer;
    ReadLongStr: Pointer;
    ReadStrV: Pointer;
    ReadLongStrV: Pointer;
    Reset: Pointer;
    StrRead: Pointer;
    StrWrite: Pointer;
    WriteStr: Pointer;
    WriteLongStr: Pointer;
    Eof: Pointer;
    end;

  PTDosStream = ^TTDosStream;
  TTDosStream = packed record
    VMT: Pointer;
    Init: Pointer;
    Open: Pointer;
    end;

  PTBufStream = ^TTBufStream;
  TTBufStream = packed record
    VMT: Pointer;
    Init: Pointer;
    end;

  PTMemoryStream = ^TTMemoryStream;
  TTMemoryStream = packed record
    VMT: Pointer;
    Init: Pointer;
    end;

  PTCollection = ^TTCollection;
  TTCollection = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    At: Pointer;
    AtDelete: Pointer;
    AtFree: Pointer;
    AtInsert: Pointer;
    AtPut: Pointer;
    AtReplace: Pointer;
    Delete: Pointer;
    DeleteAll: Pointer;
    FirstThat: Pointer;
    ForEach: Pointer;
    Free: Pointer;
    FreeAll: Pointer;
    LastThat: Pointer;
    Pack: Pointer;
    Store: Pointer;
    end;

  PTSortedCollection = ^TTSortedCollection;
  TTSortedCollection = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    Sort: Pointer;
    end;

  PTLineCollection = ^TTLineCollection;
  TTLineCollection = packed record
    VMT: Pointer;
    Init: Pointer;
    end;

  PTStringCollection = ^TTStringCollection;
  TTStringCollection = packed record
    VMT: Pointer;
    Init: Pointer;
    end;

  PTStrCollection = ^TTStrCollection;
  TTStrCollection = packed record
    VMT: Pointer;
    Init: Pointer;
    end;

  PTFilesCollection = ^TTFilesCollection;
  TTFilesCollection = packed record
    VMT: Pointer;
    Load: Pointer;
    end;

  PTView = ^TTView;
  TTView = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    BlockCursor: Pointer;
    ClearEvent: Pointer;
    CommandEnabled: Pointer;
    DisableCommands: Pointer;
    DragView: Pointer;
    DrawView: Pointer;
    EnableCommands: Pointer;
    EventAvail: Pointer;
    Exposed: Pointer;
    Focus: Pointer;
    GetBounds: Pointer;
    GetClipRect: Pointer;
    GetColor: Pointer;
    GetCommands: Pointer;
    GetExtent: Pointer;
    GetPeerViewPtr: Pointer;
    GetState: Pointer;
    GrowTo: Pointer;
    Hide: Pointer;
    HideCursor: Pointer;
    KeyEvent: Pointer;
    Locate: Pointer;
    MakeFirst: Pointer;
    MakeGlobal: Pointer;
    MakeLocal: Pointer;
    MouseEvent: Pointer;
    MouseInView: Pointer;
    MoveTo: Pointer;
    NextView: Pointer;
    NormalCursor: Pointer;
    Prev: Pointer;
    PrevView: Pointer;
    PutInFrontOf: Pointer;
    PutPeerViewPtr: Pointer;
    Select: Pointer;
    SetBounds: Pointer;
    SetCommands: Pointer;
    SetCursor: Pointer;
    Show: Pointer;
    ShowCursor: Pointer;
    Store: Pointer;
    TopView: Pointer;
    WriteBuf: Pointer;
    WriteChar: Pointer;
    WriteLine: Pointer;
    WriteStr: Pointer;
    MenuEnabled: Pointer;
    DrawCursor: Pointer;
    DrawHide: Pointer;
    DrawShow: Pointer;
    DrawUnderRect: Pointer;
    DrawUnderView: Pointer;
    end;

  PTFrame = ^TTFrame;
  TTFrame = packed record
    VMT: Pointer;
    Init: Pointer;
    end;

  PTScrollBar = ^TTScrollBar;
  TTScrollBar = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    SetParams: Pointer;
    SetRange: Pointer;
    SetStep: Pointer;
    SetValue: Pointer;
    Store: Pointer;
    end;

  PTGroup = ^TTGroup;
  TTGroup = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Delete: Pointer;
    ExecView: Pointer;
    First: Pointer;
    FirstThat: Pointer;
    FocusNext: Pointer;
    ForEach: Pointer;
    GetSubViewPtr: Pointer;
    Insert: Pointer;
    InsertBefore: Pointer;
    Lock: Pointer;
    PutSubViewPtr: Pointer;
    SelectNext: Pointer;
    Store: Pointer;
    UnLock: Pointer;
    FreeBuffer: Pointer;
    GetBuffer: Pointer;
    InsertView: Pointer;
    SetCurrent: Pointer;
    end;

  PTWindow = ^TTWindow;
  TTWindow = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    StandardScrollBar: Pointer;
    Store: Pointer;
    end;

  PTMenuView = ^TTMenuView;
  TTMenuView = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    FindItem: Pointer;
    HotKey: Pointer;
    Store: Pointer;
    end;

  PTMenuBar = ^TTMenuBar;
  TTMenuBar = packed record
    VMT: Pointer;
    Init: Pointer;
    end;

  PTMenuBox = ^TTMenuBox;
  TTMenuBox = packed record
    VMT: Pointer;
    Init: Pointer;
    end;

  PTMenuPopup = ^TTMenuPopup;
  TTMenuPopup = packed record
    VMT: Pointer;
    Init: Pointer;
    end;

  PTStatusLine = ^TTStatusLine;
  TTStatusLine = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    end;

  PTDialog = ^TTDialog;
  TTDialog = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    end;

  PTInputLine = ^TTInputLine;
  TTInputLine = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    SelectAll: Pointer;
    SetValidator: Pointer;
    Store: Pointer;
    CanScroll: Pointer;
    end;

  PTButton = ^TTButton;
  TTButton = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    DrawState: Pointer;
    MakeDefault: Pointer;
    Store: Pointer;
    end;

  PTCluster = ^TTCluster;
  TTCluster = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    ButtonState: Pointer;
    DrawBox: Pointer;
    DrawMultiBox: Pointer;
    SetButtonState: Pointer;
    Store: Pointer;
    end;

  PTRadioButtons = ^TTRadioButtons;
  TTRadioButtons = packed record
    VMT: Pointer;
    end;

  PTCheckBoxes = ^TTCheckBoxes;
  TTCheckBoxes = packed record
    VMT: Pointer;
    end;

  PTMultiCheckBoxes = ^TTMultiCheckBoxes;
  TTMultiCheckBoxes = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    end;

  PTComboBox = ^TTComboBox;
  TTComboBox = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    end;

  PTScroller = ^TTScroller;
  TTScroller = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    ScrollTo: Pointer;
    SetLimit: Pointer;
    Store: Pointer;
    end;

  PTListViewer = ^TTListViewer;
  TTListViewer = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    SetRange: Pointer;
    Store: Pointer;
    end;

  PTListBox = ^TTListBox;
  TTListBox = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    end;

  PTStaticText = ^TTStaticText;
  TTStaticText = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    end;

  PTParamText = ^TTParamText;
  TTParamText = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    end;

  PTLabel = ^TTLabel;
  TTLabel = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    end;

  PTHistoryViewer = ^TTHistoryViewer;
  TTHistoryViewer = packed record
    VMT: Pointer;
    Init: Pointer;
    HistoryWidth: Pointer;
    end;

  PTHistoryWindow = ^TTHistoryWindow;
  TTHistoryWindow = packed record
    VMT: Pointer;
    Init: Pointer;
    end;

  PTHistory = ^TTHistory;
  TTHistory = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    end;

  PTBackground = ^TTBackground;
  TTBackground = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    end;

  PTDesktop = ^TTDesktop;
  TTDesktop = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Cascade: Pointer;
    Clear: Pointer;
    Store: Pointer;
    Tile: Pointer;
    end;

  PTProgram = ^TTProgram;
  TTProgram = packed record
    VMT: Pointer;
    Init: Pointer;
    CanMoveFocus: Pointer;
    ExecuteDialog: Pointer;
    InsertWindow: Pointer;
    ActivateView: Pointer;
    SetScreenMode: Pointer;
    ValidView: Pointer;
    end;

  PTApplication = ^TTApplication;
  TTApplication = packed record
    VMT: Pointer;
    Init: Pointer;
    Cascade: Pointer;
    ShowUserScreen: Pointer;
    Tile: Pointer;
    end;

  PTDNApplication = ^TTDNApplication;
  TTDNApplication = packed record
    VMT: Pointer;
    Init: Pointer;
    ViewFile: Pointer;
    AddFormat: Pointer;
    EditFile: Pointer;
    RetrieveDesktop: Pointer;
    SaveDesktop: Pointer;
    LoadDesktop: Pointer;
    StoreDesktop: Pointer;
    ChgColors: Pointer;
    end;

  PTUniWindow = ^TTUniWindow;
  TTUniWindow = packed record
    VMT: Pointer;
    MakeScrollBar: Pointer;
    end;

  PTXFileEditor = ^TTXFileEditor;
  TTXFileEditor = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    DoHighlite: Pointer;
    GetLine: Pointer;
    GetSelection: Pointer;
    ValidBlock: Pointer;
    CalcMenu: Pointer;
    Search: Pointer;
    InsertBlock: Pointer;
    ModifyLine: Pointer;
    SetLimits: Pointer;
    ScrollTo: Pointer;
    LimitX: Pointer;
    LimitY: Pointer;
    StoreUndoInfo: Pointer;
    KeyMapAtInsert: Pointer;
    KeyMapAtReplace: Pointer;
    end;

  PTEditWindow = ^TTEditWindow;
  TTEditWindow = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    end;

  PTPercentGauge = ^TTPercentGauge;
  TTPercentGauge = packed record
    VMT: Pointer;
    Init: Pointer;
    AddProgress: Pointer;
    SolveForX: Pointer;
    SolveForY: Pointer;
    end;

  PTBarGauge = ^TTBarGauge;
  TTBarGauge = packed record
    VMT: Pointer;
    end;

  PTWhileView = ^TTWhileView;
  TTWhileView = packed record
    VMT: Pointer;
    Init: Pointer;
    Write: Pointer;
    ClearInterior: Pointer;
    end;

  PTViewScroll = ^TTViewScroll;
  TTViewScroll = packed record
    VMT: Pointer;
    GetPartCode: Pointer;
    GetSize: Pointer;
    DrawPos: Pointer;
    end;

  PTFileViewer = ^TTFileViewer;
  TTFileViewer = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    ReadFile: Pointer;
    WriteModify: Pointer;
    Seek: Pointer;
    SaveToFile: Pointer;
    DoHighlite: Pointer;
    SeekEof: Pointer;
    SeekBof: Pointer;
    BreakOnStreamReadError: Pointer;
    end;

  PTDrive = ^TTDrive;
  TTDrive = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    end;

  PTFindDrive = ^TTFindDrive;
  TTFindDrive = packed record
    VMT: Pointer;
    Init: Pointer;
    InitList: Pointer;
    Load: Pointer;
    NewUpFile: Pointer;
    end;

  PTTempDrive = ^TTTempDrive;
  TTTempDrive = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    end;

  PTArcDrive = ^TTArcDrive;
  TTArcDrive = packed record
    VMT: Pointer;
    Init: Pointer;
    InitCol: Pointer;
    Load: Pointer;
    ReadArchive: Pointer;
    Exec: Pointer;
    MakeListFile: Pointer;
    ExtractFiles: Pointer;
    StdMsg4: Pointer;
    end;

  PTArvidDrive = ^TTArvidDrive;
  TTArvidDrive = packed record
    VMT: Pointer;
    Init: Pointer;
    SeekDirectory: Pointer;
    end;

  PDNMethods = ^TDNMethods;
  TDNMethods = packed record
    RecordSize: Integer;
    Reserved1: Integer;
    Reserved2: Integer;
    _TEmptyObject: PTEmptyObject;
    _TObject: PTObject;
    _TRegExp: PTRegExp;
    _T1: Pointer;
    _T2: Pointer;
    _T3: Pointer;
    _TStream: PTStream;
    _TDosStream: PTDosStream;
    _TBufStream: PTBufStream;
    _TMemoryStream: PTMemoryStream;
    _TCollection: PTCollection;
    _TSortedCollection: PTSortedCollection;
    _TLineCollection: PTLineCollection;
    _TStringCollection: PTStringCollection;
    _TStrCollection: PTStrCollection;
    _TFilesCollection: PTFilesCollection;
    _TView: PTView;
    _TFrame: PTFrame;
    _TScrollBar: PTScrollBar;
    _TGroup: PTGroup;
    _TWindow: PTWindow;
    _TMenuView: PTMenuView;
    _TMenuBar: PTMenuBar;
    _TMenuBox: PTMenuBox;
    _TMenuPopup: PTMenuPopup;
    _TStatusLine: PTStatusLine;
    _TDialog: PTDialog;
    _TInputLine: PTInputLine;
    _TButton: PTButton;
    _TCluster: PTCluster;
    _TRadioButtons: PTRadioButtons;
    _TCheckBoxes: PTCheckBoxes;
    _TMultiCheckBoxes: PTMultiCheckBoxes;
    _TComboBox: PTComboBox;
    _TScroller: PTScroller;
    _TListViewer: PTListViewer;
    _TListBox: PTListBox;
    _TStaticText: PTStaticText;
    _TParamText: PTParamText;
    _TLabel: PTLabel;
    _THistoryViewer: PTHistoryViewer;
    _THistoryWindow: PTHistoryWindow;
    _THistory: PTHistory;
    _TBackground: PTBackground;
    _TDesktop: PTDesktop;
    _TProgram: PTProgram;
    _TApplication: PTApplication;
    _TDNApplication: PTDNApplication;
    _TUniWindow: PTUniWindow;
    _TXFileEditor: PTXFileEditor;
    _TEditWindow: PTEditWindow;
    _TPercentGauge: PTPercentGauge;
    _TBarGauge: PTBarGauge;
    _TWhileView: PTWhileView;
    _TViewScroll: PTViewScroll;
    _TFileViewer: PTFileViewer;
    _TDrive: PTDrive;
    _TFindDrive: PTFindDrive;
    _TTempDrive: PTTempDrive;
    _TArcDrive: PTArcDrive;
    _TArvidDrive: PTArvidDrive
    end;

const
  _TEmptyObject: TTEmptyObject =
    (
    VMT: TypeOf(TEmptyObject);
    Init: @TEmptyObject.Init;
    Free: @TEmptyObject.Free
    );

  _TObject: TTObject =
    (
    VMT: TypeOf(TObject);
    Init: @TObject.Init
    );

  _TRegExp: TTRegExp =
    (
    VMT: TypeOf(TRegExp);
    Init: @TRegExp.Init;
    Reset: @TRegExp.Reset;
    CompileString: @TRegExp.CompileString;
    CompileStr: @TRegExp.CompileStr;
    Compile: @TRegExp.Compile;
    Execute: @TRegExp.Execute;
    SubstituteString: @TRegExp.SubstituteString;
    SubstituteStr: @TRegExp.SubstituteStr;
    Substitute: @TRegExp.Substitute
    );

  _TStream: TTStream =
    (
    VMT: TypeOf(TStream);
    CopyFrom: @TStream.CopyFrom;
    Get: @TStream.Get;
    Put: @TStream.Put;
    ReadStr: @TStream.ReadStr;
    ReadLongStr: @TStream.ReadLongStr;
    ReadStrV: @TStream.ReadStrV;
    ReadLongStrV: @TStream.ReadLongStrV;
    Reset: @TStream.Reset;
    StrRead: @TStream.StrRead;
    StrWrite: @TStream.StrWrite;
    WriteStr: @TStream.WriteStr;
    WriteLongStr: @TStream.WriteLongStr;
    Eof: @TStream.Eof
    );

  _TDosStream: TTDosStream =
    (
    VMT: TypeOf(TDOSStream);
    Init: @TDosStream.Init;
    Open: @TDosStream.Open
    );

  _TBufStream: TTBufStream =
    (
    VMT: TypeOf(TBufStream);
    Init: @TBufStream.Init
    );

  _TMemoryStream: TTMemoryStream =
    (
    VMT: TypeOf(TMemoryStream);
    Init: @TMemoryStream.Init
    );

  _TCollection: TTCollection =
    (
    VMT: TypeOf(TCollection);
    Init: @TCollection.Init;
    Load: @TCollection.Load;
    At: @TCollection.At;
    AtDelete: @TCollection.AtDelete;
    AtFree: @TCollection.AtFree;
    AtInsert: @TCollection.AtInsert;
    AtPut: @TCollection.AtPut;
    AtReplace: @TCollection.AtReplace;
    Delete: @TCollection.Delete;
    DeleteAll: @TCollection.DeleteAll;
    FirstThat: @TCollection.FirstThat;
    ForEach: @TCollection.ForEach;
    Free: @TCollection.Free;
    FreeAll: @TCollection.FreeAll;
    LastThat: @TCollection.LastThat;
    Pack: @TCollection.Pack;
    Store: @TCollection.Store
    );

  _TSortedCollection: TTSortedCollection =
    (
    VMT: TypeOf(TSortedCollection);
    Init: @TSortedCollection.Init;
    Load: @TSortedCollection.Load;
    Store: @TSortedCollection.Store;
    Sort: @TSortedCollection.Sort
    );

  _TLineCollection: TTLineCollection =
    (
    VMT: TypeOf(TLineCollection);
    Init: @TLineCollection.Init
    );

  _TStringCollection: TTStringCollection =
    (
    VMT: TypeOf(TStringCollection);
    Init: @TStringCollection.Init
    );

  _TStrCollection: TTStrCollection =
    (
    VMT: TypeOf(TStrCollection);
    Init: @TStrCollection.Init
    );

  _TFilesCollection: TTFilesCollection =
    (
    VMT: TypeOf(TFilesCollection);
    Load: @TFilesCollection.Load
    );

  _TView: TTView =
    (
    VMT: TypeOf(TView);
    Init: @TView.Init;
    Load: @TView.Load;
    BlockCursor: @TView.BlockCursor;
    ClearEvent: @TView.ClearEvent;
    CommandEnabled: @TView.CommandEnabled;
    DisableCommands: @TView.DisableCommands;
    DragView: @TView.DragView;
    DrawView: @TView.DrawView;
    EnableCommands: @TView.EnableCommands;
    EventAvail: @TView.EventAvail;
    Exposed: @TView.Exposed;
    Focus: @TView.Focus;
    GetBounds: @TView.GetBounds;
    GetClipRect: @TView.GetClipRect;
    GetColor: @TView.GetColor;
    GetCommands: @TView.GetCommands;
    GetExtent: @TView.GetExtent;
    GetPeerViewPtr: @TView.GetPeerViewPtr;
    GetState: @TView.GetState;
    GrowTo: @TView.GrowTo;
    Hide: @TView.Hide;
    HideCursor: @TView.HideCursor;
    KeyEvent: @TView.KeyEvent;
    Locate: @TView.Locate;
    MakeFirst: @TView.MakeFirst;
    MakeGlobal: @TView.MakeGlobal;
    MakeLocal: @TView.MakeLocal;
    MouseEvent: @TView.MouseEvent;
    MouseInView: @TView.MouseInView;
    MoveTo: @TView.MoveTo;
    NextView: @TView.NextView;
    NormalCursor: @TView.NormalCursor;
    Prev: @TView.Prev;
    PrevView: @TView.PrevView;
    PutInFrontOf: @TView.PutInFrontOf;
    PutPeerViewPtr: @TView.PutPeerViewPtr;
    Select: @TView.Select;
    SetBounds: @TView.SetBounds;
    SetCommands: @TView.SetCommands;
    SetCursor: @TView.SetCursor;
    Show: @TView.Show;
    ShowCursor: @TView.ShowCursor;
    Store: @TView.Store;
    TopView: @TView.TopView;
    WriteBuf: @TView.WriteBuf;
    WriteChar: @TView.WriteChar;
    WriteLine: @TView.WriteLine;
    WriteStr: @TView.WriteStr;
    MenuEnabled: @TView.MenuEnabled;
    DrawCursor: @TView.DrawCursor;
    DrawHide: @TView.DrawHide;
    DrawShow: @TView.DrawShow;
    DrawUnderRect: @TView.DrawUnderRect;
    DrawUnderView: @TView.DrawUnderView
    );

  _TFrame: TTFrame =
    (
    VMT: TypeOf(TFrame);
    Init: @TFrame.Init
    );

  _TScrollBar: TTScrollBar =
    (
    VMT: TypeOf(TScrollBar);
    Init: @TScrollBar.Init;
    Load: @TScrollBar.Load;
    SetParams: @TScrollBar.SetParams;
    SetRange: @TScrollBar.SetRange;
    SetStep: @TScrollBar.SetStep;
    SetValue: @TScrollBar.SetValue;
    Store: @TScrollBar.Store
    );

  _TGroup: TTGroup =
    (
    VMT: TypeOf(TGroup);
    Init: @TGroup.Init;
    Load: @TGroup.Load;
    Delete: @TGroup.Delete;
    ExecView: @TGroup.ExecView;
    First: @TGroup.First;
    FirstThat: @TGroup.FirstThat;
    FocusNext: @TGroup.FocusNext;
    ForEach: @TGroup.ForEach;
    GetSubViewPtr: @TGroup.GetSubViewPtr;
    Insert: @TGroup.Insert;
    InsertBefore: @TGroup.InsertBefore;
    Lock: @TGroup.Lock;
    PutSubViewPtr: @TGroup.PutSubViewPtr;
    SelectNext: @TGroup.SelectNext;
    Store: @TGroup.Store;
    UnLock: @TGroup.UnLock;
    FreeBuffer: @TGroup.FreeBuffer;
    GetBuffer: @TGroup.GetBuffer;
    InsertView: @TGroup.InsertView;
    SetCurrent: @TGroup.SetCurrent
    );

  _TWindow: TTWindow =
    (
    VMT: TypeOf(TWindow);
    Init: @TWindow.Init;
    Load: @TWindow.Load;
    StandardScrollBar: @TWindow.StandardScrollBar;
    Store: @TWindow.Store
    );

  _TMenuView: TTMenuView =
    (
    VMT: TypeOf(TMenuView);
    Init: @TMenuView.Init;
    Load: @TMenuView.Load;
    FindItem: @TMenuView.FindItem;
    HotKey: @TMenuView.HotKey;
    Store: @TMenuView.Store
    );

  _TMenuBar: TTMenuBar =
    (
    VMT: TypeOf(TMenuBar);
    Init: @TMenuBar.Init
    );

  _TMenuBox: TTMenuBox =
    (
    VMT: TypeOf(TMenuBox);
    Init: @TMenuBox.Init
    );

  _TMenuPopup: TTMenuPopup =
    (
    VMT: TypeOf(TMenuPopup);
    Init: @TMenuPopup.Init
    );

  _TStatusLine: TTStatusLine =
    (
    VMT: TypeOf(TStatusLine);
    Init: @TStatusLine.Init;
    Load: @TStatusLine.Load;
    Store: @TStatusLine.Store
    );

  _TDialog: TTDialog =
    (
    VMT: TypeOf(TDialog);
    Init: @TDialog.Init;
    Load: @TDialog.Load;
    Store: @TDialog.Store
    );

  _TInputLine: TTInputLine =
    (
    VMT: TypeOf(TInputLine);
    Init: @TInputLine.Init;
    Load: @TInputLine.Load;
    SelectAll: @TInputLine.SelectAll;
    SetValidator: @TInputLine.SetValidator;
    Store: @TInputLine.Store;
    CanScroll: @TInputLine.CanScroll
    );

  _TButton: TTButton =
    (
    VMT: TypeOf(TButton);
    Init: @TButton.Init;
    Load: @TButton.Load;
    DrawState: @TButton.DrawState;
    MakeDefault: @TButton.MakeDefault;
    Store: @TButton.Store
    );

  _TCluster: TTCluster =
    (
    VMT: TypeOf(TCluster);
    Init: @TCluster.Init;
    Load: @TCluster.Load;
    ButtonState: @TCluster.ButtonState;
    DrawBox: @TCluster.DrawBox;
    DrawMultiBox: @TCluster.DrawMultiBox;
    SetButtonState: @TCluster.SetButtonState;
    Store: @TCluster.Store
    );

  _TRadioButtons: TTRadioButtons =
    (
    VMT: TypeOf(TRadioButtons)
    );

  _TCheckBoxes: TTCheckBoxes =
    (
    VMT: TypeOf(TCheckBoxes)
    );

  _TMultiCheckBoxes: TTMultiCheckBoxes =
    (
    VMT: TypeOf(TMultiCheckBoxes);
    Init: @TMultiCheckBoxes.Init;
    Load: @TMultiCheckBoxes.Load;
    Store: @TMultiCheckBoxes.Store
    );

  _TComboBox: TTComboBox =
    (
    VMT: TypeOf(TComboBox);
    Init: @TComboBox.Init;
    Load: @TComboBox.Load;
    Store: @TComboBox.Store
    );

  _TScroller: TTScroller =
    (
    VMT: TypeOf(TScroller);
    Init: @TScroller.Init;
    Load: @TScroller.Load;
    ScrollTo: @TScroller.ScrollTo;
    SetLimit: @TScroller.SetLimit;
    Store: @TScroller.Store
    );

  _TListViewer: TTListViewer =
    (
    VMT: TypeOf(TListViewer);
    Init: @TListViewer.Init;
    Load: @TListViewer.Load;
    SetRange: @TListViewer.SetRange;
    Store: @TListViewer.Store
    );

  _TListBox: TTListBox =
    (
    VMT: TypeOf(TListBox);
    Init: @TListBox.Init;
    Load: @TListBox.Load;
    Store: @TListBox.Store
    );

  _TStaticText: TTStaticText =
    (
    VMT: TypeOf(TStaticText);
    Init: @TStaticText.Init;
    Load: @TStaticText.Load;
    Store: @TStaticText.Store
    );

  _TParamText: TTParamText =
    (
    VMT: TypeOf(TParamText);
    Init: @TParamText.Init;
    Load: @TParamText.Load;
    Store: @TParamText.Store
    );

  _TLabel: TTLabel =
    (
    VMT: TypeOf(TLabel);
    Init: @TLabel.Init;
    Load: @TLabel.Load;
    Store: @TLabel.Store
    );

  _THistoryViewer: TTHistoryViewer =
    (
    VMT: TypeOf(THistoryViewer);
    Init: @THistoryViewer.Init;
    HistoryWidth: @THistoryViewer.HistoryWidth
    );

  _THistoryWindow: TTHistoryWindow =
    (
    VMT: TypeOf(THistoryWindow);
    Init: @THistoryWindow.Init
    );

  _THistory: TTHistory =
    (
    VMT: TypeOf(THistory);
    Init: @THistory.Init;
    Load: @THistory.Load;
    Store: @THistory.Store
    );

  _TBackground: TTBackground =
    (
    VMT: TypeOf(TBackground);
    Init: @TBackground.Init;
    Load: @TBackground.Load;
    Store: @TBackground.Store
    );

  _TDesktop: TTDesktop =
    (
    VMT: TypeOf(TDesktop);
    Init: @TDesktop.Init;
    Load: @TDesktop.Load;
    Cascade: @TDesktop.Cascade;
    Clear: @TDesktop.Clear;
    Store: @TDesktop.Store;
    Tile: @TDesktop.Tile
    );

  _TProgram: TTProgram =
    (
    VMT: TypeOf(TProgram);
    Init: @TProgram.Init;
    CanMoveFocus: @TProgram.CanMoveFocus;
    ExecuteDialog: @TProgram.ExecuteDialog;
    InsertWindow: @TProgram.InsertWindow;
    ActivateView: @TProgram.ActivateView;
    SetScreenMode: @TProgram.SetScreenMode;
    ValidView: @TProgram.ValidView
    );

  _TApplication: TTApplication =
    (
    VMT: TypeOf(TApplication);
    Init: @TApplication.Init;
    Cascade: @TApplication.Cascade;
    ShowUserScreen: @TApplication.ShowUserScreen;
    Tile: @TApplication.Tile
    );

  _TDNApplication: TTDNApplication =
    (
    VMT: TypeOf(TDNApplication);
    Init: @TDNApplication.Init;
    ViewFile: @TDNApplication.ViewFile;
    AddFormat: @TDNApplication.AddFormat;
    EditFile: @TDNApplication.EditFile;
    RetrieveDesktop: @TDNApplication.RetrieveDesktop;
    SaveDesktop: @TDNApplication.SaveDesktop;
    LoadDesktop: @TDNApplication.LoadDesktop;
    StoreDesktop: @TDNApplication.StoreDesktop;
    ChgColors: @TDNApplication.ChgColors
    );

  _TUniWindow: TTUniWindow =
    (
    VMT: TypeOf(TUniWindow);
    MakeScrollBar: @TUniWindow.MakeScrollBar
    );

  _TXFileEditor: TTXFileEditor =
    (
    VMT: TypeOf(TXFileEditor);
    Init: @TXFileEditor.Init;
    Load: @TXFileEditor.Load;
    Store: @TXFileEditor.Store;
    DoHighlite: @TXFileEditor.DoHighlite;
    GetLine: @TXFileEditor.GetLine;
    GetSelection: @TXFileEditor.GetSelection;
    ValidBlock: @TXFileEditor.ValidBlock;
    CalcMenu: @TXFileEditor.CalcMenu;
    Search: @TXFileEditor.Search;
    InsertBlock: @TXFileEditor.InsertBlock;
    ModifyLine: @TXFileEditor.ModifyLine;
    SetLimits: @TXFileEditor.SetLimits;
    ScrollTo: @TXFileEditor.ScrollTo;
    LimitX: @TXFileEditor.LimitX;
    LimitY: @TXFileEditor.LimitY;
    StoreUndoInfo: @TXFileEditor.StoreUndoInfo;
    KeyMapAtInsert: @TXFileEditor.KeyMapAtInsert;
    KeyMapAtReplace: @TXFileEditor.KeyMapAtReplace
    );

  _TEditWindow: TTEditWindow =
    (
    VMT: TypeOf(TEditWindow);
    Init: @TEditWindow.Init;
    Load: @TEditWindow.Load;
    Store: @TEditWindow.Store
    );

  _TPercentGauge: TTPercentGauge =
    (
    VMT: TypeOf(TPercentGauge);
    Init: @TPercentGauge.Init;
    AddProgress: @TPercentGauge.AddProgress;
    SolveForX: @TPercentGauge.SolveForX;
    SolveForY: @TPercentGauge.SolveForY
    );

  _TBarGauge: TTBarGauge =
    (
    VMT: TypeOf(TBarGauge)
    );

  _TWhileView: TTWhileView =
    (
    VMT: TypeOf(TWhileView);
    Init: @TWhileView.Init;
    Write: @TWhileView.Write;
    ClearInterior: @TWhileView.ClearInterior
    );

  _TViewScroll: TTViewScroll =
    (
    VMT: TypeOf(TViewScroll);
    GetPartCode: @TViewScroll.GetPartCode;
    GetSize: @TViewScroll.GetSize;
    DrawPos: @TViewScroll.DrawPos
    );

  _TFileViewer: TTFileViewer =
    (
    VMT: TypeOf(TFileViewer);
    Init: @TFileViewer.Init;
    Load: @TFileViewer.Load;
    Store: @TFileViewer.Store;
    ReadFile: @TFileViewer.ReadFile;
    WriteModify: @TFileViewer.WriteModify;
    Seek: @TFileViewer.Seek;
    SaveToFile: @TFileViewer.SaveToFile;
    DoHighlite: @TFileViewer.DoHighlite;
    SeekEof: @TFileViewer.SeekEof;
    SeekBof: @TFileViewer.SeekBof;
    BreakOnStreamReadError: @TFileViewer.BreakOnStreamReadError
    );

  _TDrive: TTDrive =
    (
    VMT: TypeOf(TDrive);
    Init: @TDrive.Init;
    Load: @TDrive.Load
    );

  _TFindDrive: TTFindDrive =
    (
    VMT: TypeOf(TFindDrive);
    Init: @TFindDrive.Init;
    InitList: @TFindDrive.InitList;
    Load: @TFindDrive.Load;
    NewUpFile: @TFindDrive.NewUpFile
    );

  _TTempDrive: TTTempDrive =
    (
    VMT: TypeOf(TTempDrive);
    Init: @TTempDrive.Init;
    Load: @TTempDrive.Load
    );

  _TArcDrive: TTArcDrive =
    (
    VMT: TypeOf(TArcDrive);
    Init: @TArcDrive.Init;
    InitCol: @TArcDrive.InitCol;
    Load: @TArcDrive.Load;
    ReadArchive: @TArcDrive.ReadArchive;
    Exec: @TArcDrive.Exec;
    MakeListFile: @TArcDrive.MakeListFile;
    ExtractFiles: @TArcDrive.ExtractFiles;
    StdMsg4: @TArcDrive.StdMsg4
    );

  _TArvidDrive: TTArvidDrive =
    (
    VMT: TypeOf(TArvidDrive);
    Init: @TArvidDrive.Init;
    SeekDirectory: @TArvidDrive.SeekDirectory
    );

  DNMethods: TDNMethods =
    (
    RecordSize: SizeOf(TDNMethods);
    Reserved1: 0;
    Reserved2: 0;
    _TEmptyObject: @_TEmptyObject;
    _TObject: @_TObject;
    _TRegExp: @_TRegExp;
    _T1: nil;
    _T2: nil;
    _T3: nil;
    _TStream: @_TStream;
    _TDosStream: @_TDosStream;
    _TBufStream: @_TBufStream;
    _TMemoryStream: @_TMemoryStream;
    _TCollection: @_TCollection;
    _TSortedCollection: @_TSortedCollection;
    _TLineCollection: @_TLineCollection;
    _TStringCollection: @_TStringCollection;
    _TStrCollection: @_TStrCollection;
    _TFilesCollection: @_TFilesCollection;
    _TView: @_TView;
    _TFrame: @_TFrame;
    _TScrollBar: @_TScrollBar;
    _TGroup: @_TGroup;
    _TWindow: @_TWindow;
    _TMenuView: @_TMenuView;
    _TMenuBar: @_TMenuBar;
    _TMenuBox: @_TMenuBox;
    _TMenuPopup: @_TMenuPopup;
    _TStatusLine: @_TStatusLine;
    _TDialog: @_TDialog;
    _TInputLine: @_TInputLine;
    _TButton: @_TButton;
    _TCluster: @_TCluster;
    _TRadioButtons: @_TRadioButtons;
    _TCheckBoxes: @_TCheckBoxes;
    _TMultiCheckBoxes: @_TMultiCheckBoxes;
    _TComboBox: @_TComboBox;
    _TScroller: @_TScroller;
    _TListViewer: @_TListViewer;
    _TListBox: @_TListBox;
    _TStaticText: @_TStaticText;
    _TParamText: @_TParamText;
    _TLabel: @_TLabel;
    _THistoryViewer: @_THistoryViewer;
    _THistoryWindow: @_THistoryWindow;
    _THistory: @_THistory;
    _TBackground: @_TBackground;
    _TDesktop: @_TDesktop;
    _TProgram: @_TProgram;
    _TApplication: @_TApplication;
    _TDNApplication: @_TDNApplication;
    _TUniWindow: @_TUniWindow;
    _TXFileEditor: @_TXFileEditor;
    _TEditWindow: @_TEditWindow;
    _TPercentGauge: @_TPercentGauge;
    _TBarGauge: @_TBarGauge;
    _TWhileView: @_TWhileView;
    _TViewScroll: @_TViewScroll;
    _TFileViewer: @_TFileViewer;
    _TDrive: @_TDrive;
    _TFindDrive: @_TFindDrive;
    _TTempDrive: @_TTempDrive;
    _TArcDrive: @_TArcDrive;
    _TArvidDrive: @_TArvidDrive
    );

implementation

uses
  SysUtils
  ;

function TryExcept(Proc: TProcedure): Pointer;
  begin
  Result := nil;
  try
    Proc
  except
    on E: Exception do
      Result := E;
  end;
  end;

function DosError: Integer;
  begin
  DosError := Dos.DosError;
  end;

function ExecView(P: PView): Integer;
  begin
  ExecView := Desktop^.ExecView(P);
  end;

function ExecAndDisposeMenu(Menu: PMenu): Integer;
  var
    R: TRect;
    MenuBox: PMenuBox;
  begin
  MenuBox := New(PMenuBox, Init(R, Menu, nil));
  MenuBox^.Options := MenuBox^.Options or ofCentered;
  ExecAndDisposeMenu := Desktop^.ExecView(MenuBox);
  DisposeMenu(Menu);
  Dispose(MenuBox, Done);
  end;

function NewStatusDef(AMin, AMax: Word; AItems: PStatusItem;
     ANext: PStatusDef): PStatusDef;
  type
    PBuffer = ^TBuffer;
    TBuffer = array[SmallWord] of Boolean;
  var
    PP: ^PStatusDef;
    Buffer: PBuffer;
    I: SmallWord;
  begin
  if  (AMin = 0) and (AMax = 0) then
    begin
    New(Buffer);
    FillChar(Buffer^, SizeOf(TBuffer), #0);
    PP := @StatusLine^.Defs;
    while PP^ <> nil do
      begin
      if  (PP^^.Min = 0) and (PP^^.Max = $FFFF) then
        begin
        for I := $FFFF downto 0 do
          if not Buffer^[I] then
            begin
            PP^:= Menus.NewStatusDef(I, I, AItems, PP^);
            NewStatusDef := Pointer(I);
            Break;
            end;
        Dispose(Buffer);
        Exit;
        end
      else
        for I := PP^^.Min to PP^^.Max do
          Buffer^[I] := True;

      PP := @PP^^.Next;
      end;
    NewStatusDef := nil;
    Dispose(Buffer);
    end
  else
    NewStatusDef := Menus.NewStatusDef(AMin, AMax, AItems, ANext);
  end { NewStatusDef };

function GetActivePanel: Pointer;
  var
    PX: PXDoubleWindow;
    P: PView{PFilePanel};
    i: TPanelNum;
  begin
  PX := Pointer(Desktop^.Current);
  Result := nil;
  if TypeOf(PX^) = TypeOf(TXDoubleWindow) then
    with PX^ do
      for i := pLeft to pRight do
        begin
        P := Panel[i].FilePanel;
        if P^.GetState(sfSelected) then
          begin
          Result := P;
          Exit;
          end;
        end;
  end;

function GetPassivePanel: Pointer;
  var
    PX: PXDoubleWindow;
    P: PView{PFilePanel};
    i: TPanelNum;
  begin
  PX := Pointer(Desktop^.Current);
  Result := nil;
  if TypeOf(PX^) = TypeOf(TXDoubleWindow) then
    with PX^ do
      for i := pLeft to pRight do
        begin
        P := Panel[i].FilePanel;
        if P^.GetState(sfSelected) then
          begin
          Result := Panel[not i].FilePanel;
          Exit;
          end;
        end;
  end;

function OpenRezX(const PluginName: String): LongInt;
  var
    P: PString;
  begin
  Result := OpenRez(PluginName);
  if Result = 0 then
    begin
    P := @PluginName;
    MessageBox(GetString(dlPlugins6), @P, mfError+mfOKButton);
    end;
  end;

end.
