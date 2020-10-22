unit _Model1;
(******

DN/2 Plugin Interface - functional model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines
  ;

type
  PEmptyObjectVMT = ^TEmptyObjectVMT;
  PObjectVMT = ^TObjectVMT;
  PRegExpVMT = ^TRegExpVMT;
  PStreamVMT = ^TStreamVMT;
  PDosStreamVMT = ^TDosStreamVMT;
  PBufStreamVMT = ^TBufStreamVMT;
  PMemoryStreamVMT = ^TMemoryStreamVMT;
  PCollectionVMT = ^TCollectionVMT;
  PSortedCollectionVMT = ^TSortedCollectionVMT;
  PLineCollectionVMT = ^TLineCollectionVMT;
  PStringCollectionVMT = ^TStringCollectionVMT;
  PStrCollectionVMT = ^TStrCollectionVMT;
  PFilesCollectionVMT = ^TFilesCollectionVMT;
  PViewVMT = ^TViewVMT;
  PFrameVMT = ^TFrameVMT;
  PScrollBarVMT = ^TScrollBarVMT;
  PGroupVMT = ^TGroupVMT;
  PWindowVMT = ^TWindowVMT;
  PMenuViewVMT = ^TMenuViewVMT;
  PMenuBarVMT = ^TMenuBarVMT;
  PMenuBoxVMT = ^TMenuBoxVMT;
  PMenuPopupVMT = ^TMenuPopupVMT;
  PStatusLineVMT = ^TStatusLineVMT;
  PDialogVMT = ^TDialogVMT;
  PInputLineVMT = ^TInputLineVMT;
  PButtonVMT = ^TButtonVMT;
  PClusterVMT = ^TClusterVMT;
  PRadioButtonsVMT = ^TRadioButtonsVMT;
  PCheckBoxesVMT = ^TCheckBoxesVMT;
  PMultiCheckBoxesVMT = ^TMultiCheckBoxesVMT;
  PComboBoxVMT = ^TComboBoxVMT;
  PScrollerVMT = ^TScrollerVMT;
  PListViewerVMT = ^TListViewerVMT;
  PListBoxVMT = ^TListBoxVMT;
  PStaticTextVMT = ^TStaticTextVMT;
  PParamTextVMT = ^TParamTextVMT;
  PLabelVMT = ^TLabelVMT;
  PHistoryViewerVMT = ^THistoryViewerVMT;
  PHistoryWindowVMT = ^THistoryWindowVMT;
  PHistoryVMT = ^THistoryVMT;
  PBackgroundVMT = ^TBackgroundVMT;
  PDesktopVMT = ^TDesktopVMT;
  PProgramVMT = ^TProgramVMT;
  PApplicationVMT = ^TApplicationVMT;
  PDNApplicationVMT = ^TDNApplicationVMT;
  PUniWindowVMT = ^TUniWindowVMT;
  PXFileEditorVMT = ^TXFileEditorVMT;
  PEditWindowVMT = ^TEditWindowVMT;
  PPercentGaugeVMT = ^TPercentGaugeVMT;
  PBarGaugeVMT = ^TBarGaugeVMT;
  PWhileViewVMT = ^TWhileViewVMT;
  PViewScrollVMT = ^TViewScrollVMT;
  PFileViewerVMT = ^TFileViewerVMT;
  PDriveVMT = ^TDriveVMT;
  PFindDriveVMT = ^TFindDriveVMT;
  PTempDriveVMT = ^TTempDriveVMT;
  PArcDriveVMT = ^TArcDriveVMT;
  PArvidDriveVMT = ^TArvidDriveVMT;


  PEmptyObject = ^TEmptyObject;
  TEmptyObject = packed record
    VMT: PEmptyObjectVMT;
    end;

  PObject = ^TObject;
  TObject = packed record
    VMT: PObjectVMT;
    ObjectIsInited: Boolean;
    end;

  PRegExp = ^TRegExp;
  TRegExp = packed record
    VMT: PRegExpVMT;
    ObjectIsInited: Boolean;
    FStatus: TRegExpStatus;
    FStart: Integer;
    FLength: Integer;
    end;

  PStream = ^TStream;
  TStream = packed record
    VMT: PStreamVMT;
    ObjectIsInited: Boolean;
    Status: Integer;
    ErrorInfo: Integer;
    StreamSize: LongInt;
    Position: LongInt;
    end;

  PDosStream = ^TDosStream;
  TDOSStream = packed record
    VMT: PDosStreamVMT;
    ObjectIsInited: Boolean;
    Status: Integer;
    ErrorInfo: Integer;
    StreamSize: LongInt;
    Position: LongInt;
    Handle: Integer;
    FName: AsciiZ;
    FMode: Word;
    end;

  PBufStream = ^TBufStream;
  TBufStream = packed record
    VMT: PBufStreamVMT;
    ObjectIsInited: Boolean;
    Status: Integer;
    ErrorInfo: Integer;
    StreamSize: LongInt;
    Position: LongInt;
    Handle: Integer;
    FName: AsciiZ;
    FMode: Word;
    Buffer: PByteArray;
    BufSize: LongInt;
    BufPtr: LongInt;
    BufEnd: LongInt;
    ModBufStart: LongInt;
    ModBufEnd: LongInt;
    end;

  PMemoryStream = ^TMemoryStream;
  TMemoryStream = packed record
    VMT: PMemoryStreamVMT;
    ObjectIsInited: Boolean;
    Status: Integer;
    ErrorInfo: Integer;
    StreamSize: LongInt;
    Position: LongInt;
    BlkCount: LongInt;
    BlkSize: Word;
    MemSize: LongInt;
    BlkList: PPointerArray;
    end;

  PCollection = ^TCollection;
  TCollection = packed record
    VMT: PCollectionVMT;
    ObjectIsInited: Boolean;
    Items: Pointer {PItemList};
    Count: LongInt;
    Limit: LongInt;
    Delta: LongInt;
    Status, ErrorInfo: Integer;
    end;

  PSortedCollection = ^TSortedCollection;
  TSortedCollection = packed record
    VMT: PSortedCollectionVMT;
    ObjectIsInited: Boolean;
    Items: Pointer {PItemList};
    Count: LongInt;
    Limit: LongInt;
    Delta: LongInt;
    Status, ErrorInfo: Integer;
    Duplicates: Boolean;
    end;

  PLineCollection = ^TLineCollection;
  TLineCollection = packed record
    VMT: PLineCollectionVMT;
    ObjectIsInited: Boolean;
    Items: Pointer {PItemList};
    Count: LongInt;
    Limit: LongInt;
    Delta: LongInt;
    Status, ErrorInfo: Integer;
    LongStrings: Boolean;
    end;

  PStringCollection = ^TStringCollection;
  TStringCollection = packed record
    VMT: PStringCollectionVMT;
    ObjectIsInited: Boolean;
    Items: Pointer {PItemList};
    Count: LongInt;
    Limit: LongInt;
    Delta: LongInt;
    Status, ErrorInfo: Integer;
    Duplicates: Boolean;
    LongStrings: Boolean;
    end;

  PStrCollection = ^TStrCollection;
  TStrCollection = packed record
    VMT: PStrCollectionVMT;
    ObjectIsInited: Boolean;
    Items: Pointer {PItemList};
    Count: LongInt;
    Limit: LongInt;
    Delta: LongInt;
    Status, ErrorInfo: Integer;
    Duplicates: Boolean;
    end;

  PFilesCollection = ^TFilesCollection;
  TFilesCollection = packed record
    VMT: PFilesCollectionVMT;
    ObjectIsInited: Boolean;
    Items: Pointer {PItemList};
    Count: LongInt;
    Limit: LongInt;
    Delta: LongInt;
    Status, ErrorInfo: Integer;
    Duplicates: Boolean;
    SortMode: Byte;
    Selected: LongInt;
    Panel: Pointer;
    {$IFDEF WIN32}
    LFNActive: Boolean;
    {$ENDIF}
    end;

  PView = ^TView;
  PGroup = ^TGroup;

  TView = packed record
    VMT: PViewVMT;
    ObjectIsInited: Boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: Byte;
    DragMode: Byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: LongInt;
    ClearPositionalEvents: Boolean;
    end;

  PFrame = ^TFrame;
  TFrame = packed record
    View: TView;
    end;

  PScrollBar = ^TScrollBar;
  TScrollBar = packed record
    View: TView;
    Value: LongInt;
    Min: LongInt;
    Max: LongInt;
    PgStep: LongInt;
    ArStep: LongInt;
    Step: Longint;
    ForceScroll: Boolean;
    end;

  TGroup = packed record
    View: TView;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: Byte;
    EndState: Word;
    end;

  PWindow = ^TWindow;
  TWindow = packed record
    View: TView;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: Byte;
    EndState: Word;
    Flags: Byte;
    ZoomRect: TRect;
    MaxiRect: TRect;
    Number: AInt;
    Palette: AInt;
    Frame: PFrame;
    Title: PString;
    end;

  PMenuView = ^TMenuView;
  TMenuView = packed record
    View: TView;
    ParentMenu: PMenuView;
    Menu: PMenu;
    Current: PMenuItem;
    HaveSubmenu: Boolean;
    LastActionIsExpand: Boolean;
    end;

  PMenuBar = ^TMenuBar;
  TMenuBar = packed record
    View: TView;
    ParentMenu: PMenuView;
    Menu: PMenu;
    Current: PMenuItem;
    HaveSubmenu: Boolean;
    LastActionIsExpand: Boolean;
    end;

  PMenuBox = ^TMenuBox;
  TMenuBox = packed record
    View: TView;
    ParentMenu: PMenuView;
    Menu: PMenu;
    Current: PMenuItem;
    HaveSubmenu: Boolean;
    LastActionIsExpand: Boolean;
    TopItem: PMenuItem;
    ComboBoxPal: Boolean;
    end;

  PMenuPopup = ^TMenuPopup;
  TMenuPopup = packed record
    View: TView;
    ParentMenu: PMenuView;
    Menu: PMenu;
    Current: PMenuItem;
    HaveSubmenu: Boolean;
    LastActionIsExpand: Boolean;
    TopItem: PMenuItem;
    ComboBoxPal: Boolean;
    end;

  PStatusLine = ^TStatusLine;
  TStatusLine = packed record
    View: TView;
    Items: PStatusItem;
    Defs: PStatusDef;
    end;

  PDialog = ^TDialog;
  TDialog = packed record
    View: TView;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: Byte;
    EndState: Word;
    Flags: Byte;
    ZoomRect: TRect;
    MaxiRect: TRect;
    Number: AInt;
    Palette: AInt;
    Frame: PFrame;
    Title: PString;
    DirectLink: array [1..9] of PView;
    end;

  PInputline = ^TInputLine;
  TInputLine = packed record
    View: TView;
    Data: AnsiString;
    MaxLen: LongInt;
    CurPos: LongInt;
    FirstPos: LongInt;
    SelStart: LongInt;
    SelEnd: LongInt;
    Validator: Pointer {PValidator};
    LC, RC: Char;
    C: array [1..4] of Byte;
    end;

  PButton = ^TButton;
  TButton = packed record
    View: TView;
    Title: PString;
    Command: AWord;
    Flags: Byte;
    AmDefault: Boolean;
    end;

  PCluster = ^TCluster;
  TCluster = packed record
    View: TView;
    Value: LongInt;
    Sel: AInt;
    EnableMask: LongInt;
    Strings: TStringCollection;
    end;

  PRadioButtons = ^TRadioButtons;
  TRadioButtons = packed record
    View: TView;
    Value: LongInt;
    Sel: AInt;
    EnableMask: LongInt;
    Strings: TStringCollection;
    end;

  PCheckBoxes = ^TCheckBoxes;
  TCheckBoxes = packed record
    View: TView;
    Value: LongInt;
    Sel: AInt;
    EnableMask: LongInt;
    Strings: TStringCollection;
    end;

  PMultiCheckBoxes = ^TMultiCheckBoxes;
  TMultiCheckBoxes = packed record
    View: TView;
    Value: LongInt;
    Sel: AInt;
    EnableMask: LongInt;
    Strings: TStringCollection;
    SelRange: Byte;
    Flags: AWord;
    States: PString;
    end;

  PComboBox = ^TComboBox;
  TComboBox = packed record
    View: TView;
    Selected: Word;
    Count: Word;
    Menu: Pointer;
    Items: array[1..10] of Pointer;
    end;

  PScroller = ^TScroller;
  TScroller = packed record
    View: TView;
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    Delta: TPoint;
    Limit: TPoint;
    DrawLock: Byte;
    DrawFlag: Boolean;
    end;

  PListViewer = ^TListViewer;
  TListViewer = packed record
    View: TView;
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    NumCols: LongInt;
    TopItem: LongInt;
    Focused: LongInt;
    Range: LongInt;
    end;

  PListBox = ^TListBox;
  TListBox = packed record
    View: TView;
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    NumCols: LongInt;
    TopItem: LongInt;
    Focused: LongInt;
    Range: LongInt;
    List: PCollection;
    end;

  PStaticText = ^TStaticText;
  TStaticText = packed record
    View: TView;
    Text: PString;
    end;

  PParamText = ^TParamText;
  TParamText = packed record
    View: TView;
    Text: PString;
    ParamCount: AInt;
    ParamList: Pointer;
    end;

  PLabel = ^TLabel;
  TLabel = packed record
    View: TView;
    Text: PString;
    Link: PView;
    Light: Boolean;
    end;

  PHistoryViewer = ^THistoryViewer;
  THistoryViewer = packed record
    View: TView;
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    NumCols: LongInt;
    TopItem: LongInt;
    Focused: LongInt;
    Range: LongInt;
    HistoryId: AWord;
    SearchPos: AWord;
    end;

  PHistoryWindow = ^THistoryWindow;
  THistoryWindow = packed record
    View: TView;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: Byte;
    EndState: Word;
    Flags: Byte;
    ZoomRect: TRect;
    MaxiRect: TRect;
    Number: AInt;
    Palette: AInt;
    Frame: PFrame;
    Title: PString;
    Viewer: PListViewer;
    end;

  PHistory = ^THistory;
  THistory = packed record
    View: TView;
    Link: PInputline;
    HistoryId: AWord;
    end;

  PBackground = ^TBackground;
  TBackground = packed record
    View: TView;
    Pattern: Char;
    end;

  PDesktop = ^TDesktop;
  TDesktop = packed record
    View: TView;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: Byte;
    EndState: Word;
    Background: PBackground;
    TileColumnsFirst: Boolean;
    end;

  PProgram = ^TProgram;
  TProgram = packed record
    View: TView;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: Byte;
    EndState: Word;
    IdleSecs: TEventTimer;
    FullSpeed: Boolean;
    end;

  PApplication = ^TApplication;
  TApplication = packed record
    View: TView;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: Byte;
    EndState: Word;
    IdleSecs: TEventTimer;
    FullSpeed: Boolean;
    Clock: PView;
    end;

  PDNApplication = ^TDNApplication;
  TDNApplication = packed record
    View: TView;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: Byte;
    EndState: Word;
    IdleSecs: TEventTimer;
    FullSpeed: Boolean;
    Clock: PView;
    IdleClick: TEventTimer;
    IdleEvt: TEvent;
    TreeReader: Pointer {PTreeReader};
    Pk1, Pk2, Pk3, Pk4: PView;
    end;

  PUniWindow = ^TUniWindow;
  TUniWindow = packed record
    View: TView;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: Byte;
    EndState: Word;
    Flags: Byte;
    ZoomRect: TRect;
    MaxiRect: TRect;
    Number: AInt;
    Palette: AInt;
    Frame: PFrame;
    Title: PString;
    end;

  PXFileEditor = ^TXFileEditor;
  TXFileEditor = packed record
    View: TView;
    HScroll, VScroll: PScrollBar;
    ReplaceAll: Boolean;
    Delta: TPoint;
    EditName: String;
    FileLines: PLineCollection;
    isValid: Boolean;
    Marking: Boolean;
    SmartPad: Boolean;
    ClipBrd: Boolean;
    OptMenu: PMenu;
    Mark, Sel: TRect;
    Pos, LastPos: TPoint;
    MarkPos: TPosArray;
    WorkLine: LongInt;
    DrawMode: Integer;
    WorkString, LastShape: LongString;
    OldBlockValid, SearchOnDisplay,
    InsertMode, VertBlock, Modified,
    WorkModified, JustSaved, BlockVisible,
    SpecChar, WasDelete, MouseMark, UnMark,
    LineMarking, OptimalFill, RulerVisible,
    EnableMarking, TabReplace, SearchActive: Boolean;
    UndoInfo: PCollection;
    RedoInfo: PCollection;
    UndoTimes, LastSaveUndoTimes: LongInt;
    ChPosition: Boolean;
    Macros: PCollection;
    Locker: PStream;
    LastDir: Integer;
    MemEnough: Boolean;
    KeyMap: TKeyMap;
    EdOpt: TEditOptions;
    HiLitePar: THighliteParams;
    InfoL, BMrk: PView;
    MenuItemStr: array[Boolean] of PString;
    RegExp: PRegExp;
    end;

  PEditWindow = ^TEditWindow;
  TEditWindow = packed record
    View: TView;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: Byte;
    EndState: Word;
    Flags: Byte;
    ZoomRect: TRect;
    MaxiRect: TRect;
    Number: AInt;
    Palette: AInt;
    Frame: PFrame;
    Title: PString;
    AInfo: Pointer {PInfoLine};
    ABookLine: Pointer {PBookmarkLine};
    Intern: Pointer {PFileEditor};
    MenuBar: PMenuBar;
    UpMenu: PMenu;
    ModalEnd: Boolean;
    end;

  PPercentGauge = ^TPercentGauge;
  TPercentGauge = packed record
    View: TView;
    MaxValue: LongInt;
    CurValue: LongInt;
    end;

  PBarGauge = ^TBarGauge;
  TBarGauge = packed record
    View: TView;
    MaxValue: LongInt;
    CurValue: LongInt;
    end;

  PWhileView = ^TWhileView;
  TWhileView = packed record
    View: TView;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: Byte;
    EndState: Word;
    Lines: PCollection;
    But: PButton;
    QuitNormal: Boolean;
    Top, Bottom: String;
    Side: (sdLeft, sdRight);
    end;

  PViewScroll = ^TViewScroll;
  TViewScroll = packed record
    View: TView;
    MaxV, Value: LongInt;
    end;

  PFileViewer = ^TFileViewer;
  TFileViewer = packed record
    View: TView;
    OldSizeX: Integer;
    Filtr: Boolean;
    NoEdit: Boolean;
    FileName: String;
    VFileName: String;
    Buf: PByteArray;
    Fl: PStream;
    UpdateViewTmr: TEventTimer;
    XDelta, ViewMode, HexPos: AInt;
    SearchActive: Boolean;
    SearchResultVisible: Boolean;
    SearchX: LongInt;
    SB: PView;
    Wrap: Byte;
    Lines: array[0..200] of
    record
      Pos: LongInt;
      len: Word;
      end;
    FilePos, FileSize, NumLines: LongInt;
    ExposedPos, ExposedLine: LongInt;
    Cur: TPoint;
    Info: PView;
    BufPos: LongInt;
    BufSize, MaxLines: LongInt;
    BufLines: AInt;
    KillAfterUse, isValid, QuickView, Loaded, HexEdit, BufModified:
     Boolean;
    FakeKillAfterUse: Boolean;
    Filter: Byte;
    XLAT: TXlat;
    UseXLat: Boolean;
    XLatFile: PString;
    KeyMap: TKeyMap;
    MarkPos: TPosArray;
    CtrlK: Boolean;
    CtrlQ: Boolean;
    HiLite: Boolean;
    ScrollEOF: Boolean;
    HiLitePar: THighliteParams;
    end;

  PDrive = ^TDrive;
  TDrive = packed record
    VMT: PDriveVMT;
    ObjectIsInited: Boolean;
    Owner: Pointer;
    Prev: PDrive;
    DriveType: TDriveType;
    CurDir: String;
    DizOwner: String;
    NoMemory: Boolean;
    Flags: AWord;
    LFNLen: Byte;
    EXTLen: Byte;
    DirFLP, FilFLP: AWord;
    Param, OldParam: Byte;
    innum: Byte;
    SizeX: LongInt;
    {$IFDEF OS2}
    ShowLogNames: Boolean;
    {$ENDIF}
    end;

  PFindDrive = ^TFindDrive;
  TFindDrive = packed record
    VMT: PFindDriveVMT;
    ObjectIsInited: Boolean;
    Owner: Pointer;
    Prev: PDrive;
    DriveType: TDriveType;
    CurDir: String;
    DizOwner: String;
    NoMemory: Boolean;
    Flags: AWord;
    LFNLen: Byte;
    EXTLen: Byte;
    DirFLP, FilFLP: AWord;
    Param, OldParam: Byte;
    innum: Byte;
    SizeX: LongInt;
    {$IFDEF OS2}
    ShowLogNames: Boolean;
    {$ENDIF}
    isDisposable: Boolean;
    Files: PFilesCollection;
    Dirs: PSortedCollection;
    ListFile: PString;
    UpFile: PFileRec;
    AMask, AWhat: PString;
    end;

  PTempDrive = ^TTempDrive;
  TTempDrive = packed record
    VMT: PTempDriveVMT;
    ObjectIsInited: Boolean;
    Owner: Pointer;
    Prev: PDrive;
    DriveType: TDriveType;
    CurDir: String;
    DizOwner: String;
    NoMemory: Boolean;
    Flags: AWord;
    LFNLen: Byte;
    EXTLen: Byte;
    DirFLP, FilFLP: AWord;
    Param, OldParam: Byte;
    innum: Byte;
    SizeX: LongInt;
    {$IFDEF OS2}
    ShowLogNames: Boolean;
    {$ENDIF}
    isDisposable: Boolean;
    Files: PFilesCollection;
    Dirs: PSortedCollection;
    ListFile: PString;
    UpFile: PFileRec;
    AMask, AWhat: PString;
    end;

  PArcDrive = ^TArcDrive;
  TArcDrive = packed record
    VMT: PArcDriveVMT;
    ObjectIsInited: Boolean;
    Owner: Pointer;
    Prev: PDrive;
    DriveType: TDriveType;
    CurDir: String;
    DizOwner: String;
    NoMemory: Boolean;
    Flags: AWord;
    LFNLen: Byte;
    EXTLen: Byte;
    DirFLP, FilFLP: AWord;
    Param, OldParam: Byte;
    innum: Byte;
    SizeX: LongInt;
    {$IFDEF OS2}
    ShowLogNames: Boolean;
    {$ENDIF}
    ArcName: String;
    VArcName: String;
    AType: Pointer {PARJArchive};
    Files: Pointer {PDirStorage};
    KillAfterUse: Boolean;
    FakeKillAfterUse: Boolean;
    ArcDate: LongInt;
    ForceRescan: Boolean;
    Password: String;
    end;

  PArvidDrive = ^TArvidDrive;
  TArvidDrive = packed record
    VMT: PArvidDriveVMT;
    ObjectIsInited: Boolean;
    Owner: Pointer;
    Prev: PDrive;
    DriveType: TDriveType;
    CurDir: String;
    DizOwner: String;
    NoMemory: Boolean;
    Flags: AWord;
    LFNLen: Byte;
    EXTLen: Byte;
    DirFLP, FilFLP: AWord;
    Param, OldParam: Byte;
    innum: Byte;
    SizeX: LongInt;
    {$IFDEF OS2}
    ShowLogNames: Boolean;
    {$ENDIF}
    Name: PString;
    Stream: PStream;
    CurFile: LongInt;
    CurDirPos: LongInt;
    PosTableOfs: LongInt;
    CurFileNum: AWord;
    CurLevel: AWord;
    CurDate: LongInt;
    KillAfterUse: Boolean;
    filetype: TAvdType;
    D: TTdrHeader;
    AVT: TAvtHeader;
    TapeFmt: AWord;
    TapeTotalTime: AWord;
    TapeRecordedTime: AWord;
    TotFiles: LongInt;
    TotLen: LongInt;
    CurDirCellPos: LongInt;
    end;


  TEmptyObjectVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    end;

  TObjectVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    end;

  TRegExpVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Error: procedure (AStatus: TRegExpStatus; Obj: Pointer);
    CheckBreak: function (Obj: Pointer): Boolean;
    Escape: procedure (AChar: Char; var ASubExp: PChar;
       var ALen: Integer; Obj: Pointer);
    end;

  TStreamVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Error: procedure (Code, Info: Integer; Obj: Pointer);
    Flush: procedure (Obj: Pointer);
    GetPos: function (Obj: Pointer): LongInt;
    GetSize: function (Obj: Pointer): LongInt;
    Read: procedure (var Buf; Count: LongInt; Obj: Pointer);
    Seek: procedure (Pos: LongInt; Obj: Pointer);
    Truncate: procedure (Obj: Pointer);
    Write: procedure (const Buf; Count: LongInt; Obj: Pointer);
    DoOpen: procedure (OpenMode: Word; Obj: Pointer);
    Close: procedure (Obj: Pointer);
    end;

  TDosStreamVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Error: procedure (Code, Info: Integer; Obj: Pointer);
    Flush: procedure (Obj: Pointer);
    GetPos: function (Obj: Pointer): LongInt;
    GetSize: function (Obj: Pointer): LongInt;
    Read: procedure (var Buf; Count: LongInt; Obj: Pointer);
    Seek: procedure (Pos: LongInt; Obj: Pointer);
    Truncate: procedure (Obj: Pointer);
    Write: procedure (const Buf; Count: LongInt; Obj: Pointer);
    DoOpen: procedure (OpenMode: Word; Obj: Pointer);
    Close: procedure (Obj: Pointer);
    ReadBlock: procedure (var Buf; Count: LongInt; var BytesRead: Word;
       Obj: Pointer);
    end;

  TBufStreamVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Error: procedure (Code, Info: Integer; Obj: Pointer);
    Flush: procedure (Obj: Pointer);
    GetPos: function (Obj: Pointer): LongInt;
    GetSize: function (Obj: Pointer): LongInt;
    Read: procedure (var Buf; Count: LongInt; Obj: Pointer);
    Seek: procedure (Pos: LongInt; Obj: Pointer);
    Truncate: procedure (Obj: Pointer);
    Write: procedure (const Buf; Count: LongInt; Obj: Pointer);
    DoOpen: procedure (OpenMode: Word; Obj: Pointer);
    Close: procedure (Obj: Pointer);
    ReadBlock: procedure (var Buf; Count: LongInt; var BytesRead: Word;
       Obj: Pointer);
    end;

  TMemoryStreamVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Error: procedure (Code, Info: Integer; Obj: Pointer);
    Flush: procedure (Obj: Pointer);
    GetPos: function (Obj: Pointer): LongInt;
    GetSize: function (Obj: Pointer): LongInt;
    Read: procedure (var Buf; Count: LongInt; Obj: Pointer);
    Seek: procedure (Pos: LongInt; Obj: Pointer);
    Truncate: procedure (Obj: Pointer);
    Write: procedure (const Buf; Count: LongInt; Obj: Pointer);
    DoOpen: procedure (OpenMode: Word; Obj: Pointer);
    Close: procedure (Obj: Pointer);
    end;

  TCollectionVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Error: procedure (Code, Info: Integer; Obj: Pointer);
    FreeItem: procedure (Item: Pointer; Obj: Pointer);
    GetItem: function (var S: TStream; Obj: Pointer): Pointer;
    IndexOf: function (Item: Pointer; Obj: Pointer): LongInt;
    Insert: procedure (Item: Pointer; Obj: Pointer);
    PutItem: procedure (var S: TStream; Item: Pointer; Obj: Pointer);
    SetLimit: procedure (ALimit: LongInt; Obj: Pointer);
    end;

  TSortedCollectionVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Error: procedure (Code, Info: Integer; Obj: Pointer);
    FreeItem: procedure (Item: Pointer; Obj: Pointer);
    GetItem: function (var S: TStream; Obj: Pointer): Pointer;
    IndexOf: function (Item: Pointer; Obj: Pointer): LongInt;
    Insert: procedure (Item: Pointer; Obj: Pointer);
    PutItem: procedure (var S: TStream; Item: Pointer; Obj: Pointer);
    SetLimit: procedure (ALimit: LongInt; Obj: Pointer);
    Compare: function (Key1, Key2: Pointer; Obj: Pointer): Integer;
    KeyOf: function (Item: Pointer; Obj: Pointer): Pointer;
    Search: function (Key: Pointer; var Index: LongInt; Obj: Pointer)
    : Boolean;
    end;

  TLineCollectionVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Error: procedure (Code, Info: Integer; Obj: Pointer);
    FreeItem: procedure (Item: Pointer; Obj: Pointer);
    GetItem: function (var S: TStream; Obj: Pointer): Pointer;
    IndexOf: function (Item: Pointer; Obj: Pointer): LongInt;
    Insert: procedure (Item: Pointer; Obj: Pointer);
    PutItem: procedure (var S: TStream; Item: Pointer; Obj: Pointer);
    SetLimit: procedure (ALimit: LongInt; Obj: Pointer);
    end;

  TStringCollectionVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Error: procedure (Code, Info: Integer; Obj: Pointer);
    FreeItem: procedure (Item: Pointer; Obj: Pointer);
    GetItem: function (var S: TStream; Obj: Pointer): Pointer;
    IndexOf: function (Item: Pointer; Obj: Pointer): LongInt;
    Insert: procedure (Item: Pointer; Obj: Pointer);
    PutItem: procedure (var S: TStream; Item: Pointer; Obj: Pointer);
    SetLimit: procedure (ALimit: LongInt; Obj: Pointer);
    Compare: function (Key1, Key2: Pointer; Obj: Pointer): Integer;
    KeyOf: function (Item: Pointer; Obj: Pointer): Pointer;
    Search: function (Key: Pointer; var Index: LongInt; Obj: Pointer)
    : Boolean;
    end;

  TStrCollectionVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Error: procedure (Code, Info: Integer; Obj: Pointer);
    FreeItem: procedure (Item: Pointer; Obj: Pointer);
    GetItem: function (var S: TStream; Obj: Pointer): Pointer;
    IndexOf: function (Item: Pointer; Obj: Pointer): LongInt;
    Insert: procedure (Item: Pointer; Obj: Pointer);
    PutItem: procedure (var S: TStream; Item: Pointer; Obj: Pointer);
    SetLimit: procedure (ALimit: LongInt; Obj: Pointer);
    Compare: function (Key1, Key2: Pointer; Obj: Pointer): Integer;
    KeyOf: function (Item: Pointer; Obj: Pointer): Pointer;
    Search: function (Key: Pointer; var Index: LongInt; Obj: Pointer)
    : Boolean;
    end;

  TFilesCollectionVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Error: procedure (Code, Info: Integer; Obj: Pointer);
    FreeItem: procedure (Item: Pointer; Obj: Pointer);
    GetItem: function (var S: TStream; Obj: Pointer): Pointer;
    IndexOf: function (Item: Pointer; Obj: Pointer): LongInt;
    Insert: procedure (Item: Pointer; Obj: Pointer);
    PutItem: procedure (var S: TStream; Item: Pointer; Obj: Pointer);
    SetLimit: procedure (ALimit: LongInt; Obj: Pointer);
    Compare: function (Key1, Key2: Pointer; Obj: Pointer): Integer;
    KeyOf: function (Item: Pointer; Obj: Pointer): Pointer;
    Search: function (Key: Pointer; var Index: LongInt; Obj: Pointer)
    : Boolean;
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  TViewVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    end;

  TFrameVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    end;

  TScrollBarVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    ScrollDraw: procedure (Obj: Pointer);
    ScrollStep: function (Part: LongInt; Obj: Pointer): LongInt;
    end;

  TGroupVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    end;

  TWindowVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Close: procedure (Obj: Pointer);
    GetTitle: function (MaxSize: Integer; Obj: Pointer): String;
    InitFrame: procedure (Obj: Pointer);
    Zoom: procedure (Obj: Pointer);
    Maxi: procedure (Obj: Pointer);
    ReactOnCmd: function (Obj: Pointer): Boolean;
    end;

  TMenuViewVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetItemRect: procedure (Item: PMenuItem; var R: TRect; Obj: Pointer);
    NewSubView: function (var Bounds: TRect; AMenu: PMenu;
       AParentMenu: PMenuView; Obj: Pointer): PMenuView;
    end;

  TMenuBarVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetItemRect: procedure (Item: PMenuItem; var R: TRect; Obj: Pointer);
    NewSubView: function (var Bounds: TRect; AMenu: PMenu;
       AParentMenu: PMenuView; Obj: Pointer): PMenuView;
    end;

  TMenuBoxVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetItemRect: procedure (Item: PMenuItem; var R: TRect; Obj: Pointer);
    NewSubView: function (var Bounds: TRect; AMenu: PMenu;
       AParentMenu: PMenuView; Obj: Pointer): PMenuView;
    end;

  TMenuPopupVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetItemRect: procedure (Item: PMenuItem; var R: TRect; Obj: Pointer);
    NewSubView: function (var Bounds: TRect; AMenu: PMenu;
       AParentMenu: PMenuView; Obj: Pointer): PMenuView;
    end;

  TStatusLineVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    Hint: function (AHelpCtx: Word; Obj: Pointer): String;
    end;

  TDialogVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Close: procedure (Obj: Pointer);
    GetTitle: function (MaxSize: Integer; Obj: Pointer): String;
    InitFrame: procedure (Obj: Pointer);
    Zoom: procedure (Obj: Pointer);
    Maxi: procedure (Obj: Pointer);
    ReactOnCmd: function (Obj: Pointer): Boolean;
    end;

  TInputLineVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    end;

  TButtonVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    Press: procedure (Obj: Pointer);
    end;

  TClusterVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    Mark: function (Item: Integer; Obj: Pointer): Boolean;
    MultiMark: function (Item: Integer; Obj: Pointer): Byte;
    Press: procedure (Item: Integer; Obj: Pointer);
    MovedTo: procedure (Item: Integer; Obj: Pointer);
    end;

  TRadioButtonsVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    Mark: function (Item: Integer; Obj: Pointer): Boolean;
    MultiMark: function (Item: Integer; Obj: Pointer): Byte;
    Press: procedure (Item: Integer; Obj: Pointer);
    MovedTo: procedure (Item: Integer; Obj: Pointer);
    end;

  TCheckBoxesVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    Mark: function (Item: Integer; Obj: Pointer): Boolean;
    MultiMark: function (Item: Integer; Obj: Pointer): Byte;
    Press: procedure (Item: Integer; Obj: Pointer);
    MovedTo: procedure (Item: Integer; Obj: Pointer);
    end;

  TMultiCheckBoxesVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    Mark: function (Item: Integer; Obj: Pointer): Boolean;
    MultiMark: function (Item: Integer; Obj: Pointer): Byte;
    Press: procedure (Item: Integer; Obj: Pointer);
    MovedTo: procedure (Item: Integer; Obj: Pointer);
    end;

  TComboBoxVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    end;

  TScrollerVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    ScrollDraw: procedure (Obj: Pointer);
    end;

  TListViewerVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    FocusItem: procedure (Item: LongInt; Obj: Pointer);
    GetText: function (Item: LongInt; MaxLen: Integer; Obj: Pointer)
    : String;
    IsSelected: function (Item: LongInt; Obj: Pointer): Boolean;
    SelectItem: procedure (Item: LongInt; Obj: Pointer);
    FocusItemNum: procedure (Item: LongInt; Obj: Pointer);
    end;

  TListBoxVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    FocusItem: procedure (Item: LongInt; Obj: Pointer);
    GetText: function (Item: LongInt; MaxLen: Integer; Obj: Pointer)
    : String;
    IsSelected: function (Item: LongInt; Obj: Pointer): Boolean;
    SelectItem: procedure (Item: LongInt; Obj: Pointer);
    FocusItemNum: procedure (Item: LongInt; Obj: Pointer);
    NewLisT: procedure (AList: PCollection; Obj: Pointer);
    end;

  TStaticTextVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetText: procedure (var S: String; Obj: Pointer);
    end;

  TParamTextVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetText: procedure (var S: String; Obj: Pointer);
    end;

  TLabelVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetText: procedure (var S: String; Obj: Pointer);
    end;

  THistoryViewerVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    FocusItem: procedure (Item: LongInt; Obj: Pointer);
    GetText: function (Item: LongInt; MaxLen: Integer; Obj: Pointer)
    : String;
    IsSelected: function (Item: LongInt; Obj: Pointer): Boolean;
    SelectItem: procedure (Item: LongInt; Obj: Pointer);
    FocusItemNum: procedure (Item: LongInt; Obj: Pointer);
    end;

  THistoryWindowVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Close: procedure (Obj: Pointer);
    GetTitle: function (MaxSize: Integer; Obj: Pointer): String;
    InitFrame: procedure (Obj: Pointer);
    Zoom: procedure (Obj: Pointer);
    Maxi: procedure (Obj: Pointer);
    ReactOnCmd: function (Obj: Pointer): Boolean;
    GetSelection: function (Obj: Pointer): String;
    InitViewer: procedure (HistoryId: AWord; Obj: Pointer);
    end;

  THistoryVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    InitHistoryWindow: function (var Bounds: TRect; Obj: Pointer)
    : PHistoryWindow;
    RecordHistory: procedure (const S: String; Obj: Pointer);
    end;

  TBackgroundVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    end;

  TDesktopVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    InitBackground: procedure (Obj: Pointer);
    TileError: procedure (Obj: Pointer);
    end;

  TProgramVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Idle: procedure (Obj: Pointer);
    InitDesktop: procedure (Obj: Pointer);
    InitMenuBar: procedure (Obj: Pointer);
    InitScreen: procedure (Obj: Pointer);
    InitStatusLine: procedure (Obj: Pointer);
    InitCommandLine: procedure (Obj: Pointer);
    OutOfMemory: procedure (Obj: Pointer);
    Run: procedure (Obj: Pointer);
    end;

  TApplicationVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Idle: procedure (Obj: Pointer);
    InitDesktop: procedure (Obj: Pointer);
    InitMenuBar: procedure (Obj: Pointer);
    InitScreen: procedure (Obj: Pointer);
    InitStatusLine: procedure (Obj: Pointer);
    InitCommandLine: procedure (Obj: Pointer);
    OutOfMemory: procedure (Obj: Pointer);
    Run: procedure (Obj: Pointer);
    WhenShow: procedure (Obj: Pointer);
    GetTileRect: procedure (var R: TRect; Obj: Pointer);
    end;

  TDNApplicationVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Idle: procedure (Obj: Pointer);
    InitDesktop: procedure (Obj: Pointer);
    InitMenuBar: procedure (Obj: Pointer);
    InitScreen: procedure (Obj: Pointer);
    InitStatusLine: procedure (Obj: Pointer);
    InitCommandLine: procedure (Obj: Pointer);
    OutOfMemory: procedure (Obj: Pointer);
    Run: procedure (Obj: Pointer);
    WhenShow: procedure (Obj: Pointer);
    GetTileRect: procedure (var R: TRect; Obj: Pointer);
    end;

  TUniWindowVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Close: procedure (Obj: Pointer);
    GetTitle: function (MaxSize: Integer; Obj: Pointer): String;
    InitFrame: procedure (Obj: Pointer);
    Zoom: procedure (Obj: Pointer);
    Maxi: procedure (Obj: Pointer);
    ReactOnCmd: function (Obj: Pointer): Boolean;
    end;

  TXFileEditorVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    HandleCommand: function (var Event: TEvent; Obj: Pointer): Boolean;
    end;

  TEditWindowVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Close: procedure (Obj: Pointer);
    GetTitle: function (MaxSize: Integer; Obj: Pointer): String;
    InitFrame: procedure (Obj: Pointer);
    Zoom: procedure (Obj: Pointer);
    Maxi: procedure (Obj: Pointer);
    ReactOnCmd: function (Obj: Pointer): Boolean;
    end;

  TPercentGaugeVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    UpdateView: procedure (Progress: LongInt; Obj: Pointer);
    end;

  TBarGaugeVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    UpdateView: procedure (Progress: LongInt; Obj: Pointer);
    end;

  TWhileViewVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    end;

  TViewScrollVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    end;

  TFileViewerVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj: Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): Word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: Word; Obj: Pointer);
    Execute: function (Obj: Pointer): Word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): Word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: Word; Enable: Boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: Word; Obj: Pointer): Boolean;
    Update: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    HideView: procedure (Obj: Pointer);
    ShowView: procedure (Obj: Pointer);
    CountDown: procedure (Obj: Pointer; ANumber: Integer);
    CountUp: procedure (Obj: Pointer; ANumber: Integer);
    MakeLines: procedure (Obj: Pointer);
    end;

  TDriveVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    KillUse: procedure (Obj: Pointer);
    lChDir: procedure (ADir: String; Obj: Pointer);
    GetDir: function (Obj: Pointer): String;
    GetDirectory: function (const FileMask: String;
      var TotalInfo: TSize; Obj: Pointer): PCollection;
    CopyFiles: procedure (Files: PCollection; Own: PView;
       MoveMode: Boolean; Obj: Pointer);
    CopyFilesInto: procedure (Files: PCollection; Own: PView;
       MoveMode: Boolean; Obj: Pointer);
    EraseFiles: procedure (Files: PCollection; Obj: Pointer);
    UseFile: procedure (P: PFileRec; Command: Word; Obj: Pointer);
    GetFreeSpace: procedure (var S: String; Obj: Pointer);
    Disposable: function (Obj: Pointer): Boolean;
    GetRealName: function (Obj: Pointer): String;
    GetInternalName: function (Obj: Pointer): String;
    GetFull: procedure (var B; P: PFileRec; C, Sc: Word; Obj: Pointer);
    GetEmpty: procedure (var B; Sc: Word; Obj: Pointer);
    CalcLengthWithoutName: function (Obj: Pointer): Integer;
    CalcLength: function (Obj: Pointer): Integer;
    RereadDirectory: procedure (S: String; Obj: Pointer);
    MakeTop: procedure (var S: String; Obj: Pointer);
    GetDown: procedure (var B; C: Word; P: PFileRec; Obj: Pointer);
    HandleCommand: procedure (Command: Word; InfoPtr: Pointer;
       Obj: Pointer);
    GetDirInfo: procedure (var B: TDiskInfoRec; Obj: Pointer);
    GetRealDir: function (Obj: Pointer): String;
    MakeDir: procedure (Obj: Pointer);
    isUp: function (Obj: Pointer): Boolean;
    ChangeUp: procedure (var S: String; Obj: Pointer);
    ChangeRoot: procedure (Obj: Pointer);
    GetFullFlags: function (Obj: Pointer): Word;
    EditDescription: procedure (PF: PFileRec; Obj: Pointer);
    GetDirLength: procedure (PF: PFileRec; Obj: Pointer);
    GetParam: procedure (n: Byte; Obj: Pointer);
    end;

  TFindDriveVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    KillUse: procedure (Obj: Pointer);
    lChDir: procedure (ADir: String; Obj: Pointer);
    GetDir: function (Obj: Pointer): String;
    GetDirectory: function (const FileMask: String;
      var TotalInfo: TSize; Obj: Pointer): PCollection;
    CopyFiles: procedure (Files: PCollection; Own: PView;
       MoveMode: Boolean; Obj: Pointer);
    CopyFilesInto: procedure (Files: PCollection; Own: PView;
       MoveMode: Boolean; Obj: Pointer);
    EraseFiles: procedure (Files: PCollection; Obj: Pointer);
    UseFile: procedure (P: PFileRec; Command: Word; Obj: Pointer);
    GetFreeSpace: procedure (var S: String; Obj: Pointer);
    Disposable: function (Obj: Pointer): Boolean;
    GetRealName: function (Obj: Pointer): String;
    GetInternalName: function (Obj: Pointer): String;
    GetFull: procedure (var B; P: PFileRec; C, Sc: Word; Obj: Pointer);
    GetEmpty: procedure (var B; Sc: Word; Obj: Pointer);
    CalcLengthWithoutName: function (Obj: Pointer): Integer;
    CalcLength: function (Obj: Pointer): Integer;
    RereadDirectory: procedure (S: String; Obj: Pointer);
    MakeTop: procedure (var S: String; Obj: Pointer);
    GetDown: procedure (var B; C: Word; P: PFileRec; Obj: Pointer);
    HandleCommand: procedure (Command: Word; InfoPtr: Pointer;
       Obj: Pointer);
    GetDirInfo: procedure (var B: TDiskInfoRec; Obj: Pointer);
    GetRealDir: function (Obj: Pointer): String;
    MakeDir: procedure (Obj: Pointer);
    isUp: function (Obj: Pointer): Boolean;
    ChangeUp: procedure (var S: String; Obj: Pointer);
    ChangeRoot: procedure (Obj: Pointer);
    GetFullFlags: function (Obj: Pointer): Word;
    EditDescription: procedure (PF: PFileRec; Obj: Pointer);
    GetDirLength: procedure (PF: PFileRec; Obj: Pointer);
    GetParam: procedure (n: Byte; Obj: Pointer);
    end;

  TTempDriveVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    KillUse: procedure (Obj: Pointer);
    lChDir: procedure (ADir: String; Obj: Pointer);
    GetDir: function (Obj: Pointer): String;
    GetDirectory: function (const FileMask: String;
      var TotalInfo: TSize; Obj: Pointer): PCollection;
    CopyFiles: procedure (Files: PCollection; Own: PView;
       MoveMode: Boolean; Obj: Pointer);
    CopyFilesInto: procedure (Files: PCollection; Own: PView;
       MoveMode: Boolean; Obj: Pointer);
    EraseFiles: procedure (Files: PCollection; Obj: Pointer);
    UseFile: procedure (P: PFileRec; Command: Word; Obj: Pointer);
    GetFreeSpace: procedure (var S: String; Obj: Pointer);
    Disposable: function (Obj: Pointer): Boolean;
    GetRealName: function (Obj: Pointer): String;
    GetInternalName: function (Obj: Pointer): String;
    GetFull: procedure (var B; P: PFileRec; C, Sc: Word; Obj: Pointer);
    GetEmpty: procedure (var B; Sc: Word; Obj: Pointer);
    CalcLengthWithoutName: function (Obj: Pointer): Integer;
    CalcLength: function (Obj: Pointer): Integer;
    RereadDirectory: procedure (S: String; Obj: Pointer);
    MakeTop: procedure (var S: String; Obj: Pointer);
    GetDown: procedure (var B; C: Word; P: PFileRec; Obj: Pointer);
    HandleCommand: procedure (Command: Word; InfoPtr: Pointer;
       Obj: Pointer);
    GetDirInfo: procedure (var B: TDiskInfoRec; Obj: Pointer);
    GetRealDir: function (Obj: Pointer): String;
    MakeDir: procedure (Obj: Pointer);
    isUp: function (Obj: Pointer): Boolean;
    ChangeUp: procedure (var S: String; Obj: Pointer);
    ChangeRoot: procedure (Obj: Pointer);
    GetFullFlags: function (Obj: Pointer): Word;
    EditDescription: procedure (PF: PFileRec; Obj: Pointer);
    GetDirLength: procedure (PF: PFileRec; Obj: Pointer);
    GetParam: procedure (n: Byte; Obj: Pointer);
    end;

  TArcDriveVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    KillUse: procedure (Obj: Pointer);
    lChDir: procedure (ADir: String; Obj: Pointer);
    GetDir: function (Obj: Pointer): String;
    GetDirectory: function (const FileMask: String;
      var TotalInfo: TSize; Obj: Pointer): PCollection;
    CopyFiles: procedure (Files: PCollection; Own: PView;
       MoveMode: Boolean; Obj: Pointer);
    CopyFilesInto: procedure (Files: PCollection; Own: PView;
       MoveMode: Boolean; Obj: Pointer);
    EraseFiles: procedure (Files: PCollection; Obj: Pointer);
    UseFile: procedure (P: PFileRec; Command: Word; Obj: Pointer);
    GetFreeSpace: procedure (var S: String; Obj: Pointer);
    Disposable: function (Obj: Pointer): Boolean;
    GetRealName: function (Obj: Pointer): String;
    GetInternalName: function (Obj: Pointer): String;
    GetFull: procedure (var B; P: PFileRec; C, Sc: Word; Obj: Pointer);
    GetEmpty: procedure (var B; Sc: Word; Obj: Pointer);
    CalcLengthWithoutName: function (Obj: Pointer): Integer;
    CalcLength: function (Obj: Pointer): Integer;
    RereadDirectory: procedure (S: String; Obj: Pointer);
    MakeTop: procedure (var S: String; Obj: Pointer);
    GetDown: procedure (var B; C: Word; P: PFileRec; Obj: Pointer);
    HandleCommand: procedure (Command: Word; InfoPtr: Pointer;
       Obj: Pointer);
    GetDirInfo: procedure (var B: TDiskInfoRec; Obj: Pointer);
    GetRealDir: function (Obj: Pointer): String;
    MakeDir: procedure (Obj: Pointer);
    isUp: function (Obj: Pointer): Boolean;
    ChangeUp: procedure (var S: String; Obj: Pointer);
    ChangeRoot: procedure (Obj: Pointer);
    GetFullFlags: function (Obj: Pointer): Word;
    EditDescription: procedure (PF: PFileRec; Obj: Pointer);
    GetDirLength: procedure (PF: PFileRec; Obj: Pointer);
    GetParam: procedure (n: Byte; Obj: Pointer);
    end;

  TArvidDriveVMT = packed record
    NotForYou1: Integer;
    NotForYou2: Integer;
    NotForYou3: Integer;
    Done: procedure (DP: Integer; Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    KillUse: procedure (Obj: Pointer);
    lChDir: procedure (ADir: String; Obj: Pointer);
    GetDir: function (Obj: Pointer): String;
    GetDirectory: function (const FileMask: String;
      var TotalInfo: TSize; Obj: Pointer): PCollection;
    CopyFiles: procedure (Files: PCollection; Own: PView;
       MoveMode: Boolean; Obj: Pointer);
    CopyFilesInto: procedure (Files: PCollection; Own: PView;
       MoveMode: Boolean; Obj: Pointer);
    EraseFiles: procedure (Files: PCollection; Obj: Pointer);
    UseFile: procedure (P: PFileRec; Command: Word; Obj: Pointer);
    GetFreeSpace: procedure (var S: String; Obj: Pointer);
    Disposable: function (Obj: Pointer): Boolean;
    GetRealName: function (Obj: Pointer): String;
    GetInternalName: function (Obj: Pointer): String;
    GetFull: procedure (var B; P: PFileRec; C, Sc: Word; Obj: Pointer);
    GetEmpty: procedure (var B; Sc: Word; Obj: Pointer);
    CalcLengthWithoutName: function (Obj: Pointer): Integer;
    CalcLength: function (Obj: Pointer): Integer;
    RereadDirectory: procedure (S: String; Obj: Pointer);
    MakeTop: procedure (var S: String; Obj: Pointer);
    GetDown: procedure (var B; C: Word; P: PFileRec; Obj: Pointer);
    HandleCommand: procedure (Command: Word; InfoPtr: Pointer;
       Obj: Pointer);
    GetDirInfo: procedure (var B: TDiskInfoRec; Obj: Pointer);
    GetRealDir: function (Obj: Pointer): String;
    MakeDir: procedure (Obj: Pointer);
    isUp: function (Obj: Pointer): Boolean;
    ChangeUp: procedure (var S: String; Obj: Pointer);
    ChangeRoot: procedure (Obj: Pointer);
    GetFullFlags: function (Obj: Pointer): Word;
    EditDescription: procedure (PF: PFileRec; Obj: Pointer);
    GetDirLength: procedure (PF: PFileRec; Obj: Pointer);
    GetParam: procedure (n: Byte; Obj: Pointer);
    end;

const
  TEmptyObject_VMTSize = SizeOf(TEmptyObjectVMT) div 4-3;
  TObject_VMTSize = SizeOf(TObjectVMT) div 4-3;
  TRegExp_VMTSize = SizeOf(TRegExpVMT) div 4-3;
  TStream_VMTSize = SizeOf(TStreamVMT) div 4-3;
  TDosStream_VMTSize = SizeOf(TDosStreamVMT) div 4-3;
  TBufStream_VMTSize = SizeOf(TBufStreamVMT) div 4-3;
  TMemoryStream_VMTSize = SizeOf(TMemoryStreamVMT) div 4-3;
  TCollection_VMTSize = SizeOf(TCollectionVMT) div 4-3;
  TSortedCollection_VMTSize = SizeOf(TSortedCollectionVMT) div 4-3;
  TLineCollection_VMTSize = SizeOf(TLineCollectionVMT) div 4-3;
  TStringCollection_VMTSize = SizeOf(TStringCollectionVMT) div 4-3;
  TStrCollection_VMTSize = SizeOf(TStrCollectionVMT) div 4-3;
  TFilesCollection_VMTSize = SizeOf(TFilesCollectionVMT) div 4-3;
  TView_VMTSize = SizeOf(TViewVMT) div 4-3;
  TFrame_VMTSize = SizeOf(TFrameVMT) div 4-3;
  TScrollBar_VMTSize = SizeOf(TScrollBarVMT) div 4-3;
  TGroup_VMTSize = SizeOf(TGroupVMT) div 4-3;
  TWindow_VMTSize = SizeOf(TWindowVMT) div 4-3;
  TMenuView_VMTSize = SizeOf(TMenuViewVMT) div 4-3;
  TMenuBar_VMTSize = SizeOf(TMenuBarVMT) div 4-3;
  TMenuBox_VMTSize = SizeOf(TMenuBoxVMT) div 4-3;
  TMenuPopup_VMTSize = SizeOf(TMenuPopupVMT) div 4-3;
  TStatusLine_VMTSize = SizeOf(TStatusLineVMT) div 4-3;
  TDialog_VMTSize = SizeOf(TDialogVMT) div 4-3;
  TInputLine_VMTSize = SizeOf(TInputLineVMT) div 4-3;
  TButton_VMTSize = SizeOf(TButtonVMT) div 4-3;
  TCluster_VMTSize = SizeOf(TClusterVMT) div 4-3;
  TRadioButtons_VMTSize = SizeOf(TRadioButtonsVMT) div 4-3;
  TCheckBoxes_VMTSize = SizeOf(TCheckBoxesVMT) div 4-3;
  TMultiCheckBoxes_VMTSize = SizeOf(TMultiCheckBoxesVMT) div 4-3;
  TComboBox_VMTSize = SizeOf(TComboBoxVMT) div 4-3;
  TScroller_VMTSize = SizeOf(TScrollerVMT) div 4-3;
  TListViewer_VMTSize = SizeOf(TListViewerVMT) div 4-3;
  TListBox_VMTSize = SizeOf(TListBoxVMT) div 4-3;
  TStaticText_VMTSize = SizeOf(TStaticTextVMT) div 4-3;
  TParamText_VMTSize = SizeOf(TParamTextVMT) div 4-3;
  TLabel_VMTSize = SizeOf(TLabelVMT) div 4-3;
  THistoryViewer_VMTSize = SizeOf(THistoryViewerVMT) div 4-3;
  THistoryWindow_VMTSize = SizeOf(THistoryWindowVMT) div 4-3;
  THistory_VMTSize = SizeOf(THistoryVMT) div 4-3;
  TBackground_VMTSize = SizeOf(TBackgroundVMT) div 4-3;
  TDesktop_VMTSize = SizeOf(TDesktopVMT) div 4-3;
  TProgram_VMTSize = SizeOf(TProgramVMT) div 4-3;
  TApplication_VMTSize = SizeOf(TApplicationVMT) div 4-3;
  TDNApplication_VMTSize = SizeOf(TDNApplicationVMT) div 4-3;
  TUniWindow_VMTSize = SizeOf(TUniWindowVMT) div 4-3;
  TEditWindow_VMTSize = SizeOf(TEditWindowVMT) div 4-3;
  TXFileEditor_VMTSize = SizeOf(TXFileEditorVMT) div 4-3;
  TPercentGauge_VMTSize = SizeOf(TPercentGaugeVMT) div 4-3;
  TBarGauge_VMTSize = SizeOf(TBarGaugeVMT) div 4-3;
  TWhileView_VMTSize = SizeOf(TWhileViewVMT) div 4-3;
  TViewScroll_VMTSize = SizeOf(TViewScrollVMT) div 4-3;
  TFileViewer_VMTSize = SizeOf(TFileViewerVMT) div 4-3;
  TDrive_VMTSize = SizeOf(TDriveVMT) div 4-3;
  TFindDrive_VMTSize = SizeOf(TFindDriveVMT) div 4-3;
  TTempDrive_VMTSize = SizeOf(TTempDriveVMT) div 4-3;
  TArcDrive_VMTSize = SizeOf(TArcDriveVMT) div 4-3;
  TArvidDrive_VMTSize = SizeOf(TArvidDriveVMT) div 4-3;

implementation

end.
