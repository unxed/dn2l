unit _Editors;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Streams, _Collect, _Views, _Menus
  ;

type
  PUniWindow = ^TUniWindow;
  TUniWindow = object(TWindow)
    {function GetPalette: PPalette; virtual;}
    function MakeScrollBar(AOptions: Word): PScrollBar;
    {procedure InitFrame; virtual;}
    {function ReactOnCmd: Boolean; virtual;}
    end;

  PXFileEditor = ^TXFileEditor;
  TXFileEditor = object(TView)
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
    RegExp: Pointer {PRegExp};
    constructor Init(var Bounds: TRect;
         AHScrollBar, AVScrollBar: PScrollBar; var FileName: String);
    constructor Load(var S: TStream);
    {destructor Done; virtual;}
    procedure Store(var S: TStream);
    {procedure Awaken; virtual;}
    procedure DoHighlite(var B; const S: LongString; const Attr: String);
    {procedure HandleEvent(var Event: TEvent); virtual;}
    {procedure Draw; virtual;}
    {function Valid(Command: Word): Boolean; virtual;}
    {function GetPalette: PPalette; virtual;}
    function GetLine(Index: LongInt): LongString;
    function GetSelection: PCollection;
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    function ValidBlock: Boolean;
    procedure CalcMenu;
    function Search(StartX, StartY: Word): Boolean;
    procedure InsertBlock(ABlock: PCollection; SaveUndo: Boolean);
    procedure ModifyLine(Index: LongInt; S: LongString;
         DelSpaces: Boolean);
    procedure SetLimits;
    {procedure ChangeBounds(var R: TRect); virtual;}
    procedure ScrollTo(DeltaX, DeltaY: LongInt);
    function LimitX: LongInt;
    function LimitY: LongInt;
    procedure StoreUndoInfo(What: Word; Where: TPoint; var Info);
    function HandleCommand(var Event: TEvent): Boolean; virtual;
    {function KeyMapConvertStr(S: LongString; toAscii: Boolean): LongString;}
    procedure KeyMapAtInsert(N: LongInt; P: PLongString);
    procedure KeyMapAtReplace(N: LongInt; P: PLongString);
    end;

  PEditWindow = ^TEditWindow;
  TEditWindow = object(TUniWindow)
    AInfo: Pointer {PInfoLine};
    ABookLine: Pointer {PBookmarkLine};
    Intern: PXFileEditor;
    MenuBar: PMenuBar;
    UpMenu: PMenu;
    ModalEnd: Boolean;
    constructor Init(R: TRect; FileName: String);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    {function Execute: Word; virtual;}
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    end;

implementation

uses
  _DNFuncs
  ;

function TUniWindow.MakeScrollBar(AOptions: Word): PScrollBar;
  begin
  Result := PScrollBar(_TUniWindow^.MakeScrollBar(AOptions, @Self));
  end;

constructor TXFileEditor.Init(var Bounds: TRect;
     AHScrollBar, AVScrollBar: PScrollBar; var FileName: String);
  begin
  _TXFileEditor^.Init(Bounds, _Model1.PScrollBar(AHScrollBar),
     _Model1.PScrollBar(AVScrollBar), FileName, nil, @Self);
  end;

constructor TXFileEditor.Load(var S: TStream);
  begin
  _TXFileEditor^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TXFileEditor.Store(var S: TStream);
  begin
  _TXFileEditor^.Store(_Model1.TStream(S), @Self);
  end;

procedure TXFileEditor.DoHighlite(var B; const S: LongString;
     const Attr: String);
  begin
  _TXFileEditor^.DoHighlite(B, S, Attr, @Self);
  end;

function TXFileEditor.GetLine(Index: LongInt): LongString;
  begin
  Result := _TXFileEditor^.GetLine(Index, @Self);
  end;

function TXFileEditor.GetSelection: PCollection;
  begin
  Result := PCollection(_TXFileEditor^.GetSelection(@Self));
  end;

function TXFileEditor.ValidBlock: Boolean;
  begin
  Result := _TXFileEditor^.ValidBlock(@Self);
  end;

procedure TXFileEditor.CalcMenu;
  begin
  _TXFileEditor^.CalcMenu(@Self);
  end;

function TXFileEditor.Search(StartX, StartY: Word): Boolean;
  begin
  Result := _TXFileEditor^.Search(StartX, StartY, @Self);
  end;

procedure TXFileEditor.InsertBlock(ABlock: PCollection; SaveUndo: Boolean);
  begin
  _TXFileEditor^.InsertBlock(_Model1.PCollection(ABlock), SaveUndo, @Self);
  end;

procedure TXFileEditor.ModifyLine(Index: LongInt; S: LongString;
     DelSpaces: Boolean);
  begin
  _TXFileEditor^.ModifyLine(Index, S, DelSpaces, @Self);
  end;

procedure TXFileEditor.SetLimits;
  begin
  _TXFileEditor^.SetLimits(@Self);
  end;

procedure TXFileEditor.ScrollTo(DeltaX, DeltaY: LongInt);
  begin
  _TXFileEditor^.ScrollTo(DeltaX, DeltaY, @Self);
  end;

function TXFileEditor.LimitX: LongInt;
  begin
  Result := _TXFileEditor^.LimitX(@Self);
  end;

function TXFileEditor.LimitY: LongInt;
  begin
  Result := _TXFileEditor^.LimitY(@Self);
  end;

procedure TXFileEditor.StoreUndoInfo(What: Word; Where: TPoint; var Info);
  begin
  _TXFileEditor^.StoreUndoInfo(What, Where, Info, @Self);
  end;

function TXFileEditor.HandleCommand(var Event: TEvent): Boolean;
  assembler; {&Frame-}
asm
end;

{function TXFileEditor.KeyMapConvertStr(S: LongString; toAscii: Boolean)
  : LongString;
  begin
  Result := _TXFileEditor^.KeyMapConvertStr(S, toAscii, @Self);
  end;}

procedure TXFileEditor.KeyMapAtInsert(N: LongInt; P: PLongString);
  begin
  _TXFileEditor^.KeyMapAtInsert(N, P, @Self);
  end;

procedure TXFileEditor.KeyMapAtReplace(N: LongInt; P: PLongString);
  begin
  _TXFileEditor^.KeyMapAtReplace(N, P, @Self);
  end;

constructor TEditWindow.Init(R: TRect; FileName: String);
  begin
  _TEditWindow^.Init(R, FileName, nil, @Self);
  end;

constructor TEditWindow.Load(var S: TStream);
  begin
  _TEditWindow^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TEditWindow.Store(var S: TStream);
  begin
  _TEditWindow^.Store(_Model1.TStream(S), @Self);
  end;

end.
