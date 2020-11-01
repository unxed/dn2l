unit _Dialogs;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Streams, _Collect, _Views
  ;

type
  PDialog = ^TDialog;
  TDialog = object(TWindow)
    DirectLink: array [1..9] of PView;
    constructor Init(var Bounds: TRect; const ATitle: String);
    constructor Load(var S: TStream);
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    {function Valid(Command: Word): Boolean; virtual;}
    procedure Store(var S: TStream);
    end;

  PInputline = ^TInputLine;
  TInputLine = object(TView)
    Data: AnsiString;
    MaxLen: LongInt;
    CurPos: LongInt;
    FirstPos: LongInt;
    SelStart: LongInt;
    SelEnd: LongInt;
    Validator: Pointer {PValidator};
    LC, RC: Char;
    C: array [1..4] of Byte;
    constructor Init(var Bounds: TRect; AMaxLen: LongInt);
    constructor Load(var S: TStream);
    {destructor Done; virtual;}
    {function DataSize: Word; virtual;}
    {procedure Draw; virtual;}
    {procedure GetData(var Rec); virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure SelectAll(Enable: Boolean);
    {procedure SetData(var Rec); virtual;}
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    procedure SetValidator(AValid: Pointer {PValidator});
    procedure Store(var S: TStream);
    {function Valid(Command: Word): Boolean; virtual;}
    function CanScroll(Delta: Integer): Boolean;
    end;

  PButton = ^TButton;
  TButton = object(TView)
    Title: PString;
    Command: AWord;
    Flags: Byte;
    AmDefault: Boolean;
    constructor Init(var Bounds: TRect; const ATitle: String;
         ACommand: Word; AFlags: Word);
    constructor Load(var S: TStream);
    {destructor Done; virtual;}
    {procedure Draw; virtual;}
    procedure DrawState(Down: Boolean);
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure MakeDefault(Enable: Boolean);
    procedure Press; virtual;
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    procedure Store(var S: TStream);
    end;

  PCluster = ^TCluster;
  TCluster = object(TView)
    Value: LongInt;
    Sel: AInt;
    EnableMask: LongInt;
    Strings: TStringCollection;
    constructor Init(var Bounds: TRect; AStrings: PSItem);
    constructor Load(var S: TStream);
    {destructor Done; virtual;}
    function ButtonState(Item: Integer): Boolean;
    {function DataSize: Word; virtual;}
    procedure DrawBox(const Icon: String; Marker: Char);
    procedure DrawMultiBox(const Icon, Marker: String);
    {procedure GetData(var Rec); virtual;}
    {function GetHelpCtx: Word; virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function Mark(Item: Integer): Boolean; virtual;
    function MultiMark(Item: Integer): Byte; virtual;
    procedure Press(Item: Integer); virtual;
    procedure MovedTo(Item: Integer); virtual;
    procedure SetButtonState(AMask: LongInt; Enable: Boolean);
    {procedure SetData(var Rec); virtual;}
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    procedure Store(var S: TStream);
    end;

  PRadioButtons = ^TRadioButtons;
  TRadioButtons = object(TCluster)
    {procedure Draw; virtual;}
    {function Mark(Item: Integer): Boolean; virtual;}
    {procedure MovedTo(Item: Integer); virtual;}
    {procedure Press(Item: Integer); virtual;}
    {procedure SetData(var Rec); virtual;}
    end;

  PCheckBoxes = ^TCheckBoxes;
  TCheckBoxes = object(TCluster)
    {procedure Draw; virtual;}
    {function Mark(Item: Integer): Boolean; virtual;}
    {procedure Press(Item: Integer); virtual;}
    end;

  PMultiCheckBoxes = ^TMultiCheckBoxes;
  TMultiCheckBoxes = object(TCluster)
    SelRange: Byte;
    Flags: AWord;
    States: PString;
    constructor Init(var Bounds: TRect; AStrings: PSItem;
         ASelRange: Byte; AFlags: Word; const AStates: String);
    constructor Load(var S: TStream);
    {destructor Done; virtual;}
    {function DataSize: Word; virtual;}
    {procedure Draw; virtual;}
    {procedure GetData(var Rec); virtual;}
    {function MultiMark(Item: Integer): Byte; virtual;}
    {procedure Press(Item: Integer); virtual;}
    {procedure SetData(var Rec); virtual;}
    procedure Store(var S: TStream);
    end;

  PScroller = ^TScroller;
  TScroller = object(TView)
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    Delta: TPoint;
    Limit: TPoint;
    DrawLock: Byte;
    DrawFlag: Boolean;
    constructor Init(var Bounds: TRect;
         AHScrollBar, AVScrollBar: PScrollBar);
    constructor Load(var S: TStream);
    {procedure ChangeBounds(var Bounds: TRect); virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure ScrollDraw; virtual;
    procedure ScrollTo(X, Y: LongInt);
    procedure SetLimit(X, Y: LongInt);
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    procedure Store(var S: TStream);
    end;

  PListViewer = ^TListViewer;
  TListViewer = object(TView)
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    NumCols: LongInt;
    TopItem: LongInt;
    Focused: LongInt;
    Range: LongInt;
    constructor Init(var Bounds: TRect; ANumCols: LongInt;
         AHScrollBar, AVScrollBar: PScrollBar);
    constructor Load(var S: TStream);
    {procedure ChangeBounds(var Bounds: TRect); virtual;}
    {procedure Draw; virtual;}
    procedure FocusItem(Item: LongInt); virtual;
    {function GetPalette: PPalette; virtual;}
    function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    function IsSelected(Item: LongInt): Boolean; virtual;
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure SelectItem(Item: LongInt); virtual;
    procedure SetRange(ARange: LongInt);
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    procedure Store(var S: TStream);
    procedure FocusItemNum(Item: LongInt); virtual;
    end;

  PListBox = ^TListBox;
  TListBox = object(TListViewer)
    List: PCollection;
    constructor Init(var Bounds: TRect; ANumCols: Word;
         AScrollBar: PScrollBar);
    constructor Load(var S: TStream);
    {function DataSize: Word; virtual;}
    {procedure GetData(var Rec); virtual;}
    {function GetText(Item: LongInt; MaxLen: Integer): String; virtual;}
    procedure NewLisT(AList: PCollection); virtual;
    {procedure SetData(var Rec); virtual;}
    procedure Store(var S: TStream);
    end;

  PStaticText = ^TStaticText;
  TStaticText = object(TView)
    Text: PString;
    constructor Init(var Bounds: TRect; const AText: String);
    constructor Load(var S: TStream);
    {destructor Done; virtual;}
    {procedure Draw; virtual;}
    {function GetPalette: PPalette; virtual;}
    procedure GetText(var S: String); virtual;
    procedure Store(var S: TStream);
    end;

  PParamText = ^TParamText;
  TParamText = object(TStaticText)
    ParamCount: AInt;
    ParamList: Pointer;
    constructor Init(var Bounds: TRect; const AText: String;
         AParamCount: AInt);
    constructor Load(var S: TStream);
    {function DataSize: Word; virtual;}
    {procedure GetText(var S: String); virtual;}
    {procedure SetData(var Rec); virtual;}
    procedure Store(var S: TStream);
    end;

  PLabel = ^TLabel;
  TLabel = object(TStaticText)
    Link: PView;
    Light: Boolean;
    constructor Init(var Bounds: TRect; const AText: String; ALink: PView);
    constructor Load(var S: TStream);
    {procedure Draw; virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure Store(var S: TStream);
    end;

  PHistoryViewer = ^THistoryViewer;
  THistoryViewer = object(TListViewer)
    HistoryId: AWord;
    SearchPos: AWord;
    constructor Init(var Bounds: TRect;
         AHScrollBar, AVScrollBar: PScrollBar; AHistoryId: AWord);
    {function GetPalette: PPalette; virtual;}
    {function GetText(Item: LongInt; MaxLen: Integer): String; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function HistoryWidth: Integer;
    end;

  PHistoryWindow = ^THistoryWindow;
  THistoryWindow = object(TWindow)
    Viewer: PListViewer;
    constructor Init(var Bounds: TRect; HistoryId: AWord);
    {function GetPalette: PPalette; virtual;}
    function GetSelection: String; virtual;
    procedure InitViewer(HistoryId: AWord); virtual;
    end;

  PHistory = ^THistory;
  THistory = object(TView)
    Link: PInputline;
    HistoryId: AWord;
    constructor Init(var Bounds: TRect; ALink: PInputline;
         AHistoryId: AWord);
    constructor Load(var S: TStream);
    {procedure Draw; virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function InitHistoryWindow(var Bounds: TRect): PHistoryWindow; virtual;
    procedure RecordHistory(const S: String); virtual;
    procedure Store(var S: TStream);
    end;

  PComboBox = ^TComboBox;
  TComboBox = object(TView)
(*  Selected: Word; // текущий номер варианта (нумерация от 1)
    Count: Word; { не отрывать от Selected! См. Load,Store}
    Menu: PMenu;
    Items: array[1..10] of PMenuItem; // прямые ссылки в меню
    constructor Init(var Bounds: TRect; AStrings: PSItem);
    procedure BuildMenu(AStrings: PSItem);
    destructor Done; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function DataSize: Word; virtual;
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); *)
    end;

implementation

uses
  _DNFuncs
  ;

constructor TDialog.Init(var Bounds: TRect; const ATitle: String);
  begin
  _TDialog^.Init(Bounds, ATitle, nil, @Self);
  end;

constructor TDialog.Load(var S: TStream);
  begin
  _TDialog^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TDialog.Store(var S: TStream);
  begin
  _TDialog^.Store(_Model1.TStream(S), @Self);
  end;

constructor TInputLine.Init(var Bounds: TRect; AMaxLen: LongInt);
  begin
  _TInputLine^.Init(Bounds, AMaxLen, nil, @Self);
  end;

constructor TInputLine.Load(var S: TStream);
  begin
  _TInputLine^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TInputLine.SelectAll(Enable: Boolean);
  begin
  _TInputLine^.SelectAll(Enable, @Self);
  end;

procedure TInputLine.SetValidator(AValid: Pointer {PValidator});
  begin
  _TInputLine^.SetValidator(AValid, @Self);
  end;

procedure TInputLine.Store(var S: TStream);
  begin
  _TInputLine^.Store(_Model1.TStream(S), @Self);
  end;

function TInputLine.CanScroll(Delta: Integer): Boolean;
  begin
  Result := _TInputLine^.CanScroll(Delta, @Self);
  end;

constructor TButton.Init(var Bounds: TRect; const ATitle: String;
     ACommand: Word; AFlags: Word);
  begin
  _TButton^.Init(Bounds, ATitle, ACommand, AFlags, nil, @Self);
  end;

constructor TButton.Load(var S: TStream);
  begin
  _TButton^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TButton.DrawState(Down: Boolean);
  begin
  _TButton^.DrawState(Down, @Self);
  end;

procedure TButton.MakeDefault(Enable: Boolean);
  begin
  _TButton^.MakeDefault(Enable, @Self);
  end;

procedure TButton.Press;
  assembler; {&Frame-}
asm
end;

procedure TButton.Store(var S: TStream);
  begin
  _TButton^.Store(_Model1.TStream(S), @Self);
  end;

constructor TCluster.Init(var Bounds: TRect; AStrings: PSItem);
  begin
  _TCluster^.Init(Bounds, AStrings, nil, @Self);
  end;

constructor TCluster.Load(var S: TStream);
  begin
  _TCluster^.Load(_Model1.TStream(S), nil, @Self);
  end;

function TCluster.ButtonState(Item: Integer): Boolean;
  begin
  Result := _TCluster^.ButtonState(Item, @Self);
  end;

procedure TCluster.DrawBox(const Icon: String; Marker: Char);
  begin
  _TCluster^.DrawBox(Icon, Marker, @Self);
  end;

procedure TCluster.DrawMultiBox(const Icon, Marker: String);
  begin
  _TCluster^.DrawMultiBox(Icon, Marker, @Self);
  end;

function TCluster.Mark(Item: Integer): Boolean;
  assembler; {&Frame-}
asm
end;

function TCluster.MultiMark(Item: Integer): Byte;
  assembler; {&Frame-}
asm
end;

procedure TCluster.Press(Item: Integer);
  assembler; {&Frame-}
asm
end;

procedure TCluster.MovedTo(Item: Integer);
  assembler; {&Frame-}
asm
end;

procedure TCluster.SetButtonState(AMask: LongInt; Enable: Boolean);
  begin
  _TCluster^.SetButtonState(AMask, Enable, @Self);
  end;

procedure TCluster.Store(var S: TStream);
  begin
  _TCluster^.Store(_Model1.TStream(S), @Self);
  end;

constructor TMultiCheckBoxes.Init(var Bounds: TRect; AStrings: PSItem;
     ASelRange: Byte; AFlags: Word; const AStates: String);
  begin
  _TMultiCheckBoxes^.Init(Bounds, AStrings, ASelRange, AFlags, AStates,
     nil, @Self);
  end;

constructor TMultiCheckBoxes.Load(var S: TStream);
  begin
  _TMultiCheckBoxes^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TMultiCheckBoxes.Store(var S: TStream);
  begin
  _TMultiCheckBoxes^.Store(_Model1.TStream(S), @Self);
  end;

constructor TScroller.Init(var Bounds: TRect;
     AHScrollBar, AVScrollBar: PScrollBar);
  begin
  _TScroller^.Init(Bounds, _Model1.PScrollBar(AHScrollBar),
     _Model1.PScrollBar(AVScrollBar), nil, @Self);
  end;

constructor TScroller.Load(var S: TStream);
  begin
  _TScroller^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TScroller.ScrollDraw;
  assembler; {&Frame-}
asm
end;

procedure TScroller.ScrollTo(X, Y: LongInt);
  begin
  _TScroller^.ScrollTo(X, Y, @Self);
  end;

procedure TScroller.SetLimit(X, Y: LongInt);
  begin
  _TScroller^.SetLimit(X, Y, @Self);
  end;

procedure TScroller.Store(var S: TStream);
  begin
  _TScroller^.Store(_Model1.TStream(S), @Self);
  end;

constructor TListViewer.Init(var Bounds: TRect; ANumCols: LongInt;
     AHScrollBar, AVScrollBar: PScrollBar);
  begin
  _TListViewer^.Init(Bounds, ANumCols, _Model1.PScrollBar(AHScrollBar),
     _Model1.PScrollBar(AVScrollBar), nil, @Self);
  end;

constructor TListViewer.Load(var S: TStream);
  begin
  _TListViewer^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TListViewer.FocusItem(Item: LongInt);
  assembler; {&Frame-}
asm
end;

function TListViewer.GetText(Item: LongInt; MaxLen: Integer): String;
  assembler; {&Frame-}
asm
end;

function TListViewer.IsSelected(Item: LongInt): Boolean;
  assembler; {&Frame-}
asm
end;

procedure TListViewer.SelectItem(Item: LongInt);
  assembler; {&Frame-}
asm
end;

procedure TListViewer.SetRange(ARange: LongInt);
  begin
  _TListViewer^.SetRange(ARange, @Self);
  end;

procedure TListViewer.Store(var S: TStream);
  begin
  _TListViewer^.Store(_Model1.TStream(S), @Self);
  end;

procedure TListViewer.FocusItemNum(Item: LongInt);
  assembler; {&Frame-}
asm
end;

constructor TListBox.Init(var Bounds: TRect; ANumCols: Word;
     AScrollBar: PScrollBar);
  begin
  _TListBox^.Init(Bounds, ANumCols, _Model1.PScrollBar(AScrollBar), nil,
     @Self);
  end;

constructor TListBox.Load(var S: TStream);
  begin
  _TListBox^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TListBox.NewLisT(AList: PCollection);
  assembler; {&Frame-}
asm
end;

procedure TListBox.Store(var S: TStream);
  begin
  _TListBox^.Store(_Model1.TStream(S), @Self);
  end;

constructor TStaticText.Init(var Bounds: TRect; const AText: String);
  begin
  _TStaticText^.Init(Bounds, AText, nil, @Self);
  end;

constructor TStaticText.Load(var S: TStream);
  begin
  _TStaticText^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TStaticText.GetText(var S: String);
  assembler; {&Frame-}
asm
end;

procedure TStaticText.Store(var S: TStream);
  begin
  _TStaticText^.Store(_Model1.TStream(S), @Self);
  end;

constructor TParamText.Init(var Bounds: TRect; const AText: String;
     AParamCount: AInt);
  begin
  _TParamText^.Init(Bounds, AText, AParamCount, nil, @Self);
  end;

constructor TParamText.Load(var S: TStream);
  begin
  _TParamText^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TParamText.Store(var S: TStream);
  begin
  _TParamText^.Store(_Model1.TStream(S), @Self);
  end;

constructor TLabel.Init(var Bounds: TRect; const AText: String;
     ALink: PView);
  begin
  _TLabel^.Init(Bounds, AText, _Model1.PView(ALink), nil, @Self);
  end;

constructor TLabel.Load(var S: TStream);
  begin
  _TLabel^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TLabel.Store(var S: TStream);
  begin
  _TLabel^.Store(_Model1.TStream(S), @Self);
  end;

constructor THistoryViewer.Init(var Bounds: TRect;
     AHScrollBar, AVScrollBar: PScrollBar; AHistoryId: AWord);
  begin
  _THistoryViewer^.Init(Bounds, _Model1.PScrollBar(AHScrollBar),
     _Model1.PScrollBar(AVScrollBar), AHistoryId, nil, @Self);
  end;

function THistoryViewer.HistoryWidth: Integer;
  begin
  Result := _THistoryViewer^.HistoryWidth(@Self);
  end;

constructor THistoryWindow.Init(var Bounds: TRect; HistoryId: AWord);
  begin
  _THistoryWindow^.Init(Bounds, HistoryId, nil, @Self);
  end;

function THistoryWindow.GetSelection: String;
  assembler; {&Frame-}
asm
end;

procedure THistoryWindow.InitViewer(HistoryId: AWord);
  assembler; {&Frame-}
asm
end;

constructor THistory.Init(var Bounds: TRect; ALink: PInputline;
     AHistoryId: AWord);
  begin
  _THistory^.Init(Bounds, _Model1.PInputline(ALink), AHistoryId, nil,
     @Self);
  end;

constructor THistory.Load(var S: TStream);
  begin
  _THistory^.Load(_Model1.TStream(S), nil, @Self);
  end;

function THistory.InitHistoryWindow(var Bounds: TRect): PHistoryWindow;
  assembler; {&Frame-}
asm
end;

procedure THistory.RecordHistory(const S: String);
  assembler; {&Frame-}
asm
end;

procedure THistory.Store(var S: TStream);
  begin
  _THistory^.Store(_Model1.TStream(S), @Self);
  end;

end.
