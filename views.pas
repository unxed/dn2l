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
{AK155 = Alexey Korop, 2:461/155@fidonet}

{$I STDEFINE.INC}

unit Views;

{-----------------------------------------------------}
{ This module is based on Turbo Vision Views Unit     }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}

interface

uses
  Collect, Defines, Objects2, Streams, Drivers, Memory, xTime, Commands
  ;

const

  { TView State masks }

  sfVisible = $0001;
  sfCursorVis = $0002;
  sfCursorIns = $0004;
  sfShadow = $0008;
  sfActive = $0010;
  sfSelected = $0020;
  sfFocused = $0040;
  sfDragging = $0080;
  sfDisabled = $0100;
  sfModal = $0200;
  sfDefault = $0400;
  sfExposed = $0800;
  sfTagged = $1000; {<Views.004>}

  { TView Option masks }

  ofSelectable = $0001;
  ofTopSelect = $0002;
  ofFirstClick = $0004;
  ofFramed = $0008;
  ofPreProcess = $0010;
  ofPostProcess = $0020;
  ofBuffered = $0040;
  ofTileable = $0080;
  ofCenterX = $0100;
  ofCenterY = $0200;
  ofCentered = $0300;
  ofValidate = $0400;
  ofSecurity = $0800;
  ofVersion = $3000;
  ofVersion10 = $0000;
  ofVersion20 = $1000;

  { TView GrowMode masks }

  gfGrowLoX = $01;
  gfGrowLoY = $02;
  gfGrowHiX = $04;
  gfGrowHiY = $08;
  gfGrowAll = $0F;
  gfGrowRel = $10;

  { TView DragMode masks }

  dmDragMove = $01;
  dmDragGrow = $02;
  dmLimitLoX = $10;
  dmLimitLoY = $20;
  dmLimitHiX = $40;
  dmLimitHiY = $80;
  dmLimitAll = $F0;

  { TView Help context codes }

  hcNoContext = 0;
  hcDragging = 1;

  { TScrollBar part codes }

  sbLeftArrow = 0;
  sbRightArrow = 1;
  sbPageLeft = 2;
  sbPageRight = 3;
  sbUpArrow = 4;
  sbDownArrow = 5;
  sbPageUp = 6;
  sbPageDown = 7;
  sbIndicator = 8;

  { TScrollBar options for TWindow.StandardScrollBar }

  sbHorizontal = $0000;
  sbVertical = $0001;
  sbHandleKeyboard = $0002;

  { TWindow Flags masks }

  wfMove = $01;
  wfGrow = $02;
  wfClose = $04;
  wfZoom = $08;
  wfMaxi = $10;

  { TWindow number constants }

  wnNoNumber = 255;

  { TWindow palette entries }

  wpBlueWindow = 0;
  wpCyanWindow = 1;
  wpGrayWindow = 2;

  { Color palettes }

  CFrame = #1#1#2#2#3;
  CScrollBar = #4#5#5;
  CScroller = #6#7;
  CListViewer = #26#26#27#28#29;

  CBlueWindow = #8#9#10#11#12#13#14#15;
  CCyanWindow = #16#17#18#19#20#21#22#23;
  CGrayWindow = #24#25#26#27#28#29#30#31;

  { TDrawBuffer maximum view width }

  MaxViewWidth = 256;

type

  { Command sets }

  PCommandSet = ^TCommandSet;
  TCommandSet = set of Byte;

  { Color palette type }

  PPalette = ^TPalette;
  TPalette = String;

  { TDrawBuffer, buffer used by draw methods }

  TDrawBuffer = array[0..MaxViewWidth-1] of AWord;

  { TView object Pointer }

  PView = ^TView;

  { TGroup object Pointer }

  PGroup = ^TGroup;

  { TView object }

  TView = object(TObject)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
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
      {AK155 Очищать мышиное событие, не обработанное членами группы.
        Введение этого флага вызвано тем, что слишком во многих случаях
      члены группы не очищают событие с принадлежащими им координатами.
      Например, evMouseDown на рамке окна (вне кнопок) не очищается.
      А хотелось бы, чтобы на уровне Desktop после HandleEvent всех
      окон сохранялось только такое событие, координаты которого
      лежат вне всех окон.
      }
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Awaken; virtual;
    procedure BlockCursor;
    procedure CalcBounds(var Bounds: TRect; Delta: TPoint); virtual;
    procedure ChangeBounds(var Bounds: TRect); virtual;
    procedure ClearEvent(var Event: TEvent);
    function CommandEnabled(Command: Word): Boolean;
    function DataSize: Word; virtual;
    procedure DisableCommands(Commands: TCommandSet);
    procedure DragView(var Event: TEvent; Mode: Byte; var Limits: TRect;
        MinSize, MaxSize: TPoint);
    procedure Draw; virtual;
    procedure DrawView;
    procedure EnableCommands(Commands: TCommandSet);
    procedure EndModal(Command: Word); virtual;
    function EventAvail: Boolean;
    function Execute: Word; virtual;
    function Exposed: Boolean;
    function Focus: Boolean;
    procedure GetBounds(var Bounds: TRect);
    procedure GetClipRect(var Clip: TRect);
    function GetColor(Color: Word): Word;
    procedure GetCommands(var Commands: TCommandSet);
    procedure GetData(var Rec); virtual;
    procedure GetEvent(var Event: TEvent); virtual;
    procedure GetExtent(var Extent: TRect);
    function GetHelpCtx: Word; virtual;
    function GetPalette: PPalette; virtual;
    procedure GetPeerViewPtr(var S: TStream; var P);
    function GetState(AState: Word): Boolean;
    procedure GrowTo(X, Y: LongInt);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Hide;
    procedure HideCursor;
    procedure KeyEvent(var Event: TEvent);
    procedure Locate(var Bounds: TRect);
    procedure MakeFirst;
    procedure MakeGlobal(Source: TPoint; var Dest: TPoint);
    procedure MakeLocal(Source: TPoint; var Dest: TPoint);
    function MouseEvent(var Event: TEvent; Mask: Word): Boolean;
    function MouseInView(Mouse: TPoint): Boolean;
    procedure MoveTo(X, Y: LongInt);
    function NextView: PView;
    procedure NormalCursor;
    function Prev: PView;
    function PrevView: PView;
    procedure PutEvent(var Event: TEvent); virtual;
    procedure PutInFrontOf(Target: PView);
    procedure PutPeerViewPtr(var S: TStream; P: PView);
    procedure Select;
    procedure SetBounds(var Bounds: TRect);
    procedure SetCommands(Commands: TCommandSet);
    {DataCompBoy
    procedure   SetCmdState(Commands: TCommandSet; Enable: Boolean);
}
    procedure SetCursor(X, Y: LongInt);
    procedure SetData(var Rec); virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Show;
    procedure ShowCursor;
    procedure SizeLimits(var Min, Max: TPoint); virtual;
    procedure Store(var S: TStream);
    function TopView: PView;
    function Valid(Command: Word): Boolean; virtual;
    procedure WriteBuf(X, Y, W, H: Integer; var Buf);
    procedure WriteChar(X, Y: Integer; C: Char; Color: Byte;
        Count: Integer);
    procedure WriteLine(X, Y, W, H: Integer; var Buf);
    procedure WriteStr(X, Y: Integer; Str: String; Color: Byte);
    procedure Update; virtual;
    function MenuEnabled(Command: Word): Boolean; {-$VOL} {<Views.003>}
    //  private
    procedure DrawCursor;
    procedure DrawHide(LastView: PView);
    procedure DrawShow(LastView: PView);
    procedure DrawUnderRect(var R: TRect; LastView: PView);
    procedure DrawUnderView(DoShadow: Boolean; LastView: PView);
    procedure ResetCursor; virtual;
    end;

  { TFrame types }

  TTitleStr = String;

  { TFrame object }

  { Palette layout }
  { 1 = Passive frame }
  { 2 = Passive title }
  { 3 = Active frame }
  { 4 = Active title }
  { 5 = Icons }

  PFrame = ^TFrame;
  TFrame = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    FrameMode: Word;
    constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure FrameLine(var FrameBuf; Y, N: Integer; Color: Byte); virtual;
    function GetTitleWidth: integer; virtual;
    end;

  { ScrollBar characters }

  TScrollChars = array[0..4] of Char;

  { TScrollBar object }

  { Palette layout }
  { 1 = Page areas }
  { 2 = Arrows }
  { 3 = Indicator }

  PScrollBar = ^TScrollBar;
  TScrollBar = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Value: LongInt;
    Min: LongInt;
    Max: LongInt;
    PgStep: LongInt;
    ArStep: LongInt;
    Step: Longint;
    ForceScroll: Boolean;
      { AK155 06-02-2006.
        Этот флаг устанавливается, если мышью нажимать на стрелочки
      скроллбара, и сбрасывается после посылки cmScrollBarChanged.
      Смысл - рекомендация тому, кто обрабатывает cmScrollBarChanged,
      сразу же скроллировать изображение, а не бессмысленно гнать
      курсор по экрану. Если бы пользователь хотел переместить
      курсор в пределах экрана, он бы ткнул мышью, куда ему надо,
      а не брался за стрелочки скроллбара.
        Это флаг можно также установить снаружи перед вызовом SetValue,
      которое в концце концов приведёт к посылке cmScrollBarChanged.}
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure ScrollDraw; virtual;
    function ScrollStep(Part: LongInt): LongInt; virtual;
    procedure SetParams(AValue, AMin, AMax, APgStep, AArStep: LongInt);
    procedure SetRange(AMin, AMax: LongInt);
    procedure SetStep(APgStep, AArStep: LongInt);
    procedure SetValue(AValue: LongInt);
    procedure Store(var S: TStream);
  private
    Chars: TScrollChars;
    procedure DrawPos(Pos: LongInt);
    function GetPos: LongInt;
    function GetSize: LongInt;
    end;

  PMyScrollBar = ^TMyScrollBar;
  TMyScrollBar = object(TScrollBar)
    procedure Draw; virtual;
  private
    procedure DrawPos(Pos: LongInt);
    end;

  { Video buffer }

  PVideoBuf = ^TVideoBuf;
  TVideoBuf = array[0..3999] of AWord;

  { Selection modes }

  SelectMode = (NormalSelect, EnterSelect, LeaveSelect);

  { TGroup object }

  TGroup = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Last: PView;
    Current: PView;
    Phase: (phFocused, phPreProcess, phPostProcess);
    Buffer: PVideoBuf;
    Clip: TRect;
    TaggedSubviewCount: Longint; {<Views.004>}
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Awaken; virtual;
    procedure ChangeBounds(var Bounds: TRect); virtual;
    function DataSize: Word; virtual;
    procedure Delete(P: PView);
    procedure Draw; virtual;
    procedure EndModal(Command: Word); virtual;
    procedure EventError(var Event: TEvent); virtual;
    function ExecView(P: PView): Word;
    function Execute: Word; virtual;
    function First: PView;
    function FirstThat(P: Pointer): PView;
    function FocusNext(Forwards: Boolean): Boolean;
    procedure ForEach(P: Pointer);
    procedure GetData(var Rec); virtual;
    function GetHelpCtx: Word; virtual;
    procedure GetSubViewPtr(var S: TStream; var P);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Insert(P: PView);
    procedure InsertBefore(P, Target: PView);
    procedure Lock;
    procedure PutSubViewPtr(var S: TStream; P: PView);
    procedure Redraw; virtual;
    procedure SelectNext(Forwards: Boolean);
    procedure SetData(var Rec); virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);
    procedure UnLock;
    function Valid(Command: Word): Boolean; virtual;
    procedure FreeBuffer;
    procedure GetBuffer;
    procedure InsertView(P, Target: PView);
    procedure SetCurrent(P: PView; Mode: SelectMode);
  private
    LockFlag: Byte;
    EndState: Word;
    function At(Index: LongInt): PView;
    procedure DrawSubViews(P, Bottom: PView);
    function FirstMatch(AState: Word; AOptions: Word): PView;
    function FindNext(Forwards: Boolean): PView;
    function IndexOf(P: PView): LongInt;
    procedure RemoveView(P: PView);
    procedure ResetCurrent;
    procedure ResetCursor; virtual;
    end;

  { TWindow object }

  { Palette layout }
  { 1 = Frame passive }
  { 2 = Frame active }
  { 3 = Frame icon }
  { 4 = ScrollBar page area }
  { 5 = ScrollBar controls }
  { 6 = Scroller normal text }
  { 7 = Scroller selected text }
  { 8 = Reserved }

  PWindow = ^TWindow;
  TWindow = object(TGroup)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Flags: Byte;
    ZoomRect: TRect;
    MaxiRect: TRect;
    Number: AInt;
    Palette: AInt;
    Frame: PFrame;
    Title: PString;
    constructor Init(var Bounds: TRect; const ATitle: TTitleStr;
         ANumber: Integer);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Close; virtual;
    function GetPalette: PPalette; virtual;
    function GetTitle(MaxSize: Integer): TTitleStr; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure InitFrame; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure SizeLimits(var Min, Max: TPoint); virtual;
    function StandardScrollBar(AOptions: Word): PScrollBar;
    procedure Store(var S: TStream);
    procedure Zoom; virtual;
    procedure Maxi; virtual;
    function ReactOnCmd: Boolean; virtual;
    procedure Draw; virtual; {DataCompBoy}
    end;

  { Message dispatch function }

function Message(Receiver: PView; What, Command: Word; InfoPtr: Pointer)
  : Pointer;
function MessageL(Receiver: PView; What, Command: Word; InfoLng: LongInt)
  : Pointer;

{ Views registration procedure }

const

  { Event masks }

  PositionalEvents = evMouse;
  FocusedEvents = evKeyboard+evCommand;

  { Minimum window size }

  MinWinSize: TPoint = (X: 16; Y: 6);

  { Shadow definitions }

  ShadowSize: TPoint = (X: 2; Y: 1);
  ShadowAttr = $08;

  { Markers control }

  ShowMarkers: Boolean = False;

  { MapColor error return value }

  ErrorAttr = $CF;

  { Characters used for drawing selected and default items in  }
  { monochrome color sets                                      }

  SpecialChars: array[0..5] of Char = ('>', '<', ')', '(', ' ', ' ');

  { True if the command set has changed since being set to false }

  CommandSetChanged: Boolean = False;
  UpdateViews: PCollection = nil;
  UpdatePrior: PCollection = nil;

const

  { Current command set. All but window commands are active by default }

  CurCommandSet: TCommandSet =
  [0..255]-[cmZoom, cmClose, cmResize, cmNext, cmPrev, cmMaxi];

procedure RegisterToPrior(P: PView);
  {` Точный аналог RegisterToBackground, но работает со списком
   UpdatePrior. Фактически не используется и смысла в нём нет никакого. `}

procedure RegisterToBackground(P: PView);
  {` Включает P в начало списка периодического обновления UpdateViews.
  При включении не контролируется, не включён ли P уже, то есть
  повторное включение возможно.
     Период задаёт P^.UpdTicks, счёт идёт в P^.UpTmr.
     См. также Deregister, UpdateAll.
      `}

procedure Deregister(P: PView);
  {` Исключает P из обоих списков периодического обновления, если он
  там был. Если он там был не один раз, будет исключён только один.
  См. RegisterToBackground `}

procedure UpdateAll(All: Boolean);
  {` Проверить списки периодического обновления. Для каждлого элемента,
  у которого таймер истёк, вызывается Update и таймер перезапускается.
  Сначала делается проход по списку UpdateViews, а затем по UpdatePrior.
  `} {<Views.002>}

function GetNum: AInt;

var
  ModalCount: Word = 0; {<Views.001>}
    {` счётчик модальных окон, открытых друг над другом. Влияет на
    способ открытия окна хелпа `}

  TaggedDataCount: Longint; {<Views.004>}
  TaggedDataOnly: Boolean;

implementation
uses
  Startup, DNApp, Advance, Advance1, Advance2, DnIni,
  VpSysLow,
  Lfn, TitleSet, {DataCompBoy} {AK155 -}VideoMan
  ;

const
  Numbers: AWord = 0;
function GetNum: AInt;
  assembler;
asm
 push cx
 push bx
 mov ax, Numbers
 xor cx, cx
 shr ax, 1
@@1:
 inc cx
 shr ax, 1
 jc  @@1
 cmp cx, 10
 jb  @@ex
 xor cx, cx
@@ex:
 push cx
 mov ax, 1
 shl ax, cl
 mov bx, Numbers
 or  bx, ax
 mov Numbers, bx
 pop ax
 pop bx
 pop cx
end;

type
  PFixupList = ^TFixupList;
  TFixupList = array[1..4096] of Pointer;

const
  OwnerGroup: PGroup = nil;
  FixUpList: PFixupList = nil;
  TheTopView: PView = nil;

  {$I Views._VP}
procedure TPointInc(var A: TPoint; B: TPoint);
  begin
  Inc(A.X, B.X);
  Inc(A.Y, B.Y);
  end;

procedure TPointDec(var A: TPoint; B: TPoint);
  begin
  Dec(A.X, B.X);
  Dec(A.Y, B.Y);
  end;

{ TView }

constructor TView.Init(var Bounds: TRect);
  begin
  TObject.Init;
  Owner := nil;
  State := sfVisible;
  SetBounds(Bounds);
  DragMode := dmLimitLoY;
  HelpCtx := hcNoContext;
  EventMask := evMouseDown+evKeyDown+evCommand;
  end;

constructor TView.Load(var S: TStream);
  begin
  TObject.Init;
  S.Read(Origin,
    SizeOf(TPoint)*3+
    SizeOf(Byte)*2+
    SizeOf(AWord)*4);
  end;

destructor TView.Done;
  begin
  Deregister(@Self);
  Hide;
  if Owner <> nil then
    Owner^.Delete(@Self);
  end;

procedure TView.Awaken;
  begin
  end;

procedure TView.BlockCursor;
  begin
  SetState(sfCursorIns, True);
  end;

procedure TView.CalcBounds(var Bounds: TRect; Delta: TPoint);
  var
    S, D: LongInt;
    {  Min, Max: TPoint;}

  procedure Grow(var I: LongInt);
    begin
    if GrowMode and gfGrowRel = 0 then
      Inc(I, D)
    else
      I := (I*S+(S-D) shr 1) div (S-D);
    end;
  {
function Range(Val, Min, Max: LongInt): LongInt;
begin
  if Val < Min then Range := Min else
    if Val > Max then Range := Max else
      Range := Val;
end;
}
  begin
  GetBounds(Bounds);
  S := Owner^.Size.X;
  D := Delta.X;
  if GrowMode and gfGrowLoX <> 0 then
    Grow(Bounds.A.X);
  if GrowMode and gfGrowHiX <> 0 then
    Grow(Bounds.B.X);
  if Bounds.B.X-Bounds.A.X > MaxViewWidth then
    Bounds.B.X := Bounds.A.X+MaxViewWidth;
  S := Owner^.Size.Y;
  D := Delta.Y;
  if GrowMode and gfGrowLoY <> 0 then
    Grow(Bounds.A.Y);
  if GrowMode and gfGrowHiY <> 0 then
    Grow(Bounds.B.Y);
  {AK155 IMHO от коррекции по Min и Max может быть только вред.
Из-за этого гориз. скроллбар, который свели на нет, потом при
обратной растяжке накрывает угол окна.
  SizeLimits(Min, Max);
  Bounds.B.X := Bounds.A.X + Range(Bounds.B.X - Bounds.A.X, Min.X, Max.X);
  Bounds.B.Y := Bounds.A.Y + Range(Bounds.B.Y - Bounds.A.Y, Min.Y, Max.Y);
}
  if  (Origin.X < 0) and (Origin.Y < 0) then
    Bounds.A := Origin;
  end { TView.CalcBounds };

procedure TView.ChangeBounds(var Bounds: TRect);
  begin
  SetBounds(Bounds);
  DrawView;
  end;

procedure TView.ClearEvent(var Event: TEvent);
  begin
  Event.What := evNothing;
  Event.InfoPtr := @Self;
  end;

function TView.MenuEnabled(Command: Word): Boolean; {-$VOL}
  begin
  {$IFNDEF GAME}
  if  (Command = cmGame) {$ELSE}
  if  ( (Command = cmGame) and (not EnableGame)) {$ENDIF}
    {$IFNDEF TrashCan} or (Command = cmHideShowTools) {$ENDIF}
    {$IFNDEF UUENCODE} or (Command = cmUUEncodeFile) {$ENDIF}
    {$IFNDEF UUDECODE} or (Command = cmUUDecodeFile) {$ENDIF}
    {$IFNDEF CALENDAR} or (Command = cmCalendar) {$ENDIF}
    or (Command = cmPlayCD)
    {$IFNDEF PHONES} {or (Command in PhonesCommands)}
    or (Command = cmPhoneBook)
    or (Command = cmDialPhone)
    or (Command = cmUndial) {$ENDIF}
    {$IFNDEF MODEM} or (Command = cmSendInitModem)
    or (Command = cmSendModemBreak)
    or (Command = cmSetupModem)
    or (Command = cmHangUp)
    or (Command = cmAdvancePortSetup)
    or (Command = cmTerminal)
    {$IFDEF LINK} or (Command = cmNavyLink)
    or (Command = cmNavyLinkSetup) {$ENDIF}
    {$ENDIF}
    {$IFDEF MINARCH} or (Command = cmReadArchive)
    or ((Command >= cmLoConfigArchiver) and
        (Command <= cmHiConfigArchiver) and
        (Command <> cmConfigACE) and
        (Command <> cmConfigARJ) and
        (Command <> cmConfigCAB) and
        (Command <> cmConfigHA) and
        (Command <> cmConfigLHA) and
        (Command <> cmConfigRAR) and
        (Command <> cmConfigZIP)) {$ENDIF}
    {$IFNDEF SS} or (Command = cmSaversSetup)
    or (Command = cmScreenRest) {$ENDIF}
    {$IFNDEF Printer}
    or (Command = cmSinglePrint)
    or (Command = cmPrintFile)
    or (Command = cmFilePrint)
    or (Command = cmBlockPrint)
    or (Command = cmPanelPrint) {$ENDIF}
    {$IFNDEF PrintManager}
    or (Command = cmSetupPrinter) {$ENDIF}
    {$IFNDEF NETINFO} or (Command = cmGetNetInfo)
    or (Command = cmNetInfo) {$ENDIF}
    or (Command = cmSystemInfo)
    or (Command = cmMemoryInfo)

    then
    MenuEnabled := False
  else
    {MenuEnabled:=(Command > 255) or (Command in CurCommandSet);}
    MenuEnabled := (Command > 255) or (Lo(Command) in CurCommandSet);
  end { TView.MenuEnabled }; {-$VOL}

function TView.CommandEnabled(Command: Word): Boolean;
  begin
  CommandEnabled := MenuEnabled(Command) or (Command = cmRedo);
  end;

function TView.DataSize: Word;
  begin
  DataSize := 0;
  end;

procedure TView.DisableCommands(Commands: TCommandSet);
  begin
  CommandSetChanged := CommandSetChanged or (CurCommandSet*Commands <> []);
  CurCommandSet := CurCommandSet-Commands;
  end;

procedure TView.DragView(var Event: TEvent; Mode: Byte;
    var Limits: TRect; MinSize, MaxSize: TPoint);
  var
    P, S: TPoint;
    SaveBounds: TRect;

  function Min(I, J: LongInt): LongInt;
    begin
    if I < J then
      Min := I
    else
      Min := J;
    end;

  function Max(I, J: LongInt): LongInt;
    begin
    if I > J then
      Max := I
    else
      Max := J;
    end;

  procedure MoveGrow(P, S: TPoint);
    var
      R: TRect;
    begin
    S.X := Min(Max(S.X, MinSize.X), MaxSize.X);
    S.Y := Min(Max(S.Y, MinSize.Y), MaxSize.Y);
    P.X := Min(Max(P.X, Limits.A.X-S.X+1), Limits.B.X-1);
    P.Y := Min(Max(P.Y, Limits.A.Y-S.Y+1), Limits.B.Y-1);
    if Mode and dmLimitLoX <> 0 then
      P.X := Max(P.X, Limits.A.X);
    if Mode and dmLimitLoY <> 0 then
      P.Y := Max(P.Y, Limits.A.Y);
    if Mode and dmLimitHiX <> 0 then
      P.X := Min(P.X, Limits.B.X-S.X);
    if Mode and dmLimitHiY <> 0 then
      P.Y := Min(P.Y, Limits.B.Y-S.Y);
    R.Assign(P.X, P.Y, P.X+S.X, P.Y+S.Y);
    Locate(R);
    end;

  procedure Change(DX, DY: LongInt);
    begin
    if  (Mode and dmDragMove <> 0) and (ShiftState and $03 = 0) then
      begin
      Inc(P.X, DX);
      Inc(P.Y, DY);
      end
    else if (Mode and dmDragGrow <> 0) and (ShiftState and $03 <> 0)
    then
      begin
      Inc(S.X, DX);
      Inc(S.Y, DY);
      end;
    end;

  {AK155 13-08-2003 Введено прекращение по Esc перетаскивания и изменения
размера. По мотивам патча
dn3421-views(i)-dragging_and_resizing_terminates_by_esc.patch
by Eugeny Zvyagintzev, но реализация более экономичная.
}
  begin { TView.DragView }
  SetState(sfDragging, True);
  if Event.What = evMouseDown then
    begin
    GetBounds(SaveBounds);
    if Mode and dmDragMove <> 0 then
      begin
      P.X := Origin.X-Event.Where.X;
      P.Y := Origin.Y-Event.Where.Y;
      repeat
        if  (Event.What = evKeyDown) then
          begin
          if Event.KeyCode = kbESC then
            begin
            Locate(SaveBounds);
            Break;
            end;
          end
        else
          begin
          Inc(Event.Where.X, P.X);
          Inc(Event.Where.Y, P.Y);
          MoveGrow(Event.Where, Size);
          end;
      until not MouseEvent(Event, evMouseMove or evKeyDown);
      end
    else
      begin
      P.X := Size.X-Event.Where.X;
      P.Y := Size.Y-Event.Where.Y;
      repeat
        if  (Event.What = evKeyDown) then
          begin
          if Event.KeyCode = kbESC then
            begin
            Locate(SaveBounds);
            Break;
            end;
          end
        else
          begin
          Inc(Event.Where.X, P.X);
          Inc(Event.Where.Y, P.Y);
          MoveGrow(Origin, Event.Where);
          end;
      until not MouseEvent(Event, evMouseMove or evKeyDown);
      end;
    end
  else
    begin
    GetBounds(SaveBounds);
    repeat
      P := Origin;
      S := Size;
      KeyEvent(Event);
      case Event.KeyCode of
        kbLeft, kbShiftLeft, kbShiftTab:
          Change(-1, 0);
        kbRight, kbShiftRight, kbTab:
          Change(1, 0);
        kbUp, kbShiftUp:
          Change(0, -1);
        kbDown, kbShiftDown:
          Change(0, 1);
        kbCtrlLeft:
          Change(-8, 0);
        kbCtrlRight:
          Change(8, 0);
        kbHome:
          P.X := Limits.A.X;
        kbEnd:
          P.X := Limits.B.X-S.X;
        kbPgUp:
          P.Y := Limits.A.Y;
        kbPgDn:
          P.Y := Limits.B.Y-S.Y;
      end {case};
      MoveGrow(P, S);
    until (Event.KeyCode = kbEnter) or (Event.KeyCode = kbESC);
    if Event.KeyCode = kbESC then
      Locate(SaveBounds);
    end;
  SetState(sfDragging, False);
  end { TView.DragView };

procedure TView.Draw;
  var
    B: TDrawBuffer;
  begin
  MoveChar(B, ' ', GetColor(1), Size.X);
  WriteLine(0, 0, Size.X, Size.Y, B);
  end;

procedure TView.DrawCursor;
  begin
  if State and sfFocused <> 0 then
    ResetCursor;
  end;

procedure TView.DrawHide(LastView: PView);
  begin
  DrawCursor;
  DrawUnderView(State and sfShadow <> 0, LastView);
  end;

procedure TView.DrawShow(LastView: PView);
  begin
  DrawView;
  if State and sfShadow <> 0 then
    DrawUnderView(True, LastView);
  end;

procedure TView.DrawUnderRect(var R: TRect; LastView: PView);
  begin
  Owner^.Clip.Intersect(R);
  Owner^.DrawSubViews(NextView, LastView);
  Owner^.GetExtent(Owner^.Clip);
  end;

procedure TView.DrawUnderView(DoShadow: Boolean; LastView: PView);
  var
    R: TRect;
  begin
  GetBounds(R);
  if DoShadow then
    begin
    Inc(R.B.X, ShadowSize.X);
    Inc(R.B.Y, ShadowSize.Y);
    end;
  DrawUnderRect(R, LastView);
  end;

procedure TView.DrawView;
  begin
  if Exposed then
    begin
    Draw;
    DrawCursor;
    end;
  end;

procedure TView.EnableCommands(Commands: TCommandSet);
  begin
  CommandSetChanged := CommandSetChanged or
      (CurCommandSet*Commands <> Commands);
  CurCommandSet := CurCommandSet+Commands;
  end;

procedure TView.EndModal(Command: Word);
  var
    P: PView;
  begin
  P := TopView;
  if TopView <> nil then
    TopView^.EndModal(Command);
  end;

function TView.EventAvail: Boolean;
  var
    Event: TEvent;
  begin
  GetEvent(Event);
  if Event.What <> evNothing then
    PutEvent(Event);
  EventAvail := Event.What <> evNothing;
  end;

procedure TView.GetBounds(var Bounds: TRect);
  begin
  Bounds.A.X := Origin.X;
  Bounds.A.Y := Origin.Y;
  Bounds.B.X := Origin.X+Size.X;
  Bounds.B.Y := Origin.Y+Size.Y;
  end;

function TView.Execute: Word;
  begin
  Execute := cmCancel;
  end;

function TView.Focus: Boolean;
  begin
  Result := True;
  if State and (sfSelected+sfModal) = 0 then
    begin
    if Owner <> nil then
      begin
      Result := Owner^.Focus;
      if Result then
        if  ( (Owner^.Current = nil) or
              (Owner^.Current^.Options and ofValidate = 0) or
              (Owner^.Current^.Valid(cmReleasedFocus)))
        then
          Select
        else
          Result := False;
      end;
    end;
  end;

procedure TView.GetClipRect(var Clip: TRect);
  begin
  GetBounds(Clip);
  if Owner <> nil then
    Clip.Intersect(Owner^.Clip);
  Clip.Move(-Origin.X, -Origin.Y);
  end;

procedure TView.GetCommands(var Commands: TCommandSet);
  begin
  Commands := CurCommandSet;
  end;

procedure TView.GetData(var Rec);
  begin
  end;

procedure TView.GetEvent(var Event: TEvent);
  begin
  if Owner <> nil then
    Owner^.GetEvent(Event);
  end;

procedure TView.GetExtent(var Extent: TRect);
  begin
  Extent.A.X := 0;
  Extent.A.Y := 0;
  Extent.B.X := Size.X;
  Extent.B.Y := Size.Y;
  end;

function TView.GetHelpCtx: Word;
  begin
  if State and sfDragging <> 0 then
    GetHelpCtx := hcDragging
  else
    GetHelpCtx := HelpCtx;
  end;

function TView.GetPalette: PPalette;
  begin
  GetPalette := nil;
  end;

procedure TView.GetPeerViewPtr(var S: TStream; var P);
  var
    Index: LongInt;
  begin
  S.Read(Index, SizeOf(Index));
  if  (Index = 0) or (OwnerGroup = nil) then
    Pointer(P) := nil
  else
    begin
    Pointer(P) := FixUpList^[Index];
    FixUpList^[Index] := @P;
    end;
  end;

function TView.GetState(AState: Word): Boolean;
  begin
  GetState := State and AState = AState;
  end;

procedure TView.GrowTo(X, Y: LongInt);
  var
    R: TRect;
  begin
  R.Assign(Origin.X, Origin.Y, Origin.X+X, Origin.Y+Y);
  Locate(R);
  end;

procedure TView.HandleEvent(var Event: TEvent);
  begin
  if Event.What = evMouseDown then
    if  (State and (sfSelected+sfDisabled) = 0) and
        (Options and ofSelectable <> 0)
    then
      if not Focus or (Options and ofFirstClick = 0) then
        ClearEvent(Event);
  end;

procedure TView.Hide;
  begin
  if State and sfVisible <> 0 then
    SetState(sfVisible, False);
  end;

procedure TView.HideCursor;
  begin
  SetState(sfCursorVis, False);
  end;

procedure TView.KeyEvent(var Event: TEvent);
  begin
  repeat
    GetEvent(Event);
    if Event.What = evNothing then
      TinySlice;
  until Event.What = evKeyDown;
  end;

procedure TView.Locate(var Bounds: TRect);
  var
    R: TRect;
    Min, Max: TPoint;

  function Range(Val, Min, Max: LongInt): LongInt;
    begin
    if Val < Min then
      Range := Min
    else if Val > Max then
      Range := Max
    else
      Range := Val;
    end;

  begin
  SizeLimits(Min, Max);
  if not ((Bounds.A.X = -1) and (Bounds.B.X = Max.X+1)) then
    Bounds.B.X := Bounds.A.X+Range(Bounds.B.X-Bounds.A.X, Min.X, Max.X);

  if not ((Bounds.A.Y = -1) and (Bounds.B.Y = Max.Y+1)) then
    Bounds.B.Y := Bounds.A.Y+Range(Bounds.B.Y-Bounds.A.Y, Min.Y, Max.Y);

  GetBounds(R);
  if not Bounds.Equals(R) then
    begin
    ChangeBounds(Bounds);
    if  (Owner <> nil) and (State and sfVisible <> 0) then
      begin
      if State and sfShadow <> 0 then
        begin
        R.Union(Bounds);
        Inc(R.B.X, ShadowSize.X);
        Inc(R.B.Y, ShadowSize.Y);
        end;
      DrawUnderRect(R, nil);
      end;
    end;
  end { TView.Locate };

procedure TView.MakeFirst;
  begin
  if  (@Self <> nil) and (Owner <> nil) and (Owner^.First <> nil) then
    PutInFrontOf(Owner^.First);
  end;

procedure TView.MakeGlobal(Source: TPoint; var Dest: TPoint);
  var
    cur: PView;
  begin
  cur := @Self;
  Dest := Source;
  TPointInc(Dest, Origin);
  while cur^.Owner <> nil do
    begin
    cur := cur^.Owner;
    TPointInc(Dest, cur^.Origin);
    end;
  end;

procedure TView.MakeLocal(Source: TPoint; var Dest: TPoint);
  var
    cur: PView;
  begin
  cur := @Self;
  Dest := Source;
  TPointDec(Dest, Origin);
  while cur^.Owner <> nil do
    begin
    cur := cur^.Owner;
    TPointDec(Dest, cur^.Origin);
    end;
  end;

function TView.MouseEvent(var Event: TEvent; Mask: Word): Boolean;
  label Ex;
  begin
  if ButtonCount = 0 then
    Exit;
  {  asm
    mov ax, 3
    push bp
    int 33h
    pop bp
    or  bx, bx
    jz  Ex
  end;}
  repeat
    GetEvent(Event);
    if Event.What = evNothing then
      TinySlice;
  until Event.What and (Mask or evMouseUp) <> 0;
  MouseEvent := Event.What <> evMouseUp;
Ex:
  end;

function TView.MouseInView(Mouse: TPoint): Boolean;
  var
    Extent: TRect;
  begin
  MakeLocal(Mouse, Mouse);
  GetExtent(Extent);
  MouseInView := Extent.Contains(Mouse);
  end;

procedure TView.MoveTo(X, Y: LongInt);
  var
    R: TRect;
  begin
  R.Assign(X, Y, X+Size.X, Y+Size.Y);
  Locate(R);
  end;

function TView.NextView: PView;
  begin
  if @Self = Owner^.Last then
    NextView := nil
  else
    NextView := Next;
  end;

procedure TView.NormalCursor;
  begin
  SetState(sfCursorIns, False);
  end;

function TView.Prev: PView;
  var
    P: PView;
  begin
  P := @Self;
  while (P <> nil) and (P^.Next <> @Self) do
    P := P^.Next;
  Prev := P;
  end;

function TView.PrevView: PView;
  begin
  if @Self = Owner^.First then
    PrevView := nil
  else
    PrevView := Prev;
  end;

procedure TView.PutEvent(var Event: TEvent);
  begin
  if Owner <> nil then
    Owner^.PutEvent(Event);
  end;

procedure TView.PutInFrontOf(Target: PView);
  var
    P, LastView: PView;
    R: TRect;

  procedure MoveView;
    begin
    Owner^.RemoveView(@Self);
    Owner^.InsertView(@Self, Target);
    end;

  begin
  if  (Owner <> nil) and (Target <> @Self) and (Target <> NextView) and
      ( (Target = nil) or (Target^.Owner = Owner))
  then
    if State and sfVisible = 0 then
      MoveView
    else
      begin
      LastView := NextView;
      P := Target;
      while (P <> nil) and (P <> LastView) do
        P := P^.NextView;
      if P = nil then
        LastView := Target;
      State := State and not sfVisible;
      if LastView = Target then
        DrawHide(LastView);
      MoveView;
      State := State or sfVisible;
      if LastView <> Target then
        DrawShow(LastView);
      if Options and ofSelectable <> 0 then
        Owner^.ResetCurrent;
      end;
  end { TView.PutInFrontOf };

procedure TView.PutPeerViewPtr(var S: TStream; P: PView);
  var
    Index: LongInt;
  begin
  if  (P = nil) or (OwnerGroup = nil) then
    Index := 0
  else
    Index := OwnerGroup^.IndexOf(P);
  S.Write(Index, SizeOf(Index));
  end;

procedure TView.Select;
  begin
  if Options and ofSelectable <> 0 then
    if Options and ofTopSelect <> 0 then
      MakeFirst
    else if Owner <> nil then
      Owner^.SetCurrent(@Self, NormalSelect);
  end;

procedure TView.SetBounds(var Bounds: TRect);
  begin
  Origin := Bounds.A;
  Size := Bounds.B;
  TPointDec(Size, Origin);
  end;

{DataCompBoy
procedure TView.SetCmdState(Commands: TCommandSet; Enable: Boolean);
begin
  if Enable then EnableCommands(Commands)
  else DisableCommands(Commands);
end;
}
procedure TView.SetCommands(Commands: TCommandSet);
  begin
  CommandSetChanged := CommandSetChanged or (CurCommandSet <> Commands);
  CurCommandSet := Commands;
  end;

procedure TView.SetCursor(X, Y: LongInt);
  begin
  Cursor.X := X;
  Cursor.Y := Y;
  DrawCursor;
  end;

procedure TView.SetData(var Rec);
  begin
  end;

procedure TView.SetState(AState: Word; Enable: Boolean);
  var
    Command: Word;
  begin
  if Enable then
    State := State or AState
  else
    State := State and not AState;
  if Owner <> nil then
    case AState of
      sfVisible:
        begin
        if Owner^.State and sfExposed <> 0 then
          SetState(sfExposed, Enable);
        if Enable then
          DrawShow(nil)
        else
          DrawHide(nil);
        if Options and ofSelectable <> 0 then
          Owner^.ResetCurrent;
        end;
      sfCursorVis, sfCursorIns:
        DrawCursor;
      sfShadow:
        DrawUnderView(True, nil);
      sfFocused:
        begin
        ResetCursor;
        if Enable then
          Command := cmReceivedFocus
        else
          Command := cmReleasedFocus;
        Message(Owner, evBroadcast, Command, @Self);
        end;
    end {case};
  end { TView.SetState };

procedure TView.Show;
  begin
  if State and sfVisible = 0 then
    SetState(sfVisible, True);
  end;

procedure TView.ShowCursor;
  begin
  SetState(sfCursorVis, True);
  end;

procedure TView.SizeLimits(var Min, Max: TPoint);
  begin
  Min.X := 0;
  Min.Y := 0;
  if Owner <> nil
  then
    Max := Owner^.Size
  else
    begin
    Max.X := High(Max.X);
    Max.Y := High(Max.Y);
    end;
  end;

procedure TView.Store(var S: TStream);
  var
    SaveState: Word;
  begin
  SaveState := State;
  State := State and not (sfActive+sfSelected+sfFocused+sfExposed);
  S.Write(Origin,
    SizeOf(TPoint)*3+
    SizeOf(Byte)*2+
    SizeOf(AWord)*4);
  State := SaveState;
  end;

function TView.TopView: PView;
  var
    P: PView;
  begin
  if TheTopView = nil then
    begin
    P := @Self;
    while (P <> nil) and (P^.State and sfModal = 0) do
      P := P^.Owner;
    TopView := P;
    end
  else
    TopView := TheTopView;
  end;

function TView.Valid(Command: Word): Boolean;
  begin
  Valid := True;
  end;

{ TFrame }

constructor TFrame.Init(var Bounds: TRect);
  begin
  TView.Init(Bounds);
  GrowMode := gfGrowHiX+gfGrowHiY;
  EventMask := EventMask or evBroadcast;
  end;

procedure TFrame.FrameLine(var FrameBuf; Y, N: Integer;
    Color: Byte);
  const
    InitFrame: array[0..17] of Byte =
      ($06, $0A, $0C, $05, $00, $05, $03, $0A, $09,
      $16, $1A, $1C, $15, $00, $15, $13, $1A, $19);
    FrameChars: array[0..31] of Char =
    #32#32#32#192#32#179#218#195#32#217#196#193#191#180#194#197#32#32#32#200#32#186#201#199#32#188#205#207#187#182#209#32
    ;
  var
    FrameMask: array[0..MaxViewWidth-1] of Byte;
    ColorMask: Word;
    i, j, k: LongInt;
    CurrView: PView;
  begin
  FrameMask[0] := InitFrame[N];
  FillChar(FrameMask[1], Size.X-2, InitFrame[N+1]);
  FrameMask[Size.X-1] := InitFrame[N+2];
  CurrView := Owner^.Last^.Next;
  while (CurrView <> @Self) do
    begin
    if  ( (CurrView^.Options and ofFramed) <> 0) and
        ( (CurrView^.State and sfVisible) <> 0)
    then
      begin
      i := Y-CurrView^.Origin.Y;
      if i > CurrView^.Size.Y then
        i := 0
      else if i = CurrView^.Size.Y then
        i := $0a03
      else if i >= 0 then
        i := $0005
      else if i < -1 then
        i := 0
      else
        i := $0a06;
      if i <> 0 then
        begin
        k := CurrView^.Origin.X;
        if k < 1 then
          k := 1;
        j := k+CurrView^.Size.X;
        if j > Size.X then
          j := Size.X;
        if k < j then
          begin
          FrameMask[k-1] := FrameMask[k-1] or Lo(i);
          FrameMask[j] := FrameMask[j] or (Lo(i) xor Hi(i));
          if Hi(i) <> 0 then
            begin
            Dec(j, k);
            repeat
              FrameMask[k] := FrameMask[k] or Hi(i);
              Inc(k);
              Dec(j);
            until j = 0;
            end;
          end;
        end;
      end;
    CurrView := CurrView^.Next;
    end;
  ColorMask := Color shl 8;
  for i := 0 to Size.X-1 do
    TVideoBuf(FrameBuf)[i] := Ord(FrameChars[FrameMask[i]]) or ColorMask;
  end { TFrame.FrameLine };

function TFrame.GetTitleWidth: integer;
  begin
  GetTitleWidth := Size.X;
  end;

procedure TFrame.Draw;
  var
    CFrame, CTitle: Word;
    F, I, L, Width: Integer;
    B: TDrawBuffer;
    Title: TTitleStr;
    Min, Max: TPoint;
  begin
  if State and sfDragging <> 0 then
    begin
    CFrame := $0505;
    CTitle := $0005;
    F := 0;
    end
  else if State and sfActive = 0 then
    begin
    CFrame := $0101;
    CTitle := $0002;
    F := 0;
    end
  else
    begin
    CFrame := $0503;
    CTitle := $0004;
    F := 9;
    end;
  CFrame := GetColor(CFrame);
  CTitle := GetColor(CTitle);
  Width := GetTitleWidth;
  L := Width-10;
  if PWindow(Owner)^.Flags and (wfClose+wfZoom) <> 0 then
    Dec(L, 6);
  FrameLine(B, 0, F, Byte(CFrame));
  if Owner <> nil then
    Title := PWindow(Owner)^.GetTitle(L)
  else
    Title := '';
  if Title <> '' then
    begin
    L := Length(Title);
    if L > Width-10 then
      L := Width-10;
    if L < 0 then
      L := 0;
    I := (Width-L) shr 1;
    MoveChar(B[I-1], ' ', CTitle, 1);
    MoveBuf(B[I], Title[1], CTitle, L);
    MoveChar(B[I+L], ' ', CTitle, 1);
    end;
  Width := Size.X;
  if State and sfActive <> 0 then
    begin
    if PWindow(Owner)^.Flags and wfClose <> 0 then
      MoveCStr(B[2], '[~'#254'~]', CFrame);
    if PWindow(Owner)^.Flags and wfZoom <> 0 then
      begin
      MoveCStr(B[Width-5], '[~'#24'~]', CFrame);
      Owner^.SizeLimits(Min, Max);
      if Owner^.Size.Equals(Max) then
        WordRec(B[Width-4]).Lo := 18;
      end;
    end;
  if  (PWindow(Owner)^.Number in [1..9])
  then
    WordRec(B[3]).Lo := PWindow(Owner)^.Number+$30;
  WriteLine(0, 0, Size.X, 1, B);
  for I := 1 to Size.Y-2 do
    begin
    FrameLine(B, I, F+3, Byte(CFrame));
    WriteLine(0, I, Size.X, 1, B);
    end;
  FrameLine(B, Size.Y-1, F+6, Byte(CFrame));
  if State and sfActive <> 0 then
    if PWindow(Owner)^.Flags and wfGrow <> 0 then
      MoveCStr(B[Width-2], '~'#196#217'~', CFrame);
  WriteLine(0, Size.Y-1, Size.X, 1, B);
  end { TFrame.Draw };

function TFrame.GetPalette: PPalette;
  const
    P: String[Length(CFrame)] = CFrame;
  begin
  GetPalette := @P;
  end;

procedure TFrame.HandleEvent(var Event: TEvent);
  var
    Mouse: TPoint;

  procedure DragWindow(Mode: Byte);
    var
      Limits: TRect;
      Min, Max: TPoint;
    begin
    Owner^.Owner^.GetExtent(Limits);
    Owner^.SizeLimits(Min, Max);
    Owner^.DragView(Event, Owner^.DragMode or Mode, Limits, Min, Max);
    ClearEvent(Event);
    end;

  begin
  TView.HandleEvent(Event);
  if Event.What = evMouseDown then
    begin
    MakeLocal(Event.Where, Mouse);
    if Mouse.Y = 0 then
      begin
      if  (PWindow(Owner)^.Flags and wfClose <> 0) and
          (State and sfActive <> 0) and (Mouse.X >= 2)
           and (Mouse.X <= 4)
      then
        begin
        Event.What := evCommand;
        Event.Command := cmClose;
        Event.InfoPtr := Owner;
        PutEvent(Event);
        ClearEvent(Event);
        end
      else if (PWindow(Owner)^.Flags and wfZoom <> 0) and
          (State and sfActive <> 0) and (Event.Double or
            (Mouse.X >= Size.X-5) and
            (Mouse.X <= Size.X-3))
      then
        begin
        Event.What := evCommand;
        Event.Command := cmZoom;
        Event.InfoPtr := Owner;
        PutEvent(Event);
        ClearEvent(Event);
        end
      else if PWindow(Owner)^.Flags and wfMove <> 0 then
        DragWindow(dmDragMove);
      end
    else if (State and sfActive <> 0) and (Mouse.X >= Size.X-2) and
        (Mouse.Y >= Size.Y-1)
    then
      if PWindow(Owner)^.Flags and wfGrow <> 0 then
        DragWindow(dmDragGrow);
    end;
  end { TFrame.HandleEvent };

procedure TFrame.SetState(AState: Word; Enable: Boolean);
  begin
  TView.SetState(AState, Enable);
  if AState and (sfActive+sfDragging) <> 0 then
    DrawView;
  end;

{ TScrollBar }

constructor TScrollBar.Init(var Bounds: TRect);
  const
    VChars: TScrollChars = (#30, #31, #177, #254, #178);
    HChars: TScrollChars = (#17, #16, #177, #254, #178);
  begin
  TView.Init(Bounds);
  Value := 0;
  Min := 0;
  Max := 0;
  PgStep := 1;
  ArStep := 1;
  if Size.X = 1 then
    begin
    GrowMode := gfGrowLoX+gfGrowHiX+gfGrowHiY;
    Chars := VChars;
    if Length(VertScrollBarChars) = 5 then
      Move(VertScrollBarChars[1], Chars, 5);
    end
  else
    begin
    GrowMode := gfGrowLoY+gfGrowHiX+gfGrowHiY;
    Chars := HChars;
    if Length(HorizScrollBarChars) = 5 then
      Move(HorizScrollBarChars[1], Chars, 5);
    end;
  end { TScrollBar.Init };

constructor TScrollBar.Load(var S: TStream);
  begin
  TView.Load(S);
  S.Read(Value, SizeOf(LongInt)*6+SizeOf(Boolean)+SizeOf(TScrollChars));
  end;

procedure TScrollBar.Draw;
  begin
  DrawPos(GetPos);
  end;

procedure TMyScrollBar.Draw;
  var
    chrs: TScrollChars;
  begin
  if Startup.FMSetup.Show and fmsShowScrollBar <> 0 then
    DrawPos(GetPos)
  else
    begin
    chrs := Chars;
    Chars := #186#186#186#186#186;
    DrawPos(GetPos);
    Chars := chrs;
    end;
  end;

procedure TMyScrollBar.DrawPos(Pos: LongInt);
  var
    S: LongInt;
    B: TDrawBuffer;
    col1, col2, col3: Byte;
  begin
  S := GetSize-1;
  if Startup.FMSetup.Show and fmsShowScrollBar <> 0 then
    begin
    col1 := GetColor(1);
    col2 := GetColor(2);
    col3 := GetColor(3);
    end
  else
    begin
    col1 := GetColor(1);
    col2 := col1;
    col3 := col2;
    end;
  MoveChar(B[0], Chars[0], col2, 1);
  if Max = Min then
    MoveChar(B[1], Chars[4], col1, S-1)
  else
    begin
    MoveChar(B[1], Chars[2], col1, S-1);
    MoveChar(B[Pos], Chars[3], col3, 1);
    end;
  MoveChar(B[S], Chars[1], col2, 1);
  WriteBuf(0, 0, Size.X, Size.Y, B);
  end { TMyScrollBar.DrawPos };

procedure TScrollBar.DrawPos(Pos: LongInt);
  var
    S: LongInt;
    B: TDrawBuffer;
  begin
  S := GetSize-1;
  MoveChar(B[0], Chars[0], GetColor(2), 1);
  if Max = Min then
    MoveChar(B[1], Chars[4], GetColor(1), S-1)
  else
    begin
    MoveChar(B[1], Chars[2], GetColor(1), S-1);
    MoveChar(B[Pos], Chars[3], GetColor(3), 1);
    end;
  MoveChar(B[S], Chars[1], GetColor(2), 1);
  WriteBuf(0, 0, Size.X, Size.Y, B);
  end;

function TScrollBar.GetPalette: PPalette;
  const
    P: String[Length(CScrollBar)] = CScrollBar;
  begin
  GetPalette := @P;
  end;

function TScrollBar.GetPos: LongInt;
  var
    R: LongInt;
  begin
  R := Max-Min;
  if R = 0 then
    GetPos := 1
  else
    GetPos := ((((Value-Min)*(GetSize-3))+R shr 1) div R)+1;
  end;

function TScrollBar.GetSize: LongInt;
  var
    S: LongInt;
  begin
  if Size.X = 1 then
    S := Size.Y
  else
    S := Size.X;
  if S < 3 then
    GetSize := 3
  else
    GetSize := S;
  end;

procedure TScrollBar.HandleEvent(var Event: TEvent);
  var
    Tracking: Boolean;
    I, P, S, ClickPart: LongInt;
    Mouse: TPoint;
    Extent: TRect;

  function GetPartCode: LongInt;
    var
      Mark, Part: LongInt;
    begin
    Part := -1;
    if extent.Contains(Mouse) then
      begin
      if Size.X = 1 then
        Mark := Mouse.Y
      else
        Mark := Mouse.X;
      if Mark = P then
        Part := sbIndicator
      else
        begin
        if Mark < 1 then
          Part := sbLeftArrow
        else if Mark < P then
          Part := sbPageLeft
        else if Mark < S then
          Part := sbPageRight
        else
          Part := sbRightArrow;
        if Size.X = 1 then
          Inc(Part, 4);
        end;
      end;
    GetPartCode := Part;
    end { GetPartCode: };

  procedure Clicked;
    begin
    Message(Owner, evBroadcast, cmScrollBarClicked, @Self);
    end;

  begin { TScrollBar.HandleEvent }
  TView.HandleEvent(Event);
  ForceScroll := False;
  case Event.What of
    evMouseDown:
      begin
      Clicked;
      MakeLocal(Event.Where, Mouse);
      GetExtent(Extent);
      Extent.Grow(1, 1);
      P := GetPos;
      S := GetSize-1;
      ClickPart := GetPartCode;
      if ClickPart <> sbIndicator then
        begin
        repeat
          MakeLocal(Event.Where, Mouse);
          if GetPartCode = ClickPart then
            begin
            ForceScroll := True;
            SetValue(Value+ScrollStep(ClickPart));
            end;
        until not MouseEvent(Event, evMouseAuto);
        end
      else
        begin
        repeat
          MakeLocal(Event.Where, Mouse);
          Tracking := Extent.Contains(Mouse);
          if Tracking then
            begin
            if Size.X = 1 then
              I := Mouse.Y
            else
              I := Mouse.X;
            if I <= 0 then
              I := 1;
            if I >= S then
              I := S-1;
            end
          else
            I := GetPos;
          if I <> P then
            begin
            DrawPos(I);
            P := I;
            end;
        until not MouseEvent(Event, evMouseMove);
        if Tracking and (S > 2) then
          begin
          Dec(S, 2);
          SetValue(((((P-1)*(Max-Min))+S shr 1) div S)+Min);
          end;
        end;
      ClearEvent(Event);
      end;
    evKeyDown:
      if State and sfVisible <> 0 then
        begin
        ClickPart := sbIndicator;
        if Size.Y = 1 then
          case {CtrlToArrow}(Event.KeyCode) of
            kbLeft, kbShiftLeft:
              ClickPart := sbLeftArrow;
            kbRight, kbShiftRight:
              ClickPart := sbRightArrow;
            kbCtrlLeft, kbCtrlShiftLeft:
              ClickPart := sbPageLeft;
            kbCtrlRight, kbCtrlShiftRight:
              ClickPart := sbPageRight;
            kbHome, kbShiftHome:
              I := Min;
            kbEnd, kbShiftEnd:
              I := Max;
            else {case}
              Exit;
          end
        else
          case {CtrlToArrow}(Event.KeyCode) of
            kbUp, kbShiftUp:
              ClickPart := sbUpArrow;
            kbDown, kbShiftDown:
              ClickPart := sbDownArrow;
            kbPgUp, kbShiftPgUp:
              ClickPart := sbPageUp;
            kbPgDn, kbShiftPgDn:
              ClickPart := sbPageDown;
            kbCtrlPgUp, kbCtrlShiftPgUp:
              I := Min;
            kbCtrlPgDn, kbCtrlShiftPgDn:
              I := Max;
            else {case}
              Exit;
          end {case};
        Clicked;
        if ClickPart <> sbIndicator then
          I := Value+ScrollStep(ClickPart);
        SetValue(I);
        ClearEvent(Event);
        end;
  end {case};
  end { TScrollBar.HandleEvent };

procedure TScrollBar.ScrollDraw;
  begin
  Message(Owner, evBroadcast, cmScrollBarChanged, @Self);
  ForceScroll := False;
  end;

function TScrollBar.ScrollStep(Part: LongInt): LongInt;
  begin
  if Part and 2 = 0 then
    Step := ArStep
  else
    Step := PgStep;
  if Part and 1 = 0 then
    Step := -Step;
  ScrollStep := Step;
  end;

procedure TScrollBar.SetParams(AValue, AMin, AMax, APgStep,
    AArStep: LongInt);
  var
    SValue: LongInt;
  begin
  if AMax < AMin then
    AMax := AMin;
  if AValue < AMin then
    AValue := AMin;
  if AValue > AMax then
    AValue := AMax;
  SValue := Value;
  if  (SValue <> AValue) or (Min <> AMin) or (Max <> AMax) then
    begin
    Value := AValue;
    Min := AMin;
    Max := AMax;
    DrawView;
    if SValue <> AValue then
      ScrollDraw;
    end;
  PgStep := APgStep;
  ArStep := AArStep;
  end { TScrollBar.SetParams };

procedure TScrollBar.SetRange(AMin, AMax: LongInt);
  begin
  SetParams(Value, AMin, AMax, PgStep, ArStep);
  end;

procedure TScrollBar.SetStep(APgStep, AArStep: LongInt);
  begin
  SetParams(Value, Min, Max, APgStep, AArStep);
  end;

procedure TScrollBar.SetValue(AValue: LongInt);
  begin
  SetParams(AValue, Min, Max, PgStep, ArStep);
  end;

procedure TScrollBar.Store(var S: TStream);
  begin
  TView.Store(S);
  S.Write(Value, SizeOf(LongInt)*6+SizeOf(Boolean)+SizeOf(TScrollChars));
  end;

{ TGroup }

constructor TGroup.Init(var Bounds: TRect);
  begin
  TView.Init(Bounds);
  Options := Options or (ofSelectable+ofBuffered);
  GetExtent(Clip);
  EventMask := $FFFF;
  end;

constructor TGroup.Load(var S: TStream);
  var
    FixupSave: PFixupList;
    Count, I: LongInt;
    P, Q: ^Pointer;
    V, Cur: PView;
    OwnerSave: PGroup;
  begin
  TView.Load(S);
  GetExtent(Clip);
  OwnerSave := OwnerGroup;
  OwnerGroup := @Self;
  FixupSave := FixUpList;
  S.Read(Count, SizeOf(Count));
  GetMem(FixUpList, Count*SizeOf(Pointer));
  FillChar(FixUpList^, Count*SizeOf(Pointer), 0);
  for I := 1 to Count do
    begin
    V := PView(S.Get);
    if V <> nil then
      InsertView(V, nil);
    end;
  GetSubViewPtr(S, Cur);
//  if Cur = nil then
    {Cat}
//    Fail;
  V := Last;
  for I := 1 to Count do
    begin
    V := V^.Next;
    P := FixUpList^[I];
    while P <> nil do
      begin
      Q := P;
      P := P^;
      Q^:= V;
      end;
    end;
  SetCurrent(Cur, NormalSelect);
  FreeMem(FixUpList, Count*SizeOf(Pointer));
  OwnerGroup := OwnerSave;
  FixUpList := FixupSave;
  if OwnerGroup = nil then
    Awaken;
  end { TGroup.Load };

destructor TGroup.Done;
  var
    P, T: PView;
  begin
  Hide;
  P := Last;
  if P <> nil then
    begin
    repeat
      P^.Hide;
      P := P^.Prev;
    until P = Last;
    repeat
      Dispose(Last, Done);
    until Last = nil;
    end;
  FreeBuffer;
  TView.Done;
  end;

function TGroup.At(Index: LongInt): PView;
  var
    temp: PView;
  begin
  temp := Last;
  while Index > 0 do
    begin
    Dec(Index);
    temp := temp^.Next;
    end;
  At := temp;
  end;

procedure TGroup.Awaken;

  procedure DoAwaken(P: PView);
    begin
    P^.Awaken;
    end;

  begin
  ForEach(@DoAwaken);
  end;

procedure TGroup.ChangeBounds(var Bounds: TRect);
  var
    D: TPoint;

  procedure DoCalcChange(P: PView);
    var
      R: TRect;
    begin
    P^.CalcBounds(R, D);
    P^.ChangeBounds(R);
    end;

  begin
  D.X := Bounds.B.X-Bounds.A.X-Size.X;
  D.Y := Bounds.B.Y-Bounds.A.Y-Size.Y;
  if D.EqualsXY(0, 0) then
    begin
    SetBounds(Bounds);
    DrawView;
    end
  else
    begin
    FreeBuffer;
    SetBounds(Bounds);
    GetExtent(Clip);
    GetBuffer;
    Lock;
    ForEach(@DoCalcChange);
    UnLock;
    end;
  end { TGroup.ChangeBounds };

function TGroup.DataSize: Word;
  var
    T: Word;

  procedure AddSubviewDataSize(P: PView);
    begin
    Inc(T, P^.DataSize);
    end;

  begin
  T := 0;
  ForEach(@AddSubviewDataSize);
  DataSize := T;
  end;

procedure TGroup.Delete(P: PView);
  var
    SaveState: Word;
  begin
  SaveState := P^.State;
  P^.Hide;
  RemoveView(P);
  P^.Owner := nil;
  P^.Next := nil;
  if SaveState and sfVisible <> 0 then
    P^.Show;
  end;

procedure TGroup.Draw;
  var
    R: TRect;
  begin
  if Buffer = nil then
    begin
    GetBuffer;
    if Buffer <> nil then
      begin
      Inc(LockFlag);
      Redraw;
      Dec(LockFlag);
      end;
    end;
  if Buffer <> nil then
    WriteBuf(0, 0, Size.X, Size.Y, Buffer^)
  else
    begin
    GetClipRect(Clip);
    Redraw;
    GetExtent(Clip);
    end;
  end { TGroup.Draw };

procedure TGroup.DrawSubViews(P, Bottom: PView);
  begin
  while P <> Bottom do
    begin
    P^.DrawView;
    P := P^.NextView;
    end;
  end;

procedure TGroup.EndModal(Command: Word);
  begin
  if State and sfModal <> 0 then
    EndState := Command
  else
    TView.EndModal(Command);
  end;

procedure TGroup.EventError(var Event: TEvent);
  begin
  if Owner <> nil then
    Owner^.EventError(Event);
  end;

function TGroup.Execute: Word;
  var
    E: TEvent;
  begin
  repeat
    EndState := 0;
    repeat
      GetEvent(E);
      if E.What = evNothing then
        TinySlice {AK155}
      else
        begin
        HandleEvent(E);
        if E.What <> evNothing then
          EventError(E);
        end;
    until EndState <> 0;
  until Valid(EndState);
  Execute := EndState;
  end;

function TGroup.ExecView(P: PView): Word;
  var
    SaveOptions: Word;
    SaveOwner: PGroup;
    SaveTopView: PView;
    SaveCurrent: PView;
    SaveCommands: TCommandSet;
    PP: TPoint;
    EV: TEvent;
  begin
  if P <> nil then
    begin
    SaveOptions := P^.Options;
    SaveOwner := P^.Owner;
    SaveTopView := TheTopView;
    SaveCurrent := Current;
    GetCommands(SaveCommands);
    TheTopView := P;
    P^.Options := P^.Options and not ofSelectable;
    P^.SetState(sfModal, True);
    Inc(ModalCount);
    SetCurrent(P, EnterSelect);
    if SaveOwner = nil then
      Insert(P);
    ExecView := P^.Execute;
    if SaveOwner = nil then
      Delete(P);
    SetCurrent(SaveCurrent, LeaveSelect);
    Dec(ModalCount);
    P^.SetState(sfModal, False);
    P^.Options := SaveOptions;
    P := Current;
    if  (P <> nil) and not P^.GetState(sfFocused) then
      begin
      EV.What := evMouseDown;
      PP.X := P^.Origin.X;
      PP.Y := P^.Size.Y-1;
      MakeGlobal(PP, PP);
      EV.Where := PP;
      EV.Buttons := mbRightButton;
      PutEvent(EV);
      end;
    TheTopView := SaveTopView;
    SetCommands(SaveCommands);
    end
  else
    ExecView := cmCancel;
  end { TGroup.ExecView };

function TGroup.First: PView;
  begin
  if Last = nil then
    First := nil
  else
    First := Last^.Next;
  end;

function TGroup.FirstMatch(AState: Word; AOptions: Word): PView;

  function Matches(P: PView): Boolean;
    begin
    Matches := (P^.State and AState = AState) and
        (P^.Options and AOptions = AOptions);
    end;

  begin
  FirstMatch := FirstThat(@Matches);
  end;

{AK155}
(*
function TGroup.FirstThat(P: Pointer): PView;
var
  Tp : PView;
begin
  if (Last<>nil) then
  begin
    Tp := Last;
    repeat
      Tp := Tp^.Next;
      if Byte(Longint(CallPointerMethodLocal(P,PreviousFramePointer,@self,Tp)))<>0 then
      begin
        FirstThat := Tp;
        Exit;
      end;
    until (Tp=Last);
    FirstThat := nil;
  end
  else
    FirstThat := nil;
end;
*)

function TGroup.FirstThat(P: Pointer): PView;
  assembler; {&USES None}
  {&FRAME-} {AK155}
  var
    ALast: Pointer;
    asm
                mov     eax,Self
                mov     eax,[eax].TGroup.Last
                test    eax,eax
                jz      @@2
                mov     ALast,eax
              @@1:
                mov     ecx,P
                mov     eax,[eax].TView.Next
                push    eax
                push    eax                     {[1]:Pointer = PView }
                Call    ecx
                test    al,al
                pop     eax
                jnz     @@2
                cmp     eax,ALast
                jne     @@1
                xor     eax,eax
              @@2:
end; {/AK155}

  function TGroup.FindNext(Forwards: Boolean): PView;
    var
      P: PView;
    begin
    FindNext := nil;
    if Current <> nil then
      begin
      P := Current;
      repeat
        if Forwards then
          P := P^.Next
        else
          P := P^.Prev;
      until ((P^.State and (sfVisible+sfDisabled) = sfVisible) and
          (P^.Options and ofSelectable <> 0)) or (P = Current);
      if P <> Current then
        FindNext := P;
      end;
    end;

  function TGroup.FocusNext(Forwards: Boolean): Boolean;
    var
      P: PView;
    begin
    P := FindNext(Forwards);
    FocusNext := True;
    if P <> nil then
      FocusNext := P^.Focus;
    end;

  {AK155 }
  (*
procedure TGroup.ForEach(P: Pointer);
var
  Tp,Hp,L0 : PView;
{ Vars Hp and L0 are necessary to hold original pointers in case   }
{ when some view closes himself as a result of broadcast message ! }
begin
  if (Last<>nil) then
  begin
     Tp:=Last;
     Hp:=Tp^.Next;
     L0:=Last;
     repeat
       Tp:=Hp;
       if tp=nil then
        exit;
       Hp:=Tp^.Next;
       CallPointerMethodLocal(P,PreviousFramePointer,@self,Tp);
     until (Tp=L0);
  end;
end;
*)

  procedure TGroup.ForEach(P: Pointer);
    assembler; {&USES ebx}
    {&FRAME-} {AK155 }
    var
      ALast: Pointer;
      asm
                mov     ecx,Self
                mov     ecx,[ecx].TGroup.Last
                jecxz   @@RET
                mov     ebx,P
                mov     ALast,ecx
                mov     ecx,[ecx].TView.Next
              @@1:
                cmp     ecx,ALast
                je      @@2
                push    [ecx].TView.Next
                push    ecx
                Call    ebx
                pop     ecx
                jmp     @@1
              @@2:
                push    ecx
                Call    ebx
              @@RET:
end; {/AK155}

    procedure TGroup.FreeBuffer;
      begin
      if  (Options and ofBuffered <> 0) and (Buffer <> nil) then
        DisposeCache(Pointer(Buffer));
      end;

    { Allocate a group buffer if the group is exposed, buffered, and
  its area is less than 32768 bytes }

    procedure TGroup.GetBuffer;
      begin
      if  ( (State and sfExposed) <> 0) and
          ( (Options and ofBuffered) <> 0) and
          (Buffer = nil)
      then
        begin
        NewCache(Pointer(Buffer), (Size.X*Size.Y) shl 1);
        if Buffer <> nil then
          begin
          Inc(LockFlag);
          Redraw;
          Dec(LockFlag);
          end;
        end;
      end;

    procedure TGroup.GetData(var Rec);
      type
        Bytes = array[0..65534] of Byte;
      var
        I: Word;
        V: PView;
      begin
      I := 0;
      if Last <> nil then
        begin
        V := Last;
        repeat
          if not TaggedDataOnly or (TaggedDataCount = 0) or
             (V^.State and sfTagged <> 0)
          then
            V^.GetData(Bytes(Rec)[I]);
          Inc(I, V^.DataSize);
          V := V^.Prev;
        until V = Last;
        end;
      end;

    function TGroup.GetHelpCtx: Word;
      var
        H: Word;
      begin
      H := hcNoContext;
      if Current <> nil then
        H := Current^.GetHelpCtx;
      if H = hcNoContext then
        H := TView.GetHelpCtx;
      GetHelpCtx := H;
      end;

    procedure TGroup.GetSubViewPtr(var S: TStream; var P);
      var
        Index: Word;
      begin
      S.Read(Index, SizeOf(Word));
      if  (Index > 0) and (S.Status = stOK) then
        {Cat: добавил проверку статуса}
        Pointer(P) := At(Index)
      else
        Pointer(P) := nil;
      end;
    (*
procedure TGroup.HandleEvent(var Event: TEvent);

procedure DoHandleEvent(P: PView); {$IFDEF BIT_16}far;{$ENDIF}
begin
  if (P = nil) or ((P^.State and sfDisabled <> 0)
    and (Event.What and (PositionalEvents or FocusedEvents) <> 0)) then Exit;
  case Phase of
    phPreProcess: if P^.Options and ofPreProcess = 0 then Exit;
    phPostProcess: if P^.Options and ofPostProcess = 0 then Exit;
  end;
  if Event.What and P^.EventMask <> 0 then P^.HandleEvent(Event);
end;

function ContainsMouse(P: PView): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
begin
  ContainsMouse := (P^.State and sfVisible <> 0) and
    P^.MouseInView(Event.Where);
end;

begin
  TView.HandleEvent(Event);
  if Event.What and FocusedEvents <> 0 then
  begin
    Phase := phPreProcess;
    ForEach(@DoHandleEvent);
    Phase := phFocused;
    if Current<>nil then DoHandleEvent(Current);
    Phase := phPostProcess;
    ForEach(@DoHandleEvent);
  end else
  begin
    Phase := phFocused;
    if (Event.What and PositionalEvents <> 0) then
      DoHandleEvent(FirstThat(@ContainsMouse)) else
      ForEach(@DoHandleEvent);
  end;
end;
*)
    procedure TGroup.HandleEvent(var Event: TEvent);

      { CN fix: we call FirstThat instead ForEach }
      function DoHandleEvent(P: PView): Boolean;
        begin
        DoHandleEvent := False;
        if  (P = nil) or ((P^.State and sfDisabled <> 0) and
              (Event.What and (PositionalEvents or FocusedEvents) <> 0))
        then
          Exit;
        case Phase of
          phPreProcess:
            if P^.Options and ofPreProcess = 0 then
              Exit;
          phPostProcess:
            if P^.Options and ofPostProcess = 0 then
              Exit;
        end {case};
        if  (Event.What <> evMouseMove) and
            (Event.What and P^.EventMask = 0)
        then
          Exit;
        P^.HandleEvent(Event);
        DoHandleEvent := Event.What = evNothing;
        end;

      function ContainsMouse(P: PView): Boolean;
        begin
        ContainsMouse := (P^.State and sfVisible <> 0) and
          P^.MouseInView(Event.Where);
        end;

      var
        NV: PView;
      begin { TGroup.HandleEvent }
      if Event.What and EventMask = 0 then
        Exit; { Fixed 05.22.95 }
      TView.HandleEvent(Event);
      if Event.What and FocusedEvents <> 0 then
        begin
        Phase := phPreProcess;
        FirstThat(@DoHandleEvent);
        if Event.What = evNothing then
          Exit;
        Phase := phFocused;
        DoHandleEvent(Current);
        if Event.What = evNothing then
          Exit;
        Phase := phPostProcess;
        FirstThat(@DoHandleEvent);
        end
      else if (Event.What = evCommand) and (Event.Command = cmTagData)
      then
        begin
        if TaggedSubviewCount = 0 then
          begin
          State := State or sfTagged;
          Message(Owner, evCommand, cmTagData, nil);
          end;
        Inc(TaggedSubviewCount);
        ClearEvent(Event);
        end
      else if (Event.What = evCommand) and (Event.Command = cmUntagData)
      then
        begin
        Dec(TaggedSubviewCount);
        if TaggedSubviewCount = 0 then
          begin
          State := State and not sfTagged;
          Message(Owner, evCommand, cmUntagData, nil);
          end;
        ClearEvent(Event);
        end
      else
        begin
        Phase := phFocused;
        if  (Event.What and PositionalEvents <> 0) then
          begin
          DoHandleEvent(FirstThat(@ContainsMouse));
          if ClearPositionalEvents then
            ClearEvent(Event);
          end
        else
          FirstThat(@DoHandleEvent);
        end;
      end { TGroup.HandleEvent };

    function TGroup.IndexOf(P: PView): LongInt;
      var
        temp: PView;
        index: LongInt;
      begin
      index := 0;
      if Last <> nil then
        begin
        temp := Last;
        repeat
          Inc(index);
          temp := temp^.Next;
        until (temp = P) or (temp = Last);
        if temp <> P then
          index := 0;
        end;
      IndexOf := index;
      end;

    procedure TGroup.Insert(P: PView);
      begin
      InsertBefore(P, First);
      end;

    procedure TGroup.InsertBefore(P, Target: PView);
      var
        SaveState: Word;
      begin
      if  (P <> nil) and (P^.Owner = nil) and
          ( (Target = nil) or (Target^.Owner = @Self))
      then
        begin
        if P^.Options and ofCenterX <> 0 then
          P^.Origin.X := (Size.X-P^.Size.X) div 2;
        if P^.Options and ofCenterY <> 0 then
          P^.Origin.Y := (Size.Y-P^.Size.Y) div 2;
        SaveState := P^.State;
        P^.Hide;
        InsertView(P, Target);
        if SaveState and sfVisible <> 0 then
          P^.Show;
        if State and sfActive <> 0 then
          P^.SetState(sfActive, True);
        end;
      end;

    procedure TGroup.InsertView(P, Target: PView);
      begin
      P^.Owner := @Self;
      if Target <> nil then
        begin
        Target := Target^.Prev;
        P^.Next := Target^.Next;
        Target^.Next := P;
        end
      else
        begin
        if Last = nil then
          P^.Next := P
        else
          begin
          P^.Next := Last^.Next;
          Last^.Next := P;
          end;
        Last := P;
        end;
      end;

    procedure TGroup.Lock;
      begin
      if  (Buffer <> nil) or (LockFlag <> 0) then
        Inc(LockFlag);
      end;

    procedure TGroup.PutSubViewPtr(var S: TStream; P: PView);
      var
        Index: Word;
      begin
      if P = nil then
        Index := 0
      else
        Index := IndexOf(P);
      S.Write(Index, SizeOf(Word));
      end;

    procedure TGroup.Redraw;
      begin
      DrawSubViews(First, nil);
      end;

    procedure TGroup.RemoveView(P: PView);
      var
        cur: PView;
      begin
      if Last <> nil then
        begin
        cur := Last;
        while True do
          begin
          if P = cur^.Next then
            begin
            cur^.Next := P^.Next;
            if Last = P then
              begin
              if cur^.Next = P then
                Last := nil
              else
                Last := cur;
              Break
              end;
            end;
          if cur^.Next = Last then
            Break;
          cur := cur^.Next;
          end;
        end
      end { TGroup.RemoveView };

    procedure TGroup.ResetCurrent;
      begin
      SetCurrent(FirstMatch(sfVisible, ofSelectable), NormalSelect);
      end;

    procedure TGroup.ResetCursor;
      begin
      if Current <> nil then
        Current^.ResetCursor;
      end;

    procedure TGroup.SelectNext(Forwards: Boolean);
      var
        P: PView;
      begin
      P := FindNext(Forwards);
      if P <> nil then
        P^.Select;
      end;

    procedure TGroup.SetCurrent(P: PView; Mode: SelectMode);

      procedure SelectView(P: PView; Enable: Boolean);
        begin
        if P <> nil then
          P^.SetState(sfSelected, Enable);
        end;

      procedure FocusView(P: PView; Enable: Boolean);
        begin
        if  (State and sfFocused <> 0) and (P <> nil) then
          P^.SetState(sfFocused, Enable);
        end;

      begin
      if Current <> P then
        begin
        Lock;
        FocusView(Current, False);
        if Mode <> EnterSelect then
          SelectView(Current, False);
        if Mode <> LeaveSelect then
          SelectView(P, True);
        FocusView(P, True);
        Current := P;
        UnLock;
        end;
      end { TGroup.SetCurrent };

    procedure TGroup.SetData(var Rec);
      type
        Bytes = array[0..65534] of Byte;
      var
        I: Word;
        V: PView;
      begin
      I := 0;
      if Last <> nil then
        begin
        V := Last;
        repeat
          V^.SetData(Bytes(Rec)[I]);
          Inc(I, V^.DataSize);
          V := V^.Prev;
        until V = Last;
        end;
      end;

    procedure TGroup.SetState(AState: Word; Enable: Boolean);

      procedure DoSetState(P: PView);
        begin
        P^.SetState(AState, Enable);
        end;

      procedure DoExpose(P: PView);
        begin
        if P^.State and sfVisible <> 0 then
          P^.SetState(sfExposed, Enable);
        end;

      begin
      TView.SetState(AState, Enable);
      case AState of
        sfActive, sfDragging:
          begin
          Lock;
          ForEach(@DoSetState);
          UnLock;
          end;
        sfFocused:
          if Current <> nil then
            Current^.SetState(sfFocused, Enable);
        sfExposed:
          begin
          ForEach(@DoExpose);
          if not Enable then
            FreeBuffer;
          end;
      end {case};
      end { TGroup.SetState };

    procedure TGroup.Store(var S: TStream);
      var
        Count: LongInt;
        OwnerSave: PGroup;

      procedure DoPut(P: PView);
        begin
        S.Put(P);
        end;

      begin
      TView.Store(S);
      OwnerSave := OwnerGroup;
      OwnerGroup := @Self;
      Count := IndexOf(Last);
      S.Write(Count, SizeOf(Count));
      ForEach(@DoPut);
      PutSubViewPtr(S, Current);
      OwnerGroup := OwnerSave;
      end;

    procedure TGroup.UnLock;
      begin
      if LockFlag <> 0 then
        begin
        Dec(LockFlag);
        if LockFlag = 0 then
          DrawView;
        end;
      end;

    function TGroup.Valid(Command: Word): Boolean;

      function IsInvalid(P: PView): Boolean;
        begin
        IsInvalid := not P^.Valid(Command);
        end;

      begin
      Valid := True;
      if Command = cmReleasedFocus then
        begin
        if  (Current <> nil) and (Current^.Options and ofValidate <> 0)
        then
          Valid := Current^.Valid(Command);
        end
      else
        Valid := FirstThat(@IsInvalid) = nil;
      end;

    { TWindow }

    constructor TWindow.Init(var Bounds: TRect; const ATitle: TTitleStr;
        ANumber: Integer);
      begin
      TGroup.Init(Bounds);
      State := State or sfShadow;
      Options := Options or (ofSelectable+ofTopSelect);
      GrowMode := gfGrowAll+gfGrowRel;
      Flags := wfMove+wfGrow+wfClose+wfZoom;
      Title := NewStr(ATitle);
      if ANumber <> wnNoNumber then
        Number := GetNum
      else
        Number := 0;
      Palette := wpBlueWindow;
      InitFrame;
      if Frame <> nil then
        Insert(Frame);
      GetBounds(ZoomRect);
      MaxiRect.A.X := High(MaxiRect.A.X);
      ClearPositionalEvents := True;
      end;

    constructor TWindow.Load(var S: TStream);
      var
        FrameIdent: AWord;
      begin
      TGroup.Load(S);
      S.Read(Flags, SizeOf(Byte)+SizeOf(TRect)*2+2*SizeOf(AInt));
      if Number in [1..9] then
        Numbers := Numbers or (1 shl Number);
      GetSubViewPtr(S, Frame);
      if Frame = nil then
        {Cat}
        Fail;
      Title := S.ReadStr;
      ClearPositionalEvents := True;
      end;

    destructor TWindow.Done;
      begin
      if Number in [1..9] then
        Numbers := Numbers and not (1 shl Number);
      inherited Done;
      DisposeStr(Title);
      end;

    procedure TWindow.Close;
      begin
      if Valid(cmClose) then
        Free;
      end;

    function TWindow.ReactOnCmd;
      begin
      ReactOnCmd := False;
      end;

    function TWindow.GetPalette: PPalette;
      const
        P: array[wpBlueWindow..wpGrayWindow] of String[Length(CBlueWindow)
        ] =
          (CBlueWindow, CCyanWindow, CGrayWindow);
      begin
      GetPalette := @P[Palette];
      end;

    function TWindow.GetTitle(MaxSize: Integer): TTitleStr;
      begin
      if Title <> nil
      then
        if Length(Title^) > MaxSize
        then
          GetTitle := Cut(Title^, MaxSize)
        else
          GetTitle := Title^
      else
        GetTitle := '';
      end;

    procedure TWindow.HandleEvent(var Event: TEvent);
      var
        Limits: TRect;
        Min, Max: TPoint;
      begin
      TGroup.HandleEvent(Event);
      if  (Event.What = evCommand) then
        case Event.Command of
          cmResize:
            if Flags and (wfMove+wfGrow) <> 0 then
              begin
              Owner^.GetExtent(Limits);
              {Dec(Limits.A.Y, Limits.B.Y);} { X-Man }
              SizeLimits(Min, Max);
              DragView(Event, DragMode or (Flags and (wfMove+wfGrow)),
                Limits, Min, Max);
              ClearEvent(Event);
              end;
          cmClose:
            if  (Flags and wfClose <> 0) and
                ( (Event.InfoPtr = nil) or (Event.InfoPtr = @Self))
            then
              begin
              ClearEvent(Event);
              if State and sfModal = 0 then
                Close
              else
                begin
                Event.What := evCommand;
                Event.Command := cmCancel;
                PutEvent(Event);
                ClearEvent(Event);
                end;
              end;
          cmZoom:
            if  (Flags and wfZoom <> 0) and
                ( (Event.InfoPtr = nil) or (Event.InfoPtr = @Self))
            then
              begin
              Zoom;
              ClearEvent(Event);
              end;
          cmMaxi:
            if  (Flags and wfMaxi <> 0) and
                ( (Event.InfoPtr = nil) or (Event.InfoPtr = @Self))
            then
              begin
              Maxi;
              ClearEvent(Event);
              end;
        end
      else if Event.What = evKeyDown then
        case Event.KeyCode of
          kbTab:
            begin
            FocusNext(False);
            ClearEvent(Event);
            end;
          kbShiftTab:
            begin
            FocusNext(True);
            ClearEvent(Event);
            end;
        end
      else if (Event.What = evBroadcast)
           and (Event.Command = cmSelectWindowNum)
        and (Event.InfoInt = Number) and (Options and ofSelectable <> 0)
      then
        begin
        Select;
        ClearEvent(Event);
        end;
      end { TWindow.HandleEvent };

    procedure TWindow.InitFrame;
      var
        R: TRect;
      begin
      GetExtent(R);
      Frame := New(PFrame, Init(R));
      end;

    procedure TWindow.SetState(AState: Word; Enable: Boolean);
      var
        WindowCommands: TCommandSet;
      begin
      if  (AState and sfActive <> 0) and ReactOnCmd and
          (InterfaceData.Options and (ouiAutoCmdLine+ouiHideCmdline) =
           ouiAutoCmdLine)
      then
        ToggleCommandLine(not Enable);
      TGroup.SetState(AState, Enable);
      if AState = sfSelected then
        begin
        SetState(sfActive, Enable);
        if GetState(sfActive) then
          if Title <> nil then
            SetTitle(GetTitle(255));
        WindowCommands := [cmNext, cmPrev, cmNext2, cmPrev2, cmNext3,
           cmPrev3,
          cmTile, cmCascade, cmClearDesktop, cmWindowManager];
        if Flags and wfGrow+wfMove <> 0 then
          WindowCommands := WindowCommands+[cmResize];
        if Flags and wfClose <> 0 then
          WindowCommands := WindowCommands+[cmClose];
        if Flags and wfZoom <> 0 then
          WindowCommands := WindowCommands+[cmZoom];
        if Flags and wfMaxi <> 0 then
          WindowCommands := WindowCommands+[cmMaxi];
        if Enable then
          EnableCommands(WindowCommands)
        else
          DisableCommands(WindowCommands);
        end;
      end { TWindow.SetState };

    function TWindow.StandardScrollBar(AOptions: Word): PScrollBar;
      var
        R: TRect;
        S: PScrollBar;
      begin
      GetExtent(R);
      if AOptions and sbVertical = 0 then
        R.Assign(R.A.X+2, R.B.Y-1, R.B.X-2, R.B.Y)
      else
        R.Assign(R.B.X-1, R.A.Y+1, R.B.X, R.B.Y-1);
      S := New(PScrollBar, Init(R));
      Insert(S);
      if AOptions and sbHandleKeyboard <> 0 then
        S^.Options := S^.Options or ofPostProcess;
      StandardScrollBar := S;
      end;

    procedure TWindow.SizeLimits(var Min, Max: TPoint);
      begin
      TView.SizeLimits(Min, Max);
      Min.X := MinWinSize.X;
      Min.Y := MinWinSize.Y;
      end;

    procedure TWindow.Store(var S: TStream);
      begin
      TGroup.Store(S);
      S.Write(Flags, SizeOf(Byte)+SizeOf(TRect)*2+2*SizeOf(AInt));
      PutSubViewPtr(S, Frame);
      S.WriteStr(Title);
      end;

    procedure TWindow.Zoom;
      var
        R: TRect;
        Max, Min: TPoint;
      begin
      SizeLimits(Min, Max);
      if MaxiRect.A.X < High(MaxiRect.A.X) then
        Locate(MaxiRect)
      else if not Size.Equals(Max) then
        begin
        GetBounds(ZoomRect);
        R.A.Assign(0, 0);
        R.B := Max;
        Locate(R);
        end
      else
        Locate(ZoomRect);
      MaxiRect.A.X := High(MaxiRect.A.X);
      end;

    procedure TWindow.Maxi;
      var
        R: TRect;
        Max, Min: TPoint;
      begin
      if MaxiRect.A.X < High(MaxiRect.A.X) then
        begin
        Locate(MaxiRect);
        MaxiRect.A.X := High(MaxiRect.A.X);
        end
      else
        begin
        GetBounds(MaxiRect);
        SizeLimits(Min, Max);
        R.A.Assign(0, 0);
        R.B := Max;
        Dec(R.A.X);
        Inc(R.B.X);
        Locate(R);
        end;
      end;

    procedure TWindow.Draw;
      begin
      if GetState(sfActive) then
        if Title <> nil then
          SetTitle(GetTitle(255));
      inherited Draw;
      end;

    { Message dispatch function }

    function Message(Receiver: PView; What, Command: Word;
         InfoPtr: Pointer): Pointer;
      var
        Event: TEvent;
      begin
      Message := nil;
      if Receiver <> nil then
        begin
        Event.What := What;
        Event.Command := Command;
        Event.InfoPtr := InfoPtr;
        Receiver^.HandleEvent(Event);
        if Event.What = evNothing then
          Message := Event.InfoPtr;
        end;
      end;

    function MessageL(Receiver: PView; What, Command: Word;
         InfoLng: LongInt): Pointer;
      var
        Event: TEvent;
      begin
      MessageL := nil;
      if Receiver <> nil then
        begin
        Event.What := What;
        Event.Command := Command;
        Event.InfoLong := InfoLng;
        Receiver^.HandleEvent(Event);
        if Event.What = evNothing then
          MessageL := Event.InfoPtr;
        end;
      end;

    { Background work }

    procedure TView.Update;
      begin
      end;

    procedure RegisterToPrior(P: PView);
      begin
      if P = nil then
        Exit;
      if UpdatePrior = nil then
        New(UpdatePrior, Init(10, 10));
      UpdatePrior^.AtInsert(0, P);
      end;

    procedure RegisterToBackground(P: PView);
      begin
      if P = nil then
        Exit;
      if UpdateViews = nil then
        New(UpdateViews, Init(10, 10));
      UpdateViews^.AtInsert(0, P);
      end;

    procedure Deregister(P: PView);
      var
        I: LongInt;
      begin
      if UpdateViews <> nil then
        with UpdateViews^ do
          begin
          I := 0;
          while I < Count do
            if P = At(I)
            then
              AtDelete(I)
            else
              Inc(I);
          if Count = 0 then
            begin
            Dispose(UpdateViews, Done);
            UpdateViews := nil;
            end;
          end;
      if UpdatePrior <> nil then
        with UpdatePrior^ do
          begin
          I := 0;
          while I < Count do
            if P = At(I)
            then
              AtDelete(I)
            else
              Inc(I);
          if Count = 0 then
            begin
            Dispose(UpdatePrior, Done);
            UpdatePrior := nil;
            end;
          end;
      end { Deregister };

    procedure UpdateAll;
      procedure DoUpdate(P: PView);
        begin
        if  (P <> nil) and ((P^.UpdTicks = 0) or TimerExpired(P^.UpTmr))
        then
          begin
          P^.Update;
          NewTimer(P^.UpTmr, P^.UpdTicks);
          end;
        end;
      begin
      if UpdateViews <> nil then
        UpdateViews^.ForEach(@DoUpdate);
      if All and (UpdatePrior <> nil) and (UpdatePrior^.Count > 0) then
        DoUpdate(UpdatePrior^.At(0));
      end;

end.
