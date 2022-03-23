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

unit DNApp;

interface

uses
  Defines, Objects2, Streams, Drivers, Memory, Views, Menus,
  Dialogs, RStrings, Commands, xTime, Objects
  , Startup, Startupp
  ;

const

  EventsLen: Byte = 0;
  MaxEvents = 15;
var
  EventQueue: array[1..MaxEvents] of TEvent;

const
  IdleWas: Boolean = False; { Used by GetEvent }

  { TApplication palette entries }

  apColor = 0;
  apBlackWhite = 1;
  apMonochrome = 2;

  { TApplication palettes }

  { Turbo Vision 1.0 Color Palettes }

  CColor =
  #$0A#$70#$79#$75#$30#$39#$35#$8C#$8B#$8A#$31#$31#$78#$7F#$1F+
  #$31#$3F#$3A#$13#$13#$3E#$21#$3F#$70#$7F#$7A#$13#$13#$70#$7F#$7E+
  #$70#$70#$73#$13#$13#$70#$70#$7F#$75#$5B#$6F#$CF#$78#$5D#$78#$70+
  #$7F#$76#$0F#$9F#$0B#$9F#$79#$87#$78#$30#$0F#$3F#$31#$70#$79#$3E+
  #$30#$38#$3F#$0F#$07#$0B#$87#$8F#$87#$87#$8B#$B0#$87#$7F#$1C#$00+
  #$87#$8B#$87#$8B#$B0#$8B#$8B#$0F#$30#$7F#$30#$8B#$87#$70#$87#$8F+
  #$3F#$8F#$3F#$8E#$8E#$70#$8B#$87#$30#$3F#$0F#$3E#$0E#$31#$31#$00+
  #$87#$8F#$87#$8B#$B0#$87#$70#$8B#$30#$3F#$87#$8F#$87#$8F#$70#$7F+
  #$8F#$30#$8B#$83#$7F#$79#$39#$7E#$7E#$7F#$70#$37#$3F#$3F#$13#$13+
  #$30#$3F#$8F#$30#$38#$3E#$0F#$07#$0E#$87#$8B#$87#$87#$8B#$B0#$87+
  #$70#$8E#$30#$3B#$83#$8E#$38#$3F#$3A#$31#$3F#$70#$87#$8F#$8D#$89+
  #$84#$82#$C8#$6D#$80#$8C#$0C#$7C#$0B#$0F#$8B#$8B#$8E#$8F#$8D#$8B+
  #$83#$86#$81#$8E#$8A#$8F#$8E#$83#$8F#$8D#$8B#$8F#$8E#$70#$3F#$30+
  #$73#$3B#$70#$73#$3F#$3F#$4F#$4F#$70#$0F#$78#$7E#$48#$0E#$3F#$3F+
  #$08#$CF#$CE#$0F#$0E;

  CBlackWhite =
  #$0F#$70#$78#$7F#$07#$07#$0F#$07#$0F#$07#$70#$70#$07#$70#$0F+
  #$07#$0F#$07#$70#$70#$07#$70#$0F#$70#$7F#$7F#$70#$07#$70#$07#$0F+
  #$70#$7F#$7F#$70#$07#$70#$70#$7F#$7F#$07#$0F#$0F#$78#$0F#$78#$70+
  #$7F#$7F#$0F#$70#$0F#$07#$70#$70#$70#$07#$70#$0F#$07#$07#$78#$00+
  #$70#$78#$7F#$07#$07#$0F#$07#$0F#$0F#$0F#$70#$70#$07#$70#$07#$00+
  { 64 - 79 Editor}
  #$07#$0F#$0F#$70#$70#$07#$07#$F0#$70#$7F#$7F#$0F#$70#$70#$07#$07+
  #$70#$0F#$7F#$0F#$0F#$07#$70#$7F#$0F#$0F#$0F#$70#$7F#$0F#$0F#$00+
  #$07#$0F#$0F#$78#$78#$07#$78+
  #$07#$70#$7F#$07#$0F#$07#$0F+ {119 - 125 - File Info}
  #$70#$7F#$07#$0F#$07#$08#$7F#$79#$39#$7E#$7E#$7F#$70+
  {126 - 138 - File Find}
  #$07#$0F#$07#$70#$70#$07#$0F#$70+
  #$70#$78#$7F#$07#$07#$0F#$07#$0F#$0F#$0F#$70#$70#$07#$70#$0F#$70+
  #$0F#$08#$0F#$07#$0F#$0F#$0F#$07#$70#$07#$0F#$08#$07#$07#$07#$7F+
  #$7F#$07#$07#$0F#$78#$08#$0F#$0F#$0F#$78#$0F#$0F#$0F#$07#$07#$07+
  #$07#$07#$0F#$0F#$08#$0F#$0F#$0F#$0F#$0F#$70#$1F#$80#$7F#$8F#$78+
  #$7F#$0F#$0F#$70#$70#$07#$07#$08#$0F#$78#$7F#$1F#$17#$71#$30#$3F#$3F#$3F;

  CMonochrome =
  #$70#$07#$07#$01#$70#$70#$0F#$07#$0F#$0F#$0F#$0F#$70#$0F#$1F+
  #$31#$3F#$3A#$13#$13#$3E#$21#$3F#$70#$70#$0F#$13#$13#$70#$7F#$7E+
  #$70#$70#$70#$0F#$0F#$70#$70#$70#$0F#$07#$07#$07#$07#$01#$70#$70+
  #$07#$0F#$07#$70#$0F#$0F#$70#$0F#$0F#$07#$70#$01#$07#$70#$70#$0F+
  #$70#$70#$01#$0F#$07#$01#$07#$0F#$0F#$0F#$0F#$0F#$07#$70#$1C#$00+
  { 64 - 79 Editor}
  #$07#$0F#$0F#$0F#$0F#$07#$0F#$01#$70#$70#$70#$0F#$07#$0F#$07#$07+
  #$70#$0F#$70#$01#$01#$70#$0F#$07#$07#$07#$70#$0F#$70#$01#$01#$00+
  #$07#$0F#$0F#$0F#$0F#$07#$70+
  #$0F#$07#$0F#$07#$0F#$07#$0F+ {119 - 125 - File Info}
  #$70#$7F#$70#$0F#$0F#$07#$01#$79#$39#$7E#$7E#$7F#$70+
  {126 - 138 - File Find}
  #$07#$07#$0F#$0F#$0F#$07#$0F#$70+
  #$70#$70#$01#$0F#$07#$01#$07#$0F#$0F#$0F#$0F#$0F#$07#$70#$70#$70+
  #$01#$01#$0F#$07#$0F#$0F#$0F#$07#$0F#$07#$07#$07#$07#$07#$07#$0F+
  #$01#$07#$07#$01#$70#$0F#$01#$07#$0F#$70#$0F#$01#$0F#$07#$07#$07+
  #$07#$07#$0F#$0F#$01#$0F#$0F#$0F#$0F#$0F#$07#$3F#$70#$0F#$70#$01+
  #$0F#$0F#$0F#$70#$70#$07#$07#$07#$01#$70#$0F#$3F#$3F#$01#$70#$70#$07#$0F;

  { TBackground palette }

  CBackground = #1;

const
  SystemColors: array[apColor..apMonochrome] of String =
    (CColor, CBlackWhite, CMonochrome);

  { TBackground object }

type
  PBackground = ^TBackground;
  TBackground = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Pattern: Char;
    constructor Init(var Bounds: TRect; APattern: Char);
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure Store(var S: TStream);
    end;

  { TDesktop object }

  PDesktop = ^TDesktop;
  TDesktop = object(TGroup)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Background: PBackground;
    TileColumnsFirst: Boolean;
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    procedure Cascade(var R: TRect);
    procedure Clear;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure InitBackground; virtual;
    procedure Store(var S: TStream);
    procedure Tile(var R: TRect);
    procedure TileError; virtual;
    end;

  { TProgram object }

  { Palette layout }
  {     1 = TBackground }
  {  2- 7 = TMenuView and TStatusLine }
  {  8-15 = TWindow(Blue) }
  { 16-23 = TWindow(Cyan) }
  { 24-31 = TWindow(Gray) }
  { 32-63 = TDialog }

  PProgram = ^TProgram;
  TProgram = object(TGroup)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    IdleSecs: TEventTimer;
    {$IFDEF WIN32}
    {CS: CRITICAL_SECTION;}
    {$ENDIF}
    constructor Init;
    destructor Done; virtual;
    function CanMoveFocus: Boolean;
    function ExecuteDialog(P: PDialog; Data: Pointer): Word;
    {$IFDEF SS}
    procedure InsertIdler;
    procedure InsertIdlerN(N: Integer);
    procedure InsertAvIdlerN(Data: TSaversData; n: Integer);
    {$ENDIF}
    procedure GetEvent(var Event: TEvent); virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Idle; virtual;
    procedure InitDesktop; virtual;
    procedure InitMenuBar; virtual;
    procedure InitScreen; virtual;
    procedure InitStatusLine; virtual;
    procedure InitCommandLine; virtual;
    function InsertWindow(P: PWindow): PWindow;
    procedure ActivateView(P: PView);
    procedure OutOfMemory; virtual;
    procedure PutEvent(var Event: TEvent); virtual;
    procedure Run; virtual;
    function SetScreenMode(Mode: Word): Boolean;
    function ValidView(P: PView): PView;
    procedure Redraw; virtual;
    end;

  { TApplication object }

  PApplication = ^TApplication;
  TApplication = object(TProgram)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Clock: PView;
    constructor Init;
    destructor Done; virtual;
    procedure Cascade;
    procedure ShowUserScreen;
    procedure WhenShow; virtual;
    procedure GetTileRect(var R: TRect); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Tile;
    end;

  { Interface and resource procedures }

procedure UpdateWriteView(P: Pointer);
procedure OpenResource;
function ExecResource(Key: TDlgIdx; var Data): Word;
  {` Загрузить диалог из ресурса. Если есть PreExecuteDialog,
  то выполнить её. Выполнить диалог через ExecDialog`}
function ExecDialog(D: PDialog; var Data): Word;
  {` Выполнить диалог со стандартным обменом данными, то есть
   туда данные передаются всегда, а возвращаются только если
   результат - не cmCancel `}
function LoadResource(Key: TDlgIdx): PObject;
function GlobalMessage(What, Command: Word; InfoPtr: Pointer): Pointer;
function GlobalMessageL(What, Command: Word; InfoLng: LongInt): Pointer;
procedure GlobalEvent(What, Command: Word; InfoPtr: Pointer);
function ViewPresent(Command: Word; InfoPtr: Pointer): PView;
function WriteMsg(Text: String): PView;
function _WriteMsg(const Text: String): PView;
procedure ForceWriteShow(P: Pointer);
function GetString(Index: TStrIdx): String;
procedure ToggleCommandLine(OnOff: Boolean);
procedure AdjustToDesktopSize(var R: TRect; OldDeskSize: TPoint);

const

  { Public variables }

  {Cat: порядок переменных не менять, иначе будут проблемы с плагинами}

  Application: PProgram = nil;
  Desktop: PDesktop = nil;
  StatusLine: PStatusLine = nil;
  MenuBar: PMenuView = nil;
  CommandLine: PView = nil;
  ResourceStream: PStream = nil;
  LngStream: PStream = nil;
  LStringList: PStringList = nil;
  Resource: PIdxResource = nil;
  appPalette: Integer = apColor;

type
  PWriteWin = ^TWriteWin;
  TWriteWin = object(TWindow)
    Tmr: TEventTimer;
    IState: Byte;
    end;

  {$IFDEF OS2}
const
  PMWindowed: Boolean = False;
  {Устанавливается в dn2pmapi}
  {$ENDIF}

var
  PreExecuteDialog: procedure(D: PView);
    {` Подготовка диалога к выполнению. Может присваиваться перед
     вызовом ExecResource; автоматически очищается в ExecResource.
     Она вызывается только в ExecResource. Ни в TGroup.ExecView,
     ни в TGroup.ExecuteDialog PreExecuteDialog не вызывается.
     См. также TDialog.DirectLink `}

implementation

uses
  Dos, DNHelp, Advance, Advance1, Advance2, VideoMan,
  UserMenu, Microed, FViewer, DblWnd, CmdLine,
  {$IFDEF DNPRG} {Dn2PmApi // commented by unxed,} {$ENDIF} {AK155}
  {$IFDEF SS}Idlers, {$ENDIF}
  VpSysLow, Lfnvp, HistList, Advance7,
  DnIni,
  DnExec
  ;

var

  { Private variables }

  LastComTime: TEventTimer;

procedure ToggleCommandLine(OnOff: Boolean);
  begin
  if  (CommandLine <> nil)
       and (CommandLine^.GetState(sfVisible) xor OnOff)
  then
    Message(Application, evCommand, cmHideCmdLine, nil);
  end;

procedure AdjustToDesktopSize(var R: TRect; OldDeskSize: TPoint);
  var
    KX, KY: Real;
  begin
  if OldDeskSize.X = 0 then
    OldDeskSize.X := Desktop^.Size.X;
  if OldDeskSize.Y = 0 then
    OldDeskSize.Y := Desktop^.Size.Y;
  if Desktop^.Size.Equals(OldDeskSize) then
    Exit;
  KX := Desktop^.Size.X/OldDeskSize.X;
  R.A.X := Trunc(R.A.X*KX);
  R.B.X := Trunc(R.B.X*KX);
  KY := Desktop^.Size.Y/OldDeskSize.Y;
  R.A.Y := Trunc(R.A.Y*KY);
  R.B.Y := Trunc(R.B.Y*KY);
  if R.Empty or (R.A.X >= Desktop^.Size.X) or (R.B.X < 0)
    or (R.A.Y >= Desktop^.Size.Y) or (R.B.Y < 0)
    or (R.B.X-R.A.X < MinWinSize.X)
    or (R.B.Y-R.A.Y < MinWinSize.Y)
  then
    Desktop^.GetExtent(R);
  end { AdjustToDesktopSize };

{ TBackground }

constructor TBackground.Init(var Bounds: TRect; APattern: Char);
  begin
  TView.Init(Bounds);
  GrowMode := gfGrowHiX+gfGrowHiY;
  Pattern := APattern;
  end;

constructor TBackground.Load(var S: TStream);
  begin
  TView.Load(S);
  S.Read(Pattern, SizeOf(Pattern));
  end;

procedure TBackground.Draw;
  var
    B, B1: TDrawBuffer;
    I, H, Src: Integer;
  begin
  MoveChar(B, ' ', $07, Size.X);

  if UserScreen = nil
  then
    WriteLine(0, 0, Size.X, Size.Y, B)
  else
    begin
    H := (UserScreenSize div (UserScreenWidth*2));
    Src := 0;
    if  (InterfaceData.Options and ouiHideMenu = 0)
         and (Size.Y <> Application^.Size.Y)
    then
      begin
      Inc(Src, UserScreenWidth);
      Dec(H);
      end;
    for I := 0 to H-1 do
      begin
      B1 := B;
      Move(PAWordArray(UserScreen)^[Src], B1, UserScreenWidth*2);
      WriteLine(0, I, Size.X, 1, B1);
      Inc(Src, UserScreenWidth);
      end;
    WriteLine(0, H, Size.X, Size.Y-H+1, B);
    end;
  end { TBackground.Draw };

function TBackground.GetPalette: PPalette;
  const
    P: String[Length(CBackground)] = CBackground;
  begin
  GetPalette := @P;
  end;

procedure TBackground.Store(var S: TStream);
  begin
  TView.Store(S);
  S.Write(Pattern, SizeOf(Pattern));
  end;

{ TDesktop object }

constructor TDesktop.Init(var Bounds: TRect);
  begin
  inherited Init(Bounds);
  GrowMode := gfGrowHiX+gfGrowHiY;
  HelpCtx := hcDesktop;
  InitBackground;
  if Background <> nil then
    Insert(Background);
  end;

constructor TDesktop.Load(var S: TStream);
  begin
  inherited Load(S);
  GetSubViewPtr(S, Background);
  if Background = nil then
    {Cat}
    Fail;
  S.Read(TileColumnsFirst, SizeOf(TileColumnsFirst));
  end;

function Tileable(P: PView): Boolean;
  begin
  Tileable := (P^.Options and ofTileable <> 0) and
      (P^.State and sfVisible <> 0);
  end;

procedure TDesktop.Clear;
  procedure CloseView(P: PView);
    begin
    Message(P, evCommand, cmClose, nil)
    end;
  begin
  if Desktop^.Valid(cmClose) then
    Desktop^.ForEach(@CloseView);
  end;

procedure TDesktop.Cascade(var R: TRect);
  var
    CascadeNum: Integer;
    LastView: PView;
    Min, Max: TPoint;

  procedure DoCount(P: PView);
    begin
    if Tileable(P) then
      begin
      Inc(CascadeNum);
      LastView := P;
      end;
    end;

  procedure DoCascade(P: PView);
    var
      NR: TRect;
    begin
    if Tileable(P) and (CascadeNum >= 0) then
      begin
      NR.Copy(R);
      Inc(NR.A.X, CascadeNum);
      Inc(NR.A.Y, CascadeNum);
      P^.Locate(NR);
      Dec(CascadeNum);
      end;
    end;

  begin { TDesktop.Cascade }
  CascadeNum := 0;
  ForEach(@DoCount);
  if CascadeNum > 0 then
    begin
    LastView^.SizeLimits(Min, Max);
    if  (Min.X > R.B.X-R.A.X-CascadeNum) or
        (Min.Y > R.B.Y-R.A.Y-CascadeNum)
    then
      TileError
    else
      begin
      Dec(CascadeNum);
      Lock;
      ForEach(@DoCascade);
      UnLock;
      end;
    end;
  end { TDesktop.Cascade };

procedure TDesktop.HandleEvent(var Event: TEvent);
  var
    PCur, PNext: PView;
    E: TEvent;

  procedure _Next;
    begin
    SelectNext(False);
    end;
  procedure _Prev;
    begin
    if Valid(cmReleasedFocus) and (Current <> nil) then
      Current^.PutInFrontOf(Background);
    end;

  procedure _NextSame;
    var
      Old: PView;
    begin
    Old := Desktop^.Current;
    Lock;
    repeat
      _Next
    until (Old = Desktop^.Current)
         or (TypeOf(Desktop^.Current^) = TypeOf(Old^));
    UnLock;
    end;

  procedure _PrevSame;
    var
      Old: PView;
    begin
    Old := Desktop^.Current;
    Lock;
    repeat
      _Prev;
    until (Old = Desktop^.Current)
         or (TypeOf(Desktop^.Current^) = TypeOf(Old^));
    UnLock;
    end;

  procedure _NextOther;
    var
      Old: PView;
    begin
    Old := Desktop^.Current;
    Lock;
    repeat
      _Next
    until (Old = Desktop^.Current)
         or (TypeOf(Desktop^.Current^) <> TypeOf(Old^));
    UnLock;
    end;

  procedure _PrevOther;
    var
      Old: PView;
    begin
    Old := Desktop^.Current;
    Lock;
    repeat
      _Prev;
    until (Old = Desktop^.Current)
         or (TypeOf(Desktop^.Current^) <> TypeOf(Old^));
    UnLock;
    end;

  begin { TDesktop.HandleEvent }
  TGroup.HandleEvent(Event);
  if Event.What = evCommand then
    begin
    case Event.Command of
      cmNext:
        _Next;
      cmPrev:
        _Prev;
      cmNext2:
        _NextSame;
      cmPrev2:
        _PrevSame;
      cmNext3:
        _NextOther;
      cmPrev3:
        _PrevOther;
      else {case}
        Exit;
    end {case};
    ClearEvent(Event);
    end;
  end { TDesktop.HandleEvent };

procedure TDesktop.InitBackground;
  var
    R: TRect;
  begin
  GetExtent(R);
  New(Background, Init(R, #176));
  end;

function ISqr(X: Integer): Integer;
  assembler;
  {&Frame-} {$USES EBX, ECX}
asm
        MOV     ECX,X
        XOR     EBX,EBX
@@1:    INC     EBX
        MOV     EAX,EBX
        IMUL    EAX
        CMP     EAX,ECX
        JLE     @@1
        MOV     EAX,EBX
        DEC     EAX
end;

procedure MostEqualDivisors(N: Integer; var X, Y: Integer;
     FavorY: Boolean);
  var
    I: Integer;
  begin
  I := ISqr(N);
  if  ( (N mod I) <> 0) then
    if  (N mod (I+1)) = 0 then
      Inc(I);
  if I < (N div I) then
    I := N div I;
  if FavorY then
    begin
    X := N div I;
    Y := I;
    end
  else
    begin
    Y := N div I;
    X := I;
    end;
  end;

procedure TDesktop.Store(var S: TStream);
  begin
  inherited Store(S);
  PutSubViewPtr(S, Background);
  S.Write(TileColumnsFirst, SizeOf(TileColumnsFirst));
  end;

procedure TDesktop.Tile(var R: TRect);
  var
    NumCols, NumRows, NumTileable, LeftOver, TileNum: Integer;

  procedure DoCountTileable(P: PView);
    begin
    if Tileable(P) then
      Inc(NumTileable);
    end;

  function DividerLoc(Lo, Hi, Num, Pos: Integer): Integer;
    begin
    DividerLoc := (((Hi-Lo)*Pos) div Num)+Lo;
    end;

  procedure CalcTileRect(Pos: Integer; var NR: TRect);
    var
      X, Y, D: Integer;
    begin
    D := (NumCols-LeftOver)*NumRows;
    if Pos < D then
      begin
      X := Pos div NumRows;
      Y := Pos mod NumRows;
      end
    else
      begin
      X := (Pos-D) div (NumRows+1)+(NumCols-LeftOver);
      Y := (Pos-D) mod (NumRows+1);
      end;
    NR.A.X := DividerLoc(R.A.X, R.B.X, NumCols, X);
    NR.B.X := DividerLoc(R.A.X, R.B.X, NumCols, X+1);
    if Pos >= D then
      begin
      NR.A.Y := DividerLoc(R.A.Y, R.B.Y, NumRows+1, Y);
      NR.B.Y := DividerLoc(R.A.Y, R.B.Y, NumRows+1, Y+1);
      end
    else
      begin
      NR.A.Y := DividerLoc(R.A.Y, R.B.Y, NumRows, Y);
      NR.B.Y := DividerLoc(R.A.Y, R.B.Y, NumRows, Y+1);
      end;
    end { CalcTileRect };

  procedure DoTile(P: PView);
    var
      R: TRect;
    begin
    if Tileable(P) then
      begin
      CalcTileRect(TileNum, R);
      P^.Locate(R);
      Dec(TileNum);
      end;
    end;

  begin { TDesktop.Tile }
  NumTileable := 0;
  ForEach(@DoCountTileable);
  if NumTileable > 0 then
    begin
    MostEqualDivisors(NumTileable, NumCols, NumRows, not TileColumnsFirst);
    if  ( (R.B.X-R.A.X) div NumCols = 0) or
        ( (R.B.Y-R.A.Y) div NumRows = 0)
    then
      TileError
    else
      begin
      LeftOver := NumTileable mod NumCols;
      TileNum := NumTileable-1;
      Lock;
      ForEach(@DoTile);
      UnLock;
      end;
    end;
  end { TDesktop.Tile };

procedure TDesktop.TileError;
  begin
  end;

{ TProgram }

constructor TProgram.Init;
  var
    R: TRect;
  begin
  Application := @Self;
  InitScreen;
  R.Assign(0, 0, ScreenWidth, ScreenHeight);
  TGroup.Init(R);
  State := sfVisible+sfSelected+sfFocused+sfModal+sfExposed;
  Options := 0;
  Buffer := ScreenBuffer;
  InitStatusLine;
  InitMenuBar;
  InitDesktop;
  if StatusLine <> nil then
    Insert(StatusLine);
  if MenuBar <> nil then
    Insert(MenuBar);
  if Desktop <> nil then
    Insert(Desktop);
  InitCommandLine;
  if StatusLine <> nil then
    StatusLine^.GrowTo(StatusLine^.Size.X, 1);
  if MenuBar <> nil then
    MenuBar^.GrowTo(MenuBar^.Size.X, 1);
  NewTimer(LastComTime, 0);
  NewTimer(IdleSecs, 0);
  end { TProgram.Init };

destructor TProgram.Done;
  begin
  if MenuBar <> nil then
    Dispose(MenuBar, Done);
  MenuBar := nil;
  if StatusLine <> nil then
    Dispose(StatusLine, Done);
  StatusLine := nil;
  if Desktop <> nil then
    Dispose(Desktop, Done);
  Desktop := nil;
  Application := nil;
  inherited Done;
  end;

function TProgram.CanMoveFocus: Boolean;
  begin
  CanMoveFocus := Desktop^.Valid(cmReleasedFocus);
  end;

function TProgram.ExecuteDialog(P: PDialog; Data: Pointer): Word;
  var
    C: Word;
  begin
  ExecuteDialog := cmCancel;
  if ValidView(P) <> nil then
    begin
    if Data <> nil then
      P^.SetData(Data^);
    C := {Desktop^.}ExecView(P); { X-Man }
    if  (C <> cmCancel) and (Data <> nil) then
      P^.GetData(Data^);
    Dispose(P, Done);
    ExecuteDialog := C;
    end;
  end;

procedure ExecExit;
  var
    S: String; {DataCompBoy}
  begin
  {if SSaver <> nil then SSaver^.Free;}
  S := GetEnv('DNIDLE');
  if S = '' then
    begin
    TottalExit := True;
    Application^.Done;
    SysTVInitCursor;
    Halt(0);
    end
  else
    Message(Application, evCommand, cmExecString, @S);
  end;

{$IFDEF SS}
procedure TProgram.InsertIdler;
  begin
  if {$IFDEF OS2}PMWindowed or {$ENDIF}(SkyEnabled > 0)
     or SaversData.Mouse and
      (MouseWhere.Y = Size.Y-1) and (MouseWhere.X = Size.X-1)
    or (SaversData.Selected.List = nil) or
      (SaversData.Selected.List^.Count = 0) or
      (SSaver <> nil)
  then
    Exit;
  InsertIdlerN(Random(SaversData.Selected.List^.Count));
  end;

procedure TProgram.InsertIdlerN;
  var
    I: Integer;
    S: String;
    R: TRect;
    Event: TEvent;
    MX, MY: Word;
  begin
  if  (SkyEnabled > 0) or SaversData.Mouse and
      (MouseWhere.Y = Size.Y-1) and (MouseWhere.X = Size.X-1)
    or (SaversData.Selected.List = nil) or
      (SaversData.Selected.List^.Count = 0) or
      (SSaver <> nil)
  then
    Exit;

  S := PString(SaversData.Selected.List^.At(N))^;
  if S[1] = #249 then
    begin
    Application^.GetExtent(R);
    case S[3] of
      'F':
        SSaver := New(PProjector, Init);
      'B':
        SSaver := New(PSSaver, Init(R));
      'C':
        SSaver := New(PClockSaver, Init);
      else {case}
        SSaver := New(PStarSkySaver, Init);
    end {case};
    if  (SSaver <> nil) then
      begin
      if ExecView(SSaver) = cmCancel then
        ExecExit;
      Dispose(SSaver, Done);
      SSaver := nil;
      end;
    end
  else
    begin
    CallExternalSaver(S);
    end;
  end { TProgram.InsertIdlerN };

procedure TProgram.InsertAvIdlerN;
  var
    I: Integer;
    S: String;
    R: TRect;
    Event: TEvent;
    MX, MY: Word;
  begin
  if  (SkyEnabled > 0) or SaversData.Mouse and
      (MouseWhere.Y = Size.Y-1) and (MouseWhere.X = Size.X-1)
    or (Data.Available.List = nil) or
      (Data.Available.List^.Count = 0) or
      (SSaver <> nil)
  then
    Exit;

  S := PString(Data.Available.List^.At(n))^;
  if S[1] = #249 then
    begin
    Application^.GetExtent(R);
    case S[3] of
      'F':
        SSaver := New(PProjector, Init);
      'B':
        SSaver := New(PSSaver, Init(R));
      'C':
        SSaver := New(PClockSaver, Init);
      else {case}
        SSaver := New(PStarSkySaver, Init);
    end {case};
    if  (SSaver <> nil) then
      begin
      if ExecView(SSaver) = cmCancel then
        ExecExit;
      Dispose(SSaver, Done);
      SSaver := nil;
      end;
    end
  else
    CallExternalSaver(S);
  end { TProgram.InsertAvIdlerN };
{$ENDIF}

procedure TProgram.GetEvent(var Event: TEvent);

  function ContainsMouse(P: PView): Boolean;
    begin
    ContainsMouse := (P^.State and sfVisible <> 0) and
      P^.MouseInView(Event.Where);
    end;

  var
    P: procedure;
    R: TRect;
    A1, A2, CursorY: SmallWord;
    y1, y2: Integer;
   {$IFDEF OS2}
    FocusTmr: TEventTimer; {JO}
   {$ENDIF}
  label 11;
  begin
11:
  ShiftState := SysTVGetShiftState;
  if EventsLen > 0 then
    begin
    Event := EventQueue[1];
    Dec(EventsLen);
    Move(EventQueue[2], EventQueue[1], SizeOf(Event)*EventsLen);
    NewTimer(IdleSecs, 0);
   {$IFDEF OS2}
    NewTimer(FocusTmr, 450);
   {$ENDIF}
    SliceAwake;
    end
  else
    begin
    EventsLen := 0;
    GetMouseEvent(Event);
    if Event.What = evNothing then
      begin
      {$IFDEF DNPRG}
      {AK155 В неактивном оконном состоянии не надо реагировать на
          Shift, Alt, Ctrl. И надо прекратить отлов комбинаций вроде
          нажатия и отпускания Alt, что не не попадать в быстрый поиск,
          если покинули DN по Alt-Tab и вернулись по Alt-Tab }
      //JO: поскольку в осевом VIO-окне из-за такого сбpасывания
      //    OldShiftState не pаботает активизация быстpого поиска
      //    по Alt-Alt, то делаем его по таймеpу, чтобы успеть отpаботать
      //    эту комбиацию клавиш
      GetKeyEvent(Event);
      if false then // fixme: commented by unxed
      //if  DN_IsBGWindow then
        begin
       {$IFDEF OS2}
        if TimerExpired(FocusTmr) then
       {$ENDIF}
          begin
          OldShiftState := 0;
          DoubleAltUnlock := False;
          DoubleCtrlUnlock := False;
          end;
        ClearEvent(Event);
        end
      else
        {$ENDIF}
        begin
       {$IFDEF OS2}
        NewTimer(FocusTmr, 450);
       {$ENDIF}
        end;
      if Event.What = evNothing then
        begin
        Idle;
        if EventsLen > 0 then
          goto 11;
        if  (ElapsedTime(IdleSecs) > 3600*1000) and
            (StartupData.Unload and osuInactivityExit <> 0)
        then
          begin
          ExecExit;
          end;
        end
      else
        begin
        NewTimer(IdleSecs, 0);
       {$IFDEF OS2}
        NewTimer(FocusTmr, 450);
       {$ENDIF}
        SliceAwake;
        end;
      end
    else
      begin
      NewTimer(IdleSecs, 0);
     {$IFDEF OS2}
      NewTimer(FocusTmr, 450);
     {$ENDIF}
      SliceAwake;
      end;
    end;

  if  (Event.What <> evNothing) or (SkyEnabled <> 0) then
    NewTimer(LastComTime, 0)
  else
    {$IFDEF SS}
   if (SSaver = nil) and (SkyEnabled = 0) and (SkyDelay <> 255) and
      (ElapsedTime(LastComTime) >= SkyDelay*60*1000)
  then
    begin
    InsertIdler;
    NewTimer(LastComTime, 0)
    end;
  {$ENDIF}
  if  (StatusLine <> nil)
    {$IFDEF SS}
    and (SSaver = nil)
    {$ENDIF}
    then
    if  (Event.What and evMouseDown <> 0)
         and (Event.Buttons and mbLeftButton <> 0) and
        (FirstThat(@ContainsMouse) = PView(StatusLine))
    then
      StatusLine^.HandleEvent(Event);

  if  (IdleWas) and
      (SkyEnabled = 0) and
    {$IFDEF SS}
      (SSaver = nil) and
    {$ENDIF}
      (CommandLine <> nil) and
      (CommandLine^.Size.Y <> 0) and
      (CommandLine^.GetState(sfVisible))
  then
    begin
    IdleWas := False;
    (*
 {AK155 12-12-2001 Это хитрое условие вызова CommandLine^.Update было нужно,
 чтобы комстрока не отнимала курсор у строк ввода, редактора и т.п. Но теперь
весь анализ перенесен внутрь CommandLine^.Update }
    {$IFDEF VIRTUALPASCAL}
     SysGetCurPos(A2, CursorY); {AK155}
    {$ELSE}
    asm
     mov ah,3
     xor bx,bx
     int 10h
     mov A1, dx
     mov A2, cx
    end;
    CursorY := Hi(A1);
    {$ENDIF}
    if CursorY = Size.Y - 1 - Byte( InterfaceData.Options and ouiHideStatus = 0 )
    then
{/AK155}*)
    CommandLine^.Update;
    end;
  end { TProgram.GetEvent };

function TProgram.GetPalette: PPalette;
  begin
  GetPalette := @SystemColors[appPalette];
  end;

procedure TProgram.Redraw;
  begin
  if  (Size.X <> ScreenWidth) or (Size.Y <> ScreenHeight) then
    GrowTo(ScreenWidth, ScreenHeight)
  else
    inherited Redraw;
  end;

procedure TProgram.HandleEvent(var Event: TEvent);
  var
    I: Word;
    C: Char;
  begin
  if  (Event.What = evKeyDown) and
      (Event.KeyCode >= kbCtrlShift1) and
      (Event.KeyCode <= kbCtrlShift9)
  then
    if MessageL(Desktop, evBroadcast,
        cmSelectWindowNum,
          (Event.KeyCode-kbCtrlShift1) shr 8+1) <> nil
    then
      ClearEvent(Event);
  TGroup.HandleEvent(Event);
  if Event.What = evCommand then
    if Event.Command = cmQuit then
      begin
      EndModal(cmQuit);
      ClearEvent(Event);
      end;
  end;

procedure TProgram.Idle;
  var
    P: PView;
    E: TEvent;
    A1, A2: Word;
  begin
  {$IFDEF DNPRG} {$IFDEF OS2}
  {AK155 В неактивном оконном состоянии не надо реагировать на
          Shift, Alt, Ctrl}
  if not DN_IsBGWindow then
    {$ENDIF} {$ENDIF}
    if StatusLine <> nil then
      StatusLine^.Update;
  if CommandSetChanged then
    begin
    Message(@Self, evBroadcast, cmCommandSetChanged, nil);
    CommandSetChanged := False;
    end;
  end;

procedure TProgram.InitDesktop;
  var
    R: TRect;
  begin
  GetExtent(R);
  if SystemData.Options and ouiHideMenu = 0 then
    Inc(R.A.Y);
  if SystemData.Options and ouiHideStatus = 0 then
    Dec(R.B.Y);
  New(Desktop, Init(R));
  end;

procedure TProgram.InitMenuBar;
  var
    R: TRect;
  begin
  GetExtent(R);
  R.B.Y := R.A.Y+1;
  MenuBar := New(PMenuBar, Init(R, nil));
  end;

procedure TProgram.InitScreen;
  begin
  if ScreenMode <> smMono then
    begin
    if ScreenWidth div ScreenHeight < 2 then
      ShadowSize.X := 1
    else
      ShadowSize.X := 2;
    ShadowSize.Y := 1;
    ShowMarkers := False;
    if ScreenMode = smBW80 then
      appPalette := apBlackWhite { else
      AppPalette := apColor};
    end
  else
    begin
    ShadowSize.X := 0;
    ShadowSize.Y := 0;
    ShowMarkers := True;
    appPalette := apMonochrome;
    end;
  end;

procedure TProgram.InitStatusLine;
  var
    R: TRect;
  begin
  GetExtent(R);
  R.A.Y := R.B.Y-1;
  New(StatusLine, Init(R,
      NewStatusDef(0, $FFFF,
        NewStatusKey('~Alt-X~ Exit', kbAltX, cmQuit,
          nil), nil)));
  end;

procedure TProgram.InitCommandLine;
  begin
  end;

function TProgram.InsertWindow(P: PWindow): PWindow;
  var
    PP: TPoint;
    EV: TEvent;
    PV: PView;
  begin
  InsertWindow := nil;
  if ValidView(P) <> nil then
    if CanMoveFocus then
      begin
      Desktop^.Insert(P);
      InsertWindow := P;
      {P^.Select;}
      ActivateView(P);
      end
    else
      Dispose(P, Done);
  Idle;
  end;

procedure TProgram.ActivateView;
  var
    EV: TEvent;
    PP: TPoint;
  begin
  if P = nil then
    Exit;
  EV.What := evMouseDown;
  PP.X := P^.Origin.X;
  PP.Y := P^.Origin.Y+P^.Size.Y-2;
  Desktop^.MakeGlobal(PP, PP);
  EV.Where := PP;
  EV.Buttons := mbRightButton; {PutEvent(EV);}
  end;

procedure TProgram.OutOfMemory;
  begin
  end;

procedure TProgram.PutEvent(var Event: TEvent);
  begin
  if  (Event.What = evNothing) then
    Exit;
  if  (EventsLen = MaxEvents) then
    Move(EventQueue[2], EventQueue[1], (MaxEvents-1)*SizeOf(Event))
  else
    Inc(EventsLen);
  EventQueue[EventsLen] := Event;
  end;

procedure TProgram.Run;
  begin
  Execute;
  end;

function TProgram.SetScreenMode(Mode: Word): Boolean;
  var
    R: TRect;
  begin
  HideMouse;
  DoneEvents;
  DoneMemory;
  DoneVideo;
  Result := SetVideoMode(Mode);
  if Result then
    begin
    if  (StartupData.Load and osuRestoreScrMode <> 0) then
      begin
      {$IFDEF OS2}
      if not PMWindowed then
        begin
        {$ENDIF}
        NonVIOScreenMode := ScreenMode;
        {$IFDEF OS2}
        end
      else
        VIOScreenMode := ScreenMode;
      {$ENDIF}
      end;
    end;
  InitVideo;
  InitMemory;
  InitEvents;
  InitScreen;
  Buffer := ScreenBuffer;
  R.Assign(0, 0, ScreenWidth, ScreenHeight);
  ChangeBounds(R);
  ShowMouse;
  SetBlink(CurrentBlink);
  end { TProgram.SetScreenMode };

function TProgram.ValidView(P: PView): PView;
  begin
  ValidView := nil;
  if P <> nil then
    begin
    if LowMemory then
      begin
      Dispose(P, Done);
      OutOfMemory;
      Exit;
      end;
    if not P^.Valid(cmValid) then
      begin
      Dispose(P, Done);
      Exit;
      end;
    ValidView := P;
    end;
  end;

{ TApplication }

constructor TApplication.Init;
  begin
  InitMemory;
  InitVideo;
  InitEvents;
  InitHistory;
  TProgram.Init;
  end;

destructor TApplication.Done;
  begin
  if LStringList <> nil then
    Dispose(LStringList, Done);
  LStringList := nil;
  if LngStream <> nil then
    Dispose(LngStream, Done);
  LngStream := nil;
  if Resource <> nil then
    Dispose(Resource, Done);
  Resource := nil;
  TProgram.Done;
  DoneHistory;
  DoneSysError;
  DoneEvents;
  DoneVideo;
  DoneMemory;
  end;

procedure TApplication.ShowUserScreen;
  var
    PV: PView;
    R: TRect;
  begin
  GetExtent(R);
  PV := New(PBackground, Init(R, ' '));
  if CommandLine <> nil then
    CommandLine^.SetState(sfDisabled, True);
  Insert(PV);
  if  (StartupData.Load and osuResetPalette <> 0) and VGASystem
  then
    SetPalette(vga_default);
  WhenShow;
  if CommandLine <> nil then
    CommandLine^.SetState(sfDisabled, False);
  if  (StartupData.Load and osuResetPalette <> 0) and VGASystem
  then
    SetPalette(VGA_palette);
  PV^.Free;
  end { TApplication.ShowUserScreen };

procedure TApplication.WhenShow;
  var
    Event: TEvent;
  begin
  repeat
    GetEvent(Event);
    if Event.What = evNothing then
      TinySlice;
  until (Event.What and (evKeyDown+evMouseDown) <> 0);
  ClearEvent(Event);
  end;

procedure TApplication.Cascade;
  var
    R: TRect;
  begin
  GetTileRect(R);
  if Desktop <> nil then
    Desktop^.Cascade(R);
  end;

procedure TApplication.GetTileRect(var R: TRect);
  begin
  Desktop^.GetExtent(R);
  end;

procedure TApplication.HandleEvent(var Event: TEvent);
  begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      begin
      case Event.Command of
        cmTile:
          Tile;
        cmCascade:
          Cascade;
        else {case}
          Exit;
      end {case};
      ClearEvent(Event);
      end;
    {Cat: штука странная, к тому же kbCtrlShiftTilde в DN/2 не ловится}
    (*
    evKeyDown: if Event.KeyCode=kbCtrlShiftTilde then begin
                MessageBox('Status dumped to DN.ERR',nil,0);
                ClearEvent(Event);
               end;
*)
    evMouseDown:
      begin {AK155 Щелчок мыши вне всего - как Ctrl-O }
      Event.What := evCommand; Event.Command := cmShowOutput;
      PutEvent(Event);
      ClearEvent(Event);
      end;
  end {case};
  end { TApplication.HandleEvent };

procedure TApplication.Tile;
  var
    R: TRect;
  begin
  GetTileRect(R);
  if Desktop <> nil then
    Desktop^.Tile(R);
  end;

procedure ResourceFail(const S: String);
  begin
  Writeln('Could not open resource file ('+S+')');
  SysTVInitCursor;
  Halt(219);
  end;

function OpenResourceStream(Ext: ExtStr): PBufStream;
  var
    S: String;
    PS: PBufStream;
  begin
  S := GetEnv('DNDLG');
  if S = '' then
    S := SourceDir;
  MakeSlash(S);
  PS := New(PBufStream,
      Init(S+LngId+Ext, Open_Share_DenyWrite+Open_Access_ReadOnly, 1024));
  if PS^.Status <> stOK then
    begin
    Dispose(PS, Done);
    PS := New(PBufStream,
        Init(StartupDir+LngId+Ext,
           Open_Share_DenyWrite+Open_Access_ReadOnly, 1024));
    if PS^.Status <> stOK then
      ResourceFail(LngId+Ext);
    end;
  OpenResourceStream := PS;
  end { OpenResourceStream };

procedure OpenResource;
  begin
  if Resource <> nil then
    Exit;
  ResourceStream := OpenResourceStream('.DLG');
  New(Resource, Init(ResourceStream));
  end;

function LoadDialog(Key: TDlgIdx): PDialog;
  begin
  Result := nil;
  OpenResource;
  if Resource = nil then
    Exit;
  Result := PDialog(Resource^.Get(Key));
  Result := PDialog(Application^.ValidView(Result));
  if Result = nil then
    Exit;
  end;

function ExecDialog(D: PDialog; var Data): Word;
  begin
  D^.SetData(Data);
  Result := Desktop^.ExecView(D);
  if Result <> cmCancel then
    D^.GetData(Data);
  end;

function ExecResource(Key: TDlgIdx; var Data): Word;
  assembler; {&USES None} {&FRAME-}
{ AK155. Тут может вызываться PreExecuteDialog, которая вполне
может оказаться локальной процедурой. Поэтому ExecResource
не должна иметь стекового фрейма, иначе в PreExecuteDialog будут
испорчены обращения к локальным переменным объемлющей процедуры.
Паскальную процедуру без фрейма VP умеет строить только если
у неё нет ни параметров, ни локальных переменных, поэтому
приходится писать на ассемблере. Чтобы не делать ассемблерную
процедуру слишком большой, из прежнего текста ExecResource
вычленены LoadDialog и ExecDialog. Последняя, кстати, может
пригодиться и сама по себе, поэтому она вынесена в интерфейс. }
  var
    D: PDialog;
    Res: Longint;
  asm
  mov [Res], cmCancel; // Result := cmCancel;

  movsx eax,byte ptr[Key]
  push eax
  call LoadDialog
  mov [D],eax // D := LoadDialog(Key);

  or eax,eax
  jz @@1 // if D = nil then Exit;

  cmp [PreExecuteDialog],0
  jz @@2 // if @PreExecuteDialog <> nil then

  push dword ptr[D]
  call [PreExecuteDialog] //  PreExecuteDialog(D);
@@2:

  mov  eax,[Data]
  push [D]
  push eax
  call ExecDialog
  mov [Res],eax // Result := ExecDialog(D, Data);

  mov eax,[D]
  push 1
  push eax
  mov eax,[eax]
  call dword ptr [eax + $0C] //  Dispose(D, Done);

  mov [PreExecuteDialog],0 // @PreExecuteDialog := nil;
@@1:
  mov eax,[Res]
  end;


function LoadResource;
  begin
  LoadResource := nil;
  OpenResource;
  if Resource = nil then
    Exit;
  LoadResource := Resource^.Get(Key);
  end;

function GlobalMessage;

  var
    Event: TEvent;
    Ptr: Pointer;

  procedure Action(View: PView);
    begin
    Event.What := What;
    Event.Command := Command;
    Event.InfoPtr := InfoPtr;
    View^.HandleEvent(Event);
    if Event.What = evNothing then
      Ptr := Event.InfoPtr;

    end;

  begin
  Ptr := nil;
  if Command = cmPanelReread then
    Command := cmTotalReread;
  Application^.ForEach(@Action);      {<dnapp.001>}
  Desktop^.ForEach(@Action);
  GlobalMessage := Ptr;
  end { GlobalMessage };

function GlobalMessageL;

  var
    Event: TEvent;
    Ptr: Pointer;

  procedure Action(View: PView);
    begin
    Event.What := What;
    Event.Command := Command;
    Event.InfoLong := InfoLng;
    View^.HandleEvent(Event);
    if Event.What = evNothing then
      Ptr := Event.InfoPtr;

    end;

  begin
  Ptr := nil;
  if Command = cmPanelReread then
    Command := cmTotalReread;
  Application^.ForEach(@Action);
  Desktop^.ForEach(@Action);
  GlobalMessageL := Ptr;
  end { GlobalMessageL };

procedure GlobalEvent;

  var
    Event: TEvent;

  procedure Action(View: PView);
    begin
    if  (Event.What <> evNothing) and (View <> nil) then
      View^.HandleEvent(Event);
    end;

  begin
  Event.What := What;
  if Command = cmPanelReread then
    Event.Command := cmTotalReread
  else
    Event.Command := Command;
  Event.InfoPtr := InfoPtr;
  Application^.ForEach(@Action);
  Desktop^.ForEach(@Action);
  end;

var
  ActInt: Boolean;
  V: PView;
  Event: TEvent;

function ViewPresent;

  procedure FAction(View: PView);
    begin
    if ActInt then
      Exit;
    View^.HandleEvent(Event);
    if Event.What = evNothing then
      V := Event.InfoPtr;
    end;

  begin
  V := nil;
  ActInt := False;
  Event.What := evCommand;
  Event.Command := Command;
  Event.InfoPtr := InfoPtr;
  Application^.ForEach(@FAction);
  Desktop^.ForEach(@FAction);
  ViewPresent := V;
  end;

procedure UpdateWriteView(P: Pointer);
  begin
  if P = nil then
    Exit;
  with PWriteWin(P)^ do
    begin
    case IState of
      0:
        if TimerExpired(Tmr) then
          Desktop^.Insert(P)
        else
          Exit;
      1:
        if TimerExpired(Tmr) then
          Application^.Insert(P)
        else
          Exit;
      2:
        Exit;
    end {case};
    IState := 2;
    end;
  end { UpdateWriteView };

function WriteMsg;
  var
    W: PWriteWin;
    R: TRect;
    P: PStaticText;
  begin
  R.Assign(1, 1, 30, 8);
  New(W, Init(R, '', wnNoNumber));
  W^.Options := W^.Options or ofCentered;
  W^.Palette := wpGrayWindow;
  W^.Flags := 0;
  W^.Options := W^.Options and not (ofSelectable or ofTopSelect);

  W^.GetExtent(R);
  R.Grow(-1, -1);
  New(P, Init(R, Text));
  W^.Insert(P);

  W^.SetState(sfShadow, True);
  W^.IState := Byte(Text[1] = ' ');
  NewTimer(W^.Tmr, 200);
  Desktop^.Insert(W);
  WriteMsg := W;
  end { WriteMsg };

procedure ForceWriteShow;
  begin
  if P = nil then
    Exit;
  NewTimer(PWriteWin(P)^.Tmr, 0);
  UpdateWriteView(P);
  end;

function _WriteMsg;
  var
    W: PWriteWin;
  begin
  W := PWriteWin(WriteMsg(Text));
  ForceWriteShow(W);
  _WriteMsg := W;
  end;

procedure InitLngStream;
  var
    PS, XS: PStream;
    S: String;

  begin
  PS := OpenResourceStream('.LNG');
  XS := New(PMemoryStream, Init(i32(PS^.GetSize), i32(PS^.GetSize)));
  if XS^.Status <> stOK then
    begin
    Dispose(XS, Done);
    XS := nil;
    end;

  if XS <> nil then
    begin
    XS^.CopyFrom(PS^, PS^.GetSize);
    if XS^.Status = stOK then
      begin
      Dispose(PS, Done);
      PS := XS;
      end
    else
      Dispose(XS, Done);
    end;

  LngStream := PS;

  PS^.Seek(0);
  LStringList := PStringList(PS^.Get);
  if  (PS^.Status <> stOK) or (LStringList = nil) then
    ResourceFail('reading '+LngId+'.LNG');

  end { InitLngStream };

function GetString;
  var
    S: String;
    I: Integer;
  begin
  if LStringList = nil then
    InitLngStream;
  S := LStringList^.Get(Word(Index));
  {Cat: вообще говоря, здесь имело бы смысл проверять, не получили ли мы
        пустую строку; если мы будем возвращать пустую строку, то может
        случиться беда вроде такой: NewStr(GetString(...)), т.е. в случае
        кривого файла ресурсов получим nil и при обращении к этой строке
        ДН упадёт. Однако, в языковом файле пустые строки всё же зачем-то
        присутствуют}
  GetString := S;
  end { GetString };

end.
