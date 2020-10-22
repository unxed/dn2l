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

unit Idlers;

interface

uses
  Views, Drivers, Defines, xTime
  ;

type
  PSSaver = ^TSSaver;
  TSSaver = object(TView)
    constructor Init(var Bounds: TRect);
    function Execute: Word; virtual;
    procedure Update; virtual;
    procedure Draw; virtual;
    destructor Done; virtual;
    end;

procedure CallExternalSaver(const FN: String);

type
  PProjector = ^TProjector;
  TProjector = object(TSSaver)
    Center: TPoint;
    Radius: Integer;
    Screen: Pointer;
    NextU: LongInt;
    LastDay: Byte;
    DX, DY, DDX: Integer;
    constructor Init;
    procedure Draw; virtual;
    procedure Update; virtual;
    end;

const
  StarSkyMult = 256;
  CNumSkyStars = 200;

type
  TSkyStar = record
    X, Y, DX, DY, Speed, Stat, Stage: Integer;
    Attr: Byte;
    Next: TEventTimer
    end;

  PStarSkySaver = ^TStarSkySaver;
  TStarSkySaver = object(TSSaver)
    Stars: array[1..CNumSkyStars] of TSkyStar;
    NumSkyStars: Integer;
    CommonDelay: Byte;
    constructor Init;
    procedure Draw; virtual;
    procedure Update; virtual;
    procedure InitStar(Index: Integer);
    end;

  TDestination = (dsUp, dsRight, dsDown, dsLeft);

  PClockSaver = ^TClockSaver;
  TClockSaver = object(TSSaver)
    X, Y, DX, DY, DDY: Integer;
    Dest: TDestination;
    Rest, Clr: Byte; {JO}
    dH, dM, dS, dSS: Word;
    constructor Init;
    procedure Update; virtual;
    procedure Draw; virtual;
    destructor Done; virtual;
    end;

const
  SSaver: PSSaver = nil;

implementation

uses
  Dos, DnExec, DNApp, Advance, Advance1, Memory, Startup, Commands,
   VideoMan
  ;

const
  StarChars: array[0..5] of Char = (#32, #250, #249, #7, #4, #15);
  MColors: array[0..3] of Byte = (1, 3, 9, 11);

procedure TSSaver.Update;
  begin
  end;

function TSSaver.Execute;
  var
    Event: TEvent;
    W: Word;
    WW: Word;
    T: TEventTimer;
  begin
  W := ShiftState shl 8+ShiftState2;
  NewTimer(T, 60*60*1000);
  repeat
    TinySlice;
    GetEvent(Event);
    Update;
    HideCursor;
    Application^.Idle;
    if StartupData.Unload and osuInactivityExit = 0 then
      NewTimer(T, 60*60*1000);
    WW := ShiftState shl 8+ShiftState2;
  until (Event.What <> evNothing) or (WW <> W) or TimerExpired(T);
  if TimerExpired(T) then
    Execute := cmCancel
  else
    Execute := cmOK;
  end { TSSaver.Execute };

procedure TSSaver.Draw;
  var
    B: TDrawBuffer;
  begin
  MoveChar(B, ' ', 07, Size.X);
  WriteLine(0, 0, Size.X, Size.Y, B);
  end;

constructor TSSaver.Init;
  begin
  inherited Init(Bounds);
  Options := Options or ofSelectable;
  HideCursor;
  HideMouse;
  end;

destructor TSSaver.Done;
  begin
  SSaver := nil;
  ShowMouse;
  inherited Done;
  end;

constructor TStarSkySaver.Init;
  label 1;
  var
    R: TRect;
    I, J: Integer;
  begin
  Randomize;
  Application^.GetExtent(R);
  inherited Init(R);
  NumSkyStars := 2*Size.X-64;
  for I := 1 to NumSkyStars do
    InitStar(I);
  CommonDelay := 131 div Size.X*2;
  end;

procedure TStarSkySaver.InitStar;
  var
    A, R: Integer;
  begin
  if Random(4) = 3 then
    with Stars[Index] do
      begin
      Speed := Size.X div 44+Random(3);
      A := Random(360);
      DX := Round(StarSkyMult*cos((2*PI*A)/360));
      DY := Round(StarSkyMult*sin((2*PI*A)/360));
      R := (1+Random(2))*(1+131 div Size.X);
      X := Size.X div 2*StarSkyMult+DX*Speed div R;
      Y := Size.Y div 2*StarSkyMult+DY*Speed div R;
      if Random(5) = 4 then
        Attr := 15
      else
        Attr := 7;
      Stat := 0;
      Stage := 10;
      NewTimer(Next, 0);
      end;
  end { TStarSkySaver.InitStar };

procedure TStarSkySaver.Draw;
  var
    B: TDrawBuffer;
    I, K: Integer;
    W: Word;
  begin
  MoveChar(B, ' ', $07, Size.X);
  WriteLine(0, 0, Size.X, Size.Y, B);
  if ScreenHeight*2 > ScreenWidth then
    K := 1
  else
    K := 2;
  for I := 1 to NumSkyStars div 2 do
    with Stars[I] do
      begin
      W := Attr*256+Byte(StarChars[8*Abs(X-Size.X div 2) div Size.X+1]);
      WriteBuf(X div StarSkyMult, Y div StarSkyMult, 1, 1, W);
      end;
  end;

procedure TStarSkySaver.Update;
  var
    W, I, J, K: Word;
    M: LongInt;
    Hour, Min, Sec, Sec100: Word;
    P: TPoint;
  const
    MM: array[0..1] of Integer = (-1, 1);
  label 1;

  function MMM(X, Y: Integer): Integer;
    var
      DX, DY: Integer;
    begin
    DX := 8*Abs(X div StarSkyMult-Size.X div 2) div Size.X+1;
    DY := 8*Abs(Y div StarSkyMult-Size.Y div 2) div Size.Y+1;
    if DX < DY then
      MMM := DY
    else
      MMM := DX;
    end;

  begin { TStarSkySaver.Update }
  if ScreenHeight*2 > ScreenWidth then
    K := 1
  else
    K := 2;
  for I := 1 to NumSkyStars div K do
    with Stars[I] do
      if TimerExpired(Next) then
        begin
        W := 0;
        WriteBuf(X div StarSkyMult, Y div StarSkyMult, 1, 1, W);
        Inc(X, (DX*Stage) div 12);
        Inc(Y, (DY*Stage) div 12);
        Inc(Stage);
        Stat := MMM(X, Y);
        P.X := X div StarSkyMult;
        P.Y := Y div StarSkyMult;
        MakeGlobal(P, P);
        if not MouseInView(P) or (DX = 0) and (DY = 0) then
          begin
          InitStar(I);
          end;
        W := Attr*256+Byte(StarChars[Stat]);
        WriteBuf(X div StarSkyMult, Y div StarSkyMult, 1, 1, W);
        NewTimer(Next, (CommonDelay+Speed)*10);
        end;
  if not MouseVisible then
    HideMouse;
  end { TStarSkySaver.Update };

constructor TProjector.Init;
  label 1;
  var
    R: TRect;
    I, J: Integer;
  begin
  I := ScreenHeight*ScreenWidth*2;
  if MaxAvail < I then
    Fail;
  Randomize;
  Application^.GetExtent(R);
  inherited Init(R);
  GetMem(Screen, I);
  Move(ScreenBuffer^, Screen^, I);
  Radius := 4;
  Center.X := Size.X div 2;
  Center.Y := Size.Y div 2;
  end;

type
  SpecArray = array[-3..4] of record
    l, R: ShortInt
    end;

procedure TProjector.Draw;
  const
    RR: array[1..4] of SpecArray =
      (
        ( (l: 2; R: 2), (l: 3; R: 3), (l: 4; R: 4), (l: 4; R: 4), (l: 4;
           R: 4), (l: 4; R: 4), (l: 3; R: 3), (l: 2; R: 2)),
        ( (l: -1; R: 0), (l: 2; R: 1), (l: 3; R: 2), (l: 4; R: 3), (l: 4;
           R: 3), (l: 4; R: 3), (l: 3; R: 2), (l: 2; R: 1)),
        ( (l: -1; R: 0), (l: -1; R: 0), (l: 1; R: 1), (l: 2; R: 2), (l: 3;
           R: 3), (l: 3; R: 3), (l: 2; R: 2), (l: 1; R: 1)),
        ( (l: -1; R: 0), (l: -1; R: 0), (l: 1; R: 0), (l: 2; R: 1), (l: 3;
           R: 2), (l: 2; R: 1), (l: 1; R: 0), (l: -1; R: 0))
      );
  var
    B: array[-20..220] of AWord;
    I, K, L: Integer;
    W: Word;
  begin
  if ScreenHeight*2 > ScreenWidth then
    K := 1
  else
    K := 2;
  for I := 0 to Size.Y-1 do
    begin
    MoveChar(B[0], ' ', $07, Size.X);
    L := I-Center.Y;
    if  (L >= -3) and (L <= 4) and (RR[Radius][L].L >= 0) then
      begin
      Move(PAWordArray(Screen)^
        [I*ScreenWidth+1+Center.X-RR[Radius][L].L*K],
        B[1+Center.X-RR[Radius][L].L*K],
          (RR[Radius][L].L+RR[Radius][L].R)*2*K);
      end;
    WriteLine(0, I, Size.X, 1, B[0]);
    end;
  end { TProjector.Draw };

procedure TProjector.Update;
  label 1;
  var
    Hour, Min, Sec, Sec100: Word;
    DT: DateTime;
    M, K: LongInt;
    P: TPoint;

  function MMM(X, Y: Integer): Integer;
    var
      DX, DY: Integer;
    begin
    DX := 6*Abs(X-Size.X div 2) div Size.X+1;
    DY := 6*Abs(Y-Size.Y div 2) div Size.Y+1;
    if DX < DY then
      MMM := DY
    else
      MMM := DX;
    end;

  begin
  GetTime(Hour, Min, Sec, Sec100);
  GetDate(DT.Year, DT.Month, DT.Day, DT.Sec);
  M := Sec100+Sec*100+LongInt(Min)*10000+LongInt(Hour)*1000000;
  if  (M < NextU) and (LastDay = DT.Day) then
    Exit;
  LastDay := DT.Day;
  if ScreenHeight*2 > ScreenWidth then
    K := 1
  else
    K := 2;
  if DDX <= 0 then
    begin
1:
    P := Center;
    DX := Random(7)-3;
    DY := Random(7)-3;
    DDX := Random(20);
    Inc(P.X, DX);
    Inc(P.Y, DY);
    if  (P.X <= 0) or (P.Y <= 0) or
        (P.X >= Size.X) or (P.Y >= Size.Y)
    then
      goto 1;
    end;
  Inc(Center.X, DX);
  Inc(Center.Y, DY);
  Dec(DDX);
  Radius := MMM(Center.X, Center.Y);
  DrawView;
  NextU := Sec100+15+Sec*100+LongInt(Min)*10000+LongInt(Hour)*1000000;
  if  (Center.X <= Radius) or (Center.Y <= Radius) or
      (Center.X >= (Size.X-Radius)) or (Center.Y >= (Size.Y-Radius))
  then
    DDX := 0;
  if not MouseVisible then
    HideMouse;
  end { TProjector.Update };

constructor TClockSaver.Init;
  var
    R: TRect;
  begin
  Application^.GetExtent(R);
  inherited Init(R);
  X := Size.X div 2-3;
  Y := Size.Y div 2;
  DX := 1-Random(3);
  DY := 1-Random(3);
  DDY := 3+Random(10);
  Clr := $09; {JO}
  SetBlink(True);
  Update;
  end;

procedure TClockSaver.Draw;
  var
    B, BB: TDrawBuffer;
    I: Integer;
  begin
  MoveChar(BB, ' ', $07, Size.X);
  for I := 0 to Size.Y-1 do
    if I <> Y
    then
      WriteLine(0, I, Size.X, 1, BB)
    else
      begin
      Move(BB, B, Size.X*2);
      MoveStr(B[X], Dec2(dH)+':'+Dec2(dM), Clr); {JO}
      {$IFNDEF Win32}
      WordRec(B[X+2]).Hi := Clr+$80; {JO}
      {$ENDIF}
      WriteLine(0, I, Size.X, 1, B);
      end
  end;

procedure TClockSaver.Update;
  var
    H, M, S, SS: Word;
    sX, sY: Integer;
    B: TDrawBuffer;
  label
    Skip;
  begin
  GetTime(H, M, S, SS);
  if  (S <> dS) or (M <> dM) or (H <> dH) or (dSS <> SS)
       and (SS and 7 = 7)
  then
    begin
    dH := H;
    dM := M;
    dS := S;
    dSS := SS;
    MoveChar(B, ' ', $07, 5);
    WriteLine(X, Y, 5, 1, B);
    Inc(X, DX);
    Inc(Y, DY);
    if X < 0 then
      X := 0;
    if X > Size.X-6 then
      X := Size.X-6;
    if Y < 0 then
      Y := 0;
    if Y > Size.Y-1 then
      Y := Size.Y-1;
    Dec(DDY);
    if DDY <= 0 then
      begin
      DX := 1-Random(3);
      DY := 1-Random(3);
      DDY := 3+Random(10);
      if Clr < $0E then
        Clr := Clr+1
      else
        Clr := $09; {JO}
      end;
    MoveStr(B[0], Dec2(dH)+':'+Dec2(dM), Clr); {JO}
    {$IFNDEF Win32}
    WordRec(B[2]).Hi := Clr+$80; {JO}
    {$ENDIF}
    WriteLine(X, Y, 5, 1, B);
    end;
  if not MouseVisible then
    HideMouse;
  end { TClockSaver.Update };

destructor TClockSaver.Done;
  begin
  SetBlink(CurrentBlink);
  inherited Done;
  end;

procedure CallExternalSaver(const FN: String);
  var
    MX, MY: Word;
    Event: TEvent;
    SM: Word;
  begin
  MX := MouseX;
  MY := MouseY;
  if  (MX div 8 = ScreenWidth-1) and (MY < 8) then
    Dec(MX, 8);
  HideMouse;
  DoneEvents;
  {JO}
  SM := ScreenMode;
  DoneVideo;
  {/JO}
  DoneDOSMem;
  SwapVectors;
  DoneMemory;
  {  DoneSysError;} {X-Man}
  AnsiExec(SourceDir+'SSAVERS\'+FN, '');
  {  InitSysError;} {X-Man}
  SwapVectors;
  InitDOSMem;
  InitMemory;
  InitVideo;
  InitEvents;
  HideMouse;
  if ButtonCount > 0 then
    if MouseVisible then
      HideMouse
    else
      begin
      MouseX := MX;
      MouseY := MY;
      end;
  MouseWhere.X := MX div 8;
  MouseWhere.Y := MY div 8;
  ShowMouse;
  Application^.Redraw;
  if DosError = 0 then
    begin
    Event.What := evCommand;
    Event.Command := cmValid;
    Event.InfoPtr := nil;
    Application^.PutEvent(Event);
    end;
  end { CallExternalSaver };

end.
