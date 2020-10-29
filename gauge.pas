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

unit Gauge;

{$V-}

interface

uses
  Drivers, Defines, Views, Dialogs, Objects
  ;

const
  cmUpdateGauge = 12000;
  cmResetGauge = 12001;
  cmAddGauge = 12002;

type
  PPercentGauge = ^TPercentGauge;
  TPercentGauge = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    MaxValue: LongInt;
    CurValue: LongInt;
    constructor Init(var Bounds: TRect; AMaxValue: LongInt);
    procedure Draw; virtual;
    procedure UpdateView(Progress: LongInt); virtual;
    procedure AddProgress(Progress: LongInt);
    procedure HandleEvent(var Event: TEvent); virtual;
    function SolveForX(Y, Z: LongInt): Integer;
    function SolveForY(X, Z: LongInt): Integer;
    end;

  PBarGauge = ^TBarGauge;
  TBarGauge = object(TPercentGauge)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    procedure Draw; virtual;
    end;

  PWhileView = ^TWhileView;
  TWhileView = object(TGroup)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Lines: PCollection;
    But: PButton;
    QuitNormal: Boolean;
    Top, Bottom: String[SizeOf(String)-1];
    constructor Init(Bounds: TRect);
    procedure Write(N: Integer; S: String);
    function GetPalette: PPalette; virtual;
    function Valid(C: Word): Boolean; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    destructor Done; virtual;
    procedure ClearInterior;
  private
    Side: (sdLeft, sdRight);
    procedure InsBut;
    end;

procedure DispatchEvents(InfoView: PWhileView; var CancelParam: Boolean);

implementation
uses
  DNApp, Commands, Dos, TitleSet, Advance, Advance1, Advance2
  ;

constructor TPercentGauge.Init(var Bounds: TRect; AMaxValue: LongInt);
  begin
  inherited Init(Bounds);
  EventMask := EventMask or evBroadcast;
  MaxValue := AMaxValue;
  CurValue := 0;
  end;

procedure TPercentGauge.Draw;
  var
    B: TDrawBuffer;
    C: Word;
    S: String[10];
    PercentDone: LongInt;
  begin
  C := GetColor(1);
  MoveChar(B, ' ', C, Size.X);
  PercentDone := SolveForY(CurValue, MaxValue);
  FormatStr(S, '%-3d%%', PercentDone);
  MoveStr(B, S, C);
  WriteLine(0, 0, Size.X, Size.Y, B);
  end;

procedure TPercentGauge.UpdateView(Progress: LongInt);
  begin
  CurValue := Progress;
  DrawView;
  end;

procedure TPercentGauge.AddProgress(Progress: LongInt);
  begin
  UpdateView(Progress+CurValue);
  end;

procedure TPercentGauge.HandleEvent(var Event: TEvent);
  begin
  inherited HandleEvent(Event);
  if Event.What = evBroadcast then
    begin
    case Event.Command of
      cmUpdateGauge:
        begin
        UpdateView(Event.InfoLong);
        end;
      cmResetGauge:
        begin
        MaxValue := Event.InfoLong;
        UpdateView(0);
        end;
      cmAddGauge:
        begin
        AddProgress(Event.InfoLong);
        end;
    end {case};
    end;
  end;

{ This function solves for x in the equation "x is y% of z". }
function TPercentGauge.SolveForX(Y, Z: LongInt): Integer;
  begin
  SolveForX := Trunc(Z*(Y*0.01));
  end;

{ This function solves for y in the equation "x is y% of z". }
function TPercentGauge.SolveForY(X, Z: LongInt): Integer;
  begin
  if Z = 0 then
    SolveForY := 0
  else
    SolveForY := Trunc((X*100)/Z);
  end;

{ TBarGauge }
procedure TBarGauge.Draw;
  var
    B: TDrawBuffer;
    C: Word;
    PercentDone: LongInt;
    FillSize: Integer;
  begin
  C := GetColor(1);
  MoveChar(B, #176, C, Size.X);
  PercentDone := SolveForY(CurValue, MaxValue);
  FillSize := SolveForX(PercentDone, Size.X);
  if FillSize > Size.X then
    FillSize := Size.X;
  MoveChar(B, #178, C, FillSize);
  WriteLine(0, 0, Size.X, Size.Y, B);
  end;

procedure TWhileView.InsBut;
  var
    R: TRect;
  begin
  R.Assign(1, Size.Y-3, 13, Size.Y-1);
  New(But, Init(R, GetString(dlStop), cmCancel, bfDefault));
  But^.Options := But^.Options or ofCenterX;
  Insert(But);
  end;

constructor TWhileView.Init;
  var
    I: Integer;
  begin
  TGroup.Init(Bounds);
  QuitNormal := False;
  Options := (Options or ofCentered or ofTopSelect) and not ofSelectable;
  Lines := New(PLineCollection, Init(Bounds.B.Y-Bounds.A.Y, 10, False));
  for I := 0 to Bounds.B.Y-Bounds.A.Y-2 do
    Lines^.Insert(NewStr(''));
  SetState(sfShadow, True);
  InsBut;
  Top := '';
  Bottom := '';
  end;

procedure TWhileView.Write;
  var
    R: TRect;
    B: TDrawBuffer;
    I: Integer;
    S1: String;
  begin
  if N < Lines^.Count then
    begin
    if CnvString(Lines^.At(N)) = S then
      Exit;
    Lines^.AtFree(N);
    end
  else
    Exit;
  Lines^.AtInsert(N, NewStr(S));
  if Length(S) > Size.X-4 then
    begin
    Lock;
    R.A.Y := Origin.Y;
    R.B.Y := R.A.Y+Size.Y;
    if Side = sdLeft then
      begin
      R.B.X := Origin.X+Size.X+(Length(S)-Size.X+4) div 2;
      R.A.X := R.B.X-Length(S)-4;
      end
    else
      begin
      R.A.X := Origin.X-(Length(S)-Size.X+4) div 2;
      R.B.X := R.A.X+Length(S)+4;
      end;
    Boolean(Side) := not Boolean(Side);
    Locate(R);
    Dispose(But, Done);
    But := nil;
    InsBut;
    UnLock;
    DrawView;
    end
  else
    begin
    FillChar(S1[1], Size.X-2, ' '); {JO}
    SetLength(S1, Size.X-2);
    Move(S[1], S1[(Size.X-Length(S)) div 2], Length(S));
    WriteStr(1, N+1, S1, 7);
    end;
  end { TWhileView.Write };

procedure TWhileView.ClearInterior;
  var
    I, C: Integer;
  begin
  C := Lines^.Count-1;
  Lines^.FreeAll;
  for I := 0 to C do
    Lines^.Insert(NewStr(''));
  DrawView;
  end;

procedure TWhileView.SetState;
  var
    WindowCommands: TCommandSet;
  begin
  inherited SetState(AState, Enable);
  if AState = sfSelected then
    begin
    SetState(sfActive, Enable);
    if Top <> '' then
      SetTitle(Top);
    WindowCommands := [cmNext, cmPrev];
    if Enable then
      EnableCommands(WindowCommands)
    else
      DisableCommands(WindowCommands);
    end;
  end;

function TWhileView.GetPalette;
  const
    P: String[Length(CDialog)] = CDialog;
  begin
  GetPalette := @P;
  end;

procedure TWhileView.HandleEvent;
  var
    P: TPoint;
    R: TRect;

  procedure MoveView;
    begin
    Desktop^.GetExtent(R);
    Inc(R.A.Y, Size.Y-1);
    SetState(sfDragging, True);
    DrawView;
    DragView(Event, dmDragMove, R, Size, Size);
    ClearEvent(Event);
    end;

  begin
  Owner^.GetExtent(R);
  case Event.What of
    evKeyDown:
      case Event.KeyCode of
        kbESC, kbEnter:
          begin
          Event.What := evCommand;
          Event.Command := cmCancel;
          Event.InfoPtr := But;
          Application^.PutEvent(Event);
          ClearEvent(Event);
          end;
        kbLeft:
          begin
          if Origin.X+Size.X > 1 then
            MoveTo(Origin.X-1, Origin.Y);
          ClearEvent(Event);
          end;
        kbRight:
          begin
          if Origin.X < R.B.X-1 then
            MoveTo(Origin.X+1, Origin.Y);
          ClearEvent(Event);
          end;
        kbUp:
          begin
          if Origin.Y > 0 then
            MoveTo(Origin.X, Origin.Y-1);
          ClearEvent(Event);
          end;
        kbDown:
          begin
          if Origin.Y < R.B.Y-1 then
            MoveTo(Origin.X, Origin.Y+1);
          ClearEvent(Event);
          end;
      end {case};
    evMouseDown:
      begin
      MakeLocal(Event.Where, P);
      if P.Y = 0 then
        MoveView;
      end;
    evCommand:
      case Event.Command of
        cmGetName:
          PString(Event.InfoPtr)^:= Top;
        cmResize:
          MoveView;
        cmClose, cmQuit:
          if not QuitNormal then
            ClearEvent(Event);
      end {case};
  end {case};
  TGroup.HandleEvent(Event);
  end { TWhileView.HandleEvent };

procedure TWhileView.Draw;
  var
    B: TDrawBuffer;
    B1: array[0..127] of record
      C: Char;
      A: Byte;
      end absolute B;
    C1, C2: Byte;
    I: Integer;
    PS: PString;
    R: TRect;
  begin
  if Length(Top) > Size.X-4 then
    begin
    Lock;
    R.A.Y := Origin.Y;
    R.B.Y := R.A.Y+Size.Y;
    if Side = sdLeft then
      begin
      R.B.X := Origin.X+Size.X+(Length(Top)-Size.X+4) div 2;
      R.A.X := R.B.X-Length(Top)-4;
      end
    else
      begin
      R.A.X := Origin.X-(Length(Top)-Size.X+4) div 2;
      R.B.X := R.A.X+Length(Top)+4;
      end;
    Boolean(Side) := not Boolean(Side);
    Locate(R);
    Dispose(But, Done);
    But := nil;
    InsBut;
    UnLock;
    end;
  if Length(Bottom) > Size.X-4 then
    begin
    Lock;
    R.A.Y := Origin.Y;
    R.B.Y := R.A.Y+Size.Y;
    if Side = sdLeft then
      begin
      R.B.X := Origin.X+Size.X+(Length(Bottom)-Size.X+4) div 2;
      R.A.X := R.B.X-Length(Bottom)-4;
      end
    else
      begin
      R.A.X := Origin.X-(Length(Bottom)-Size.X+4) div 2;
      R.B.X := R.A.X+Length(Bottom)+4;
      end;
    Boolean(Side) := not Boolean(Side);
    Locate(R);
    Dispose(But, Done);
    But := nil;
    InsBut;
    UnLock;
    end;
  C1 := GetColor(2+Byte(GetState(sfDragging)));
  C2 := GetColor(7);
  MoveChar(B, #205, C1, Size.X);
  if Top <> '' then
    MoveStr(B[(Size.X-Length(Top)) div 2-1], ' '+Top+' ', C1);
  B1[0].C := #201;
  B1[Size.X-1].C := #187;
  WriteLine(0, 0, Size.X, 1, B);
  MoveChar(B, #205, C1, Size.X);
  if Bottom <> '' then
    MoveStr(B[(Size.X-Length(Bottom)) div 2-1], ' '+Bottom+' ', C1);
  B1[0].C := #200;
  B1[Size.X-1].C := #188;
  WriteLine(0, Size.Y-1, Size.X, 1, B);

  B1[0].C := #186;
  B1[Size.X-1].C := #186;
  for I := 0 to Size.Y-5 do
    begin
    MoveChar(B[1], ' ', C2, Size.X-2);
    if I < Lines^.Count then
      begin
      PS := Lines^.At(I);
      if PS <> nil then
        MoveStr(B[(Size.X-Length(PS^)) div 2], PS^, C2);
      end;
    WriteLine(0, I+1, Size.X, 1, B);
    end;
  MoveChar(B[1], ' ', C2, Size.X-2);
  WriteLine(0, But^.Origin.Y, But^.Origin.X, 2, B);
  WriteLine(But^.Origin.X+But^.Size.X, But^.Origin.Y, Size.X, 2,
    B[But^.Origin.X+But^.Size.X]);
  But^.Draw;
  end { TWhileView.Draw };

function TWhileView.Valid;
  begin
  Result := (C <> cmQuit) and (C <> cmClose) and inherited Valid(C);
  end;

destructor TWhileView.Done;
  begin
  Dispose(Lines, Done);
  Lines := nil;
  TGroup.Done;
  end;

procedure DispatchEvents(InfoView: PWhileView; var CancelParam: Boolean);
  var
    Event: TEvent;
  begin
  Application^.GetEvent(Event);
  if  (Event.What = evCommand) and (Event.Command = cmCancel) and
      (Event.InfoPtr = InfoView^.But) or
      (Event.What = evKeyDown) and (Event.KeyCode = kbESC)
  then
    begin
    InfoView^.ClearEvent(Event);
    CancelParam := True;
    end;
  if  (Event.What <> evNothing)
    { and not ((Event.What = evKeyDown) and
    (Event.KeyCode = kbEnter))}
    then
    {and InfoView^.MouseInView(Event.Where)
    or (Event.What = evKeyDown) and ((Event.ScanCode=kbAltS) or (Event.))
    then Application}InfoView^.HandleEvent(Event);
  end;

end.
