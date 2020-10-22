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

unit ASCIITab;

interface

uses
  Defines, Views, Drivers, Streams

  ;

const
  boundsASCII: TPoint = (X: 0; Y: 0);
  CharASCII: Char = #04;
  AsciiTableCommandBase: AWord = 910;

type
  PTable = ^TTable;
  TTable = object(TView)
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function DataSize: Word; virtual;
    procedure GetData(var Data); virtual;
    procedure SetData(var Data); virtual;
    end;

  PReport = ^TReport;
  TReport = object(TView)
    ASCIIChar: LongInt;
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Store(var S: TStream);
    end;

  PASCIIChart = ^TASCIIChart;
  TASCIIChart = object(TWindow)
    constructor Init(var R: TRect);
    function Execute: Word; virtual;
    destructor Done; virtual;
    end;

procedure ASCIITable;

var
  fASCIITable: Boolean;
    {` Признак того, что активна ASCIITable. Он используется для
  того, чтобы символ вставлять в редактируемую строку, а не
  обрабатывать каким-то иным образом. Например, код 27 (Esc), если
  он введён с клавиатуры, завершает диалог, а если он сгенерирован
  ASCIITable, он должен быть вставлен в строку ввода. `}

implementation

uses
  Advance, Advance1, DNApp, Commands, DNHelp
  ;

const
  cmCharacterFocused = 0;

procedure TTable.Draw;
  var
    Buf: TDrawBuffer;
    X, Y: Integer;
    Color: Byte;
  begin
  Color := GetColor(6);
  for Y := 0 to Size.Y-1 do
    begin
    MoveChar(Buf, ' ', Color, Size.X);
    for X := 0 to Size.X-1 do
      MoveChar(Buf[X], Chr(32*Y+X), Color, 1);
    WriteLine(0, Y, Size.X, 1, Buf);
    end;
  ShowCursor;
  end;

procedure ASCIITable;
  var
    P: PWindow;
    W: Word;
    E: TEvent;
    R: TRect;
    CR: PView;
  function GetCH: Boolean;
    begin
    W := Desktop^.ExecView(P);
    P^.GetData(CharASCII);
    E.What := evKeyDown;
    E.KeyCode := Byte(CharASCII);
    if W in [cmOK, cmYes] then
      Application^.HandleEvent(E);
    GetCH := W = cmYes;
    end;
  begin
  P := New(PASCIIChart, Init(R));
  P^.MoveTo(boundsASCII.X, boundsASCII.Y);
  P^.SetData(CharASCII);
  CR := Desktop^.Current;
  {  Desktop^.Lock;}
  while GetCH do
    ;
  {  Desktop^.UnLock;}
  boundsASCII := P^.Origin;
  Dispose(P, Done);
  if CR <> nil then
    CR^.Select;
  end { ASCIITable };

procedure TTable.HandleEvent(var Event: TEvent);
  var
    CurrentSpot: TPoint;
    C: Char;

  procedure CharFocused;
    begin
    MessageL(Owner, evBroadcast, AsciiTableCommandBase+cmCharacterFocused,
      Cursor.X+32*Cursor.Y);
    end;

  var
    cX, cY: Integer;
  begin
  if Event.What = evMouseDown then
    begin
    if Event.Double then
      begin
      Event.What := evKeyDown;
      GetData(C);
      Event.KeyCode := Byte(C);
      PutEvent(Event);
      ClearEvent(Event);
      Exit;
      end;
    repeat
      if MouseInView(Event.Where) then
        begin
        MakeLocal(Event.Where, CurrentSpot);
        SetCursor(CurrentSpot.X, CurrentSpot.Y);
        CharFocused;
        end;
    until not MouseEvent(Event, evMouseMove);
    ClearEvent(Event);
    end
  else if Event.What = evKeyDown then
    with Cursor do
      begin
      cX := X;
      cY := Y;
      case Event.KeyCode of
        kbCtrlPgUp:
          SetCursor(0, 0);
        kbCtrlPgDn:
          SetCursor(Size.X-1, Size.Y-1);
        kbCtrlHome, kbPgUp:
          SetCursor(X, 0);
        kbCtrlEnd, kbPgDn:
          SetCursor(X, Size.Y-1);
        kbHome:
          SetCursor(0, Y);
        kbEnd:
          SetCursor(Size.X-1, Y);
        kbUp:
          begin
          Dec(cY, 1);
          if cY < 1 then
            cY := 0;
          SetCursor(cX, cY);
          end;
        kbDown:
          begin
          Inc(cY, 1);
          if cY > Size.Y-1 then
            cY := Size.Y-1;
          SetCursor(cX, cY);
          end;
        kbLeft:
          begin
          Dec(cX, 1);
          if cX < 1 then
            cX := 0;
          SetCursor(cX, cY);
          end;
        kbRight:
          begin
          Inc(cX, 1);
          if cX > Size.X-1 then
            cX := Size.X-1;
          SetCursor(cX, cY);
          end;
        kbCtrlUp:
          begin
          Dec(cY, 2);
          if cY < 1 then
            cY := 0;
          SetCursor(cX, cY);
          end;
        kbCtrlDown:
          begin
          Inc(cY, 2);
          if cY > Size.Y-1 then
            cY := Size.Y-1;
          SetCursor(cX, cY);
          end;
        kbCtrlLeft:
          begin
          Dec(cX, 5);
          if cX < 1 then
            cX := 0;
          SetCursor(cX, cY);
          end;
        kbCtrlRight:
          begin
          Inc(cX, 5);
          if cX > Size.X-1 then
            cX := Size.X-1;
          SetCursor(cX, cY);
          end;
        else {case}
          if  (Ord(Event.CharCode) > 0) then
            SetCursor(Ord(Event.CharCode) mod 32, Ord(Event.CharCode) div
               32);
      end {case};
      CharFocused;
      ClearEvent(Event);
      end;
  TView.HandleEvent(Event);
  end { TTable.HandleEvent };

{ TReport }

constructor TReport.Load(var S: TStream);
  begin
  TView.Load(S);
  S.Read(ASCIIChar, SizeOf(ASCIIChar));
  end;

procedure TReport.Draw;
  var
    Ch: LongInt;
    Buf: TDrawBuffer;
    TempStr: String;
  begin
  if ASCIIChar > 0 then
    FormatStr(TempStr,
       ' Char: ~%c~ Decimal: ~%0#%3d~ Hex: ~%0#%02x~   ', ASCIIChar)
  else
    FormatStr(TempStr, ' Char:   Decimal: ~%0#%3d~ Hex: ~%0#%02x~   ',
       ASCIIChar);
  MoveCStr(Buf, TempStr, GetColor(07) shl 8+GetColor(06));
  MoveChar(Buf[Size.X-2], #$FE, ASCIIChar, 1);
  WriteBuf(0, 0, 50, 1, Buf);
  { WriteStr(0, 0, TempStr, 6);}
  end;

procedure TReport.HandleEvent(var Event: TEvent);
  var
    Table: PTable;
  begin
  TView.HandleEvent(Event);
  if Event.What = evBroadcast then
    if Event.Command = AsciiTableCommandBase+cmCharacterFocused then
      begin
      ASCIIChar := Event.InfoLong;
      DrawView;
      end;
  end;

procedure TReport.Store(var S: TStream);
  begin
  TView.Store(S);
  S.Write(ASCIIChar, SizeOf(ASCIIChar));
  end;

function TTable.DataSize;
  begin
  DataSize := 1;
  end;

procedure TTable.GetData;
  begin
  Byte(Data) := Cursor.Y*32+Cursor.X;
  end;

procedure TTable.SetData;
  begin
  SetCursor(Byte(Data) mod 32, Byte(Data) div 32);
  MessageL(Owner, evBroadcast, AsciiTableCommandBase+cmCharacterFocused,
    Cursor.X+32*Cursor.Y);
  Owner^.Redraw;
  end;

constructor TASCIIChart.Init;
  var
    Control: PView;
  begin
  R.Assign(0, 0, 34, 12);
  TWindow.Init(R, GetString(dlASCIIChart), wnNoNumber);
  Flags := Flags and not (wfGrow+wfZoom);
  Options := Options and ofTopSelect;
  HelpCtx := hcAsciiChart;
  Palette := wpGrayWindow;

  R.Grow(-1, -1);
  R.A.Y := R.B.Y-1;
  Control := New(PReport, Init(R));
  with Control^ do
    begin
    Options := Options or ofFramed;
    EventMask := EventMask or evBroadcast;
    end;
  Insert(Control);

  GetExtent(R);
  R.Grow(-1, -1);
  R.B.Y := R.B.Y-2;
  Control := New(PTable, Init(R));
  with Control^ do
    begin
    Options := Options or ofFramed or ofSelectable;
    EventMask := $FFFF;
    BlockCursor;
    end;
  Insert(Control);
  Control^.Select;
  fASCIITable := True;
  end { TASCIIChart.Init };

function TASCIIChart.Execute: Word;
  var
    Event: TEvent;
  begin
  repeat
    GetEvent(Event);
    case Event.What of
      evKeyDown:
        case Event.KeyCode of
          kbCtrlB, kbCtrlP:
            begin
            Event.What := evCommand;
            Event.Command := cmOK;
            end;
          else {case}
            if Event.CharCode > #0 then
              begin
              SetData(Event.CharCode);
              Event.What := evCommand;
              Event.Command := cmOK;
              end;
        end {case};
      evCommand:
        if Event.Command = cmClose then
          Event.Command := cmCancel;
    else
      TinySlice;
    end {case};
    if Event.What <> evNothing then
      HandleEvent(Event);
  until (Event.What = evCommand) and ((Event.Command < 255) and
      (Event.Command in [cmOK, cmCancel, cmYes]));
  Execute := Event.Command;
  end { TASCIIChart.Execute: };

destructor TASCIIChart.Done;
  begin
  fASCIITable := False;
  inherited Done;
  end;

end.
