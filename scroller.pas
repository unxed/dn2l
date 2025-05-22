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

unit
Scroller;

interface

uses
  Views, Defines, Streams, Drivers
  ;

type
  { TScroller object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }

  PScroller = ^TScroller;
  TScroller = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    Delta: TPoint;
    Limit: TPoint;
    constructor Init(var Bounds: TRect;
         AHScrollBar, AVScrollBar: PScrollBar);
    constructor Load(var S: TStream);
    procedure ChangeBounds(var Bounds: TRect); virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure ScrollDraw; virtual;
    procedure ScrollTo(X, Y: LongInt);
    procedure SetLimit(X, Y: LongInt);
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);
  private
    DrawLock: Byte;
    DrawFlag: Boolean;
    procedure CheckDraw;
    end;

  { TListViewer }

  { Palette layout }
  { 1 = Active }
  { 2 = Inactive }
  { 3 = Focused }
  { 4 = Selected }
  { 5 = Divider }

  PListViewer = ^TListViewer;
  TListViewer = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    NumCols: LongInt;
    TopItem: LongInt;
    Focused: LongInt;
    Range: LongInt;
    constructor Init(var Bounds: TRect; ANumCols: LongInt;
        AHScrollBar, AVScrollBar: PScrollBar);
    constructor Load(var S: TStream);
    procedure ChangeBounds(var Bounds: TRect); virtual;
    procedure Draw; virtual;
    procedure FocusItem(Item: LongInt); virtual;
    function GetPalette: PPalette; virtual;
    function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    function IsSelected(Item: LongInt): Boolean; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SelectItem(Item: LongInt); virtual;
    procedure SetRange(ARange: LongInt);
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);
  private
    procedure FocusItemNum(Item: LongInt); virtual;
    end;

implementation

uses
  Commands
  ;

{ TScroller }

constructor TScroller.Init(var Bounds: TRect; AHScrollBar,
    AVScrollBar: PScrollBar);
  begin
  TView.Init(Bounds);
  Options := Options or ofSelectable;
  EventMask := EventMask or evBroadcast;
  HScrollBar := AHScrollBar;
  VScrollBar := AVScrollBar;
  end;

constructor TScroller.Load(var S: TStream);
  begin
  TView.Load(S);
  GetPeerViewPtr(S, HScrollBar);
  GetPeerViewPtr(S, VScrollBar);
  S.Read(Delta, SizeOf(TPoint)*2);
  end;

procedure TScroller.ChangeBounds(var Bounds: TRect);
  begin
  SetBounds(Bounds);
  Inc(DrawLock);
  SetLimit(Limit.X, Limit.Y);
  Dec(DrawLock);
  DrawFlag := False;
  DrawView;
  end;

procedure TScroller.CheckDraw;
  begin
  if  (DrawLock = 0) and DrawFlag then
    begin
    DrawFlag := False;
    DrawView;
    end;
  end;

function TScroller.GetPalette: PPalette;
  const
    P: String[Length(CScroller)] = CScroller;
  begin
  GetPalette := @P;
  end;

procedure TScroller.HandleEvent(var Event: TEvent);
  begin
  TView.HandleEvent(Event);
  if  (Event.What = evBroadcast)
       and (Event.Command = cmScrollBarChanged) and
      ( (Event.InfoPtr = HScrollBar) or (Event.InfoPtr = VScrollBar))
  then
    ScrollDraw;
  end;

procedure TScroller.ScrollDraw;
  var
    D: TPoint;
  begin
  if HScrollBar <> nil then
    D.X := HScrollBar^.Value
  else
    D.X := 0;
  if VScrollBar <> nil then
    D.Y := VScrollBar^.Value
  else
    D.Y := 0;
  if  (D.X <> Delta.X) or (D.Y <> Delta.Y) then
    begin
    SetCursor(Cursor.X+Delta.X-D.X, Cursor.Y+Delta.Y-D.Y);
    Delta := D;
    if DrawLock <> 0 then
      DrawFlag := True
    else
      DrawView;
    end;
  end;

procedure TScroller.ScrollTo(X, Y: LongInt);
  begin
  Inc(DrawLock);
  if HScrollBar <> nil then
    HScrollBar^.SetValue(X);
  if VScrollBar <> nil then
    VScrollBar^.SetValue(Y);
  Dec(DrawLock);
  CheckDraw;
  end;

procedure TScroller.SetLimit(X, Y: LongInt);
  begin
  Limit.X := X;
  Limit.Y := Y;
  Inc(DrawLock);
  if HScrollBar <> nil then
    HScrollBar^.SetParams(HScrollBar^.Value, 0, X-Size.X, Size.X-1,
      HScrollBar^.ArStep);
  if VScrollBar <> nil then
    VScrollBar^.SetParams(VScrollBar^.Value, 0, Y-Size.Y, Size.Y-1,
      VScrollBar^.ArStep);
  Dec(DrawLock);
  CheckDraw;
  end;

procedure TScroller.SetState(AState: Word; Enable: Boolean);

  procedure ShowSBar(SBar: PScrollBar);
    begin
    if  (SBar <> nil) then
      if GetState(sfActive+sfSelected) then
        SBar^.Show
      else
        SBar^.Hide;
    end;

  begin
  TView.SetState(AState, Enable);
  if AState and (sfActive+sfSelected) <> 0 then
    begin
    ShowSBar(HScrollBar);
    ShowSBar(VScrollBar);
    end;
  end;

procedure TScroller.Store(var S: TStream);
  begin
  TView.Store(S);
  PutPeerViewPtr(S, HScrollBar);
  PutPeerViewPtr(S, VScrollBar);
  S.Write(Delta, SizeOf(TPoint)*2);
  end;

{ TListViewer }

constructor TListViewer.Init(var Bounds: TRect; ANumCols: LongInt;
    AHScrollBar, AVScrollBar: PScrollBar);
  var
    ArStep, PgStep: LongInt;
  begin
  TView.Init(Bounds);
  Options := Options or (ofFirstClick+ofSelectable);
  EventMask := EventMask or evBroadcast;
  Range := 0;
  NumCols := ANumCols;
  Focused := 0;
  if AVScrollBar <> nil then
    begin
    if NumCols = 1 then
      begin
      PgStep := Size.Y-1;
      ArStep := 1;
      end
    else
      begin
      PgStep := Size.Y*NumCols;
      ArStep := Size.Y;
      end;
    AVScrollBar^.SetStep(PgStep, ArStep);
    end;
  if AHScrollBar <> nil then
    AHScrollBar^.SetStep(Size.X div NumCols, 1);
  HScrollBar := AHScrollBar;
  VScrollBar := AVScrollBar;
  end { TListViewer.Init };

constructor TListViewer.Load(var S: TStream);
  begin
  TView.Load(S);
  GetPeerViewPtr(S, HScrollBar);
  GetPeerViewPtr(S, VScrollBar);
  S.Read(NumCols, SizeOf(LongInt)*4);
  end;

procedure TListViewer.ChangeBounds(var Bounds: TRect);
  begin
  TView.ChangeBounds(Bounds);
  if HScrollBar <> nil then
    HScrollBar^.SetStep(Size.X div NumCols, HScrollBar^.ArStep);
  if VScrollBar <> nil then
    VScrollBar^.SetStep(Size.Y, VScrollBar^.ArStep);
  end;

procedure TListViewer.Draw;
  var
    I, J, Item: LongInt;
    NormalColor, SelectedColor, FocusedColor, Color: Word;
    ColWidth, CurCol, Indent: LongInt;
    B: TDrawBuffer;
    Text: String;
    SCOff: Byte;
  begin
  if State and (sfSelected+sfActive) = (sfSelected+sfActive) then
    begin
    NormalColor := GetColor(1);
    FocusedColor := GetColor(3);
    SelectedColor := GetColor(4);
    end
  else
    begin
    NormalColor := GetColor(2);
    SelectedColor := GetColor(4);
    end;
  if HScrollBar <> nil then
    Indent := HScrollBar^.Value
  else
    Indent := 0;
  ColWidth := Size.X div NumCols+1;
  for I := 0 to Size.Y-1 do
    begin
    for J := 0 to NumCols-1 do
      begin
      Item := J*Size.Y+I+TopItem;
      CurCol := J*ColWidth;
      if  (State and (sfSelected+sfActive) = (sfSelected+sfActive)) and
          (Focused = Item) and (Range > 0)
      then
        begin
        Color := FocusedColor;
        SetCursor(CurCol+1, I);
        SCOff := 0;
        end
      else if (Item < Range) and IsSelected(Item) then
        begin
        Color := SelectedColor;
        SCOff := 2;
        end
      else
        begin
        Color := NormalColor;
        SCOff := 4;
        end;
      MoveChar(B[CurCol], ' ', Color, ColWidth);
      if Item < Range then
        begin
        Text := GetText(Item, ColWidth+Indent);
        Text := Copy(Text, Indent, ColWidth);
        MoveStr(B[CurCol+1], Text, Color);
        if ShowMarkers then
          begin
          WordRec(B[CurCol]).Lo := Byte(SpecialChars[SCOff]);
          WordRec(B[CurCol+ColWidth-2]).Lo := Byte(SpecialChars[SCOff+1]);
          end;
        end;
      MoveChar(B[CurCol+ColWidth-1], #179, GetColor(5), 1);
      end;
    WriteLine(0, I, Size.X, 1, B);
    end;
  end { TListViewer.Draw };

procedure TListViewer.FocusItem(Item: LongInt);
  begin
  if VScrollBar <> nil then
    VScrollBar^.SetValue(Item);
  Item := VScrollBar^.Value;
  Focused := Item;
  if Item < TopItem then
    if NumCols = 1 then
      TopItem := Item
    else
      TopItem := Item-Item mod Size.Y
  else if Item >= TopItem+(Size.Y*NumCols) then
    if NumCols = 1 then
      TopItem := Item-Size.Y+1
    else
      TopItem := Item-Item mod Size.Y-(Size.Y*(NumCols-1));
  end;

procedure TListViewer.FocusItemNum(Item: LongInt);
  begin
  if Item < 0 then
    Item := 0
  else if (Item >= Range) and (Range > 0) then
    Item := Range-1;
  if Range <> 0 then
    FocusItem(Item);
  end;

function TListViewer.GetPalette: PPalette;
  const
    P: String[Length(CListViewer)] = CListViewer;
  begin
  GetPalette := @P;
  end;

function TListViewer.GetText(Item: LongInt; MaxLen: Integer): String;
  begin
  end;

function TListViewer.IsSelected(Item: LongInt): Boolean;
  begin
  IsSelected := Item = Focused;
  end;

procedure TListViewer.HandleEvent(var Event: TEvent);
  const
    MouseAutosToSkip = 4;
  var
    Mouse: TPoint;
    ColWidth: Word;
    OldItem, NewItem: LongInt;
    Count: LongInt;
  begin
  TView.HandleEvent(Event);
  if Event.What = evMouseDown then
    begin
    ColWidth := Size.X div NumCols+1;
    OldItem := Focused;
    MakeLocal(Event.Where, Mouse);
    if MouseInView(Event.Where) then
      NewItem := Mouse.Y+(Size.Y*(Mouse.X div ColWidth))+TopItem
    else
      NewItem := OldItem;
    Count := 0;
    if NewItem <> OldItem then
      begin
      FocusItemNum(NewItem);
      DrawView;
      NewItem := OldItem;
      end;
    if  (Options and ofSecurity <> 0) and Event.Double then
      begin
      Message(Owner, evBroadcast, cmDefault, nil);
      ClearEvent(Event);
      Exit;
      end;
    repeat
      if NewItem <> OldItem then
        begin
        FocusItemNum(NewItem);
        DrawView;
        end;
      OldItem := NewItem;
      MakeLocal(Event.Where, Mouse);
      if MouseInView(Event.Where) then
        NewItem := Mouse.Y+(Size.Y*(Mouse.X div ColWidth))+TopItem
      else
        begin
        if NumCols = 1 then
          begin
          if Event.What = evMouseAuto then
            Inc(Count);
          if Count = MouseAutosToSkip then
            begin
            Count := 0;
            if Mouse.Y < 0 then
              NewItem := Focused-1
            else if Mouse.Y >= Size.Y then
              NewItem := Focused+1;
            end;
          end
        else
          begin
          if Event.What = evMouseAuto then
            Inc(Count);
          if Count = MouseAutosToSkip then
            begin
            Count := 0;
            if Mouse.X < 0 then
              NewItem := Focused-Size.Y
            else if Mouse.X >= Size.X then
              NewItem := Focused+Size.Y
            else if Mouse.Y < 0 then
              NewItem := Focused-Focused mod Size.Y
            else if Mouse.Y > Size.Y then
              NewItem := Focused-Focused mod Size.Y+Size.Y-1;
            end
          end;
        end;
    until not MouseEvent(Event, evMouseMove+evMouseAuto);
    FocusItemNum(NewItem);
    DrawView;
    if Event.Double and (Range > Focused) then
      SelectItem(Focused);
    ClearEvent(Event);
    end
  else if Event.What = evKeyDown then
    begin
    if  (Event.CharCode = ' ') and (Focused < Range) then
      begin
      SelectItem(Focused);
      NewItem := Focused;
      end
    else
      case {CtrlToArrow}(Event.KeyCode) of
        kbUp, kbShiftUp:
          NewItem := Focused-1;
        kbDown, kbShiftDown:
          NewItem := Focused+1;
        kbRight:
          if NumCols > 1 then
            NewItem := Focused+Size.Y
          else
            Exit;
        kbLeft:
          if NumCols > 1 then
            NewItem := Focused-Size.Y
          else
            Exit;
        kbPgDn:
          NewItem := Focused+Size.Y*NumCols;
        kbPgUp:
          NewItem := Focused-Size.Y*NumCols;
        kbHome:
          NewItem := TopItem;
        kbEnd:
          NewItem := TopItem+(Size.Y*NumCols)-1;
        kbCtrlPgDn:
          NewItem := Range-1;
        kbCtrlPgUp:
          NewItem := 0;
        else {case}
          Exit;
      end {case};
    FocusItemNum(NewItem);
    DrawView;
    ClearEvent(Event);
    end
  else if Event.What = evBroadcast then
    if Options and ofSelectable <> 0 then
      if  (Event.Command = cmScrollBarClicked) and
          ( (Event.InfoPtr = HScrollBar) or (Event.InfoPtr =
           VScrollBar))
      then
        Select
      else if (Event.Command = cmScrollBarChanged) then
        begin
        if  (VScrollBar = Event.InfoPtr) then
          begin
          FocusItemNum(VScrollBar^.Value);
          DrawView;
          {ClearEvent(Event);}
          end
        else if (HScrollBar = Event.InfoPtr) then
          DrawView;
        end;
  end { TListViewer.HandleEvent };

procedure TListViewer.SelectItem(Item: LongInt);
  begin
  Message(Owner, evBroadcast, cmListItemSelected, @Self);
  end;

procedure TListViewer.SetRange(ARange: LongInt);
  begin
  Range := ARange;
  if VScrollBar <> nil then
    begin
    if Focused > ARange then
      Focused := 0;
    VScrollBar^.SetParams(Focused, 0, ARange-1, VScrollBar^.PgStep,
      VScrollBar^.ArStep);
    end;
  end;

procedure TListViewer.SetState(AState: Word; Enable: Boolean);

  procedure ShowSBar(SBar: PScrollBar);
    begin
    if  (SBar <> nil) then
      if GetState(sfActive) and GetState(sfVisible) then
        SBar^.Show
      else
        SBar^.Hide;
    end;

  begin
  TView.SetState(AState, Enable);
  if AState and (sfSelected+sfActive+sfVisible) <> 0 then
    begin
    ShowSBar(HScrollBar);
    ShowSBar(VScrollBar);
    DrawView;
    end;
  end;

procedure TListViewer.Store(var S: TStream);
  begin
  TView.Store(S);
  PutPeerViewPtr(S, HScrollBar);
  PutPeerViewPtr(S, VScrollBar);
  S.Write(NumCols, SizeOf(LongInt)*4);
  end;

end.
