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

unit ColorSel;

{-----------------------------------------------------}
{ This module is based on Turbo Vision ColorSel Unit  }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}

interface

uses
  Defines, Streams, Views, Dialogs, Scroller,
  Advance, Drivers
  ;

const
  cmColorForegroundChanged = 71;
  cmColorBackgroundChanged = 72;
  cmColorSet = 73;
  cmNewColorItem = 74;
  cmNewColorIndex = 75;
  cmSaveColorIndex = 76;

type

  { TColorItem }

  PColorItem = ^TColorItem;
  TColorItem = record
    Name: PString;
    Index: Byte;
    Next: PColorItem;
    end;

  { TColorGroup }

  PColorGroup = ^TColorGroup;
  TColorGroup = record
    Name: PString;
    Index: Byte;
    Items: PColorItem;
    Next: PColorGroup;
    end;

  { TColorSelector }

  TColorSel = (csBackground, csForeground);

  PColorSelector = ^TColorSelector;
  TColorSelector = object(TView)
    Color: Byte;
    SelType: TColorSel;
    constructor Init(var Bounds: TRect; ASelType: TColorSel);
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Store(var S: TStream);
    end;

  { TMonoSelector }
  PMonoSelector = ^TMonoSelector;
  TMonoSelector = object(TCluster)
    constructor Init(var Bounds: TRect; AStrings: PSItem);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function Mark(Item: Integer): Boolean; virtual;
    procedure NewColor; virtual;
    procedure Press(Item: Integer); virtual;
    procedure MovedTo(Item: Integer); virtual;
    end;

  { T_BWSelector }
  P_BWSelector = ^T_BWSelector;
  T_BWSelector = object(TMonoSelector)
    SelType: TColorSel; {Is't a selector of Foreground color ? }
    constructor Init(var Bounds: TRect; ASelType: TColorSel;
         AStrings: PSItem);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure NewColor; virtual;
    end;

  { TColorDisplay }

  PColorDisplay = ^TColorDisplay;
  TColorDisplay = object(TView)
    Color: ^Byte;
    Text: PString;
    constructor Init(var Bounds: TRect; AText: PString);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetColor(var AColor: Byte); virtual;
    procedure Store(var S: TStream);
    end;

  { TColorGroupList }

  PColorGroupList = ^TColorGroupList;
  TColorGroupList = object(TListViewer)
    Groups: PColorGroup;
    constructor Init(var Bounds: TRect; AScrollBar: PScrollBar;
        AGroups: PColorGroup);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure FocusItem(Item: LongInt); virtual;
    function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Store(var S: TStream);
    procedure SetGroupIndex(GroupNum, ItemNum: Byte);
    function GetGroup(GroupNum: Byte): PColorGroup;
    function GetGroupIndex(GroupNum: Byte): Byte;
    function GetNumGroups: Byte;
    end;

  { TColorItemList }

  PColorItemList = ^TColorItemList;
  TColorItemList = object(TListViewer)
    Items: PColorItem;
    constructor Init(var Bounds: TRect; AScrollBar: PScrollBar;
        AItems: PColorItem);
    procedure FocusItem(Item: LongInt); virtual;
    function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  { TColorDialog }

  PColorDialog = ^TColorDialog;
  TColorDialog = object(TDialog)
    GroupIndex: Byte;
    Display: PColorDisplay;
    Groups: PColorGroupList;
    ForLabel: PLabel;
    ForSel: PColorSelector;
    BakLabel: PLabel;
    BakSel: PColorSelector;
    MonoLabel: PLabel;
    BWSel: P_BWSelector;
    MonoSel: PMonoSelector;
    {    MonoLabelBack: PLabel;   }
    BWSelBack: P_BWSelector;
    DefaultColors: PCheckBoxes;
    Pal: TPalette;
    DefaultColorsFlag: Boolean;
    constructor Init( {APalette: TPalette;}AGroups: PColorGroup);
    destructor Done; virtual;
    constructor Load(var S: TStream);
    function DataSize: Word; virtual;
    procedure GetData(var Rec); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure GetEvent(var Event: TEvent); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
    procedure GetIndexes(var Colors: PColorIndex);
    procedure SetIndexes(var Colors: PColorIndex);
    procedure SwitchTypeSelector;
    procedure SetAppColor;
    end;

  { Color list building routines }

function ColorItem(const Name: String; Index: Byte;
    Next: PColorItem): PColorItem;
function ColorGroup(const Name: String; Items: PColorItem;
    Next: PColorGroup): PColorGroup;

implementation
uses
  Startup, DNApp, Commands, DNUtil, VideoMan
  , ColorVGA, Memory, Advance1, Advance2, VPUtils
  ;

type
  ColorArray = array[0..4] of Byte;

var
  OldColors: array[apColor..apMonochrome] of PString;

const
  MonoColorsSet: ColorArray = ($07, $0F, $01, $70, $09);
  BWColorsSet: ColorArray = ($00, $08, $07, $0F, $09);
var
  MonoColors: ColorArray;

const
  cmPalMonochrome = 11300;
  cmPalColor = 11301;
  cmPalBlackWhite = 11303;

  { TColorSelector }

constructor TColorSelector.Init(var Bounds: TRect; ASelType: TColorSel);
  begin
  TView.Init(Bounds);
  Options := Options or (ofSelectable+ofFirstClick+ofFramed);
  EventMask := EventMask or evBroadcast;
  SelType := ASelType;
  Color := 0;
  end;

constructor TColorSelector.Load(var S: TStream);
  begin
  TView.Load(S);
  S.Read(Color, SizeOf(Byte)+SizeOf(TColorSel));
  end;

procedure TColorSelector.Draw;
  var
    B: TDrawBuffer;
    C, I, J: Integer;
    a: Boolean;
  begin
  a := PColorDialog(Owner)^.BakSel = @Self;

  MoveChar(B, ' ', $70, Size.X);
  for I := 0 to Size.Y do
    begin
    if I < 4 then
      for J := 0 to 3 do
        begin
        C := I*4+J;
        if a then
          MoveChar(B[J*3], #219, C shl 4+C, 3)
        else
          MoveChar(B[J*3], #219, C, 3);
        if C = Byte(Color) then
          begin
          WordRec(B[J*3+1]).Lo := 8;
          if C = 0 then
            WordRec(B[J*3+1]).Hi := $70
          else if a then
            WordRec(B[J*3+1]).Hi := C;
          end;
        end;
    WriteLine(0, I, Size.X, 1, B);
    end;
  end { TColorSelector.Draw };

procedure TColorSelector.HandleEvent(var Event: TEvent);
  const
    Width = 4;
  var
    MaxCol: Byte;
    Mouse: TPoint;
    OldColor: LongInt;

  procedure ColorChanged;
    var
      Msg: Integer;
    begin
    if SelType = csForeground then
      Msg := cmColorForegroundChanged
    else
      Msg := cmColorBackgroundChanged;
    MessageL(Owner, evBroadcast, Msg, Color);
    end;

  begin
  TView.HandleEvent(Event);
  case Event.What of
    evMouseDown:
      begin
      OldColor := Color;
      repeat
        if MouseInView(Event.Where) then
          begin
          MakeLocal(Event.Where, Mouse);
          Color := Mouse.Y*4+Mouse.X div 3;
          end
        else
          Color := OldColor;
        ColorChanged;
        DrawView;
      until not MouseEvent(Event, evMouseMove);
      end;
    evKeyDown:
      begin
      if SelType = csBackground then
        MaxCol := 15
      else
        MaxCol := 15;
      case {CtrlToArrow}(Event.KeyCode) of
        kbLeft:
          if Color > 0 then
            Dec(Color)
          else
            Color := MaxCol;
        kbRight:
          if Color < MaxCol then
            Inc(Color)
          else
            Color := 0;
        kbUp:
          if Color > Width-1 then
            Dec(Color, Width)
          else if Color = 0 then
            Color := MaxCol
          else
            Inc(Color, MaxCol-Width);
        kbDown:
          if Color < MaxCol-(Width-1) then
            Inc(Color, Width)
          else if Color = MaxCol then
            Color := 0
          else
            Dec(Color, MaxCol-Width);
        else {case}
          Exit;
      end {case};
      end;
    evBroadcast:
      if Event.Command = cmColorSet then
        begin
        if SelType = csBackground then
          Color := Event.InfoByte shr 4
        else
          Color := Event.InfoByte and $0F;
        DrawView;
        Exit;
        end
      else
        Exit;
    else {case}
      Exit;
  end {case};
  DrawView;
  ColorChanged;
  ClearEvent(Event);
  end { TColorSelector.HandleEvent };

procedure TColorSelector.Store(var S: TStream);
  begin
  TView.Store(S);
  S.Write(Color, SizeOf(Byte)+SizeOf(TColorSel));
  end;

{ TMonoSelector }

constructor TMonoSelector.Init(var Bounds: TRect; AStrings: PSItem);
  begin
  Options := Options or (ofSelectable+ofFirstClick+ofFramed);
  inherited Init(Bounds, AStrings);
  EventMask := EventMask or evBroadcast;
  end;

procedure TMonoSelector.Draw;
  const
    Button = ' ( ) ';
  begin
  DrawBox(Button, #7);
  end;

procedure TMonoSelector.HandleEvent(var Event: TEvent);
  var
    i: Byte;
  begin
  inherited HandleEvent(Event);
  if GetState(sfVisible) then
    if  (Event.What = evBroadcast) and (Event.Command = cmColorSet) then
      begin
      Value := Event.InfoByte;
      for i := 0 to 3 do
        if MonoColors[i] = Value then
          begin
          MovedTo(i);
          DrawView;
          Exit
          end;
      MovedTo(-1);
      end;
  end;


function TMonoSelector.Mark(Item: Integer): Boolean;
  begin
  Mark := MonoColors[Item] = Value;
  end;

procedure TMonoSelector.NewColor;
  begin
  MessageL(Owner, evBroadcast, cmColorForegroundChanged, Value and $0F);
  MessageL(Owner, evBroadcast, cmColorBackgroundChanged, (Value shr 4)
     and $0F);
  end;

procedure TMonoSelector.Press(Item: Integer);
  begin
  Value := MonoColors[Item];
  NewColor;
  end;

procedure TMonoSelector.MovedTo(Item: Integer);
  const
    Button = ' ( ) ';
  begin
  if Item = -1 then
    begin
    DrawBox(Button, ' ');
    Exit;
    end;
  Value := MonoColors[Item];
  NewColor;
  end;

{ T_BWSelector }

constructor T_BWSelector.Init(var Bounds: TRect; ASelType: TColorSel;
    AStrings: PSItem);
  begin
  SelType := ASelType;
  TCluster.Init(Bounds, AStrings);
  EventMask := EventMask or evBroadcast;
  Options := Options or (ofSelectable+ofFirstClick+ofFramed);
  end;

procedure T_BWSelector.HandleEvent(var Event: TEvent);
  var
    i: Byte;

  begin
  inherited HandleEvent(Event);
  if GetState(sfVisible) then
    if  (Event.What = evBroadcast) and (Event.Command = cmColorSet) then
      begin
      Value := Event.InfoByte;
      case SelType of
        csForeground:
          for i := 0 to 3 do
            if MonoColors[i] = (Value and $0F) then
              begin
              MovedTo(i);
              DrawView;
              Exit
              end;
        csBackground:
          for i := 0 to 3 do
            if MonoColors[i] = (Value shr 4) then
              begin
              MovedTo(i);
              DrawView;
              Exit
              end;
      end {case};
      MovedTo(-1);
      end;
  end { T_BWSelector.HandleEvent };

procedure T_BWSelector.NewColor;
  begin
  if SelType = csForeground
  then
    MessageL(Owner, evBroadcast, cmColorForegroundChanged, Value and $0F)
  else
    MessageL(Owner, evBroadcast, cmColorBackgroundChanged, Value and $0F);
  end;

{ TColorDisplay }

constructor TColorDisplay.Init(var Bounds: TRect; AText: PString);
  begin
  TView.Init(Bounds);
  EventMask := EventMask or evBroadcast;
  Text := AText;
  Color := nil;
  end;

constructor TColorDisplay.Load(var S: TStream);
  begin
  TView.Load(S);
  Text := S.ReadStr;
  end;

destructor TColorDisplay.Done;
  begin
  DisposeStr(Text);
  TView.Done;
  end;

procedure TColorDisplay.Draw;
  var
    B: TDrawBuffer;
    I: Integer;
    C: Byte;
  begin
  C := Color^;
  FillChar(B, Size.X * Size.Y * 2, 0);
  for I := 0 to Size.X div Length(Text^) do
    MoveStr(B[I*Length(Text^)], Text^, C);
  WriteLine(0, 0, Size.X, Size.Y, B);
  end;

procedure TColorDisplay.HandleEvent(var Event: TEvent);
  begin
  TView.HandleEvent(Event);
  case Event.What of
    evBroadcast:
      case Event.Command of
        cmColorBackgroundChanged:
          begin
          Color^:= (Color^ and $0F) or (Event.InfoByte shl 4 and $F0);
          DrawView;
          end;
        cmColorForegroundChanged:
          begin
          Color^:= (Color^ and $F0) or (Event.InfoByte and $0F);
          DrawView;
          end;
      end {case};
  end {case};
  end;

procedure TColorDisplay.SetColor(var AColor: Byte);
  begin
  Color := @AColor;
  MessageL(Owner, evBroadcast, cmColorSet, Color^);
  DrawView;
  end;

procedure TColorDisplay.Store(var S: TStream);
  begin
  TView.Store(S);
  S.WriteStr(Text);
  end;

{ TColorGroupList }

constructor TColorGroupList.Init(var Bounds: TRect; AScrollBar: PScrollBar;
    AGroups: PColorGroup);
  var
    I: Integer;
  begin
  TListViewer.Init(Bounds, 1, nil, AScrollBar);
  Groups := AGroups;
  I := 0;
  while AGroups <> nil do
    begin
    AGroups := AGroups^.Next;
    Inc(I);
    end;
  SetRange(I);
  end;

constructor TColorGroupList.Load(var S: TStream);

  function ReadItems: PColorItem;
    var
      Itms: PColorItem;
      CurItm: ^PColorItem;
      Count: Integer;
      I: Integer;
    begin
    S.Read(Count, SizeOf(Integer));
    Itms := nil;
    CurItm := @Itms;
    for I := 1 to Count do
      begin
      New(CurItm^);
      with CurItm^^ do
        begin
        Name := S.ReadStr;
        S.Read(Index, SizeOf(Byte));
        end;
      CurItm := @CurItm^^.Next;
      end;
    CurItm^:= nil;
    ReadItems := Itms;
    end { ReadItems: };

  function ReadGroups: PColorGroup;
    var
      Grps: PColorGroup;
      CurGrp: ^PColorGroup;
      Count, I: Integer;
    begin
    S.Read(Count, SizeOf(Integer));
    Grps := nil;
    CurGrp := @Grps;
    for I := 1 to Count do
      begin
      New(CurGrp^);
      with CurGrp^^ do
        begin
        Name := S.ReadStr;
        Items := ReadItems;
        end;
      CurGrp := @CurGrp^^.Next;
      end;
    CurGrp^:= nil;
    ReadGroups := Grps;
    end;

  begin { TColorGroupList.Load }
  TListViewer.Load(S);
  Groups := ReadGroups;
  end { TColorGroupList.Load };

destructor TColorGroupList.Done;

  procedure FreeItems(CurITem: PColorItem);
    var
      P: PColorItem;
    begin
    while CurITem <> nil do
      begin
      P := CurITem;
      DisposeStr(CurITem^.Name);
      CurITem := CurITem^.Next;
      Dispose(P);
      end;
    end;

  procedure FreeGroups(CurGroup: PColorGroup);
    var
      P: PColorGroup;
    begin
    while CurGroup <> nil do
      begin
      P := CurGroup;
      FreeItems(CurGroup^.Items);
      DisposeStr(CurGroup^.Name);
      CurGroup := CurGroup^.Next;
      Dispose(P);
      end
    end;

  begin { TColorGroupList.Done }
  TListViewer.Done;
  FreeGroups(Groups);
  end { TColorGroupList.Done };

procedure TColorGroupList.FocusItem(Item: LongInt);
  var
    CurGroup: PColorGroup;
  begin
  TListViewer.FocusItem(Item);
  Item := Focused; {PZ 2000.06.10}
  CurGroup := Groups;
  while Item > 0 do
    begin
    CurGroup := CurGroup^.Next;
    Dec(Item);
    end;
  Message(Owner, evBroadcast, cmNewColorItem, CurGroup);
  end;

function TColorGroupList.GetText(Item: LongInt; MaxLen: Integer): String;
  var
    CurGroup: PColorGroup;
    I: Integer;
  begin
  CurGroup := Groups;
  while Item > 0 do
    begin
    CurGroup := CurGroup^.Next;
    Dec(Item);
    end;
  GetText := CurGroup^.Name^;
  end;

procedure TColorGroupList.Store(var S: TStream);

  procedure WriteItems(Items: PColorItem);
    var
      CurItm: PColorItem;
      Count: Integer;
    begin
    Count := 0;
    CurItm := Items;
    while CurItm <> nil do
      begin
      CurItm := CurItm^.Next;
      Inc(Count);
      end;
    S.Write(Count, SizeOf(Integer));
    CurItm := Items;
    while CurItm <> nil do
      begin
      with CurItm^ do
        begin
        S.WriteStr(Name);
        S.Write(Index, SizeOf(Byte));
        end;
      CurItm := CurItm^.Next;
      end;
    end { WriteItems };

  procedure WriteGroups(Groups: PColorGroup);
    var
      CurGrp: PColorGroup;
      Count: Integer;
    begin
    Count := 0;
    CurGrp := Groups;
    while CurGrp <> nil do
      begin
      CurGrp := CurGrp^.Next;
      Inc(Count);
      end;
    S.Write(Count, SizeOf(Integer));
    CurGrp := Groups;
    while CurGrp <> nil do
      begin
      with CurGrp^ do
        begin
        S.WriteStr(Name);
        WriteItems(Items);
        end;
      CurGrp := CurGrp^.Next;
      end;
    end { WriteGroups };

  begin { TColorGroupList.Store }
  TListViewer.Store(S);
  WriteGroups(Groups);
  end { TColorGroupList.Store };

procedure TColorGroupList.HandleEvent(var Event: TEvent);
  begin
  TListViewer.HandleEvent(Event);
  if Event.What = evBroadcast then
    if Event.Command = cmSaveColorIndex then
      SetGroupIndex(Focused, Event.InfoByte);
  end;

procedure TColorGroupList.SetGroupIndex(GroupNum, ItemNum: Byte);
  var
    Group: PColorGroup;
  begin
  Group := GetGroup(GroupNum);
  if Group <> nil then
    Group^.Index := ItemNum;
  end;

function TColorGroupList.GetGroupIndex(GroupNum: Byte): Byte;
  var
    Group: PColorGroup;
  begin
  Group := GetGroup(GroupNum);
  if Group <> nil then
    GetGroupIndex := Group^.Index
  else
    GetGroupIndex := 0;
  end;

function TColorGroupList.GetGroup(GroupNum: Byte): PColorGroup;
  var
    Group: PColorGroup;
  begin
  Group := Groups;
  while GroupNum > 0 do
    begin
    Group := Group^.Next;
    Dec(GroupNum);
    end;
  GetGroup := Group;
  end;

function TColorGroupList.GetNumGroups: Byte;
  var
    Index: Byte;
    Group: PColorGroup;
  begin
  Index := 0;
  Group := Groups;
  while Group <> nil do
    begin
    Inc(Index);
    Group := Group^.Next;
    end;
  GetNumGroups := Index;
  end;

{ TColorItemList }

constructor TColorItemList.Init(var Bounds: TRect; AScrollBar: PScrollBar;
    AItems: PColorItem);
  var
    I: Integer;
  begin
  TListViewer.Init(Bounds, 1, nil, AScrollBar);
  EventMask := EventMask or evBroadcast;
  Items := AItems;
  I := 0;
  while AItems <> nil do
    begin
    AItems := AItems^.Next;
    Inc(I);
    end;
  SetRange(I);
  end;

procedure TColorItemList.FocusItem(Item: LongInt);
  var
    CurItem: PColorItem;
  begin
  TListViewer.FocusItem(Item);
  Item := Focused; {PZ 2000.06.10}
  MessageL(Owner, evBroadcast, cmSaveColorIndex, Item);
  CurItem := Items;
  while Item > 0 do
    begin
    if CurItem <> nil then
      CurItem := CurItem^.Next;
    Dec(Item);
    end;
  if CurItem <> nil then
    MessageL(Owner, evBroadcast, cmNewColorIndex, CurItem^.Index);
  end;

function TColorItemList.GetText(Item: LongInt; MaxLen: Integer): String;
  var
    CurItem: PColorItem;
  begin
  CurItem := Items;
  while Item > 0 do
    begin
    CurItem := CurItem^.Next;
    Dec(Item);
    end;
  GetText := CurItem^.Name^;
  end;

procedure TColorItemList.HandleEvent(var Event: TEvent);
  var
    CurItem: PColorItem;
    Group: PColorGroup;
    I: Integer;
  begin
  TListViewer.HandleEvent(Event);
  if Event.What = evBroadcast then
    case Event.Command of
      cmNewColorItem:
        begin
        Group := Event.InfoPtr;
        Items := Group^.Items;
        CurItem := Items;
        I := 0;
        while CurItem <> nil do
          begin
          CurItem := CurItem^.Next;
          Inc(I);
          end;
        SetRange(I);
        FocusItem(Group^.Index);
        DrawView;
        end;
    end {case};
  end { TColorItemList.HandleEvent };

{ TColorDialog }

constructor TColorDialog.Init(AGroups: PColorGroup);
  var
    R: TRect;
    P: PView;
    GroupWidth, ItemsWidth: Integer;
    G: PColorGroup;
    I: PColorItem;
    IX, IW: Integer;

  begin
  GroupWidth := 0;
  ItemsWidth := 0;
  G := AGroups;
  while G <> nil do
    begin
    GroupWidth := Max(GroupWidth, Length(CnvString(G^.Name)));
    I := G^.Items;
    while I <> nil do
      begin
      ItemsWidth := Max(ItemsWidth, Length(CnvString(I^.Name)));
      I := I^.Next
      end;
    G := G^.Next
    end;
  R.Assign(0, 0, 30+GroupWidth+ItemsWidth, 22);
  TDialog.Init(R, GetString(dlColorsTitle));
  Options := Options or ofCentered;
  {  Pal := '';}

  IW := 5+GroupWidth;

  R.Assign(IW, 3, IW+1, 16);
  P := New(PScrollBar, Init(R));
  Insert(P);
  R.Assign(3, 3, IW, 16);
  Groups := New(PColorGroupList, Init(R, PScrollBar(P), AGroups));
  Insert(Groups);

  FreeStr := GetString(dl_G_roup);
  R.Assign(2, 2, 4+Length(FreeStr), 3);
  Insert(New(PLabel, Init(R, FreeStr, Groups)));

  IX := IW+3;
  IW := IX+ItemsWidth+2;

  R.Assign(IW, 3, IW+1, 16);
  P := New(PScrollBar, Init(R));
  Insert(P);
  R.Assign(IX, 3, IW, 16);
  P := New(PColorItemList, Init(R, PScrollBar(P), AGroups^.Items));
  Insert(P);

  FreeStr := GetString(dl_I_tem);
  R.Assign(IX-1, 2, IX+Length(FreeStr), 3);
  Insert(New(PLabel, Init(R, FreeStr, P)));

  IX := IW+4;
  IW := IX+12;
  R.Assign(IX, 3, IW, 7);
  ForSel := New(PColorSelector, Init(R, csForeground));
  Insert(ForSel);

  Dec(R.A.X);
  Inc(R.B.X);
  BWSel := New(P_BWSelector, Init(R, csForeground,
        NewSItem(GetString(dlColorsBlack),
          NewSItem(GetString(dlColorsDark),
            NewSItem(GetString(dlColorsGray),
              NewSItem(GetString(dlColorsWhite), nil))))));
  BWSel^.Hide;
  Insert(BWSel);

  Inc(R.A.X);
  Dec(R.B.X);
  Dec(R.A.X, 2);
  Inc(R.B.X, 2);
  MonoSel := New(PMonoSelector, Init(R,
        NewSItem(GetString(dlColorsNormal),
          NewSItem(GetString(dlColorsHighlight),
            NewSItem(GetString(dlColorsUnderline),
              NewSItem(GetString(dlColorsInverse), nil))))));

  MonoSel^.Hide;
  Insert(MonoSel);

  FreeStr := GetString(dl_F_oreground);
  IX := Length(FreeStr);
  if Pos('~', FreeStr) > 0 then
    Dec(IX, 2);
  Inc(R.A.X, 2+((10-IX) div 2));
  Dec(R.B.X, 2+((10-IX) div 2));
  if  (IX and 1) > 0 then
    Dec(R.B.X);
  Dec(R.A.Y);
  R.B.Y := R.A.Y+1;
  ForLabel := New(PLabel, Init(R, FreeStr, ForSel));
  Insert(ForLabel);
  if  (IX and 1) > 0 then
    Inc(R.B.X);

  Dec(R.A.X, ((10-IX) div 2));
  Inc(R.B.X, ((10-IX) div 2));
  FreeStr := GetString(dlColorsColors);
  MonoLabel := New(PLabel, Init(R, FreeStr, MonoSel));
  MonoLabel^.Hide;
  Insert(MonoLabel);

  Inc(R.A.Y, 7);
  Inc(R.B.Y, 10);
  BakSel := New(PColorSelector, Init(R, csBackground));
  Insert(BakSel);

  Dec(R.A.X);
  Inc(R.B.X);
  BWSelBack := New(P_BWSelector, Init(R, csBackground,
        NewSItem(GetString(dlColorsBlack),
          NewSItem(GetString(dlColorsDark),
            NewSItem(GetString(dlColorsGray),
              NewSItem(GetString(dlColorsWhite), nil))))));

  BWSelBack^.Hide;
  Insert(BWSelBack);

  FreeStr := GetString(dl_B_ackground);
  IX := Length(FreeStr);
  if Pos('~', FreeStr) > 0 then
    Dec(IX, 2);

  Inc(R.A.X, 1+((10-IX) div 2));
  Dec(R.B.X, 1+((10-IX) div 2));
  if  (IX and 1) > 0 then
    Dec(R.B.X);
  Dec(R.A.Y);
  R.B.Y := R.A.Y+1;
  BakLabel := New(PLabel, Init(R, FreeStr, BakSel));
  Insert(BakLabel);
  Dec(R.A.X, 2+((10-IX) div 2));
  Inc(R.B.X, 2+((10-IX) div 2));
  if  (IX and 1) > 0 then
    Inc(R.B.X);

  Inc(R.A.Y, 7);
  Inc(R.B.Y, 9);
  FreeStr := GetString(dlColorsText);
  Display := New(PColorDisplay, Init(R, NewStr(' '+FreeStr)));
  Insert(Display);

  if  (Display <> nil) and (AGroups <> nil) and (AGroups^.Items <> nil)
  then
    Display^.SetColor(Byte(Pal[AGroups^.Items^.Index]));

  IX := GroupWidth+ItemsWidth-39;

  FreeStr := GetString(dlColorsDefault);
  if Pos('~', FreeStr) = 0 then
    IW := 14
  else
    IW := 12; {!! IB}
  R.Assign(4, 17, Length(FreeStr)+IW+Length(GetString(dlColorsBlinking)),
     18);
  DefaultColors := New(PCheckBoxes, Init(R,
        NewSItem(FreeStr,
          NewSItem(GetString(dlColorsBlinking),
            nil))));
  Insert(DefaultColors);
  IW := Byte(CurrentBlink)*2;
  DefaultColors^.SetData(IW);

  R.Assign(2 {+IX}, 19, 13 {+IX}, 21);
  FreeStr := GetString(dl_C_olor);
  P := New(PButton, Init(R, FreeStr, cmPalColor, bfNormal or bfBroadcast));
  Insert(P);

  R.Assign(13 {+IX}, 19, 22 {+IX}, 21);
  FreeStr := GetString(dlColorsB_W_);
  P := New(PButton, Init(R, FreeStr, cmPalBlackWhite, bfNormal or
         bfBroadcast));
  Insert(P);

  R.Assign(22 {+IX}, 19, 32 {+IX}, 21);
  FreeStr := GetString(dlColorsMono);
  P := New(PButton, Init(R, FreeStr, cmPalMonochrome, bfNormal or
         bfBroadcast));
  Insert(P);

  R.Assign(32+(IX div 2), 19, 47+(IX div 2), 21);
  FreeStr := GetString(dlColorsVGA);
  P := New(PButton, Init(R, FreeStr, cmPalVGA, bfNormal or bfBroadcast));
  Insert(P);

  R.Assign(47+IX, 19, 57+IX, 21);
  FreeStr := GetString(dlOKButton);
  Insert(New(PButton, Init(R, FreeStr, cmOK, bfDefault)));

  R.Assign(57+IX, 19, 67+IX, 21);
  FreeStr := GetString(dlCancelButton);
  Insert(New(PButton, Init(R, FreeStr, cmCancel, bfNormal)));

  SelectNext(False);
  end { TColorDialog.Init };

destructor TColorDialog.Done;
  var
    i: Byte;
  begin
  for i := apColor to apMonochrome do
    DisposeStr(OldColors[i]);
  inherited Done;
  end;

constructor TColorDialog.Load(var S: TStream);
  var
    Len: Byte;
    R: TRect;
    P: PView;

  begin
  TDialog.Load(S);
  GetSubViewPtr(S, Display);
  GetSubViewPtr(S, Groups);
  GetSubViewPtr(S, ForLabel);
  GetSubViewPtr(S, ForSel);
  GetSubViewPtr(S, BakLabel);
  GetSubViewPtr(S, BakSel);
  GetSubViewPtr(S, MonoSel);
  GetSubViewPtr(S, MonoLabel);
  GetSubViewPtr(S, BWSel);
  GetSubViewPtr(S, BWSelBack);
  GetSubViewPtr(S, DefaultColors);
  {Cat}
  if  (Display = nil) or (Groups = nil) or (ForLabel = nil)
    or (ForSel = nil) or (BakLabel = nil) or (BakSel = nil)
    or (MonoSel = nil) or (MonoLabel = nil) or (BWSel = nil)
    or (BWSelBack = nil) or (DefaultColors = nil)
  then
    Fail;
  {/Cat}
  S.ReadStrV(Pal);
  {S.Read(Len, SizeOf(Byte)); S.Read(Pal[1], Len); SetLength(Pal, Len);}
  if VGASystem {$IFDEF OS2} and not PMWindowed {$ENDIF} then
    EnableCommands([cmPalVGA])
  else
    DisableCommands([cmPalVGA]);
  {!! IB}
  for Len := apColor to apMonochrome do
    OldColors[Len] := NewStr(SystemColors[Len]);
  DefaultColorsFlag := False;
  Len := Byte(CurrentBlink)*2;
  if DefaultColors <> nil then
    DefaultColors^.SetData(Len);
  end { TColorDialog.Load };

procedure TColorDialog.SetAppColor;
  begin
  if DefaultColorsFlag then
    case appPalette of
      apColor:
        SystemColors[apColor] := CColor;
      apBlackWhite:
        SystemColors[apBlackWhite] := CBlackWhite;
      apMonochrome:
        SystemColors[apMonochrome] := CMonochrome;
    end
  else
    SystemColors[appPalette] := OldColors[appPalette]^;
  end;

procedure TColorDialog.GetEvent(var Event: TEvent);
  begin
  if DefaultColors^.Mark(1) <> CurrentBlink then
    begin
    CurrentBlink := not CurrentBlink;
    SetBlink(CurrentBlink);
    ConfigModified := True;
    end;

  if DefaultColors^.Mark(0) <> DefaultColorsFlag then
    begin
    DefaultColorsFlag := not DefaultColorsFlag;
    SetAppColor;

    DoneMemory;
    Pal := Application^.GetPalette^;
    Application^.Redraw;
    end;

  inherited GetEvent(Event);
  end;

procedure TColorDialog.HandleEvent(var Event: TEvent);

  var
    C: Byte;
    ItemList: PColorItemList;
  begin
  if Event.What = evBroadcast then
    case Event.Command of
      cmNewColorItem:
        GroupIndex := Groups^.Focused;
      cmPalMonochrome:
        begin
        ShowMarkers := True;
        appPalette := apMonochrome;
        SetAppColor;
        SwitchTypeSelector;
        end;
      cmPalBlackWhite:
        begin
        ShowMarkers := False;
        appPalette := apBlackWhite;
        SetAppColor;
        SwitchTypeSelector;
        end;
      cmPalColor:
        begin
        ShowMarkers := False;
        appPalette := apColor;
        SetAppColor;
        SwitchTypeSelector;
        end;
      cmPalVGA:
        VGAColorRegister;
    end {case};

  inherited HandleEvent(Event);
  if Event.What = evBroadcast then
    if Event.Command = cmNewColorIndex then
      Display^.SetColor(Byte(Pal[Event.InfoByte]));
  end { TColorDialog.HandleEvent };

procedure TColorDialog.Store(var S: TStream);
  begin
  TDialog.Store(S);
  PutSubViewPtr(S, Display);
  PutSubViewPtr(S, Groups);
  PutSubViewPtr(S, ForLabel);
  PutSubViewPtr(S, ForSel);
  PutSubViewPtr(S, BakLabel);
  PutSubViewPtr(S, BakSel);
  PutSubViewPtr(S, MonoSel);
  PutSubViewPtr(S, MonoLabel);
  PutSubViewPtr(S, BWSel);
  PutSubViewPtr(S, BWSelBack);
  PutSubViewPtr(S, DefaultColors);
  S.WriteStr(@Pal) {S.Write(Pal, Length(Pal)+1);}
  end;

function TColorDialog.DataSize: Word;
  begin
  DataSize := SizeOf(TPalette);
  end;

procedure TColorDialog.GetData(var Rec);
  begin
  GetIndexes(ColorIndexes);
  String(Rec) := Pal;
  end;

procedure TColorDialog.SwitchTypeSelector;
  var
    ViewTop: PGroup;
  begin
  ViewTop := PGroup(Application^.TopView);
  ViewTop := PGroup(ViewTop^.Current);
  case appPalette of
    apMonochrome:
      begin
      MonoColors := MonoColorsSet;
      ForLabel^.Hide;
      ForSel^.Hide;
      BakLabel^.Hide;
      BakSel^.Hide;
      MonoLabel^.Show;
      MonoSel^.Show;
      BWSel^.Hide;
      BWSelBack^.Hide;
      {     BWSel^.SelType:=csForeground;
     ForLabel^.Link:=BWSel;
     BakLabel^.Link:=BWSelBack;    }
      end;
    apColor:
      begin
      MonoSel^.Hide;
      BWSelBack^.Hide;
      MonoLabel^.Hide;
      BWSel^.Hide;
      ForLabel^.Show;
      ForSel^.Show;
      BakLabel^.Show;
      BakSel^.Show;
      ForLabel^.Link := ForSel;
      BakLabel^.Link := BakSel;
      end;
    apBlackWhite:
      begin
      MonoSel^.Hide;
      MonoColors := BWColorsSet;
      MonoLabel^.Hide;
      BakSel^.Hide;
      ForSel^.Hide;
      ForLabel^.Show;
      BakLabel^.Show;
      BWSel^.Show;
      BWSelBack^.Show;
      BWSel^.SelType := csForeground;
      ForLabel^.Link := BWSel;
      BakLabel^.Link := BWSelBack;
      end
  end {case};
  DoneMemory;
  Pal := Application^.GetPalette^;
  Application^.Redraw;
  with Groups^ do
    FocusItem(Focused);
  if  (ViewTop^.State and sfVisible) <> 0 then
    ViewTop^.Select
  else
    Groups^.Select;
  end { TColorDialog.SwitchTypeSelector };

procedure TColorDialog.SetData(var Rec);
  begin
  Pal := String(Rec);
  SetIndexes(ColorIndexes);
  {  Display^.SetColor(Byte(Pal[Groups^.GetGroupIndex(GroupIndex)]));}
  {  Groups^.FocusItem(GroupIndex); } {PZ 2000.06.10}
  SwitchTypeSelector;
  end;

procedure TColorDialog.SetIndexes(var Colors: PColorIndex);
  var
    NumGroups, Index: Byte;
  begin
  NumGroups := Groups^.GetNumGroups;
  if  (Colors <> nil) and (Colors^.ColorSize <> NumGroups) then
    begin
    FreeMem(Colors, 2+Colors^.ColorSize);
    Colors := nil;
    end;
  if Colors = nil then
    begin
    GetMem(Colors, 2+NumGroups);
    FillChar(Colors^, 2+NumGroups, 0);
    Colors^.ColorSize := NumGroups;
    end;
  for Index := 0 to NumGroups-1 do
    Groups^.SetGroupIndex(Index, Colors^.ColorIndex[Index]);
  GroupIndex := Colors^.GroupIndex;
  end;

procedure TColorDialog.GetIndexes(var Colors: PColorIndex);
  var
    NumGroups, Index: Byte;
  begin
  NumGroups := Groups^.GetNumGroups;
  if  (Colors <> nil) and (Colors^.ColorSize <> NumGroups) then
    begin
    FreeMem(Colors, 2+Colors^.ColorSize);
    Colors := nil;
    end;
  if Colors = nil then
    begin
    GetMem(Colors, 2+NumGroups);
    FillChar(Colors^, 2+NumGroups, 0);
    Colors^.ColorSize := NumGroups;
    end;
  Colors^.GroupIndex := GroupIndex;
  for Index := 0 to NumGroups-1 do
    Colors^.ColorIndex[Index] := Groups^.GetGroupIndex(Index);
  end;

{ -- Color list building routines -- }

function ColorItem(const Name: String; Index: Byte;
    Next: PColorItem): PColorItem;
  var
    Item: PColorItem;
  begin
  New(Item);
  Item^.Name := NewStr(Name);
  Item^.Index := Index;
  Item^.Next := Next;
  ColorItem := Item;
  end;

function ColorGroup(const Name: String; Items: PColorItem;
    Next: PColorGroup): PColorGroup;
  var
    Group: PColorGroup;
  begin
  New(Group);
  Group^.Name := NewStr(Name);
  Group^.Items := Items;
  Group^.Next := Next;
  ColorGroup := Group;
  end;

end.
