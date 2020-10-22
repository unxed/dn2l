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

unit Menus;

{-----------------------------------------------------}
{ This module is based on Turbo Vision Menus Unit     }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}

interface

uses
  Defines, Streams, Drivers, Views
  ;

const

  { Color palettes }

  CMenuView = #2#3#4#5#6#7#128#129;
  CStatusLine = #2#3#4#5#6#7;

const
  miDisabled = 1;
   {` Бит TMenuItem.Flags. Пункт недоступен `}
  miSubmenu = 2;
   {` Бит TMenuItem.Flags. Пункт является подменю`}
  miExecDefault = 4;
   {` Бит TMenuItem.Flags. Пункт является подменю, но по Enter
    не раскрывается подменю, а выполняется пункт по умолчанию
    этого подменю. Обычно при наличии этого бита Command <> 0 `}
  miDoNotDisposeSubmenu = 8;
   {` Бит TMenuItem.Flags. При освобождении меню не освобождать
    подменю, на которое ссылается данный пункт `}
  miAllowChangeDefault = 16;
   {` Бит TMenuItem.Flags. У подменю можно сменять пункт по умолчанию
     (нажатием пробела) `}
  miParam = 32;
   {` Бит TMenuItem.Flags. Пункт имеет непустой Param `}


type

  { TMenu types }

  TMenuStr = String[81];

  PMenu = ^TMenu;

  PMenuItem = ^TMenuItem;
  TMenuItem = record
    Next: PMenuItem;
    Name: PString;
    Command: Word;
    Flags: Byte;
    KeyCode: Word;
    HelpCtx: Word;
    Param: PString;
    SubMenu: PMenu;
    end;

  TMenu = record
    Items: PMenuItem;
    Default: PMenuItem;
    end;

  { TMenuView object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PMenuView = ^TMenuView;
  TMenuView = object(TView)
    ParentMenu: PMenuView;
    Menu: PMenu;
    Current: PMenuItem;
    HaveSubmenu: Boolean;
    LastActionIsExpand: Boolean;
      { Такое поле аккуратнее, чем проверки TypeOf(Self) = TypeOf(TMenuBar),
       поскольку такая проверка не даст True для типа, производного от
       TMenuBar. Ни к чему строить такие ловушки. Для классов есть
       операция 'is', но для объектов её аналога нет, поэтому приходится
       исхитряться. }
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    function Execute: Word; virtual;
      { AK155 21.05.2005 (ak50521a.dif) Добавил учёт поля Disabled.
        До этого Disabled-элемент в Draw отображался бледно, но
        в Execute выбирался наравне с доступными. Это может понадобиться
        для элемента, команда которого не может быть отключена, так как
        имеет слишком большой номер. Например, для отключения элемента
        комбобокса. }
    function FindItem(Ch: Char): PMenuItem;
    procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;
    function GetHelpCtx: Word; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function HotKey(KeyCode: Word): PMenuItem;
    function NewSubView(var Bounds: TRect; AMenu: PMenu;
        AParentMenu: PMenuView): PMenuView; virtual;
    procedure Store(var S: TStream);
    function RightExpand: Boolean; virtual;
      { по kbRight раскрывать подменю }
    function LeftCollapse: Boolean; virtual;
      { по kbLeft сворачивать развёрнутое подменю }
    end;

  { TMenuBar object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PMenuBar = ^TMenuBar;
  TMenuBar = object(TMenuView)
    constructor Init(var Bounds: TRect; AMenu: PMenu);
    destructor Done; virtual;
    procedure Draw; virtual;
    procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;
    function Execute: Word; virtual;
    end;

  { TMenuBox object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PMenuBox = ^TMenuBox;
  TMenuBox = object(TMenuView)
    TopItem: PMenuItem;
    ComboBoxPal: Boolean;
    constructor Init(var Bounds: TRect; AMenu: PMenu;
        AParentMenu: PMenuView);
    procedure Draw; virtual;
    procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;
    function Execute: Word; virtual;
    function RightExpand: Boolean; virtual;
    function LeftCollapse: Boolean; virtual;
    end;

  { TMenuPopup object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PMenuPopup = ^TMenuPopup;
  TMenuPopup = object(TMenuBox)
    constructor Init(var Bounds: TRect; AMenu: PMenu);
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  { TStatusItem }

  PStatusItem = ^TStatusItem;
  TStatusItem = record
    Next: PStatusItem;
    Text: PString;
    KeyCode: Word;
    Command: Word;
    end;

  { TStatusDef }

  PStatusDef = ^TStatusDef;
  TStatusDef = record
    Next: PStatusDef;
    Min, Max: Word;
    Items: PStatusItem;
    end;

  { TStatusLine }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PStatusLine = ^TStatusLine;
  TStatusLine = object(TView)
    Items: PStatusItem;
    Defs: PStatusDef;
    constructor Init(var Bounds: TRect; ADefs: PStatusDef);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function Hint(AHelpCtx: Word): String; virtual;
    procedure Store(var S: TStream);
    procedure Update; virtual;
  private
    procedure DrawSelect(Selected: PStatusItem);
    procedure FindItems;
    end;

  { TMenuItem routines }

function NewItem(Name, Param: TMenuStr; KeyCode: Word; Command: Word;
    AHelpCtx: Word; Next: PMenuItem): PMenuItem;
function NewLine(Next: PMenuItem): PMenuItem;
function NewSubMenu(Name: TMenuStr; AHelpCtx: Word; SubMenu: PMenu;
    Next: PMenuItem): PMenuItem;

{ TMenu routines }

function NewMenu(Items: PMenuItem): PMenu;
procedure DisposeMenu(Menu: PMenu);

{ TStatusLine routines }

function NewStatusDef(AMin, AMax: Word; AItems: PStatusItem;
    ANext: PStatusDef): PStatusDef;
function NewStatusKey(const AText: String; AKeyCode: Word; ACommand: Word;
    ANext: PStatusItem): PStatusItem;

const
  dfByCommand = $000;
  dfByPosition = $100;

function LookUpMenu(Menu: PMenu; idCheckItem: Word; Flags: Word)
  : PMenuItem;
function MenuIndexOf(Menu: PMenu; idCheckItem: PMenuItem): Word;

procedure StoreMenuDefaults(Menu: PMenu; var S: TStream);
procedure LoadMenuDefaults(Menu: PMenu; var S: TStream);

const
  MenuActive: Boolean = False;

var
  ParentItem: PMenuItem;
    {` Пункт родительского меню, из которого было вызвано подменю.`}

implementation
uses
  Advance, Advance1, Advance2, Commands, DNHelp, DNApp
  , U_KeyMap
  ;

const
  OldKbdState: Byte = 0;

  { TMenuItem routines }

function NewItem(Name, Param: TMenuStr; KeyCode: Word; Command: Word;
    AHelpCtx: Word; Next: PMenuItem): PMenuItem;
  const
    T: PView = nil;
  var
    P: PMenuItem;
  begin
  if  (Name <> '') and (Command <> 0) then
    begin
    New(P);
    P^.Next := Next;
    P^.Name := NewStr(Name);
    P^.Command := Command;
    P^.Flags := miParam;
    if not T^.MenuEnabled(Command) then {-$VOL}
      P^.Flags := miDisabled;
    P^.KeyCode := KeyCode;
    P^.HelpCtx := AHelpCtx;
    P^.Param := NewStr(Param);
    P^.Submenu := nil;
    NewItem := P;
    end
  else
    NewItem := Next;
  end;

function NewLine(Next: PMenuItem): PMenuItem;
  var
    P: PMenuItem;
  begin
  New(P);
  FillChar(P^, SizeOf(P^), 0);
  P^.Next := Next;
  P^.Name := nil;
  P^.HelpCtx := hcNoContext;
  NewLine := P;
  end;

function NewSubMenu(Name: TMenuStr; AHelpCtx: Word; SubMenu: PMenu;
    Next: PMenuItem): PMenuItem;
  var
    P: PMenuItem;
  begin
  if  (Name <> '') and (SubMenu <> nil) then
    begin
    New(P);
    P^.Next := Next;
    P^.Name := NewStr(Name);
    P^.Command := 0;
    P^.KeyCode := 0;
    P^.Flags := miSubmenu;
    P^.HelpCtx := AHelpCtx;
    P^.SubMenu := SubMenu;
    P^.Param := nil;
    NewSubMenu := P;
    end
  else
    NewSubMenu := Next;
  end;

{ TMenu routines }

function NewMenu(Items: PMenuItem): PMenu;
  var
    P: PMenu;
  begin
  New(P);
  P^.Items := Items;
  P^.Default := Items;
  NewMenu := P;
  end;

procedure DisposeMenu(Menu: PMenu);
  var
    P, Q: PMenuItem;
  begin
  if Menu <> nil then
    begin
    P := Menu^.Items;
    while P <> nil do
      begin
      if P^.Name <> nil then
        begin
        DisposeStr(P^.Name);
        DisposeStr(P^.Param);
        if P^.Flags and miDoNotDisposeSubmenu = 0 then
          DisposeMenu(P^.SubMenu);
        end;
      Q := P;
      P := P^.Next;
      Dispose(Q);
      end;
    Dispose(Menu);
    end;
  end { DisposeMenu };

{ TMenuView }

constructor TMenuView.Init(var Bounds: TRect);
  begin
  Bounds.B.X := Bounds.A.X; {new}
  Bounds.B.Y := Bounds.A.Y; {new}
  TView.Init(Bounds);
  EventMask := EventMask or evBroadcast;
  end;

constructor TMenuView.Load(var S: TStream);

  function DoLoadMenu: PMenu;
    var
      Item: PMenuItem;
      Last: ^PMenuItem;
      Menu: PMenu;
      Tok: Byte;
    begin
    New(Menu);
    Last := @Menu^.Items;
    Item := nil;
    S.Read(Tok, 1);
    while Tok <> 0 do
      begin
      New(Item);
      Last^:= Item;
      Last := @Item^.Next;
      with Item^ do
        begin
        Name := S.ReadStr;
        S.Read(Command, SizeOf(Command));
        S.Read(Flags, SizeOf(Flags));
        S.Read(KeyCode, SizeOf(KeyCode));
        S.Read(HelpCtx, SizeOf(HelpCtx));
        if  (Name <> nil) then
          begin
          Param := nil;
          if (Flags and miParam) <> 0 then
            Param := S.ReadStr;
          SubMenu := nil;
          if (Flags and miSubmenu) <> 0 then
            SubMenu := DoLoadMenu;
          end;
        end;
      S.Read(Tok, 1);
      end;
    Last^:= nil;
    Menu^.Default := Menu^.Items;
    DoLoadMenu := Menu;
    end { DoLoadMenu: };

  begin { TMenuView.Load }
  TView.Load(S);
  Menu := DoLoadMenu;
  end { TMenuView.Load };

function TMenuView.Execute: Word;
  type
    MenuAction = (DoNothing, DoSelect, DoReturn);
  var
    AutoSelect: Boolean;
    Action: MenuAction;
    Ch: Char;
    ItemShown, P: PMenuItem;
    Target: PMenuView;
    R: TRect;
    E: TEvent;
    MouseActive: Boolean;
    MouseWalk: Boolean;
    Going: Boolean; {new}
    Mouse: TPoint;

  procedure TrackKey(FindNext: Boolean);forward;

  procedure TrackMouse;
    var
      R: TRect;
    label q;
    begin
    MakeLocal(E.Where, Mouse);
    if Size.Y > 1 then
      begin {new begin}
      if Mouse.Y = 0 then
        begin
        if not Going then
          begin
          Going := True;
          TrackKey(False);
          end
        else
          begin
          TrackKey(False);
          if  (Current <> nil) and (Current^.Next = nil) then
            TrackKey(True);
          end
        end
      else if Mouse.Y >= Size.Y-1 then
        begin
        if not Going then
          begin
          Going := True;
          TrackKey(True);
          end
        else
          begin
          TrackKey(True);
          if Current = Menu^.Items then
            TrackKey(False);
          end
        end
      else
        goto q; {new end}
      AutoSelect := False;
      Action := DoNothing;
      MouseWalk := True;
      MouseActive := True;
      Exit;
      end;
q:
    MouseWalk := False;
    Current := Menu^.Items;
    while Current <> nil do
      begin
      GetItemRect(Current, R);
      if R.Contains(Mouse) then
        begin
        MouseActive := True;
        Exit;
        end;
      Current := Current^.Next;
      end;
    end { TrackMouse };

  procedure TrackKey(FindNext: Boolean);

    procedure NextItem;
      begin
      Current := Current^.Next;
      if Current = nil then
        Current := Menu^.Items;
      end;

    procedure PrevItem;
      var
        P: PMenuItem;
      begin
      P := Current;
      if P = Menu^.Items then
        P := nil;
      repeat
        NextItem
      until Current^.Next = P;
      end;

    begin { TrackKey }
    if Current <> nil then
      repeat
        if FindNext then
          NextItem
        else
          PrevItem;
      until Current^.Name <> nil;
    end { TrackKey };

  function MouseInOwner: Boolean;
    var
      Mouse: TPoint;
      R: TRect;
    begin
    if  (ParentMenu <> nil) and (ParentMenu^.Size.Y = 1) then
      begin
      ParentMenu^.MakeLocal(E.Where, Mouse);
      ParentMenu^.GetItemRect(ParentMenu^.Current, R);
      MouseInOwner := R.Contains(Mouse);
      end
    else
      MouseInOwner := False;
    end;

  function MouseInMenus: Boolean;
    var
      P: PMenuView;
    begin
    P := ParentMenu;
    while (P <> nil) and not P^.MouseInView(E.Where) do
      P := P^.ParentMenu;
    MouseInMenus := P <> nil;
    end;

  function TopMenu: PMenuView;
    var
      P: PMenuView;
    begin
    P := @Self;
    while P^.ParentMenu <> nil do
      P := P^.ParentMenu;
    TopMenu := P;
    end;

  var
    Frst: Boolean;

  var
    W, H, L: Integer;
    R2: TRect;
    ExecDefault: Boolean;
  label
    lEnter, lHotkey;
  begin { TMenuView.Execute: }
  LastActionIsExpand := False;
  AutoSelect := False;
  Result := 0;
  ItemShown := nil;
  Current := Menu^.Default;
  MouseActive := False;
  Frst := False;
  repeat
    MenuActive := True; {new end}
    Action := DoNothing;
    GetEvent(E);
    case E.What of
      evMouseDown:
        if  (MouseInView(E.Where) or MouseInOwner) then
          begin
          TrackMouse;
          if Size.Y = 1 then
            AutoSelect := True;
          end
        else
          Action := DoReturn;
      evMouseUp:
        if not MouseWalk then
          begin
          TrackMouse;
          if MouseInOwner
          then
            Current := Menu^.Default
          else if (Current <> nil) and (Current^.Name <> nil)
          then
            begin
            Action := DoSelect;
            ExecDefault := Mouse.X < Size.X-5;
            end
          else if (Current <> nil) and (Current^.Name = nil)
          then
            Action := DoNothing
          else if MouseActive
          then
            Action := DoReturn
          else
            begin
            Current := Menu^.Default;
            if Current = nil then
              Current := Menu^.Items;
            Action := DoNothing;
            end;
          end
        else
          Going := False; {new}
      evMouseMove:
        if  (E.Buttons <> 0) then
          begin
          TrackMouse;
          if not (MouseInView(E.Where) or MouseInOwner) and
            MouseInMenus
          then
            Action := DoReturn;
          end;
      evMouseAuto:
        if  (E.Buttons <> 0) then
          begin
          TrackMouse;
          if not (MouseInView(E.Where) or MouseInOwner) and
            MouseInMenus
          then
            Action := DoReturn;
          end;
      evKeyDown:
        case E.KeyCode of
          {new begin}
          kbDownUp, kbUpUp:
            if Going then
              Going := False;
          else {case}
            case {CtrlToArrow}(E.KeyCode) of
              kbUp, kbDown:
                if Size.Y <> 1 then
                  begin
                  if {CtrlToArrow}(E.KeyCode) = kbDown then
                    if not Going then
                      begin
                      Going := True;
                      TrackKey(True);
                      end
                    else
                      begin
                      TrackKey(True);
                      {if Current = Menu^.Items then TrackKey(False);}
                      {JO - зачем здесь это???}
                      end
                  else if not Going then
                    begin
                    Going := True;
                    TrackKey(False);
                    end
                  else
                    begin
                    TrackKey(False);
                    {if Current^.Next = nil then TrackKey(true);}
                    {JO - зачем здесь это???}
                    end;
                  end
                else if E.KeyCode = kbDown then
                  AutoSelect := True; {new end}
              kbRight:
                begin
                if RightExpand then
                  goto lEnter;
                if ParentMenu = nil then
                  TrackKey( {CtrlToArrow}(E.KeyCode) = kbRight)
                else
                  Action := DoReturn;
                end;
              kbLeft:
                begin
                if LeftCollapse then
                  {nothing to do}
                else if ParentMenu = nil then
                  TrackKey( {CtrlToArrow}(E.KeyCode) = kbRight)
                else
                  Action := DoReturn;
                end;
              kbHome, kbEnd:
                if Size.Y <> 1 then
                  begin
                  Current := Menu^.Items;
                  if E.KeyCode = kbEnd then
                    TrackKey(False);
                  end;
              kbPgUp: {new begin}
                if Size.Y <> 1 then
                  begin
                  for W := 1 to Size.Y-2 do
                    if Current = Menu^.Items then
                      Break
                    else
                      begin
                      P := Current;
                      Current := Menu^.Items;
                      repeat
                        Current := Current^.Next;
                        if Current = nil then
                          Current := Menu^.Items;
                      until Current^.Next = P;
                      end;
                  while Current^.Name = nil do
                    begin
                    Current := Current^.Next;
                    if Current = nil then
                      Current := Menu^.Items;
                    end;
                  end;
              kbPgDn:
                if Size.Y <> 1 then
                  begin
                  for W := 1 to Size.Y-2 do
                    if  (Current = Menu^.Items) {and (W>1)}
                      {JO - зачем здесь это???}
                      then
                      begin
                      TrackKey(False);
                      Break
                      end
                    else
                      begin
                      Current := Current^.Next;
                      if Current = nil then
                        Current := Menu^.Items;
                      end;
                  if Current = nil then
                    Current := Menu^.Items;
                  while Current^.Name = nil do
                    begin
                    Current := Current^.Next;
                    if Current = nil then
                      Current := Menu^.Items;
                    end;
                  end; {new end}
              kbEnter, kbShiftEnter:
                begin
lEnter:
                if Size.Y = 1 then
                  AutoSelect := True;
                Action := DoSelect;
                ExecDefault := E.KeyCode = kbEnter;
                end;
              kbSpace:
                begin
                { Внутри подменю с умолчанием пробел меняет умолчание;
                  но в других случаях он может оказаться корячей клавишей
                  (например, в меню выбора дисков)}
                if  (ParentMenu <> nil) and
                    (ParentMenu^.Current^.Flags and
                         miAllowChangeDefault <> 0)
                then
                  begin
                  Action := DoNothing;
                  Menu^.Default := Current;
                  Draw;
                  end
                else
                  goto lHotkey;
                end;
              kbDoubleAlt:
                begin
                if not Frst then
                  begin
                  Frst := True;
                  ClearEvent(E)
                  end
                else
                  begin
                  Action := DoReturn;
                  if  (ParentMenu = nil) or (ParentMenu^.Size.Y <> 1)
                  then
                    ClearEvent(E);
                  end;
                end;
              kbESC:
                begin
                Action := DoReturn;
                Result := 0;
                if  (ParentMenu = nil) or (ParentMenu^.Size.Y <> 1) then
                  ClearEvent(E);
                end;
              else {case}
                begin
lHotkey:
                Target := @Self;
                Ch := GetAltChar(E.KeyCode);
                if Ch = #0 then
                  Ch := E.CharCode
                else
                  Target := TopMenu;
                P := Target^.FindItem(Ch);
                if  (Target = @Self) and (P = nil)
                then
                  P := FindItem(GetAltChar(E.KeyCode and $FFFF00));
                if P = nil then
                  begin
                  P := TopMenu^.HotKey(E.KeyCode);
                  if  (P <> nil) and CommandEnabled(P^.Command) then
                    begin
                    Result := P^.Command;
                    Action := DoReturn;
                    end
                  end
                else if Target = @Self then
                  begin
                  if Size.Y = 1 then
                    AutoSelect := True;
                  Action := DoSelect;
                  ExecDefault := True;
                  Current := P;
                  end
                else if (ParentMenu <> Target)
                     or (ParentMenu^.Current <> P)
                then
                  Action := DoReturn;
                end;
            end {case }
        end {case}; {new}
      evCommand:
        if  (E.Command = cmMenu) or (E.Command = cmMainMenu) then
          begin
          AutoSelect := False;
          if not Frst then
            begin
            Frst := True;
            ClearEvent(E)
            end
          else
            begin
            Action := DoReturn;
            if  (ParentMenu = nil) or (ParentMenu^.Size.Y <> 1) then
              ClearEvent(E);
            end;
          {if ParentMenu <> nil then Action := DoReturn;}
          end
        else
          Action := DoReturn;
      else
        TinySlice;
    end {case};
    if ItemShown <> Current then
      begin
      ItemShown := Current;
      DrawView;
      end;
    LastActionIsExpand := False;
    if  (Action = DoSelect) or ((Action = DoNothing) and AutoSelect)
    then
      begin
      if Current <> nil then
        with Current^ do
          if Name <> nil then
            if Flags and miSubmenu <> 0 then
              begin
              ParentItem := Current;
              if ExecDefault and (Flags and miExecDefault <> 0) then
                Result := Submenu^.Default^.Command
              else
                begin
                LastActionIsExpand := True;
                if E.What and (evMouseDown+evMouseMove) <> 0 then
                  PutEvent(E);
                GetItemRect(Current, R);
                R.A.X := R.A.X+Origin.X;
                R.A.Y := R.B.Y+Origin.Y;
                R.B := Owner^.Size;
                if Size.Y = 1 then
                  Dec(R.A.X);
                Target := TopMenu^.NewSubView(R, SubMenu, @Self);
                Result := Owner^.ExecView(Target);
                Dispose(Target, Done);
                end;
              end
            else if (Action = DoSelect) and ((Flags and miDisabled) = 0) then
              begin
              if (ParentMenu <> nil) and
                (ParentMenu^.Current^.Flags and miExecDefault <> 0)
              then
                ParentItem := ParentMenu^.Current
              else
                ParentItem := nil;
              Result := Command;
              end;
      end;
    if  (Result <> 0) and CommandEnabled(Result) then
      begin
      Action := DoReturn;
      ClearEvent(E);
      end;
  until Action = DoReturn;
  if E.What <> evNothing then
    if  (ParentMenu <> nil) or (E.What = evCommand) then
      PutEvent(E);
  if Current <> nil then
    begin
    { Выбранный пункт не делаем пунктом по умолчанияю, если
    пункт родительского меню имеет подменю с miExecDefault.
    В этом случае смена умолчания - только пробелом, да и то
    только при наличии разрешения (miAllowChangeDefault) }
    if (ParentMenu = nil) or
       (ParentMenu^.Current^.Flags and miExecDefault = 0)
    then
      Menu^.Default := Current;
    Current := nil;
    DrawView;
    end;
  MenuActive := False;
  end { TMenuView.Execute: };

function TMenuView.FindItem(Ch: Char): PMenuItem;
  var
    P: PMenuItem;
    I: Integer;
  begin
  Ch := UpCaseArray[Ch];
  P := Menu^.Items;
  while P <> nil do
    begin
    if  (P^.Name <> nil) and ((P^.Flags and miDisabled) = 0) then
      begin
      I := Pos('~', P^.Name^);
      if  (I <> 0) and (Ch = UpCaseArray[P^.Name^[I+1]]) then
        begin
        FindItem := P;
        Exit;
        end;
      end;
    P := P^.Next;
    end;
  FindItem := nil;
  end;

procedure TMenuView.GetItemRect(Item: PMenuItem; var R: TRect);
  begin
  end;

function TMenuView.GetHelpCtx: Word;
  var
    C: PMenuView;
  label
    Loop;
  begin
  C := @Self;
  GetHelpCtx := hcNoContext;
Loop:
  if C = nil then
    Exit;
  if  (C^.Current <> nil) and (C^.Current^.HelpCtx <> hcNoContext) then
    begin
    GetHelpCtx := C^.Current^.HelpCtx;
    Exit;
    end;
  GetHelpCtx := C^.HelpCtx;
  C := C^.ParentMenu;
  goto Loop;
  end;

function TMenuView.GetPalette: PPalette;
  const
    P: String[Length(CMenuView)] = CMenuView;
  begin
  GetPalette := @P;
  end;

procedure TMenuView.HandleEvent(var Event: TEvent);
  var
    CallDraw: Boolean;
    P: PMenuItem;

  procedure UpdateMenu(Menu: PMenu);
    var
      P: PMenuItem;
      CommandState: Boolean;
    begin
    P := Menu^.Items;
    while P <> nil do
      begin
      if P^.Name <> nil then
        if P^.Flags and miSubmenu <> 0 then
          UpdateMenu(P^.SubMenu);
        if P^.Command <> 0 then
          begin
          CommandState := MenuEnabled(P^.Command);
          if ((P^.Flags and miDisabled) <> 0) = CommandState then
            begin
            P^.Flags := P^.Flags xor miDisabled;
            CallDraw := True;
            end;
          end;
      P := P^.Next;
      end;
    end { UpdateMenu };

  procedure DoSelect;
    begin
    PutEvent(Event);
    Event.Command := Owner^.ExecView(@Self);
    if  (Event.Command <> 0) and CommandEnabled(Event.Command) then
      begin
      Event.What := evCommand;
      Event.InfoPtr := nil;
      PutEvent(Event);
      end;
    ClearEvent(Event);
    end;

  var
    q: TPoint;
  begin { TMenuView.HandleEvent }
  if Menu <> nil then
    case Event.What of
      evMouseDown:
        DoSelect;
      evKeyDown:
        { AK155 9-07-2002 То, что каждый символ протаскивается через все меню,
не добавляет скорости. Реально тут хоткеи обрабатываются только для
неактивного меню, поэтому имеет смысл анализировать только клавиши
с Ctrl или Alt, а также некоторые с Shift (те, которые не светятся
в StatusLine). Hо поскольку выискивать все шифтовые клавиши не хочется,
пусть проверяются все. Реально есть, как минимум, такие:
GrayPlus, GrayMinus, F2, F10, F11, F12 }
        if  ( (Event.KeyCode and $F0000) <> 0) {AK155} and
            (FindItem(GetAltChar(Event.KeyCode)) <> nil)
        then
          DoSelect
        else
          begin
          P := HotKey(Event.KeyCode);
          if  (P <> nil) and CommandEnabled(P^.Command) then
            begin
            Event.What := evCommand;
            Event.Command := P^.Command;
            Event.InfoPtr := nil;
            PutEvent(Event);
            ClearEvent(Event);
            end;
          end;
      evCommand:
        if Event.Command = cmMenu then
          DoSelect;
      evBroadcast:
        if Event.Command = cmCommandSetChanged then
          begin
          CallDraw := False;
          UpdateMenu(Menu);
          if CallDraw then
            DrawView;
          end;
    end {case};
  end { TMenuView.HandleEvent };

function TMenuView.HotKey(KeyCode: Word): PMenuItem;

  function FindHotKey(P: PMenuItem): PMenuItem;
    var
      T: PMenuItem;
    begin
    while P <> nil do
      begin
      if P^.Name <> nil then
        if P^.Flags and miSubmenu <> 0 then
          begin
          T := FindHotKey(P^.SubMenu^.Items);
          if T <> nil then
            begin
            FindHotKey := T;
            Exit;
            end;
          end
        else if ((P^.Flags and miDisabled) = 0) and (P^.KeyCode <> kbNoKey) and
            (P^.KeyCode = KeyCode)
        then
          begin
          FindHotKey := P;
          Exit;
          end;
      P := P^.Next;
      end;
    FindHotKey := nil;
    end { FindHotKey };

  begin { TMenuView.HotKey }
  HotKey := FindHotKey(Menu^.Items);
  end { TMenuView.HotKey };

function TMenuView.NewSubView(var Bounds: TRect; AMenu: PMenu;
    AParentMenu: PMenuView): PMenuView;
  begin
  NewSubView := New(PMenuBox, Init(Bounds, AMenu, AParentMenu));
  end;

procedure TMenuView.Store(var S: TStream);

  procedure DoStoreMenu(Menu: PMenu);
    var
      Item: PMenuItem;
      Tok: Byte;
    begin
    Tok := $FF;
    Item := Menu^.Items;
    while Item <> nil do
      begin
      with Item^ do
        begin
        S.Write(Tok, 1);
        S.WriteStr(Name);
        S.Write(Command, SizeOf(Command));
        S.Write(Flags, SizeOf(Flags));
        S.Write(KeyCode, SizeOf(KeyCode));
        S.Write(HelpCtx, SizeOf(HelpCtx));
        if  (Name <> nil) then
          begin
          if (Flags and miParam) <> 0 then
            S.WriteStr(Param);
          if (Flags and miSubmenu) <> 0 then
            DoStoreMenu(SubMenu);
          end;
        end;
      Item := Item^.Next;
      end;
    Tok := 0;
    S.Write(Tok, 1);
    end { DoStoreMenu };

  begin { TMenuView.Store }
  TView.Store(S);
  DoStoreMenu(Menu);
  end { TMenuView.Store };

function TMenuView.RightExpand: Boolean;
  begin
  Result := False;
  end;

function TMenuView.LeftCollapse: Boolean;
  begin
  Result := False;
  end;

{ TMenuBar }

constructor TMenuBar.Init(var Bounds: TRect; AMenu: PMenu);
  var
    R: TRect;
  begin
  TMenuView.Init(Bounds);
  GrowMode := gfGrowHiX;
  Menu := AMenu;
  Options := Options or ofPreProcess;
  end;

destructor TMenuBar.Done;
  begin
  TMenuView.Done;
  DisposeMenu(Menu);
  end;

procedure TMenuBar.Draw;
  var
    X, L: Integer;
    CNormal, CSelect, CNormDisabled, CSelDisabled, Color: Word;
    P: PMenuItem;
    B: TDrawBuffer;
  begin
  CNormal := GetColor($0301);
  CSelect := GetColor($0604);
  CNormDisabled := GetColor($0202);
  CSelDisabled := GetColor($0505);
  MoveChar(B, ' ', Byte(CNormal), Size.X);
  if Menu <> nil then
    begin
    X := 1;
    P := Menu^.Items;
    while P <> nil do
      begin
      if P^.Name <> nil then
        begin
        L := CStrLen(P^.Name^);
        if X+L < Size.X then
          begin
          if ((P^.Flags and miDisabled) <> 0) then
            if P = Current then
              Color := CSelDisabled
            else
              Color := CNormDisabled
          else if P = Current then
            Color := CSelect
          else
            Color := CNormal;
          MoveChar(B[X], ' ', Byte(Color), 1);
          MoveCStr(B[X+1], P^.Name^, Color);
          MoveChar(B[X+L+1], ' ', Byte(Color), 1);
          end;
        Inc(X, L+2);
        end;
      P := P^.Next;
      end;
    end;
  WriteBuf(0, 0, Size.X, 1, B);
  end { TMenuBar.Draw };

procedure TMenuBar.GetItemRect(Item: PMenuItem; var R: TRect);
  var
    P: PMenuItem;
  begin
  R.Assign(1, 0, 1, 1);
  P := Menu^.Items;
  while True do
    begin
    R.A.X := R.B.X;
    if P^.Name <> nil then
      Inc(R.B.X, CStrLen(P^.Name^)+2);
    if P = Item then
      Exit;
    P := P^.Next;
    end;
  end;

function TMenuBar.Execute: Word;
  begin
  Message(Owner, evCommand, cmMenuOn, @Self);
  Result := inherited Execute;
  Message(Owner, evCommand, cmMenuOff, @Self);
  end;

{ TMenuBox }

constructor TMenuBox.Init(var Bounds: TRect; AMenu: PMenu;
    AParentMenu: PMenuView);
  begin
  TMenuView.Init(Bounds);
  State := State or sfShadow;
  Options := Options or ofPreProcess;
  Menu := AMenu;
  ParentMenu := AParentMenu;
  if Menu <> nil then
    TopItem := Menu^.Items;
  ComboBoxPal := False;
  end;

procedure TMenuBox.Draw;
  var
    CNormal, CSelect, CNormDisabled, CSelDisabled, Color: Word;
    Y: Integer;
    P: PMenuItem;
    B: TDrawBuffer;
    TopIndex: Integer;

  function PastEnd: Boolean;
    var
      P: PMenuItem;
      H: Integer;
    begin
    H := 1;
    P := TopItem;
    while (P <> nil) and (H < Size.Y) do
      begin
      Inc(H);
      P := P^.Next;
      end;
    PastEnd := H > Size.Y-1;
    end;

  procedure FrameLine(N: Integer);
    const
      FrameChars: array[0..19] of Char =
      #32#218#196#191#32#32#192#196#217#32#32#179#32#179#32#32#195#196#180#32
      ;
      {'  |'' --  ''|       |.. --  ..|       |      |         |-  --  -|     '}
      UpArr: array[0..2] of Char = #32#30#32; { ^ }
      DnArr: array[0..2] of Char = #32#31#32; { v }
    begin
    MoveBuf(B[0], FrameChars[N], Byte(CNormal), 2);
    MoveChar(B[2], FrameChars[N+2], Byte(Color), Size.X-4);
    MoveBuf(B[Size.X-2], FrameChars[N+3], Byte(CNormal), 2);
    if  (N = 0) and (TopItem <> Menu^.Items)
    then
      MoveBuf(B[4], UpArr, Byte(Color), 3)
    else if (N = 5) and (PastEnd) then
      MoveBuf(B[4], DnArr, Byte(Color), 3);
    end;

  procedure DrawLine;
    begin
    WriteBuf(0, Y, Size.X, 1, B);
    Inc(Y);
    end;

  var
    ParamEnd: Integer;
    SubmenuArrow: string[3];

  {--- start -------- Eugeny Zvyagintzev ---- 28-09-2003 ----}
  {Procedure was optimized: multiple recursion calls are removed}
  begin { TMenuBox.Draw }
  if not ComboBoxPal then
    begin
    CNormal := GetColor($0301);
    CSelect := GetColor($0604);
    CNormDisabled := GetColor($0202);
    CSelDisabled := GetColor($0505);
    end
  else
    begin
//JO: фон подсвеченных хоткеев делаем таким же как общий фон комбобокса,
//    а цвет буковок - как у хоткеев в обычном меню
    CNormal := (GetColor($0307) and $0FFF) + (GetColor($0700) and $F000);
    CSelect := (GetColor($0608) and $0FFF) + (GetColor($0800) and $F000);
//JO: аналогично - с недоступными элементами
    CNormDisabled := (GetColor($0202) and $0F0F)
                      + (GetColor($0707) and $F0F0);
    CSelDisabled := (GetColor($0505) and $0F0F)
                      + (GetColor($0808) and $F0F0);
    end;
  Y := 0;
  ParamEnd := Size.X-3;
  if HaveSubmenu then
    Dec(ParamEnd, 3);
  Color := CNormal;
  FrameLine(0);
  DrawLine;

  if  (Menu <> nil) and (Menu^.Items <> nil) then
    begin
    P := Menu^.Items;
    if TopItem = nil then
      TopItem := Menu^.Items;
    TopIndex := MenuIndexOf(Menu, TopItem);
    if Current <> nil then
      Y := MenuIndexOf(Menu, Current)
    else
      Y := 0;
    if TopIndex > Y then
      TopIndex := Y;
    if  (Y-TopIndex) >= Size.Y-2 then
      TopIndex := Y-Size.Y+3;
    TopItem := LookUpMenu(Menu, TopIndex, dfByPosition);
    Y := 0;
    FrameLine(0);
    DrawLine;

    P := TopItem;
    Y := 1;
    while (Y < Size.Y-1) do
      begin
      Color := CNormal;
      if P^.Name = nil then
        FrameLine(15)
      else
        begin
        if ((P^.Flags and miDisabled) <> 0) then
          if P = Current then
            Color := CSelDisabled
          else
            Color := CNormDisabled
        else if P = Current then
          Color := CSelect;
        FrameLine(10);
        MoveCStr(B[3], P^.Name^, Color);
        if (P = Menu.Default) and (ParentMenu <> nil) and
          (ParentMenu^.Current^.Flags and miExecDefault <> 0)
        then { помечаем алмазиком пункт, который выполняется по
          Enter на пункте родительского меню }
          MoveChar(B[2], #4, Byte(Color), 1);
        if P^.Param <> nil then
          MoveStr(B[ParamEnd-Length(P^.Param^)],
            P^.Param^, Byte(Color));
        if P^.Flags and miSubmenu <> 0 then
          begin
          if P^.Flags and miExecDefault <> 0 then
            SubmenuArrow := '['#16']'
          else
            SubmenuArrow := ' '#16' ';
          MoveStr(B[Size.X-5], SubmenuArrow, Byte(Color));
          end
        end;
      DrawLine;
      P := P^.Next;
      end;
    end;
  Color := CNormal;
  FrameLine(5);
  DrawLine;
  end { TMenuBox.Draw };
{--- finish -------- Eugeny Zvyagintzev ---- 28-09-2003 ----}

procedure TMenuBox.GetItemRect(Item: PMenuItem; var R: TRect);
  var
    Y: Integer;
    P: PMenuItem;
  begin
  Y := 1;
  P := TopItem;
  while (P <> Item) and (P <> nil) do
    begin
    Inc(Y);
    P := P^.Next;
    end;
  if  (P = nil) then
    R.Assign(0, 0, 0, 0)
  else
    R.Assign(2, Y, Size.X-2, Y+1);
  end;

function TMenuBox.Execute: Word;
  var
    W, H, L: Integer;
    R2: TRect;
    P: PMenuItem;
    R: TRect;
  begin
  W := 10;
  H := 2;
  if Menu <> nil then
    begin
    P := Menu^.Items;
    while P <> nil do
      begin
      if P^.Name <> nil then
        begin
        L := CStrLen(P^.Name^)+6;
        if P^.Param <> nil then
          Inc(L, CStrLen(P^.Param^)+2);
        if P^.Flags and miSubmenu <> 0 then
          begin
          Inc(L, 3);
          HaveSubmenu := True;
          end;
        if L > W then
          W := L;
        end;
      Inc(H);
      P := P^.Next;
      end;
    end;
  GetBounds(R);
  Owner^.GetExtent(R2); {new begin}
  if Options and ofCenterX <> 0 then
    begin
    R.A.X := (R2.B.X-R2.A.X-W) div 2;
    end;
  R.B.X := R.A.X+W;

  if Options and ofCenterY <> 0 then
    begin
    R.A.Y := (R2.B.Y-R2.A.Y-H) div 2;
    end;
  R.B.Y := R.A.Y+H;

  if R.B.Y > R2.B.Y then
    begin
    R.A.Y := R2.B.Y-H;
    if R.A.Y < 0 then
      R.A.Y := 0;
    R.B.Y := R.A.Y+H;
    if R.B.Y > R2.B.Y then
      R.B.Y := R2.B.Y;
    end;
  if R.B.X > R2.B.X then
    begin
    R.A.X := R2.B.X-(R.B.X-R.A.X);
    R.B.X := R2.B.X;
    end;
  SetBounds(R);
  Owner^.Redraw;
  Result := inherited Execute;
  end { TMenuBox.Execute };

function TMenuBox.RightExpand: Boolean;
  begin
  Result := not LastActionIsExpand and (Current^.Flags and miSubmenu <> 0);
    { Обычно по kbRight надо раскрывать подменю. Но если оно только что
    было раскрыто и теперь закрывается по нажатию стрелки ВПРАВО,
    то надо не опять раскрывать его, а передавать эту kbRight
    вышележащемму меню }
  end;

function TMenuBox.LeftCollapse: Boolean;
  begin
  Result := LastActionIsExpand;
  end;

{TMenuPopup}

constructor TMenuPopup.Init(var Bounds: TRect; AMenu: PMenu);
  begin
  inherited Init(Bounds, AMenu, nil);
  end;

procedure TMenuPopup.HandleEvent(var Event: TEvent);
  var
    P: PMenuItem;
  begin
  case Event.What of
    evKeyDown:
      begin
      P := FindItem(GetCtrlChar(Event.KeyCode));
      if P = nil then
        P := HotKey(Event.KeyCode);
      if  (P <> nil) and (CommandEnabled(P^.Command)) then
        begin
        Event.What := evCommand;
        Event.Command := P^.Command;
        Event.InfoPtr := nil;
        PutEvent(Event);
        ClearEvent(Event);
        end
      else if GetAltChar(Event.KeyCode) <> #0 then
        ClearEvent(Event);
      end;
  end {case};
  inherited HandleEvent(Event);
  end { TMenuPopup.HandleEvent };

{ TStatusLine }

constructor TStatusLine.Init(var Bounds: TRect; ADefs: PStatusDef);
  begin
  TView.Init(Bounds);
  Options := Options or ofPreProcess;
  EventMask := EventMask or evBroadcast;
  GrowMode := gfGrowLoY+gfGrowHiX+gfGrowHiY;
  Defs := ADefs;
  FindItems;
  end;

constructor TStatusLine.Load(var S: TStream);

  function DoLoadStatusItems: PStatusItem;
    var
      Count: AInt;
      Cur, First: PStatusItem;
      Last: ^PStatusItem;
    begin
    Cur := nil;
    Last := @First;
    S.Read(Count, SizeOf(AInt));
    while Count > 0 do
      begin
      New(Cur);
      Last^:= Cur;
      Last := @Cur^.Next;
      Cur^.Text := S.ReadStr;
      S.Read(Cur^.KeyCode, SizeOf(Cur^.KeyCode));
      S.Read(Cur^.Command, SizeOf(Cur^.Command));
      Dec(Count);
      end;
    Last^:= nil;
    DoLoadStatusItems := First;
    end;

  function DoLoadStatusDefs: PStatusDef;
    var
      AMin: Word;
      Cur, First: PStatusDef;
      Last: ^PStatusDef;
      Count: AInt;
    begin
    Last := @First;
    S.Read(Count, SizeOf(AInt));
    while Count > 0 do
      begin
      New(Cur);
      Last^:= Cur;
      Last := @Cur^.Next;
      S.Read(Cur^.Min, 2*SizeOf(Word));
      Cur^.Items := DoLoadStatusItems;
      Dec(Count);
      end;
    Last^:= nil;
    DoLoadStatusDefs := First;
    end;

  begin { TStatusLine.Load }
  TView.Load(S);
  Defs := DoLoadStatusDefs;
  FindItems;
  end { TStatusLine.Load };

destructor TStatusLine.Done;
  var
    T: PStatusDef;

  procedure DisposeItems(Item: PStatusItem);
    var
      T: PStatusItem;
    begin
    while Item <> nil do
      begin
      T := Item;
      Item := Item^.Next;
      DisposeStr(T^.Text);
      Dispose(T);
      end;
    end;

  begin
  while Defs <> nil do
    begin
    T := Defs;
    Defs := Defs^.Next;
    DisposeItems(T^.Items);
    Dispose(T);
    end;
  TView.Done;
  end { TStatusLine.Done };

procedure TStatusLine.Draw;
  begin
  DrawSelect(nil);
  end;

procedure TStatusLine.DrawSelect(Selected: PStatusItem);
  var
    B: TDrawBuffer;
    T: PStatusItem;
    I, L: Integer;
    CSelect, CNormal, CSelDisabled, CNormDisabled: Word;
    Color: Word;
    HintBuf: String;
    FirstChar: Char;
  begin
  CNormal := GetColor($0301);
  CSelect := GetColor($0604);
  CNormDisabled := GetColor($0202);
  CSelDisabled := GetColor($0505);
  MoveChar(B, ' ', Byte(CNormal), Size.X);
  T := Items;
  I := 0;
  if OldKbdState and 12 = 12 then
    FirstChar := '^'
  else {new}
   if OldKbdState and 8 <> 0 then
    FirstChar := '-'
  else if OldKbdState and 4 <> 0 then
    FirstChar := '+'
  else if OldKbdState and 3 <> 0 then
    FirstChar := ':'
  else
    FirstChar := ' ';
  while T <> nil do
    begin
    if  (T^.Text <> nil)
           and ((FirstChar = ' ') and not (T^.Text^[1] in [':', '+',
           '-', '^'])
        or (T^.Text^[1] = FirstChar))
    then
      begin
      L := CStrLen(T^.Text^)-Byte(FirstChar <> ' ');
      if I+L < Size.X then
        begin
        if CommandEnabled(T^.Command) then
          if T = Selected then
            Color := CSelect
          else
            Color := CNormal
        else if T = Selected then
          Color := CSelDisabled
        else
          Color := CNormDisabled;
        MoveChar(B[I], ' ', Byte(Color), 1);
        MoveCStr(B[I+1], Copy(T^.Text^, 1+Byte(FirstChar <> ' '),
             MaxStringLength), Color);
        MoveChar(B[I+L+1], ' ', Byte(Color), 1);
        end;
      Inc(I, L+2);
      end;
    T := T^.Next;
    end;
  if I < Size.X-2 then
    begin
    HintBuf := Hint(HelpCtx);
    if HintBuf <> '' then
      begin
      MoveChar(B[I], #179, Byte(CNormal), 1);
      Inc(I, 2);
      if I+Length(HintBuf) > Size.X then
        SetLength(HintBuf, Size.X-I);
      MoveStr(B[I], HintBuf, Byte(CNormal));
      end;
    end;
  WriteLine(0, 0, Size.X, 1, B);
  end { TStatusLine.DrawSelect };

procedure TStatusLine.FindItems;
  var
    P: PStatusDef;
  begin
  P := Defs;
  while (P <> nil) and ((HelpCtx < P^.Min) or (HelpCtx > P^.Max)) do
    P := P^.Next;
  if P = nil then
    Items := nil
  else
    Items := P^.Items;
  end;

function TStatusLine.GetPalette: PPalette;
  const
    P: String[Length(CStatusLine)] = CStatusLine;
  begin
  GetPalette := @P;
  end;

procedure TStatusLine.HandleEvent(var Event: TEvent);
  var
    Mouse: TPoint;
    T: PStatusItem;
    S: String[20];

  function ItemMouseIsIn: PStatusItem;
    var
      I, K: Word;
      T: PStatusItem;
      FirstChar: Char;
    begin
    ItemMouseIsIn := nil;
    if Mouse.Y <> 0 then
      Exit;
    I := 0;
    T := Items;
    if OldKbdState and 12 = 12 then
      FirstChar := '^'
    else {new}
     if OldKbdState and 8 <> 0 then
      FirstChar := '-'
    else if OldKbdState and 4 <> 0 then
      FirstChar := '+'
    else if OldKbdState and 3 <> 0 then
      FirstChar := ':'
    else
      FirstChar := ' ';
    while T <> nil do
      begin
      if  (T^.Text <> nil)
             and ((FirstChar = ' ') and not (T^.Text^[1] in [':', '+',
             '-', '^'])
          or (T^.Text^[1] = FirstChar))
      then
        begin
        K := I+CStrLen(T^.Text^)-Byte(FirstChar <> ' ')+2;
        if  (Mouse.X >= I) and (Mouse.X < K) then
          begin
          ItemMouseIsIn := T;
          Exit;
          end;
        I := K;
        end;
      T := T^.Next;
      end;
    end { ItemMouseIsIn: };

  begin { TStatusLine.HandleEvent }
  TView.HandleEvent(Event);
  case Event.What of
    evMouseDown:
      begin
      T := nil;
      repeat
        MakeLocal(Event.Where, Mouse);
        if T <> ItemMouseIsIn then
          begin
          T := ItemMouseIsIn;
          DrawSelect(T);
          end;
      until not MouseEvent(Event, evMouseMove);
      if  (T <> nil) and CommandEnabled(T^.Command) then
        begin
        Event.What := evCommand;
        Event.Command := T^.Command;
        Event.InfoPtr := nil;
        PutEvent(Event);
        end;
      ClearEvent(Event);
      DrawView;
      end;
    evKeyDown:
      begin
      T := Items;
      while T <> nil do
        begin
        if T^.Text <> nil then
          S := T^.Text^
        else
          S := '';
        if  (Event.KeyCode = T^.KeyCode) and
          CommandEnabled(T^.Command) or
            (T^.KeyCode = kbShortCut) and
            (UpCaseArray[S[PosChar('~', S)+1]] =
             UpCaseArray[Event.CharCode])
        then
          begin
          Event.What := evCommand;
          Event.Command := T^.Command;
          Event.InfoPtr := nil;
          Exit;
          end;
        T := T^.Next;
        end;
      end;
    evBroadcast:
      if Event.Command = cmCommandSetChanged then
        DrawView;
  end {case};
  end { TStatusLine.HandleEvent };

function TStatusLine.Hint(AHelpCtx: Word): String;
  begin
  if AHelpCtx = hcTeam then
    Hint := GetString(dlTeamHint)
  else
    Hint := '';
  end;

procedure TStatusLine.Store(var S: TStream);

  procedure DoStoreStatusItems(Cur: PStatusItem);
    var
      T: PStatusItem;
      Count: AInt;
    begin
    Count := 0;
    T := Cur;
    while T <> nil do
      begin
      Inc(Count);
      T := T^.Next
      end;
    S.Write(Count, SizeOf(AInt));
    while Cur <> nil do
      begin
      S.WriteStr(Cur^.Text);
      S.Write(Cur^.KeyCode, SizeOf(Cur^.KeyCode));
      S.Write(Cur^.Command, SizeOf(Cur^.Command));
      Cur := Cur^.Next;
      end;
    end;

  procedure DoStoreStatusDefs(Cur: PStatusDef);
    var
      Count: AInt;
      T: PStatusDef;
    begin
    Count := 0;
    T := Cur;
    while T <> nil do
      begin
      Inc(Count);
      T := T^.Next
      end;
    S.Write(Count, SizeOf(AInt));
    while Cur <> nil do
      begin
      with Cur^ do
        begin
        S.Write(Min, SizeOf(Word)*2);
        DoStoreStatusItems(Items);
        end;
      Cur := Cur^.Next;
      end;
    end { DoStoreStatusDefs };

  begin { TStatusLine.Store }
  TView.Store(S);
  DoStoreStatusDefs(Defs);
  end { TStatusLine.Store };

procedure TStatusLine.Update;
  var
    H: Word;
    P: PView;
  begin
  P := TopView;
  if P <> nil then
    H := P^.GetHelpCtx
  else
    H := hcNoContext;
  if  (HelpCtx <> H) or (OldKbdState <> ShiftState) then
    begin
    OldKbdState := ShiftState;
    HelpCtx := H;
    FindItems;
    DrawView;
    end;
  end;

function NewStatusDef(AMin, AMax: Word; AItems: PStatusItem;
    ANext: PStatusDef): PStatusDef;
  var
    T: PStatusDef;
  begin
  New(T);
  with T^ do
    begin
    Next := ANext;
    Min := AMin;
    Max := AMax;
    Items := AItems;
    end;
  NewStatusDef := T;
  end;

function NewStatusKey(const AText: String; AKeyCode: Word; ACommand: Word;
    ANext: PStatusItem): PStatusItem;
  var
    T: PStatusItem;
  begin
  New(T);
  T^.Text := NewStr(AText);
  T^.KeyCode := AKeyCode;
  T^.Command := ACommand;
  T^.Next := ANext;
  NewStatusKey := T;
  end;

function LookUpMenu(Menu: PMenu; idCheckItem: Word; Flags: Word)
  : PMenuItem;
  var
    Item, LookedUp: PMenuItem;
    i: Integer;
  begin
  LookUpMenu := nil;
  if Menu <> nil then
    with Menu^ do
      begin
      Item := Items;
      if Flags and dfByPosition <> 0
      then
        begin
        for i := 1 to idCheckItem-1 do
          begin
          Item := Item^.Next;
          if Item = nil then
            Break
          end;
        LookUpMenu := Item
        end
      else
        begin
        LookedUp := nil;
        if Item <> nil then
          repeat
            if  (Item^.Flags and miSubmenu <> 0) and (Item^.Name <> nil)
            then
              LookedUp := LookUpMenu(Item^.SubMenu, idCheckItem, Flags)
            else if (Item^.Command = idCheckItem)
            then
              LookedUp := Item;
            Item := Item^.Next;
          until (Item = nil) or (LookedUp <> nil);
        LookUpMenu := LookedUp
        end
      end
  end { LookUpMenu };

function MenuIndexOf(Menu: PMenu; idCheckItem: PMenuItem): Word; {new}
  var
    Item: PMenuItem;
    I: Word;
  begin
  MenuIndexOf := 0;
  if Menu <> nil then
    with Menu^ do
      begin
      Item := Items;
      I := 1;
      while (Item <> idCheckItem) and (Item <> nil) do
        begin
        Inc(I);
        Item := Item^.Next;
        end;
      if Item = nil then
        MenuIndexOf := 0
      else
        MenuIndexOf := I;
      end
  end;

procedure StoreMenuDefaults(Menu: PMenu; var S: TStream); {new}
  var
    Item: PMenuItem;
    I: Word;
  begin
  if Menu <> nil then
    with Menu^ do
      begin
      I := MenuIndexOf(Menu, Default);
      S.Write(I, SizeOf(I));
      Item := Items;
      if Item <> nil then
        repeat
          if  (Item^.Flags and miSubmenu <> 0) and (Item^.Name <> nil)
          then
            StoreMenuDefaults(Item^.SubMenu, S);
          Item := Item^.Next;
        until (Item = nil);
      end
  end;

procedure LoadMenuDefaults(Menu: PMenu; var S: TStream); {new}
  var
    Item: PMenuItem;
    I: Word;
  begin
  if Menu <> nil then
    with Menu^ do
      begin
      S.Read(I, SizeOf(I));
      Default := LookUpMenu(Menu, I, dfByPosition);
      Item := Items;
      if Item <> nil then
        repeat
          if  (Item^.Flags and miSubmenu <> 0) and (Item^.Name <> nil)
          then
            LoadMenuDefaults(Item^.SubMenu, S);
          Item := Item^.Next;
        until (Item = nil);
      end
  end;

end.
