unit _Menus;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Streams, _Views
  ;

type
  PMenuView = ^TMenuView;
  TMenuView = object(TView)
    ParentMenu: PMenuView;
    Menu: PMenu;
    Current: PMenuItem;
    HaveSubmenu: Boolean;
    LastActionIsExpand: Boolean;
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    {function Execute: Word; virtual;}
    function FindItem(Ch: Char): PMenuItem;
    procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;
    {function GetHelpCtx: Word; virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function HotKey(KeyCode: Word): PMenuItem;
    function NewSubView(var Bounds: TRect; AMenu: PMenu;
         AParentMenu: PMenuView): PMenuView; virtual;
    procedure Store(var S: TStream);
    end;

  PMenuBar = ^TMenuBar;
  TMenuBar = object(TMenuView)
    constructor Init(var Bounds: TRect; AMenu: PMenu);
    {destructor Done; virtual;}
    {procedure Draw; virtual;}
    {procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;}
    end;

  PMenuBox = ^TMenuBox;
  TMenuBox = object(TMenuView)
    TopItem: PMenuItem;
    ComboBoxPal: Boolean;
    constructor Init(var Bounds: TRect; AMenu: PMenu;
         AParentMenu: PMenuView);
    {procedure Draw; virtual;}
    {procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;}
    end;

  PMenuPopup = ^TMenuPopup;
  TMenuPopup = object(TMenuBox)
    constructor Init(var Bounds: TRect; AMenu: PMenu);
    {procedure HandleEvent(var Event: TEvent); virtual;}
    end;

  PStatusLine = ^TStatusLine;
  TStatusLine = object(TView)
    Items: PStatusItem;
    Defs: PStatusDef;
    constructor Init(var Bounds: TRect; ADefs: PStatusDef);
    constructor Load(var S: TStream);
    {destructor Done; virtual;}
    {procedure Draw; virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function Hint(AHelpCtx: Word): String; virtual;
    procedure Store(var S: TStream);
    {procedure Update; virtual;}
    end;

implementation

uses
  _DNFuncs
  ;

constructor TMenuView.Init(var Bounds: TRect);
  begin
  _TMenuView^.Init(Bounds, nil, @Self);
  end;

constructor TMenuView.Load(var S: TStream);
  begin
  _TMenuView^.Load(_Model1.TStream(S), nil, @Self);
  end;

function TMenuView.FindItem(Ch: Char): PMenuItem;
  begin
  Result := _TMenuView^.FindItem(Ch, @Self);
  end;

procedure TMenuView.GetItemRect(Item: PMenuItem; var R: TRect);
  assembler; {&Frame-}
asm
end;

function TMenuView.HotKey(KeyCode: Word): PMenuItem;
  begin
  Result := _TMenuView^.HotKey(KeyCode, @Self);
  end;

function TMenuView.NewSubView(var Bounds: TRect; AMenu: PMenu;
     AParentMenu: PMenuView): PMenuView;
  assembler; {&Frame-}
asm
end;

procedure TMenuView.Store(var S: TStream);
  begin
  _TMenuView^.Store(_Model1.TStream(S), @Self);
  end;

constructor TMenuBar.Init(var Bounds: TRect; AMenu: PMenu);
  begin
  _TMenuBar^.Init(Bounds, AMenu, nil, @Self);
  end;

constructor TMenuBox.Init(var Bounds: TRect; AMenu: PMenu;
     AParentMenu: PMenuView);
  begin
  _TMenuBox^.Init(Bounds, AMenu, _Model1.PMenuView(AParentMenu), nil,
     @Self);
  end;

constructor TMenuPopup.Init(var Bounds: TRect; AMenu: PMenu);
  begin
  _TMenuPopup^.Init(Bounds, AMenu, nil, @Self);
  end;

constructor TStatusLine.Init(var Bounds: TRect; ADefs: PStatusDef);
  begin
  _TStatusLine^.Init(Bounds, ADefs, nil, @Self);
  end;

constructor TStatusLine.Load(var S: TStream);
  begin
  _TStatusLine^.Load(_Model1.TStream(S), nil, @Self);
  end;

function TStatusLine.Hint(AHelpCtx: Word): String;
  assembler; {&Frame-}
asm
end;

procedure TStatusLine.Store(var S: TStream);
  begin
  _TStatusLine^.Store(_Model1.TStream(S), @Self);
  end;

end.
