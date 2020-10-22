unit _Apps;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Streams, _Views, _Dialogs
  ;

type
  PBackground = ^TBackground;
  TBackground = object(TView)
    Pattern: Char;
    constructor Init(var Bounds: TRect; APattern: Char);
    constructor Load(var S: TStream);
    {procedure Draw; virtual;}
    {function GetPalette: PPalette; virtual;}
    procedure Store(var S: TStream);
    end;

  PDesktop = ^TDesktop;
  TDesktop = object(TGroup)
    Background: PBackground;
    TileColumnsFirst: Boolean;
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    procedure Cascade(var R: TRect);
    procedure Clear;
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure InitBackground; virtual;
    procedure Store(var S: TStream);
    procedure Tile(var R: TRect);
    procedure TileError; virtual;
    end;

  PProgram = ^TProgram;
  TProgram = object(TGroup)
    IdleSecs: TEventTimer;
    FullSpeed: Boolean;
    constructor Init;
    {destructor Done; virtual;}
    function CanMoveFocus: Boolean;
    function ExecuteDialog(P: PDialog; Data: Pointer): Word;
    {procedure GetEvent(var Event: TEvent); virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure Idle; virtual;
    procedure InitDesktop; virtual;
    procedure InitMenuBar; virtual;
    procedure InitScreen; virtual;
    procedure InitStatusLine; virtual;
    procedure InitCommandLine; virtual;
    function InsertWindow(P: PWindow): PWindow;
    procedure ActivateView(P: PView);
    procedure OutOfMemory; virtual;
    {procedure PutEvent(var Event: TEvent); virtual;}
    procedure Run; virtual;
    procedure SetScreenMode(Mode: Word);
    function ValidView(P: PView): PView;
    {procedure Redraw; virtual;}
    end;

  PApplication = ^TApplication;
  TApplication = object(TProgram)
    Clock: PView;
    constructor Init;
    {destructor Done; virtual;}
    procedure Cascade;
    procedure ShowUserScreen;
    procedure WhenShow; virtual;
    procedure GetTileRect(var R: TRect); virtual;
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure Tile;
    end;

  PDNApplication = ^TDNApplication;
  TDNApplication = object(TApplication)
    IdleClick: TEventTimer;
    IdleEvt: TEvent;
    TreeReader: Pointer {PTreeReader};
    Pk1, Pk2, Pk3, Pk4: PView;
    constructor Init;
    {destructor Done; virtual;}
    {procedure InitMenuBar; virtual;}
    {procedure InitCommandLine; virtual;}
    {procedure InitDesktop; virtual;}
    {procedure InitStatusLine; virtual;}
    procedure ViewFile(AltExt, NoExtFile: Boolean; FileName: String);
    procedure AddFormat;
    procedure EditFile(Intern: Boolean; FileName: String);
    {procedure OutOfMemory; virtual;}
    procedure RetrieveDesktop(const FileName: String; LS: PStream;
         LoadColors: Boolean);
    procedure SaveDesktop(const FileName: String);
    procedure LoadDesktop(var S: TStream);
    procedure StoreDesktop(var S: TStream);
    procedure ChgColors;
    {procedure EventError(var Event: TEvent); virtual;}
    end;

implementation

uses
  _DNFuncs
  ;

constructor TBackground.Init(var Bounds: TRect; APattern: Char);
  begin
  _TBackGround^.Init(Bounds, APattern, nil, @Self);
  end;

constructor TBackground.Load(var S: TStream);
  begin
  _TBackGround^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TBackground.Store(var S: TStream);
  begin
  _TBackGround^.Store(_Model1.TStream(S), @Self);
  end;

constructor TDesktop.Init(var Bounds: TRect);
  begin
  _TDesktop^.Init(Bounds, nil, @Self);
  end;

constructor TDesktop.Load(var S: TStream);
  begin
  _TDesktop^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TDesktop.Cascade(var R: TRect);
  begin
  _TDesktop^.Cascade(R, @Self);
  end;

procedure TDesktop.Clear;
  begin
  _TDesktop^.Clear(@Self);
  end;

procedure TDesktop.InitBackground;
  assembler; {&Frame-}
asm
end;

procedure TDesktop.Store(var S: TStream);
  begin
  _TDesktop^.Store(_Model1.TStream(S), @Self);
  end;

procedure TDesktop.Tile(var R: TRect);
  begin
  _TDesktop^.Tile(R, @Self);
  end;

procedure TDesktop.TileError;
  assembler; {&Frame-}
asm
end;

constructor TProgram.Init;
  begin
  _TProgram^.Init(nil, @Self);
  end;

function TProgram.CanMoveFocus: Boolean;
  begin
  Result := _TProgram^.CanMoveFocus(@Self);
  end;

function TProgram.ExecuteDialog(P: PDialog; Data: Pointer): Word;
  begin
  Result := _TProgram^.ExecuteDialog(_Model1.PDialog(P), Data, @Self);
  end;

procedure TProgram.Idle;
  assembler; {&Frame-}
asm
end;

procedure TProgram.InitDesktop;
  assembler; {&Frame-}
asm
end;

procedure TProgram.InitMenuBar;
  assembler; {&Frame-}
asm
end;

procedure TProgram.InitScreen;
  assembler; {&Frame-}
asm
end;

procedure TProgram.InitStatusLine;
  assembler; {&Frame-}
asm
end;

procedure TProgram.InitCommandLine;
  assembler; {&Frame-}
asm
end;

function TProgram.InsertWindow(P: PWindow): PWindow;
  begin
  Result := PWindow(_TProgram^.InsertWindow(_Model1.PWindow(P), @Self));
  end;

procedure TProgram.ActivateView(P: PView);
  begin
  _TProgram^.ActivateView(_Model1.PView(P), @Self);
  end;

procedure TProgram.OutOfMemory;
  assembler; {&Frame-}
asm
end;

procedure TProgram.Run;
  assembler; {&Frame-}
asm
end;

procedure TProgram.SetScreenMode(Mode: Word);
  begin
  _TProgram^.SetScreenMode(Mode, @Self);
  end;

function TProgram.ValidView(P: PView): PView;
  begin
  Result := PView(_TProgram^.ValidView(_Model1.PView(P), @Self));
  end;

constructor TApplication.Init;
  begin
  _TApplication^.Init(nil, @Self);
  end;

procedure TApplication.Cascade;
  begin
  _TApplication^.Cascade(@Self);
  end;

procedure TApplication.ShowUserScreen;
  begin
  _TApplication^.ShowUserScreen(@Self);
  end;

procedure TApplication.WhenShow;
  assembler; {&Frame-}
asm
end;

procedure TApplication.GetTileRect(var R: TRect);
  assembler; {&Frame-}
asm
end;

procedure TApplication.Tile;
  begin
  _TApplication^.Tile(@Self);
  end;

constructor TDNApplication.Init;
  begin
  _TDNApplication^.Init(nil, @Self);
  end;

procedure TDNApplication.ViewFile(AltExt, NoExtFile: Boolean;
     FileName: String);
  begin
  _TDNApplication^.ViewFile(AltExt, NoExtFile, FileName, @Self);
  end;

procedure TDNApplication.AddFormat;
  begin
  _TDNApplication^.AddFormat(@Self);
  end;

procedure TDNApplication.EditFile(Intern: Boolean; FileName: String);
  begin
  _TDNApplication^.EditFile(Intern, FileName, @Self);
  end;

procedure TDNApplication.RetrieveDesktop(const FileName: String;
     LS: PStream; LoadColors: Boolean);
  begin
  _TDNApplication^.RetrieveDesktop(FileName, _Model1.PStream(LS),
     LoadColors, @Self);
  end;

procedure TDNApplication.SaveDesktop(const FileName: String);
  begin
  _TDNApplication^.SaveDesktop(FileName, @Self);
  end;

procedure TDNApplication.LoadDesktop(var S: TStream);
  begin
  _TDNApplication^.LoadDesktop(_Model1.TStream(S), @Self);
  end;

procedure TDNApplication.StoreDesktop(var S: TStream);
  begin
  _TDNApplication^.StoreDesktop(_Model1.TStream(S), @Self);
  end;

procedure TDNApplication.ChgColors;
  begin
  _TDNApplication^.ChgColors(@Self);
  end;

end.
