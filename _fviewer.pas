unit _FViewer;
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
  PViewScroll = ^TViewScroll;
  TViewScroll = object(TView)
    MaxV, Value: LongInt;
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function GetPartCode: LongInt;
    {procedure Draw; virtual;}
    function GetSize: Integer;
    procedure DrawPos(Pos: Integer);
    end;

  PFileViewer = ^TFileViewer;
  TFileViewer = object(TView)
    OldSizeX: Integer;
    Filtr: Boolean;
    NoEdit: Boolean;
    FileName: String;
    VFileName: String;
    Buf: PByteArray;
    Fl: PStream;
    UpdateViewTmr: TEventTimer;
    XDelta, ViewMode, HexPos: AInt;
    SearchActive: Boolean;
    SearchResultVisible: Boolean;
    SearchX: LongInt;
    SB: PView;
    Wrap: Byte;
    Lines: array[0..200] of
    record
      Pos: LongInt;
      len: Word;
      end;
    FilePos, FileSize, NumLines: LongInt;
    ExposedPos, ExposedLine: LongInt;
    Cur: TPoint;
    Info: PView;
    BufPos: LongInt;
    BufSize, MaxLines: LongInt;
    BufLines: AInt;
    KillAfterUse, isValid, QuickView, Loaded, HexEdit, BufModified:
     Boolean;
    FakeKillAfterUse: Boolean;
    Filter: Byte;
    XLAT: TXlat;
    UseXLat: Boolean;
    XLatFile: PString;
    KeyMap: TKeyMap;
    MarkPos: TPosArray;
    CtrlK: Boolean;
    CtrlQ: Boolean;
    HiLite: Boolean;
    ScrollEOF: Boolean;
    HiLitePar: THighliteParams;
    constructor Init(var Bounds: TRect; AStream: PStream;
        const AFileName, AVFileName: String;
        ASB: PView; Quick, Hex: Boolean);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    {destructor Done; virtual;}
    procedure ShowView; virtual;
    procedure HideView; virtual;
    {procedure Draw; virtual;}
    {procedure SetXlatFile(const FName: String; DoRedraw: Boolean);}
    function ReadFile(const FName, VFName: String; NewStream: Boolean)
      : Boolean;
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function WriteModify: Boolean;
    procedure CountDown(ANumber: Integer); virtual;
    procedure CountUp(ANumber: Integer); virtual;
    procedure Seek(APos: LongInt);
    procedure MakeLines; virtual;
    procedure SaveToFile(FN: String);
    {function Valid(Command: Word): Boolean; virtual;}
    {procedure ChangeBounds(var Bounds: TRect); virtual;}
    {function  GetPalette: PPalette; virtual;}
    procedure DoHighlite(var B; const S: String; const Attr: String);
    {procedure Update; virtual;}
    procedure SeekEof;
    procedure SeekBof;
    function BreakOnStreamReadError: Boolean;
    end;

implementation

uses
  _DNFuncs
  ;

function TViewScroll.GetPartCode: LongInt;
  begin
  Result := _TViewScroll^.GetPartCode(@Self);
  end;

function TViewScroll.GetSize: Integer;
  begin
  Result := _TViewScroll^.GetSize(@Self);
  end;

procedure TViewScroll.DrawPos(Pos: Integer);
  begin
  _TViewScroll^.DrawPos(Pos, @Self);
  end;

constructor TFileViewer.Init(var Bounds: TRect; AStream: PStream;
    const AFileName, AVFileName: String;
    ASB: PView; Quick, Hex: Boolean);
  begin
  _TFileViewer^.Init(Bounds, _Model1.PStream(AStream), AFileName,
     AVFileName, _Model1.PView(ASB), Quick, Hex, nil, @Self);
  end;

constructor TFileViewer.Load(var S: TStream);
  begin
  _TFileViewer^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TFileViewer.Store(var S: TStream);
  begin
  _TFileViewer^.Store(_Model1.TStream(S), @Self);
  end;

procedure TFileViewer.ShowView;
  assembler; {&Frame-}
asm
end;

procedure TFileViewer.HideView;
  assembler; {&Frame-}
asm
end;

{procedure TFileViewer.SetXlatFile(const FName: String; DoRedraw: Boolean);
  begin
  _TFileViewer^.SetXlatFile(FName, DoRedraw, @Self);
  end;}

function TFileViewer.ReadFile(const FName, VFName: String;
     NewStream: Boolean): Boolean;
  begin
  Result := _TFileViewer^.ReadFile(FName, VFName, NewStream, @Self);
  end;

function TFileViewer.WriteModify: Boolean;
  begin
  Result := _TFileViewer^.WriteModify(@Self);
  end;

procedure TFileViewer.CountDown(ANumber: Integer);
  assembler; {&Frame-}
asm
end;

procedure TFileViewer.CountUp(ANumber: Integer);
  assembler; {&Frame-}
asm
end;

procedure TFileViewer.Seek(APos: LongInt);
  begin
  _TFileViewer^.Seek(APos, @Self);
  end;

procedure TFileViewer.MakeLines;
  assembler; {&Frame-}
asm
end;

procedure TFileViewer.SaveToFile(FN: String);
  begin
  _TFileViewer^.SaveToFile(FN, @Self);
  end;

procedure TFileViewer.DoHighlite(var B; const S: String;
     const Attr: String);
  begin
  _TFileViewer^.DoHighlite(B, S, Attr, @Self);
  end;

procedure TFileViewer.SeekEof;
  begin
  _TFileViewer^.SeekEof(@Self);
  end;

procedure TFileViewer.SeekBof;
  begin
  _TFileViewer^.SeekBof(@Self);
  end;

function TFileViewer.BreakOnStreamReadError: Boolean;
  begin
  Result := _TFileViewer^.BreakOnStreamReadError(@Self);
  end;

end.
