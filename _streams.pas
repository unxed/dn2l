unit _Streams;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Objects
  ;

type
  PStream = ^TStream;
  TStream = object(TObject)
    Status: Integer;
    ErrorInfo: Integer;
    StreamSize: LongInt;
    Position: LongInt;
    constructor Init;
    procedure CopyFrom(var S: TStream; Count: LongInt);
    procedure Error(Code, Info: Integer); virtual;
    procedure Flush; virtual;
    function Get: PObject;
    function GetPos: LongInt; virtual;
    function GetSize: LongInt; virtual;
    procedure Put(P: PObject);
    procedure Read(var Buf; Count: LongInt); virtual;
    function ReadStr: PString;
    function ReadLongStr: PLongString;
    procedure ReadStrV(var S: String);
    procedure ReadLongStrV(var S: LongString);
    procedure Reset;
    procedure Seek(Pos: LongInt); virtual;
    function StrRead: PChar;
    procedure StrWrite(P: PChar);
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: LongInt); virtual;
    procedure WriteStr(P: PString);
    procedure WriteLongStr(P: PLongString);
    function Eof: Boolean;
    procedure DoOpen(OpenMode: Word); virtual;
    procedure Close; virtual;
    end;

  PDosStream = ^TDosStream;
  TDOSStream = object(TStream)
    Handle: Integer;
    FName: AsciiZ;
    FMode: Word;
    constructor Init(FileName: String; Mode: Word);
    procedure Open(FileName: String; Mode: Word);
    {destructor Done; virtual;}
    {procedure Read(var Buf; Count: LongInt); virtual;}
    procedure ReadBlock(var Buf; Count: LongInt; var BytesRead: Word);
     virtual;
    {procedure Seek(Pos: LongInt); virtual;}
    {procedure Truncate; virtual;}
    {procedure Write(const Buf; Count: LongInt); virtual;}
    {procedure DoOpen(OpenMode: Word); virtual;}
    {procedure Close; virtual;}
    end;

  PBufStream = ^TBufStream;
  TBufStream = object(TDOSStream)
    Buffer: PByteArray;
    BufSize: LongInt;
    BufPtr: LongInt;
    BufEnd: LongInt;
    ModBufStart, ModBufEnd: LongInt;
    constructor Init(FileName: String; Mode: Word; Size: LongInt);
    {destructor Done; virtual;}
    {procedure Flush; virtual;}
    {procedure Read(var Buf; Count: LongInt); virtual;}
    {procedure Seek(Pos: LongInt); virtual;}
    {procedure Truncate; virtual;}
    {procedure Write(const Buf; Count: LongInt); virtual;}
    {procedure DoOpen(OpenMode: Word); virtual;}
    {procedure Close; virtual;}
    end;

  PMemoryStream = ^TMemoryStream;
  TMemoryStream = object(TStream)
    BlkCount: LongInt;
    BlkSize: Word;
    MemSize: LongInt;
    BlkList: PPointerArray;
    constructor Init(ALimit: LongInt; ABlockSize: Word);
    {destructor Done; virtual;}
    {procedure Read(var Buf; Count: LongInt); virtual;}
    {procedure Truncate; virtual;}
    {procedure Write(const Buf; Count: LongInt); virtual;}
    end;

implementation

uses
  _DNFuncs
  ;

constructor TStream.Init;
  begin
  _TObject^.Init(nil, @Self);
  end;

procedure TStream.CopyFrom(var S: TStream; Count: LongInt);
  begin
  _TStream^.CopyFrom(_Model1.TStream(S), Count, @Self);
  end;

procedure TStream.Error(Code, Info: Integer);
  assembler; {&Frame-}
asm
end;

procedure TStream.Flush;
  assembler; {&Frame-}
asm
end;

function TStream.Get: PObject;
  begin
  Result := PObject(_TStream^.Get(@Self));
  end;

function TStream.GetPos: LongInt;
  assembler; {&Frame-}
asm
end;

function TStream.GetSize: LongInt;
  assembler; {&Frame-}
asm
end;

procedure TStream.Put(P: PObject);
  begin
  _TStream^.Put(_Model1.PObject(P), @Self);
  end;

procedure TStream.Read(var Buf; Count: LongInt);
  assembler; {&Frame-}
asm
end;

function TStream.ReadStr: PString;
  begin
  Result := _TStream^.ReadStr(@Self);
  end;

function TStream.ReadLongStr: PLongString;
  begin
  Result := _TStream^.ReadLongStr(@Self);
  end;

procedure TStream.ReadStrV(var S: String);
  begin
  _TStream^.ReadStrV(S, @Self);
  end;

procedure TStream.ReadLongStrV(var S: LongString);
  begin
  _TStream^.ReadLongStrV(S, @Self);
  end;

procedure TStream.Reset;
  begin
  _TStream^.Reset(@Self);
  end;

procedure TStream.Seek(Pos: LongInt);
  assembler; {&Frame-}
asm
end;

function TStream.StrRead: PChar;
  begin
  Result := _TStream^.StrRead(@Self);
  end;

procedure TStream.StrWrite(P: PChar);
  begin
  _TStream^.StrWrite(P, @Self);
  end;

procedure TStream.Truncate;
  assembler; {&Frame-}
asm
end;

procedure TStream.Write(const Buf; Count: LongInt);
  assembler; {&Frame-}
asm
end;

procedure TStream.WriteStr(P: PString);
  begin
  _TStream^.WriteStr(P, @Self);
  end;

procedure TStream.WriteLongStr(P: PLongString);
  begin
  _TStream^.WriteLongStr(P, @Self);
  end;

function TStream.Eof: Boolean;
  begin
  Result := _TStream^.Eof(@Self);
  end;

procedure TStream.DoOpen(OpenMode: Word);
  assembler; {&Frame-}
asm
end;

procedure TStream.Close;
  assembler; {&Frame-}
asm
end;

constructor TDOSStream.Init(FileName: String; Mode: Word);
  begin
  _TDosStream^.Init(FileName, Mode, nil, @Self);
  end;

procedure TDOSStream.Open(FileName: String; Mode: Word);
  begin
  _TDosStream^.Open(FileName, Mode, @Self);
  end;

procedure TDOSStream.ReadBlock(var Buf; Count: LongInt;
     var BytesRead: Word);
  assembler; {&Frame-}
asm
end;

constructor TBufStream.Init(FileName: String; Mode: Word; Size: LongInt);
  begin
  _TBufStream^.Init(FileName, Mode, Size, nil, @Self);
  end;

constructor TMemoryStream.Init(ALimit: LongInt; ABlockSize: Word);
  begin
  _TMemoryStream^.Init(ALimit, ABlockSize, nil, @Self);
  end;

end.
