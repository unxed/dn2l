unit _Collect;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Objects, _Streams
  ;

type
  PCollection = ^TCollection;
  TCollection = object(TObject)
    Items: Pointer {PItemList};
    Count: LongInt;
    Limit: LongInt;
    Delta: LongInt;
    Status, ErrorInfo: Integer;
    constructor Init(ALimit, ADelta: LongInt);
    constructor Load(var S: TStream);
    {destructor Done; virtual;}
    function At(Index: LongInt): Pointer;
    procedure AtDelete(Index: LongInt);
    procedure AtFree(Index: LongInt);
    procedure AtInsert(Index: LongInt; Item: Pointer);
    procedure AtPut(Index: LongInt; Item: Pointer);
    procedure AtReplace(Index: LongInt; Item: Pointer);
    procedure Delete(Item: Pointer);
    procedure DeleteAll;
    procedure Error(Code, Info: Integer); virtual;
    function FirstThat(Test: Pointer): Pointer;
    procedure ForEach(Action: Pointer);
    procedure Free(Item: Pointer);
    procedure FreeAll;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    function IndexOf(Item: Pointer): LongInt; virtual;
    procedure Insert(Item: Pointer); virtual;
    function LastThat(Test: Pointer): Pointer;
    procedure Pack;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    procedure SetLimit(ALimit: LongInt); virtual;
    procedure Store(var S: TStream);
    end;

  PSortedCollection = ^TSortedCollection;
  TSortedCollection = object(TCollection)
    Duplicates: Boolean;
    constructor Init(ALimit, ADelta: LongInt);
    constructor Load(var S: TStream);
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    {function IndexOf(Item: Pointer): LongInt; virtual;}
    {procedure Insert(Item: Pointer); virtual;}
    function KeyOf(Item: Pointer): Pointer; virtual;
    function Search(Key: Pointer; var Index: LongInt): Boolean; virtual;
    procedure Store(var S: TStream);
    procedure Sort;
    end;

  PLineCollection = ^TLineCollection;
  TLineCollection = object(TCollection)
    LongStrings: Boolean;
    constructor Init(ALimit, ADelta: LongInt; ALongStrings: Boolean);
    {procedure FreeItem(P: Pointer); virtual;}
    {procedure PutItem(var S: TStream; Item: Pointer); virtual;}
    {function GetItem(var S: TStream): Pointer; virtual;}
    end;

  PStringCollection = ^TStringCollection;
  TStringCollection = object(TSortedCollection)
    LongStrings: Boolean;
    constructor Init(ALimit, ADelta: LongInt; ALongStrings: Boolean);
    {function Compare(Key1, Key2: Pointer): Integer; virtual;}
    {procedure FreeItem(Item: Pointer); virtual;}
    {function GetItem(var S: TStream): Pointer; virtual;}
    {procedure PutItem(var S: TStream; Item: Pointer); virtual;}
    end;

  PStrCollection = ^TStrCollection;
  TStrCollection = object(TSortedCollection)
    constructor Init(ALimit, ADelta: LongInt);
    {function Compare(Key1, Key2: Pointer): Integer; virtual;}
    {procedure FreeItem(Item: Pointer); virtual;}
    {function GetItem(var S: TStream): Pointer; virtual;}
    {procedure PutItem(var S: TStream; Item: Pointer); virtual;}
    end;

  PFilesCollection = ^TFilesCollection;
  TFilesCollection = object(TSortedCollection)
    SortMode: Byte;
    Selected: LongInt;
    Panel: Pointer;
    {$IFDEF WIN32}
    LFNActive: Boolean;
    {$ENDIF}
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); virtual;
    {procedure FreeItem(Item: Pointer); virtual;}
    {function Compare(Key1, Key2: Pointer): Integer; virtual;}
    end;

implementation

uses
  _DNFuncs
  ;

constructor TCollection.Init(ALimit, ADelta: LongInt);
  begin
  _TCollection^.Init(ALimit, ADelta, nil, @Self);
  end;

constructor TCollection.Load(var S: TStream);
  begin
  _TCollection^.Load(_Model1.TStream(S), nil, @Self);
  end;

function TCollection.At(Index: LongInt): Pointer;
  begin
  Result := _TCollection^.At(Index, @Self);
  end;

procedure TCollection.AtDelete(Index: LongInt);
  begin
  _TCollection^.AtDelete(Index, @Self);
  end;

procedure TCollection.AtFree(Index: LongInt);
  begin
  _TCollection^.AtFree(Index, @Self);
  end;

procedure TCollection.AtInsert(Index: LongInt; Item: Pointer);
  begin
  _TCollection^.AtInsert(Index, Item, @Self);
  end;

procedure TCollection.AtPut(Index: LongInt; Item: Pointer);
  begin
  _TCollection^.AtPut(Index, Item, @Self);
  end;

procedure TCollection.AtReplace(Index: LongInt; Item: Pointer);
  begin
  _TCollection^.AtReplace(Index, Item, @Self);
  end;

procedure TCollection.Delete(Item: Pointer);
  begin
  _TCollection^.Delete(Item, @Self);
  end;

procedure TCollection.DeleteAll;
  begin
  _TCollection^.DeleteAll(@Self);
  end;

procedure TCollection.Error(Code, Info: Integer);
  assembler; {&Frame-}
asm
end;

function TCollection.FirstThat(Test: Pointer): Pointer;
  begin
  Result := _TCollection^.FirstThat(Test, @Self);
  end;

procedure TCollection.ForEach(Action: Pointer);
  begin
  _TCollection^.ForEach(Action, @Self);
  end;

procedure TCollection.Free(Item: Pointer);
  begin
  _TCollection^.Free(Item, @Self);
  end;

procedure TCollection.FreeAll;
  begin
  _TCollection^.FreeAll(@Self);
  end;

procedure TCollection.FreeItem(Item: Pointer);
  assembler; {&Frame-}
asm
end;

function TCollection.GetItem(var S: TStream): Pointer;
  assembler; {&Frame-}
asm
end;

function TCollection.IndexOf(Item: Pointer): LongInt;
  assembler; {&Frame-}
asm
end;

procedure TCollection.Insert(Item: Pointer);
  assembler; {&Frame-}
asm
end;

function TCollection.LastThat(Test: Pointer): Pointer;
  begin
  Result := _TCollection^.LastThat(Test, @Self);
  end;

procedure TCollection.Pack;
  begin
  _TCollection^.Pack(@Self);
  end;

procedure TCollection.PutItem(var S: TStream; Item: Pointer);
  assembler; {&Frame-}
asm
end;

procedure TCollection.SetLimit(ALimit: LongInt);
  assembler; {&Frame-}
asm
end;

procedure TCollection.Store(var S: TStream);
  begin
  _TCollection^.Store(_Model1.TStream(S), @Self);
  end;

constructor TSortedCollection.Init(ALimit, ADelta: LongInt);
  begin
  _TSortedCollection^.Init(ALimit, ADelta, nil, @Self);
  end;

constructor TSortedCollection.Load(var S: TStream);
  begin
  _TSortedCollection^.Load(_Model1.TStream(S), nil, @Self);
  end;

function TSortedCollection.Compare(Key1, Key2: Pointer): Integer;
  assembler; {&Frame-}
asm
end;

function TSortedCollection.KeyOf(Item: Pointer): Pointer;
  assembler; {&Frame-}
asm
end;

function TSortedCollection.Search(Key: Pointer; var Index: LongInt)
  : Boolean;
  assembler; {&Frame-}
asm
end;

procedure TSortedCollection.Store(var S: TStream);
  begin
  _TSortedCollection^.Store(_Model1.TStream(S), @Self);
  end;

procedure TSortedCollection.Sort;
  begin
  _TSortedCollection^.Sort(@Self);
  end;

constructor TLineCollection.Init(ALimit, ADelta: LongInt;
     ALongStrings: Boolean);
  begin
  _TLineCollection^.Init(ALimit, ADelta, ALongStrings, nil, @Self);
  end;

constructor TStringCollection.Init(ALimit, ADelta: LongInt;
     ALongStrings: Boolean);
  begin
  _TStringCollection^.Init(ALimit, ADelta, ALongStrings, nil, @Self);
  end;

constructor TStrCollection.Init(ALimit, ADelta: LongInt);
  begin
  _TStrCollection^.Init(ALimit, ADelta, nil, @Self);
  end;

constructor TFilesCollection.Load(var S: TStream);
  begin
  _TFilesCollection^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TFilesCollection.Store(var S: TStream);
  assembler; {&Frame-}
asm
end;

end.
