unit Objects2;

{$I STDEFINE.INC}

interface

uses objects, defines, advance1;

const
  vmtHeaderSize = 12;

type
  PEmptyObject = ^TEmptyObject;
  TEmptyObject = object
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    constructor Init;
    procedure Free;
    destructor Done; virtual;
    end;

  PObject = ^TObject;
  TObject = object(TEmptyObject)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    ObjectIsInited: Boolean;
    constructor Init;
    destructor Done; virtual;
    end;

procedure FreeObject(var O);

procedure ObjChangeType(P: PObject; NewType: Pointer);
  {` Подмена типа экземпляра объекта. В качестве NewType надо
  указывать результат TypeOf(новый тип). Новый тип, естественно,
  должен быть совместим по полям с типом, который имел объект
  при рождении.
     Основное назначение - подмена виртуальных методов, то есть
  изменение поведения без исзменения данных.
  `}

  { TLineCollection }

// by unxed
// taken from DN TV as it is the code by DN crew
type
  PLineCollection = ^TLineCollection;
  TLineCollection = object(TCollection)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    LongStrings: Boolean; {Cat}
    constructor Init(ALimit, ADelta: LongInt; ALongStrings: Boolean);
    {Cat}
    procedure FreeItem(P: Pointer); virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    end;

implementation

const
  LinkSize = SizeOf(TEmptyObject);

constructor TEmptyObject.Init;
  begin
  FillChar(Pointer(LongInt(@Self)+LinkSize)^, SizeOf(Self)-LinkSize, #0)
  ; { Clear data fields }
  end;

procedure TEmptyObject.Free;
  begin
  if @Self <> nil then
    Dispose(PEmptyObject(@Self), Done);
  end;

destructor TEmptyObject.Done;
  begin
  end;

constructor TObject.Init;
  begin
  inherited Init;
  ObjectIsInited := True;
  end;

destructor TObject.Done;
  begin
  ObjectIsInited := False;
  end;

procedure FreeObject(var O);
  var
    OO: PObject absolute O;
  begin
  if OO <> nil then
    begin
    Dispose(OO, Done);
    OO := nil;
    end;
  end;

procedure ObjChangeType(P: PObject; NewType: Pointer);
  begin
  PWord(P)^ := Word(NewType);
  end;

// by unxed
// taken from DN TV as it is the code by DN crew

function TStream_ReadLongStr(S: TStream): PLongString;
  var
    L: LongInt;
    P: PLongString;
  begin
  S.Read(L, SizeOf(L));
  if L > 0 then
    if L > MaxLongStringLength then
      begin
      New(P);
      SetLength(P^, MaxLongStringLength);
      S.Read(P^[1], MaxLongStringLength);
      S.Seek(S.Position-MaxLongStringLength+L);
      TStream_ReadLongStr := P;
      end
    else
      begin
      New(P);
      SetLength(P^, L);
      S.Read(P^[1], L);
      TStream_ReadLongStr := P;
      end
  else
    TStream_ReadLongStr := nil;
  end { TStream.ReadLongStr: };

procedure TStream_WriteLongStr(S: TStream; P: PLongString);
  var
    L: LongInt;
  begin
  if P <> nil then
    begin
    L := Length(P^);
    S.Write(L, 4);
    S.Write(P^[1], L);
    end
  else
    begin
    L := 0;
    S.Write(L, SizeOf(L));
    end
  end;

{ TLineCollection }
{Cat: добавил возможность хранить в коллекции длинные строки}
constructor TLineCollection.Init(ALimit, ADelta: LongInt;
     ALongStrings: Boolean);
  begin
  inherited Init(ALimit, ADelta);
  LongStrings := ALongStrings;
  end;

procedure TLineCollection.FreeItem(P: Pointer);
  begin
  if LongStrings then
    DisposeLongStr(PLongString(P))
  else
    DisposeStr(PString(P));
  end;

procedure TLineCollection.PutItem;
  begin
  if LongStrings then
    TStream_WriteLongStr(S, Item)
    //S.WriteLongStr(Item)
  else
    S.WriteStr(Item);
  end;

function TLineCollection.GetItem;
  begin
  if LongStrings then
    GetItem := TStream_ReadLongStr(S)
  else
    GetItem := S.ReadStr;
  end;

end.
