unit Objects2;

{$I STDEFINE.INC}

interface

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

end.
