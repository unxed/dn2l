{$MODE DELPHI}
unit Hash;

interface
uses
  Objects, Objects2;

type
  THashIndex = Longint;
  THashTable = array[0..0] of THashIndex;

  PHash = ^THash;
  {`2 Хеш-таблица, добавочная к коллекции. Создаётся после
  заполнения коллекции, так как требует знания окончательного
  числа элементов. Используется для очень быстрого поиска
  элемента в коллекции, обычно несортированной.
     Для разрешения коллизий используется рехеширование,
  поэтому переполнение хеш-таблицы абсолютно недопустимо,
  а почти полное заполнение весьма нежелательно.
    Размер хеш-таблицы - степень 2, превышающая число элементов
  на 10% или более. Если при создании для такой таблицы не
  хватит памяти, после Init будет HT=nil.
  `}
  THash = object(TObject)
    HT: ^THashTable;
      {` Хеш-таблица.
      Содержит индексы в Items^ или EmptyIndex (свободные) `}
    Count: Integer;
      {` Число элементов HT`}
    Items: PItemList;
      {` Копия Items коллекции `}
    hf: integer;
      {` Индекс в HT, 0..Count-1 `}
    RehashStep: integer;
      {` Шаг рехеширования последнего поиска. Рехеширование
      делается прибавлением этого шага по модулю размена HT.
      Шаг должен быть взаимно прост с Count, то есть, поскольку
      Count - степень двойки, RehashStep должен быть нечётным `}
    procedure Hash(Item: Pointer); virtual;
      {` На основании содержимого Item^ вычисляется стартовый
      хеш-индекс hf и шаг рехеширования RehashStep.
      Этот метод обязательно надо перекрыть.`}
    function Equal(Item1, Item2: Pointer): Boolean; virtual;
      {` Совпадают ли ключи Item1^ и Item2^.
      Этот метод обязательно надо перекрыть. `}
    constructor Init(BaseColl: PCollection);
      {` резервирование памяти под HT^ и очистка HT `}
    function GetHashIndex(Item: Pointer; var N: THashIndex): Boolean;
      {` Поиск элемента в хеш-таблице.
      Если найден (результат True) N - индекс в HT.
      Если не найден  (результат False) N - индекс в HT, куда
      его надо записать. `}
    function AddItem(CollIndex: Integer): Boolean;
      {` Запись в хеш-таблицу нового элемента Items^[CollIndex]^.
      Результат False, если элемент c таким ключом уже есть `}
    destructor Done; virtual;
    end;

implementation

const
  EmptyIndex = $FFFFFFFF;

constructor THash.Init(BaseColl: PCollection);
  var
    Size: Longint;
    MinCount: Integer;
  begin
  inherited Init;
  MinCount := BaseColl^.Count * 11 div 10; // запас 10%
  Count := 1024;
  while Count < MinCount do
    Count := Count*2;
  Size := Count * SizeOf(Longint);
  GetMem(HT, Size);
  if HT <> nil then
    FillChar(HT^, Size, $FF);
  Items := BaseColl^.Items;
  end;

destructor THash.Done;
  begin
  if HT <> nil then
    FreeMem(HT);
  inherited Done;
  end;

procedure THash.Hash(Item: Pointer);
  begin
  RunError(211);
  end;

function THash.Equal(Item1, Item2: Pointer): Boolean;
  begin
  RunError(211);
  end;

function THash.GetHashIndex(Item: Pointer; var N: THashIndex): Boolean;
  begin
  Hash(Item);
  while True do
    begin
    N := HT^[hf];
    if N = EmptyIndex then
      begin
      Result := False;
      Break; { не нашли }
      end;
    if Equal(Item, Items^[N]) then
      begin
      Result := True;
      Break; { нашли }
      end;
    { рехеширование }
    hf := (hf + RehashStep) mod Count;
    end;
  { не нашли }
  end;

function THash.AddItem(CollIndex: Integer): Boolean;
  var
    N: THashIndex;
  begin
  Result := not GetHashIndex(Items^[CollIndex], N);
  if Result then
    HT^[hf] := CollIndex;
  end;

end.
