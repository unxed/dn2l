unit TopView;

interface

uses
  Views, Streams, Drivers
  ;

type
  PTopView = ^TTopView;
  {`2 Базовый тип для текста, выводимого в заголовке панели. }
  TTopView = object(TView)
    Panel: PView; //фактически -  PFilePanel
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); virtual;
    procedure Draw; virtual;
      {` Текст центрируется, не перекрывая элемент управления окна
      менеджера `}
    function GetPalette: PPalette; virtual;
    function GetText(MaxWidth: Integer): String; virtual;
      {` Этот метод обязательно должен быть перекрыт `}
    end;
  {`}

  PSortView = ^TSortView;
    {`2 Индикация текущей сортировки панели буковкой в левом верхнем углу `}
  TSortView = object(TView)
    Panel: PView; //фактически -  PFilePanel;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); virtual;
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

implementation

uses
  Defines, DblWnd, Advance1, flpanelx, Commands, dnApp, PDSetup
  ;

const
  CTopView = #11#12;

constructor TTopView.Load(var S: TStream);
  begin
  inherited Load(S);
  GetPeerViewPtr(S, Panel);
  end;

procedure TTopView.Store(var S: TStream);
  begin
  inherited Store(S);
  PutPeerViewPtr(S, Panel);
  end;

function TTopView.GetPalette: PPalette;
  const
    S: String[Length(CTopView)] = CTopView;
  begin
  GetPalette := @S;
  end;

function TTopView.GetText(MaxWidth: Integer): String;
  begin
  end;

procedure TTopView.Draw;
  var
    C: Word;
    B: TDrawBuffer;
    S: String;
    R, OldR: TRect;
    D: Integer;
    Width: Integer;
    Right: Boolean;
  begin
  Right := PDoubleWindow(Owner)^.Panel[pRight].AnyPanel = Panel;
  Width := Panel^.Size.X - 4 - Ord(Right);
    {4 - это ширина элемента управления (номера окна в левой панели
     и кнопки максимизации в правой панели. Для правой панели ещё
     один символ - это пробел между индикатором сортировки и TopView }
  if Width < 1 then
    Exit;
  S := GetText(Width);
  if Length(S) < Width - 2 then
    S := ' ' + S + ' ';
  R.A := Panel^.Origin;
  R.B.Y := R.A.Y;
  Dec(R.A.Y);
  D := (Width - Length(S) + 4) div 2;
  if D >= 4 then
    Inc(R.A.X, D) { пока можно, центрируем без учёта асимметрии }
  else if Right then { правая панель, прижимаем к кнопке максимизации }
    inc(R.A.X, Width - Length(S) + 1)
  else { левая панель, прижимаем к номеру окна }
    inc(R.A.X, 4);
  R.B.X := R.A.X + Length(S);
  GetBounds(OldR);
  if not MemEqual(R, OldR, SizeOf(R)) then
    begin
    Locate(R); { Тут будет рекурсия, поэтому второй раз рисовать не надо }
    Exit;
    end;
  C := GetColor(1);
  if not Panel^.GetState(sfSelected) then
    C := GetColor(2);
  MoveStr(B[0], S, C);
  WriteLine(0, 0, Size.X, Size.Y, B);
  end { TTopView.Draw };

{ ---------------------------- TSortView ------------------------------ }

constructor TSortView.Load(var S: TStream);
  begin
  inherited Load(S);
  GetPeerViewPtr(S, Panel);
  end;

procedure TSortView.Store(var S: TStream);
  begin
  inherited Store(S);
  PutPeerViewPtr(S, Panel);
  end;

procedure TSortView.Draw;
  var
    B: Word;
    C: Char;
    R: TRect;
    SortSetup: ^TPanelSortSetup;
  begin
  if (Size.X <> 1) or (Size.Y <> 1) then
    begin
    GetBounds(R);
    R.B.X := R.A.X;
    Dec(R.A.X);
    R.B.Y := R.A.Y + 1;
    Locate(R); // тут будет рекурсия, которая и нарисует
    Exit;
    end;
  SortSetup := @PFilePanelRoot(Panel)^.PanSetup^.Sort;
  C := GetString(dlSortTag)[SortSetup^.SortMode + 1];
  if (SortSetup^.SortFlags and psfInverted) <> 0  then
    C := Upcase(C);
  MoveChar(B, C, Panel^.Owner^.GetColor(3), 1);
  WriteLine(0, 0, 1, 1, B);
  end;

procedure TSortView.HandleEvent(var Event: TEvent);
  begin
  if Event.What = evMouseDown then
    begin
    ClearEvent(Event);
    Message(Panel, evCommand, {cmPanelSortSetup}cmSortBy, nil);
    end;
  end;

end.
