{$MODE DELPHI}
unit dnctrls;

// by unxed
// taken from DN TV as it is the code by DN crew

// some controls not defined in FV

interface

uses
    Objects, Menus, Drivers, advance1,

  Defines, Streams,
  Scroller, Validate,
  ASCIITab, HistList, Advance, Advance2,
  xTime, DNApp, Objects2
  , VpSysLow, DnIni, Advance6, UKeyMap,

    // replacement for
    //vars,

    Startup, Commands, views, dialogs
    ;

const
  CComboBox = #35#36;

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
  PLongInputline = ^TLongInputLine;
  {`2 Аналог TInputLine, но GetData и SetData работают с AnsiString.
    Внимание! Load и Store, как и раньше, работают с 1-байтной длиной!.
      TLongInputLine не должен иметь истории, так как это, во-первых,
    приведёт к ошибке в SetData при извлечени из истории, и, во-вторых,
    не имеет смысла, так как история работает с короткими строками, то
    tcnm полноценной истории для TLongInputLine быть не может, пока
    история не будет переведена на AnsiString. }
  TLongInputLine = object(TInputLine)
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    end;
    {`}

  PMyScrollBar = ^TMyScrollBar;
  TMyScrollBar = object(TScrollBar)
    procedure Draw; virtual;
  private
    procedure DrawPos(Pos: LongInt);
    end;

  PComboBox = ^TComboBox;
  {`2 Комбобкс с выбором только из списка, без возможности редактирования.
        Допускается до 10 вариантов. Они соответствуют командам
    1601..1610 строго согласно позиции в выпадающем меню.
        Способа задизейблить сами команды команд нет, но пункты меню
    можно сделать неактивными непосредственно: установить бит miDisabled
    в соответствующих TMenuItem.Flags) }
  TComboBox = object(TView)
    Selected: Word; // текущий номер варианта (нумерация от 1)
    Count: Word; { не отрывать от Selected! См. Load,Store}
    Menu: PMenu;
    Items: array[1..10] of PMenuItem; // прямые ссылки в меню
    constructor Init(var Bounds: TRect; AStrings: PSItem);
    procedure BuildMenu(AStrings: PSItem);
    destructor Done; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function DataSize: DWord; virtual;
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    end;
  {`}

function TPoint_EqualsXY(Point: TPoint; AX, AY: LongInt): boolean;

procedure TStream_ReadStrV(var Stream: TStream; var S: String);
function TStream_Eof(var S: TStream): Boolean;

implementation

procedure TLongInputLine.GetData(var Rec);
  begin
  (*
  // fixme: commented by unxed
  if (Data = '') or (Validator = nil) or
      (Validator^.Transfer(Data, @Rec, vtGetData) = 0)
  then
    AnsiString(Rec) := Copy(Data, 1, MaxLen);
    *)
  end;

procedure TLongInputLine.SetData(var Rec);
  begin
  (*
  // fixme: commented by unxed
  if  (Validator = nil) or
      (Validator^.Transfer(Data, @Rec, vtSetData) = 0)
  then
    Data := AnsiString(Rec);
  Message(Owner, evBroadcast, cmUpdateHexViews, @Self);
  SelectAll(True);
  *)
  end;

procedure TMyScrollBar.Draw;
  var
    chrs: TScrollChars;
  begin
  // fixme: commented by unxed
  {
  if Startup.FMSetup.Show and fmsShowScrollBar <> 0 then
    DrawPos(GetPos)
  else
    begin
    chrs := Chars;
    Chars := #186#186#186#186#186;
    DrawPos(GetPos);
    Chars := chrs;
    end;
    }
  end;

procedure TMyScrollBar.DrawPos(Pos: LongInt);
  var
    S: LongInt;
    B: TDrawBuffer;
    col1, col2, col3: Byte;
  begin
  // fixme: commented by unxed
{
  S := GetSize-1;
  if Startup.FMSetup.Show and fmsShowScrollBar <> 0 then
    begin
    col1 := GetColor(1);
    col2 := GetColor(2);
    col3 := GetColor(3);
    end
  else
    begin
    col1 := GetColor(1);
    col2 := col1;
    col3 := col2;
    end;
  MoveChar(B[0], Chars[0], col2, 1);
  if Max = Min then
    MoveChar(B[1], Chars[4], col1, S-1)
  else
    begin
    MoveChar(B[1], Chars[2], col1, S-1);
    MoveChar(B[Pos], Chars[3], col3, 1);
    end;
  MoveChar(B[S], Chars[1], col2, 1);
  WriteBuf(0, 0, Size.X, Size.Y, B);
}  end { TMyScrollBar.DrawPos };


function TPoint_EqualsXY(Point: TPoint; AX, AY: LongInt): boolean;
begin
    Result := (Point.X = AX) and (Point.Y = AY);
end;

procedure TStream_ReadStrV(var Stream: TStream; var S: String);
  var
    L: LongInt;
  begin
  L := 0;
  Stream.Read(L, 1);
  if L > 0 then
    begin
    SetLength(S, L);
    Stream.Read(S[1], L);
    end
  else
    S := '';
  end;

function TStream_Eof(var S: TStream): Boolean;
  begin
    TStream_Eof := (S.GetPos >= S.GetSize);
  end;

constructor TComboBox.Init(var Bounds: TRect; AStrings: PSItem);
  begin
  TView.Init(Bounds);
  Options := ofSelectable;
  BuildMenu(AStrings);
  Selected := 1;
  end;

procedure TComboBox.BuildMenu(AStrings: PSItem);
  var
    i: Integer;
    LastItem: PMenuItem;
    Tail: ^PMenuItem;
    PrevSItem: PSItem;
  begin
  Menu := NewMenu(nil);
  Tail := @Menu^.Items;
  Count := 0;
  while AStrings <> nil do
    begin
    Inc(Count);
    LastItem := NewItem(
      CenterStr(Copy(AStrings^.Value^, 1, Size.X-2), Size.X-2),
      '',  kbNoKey, 1600+i, 0, nil);
    Items[Count] := LastItem;
    Tail^ := LastItem;
    Tail := @LastItem.Next;
    DisposeStr(AStrings^.Value);
    PrevSItem := AStrings;
    AStrings := AStrings^.Next;
    Dispose(PrevSItem);
    end;
  end;

destructor TComboBox.Done;
  begin
  DisposeMenu(Menu);
  TView.Done;
  end;

procedure TComboBox.SetState(AState: Word; Enable: Boolean);
  begin
  TView.SetState(AState, Enable);
  DrawView;
  end;

function TComboBox.GetPalette: PPalette;
  const
    P: String[Length(CComboBox)] = CComboBox;
  begin
  GetPalette := @P;
  end;

procedure TComboBox.Draw;
  var
    B: TDrawBuffer;
    C: Byte;
    I: Word;
  begin
  I := 1 + 1*Ord(State and sfFocused <> 0);
  C := GetColor(I);
  MoveChar(B[0], '[', C, 1);
  MoveChar(B[1], ' ', C, Size.x-2);
  MoveStr(B[1], Items[Selected]^.Name^, C);
  MoveChar(B[Size.x-1], ']', C, Size.x);
  WriteLine(0, 0, Size.X, Size.Y, B);
  end;


procedure TComboBox.HandleEvent(var Event: TEvent);

  procedure OpenList;
    var
      R: TRect;
      MB: PMenuBox;
      C: Word;
    begin
{ Меню-список открываем поверх строки, совмещая строку
и соответствующий пункт меню. Меню вставляем в приложение, так
как если его вставлять в диалог, то в некоторых палитрах цвета
получаются очень странные.
}
    R.Assign(-2,-Selected,0,0);
    MakeGlobal(R.A, R.A);
    if R.A.Y < 0 then
      begin
      Dec(R.B.Y, R.A.Y);
      R.A.Y := 0;
      end;
    New(MB, Init(R, Menu, nil));
    MB^.Menu^.Default := Items[Selected];
    // fixme: commented by unxed
    //MB^.ComboBoxPal := True;
    C := Application^.ExecView(MB);
    if C <> 0 then
      begin
      Selected := C-1600;
      Draw;
//      Owner^.SelectNext(False);
      end;
    ClearEvent(Event);
    end;

  begin
  case Event.What of
    evMouseDown:
      begin
      Select; { Вместо TView.HandleEvent }
      OpenList;
      end;
    evKeyDown:
      case Event.KeyCode of
        kbSpace, kbPgDn:
          begin
          repeat
            Selected := Selected mod Count + 1;
          //until (Items[Selected]^.Flags and miDisabled) = 0;
          until (miDisabled) = 0; // fixme: flags not supported by fv. disabled by unxed
          ClearEvent(Event);
          Draw;
          end;
        kbPgUp:
          begin
          repeat
            if Selected <= 1
              then Selected := Count
                else Selected := Selected - 1;
          //until (Items[Selected]^.Flags and miDisabled) = 0;
          until (miDisabled) = 0; // fixme: flags not supported by fv. disabled by unxed
          ClearEvent(Event);
          Draw;
          end;

(*
        kbDown, kbUp: { протез навигации стрелками }
          begin
          PGroup(Owner)^.SelectNext(Event.KeyCode = kbUp);
          ClearEvent(Event);
          end;
*)
        kbAltDown, kbCtrlDown:
          OpenList;
        else
          if (Event.KeyCode and $FF0000 = 0) and (Event.CharCode > ' ')
          then
            OpenList;
      end {case};
  end {case};
//  inherited HandleEvent делать больше нечего }
  end { TComboBox.HandleEvent };

function TComboBox.DataSize: DWord;
  begin
  DataSize := SizeOf(Word);
  end;

procedure TComboBox.GetData(var Rec);
  begin
  Word(Rec) := Selected-1; // совместимость с TRadioButtons
  end;

procedure TComboBox.SetData(var Rec);
  begin
  Selected := Word(Rec)+1; // совместимость с TRadioButtons
  DrawView;
  end;

constructor TComboBox.Load(var S: TStream);
  var
    i: Integer;
    PLastItem: ^PMenuItem;
    LastItem: PMenuItem;
    P: PShortString; // fixme: string -> shortstr by unxed
  begin
  TView.Load(S);
  S.Read(Selected, 2*SizeOf(Word)); // включая Count
  Menu := NewMenu(nil);
  PLastItem := @Menu^.Items;
  for i := 1 to Count do
    begin
    P := S.ReadStr;
    LastItem := NewItem(Copy(P^, 1, Size.X-2),
      '',  kbNoKey, 1600+i, 0, nil);
    DisposeStr(P);
    PLastItem^ := LastItem;
    Items[i] := LastItem;
    PLastItem := @PLastItem^.Next;
    end;
  Selected := 1;
  end;

procedure TComboBox.Store(var S: TStream);
  var
    i: Integer;
  begin
  TView.Store(S);
  S.Write(Selected, 2*SizeOf(Selected)); // включая Count
  for i := 1 to Count do
    S.WriteStr(Items[i]^.Name);
  end;

end.
