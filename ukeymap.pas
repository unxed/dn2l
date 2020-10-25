{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08
//  Based on Dos Navigator (C) 1991-99 RIT Research Labs
//
//  This programs is free for commercial and non-commercial use as long as
//  the following conditions are aheared to.
//
//  Copyright remains RIT Research Labs, and as such any Copyright notices
//  in the code are not to be removed. If this package is used in a
//  product, RIT Research Labs should be given attribution as the RIT Research
//  Labs of the parts of the library used. This can be in the form of a textual
//  message at program startup or in documentation (online or textual)
//  provided with the package.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//  1. Redistributions of source code must retain the copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. All advertising materials mentioning features or use of this software
//     must display the following acknowledgement:
//     "Based on Dos Navigator by RIT Research Labs."
//
//  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS
//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
//  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  The licence and distribution terms for any publically available
//  version or derivative of this code cannot be changed. i.e. this code
//  cannot simply be copied and put under another distribution licence
//  (including the GNU Public Licence).
//
//////////////////////////////////////////////////////////////////////////}

unit UKeyMap;
{<U_KeyMap.001>}

interface

uses
  Objects2, Defines, Advance1
  ;

type
  TKeyMap = (kmXlat, kmNone, kmAscii, kmAnsi,
    kmKoi8r, km5, km6, km7, km8, km9);
  {`kmXlat и kmNone должны быть перед kmAscii, а
  остальные - после kmAscii`}

type
  PCodeConv = ^TCodeConv;
  TCodeConv = record
    A, B: Char
    end;

var
  RollKeyMap: array[TKeyMap] of TKeyMap;
  {` Код следующий по кругу встроенной таблицы `}
  MaxKeyMap: TKeyMap;
  {` Максимальный номер установленной перекодировочной таблицы.`}

type
  {(c) SeYKo}
  TCodePageDetector = object(TObject)
    procedure CheckString(P1: PChar; len: Integer);
    function DetectedCodePage: TKeyMap;
  private
    Koi, KoiA, Win, WinA, Win2: LongInt;
    end;
  {(c) SeYKo}

const
  ToUpAscii = 0;
    {` Индекс для TXLatCP - перекодировка в ASCII c
    переводом на верхний регистр. Испоьзуется при регистронезависимом
    поиске. (0 = ord(False))`}
  ToAscii = 1;
    {` Индекс для TXLatCP - перекодировка в ASCII. (1 = ord(True))`}
  FromAscii = 2;
    {` Индекс для TXLatCP, перекодировка из ASCII`}

type
  PXlatCP = ^TXLatCP;
  TXlatCP = array[0..2] of TXLat;

  TKeyMapDescr = record
    {` Описатель кодировки}
    Tag: string[3];
      {` Обозначение для показа в рамке окна. Обязательно буквы
      только английские только на верхнем регистре!
      См. ProcessDefCodepage `}
    XlatCP: PXlatCP;
    end;
    {`}

var
  ToggleCaseArray: TXlat;
    {` Переворот регистра в ASCII, типа "cAPS lOCK" <-> "Caps Lock"`}
  LayoutConvXlat: TXLat;
    {` Исправление раскладки
  Для случая двух раскладок клавиатуры (например, 866 и 850)
  перекодирует каждый символ в такой, какой был бы введен с той
  же клавиши в другой раскладке.
  Определяется xlt-файлом, заданным ini-переменной KbdToggleLayout `}

  ABCSortXlat: TXLat;
    {' Таблица весов для алфавитной сортировки; без одинаковых весов '}

  DosXlatCP: TXLatCP absolute UpCaseArray;
  WinXlatCP: TXLatCP;

const
  KeyMapDescr: array[TKeyMap] of TKeyMapDescr =
   {`Встроенные кодировки `}
    ((Tag: 'DOS'; XlatCP: @DosXlatCP) //kmXlat
    ,(Tag: 'DOS'; XlatCP: @DosXlatCP) //kmNone
    ,(Tag: 'DOS'; XlatCP: @DosXlatCP) //kmAscii
    ,(Tag: 'WIN'; XlatCP: @WinXlatCP) //kmAnsi
    ,(Tag: ''; XlatCP: nil)
    ,(Tag: ''; XlatCP: nil)
    ,(Tag: ''; XlatCP: nil)
    ,(Tag: ''; XlatCP: nil)
    ,(Tag: ''; XlatCP: nil)
    ,(Tag: ''; XlatCP: nil)
    );

function ProcessDefCodepage(DefCodepageS: String): TKeyMap;

procedure NullXLAT(var X: TXlat);
  {` Заполнить отждественную перекодировку `}

procedure AcceptToAscii(var XLatCP: TXLatCP);
  {` В XLatCP на основе готовой таблицы [ToAscii] заполнить остальные `}

function ReadXlt(FN: string; var N: Integer): PCodeConv;
  {` Прочитать xlt-файл. Память под результат резервируется.
  N - длина прочитанного файла. Если прочитать не удалось,
  результат nil, а N = 0 `}

procedure ConvToXlat(Conv: PCodeConv; L: Integer; var Xlat: TXLat);
  {` Преобразование PCodeConv длиной L в Xlat. Начальное заполнение
  XLat должна сделать вызывающая программа. Conv освобождается.`}

function BuildCodeTable(const S: string; var XlatCP: TXLatCP): Boolean;
  {` Строится XlatCP для кодировки S. S может представлять собой
  либо число (тогда это номер кодовой страницы), либо имя
  xlt-файла. Если имя файла не содержит пути (распознаётся по '\'),
  то файл ищется в стандартном XLT-каталоге. Результат - успех. `}

function BuildABCSortXlat(const FN: string): Boolean;
  {` Строится ABCSortXlat для кодировки FN. FN может представлять собой
  либо число (тогда это номер кодовой страницы), либо имя
  xlt-файла. Если имя файла не содержит пути (распознаётся по '\'),
  то файл ищется в стандартном XLT-каталоге.
  Если FN='' ил ошибка, то ABCSortXlat - тождественная перекодировка.
  Результат - успех.
  !! Внимание: в настоящий момент (04.05.2005) реализован только
     xlt-вариант FN.
  `}

function BuildLayoutConvXlat(const FN: string): Boolean;
  {` Строится LayoutConvXlat для кодировки FN. FN может представлять собой
  либо число (тогда это номер кодовой страницы), либо имя
  xlt-файла. Если имя файла не содержит пути (распознаётся по '\'),
  то файл ищется в стандартном XLT-каталоге.
  Если FN='', то LayoutConvXlat - тождественная перекодировка.
  Результат - успех.
  `}

procedure XLatStr(var S: String; const XLat: TXLat);

procedure XLatBuf(var B; Len: Integer; const XTable: TXLat);

procedure FreeCodetables;
  {`Уничтожение всех таблиц, кроме ASCII. При построении таблиц
  необходимо соблюдать порядок вызова функций: FreeCodetables,
  BuildWinCodeTable, InitCodeTables. Только при таком порядке
  значение MaxKeyMap будет корректным, не будет оставаться мусора
  в куче и не будет адресации через nil`}

function BuildWinCodeTable(S: string): Boolean;
  {` Обработка виндовой кодовой страницы. S должен иметь имеет вид
  837 'win866r.xlt'. См. также FreeCodetables`}

function InitCodeTables(CodeTables: string): Boolean;
  {` Обработка ini-переменной CodeTables. Состоит из разделённых
  пробелами элементов вида KOI:837 или вида KOI:koi8-r.xlt `}

procedure OemToCharSt(var OemS: String); {JO}
  {` Перевод из DOS в WIN `}
procedure CharToOemSt(var CharS: String); {JO}
  {` Перевод из WIN в DOS `}
function OemToCharStr(const OemS: String): String;
  {` Перевод из DOS в WIN `}
function CharToOemStr(const CharS: String): String;
  {` Перевод из WIN в DOS `}

implementation
  uses
    Country_, advance, Streams;

procedure XLatBuf(var B; Len: Integer; const XTable: TXLat);
  assembler;
  {&Frame-} {$USES ESI, EBX, ECX, EDI}
asm
    cmp  Len, 0
    jle  @@1
    mov  ECX, Len
    mov  ESI, B
    mov  EBX, XTable
    xor  EAX, EAX
    cld
 @@Loop:
    lodsb
    mov  EDI, EAX
    mov  AL, [EBX+EDI]
    mov  [ESI-1], AL
    loop @@Loop
 @@1:
  end;

const
  WinSetA = [#224, #229, #232, #238, #243];
  KoiSetA = [#193, #197, #201, #207, #213];

procedure TCodePageDetector.CheckString(P1: PChar; len: Integer);
  var
    C: Char;
    P2: PChar;
  begin
  P2 := P1+len;
  while P1 < P2 do
    begin
    C := P1^;
    Inc(P1);
    if C >= #$C0 then
      if C <= #$DF then
        begin
        Inc(Koi);
        if C in KoiSetA then
          Inc(KoiA);
        end
      else
        begin
        Inc(Win);
        if C in WinSetA then
          Inc(WinA);
        if C >= #$F0 then
          Inc(Win2);
        end;
    end;
  end { TCodePageDetector.CheckString };

function TCodePageDetector.DetectedCodePage: TKeyMap;
  begin
  DetectedCodePage := kmAscii;
  if  (Koi <> 0) and (KoiA <> 0) and (Win <> 0) and
      (Win >= Koi div 500) and (Win <= Koi div 5) and
      (KoiA >= Koi div 5)
  then
    begin
    if KeyMapDescr[kmKoi8r].Tag = 'KOI' then
      DetectedCodePage := kmKoi8r;
    end
  else if (Win <> 0) and (WinA <> 0) and (Koi <> 0) and
      (Koi >= Win div 500) and (Koi <= Win div 5) and
      (WinA >= Win div 5) and (Win2 >= Win div 5)
  then
    DetectedCodePage := kmAnsi;
  end;
{(c) SeYKo}

function ProcessDefCodepage(DefCodepageS: String): TKeyMap;
  var
    i: Integer;
    k: TKeyMap;
  begin
{ Хочется написать UpStr(DefCodepageS), но лучше этого не делать, так
как это создаёт жуткую закрутку ссылок между модулями через advance1.
и, главное, работает с какой-то заранее неизвестной UpCaseArray.
Поскольку параметры в dn.ini все английские, делаем просто:}
  for i := 1 to Length(DefCodepageS) do
    DefCodepageS[i] := System.UpCase(DefCodepageS[i]);

  if DefCodepageS = 'AUTO' then
    Result := kmNone
  else
    begin
    Result := kmAscii; // на случай некорректного значения
    for k := Succ(kmAscii) to MaxKeyMap do
      if DefCodepageS = KeyMapDescr[k].Tag then
        begin
        Result := k;
        Exit;
        end;
    end;
  end;

procedure NullXLAT;
  var
    C: Char;
  begin
  for C := #0 to #255 do
    X[C] := C;
  end;

procedure XLatLongStr(const InStr: LongString; var OutStr: LongString;
   const XLat: TXLat);
  var
    i: Longint;
  begin
  SetLength(OutStr, Length(InStr));
  for i := 1 to Length(InStr) do
    OutStr[i] := XLat[InStr[i]];
  end;

function Ascii_Ansi(const S: LongString): LongString;
  begin
  XLatLongStr(S, Result, KeyMapDescr[kmAnsi].XlatCP^[FromAscii]);
  end;

function Ansi_Ascii(const S: LongString): LongString;
  begin
  XLatLongStr(S, Result, KeyMapDescr[kmAnsi].XlatCP^[ToAscii]);
  end;

procedure AcceptToAscii(var XLatCP: TXLatCP);
  var
    C: Char;
  begin
{ Таблица ToAscii бывает не обратимой однозначно. Например,
под WinNT построение перевода через юникод приводит к
перекодировкам в "похожие", символы вроде копипайта в 'C'.
Но "правильные" коды всегда меньше "похожих", поэтому обращение
таблицы надо делать не от #00 к #$FF, а наоборот. }
  FillChar(XLatCP[FromAscii], SizeOf(TXLat), '?');
  for C := High(TXLat) downto Low(TXLat) do
    begin
    XLatCP[ToUpAscii][C] := Upcase(XLatCP[ToAscii][C]);
    XLatCP[FromAscii][XLatCP[ToAscii][C]] := C;
    end;
  end;

function ReadXlt(FN: string; var N: Integer): PCodeConv;
  var
    S: TDOSStream;
    I, J: Integer;
  begin
  Result := nil;
  N := 0;
  if FN <> '' then
    begin
    if Pos('\', FN) = 0 then
      FN := SourceDir+'XLT\' + FN;
    S.Init(FN, stOpenRead);
    if  (S.GetSize >= 2) and (S.GetSize <= 256*4) then
      begin
      N := i32(S.GetSize);
      GetMem(Result, N);
      S.Read(Result^, N);
      end;
    S.Done;
    end;
  end;

function BuildLayoutConvXlat(const FN: string): Boolean;
  var
    Conv, Conv0: PCodeConv;
    I, J: Integer;
  begin
  Result := True;
  NullXlat(LayoutConvXlat);
  if FN = '' then
    Exit;
  Conv0 := ReadXlt(FN, J);
  if Conv0 <> nil then
    begin
    Conv := Conv0;
    for I := 1 to J div 2 do
      begin
      if not (Conv^.A in [#$0D, #$0A]) then
        begin
        LayoutConvXlat[Conv^.A] := Conv^.B;
        LayoutConvXlat[Conv^.B] := Conv^.A;
        end;
      inc(Conv);
      end;
    FreeMem(Conv0);
    end
  else
    Result := False;
  end;

function BuildABCSortXlat(const FN: string): Boolean;
  var
    L: Integer;
    Conv: PCodeConv;
  begin
  Result := True;
  NullXlat(ABCSortXlat);
  if FN = '' then
    Exit;
  NullXlat(ABCSortXlat);
  Conv := ReadXlt(FN, L);
  if Conv = nil then
    begin
    Result := False;
    Exit;
    end;
  ConvToXlat(Conv, L, ABCSortXlat);
  end;

procedure XLatStr(var S: String; const XLat: TXLat);
  var
    i: Longint;
  begin
  for i := 1 to Length(S) do
    S[i] := XLat[S[i]];
  end;

procedure InitUpcase;
  var
    C, L, U: Char;
  begin
  QueryUpcaseTable;
  NullXLAT(LowCaseArray);
{см. комментарий к AcceptToAscii}
  for C := High(TXlat) downto Low(TXlat) do
    if UpCaseArray[C] <> C then
      LowCaseArray[UpCaseArray[C]] := C;
  for C := High(TXlat) downto Low(TXlat) do
    begin
    L := LowCaseArray[C];
    U := UpCaseArray[C];
    ToggleCaseArray[L] := U;
    ToggleCaseArray[U] := L;
    end;
  end { InitUpcase };

procedure ConvToXlat(Conv: PCodeConv; L: Integer; var Xlat: TXLat);
  var
    I: Integer;
    Conv0: PCodeConv;
  begin
  Conv0 := Conv;
  for I := 1 to L div 2 do
    begin
    if not (Conv^.A in [#$0D, #$0A]) then
      Xlat[Conv^.A] := Conv^.B;
    Inc(Conv);
    end;
  FreeMem(Conv0);
  end;

function BuildCodeTable(const S: string; var XlatCP: TXLatCP): Boolean;
  var
    CP: Integer;
    Err: Integer;
    I, L: Integer;
    Conv: PCodeConv;
    C: Char;
  begin
  Result := False;
  Val(S, CP, Err);
  if Err = 0 then
    Result := QueryToAscii(CP, XlatCP[ToAscii])
  else
    begin { должно быть имя xlt-файла}
    NullXLAT(XlatCP[ToAscii]);
    Conv := ReadXlt(S, L);
    if Conv <> nil then
      begin
      ConvToXlat(Conv, L, XlatCP[ToAscii]);
      Result := True;
      end;
    end;
  if Result then
    AcceptToAscii(XlatCP);
  end;

procedure FreeCodetables;
  var
    km: TKeyMap;
    i: Integer;
  begin
  for km := Succ(kmAnsi) to MaxKeyMap do
    FreeMem(KeyMapDescr[km].XlatCP);
  MaxKeyMap := kmAscii;
  for km := Low(km) to kmAscii do
    RollKeyMap[km] := kmAnsi;
  for i := Low(TXLatCP) to High(TXLatCP) do
    WinXlatCP[i] := NullXlatTable;
  end;

function BuildWinCodeTable(S: string): Boolean;
  var
    km: TKeyMap;
  begin
  Result := BuildCodeTable(S, WinXlatCP);
  if Result then
    begin
    AcceptToAscii(WinXlatCP);
    MaxKeyMap := kmAnsi;
    for km := Low(km) to kmAscii do
      RollKeyMap[km] := kmAnsi;
    end;
  end;

function InitCodeTables(CodeTables: string): Boolean;
  var
    l1, l2: Integer;
    CP: Word;
    Err: Integer;
    I, J: Integer;
    S: String;
    km: TKeyMap;
    GoodTable: Boolean;
    C: Char;
    b: Boolean;
  begin
  Result := False;
  CodeTables := CodeTables + ' '; // Для Pos(' ');
  while CodeTables <> '' do
    begin
    l1 := Pos(':', CodeTables);
    if l1 = 0 then
      Break;
    l2 := Pos(' ', CodeTables);
    if l2 = 0 then
      Break;
    Inc(MaxKeyMap);
    GoodTable := False;
    with KeyMapDescr[MaxKeyMap] do
      begin
      GetMem(XlatCP, SizeOf(TXlatCP));
      Tag := DelSpaces(Copy(CodeTables, 1, l1-1));
      end;
    S := DelSpaces(Copy(CodeTables, l1+1, l2-l1));
    System.Delete(CodeTables, 1, l2);
    GoodTable := BuildCodeTable(S, KeyMapDescr[MaxKeyMap].XlatCP^);
    with KeyMapDescr[MaxKeyMap] do
      if not GoodTable then
        begin
        FreeMem(XlatCP);
        Dec(MaxKeyMap);
        Exit;
        end
      else
        begin
        RollKeyMap[Pred(MaxKeyMap)] := MaxKeyMap;
        RollKeyMap[MaxKeyMap] := kmAscii;
        end;
    end;
  DelSpace(CodeTables);
  Result := CodeTables = '';
  end;

{JO}
procedure OemToCharSt(var OemS: String);
  begin
  XLatBuf(OemS[1], Length(OemS), WinXlatCP[FromAscii]);
  end;

procedure CharToOemSt(var CharS: String);
  begin
  XLatBuf(CharS[1], Length(CharS), WinXlatCP[ToAscii]);
  end;

function OemToCharStr(const OemS: String): String;
  var
    I: Byte;
  begin
  SetLength(Result, Length(OemS));
  for I := 1 to Length(OemS) do
    Result[I] := WinXlatCP[FromAscii][OemS[I]];
  end;

function CharToOemStr(const CharS: String): String;
  var
    I: Byte;
  begin
  SetLength(Result, Length(CharS));
  for I := 1 to Length(CharS) do
    Result[I] := WinXlatCP[ToAscii][CharS[I]];
  end;
{/JO}

begin
NullXLAT(NullXlatTable);
NullXlatTable1 := NullXlatTable;
FreeCodetables; {Это безопасно, так как MaxKeyMap сейчас нулевой }
GetSysCountryInfo; InitUpcase;
end.
