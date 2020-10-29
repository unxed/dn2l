// copied from LIBWLF/country_.pas by unxed
{ Win32-specific country tools unit by A.Korop (AK155)}
{$MODE DELPHI}
unit Country;

interface
uses
  Defines;

procedure QueryUpcaseTable;

procedure GetSysCountryInfo;

function QueryToAscii(CP: word; var ToAscii: TXLat): Boolean;

function QueryABCSort(CP: Word; var ABCSortXlat: TXLat): Boolean;

implementation

uses
  {Windows, *}advance, advance1, strings, UKeyMap
  ;

{ AK155 Чтобы получить таблицу перевода на верхний регистр для кодировки
OEM, я не нашёл ничего лучше, чем перекодировать набор символов в ANSI,
перевести на верхний регистр в ANSI и перекодировать обратно. При этом
приходится бороться с символами, которые не перекодируются туда-сюда
(например, псевдографика). Их считаем не буквами, то есть не имеющими
различия верхнего и нижнего регистра.
  Делать это пришлось через OEM, а не через юникод, так как в Win9x
нет перевода на верхний регистр в юникоде.
  Если кто-то умеет создать эту таблицу более цивилизованно - милости
просим. }

procedure QueryUpcaseTable;
  const
    LT = SizeOf(TXLat);
  var
    CommonChars: TXLat;
    C: Char;
  begin
  NullXlat(UpCaseArray);
  //by unxed. fixme
  //OemToCharBuff(@UpCaseArray, @UpCaseArray, LT);
  //CharUpperBuff(@UpCaseArray, LT);
  //CharToOemBuff(@UpCaseArray, @UpCaseArray, LT);

  NullXlat(CommonChars);
  //OemToCharBuff(@CommonChars, @CommonChars, LT);
  //CharToOemBuff(@CommonChars, @CommonChars, LT);

  for C := Low(TXLat) to High(TXLat) do
    begin
    if C <> CommonChars[C] then
      UpcaseArray[C] := C;
    end;
  end;

var
  ResBuf: array[0..7] of char; // результат GetInfoElement, ASCIIZ

type
    LCTYPE = ^char;

procedure GetInfoElement(ElType: LCTYPE);
  var
    l: Longint;
  begin
  {
  l := GetLocaleInfo(
    LOCALE_USER_DEFAULT, // LCID Locale,      // locale identifier
    ElType,  // information type
    @ResBuf, //LPTSTR lpLCData,  // information buffer
    SizeOf(ResBuf)//int cchData       // size of buffer
  );
  }
  //CharToOem(@ResBuf, @ResBuf);
  end;

procedure GetSysCountryInfo;
  begin
  with CountryInfo do
    begin
    {
    GetInfoElement(LOCALE_IDATE );
    DateFmt := byte(ResBuf[0]) - byte('0');

    GetInfoElement(LOCALE_ITIME);
    TimeFmt := byte(ResBuf[0]) - byte('0');

    GetInfoElement(LOCALE_SDATE);
    DateSep := ResBuf[0];

    GetInfoElement(LOCALE_STIME);
    TimeSep := ResBuf[0];

    GetInfoElement(LOCALE_STHOUSAND);
    ThouSep := ResBuf[0];
    if ThouSep = #255 then
      ThouSep := ' '; // был неразрывый пробел?

    GetInfoElement(LOCALE_SDECIMAL);
    DecSep := ResBuf[0];

    GetInfoElement(LOCALE_ICURRDIGITS);
    DecSign := ResBuf[0];

    GetInfoElement(LOCALE_SCURRENCY);
    Currency := StrPas(ResBuf);

    GetInfoElement(LOCALE_ICURRENCY);
    CurrencyFmt := byte(ResBuf[0]) - byte('0');
    }
    end;
  end;

function QueryToAscii(CP: word; var ToAscii: TXLat): Boolean;
  const
    LT = SizeOf(TXLat);
  var
    //UniString, UniStringBack: array[0..LT-1] of WChar;
    i: Integer;
    l: Longint;
  begin
(*
  NullXlat(ToAscii);
  l := MultiByteToWideChar(CP, 0,
    @ToAscii, LT,
    @UniString, LT);
  Result := l=LT;

  l := WideCharToMultiByte(CP_OEMCP, 0,
    @UniString, LT,
    @ToAscii, LT,
    nil, nil);
  Result := Result and (l=LT);
   
{  Нижеследующее шаманство нужно вот зачем.
   При перекодировании из юникода в 1-байтную кодировку по умолчанию
система оказывает медвежью услугу: юникодные символы, не имющие
точного перевода, но имеющие "похожий" перевод, переводятся в эти самые
"похожие" символы. В результате таблица ToAscii оказывается
неоднозначной, что приводит к неприятностям при построении обратной
таблицы. Эту глюкофичу можно отключить при помощи флага
WC_NO_BEST_FIT_CHARS, но он работает только под NT 5. Вот и приходится
подчищать построенную таблицу от неоднозначностей.
   Использование "?" в качестве заменителя "непереводимых" символов
безопасно, так как его код достаточно маленький, а построение обратной
таблицы идёт от больших кодов к меньшим.
}

  l := MultiByteToWideChar(CP_OEMCP, 0,
    @ToAscii, LT,
    @UniStringBack, LT);
  Result := Result and (l=LT);

  for i := 0 to LT-1 do
    if UniString[i] <> UniStringBack[i] then
      ToAscii[Char(i)] := '?';
*)
  end;

function QueryABCSort(CP: Word; var ABCSortXlat: TXLat): Boolean;
  begin
  Result := False; //!! Пока не реализовано (04.09.2005)
  end;

end.
