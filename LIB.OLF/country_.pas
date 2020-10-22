{ OS/2-specific country tools unit by A.Korop (AK155)}
{$IFNDEF OS2} This is the OS/2 version! {$ENDIF}
unit Country_;

interface

uses
  U_KeyMap, Defines;

procedure GetSysCountryInfo;
  {` Заполняет CountryInfo (advance) данными от системы `}

procedure QueryUpcaseTable;
  {` Для текущей кодовой страницы запрашивается у ОС таблица перекодировки
  на верхний регистр. Результат - UpCaseArray `}

function QueryToAscii(CP: Word; var ToAscii: TXLat): Boolean;
  {` Для кодовой страницы CP запрашивается у ОС таблица перекодировки
  из CP в текущую кодовую страницу`}

function QueryABCSort(CP: Word; var ABCSortXlat: TXLat): Boolean;
  {` Для кодовой страницы CP запрашивается у ОС таблица весов
  для алфавитной сортировки. Если CP=0 - для текущей кодовой страницы.`}

implementation

uses
  Os2Def, Os2Base
  , advance, advance1, strings
  ;

// =========== uncode API (uconv.dll)
type
  size_t = Word;
  UniChar = SmallWord;
  UniCharArray = array[0..High(Word) div SizeOf(UniChar)] of UniChar;
  pUniCharArray = ^UniCharArray;
  UconvObject = Pointer;

{&stdcall+}

var
  UniCreateUconvObject: function (const cpname: UniCharArray;
    var uconv_object: UconvObject): Integer;
  UniMapCpToUcsCp: function(const ulCodepage: Longint;
    var ucsCodepage: UniCharArray; const n: size_t): Integer;
  UniUconvToUcs: function(const uconv_object: UconvObject;
    var inbuf: PChar; var inbytesleft: size_t;
    var ucsbuf: pUniCharArray; var UniCharsleft: size_t;
    var nonidentical: size_t): Integer;
  UniUconvFromUcs: function(const uconv_object: UconvObject;
    var ucsbuf:pUniCharArray;var UniCharsleft: size_t;
    var outbuf:pchar;var outbytesleft: size_t;
    var nonidentical: size_t): Integer;
  UniFreeUconvObject: function(const uconv_object: UconvObject): Integer;

{&stdcall-}
// =========== uncode API

var
  Country: Os2Base.CountryCode; // Country code info (0 = current country)
  CtryInfo: Os2Base.CountryInfo; // Buffer for country-specific information
  default_cp_conversion : UconvObject=nil;

procedure QueryUpcaseTable;
  var
    Country   : CountryCode;
    rc        : ApiRet;      // Return code
  begin
  Country.country  := 0; // Current
  Country.codepage := 0; // Current
  NullXlat(UpCaseArray);
  rc := DosMapCase(
    SizeOf(UpCaseArray), // Length of string to convert
    Country,            // Country and code page info
    @UpCaseArray);       // String to convert
  end;

function QueryCountryInfo: Boolean;
  var
    ulInfoLen : ULong;
    rc         : ApiRet;
  begin
  ulInfoLen := 0;
  rc := DosQueryCtryInfo(
    sizeof(CtryInfo),
    Country,
    CtryInfo,
    ulInfoLen);
  Result := (rc = No_Error);
  end;

procedure GetSysCountryInfo;
  begin
  QueryCountryInfo;
  with advance.CountryInfo, CtryInfo do
    begin
    DateFmt := fsDateFmt;
    TimeFmt := fsTimeFmt;
    DateSep := szDateSeparator[0];
    TimeSep := szTimeSeparator[0];
    ThouSep := szThousandsSeparator[0];
    DecSep := szDecimal[0];
    DecSign := Char(byte('0') + cDecimalPlace);
    Currency := StrPas(szCurrency);
    if (fsCurrencyFmt and 4) <> 0 then
      fsCurrencyFmt := 4;
    CurrencyFmt := fsCurrencyFmt;
    end;
  end;

const
  UniNull: array[0..0] of UniChar = (0);
var
  EmptyUniStr: UniCharArray absolute UniNull;
var
  UniStr12: array[0..12] of UniChar;
  UniStrCP: UniCharArray absolute UniStr12; // строка с обозначением CP

procedure LoadUconv;
  var
    FailedModule: array[0..255] of Char;
    LibHandle: hModule;
  begin
  if DosLoadModule(FailedModule, SizeOf(FailedModule), 'UCONV',
       LibHandle) <> 0
  then
    begin
    writeln('Unable to load UCONV library');
    exit;
    end;
  DosQueryProcAddr(LibHandle, 1, nil, @UniCreateUconvObject);
  DosQueryProcAddr(LibHandle, 2, nil, @UniUconvToUcs);
  DosQueryProcAddr(LibHandle, 3, nil, @UniUconvFromUcs);
  DosQueryProcAddr(LibHandle, 4, nil, @UniFreeUconvObject);
  DosQueryProcAddr(LibHandle, 10, nil, @UniMapCpToUcsCp);
  end;

{ Заменяющий символ для непереводимых символов - '?' }
const
  UniSubstChar: array[1..14] of SmallWord = // @subchar=\x3F
    (Ord('@'), Ord('s'), Ord('u'), Ord('b')
    , Ord('c'), Ord('h'), Ord('a'), Ord('r'), Ord('=')
    , Ord('\'), Ord('x'), Ord('3'), Ord('F'), 0);

function QueryToAscii(CP: word; var ToAscii: TXLat): Boolean;
  var
    UniXlat, UniXlat0: pUniCharArray;
    uco_CP: UconvObject;
    UniBuf: array[0..SizeOf(TXLat)] of UniChar;
    pCP: PChar;
    lCP, lUni: Word;
    l: Longint;
    rc: ApiRet;
  begin
  Result := False;
  if @UniCreateUconvObject = nil then
    exit;
  if not Assigned(default_cp_conversion) then
    begin
    rc := UniCreateUconvObject(UniCharArray((@UniSubstChar)^),
      default_cp_conversion);
    if rc <> 0 then
      exit;
    end;
  rc := UniMapCpToUcsCp(CP, UniStrCP, 12);
  rc := rc or UniCreateUconvObject(UniStrCP, uco_CP);

  NullXlat(ToAscii);

  UniXlat := @UniBuf;
  UniXlat0 := UniXlat;
  l := 0;
  pCP := @ToAscii;
  lCP := SizeOf(ToAscii); lUni := lCP;

  rc := rc or UniUconvToUcs(uco_CP, pCP, lCP,
    UniXlat, lUni, l);

  pCP := @ToAscii;
  UniXlat := UniXlat0;
  lCP := SizeOf(ToAscii); lUni := lCP;

  rc := rc or UniUconvFromUcs(default_cp_conversion, UniXlat, lUni,
    pCP, lCP, l);

  rc := rc or UniFreeUconvObject(uco_CP);
  result := rc = 0;
  end;

function QueryABCSort(CP: Word; var ABCSortXlat: TXLat): Boolean;
  var
    UserInfo  : CountryCode; // Country and code page requested
    ulSeqLen  : ULong;       // Length of sequence returned
    i         : char;       // Two loop indices
    j         : ULong;
    rc        : ApiRet;      // Return code
  begin
  UserInfo.country := 0;  // Request information about current country
  UserInfo.codepage := CP; // ... and current code page
  rc := DosQueryCollate(
    SizeOf(ABCSortXlat),  // Length of output area
    UserInfo,           // Country and codepage info
    @ABCSortXlat,          // Area for collating sequence
    ulSeqLen);          // Length of data returned

  Result := (rc = No_Error) and (ulSeqLen = 256);
  end;

begin
LoadUconv;
end.
