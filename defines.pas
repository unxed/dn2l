unit Defines;

{$I STDEFINE.INC}

interface

uses
  VpSysLow
  ;

const
  MaxLongStringLength = 1048575;
  MaxBytes = 128*1024*1024;
  MaxWords = MaxBytes div SizeOf(Word);
  MaxPtrs = MaxBytes div SizeOf(Pointer);

type
  TFileSize = Longint;
  
  Str2 = String[2];
  Str3 = String[3];
  Str4 = String[4];
  Str5 = String[5];
  Str6 = String[6];
  Str8 = String[8];
  Str12 = String[12];
  Str40 = String[40];
  Str50 = String[50];

  PStr2 = ^Str2;
  PStr3 = ^Str3;
  PStr4 = ^Str4;
  PStr5 = ^Str5;
  PStr6 = ^Str6;
  PStr8 = ^Str8;
  PStr12 = ^Str12;
  PStr40 = ^Str40;
  PStr50 = ^Str50;

  PString = ^String;
  LongString = AnsiString;
  PLongString = ^LongString;

  TCharSet = set of Char;
  PCharSet = ^TCharSet;

  WordRec = record
    Lo, Hi: Byte;
    end;

  LongRec = record
    Lo, Hi: SmallWord;
    end;

  SW_Word = LongInt;
  Sw_Integer = LongInt;

  AInt = SmallInt;
  AWord = SmallWord;

  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxBytes-1] of Byte;

  PWordArray = ^TWordArray;
  TWordArray = array[0..MaxWords-1] of Word;

  PAWordArray = ^TAWordArray;
  TAWordArray = array[0..MaxWords-1] of AWord;

  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..MaxPtrs-1] of Pointer;

  FNameStr = String;
  AsciiZ = array[0..255] of Char;
  THandle = Integer;

  PPoint = ^TPoint;
  TPoint = object
    X, Y: LongInt;
    function Equals(P: TPoint): Boolean;
    function EqualsXY(AX, AY: LongInt): Boolean;
    procedure Assign(AX, AY: LongInt);
    function isLE(P: TPoint): Boolean; {less then or equal}
    function isGE(P: TPoint): Boolean; {great then or equal}
    end;

  TRect = object
    A, B: TPoint;
    procedure Assign(XA, YA, XB, YB: LongInt);
    procedure Copy(R: TRect);
    procedure Move(ADX, ADY: LongInt);
    procedure Grow(ADX, ADY: LongInt);
    procedure Intersect(R: TRect);
    procedure Union(R: TRect);
    function Contains(P: TPoint): Boolean;
    function Equals(R: TRect): Boolean;
    function Empty: Boolean;
    end;

  TSize = Comp; {64 bit integer type for file sizes}

  PXLat = ^TXlat;
  TXlat = array[Char] of Char;
    {`Таблица перекодировки. Символ в исходной кодировке - индекс,
     соответствующий элемент - символ в новой кодировке `}

procedure Beep(Freq, Dur: LongInt);

implementation

procedure Beep(Freq, Dur: LongInt);
  begin
// fixme: commented by unxed
//  VpSysLow.SysBeepEx(Freq, Dur);
  end;

procedure CheckEmpty(var Rect: TRect);
  begin
  with Rect do
    if  (A.X >= B.X) or (A.Y >= B.Y) then
      begin
      A.X := 0;
      A.Y := 0;
      B.X := 0;
      B.Y := 0;
      end;
  end;

function TPoint.Equals(P: TPoint): Boolean;
  begin
  Equals := (X = P.X) and (Y = P.Y);
  end;

function TPoint.EqualsXY(AX, AY: LongInt): Boolean;
  begin
  EqualsXY := (X = AX) and (Y = AY);
  end;

procedure TPoint.Assign(AX, AY: LongInt);
  begin
  X := AX;
  Y := AY;
  end;

function TPoint.isLE(P: TPoint): Boolean;
  begin
  isLE := False;
  if  (Y = P.Y) and (X <= P.X) or (Y < P.Y) then
    isLE := True;
  end;

function TPoint.isGE(P: TPoint): Boolean;
  begin
  isGE := False;
  if  (Y = P.Y) and (X >= P.X) or (Y > P.Y) then
    isGE := True;
  end;

procedure TRect.Assign(XA, YA, XB, YB: LongInt);
  begin
  A.X := XA;
  A.Y := YA;
  B.X := XB;
  B.Y := YB;
  end;

procedure TRect.Copy(R: TRect);
  begin
  A := R.A;
  B := R.B;
  end;

procedure TRect.Move(ADX, ADY: LongInt);
  begin
  Inc(A.X, ADX);
  Inc(A.Y, ADY);
  Inc(B.X, ADX);
  Inc(B.Y, ADY);
  end;

procedure TRect.Grow(ADX, ADY: LongInt);
  begin
  Dec(A.X, ADX);
  Dec(A.Y, ADY);
  Inc(B.X, ADX);
  Inc(B.Y, ADY);
  CheckEmpty(Self);
  end;

procedure TRect.Intersect(R: TRect);
  begin
  if  (R.A.X > A.X) then
    A.X := R.A.X;
  if  (R.A.Y > A.Y) then
    A.Y := R.A.Y;
  if  (R.B.X < B.X) then
    B.X := R.B.X;
  if  (R.B.Y < B.Y) then
    B.Y := R.B.Y;
  CheckEmpty(Self);
  end;

procedure TRect.Union(R: TRect);
  begin
  if  (R.A.X < A.X) then
    A.X := R.A.X;
  if  (R.A.Y < A.Y) then
    A.Y := R.A.Y;
  if  (R.B.X > B.X) then
    B.X := R.B.X;
  if  (R.B.Y > B.Y) then
    B.Y := R.B.Y;
  end;

function TRect.Contains(P: TPoint): Boolean;
  begin
  Contains := (P.X >= A.X) and (P.X < B.X) and
      (P.Y >= A.Y) and (P.Y < B.Y);
  end;

function TRect.Equals(R: TRect): Boolean;
  begin
  Equals := (A.X = R.A.X) and (A.Y = R.A.Y) and
      (B.X = R.B.X) and (B.Y = R.B.Y);
  end;

function TRect.Empty: Boolean;
  begin
  Empty := (A.X >= B.X) or (A.Y >= B.Y);
  end;

end.
