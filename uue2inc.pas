unit Uue2Inc;
(******

Pascal version of UUE2INC.ASM
Written by Cat 2:5030/1326.13
(for use in DN/2)

******)

{$I STDEFINE.INC}
{$D-,E+,I-,L-,N+,Q-,R-,S-,Y-}

interface

uses
  use16
  ;

type
  T64 = record
    case Byte of
      0: (l0, l1: LongInt);
      1: (W0, W1, W2, W3: Word);
      2: (C0: Comp);
    end;

procedure Prepare1Str(var Sou, Dst);
function GetUUxlt(b: Byte): Char;
function GetLnCrc(var Buf; Size: LongInt): Char;
procedure cCRC(var Buf; Size: LongInt; var PrevSum: Word);
procedure CRC64(var Buf; Size: LongInt; var PrevSum: T64;
     var PrevCnt: Word);
procedure Clear64(var n: T64);

implementation

const
  UUxlt: array[0..63] of Char =
  '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
  {
const
  Poly64:array[0..255] of byte=
    (158,253,28,193,115,109,43,231,98,5,102,194,72,235,155,133,
     157,186,180,252,77,81,53,61,3,13,198,128,96,132,78,229,
     195,35,115,28,147,68,137,57,49,164,51,26,1,66,83,47,
     60,27,1,151,106,72,167,204,188,104,131,99,170,112,31,146,
     137,57,139,122,249,109,97,124,101,163,230,2,248,224,206,138,
     180,184,117,225,153,196,140,216,9,62,157,224,207,224,215,81,
     125,103,5,94,11,81,172,227,216,170,164,150,146,34,185,81,
     141,154,26,112,157,243,166,135,127,217,100,227,77,178,172,143,
     162,23,213,237,102,95,115,4,91,37,101,20,232,230,82,34,
     195,12,79,121,108,6,203,96,168,70,252,124,88,73,102,157,
     109,248,67,241,220,11,214,217,177,66,248,223,201,145,109,130,
     198,159,197,218,49,49,225,80,0,87,91,226,215,138,101,176,
     201,251,236,215,112,203,8,192,143,245,255,125,182,10,115,215,
     122,40,133,18,75,173,232,167,248,162,77,108,103,223,153,226,
     20,88,192,177,91,27,79,123,162,245,237,154,228,191,95,109,
     56,191,230,68,75,184,239,25,246,128,114,151,86,55,137,48);

Cat:  this array is not used in my version

  }

procedure Prepare1Str(var Sou, Dst);
  var
    i, j: Byte;
    sou_: array[0..44] of Byte absolute Sou;
    dst_: array[0..59] of Char absolute Dst;
  begin
  i := 0;
  j := 0;
  while i < 45 do
    begin
    dst_[j+0] := UUxlt[sou_[i] shr 2];
    dst_[j+1] := UUxlt[((sou_[i] and 3) shl 4)+(sou_[i+1] shr 4)];
    dst_[j+2] := UUxlt[((sou_[i+1] and 15) shl 2)+(sou_[i+2] shr 6)];
    dst_[j+3] := UUxlt[sou_[i+2] and 63];

    Inc(i, 3);
    Inc(j, 4);
    end
  end;

function GetUUxlt(b: Byte): Char;
  begin
  GetUUxlt := UUxlt[b]
  end;

procedure cCRC(var Buf; Size: LongInt; var PrevSum: Word);
  var
    buf_: array[1..65520] of Byte absolute Buf;
    i: LongInt;
  begin
  for i := 1 to Size do
    if  (PrevSum and 1) = 0 then
      {suxx! no "ror" operation...}
      PrevSum := (PrevSum shr 1)+buf_[i]
    else
      PrevSum := (PrevSum shr 1)+buf_[i]+$8000
  end;

procedure CRC64(var Buf; Size: LongInt; var PrevSum: T64;
     var PrevCnt: Word);
  var
    buf_: array[1..65520] of Byte absolute Buf;
    i: LongInt;

  procedure AddByte(b: Byte);
    near;
    begin
    with PrevSum do
      begin
      C0 := C0+b;
      if  (l1 and $80000000) = 0 then
        {"shl" can't be used with comp...}
        if  (l0 and $80000000) = 0 then
          begin
          l0 := l0 shl 1;
          l1 := l1 shl 1
          end
        else
          begin
          l0 := l0 shl 1;
          l1 := (l1 shl 1)+1
          end
      else if (l0 and $80000000) = 0 then
        begin
        l0 := (l0 shl 1)+1;
        l1 := l1 shl 1
        end
      else
        begin
        l0 := (l0 shl 1)+1;
        l1 := (l1 shl 1)+1
        end
      end
    end { AddByte };

  begin { CRC64 }
  for i := 1 to Size do
    begin
    AddByte(buf_[i]);
    AddByte(Byte(PrevCnt));
    Inc(PrevCnt)
    end
  end { CRC64 };

procedure Clear64(var n: T64);
  begin
  with n do
    begin
    l0 := 2055944585;
    l1 := 2086759929;
    end
  end;

function GetLnCrc(var Buf; Size: LongInt): Char;
  var
    buf_: array[1..65520] of Byte absolute Buf;
    i: LongInt;
    dx: Word;
  begin
  dx := 0;
  for i := 1 to Size do
    Inc(dx, (buf_[i]-$20) and $3F);
  GetLnCrc := UUxlt[dx and $3F]
  end;

begin
end.
