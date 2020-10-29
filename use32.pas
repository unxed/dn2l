unit Use32;

{&Use32-}

interface

type
{$IFDEF USE32}
  Integer    = System.Longint;
  Word       = System.Longint;
const
  MaxInt     = MaxLongint;
{$ELSE}
  SmallInt   = System.Integer;
  SmallWord  = System.Word;
{$ENDIF}
type
  PByte      = ^Byte;
  PWord      = ^Word;
  PLongint   = ^Longint;
  PSmallInt  = ^SmallInt;
  PSmallWord = ^SmallWord;

implementation

end.
