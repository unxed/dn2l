unit PlugRez;
(******

Access *.REZ files from plugins
Written by Cat 2:5030/1326.13
(for use in DN/2 Plugin Version)

******)

{$I STDEFINE.INC}

{Cat
   17/01/2002 - начало сооружения этого модуля
                Версия плагинов EventCatcher - 2.0
}

{$IFNDEF OS2}
{$IFNDEF WIN32}
{$ERROR Only OS2 or WIN32!}
{$ENDIF}
{$ENDIF}

interface

uses
  Objects2
  ;

function OpenRez(const PluginName: ShortString): LongInt;
procedure CloseRez(RezId: LongInt);
function GetRezString(RezId: LongInt; ItemId: SmallWord): String;
function GetRezObject(RezId: LongInt; ItemId: SmallWord): PObject;

implementation

uses
  Streams, Advance, Advance1, Advance7
  ;

type
  PPluginRezData = ^TPluginRezData;
  TPluginRezData = record
    Stream: PStream;
    StringsCount: SmallWord;
    ObjectsCount: SmallWord;
    Offset: array[0..0] of LongInt;
    end;

function OpenRez(const PluginName: ShortString): LongInt;
  const
    RezLabel = 'Dos Navigator /2 Plugin Resource File'#26;
  var
    P: PPluginRezData;
    S: PStream;
    SC, OC: SmallWord;
    L: String;
    OffsetTableOffset: LongInt;
  begin
  S := New(PBufStream, Init(SourceDir+PluginName+'/'+LngId+'.REZ', // slash change by unxed
         stOpenRead, 16384));
  S^.Read(L[1], Length(RezLabel));
  S^.Read(SC, SizeOf(SC));
  S^.Read(OC, SizeOf(OC));
  L[0] := Char(Length(RezLabel));
  if  (S^.Status <> stOK) or (L <> RezLabel)
         or (MaxAvail < SizeOf(TPluginRezData)+(SC+OC)*SizeOf(LongInt))
  then
    begin
    Dispose(S, Done);
    OpenRez := 0;
    Exit;
    end;

  GetMem(P,
     SizeOf(TPluginRezData)-SizeOf(LongInt)+(SC+OC)*SizeOf(LongInt));
  with P^ do
    begin
    Stream := S;
    StringsCount := SC;
    ObjectsCount := OC;
    S^.Read(OffsetTableOffset, SizeOf(OffsetTableOffset));
    S^.Seek(OffsetTableOffset);
    S^.Read(Offset, (SC+OC)*SizeOf(LongInt));
    if S^.Status <> stOK then
      begin
      Dispose(S, Done);
      FreeMem(P
        {, SizeOf(TPluginRezData)-SizeOf(LongInt)+(SC+OC)*SizeOf(LongInt)}
        );
      OpenRez := 0;
      Exit;
      end;
    end;

  OpenRez := LongInt(P);
  end { OpenRez };

procedure CloseRez(RezId: LongInt);
  begin
  if RezId <> 0 then
    with PPluginRezData(RezId)^ do
      begin
      Dispose(Stream, Done);
      FreeMem(Pointer(RezId)
        {, SizeOf(TPluginRezData)-SizeOf(LongInt)+(StringsCount+ObjectsCount)*SizeOf(LongInt)}
        )
      end;
  end;

function GetRezString(RezId: LongInt; ItemId: SmallWord): String;
  var
    S: String;
  begin
  with PPluginRezData(RezId)^, Stream^ do
    if ItemId >= StringsCount then
      GetRezString := '?'
    else
      begin
      Seek(Offset[ItemId]);
      ReadStrV(S);
      if  (Status <> stOK) or (S = '') then
        GetRezString := '?'
      else
        GetRezString := S;
      end;
  end;

function GetRezObject(RezId: LongInt; ItemId: SmallWord): PObject;
  var
    P: PObject;
  begin
  with PPluginRezData(RezId)^, Stream^ do
    if ItemId >= ObjectsCount then
      GetRezObject := nil
    else
      begin
      Seek(Offset[ItemId+StringsCount]);
      P := Get;
      if Status <> stOK then
        GetRezObject := nil
      else
        GetRezObject := P;
      end;
  end;

end.
