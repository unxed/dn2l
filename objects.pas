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

{$I STDEFINE.INC}
{$DEFINE NoCallspcb}

{-----------------------------------------------------}
{ This module is based on Turbo Vision Objects Unit   }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}
{AK155 = Alexey Korop, 2:461/155@fidonet}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{Cat
   28/08/2001 - переделал строковые коллекции для обеспечения возможности
   хранить в них не только String-и, но и LongString-и
}

unit Objects;

interface

uses
  Defines, Objects2, Streams
  ;

const
  MaxCollectionSize = MaxBytes div SizeOf(Pointer);

  { TCollection error codes }

  coIndexError = -1; { Index out of range }
  coOverflow = -2; { Overflow }

type
  PItemList = ^TItemList;
  TItemList = array[0..MaxCollectionSize-1] of Pointer;

  { TCollection object }

  PCollection = ^TCollection;
  TCollection = object(TObject)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Items: PItemList;
    Count: LongInt;
    Limit: LongInt;
    Delta: LongInt;
    Status, ErrorInfo: Integer; {!!! в OldColection}
    constructor Init(ALimit, ADelta: LongInt);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function At(Index: LongInt): Pointer;
    procedure AtDelete(Index: LongInt);
    procedure AtFree(Index: LongInt);
    procedure AtInsert(Index: LongInt; Item: Pointer);
    procedure AtPut(Index: LongInt; Item: Pointer);
    procedure AtReplace(Index: LongInt; Item: Pointer);
    procedure Delete(Item: Pointer);
    procedure DeleteAll;
    procedure Error(Code, Info: Integer); virtual;
    function FirstThat(Test: Pointer): Pointer;
    procedure ForEach(Action: Pointer);
    procedure Free(Item: Pointer);
    procedure FreeAll;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    function IndexOf(Item: Pointer): LongInt; virtual;
    procedure Insert(Item: Pointer); virtual;
    function LastThat(Test: Pointer): Pointer;
    procedure Pack;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    procedure SetLimit(ALimit: LongInt); virtual;
    procedure Store(var S: TStream);
    end;

  { TSortedCollection object }

  PSortedCollection = ^TSortedCollection;
  TSortedCollection = object(TCollection)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Duplicates: Boolean;
    constructor Init(ALimit, ADelta: LongInt);
    constructor Load(var S: TStream);
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    function IndexOf(Item: Pointer): LongInt; virtual;
    procedure Insert(Item: Pointer); virtual;
    function KeyOf(Item: Pointer): Pointer; virtual;
    function Search(Key: Pointer; var Index: LongInt): Boolean; virtual;
    procedure Store(var S: TStream);
    procedure Sort;
    procedure QSort;
    procedure QuickSort(L, R: LongInt);
    end;

  { TLineCollection }

  PLineCollection = ^TLineCollection;
  TLineCollection = object(TCollection)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    LongStrings: Boolean; {Cat}
    constructor Init(ALimit, ADelta: LongInt; ALongStrings: Boolean);
    {Cat}
    procedure FreeItem(P: Pointer); virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    end;

  { TStringCollection object }

  PStringCollection = ^TStringCollection;
  TStringCollection = object(TSortedCollection)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    LongStrings: Boolean; {Cat}
    constructor Init(ALimit, ADelta: LongInt; ALongStrings: Boolean);
    {Cat}
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    end;

  { TStrCollection object }

  PStrCollection = ^TStrCollection;
  TStrCollection = object(TSortedCollection)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    end;

  { TUnSortedStrCollection - UNSORTED STRING COLLECTION OBJECT }

  { This is a completely >> NEW << object which holds a collection of  }
  { strings but does not alphabetically sort them. It is a very useful }
  { object for insert ordered list boxes!                              }

  PUnSortedStrCollection = ^TUnSortedStrCollection;
  TUnSortedStrCollection = object(TStringCollection)
    procedure Insert(Item: Pointer); virtual;
    end;
  (*
{ TResourceCollection object }

  PResourceCollection = ^TResourceCollection;
  TResourceCollection = object(TStringCollection)
    procedure FreeItem(Item: Pointer); virtual;
    function  GetItem(var S: TStream): Pointer; virtual;
    function  KeyOf(Item: Pointer): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
  end;

{ TResourceFile object }

  PResourceFile = ^TResourceFile;
  TResourceFile = object(TObject)
    Stream: PStream;
    Modified: Boolean;
    constructor Init(AStream: PStream);
    destructor  Done; virtual;
    function  Count: LongInt;
    procedure Delete(Key: String);
    procedure Flush;
    function  Get(Key: String): PObject;
    function  KeyAt(I: LongInt): String;
    procedure Put(Item: PObject; Key: String);
    function  SwitchTo(AStream: PStream; Pack: Boolean): PStream;
  private
    BasePos: Longint;
    IndexPos: Longint;
    Index: TResourceCollection;
  end;
*)
  { TStringList object }

  TStrIndexRec = record
    Key, Count, Offset: AWord;
    end;

  PStrIndex = ^TStrIndex;
  TStrIndex = array[0..9999] of TStrIndexRec;

  PStringList = ^TStringList;
  TStringList = object(TObject)
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function Get(Key: AWord): String;
  private
    Stream: PStream;
    BasePos: LongInt;
    IndexSize: AWord;
    Index: PStrIndex;
    procedure ReadStr(var S: String; Offset, Skip: AWord);
    end;

  {-DataCompBoy-}
type
  PDirCol = ^TDirCol;
  TDirCol = object(TStringCollection)
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    end;
  {-DataCompBoy-}

implementation
uses
  Memory,
  Strings,
  Advance1
  ;

constructor TCollection.Init(ALimit, ADelta: LongInt);
  begin
  inherited Init;
  Delta := ADelta;
  SetLimit(ALimit);
  end;

constructor TCollection.Load(var S: TStream);
  var
    C, I: LongInt;
  begin
  TObject.Init;
  S.Read(Count, SizeOf(Count));
  S.Read(Limit, SizeOf(Limit));
  S.Read(Delta, SizeOf(Delta));
  if  (Count > Limit) or (Delta < 0) then
    Fail;
  C := Count;
  I := Limit;
  Count := 0;
  Limit := 0;
  SetLimit(I);
  for I := 0 to C-1 do
    begin
    AtInsert(I, GetItem(S));
    if  (S.Status <> stOK) then
      begin
      SetLimit(0);
      Fail;
      end;
    end;
  end { TCollection.Load };

function TCollection.At(Index: LongInt): Pointer;
  begin
  if  (Index < 0) or (Index >= Count) or (Items = nil)
  then
    begin
    Error(coIndexError, Index);
    At := nil;
    end
  else
    At := Items^[Index];
  end;

procedure TCollection.AtDelete(Index: LongInt);
  begin
  if  (Index >= 0) and (Index < Count) and (Items <> nil) then
    begin
    Dec(Count);
    if Count > Index then
      Move(Items^[Index+1], Items^[Index], (Count-Index)*SizeOf(Pointer));
    end
  else
    Error(coIndexError, Index);
  end;

procedure TCollection.AtInsert(Index: LongInt; Item: Pointer);
  var
    I: LongInt;
  begin
  if  (Index >= 0) and (Index <= Count) then
    begin
    if Count = Limit then
      SetLimit(Limit+Delta);
    if Limit > Count then
      begin
      if Index < Count then
        for I := Count-1 downto Index do
          Items^[I+1] := Items^[I];
      Items^[Index] := Item;
      Inc(Count);
      end
    else
      Error(coOverflow, Index);
    end
  else
    Error(coIndexError, Index);
  end;

procedure TCollection.AtPut(Index: LongInt; Item: Pointer);
  begin
  if  (Index >= 0) and (Index < Count) and (Items <> nil)
  then
    Items^[Index] := Item
  else
    Error(coIndexError, Index);
  end;

procedure TCollection.SetLimit(ALimit: LongInt);
  var
    AItems: PItemList;
  begin
  if ALimit < Count then
    ALimit := Count;
  if ALimit > MaxCollectionSize then
    ALimit := MaxCollectionSize;
  if ALimit <> Limit then
    begin
    if ALimit = 0 then
      AItems := nil
    else
      begin
      GetMem(AItems, ALimit*SizeOf(Pointer));
      if  (AItems <> nil) then
        FillChar(AItems^, ALimit*SizeOf(Pointer), #0);
      end;
    if  (AItems <> nil) or (ALimit = 0) then
      begin
      if  (AItems <> nil) and (Items <> nil) and (Count <> 0) then
        Move(Items^, AItems^, Count*SizeOf(Pointer));
      if  (Limit <> 0) and (Items <> nil) then
        FreeMem(Items, Limit*SizeOf(Pointer));
      end;
    Items := AItems;
    Limit := ALimit;
    end;
  end { TCollection.SetLimit };

destructor TCollection.Done;
  begin
  FreeAll;
  SetLimit(0);
  end;

procedure TCollection.AtFree(Index: LongInt);
  var
    Item: Pointer;
  begin
  Item := At(Index);
  AtDelete(Index);
  FreeItem(Item);
  end;

procedure TCollection.AtReplace;
  var
    P: Pointer;
  begin
  if  (Limit < Index) and (Delta > 0) then
    SetLimit(Index+1);
  while Count < (Index+1) do
    Insert(nil);
  P := At(Index);
  AtPut(Index, Item);
  if P <> nil then
    FreeItem(P);
  end;

procedure TCollection.Delete(Item: Pointer);
  begin
  AtDelete(IndexOf(Item));
  end;

procedure TCollection.DeleteAll;
  begin
  if @Self <> nil then
    Count := 0;
  end;

procedure TCollection.Error(Code, Info: Integer);
  begin
  Status := Code;
  ErrorInfo := Info;
  {RunError(212 - Code);}
  end;

{AK155: replace with TCollection.FirstThat from VP2.1 Objects.pas }
function TCollection.FirstThat(Test: Pointer): Pointer;
  assembler; {$USES ebx}
  {$FRAME-}
asm
                mov     edx,Self
                mov     ecx,[edx].TCollection.Count
                jecxz   @@3
                mov     ebx,Test
                mov     edx,[edx].TCollection.Items
              @@1:
                push    edx
                push    ecx
                push    DWord Ptr [edx]         { [1]:Pointer = Item }
                Call    ebx
                pop     ecx
                pop     edx
                test    al,al
                jnz     @@2
                add     edx,4
                loop    @@1
                jmp     @@3
              @@2:
                mov     ecx,[edx]
              @@3:
                mov     eax,ecx
end; {/AK155}

{AK155: pick ForEach from VP 2.1.231 RTL source}
procedure TCollection.ForEach(Action: Pointer);
  assembler; {$USES ebx}
  {$FRAME-}
asm
                mov     edx,Self
                mov     ecx,[edx].TCollection.Count
                jecxz   @@2
                mov     ebx,Action
                mov     edx,[edx].TCollection.Items
              @@1:
                push    edx
                push    ecx
                push    DWord Ptr [edx]         { [1]:Pointer = Item }
                Call    ebx
                pop     ecx
                pop     edx
                add     edx,4
                loop    @@1
              @@2:
end; {/AK155}

procedure TCollection.Free(Item: Pointer);
  begin
  Delete(Item);
  FreeItem(Item);
  end;

procedure TCollection.FreeAll;
  var
    I: LongInt;
  begin
  for I := Count-1 downto 0 do
    FreeItem(At(I));
  Count := 0;
  end;

procedure TCollection.FreeItem(Item: Pointer);
  begin
  FreeObject(Item);
  end;

function TCollection.GetItem(var S: TStream): Pointer;
  begin
  GetItem := S.Get;
  end;

function TCollection.IndexOf(Item: Pointer): LongInt;
  var
    I: LongInt;
  begin
  IndexOf := -1;
  for I := 0 to Count-1 do
    if At(I) = Item then
      begin
      IndexOf := I;
      Break;
      end;
  end;

procedure TCollection.Insert(Item: Pointer);
  begin
  AtInsert(Count, Item);
  end;

function TCollection.LastThat(Test: Pointer): Pointer;
  assembler; {$USES ebx}
  {$FRAME-} {AK155}
asm
                mov     edx,Self
                mov     ecx,[edx].TCollection.Count
                jecxz   @@3
                mov     edx,[edx].TCollection.Items
                lea     edx,[edx+ecx*4]
                mov     ebx,Test
              @@1:
                sub     edx,4
                push    edx
                push    ecx
                push    DWord Ptr [edx]         { [1]:Pointer = Item }
                Call    ebx
                pop     ecx
                pop     edx
                test    al,al
                jnz     @@2
                loop    @@1
                jmp     @@3
              @@2:
                mov     ecx,[edx]
              @@3:
                mov     eax,ecx

end; {/AK155}

procedure TCollection.Pack;
  var
    I: LongInt;
  begin
  for I := Count-1 downto 0 do
    if At(I) = nil then
      AtDelete(I);
  end;

procedure TCollection.PutItem(var S: TStream; Item: Pointer);
  begin
  S.Put(Item);
  end;

procedure TCollection.Store(var S: TStream);
  procedure DoPutItem(P: Pointer);
    begin
    PutItem(S, P);
    end;
  begin
  S.Write(Count, SizeOf(Count));
  S.Write(Limit, SizeOf(Limit));
  S.Write(Delta, SizeOf(Delta));
  ForEach(@DoPutItem);
  end;

{ TSortedCollection }

constructor TSortedCollection.Init(ALimit, ADelta: LongInt);
  begin
  TCollection.Init(ALimit, ADelta);
  Duplicates := False;
  end;

constructor TSortedCollection.Load(var S: TStream);
  begin
  inherited Load(S);
  S.Read(Duplicates, SizeOf(Duplicates));
  end;

function TSortedCollection.Compare(Key1, Key2: Pointer): Integer;
  begin
  end;

function TSortedCollection.IndexOf(Item: Pointer): LongInt;
  var
    I: LongInt;
  begin
  IndexOf := -1;
  if Search(KeyOf(Item), I) then
    begin
    if Duplicates then
      while (I < Count) and (Item <> At(I)) do
        Inc(I);
    if I < Count then
      IndexOf := I;
    end;
  end;

procedure TSortedCollection.Insert(Item: Pointer);
  var
    I: LongInt;
  begin
  if not Search(KeyOf(Item), I) or Duplicates then
    AtInsert(I, Item);
  end;

function TSortedCollection.KeyOf(Item: Pointer): Pointer;
  begin
  KeyOf := Item;
  end;

function TSortedCollection.Search
    (Key: Pointer; var Index: LongInt): Boolean;
  var
    L, H, I, C: LongInt;
  begin
  Search := False;
  L := 0;
  H := Count-1;
  while L <= H do
    begin
    I := (L+H) shr 1;
    C := Compare(KeyOf(At(I)), Key);
    if C < 0
    then
      L := I+1
    else
      begin
      if C = 0 then
        begin
        Search := True;
        if not Duplicates then
          L := I;
        Break;
        end;
      H := I-1;
      end;
    end;
  Index := L;
  end { TSortedCollection.Search };

procedure TSortedCollection.Store(var S: TStream);
  begin
  TCollection.Store(S);
  S.Write(Duplicates, SizeOf(Duplicates));
  end;

{ TStringCollection }

{Cat: добавил возможность хранить в коллекции длинные строки}
constructor TStringCollection.Init(ALimit, ADelta: LongInt;
     ALongStrings: Boolean);
  begin
  inherited Init(ALimit, ADelta);
  LongStrings := ALongStrings;
  end;

function TStringCollection.Compare(Key1, Key2: Pointer): Integer;
  var
    I, J: LongInt;
    P1, P2: PString;
    PL1, PL2: PLongString;
  begin
  if LongStrings then
    begin
    PL1 := PLongString(Key1);
    PL2 := PLongString(Key2);
    if Length(PL1^) < Length(PL2^) then
      J := Length(PL1^)
    else
      J := Length(PL2^);
    I := 1;
    while (I < J) and (PL1^[I] = PL2^[I]) do
      Inc(I);
    if  (I = J) then
      begin
      if  (PL1^[I] < PL2^[I]) then
        Compare := -1
      else if (PL1^[I] > PL2^[I]) then
        Compare := 1
      else if Length(PL1^) > Length(PL2^) then
        Compare := 1
      else if Length(PL1^) < Length(PL2^) then
        Compare := -1
      else
        Compare := 0;
      end
    else if (PL1^[I] < PL2^[I]) then
      Compare := -1
    else
      Compare := 1;
    end
  else
    begin
    P1 := PString(Key1);
    P2 := PString(Key2);
    if Length(P1^) < Length(P2^) then
      J := Length(P1^)
    else
      J := Length(P2^);
    I := 1;
    while (I < J) and (P1^[I] = P2^[I]) do
      Inc(I);
    if  (I = J) then
      begin
      if  (P1^[I] < P2^[I]) then
        Compare := -1
      else if (P1^[I] > P2^[I]) then
        Compare := 1
      else if Length(P1^) > Length(P2^) then
        Compare := 1
      else if Length(P1^) < Length(P2^) then
        Compare := -1
      else
        Compare := 0;
      end
    else if (P1^[I] < P2^[I]) then
      Compare := -1
    else
      Compare := 1;
    end;
  end { TStringCollection.Compare };

procedure TStringCollection.FreeItem(Item: Pointer);
  begin
  if LongStrings then
    DisposeLongStr(PLongString(Item))
  else
    DisposeStr(PString(Item));
  end;

function TStringCollection.GetItem(var S: TStream): Pointer;
  begin
  if LongStrings then
    GetItem := S.ReadLongStr
  else
    GetItem := S.ReadStr;
  end;

procedure TStringCollection.PutItem(var S: TStream; Item: Pointer);
  begin
  if LongStrings then
    S.WriteLongStr(Item)
  else
    S.WriteStr(Item);
  end;
{/Cat}

{ TLineCollection }
{Cat: добавил возможность хранить в коллекции длинные строки}
constructor TLineCollection.Init(ALimit, ADelta: LongInt;
     ALongStrings: Boolean);
  begin
  inherited Init(ALimit, ADelta);
  LongStrings := ALongStrings;
  end;

procedure TLineCollection.FreeItem(P: Pointer);
  begin
  if LongStrings then
    DisposeLongStr(PLongString(P))
  else
    DisposeStr(PString(P));
  end;

procedure TLineCollection.PutItem;
  begin
  if LongStrings then
    S.WriteLongStr(Item)
  else
    S.WriteStr(Item);
  end;

function TLineCollection.GetItem;
  begin
  if LongStrings then
    GetItem := S.ReadLongStr
  else
    GetItem := S.ReadStr;
  end;

{ TStrCollection }

function TStrCollection.Compare(Key1, Key2: Pointer): Integer;
  begin
  Compare := StrComp(Key1, Key2);
  end;

procedure TStrCollection.FreeItem(Item: Pointer);
  begin
  StrDispose(PChar(Item));
  end;

function TStrCollection.GetItem(var S: TStream): Pointer;
  begin
  GetItem := S.StrRead;
  end;

procedure TStrCollection.PutItem(var S: TStream; Item: Pointer);
  begin
  S.StrWrite(Item);
  end;

procedure TUnSortedStrCollection.Insert(Item: Pointer);
  begin
  AtInsert(Count, Item);
  end;
(*
{ Private resource manager types }

const
  RStreamMagic: Longint = $52504246; { 'FBPR' }
  RStreamBackLink: Longint = $4C424246; { 'FBBL' }

type
  PResourceItem = ^TResourceItem;
  TResourceItem = record
    Posn: Longint;
    Size: Longint;
    Key: String;
  end;

{ TResourceCollection }

procedure TResourceCollection.FreeItem(Item: Pointer);
begin
  FreeMem(Item, Length(PResourceItem(Item)^.Key) +
    (SizeOf(TResourceItem) - SizeOf(String) + 1));
end;

function TResourceCollection.GetItem(var S: TStream): Pointer;
var
  Pos: Longint;
  Size: Longint;
  L: Byte;
  P: PResourceItem;
begin
  S.Read(Pos, SizeOf(Pos));
  S.Read(Size, SizeOf(Size));
{Cat:warn AnsiString}
  S.Read(L, 1);
  GetMem(P, L + (SizeOf(TResourceItem) - SizeOf(String) + 1));
  P^.Posn:=Pos;
  P^.Size:=Size;
  SetLength(P^.Key, L);
  S.Read(P^.Key[1], L);
  GetItem:=P;
end;

function TResourceCollection.KeyOf(Item: Pointer): Pointer;
begin
  KeyOf:=@PResourceItem(Item)^.Key; { Pointer to key }
end;

procedure TResourceCollection.PutItem(var S: TStream; Item: Pointer);
begin
  S.Write(PResourceItem(Item)^, Length(PResourceItem(Item)^.Key) +
    (SizeOf(TResourceItem) - SizeOf(String) + 1));
end;

{ TResourceFile }

constructor TResourceFile.Init(AStream: PStream);
type

  {$IFDEF NewExeFormat}
  TExeHeader = record
    eHdrSize:   Word;
    eMinAbove:  Word;
    eMaxAbove:  Word;
    eInitSS:    Word;
    eInitSP:    Word;
    eCheckSum:  Word;
    eInitPC:    Word;
    eInitCS:    Word;
    eRelocOfs:  Word;
    eOvlyNum:   Word;
    eRelocTab:  Word;
    eSpace:     Array[1..30] of Byte;
    eNewHeader: Word;
  end;
  {$ENDIF}

  THeader = record
    Signature: Word;
    case Integer of
      0: (
        LastCount: Word;
        PageCount: Word;
        ReloCount: Word);
      1: (
        InfoType: Word;
        InfoSize: Longint);
  end;
var
  Found, Stop: Boolean;
  Header: THeader;

  {$IFDEF NewExeFormat}
  ExeHeader: TExeHeader;
  {$ENDIF}

begin
  TObject.Init;
  Stream:=AStream;
  Found:=False;
  if (Stream<>Nil) then begin
    BasePos:=Stream^.GetPos;
    repeat
      Stop:=True;
      if BasePos <= Stream^.GetSize - SizeOf(THeader) then
      begin
        Stream^.Seek(BasePos);
        Stream^.Read(Header, SizeOf(THeader));
        case Header.Signature of

        {$IFDEF NewExeFormat}
          $5A4D:
            begin
              Stream^.Read(ExeHeader, SizeOf(TExeHeader));
              BasePos:=ExeHeader.eNewHeader;
              Stop:=False;
            end;
          $454E:
            begin
              BasePos:=Stream^.GetSize - 8;
              Stop:=False;
            end;
          $4246:
            begin
              Stop:=False;
              case Header.Infotype of
                $5250:                                    {Found Resource}
                  begin
                    Found:=True;
                    Stop:=True;
                  end;
                $4C42: Dec(BasePos, Header.InfoSize - 8); {Found BackLink}
                $4648: Dec(BasePos, SizeOf(THeader) * 2); {Found HelpFile}
              else
                Stop:=True;
              end;
            end;
          $424E:
            if Header.InfoType = $3230 then               {Found Debug Info}
            begin
              Dec(BasePos, Header.InfoSize);
              Stop:=False;
            end;

        {$ELSE}
          $5A4D:
            begin
              Inc(BasePos, LongMul(Header.PageCount, 512) -
                (-Header.LastCount and 511));
              Stop:=False;
            end;
          $4246:
            if Header.InfoType = $5250 then Found:=True else
            begin
              Inc(BasePos, Header.InfoSize + 8);
              Stop:=False;
            end;

        {$ENDIF}
        end;
      end;
    until Stop;
  end;
  if Found then
  begin
    Stream^.Seek(BasePos + SizeOf(Longint) * 2);
    Stream^.Read(IndexPos, SizeOf(Longint));
    Stream^.Seek(BasePos + IndexPos);
    Index.Load(Stream^);
  end else
  begin
    IndexPos:=SizeOf(Longint) * 3;
    Index.Init(0, 8);
  end;
end;

destructor TResourceFile.Done;
begin
  Flush;
  Index.Done;
  If (Stream <> nil) then Dispose(Stream,Done); Stream:=nil;
end;

function TResourceFile.Count: LongInt;
begin
  Count:=Index.Count;
end;

procedure TResourceFile.Delete(Key: String);
var
  I: LongInt;
begin
  if Index.Search(@Key, I) then
  begin
    Index.Free(Index.At(I));
    Modified:=True;
  end;
end;

procedure TResourceFile.Flush;
var
  ResSize: Longint;
  LinkSize: Longint;
begin
  if Modified and (Stream <> nil) then
  begin
    Stream^.Seek(BasePos + IndexPos);
    Index.Store(Stream^);
    ResSize:=Stream^.GetPos - BasePos;
    LinkSize:=ResSize + SizeOf(Longint) * 2;
    Stream^.Write(RStreamBackLink, SizeOf(Longint));
    Stream^.Write(LinkSize, SizeOf(Longint));
    Stream^.Seek(BasePos);
    Stream^.Write(RStreamMagic, SizeOf(Longint));
    Stream^.Write(ResSize, SizeOf(Longint));
    Stream^.Write(IndexPos, SizeOf(Longint));
    Stream^.Flush;
  end;
  Modified:=False;
end;

function TResourceFile.Get(Key: String): PObject;
var
  I: LongInt;
begin
  if (Stream = nil) or (not Index.Search(@Key, I)) then Get:=nil else
  begin
    Stream^.Seek(BasePos + PResourceItem(Index.At(I))^.Posn);
    Get:=Stream^.Get;
  end;
end;

function TResourceFile.KeyAt(I: LongInt): String;
begin
  KeyAt:=PResourceItem(Index.At(I))^.Key;
end;

procedure TResourceFile.Put(Item: PObject; Key: String);
var
  I: LongInt;
  P: PResourceItem;
begin
  if Stream = Nil Then Exit;
  if Index.Search(@Key, I) then P:=Index.At(I) else
  begin
    GetMem(P, Length(Key) + (SizeOf(TResourceItem) - SizeOf(String) + 1));
    if (P <> nil) then begin
      P^.Key:=Key;
      Index.AtInsert(I, P);
    end;
  end;
  If (P <> nil) Then Begin
    P^.Posn:=IndexPos;
    Stream^.Seek(BasePos + IndexPos);
    Stream^.Put(Item);
    IndexPos:=Stream^.GetPos - BasePos;
    P^.Size:=IndexPos - P^.Posn;
    Modified:=True;
  end;
end;

function TResourceFile.SwitchTo(AStream: PStream; Pack: Boolean): PStream;
var
  NewBasePos: Longint;

  procedure DoCopyResource(Item: PResourceItem);
  {$IFDEF BIT_16} far; {$ENDIF}
  begin
    Stream^.Seek(BasePos + Item^.Posn);
    Item^.Posn:=AStream^.GetPos - NewBasePos;
    AStream^.CopyFrom(Stream^, Item^.Size);
  end;

begin
  SwitchTo:=Stream;
  If (AStream <> Nil) AND (Stream <> Nil) then begin
    NewBasePos:=AStream^.GetPos;
    if Pack then
    begin
      AStream^.Seek(NewBasePos + SizeOf(Longint) * 3);
      Index.ForEach(@DoCopyResource);
      IndexPos:=AStream^.GetPos - NewBasePos;
    end else
    begin
      Stream^.Seek(BasePos);
      AStream^.CopyFrom(Stream^, IndexPos);
    end;
    Stream:=AStream;
    Modified:=True;
    BasePos:=NewBasePos;
  end;
end;
*)
{ TStringList }

constructor TStringList.Load(var S: TStream);
  var
    Size: AWord;
  begin
  TObject.Init;
  Stream := @S;
  S.Read(Size, SizeOf(Size));
  BasePos := i32(S.GetPos);
  S.Seek(BasePos+Size);
  S.Read(IndexSize, SizeOf(IndexSize));
  GetMem(Index, IndexSize*SizeOf(TStrIndexRec));
  S.Read(Index^, IndexSize*SizeOf(TStrIndexRec));
  end;

destructor TStringList.Done;
  begin
  FreeMem(Index, IndexSize*SizeOf(TStrIndexRec));
  end;

function TStringList.Get(Key: AWord): String;
  var
    I: AWord;
    S: String;
  begin
  S := '';
  if  (IndexSize > 0) then
    begin
    I := 0;
    while (I < IndexSize) and (S = '') do
      begin
      if  ( (Key-Index^[I].Key) < Index^[I].Count) then
        ReadStr(S, Index^[I].Offset, Key-Index^[I].Key);
      Inc(I);
      end;
    end;
  Get := S;
  end;

procedure TStringList.ReadStr(var S: String; Offset, Skip: AWord);
  {
var
  B: Byte; }
  begin
  Stream^.Seek(BasePos+Offset);
  Stream^.Status := 0;
  Inc(Skip);
  repeat
    {Cat}
    (*
    Stream^.Read(B, 1);
    SetLength(S, B);
    Stream^.Read(S[1],B);
*)
    Stream^.ReadStrV(S);
    {/Cat}
    Dec(Skip);
  until Skip = 0;
  end;

procedure TSortedCollection.Sort;
  begin
  if Count <= 0 then
    Exit; {JO не удалять! иначе падаем по Ctrl-H и т.п.}
  QSort;
  end;

var
  P, T: Pointer;

procedure TSortedCollection.QuickSort(L, R: LongInt);
  var
    I, J: LongInt;
  begin
  repeat
    I := L;
    J := R;
    P := At((L+R) shr 1);
    repeat
      while Compare(At(I), P) < 0 do
        Inc(I);
      while Compare(At(J), P) > 0 do
        Dec(J);
      if I <= J then
        begin
        T := At(I);
        AtPut(I, At(J));
        AtPut(J, T);
        Inc(I);
        Dec(J);
        end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
  end { TSortedCollection.QuickSort };

procedure TSortedCollection.QSort;
  begin
  if Count > 0 then
    QuickSort(0, Count-1);
  end;

{NOT QSort}

{-DataCompBoy-}
function TDirCol.Compare;
  var
    a, b, c: Byte;
  begin
  Compare := 0;
  a := 0;
  for c := 1 to Length(PString(Key1)^) do
    if PString(Key1)^[c] in ['/', '\'] then
      Inc(a);
  b := 0;
  for c := 1 to Length(PString(Key2)^) do
    if PString(Key2)^[c] in ['/', '\'] then
      Inc(b);
  if a > b then
    Compare := -1
  else
    Compare := +1
  end;
{-DataCompBoy-}

{OldCollection !!!}

end.
