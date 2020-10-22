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

unit
HelpKern;

{ Used by TVHC and HELPFILE }

interface

uses
  Defines, Objects2, Streams
  ;

type

  THelpParameters = record
    Index: AWord;
    CursorXY,
    DeltaXY: TPoint;
    end;
  { TParagraph }

  PParagraph = ^TParagraph;
  TParagraph = record
    Next: PParagraph;
    Wrap: Boolean;
    Size: AWord;
    Text: record
      end;
    end;

  { THelpTopic }

  TCrossRef = record
    Ref: AWord;
    Offset: AInt;
    Length: Byte;
    end;

  PCrossRefs = ^TCrossRefs;
  TCrossRefs = array[1..10000] of TCrossRef;
  TCrossRefHandler = procedure (var S: TStream; XRefValue: AInt);

  PHelpTopic = ^THelpTopic;
  THelpTopic = object(TObject)
    constructor Init;
    constructor Load(var S: TStream);
    destructor Done; virtual;
    {DataCompBoy
    procedure AddCrossRef(Ref: TCrossRef);
}
    procedure AddParagraph(P: PParagraph);
    procedure GetCrossRef(I: AInt; var Loc: TPoint; var Length: Byte;
        var Ref: AWord);
    function GetLine(Line: AInt): String;
    function GetNumCrossRefs: AInt;
    function NumLines: AInt;
    procedure SetCrossRef(I: AInt; var Ref: TCrossRef);
    procedure SetNumCrossRefs(I: AInt);
    procedure SetWidth(AWidth: AInt);
    procedure Store(var S: TStream);
  private
    Paragraphs: PParagraph;
    NumRefs: AInt;
    CrossRefs: PCrossRefs;
    Width: AInt;
    LastOffset: AInt;
    LastLine: AInt;
    LastParagraph: PParagraph;
    function WrapText(var Text; Size: Integer; var Offset: Integer;
        Wrap: Boolean): String;
    end;

  { THelpIndex }

  PIndexArray = ^TIndexArray;
  TIndexArray = array[0..16380] of LongInt;

  PContextArray = ^TContextArray;
  TContextArray = array[0..16380] of AWord;

  PHelpIndex = ^THelpIndex;
  THelpIndex = object(TObject)
    constructor Init;
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function Position(I: AWord): LongInt;
    procedure Add(I: AWord; Val: LongInt);
    procedure Store(var S: TStream);
  private
    Size: AWord;
    Used: AWord;
    Contexts: PContextArray;
    Index: PIndexArray;
    function Find(I: AWord): AWord;
    end;

  { THelpFile }

  PHelpFile = ^THelpFile;
  THelpFile = object(TObject)
    Stream: PStream;
    Modified: Boolean;
    constructor Init(S: PStream);
    destructor Done; virtual;
    function GetTopic(I: AWord): PHelpTopic;
    function InvalidTopic: PHelpTopic;
    procedure RecordPositionInIndex(I: AInt);
    procedure PutTopic(Topic: PHelpTopic);
  private
    Index: PHelpIndex;
    IndexPos: LongInt;
    end;

procedure NotAssigned(var S: TStream; Value: AInt);

const
  CrossRefHandler: TCrossRefHandler = NotAssigned;

implementation
uses
  Advance1
  ;

{ THelpTopic }

constructor THelpTopic.Init;
  begin
  inherited Init;
  LastLine := High(LastLine);
  end;

constructor THelpTopic.Load(var S: TStream);

  procedure ReadParagraphs;
    var
      I, Size: AInt;
      PP: ^PParagraph;
    begin
    S.Read(I, SizeOf(I));
    PP := @Paragraphs;
    while I > 0 do
      begin
      S.Read(Size, SizeOf(Size));
      GetMem(PP^, SizeOf(PP^^)+Size);
      PP^^.Size := Size;
      S.Read(PP^^.Wrap, SizeOf(Boolean));
      S.Read(PP^^.Text, Size);
      PP := @PP^^.Next;
      Dec(I);
      end;
    PP^:= nil;
    end;

  procedure ReadCrossRefs;
    begin
    S.Read(NumRefs, SizeOf(AInt));
    GetMem(CrossRefs, SizeOf(TCrossRef)*NumRefs);
    S.Read(CrossRefs^, SizeOf(TCrossRef)*NumRefs);
    end;

  begin { THelpTopic.Load }
  ReadParagraphs;
  ReadCrossRefs;
  Width := 0;
  LastLine := High(LastLine);
  end { THelpTopic.Load };

destructor THelpTopic.Done;

  procedure DisposeParagraphs;
    var
      P, T: PParagraph;
    begin
    P := Paragraphs;
    while P <> nil do
      begin
      T := P;
      P := P^.Next;
      FreeMem(T, SizeOf(T^)+T^.Size);
      end;
    end;

  begin
  DisposeParagraphs;
  FreeMem(CrossRefs, SizeOf(TCrossRef)*NumRefs);
  inherited Done
  end;
{DataCompBoy
procedure THelpTopic.AddCrossRef(Ref: TCrossRef);
var
  P: PCrossRefs;
begin
  GetMem(P, (NumRefs+1) * SizeOf(TCrossRef));
  if NumRefs > 0 then
  begin
    Move(CrossRefs^, P^, NumRefs * SizeOf(TCrossRef));
    FreeMem(CrossRefs, NumRefs * SizeOf(TCrossRef));
  end;
  CrossRefs^[NumRefs] := Ref;
  Inc(NumRefs);
end;
}
procedure THelpTopic.AddParagraph(P: PParagraph);
  var
    PP: ^PParagraph;
  begin
  PP := @Paragraphs;
  while PP^ <> nil do
    PP := @PP^^.Next;
  PP^:= P;
  P^.Next := nil;
  end;

procedure THelpTopic.GetCrossRef(I: AInt; var Loc: TPoint;
    var Length: Byte; var Ref: AWord);
  var
    OldOffset, CurOffset, Offset, ParaOffset: Integer;
    P: PParagraph;
    Line: AInt;
  begin
  ParaOffset := 0;
  CurOffset := 0;
  OldOffset := 0;
  Line := 0;
  Offset := CrossRefs^[I].Offset;
  P := Paragraphs;
  while ParaOffset+CurOffset < Offset do
    begin
    OldOffset := ParaOffset+CurOffset;
    WrapText(P^.Text, P^.Size, CurOffset, P^.Wrap);
    Inc(Line);
    if CurOffset >= P^.Size then
      begin
      Inc(ParaOffset, P^.Size);
      P := P^.Next;
      CurOffset := 0;
      end;
    end;
  Loc.X := Offset-OldOffset-1;
  Loc.Y := Line;
  Length := CrossRefs^[I].Length;
  Ref := CrossRefs^[I].Ref;
  end { THelpTopic.GetCrossRef };

function THelpTopic.GetLine(Line: AInt): String;
  var
    Offset: Integer;
    I: AInt;
    P: PParagraph;
  begin
  if LastLine < Line then
    begin
    I := Line;
    Dec(Line, LastLine);
    LastLine := I;
    Offset := LastOffset;
    P := LastParagraph;
    end
  else
    begin
    P := Paragraphs;
    Offset := 0;
    LastLine := Line;
    end;
  GetLine := '';
  while (P <> nil) do
    begin
    while Offset < P^.Size do
      begin
      Dec(Line);
      GetLine := WrapText(P^.Text, P^.Size, Offset, P^.Wrap);
      if Line = 0 then
        begin
        LastOffset := Offset;
        LastParagraph := P;
        Exit;
        end;
      end;
    P := P^.Next;
    Offset := 0;
    end;
  GetLine := '';
  end { THelpTopic.GetLine };

function THelpTopic.GetNumCrossRefs: AInt;
  begin
  GetNumCrossRefs := NumRefs;
  end;

function THelpTopic.NumLines: AInt;
  var
    Offset: Integer;
    Lines: AInt;
    P: PParagraph;
  begin
  Offset := 0;
  Lines := 0;
  P := Paragraphs;
  while P <> nil do
    begin
    Offset := 0;
    while Offset < P^.Size do
      begin
      Inc(Lines);
      WrapText(P^.Text, P^.Size, Offset, P^.Wrap);
      end;
    P := P^.Next;
    end;
  NumLines := Lines;
  end;

procedure THelpTopic.SetCrossRef(I: AInt; var Ref: TCrossRef);
  begin
  if I <= NumRefs then
    CrossRefs^[I] := Ref;
  end;

procedure THelpTopic.SetNumCrossRefs(I: AInt);
  var
    P: PCrossRefs;
  begin
  if NumRefs = I then
    Exit;
  GetMem(P, I*SizeOf(TCrossRef));
  if NumRefs > 0 then
    begin
    if I > NumRefs then
      Move(CrossRefs^, P^, NumRefs*SizeOf(TCrossRef))
    else
      Move(CrossRefs^, P^, I*SizeOf(TCrossRef));
    FreeMem(CrossRefs, NumRefs*SizeOf(TCrossRef));
    end;
  CrossRefs := P;
  NumRefs := I;
  end;

procedure THelpTopic.SetWidth(AWidth: AInt);
  begin
  Width := AWidth;
  end;

procedure THelpTopic.Store(var S: TStream);

  procedure WriteParagraphs;
    var
      I: AInt;
      P: PParagraph;
    begin
    P := Paragraphs;
    I := 0;
    while P <> nil do
      begin
      Inc(I);
      P := P^.Next;
      end;
    S.Write(I, SizeOf(I));
    P := Paragraphs;
    while P <> nil do
      begin
      S.Write(P^.Size, SizeOf(P^.Size));
      S.Write(P^.Wrap, SizeOf(P^.Wrap));
      S.Write(P^.Text, P^.Size);
      P := P^.Next;
      end;
    end;

  procedure WriteCrossRefs;
    var
      I: AInt;
    begin
    S.Write(NumRefs, SizeOf(NumRefs));
    if @CrossRefHandler = @NotAssigned then
      S.Write(CrossRefs^, SizeOf(TCrossRef)*NumRefs)
    else
      for I := 1 to NumRefs do
        begin
        CrossRefHandler(S, CrossRefs^[I].Ref);
        with CrossRefs^[I] do
          S.Write(Offset, SizeOf(Offset)+SizeOf(Length));
        end;
    end;

  begin { THelpTopic.Store }
  WriteParagraphs;
  WriteCrossRefs;
  end { THelpTopic.Store };

{JO: взял THelpTopic.WrapText из VP 2.1 TVDEMO, так как в старой падала      }
{    регулярно ассемблерная функция Scan на repne   scasb ; решил, что       }
{    разбираться с этой хренью будет себе дороже и взял готовое решение      }
function THelpTopic.WrapText(var Text; Size: Integer;
    var Offset: Integer; Wrap: Boolean): String;
  type
    PCArray = ^CArray;
    CArray = array[0..32767] of Char;
  var
    Line: String;
    I, P: Integer;

  function IsBlank(Ch: Char): Boolean;
    begin
    IsBlank := (Ch = ' ') or (Ch = #13) or (Ch = #10);
    end;

{ Найти число символов до указанного включительно.
Если не найден - до последнего символа в буфере.
Size - это размер буфера, а не длина зоны поиска.
Зона поиска - от Offset до Size (исключительно), но
не более 256 символов. Не допускается Offset = Size.
}
  function Scan(var P; Offset, Size: Integer; C: Char): Integer;
    assembler;
    {&USES esi,edi} {&FRAME-}
  asm
        cld
        mov     edi,P
        add     edi,&Offset
        mov     edx,Size
        sub     edx,&Offset
        test    dh,dh
        jz      @@1
        mov     edx,256
      @@1:
        mov     ecx,edx
        mov     al,C
        repne   scasb
        sub     ecx,edx
        neg     ecx
        xchg    eax,ecx
end;

  procedure TextToLine(var Text; Offset, Length: Integer;
       var Line: String);
    assembler;
    {&USES esi,edi} {&FRAME-}
  asm
        cld
        mov     esi,Text
        add     esi,&Offset
        mov     edi,Line
        mov     eax,Length
        stosb
        xchg    eax,ecx
        rep     movsb
end;

  begin { THelpTopic.WrapText }
  I := Scan(Text, Offset, Size, #13);
  if  (I >= Width) and Wrap then
    begin
    I := Offset+Width-1;
    if I > Size then
      I := Size
    else
      begin
      while (I > Offset) and not IsBlank(PCArray(@Text)^[I]) do
        Dec(I);
      if I = Offset then
        I := Offset+Width
      else
        Inc(I);
      end;
    if I = Offset then
      I := Offset+Width;
    Dec(I, Offset);
    end;
  TextToLine(Text, Offset, I, Line);
  if Line[Length(Line)] = #13 then
    SetLength(Line, Length(Line)-1);
  Inc(Offset, I);
  WrapText := Line;
  end { THelpTopic.WrapText };

{ THelpIndex }

constructor THelpIndex.Init;
  begin
  inherited Init;
  Size := 0;
  Contexts := nil;
  Index := nil;
  end;

constructor THelpIndex.Load(var S: TStream);
  begin
  S.Read(Used, SizeOf(Used));
  S.Read(Size, SizeOf(Size));
  if Size = 0 then
    begin
    Contexts := nil;
    Index := nil;
    end
  else
    begin
    GetMem(Contexts, SizeOf(Contexts^[0])*Size);
    S.Read(Contexts^, SizeOf(Contexts^[0])*Size);
    GetMem(Index, SizeOf(Index^[0])*Size);
    S.Read(Index^, SizeOf(Index^[0])*Size);
    end;
  end;

destructor THelpIndex.Done;
  begin
  FreeMem(Index, SizeOf(Index^[0])*Size);
  FreeMem(Contexts, SizeOf(Contexts^[0])*Size);
  inherited Done;
  end;

function THelpIndex.Find(I: AWord): AWord;
  var
    Hi, Lo, Pos: AInt;
  begin
  Lo := 0;
  Hi := Used-1;
  while Lo <= Hi do
    begin
    Pos := (Lo+Hi) div 2;
    if I > Contexts^[Pos] then
      Lo := Pos+1
    else
      begin
      Hi := Pos-1;
      if I = Contexts^[Pos] then
        Lo := Pos;
      end;
    end;
  Find := Lo;
  end;

function THelpIndex.Position(I: AWord): LongInt;
  begin
  Position := Index^[Find(I)];
  end;

procedure THelpIndex.Add(I: AWord; Val: LongInt);
  const
    Delta = 10;
  var
    P: PIndexArray;
    NewSize: AInt;
    Pos: AInt;

  function Grow(P: Pointer; OldSize, NewSize, ElemSize: AInt): Pointer;
    var
      NewP: PByteArray;
    begin
    GetMem(NewP, NewSize*ElemSize);
    if NewP <> nil then
      begin
      if P <> nil then
        Move(P^, NewP^, OldSize*ElemSize);
      FillChar(NewP^[OldSize*ElemSize], (NewSize-Size)*ElemSize, $FF);
      end;
    if OldSize > 0 then
      FreeMem(P, OldSize*ElemSize);
    Grow := NewP;
    end;

  begin { THelpIndex.Add }
  Pos := Find(I);
  if  (Contexts = nil) or (Contexts^[Pos] <> I) then
    begin
    Inc(Used);
    if Used >= Size then
      begin
      NewSize := (Used+Delta) div Delta*Delta;
      Contexts := Grow(Contexts, Size, NewSize, SizeOf(Contexts^[0]));
      Index := Grow(Index, Size, NewSize, SizeOf(Index^[0]));
      Size := NewSize;
      end;
    if Pos < Used then
      begin
      Move(Contexts^[Pos], Contexts^[Pos+1], (Used-Pos-1)*
        SizeOf(Contexts^[0]));
      Move(Index^[Pos], Index^[Pos+1], (Used-Pos-1)*
        SizeOf(Index^[0]));
      end;
    end;
  Contexts^[Pos] := I;
  Index^[Pos] := Val;
  end { THelpIndex.Add };

procedure THelpIndex.Store(var S: TStream);
  begin
  S.Write(Used, SizeOf(Used));
  S.Write(Size, SizeOf(Size));
  S.Write(Contexts^, SizeOf(Contexts^[0])*Size);
  S.Write(Index^, SizeOf(Index^[0])*Size);
  end;

{ THelpFile }

const
  MagicHeader = $46484246; {'FBHF'}

constructor THelpFile.Init(S: PStream);
  var
    Magic: LongInt;
  begin
  Magic := 0;
  S^.Seek(0);
  if S^.GetSize > SizeOf(Magic) then
    S^.Read(Magic, SizeOf(Magic));
  if Magic <> MagicHeader then
    begin
    IndexPos := 12;
    S^.Seek(IndexPos);
    Index := New(PHelpIndex, Init);
    Modified := True;
    end
  else
    begin
    S^.Seek(8);
    S^.Read(IndexPos, SizeOf(IndexPos));
    S^.Seek(IndexPos);
    Index := PHelpIndex(S^.Get);
    Modified := False;
    end;
  Stream := S;
  end { THelpFile.Init };

destructor THelpFile.Done;
  var
    Magic, Size{!!s}: LongInt;
  begin
  if Modified then
    begin
    Stream^.Seek(IndexPos);
    Stream^.Put(Index);
    Stream^.Seek(0);
    Magic := MagicHeader;
    Size := i32(Stream^.GetSize-8);
    Stream^.Write(Magic, SizeOf(Magic));
    Stream^.Write(Size, SizeOf(Size));
    Stream^.Write(IndexPos, SizeOf(IndexPos));
    end;
  Dispose(Stream, Done);
  Stream := nil;
  Dispose(Index, Done);
  Index := nil;
  end;

function THelpFile.GetTopic(I: AWord): PHelpTopic;
  var
    Pos: LongInt;
  begin
  Pos := Index^.Position(I);
  if Pos > 0 then
    begin
    Stream^.Seek(Pos);
    GetTopic := PHelpTopic(Stream^.Get);
    end
  else
    GetTopic := InvalidTopic;
  end;

function THelpFile.InvalidTopic: PHelpTopic;
  var
    Topic: PHelpTopic;
    Para: PParagraph;
  const
    InvalidStr = #13' No help available in this context.';
    InvalidText: array[1..Length(InvalidStr)] of Char = InvalidStr;
  begin
  Topic := New(PHelpTopic, Init);
  GetMem(Para, SizeOf(Para^)+SizeOf(InvalidText));
  Para^.Size := SizeOf(InvalidText);
  Para^.Wrap := False;
  Para^.Next := nil;
  Move(InvalidText, Para^.Text, SizeOf(InvalidText));
  Topic^.AddParagraph(Para);
  InvalidTopic := Topic;
  end;

procedure THelpFile.RecordPositionInIndex(I: AInt);
  begin
  Index^.Add(I, IndexPos);
  Modified := True;
  end;

procedure THelpFile.PutTopic(Topic: PHelpTopic);
  begin
  Stream^.Seek(IndexPos);
  Stream^.Put(Topic);
  IndexPos := i32(Stream^.GetPos){!!s};
  Modified := True;
  end;

procedure NotAssigned(var S: TStream; Value: AInt);
  begin
  end;

end.
