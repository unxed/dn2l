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
{Cat:todo}
{$UNDEF NOASM} {Cat: иначе не работает}

unit HistList;

{-----------------------------------------------------}
{ This module is based on Turbo Vision HistList Unit  }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}

{$F+,X+,I-,S-}

{****************************************************************************
   History buffer structure:

    Byte Byte String          Byte Byte String
    +-------------------------+-------------------------+--...--+
    | 0 | Id | History string | 0 | Id | History string |       |
    +-------------------------+-------------------------+--...--+

 ***************************************************************************}

interface

uses
  Defines
  ;

const
  HistoryBlock: PByteArray = nil;
  HistorySize: Word = 2048;
  HistoryUsed: Word = 0;
  MaxHistorySize: Word = 20;

procedure HistoryAdd(Id: Byte; const Str: String);
function HistoryCount(Id: Byte): Word;
function HistoryStr(Id: Byte; Index: Integer): String;
procedure DeleteHistoryStr(Id: Byte; Index: Integer);
procedure ClearHistory;

procedure InitHistory;
procedure DoneHistory;

{DataCompBoy
procedure StoreHistory(var S: TStream);
procedure LoadHistory(var S: TStream);
}
var
  CurId: Byte;
  CurString: PString;
  {$IFDEF NOASM}
  CurPos: Word;
  {$ENDIF}

implementation
uses
  Memory
  ;

{ Advance CurString to next string with an ID of CurId }

procedure AdvanceStringPointer;
  {$IFNDEF NOASM}
  {$USES ESI, EDX, EBX, ECX} {$FRAME-}
  assembler;
asm
        MOV     ECX,HistoryUsed
        MOV     BL, CurId
        MOV     ESI,CurString
        XOR     EAX, EAX
        MOV     EDX,[HistoryBlock]
        OR      EDX,ESI
        JZ      @@3
        CLD
        JMP     @@2
 @@1:   LODSW
        CMP     AH,BL { BL = CurId }
        JE      @@3
 @@2:   LODSB
        XOR     AH, AH
        ADD     ESI,EAX
        CMP     ESI,ECX { ECX = HistoryUsed }
        JB      @@1
        XOR     ESI,ESI
        MOV     EDX,ESI
 @@3:   MOV     CurString,ESI
 end;
  {$ELSE NOASM}
  begin { AdvanceStringPointer }
  if HistoryBlock = nil then
    Exit;
  repeat
    CurPos := CurPos+Length(CurString^)+3;
    if CurPos >= HistoryUsed then
      begin
      CurPos := -1;
      Break;
      end;
  until HistoryBlock^[CurPos+1] = CurId;
  if CurPos >= 0
  then
    CurString := @(HistoryBlock^[CurPos])
  else
    CurString := nil;
  end { AdvanceStringPointer };
{$ENDIF}

{ Deletes the current string from the table }

procedure DeleteString;
  {$IFNDEF NOASM}
  {&Frame-} {$USES ESI, EDI, ECX}
  assembler;
asm
        MOV     ECX,HistoryUsed
        CLD
        MOV     EDI,CurString
        MOV     ESI,EDI
        DEC     EDI
        DEC     EDI
        XOR     EAX,EAX
        MOV     AL, BYTE PTR [ESI]
        INC     EAX
        ADD     ESI,EAX
        SUB     ECX,ESI
        REP     MOVSB
        MOV     HistoryUsed,EDI
 end;
  {$ELSE}
  var
    q: Word;
  begin { DeleteString }
  q := CurPos+Length(CurString^)+3;
  if q >= HistoryUsed then
    begin
    HistoryUsed := CurPos;
    Exit;
    end;
  Move(HistoryBlock^[q], HistoryBlock^[CurPos], HistoryUsed-q);
  HistoryUsed := q;
  end { DeleteString };
{$ENDIF}

{ Insert a string into the table }

procedure InsertString(Id: Byte; const Str: String);
  {$IFNDEF NOASM}
  {&Frame-} {$USES EDX, EDI, ESI, EBX, ECX}
  assembler;
asm
        STD

        { Position EDI to the end the buffer  }
        {          EDX to beginning of buffer }
        MOV     EDX,HistoryBlock
        MOV     EDI,HistoryUsed
        MOV     ESI,Str
        XOR     EBX,EBX
        MOV     BL,[ESI]
        INC     EBX
        INC     EBX
        INC     EBX
 @@1:   MOV     EAX,EDI
        ADD     EAX,EBX
        SUB     EAX,EDX { EDX = HistoryBlock }
        CMP     EAX,HistorySize
        JB      @@2

        { Drop the last string off the end of the list }
        DEC     EDI
        XOR     AL,AL
        MOV     ECX,0FFFFH
        REPNE   SCASB
        INC     EDI
        JMP     @@1

        { Move the table down the size of the string }
 @@2:   MOV     ESI,EDI
        ADD     EDI,EBX
        MOV     HistoryUsed,EDI
        MOV     ECX,ESI
        SUB     ECX,EDX { EDX = HistoryBlock }
        REP     MOVSB

        { Copy the string into the position }
        CLD
        MOV     EDI,EDX { EDX = HistoryBlock.Word[0] }
        INC     EDI
        XOR     AX,AX
        MOV     AH,Id
        STOSW
        MOV     ESI,Str
        LODSB
        STOSB
        XOR     ECX,ECX
        MOV     CL,AL
        REP     MOVSB
        CLD // MS Windows do not like the direction flag (AK155)
 end;
  {$ELSE}
  var
    q: Word;
  begin { InsertString }
  if HistoryUsed+Length(Str)+3 >= HistorySize then
    begin
    q := CurPos;
    while CurPos+Length(CurString^)+3 < HistoryUsed do
      begin
      CurPos := CurPos+Length(CurString^)+3;
      CurString := @(HistoryBlock^[q]);
      end;
    DeleteString;
    CurPos := q;
    end;
  q := HistorySize;
  HistoryBlock^[q] := 0;
  HistoryBlock^[q+1] := Id;
  Move(Str, HistoryBlock^[q+2], Length(Str)+1);
  HistoryUsed := HistoryUsed+Length(Str)+3;
  end { InsertString };
{$ENDIF}

procedure StartId(Id: Byte);
  begin
  CurId := Id;
  CurString := @(HistoryBlock^);
  end;

function HistoryCount(Id: Byte): Word;
  var
    Count: Word;
  begin
  StartId(Id);
  Count := 0;
  AdvanceStringPointer;
  while CurString <> nil do
    begin
    Inc(Count);
    AdvanceStringPointer;
    end;
  HistoryCount := Count;
  end;

procedure HistoryAdd(Id: Byte; const Str: String);
  var
    C: Char;
    I, J: Integer;
  begin
  if Str = '' then
    Exit;
  if (HistoryUsed - Word(HistoryBlock)) < 3 then
    HistoryUsed := Word(HistoryBlock) + 1;

  I := 0;
  StartId(Id);

  C := ' ';
  { Delete duplicates }
  AdvanceStringPointer;
  while CurString <> nil do
    begin
    if Str = Copy(CurString^, 1, Length(CurString^)-1) then
      begin
      C := CurString^[Length(CurString^)];
      DeleteString;
      Dec(I);
      end;
    Inc(I);
    AdvanceStringPointer;
    end;

  if I > MaxHistorySize-1 then
    for J := I-1 downto 0 do
      begin
      HistoryStr(Id, J);
      if CurString = nil then
        Break;
      if CurString^[Length(CurString^)] <> '+' then
        begin
        DeleteString;
        Dec(I);
        Inc(J);
        if I < MaxHistorySize then
          Break;
        end;
      end;
  {
  if HistoryUsed < Length(Str) + 4 then
    begin
      CurString := HistoryBlock;
      Inc(LongInt(CurString), PtrRec(HistoryBlock).Ofs+1);
      while (LongInt(CurString)-LongInt(HistoryBlock) < HistoryUsed)
            and (HistoryUsed < Length(Str) + 4) do
        begin
          if CurString^[Length(CurString^)] <> '+' then DeleteString;
          Inc(LongInt(CurString), Length(CurString^)+3);
        end;

    end;
 }
  CurString := nil;

  InsertString(Id, Str+C);
  end { HistoryAdd };

function HistoryStr(Id: Byte; Index: Integer): String;
  var
    I: Integer;
  begin
  StartId(Id);
  for I := 0 to Index do
    AdvanceStringPointer;
  if CurString <> nil then
    HistoryStr := Copy(CurString^, 1, Length(CurString^)-1)
  else
    HistoryStr := '';
  end;

procedure ClearHistory;
  begin
  FillChar(HistoryBlock^, HistorySize, 0);
  PChar(HistoryBlock)^:= #0;
  HistoryUsed := Word(HistoryBlock)+1;
  end;

{DataCompBoy
procedure StoreHistory(var S: TStream);
var
  Size: Word;
begin
  Size := HistoryUsed - PtrRec(HistoryBlock).Ofs;
  S.Write(Size, SizeOf(Word));
  S.Write(HistoryBlock^, Size);
end;

procedure LoadHistory(var S: TStream);
var
  Size, I: Word;
begin
  S.Read(Size, SizeOf(Word));
  if HistoryBlock = nil then InitHistory;
  if HistorySize < Size then I := Size - HistorySize else I := 0;
  S.Read(HistoryBlock^, Size - I);
  S.Seek(S.GetPos + I);
  HistoryUsed := PtrRec(HistoryBlock).Ofs + Size;
end;
}
procedure InitHistory;
  begin
  HistoryBlock := MemAlloc(HistorySize);
  ClearHistory;
  end;

procedure DoneHistory;
  begin
  FreeMem(HistoryBlock, HistorySize);
  HistoryBlock := nil;
  end;

procedure DeleteHistoryStr(Id: Byte; Index: Integer);
  begin
  HistoryStr(Id, Index);
  if CurString <> nil then
    DeleteString;
  end;

end.
