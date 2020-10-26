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
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

unit Advance6;

interface

uses
  Defines
  ;

procedure MakeCRCTable;
function GetLineNumberForOffset(const FName: String; Offset: LongInt)
  : LongInt;
function GetOffsetForLineNumber
  {` При установке номера, превышающего размер файла, результат -1.
   Эта функция правильно работает, если разделитель строк - Lf или
   CrLf. А голый Cr в качестве разделителя она не понимает, то есть
   выдаст -1.
     Ни анализа IOResult в процессе, ни ClrIO в конце не делается. }
    (const FName: String; LineNm: LongInt): LongInt;
  {`}

procedure ResourceAccessError;
function HotKey(const S: String): Char;

implementation
uses
  UKeyMap, Country,
  VpSysLow,
  Advance, Advance1, Lfnvp, VideoMan
  ;

function GetLineNumberForOffset(const FName: String; Offset: LongInt)
  : LongInt;
  var
    F: lFile;
    q: PByteArray;
    bl: Integer;
    ln: LongInt;
    fp: LongInt;
    bp: LongInt;
  begin
  GetMem(q, 4096);
  lAssignFile(F, FName);
  ln := 1;
  fp := 0;
  lResetFileReadOnly(F, 1);
  repeat
    BlockRead(F.F, q^, 4096, bl);
    bp := 0;
    repeat
      if q^[bp] = 10 then
        Inc(ln);
      Inc(bp);
      Inc(fp);
    until (bp >= bl) or (fp >= Offset)
  until fp >= Offset;
  GetLineNumberForOffset := ln;
  Close(F.F);
  FreeMem(q, 4096);
  end { GetLineNumberForOffset };

function GetOffsetForLineNumber(const FName: String; LineNm: LongInt)
  : LongInt;
  var
    F: lFile;
    q: PByteArray;
    bl: Integer;
    ln: LongInt;
    fp: LongInt;
    bp: LongInt;
  begin
  GetMem(q, 4096);
  lAssignFile(F, FName);
  ln := 1;
  fp := 0;
  lResetFileReadOnly(F, 1);
  while (ln < LineNm) and not Eof(F.F) do
    begin
    BlockRead(F.F, q^, 4096, bl);
    bp := 0;
    while (bp < bl) and (ln < LineNm) do
      begin
      if q^[bp] = 10 then
        Inc(ln);
      Inc(bp);
      Inc(fp);
      end;
    end;
  if ln >= LineNm then
    GetOffsetForLineNumber := fp
  else
    GetOffsetForLineNumber := -1;
  Close(F.F);
  FreeMem(q, 4096);
  end { GetOffsetForLineNumber };

procedure ResourceAccessError;
  begin
  {Cat}
  if IOResult <> 0 then
    ;
  ClearScreen;
  Writeln('Resource access error');
  SysTVInitCursor;
  Halt(219);
  {/Cat}
  {RunError(219);}
  end;

function HotKey(const S: String): Char;
  var
    P: Word;
  begin
  {
  P := Pos('~', S);
  if P <> 0 then
    HotKey := UpCaseArray[S[P+1]]
  else
    HotKey := #0;
  }
  // generates fatal errors
  // fixme: commented by unxed
  end;

procedure MakeCRCTable;
  var
    c: LongInt;
    n, k: Integer;
    poly: LongInt; { polynomial exclusive-or pattern }

  const
    { terms of polynomial defining this crc (except x^32): }
    p: array[0..13] of Byte = (0, 1, 2, 4, 5, 7, 8, 10, 11, 12, 16, 22,
       23, 26);

  begin
  New(Crc_Table);
  { make exclusive-or pattern from polynomial ($EDB88320) }
  poly := 0;
  for n := 0 to (SizeOf(p) div SizeOf(Byte))-1 do
    poly := poly or (LongInt(1) shl (31-p[n]));

  for n := 0 to 255 do
    begin
    c := n;
    for k := 0 to 7 do
      begin
      if  (c and 1) <> 0 then
        c := poly xor (c shr 1)
      else
        c := (c shr 1);
      end;
    Crc_Table^[n] := c;
    end;
  Crc_Table_Empty := False;
  end { MakeCRCTable };

end.
