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
unit ListMakr;

interface

uses
  Defines, Objects2, Streams, Objects, ObjType
  ;

type
  { TStrListMaker object }

  PStrListMaker = ^TStrListMaker;
  TStrListMaker = object(TObject)
    constructor Init(AStrSize, AIndexSize: AWord);
    destructor Done; virtual;
    procedure Put(Key: AWord; S: String);
    procedure Store(var S: TStream);
  private
    StrPos: AWord;
    StrSize: AWord;
    Strings: PByteArray;
    IndexPos: AWord;
    IndexSize: AWord;
    Index: PStrIndex;
    Cur: TStrIndexRec;
    procedure CloseCurrent;
    end;

const
  RStrListMaker: TStreamRec = (
    ObjType: otStrListMaker;
    VmtLink: (TypeOf(TStrListMaker));
    Load: nil;
    Store: @TStrListMaker.Store);

implementation

{ TStrListMaker }

constructor TStrListMaker.Init(AStrSize, AIndexSize: AWord);
  begin
  inherited Init;
  StrSize := AStrSize;
  IndexSize := AIndexSize;
  GetMem(Strings, AStrSize);
  GetMem(Index, AIndexSize*SizeOf(TStrIndexRec));
  end;

destructor TStrListMaker.Done;
  begin
  FreeMem(Index, IndexSize*SizeOf(TStrIndexRec));
  FreeMem(Strings, StrSize);
  end;

procedure TStrListMaker.CloseCurrent;
  begin
  if Cur.Count <> 0 then
    begin
    Index^[IndexPos] := Cur;
    Inc(IndexPos);
    Cur.Count := 0;
    end;
  end;

{Cat: переделал для совместимости с AnsiString}
procedure TStrListMaker.Put(Key: AWord; S: String);
  begin
  if  (Cur.Count = 16) or (Key <> Cur.Key+Cur.Count) then
    CloseCurrent;
  if Cur.Count = 0 then
    begin
    Cur.Key := Key;
    Cur.Offset := StrPos;
    end;
  Inc(Cur.Count);
  Move(S, Strings^[StrPos], Length(S)+1);
  Inc(StrPos, Length(S)+1);
  end;
{/Cat}

procedure TStrListMaker.Store(var S: TStream);
  begin
  CloseCurrent;
  S.Write(StrPos, SizeOf(StrPos));
  S.Write(Strings^, StrPos);
  S.Write(IndexPos, SizeOf(IndexPos));
  S.Write(Index^, IndexPos*SizeOf(TStrIndexRec));
  end;

end.
