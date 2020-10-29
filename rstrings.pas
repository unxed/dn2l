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

{(c) Necromancer's Dos Navigator Team/Necromancer}
unit RStrings;

interface

uses
  Defines, objects, Objects2, Streams, Advance6, Commands
  ;

type
  TOffsetType = LongInt;

  PIndexArray = ^TIndexArray;
  TIndexArray = array[0..65520 div SizeOf(TOffsetType)-1] of TOffsetType;

  PIdxResource = ^TIdxResource;
  TIdxResource = object(TObject)
    Stream: PStream;
    Index: PIndexArray;
    Count: AInt;
    constructor Init(AStream: PStream);
    destructor Done; virtual;
    function Get(Key: TDlgIdx): PObject;
    end;

  PIdxMaker = ^TIdxMaker;
  TIdxMaker = object(TObject)
    Stream: PStream;
    TempStream: PBufStream;
    Index: PIndexArray;
    Count: AInt;
    constructor Init(AStream: PStream);
    destructor Done; virtual;
    procedure Put(Item: PObject; Key: TDlgIdx);
    function Empty(Key: TDlgIdx): Boolean;
    end;

  {----------------------------------------------------------------------------}
implementation
uses
  DNUtil, Advance1
  ;
{----------------------------------------------------------------------------}
const
  TempStreamName = '$MAKERES.TMP';

constructor TIdxResource.Init;
  begin
  Stream := AStream;
  AStream^.Read(Count, SizeOf(Count));
  GetMem(Index, Count*SizeOf(TOffsetType));
  AStream^.Read(Index^, Count*SizeOf(TOffsetType));
  end;

destructor TIdxResource.Done;
  begin
  FreeMem(Index, Count*SizeOf(TOffsetType));
  Dispose(Stream, Done);
  end;

function TIdxResource.Get;

  procedure Chk;
    begin
    if Stream^.Status <> stOK then
      ResourceAccessError
    end;

  begin
  Chk;
  Stream^.Seek(Index^[Integer(Key)]);
  Chk;
  Get := Stream^.Get;
  Chk;
  end;

constructor TIdxMaker.Init;
  begin
  New(TempStream, Init(TempStreamName, stCreate, 1024));
  Stream := AStream;
  Count := 0;
  GetMem(Index, 65520);
  FillChar(Index^, 65520, $FF);
  end;

procedure TIdxMaker.Put;
  begin
  if Count <= Integer(Key) then
    Count := Integer(Key)+1;
  Index^[Integer(Key)] := i32(TempStream^.GetPos);
  TempStream^.Put(Item);
  if TempStream^.Status <> stOK then
    begin
    Writeln('Cannot write object #', Integer(Key));
    Halt(2);
    end;
  end;

function TIdxMaker.Empty;
  begin
  Empty := Index^[Integer(Key)] = -1
  end;

destructor TIdxMaker.Done;
  var
    I: LongInt;
    IdxSize: LongInt;
    MaxPos: LongInt;
    B: Byte;
    F: file;
  begin
  IdxSize := Count*SizeOf(TOffsetType);
  for I := 0 to Count-1 do
    Inc(Index^[I], IdxSize+SizeOf(Count));
  Stream^.Write(Count, SizeOf(Count));
  Stream^.Write(Index^, Count*SizeOf(TOffsetType));
  MaxPos := i32(TempStream^.GetPos);
  TempStream^.Seek(0);
  for I := 0 to MaxPos do
    begin
    TempStream^.Read(B, 1);
    Stream^.Write(B, 1);
    end;
  if Stream^.Status <> stOK then
    Writeln('Status is invalid');
  Dispose(Stream, Done);
  Dispose(TempStream, Done);
  FreeMem(Index, 65520);
  Assign(F, TempStreamName);
  Erase(F);
  inherited Done;
  end { TIdxMaker.Done };

end.
