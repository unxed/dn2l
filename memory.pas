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
unit Memory;

{-----------------------------------------------------}
{ This module is based on Turbo Vision Memry Unit     }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}

interface

const
  MaxHeapSize {: Word} = 655360 div 16; { 640K }
  LowMemSize {: Word} = 4096; {   4K }
  MaxBufMem {: Word} = 65536 div 16; {  64K }

procedure InitMemory;
procedure DoneMemory;
procedure InitDOSMem;
procedure DoneDOSMem;
function LowMemory: Boolean;
function MemAlloc(Size: Word): Pointer;
function MemAllocSeg(Size: Word): Pointer;
procedure NewCache(var P: Pointer; Size: Word);
procedure DisposeCache(P: Pointer);

implementation

type
  PtrRec = record
    Ofs: LongInt;
    end;

type
  PCache = ^TCache;
  TCache = record
    Next: PCache;
    Master: ^Pointer;
    Size: Word;
    Data: record
      end;
    end;

  PBuffer = ^TBuffer;
  TBuffer = record
    Next: PBuffer;
    Size: Word;
    Data: record
      end;
    end;

const
  CacheList: PCache = nil;
  SafetyPool: Pointer = nil;
  BufferList: PBuffer = nil;
  SafetyPoolSize: Word = 0;
  DisablePool: Boolean = False;

function FreeCache: Boolean;
  begin
  FreeCache := False;
  if CacheList <> nil then
    begin
    DisposeCache(CacheList^.Next^.Master^);
    FreeCache := True;
    end;
  end;

function FreeSafetyPool: Boolean;
  begin
  FreeSafetyPool := False;
  if SafetyPool <> nil then
    begin
    FreeMem(SafetyPool, SafetyPoolSize);
    SafetyPool := nil;
    FreeSafetyPool := True;
    end;
  end;

function HeapNotify(Size: Word): Integer;
  begin
  if FreeCache then
    HeapNotify := 2
  else if DisablePool then
    HeapNotify := 1
  else if FreeSafetyPool then
    HeapNotify := 2
  else
    HeapNotify := 0;
  end;

procedure InitMemory;
  begin
  HeapError := @HeapNotify;
  SafetyPoolSize := LowMemSize*16;
  LowMemory;
  end;

procedure DoneMemory;
  begin
  while FreeCache do
    ;
  FreeSafetyPool;
  end;

procedure InitDOSMem;
  begin
  end;

procedure DoneDOSMem;
  begin
  end;

function LowMemory: Boolean;
  begin
  LowMemory := False;
  if SafetyPool = nil then
    begin
    SafetyPool := MemAlloc(SafetyPoolSize);
    if SafetyPool = nil then
      LowMemory := True;
    end;
  end;

function MemAlloc(Size: Word): Pointer;
  var
    P: Pointer;
  begin
  DisablePool := True;
  GetMem(P, Size);
  DisablePool := False;
  MemAlloc := P;
  end;

procedure NewCache(var P: Pointer; Size: Word);
  var
    Cache: PCache;
  begin
  Inc(Size, SizeOf(TCache));
  if MaxAvail >= Size then
    GetMem(Cache, Size)
  else
    Cache := nil;
  if Cache <> nil then
    begin
    if CacheList = nil then
      Cache^.Next := Cache
    else
      begin
      Cache^.Next := CacheList^.Next;
      CacheList^.Next := Cache;
      end;
    CacheList := Cache;
    Cache^.Master := @P;
    Cache^.Size := Size;
    Inc(PtrRec(Cache).Ofs, SizeOf(TCache));
    end;
  P := Cache;
  end { NewCache };

procedure DisposeCache(P: Pointer);
  var
    Cache, C: PCache;
  begin
  PtrRec(Cache).Ofs := PtrRec(P).Ofs-SizeOf(TCache);
  C := CacheList;
  while (C^.Next <> Cache) and (C^.Next <> CacheList) do
    C := C^.Next;
  if C^.Next = Cache then
    begin
    if C = Cache then
      CacheList := nil
    else
      begin
      if CacheList = Cache then
        CacheList := C;
      C^.Next := Cache^.Next;
      end;
    Cache^.Master^:= nil;
    FreeMem(Cache, Cache^.Size);
    end;
  end;

procedure NewBuffer(var P: Pointer; Size: Word);
  var
    Buffer: PBuffer;
  begin
  Inc(Size, SizeOf(TBuffer));
  Buffer := MemAlloc(Size);
  if Buffer <> nil then
    begin
    Buffer^.Next := BufferList;
    Buffer^.Size := Size;
    BufferList := Buffer;
    Inc(PtrRec(Buffer).Ofs, SizeOf(TBuffer));
    end;
  P := Buffer;
  end;

procedure DisposeBuffer(P: Pointer);
  var
    Buffer, PrevBuf: PBuffer;
  begin
  if P <> nil then
    begin
    Dec(PtrRec(P).Ofs, SizeOf(TBuffer));
    Buffer := BufferList;
    PrevBuf := nil;
    while (Buffer <> nil) and (P <> Buffer) do
      begin
      PrevBuf := Buffer;
      Buffer := Buffer^.Next;
      end;
    if Buffer <> nil then
      begin
      if PrevBuf = nil then
        BufferList := Buffer^.Next
      else
        PrevBuf^.Next := Buffer^.Next;
      FreeMem(Buffer, Buffer^.Size);
      end;
    end;
  end { DisposeBuffer };

function GetBufferSize(P: Pointer): Word;
  begin
  if P = nil then
    GetBufferSize := 0
  else
    begin
    Dec(PtrRec(P).Ofs, SizeOf(TBuffer));
    GetBufferSize := PBuffer(P)^.Size;
    end;
  end;

function SetBufferSize(P: Pointer; Size: Word): Boolean;
  begin
  SetBufferSize := False;
  end;

function MemAllocSeg(Size: Word): Pointer;
  begin
  MemAllocSeg := MemAlloc(Size);
  end;

end.
