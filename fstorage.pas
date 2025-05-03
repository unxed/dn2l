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

unit FStorage;

interface

uses
  Objects2, Streams, Defines, FilesCol
  ;

type
  TStoredID = (siUndefined, siEmpty, siDirectory, siFile, siFileDir,
     siEnd);

  TStored = record
    Id: TStoredID;
    len: Word; {DataCompBoy}
    { 8((( }
    end;

  {-DataCompBoy-}
  TStoredFile = record
    T: TStored;
    Size: TSize;
    CSize: TSize;
    Date: LongInt;
    Attr: Byte;
    Name: String;
    end;
  {JO: имя здесь должно обязательно быть в конце, т.к. мы сохраняем не всю }
  {    длину String, а только реальную длину имени                         }
  {-DataCompBoy-}

  TSwapLevel = (slNone, slCnv, slFail);

  PDirStorage = ^TDirStorage;
  TDirStorage = object(TObject)
    SwapLevel: TSwapLevel;
    Dirs: LongInt;
    Files: LongInt;
    Items: LongInt;
    Stream: PStream;
    FilePtr: LongInt;
    CurFile: TStoredFile;
    Last: Boolean;
    CurDir, LastDir: String;
    CurPos: LongInt;
    TotalLength, TotalCLength: TSize;
    vSavePos, vSize{!!s}: LongInt;
    constructor Init;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    destructor Done; virtual;
    procedure AddFile(FName: String; Size, CSize: TSize; Date: LongInt;
         Attr: Byte);
    procedure ResetPointer(const Dir: String);
    function GetNextFile: Boolean;
    procedure UpdateRecord;
    procedure DeleteFile(P: PFileRec);
    procedure InitStream(X, E, M: LongInt);
    procedure Truncate(N: LongInt);
    procedure WipeCur;
    procedure vWrite(var Buf; Count: Word);
    procedure vRead(var Buf; Count: Word);
    procedure vSeek(Pos: LongInt);
    procedure vTruncate;
    procedure FixError;
    procedure TryStream(Sz: LongInt);
    end;

const
  TStoredFixLength = SizeOf(TStoredFile)-SizeOf(String);

implementation
uses
  Startup, Lfnvp, Files, VPUtils, Advance1, Advance2, Dos
  ;

procedure __Error;
  begin
  RunError(224);
  end;

procedure TDirStorage.TryStream;
  begin
  repeat
    SwapLevel := Succ(SwapLevel);
    if SwapLevel = slFail then
      __Error;
    if Sz = -1 then
      InitStream(4096, 16384, 2048)
    else
      InitStream(Sz, Sz, Sz);
  until Stream <> nil;
  end;

constructor TDirStorage.Init;
  begin
  inherited Init;
  SwapLevel := slNone;

  TryStream(-1);

  TotalLength := 0;
  TotalCLength := 0;
  vSize := 0;
  vSavePos := 0;
  end;

procedure TDirStorage.FixError;
  label
    LLL;
  const
    BufSize = 512;
  var
    OldStream: PStream;
    SavePos, Count: LongInt;
    Buffer: array[0..BufSize-1] of Byte;
    N: Word;
  begin
  OldStream := Stream;
  OldStream^.Status := stOK;

LLL:

  TryStream(-1);

  Count := vSize;
  OldStream^.Seek(0);
  while Count > 0 do
    begin
    if Count > BufSize then
      N := BufSize
    else
      N := Count;
    OldStream^.Read(Buffer, N);
    if OldStream^.Status <> stOK then
      __Error;
    Stream^.Write(Buffer, N);
    if Stream^.Status <> stOK then
      begin
      Dispose(Stream, Done);
      Stream := nil;
      goto LLL
      end;
    Dec(Count, N);
    end;
  Dispose(OldStream, Done);
  Stream^.Seek(vSavePos);
  end { TDirStorage.FixError };

procedure TDirStorage.InitStream;
  begin
  Stream := nil;
  Last := True;
  CurPos := -1;
  case SwapLevel of
    slCnv:
      begin
      Stream := New(PMemoryStream, Init(M, 2048));
      if Stream^.Status <> stOK then
        begin
        Dispose(Stream, Done);
        Stream := nil
        end;
      end;
  end {case};
  end { TDirStorage.InitStream };

destructor TDirStorage.Done;
  begin
  if Stream <> nil then
    Dispose(Stream, Done);
  Stream := nil;
  end;

{-DataCompBoy-}
procedure TDirStorage.AddFile;
  var
    Dr: String;
    Nm: String;
    Xt: String;
    SF: TStoredFile;
    TS: TStored absolute SF;
    B: array[0..542] of Byte;
    L: Word;
  begin
  if Stream = nil then
    Exit;
  Stream^.Status := stOK;
  vSeek(Max(0, (vSize)-TStoredFixLength));
  L := 0;
  if  (FName <> #0) and (FName <> '') then
    begin
    lFSplit(FName, Dr, Nm, Xt);
    MakeSlash(Dr);
    if  (UpStrg(CurDir) <> UpStrg(Dr)) then
      begin
      CurDir := Dr;
      SF.T.Id := siDirectory;
      L := TStoredFixLength+1+Length(Dr);
      SF.Name := Dr;
      SF.T.len := L;
      Move(SF, B, L);
      Inc(Items);
      end;
    if Attr and Directory <> 0
    then
      SF.T.Id := siFileDir
    else
      SF.T.Id := siFile;
    SF.Name := GetName(FName);
    SF.T.len := TStoredFixLength+1+Length(SF.Name);
    SF.Size := Size;
    TotalLength := TotalLength+Size;
    TotalCLength := TotalCLength+CSize;
    SF.CSize := CSize;
    SF.Date := Date;
    SF.Attr := Attr;
    Move(SF, B[L], SF.T.len);
    Inc(L, SF.T.len);
    Inc(Files);
    Inc(Items);
    if Attr and Directory <> 0 then
      Inc(Dirs);
    end;

  TS.Id := siEnd;
  TS.len := TStoredFixLength;
  Move(TS, B[L], TS.len);
  Inc(L, TS.len);

  vWrite(B, L);
  Last := True;
  end { TDirStorage.AddFile };
{-DataCompBoy-}

procedure TDirStorage.ResetPointer;
  begin
  if Stream = nil then
    Exit;
  CurPos := 0;
  FilePtr := -1;
  CurPos := -1;
  Last := vSize = 0;
  vSeek(0);
  FillChar(CurFile, SizeOf(CurFile), 0);
  CurDir := Dir;
  LastDir := '';
  MakeSlash(CurDir);
  end;

{-DataCompBoy-}
function TDirStorage.GetNextFile;
  var
    SF: TStoredFile;
    Dr: String;
  begin
  Dr := LastDir;
  GetNextFile := False;
  FillChar(CurFile, SizeOf(CurFile), 0);
  Last := True;
  CurPos := -1;
  if Stream = nil then
    Exit;
  while Stream^.Status = stOK do
    begin
    FilePtr := vSavePos;
    vRead(SF, TStoredFixLength);
    if SF.T.len > TStoredFixLength then
      vRead(SF.Name, SF.T.len-TStoredFixLength);
    case SF.T.Id of
      siDirectory:
        begin
        Dr := SF.Name;
        LastDir := Dr
        end;
      siEnd:
        Break;
      siFile, siFileDir:
        begin
        CurPos := vSavePos-SF.T.len;
        if  (CurDir = '') or (UpStrg(Dr) = UpStrg(CurDir)) then
          begin
          CurFile := SF;
          Last := vSize-vSavePos <= TStoredFixLength;
          LastDir := Dr;
          GetNextFile := True;
          if CurFile.Name <> '' then
            Break;
          end;
        end;
    end {case};
    end;
  end { TDirStorage.GetNextFile };
{-DataCompBoy-}

procedure TDirStorage.UpdateRecord;
  var
    s: TStored;
  begin
  if Stream = nil then
    Exit;
  if  (CurPos > 0) and (CurFile.Name <> '') then
    begin
    Stream^.Status := stOK;
    vSeek(CurPos);
    vRead(s, SizeOf(s));
    vSeek(CurPos);
    vWrite(CurFile, s.len);
    end;
  end;

constructor TDirStorage.Load;
  var
    P, L: LongInt; {!!s}
  begin
  TObject.Init;
  S.Read(Dirs, SizeOf(Dirs));
  S.Read(Files, SizeOf(Files));
  S.Read(TotalLength, SizeOf(TotalLength));
  S.Read(TotalCLength, SizeOf(TotalCLength));
  S.Read(L, SizeOf(L));
  vSize := L;
  TryStream(L);
  P := i32(S.GetPos);
  repeat
    Stream^.CopyFrom(S, L);
    if Stream^.Status = stOK then
      Break
    else
      begin
      Dispose(Stream, Done);
      Stream := nil;
      TryStream(L);
      S.Seek(P);
      end;
  until False;
  end { TDirStorage.Load };

procedure TDirStorage.Store;
  var
    L: LongInt;
  begin
  AddFile(#0, 0, 0, 0, 0);
  S.Write(Dirs, SizeOf(Dirs));
  S.Write(Files, SizeOf(Files));
  S.Write(TotalLength, SizeOf(TotalLength));
  S.Write(TotalCLength, SizeOf(TotalCLength));
  L := vSize;
  S.Write(L, SizeOf(L));
  vSeek(0);
  Stream^.Status := stOK;
  S.CopyFrom(Stream^, vSize);
  end;

{-DataCompBoy-}
procedure TDirStorage.DeleteFile;
  var
    Dr: String;
    Nm: String;
  begin
  Dr := P^.Owner^;
  MakeSlash(Dr);
  ResetPointer('');
  Nm := UpStrg(P^.FlName[True]);
  while not Last and GetNextFile do
    if  (UpStrg(CurFile.Name) = Nm) and (LastDir = Dr) then
      WipeCur;
  end;
{-DataCompBoy-}

procedure TDirStorage.Truncate(N: LongInt);
  var
    ST: TStored;
    L, NF, ND, NI: LongInt;
  begin
  ResetPointer('');
  NF := 0;
  ND := 0;
  NI := 0;
  while Stream^.Status = stOK do
    begin
    Inc(NI);
    L := vSavePos;
    vRead(ST, TStoredFixLength);
    if ST.len > TStoredFixLength then
      vSeek(L+ST.len);
    case ST.Id of
      siEnd:
        Break;
      siFile, siFileDir:
        begin
        Dec(N);
        if N > 0 then
          begin
          Inc(NF);
          if ST.Id = siFileDir then
            Inc(ND);
          end
        else
          begin
          ST.Id := siEnd;
          ST.len := TStoredFixLength;
          vSeek(L);
          vWrite(ST, ST.len);
          vTruncate;
          Dirs := ND;
          Files := NF;
          Items := NI;
          Break;
          end
        end;
    end {case};
    end;
  end { TDirStorage.Truncate };

procedure TDirStorage.WipeCur;
  var
    L: LongInt;
    s: TStored;
  begin
  L := vSavePos;
  Stream^.Status := stOK;
  vSeek(FilePtr);
  vRead(s, SizeOf(s)); {DataCompBoy}
  vSeek(FilePtr); {DataCompBoy}
  CurFile.T.Id := siEmpty;
  vWrite(CurFile, s.len); {DataCompBoy}
  vSeek(L);
  Dec(Files);
  if s.Id = siFileDir then
    Dec(Dirs);
  end;

procedure TDirStorage.vWrite;
  begin
  repeat
    Stream^.Write(Buf, Count);
    if Stream^.Status = stOK
    then
      begin
      vSize := Max(vSize, vSavePos+Count);
      Inc(vSavePos, Count);
      Break;
      end
    else
      FixError;
  until False;
  end;

procedure TDirStorage.vSeek;
  begin
  Stream^.Seek(Pos);
  vSavePos := Pos;
  end;

procedure TDirStorage.vTruncate;
  begin
  Stream^.Truncate;
  if Stream^.Status = stOK
  then
    begin
    vSize := i32(Stream^.GetPos);
    vSavePos := vSize
    end
  else
    FixError;
  end;

procedure TDirStorage.vRead;
  begin
  Stream^.Read(Buf, Count);
  if Stream^.Status = stOK
  then
    Inc(vSavePos, Count)
  else
    __Error;
  end;

end.
