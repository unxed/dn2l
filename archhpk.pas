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
unit archHPK; {HPK}

interface

uses
  Archiver, Advance, Advance1, Defines, Objects2, Streams, Dos, xTime,
  Objects
  ;

type
  PHPKArchive = ^THPKArchive;
  THPKArchive = object(TARJArchive)
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
    end;

type
  PHPKRec = ^THPKRec;
  THPKRec = record
    parentIndex: LongInt;
    PSize, USize: LongInt;
    Date: LongInt;
    Name: PString;
    end;

  PHPKCollection = ^THPKCollection;
  THPKCollection = object(TCollection)
    procedure FreeItem(P: Pointer); virtual;
    end;

var
  HPKCol: PHPKCollection;

implementation

{ ----------------------------- HPK ------------------------------------}

constructor THPKArchive.Init;
  var
    Sign: TStr5;
    q: String;
  begin
  Sign := GetSign;
  SetLength(Sign, Length(Sign)-1);
  Sign := Sign+#0;
  FreeStr := SourceDir+DNARC;
  TObject.Init;
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, 'HPACK'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'HPACK'));
  Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'X'));
  ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP, 'X'));
  Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'A -DA -A'));
  Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'A -DA -A'));
  Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'D'));
  Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, '-C'));
  Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 'T'));
  IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths, ''));
  ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, ''));
  RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, '-E'));
  SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''));
  Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
  RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,
         ''));
  SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         ''));
  StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, '-0'));
  FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, ''));
  FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, ''));
  NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, ''));
  GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, ''));
  UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, ''));
  ComprListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListChar,
         ' '));
  ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListChar,
       ' '));

  q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
  AllVersion := q <> '0';
  q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '0');
  PutDirs := q <> '0';
  {$IFNDEF DPMI32}
  q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '0');
  ShortCmdLine := q <> '0';
  {$ELSE}
  q := GetVal(@Sign[1], @FreeStr[1], PSwapWhenExec, '0');
  SwapWhenExec := q <> '0';
  {$ENDIF}
  {$IFNDEF OS2}
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
  UseLFN := q <> '0';
  {$ENDIF}
  end { THPKArchive.Init };

function THPKArchive.GetID;
  begin
  GetID := arcHPK;
  end;

function THPKArchive.GetSign;
  begin
  GetSign := sigHPK;
  end;

procedure THPKCollection.FreeItem(P: Pointer);
  begin
  if P <> nil then
    begin
    DisposeStr(PHPKRec(P)^.Name);
    Dispose(PHPKRec(P));
    end;
  end;

procedure THPKArchive.GetFile;
  var
    DT: DateTime;
    R: PHPKRec;
  begin
  if HPKCol^.Count = 0 then
    begin
    FileInfo.Last := 1;
    Dispose(HPKCol, Done);
    HPKCol := nil;
    Exit;
    end;
  FileInfo.USize := PHPKRec(HPKCol^.At(0))^.USize;
  FileInfo.PSize := PHPKRec(HPKCol^.At(0))^.PSize;
  GetUNIXDate(PHPKRec(HPKCol^.At(0))^.Date, DT.Year, DT.Month, DT.Day,
     DT.Hour, DT.Min, DT.Sec);
  PackTime(DT, FileInfo.Date);
  if PHPKRec(HPKCol^.At(0))^.Name <> nil {DataCompBoy}
    then
    FileInfo.FName := PHPKRec(HPKCol^.At(0))^.Name^ {DataCompBoy}
  else
    FileInfo.FName := ''; {DataCompBoy}
  FileInfo.Last := 0;
  FileInfo.Attr := 0;
  HPKCol^.AtFree(0);
  end { THPKArchive.GetFile };

end.
