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
unit archTAR; {TAR}

interface

uses
  Archiver, Advance, Advance1, Defines, Objects2, Streams, Dos, xTime
  ;

type
  PTARArchive = ^TTARArchive;
  TTARArchive = object(TARJArchive)
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
    end;

const
  MaxTName = 100;
  Txt_Word = 8;
  Txt_Long = 12;
  BlkSize = 512;

type
  TARHdr = record
    FName: array[1..MaxTName] of Char;
    Mode: array[1..Txt_Word] of Char;
    uid: array[1..Txt_Word] of Char;
    gid: array[1..Txt_Word] of Char;
    Size: array[1..Txt_Long] of Char;
    mtime: array[1..Txt_Long] of Char;
    chksum: array[1..Txt_Word] of Char;
    filetype: Char;
    linkname: array[1..MaxTName] of Char;
    case Byte of
      0: (
        (* old-fashion data & padding *)
        comment:
         array[1..BlkSize-MaxTName-8-8-8-12-12-8-1-MaxTName-12-12] of Char;
        SrcSum: array[1..Txt_Long] of Char;
        SrcLen: array[1..Txt_Long] of Char;
        );
      1: (
        (* System V extensions *)
        extent: array[1..4] of Char;
        AllExt: array[1..4] of Char;
        Total: array[1..Txt_Long] of Char;
        );
      2: (
        (* P1003 & GNU extensions *)
        magic: array[1..8] of Char;
        UName: array[1..32] of Char;
        gname: array[1..32] of Char;
        devmajor: array[1..Txt_Word] of Char;
        devminor: array[1..Txt_Word] of Char;
        (* the following fields are added gnu and NOT standard *)
        ATime: array[1..12] of Char;
        ctime: array[1..12] of Char;
        Offset: array[1..12] of Char;
        );
    end;

implementation

{ ----------------------------- TAR ------------------------------------}

constructor TTARArchive.Init;
  var
    Sign: TStr5;
    q: String;
  begin
  Sign := GetSign;
  SetLength(Sign, Length(Sign)-1);
  Sign := Sign+#0;
  FreeStr := SourceDir+DNARC;
  TObject.Init;
  {$IFDEF WIN32}
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, '7Z'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, '7Z'));
  Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'e'));
  ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP, 'x'));
  Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a -ttar'));
  Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, ''));
  Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'd'));
  Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, '-p'));
  Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 't'));
  IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths, ''));
  ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, '-y'));
  RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''));
  SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''));
  Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
  RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PRecurseSubDirs, '-r0'));
  SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         ''));
  StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, ''));
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
         '@'));
  ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListChar,
       '@'));
  {$ELSE}
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, 'TAR'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'TAR'));
  Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'xf'));
  ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP, 'xf'));
  Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'cvf'));
  Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'cvf'));
  Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'df'));
  Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, ''));
  Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 'tf'));
  IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths, ''));
  ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, ''));
  RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''));
  SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''));
  Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
  RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,
         ''));
  StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, ''));
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
  {$ENDIF}

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
  end { TTARArchive.Init };

function TTARArchive.GetID;
  begin
  GetID := arcTAR;
  end;

function TTARArchive.GetSign;
  begin
  GetSign := sigTAR;
  end;

procedure TTARArchive.GetFile;
  var
    Buffer: array[0..BlkSize-1] of Char;
    Hdr: TARHdr absolute Buffer;
    DT: DateTime;
    W: AWord;
  begin
  if ArcFile^.GetPos = ArcFile^.GetSize then
    begin
    FileInfo.Last := 1;
    Exit
    end;
  ArcFile^.Read(Buffer, BlkSize);
  if ArcFile^.Status <> stOK then
    begin
    FileInfo.Last := 2;
    Exit
    end;
  FileInfo.Last := 0;
  if Hdr.filetype = '5' {directory}
    then FileInfo.Attr := Directory
    else FileInfo.Attr := 0;
  FileInfo.FName := Hdr.FName+#0;
  SetLength(FileInfo.FName, PosChar(#0, FileInfo.FName)-1);
  if FileInfo.FName = '' then
    begin
    FileInfo.Last := 1;
    Exit
    end;
  FileInfo.USize := FromOct(Hdr.Size);
  FileInfo.PSize := FileInfo.USize;
  GetUNIXDate(i32(FromOct(Hdr.mtime)), DT.Year, DT.Month, DT.Day, DT.Hour,
     DT.Min, DT.Sec);
  PackTime(DT, FileInfo.Date);
(*
  ArcFile^.Seek(ArcFile^.GetPos+
     (Trunc((FileInfo.PSize+BlkSize-1) / BlkSize)*BlkSize));
*)
  W := Word(CompRec(FileInfo.PSize).Lo) and (BlkSize-1);
  ArcFile^.Seek(CompToFSize(ArcFile^.GetPos + FileInfo.PSize -
                            W + BlkSize*Byte(W<>0)));
  end { TTARArchive.GetFile };

end.
