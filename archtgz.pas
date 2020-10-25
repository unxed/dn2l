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
unit archTGZ; {TGZ & TAZ & TAR.GZ}

interface

uses
  Archiver, Advance, Advance1, Defines, Objects2, Streams, Dos, xTime,
   Advance2
  ;

type
  PTGZArchive = ^TTGZArchive;
  TTGZArchive = object(TARJArchive)
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
    end;

implementation
{ ----------------------------- TAR ------------------------------------}

constructor TTGZArchive.Init;
  var
    Sign: TStr5;
    q: String;
  begin
  TObject.Init;
  Sign := GetSign;
  SetLength(Sign, Length(Sign)-1);
  Sign := Sign+#0;
  FreeStr := SourceDir+DNARC;
  {$IFDEF WIN32}
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, '7Z'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, '7Z'));
  Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'e'));
  ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP, 'x'));
  Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a -tgzip'));
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
         PUltraCompression, '-mx'));
  ComprListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListChar,
         '@'));
  ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListChar,
       '@'));
  {$ELSE}
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, ''));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'UNTGZOS2'));
  Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, '-d'));
  ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP, '-d'));
  Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, ''));
  Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, ''));
  Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, ''));
  Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, ''));
  Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, '-t'));
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
  end { TTGZArchive.Init };

function TTGZArchive.GetID;
  begin
  GetID := arcTGZ;
  end;

function TTGZArchive.GetSign;
  begin
  GetSign := sigTGZ;
  end;

procedure TTGZArchive.GetFile;
  type
    GZipHdr = record
      Id: AWord;
      Flag: AWord;
      Time: LongInt;
      end;
  var
    P: GZipHdr;
    DT: DateTime;
    C: Char;
  begin
  ArcFile^.Read(P, SizeOf(P));
  if ArcFile^.Eof then
    begin
    FileInfo.Last := 1;
    Exit;
    end;
  GetUNIXDate(P.Time, DT.Year, DT.Month, DT.Day, DT.Hour, DT.Min, DT.Sec);
  PackTime(DT, FileInfo.Date);
  FileInfo.FName := '';
  if  (P.Flag and $800 = 0) or (P.Id = $9d1f)
  then
    if {(UpStrg(GetExt(ArcFileName)) = '.GZ') or}
        (UpCase(ArcFileName[Length(ArcFileName)]) = 'Z')
      {gzip changes last char of extension to 'z' or adds '.gz' extension}
      then
      FileInfo.FName := GetSName(ArcFileName)
    else
      FileInfo.FName := GetName(ArcFileName)
  else
    begin
    if P.Flag and $400 = 0 then
      P.Time := 10 {skip 10 bytes}
    else
      begin
      ArcFile^.Read(P.Time, SizeOf(P.Time));
      P.Time := P.Time shr 16+12;
      end;
    ArcFile^.Seek(ArcPos+P.Time);
    repeat
      ArcFile^.Read(C, 1);
      if C <> #0 then
        FileInfo.FName := FileInfo.FName+C
      else
        Break;
    until ArcFile^.Status <> stOK;
    end;
  FileInfo.PSize := ArcFile^.GetSize;
  ArcFile^.Seek(CompToFSize(FileInfo.PSize-4));
  ArcFile^.Read(FileInfo.USize, SizeOf(FileInfo.USize));
  FileInfo.Attr := 0;
  FileInfo.Last := 0;
  end { TTGZArchive.GetFile };

end.
