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
unit archACE; {ACE}

interface

uses
  dnsys, Archiver, Advance, Advance1, Defines, Objects2, Streams, Dos
  ;

type
  PACEArchive = ^TACEArchive;
  TACEArchive = object(TARJArchive)
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
    end;

type
  ACEFileHdr = record
    HeadCRC: AWord;
    HeadSize: AWord;
    HeadType: Byte;
    //1 - file
    //2 - recovery record
    //3 - large file (4+ gb)
    HeadFlags: AWord;
    PackedSize: LongInt; //qword for HeadType = 3
    OriginSize: LongInt; //qword for HeadType = 3
    DateTime: LongInt;
    Attr: LongInt;
    CRC32: LongInt;
    TechInfo: LongInt;
    Reserved: AWord;
    NameLen: AWord;
    end;
  //  HeadFlags:
  //  bit  description
  //   0   1 (ADDSIZE field present)
  //   1   presence of file comment
  //
  //   12  file continued from previous volume
  //   13  file continues on the next volume
  //   14  file encrypted with password
  //   15  solid-flag: file compressed using data
  //       of previous files of the archive

var
  ACEVerToExtr: Byte;

implementation
{ ---------------------------------- ACE --------------------------------- }

constructor TACEArchive.Init;
  var
    Sign: TStr5;
    q: String;
  begin
  Sign := GetSign;
  SetLength(Sign, Length(Sign)-1);
  Sign := Sign+#0;
  FreeStr := SourceDir+DNARC;
  TObject.Init;
  {$IFNDEF OS2}
  Packer := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PPacker, 'ACE'));
  UnPacker := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'ACE'));
  {$ELSE}
  Packer := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PPacker, 'ACE2'));
  UnPacker := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'ACE2'));
  {$ENDIF}
  Extract := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtract, 'e'));
  ExtractWP := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtractWP, 'x'));
  Add := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a'));
  Move := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PMove, 'm'));
  Delete := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PDelete, 'd'));
  Test := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PTest, 't'));
  Garble := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PGarble, '-p'));
  IncludePaths := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,
         '-ep'));
  ForceMode := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PForceMode, ''));
  RecoveryRec := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,
       '-rr'));
  SelfExtract := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,
         '-sfx'));
  Solid := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSolid, '-s'));
  RecurseSubDirs := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,
         ''));
  SetPathInside := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         ''));
  StoreCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, '-m0'));
  FastestCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, '-m1'));
  FastCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, '-m2'));
  NormalCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, '-m3'));
  GoodCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, '-m4'));
  UltraCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, '-m5'));
  ComprListChar := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PComprListChar,
         '@'));
  ExtrListChar := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtrListChar,
       '@'));

  q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
  AllVersion := q <> '0';
  q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '1');
  PutDirs := q <> '0';
  {$IFNDEF DPMI32}
  q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '0');
  ShortCmdLine := q <> '0';
  {$ELSE}
  q := GetVal(@Sign[1], @FreeStr[1], PSwapWhenExec, '1');
  SwapWhenExec := q <> '0';
  {$ENDIF}
  {$IFNDEF OS2}
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
  UseLFN := q <> '0';
  {$ENDIF}
  end { TACEArchive.Init };

function TACEArchive.GetID;
  begin
  GetID := arcACE;
  end;

function TACEArchive.GetSign;
  begin
  GetSign := sigACE;
  end;

procedure TACEArchive.GetFile;
  label 1;
  var
    FP: TFileSize;
    P: ACEFileHdr;
    C: Char;
  begin
1:
  FP := ArcFile^.GetPos;
  if  (FP = ArcFile^.GetSize) then
    begin
    FileInfo.Last := 1;
    Exit;
    end;
  ArcFile^.Read(P, 15);{HeadCRC..OriginSize}
  if  (ArcFile^.Status <> stOK) then
    begin
    FileInfo.Last := 2;
    Exit;
    end;
  if (P.HeadType <> 1) and
     (P.HeadType <> 3) then
    begin {it's not a file}
      ArcFile^.Seek(FP + P.HeadSize +
                    (P.PackedSize)*Byte(P.HeadFlags and 1) + 4);
      goto 1;
    end;
  FileInfo.PSize := P.PackedSize;
  FileInfo.USize := P.OriginSize;
  if P.HeadType = 3 then
    begin
    CompRec(FileInfo.PSize).Hi := P.OriginSize;
    ArcFile^.Read(FileInfo.USize, 8);
    end;
  ArcFile^.Read(P.DateTime, 20);{DateTime..NameLen}
  FileInfo.FName := '';
  repeat
    ArcFile^.Read(C, 1);
    Dec(P.NameLen);
    FileInfo.FName := FileInfo.FName+C;
  until P.NameLen = 0;
  FileInfo.Last := 0;
  FileInfo.Date := P.DateTime;
  FileInfo.Attr := Byte(P.Attr and not Hidden);
  if  (P.HeadFlags and $4000) <> 0 then
    FileInfo.Attr := FileInfo.Attr or Hidden;
  ArcFile^.Seek(CompToFSize(FP + P.HeadSize + FileInfo.PSize + 4));
  end { TACEArchive.GetFile };

end.
