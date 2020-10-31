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
unit archZip; {ZIP}

interface
uses
  dnsys, sysutils, Archiver, Advance, Advance1, Defines, Objects2, Streams, Dos
  ;

type
  PZIPArchive = ^TZIPArchive;
  TZIPArchive = object(TARJArchive)
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
    end;

type
  TZIPLocalHdr = record
    Id: LongInt;
    Extract: AWord;
    GeneralPurpose: AWord;
    Method: AWord;
    LastModDate: LongInt;
    CRC32: LongInt;
    CompressedSize: LongInt;
    OriginalSize: LongInt;
    FNameLength: AWord;
    ExtraField: AWord;
    end;

  TZIPCentralFileRec = record
    Id: LongInt;
    VersionMade: AWord;
    Version2Extr: AWord;
    GeneralPurpose: AWord;
    Method: AWord;
    LastModDate: LongInt;
    CRC32: LongInt;
    CompressedSize: LongInt;
    OriginalSize: LongInt;
    FNameLength: AWord;
    ExtraField: AWord;
    FileCommLength: AWord;
    DiskNumStart: AWord;
    InternalFAttr: AWord;
    ExternalFAttr: LongInt;
    OffsetLocHeader: LongInt;
    end;

var
  CentralDirRecPresent: Boolean;

implementation
uses
  UKeyMap, FViewer
  ;

{ ----------------------------- ZIP ------------------------------------}

constructor TZIPArchive.Init;
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
  {$IFDEF SEVENZIP}
  Packer := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PPacker,'7Z'));
  UnPacker := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PUnPacker, '7Z'));
  Extract := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtract, 'e'));
  ExtractWP := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtractWP, 'x'));
  Add := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a -tzip'));
  Move := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PMove, ''));
  Delete := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PDelete, 'd'));
  Garble := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PGarble, '-p'));
  Test := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PTest, 't'));
  IncludePaths := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExcludePaths, ''));
  ForceMode := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PForceMode, '-y'));
  RecoveryRec := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''));
  SelfExtract := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''));
  Solid := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
  RecurseSubDirs := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,
         '-r0'));
  SetPathInside := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         ''));
  StoreCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, '-m0'));
  FastestCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, ''));
  FastCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, ''));
  NormalCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, ''));
  GoodCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, ''));
  UltraCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, '-mx'));
  ComprListchar := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PComprListchar,
         '@'));
  ExtrListchar := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtrListchar, '@'));
  {$ELSE SEVENZIP}
  Packer := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PPacker, 'PKZIP'));
  UnPacker := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'PKUNZIP'));
  Extract := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtract, ''));
  ExtractWP := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtractWP, '-d'));
  Add := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PAdd, '-a -wsh'));
  Move := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PMove, '-m -wsh'));
  Delete := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PDelete, '-d'));
  Garble := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PGarble, '-s'));
  Test := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PTest, '-t'));
  IncludePaths := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,
         '-P'));
  ExcludePaths := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,
         '-p'));
  ForceMode := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PForceMode, ''));
  RecoveryRec := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''));
  SelfExtract := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''));
  Solid := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
  RecurseSubDirs := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,
         ''));
  SetPathInside := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         ''));
  StoreCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, '-e0'));
  FastestCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, '-es'));
  FastCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, '-ef'));
  NormalCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, '-en'));
  GoodCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, '-ex'));
  UltraCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, '-exx'));
  ComprListChar := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PComprListChar,
         '@'));
  ExtrListChar := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtrListChar,
       '@'));
  {$ENDIF}
  {$ELSE}
  Packer := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PPacker, 'ZIP'));
  UnPacker := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'UNZIP'));
  Extract := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtract, '-j --z'));
  ExtractWP := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtractWP, '--z'));
  Add := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PAdd, '-S'));
  Move := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PMove, '-m -S'));
  Delete := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PDelete, '-d'));
  Garble := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PGarble, '-P'));
  Test := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PTest, '-t -C'));
  IncludePaths := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,
         '-j'));
  ForceMode := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PForceMode, '-q'));
  RecoveryRec := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''));
  SelfExtract := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''));
  Solid := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
  RecurseSubDirs := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PRecurseSubDirs, '-r'));
  StoreCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, '-0'));
  FastestCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, '-1'));
  FastCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, '-3'));
  NormalCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, '-6'));
  GoodCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, '-8'));
  UltraCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, '-9'));
  ComprListChar := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PComprListChar,
         '-@ < '));
  ExtrListChar := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtrListChar,
       ' '));
  {$ENDIF}

  q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
  AllVersion := q <> '0';
  {$IFNDEF OS2}
  q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '1');
  {$ELSE}
  q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '0');
  {$ENDIF}
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
  end { TZIPArchive.Init };

function TZIPArchive.GetID;
  begin
  GetID := arcZIP;
  end;

function TZIPArchive.GetSign;
  begin
  GetSign := sigZIP;
  end;

{JO} {piwamoto}
procedure TZIPArchive.GetFile;
  var
    P: TZIPLocalHdr;
    HCF: TZIPCentralFileRec;
    FP, FPP: TFileSize;
    ExtraFieldHeader: record
                       HeaderID: AWord;
                       DataSize: AWord;
                      end;

  label 1;
  begin
  if CentralDirRecPresent then
    begin
    ArcFile^.Read(HCF.Id, SizeOf(HCF.Id));
    if  (ArcFile^.Status <> stOK) or (HCF.Id and $FFFF <> $4B50) then
      begin
      FileInfo.Last := 2;
      Exit;
      end;
    if  (HCF.Id = $06054B50) or (HCF.Id = $06064B50) then
      begin
      FileInfo.Last := 1;
      Exit;
      end;
    ArcFile^.Read(HCF.VersionMade, SizeOf(HCF)-SizeOf(HCF.Id));
    if HCF.FNameLength > 255 then
      HCF.FNameLength := 255;
    SetLength(FileInfo.FName, HCF.FNameLength);
    ArcFile^.Read(FileInfo.FName[1], HCF.FNameLength);
    FileInfo.Last := 0;
    FileInfo.Attr := (HCF.GeneralPurpose and 1)*Hidden;
    FileInfo.Date := HCF.LastModDate;
    FileInfo.USize := HCF.OriginalSize;
    FileInfo.PSize := HCF.CompressedSize;
    if (HCF.ExtraField <> 0) and
       (HCF.OriginalSize = $FFFFFFFF) and
       (HCF.CompressedSize = $FFFFFFFF) then
      begin {search for Zip64 extended information extra field}
        FP := ArcFile^.GetPos;
        ArcFile^.Read(ExtraFieldHeader, SizeOf(ExtraFieldHeader));
        if ExtraFieldHeader.HeaderID = 1 then
          begin
            ArcFile^.Read(FileInfo.USize, 8);
            ArcFile^.Read(FileInfo.PSize, 8);
          end;
        ArcFile^.Seek(FP);
      end;
    ArcFile^.Seek(ArcFile^.GetPos+HCF.ExtraField+HCF.FileCommLength);
    end
  else {CentralDirRecPresent}
    begin
1:
    ArcFile^.Read(P.Id, 4);
    if P.Id = $02014b50 then
      begin
      FileInfo.Last := 1;
      Exit;
      end;
    if P.Id = $08074B50 then
      {skip Spanned/Split block}
      begin
      ArcFile^.Read(P.Id, 12);
      goto 1;
      end;
    ArcFile^.Read(P.Extract, SizeOf(P)-4);
    if  (ArcFile^.Status <> stOK) or (P.Id <> $04034B50) then
      begin
      FileInfo.Last := 2;
      Exit;
      end;
    if P.FNameLength > 255 then
      P.FNameLength := 255;
    ArcFile^.Read(FileInfo.FName[1], P.FNameLength);
    SetLength(FileInfo.FName, P.FNameLength);
    FileInfo.Last := 0;
    FileInfo.Attr := (P.GeneralPurpose and 1)*Hidden;
    FileInfo.Date := P.LastModDate;
    FP := ArcFile^.GetPos+P.ExtraField+P.CompressedSize;
    if  ( (P.GeneralPurpose and 8) <> 0) and (P.CompressedSize = 0) then
      begin
      FPP := FP;
      FP := SearchFileStr(@ArcFile^, NullXlatTable, 'PK'#03#04, FPP,
           True, False, False, False, False, False);
        {local file header signature}
      if FP < 0 then
        begin
        FP := SearchFileStr(@ArcFile^, NullXlatTable, 'PK'#01#02, FPP,
           True, False, False, False, False, False);
        {central file header signature}
        if FP < 0 then
          begin
          FileInfo.Last := 2;
          Exit;
          end;
        end;
      ArcFile^.Seek(FP-8);
      ArcFile^.Read(P.CompressedSize, 8);
      end;
    FileInfo.USize := P.OriginalSize;
    FileInfo.PSize := P.CompressedSize;
    ArcFile^.Seek(FP);
    end;
  end { TZIPArchive.GetFile };

end.
