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
unit archCAB; {CAB}

interface

uses
  dnsys, Archiver, Advance, Advance1, Defines, Objects2, Streams, Dos
  ;

type
  PCABArchive = ^TCABArchive;
  TCABArchive = object(TARJArchive)
    FilesNumber: LongInt;
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
    end;

type
  PCFHEADER = ^TCFHEADER;
  TCFHEADER = record
    signature: LongInt;
    Reserved1: LongInt;
    cbCabinet: LongInt;
    Reserved2: LongInt;
    coffFiles: LongInt;
    Reserved3: LongInt;
    VersionMinor: Byte;
    VersionMajor: Byte;
    cFolders: AWord;
    cFiles: AWord;
    Flags: AWord;
    setID: AWord;
    iCabinet: AWord;
    end;

implementation
{ ---------------------- CAB (by Neverowsky A.)---------------------------}

constructor TCABArchive.Init;
  var
    Sign: TStr5;
    q: String;
  begin
  Sign := GetSign;
  SetLength(Sign, Length(Sign)-1);
  Sign := Sign+#0;
  FreeStr := SourceDir+DNARC;
  TObject.Init;
  {-i0 turns on console output}
  Packer := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PPacker, 'MSCAB -i0'));
  {$IFDEF SEVENZIP}
  UnPacker := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PUnPacker, '7Z'));
  {$ELSE SEVENZIP}
  UnPacker := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'MSCAB -i0'));
  {$ENDIF}
  Extract := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtract, 'e'));
  ExtractWP := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtractWP, 'x'));
  Add := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PAdd, '-dirs -r0 a'));
  Move := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PMove, '-dirs -r0 m'));
  Delete := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PDelete, 'd'));
  Garble := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PGarble, ''));
  Test := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PTest, 't'));
  IncludePaths := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExcludePaths, ''));
  ForceMode := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PForceMode, ''));
  RecoveryRec := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''));
  SelfExtract := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''));
  Solid := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
  RecurseSubDirs := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,
         ''));
  SetPathInside := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         ''));
  StoreCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, ''));
  FastestCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, ''));
  FastCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, ''));
  NormalCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, ''));
  GoodCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, ''));
  UltraCompression := NewStrDN2(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, ''));
  ComprListChar := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PComprListChar,
         '@'));
  ExtrListChar := NewStrDN2(GetVal(@Sign[1], @FreeStr[1], PExtrListChar,
       '@'));

  q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
  AllVersion := q <> '0';
  q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '0');
  PutDirs := q <> '0';
  {$IFNDEF DPMI32}
  q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '1');
  ShortCmdLine := q <> '0';
  {$ELSE}
  q := GetVal(@Sign[1], @FreeStr[1], PSwapWhenExec, '0');
  SwapWhenExec := q <> '0';
  {$ENDIF}
  {$IFNDEF OS2}
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
  UseLFN := q <> '0';
  {$ENDIF}

  FilesNumber := -1;
  end { TCABArchive.Init };

function TCABArchive.GetID;
  begin
  GetID := arcCAB;
  end;

function TCABArchive.GetSign;
  begin
  GetSign := sigCAB;
  end;

procedure TCABArchive.GetFile;
  var
    C: Char;
    FH: record
      cbFile: LongInt;
      uoffFolderStart: LongInt;
      iFolder: AWord;
      {     date:     AWord;
      time:     AWord; }
      DateTime: LongInt;
      attribs: AWord;
      {     u1  szName[]; }
      end;
    CFHEADER: TCFHEADER;
  begin
  if  (FilesNumber < 0) then
    begin
    ArcFile^.Read(CFHEADER, SizeOf(CFHEADER));
    FilesNumber := CFHEADER.cFiles;
    ArcFile^.Seek(ArcPos+CFHEADER.coffFiles);
    end;
  if  (FilesNumber = 0) then
    begin
    FileInfo.Last := 1;
    Exit;
    end;
  Dec(FilesNumber);
  ArcFile^.Read(FH, SizeOf(FH));
  if  (ArcFile^.Status <> 0) then
    begin
    FileInfo.Last := 2;
    Exit;
    end;
  FileInfo.FName := '';
  repeat
    ArcFile^.Read(C, 1);
    if C <> #0 then
      FileInfo.FName := FileInfo.FName+C;
  until (C = #0) or (Length(FileInfo.FName) > 100);
  if  (Length(FileInfo.FName) > 100) or (FileInfo.FName = '')
  then
    begin
    FileInfo.Last := 2;
    Exit;
    end;
  FileInfo.Attr := FH.attribs and not Hidden;
  FileInfo.USize := FH.cbFile;
  FileInfo.PSize := FH.cbFile;
  FileInfo.Date := (FH.DateTime shr 16) or (FH.DateTime shl 16);
  FileInfo.Last := 0;
  end { TCABArchive.GetFile };

end.
