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
unit arc_ZOO; {ZOO}

{.$DEFINE DeadCode} {piwamoto}

interface

uses
  Archiver, Advance, Advance1, Defines, Objects2, Streams
  ;

type
  PZOOArchive = ^TZOOArchive;
  TZOOArchive = object(TARJArchive)
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
    end;

type
  ZOOHdr = record
    Id: LongInt;
    Info: AWord;
    NextHDR: LongInt;
    CurStart: LongInt;
    Date: LongInt;
    W: AWord;
    OriginSize: LongInt;
    PackedSize: LongInt;
    C: Char;
    Reserved: array[1..9] of Byte;
    end;

implementation

{ ----------------------------- ZOO ------------------------------------}

constructor TZOOArchive.Init;
  var
    Sign: TStr5;
    q: String;
  begin
  Sign := GetSign;
  SetLength(Sign, Length(Sign)-1);
  Sign := Sign+#0;
  FreeStr := SourceDir+DNARC;
  TObject.Init;
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, 'ZOO'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'ZOO'));
  Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'eo'));
  ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP, 'xo'));
  Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a'));
  Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'aM'));
  Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'D'));
  Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, ''));
  Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 'eN'));
  IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths, ''));
  ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, ''));
  RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''));
  SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''));
  Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
  RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,
         ''));
  SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         ''));
  StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, '+f'));
  FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, ''));
  FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, ''));
  NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, ''));
  GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, ''));
  UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, '+h'));
  ComprListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListChar,
         ' '));
  ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListChar,
       ' '));

  q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
  AllVersion := q <> '0';
  q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '0');
  PutDirs := q <> '0';
 {$IFNDEF DPMI32}
  {$IFDEF OS2}
  q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '0');
  {$ELSE}
  q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '1');
  {$ENDIF}
  ShortCmdLine := q <> '0';
 {$ELSE}
  q := GetVal(@Sign[1], @FreeStr[1], PSwapWhenExec, '0');
  SwapWhenExec := q <> '0';
 {$ENDIF}
  {$IFNDEF OS2}
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
  UseLFN := q <> '0';
  {$ENDIF}
  end { TZOOArchive.Init };

function TZOOArchive.GetID;
  begin
  GetID := arcZOO;
  end;

function TZOOArchive.GetSign;
  begin
  GetSign := sigZOO;
  end;

procedure TZOOArchive.GetFile;
  var
    P: ZOOHdr;
    FP: TFileSize;
    C: Char;
    S: String;
  begin
  ArcFile^.Read(P, 4);
  if  (ArcFile^.Status <> stOK) or (P.Id <> $FDC4A7DC) then
    begin
    FileInfo.Last := 2;
    Exit;
    end;
  ArcFile^.Read(P.Info, 2);
  if  (ArcFile^.Status <> stOK) then
    begin
    FileInfo.Last := 2;
    Exit;
    end;
  {if (P.Info = $0002) then begin FileInfo.Last := 1;Exit;end;}
  ArcFile^.Read(P.NextHDR, SizeOf(P)-6);
  {if (P.Method > 20) then begin FileInfo.Last:=2;Exit;end;}
  FileInfo.Last := 0;
  FileInfo.Attr := 0;
  FileInfo.USize := P.OriginSize;
  FileInfo.PSize := P.PackedSize;
  FileInfo.Date := (P.Date shl 16) or (P.Date shr 16);
  FileInfo.FName := '';
  FP := ArcFile^.GetPos;
  repeat
    ArcFile^.Read(C, 1);
    if C <> #0 then
      FileInfo.FName := FileInfo.FName+C;
  until (C = #0) or (Length(FileInfo.FName) > 77);
  ArcFile^.Seek(FP+19);
  ArcFile^.Read(S[0], 1);
  if S <> '' then
    begin
    ArcFile^.Read(S[1], Byte(S[0]));
    S[Length(S)] := '\';
    end;
  FileInfo.FName := S+FileInfo.FName;
  if FileInfo.FName = '' then
    begin
    FileInfo.Last := 1;
    Exit;
    end;
  ArcFile^.Seek(P.NextHDR);
  end { TZOOArchive.GetFile };

end.
