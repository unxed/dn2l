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
unit arc_UFA; {UFA}

interface

uses
  Archiver, Advance, Advance1, Defines, Objects2, Streams, Dos
  ;

type
  PUFAArchive = ^TUFAArchive;
  TUFAArchive = object(TARJArchive)
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
    end;

implementation

{ ---------------------- UFA (by Luzin Aleksey)---------------------------}

constructor TUFAArchive.Init;
  var
    Sign: TStr5;
    q: String;
  begin
  Sign := GetSign;
  SetLength(Sign, Length(Sign)-1);
  Sign := Sign+#0;
  FreeStr := SourceDir+DNARC;
  TObject.Init;
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, 'UFA'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'UFA'));
  Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'e'));
  ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP, 'x'));
  Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a'));
  Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'm'));
  Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'd'));
  Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, '-g'));
  Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 't'));
  IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths, ''));
  ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, '-y'));
  RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''));
  SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''));
  Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, '-s'));
  RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,
         ''));
  SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         ''));
  StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, '-m0'));
  FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, ''));
  FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, '-mq'));
  NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, ''));
  GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, ''));
  UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, '-mx'));
  ComprListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListChar,
         ' '));
  ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListChar,
       ' '));

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
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
  UseLFN := q <> '0';
  {$ENDIF}
  end { TUFAArchive.Init };

function TUFAArchive.GetID;
  begin
  GetID := arcUFA;
  end;

function TUFAArchive.GetSign;
  begin
  GetSign := sigUFA;
  end;

procedure TUFAArchive.GetFile;
  var
    FH: record
      Tmp: array[1..$2A] of Char;
      DateTime: LongInt;
      PackSize: LongInt;
      OriginalSize: LongInt;
      FileNameSize: AWord;
      end;
  begin
  if ArcFile^.GetPos = ArcFile^.GetSize then
    begin
    FileInfo.Last := 1;
    Exit;
    end;
  ArcFile^.Read(FH, SizeOf(FH));
  if  (ArcFile^.Status <> 0) or (FH.FileNameSize > 512) then
    begin
    FileInfo.Last := 2;
    Exit;
    end;
  if FH.FileNameSize > 250 then
    FH.FileNameSize := 250;
  SetLength(FileInfo.FName, FH.FileNameSize);
  ArcFile^.Read(FileInfo.FName[1], FH.FileNameSize);
  if FileInfo.FName = '' then
    begin
    FileInfo.Last := 2;
    Exit;
    end;
  FileInfo.Attr := 0;
  FileInfo.Last := 0;
  FileInfo.USize := FH.OriginalSize;
  FileInfo.PSize := FH.PackSize;
  FileInfo.Date := FH.DateTime;
  ArcFile^.Seek(ArcFile^.GetPos+FH.PackSize);
  end { TUFAArchive.GetFile };

end.
