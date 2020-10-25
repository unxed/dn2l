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
unit archAIN; {AIN}

interface

uses
  Archiver
  ;

type
  PAINArchive = ^TAINArchive;
  TAINArchive = object(TARJArchive)
    ListFileName: String;
    ListFile: System.Text;
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
    end;

implementation

uses
  Objects2, Advance2, Advance, DNApp, DnExec, Commands, Advance1, Messages,
  Dos
  ;

{ ------------------------------- AIN ------------------------------------- }

constructor TAINArchive.Init;
  var
    Sign: TStr5;
    q: String;
  begin
  Sign := GetSign;
  SetLength(Sign, Length(Sign)-1);
  Sign := Sign+#0;
  FreeStr := SourceDir+DNARC;
  TObject.Init;
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, 'AIN'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'AIN'));
  Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'e'));
  ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP, 'x'));
  Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a'));
  Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'm'));
  Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'd'));
  Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 't'));
  Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, '-g'));
  IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths, ''));
  ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, '-y'));
  RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''));
  SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, '-e'));
  Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
  RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,
         ''));
  SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         ''));
  StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, '-m4'));
  FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, '-m3'));
  FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, '-m3'));
  NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, '-m2'));
  GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, '-m1'));
  UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, '-m1'));
  ComprListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListChar,
         '@'));
  ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListChar,
       '@'));

  q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
  AllVersion := q <> '0';
  q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '1');
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
  end { TAINArchive.Init };

function TAINArchive.GetID;
  begin
  GetID := arcAIN;
  end;

function TAINArchive.GetSign;
  begin
  GetSign := sigAIN;
  end;

{
Модуль настраивался на ain 2.2

Для файлов с не очень длинными именами формат однострочный.
В этом же примере видно 'решение' проблемы y2k

TEMP\WINL                   5883  18.01.101  20:35:50

Для файлов с более длинными именами формат двухстрочный:

TEMP\KBM35012\KMBR.BIN
                             338  20.08.97  20:43:38

}
procedure TAINArchive.GetFile;
  var
    l: LongInt;
    DT: DateTime;
    s: String;
  begin
  if TextRec(ListFile).Handle = 0 then
    begin { первый вызов: вызов архиватора для вывода оглавления }
    FileInfo.Last := 2;
    ArcFile^.Close;
    ListFileName := MakeNormName(TempDir, '!!!DN!!!.TMP');
    s := '/C '
      {$IFDEF OS2}
      +SourceDir+'dndosout.bat '+ListFileName+' '
      {$ENDIF}
      +UnPacker^+' v '+ArcFileName
      {$IFNDEF OS2}
      +' > '+ListFileName
      {$ENDIF}
      ;
    if Length(s) < 126 then
      AnsiExec(GetEnv('COMSPEC'), s)
    else
      MessageBox(^C+GetString(dlCmdLineTooLong), nil, mfOKButton+mfError);
    System.Assign(ListFile, ListFileName);
    System.Reset(ListFile);
    if IOResult <> 0 then
      Exit;
    { Пропуск шапки и чтение первой строки файлов }
    repeat
      if Eof(ListFile) then
        Exit;
      Readln(ListFile, s);
      if IOResult <> 0 then
        Exit;
    until (Pos('File name', s) <> 0) or (Pos('Имя файла', s) <> 0);
    repeat
      if Eof(ListFile) then
        Exit;
      Readln(ListFile, s);
      if IOResult <> 0 then
        Exit;
    until s <> '';
    end
  else
    System.Readln(ListFile, s);
  l := Pos(' ', s);
  if l = 1 then
    begin
    Close(ListFile);
    EraseFile(ListFileName);
    TextRec(ListFile).Handle := 0;
    FileInfo.Last := 1;
    Exit;
    end;
  FileInfo.Last := 0;

  { чтение данных об очередном файле}
  if l = 0 then
    begin { длина и прочее в следующей строке }
    FileInfo.FName := s;
    Readln(ListFile, s);
    end
  else
    begin
    FileInfo.FName := Copy(s, 1, l-1);
    System.Delete(s, 1, l);
    end;
  DelLeft(s);
  l := Pos(' ', s);
  FileInfo.USize := StoI(Copy(s, 1, l-1));
  FileInfo.PSize := FileInfo.USize;
  System.Delete(s, 1, l);
  DelLeft(s);
  DT.Day := StoI(Copy(s, 1, 2));
  DT.Month := StoI(Copy(s, 4, 2));
  DT.Year := 1900 + StoI(fDelRight(Copy(S,7,3)));
  System.Delete(s, 1, 10);
  DelLeft(s);
  DT.Hour := StoI(Copy(s, 1, 2));
  DT.Min := StoI(Copy(s, 4, 2));
  DT.Sec := StoI(Copy(s, 7, 4));
  PackTime(DT, FileInfo.Date);
  end { TAINArchive.GetFile };

end.
