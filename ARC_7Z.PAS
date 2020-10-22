{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
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
//////////////////////////////////////////////////////////////////////////
//
//  Version history:
//
//  2005.02.07 ported from DN OSP 4.9.0 by Max Piwamoto
{  15.02.2005 AK155: Мелкие коррекции.
    * ExecAnsiString приводил к порче экрана DN, заменён на ExecStringRR.
    * В TS7ZArchive.GetFile в связи с AnsiString выплыло несколько
      некоректностей вроде S[1] для пустой строки. Исправил.
    * Приформирование в конце строки '\' для каталогов - это неправильно
      (надо сначала удалить проблелы) и не нужно (это будет сделано
      позже по FileInfo.Attr = Directory. Приводило к появлению
      фантомных каталогов с пробелами в конце (7z 4.11). Убрал.
    * Сделал переформатирование.
    - Вижу глюки с поиском извне. Если в filefind снять запрет на поиск в
      7z-архивах, то поиск почти работает, но из панели поиска переход
      на любой файл внутри 7z-архива приводит при выходе из архива к
      Sharing violation (при удалении файла в Done). Если проигнорировать
      - всё работает нормально.
      Кроме того, поиск иногда (или всегда?) показывает файл, который
      на самом деле находится в другом архиве.
      Источник проблем IMHO в том, что файл списка слишком долго держится
      открытым.
}
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
unit Arc_7Z; {7-Zip}

interface
uses
  Archiver
  ;

type
  PS7ZArchive = ^TS7ZArchive;
  TS7ZArchive = object(TARJArchive)
    ListFileName: String;
    ListFile: System.Text;
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
    destructor Done; virtual;
    end;

implementation
uses
  advance, advance1, advance2, Defines, Objects2, Streams, Dos, DnExec
  ;

{ --- 7-Zip implemented by piwamoto --- }

constructor TS7ZArchive.Init;
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
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, '7Z'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, '7Z'));
{$ELSE}
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, '7ZA'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, '7ZA'));
{$ENDIF}
  Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'e'));
  ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP, 'x'));
  Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a'));
  Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, ''));
  Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'd'));
  Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, '-p'));
  Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 't'));
  IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths, ''));
  ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, '-y'));
  RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''));
  SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,
         '-sfx'));
  Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
  RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PRecurseSubDirs, '-r0'));
  SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         ''));
  StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, '-mx0'));
  FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, '-mx1'));
  FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, '-mx1'));
  NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, '-mx5'));
  GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, '-mx7'));
  UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, '-mx9'));
  ComprListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListChar,
         '@'));
  ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListChar,
       '@'));

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
  end { TS7ZArchive.Init };

function TS7ZArchive.GetID;
  begin
  GetID := arc7Z;
  end;

function TS7ZArchive.GetSign;
  begin
  GetSign := sig7Z;
  end;

procedure TS7ZArchive.GetFile;
  var
    DT: DateTime;
    S: AnsiString;
  begin
  if TextRec(ListFile).Handle = 0 then
    begin { первый вызов: вызов архиватора для вывода оглавления }
    FreeObject(ArcFile);
    {AK155 если архив не закрыть, то архиватор
      выдаёт sharing violation }
    ListFileName := MakeNormName(TempDir, '!!!DN!!!.TMP');
    S := UnPacker^+' l '+SquashesName(ArcFileName)+' >'+ListFileName;
    ExecStringRR(S, '', False);
    System.Assign(ListFile, ListFileName);
    System.Reset(ListFile);
    repeat
      if Eof(ListFile) then
        begin
        FileInfo.Last := 2;
        Exit;
        end;
      Readln(ListFile, S);
    until (S <> '') and (S[1] = '-');
    end;
  Readln(ListFile, S);
  if (Length(S) < 54) or (S[1] = '-') then
    begin
    FileInfo.Last := 1;
    Exit;
    end;
  DT.Year := StoI(Copy(S, 1, 4));
  DT.Month := StoI(Copy(S, 6, 2));
  DT.Day := StoI(Copy(S, 9, 2));
  DT.Hour := StoI(Copy(S, 12, 2));
  DT.Min := StoI(Copy(S, 15, 2));
  DT.Sec := StoI(Copy(S, 18, 2));
  PackTime(DT, FileInfo.Date);
  FileInfo.USize := Str2Comp(fDelLeft(Copy(S, 27, 12)));
  FileInfo.PSize := Str2Comp(fDelLeft(Copy(S, 40, 12)));
  if S[21] = 'D' then
    FileInfo.Attr := Directory
  else
    FileInfo.Attr := 0;
  FileInfo.FName := '\'+fDelRight(Copy(S, 54, 255));
  FileInfo.Last := 0;
  end { TS7ZArchive.GetFile };

destructor TS7ZArchive.Done;
  begin
  if TextRec(ListFile).Handle <> 0 then
    begin
    System.Close(ListFile);
    EraseFile(ListFileName);
    end;
  inherited Done;
  end;

end.
