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
unit archUC2; {UC2}

interface

uses
  Archiver
  ;

type
  PUC2Archive = ^TUC2Archive;
  TUC2Archive = object(TARJArchive)
    ListFileName: String;
    ListFile: System.Text;
    BaseDir: String;
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
    destructor Done; virtual;
    end;

implementation

uses
  Objects2, Advance2, Advance, DNApp, DnExec, Commands, Advance1, Messages,
  Dos, LFNvp
  ;

{ ----------------------------- UC2 ------------------------------------}

constructor TUC2Archive.Init;
  var
    Sign: TStr5;
    q: String;
  begin
  Sign := GetSign;
  SetLength(Sign, Length(Sign)-1);
  Sign := Sign+#0;
  FreeStr := SourceDir+DNARC;
  TObject.Init;
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, 'UC'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'UC'));
  Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'E'));
  ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,
         'E !NOF ##.'));
  Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'A'));
  Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'AM'));
  Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'D'));
  Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, ''));
  Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 'T'));
  IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths, ''));
  ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, '-F'));
  RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''));
  SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''));
  Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
  RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,
         ''));
  SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         ''));
  StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, ''));
  FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, '-TF'));
  FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, '-TF'));
  NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, '-TN'));
  GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, '-TT'));
  UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, '-TT'));
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
  end { TUC2Archive.Init };

function TUC2Archive.GetID;
  begin
  GetID := arcUC2;
  end;

function TUC2Archive.GetSign;
  begin
  GetSign := sigUC2;
  end;

procedure TUC2Archive.GetFile;
  const
    FuckName = 'U$~RESLT.OK';
  var
    S: String;
    s1: String;

  procedure ReadName;
    var
      FName: String;
    begin
    System.Readln(ListFile, S); {NAME=[*]}
    FName := Copy(S, 13, Length(S)-13);
    System.Readln(ListFile, S);
    if S[7] = 'L' then
      begin
      {$IFNDEF OS2}
      if UseLFN then
        FName := Copy(S, 17, Length(S)-17);
      {$ENDIF}
      System.Readln(ListFile, S);
      end;
    FileInfo.FName := BaseDir+FName;
    end;

  procedure ReadDTA;
    var
      DT: DateTime;
    begin
    { DATE(MDY)= }
    DT.Month := StoI(Copy(S, 17, 2));
    DT.Day := StoI(Copy(S, 20, 2));
    DT.Year := StoI(Copy(S, 23, 4));
    System.Readln(ListFile, S);
    { TIME(HMS)= }
    DT.Hour := StoI(Copy(S, 17, 2));
    DT.Min := StoI(Copy(S, 20, 2));
    DT.Sec := StoI(Copy(S, 23, 4));
    PackTime(DT, FileInfo.Date);
    System.Readln(ListFile, S);
    { ATTRIB= }
    FileInfo.Attr := 0;
    end;
  label
    NextRecord;
  begin { TUC2Archive.GetFile }
  if TextRec(ListFile).Handle = 0 then
    begin { первый вызов: вызов архиватора для вывода оглавления }
    FreeObject(ArcFile); {AK155 если архив не закрыть, то архиватор
      выдаёт sharing violation }
    ListFileName := MakeNormName(TempDir, '!!!DN!!!.TMP');
    S := '/C '
      {$IFDEF OS2}
      +SourceDir+'dndosout.bat '+ListFileName+' '
      {$ENDIF}
      +UnPacker^+' ~D '+ArcFileName
      {$IFNDEF OS2}
      +' > '+ListFileName
      {$ENDIF}
      ;
    if Length(S) < 126 then
      AnsiExec(GetEnv('COMSPEC'), S)
    else
      MessageBox(^C+GetString(dlCmdLineTooLong), nil, mfOKButton+mfError);
    S := lfExpand(FuckName);
    if not ExistFile(S) then
      begin
      FileInfo.Last := 1;
      MessageBox(GetString(dlArcMsg6), nil, mfOKButton or mfError);
      Exit;
      end;
    EraseFile(S);
    System.Assign(ListFile, ListFileName);
    System.Reset(ListFile);
    end;
  FileInfo.Last := 0;
  { чтение данных об очередном файле}
NextRecord:
  repeat
    System.Readln(ListFile, S);
    s1 := Copy(S, 1, 7);
  until s1 <> '      T'; {TAG=[]}
  //  if s1 = 'LIST [\' then
  while s1 = 'LIST [\' do
    begin
    BaseDir := Copy(S, 7, Length(S)-7);
    System.Readln(ListFile, S);
    s1 := Copy(S, 1, 7);
    {    FileInfo.FName := BaseDir;
    FileInfo.USize := 0;
    FileInfo.PSize := 0;
    exit;}
    end;
  if s1 = 'END' then
    begin
    Close(ListFile);
    EraseFile(ListFileName);
    TextRec(ListFile).Handle := 0;
    FileInfo.Last := 1;
    Exit;
    end;
  if s1 = '   DIR' then
    begin
    ReadName;
    ReadDTA;
    //    FileInfo.Attr := Directory;
    FileInfo.FName := FileInfo.FName+'\';
    FileInfo.USize := 0;
    FileInfo.PSize := 0;
    end
  else if s1 = '   FILE' then
    begin
    ReadName;
    {VERSION=}
    System.Delete(S, 1, 14);
    if S <> '0' then
      begin
      if not AllVersion then
        begin { игнорируем этот файл }
        repeat
          Readln(ListFile, S);
        until S[7] = 'A';
        goto NextRecord;
        end;
      if Length(S) = 1 then
        S := '0'+S;
      FileInfo.FName := FileInfo.FName+';'+S;
      end;
    {SIZE=}
    System.Readln(ListFile, S);
    System.Delete(S, 1, 11);
    FileInfo.USize := StoI(S);
    FileInfo.PSize := FileInfo.USize;
    System.Readln(ListFile, S);
    if S[7] = 'C' then
      System.Readln(ListFile, S); {CHECK=...}
    { uc 2.0 этой строки не формирует }
    ReadDTA;
    end
  else
    FileInfo.Last := 2;

  end { TUC2Archive.GetFile };

destructor TUC2Archive.Done;
  begin
  if TextRec(ListFile).Handle <> 0 then
    begin
    System.Close(ListFile);
    EraseFile(ListFileName);
    end;
  inherited Done;
  end;

end.


