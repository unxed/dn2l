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
unit arc_RAR; {RAR}

interface

uses
  Archiver, Advance, Advance1, Defines, Objects2, Streams, Dos
  ;

type
  PRARArchive = ^TRARArchive;
  TRARArchive = object(TARJArchive)
    VersionToExtr: Byte;
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
    end;

type
  MainRARHdr = record
    Id: LongInt;
    HeadLen: AWord;
    HeadFlags: Byte;
    end;

  MainRAR2Hdr = record
    HeadCRC: AWord;
    HeadType: Byte;
    HeadFlags: AWord;
    HeadLen: AWord;
    Reserved1: AWord;
    Reserved2: LongInt;
    end;
  // HeadFags:
  // $01 - Атрибут тома (том многотомного архива)
  // $02 - Присутствует архивный комментарий
  // $04 - Атрибут блокировки архива
  // $08 - Атрибут непрерывного (solid) архива
  // $10 - Новая схема именования томов ('volname.partN.rar')
  // $20 - Присутствует информация об авторе или электронная подпись (AV)
  // $40 - Присутствует информация для восстановления
  // $80 - Заголовки блоков зашифрованы

var
  RAR2: Boolean;
  Encrypted: Boolean;

implementation

uses
  Messages, Commands
  ;

{ ----------------------------- RAR ------------------------------------}

constructor TRARArchive.Init;
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
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, 'RAR'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'RAR'));
  {$ELSE}
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, 'RAR32'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'RAR32'));
  {$ENDIF}
  Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'e'));
  ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP, 'x'));
  Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a'));
  Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'm'));
  Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'd'));
  Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, '-p'));
  Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 't'));
  IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,
         '-ep'));
  ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, '-y'));
  RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,
       '-rr'));
  SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,
         '-sfx'));
  Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, '-s'));
  RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PRecurseSubDirs, '-r0'));
  SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         '-ap'));
  StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, '-m0'));
  FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, '-m1'));
  FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, '-m2'));
  NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, '-m3'));
  GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, '-m4'));
  UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, '-m5'));
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
  q := GetVal(@Sign[1], @FreeStr[1], PSwapWhenExec, '1');
  SwapWhenExec := q <> '0';
  {$ENDIF}
  {$IFNDEF OS2}
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
  UseLFN := q <> '0';
  {$ENDIF}
  VersionToExtr := 0;
  end { TRARArchive.Init };

function TRARArchive.GetID;
  begin
  GetID := arcRAR;
  end;

function TRARArchive.GetSign;
  begin
  GetSign := sigRAR;
  end;

type
  LocRarHdr = record
    PSize: LongInt;
    USize: LongInt;
    CRC: AWord;
    HdrLen: AWord;
    Date: LongInt;
    Attr: Byte;
    Flags: Byte;
    Ver: Byte;
    NameLen: Byte;
    Method: Byte;
    CommLen: AWord;
    end;

  LocRar2Hdr = record
    HeadCRC: AWord;
    HeadType: Byte;
    HeadFlags: AWord;
    HeadSize: AWord;
    PSize: LongInt;
    USize: LongInt;
    OSVer: Byte;
    CRC: LongInt;
    Date: LongInt;
    Ver: Byte;
    Method: Byte;
    NameLen: AWord;
    Attr: LongInt;
    end;
  // HeadFlags:
  // $01 - файл продолжается из предыдущего тома
  // $02 - файл продолжается в следующем томе
  // $04 - файл зашифрован паролем
  // $08 - присутствует комментарий файла
  // $10 - используется информация из предыдущих файлов
  //       (флаг непрерывности) (для RAR 2.0 и старше)
  //
  //       биты 7 6 5 (для RAR 2.0 и выше):
  //            0 0 0    - размер словаря   64 Кб
  // $20        0 0 1    - размер словаря  128 Кб
  // $40        0 1 0    - размер словаря  256 Кб
  // $60        0 1 1    - размер словаря  512 Кб
  // $80        1 0 0    - размер словаря 1024 Кб
  // $A0        1 0 1    - размер словаря 2048 KB
  // $C0        1 1 0    - размер словаря 4096 KB
  // $E0        1 1 1    - файл является каталогом
  //
  // $100 - присутствуют поля HIGH_PACK_SIZE и HIGH_UNP_SIZE.
  //        Эти поля используются только для архивирования очень больших файлов
  //        (больше 2 Гб), для файлов меньшего объема эти поля отсутствуют
  // $200 - FILE_NAME содержит имена в обычном формате и в Unicode,
  //        разделённые нулём. В этом случае поле NAME_SIZE равно длине
  //        обычного имени плюс длина имени в формате Unicode плюс 1
  // $400 - после имени файла в заголовке находится 8 дополнительных байт,
  //        которые необходимы для увеличения надёжности шифрования ("соль")
  // $800 - флаг версии. Это старая версия файла, номер
  //        версии добавлен к имени файла как ';n'
  // $8000 - этот бит всегда установлен, так как общий размер
  //         блока HEAD_SIZE + PACK_SIZE (и плюс HIGH_PACK_SIZE,
  //         если установлен бит 0x100)

procedure TRARArchive.GetFile;
  var
    FP: TFileSize;
    Ps: Integer;
    P: LocRarHdr;
    P2: LocRar2Hdr;
    DirMask: LongInt;
  label 1;
  begin
1:
  if  (ArcFile^.GetPos = ArcFile^.GetSize) then
    begin
    if Encrypted then
      Msg(dlArcEncrypted, nil, mfOKButton);
    FileInfo.Last := 1;
    Exit;
    end;
  if  (ArcFile^.Status <> stOK) then
    begin
    FileInfo.Last := 2;
    Exit;
    end;
  if RAR2 then
    begin
    FP := ArcFile^.GetPos;
    ArcFile^.Read(P2, 7);
    if  (ArcFile^.Status <> stOK) then
      begin
      FileInfo.Last := 2;
      Exit;
      end;
    {piwamoto}
    {we must skip garbage (digital sign as example) at the end of archive}
    {check for valid HeadType: $72 can't be valid here, $7c..7f reserved for future RAR versions}
    if not (P2.HeadType in [$73..$7a, $7c..$7f]) then
      begin
      FileInfo.Last := 1;
      Exit;
      end;
    {/piwamoto}
    if P2.HeadType = $74 then
      begin
      ArcFile^.Read(P2.PSize, SizeOf(P2)-7);
      if  (ArcFile^.Status <> stOK) then
        begin
        FileInfo.Last := 2;
        Exit;
        end;
      FileInfo.Last := 0;
      FileInfo.Date := P2.Date;
      FileInfo.PSize := P2.PSize;
      FileInfo.USize := P2.USize;
      FileInfo.Attr := Byte(P2.HeadFlags and $04 <> 0)*Hidden;
      if P2.OSVer = 3 then
        DirMask := $4000 {Unix}
      else
        DirMask := Directory {DOS compatible};
      if P2.Attr and DirMask <> 0
      then
        FileInfo.Attr := FileInfo.Attr or Directory;
      if P2.NameLen > 255 then
        P2.NameLen := 255;
      if P2.HeadFlags and $100 <> 0 then
        begin { HIGH_PACK_SIZE and HIGH_UNP_SIZE presents}
        ArcFile^.Read(CompRec(FileInfo.PSize).Hi, 4);
        ArcFile^.Read(CompRec(FileInfo.USize).Hi, 4);
        end;
      ArcFile^.Read(FileInfo.FName[1], P2.NameLen);
      SetLength(FileInfo.FName, P2.NameLen);
      if P2.HeadFlags and $200 <> 0 then
        SetLength(FileInfo.FName, (PosChar(#0, FileInfo.FName)-1));
      {piwamoto: skip unicode names from winrar2.80beta1+ archives}
      repeat
        Ps := System.Pos('.\', FileInfo.FName);
        if Ps = 0 then
          Break;
        System.Delete(FileInfo.FName, Ps, 1);
      until False;
      if  (ArcFile^.Status <> stOK) then
        begin
        FileInfo.Last := 2;
        Exit;
        end;
      ArcFile^.Seek(CompToFSize(FP+P2.HeadSize+FileInfo.PSize));
      if P2.Ver > VersionToExtr then
        VersionToExtr := P2.Ver;
      Exit;
      end;
    if P2.HeadSize = 0 then
      P2.HeadSize := 7;
    if P2.HeadFlags and $8000 <> 0
    then
      begin
      ArcFile^.Read(FP, 4);
      ArcFile^.Seek(ArcFile^.GetPos-4);
      end
    else
      FP := 0;
    ArcFile^.Seek(ArcFile^.GetPos+FP+P2.HeadSize-7);
    goto 1;
    end
  else
    begin
    ArcFile^.Read(P, SizeOf(P)-2);
    if  (ArcFile^.Status <> stOK) or (P.NameLen = 0) then
      begin
      FileInfo.Last := 2;
      Exit;
      end;
    FileInfo.Last := 0;
    FileInfo.Date := P.Date;
    FileInfo.PSize := P.PSize;
    FileInfo.USize := P.USize;
    FileInfo.Attr := Byte(P.Flags and $04 <> 0)*Hidden;
    if P.Flags and $08 <> 0 then
      begin
      {ArcFile^.Read(P, P.CommLen);}
      ArcFile^.Seek(ArcFile^.GetPos+P.CommLen);
      if  (ArcFile^.Status <> stOK) then
        begin
        FileInfo.Last := 2;
        Exit;
        end;
      end;
    SetLength(FileInfo.FName, P.NameLen);
    ArcFile^.Read(FileInfo.FName[1], P.NameLen);
    if  (ArcFile^.Status <> stOK) then
      begin
      FileInfo.Last := 2;
      Exit;
      end;
    ArcFile^.Seek(ArcFile^.GetPos+P.PSize);
    if P.Ver > VersionToExtr then
      VersionToExtr := P.Ver;
    end;
  end { TRARArchive.GetFile };

end.
