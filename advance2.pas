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

unit Advance2; {File related functions}

interface

uses
  Defines, Advance, Views
  {$IFDEF DPMI32}, Streams{$ENDIF}
  ;

var
  ErrorFCode: Byte;

const
  AnyFileDir = $37;
    {` Сюда не входит VolumeID. Под OS/2 это то же самое, что и
     AnyFile, так как есть соответствующий IFDEF в DOS.PAS.
     Для DPMI на (кажется, Новеловских) сетевых дисках наличие
     бита VolumeId блокирует поиск всех файлов, кроме метки диска.
     Так что настоятельно рекомендуется использовать везде
     AnyFileDir вместо AnyFile и не читать метку без надобности. `}

function ExistFile(const FName: String): Boolean; {DataCompBoy}
//function  ExistDir(const DName: string): Boolean; { VK }
function FileTime(FileA: String): LongInt;
function FileNewer(FileA, FileB: String): Boolean;
function CalcTmpFName(Id: LongInt; const AExt: String; ANew: Boolean)
  : String; {DataCompBoy}
function CalcTmpId: LongInt;
procedure EraseByName(const FName: String); {DataCompBoy}
procedure EraseFile(const N: String); {DataCompBoy}
procedure EraseTempFile(S: String); {piwamoto}
{JO}
function ValidDrive(dr: Char): Boolean;
function GetDrive: Byte;
  {` Дать номер диска активной панели (0..25).
    Для сетевых путей получится 27 ('\'-'A') `}
procedure GetMask(var m: String);
function GetCurDrive: Char;
  {` Дать букву диска активной панели ('A'..'Z').
    Для сетевых путей получится '\' `}
function GetExt(const s: String): String;
  {` s - имя файла, возможно, с путём. Результат - расширение,
   начинающееся с точки. Если расширения нет - результат '.' `}
function Norm12(const s: String): Str12;
{-DataCompBoy-}
function DelSquashes(s: String): String;
  {` removes quotes `}
{$IFNDEF OS2}
function GetURZ(const s: String): Str12;
  {` cuts name to 8.3 `}
{$ENDIF}
function GetfURZ(const s: String): String;
  {` cuts name and path to 8.3 `}
function IsSeparatingChars(const s: String): Boolean;
{` JO: проверяет на наличие  пробелов и
  символов '+' ';' ',' '[' ']' '&' '^' `}
function SquashesName(const s: String): String;
  {` quotes name if needed `}
function InMask(Name, Mask: String): Boolean;
  {` does Name match Mask? `}
function InFilter(Name, Filter: String): Boolean;
  {` does Name match Filter? `}
function InDirFilter(Name, Filter: String): Boolean; {JO}
{FUNCTION  InExtMask(Name, Mask: string):Boolean;}
{does Ext match Mask? }

function InExtFilter(Name: String; const F: String): Boolean;
{` does extension of  Name match Filter? `}
(*
FUNCTION  InOldMask(Name,Mask: string):Boolean;     {DataCompBoy}
FUNCTION  InOldFilter(Name,Filter: string):Boolean; {DataCompBoy}
*)
function InSpaceMask(Name, Mask: String; ValidSpace: Boolean): Boolean;
function InSpaceFilter(Name, Filter: String): Boolean;

function IsMixedCase(const Name: String): Boolean; {JO}

function IsDir(const s: String): Boolean; {is this a directory? }
function MkName(const Nm, Mask: String): String;
{modifies name to fit Mask}
function GetPath(const S: String): String;
  {` Часть S, предшествующая имени `}
function GetName(const S: String): String;
  {` Имя с расширением `}
function GetSName(const S: String): String;
  {` Имя без расширения `}
function GetAttrStr(Attr: Word): Str6;
{$IFDEF DualName}
function GetShortRelPath(Path: String): String;
{$ENDIF}
function GetLongRelPath(Path: String): String;

{-DataCompBoy-}
function MakeFileName(S: String): String;
function MakeNormName(const S, S1: String): String; {DataCompBoy}
function GetFileAttr(const S: String): Word;
function SetFileAttr(const S: String; Attr: Word): Word;
function CorrectFile(const N: String): Boolean;
function PathExist(s: String): Boolean; {Is path exist}
{` Проверка существования каталога s (путь относительный).
  Параметр допускается как со слешем на конце, так и без него.
  Джокеры не допускаются. `}

function PathFoundInArc(S: String): Boolean; {JO}

{-DataCompBoy-}
procedure GetFTimeSizeAttr(const A: String; var ATime: LongInt;
{` Чтение через DosFileFirst данных о файле. Если файл не найден,
будет DOSError <> 0, так что эту функцию можно использовать и для
проверки наличия файла `}
    var ASize: TSize; var AAttr: Word);
{-DataCompBoy-}

var
  QSMask: string;
  LastSuccessPos: Integer;

function QSMaskPlusStar: String;
  {` добавить '*' в конце QSMask, если там её не было `}
procedure InitQuickSearch(Panel: PView);
procedure StopQuickSearch;
procedure DoQuickSearch(Key: Word);
function QuickSearchString(SizeX: Word): String;
  {` Построение строки для показа текущей маски быстрого поиска.
  Собственно строка выделяется цветом (тильдами), поэтому содержащиеся
  внутри неё тильды нужно защищать. Звезда в конце маски, если она
  подразумеваемая, выводится обычным цветом, а если действительно
  является частью маски, то выводится ярко `}

procedure FileChanged(const Name: String);
{-DataCompBoy-}

{$IFDEF DPMI32}
type
  PTempFile = ^TTempFile;
  TTempFile = object(TBufStream)
    constructor Init(const AExt: String; ABufSize: SW_Word);
    destructor Done; virtual;
    end;
  {$ENDIF}

function CompareFiles(const N1, N2: String): Boolean;

procedure MakeSlash(var S: String);
  {` Обеспечить '\' в конце S. Пустая строка остаётся пустой `}
procedure MakeNoSlash(var S: String);
  {` Обеспечить отсутствие '\' в конце S, кроме путей типа 'C:\' `}

implementation
uses
  Drivers, Dos, Lfnvp, VPUtils,
  {$IFNDEF DPMI32}Streams,{$ENDIF}
  Advance1, Strings,
  Commands, DNApp, DnIni, Memory, FlPanelX, dnHelp
  , VpSysLow, UKeyMap
  ;

var
  QSPanel: PView;
  SaveHelpCtx: Word;

{$IFDEF DPMI32}
constructor TTempFile.Init(const AExt: String; ABufSize: SW_Word);
  var
    S: FNameStr;
    L: LongInt;
  begin
  L := CalcTmpId;
  S := CalcTmpFName(L, AExt, True);
  inherited Init(S, (stCreate and fmDeny) or fmDenyAll or fmDenyChild,
     ABufSize);
  end;

destructor TTempFile.Done;
  var
    S: FNameStr;
  begin
  S := StrPas(FName);
  inherited Done;
  EraseFile(S);
  end;
{$ENDIF}

function CorrectFile(const N: String): Boolean;
  var
    I: Integer;
  begin
  CorrectFile := False;
  for I := 1 to Length(N) do
    if N[I] in IllegalCharSet then
      Exit; {DataCompBoy}
  CorrectFile := True;
  end;

{-DataCompBoy-}
function ExistFile(const FName: String): Boolean;
  var
    DirInfo: lSearchRec;
  begin
  lFindFirst(FName, Archive+ReadOnly+Hidden+SysFile, DirInfo);
  lFindClose(DirInfo);
  if DosError = 0 then
    ExistFile := True
  else
    begin
    ExistFile := False;
    ErrorFCode := DosError;
    end;
  end;
{-DataCompBoy-}
{AK155 21-01-2002 Эта программа дублирует PathExist, все ее вызовы
 (в dn1 и startup) заменил на вызовы PathExist}
(*
{ VK/ }
function  ExistDir(const DName: string): Boolean; {based on ExistFile}
var
 Dirinfo:lsearchrec;
begin
  If (DName<>'') and (DName[Length(DName)]='\')
  then lFindFirst(Copy(DName, 1, Length(DName)-1),Directory,DirInfo)
  else lFindFirst(DName,Directory,DirInfo);
 lFindClose(DirInfo);
 if DosError=0 then existdir:=True
 else begin
  Existdir:=False;
  ErrorFCode:=DosError;
 end;
end;
{ /VK }
*) {/AK155}
function FileTime(FileA: String): LongInt;
  var
    FA: lFile;
    TA: LongInt;
  begin
  lAssignFile(FA, FileA);
  lResetFileReadOnly(FA, 1);
  GetFtime(FA.F, TA);
  Close(FA.F);
  FileTime := TA;
  end;

function FileNewer(FileA, FileB: String): Boolean;
  {-Return true if FileA is newer than FileB, both known to exist}
  begin
  FileNewer := FileTime(FileA) > FileTime(FileB);
  end;

{-DataCompBoy-}
function CalcTmpFName;
  var
    I: Integer;
    S: String;
  begin
  I := 0;
  while I < 10000 do
    begin
    S := MakeNormName(SwpDir, Hex8(Id)+'.'+AExt);
    if not ExistFile(S) or not ANew then
      Break;
    Inc(Id);
    Inc(I);
    end;
  CalcTmpFName := S;
  end;
{-DataCompBoy-}

const
  LastTmpId: LongInt = -1;

function GetTmpId: LongInt;
  var
    IdL, lm, ld, lh, lmin: LongInt;
    y, m, d, dow, h, min, s, hund: Word;
  begin
  GetDate(y, m, d, dow);
  GetTime(h, min, s, hund);
  lm := m and 7;
  ld := d;
  lh := h;
  lmin := min;
  GetTmpId := (lm*259200000+
      ld*8640000+
      lh*360000+
      lmin*6000+
      s*100+
      hund+Random(4))+$DEADFACE;
  end;

function CalcTmpId: LongInt;
  var
    Id: LongInt;
    s: String;
  begin
  Id := GetTmpId;
  if  (LastTmpId <> -1) and (LastTmpId >= Id) then
    Id := LastTmpId+1;
  LastTmpId := Id;
  CalcTmpId := Id;
  end;

{-DataCompBoy-}
procedure EraseByName;
  var
    F: lFile;
  begin
  lAssignFile(F, FName);
  lEraseFile(F);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure EraseFile;
  var
    F: lFile;
    rc: LongInt;
  label TryDel;
  begin
  ClrIO;
  FileMode := $42;
  lAssignFile(F, N);
TryDel:
  lEraseFile(F);
  rc := IOResult;
  case rc of
    0, 2:
      begin
      end;
    5:
      begin
      lSetFAttr(F, Archive);
      lEraseFile(F);
      end;
    else {case}
      if SysErrorFunc(rc, Byte(N[1])-Byte('A')) = 1 then
        goto TryDel;
  end {case};
  ClrIO;
  end { EraseFile };

procedure EraseTempFile;
  var
    Dir: String;
    F: lFile;
  begin
  {JO: проверочка файла на нахождение во временном каталоге не помешает}
  Dir := GetPath(S);
  if UpStrg(Dir) = UpStrg(TempDir) then
    begin
    ClrIO;
    FileMode := $42;
    lAssignFile(F, S);
    lEraseFile(F);
    if IOResult = 5 then
      begin
      lSetFAttr(F, Archive);
      lEraseFile(F);
      end;
    ClrIO;
    end;
  end { EraseTempFile };

function ValidDrive(dr: Char): Boolean;
  var
    DriveNum, LogDrvMap: LongInt;
  begin { LogDrvMap: Bit 0: 'A:', Bit 1: 'B:', etc }
  LogDrvMap := SysGetValidDrives;
  dr := Upcase(dr);
  ValidDrive := (dr >= 'A') and (dr <= 'Z') and
    (((1 shl (Ord(dr)-Ord('A'))) and LogDrvMap) <> 0);
  end;

function GetDrive: Byte;
  begin
  Result := Byte(ActiveDir[1])-Byte('A');
  end;

procedure GetMask(var m: String);
  var
    q: String[12];
    i: Byte;
    b: Boolean;
  begin
  {Cat:warn}
  q := '????????.???';
  i := Pos('.', m);
  if i > 0 then
    if i > 9 then
      Delete(m, 9, i-9)
    else
      Insert(Copy('        ', 1, 9-i), m, i)
  else
    m := Copy(m+Strg(' ', 8), 1, 8)+'.   ';
  i := 1;
  b := True;
  while m[i] <> '.' do
    begin
    if b then
      begin
      b := b and (m[i] <> '*');
      if b then
        q[i] := m[i];
      end;
    Inc(i);
    end;
  Delete(m, 1, i);
  i := 1;
  b := True;
  while (i <= 3) and (Length(m) >= i) do
    begin
    if b then
      begin
      b := b and (m[i] <> '*');
      if b then
        q[i+9] := m[i];
      end;
    Inc(i);
    end;
  m := LowStrg(q);
  end { GetMask };

function GetCurDrive;
  begin
  GetCurDrive := Char(GetDrive+65)
  end;

function GetExt;
  var
    i: Byte;
  begin
  i := PosLastDot(s);
  if i >= Length(s) then
    Result := '.'
  else
    Result := Copy(s, i, MaxStringLength);
  end;

function Norm12;
  var
    R: String[12];
    I: Byte;
  begin
  if s[1] = '.' then
    begin
    Norm12 := AddSpace(s, 12);
    Exit
    end;
  System.FillChar(R[1], 12, ' ');
  R[0] := #12;
  R[9] := '.';
  I := PosChar('.', s);
  if I = 0 then
    I := Succ(Length(s))
  else
    Move(s[Succ(I)], R[10], Min(Length(s)-I, 3));
  if I > 8 then
    I := 8
  else
    Dec(I);
  Move(s[1], R[1], I);
  I := 1;
  while I <= 12 do
    if R[I] = '*'
    then
      while (I <> 9) and (I <= 12) do
        begin
        R[I] := '?';
        Inc(I)
        end
    else
      Inc(I);
  Norm12 := R
  end { Norm12 };

{-DataCompBoy-}
function DelSquashes(s: String): String;
  var
    i: Byte;
  begin
  i := 1;
  while i <= Length(s) do
    if s[i] = '"' then
      Delete(s, i, 1)
    else
      Inc(i);
  DelSquashes := s;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
{$IFNDEF OS2}
function GetURZ;
  var
    a, aa, aaa: String;
  begin
  lFSplit(s, a, aa, aaa);
  GetURZ := Copy(aa, 1, 8)+'.'+Copy(aaa, 2, 3);
  end;

{-DataCompBoy-}
{$ENDIF}
{-DataCompBoy-}
function GetfURZ;
  var
    a, aa, aaa: String;
  begin
  lFSplit(s, a, aa, aaa);
  GetfURZ := a+Copy(aa, 1, 8)+'.'+Copy(aaa, 2, 3);
  end;
{-DataCompBoy-}
{JO}
function IsSeparatingChars(const s: String): Boolean;
  var
    i: Byte;
  begin
  IsSeparatingChars := False;
  for i := 1 to Length(s) do
    if s[i] in [' ', '+', ';', ',', '[', ']', '&', '^'] then
      IsSeparatingChars := True;
  end;
{/JO}
{-DataCompBoy-}
function SquashesName;
  begin
  {JO}
  if IsSeparatingChars(s) then
    SquashesName := '"'+s+'"'
  else
    SquashesName := s;
  {/JO}
  end;
{-DataCompBoy-}

{ Вспомогательная программа для InMask; обработка и маски,
и имени начинается с указанных позиций.
  Имя не долджно сожержать ничего, кроме символов имени
(в частности, не должно быть обрамляющих кавычек).
  Спецсимволы маски обрабатываются так:
  - '"' игнорируется;
  - '?' сопоставляется с ровно одним символом;
  - '|' сопоставляется с ровно одной цифрой;
  - '>' сопоставляется со всеми символами до последней
    точки включительно;
  - '*' сопоставляется с любой последовательностью (пустой
    в том числе). Для этого предпринимаются попытки найти в имени
    ближайший следующий за звездой контекст маски и сопоставить
    хвосты маски и имени (рекурсия). Если этот контекст маски
    можно найти более, чем в одном месте остатка имени, все эти
    места проверяются последовательно.

  Переменная LastSuccessPos отслеживает позицию имени, на которой
остановилось успешное сопоставление с незвёздным контекстом.
Используется при быстром поиске, где маска всегда заканчивается звездой,
и определяет позицию курсора в текущем найденном имени.
В других применениях (кроме быстрого поиска) эта переменная не используется.
  }

function InMaskA(const Name: String; const  Mask: String;
    iName, iMask: Integer): Boolean;
  var
    i, l, s: Integer;
    Exact: string; {контекст, следующий за звездой }
  begin
  Result := False;
  while iMask <= Length(Mask) do
    begin
    if iName > Length(Name) then
      begin
      while (iMask <= Length(Mask))  and (Mask[iMask] = '*') do
        Inc(iMask);
      if iMask > Length(Mask) then
        Result := True;
      Exit;
      end;
    case Mask[iMask] of
      '"':
        Inc(iMask);
      '>': { Переход вправо к расширению }
        begin
        Inc(iMask);
        l := PosLastDot(Name);
        if l < iName then
          Exit;
        iName := l;
        if iName <= Length(Name) then
          inc(iName);
        LastSuccessPos := iName;
        end;
      '?':
        begin
        Inc(iMask);
        Inc(iName);
        LastSuccessPos := iName;
        end;
      '|': { Цифра }
        begin
        if not (Name[iName] in ['0'..'9']) then
          Exit;
        Inc(iMask);
        Inc(iName);
        LastSuccessPos := iName;
        end;
      '*':
        begin
        inc(iMask);
        while (iMask <= Length(Mask)) and (Mask[iMask] = '*') do
          inc(iMask);
        if iMask > Length(Mask) then
          begin
          Result := True; Exit;
          end;
        l := iMask;
        Exact := '';
        while (l <= Length(Mask)) and
              not (Mask[l] in ['?', '|', '*', '>'])
        do
          begin
          if Mask[l] <> '"' then
            Exact := Exact + Mask[l];
          inc(l);
          end;
        if l <> iMask then
          begin
          i := iName;
          while True do
            begin { попытки найти Exact и сопоставить остаток маски
              с остатком имени. Цикл нужен, так как Exact может
              встретиться несколько раз.
              i - текущая точка имени, l - текущая точка маски }
            i := SPos(Exact, Name, i);
            if i = 0 then
              Exit; { Окончательная неудача }
            inc(i, l-iMask);
            s := LastSuccessPos;
            LastSuccessPos := i;
            if InMaskA(Name, Mask, i, l) then
              begin
              Result := True; Exit; { Удача }
              end;
            LastSuccessPos := s;
            end;
          end
        else if Mask[l] in ['?', '|'] then
          begin { указанный джокер вслед за звездой }
          s := LastSuccessPos;
          for i := iName to Length(Name) do
            if InMaskA(Name, Mask, i, l) then
              begin
              Result := True; Exit; { Удача }
              end;
          LastSuccessPos := s;
          end;
        end;
      else {case}
        begin
        if Mask[iMask] <> Name[iName] then
          exit;
        Inc(iMask);
        Inc(iName);
        LastSuccessPos := iName;
        end
    end {case};
    end;
  l := Length(Name);
  if Name[l] = '.' then
    Dec(l); { Это нужно для сопоставлений типа Name='CMD.' Mask = 'CMD',
      см. добавление точки в InMask }
  Result := (iName >= l+1);
  end;

function InMask(Name, Mask: String): Boolean;
  var
    s: Integer;
  begin
  if Name = '..' then
    begin
    Result := False; Exit;
    end;
  if Mask = '' then
    begin
    Result := True; Exit;
    end;
  if Pos('.', Name) = 0 then
    Name := Name + '.'; { Подразумеваемая точка в конце имени без расширения }
  UpStr(Mask);
  UpStr(Name);
  s := LastSuccessPos;
  LastSuccessPos := 1;
  Result := InMaskA(Name, Mask, 1, 1);
  if not Result then
    LastSuccessPos := s
  else
    if (LastSuccessPos = Length(Name)+1) and
       (Name[LastSuccessPos-1] = '.')
    then { Стали на воображаемой точке - надо вернуться }
      Dec(LastSuccessPos);
  end { InMask };

function InFilter;
  var
    i, l: Integer;
    S: String;
    Inv: Boolean;
    Literal: Boolean;
  begin
  InFilter := False;
  l := Length(Filter);
  Literal := False;
  while l > 0 do
    begin
    i := l;
    while i > 0 do
      begin
      if Filter[i] = '"' then
        Literal := not Literal
      else if not Literal and (Filter[i] in [',', ';']) then
        Break;
      Dec(i);
      end;
    S := Copy(Filter, i+1, l-i);
    DelLeft(S);
    DelRight(S);
    if S <> '' then
      begin
      Inv := S[1] = '-';
      if Inv then
        begin
        Delete(S, 1, 1); {DelFC(S);}
        DelLeft(S);
        end;
      if S <> '' then
        begin
        if InMask(Name, S) then
          begin
          Result := not Inv;
          Exit;
          end;
        end;
      end;
    l := i-1;
    end;
  end { InFilter };

{JO}
function InDirFilter;
  var
    i: Integer;
    S: String;
    B: Boolean;
    j: Boolean;
  begin
  InDirFilter := True;
  while Length(Filter) > 0 do
    begin
    i := Length(Filter)+1;
    j := False;
    repeat
      Dec(i);
      if Filter[i] = '"' then
        j := not j;
    until (i = 1) or ((Filter[i] in [';', ',']) and not j);
    if Filter[i] in [';', ','] then
      S := Copy(Filter, i+1, MaxStringLength)
    else
      S := Filter;
    B := S[1] <> '-';
    SetLength(Filter, i-1);
    InDirFilter := B;
    if not B then
      Delete(S, 1, 1); {DelFC(S);}
    DelLeft(S);
    DelRight(S);
    if  (S <> '') and (S[Length(S)] = '\')
           and InMask(Name, Copy(S, 1, Length(S)-1))
    then
      Exit;
    end;
  InDirFilter := False;
  end { InDirFilter };
{/JO}

function InExtFilter(Name: String; const F: String): Boolean;
  begin
  Result := InFilter(Copy(Name, PosLastDot(Name)+1, MaxStringLength), F);
  end;

function InSpaceMask;
  var
    i: Integer;
    j: Boolean;
  begin
  i := 13;
  repeat
    Dec(i);
    if  (Mask[i] = '?') or ((Mask[i] = ' ') and ValidSpace)
           or (UpCase(Mask[i]) = UpCase(Name[i]))
      or (i = 9)
    then
    else
      begin
      InSpaceMask := False;
      Exit
      end
  until i = 0;
  InSpaceMask := True
  end;

function InSpaceFilter;
  var
    i: Integer;
    S: String[13];
    B: Boolean;
  begin
  InSpaceFilter := True;
  if Pos(' ', Filter) > 0 then
    Filter := DelSpaces(Filter);
  UpStr(Filter);
  UpStr(Name);
  if Filter = '' then
    Exit;
  Name := Norm12(Name);
  repeat
    if Filter[Length(Filter)] = ';' then
      SetLength(Filter, Length(Filter)-1);
    if Length(Filter) <> 0 then
      begin
      i := Length(Filter);
      while (i > 1) and (Filter[pred(i)] <> ';') do
        Dec(i);
      S := Copy(Filter, i, Succ(Length(Filter)-i));
      B := S[1] = '-';
      InSpaceFilter := not B;
      if B then
        Delete(S, 1, 1); {DelFC(S);}
      DelLeft(S);
      if  (S <> '') and InSpaceMask(Name, Norm12(S), True) then
        Exit;
      SetLength(Filter, pred(i));
      end
  until Length(Filter) = 0;
  InSpaceFilter := False
  end { InSpaceFilter };

{JO} {piwamoto}
function IsMixedCase(const Name: String): Boolean;
  var
    MixedDir, MixedName, MixedExt: String;
  begin
  lFSplit(Name, MixedDir, MixedName, MixedExt);
  if  ( (UpStrg(MixedName) = MixedName) or (LowStrg(MixedName) =
         MixedName)) and
      ( (UpStrg(MixedExt) = MixedExt) or (LowStrg(MixedExt) = MixedExt))
  then
    IsMixedCase := False
  else
    IsMixedCase := True;
  end;
{JO} {piwamoto}

{-DataCompBoy-}
function IsDir(const s: String): Boolean;
  var
    SR: lSearchRec;
  begin
  lFindFirst(s, Directory shl 8 or AnyFileDir, SR);
  if DosError = 0 then
    IsDir := SR.SR.Attr and Directory <> 0
  else
    IsDir := False;
  lFindClose(SR);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function MkName(const Nm, Mask: String): String;
  var
    aa, aaa: String;
    bb, bbb: String;
    os: String;
    i: Byte;
    fp: Byte;
  begin
  lFSplit(Nm, os, aa, aaa);
  lFSplit(Mask, os, bb, bbb);
  os := '';
  fp := 0;
  for i := 1 to Length(bb) do
    begin
    Inc(fp);
    case bb[i] of
      '?':
        if fp <= Length(aa) then
          os := os+aa[fp];
      '*':
        begin
        os := os+Copy(aa, fp, MaxStringLength)
            +Copy(bb, i+1, MaxStringLength);
        if Pos('*', os) <> 0 then
          {Pavel Anufrikov -> }
          begin
          Insert(Copy(aaa, 2, MaxStringLength), os, Pos('*', os));
          aaa := '';
          end; { <- Pavel Anufrikov}
        while Pos('?', os) <> 0 do
          Delete(os, Pos('?', os), 1);
        while Pos('*', os) <> 0 do
          Delete(os, Pos('*', os), 1);
        Break;
        end;
      '>':
        if fp > 2 then
          Dec(fp, 2);
      '<':
        ;
      else {case}
        os := os+bb[i];
    end {case};
    end;
  fp := 0;
  for i := 1 to Length(bbb) do
    begin
    Inc(fp);
    case bbb[i] of
      '?':
        if fp <= Length(aaa) then
          os := os+aaa[fp];
      '*':
        begin
        os := os+Copy(aaa, fp, MaxStringLength)
            +Copy(bbb, i+1, MaxStringLength);
        while Pos('?', os) <> 0 do
          Delete(os, Pos('?', os), 1);
        while Pos('*', os) <> 0 do
          Delete(os, Pos('*', os), 1);
        Break;
        end;
      '>':
        if fp > 2 then
          Dec(fp, 2);
      '<':
        ;
      else {case}
        os := os+bbb[i];
    end {case};
    end;
  MkName := os;
  end { MkName };
{-DataCompBoy-}

function GetPath;
  var
    Name, ext: String;
  begin
  lFSplit(S, Result, Name, Ext);
  end;

function GetName;
  var
    Name, ext: String;
  begin
  lFSplit(S, Result, Name, Ext);
  Result := Name+Ext;
  end;

{-DataCompBoy-}
function GetSName;
  var
    B: Byte;
    Pe: Byte;
  begin
  Pe := Length(S)+1;
  for B := Length(S) downto 1 do
    begin
    if  (S[B] = '.') and (Pe = Length(S)+1) then
      Pe := B;
    if S[B] in ['\', '/'] then
      Break;
    end;
  if S[B] in ['\', '/'] then
    B := B+1
  else
    Pe := Pe-1; {JO}
  GetSName := Copy(S, B, Pe-B);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function GetAttrStr(Attr: Word): Str6;
  var
    AttrStr: Str6;
  begin
  AttrStr := 'RHSVDA';
  if Attr and $01 = 0 then
    AttrStr[1] := '-';
  if Attr and $02 = 0 then
    AttrStr[2] := '-';
  if Attr and $04 = 0 then
    AttrStr[3] := '-';
  if Attr and $08 = 0 then
    AttrStr[4] := '-';
  if Attr and $10 = 0 then
    AttrStr[5] := '-';
  if Attr and $20 = 0 then
    AttrStr[6] := '-';
  GetAttrStr := AttrStr;
  end;

{$IFDEF DualName}
{-DataCompBoy-}
function GetShortRelPath(Path: String): String;
  var
    CD: String;
  begin
  if Path[Length(Path)] in ['\', '/'] then
    SetLength(Path, Length(Path)-1);
  Path := lfGetShortFileName(Path);
  lGetDir(0, CD);
  if CD[Length(CD)] in ['\', '/'] then
    SetLength(CD, Length(CD)-1);
  CD := lfGetShortFileName(CD);
  if UpStrg(Copy(Path, 1, Length(CD))) = UpStrg(CD)
  then
    Delete(Path, 1, Length(CD));
  if Path[1] in ['\', '/'] then
    Delete(Path, 1, 1); {DelFC(Path);}
  GetShortRelPath := Path;
  end;
{-DataCompBoy-}
{$ENDIF}
{-DataCompBoy-}
function GetLongRelPath(Path: String): String;
  var
    CD: String;
  begin
  if Path[Length(Path)] in ['\', '/'] then
    SetLength(Path, Length(Path)-1);
  {$IFDEF DPMI32}
  Path := lfGetLongFileName(Path);
  {$ENDIF}
  lGetDir(0, CD);
  if CD[Length(CD)] in ['\', '/'] then
    SetLength(CD, Length(CD)-1);
  if UpStrg(Copy(Path, 1, Length(CD))) = UpStrg(CD)
  then
    Delete(Path, 1, Length(CD));
  if Path[1] in ['\', '/'] then
    Delete(Path, 1, 1); {DelFC(Path);}
  GetLongRelPath := Path;
  end;
{-DataCompBoy-}

function MakeFileName(S: String): String;
  var
    I: Integer;
  begin
  if  (S <> '..') and (S[Length(S)] = '.') then
    SetLength(S, Length(S)-1);
  MakeFileName := S;
  end;

{-DataCompBoy-}
function MakeNormName(const S, S1: String): String;
  var
    i, j: Byte;
  begin
  i := Length(S);
  while S[i] = ' ' do
    Dec(i);
  j := Length(S1);
  while S1[j] = ' ' do
    Dec(j);
  if i > 0 then
    begin
    if  (S[i] in ['\', '/']) then
      MakeNormName := Copy(S, 1, i)+Copy(S1, 1, j)
    else
      MakeNormName := Copy(S, 1, i)+'\'+Copy(S1, 1, j);
    end
  else
    MakeNormName := S1;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function SetFileAttr(const S: String; Attr: Word): Word;
  var
    F: lFile;
  begin
  lAssignFile(F, S);
  lSetFAttr(F, Attr);
  SetFileAttr := DosError;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function GetFileAttr(const S: String): Word;
  var
    F: lFile;
    Attr: Word;
  begin
  Attr := 0;
  lAssignFile(F, S);
  lGetFAttr(F, Attr);
  GetFileAttr := Attr;
  end;
{-DataCompBoy-}

function PathExist(s: String): Boolean;
  var
    SR: lSearchRec;
    Attr: Longint;
    IsRoot: Boolean;
  label
    FClose;
  begin
  if (s = '') or (Pos('?', s) <> 0) or (Pos('*', s) <> 0) then
    Result := false
  else
    begin
    S := lFExpand(S);
    IsRoot := S[Length(s)] = '\';

    Attr := AnyFileDir or (Directory shl 8);
    lFindFirst(S, Attr, SR);
    Result := True;
    if (DosError = 0) then
      goto FClose;

    { Под виндой (NT - точно, 98 - не знаю) в корне получается
      RC=2, если сам этот корень существует. Для несуществующей
      шары, несуществующего диска или невставленного сменного
      носителя RC другой }
    if IsRoot and (DosError = 2) then
      goto FClose; {}

    { Бывает, что каталог есть, а SysFindFirst даёт ошибку.
      Так бывает, например, в корне шары, а также при каких-то
      невыясненных обстоятельствах под OS/2 на FAT16. Поэтому, если
      сам каталог в лоб не нашли, то попытаемся найти что-то внутри
      каталога. Ограничиваться только этим тоже нельзя, так как бывает,
      что каталог совсем-совсем пустой, даже без '.' и '..' внутри.
      Так бывает, например, под Win на DirectCD CDRW.}
    Attr := AnyFileDir;
    if IsRoot then
      begin { корень диска }
      delete(S, Length(s), 1);
      end;
    S := S + '\*.*';
    lFindFirst(S, Attr, SR);
    Result := DosError in [0, 2, 18];
FClose:
    lFindClose(SR);
    end;
  end;

{JO}
function PathFoundInArc(S: String): Boolean;
  begin
  if PosChar(':', Copy(S, 3, MaxStringLength)) > 0 then
    PathFoundInArc := True
  else
    PathFoundInArc := False;
  end;
{/JO}

{-DataCompBoy-}
procedure GetFTimeSizeAttr(const A: String; var ATime: LongInt;
    var ASize: TSize; var AAttr: Word);
  var
    SR: lSearchRec;
  begin
  ClrIO;
  lFindFirst(A, AnyFileDir, SR);
  ATime := SR.SR.Time;
  ASize := SR.FullSize;
  AAttr := SR.SR.Attr;
  lFindClose(SR);
  end;
{-DataCompBoy-}

function QSMaskPlusStar: String;
  begin
  Result := QSMask;
  if Result[Length(Result)] <> '*' then
    Result := Result + '*';
  end;

procedure InitQuickSearch(Panel: PView);
  begin
  QSMask := '';
  LastSuccessPos := 1;
  QuickSearch := True;
  QSPanel := Panel;
  with PFilePanelRoot(QSPanel)^ do
    begin
    SaveHelpCtx := HelpCtx;
    HelpCtx := hcQuickSearch;
    InfoView^.Draw; { Чтобы появилась маска из одной звёздочки }
    end;
  end;

procedure StopQuickSearch;
  begin
  if QuickSearch then
    begin
    QuickSearch := False;
    with QSPanel^ do
      HelpCtx := SaveHelpCtx;
    end;
  end;

procedure DoQuickSearch(Key: Word);
  begin
  case Key of
    kbCtrlLeft:
      QSMask := QSMask + '>';
    kbCtrlRight:
      QSMask := QSMaskPlusStar + '.';
    kbBack:
      begin
      if QSMask <> '' then
        Delete(QSMask, Length(QSMask), 1);
      end
    else
      if (Char(Lo(Key)) <> '*') or (QSMask[Length(QSMask)] <> '*') then
        QSMask := QSMask+Char(Lo(Key));
  end {case};
  end { DoQuickSearch };

function QuickSearchString(SizeX: Word): String;
  var
    S: String; { текст маски, поднотовленный к показу }
    l, i: Integer;
    DefaultStar: Boolean;
  begin
  Result := GetString(dlFileSearch);
  DefaultStar := QSMask[Length(QSMask)] <> '*';
  L := SizeX - Length(Result) - Ord(DefaultStar) - 1;

  { Определяем i - начало выводимой части маски }
  if Length(QSMask) > L then
    begin  { Обрезание слева }
    Result := Result + #17'~';
    i := Length(QSMask)-L;
    end
  else
    begin
    Result := Result + '~';
    i := 0;
    end;

  l := 0; { L - это длина S }
  while i <> Length(QSMask) do
    begin
    inc(i);
    if QSMask[i] = '~' then
      begin
      Inc(l);
      S[l] := #0;
      end;
    Inc(l);
    S[l] := QSMask[i]
    end;
  SetLength(S, l+1);
  S[l+1] := '~';
  if DefaultStar then
    S := S + '*';
  Result := Result + S;
  end;

{-DataCompBoy-}
procedure FileChanged(const Name: String);
  var
    Dr: String;
    Nm: String;
    Xt: String;
  begin
  lFSplit(Name, Dr, Nm, Xt);
  Abort := False;
  GlobalMessage(evCommand, cmRereadDir, @Dr);
  GlobalMessage(evCommand, cmRereadInfo, nil);
  GlobalMessage(evCommand, cmRereadTree, @Dr);
  end;
{-DataCompBoy-}

function CompareFiles(const N1, N2: String): Boolean;
  label Finish;
  const
    BufSize = 2048;
  var
    S1, S2: TDOSStream;
    B1, B2: Pointer;
    B: Boolean;
    I: LongInt;
  begin
  CompareFiles := False;
  B := False;
  B1 := nil;
  B2 := nil;
  S1.Init(N1, stOpenRead);
  if S1.Status <> stOK then
    begin
    S1.Done;
    Exit
    end;
  S2.Init(N2, stOpenRead);
  if  (S2.Status <> stOK) or (S1.GetSize <> S2.GetSize) then
    goto Finish;
  B1 := MemAlloc(BufSize);
  if B1 = nil then
    goto Finish;
  B2 := MemAlloc(BufSize);
  if B2 = nil then
    goto Finish;
  I := BufSize;
  CompareFiles := True;
  while (S1.Status = stOK) and (S2.Status = stOK) and (I > 0) and not B
  do
    begin
    I := MinBufSize(S1.GetSize-S1.GetPos, BufSize);
    if I = 0 then
      Break;
    S1.Read(B1^, I);
    S2.Read(B2^, I);
    asm
        push esi
        push edi
        mov  edi, B1
        mov  esi, B2
        cld
        mov  ecx, I
        rep  cmpsb
        jz   @1
        xor  al, al
        mov  @Result, al
        inc  al
        mov  B, al
       @1:
        pop  edi
        pop  esi
      end;
    end;
  if not B then
    CompareFiles := (S1.Status = stOK) and (S2.Status = stOK);
Finish:
  if B1 <> nil then
    FreeMem(B1, BufSize);
  if B2 <> nil then
    FreeMem(B2, BufSize);
  S1.Done;
  S2.Done;
  end { CompareFiles };

procedure MakeSlash(var S: String);
  begin
  if (S <> '') and (S[Length(S)] <> '\') then
    S := S + '\';
  end;

procedure MakeNoSlash(var S: String);
  begin
  if (Length(S) > 1) and (S[Length(S)] in ['\', '/']) and
     (S[Length(S)-1] <> ':')
  then
    SetLength(S, Length(S)-1);
  end;

end.
