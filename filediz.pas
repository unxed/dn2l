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

unit Filediz;

interface

uses
  FilesCol, Defines, Objects2,
  Commands
  ;

type
  TDizNameProc = function (const N: string; TextStart: Integer): Boolean;
  TDizLineProc = procedure;
  TDizEndProc = function: Boolean;
var
  LastDizLine: LongString;

function GetPossibleDizOwner(N: Integer): String;
function GetDizPath(const Path: String; PreferedName: String): String;
function CalcDPath(P: PDiz; Owen: PString): String;

procedure ExportDiz(
{` Внесение нового описания вместо старого, если оно было.
  Из прежнего контейнера удаляются описания к именами FR^.FlName
  и к OldName:, если оно задано.}
  const OldName: PFlName;
{` Имя, которое было раньше; используется при переименовании `}
  const NewLongName: string;
{` Данные о новом файле. Короткое имя может быть недостоверным.
  Путь (Owner) используется только если не задан TargetPath.
  Файл с таким длинным именем должен существовать (уже или ещё).
  FR^.DIZ содержит то, что, собственно, нужно занести.
`}
  var NewDiz: PDiz;
  TargetPath: string
{` Каталог, где находятся файл и контейнер описаний; если '', то
  используется FR^.Owner `}
);
{`}

procedure DeleteDiz(FR: PFileRec);
procedure GetDiz(FR: PFileRec);
 {` Обеспечить наличие DIZ, если это возможно `}
procedure SetDescription(PF: PFileRec; DizOwner: String);
function DizFirstLine(DIZ: PDiz): String;
  {` Вернуть первую строку текста описания.
   Если DIZ=nil - результат пустой `}

function DizMaxLine(DIZ: PDiz): String;
  {` Вернуть строку текста описания максимально возможной длины.
  Многострочное описание дочитывать, заменяя CRLF и начальные
  пробелы строк одним пробелом.
   Если DIZ=nil - результат пустой `}

function OpenFileList(const AConatainerPath: string): Boolean;
procedure ReadFileList(ProcessDizName: TDizNameProc;
    ProcessDizLine: TDizLineProc; ProcessDizEnd: TDizEndProc);

implementation
uses
  files, Startup, Advance1, Advance2, Advance,
  Lfn, Dos, Messages, DNApp, Drives
{$IFDEF DualName}
  , dnini
{$ENDIF}
  ;

var
  NewContainerFile: Text;
  IgnoreDiz: Boolean;
  LName: TUseLFN;
  OldConatainerPath: string;
  OldConatainerFile: Text;
  NewContainerNotEmpty: Boolean;

function GetPossibleDizOwner(N: Integer): String;
  var
    DIZ: String;
    I: Integer;
  begin
  GetPossibleDizOwner := '';
  DIZ := FMSetup.DIZ;
  while (N > 0) and (DIZ <> '') do
    begin
    I := PosChar(';', DIZ);
    if I = 0 then
      I := Length(DIZ)+1;
    if CorrectFile(Copy(DIZ, 1, I-1)) then
      begin
      Dec(N);
      if N = 0 then
        begin
        GetPossibleDizOwner := Copy(DIZ, 1, I-1);
        Exit;
        end;
      end;
    Delete(DIZ, 1, I);
    end;
  end { GetPossibleDizOwner };

function GetDizPath(const Path: String; PreferedName: String): String;
  var
    I: Integer;
  begin { GetDizPath }
  I := 0;
  if PreferedName = '' then
    begin
    PreferedName := GetPossibleDizOwner(1);
    I := 1;
    end;
  Result := PreferedName;
  PreferedName := '';
  repeat
    if Result = '' then
      begin
      Result := PreferedName; // файл придётся создавать
      Exit;
      end;
    Result := MakeNormName(Path, Result);
    if PreferedName = '' then
      PreferedName := Result; // теперь это полный путь
    if ExistFile(Result) then
      Exit; // контейнер найден
    Inc(I);
    Result := GetPossibleDizOwner(I);
  until False;
  end { GetDizPath };
{-DataCompBoy-}

{-DataCompBoy-}
procedure ReplaceT(P: PTextReader; var F: lText; Del: Boolean);
  var
    I: Integer;
    FName: String;
    Attr: Word;
  begin
  FName := P^.FileName;
  Dispose(P, Done);
  ClrIO;
  Close(F.T);
  ClrIO;
  Attr := GetFileAttr(FName);
  ClrIO;
  EraseByName(FName);
  I := IOResult;
  ClrIO;
  if not Del then
    begin
    lRenameText(F, FName);
    if  (IOResult <> 0) then
      begin
      if  (I <> 0) then
        Erase(F.T);
      CantWrite(FName);
      end
    else
      SetFileAttr(FName, Attr);
    end
  else
    lEraseText(F);
  ClrIO;
  end { ReplaceT };
{-DataCompBoy-}

{-DataCompBoy-}
procedure SetDescription(PF: PFileRec; DizOwner: String);
  var
    NewDIZ: LongString;
    K: Byte;
    S: string;
  begin
  if  (PF = nil) then
    Exit;
  if DizOwner = '' then
    begin
    S := GetPossibleDizOwner(1);
    if S = '' then
      begin
      MessageBox(GetString(dlNoPossibleName), nil, mfError);
      Exit;
      end;
    DizOwner := MakeNormName(PF^.Owner^, S);
    end;
  if  (PF^.DIZ <> nil) then
    NewDIZ := PF^.DIZ^.DIZText;
  K := PosChar(#13, NewDIZ);
  {JO: проверяем на наличие дополнительных строк.
   Если есть - редактируем только первую строку.}
  if K = 0 then
    K := 255;
  S := Copy(NewDiz, 1, K-1);
  if BigInputBox(GetString(dlEditDesc), GetString(dl_D_escription), S,
       255, hsEditDesc) <> cmOK
  then
    Exit;
  DelLeft(S);
  Delete(NewDiz, 1, K-1);
  Insert(S, NewDiz, 1);
  if PF^.DIZ = nil then
    begin
    New(PF^.DIZ);
    PF^.DIZ^.Container := nil;
    end;
  PF^.DIZ^.DizText := NewDiz;
  ExportDiz(nil, PF^.FlName[True], PF^.DIZ, PF^.Owner^);
  { if IOResult <> 0 then CantWrite(DIZOwner); }
  if not Startup.AutoRefreshPanels then
    RereadDirectory(PF^.Owner^);
  end { SetDescription };
{-DataCompBoy-}

  { прочитать непустую строку (хвостовые пробелы отбрасываются)}
function ReadNextS: Boolean;
  var
    l: Integer;
  begin
  Result := False;
  LastDizLine := '';
  repeat
    if Eof(OldConatainerFile) then
      Exit;
    Readln(OldConatainerFile, LastDizLine);
    if IOResult <> 0 then
      Exit;
    l := Length(LastDizLine);
    while (l <> 0) and (LastDizLine[l] = ' ') do
      Dec(l);
  until l <> 0;
  if l <> Length(LastDizLine) then
    SetLength(LastDizLine, l);
  Result := True;
  end;

function OpenFileList(const AConatainerPath: string): Boolean;
  begin
  OldConatainerPath := AConatainerPath;
  Assign(OldConatainerFile, OldConatainerPath);
  Reset(OldConatainerFile);
  if (IOResult = 0) and ReadNextS then
    begin
//      Descriptions := New(PDIZCol, Init($10, $10));
    Result := True;
    end
  else
    begin
    System.Close(OldConatainerFile);
    ClrIO;
    Result := False;
    end;
  end;

procedure ReadFileList(ProcessDizName: TDizNameProc;
    ProcessDizLine: TDizLineProc; ProcessDizEnd: TDizEndProc);
  var
    LS: Longint;
    N: String;
    I: LongInt;
    j: LongInt;
    NameEnd: LongInt;
  label
    ReadNextLine, EndDescr, EndFile;

  begin
  while True do
    begin
    { Обработка нового описания. LastDizLine уже прочитана.}
    if (LastDizLine = '') or (LastDizLine[1] in [' ', #9, '>']) then
      goto ReadNextLine;
    { игнорируем остаток
         предыдущего многострочного описания }

    LS := Length(LastDizLine);
    if LastDizLine[1] = '"' then
      {имя в кавычках - ищем вторую кавычку }
      begin
      NameEnd := 0;
      for j := 2 to LS do
        if LastDizLine[j] = '"' then
          begin
          NameEnd := j;
          Inc(j);
          Break;
          end;
      if NameEnd <= 2 then
        goto ReadNextLine;
      if NameEnd = LS then
        goto ReadNextLine; { пустое описание никого не интересует }
      N := Copy(LastDizLine, 2, NameEnd-2);
      end
    else
      {имя не в кавычках - ищем пробел или Tab. При этом
        благодаря DelRight после него что-то должно быть }
      begin
      NameEnd := Pos(' ', LastDizLine);
      j := Pos(#9, LastDizLine);
      if  (j <> 0) and (j < NameEnd) then
        NameEnd := j // заведомо не 0
      else
        begin
        if NameEnd = 0 then
          NameEnd := j; // может быть и 0
        if NameEnd = 0 then
          goto ReadNextLine; { пустое описание никого не интересует }
        j := NameEnd;
        end;
      N := Copy(LastDizLine, 1, NameEnd-1);
      end;
    UpStr(N);
    while (J <= Length(LastDizLine)) and (LastDizLine[J] = ' ') do
      inc(J);
    ProcessDizName(N, J); {LastDizLine доступна}

    {AK155: Дочитываем многострочное описание.
Признаком дополнительной строки является побел или Tab в начале,
а также '>' в начале (files.bbs в формате AllFix).}
    while True do
      begin
      if not ReadNextS then
        Break;
      if not (LastDizLine[1] in [' ', #9, '>']) then
        Break;
      ProcessDizLine;
      end;

    if ProcessDizEnd then
      goto EndFile;
    goto EndDescr;

ReadNextLine:
    if not ReadNextS then
      goto EndFile;
EndDescr:
    if LastDizLine = '' then
      goto EndFile;
    end;
EndFile:
  Close(OldConatainerFile);
  end { ReadFileList };

var
  GetDizFound: Boolean;
  PGetDizName1, PGetDizName2: PFlName;
  GetDizText: LongString;

function GetDizNameProc(const N: string; TextStart: Integer): Boolean;
  { Для ReadFileList. Сравнение имени и приём первой строки }
  var
    I: Integer;
    F: TUseLFN;
  begin
  for F := High(TUseLFN) downto Low(TUseLFN) do
    begin
    if (PGetDizName1^[F] = N) then
      begin
      GetDizFound := True;
      GetDizText := Copy(LastDizLine, TextStart, MaxLongStringLength);
      end;
    end;
  end;

procedure GetDizLineProc;
  { Для ReadFileList. Добавление очередной строки прямо в элемент
    коллекции}
  const
    CrLf: array[0..1] of char = #13#10;
  begin
  if GetDizFound then
    GetDizText := GetDizText + CrLf + LastDizLine;
  end;

function GetDizEndProc: Boolean;
  { Для ReadFileList. Формирование признака завершения }
  begin
  Result := GetDizFound;
  end;

procedure GetDiz(FR: PFileRec);
  var
    F: TUseLFN;
    Container: String;
    GetDizFull1: array[1..SizeOf(ShortString)+SizeOf(TShortName)] of Char;
    GetDizName1: TFlName absolute GetDizFull1;
     { разместить Dummy после TFlName, как в TFileRec, в данном случае
     нельзя, так как SmartLink попросту выбросит эту переменную, к которой
     нет обращений. А в таком варианте (с absolute) память резервируется.}
  begin
  if FR^.DIZ <> nil then
    Exit;
  Container := GetDizPath(FR^.Owner^, '');
  if Container = '' then
    exit;
  if not OpenFileList(Container) then
    Exit;
  for F := High(TUseLFN) downto Low(TUseLFN) do
    CopyShortString(UpStrg(FR^.FlName[F]), GetDizName1[F]);
  PGetDizName1 := @GetDizName1;
  ReadFileList(GetDizNameProc, GetDizLineProc, GetDizEndProc);
  GetDizFound := False;
  if GetDizText <> '' then
    begin
    New(FR^.DIZ);
    FR^.DIZ^.Container := NewStr(Container);
    FR^.DIZ^.DIZText := GetDizText;
    GetDizText := '';
    end;
  end { GetDiz };

{ Обновление описания файла в конейнере }

procedure SaveDizLineProc;
  { Для ReadFileList }
  begin
  if not IgnoreDiz then
    begin
    Writeln(NewContainerFile, LastDizLine);
    NewContainerNotEmpty := True;
    end;
  end;

function SaveDizNameProc(const N: string; TextStart: Integer): Boolean;
  { Для ReadFileList. Сравнение имени; для данного имени пропуск описания,
    для прочих - вывод первой строки }
  var
    I: Integer;
    F: TUseLFN;
  begin
  for F := Low(TUseLFN) to High(TUseLFN) do
    begin
    if (PGetDizName1^[F] = N) or
       ((PGetDizName2 <> nil) and (PGetDizName2^[F] = N))
    then
      begin
      if not GetDizFound then
        LName := F;
      GetDizFound := True;
      IgnoreDiz := True;
      end;
    end;
  SaveDizLineProc;
  end;

function SaveDizEndProc: Boolean;
  { Для ReadFileList. Продолжать копирование }
  begin
  Result := False;
  IgnoreDiz := False;
  end;

procedure ExportDiz(
  const OldName: PFlName;
  const NewLongName: string;
  var NewDiz: PDiz;
  TargetPath: String
  );
{ Если в прежнем контейнере описание нашлось, то в новом оно
будет под таким же (длинным или коротким) именем }
  var
    F: TUseLFN;
    ContainerFullName: String;
    OldContainerAttr: Word;
    GetDizFull1: array[1..SizeOf(ShortString)+SizeOf(TShortName)] of Char;
    GetDizName1: TFlName absolute GetDizFull1;

  begin
  if NewDiz = nil then
    Exit;
  PGetDizName1 := @GetDizName1;
  PGetDizName2 := OldName;
  if PGetDizName2 <> nil then
    for F := Low(TUseLFN) to High(TUseLFN) do
      UpStr(PGetDizName2^[F]);
  MakeSlash(TargetPath);
  ContainerFullName := '';
  if NewDiz^.Container <> nil then
    ContainerFullName := GetName(NewDiz^.Container^);
  { сейчас ContainerFullName - только имя (или пусто) }
  ContainerFullName := GetDizPath(TargetPath, ContainerFullName);
  { А теперь - полный путь }
  Assign(NewContainerFile, TargetPath+'$DN'+ItoS(DNNumber)+'$.DIZ');
  Rewrite(NewContainerFile);
  if IOResult <> 0 then
    begin
    Exit; //! как-то не очень... Ошибку бы сообщить. И далее аналогично.
    end;
  LName := True; // по умолчанию - по длинным именам
  CopyShortString(UpStrg(NewLongName), GetDizName1[True]);
{$IFDEF DualName}
  if (FMSetup.Options and fmoDescrByShortNames) <> 0 then
    begin
    LName := False;
    GetDizName1[False] :=
      GetName(lfGetShortFileName(TargetPath + NewLongName));
    end;
    UpStr(GetDizName1[False]);
{$ENDIF}
  NewContainerNotEmpty := False;
  if OpenFileList(ContainerFullName) then
    begin
    GetFAttr(OldConatainerFile, OldContainerAttr);
    ReadFileList(SaveDizNameProc, SaveDizLineProc, SaveDizEndProc);
    EraseFile(ContainerFullName);
    end
  else
    OldContainerAttr := Archive;
  if NewDiz^.DizText <> '' then
    begin
    { Длинное имя восстановить, какое есть,
     короткое имя - на нижний регистр }
    CopyShortString(NewLongName, GetDizName1[True]);
    {$IFDEF DualName}
    LowStr(GetDizName1[False]);
    {$ENDIF}
    Writeln(NewContainerFile, SquashesName(GetDizName1[LName]) + ' ',
      NewDiz^.DizText);
    NewContainerNotEmpty := True;
    end;
  Close(NewContainerFile);
  ClrIO;
  if NewContainerNotEmpty or (FMSetup.Options and fmoKillContainer = 0)
  then
    begin
    Rename(NewContainerFile, ContainerFullName);
    SetFAttr(NewContainerFile, OldContainerAttr);
    end
  else
    Erase(NewContainerFile);
  ClrIO;
  end { ExportDiz };

procedure DeleteDiz(FR: PFileRec);
  begin
  if (FR <> nil) {Бывает для подкаталога при F6 на каталоге}
    and (FR^.DIZ <> nil)
    and (FR^.DIZ^.DizText <> '')
  then
    begin
    FR^.DIZ^.DizText := '';
    ExportDiz(@FR^.FlName[True], '', FR^.DIZ, FR^.Owner^);
    end;
  end;

function CalcDPath(P: PDiz; Owen: PString): String;
  var
    I: Integer;
    DPath: String;
    SR: lSearchRec;
  begin
  if  (P = nil) or (P^.Container = nil) then
    {! Интересно, а может ли быть P^.Container=nil?
     Такое бывает в arvidavt и arvidtdr, но они, мне кажется,
     не могут обратиться к CalcDPath, так что это условие лишнее }
    begin
    for I := 1 to 128 do
      begin
      DPath := GetPossibleDizOwner(I);
      if DPath = '' then
        Exit;
      DPath := MakeNormName(CnvString(Owen), DPath);
      if ExistFile(DPath) then
        Break;
      end;
    end
  else
    DPath := P^.Container^;
  CalcDPath := DPath;
  {lFindClose(SR);}
  end { CalcDPath };
{-DataCompBoy-}

function DizFirstLine(DIZ: PDiz): String;
  var
    l: Integer;
  begin
  Result := '';
  if Diz = nil then
    exit;
  Result := DIZ^.DIZText;
  l := PosChar(#13, Result);
  if l <> 0 then
    SetLength(Result, l-1);
  end;

function DizMaxLine(DIZ: PDiz): String;
  var
    lWrite, lRead: Integer;
    D: AnsiString;
  begin
  Result := '';
  if Diz = nil then
    exit;
  D := DIZ^.DIZText;
  lWrite := 0; lRead := 1;
  while lRead <= Length(D) do
    begin
    inc(lWrite);
    if D[lRead] in [#13, #10] then
      begin
      Result[lWrite] := ' ';
      repeat
        Inc(lRead)
      until (lRead > Length(D)) or not (D[lRead] in [#13, #10, ' ', #9]);
      end
    else
      begin
      Result[lWrite] := D[lRead];
      inc(lRead);
      end;
    if lWrite = 254 then
      Break;
    end;
  SetLength(Result, lWrite);
  end;

end.
