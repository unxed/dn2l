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

unit FilesCol;

interface

uses
  Files, Defines, Streams,
  Collect, Drivers, Hash
  ;

type
  PDiz = ^TDIZ;
  TDIZ = record
    {` Описание файла для использования в TFileRec }
    Container: PString;
      {` Полный путь файла описаний. Освобождать его не надо.
       В Арвиде не используется. `}
    DIZText: LongString;
      {` Описание без имени файла. Строки разделяются CrLf`}
    Line: LongInt;
      {` У ритлабов это был номер строки в файле описаний, где
       начинается это описание, но это нигде не использовалось.
       Используется это поле только в Арвиде, но совершенно
       в другом смысле.`}
    end;
    {`}
  TShortName = String[12];
  PFlName = ^TFlName;
  TFlName = array[TUseLFN] of TShortName;
  {Использовано тут и в TDirRec}
  TDate4 = record
    Minute, Hour, Day, Month: Byte
    end;

  PPFileRec = ^PFileRec;
  PFileRec = ^TFileRec;
  TFileRec = record
    Size: TSize;
      { Если размер неизвестнен, Size=-1 (типично для каталогов) }
    PSize: TSize;
    Owner: PString;
    DIZ: PDiz;
    Yr: Word;
    YrCreat: Word;
    YrLAcc: Word;
    TType: Byte;
    Attr: Word;
    Second: Byte;
    SecondCreat: Byte;
    SecondLAcc: Byte;
    Selected: Boolean;
    UsageCount: Byte; {DataCompBoy}
    FDate, FDateCreat, FDateLAcc: LongInt; {фактически - TDate4}
    FlName: TFlName;
    {см. files.pas }
    Dummy: array[1..SizeOf(ShortString)-SizeOf(TShortName)] of Char;
    {а это место, куда будет свешиваться хвост длинного имени в тех
      случаях, когда заводится локальная переменна типа TFileRec или
      при временном динамическом резервировании в стиле new(PFilerec).
      Нормально динамическое резервирование должно делаться через
      CreateFileRec или NewFileRec, где памяти резервируется ровно
      столько, сколько нужно. Из-за этого фокуса поле FlName
      обязательно должно быть в самом конце этой структуры.
      Копировать длинное имя надо не при помощи ':=', а при помощи
      CopyShortString. AK155 }
      {<filescol.001>}
    end;
var
  pr: TFileRec;
const
  TFileRecFixedSize = SizeOf(TFileRec)
  -SizeOf(pr.Dummy)-SizeOf(pr.FlName[True])+1;

type
  TMakeListRec = record
    FileName: String; {DataCompBoy}
    Header: String;
    HeaderMode: Word;
    Action: String;
    Footer: String;
    FooterMode: Word;
    Options: Word;
    end;

  {-DataCompBoy-}
  PUserParams = ^TUserParams;
  tUserParams = record
    Active, Passive: PFileRec;
    ActiveList, PassiveList: String;
    end;
  {-DataCompBoy-}

  {Cat: выкинул, теперь используется Collect.TLineCollection}
  (*
    PLineCollection = PTextCollection;
    TLineCollection = TTextCollection;
*)

  PFilesCollection = ^TFilesCollection;
  TFilesCollection = object(TSortedCollection)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    SortMode: Byte;
    Selected: LongInt;
    Panel: Pointer; {PFilePanel}
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    function FileCompare(Key1, Key2: Pointer): Integer;
    procedure DelDuplicates(var TotalInfo: TSize);
      {` Устранение записей, ссылающихся на один и тот же файл.
        Если коллекция сортированная, то она должна быть отсортирована
        до вызова данной программы `}
    end;

type
  PFilesHash = ^TFilesHash;
  {` Хэшировщик, применяемый для быстрого поиска по имени/пути
    в несортированных коллекциях `}
  TFilesHash = object(THash)
    procedure Hash(Item: Pointer); virtual;
    function Equal(Item1, Item2: Pointer): Boolean; virtual;
    end;

const
  cmlPathNames = 1;
  cmlAutoDetermine = 2;

  hfmAuto = 0;
  hfmInsertText = 1;
  hfmInsertFiles = 2;

function SelectDrive(X, Y: Integer; Default: Char; IncludeTemp: Boolean)
  : String;
function CopyFileRec(FR: PFileRec): PFileRec; {DataCompBoy}
function CreateFileRec(Name: String): PFileRec;
  {` Name - это имя с полным путём. На основании пути создаётся
   newstr и записывается в Owner. Вызывающая программа сама должна
   освободить owner перед уничтожением этой файловой записи. `}
{$IFDEF DualName}
function NewFileRec(const LFN, Name: String; Size: TSize;
     Date, CreationDate, LastAccDate: LongInt; Attr: Word;
     AOwner: PString): PFileRec; {DataCompBoy}
{$ELSE}
function NewFileRec(const Name: String; Size: TSize;
     Date, CreationDate, LastAccDate: LongInt; Attr: Word;
     AOwner: PString): PFileRec; {DataCompBoy}
{$ENDIF}
procedure DelFileRec(var FR: PFileRec); {DataCompBoy}
function LoadFileRec(var s: TStream): PFileRec; {DataCompBoy}
procedure StoreFileRec(var s: TStream; fr: PFileRec); {DataCompBoy}
function LoadFileRecOwn(var s: TStream; Dirs: PCollection): PFileRec;
  {` Прочитать запись, затем индекс в Dirs и заполнить Owner`}
procedure StoreFileRecOwn(var s: TStream; fr: PFileRec; Dirs: PCollection);
  {` Записать запись и затем индекс owner в Dirs`}
function PackedDate(P: PFileRec): LongInt; {DataCompBoy}
function PackedCreationDate(P: PFileRec): LongInt; {JO}
function PackedLastAccDate(P: PFileRec): LongInt; {JO}
//function GetFileType(const S: String; Attr: Byte): Integer;
{procedure ReplaceLongName(var fr: PFileRec; const NewName: string);}

function SameFile(P1, P2: PFileRec): Boolean;
  {` Относятся ли файловые записи к одному и тому же файлу,
  то есть если совпадают ли (длинное) имя и путь. `}

implementation
uses
  Lfn, DNApp, Menus, Views, FlPanelX, FlPanel, Drives,
  Objects2, Commands, Messages,
  {!!}CmdLine
  {$IFDEF MODEM} {$IFDEF LINK}, NavyLink {$ENDIF} {$ENDIF}
  {$IFDEF NETINFO}, Novell {$ENDIF}
  , VpSysLow, VPUtils
  {$IFDEF PLUGIN}, Plugin {$ENDIF}
  , FlTl, DnIni, Dos, FileType, PDSetup, UKeyMap
  , DNHelp, Advance, Advance1, Advance2, Memory, Startup
  ;

const
  pfrPacked = $80;
  pfrSelect = $40;

type
  TPackedFileRec = record
    Time: LongInt;
    Attr: Byte;
    NLen: Byte;
    end;

  {-DataCompBoy-}
function CreateFileRec(Name: String): PFileRec;
  var
    fr: PFileRec;
    lsr: lSearchRec;
    l: LongInt;
    D: DateTime;
    path: String;
    FName: String;
    iLFN: TUseLFN;
  begin
  Name := lFExpand(Name);
  lFindFirst(Name, AnyFileDir, lsr);
  lFindClose(lsr);
  path := GetPath(Name);
  Name := GetName(Name);
  {  l := TFileRecFixedSize+length(lsr.FullName);}
  l := SizeOf(TFileRec);
  GetMem(fr, l);
  CreateFileRec := fr;
  FillChar(fr^, l, 0);

  if DosError = 0 then
    begin
    {$IFDEF DualName}
    fr^.FlName[False] := lsr.SR.Name;
    {$ENDIF}
    CopyShortString(lsr.FullName, fr^.FlName[True]);
    fr^.Size := lsr.FullSize;
    fr^.PSize := lsr.FullSize;
    fr^.Owner := NewStr(path);
    fr^.DIZ := nil;

    UnpackTime(lsr.SR.Time, D);
    fr^.Yr := D.Year;
    with TDate4(fr^.FDate) do
      begin
      Month := D.Month;
      Day := D.Day;
      Hour := D.Hour;
      Minute := D.Min;
      end;
    fr^.Second := D.Sec;

    UnpackTime(lsr.SR.CreationTime, D); {JO}
    fr^.YrCreat := D.Year;
    with TDate4(fr^.FDateCreat) do
      begin
      Month := D.Month;
      Day := D.Day;
      Hour := D.Hour;
      Minute := D.Min;
      end;
    fr^.SecondCreat := D.Sec;

    UnpackTime(lsr.SR.LastAccessTime, D);
    fr^.YrLAcc := D.Year;
    with TDate4(fr^.FDateLAcc) do
      begin
      Month := D.Month;
      Day := D.Day;
      Hour := D.Hour;
      Minute := D.Min;
      end;
    fr^.SecondLAcc := D.Sec; {/JO}

    fr^.Attr := lsr.SR.Attr;
    if CharCount('.', lsr.FullName) = 0 then
      begin
      SetLength(lsr.FullName, Length(lsr.FullName)+1);
      lsr.FullName[Length(lsr.FullName)] := '.';
      fr^.TType := GetFileType(lsr.FullName, fr^.Attr);
      SetLength(lsr.FullName, Length(lsr.FullName)-1);
      end
    else
      fr^.TType := GetFileType(lsr.FullName, fr^.Attr);
    {Cat: это явно лишнее - для масок !\!.! для просмотрщиков и запуска по
      расширению должны передаваться неискажённые имена файлов
  PS  и следует ещё раз посмотреть и проверить - нужна ли вообще эта функция
     if fr^.Attr and Directory <> 0 then UpStr(fr^.Name) else LowStr(fr^.Name);}
    {/Cat}
    if fr^.Attr and Directory <> 0 then
      fr^.Size := -1;
    end
  else
    begin
    fr^.Owner := NewStr(path);
    {$IFDEF DualName}
    fr^.FlName[False] := GetURZ(GetName(lfGetShortFileName(Name)));
    {$ENDIF}
    fr^.FlName[True] := Name;
    end;
  with fr^ do
    begin
    UsageCount := 1;
    end;
  end { CreateFileRec };
{-DataCompBoy-}

{-DataCompBoy-}
function CopyFileRec;
  begin
  CopyFileRec := FR;
  with FR^ do
    Inc(UsageCount);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure DelFileRec(var FR: PFileRec);
  begin
  if FR <> nil then
    begin
    Dec(FR^.UsageCount);
    if FR^.UsageCount = 0 then
      begin
      if FR^.DIZ <> nil then
        begin
        FR^.DIZ^.DIZText := ''; // освободить строку
        Dispose(FR^.DIZ);
        end;
      {  FreeMem(FR, TFileRecFixedSize+length(FR^.FlName[true]));}
      FreeMem(FR, SizeOf(TFileRec));
      FR := nil;
      end
    end;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function LoadFileRec(var s: TStream): PFileRec;
  var
    P: PFileRec;
    l: Byte;
    FullLen: LongInt;
  begin
  s.Read(l, SizeOf(l)); {длина длинного имени}
  { FullLen := TFileRecFixedSize+l;}
  FullLen := SizeOf(TFileRec);
  GetMem(P, FullLen);
  FillChar(P^, FullLen, 0);
  with P^ do
    begin
    s.Read(Size, SizeOf(Size));
    s.Read(PSize, SizeOf(PSize));
    s.Read(Yr, SizeOf(Yr));
    s.Read(YrCreat, SizeOf(YrCreat));
    s.Read(YrLAcc, SizeOf(YrLAcc));
    s.Read(TType, SizeOf(TType));
    s.Read(Attr, SizeOf(Attr));
    s.Read(Second, SizeOf(Second));
    s.Read(SecondCreat, SizeOf(SecondCreat));
    s.Read(SecondLAcc, SizeOf(SecondLAcc));
    s.Read(Selected, SizeOf(Selected));
    s.Read(FDate, SizeOf(FDate));
    s.Read(FDateCreat, SizeOf(FDateCreat));
    s.Read(FDateLAcc, SizeOf(FDateLAcc));
    {$IFDEF DualName}
    s.Read(FlName[False], SizeOf(FlName[False]));
    {$ENDIF}
    s.Read(FlName[True], l+1);
    UsageCount := 1;
    end;
  LoadFileRec := P;
  end { LoadFileRec };

procedure StoreFileRec(var s: TStream; fr: PFileRec);
  var
    l: Byte;
  begin
  with fr^ do
    begin
    l := Length(FlName[True]);
    s.Write(l, SizeOf(l));
    s.Write(Size, SizeOf(Size));
    s.Write(PSize, SizeOf(PSize));
    s.Write(Yr, SizeOf(Yr));
    s.Write(YrCreat, SizeOf(YrCreat));
    s.Write(YrLAcc, SizeOf(YrLAcc));
    s.Write(TType, SizeOf(TType));
    s.Write(Attr, SizeOf(Attr));
    s.Write(Second, SizeOf(Second));
    s.Write(SecondCreat, SizeOf(SecondCreat));
    s.Write(SecondLAcc, SizeOf(SecondLAcc));
    s.Write(Selected, SizeOf(Selected));
    s.Write(FDate, SizeOf(FDate));
    s.Write(FDateCreat, SizeOf(FDateCreat));
    s.Write(FDateLAcc, SizeOf(FDateLAcc));
    {$IFDEF DualName}
    s.Write(FlName[False], SizeOf(FlName[False]));
    {$ENDIF}
    s.Write(FlName[True], l+1);
    end
  end { StoreFileRec };
{-DataCompBoy-}

function LoadFileRecOwn(var s: TStream; Dirs: PCollection): PFileRec;
  var
    w: LongInt;
  begin
  Result := LoadFileRec(s);
  if Result <> nil then
    begin
    s.Read(w, SizeOf(w));
    Result^.Owner := Dirs^.At(w);
    end;
  end;

procedure StoreFileRecOwn(var s: TStream; fr: PFileRec; Dirs: PCollection)
  ; {DataCompBoy}
  var
    w: LongInt;
  begin
  StoreFileRec(s, fr);
  w := Dirs^.IndexOf(fr^.Owner);
  s.Write(w, SizeOf(w));
  end;

{-DataCompBoy-}
constructor TFilesCollection.Load;
  var
    C, I: LongInt;
  begin
  TObject.Init;
  S.Read(Count, SizeOf(Count));
  S.Read(Limit, SizeOf(Limit));
  S.Read(Delta, SizeOf(Delta));
  if  (Count > Limit) or (Delta < 0) then
    Fail;
  C := Count;
  I := Limit;
  Count := 0;
  Limit := 0;
  SetLimit(I);
  for I := 0 to C-1 do
    begin
    AtInsert(I, LoadFileRec(S));
    if  (S.Status <> stOK) then
      begin
      SetLimit(0);
      Fail;
      end;
    end;
  S.Read(Selected, SizeOf(Selected));
  end { TFilesCollection.Load };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFilesCollection.Store;
  var
    I, J, Sel: LongInt;
  begin
  J := 0;
  for I := 1 to Count do
    if  (I-1 = Selected) or (PFileRec(At(I-1))^.Selected) then
      begin
      if I-1 = Selected then
        Sel := J;
      Inc(J)
      end;
  I := Count;
  Count := J;
  S.Write(Count, SizeOf(Count));
  S.Write(Limit, SizeOf(Limit));
  S.Write(Delta, SizeOf(Delta));
  Count := I;
  for I := 1 to Count do
    if  (I-1 = Selected) or (PFileRec(At(I-1))^.Selected) then
      StoreFileRec(S, At(I-1));
  S.Write(Sel, SizeOf(Sel));
  end { TFilesCollection.Store };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFilesCollection.FreeItem;
  var
    P: PFileRec absolute Item;
  begin
  DelFileRec(P);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
//JO: 02-02-2004 - добавил сортировку по пути в панелях отображающих ветвь
//                 и обратную сортировку
function TFilesCollection.Compare;
  var
    {T1: TFileRec;}
    P1: PFileRec absolute Key1;
    P2: PFileRec absolute Key2;
    C: Integer;
    SM, I1, I2, UpN: Integer;
    P1P, P2P: Boolean;
    SortFlags: Byte;
    CmpMethod: Word;
    ST1, ST2: String;
    NameDirsSortEnabled: Boolean;

  const
    Ups: array[1..4] of Word = (0, 0, 0, 0);

  label Lab1;

  function CmpName(const S1: String; const S2: String;
      const owner1: String; const owner2: String): Integer;
    begin
    if S1 > S2 then
      CmpName := +1
    else if S1 < S2 then
      CmpName := -1
    else if owner1 > owner2 then
      CmpName := +1
    else if owner1 < owner2 then
      CmpName := -1
    else
      CmpName := 0;
    end;

  function CmpEXT(S1, S2: String): Integer;
    var
      E1, E2: PString;
      B: Byte;
    const
      D1: Char = #0;
    begin
    for B := Length(S1) downto 1 do
      if S1[B] = '.' then
        Break;
    if  (B > 1) or (S1[1] = '.') then
      begin
      E1 := @S1[B];
      SetLength(E1^, Length(S1)-B);
      SetLength(S1, B-1);
      end
    else
      E1 := @D1;

    for B := Length(S2) downto 1 do
      if S2[B] = '.' then
        Break;
    if  (B > 1) or (S2[1] = '.') then
      begin
      E2 := @S2[B];
      SetLength(E2^, Length(S2)-B);
      SetLength(S2, B-1);
      end
    else
      E2 := @D1;

    if E1^ > E2^ then
      CmpEXT := +1
    else if E1^ < E2^ then
      CmpEXT := -1
    else if S1 > S2 then
      CmpEXT := +1
    else if S1 < S2 then
      CmpEXT := -1
    else
      CmpEXT := 0;
    end { CmpEXT };

  function CmpTime(Yr1, Yr2: Word; FDate1, FDate2: LongInt;
           Second1, Second2: Byte): Integer;
    begin
    if  (Yr1 < Yr2) then
      CmpTime := +1
    else if (Yr1 > Yr2) then
      CmpTime := -1
    else if (FDate1 < FDate2) then
      CmpTime := +1
    else if (FDate1 > FDate2) then
      CmpTime := -1
    else if (Second1 < Second2) then
      CmpTime := +1
    else if (Second1 > Second2) then
      CmpTime := -1
    else
      CmpTime := 0;
    end;

  var
    Name1, Name2: String; {UpStrg(P2^.Name)  // AK155}
    Own1, Own2: String;
    Branched: Boolean; {идёт ли сравнение в панели представляющей ветвь}
  const
    CompareXlat: array[0..2] of PXLat =
      (@ABCSortXlat, @UpCaseArray, @LowCaseArray);

  begin { TFilesCollection.Compare }
  NameDirsSortEnabled := False;
  if Panel <> nil then
    begin
    SortFlags := PFilePanel(Panel)^.PanSetup^.Sort.SortFlags;
    Move(PFilePanel(Panel)^.PanSetup^.Sort.Ups, Ups, SizeOf(Ups));
    CmpMethod := PFilePanel(Panel)^.PanSetup^.Sort.CompareMethod;
    if PFilePanel(Panel)^.Drive <> nil then
      {JO: все типы панели, представляющие собой раскрытую ветвь}
      Branched := PFilePanel(Panel)^.Drive^.DriveType
        in [dtFind, dtTemp, dtList, dtArcFind]
    else
      Branched := False;
    end
  else
    begin
      {! Раньше флаги брались из установок для новой панели,
       интересно, зачем? А ещё интереснее, бывает ли и нужна
       ли сортировка без панели?}
    SortFlags := 0;
    CmpMethod := 1;
    Branched := False;
    end;
  {Move(Key1^,T1,SizeOf(T1));}
  with P2^ do
    begin
    P1P := P1^.TType = ttUpDir;
    P2P := TType = ttUpDir;
    if P1P and not P2P then
      begin
      Compare := -1;
      Exit;
      end;
    if P2P and not P1P then
      begin
      Compare := +1;
      Exit;
      end;

    Name1 := P1^.FlName[uLfn];
    Name2 := FlName[uLfn];

    XLatStr(Name1, CompareXlat[CmpMethod]^);
    XLatStr(Name2, CompareXlat[CmpMethod]^);

    if P1^.Owner <> nil then
      Own1 := P1^.Owner^
    else
      Own1 := '';
    if Owner <> nil then
      Own2 := Owner^
    else
      Own2 := '';
    //JO: Нижележащее условие с Branched  сделано в интересах производительности,
    //    так как менять регистр объемлющего каталога для чего-либо кроме
    //    раскрытой ветви - пустая трата процессорного времени
    if Branched then
      begin
      MakeNoSlash(Own1);
      MakeNoSlash(Own2);
      XLatStr(Own1, CompareXlat[CmpMethod]^);
      XLatStr(Own2, CompareXlat[CmpMethod]^);
      end;

    SM := SortMode;

    if  (Panel <> nil) then
      begin
      for UpN := 1 to 4 do
        case Ups[UpN] of
     upsDirs:
          begin
//JO: если каталоги пеpемещаются в начало панели, то их соpтиpовка
//    по имени пpи соpтиpовке файлов по другим критериям осмысленна
          NameDirsSortEnabled := True;
          if  (P1^.TType = ttDirectory) and (TType <> ttDirectory) then
            begin
            Compare := -1;
            Exit;
            end;
          if  (P1^.TType <> ttDirectory) and (TType = ttDirectory) then
            begin
            Compare := 1;
            Exit;
            end;
          end;
 upsArchives:
          if not (SM in [psmSize, psmTime, psmCrTime,
                         psmLATime]) then
            begin
            if  (P1^.TType = ttArc) and (TType <> ttArc) then
              begin
              Compare := -1;
              Exit;
              end;
            if  (P1^.TType <> ttArc) and (TType = ttArc) then
              begin
              Compare := 1;
              Exit;
              end;
            end;
 upsExecutables:
          if not (SM in [psmSize, psmTime, psmCrTime,
                         psmLATime]) then
            begin
            if  (P1^.TType = ttExec) and (TType <> ttExec) then
              begin
              Compare := -1;
              Exit;
              end;
            if  (P1^.TType <> ttExec) and (TType = ttExec) then
              begin
              Compare := 1;
              Exit;
              end;
            end;
 upsHidSysFiles:
          if not (SM in [psmSize, psmTime, psmCrTime,
                         psmLATime]) then
            begin
            if  ((P1^.Attr and (Hidden+SysFile)) <> 0) and
                ((Attr and (Hidden+SysFile)) = 0) then
              begin
              Compare := -1;
              Exit;
              end;
            if  ((P1^.Attr and (Hidden+SysFile)) = 0) and
                ((Attr and (Hidden+SysFile)) <> 0) then
              begin
              Compare := 1;
              Exit;
              end;
            end;
        end; {case}

      if SortFlags and psfSortByType <> 0 then
        begin
//JO: поскольку при сортировке по типу все каталоги оказываются заведомо
//    собраны вместе, то их соpтиpовка по имени пpи соpтиpовке файлов по
//    другим критериям является осмысленной
        NameDirsSortEnabled := True;
        I1 := P1^.TType;
        I2 := TType;
        if I1 = 0 then
          I1 := 100; // файлы без типа - в конец
        if I2 = 0 then
          I2 := 100; // файлы без типа - в конец
        if I1 < I2 then
          begin
          Compare := -1;
          Exit;
          end
        else if I1 > I2 then
          begin
          Compare := 1;
          Exit;
          end;
        end;
      end; {Panel <> nil}

    if SortFlags and psfOwnerFirst <> 0 then
      begin
      if Own1 < Own2 then
        C := -1
      else if Own1 > Own2 then
        C := 1
      else
        goto Lab1;
      end
    else
      begin
Lab1:

      if (SortFlags and psfDirsByName <> 0) and
//JO: если каталоги не пеpемещаются в начало панели, то их
//    соpтиpовка по имени пpи соpтиpовке файлов по pасшиpению
//    не только бессмысленна, но и вpедна
         NameDirsSortEnabled and
           ((P1^.Attr or Attr) and Directory <> 0) then
               C := CmpName(Name1, Name2, Own1, Own2)
      else
      case SM of
//JO: реально при psmUnsorted метод Compare никогда не вызывается, но
//    на всякий случай лучше, чтобы сравнение производилось, причём
//    будет честнее, если это будет нормальное сравнение по именам и
//    путям, а не нечто, создающее псевдонесортированный беспорядок
             psmUnsorted,
        {LFN}psmLongName:
          C := CmpName(Name1, Name2, Own1, Own2);
        {LEXT}psmLongExt:
          begin
          C := CmpEXT(Name1, Name2);
          if C = 0 then
            C := CmpName(Name1, Name2, Own1, Own2);
          end;
        {SIZE}psmSize:
          if P1^.Size > Size then
            C := -1
          else if P1^.Size < Size then
            C := 1
          else
            C := CmpName(Name1, Name2, Own1, Own2);
        {DATE}psmTime:
          begin
          C := CmpTime(P1^.Yr, Yr, P1^.FDate,
                       FDate, P1^.Second, Second);
          if C = 0 then
            C := CmpName(Name1, Name2, Own1, Own2);
          end;
        {JO}
        {Creat.DATE}psmCrTime:
          begin
          C := CmpTime(P1^.YrCreat, YrCreat, P1^.FDateCreat,
                       FDateCreat, P1^.SecondCreat, SecondCreat);
          if C = 0 then
            C := CmpName(Name1, Name2, Own1, Own2);
          end;
        {L.Acc.DATE}psmLATime:
          begin
          C := CmpTime(P1^.YrLAcc, YrLAcc, P1^.FDateLAcc,
                       FDateLAcc, P1^.SecondLAcc, SecondLAcc);
          if C = 0 then
            C := CmpName(Name1, Name2, Own1, Own2);
          end;

        {DIZ}psmDIZ:
          begin
          if  (P1^.DIZ <> nil) then
            begin
            ST1 := P1^.DIZ^.DIZText;
            XLatStr(ST1, CompareXlat[CmpMethod]^);
            end
          else
            ST1 := '';
          if  (P2^.DIZ <> nil) then
            begin
            ST2 := P2^.DIZ^.DIZText;
            XLatStr(ST2, CompareXlat[CmpMethod]^);
            end
          else
            ST2 := '';
          if  (ST1 <> '') and (ST2 = '') then
            C := -1
          else if (ST1 = '') and (ST2 <> '') then
            C := 1
          else
            begin
            if ST1 > ST2 then
              C := 1
            else if ST1 < ST2 then
              C := -1
            else
              C := CmpName(Name1, Name2, Own1, Own2);
            end;
          end;
        {/JO}
      else {case}
//JO: если значение SortMode какое-нибудь нестандартное, то pеально
//    ничего не сpавниваем, пpосто инициализиpуем значение функции
          C := -1;
      end; {case}
      end;
    end; {with P2^}
  if SortFlags and psfInverted = 0 then
    Compare := C
  else
    Compare := -(C);
  end { TFilesCollection.Compare };

{-DataCompBoy-}

{JO}
//JO: 29-01-2004 - выделил ту часть TFilesCollection.Compare , которая
//    использовалась при сравнении каталогов в отдельный метод
//    TFilesCollection.FileCompare . Это оправдано, поскольку во-первых
//    TFilesCollection.Compare разрослась до безобразия, а для скорости
//    сортировки важна быстрота её вызова, а во-вторых поскольку поле
//    TFilesCollection.Sortmode используется этими двумя функциями совершенно
//    разным образом. Метод FileCompare используется только для сравнения
//    отдельно взятых файловых записей, и нигде не наследуется.
//JO: 27-04-2006 - перенёс в TFilesCollection.FileCompare из
//    TFilesCollection.Compare сравнение файлов по всем критериям сразу,
//    используемое для групповых операций с файлами в панели (в данный
//    момент используется только для разотметки файлов по cmCopyUnselect)
function TFilesCollection.FileCompare;
  var
    P1: PFileRec absolute Key1;
    P2: PFileRec absolute Key2;
    C: Integer;
    SM: Integer;
    P1P, P2P: Boolean;
    C1: Integer; { результат сравнения имён   // AK155}
    Name1, Name2: String; {UpStrg(P2^.Name)  // AK155}

  begin
  with P2^ do
    begin
    P1P := P1^.TType = ttUpDir;
    P2P := TType = ttUpDir;
    if P1P and not P2P then
      begin
      FileCompare := -1;
      Exit;
      end;
    if P2P and not P1P then
      begin
      FileCompare := +1;
      Exit;
      end;

    Name1 := P1^.FlName[uLfn];
    Name2 := FlName[uLfn];

    UpStr(Name1);
    UpStr(Name2);

    SM := SortMode;

    if  (SM and fcmCaseSensitive = 0)
      {$IFDEF Win32}
      {JO: регистрочувствительное сравнение по коротким именам лишено смысла}
      or (not uLfn)
      {$ENDIF}
      then
      begin
      if Name1 > Name2 then
        C1 := 1
      else if Name1 < Name2 then
        C1 := -1
      else
        C1 := 0;
      end
    else
      begin
      if P1^.FlName[True] < FlName[True] then
        C1 := -1
      else if P1^.FlName[True] = FlName[True] then
        C1 := 0
      else
        C1 := 1;
      end;

    if SM = fcmPreciseCompare then
//  сравнение используется в TFilePanel.HandleEvent для CM_CopyUnselect,
//  т.е. для разотметки файлов при групповых операциях
      begin
//JO: по поводу маски $3F см. комментарий AK155 от 28-11-05 к константам
//    Marked и Copied модуля FileCopy
      if (P1^.Attr and $3F = Attr and $3F)
            and (P1^.Yr = Yr)
            and (P1^.FDate = FDate) and (P1^.Second = Second)
            and (C1 = 0) and (P1^.Owner^ = Owner^)
            then
            FileCompare := 0
          else
            FileCompare := -1;
      Exit;
      end;

    if  (C1 = 0) and ((P1^.Attr or Attr) and Directory = 0) and
        ( (SM and fcmCompSize = 0) or (P1^.Size = Size)) and
        ( (SM and fcmCompTime = 0) or (P1^.Yr < Yr) or
          ( (P1^.Yr = Yr) and (P1^.FDate <= FDate)) or
          ( (P1^.Yr = Yr) and (P1^.FDate = FDate) and (P1^.Second <
             Second))) and
        ( (SM and fcmCompAttr = 0) or (P1^.Attr = Attr)) and
        ( (SM and fcmCompContent = 0) or
        CompareFiles(MakeNormName(P1^.Owner^, Name1),
          MakeNormName(Owner^, Name2)))
    then
      C := 0
    else if (C1 = -1) then
      C := -1
    else
      C := 1;
    FileCompare := C;
    end;
  end { TFilesCollection.FileCompare };
{/JO}

// JO: 18-11-2004 - ввёл полностью новые настройки показа информации
//     в меню выбора диска
function SelectDrive; {-$VIV, JO}
  var
    R: TRect;
    P: PMenuBox;
    Menu: PMenu;
    Items, Lnk: PMenuItem;
    C: Char;
    N, MaxRY: Integer;
    SC: TCharSet;
    Server_Num, Handle_Num, RetCode, DriveNum, MaxL: Integer;
    {-$VIV start}
    Server, PathName, TmpS: String; {-$VIV end}
    pSaveNeedAbort, ShowDir: Boolean;
    FreeSp: TQuad;
    Tabulated: Boolean;

  label
    InvSpace;

  function CutLongString(S: String): String;
    var
      P, L: Integer;
      S1: String;
    begin
    CutLongString := #0+S+#0;
    if not CutDriveInfo then
      Exit;
    (* X-Man *)
    CutLongString := FormatLongName(S, 30, 0,
        flnHighlight+flnHandleTildes, nfmNull)
    end;

  var
    IDDQD: record
      Fl, dr: LongInt;
      end;

  procedure GetInfo(P: PFileRec);
    begin
    if P <> nil then
      if P^.Attr and Directory = 0
      then
        Inc(IDDQD.Fl)
      else
        Inc(IDDQD.dr)
    end;

  type
    TDriveRec = record
      Dr: Char;
      DT: TDrvTypeNew;
      FullS: String[50];
      end;

 const
    MaxSizeDig = 17;

  var
    DrvCnt, I, J, K: Byte;
    DrvStrArr: array [1..26] of TDriveRec;
    MaxFullSLength: Byte;
    SizeStr: String[MaxSizeDig];

  begin { SelectDrive }
  Items := nil;
  N := 0;
  Lnk := nil;
  MaxL := 8; {-$VIV}
  DrvCnt := 0; {JO}
  MaxFullSLength := 0;
  Tabulated := (InterfaceData.DrvInfType.Tabulated and 1 <> 0);

  {$IFDEF NetBrowser}
  if IncludeTemp then
    begin
    Items := NewItem('~@:~ Network:', '', kb2, 1400,
              hcNoContext, Items);
    Inc(N);
    end;
  {$ENDIF}

  {Cat}
  {$IFDEF PLUGIN}
  if IncludeTemp then
    Inc(N, CreateDriveMenus(Items, MaxL));
  {$ENDIF}
  {/Cat}

  if IncludeTemp
    and (InterfaceData.DrvInfType.AddInfo and ditAddQick <> 0)
  then
    begin
    C := ' ';
    for DriveNum := 8 downto 0 do
      if DirsToChange[DriveNum] <> nil then
        begin
        FreeStr := '~'+ItoS(DriveNum+1)
            +':~ '+CutH(DirsToChange[DriveNum]^, 24);
        Items := NewItem(FreeStr, 'Alt-'+ItoS(DriveNum+1), kbNoKey,
            cmQuickChange1+DriveNum, hcNoContext, Items);
        MaxL := Max(CStrLen(FreeStr), MaxL);
        Inc(N);
        C := '!';
        end;
    if C = '!' then
      Items := NewLine(Items);
    end;

  {$IFDEF MODEM}
  {$IFDEF LINK}
  if IncludeTemp and (Linker <> nil) then
    begin
    CL_GetLinkDrives(SC);
    if SC <> [] then
      begin
      for C := 'Z' downto 'A' do
        if C in SC then
          Items := NewItem('~'+C+':~ ', '', kbNoKey, 2000+Byte(C),
              hcNoContext, Items);
      Items := NewSubMenu('~+:~ LINK', hcNoContext, NewMenu(Items), nil);
      Lnk := Items;
      Inc(N)
      end;
    end;
  {$ENDIF}
  {$ENDIF}

  if IncludeTemp then
    begin
    if  (InterfaceData.DrvInfType.AddInfo and ditAddTemp <> 0) then
      if TempFiles <> nil then
        begin
        IDDQD.dr := 0;
        IDDQD.Fl := 0;
        TempFiles^.ForEach(@GetInfo);
        if IDDQD.dr+IDDQD.Fl = 0 then
          FreeStr := GetString(dlEmpty)
        else if IDDQD.dr = 0 then
          FreeStr := ItoS(IDDQD.Fl)+' '+GetString(dlDIFiles)
        else if IDDQD.Fl = 0 then
          FreeStr := ItoS(IDDQD.dr)+' '+GetString(dlDirectories)
        else
          FormatStr(FreeStr, GetString(dlFilDir), IDDQD);
        end
      else
        FreeStr := GetString(dlEmpty)
    else
      FreeStr := '';

    Items := NewItem('~*:~ TEMP:'+FreeStr, '', kbSpace, 1200,
         hcTempList, Items);
    Items := NewLine(Items);
    Inc(N)
    end;

  for C := 'Z' downto 'A' do
    if ValidDrive(C) then
      begin
      Inc(DrvCnt);
      DrvStrArr[DrvCnt].Dr := C;
      DrvStrArr[DrvCnt].FullS := '~'+C+':~';
      end;

  for I := 1 to DrvCnt do
    begin
    with DrvStrArr[I] do
      if (InterfaceData.DrvInfType.ForDrives = 0) or
        ((InterfaceData.DrvInfType.ForDrives and 1 <> 0) and
         (Pos(Dr, UpStrg(InterfaceData.DrvInfType.ExceptDrv)) = 0)) then
        begin
       {DriveNum := Byte(C)-64;}
        DT := GetDriveTypeNew(Dr);
        case DT of
          dtnFloppy:
            if (InterfaceData.DrvInfType.TypeShowFor and ditFloppy <> 0) then
              FullS := FullS+GetString(sdtRemovable);
          dtnInvalid:
            FullS := FullS+GetString(sdtError);
          dtnCDRom:
            if  (InterfaceData.DrvInfType.TypeShowFor and ditCDMO <> 0) then
              FullS := FullS+GetString(sdtCDROM);
          dtnOptical:
            if  (InterfaceData.DrvInfType.TypeShowFor and ditCDMO <> 0) then
              FullS := FullS+GetString(sdtOptical);
          dtnProgram:
            if  (InterfaceData.DrvInfType.TypeShowFor and ditProgr <> 0) then
              FullS := FullS+GetString(sdtProgram);
          dtnLAN:
            if  (InterfaceData.DrvInfType.TypeShowFor and ditNet <> 0) then
              FullS := FullS+GetString(sdtRemote);
          dtnHDD:
            if  (InterfaceData.DrvInfType.TypeShowFor and ditHDD <> 0) then
              FullS := FullS+GetString(sdtFixed);
          dtRamDisk:
            if  (InterfaceData.DrvInfType.TypeShowFor and ditProgr <> 0) then
              FullS := FullS+GetString(sdtRAMDrive);
          dtnSubst:
            if  (InterfaceData.DrvInfType.TypeShowFor and ditNet <> 0) then
              FullS := FullS+GetString(sdtSubst);
          dtnUnknown:
            FullS := FullS+GetString(sdtError);
        end {case};
        {X-Man <<<}
        if MaxFullSLength < Length(FullS) then
          MaxFullSLength := Length(FullS);
        end;
    end;

  if Tabulated then
    for I := 1 to DrvCnt do
      DrvStrArr[I].FullS := AddSpace(DrvStrArr[I].FullS, MaxFullSLength);

  for I := 1 to DrvCnt do
    with DrvStrArr[I] do
      if (InterfaceData.DrvInfType.ForDrives = 0) or
        ((InterfaceData.DrvInfType.ForDrives and 1 <> 0) and
         (Pos(Dr, UpStrg(InterfaceData.DrvInfType.ExceptDrv)) = 0)) then
        begin
        if ((DT = dtnUnknown)
          or ((DT = dtnHDD)
            and (InterfaceData.DrvInfType.FSShowFor and ditHDD <> 0))
          or ((DT = dtnFloppy)
            and (InterfaceData.DrvInfType.FSShowFor and ditFloppy <> 0))
          or ((DT in [dtnCDRom,dtnOptical])
            and (InterfaceData.DrvInfType.FSShowFor and ditCDMO <> 0))
          or ((DT = dtnProgram)
            and (InterfaceData.DrvInfType.FSShowFor and ditProgr <> 0)))
        then
          begin
          FullS := FullS + ' ' + GetFSString(Dr);
          if MaxFullSLength < Length(FullS) then
            MaxFullSLength := Length(FullS);
          end;
        end;

  if Tabulated then
    for I := 1 to DrvCnt do
      if not (DrvStrArr[I].DT in [dtnLAN, dtnSubst]) then
        DrvStrArr[I].FullS := AddSpace(DrvStrArr[I].FullS, MaxFullSLength);

  for I := 1 to DrvCnt do
    with DrvStrArr[I] do
      if (InterfaceData.DrvInfType.ForDrives = 0) or
        ((InterfaceData.DrvInfType.ForDrives and 1 <> 0) and
         (Pos(Dr, UpStrg(InterfaceData.DrvInfType.ExceptDrv)) = 0)) then
        begin
        if (
          ((DT = dtnHDD)
            and (InterfaceData.DrvInfType.VLabShowFor and ditHDD <> 0))
          or ((DT = dtnFloppy)
            and (InterfaceData.DrvInfType.VLabShowFor and ditFloppy <> 0))
          or ((DT in [dtnCDRom,dtnOptical])
            and (InterfaceData.DrvInfType.VLabShowFor and ditCDMO <> 0))
          or ((DT = dtnProgram)
            and (InterfaceData.DrvInfType.VLabShowFor and ditProgr <> 0)))
        then
          begin
          FullS := FullS + ' ' + GetVolumeLabel(Dr);
          if MaxFullSLength < Length(FullS) then
            MaxFullSLength := Length(FullS);
          end;
        end;

  if Tabulated then
    for I := 1 to DrvCnt do
      if not (DrvStrArr[I].DT in [dtnLAN, dtnSubst]) then
        DrvStrArr[I].FullS := AddSpace(DrvStrArr[I].FullS, MaxFullSLength);

  K := MaxSizeDig;

  for I := 1 to DrvCnt do
    with DrvStrArr[I] do
      if (InterfaceData.DrvInfType.ForDrives = 0) or
        ((InterfaceData.DrvInfType.ForDrives and 1 <> 0) and
         (Pos(Dr, UpStrg(InterfaceData.DrvInfType.ExceptDrv)) = 0)) then
        begin
        if (
          ((DT = dtnHDD)
            and (InterfaceData.DrvInfType.FreeSpShowFor and ditHDD <> 0))
          or ((DT = dtnFloppy)
            and (InterfaceData.DrvInfType.FreeSpShowFor and ditFloppy <> 0))
          or ((DT in [dtnCDRom,dtnOptical])
            and (InterfaceData.DrvInfType.FreeSpShowFor and ditCDMO <> 0))
          or ((DT = dtnProgram)
            and (InterfaceData.DrvInfType.FreeSpShowFor and ditProgr <> 0)))
        then
          begin
          FreeSp := SysDiskFreeLong(Byte(Dr)-64);
          if FreeSp >= 0 then
            begin
            SizeStr := RtoS(FreeSp/1048576, MaxSizeDig-1, 1) + 'M';
            for J := 1 to Length (SizeStr) do
              if SizeStr[J] <> ' ' then Break;
            if J < K then K := J;
            if not Tabulated then DelLeft(SizeStr);
            FullS := FullS + ' ' + SizeStr;
            end;
          end;
        end;

  if Tabulated then
    for I := 1 to DrvCnt do
      if not (DrvStrArr[I].DT in [dtnLAN, dtnSubst]) then
        Delete(DrvStrArr[I].FullS, MaxFullSLength+1, K-1);

  for I := 1 to DrvCnt do
    with DrvStrArr[I] do
      if DT = dtnLAN then
        FullS := FullS + ' ~'+GetShare(Dr)+'~'
      else if DT = dtnSubst then
        FullS := FullS + ' ~'+GetSubst(Dr)+'~';

  for I := 1 to DrvCnt do
    with DrvStrArr[I] do
      begin
      if Length(FullS) > MaxL then
        MaxL := Length(FullS);
      Items := NewItem(FullS, '', kbNoKey, 1000+Byte(Dr),
          hcNoContext, Items);
      Inc(N);
      end;

  if not (Default in ['A'..'Z']) and not ((Default = '+') and (Lnk <> nil))
  then
    C := GetCurDrive
  else
    C := Default;
  Menu := NewMenu(Items);
  Desktop^.GetExtent(R);
  {-$VIV start}
  X := X-(MaxL div 2);
  if  (X+MaxL+4) > R.B.X then
    X := R.B.X-MaxL-4;
  if  (X < 0) then
    X := 0;
  Y := Y-(N div 2)+1;
  if  (Y+N+2) > R.B.Y then
    Y := R.B.Y-N-2;
  if  (Y < 0) then
    Y := 0;
  R.A.X := X;
  R.A.Y := Y;
  R.B.X := R.A.X+MaxL+4;
  MaxRY := R.B.Y;
  R.B.Y := R.A.Y+N+2;
  if R.A.Y = 0 then
    begin
    Inc(R.A.Y);
    Inc(R.B.Y)
    end;
  if  (R.B.Y > MaxRY) then
    R.B.Y := MaxRY;
  {-$VIV end}
  P := New(PMenuBox, Init(R, Menu, nil)); {-$VIV}
  if  (C = '+') then
    Items := Lnk
  else
    Items := P^.FindItem(C);
  if Items <> nil then
    Menu^.Default := Items;
  P^.HelpCtx := hcSelectDrive+Byte(IncludeTemp = True);

  N := Desktop^.ExecView(P);
  Dispose(P, Done);
  DisposeMenu(Menu);
  SelectDrive := '';
  if N > 1000 then
    SelectDrive := Char(N-1000)+':';
  if N = 1200 then
    SelectDrive := cTEMP_;
  {$IFDEF NetBrowser}
  if N = 1400 then
    SelectDrive := cNET_;
  {$ENDIF}
  if N > 2000 then
    SelectDrive := '+'+Char(N-2000);
  {$IFDEF PLUGIN}
  if N > 65000 then
    SelectDrive := #26+Char(N-65000); {Cat}
  {$ENDIF}
  if  (N >= cmQuickChange1) and (N <= cmQuickChange9)
  then
    SelectDrive := CnvString(DirsToChange[N-cmQuickChange1]);
  end { SelectDrive };

{-DataCompBoy-}
function NewFileRec;
  var
    PR: PFileRec;
    T: TFileRec;
    D: DateTime;
    {$IFDEF DualName}
    Name12: Str12;
    {$ENDIF}
    l: LongInt;
  begin
  {$IFDEF DualName}
  T.FlName[False] := Name;
  if Attr and Directory <> 0 then
    UpStr(T.FlName[False])
  else
    LowStr(T.FlName[False]);
  (*!  case Attr and (Hidden+SysFile) of                  {Pavel Anufrikov -> }
   Hidden  :        T.Name[9] := NameFormatChar[nfmHidden];
   SysFile :        T.Name[9] := NameFormatChar[nfmSystem];
   Hidden+SysFile : T.Name[9] := NameFormatChar[nfmHiddenSystem];
  end{case};                                         { <- Pavel Anufrikov}
!*)
  if Attr and SysFile <> 0 then
    T.FlName[False][1] := UpCase(T.FlName[False][1]);
  CopyShortString(LFN, T.FlName[True]);
  {$ELSE}
  CopyShortString(Name, T.FlName[True]);
  {$ENDIF}
  if (Attr and Directory <> 0) and (Size = 0) then
    T.Size := -1 {AK155 28-11-2005.
      Вообще-то, это не очень хорошо, менять тот размер, который
      задан в вызове. Но очень уж много этих вызовов, чтобы везде
      вставлять такой анализ и -1 в качестве размера.
      Если когда-то окажется, что этим мы испортим имеющуюся
      информацию о нулевом размере, то ничего страшного от этого
      не будет: на общем размере 0 не сказывается, а повторный подсчёт
      этого нуля, если потребуется, будет выполнен достаточно быстро.
      }
  else
    T.Size := Size;
  T.PSize := Size;
  if Date = 0 then
    begin
    T.Yr := 1980;
    T.FDate := $1111;
    T.Second := 1;
    end
  else
    begin
    UnpackTime(Date, D);
    T.Yr := D.Year;
    with TDate4(T.FDate) do
      begin
      Month := D.Month;
      Day := D.Day;
      Hour := D.Hour;
      Minute := D.Min;
      end;
    T.Second := D.Sec;
    end;
  if CreationDate = 0 then
    begin
    T.YrCreat := 0;
    T.FDateCreat := $0000;
    T.SecondCreat := 0;
    end
  else
    begin
    UnpackTime(CreationDate, D);
    T.YrCreat := D.Year;
    with TDate4(T.FDateCreat) do
      begin
      Month := D.Month;
      Day := D.Day;
      Hour := D.Hour;
      Minute := D.Min;
      end;
    T.SecondCreat := D.Sec;
    end;
  if LastAccDate = 0 then
    begin
    T.YrLAcc := 0;
    T.FDateLAcc := $0000;
    T.SecondLAcc := 0;
    end
  else
    begin
    UnpackTime(LastAccDate, D);
    T.YrLAcc := D.Year;
    with TDate4(T.FDateLAcc) do
      begin
      Month := D.Month;
      Day := D.Day;
      Hour := D.Hour;
      Minute := D.Min;
      end;
    T.SecondLAcc := D.Sec;
    end;
  T.Attr := Attr and $7FFF;
  T.Selected := False;
  T.DIZ := nil;
  T.TType := GetFileType(T.FlName[True], T.Attr);
  T.Owner := AOwner;
  T.UsageCount := 1;
  {  l := TFileRecFixedSize+length(T.FlName[true]);}
  l := SizeOf(TFileRec);
  GetMem(PR, l);
  Move(T, PR^, l);
  NewFileRec := PR;
  end { NewFileRec };
{-DataCompBoy-}

function PackedDate(P: PFileRec): LongInt;
  var
    DT: DateTime;
    L: LongInt;
  begin
  with P^ do
    begin
    DT.Year := Yr;
    with TDate4(FDate) do
      begin
      DT.Month := Month;
      DT.Day := Day;
      DT.Hour := Hour;
      DT.Min := Minute;
      end;
    DT.Sec := Second;
    end;
  PackTime(DT, L);
  PackedDate := L;
  end;

function PackedCreationDate(P: PFileRec): LongInt;
  var
    DT: DateTime;
    L: LongInt;
  begin
  with P^ do
    begin
    DT.Year := YrCreat;
    with TDate4(FDateCreat) do
      begin
      DT.Month := Month;
      DT.Day := Day;
      DT.Hour := Hour;
      DT.Min := Minute;
      end;
    DT.Sec := SecondCreat;
    end;
  PackTime(DT, L);
  PackedCreationDate := L;
  end;

function PackedLastAccDate(P: PFileRec): LongInt;
  var
    DT: DateTime;
    L: LongInt;
  begin
  with P^ do
    begin
    DT.Year := YrLAcc;
    with TDate4(FDateLAcc) do
      begin
      DT.Month := Month;
      DT.Day := Day;
      DT.Hour := Hour;
      DT.Min := Minute;
      end;
    DT.Sec := SecondLAcc;
    end;
  PackTime(DT, L);
  PackedLastAccDate := L;
  end;

{ Файловые записи считаются относящимися к одному и тому же файлу,
если совпадают (длинное) имя и путь. }
function SameFile(P1, P2: PFileRec): Boolean;
  begin
  Result := False;
  if P1^.FlName[True] <> P2^.FlName[True] then
    Exit;
  if (P1^.Owner <> P2^.Owner) and (P1^.Owner^ <> P2^.Owner^) then
    Exit;
  Result := True;
  end;

procedure TFilesHash.Hash(Item: Pointer);
  var
    i: Integer;
    R: Longint;
  begin
  R := 0;
  with PFileRec(Item)^ do
    begin
    for i := 1 to Length(FlName[True]) do
      R := R*3 + Byte(FlName[True][i]);
    if Owner <> nil then
      for i := 1 to Length(Owner^) do
        R := R*3 + Byte(PString(Owner)^[i]);
    end;
  R := (R * $33C6EF37) and $7FFFFFF;
  hf := R mod Count;
  RehashStep := ((R div Count) mod Count) or 1;
  end;

function TFilesHash.Equal(Item1, Item2: Pointer): Boolean;
  begin
  Result := SameFile(Item1, Item2);
  end;

procedure TFilesCollection.DelDuplicates(var TotalInfo: TSize);
  type
    TIsDupe = function(i: Integer): Boolean;
  var
    i,j, DupeStart, k: Integer;
    H: PFilesHash;
    S: TSize;
    IsDupe: TIsDupe;

  function IsSortedDupe(i: Integer): Boolean;
    begin { в сортированной коллекции дупы стоят рядом }
    Result := SameFile(Items^[i-1], Items^[i]);
    end;

  function IsUnsortedDupe(i: Integer): Boolean;
    begin
    Result := not H^.AddItem(i);
    end;

  begin
  if not Duplicates then
    Exit;
  TotalInfo := 0;
  H := nil;
  if SortMode = psmUnsorted then
    begin
    New(H, Init(@Self));
    if H^.HT <> nil then
      Exit; //! Наверно, памяти мало, сообщить бы об этом
    @IsDupe := @IsUnsortedDupe;
    end
  else
    @IsDupe := @IsSortedDupe;

  { Каждый пакет дупов сначала полностью выявляется, а затем
  удаляется. Удалять по одному некорректно, так как после этого
  некорректным становится сравнение следующего с предыдущим
  (удалённым). Для результатов поиска оно, впрочем, работает,
  только потому, что у записей результатов поиска UsageCount>1,
  на что закладываться нет никаго смысла.}
  j := 1; DupeStart := 1;
  for i := 1 to Count-1 do
    if not IsDupe(i) then
      begin
      for k := DupeStart to i-1 do
        DelFileRec(PFileRec(Items^[k]));
      DupeStart := i+1; // новый кандидат на начало пакета дупов
      Items^[j] := Items^[i];
      S := PFileRec(Items^[j])^.Size;
      if S > 0 then {у каталога с неизвестным размером Size=-1}
        TotalInfo := TotalInfo + S;
      inc(j);
      end;
  Count := j;
  Duplicates := False;
  { лишнюю память в Items^ не освобождаем }
  if H <> nil then
    Dispose(H, Done);
  end;

end.
