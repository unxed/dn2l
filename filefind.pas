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
{JO: 17.06.2002 - добавил поиск файлов в архивах}
{JO:  2.12.2002 - добавил разархивирование файлов, найденных в архивах}
{$I STDEFINE.INC}
unit FileFind;

interface

uses
  dnsys, drivers2, math,
  Defines, Objects2, Streams, Views, Dialogs, Drivers,
  FilesCol, Drives, Gauge, DiskInfo, Objects
  ;

type
  TFindRec = record
    Mask: String; {DataCompBoy}
    What: String;
    Options: Word;
    Where: Word;
    AddChar: String[3];
    end;

  TArcFindRec = record
    Mask: String;
    FakeWhat: String;
    Options: Word;
    end;

  PListItem = ^TListItem;
  TListItem = record
    Next: PListItem;
    Item: Pointer;
    end;

  TAdvanceSearchData = record
    After: String[17]; {Inputline}
    Before: String[17]; {Inputline}
    Greater: String[12]; {Inputline}
    Less: String[12]; {Inputline}
    Attr: Word; {Checkbox}
    end;

 const  { какие-то рудименты чего-то не доделанного Ритлабсами
  CFindWindow = #126#127#128#129#130#131#132#133#134#135#136#137#138;
  CFileFinder = #6#7#8#9;
  CFindInfo = #10#11#12#13;}

  ffSeFnd = $01; {Found a file}
  {-$VOL}
  ffSeD2Lng = $02; {Search string overflow}
  {-$VOL}
  ffSeNotFnd = $FE; {No files found}
  {-$VOL}

  ffoAdvanced = 1;
  ffoNoSort = 2; {AK155  07.12.04}
  ffoRecursive = 4;
  ffoInArch = 8; {JO  17.06.02}
  ffoCaseSens = 16;
  ffoWholeWords = 32; {-$VIV 14.05.99}
  ffoAllCP = 64; {-$VIV 14.05.99}
  ffoRegExp = 128; {Cat 06.12.01}

  FindRec: TFindRec = (Mask: '*.*';
    What: '';
    Options: 4; {рекурсивный поиск}
    Where: 0; {поиск в текущем каталоге}
    AddChar: ''
    );

  AdvanceSearchData: TAdvanceSearchData =
    (After: '';
    Before: '';
    Greater: '';
    Less: '';
    Attr: 0
    );

  {    TempStored: Boolean = False;}

var
  ArcFindRec: TArcFindRec absolute FindRec;

function FindFiles(var Files: PFilesCollection;
     var Directories: PCollection; var FindRec: TFindRec;
     PInfo: PWhileView; SourceFC: PFilesCollection; InBranch: Boolean): Byte;
function ReadList(const AName: String; var DC: PSortedCollection;
     var FC: PFilesCollection): Boolean; {DataCompBoy}
function ParseTime(S: String): LongInt;

type

  PFindDrive = ^TFindDrive;
  TFindDrive = object(TDrive)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    isDisposable: Boolean;
    Files: PFilesCollection;
    Dirs: PSortedCollection;
      {В Dirs хранятся строки встретившихся путей для того, чтобы на них
      ссылались Owner файловых записей из Files. В Dirs каждый путь
      хранится в одном экземпляре.}
    ListFile: PString;
    UpFile: PFileRec; {DataCompBoy}
    AMask, AWhat: PString;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); virtual;
    procedure NewUpFile;
    constructor Init(const AName: String; ADirs: PCollection;
         AFiles: PFilesCollection);
    constructor InitList(const AName: String); {DataCompBoy}
    procedure lChDir(ADir: String); virtual; {DataCompBoy}
    function GetDirectory(
         const FileMask: String;
        var TotalInfo: TSize): PFilesCollection; virtual;
    procedure CopyFiles(AFiles: PCollection; Own: PView;
         MoveMode: Boolean); virtual;
    procedure CopyFromArc(AFiles: PFilesCollection; Own: PView); {JO}
    procedure CopyFilesInto(AFiles: PCollection; Own: PView;
         MoveMode: Boolean); virtual;
    procedure EraseFiles(AFiles: PCollection); virtual;
    procedure UseFile(P: PFileRec; Command: Word); virtual; {JO}
    function Disposable: Boolean; virtual;
    function GetRealName: String; virtual;
    function GetInternalName: String; virtual;
    function GetDir: String; virtual;
    procedure MakeDir; virtual;
    destructor Done; virtual;
    {DataCompBoy}
    procedure GetFreeSpace(var S: String); virtual;
    function isUp: Boolean; virtual;
    procedure ChangeUp(var S: String); virtual;
    procedure ChangeRoot; virtual;
    procedure RereadDirectory(S: String); virtual;
    function GetFullFlags: Word; virtual;
    procedure GetDirInfo(var B: TDiskInfoRec); virtual;
    procedure HandleCommand(Command: Word; InfoPtr: Pointer); virtual;
    function OpenDirectory(const Dir: String;
                                 PutDirs: Boolean): PDrive; virtual;
    procedure DrvFindFile(FC: PFilesCollection); virtual;
    procedure ReadDescrptions(FilesC: PFilesCollection); virtual;
    function GetDriveLetter: Char; virtual;
    end;

  PTempDrive = ^TTempDrive;
  TTempDrive = object(TFindDrive)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    constructor Init;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); virtual;
    procedure CopyFilesInto(AFiles: PCollection; Own: PView;
         MoveMode: Boolean); virtual;
    function GetRealName: String; virtual;
    function GetInternalName: String; virtual;
    procedure CopyFiles(AFiles: PCollection; Own: PView;
         MoveMode: Boolean); virtual;
    procedure EraseFiles(AFiles: PCollection); virtual;
    procedure ChangeRoot; virtual;
    procedure GetDirInfo(var B: TDiskInfoRec); virtual;
    destructor Done; virtual;
    function GetDriveLetter: Char; virtual;
    end;

procedure CopyToTempDrive(AFiles: PCollection; Own: PView;
     ArchiveName: String);

const
  chTempDrive = '*';

{$IFDEF DualName}
var
  ShortNameSearch: Boolean;
    {` Поиск выполнять по коротким именам. Присваивается панелью
     перед поиском в соотвествии с режимом отображения панели.`}
{$ENDIF}

implementation
uses
  Lfnvp {DataCompBoy}, DNApp, Advance1, Advance2, Startup, Dos,
  Memory, Messages, HistList, Commands, FlPanelX, FlPanel
  , FViewer, Microed,
  Tree, xTime, DNUtil, UKeyMap, {!!}CmdLine, Histries,
  Archiver, ArchDet {JO},
  ArcView {JO: для разархивирования файлов найденных в архивах}
  , Events {AK155 для LongWorkBegin - LongWorkEnd}
  , FlTl {JO: для GetDriveTypeNew}
  , filetype, Eraser, Advance, Files, DnIni, Menus, FileCopy
  , PDSetup, VPUtils
  ;

const
  LowMemSize = $6000; {Local setting}

var
  FAdr: Word;
  UsM: Word;
  RD, Ofs: Word;

function ESC_Pressed: Boolean;
  var
    E: TEvent;
  begin
  Application^.Idle;
  GetKeyEvent(E);
  ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbESC)
  end;

function ReadingListMsg: PView;
  begin
  ReadingListMsg := WriteMsg(GetString(dlReadingList))
  end;

function ParseTime(S: String): LongInt;
  label 1, 2;
  var
    DT: DateTime;
    S1, S2, S3: String[5];
    TM: String[10];
    I, J: Integer;
    M: LongInt;
  begin
  ParseTime := 0;
  DT.Sec := 0;
  DT.Hour := 0;
  DT.Min := 0;
  I := PosChar(' ', S);
  if I > 0 then
    begin
    TM := DelSpaces(Copy(S, I+1, 10));
    if TM <> '' then
      begin
      I := Pos(CountryInfo.TimeSep, TM);
      if I = 0 then
        Exit;
      DT.Hour := StoI(Copy(TM, 1, I-1));
      J := 0;
2:
      case UpCase(TM[Length(TM)]) of
        'M':
          begin
          J := 12;
          goto 2
          end;
        'A':
          SetLength(TM, Length(TM)-1);
        'P':
          begin
          J := 12;
          SetLength(TM, Length(TM)-1);
          end;
        '0'..'9':
          ;
        else {case}
          goto 1;
      end {case};
      Inc(DT.Hour, J);
      DT.Min := StoI(Copy(TM, I+Length(CountryInfo.TimeSep), 10));
      end;
1:
    SetLength(S, PosChar(' ', S)-1);
    end
  else
    TM := '';
  I := Pos(CountryInfo.DateSep, S);
  if I = 0 then
    Exit;
  S1 := Copy(S, 1, I-1);
  Delete(S, 1, I+Length(CountryInfo.DateSep)-1);
  I := Pos(CountryInfo.DateSep, S);
  if I = 0 then
    Exit;
  S2 := Copy(S, 1, I-1);
  Delete(S, 1, I+Length(CountryInfo.DateSep)-1);
  case CountryInfo.DateFmt of
    0:
      begin
      DT.Year := StoI(S);
      if DT.Year < 80 then
        Inc(DT.Year, 2000)
      else if DT.Year < 100 then
        Inc(DT.Year, 1900);
      DT.Day := StoI(S2);
      if DT.Day > 31 then
        Exit;
      DT.Month := StoI(S1);
      if DT.Month > 12 then
        Exit;
      end;
1:
      begin
      DT.Year := StoI(S);
      if DT.Year < 80 then
        Inc(DT.Year, 2000)
      else if DT.Year < 100 then
        Inc(DT.Year, 1900);
      DT.Day := StoI(S1);
      if DT.Day > 31 then
        Exit;
      DT.Month := StoI(S2);
      if DT.Month > 12 then
        Exit;
      end;
2:
      begin
      DT.Year := StoI(S1);
      if DT.Year < 80 then
        Inc(DT.Year, 2000)
      else if DT.Year < 100 then
        Inc(DT.Year, 1900);
      DT.Day := StoI(S);
      if DT.Day > 31 then
        Exit;
      DT.Month := StoI(S2);
      if DT.Month > 12 then
        Exit;
      end;
  end {case};
  PackTime(DT, M);
  ParseTime := M;
  end { ParseTime };

{-DataCompBoy-}
function FindFiles(var Files: PFilesCollection;
     var Directories: PCollection; var FindRec: TFindRec;
     PInfo: PWhileView; SourceFC: PFilesCollection; InBranch: Boolean): Byte;
  label Common1;
  var
    FN: String;
    C: Char;
    Drv: PFindDrive;
    Pnl: PView;
    CancelSearch: Boolean;
    DateAfter, DateBefore,
    SizeGreat, SizeLess: LongInt;
    Attr: Byte;
    T: TEventTimer;
    FF: String;
    TotalNum: LongInt; {-$VIV}
    FFResult: Byte; {-$VOL}
    AType: PARJArchive; {JO}
    CurFileRec: PFileRec; {JO}
    CurP: LongInt; {JO}
    LookInArchives: Boolean; {JO}

  procedure InitPanel;
    begin
    Pnl := Message(Desktop, evBroadcast, cmInsertDrive, Drv);
    if Pnl <> nil then
        begin
        PFilePanel(Pnl)^.ChangeLocked := True;
{$IFDEF DualName}
       {AK155 Показ длинных/коротких имён привести в соответствие с режимом
        поиска, то есть он будет таким же, как у родительской панели }
        with PFilePanel(Pnl)^.PanSetup^.Show do
          begin
          if ShortNameSearch then
            ColumnsMask := ColumnsMask and not psLFN_InColumns
          else
            ColumnsMask := ColumnsMask or psLFN_InColumns;
          end;
{$ENDIF}
        end;
    end;

  procedure DispatchEvents;
    var
      Event: TEvent;
    begin
    if PInfo = nil then
      Exit;
    Application^.GetEvent(Event);
    if  (Event.What = evCommand) and (Event.Command = cmCancel) and
        (Event.InfoPtr = PInfo^.But)
    then
      begin
      PInfo^.ClearEvent(Event);
      CancelSearch := MessageBox(GetString(dlQueryCancelSearch), nil,
           mfYesNoConfirm) = cmYes;
      end;
    if  (Event.What <> evNothing)
           and not ((Event.What = evCommand) and (Event.Command = cmQuit))
    then
      Application^.HandleEvent(Event);
    end;

  function SearchF(FilePath: String): Boolean;
    var
      S: TDOSStream;
      {AK155: было TBufStream; буферизация здесь абсолютно не нужна }
      CaseSensitive: Boolean;
    begin
    PInfo^.Write(2, Cut(FilePath, 50));
    Inc(TotalNum); {-$VIV}
    PInfo^.Write(3, GetString(dlLookedFiles)+ItoS(TotalNum)); {-$VIV}
    S.Init(FilePath, stOpenRead);
    CaseSensitive := FindRec.Options and ffoCaseSens <> 0;
    SearchF := (S.Status = stOK) and
        (FViewer.SearchFileStr(@S,
          KeyMapDescr[kmAscii].XLatCP^[Ord(CaseSensitive)],
          FindRec.What, 0,
          CaseSensitive,
          {-$VIV 14.05.99}
          { Display       }False,
          { WholeWords    }FindRec.Options and ffoWholeWords <> 0,
          { Back          }False,
          { AllCP         }FindRec.Options and ffoAllCP <> 0,
          {-$VIV 14.05.99}
          { IsRegExp      }FindRec.Options and ffoRegExp <> 0)
        {Cat 06.12.01}
        >= 0);
    S.Done;
    end { SearchF: };

  {JO: добавил поиск в архивах 17.06.02}
  procedure SearchData(Path: String);
    label Skip,
      NotArchive; {JO}
    var
      PDir: PString;
      SR: lSearchRec;
      PName: PString; //AK155 В SR имя для сравнения с маской
      P: PFileRec;
      I: Byte;
      D: DateTime;
      DirCol: PDirCol;
      {JO}
      ArcPath: String;
      ArcTime: LongInt;
      LDir, DrName: String;
      ArcDirs: PCollection;
      PArcLastDir: PString;
      MemReq: LongInt;
      MAvail: LongInt;
      {/JO}
      SearchAttr: word;

    begin { SearchData }
    if  (Length(Path)+Length(x_x)) > (MaxPathLen) then
      {-$VOL} {DataCompBoy}
      begin
      FFResult := FFResult or ffSeD2Lng;
      Exit;
      end;

    if CancelSearch then
      Exit;
    PName := @SR.FullName;
    {$IFDEF DualName}
    if ShortNameSearch then // в панели короткие имена
      PName := @SR.SR.Name;
    {$ENDIF}
    LongWorkBegin;
    //New(DirCol, Init($10, $10, False));
    // fixme: by unxed
    New(DirCol);
    DirCol^.Insert(NewStr(Path));
    {JO: сначала один pаз опpеделяем объём доступной памяти, а затем по ходу дела}
    {    подсчтитываем насколько тpебования памяти pастут и не пpевысили ли они  }
    {    доступный изначально объём                                              }
    MemReq := LowMemSize;
    MAvail := MaxAvail;
    while DirCol^.Count > 0 do
      begin
      if CancelSearch then
        Break;
      Path := PString(DirCol^.At(0))^;
      DirCol^.AtFree(0);
      PDir := nil;
      DosError := 0;
      if PInfo <> nil then
        PInfo^.Write(2, Cut(Path, 50));
      if  (FindRec.What = '') then
        begin
        Inc(TotalNum); {-$VIV}
        PInfo^.Write(3, GetString(dlLookedDirs)+ItoS(TotalNum));
        {-$VIV}
        end;
      if Path[Length(Path)] <> '|' then
        {JO}
        begin {начало поиска не в архиве}
        SearchAttr := AnyFileDir;
        if Security then
          SearchAttr := AnyFileDir and not Hidden;
        lFindFirst(Path+x_x, SearchAttr, SR);

        while (DosError = 0) and not CancelSearch and (MAvail > MemReq)
        do
          begin
          if not IsDummyDir(SR.SR.Name)
            and (InFilter(PName^, FindRec.Mask+FindRec.AddChar))
            and ((FindRec.Options and ffoAdvanced = 0) or (SR.SR.Time >=
                 DateAfter)
              and (SR.SR.Time <= DateBefore) and (SR.FullSize >=
                 SizeGreat) and
                (SR.FullSize <= SizeLess) and ((Attr = 0) or (SR.SR.Attr
                   and Attr <> 0)))
            and ((FindRec.What = '') or (SR.SR.Attr and Directory <> 0)
               and (FindRec.What = '')
              or (FindRec.What <> '') and SearchF(Path+SR.FullName))
          then
            begin
            if PDir = nil then
              begin
              PDir := NewStr(Path);
              Inc(MemReq, Length(Path)+1);
              end;
            P := NewFileRec(SR.FullName, {$IFDEF DualName}SR.SR.Name,
                {$ENDIF}SR.FullSize, SR.SR.Time, SR.SR.CreationTime,
                 SR.SR.LastAccessTime, SR.SR.Attr, PDir);
            Inc(MemReq, SizeOf(TFileRec));
            Inc(MemReq, Length(PDir^+SR.FullName)+2);
            if Pnl = nil then
              InitPanel;
            if Pnl <> nil then
              Message(Pnl, evCommand, cmInsertFile, CopyFileRec(P));
            Files^.AtInsert(Files^.Count, P);
            if PInfo <> nil then
              PInfo^.Bottom := ItoS(Files^.Count)+FF;
            if TimerExpired(T) then
              begin
              DispatchEvents;
              if PInfo <> nil then
                PInfo^.DrawView;
              NewTimer(T, 50);
              end;
            end;
          if TimerExpired(T) then
            begin
            DispatchEvents;
            NewTimer(T, 50);
            end;
          DosError := 0;
          lFindNext(SR);
          end;
        lFindClose(SR);
        {$IFDEF OS2}
        if DosError = 49 then
          MessageBox(GetString(dl_CodePage_FS_Error), nil,
             mfError+mfOKButton);
        {$ENDIF}
        end {конец поиска не в архиве}
      else
        {JO}
        begin {начало поиска в архиве}
        CtrlBreakHit := False;
        ArcPath := Copy(Path, 1, Length(Path)-1);
        ArcTime := FileTime(ArcPath);
        New(ArcFile, Init(ArcPath, stOpenRead, 512));
        if  (ArcFile = nil) or (ArcFile^.Status <> stOK) then
          goto NotArchive;
        SkipSFX;
        AType := DetectArchive;
        (*
        if  (AType = nil)
          or (AType^.GetID in [arcAIN, arcUC2, Arc7Z])
          {временно! - надо решить проблему с лочкой dndosout.bat}
          {15.02.2005 AK155 7z никакого отношения к dndosout.bat не имеет,
           и поиск в нём почти работает, но пока подглючивает.}
          then
          goto NotArchive;
          *)
          // fixme: commented by unxed
        //ArcDirs := New(PStringCollection, Init(30, 30, False));
        // fixme: by unxed
        ArcDirs := New(PStringCollection);
        repeat
          AType^.GetFile;
          if FileInfo.Last = 0 then
            begin
            // commented by unxed
            //Replace('/', '\', FileInfo.FName);
            with FileInfo do
              begin
              if FName[1] <> '/' then // slash change by unxed
                FName := '/'+FName; // slash change by unxed
              if FName[Length(FName)] = '/' then // slash change by unxed
                begin
                SetLength(FName, Length(FName)-1);
                Attr := Attr or Directory;
                end;
              end;
            if  (GetName(FileInfo.FName) <> '')
              and InFilter(GetName(FileInfo.FName),
                 FindRec.Mask+FindRec.AddChar)
              and ((FindRec.Options and ffoAdvanced = 0) or
                   (FileInfo.Date >= DateAfter)
                and (FileInfo.Date <= DateBefore) and (FileInfo.USize >=
                   SizeGreat)
                and (FileInfo.USize <= SizeLess) and ((Attr = 0) or
                     (FileInfo.Attr and Attr <> 0)))
            then
              begin
              if  (FileInfo.Attr and Directory) <> 0 then
                begin
                if FileInfo.FName[Length(FileInfo.FName)] = '/' then // slash change by unxed
                  PArcLastDir := NewStr(UpStrg(FileInfo.FName))
                else
                  PArcLastDir := NewStr(UpStrg(FileInfo.FName+'/')); // slash change by unxed
                Inc(MemReq, Length(PArcLastDir^)+1);
                end;
              if  ( ( (FileInfo.Attr and Directory) = 0) or
                    (ArcDirs^.IndexOf(PArcLastDir) = -1))
              then
                begin
                PDir := NewStr(ArcPath+':'+GetPath(FileInfo.FName));
                Inc(MemReq, Length(PDir^)+1);
                P := NewFileRec(GetName(FileInfo.FName),
                    {$IFDEF DualName}GetName(FileInfo.FName), {$ENDIF}
                    FileInfo.USize,
                    FileInfo.Date,
                    0,
                    0,
                    FileInfo.Attr,
                    PDir);
                Inc(MemReq, SizeOf(TFileRec));
                Inc(MemReq, Length(PDir^+GetName(FileInfo.FName))+2);
                {$IFDEF DualName}
                if UpStrg(P^.FlName[False]) <> UpStrg(P^.FlName[True])
                then
                  P^.FlName[False] := NoShortName;
                {$ENDIF}
                if Directories^.IndexOf(PDir) = -1 then
                  Directories^.Insert(PDir);
                if Pnl = nil then
                  InitPanel;
                if Pnl <> nil then
                  Message(Pnl, evCommand,
                    cmInsertFile, CopyFileRec(P));
                Files^.AtInsert(Files^.Count, P);
                if  (FileInfo.Attr and Directory) <> 0 then
                  ArcDirs^.Insert(PArcLastDir);
                if PInfo <> nil then
                  PInfo^.Bottom := ItoS(Files^.Count)+FF;
                if TimerExpired(T) then
                  begin
                  DispatchEvents;
                  if PInfo <> nil then
                    PInfo^.DrawView;
                  NewTimer(T, 50);
                  end;
                end
              else
                DisposeStr(PArcLastDir);
              end;
            {добавляем каталоги которые присутствуют только в виде путей к файлам }
            LDir := GetPath(FileInfo.FName);
            if Length(LDir) > 2 then
              repeat
                PArcLastDir := NewStr(UpStrg(LDir));
                Inc(MemReq, Length(PArcLastDir^)+1);
                SetLength(LDir, Length(LDir)-1);
                for I := Length(LDir) downto 1 do
                  if LDir[I] = '/' then // slash change by unxed
                    Break;
                DrName := Copy(LDir, I+1, MaxStringLength);
                SetLength(LDir, I);
                if  (DrName <> '') and
                    ( ( (FindRec.Options and ffoAdvanced) = 0) or
                      (Attr = 0) or ((Attr and Directory) <> 0)) and
                  InFilter(DrName, FindRec.Mask+FindRec.AddChar) and
                    (ArcDirs^.IndexOf(PArcLastDir) = -1)
                then
                  begin
                  ArcDirs^.Insert(PArcLastDir);
                  PDir := NewStr(ArcPath+':'+Copy(LDir, 1, I));
                  Inc(MemReq, Length(PDir^)+1);
                  P := NewFileRec(DrName, {$IFDEF DualName}DrName, {$ENDIF}
                      0,
                      ArcTime,
                      0,
                      0,
                      $80 or Directory,
                      PDir);
                  Inc(MemReq, SizeOf(TFileRec));
                  Inc(MemReq, Length(PDir^+DrName)+2);
                  {$IFDEF DualName}
                  if UpStrg(P^.FlName[False]) <> UpStrg(P^.FlName[True])
                  then
                    P^.FlName[False] := NoShortName;
                  {$ENDIF}
                  if Directories^.IndexOf(PDir) = -1 then
                    Directories^.Insert(PDir);
                  if Pnl = nil then
                    InitPanel;
                  if Pnl <> nil then
                    Message(Pnl, evCommand,
                      cmInsertFile, CopyFileRec(P));
                  Files^.AtInsert(Files^.Count, P);
                  if PInfo <> nil then
                    PInfo^.Bottom := ItoS(Files^.Count)+FF;
                  if TimerExpired(T) then
                    begin
                    DispatchEvents;
                    if PInfo <> nil then
                      PInfo^.DrawView;
                    NewTimer(T, 50);
                    end;
                  end
                else
                  DisposeStr(PArcLastDir);
              until Length(LDir) <= 2;
            {конец добавления каталогов которые присутствуют только в виде путей к файлам}
            end;
          if TimerExpired(T) then
            begin
            DispatchEvents;
            NewTimer(T, 50);
            end;
        until (FileInfo.Last > 0)
           or CtrlBreakHit or CancelSearch or (MAvail <= MemReq);
        ArcDirs^.DeleteAll;
        Dispose(ArcDirs, Done);
        CtrlBreakHit := False;
NotArchive:
        FreeObject(ArcFile);
        end; {конец поиска в архиве}
      {/JO}
      if  (MAvail <= MemReq) then
        Drv^.NoMemory := True;

      if TimerExpired(T) then
        begin
        DispatchEvents;
        NewTimer(T, 50);
        end;
      if  ( (FindRec.Options and ffoRecursive <> ffoRecursive) and
          not LookInArchives) or
          (Path[Length(Path)] = '|')
      then
        goto Skip;
      DosError := 0;
      lFindFirst(Path+x_x, AnyFileDir, SR);
      while (DosError = 0) and not LowMemory and not CancelSearch do
        begin
        if {$IFNDEF OS2} {<VolumeId.001>}
             (SR.SR.Name[1] <> '.') and (SR.FullName <> '.')
             and (SR.FullName <> '..')
          {$ELSE}(SR.SR.Name <> '..') and (SR.SR.Name <> '.') {$ENDIF}
        then
          if  (SR.SR.Attr and Directory <> 0) then
            begin
            if  (FindRec.Options and ffoRecursive <> 0) then
              DirCol^.Insert(NewStr(Path+SR.FullName+'/')) // slash change by unxed
            end
          else
            {JO}
           if LookInArchives and
             ( (GetFileType(SR.FullName, 0) = ttArc)
              or InExtFilter(SR.FullName, AddArchives))
            {             or ((PosChar('.', SR.Fullname) = 0) and (SR.SR.Attr and SysFile = 0))}
            then
            DirCol^.Insert(NewStr(Path+SR.FullName+'|'));
        {/JO}
        lFindNext(SR);
        end;
      lFindClose(SR);
      {$IFDEF OS2}
      if DosError = 49 then
        MessageBox(GetString(dl_CodePage_FS_Error), nil,
           mfError+mfOKButton);
      {$ENDIF}
Skip:
      if PDir <> nil then
        Directories^.Insert(PDir);
      DosError := 0;
      if PInfo <> nil then
        PInfo^.DrawView;
      end;
    Dispose(DirCol, Done);
    DirCol := nil;
    LongWorkEnd;
    end { SearchData };

  { Flash >>> } {JO - вынес в отдельную пpоцедуру}
  procedure CheckPathInMask;
    begin
    MakeSlash(FN);
    if PathExist(FN+GetPath(FindRec.Mask))
         and (Pos('/', FindRec.Mask) <> 0) // slash change by unxed
    then
      begin
      FN := FN+GetPath(FindRec.Mask);
      while Pos('/', FindRec.Mask) <> 0 do // slash change by unxed
        Delete(FindRec.Mask, 1, Pos('/', FindRec.Mask)); // slash change by unxed
      end;
    MakeSlash(FN);
    end;
  { Flash <<< }

 {JO: 2-04-2006 - поиск файлов в панели поиска/ветви}
  procedure SearchDataInBranch(SrcFC: PFilesCollection);
    var
      FR: PFileRec;
      DT: DateTime;
      CurTime: Longint;
      MemReq: LongInt;
      MAvail: LongInt;
      CurSel1: LongInt; {JO}
      LCol: PStringCollection; {JO}
      CurSel: LongInt; {JO}
      PDir: PString;

    { Удаление из LCol^ всех подкаталогов. При поиске их отдельно
    просматривать не надо, так как они будут просмотрены через
    объемлющий каталог.
      Поскольку коллекция сортированная по возрастанию строк,
    объемлющий каталог всегда непосредственно предшествует всем
    своим подкаталогам. }
    procedure DelDuplicatesSubdir;
      var
        i, j: Integer;
        P0, P1: PString;
      begin
      if LCol^.Count < 2 then
        Exit;
      j := 1; P0 := LCol^.At(0);
      for i := 1 to LCol^.Count-1 do
        begin
        P1 := LCol^.At(i);
        if Pos(P0^+'/', P1^) = 1 then // slash change by unxed
          DisposeStr(P1) // удаляем подкаталог
        else
          begin
          LCol^.Items^[j] := LCol^.Items^[i];
          P0 := P1;
          inc(j);
          end;
        end;
      LCol^.Count := j;
      end;

    begin
    if SrcFC = nil then
      Exit;
    MemReq := LowMemSize;
    MAvail := MaxAvail;

    //LCol := New(PStringCollection, Init(10, 10, False));
    // fixme: by unxed
    LCol := New(PStringCollection);

    LongWorkBegin;

    for CurSel1 := 0 to SrcFC^.Count-1 do {начало цикла}
      begin
      if CancelSearch or (MAvail <= MemReq) then
        Break;

      FR := PFileRec(SrcFC^.At(CurSel1));
      FR^.Selected := False;

      DT.Year := FR^.Yr;
      with TDate4(FR^.FDate) do
        begin
        DT.Month := Month;
        DT.Day := Day;
        DT.Hour := Hour;
        DT.Min := Minute;
        end;
      DT.Sec := FR^.Second;

      PackTime(DT, CurTime);

      if not IsDummyDir(FR^.FlName[uLfn])
         and (InFilter(FR^.FlName[uLfn], FindRec.Mask+FindRec.AddChar))
          and ((FindRec.Options and ffoAdvanced = 0) or (CurTime >=
                 DateAfter)
              and (CurTime <= DateBefore) and (FR^.PSize >=
                 SizeGreat) and
                (FR^.PSize <= SizeLess) and ((Attr = 0) or (FR^.Attr
                   and Attr <> 0)))
            and (LookInArchives or not PathFoundInArc(FR^.Owner^))
            and ((FindRec.What = '') or (FR^.Attr and Directory <> 0)
               and (FindRec.What = '')
              or (FindRec.What <> '') and SearchF(MakeNormName(
                           FR^.Owner^, FR^.FlName[uLfn]))) then
        begin
        PDir := NewStr(FR^.Owner^);
        if (PDir <> nil)
          and (Directories^.IndexOf(PDir) = -1) then
            Directories^.Insert(PDir);
//JO: нижележащий кусок закомментирован, т.к. InitPanel тянет за собой
//    TFilePanelRoot.ReadDirectory , а в ней уничтожается коллекция файлов
//    текущей панели, с которой у SrcFC будуть общие записи, если последняя
//    получена с текущей панели помощью FLTools.GetSelection , и это может
//    приводить к падениям во время поиска
//    То, что мы делаем InitPanel по завершении цикла, имеет только то
//    последствие, что панель с результатами поиска мы увидим по завершении
//    цикла. Это не смертельно, т.к. поиск в панели обычно происходит быстро
       {if Pnl = nil then
          InitPanel;
        if Pnl <> nil then
          Message(Pnl, evCommand, cmInsertFile, CopyFileRec(FR));}

        Files^.AtInsert(Files^.Count, CopyFileRec(FR));
        Inc(MemReq, SizeOf(TFileRec));
        Inc(MemReq, Length(FR^.Owner^+FR^.FlName[True])+2);

        if PInfo <> nil then
          PInfo^.Bottom := ItoS(Files^.Count)+FF;
        if TimerExpired(T) then
          begin
          DispatchEvents;
          if PInfo <> nil then
            PInfo^.DrawView;
          NewTimer(T, 50);
          end;
        end;

{JO: добавляем каталоги и аpхивы в коллекцию стpок для поиска}
        if  (FR^.Attr and Directory <> 0) then
          begin
          if  (FindRec.Options and ffoRecursive <> 0) then
            LCol^.Insert(NewStr(MakeNormName(FR^.Owner^,
                                           FR^.FlName[uLfn])));
          end
        else
          if LookInArchives and
            ((FR^.TType = ttArc)
              or InExtFilter(FR^.FlName[uLfn], AddArchives))
            {             or ((PosChar('.', FR^.FlName[uLfn]) = 0) and
                             (FR^.Attr and SysFile = 0))}
          then
            LCol^.Insert(NewStr(MakeNormName(FR^.Owner^,
                                           FR^.FlName[uLfn])+'|'));
{JO: конец добавления каталогов и аpхивов в коллекцию стpок для поиска}
      if TimerExpired(T) then
        begin
        DispatchEvents;
        NewTimer(T, 50);
        end;
      end; {конец цикла}

    if (Pnl = nil) and (Files^.Count > 0) then
      InitPanel; {создаём панель; найденное в цикле ужЕ в ней}

    if LCol <> nil then
      begin
      DelDuplicatesSubdir;
      if LCol^.Count > 0 then
        for CurSel := 0 to LCol^.Count-1 do
          begin
          FN := CnvString(LCol^.At(CurSel));
          if FN[Length(FN)] <> '|' then
            CheckPathInMask;
          SearchData(FN);
          end;
      Dispose(LCol, Done);
      end;

    if (MAvail <= MemReq) then
      Drv^.NoMemory := True;
    if TimerExpired(T) then
      begin
      DispatchEvents;
      NewTimer(T, 50);
      end;
    if PInfo <> nil then
      PInfo^.DrawView;
    LongWorkEnd;
    end;
   {/JO}

  function SlowDrive(C: Char): Boolean;
    begin
     if GetDriveTypeNew(C) in [dtnFloppy, dtnCDRom, dtnOptical]
       then Result := True
     else Result := False;
    end;

  begin { FindFiles }
  TotalNum := 0; {-$VIV}
  NewTimer(T, 0);
  FF := GetString(dlFilesFound);
  if FindRec.Options and ffoAdvanced <> 0 then
    begin
    DateAfter := ParseTime(AdvanceSearchData.After);
    DateBefore := ParseTime(AdvanceSearchData.Before);
    if DateBefore = 0 then
      DateBefore := $7FFFFFFF;
    SizeGreat := StoI(AdvanceSearchData.Greater);
    SizeLess := StoI(AdvanceSearchData.Less);
    if SizeLess = 0 then
      SizeLess := $7FFFFFFF;
    Attr := 0;
    if AdvanceSearchData.Attr and 1 <> 0 then
      Attr := Archive;
    if AdvanceSearchData.Attr and 2 <> 0 then
      Attr := Attr or SysFile;
    if AdvanceSearchData.Attr and 4 <> 0 then
      Attr := Attr or Hidden;
    if AdvanceSearchData.Attr and 8 <> 0 then
      Attr := Attr or ReadOnly;
    end;
  LookInArchives := (FindRec.Options and ffoInArch <> 0)
                     and (FindRec.What = ''); {JO}
  CancelSearch := False;
  FFResult := ffSeFnd;
  SearchString.What := FindRec.What;
  SearchString.Opts := FindRec.Options shr 4; //пpопускаем пеpвые 4 чекбокса
  Microed.SearchData.Line := FindRec.What;
  Microed.SearchData.What := #0;
  Microed.SearchData.Options := SearchString.Opts;
  Microed.SearchData.Scope := 0;

//используем '<>' в качестве пpизнака панели поиска
  New(Drv, Init('<>'+FindRec.Mask, Directories, Files));
  if FindRec.What <> '' then
    Drv^.AWhat := NewStr(FindRec.What);
  Drv^.AMask := NewStr(FindRec.Mask);
  Drv^.isDisposable := False;
  Pnl := nil;
  if (FindRec.Options and ffoNoSort) <> 0 then
    RereadNoSort := True;

{JO: 2-04-2006 - поиск в ветви}
  if InBranch then
    begin
    SearchDataInBranch(SourceFC);
    goto Common1;
    end;
{/JO}

  if FindRec.Where <> 1 then {JO: если не ищем в выделенных каталогах}
    { Flash >>> }
    begin
    lGetDir(0, FN);
    CheckPathInMask;
    end;
    { Flash <<< }

  case FindRec.Where of
    0:
      SearchData(FN);
{JO: 31-03-2006 - поиск в выделенном}
    1:
      if (SourceFC <> nil) and (SourceFC^.Count > 0) then
        SearchDataInBranch(SourceFC);
{/JO}
    2:
      SearchData(Copy(FN, 1, 3));
    3:
      for C := 'A' to 'Z' do
        if ValidDrive(C) then
          if  (C = UpCase(FN[1])) or (not SlowDrive(C)) then
            SearchData(C+':\');
  end {case};
Common1:
  if MaxAvail <= LowMemSize then
    Drv^.NoMemory := True;
  if Pnl <> nil then
    begin
    if Pnl <> nil then
      PFilePanel(Pnl)^.ChangeLocked := False;
    Drv^.isDisposable := True;
    CurFileRec :=
      PFilePanel(Pnl)^.Files^.At(PFilePanel(Pnl)^.ScrollBar^.Value);
    {JO}
    Dispose(PFilePanel(Pnl)^.Files, Done);
    PFilePanel(Pnl)^.Files := New(PFilesCollection, Init($10, $10));
    Application^.Redraw;
    if (FindRec.Options and ffoNoSort) = 0 then
      begin
      PFilePanel(Pnl)^.RereadDir;
      {JO: позиционируем фокус на файле, на котором он был
       до перечитывания панели}
      with PFilePanel(Pnl)^.Files^ do
        for CurP := 0 to Count-1 do
          if  (PFileRec(At(CurP))^.FlName[True] =
               CurFileRec^.FlName[True]) and
              (PFileRec(At(CurP))^.Owner^ = CurFileRec^.Owner^)
          then
            PFilePanel(Pnl)^.ScrollBar^.SetValue(CurP);
      {/JO}
      end
    else
      PFilePanel(Pnl)^.ReadDirectory;
    end;
  RereadNoSort := False; //!!!
  if  (Files^.Count = 0) or (Pnl = nil) then
    begin
    Dispose(Drv, Done);
    Drv := nil;
    FFResult := FFResult and ffSeNotFnd; {-$VOL}
    end
    // JO: здесь сортировка не нужна, т.к. она делается в TFindDrive.GetDirectory
    //     и в результате мы получаем сортировку дважды
    {else Files^.Sort}
    ;
  FindFiles := FFResult;
  end { FindFiles };
{-DataCompBoy-}

{AK155}
function InsertFile(S: String; var DC: PSortedCollection;
    var FC: PFilesCollection): Boolean;
  var
    Dir: String;
    Nm: String;
    Xt: String;
    SR: lSearchRec;
    I: LongInt;
    Dr: PString;
    Duped: Boolean;
  begin
  InsertFile := False;
  if  (S = '')
    or (S = '*') or (S = '*.*')
    {JO: иначе в список попадёт весь каталог}
    then
    Exit;
  ClrIO;
  S := lFExpand(S);
  lFindFirst(S, AnyFileDir, SR); {JO}
  if  (DosError = 0) and not Abort then
    begin
    InsertFile := True;
    lFSplit(S, Dir, Nm, Xt);
    if DC = nil then
      //DC := New(PStringCollection, Init($10, $10, False));
      DC := New(PStringCollection);
      // fixme: by unxed
    I := DC^.IndexOf(@Dir);
    if I < 0 then
      begin
      MakeSlash(Dir);
      Dr := NewStr(Dir);
      DC^.Insert(Dr);
      end
    else
      Dr := DC^.At(I);
    end;
  while (DosError = 0) and not Abort do
    begin
    if {$IFNDEF OS2}(SR.SR.Name[1] <> '.') and (SR.FullName <> '.')
         and (SR.FullName <> '..') {$ELSE}(SR.SR.Name <> '..')
      and (SR.SR.Name <> '.') {$ENDIF}
    then
      begin
      if FC = nil then
        New(FC, Init($10, $10));
      Duped := False;
      for I := 0 to FC^.Count-1 do
        if  (SR.FullName = PFileRec(FC^.At(I))^.FlName[True])
          and (Dr^ = PFileRec(FC^.At(I))^.Owner^)
        then
          Duped := True;
      if not Duped then
        FC^.AtInsert(FC^.Count, NewFileRec(SR.FullName,
            {$IFDEF DualName}
            SR.SR.Name,
            {$ENDIF}
            SR.FullSize,
            SR.SR.Time,
            SR.SR.CreationTime,
            SR.SR.LastAccessTime,
            SR.SR.Attr,
            Dr
            )
          )
      else
        Break;
      end;
    ClrIO;
    lFindNext(SR);
    end;
  lFindClose(SR);
  {$IFDEF OS2}
  if DosError = 49 then
    MessageBox(GetString(dl_CodePage_FS_Error), nil, mfError+mfOKButton);
  {$ENDIF}
  end { InsertFile };

{-DataCompBoy-, AK155}
function ReadList(const AName: String; var DC: PSortedCollection;
    var FC: PFilesCollection): Boolean;
  var
    F: PTextReader;
    S: String;
    I, J: LongInt;
    P: PFileRec;
    D: DateTime;
    WW: PView;
  begin
  ReadList := False;

  F := New(PTextReader, Init(AName));
  if F = nil then
    Exit;

  FC := nil;
  DC := nil;
  WW := ReadingListMsg;
  while not F^.Eof and (IOResult = 0) and not Abort do
    begin
    UpdateWriteView(WW);
    S := F^.GetStr;
    if S <> '' then
      case S[1] of
        ' ', #9, '>':
          begin {игнорируем последующие строки многострочного описания}
          end;
        '"':
          begin { длинное имя в кавычках }
          System.Delete(S, 1, 1);
          SetLength(S, PosChar('"', S)-1);
          InsertFile(S, DC, FC);
          end;
        else
          {попробуем целиком, или без первого символа (BSO),
              а если не вышло - то до первого пробела или Tab}
          begin
          if not InsertFile(S, DC, FC) and
              ( (S[1] <> '~') and
              InsertFile(Copy(S, 2, Length(S)-1), DC, FC)) {BSO}
            then
            begin
            I := PosChar(' ', S);
            J := PosChar(#9, S);
            if  (I = 0) or ((J <> 0) and (J < I)) then
              I := J;
            if I <> 0 then
              InsertFile(Copy(S, 1, I-1), DC, FC);
            end;
          end;
      end {case};
    end;

  Dispose(F, Done);
  WW^.Free;
  if  (FC = nil) or (FC^.Count = 0) or (DC = nil) or (DC^.Count = 0)
  then
    begin
    if FC <> nil then
      Dispose(FC, Done);
    if DC <> nil then
      Dispose(DC, Done);
    MessageBox(^C+GetString(dlNoFilesFound), nil,
       mfInformation+mfOKButton);
    Exit
    end;
  ReadList := True;
  end { ReadList };
{-DataCompBoy-}

{--------------------------------------------------------}
{-------------------------------------------- TFindDrive }
{--------------------------------------------------------}

{JO}
function GetArcName(S: String): String;
  var
    C: Char;
  begin
  if Length(S) < 2 then
    begin
    Result := '';
    Exit;
    end;
  C := S[2];
  S[2] := ';'; {JO: реально не важно на что меняем, лишь бы не ':'}
  S := Copy(S, 1, PosChar(':', S)-1);
  if Length(S) > 1 then
    S[2] := C;
  Result := S;
  end;
{/JO}

{-DataCompBoy-}
constructor TFindDrive.Init;
  var
    S: PString;
    SS: String;
    I: LongInt;
  begin
  TObject.Init;
  ListFile := nil;
  isDisposable := True;
  DriveType := dtFind;
  ColAllowed := PanelFileColAllowed[pcList];
  CurDir := AName;
  Dirs := PSortedCollection(ADirs);
  Files := AFiles;
  lGetDir(0, SS);
  ClrIO;
  S := NewStr(SS);
  I := Dirs^.IndexOf(S);
  if I >= 0 then
    begin
    DisposeStr(S);
    S := Dirs^.At(I);
    end
  else
    Dirs^.Insert(S);
  NewUpFile;
  UpFile^.Owner := S;
  end { TFindDrive.Init };
{-DataCompBoy-}

procedure TFindDrive.NewUpFile;
  begin
  UpFile := NewFileRec( {$IFDEF DualName}'..', {$ENDIF}'..', 0, 0, 0, 0,
       Directory, nil); {DataCompBoy}
  end;

constructor TFindDrive.Load(var S: TStream);
  var
    I: LongInt;
    Q, Q2: LongInt;
  begin
  inherited Load(S);
  isDisposable := True;
  S.Read(DriveType, SizeOf(DriveType));
  AMask := S.ReadStr;
  AWhat := S.ReadStr;
  Dirs := PSortedCollection(S.Get);
  S.Read(I, SizeOf(I));
  if I < 0 then
    I := 0;
  if Dirs = nil then
    Fail;

  S.Read(Q, SizeOf(Q));
  if Q >= 0 then
    begin
    Files := New(PFilesCollection, Init(Q+1, $10));
    Files^.SortMode := psmLongName;
    Files^.Duplicates := TypeOf(Self) = TypeOf(TFindDrive);
    Files^.Panel := Self.Panel;
    for Q2 := 0 to Q do
      begin
      Files^.AtInsert(Q2, LoadFileRecOwn(S, Dirs));
      {     if PFileRec(Files^.At(Q2))^.Owner = nil then
        PFileRec(Files^.At(Q2))^.Owner := NewStr('---:---'); }
      end;
    end;

  ListFile := S.ReadStr;
  NewUpFile;
  UpFile^.Owner := Dirs^.At(I);
  end { TFindDrive.Load };

procedure TFindDrive.Store(var S: TStream);
  var
    I: LongInt;
    Q: LongInt;
  begin
  inherited Store(S);
  S.Write(DriveType, SizeOf(DriveType));
  S.WriteStr(AMask);
  S.WriteStr(AWhat);
  S.Put(Dirs);
  I := Dirs^.IndexOf(UpFile^.Owner);
  S.Write(I, SizeOf(I));
  if Files = nil then
    Q := -1 {John_SW  22-03-2003}
  else
    Q := Files^.Count-1;
  S.Write(Q, SizeOf(Q));

  for I := 0 to Q do
    StoreFileRecOwn(S, Files^.At(I), Dirs);
  S.WriteStr(ListFile);
  end;

destructor TFindDrive.Done;
  begin
  DisposeStr(AMask);
  DisposeStr(AWhat);
  if Files <> nil then
    Dispose(Files, Done);
  Files := nil;
  if UpFile <> nil then
    DelFileRec(UpFile);
  if Dirs <> nil then
    Dispose(Dirs, Done);
  Dirs := nil;
  DisposeStr(ListFile);
  inherited Done;
  end;

procedure TFindDrive.MakeDir;
  begin
  end;

procedure TFindDrive.lChDir;
  begin
  end;

{-DataCompBoy-}
function TFindDrive.GetDirectory;
  var
    AFiles: PFilesCollection;
    SR: lSearchRec;
    P, CR: PFileRec;
    D: DateTime;
    I, N: LongInt;
    DrNm: String;
    AllFiles: Boolean;
    FreeSpc: TSize;
    PD: PString;
    OW: Pointer;
    S: String;
  begin
  {$IFDEF DualName}
  uLfn := PFilePanelRoot(Panel)^.PanSetup^.Show.
    ColumnsMask and psLFN_InColumns <> 0;
  {$ENDIF}

  DosError := 0;
  Abort := False;
  AllFiles := (FileMask = x_x) or (FileMask = '*');
  ClrIO;
  FreeSpc := 0;
  TotalInfo := 0;

  AFiles := New(PFilesCollection, Init($10, $10));
  PFilesCollection(AFiles)^.Panel := Panel;
  AFiles^.Duplicates := TypeOf(Self) = TypeOf(TFindDrive);
//  PFilesCollection(AFiles)^.SortMode := SortMode;
  S := '';
  PD := nil;
  ClrIO;
  if Files <> nil then
    for N := 0 to Files^.Count-1 do
      begin
      CR := Files^.At(N);
      if CR = nil then
        Continue;
      if  (CR^.Owner = nil)
      then
        OW := Dirs^.At(0)
      else if (S = CR^.Owner^)
      then
        OW := PD
      else if Dirs^.Search(CR^.Owner, I) then
        begin
        PD := Dirs^.At(I);
        OW := PD;
        S := PD^
        end
      else
        OW := Dirs^.At(0);

      P := CopyFileRec(CR);

      if (AllFiles or (P^.Attr and Directory <> 0) or
        InFilter(P^.FlName[uLfn], FileMask))
        and ((not Security) or (P^.Attr and Hidden = 0))
      then
        with AFiles^ do
          AtInsert(Count, P)
      else
        begin
        DelFileRec(P);
        Continue
        end;
      if P^.Size > 0 then
        TotalInfo := TotalInfo+P^.Size;
      if P^.Attr and Directory <> 0 then
        begin
        P^.Attr := P^.Attr and $7FFF;
        P^.Size := -1;
        end;
      end;

  if DriveType <> dtTemp then
    AFiles^.AtInsert(0, CopyFileRec(UpFile));

  GetDirectory := AFiles;
  end { TFindDrive.GetDirectory };

procedure TFindDrive.ChangeUp;
  var
    P: PDrive;
  begin
  if ListFile = nil then
    S := ''
  else
    S := GetName(ListFile^);
  if Panel = nil then
    Exit;
{AK155 16.05.2005 Prev = nil не бывает. Подробности см. в комментарии
в TFindDrive.ChangeRoot
  if Prev = nil then
    begin
    New(Prev, Init(0, Panel));
    if Prev = nil then
      Exit;
    GlobalMessage(evCommand, cmRereadInfo, nil);
    end;
/AK155}
  PFilePanel(Panel)^.Drive := PDrive(Prev);
  Prev^.lChDir(Prev^.CurDir);

{AK155 16.05.2005 Присвоение для ActivePanel не нужно, так как
не в активной панели не может возникнуть ChangeUp. А даже если бы
и могла, то с какой стати нужно было бы эту панель активизировать?
  if  (Prev^.DriveType = dtDisk) and
      (PView(Panel)^.GetState(sfSelected+sfActive))
  then
    ActivePanel := Panel;
/AK155}
  GlobalMessage(evCommand, cmRereadInfo, nil);
  Prev := nil;
  Dispose(PDrive(@Self), Done);
  end { TFindDrive.ChangeUp };

procedure TFindDrive.ChangeRoot;
  var
    P: PDrive;
  begin
  if DriveType = dtList then
    begin
    CurDir := '';
    Exit;
    end;
{!! AK155 16.05.2005
FindDrive может появиться на панели только в результате
InsertDrive, а в нём FindDrive обязательно получит Prev <> nil.
Так что анализ не нужен и финальная часть данной процедуры
тоже не нужна
}
//  if Prev <> nil then
    begin
    PFilePanel(Panel)^.Drive := PDrive(Prev);
    Prev^.ChangeRoot;
    GlobalMessage(evCommand, cmRereadInfo, nil);
    Prev := nil;
    Dispose(PDrive(@Self), Done);
//    Exit;
    end;
(* AK155 16.05.2005
  New(Prev, Init(0, Panel));
  if Prev = nil then
    Exit;
  GlobalMessage(evCommand, cmRereadInfo, nil);
  {Prev^.Owner := Owner;}
  PDrive(PFilePanel(Panel)^.Drive) := PDrive(Prev);
  Prev^.ChangeRoot;
  if  (PView(Panel)^.GetState(sfSelected+sfActive)) then
    ActivePanel := Panel;
  GlobalMessage(evCommand, cmRereadInfo, nil);
  Prev := nil;
/AK155 16.05.2005
*)
  end { TFindDrive.ChangeRoot };

function TFindDrive.isUp;
  begin
  isUp := True;
  end;

function TFindDrive.Disposable;
  begin
  Disposable := isDisposable;
  end;

procedure DosReread(Files: PFilesCollection; Dir: String;
                    Strict: Boolean);
  var
    i, j: LongInt;
    sr: lSearchRec;
    p: PFileRec;
    D: DateTime;
    S: String;
  begin
  if Files = nil then
    Exit;
  i := 0;
  while i < Files^.Count do
    begin
    ClrIO;
    p := Files^.At(i);
    //JO: если файл не в интересующем нас каталоге или
    //    (при Strict = False) не в его подкаталогах,
    //    то его не проверяем
    if (Dir <> '') and ((UpStrg(Dir) <>
                         UpStrg(Copy(p^.Owner^, 1, Length(Dir))))
                   or (Strict and ((UpStrg(Dir) <>
                        UpStrg(p^.Owner^))))) then
      Inc(i)
    else
    {JO: проверяем, не лежит ли файл в архиве в панели поиска}
    if not PathFoundInArc(p^.Owner^) then
      begin
      with p^ do
        lFindFirst(MakeNormName(Owner^, FlName[True]), AnyFileDir, sr);
      {JO}
      lFindClose(sr);
      if  (DosError <> 0) or Abort then
        begin
        j := 0;
        if p^.Attr and Directory <> 0 then
//JO:  удаляем всё что лежало в данном каталоге, т.к. оно
//     заведомо не существует
          while j < Files^.Count do
            if (UpStrg(MakeNormName(p^.Owner^, p^.FlName[True])+'/') // slash change by unxed
                     = UpStrg(Copy(PFileRec(Files^.At(j))^.Owner^,
                                  1, Length(MakeNormName(p^.Owner^,
                                           p^.FlName[True])+'/')))) // slash change by unxed
            then
              begin
              Files^.AtFree(j);
              if j < i then Dec(i);
              end
            else
              Inc(j);
        Files^.AtFree(i);
        end
      else
        begin
        p^.Size := sr.FullSize;
        UnpackTime(sr.sr.Time, D);
        p^.Yr := D.Year;
        with TDate4(p^.FDate) do
          begin
          Month := D.Month;
          Day := D.Day;
          Hour := D.Hour;
          Minute := D.Min;
          end;
        p^.Second := D.Sec;
        p^.Attr := sr.sr.Attr;
        {JO: чтобы менялся цвет в панели поиска и временной панели при переименовании}
        p^.TType := GetFileType(p^.FlName[True], p^.Attr);
        Inc(i);
        end;
      end
    else
      begin
      {JO: проверяем, существует ли архив, в котором лежит файл }
      S := p^.Owner^;
      if Length(S) > 1 then
        S[2] := ';';
      {JO: реально не важно, какой символ взять, лишь бы не ':'}
      S := Copy(p^.Owner^, 1, PosChar(':', S)-1);
      lFindFirst(S, AnyFileDir, sr);
      lFindClose(sr);
      if  (DosError <> 0) or Abort then
      {JO: удаляем всё что лежало в данном архиве}
        begin
        j := 0;
        while j < Files^.Count do
          if (UpStrg(S+':\') =
                        UpStrg(Copy(PFileRec(Files^.At(j))^.Owner^,
                               1, Length(S+':\'))))
            then
              begin
              Files^.AtFree(j);
              if j < i then Dec(i);
              end
            else
              Inc(j);
        end
      else
        Inc(i);
      end;
    end;
//JO: непонятно, зачем здесь нужна сортировка: порядок элементов коллекции
//    данная процедура не изменяет, так что от сортировки только лишние
//    тормоза
 {if Files^.Count > 0 then
    Files^.Sort;}
  end { DosReread };

procedure TFindDrive.RereadDirectory;
  var
    PV: PView;
    STmp: String;
    PDir: PString;
    Strict: Boolean;
  begin
  if Prev <> nil then
    Prev^.RereadDirectory(S);
  if S = #22 then Exit; //см. Archiver.MakeArchive, лок. ф-цию ArcExec
  if (S <> '')
     and (S[1] = '>') // признак того, что надо перечитать подкаталоги
  then
    begin
    Strict := False; // перечитываем указанный каталог с подкаталогами
    STmp := Copy(S, 2, MaxStringLength);
    end
  else
    begin
    STmp := S;
    if S = '' then
      Strict := False // перечитываем всю коллекцию
    else
      Strict := True; // перечитываем указанный каталог без подкаталогов
    end;
  MakeSlash(STmp);
  PDir := NewStr(STmp);
  if ((DriveType <> dtArcFind)
      or not ExistFile(GetArcName(UpFile^.Owner^)))
      and
      ((S = '') or (Dirs^.IndexOf(PDir) >= 0)) then
    begin
    if S = '' then //пеpечитывание всей ветви целиком может быть долгим
      begin
      PV := ReadingListMsg;
      ForceWriteShow(PV);
      end
    else
      PV := nil;
    DosReread(Files, STmp, Strict);
    if PV <> nil then
      Dispose(PV, Done);
    end;
  DisposeStr(PDir);
  end;

function TFindDrive.GetRealName;
  begin
  GetRealName := '';
  end;

function TFindDrive.GetInternalName;
  begin
  GetInternalName := '';
  end;

function TFindDrive.GetDir: String;
  var
    S: String;
    SX: LongInt;
  begin
  if ListFile <> nil then
    GetDir := GetString(dlListPanel)+ListFile^
  else if (CurDir = cTEMP_) then
    GetDir := CurDir
  else if (Pos('><', CurDir) = 1) then
    GetDir := GetString(dlBranch)+Copy(CurDir, 3, MaxStringLength)
  else
    begin
    S := GetString(dlFindPanel);
    SX := SizeX-Length(S)-12;
    if  (AMask <> nil) and (AWhat <> nil) and (AMask^ <> x_x)
    then
      GetDir := S+
        Cut(AMask^, ((SX div 3) shl 1))+'|'+
        Cut(AWhat^, (SX-Min(((SX div 3) shl 1), Length(AMask^))))
    else if (AWhat <> nil) and ((AMask = nil) or (AMask^ = x_x))
    then
      GetDir := S+Cut('*.*|'+AWhat^, SX) {???}
    else if AMask <> nil then
      GetDir := S+Cut(AMask^, SX)
    else
      GetDir := S+'*.*';
    end;
  end { TFindDrive.GetDir: };

{JO: 20.06.2002 - возможен просмотр файла найденного в архиве}
procedure TFindDrive.UseFile;
  var
    SS, S, S2, Q: String;
    C: Char;
    Unp: String;
    OwnArc: String;
    PathInside: String;
    AType: PARJArchive;
    I: Byte;
    RunUnp: Boolean;
    {$IFNDEF OS2}
  label TryAgain;
  {$ENDIF}
  begin
  if  (DriveType in [dtFind, dtTemp]) and (P^.Owner <> nil) and
    PathFoundInArc(P^.Owner^)
  then
    begin {просмотр файла найденного в архиве}
    TempFile := '';
    if  (Command = cmEditFile) or (Command = cmFileEdit) or
        (Command = cmIntEditFile) or (Command = cmIntFileEdit)
    then
      Exit;
    { определяем имя архиватора и путь внутри архива}
    OwnArc := P^.Owner^;
    OwnArc[2] := ';';
    {JO: реально не важно, какой символ взять, лишь бы не ':'}
    I := PosChar(':', OwnArc);
    OwnArc := Copy(P^.Owner^, 1, I-1);
    PathInside := Copy(P^.Owner^, I+1, MaxStringLength);
    if PathInside[1] = '/' then // slash change by unxed
      Delete(PathInside, 1, 1);
    { детектим тип архива}
    New(ArcFile, Init(OwnArc, stOpenRead, 512));
    if  (ArcFile = nil) or (ArcFile^.Status <> stOK) then
      begin
      FreeObject(ArcFile); {Abort := true;}
      Exit;
      end;
    SkipSFX;
    AType := DetectArchive;
    FreeObject(ArcFile);
    if AType = nil then
      Exit;

    case Command of
      cmDBFView:
        C := '=';
      cmWKZView:
        C := '>';
      cmTextView:
        C := '<';
      cmHexView:
        C := '|';
      cmIntFileView:
        C := '-';
      else {case}
        C := '+';
    end {case};
    S := ' ';
    if P^.Attr and Hidden <> 0 then
      begin
      S := '';
      {$IFNDEF OS2}
TryAgain:
      {$ENDIF}
      if ExecResource(dlgSetPassword, S) <> cmOK then
        Exit;
      { Flash >>> } {JO: взял код Flash из Arcview }
      if CheckForSpaces(S) then
        S := ' '+CnvString2(AType^.Garble)+S+' '
      else
        {$IFNDEF OS2}
       if AType^.UseLFN then
        {$ENDIF}
        S := ' '+CnvString2(AType^.Garble)+'"'+S+'"'+' '
          {$IFNDEF OS2}
      else
        begin
        MessageBox(GetString(dlSpacesInPassword), nil, mfWarning+
          mfOKButton);
        goto TryAgain;
        end
        {$ENDIF}
        ;
      { Flash <<< }
      end;
    SS := MakeNormName(PathInside, P^.FlName[True]);
    if SS[1] = '/' then // slash change by unxed
      Delete(SS, 1, 1);
    S2 := OwnArc;
    {$IFNDEF OS2}
    {
    if not AType^.UseLFN then
      S2 := lfGetShortFileName(OwnArc);
    }
    // commented by unxed
    if OwnArc[Length(OwnArc)] = '.' then
      S2 := S2+'.';
    {$ENDIF}
    S := CnvString2(AType^.Extract)+' '+S+
      CnvString2(AType^.ForceMode)+' '+
      SquashesName(OwnArc)+' '+SquashesName(SS)+' ';
    DelDoubles('  ', S);
    TempFile := C+MakeNormName(TempDir, P^.FlName[True]);
    Q := '|'+MakeNormName(P^.Owner^, P^.FlName[True]);

    S2 := Copy(TempFile, 2, MaxStringLength);

    if C in ['<', '-', '|', '+'] then
      TempFile := TempFile+Q;

    RunUnp := not ExistFile(S2) or (PackedDate(P) <> FileTime(S2));
    if RunUnp then
      begin
      Unp := CnvString2(AType^.UnPacker);
      if  (AType^.GetID = arcRAR) and (PosChar(';', Unp) > 0) then
        Unp := Copy(Unp, PosChar(';', Unp)+1, MaxStringLength);
      S := Unp+' '+S;
      lGetDir(0, DirToChange);
      LFNvp.lChDir(TempDir);
      Message(Application, evCommand, cmExecString, @S);
      LFNvp.lChDir(DirToChange);
      DirToChange := '';
      end;
    {$IFDEF DPMI32}
    if not RunUnp then
      begin
    {$ENDIF}
      TempFileSWP := {$IFDEF RecodeWhenDraw}OemToCharStr{$ENDIF}
                      (TempFile); {JO}
      TempFile := ''; {-$VOL}
      Message(Application, evCommand, cmRetrieveSwp, nil); {JO}
    {$IFDEF DPMI32}
      end
    else
      TempFile := ''; {-$VOL}
    {$ENDIF}
    end {конец просмотра файла найденного в архиве}
  else
    begin
    if  (Prev <> nil) and (Prev^.DriveType in [dtArc, dtArcFind]) then
      Prev^.UseFile(P, Command)
    else
      begin
      if P^.Owner <> nil then
        S := MakeNormName(P^.Owner^, P^.FlName[uLfn]);
      Message(Application, evCommand, Command, @S);
      end;
    end;
  end { TFindDrive.UseFile };
{/JO}

procedure NewTemp;
  begin
  if TempFiles = nil then
    New(TempFiles, Init($10, $10));
  TempFiles^.SortMode := psmLongName;
  TempFiles^.Duplicates := False;
  end;

{-DataCompBoy-}
procedure CopyToTempDrive;
  var
    Info: PView;

  procedure AddRec(P: PFileRec);
    var
      DT: DateTime;
      I, J: LongInt;
      l: Pointer;
      NewP: PFileRec;
    begin
    UpdateWriteView(Info);
    if TempFiles^.Search(P, J) then
      Exit;
    I := TempDirs^.IndexOf(P^.Owner);
    if I >= 0 then
      l := TempDirs^.At(I)
    else
      begin
      if ArchiveName <> '' then
        begin
        if  (P^.Owner^ = '') then
          l := NewStr(ArchiveName+':\')
        else
          l := NewStr(ArchiveName+':'+P^.Owner^);
        end
      else
        l := NewStr(P^.Owner^);
      TempDirs^.Insert(l);
      end;
    New(NewP);
    NewP^ := P^;
    with NewP^ do
      begin
      Owner := l; {удаляться будет вместе с TempDirs }
      Selected := False;
      UsageCount := 1;
      if Diz <> nil then
        begin { Пересоздаём собственную копию }
        New(Diz);
        Diz^.DizText := P^.Diz^.DizText;
        Diz^.Container := nil;
          {! Это очень сомнительная штука,
          которая выстрелит, если будет возможно редактирование
          описания с TEMP:. Впрочем, возможно, CalcDPath вытянет}
        end;
      end;
    TempFiles^.AtInsert(J, NewP);
(*    TempFiles^.AtInsert(J, NewFileRec(P^.FlName[True],
        {$IFDEF DualName}P^.FlName[False], {$ENDIF}
        P^.Size,
        PackedDate(P),
        PackedCreationDate(P),
        PackedLastAccDate(P),
        P^.Attr,
        l
        )
      );
*)
    end { AddRec };

  begin { CopyToTempDrive }
  if TempDirs = nil then
//    TempDirs := New(PStringCollection, Init(10, 10, False));
    // fixme: by unxed
    TempDirs := New(PStringCollection);
  if TempFiles = nil then
    NewTemp;
  Info := WriteMsg(GetString(dlPleaseStandBy));
  AFiles^.ForEach(@AddRec);
  Drives.RereadDirectory(#22);
  Dispose(Info, Done);
  end { CopyToTempDrive };
{-DataCompBoy-}

constructor TTempDrive.Init;
  var
    S: PString;
    {     I: LongInt;}
  begin
  TObject.Init;
  if TempDirs = nil then
//    TempDirs := New(PStringCollection, Init(10, 10, False));
// fixme: by unxed
    TempDirs := New(PStringCollection);
  if TempFiles = nil then
    NewTemp;
  isDisposable := True;
  Dirs := TempDirs;
  Files := TempFiles;
  CurDir := cTEMP_;
  DriveType := dtTemp;
  ColAllowed := PanelFileColAllowed[pcList];
  ListFile := nil;
  Lfnvp.lGetDir(0, FreeStr); {System.GetDir(0, FreeStr);}
  {Cat}
  ClrIO;
  S := NewStr(FreeStr);
  {I := Dirs^.IndexOf(S);
 if I >= 0 then begin DisposeStr(S); S := Dirs^.At(I); end
           else Dirs^.Insert(S);}
  NewUpFile;
  UpFile^.Owner := S; {DataCompBoy}
  end { TTempDrive.Init };

constructor TTempDrive.Load(var S: TStream);
  { var Q, Q2: LongInt;}
  begin
  (*
  TDrive.Load(S);
  S.Read(DriveType,SizeOf(DriveType));
  Dirs := PSortedCollection(S.Get);
  if Dirs = nil then Dirs := New(PStringCollection, Init(10, 10));

  S.Read(Q, SizeOf(Q));
  if Q >= 0 then begin
   Files := New(PFilesCollection, Init($10, $10));
   Files^.SortMode := psmLongName;
   Files^.Duplicates := False;
{  Files^.Owner := @Self;}
   for Q2:=0 to Q do Files^.AtInsert(Q2, LoadFileRecOwn(S, Dirs));
  end else begin
   NewTemp;
   Files := TempFiles;
  end;

  TempFiles := Files;
  TempDirs  := Dirs;
  NewUpFile;
  UpFile^.Owner := S.ReadStr;
  UpFile^.OwnerDisposible:=true; *)

  TDrive.Load(S);
  if TempDirs = nil then
//    TempDirs := New(PStringCollection, Init(10, 10, False));
// fixme: by unxed
    TempDirs := New(PStringCollection);
  if TempFiles = nil then
    NewTemp;
  isDisposable := True;
  Dirs := TempDirs;
  Files := TempFiles;
  CurDir := cTEMP_;
  DriveType := dtTemp;
  ListFile := nil;
  NewUpFile;
  UpFile^.Owner := S.ReadStr;
  end { TTempDrive.Load };

procedure TTempDrive.Store;
  var
    I: LongInt;
    Q: LongInt;
  begin
  TDrive.Store(S);
  { S.Write(DriveType,SizeOf(DriveType));
  S.Put(Dirs);

  if Files=nil then Q:=-1
               else Q:=Files^.Count - 1;
  S.Write(Q, SizeOf(Q));
  for I := 0 to Q do StoreFileRecOwn(S, Files^.At(I), Dirs);}
  S.WriteStr(UpFile^.Owner)
  end;

procedure TTempDrive.CopyFilesInto;
  begin
  CopyToTempDrive(AFiles, Own, '');
  end;

procedure TTempDrive.EraseFiles;
  procedure DoErase(P: PFileRec);
    var
      I: LongInt;
    begin
    I := 0;
    if  (P <> nil) and
        (Files^.Search(P, I))
    then
      Files^.AtFree(I);
    end;
  begin
  AFiles^.ForEach(@DoErase);
  Drives.RereadDirectory(#22);
  end;

function TTempDrive.GetRealName;
  begin
  GetRealName := cTEMP_;
  end;

function TTempDrive.GetInternalName;
  begin
  GetInternalName := '';
  end;

destructor TTempDrive.Done;
  begin
  if UpFile <> nil then
    DelFileRec(UpFile);
  TDrive.Done;
  end;

procedure TFindDrive.GetFreeSpace;
  begin
  S := '';
  end;

function TFindDrive.GetFullFlags;
  begin
  GetFullFlags := psShowSize+psShowDate+psShowTime+
    psShowCrDate+psShowCrTime+psShowLADate+psShowLATime+psShowDir;
  end;

{-DataCompBoy-}
constructor TFindDrive.InitList;
  var
    FC: PSortedCollection;
    DC: PFilesCollection;
  begin
  if not ReadList(AName, FC, DC) then
    Fail;
  Init(AName, PCollection(FC), DC);
  ListFile := NewStr(AName);
  DriveType := dtList;
  AddToDirectoryHistory(ListFile^, Integer(DriveType));
  end;
{-DataCompBoy-}

procedure TTempDrive.CopyFiles;
  var
    B: Boolean;
  begin
  if ReflectCopyDirection
  then
    RevertBar := Message(Desktop, evBroadcast, cmIsRightPanel, Own) <> nil
  else
    RevertBar := False;
  FileCopy.CopyFiles(AFiles, Own, MoveMode, 1);
  end;

{JO}
procedure TFindDrive.CopyFromArc;
  var
    I: LongInt;
    FCCur: PFilesCollection;
    FR: PFileRec;
    CurArcName: String;
    Drv: PDrive;
    ExtrDir: String;
    DT: record
      S: String;
      W: Word;
      Psw: String[30];
      end;
    DDr: Char;

  function GetArcOwn(S: String): String;
    begin
    if Length(S) < 2 then
      begin
      Result := '';
      Exit;
      end;
    S[2] := ';'; {JO: реально не важно на что меняем, лишь бы не ':'}
    Result := Copy(S, PosChar(':', S)+1, MaxStringLength);
    end;

  begin { TFindDrive.CopyFromArc }
  // JO: выводим диалог разархивирования, общий для всех архивов
  ExtrDir := '';
  DT.S := '';
  DT.Psw := '';
  DT.W := UnarchiveOpt and not 2; {JO}
  Message(Application, evCommand, cmPushFirstName, @DT.S);
  if CopyDirName <> '' then
    DT.S := CopyDirName;
  if DT.S = '' then
    GlobalMessageL(evCommand, cmPushName, hsExtract);
  if DT.S = '' then
    DT.S := HistoryStr(hsExtract, 0);
  CopyDirName := '';
  if DT.S = cTEMP_ then
    begin
    CopyToTempDrive(AFiles, Own, '');
    Exit;
    end;
  {JO}
  // пpовеpяем, находится ли диск в списке дисков, на котоpые надо
  // pазаpхивиpовать не чеpез вpеменный подкаталог (по умолчанию A: и B:)
  if  (DT.S <> '') and (Length(DT.S) >= 2) then
    begin
    if DT.S[2] = ':' then
      DDr := UpCase(DT.S[1])
    else
      DDr := #1; {любой символ не входящий в 'A'..'Z'}
    end
  else
    begin
    lGetDir(0, ExtrDir);
    DDr := UpCase(ExtrDir[1]);
    ExtrDir := '';
    end;
  if  (DDr in ['A'..'Z']) and
      (SystemData.Drives[DDr] and ossUnarcToDirectly <> 0)
  then
    DT.W := DT.W and not 8
  else
    DT.W := DT.W or 8;
  {/JO}
  if not SkipCopyDialog then
    if ExecResource(dlgExtract, DT) <> cmOK then
      Exit;
  if  ( (DT.W and 1) <> (UnarchiveOpt and 1)) or
      ( (DT.W and 4) <> (UnarchiveOpt and 4))
  then
    ConfigModified := True;
  UnarchiveOpt := DT.W and not 2;
  SkipCopyDialog := False;
  ExtrDir := DT.S;

  // JO: формируем файловые коллекции для каждого архива и разархивируем
  //     файлы из архивов
  repeat
    I := 0;
    FR := AFiles^.At(0);
    // для файлов с разными путями внутри архива запускаем архиватор отдельно,
    // иначе они будут распакованы с созданием подкаталогов, а нам это не надо
    CurArcName := UpStrg(FR^.Owner^);
    New(FCCur, Init($10, $10));
    repeat
      FR := AFiles^.At(I);
      if UpStrg(FR^.Owner^) = CurArcName then
        begin
        FCCur^.AtInsert(FCCur^.Count, FR);
        AFiles^.AtDelete(I);
        end
      else
        Inc(I);
    until I >= AFiles^.Count;
    if FCCur^.Count > 0 then
      begin
      Drv := New(PArcDrive, Init(GetArcName(CurArcName),
            GetArcName(CurArcName)));
      if Drv <> nil then
        begin
        Drv^.Panel := Panel;
        // дабы обеспечить снятие выделения в панели
        Drv^.lChDir(GetArcOwn(CurArcName));
        PArcDrive(Drv)^.Password := DT.Psw;
        PArcDrive(Drv)^.ExtractFiles(FCCur, ExtrDir, Own, DT.W);
        Dispose(Drv, Done);
        end;
      end;
    FCCur^.DeleteAll;
    Dispose(FCCur, Done);
  until AFiles^.Count = 0;
  end { TFindDrive.CopyFromArc };
{/JO}

{JO}
procedure TFindDrive.CopyFiles;
  var
    FC_Disk, FC_Arc: PFilesCollection;

    // JO: разделяем коллекцию файлов в панели поиска на две: в одну помещаем
    //     файлы, которые лежат на диске, в другую - которые лежет в архивах
  procedure SeparateCollections(FC_Comm: PFilesCollection;
      var FC_Disk, FC_Arc: PFilesCollection);
    var
      I: LongInt;
      FR: PFileRec;
    begin
    FC_Disk := nil;
    FC_Arc := nil;
    if  (FC_Comm = nil) or (FC_Comm^.Count = 0) then
      Exit;
    New(FC_Disk, Init($10, $10));
    New(FC_Arc, Init($10, $10));
    for I := 0 to FC_Comm^.Count-1 do
      begin
      FR := FC_Comm^.At(I);
      if  (FR^.Owner <> nil) and PathFoundInArc(FR^.Owner^) then
        FC_Arc^.AtInsert(FC_Arc^.Count, FR)
      else
        FC_Disk^.AtInsert(FC_Disk^.Count, FR);
      end;
    if FC_Arc^.Count = 0 then
      begin
      Dispose(FC_Arc, Done);
      FC_Arc := nil;
      end;
    if FC_Disk^.Count = 0 then
      begin
      Dispose(FC_Disk, Done);
      FC_Disk := nil;
      end;
    end { SeparateCollections };

  begin { TFindDrive.CopyFiles }
  SeparateCollections(PFilesCollection(AFiles), FC_Disk, FC_Arc);
  if FC_Disk <> nil then
    begin
    if Prev <> nil then
      Prev^.CopyFiles(FC_Disk, Own, MoveMode);
    FC_Disk^.DeleteAll;
    Dispose(FC_Disk, Done);
    end;
  if FC_Arc <> nil then
    begin
    CopyFromArc(FC_Arc, Own);
    FC_Arc^.DeleteAll;
    Dispose(FC_Arc, Done);
    end;
  end { TFindDrive.CopyFiles };
{/JO}

procedure TFindDrive.CopyFilesInto;
  begin
  end;

procedure TFindDrive.EraseFiles;
  begin
  if Prev <> nil then
    Prev^.EraseFiles(AFiles);
  end;

procedure TFindDrive.GetDirInfo;
  var
    Fl: Integer;
    Sz: TSize;
    S1, S2: String[40];

    {-DataCompBoy-}
  procedure DoCount(P: PFileRec);
    begin
    if  (P <> nil) and (P^.Attr and Directory = 0) then
      begin
      Inc(Fl);
      Sz := Sz+P^.Size;
      end;
    end;
  {-DataCompBoy-}

  begin
  B.Title := NewStr(GetString(dlDIFileFind));
  B.Dir := NewStr(GetString(dlDIFFMask)+Copy(CurDir, PosChar(':',
         CurDir)+1, MaxStringLength)); {DataCompBoy}
  Fl := 0;
  Sz := 0;
  if Files <> nil then
    Files^.ForEach(@DoCount);

  if Fl = 0 then
    B.Files := NewStr(GetString(dlDINoFiles))
  else
    begin
    if Fl = 1 then
      S1 := GetString(dlDIFile)
    else
      S1 := GetString(dlDIFiles);
    if Sz = 1 then
      S2 := GetString(dlDIByte)
    else
      S2 := GetString(dlDIBytes);
    B.Files := NewStr
          ('~'+FStr(Fl)+'~ '+S1+GetString(dlDIWith)+'~'+FStr(Sz)+'~ '+S2);
    end;

  end { TFindDrive.GetDirInfo };

procedure TFindDrive.HandleCommand(Command: Word; InfoPtr: Pointer);
  begin
  if  (Prev <> nil) and (Prev^.DriveType = dtArc) and
      ( (Command = cmSetPassword) or
        (Command = cmArcTest) or
        (Command = cmExtractTo))
  then
    Prev^.HandleCommand(Command, InfoPtr);
  end;

function TFindDrive.OpenDirectory(const Dir: String;
                                        PutDirs: Boolean): PDrive;
  begin
  OpenDirectory := nil;
  end;

{JO}
procedure TFindDrive.DrvFindFile(FC: PFilesCollection);
  var
    PInfo: PWhileView;
    FFiles: PFilesCollection;
    Directories: PCollection;
    BB: Byte;
    R: TRect;
    OldWhere: Word;
    Dlg: PDialog;
    DlgCm: Word;
    TitleStr: String;
  begin
  if DriveType <> dtArcFind then
    Dlg := PDialog(LoadResource(dlgFoundFileFind))
  else
    Dlg := PDialog(LoadResource(dlgFoundArcFileFind));
  if Dlg <> nil then
    begin
    case DriveType of
      dtFind:
        if (Pos('><', CurDir) = 1) then
         TitleStr := Dlg^.Title^ + GetString(dlInBranch)
        else
         TitleStr := Dlg^.Title^ + GetString(dlInFound);
      dtArcFind:
        if (Pos('><', CurDir) = 1) then
         TitleStr := Dlg^.Title^ + GetString(dlInBranchOfArchive)
        else
         TitleStr := Dlg^.Title^ + GetString(dlInFoundInArchive);
      dtTemp: TitleStr := Dlg^.Title^ + cTEMP_;
      dtList: TitleStr := Dlg^.Title^ + GetString(dlInList);
    end; {case}
    DisposeStr(Dlg^.Title);
    Dlg^.Title := NewStr(TitleStr);
    if DriveType <> dtArcFind then
      begin
      if FindRec.Where > 1 then FindRec.Where := 0;
      Dlg^.SetData(FindRec);
      DlgCm := Application^.ExecView(Dlg);
      Dlg^.GetData(FindRec);
      end
    else
      begin
//JO: поскольку ArcFindRec по absolute совмещена с FindRec,
//    то после вызова диалога можно использовать просто FindRec
      Dlg^.SetData(ArcFindRec);
      DlgCm := Application^.ExecView(Dlg);
      Dlg^.GetData(ArcFindRec);
      FindRec.What := '';
      OldWhere := FindRec.Where;
      FindRec.Where := 0;
      end;
    Dispose(Dlg, Done);
    if DlgCm = cmCancel then Exit;
    end
  else
    Exit;
  ConfigModified := True;
  DelLeft(FindRec.Mask);
  DelRight(FindRec.Mask);
  if FindRec.Mask = '' then
    FindRec.Mask := x_x;
  if  (Pos('*', FindRec.Mask) = 0) and
      (Pos('.', FindRec.Mask) = 0) and
      (Pos(';', FindRec.Mask) = 0) and
      (Pos('?', FindRec.Mask) = 0)
  then
    FindRec.AddChar := '*.*'
  else
    FindRec.AddChar := '';
  New(FFiles, Init($10, $10));
  FFiles^.SortMode := psmLongName;
//  Directories := New(PStringCollection, Init(30, 30, False));
  // fixme: by unxed
  Directories := New(PStringCollection);
  R.Assign(1, 1, 40, 10);
  Inc(SkyEnabled);
  New(PInfo, Init(R));
  PInfo^.Options := PInfo^.Options or ofSelectable or ofCentered;
  if FindRec.What = ''
  then
    PInfo^.Top := GetString(dlDBViewSearch)+Cut(FindRec.Mask, 50)
  else
    PInfo^.Top := GetString(dlDBViewSearch)+Cut(FindRec.Mask, 30)
      +' | '+Cut(FindRec.What, 17);
  PInfo^.Bottom := GetString(dlNoFilesFound);
  PInfo^.Write(1, GetString(dlDBViewSearchingIn));
  Desktop^.Insert(PInfo);
  LongWorkBegin;
  if FindRec.Where = 0 then
    BB := FindFiles(FFiles, Directories, FindRec, PInfo, Files, True)
  else
    BB := FindFiles(FFiles, Directories, FindRec, PInfo, FC, True);
  LongWorkEnd;
  Desktop^.Delete(PInfo);
  Dec(SkyEnabled);
  Dispose(PInfo, Done);
  if DriveType = dtArcFind then
    FindRec.Where := OldWhere;
  if  (BB and ffSeD2Lng) <> 0 then
    MessageBox(GetString(dlSE_Dir2Long), nil, mfWarning+mfOKButton);
  if  (BB and ffSeNotFnd) = BB then
    MessageBox(^C+GetString(dlNoFilesFound), nil,
       mfInformation+mfOKButton);
  end; { DrvFindFile.DrvFindFile }
{/JO}

procedure TTempDrive.GetDirInfo;
  var
    Fl: LongInt;
    Sz: TSize;
    S1, S2: String[40];

    {-DataCompBoy-}
  procedure DoCount(P: PFileRec);
    begin
    if  (P <> nil) and (P^.Attr and Directory = 0) then
      begin
      Inc(Fl);
      Sz := Sz+P^.Size;
      end;
    end;
  {-DataCompBoy-}

  begin
  B.Title := NewStr(GetString(dlDITemporary));
  B.Dir := NewStr(cTEMP_);
  Fl := 0;
  Sz := 0;

  if Files <> nil then
    Files^.ForEach(@DoCount);

  if Fl = 0 then
    B.Files := NewStr(GetString(dlDINoFiles))
  else
    begin
    if Fl = 1 then
      S1 := GetString(dlDIFile)
    else
      S1 := GetString(dlDIFiles);
    if Sz = 1 then
      S2 := GetString(dlDIByte)
    else
      S2 := GetString(dlDIBytes);
    B.Files := NewStr
          ('~'+FStr(Fl)+'~ '+S1+GetString(dlDIWith)+'~'+FStr(Sz)+'~ '+S2);
    end;
  end { TTempDrive.GetDirInfo };

procedure TTempDrive.ChangeRoot;
  begin
  end;

function TTempDrive.GetDriveLetter: Char;
  begin
  Result := chTempDrive;
  end;


procedure TFindDrive.ReadDescrptions(FilesC: PFilesCollection);
  begin
  end;

function TFindDrive.GetDriveLetter: Char;
  begin
  Result := Prev^.GetDriveLetter;
  end;

end.

