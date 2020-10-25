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
{JO, AK155: 27.11.2002 - добавили раскрытие архивов в ветвь по Ctrl-H}
{JO:  1.12.2002 - добавил поиск файлов внутри архива}
{$I STDEFINE.INC}

unit ArcView;

interface

uses
  Collect, Defines, Objects2, Streams, Views,
  FilesCol, DiskInfo,
  Drives, Commands, Archiver, FStorage
  ;

type
  PArcDrive = ^TArcDrive;
  TArcDrive = object(TDrive)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    ArcName: String; {DataCompBoy}
    VArcName: String; {JO}
    AType: PARJArchive;
    Files: PDirStorage;
    KillAfterUse: Boolean;
    FakeKillAfterUse: Boolean; {временная пустышка}
    ArcDate: LongInt;
    ArcSize: TFileSize; {сохраняются вместе}
    ForceRescan: Boolean;
    Password: String;
    constructor Init(const AName, VAName: String);
    constructor InitCol(PC: PDirStorage; const AName, VAName: String);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); virtual;
    procedure RereadDirectory(S: String); virtual; {DataCompBoy}
    procedure KillUse; virtual;
    function ReadArchive: Boolean;
    procedure lChDir(ADir: String); virtual; {DataCompBoy}
    function GetDir: String; virtual;
    function GetDirectory(
         const FileMask: String;
        var TotalInfo: TSize): PFilesCollection; virtual;
    function Exec(Prg, Cmd: String; Lst: AnsiString; B: Boolean): Boolean;
    {JO:  выделил список файлов в командной строке или путь к               }
    {     файлу-списку в отдельный параметр Lst;                            }
    {     параметр B должен быть False, если используем                     }
    {     файл-список или разархивируем одиночный файл не прибегая к списку }
    {     и True, если используем список в командной строке                 }

    procedure UseFile(P: PFileRec; Command: Word); virtual;
    {DataCompBoy}
    function MakeListFile(PC: PCollection; UseUnp: Boolean;
         var B: Boolean): AnsiString;
    {JO:  параметр UseUnp указывает, будем использовать полученный      }
    {     список файлов для распаковщика (True), или паковщика (False); }
    {     переменная В возвращает, был ли создан фписок в               }
    {     командной строке (True) или в файле-списке (False)            }

    procedure CopyFiles(AFiles: PCollection; Own: PView;
         MoveMode: Boolean); virtual;
    procedure CopyFilesInto(AFiles: PCollection; Own: PView;
         MoveMode: Boolean); virtual;
    procedure EraseFiles(AFiles: PCollection); virtual;
    {procedure  GetDown(var B; C: Word; P: PFileRec); virtual;}
    {DataCompBoy}
    function GetRealName: String; virtual;
    function GetInternalName: String; virtual;
    procedure HandleCommand(Command: Word; InfoPtr: Pointer); virtual;
    procedure MakeDir; virtual;
    function isUp: Boolean; virtual;
    procedure ChangeUp(var S: String); virtual;
    procedure ChangeRoot; virtual;
    procedure ExtractFiles(AFiles: PCollection; ExtrDir: String;
         Own: PView; Options: Byte); {DataCompBoy}
    procedure GetFreeSpace(var S: String); virtual;
    procedure GetDirInfo(var B: TDiskInfoRec); virtual;
    function GetFullFlags: Word; virtual;
    procedure GetDirLength(PF: PFileRec); virtual; {DataCompBoy}
    destructor Done; virtual;
    procedure StdMsg4;
    function OpenDirectory(const Dir: String;
                                 PutDirs: Boolean): PDrive; virtual;
    procedure DrvFindFile(FC: PFilesCollection); virtual;
    procedure ReadDescrptions(FilesC: PFilesCollection); virtual;
    function GetDriveLetter: Char; virtual;
    end;

function ArcViewer(AName, VAName: String): Boolean;
{DataCompBoy}
procedure StdMsg(MsgNo: Byte);

const
  ArcPasw: String[32] = '';

type
  PFInfo = ^TFInfo;
  TFInfo = record
    FName: String;
    USize: LongInt;
    PSize: LongInt;
    Date: LongInt;
    Attr: Byte;
    Last: Byte;
    { 0 - not last    }
    { 1 - archive end }
    { 2 - broken arc  }
    end;

implementation

uses
  VpSysLow, Eraser,
  Menus, DNApp, Messages, Dialogs, Gauge, FileCopy, Memory, Startup,
  {$IFDEF ARVID}Arvid, {$ENDIF}xTime, VideoMan, DnExec, FileFind
  , UserMenu {JO: для скрывания панелей при разархивировании }
  , archZip {JO: для CentralDirRecPresent}
  , Events {AK155 для LongWorkBegin - LongWorkEnd}
  , PDSetup, FlPanelX, fnotify, Drivers
  , Lfnvp, Files, Tree, Dos, Histries, HistList, FlPanel
  , Advance, Advance1, Advance2, ArchDet
  , archRAR, archACE
  ;

const
  LowMemSize = $4000; {Local setting}

procedure CheckSlashDot(var S: String);{piwamoto}
begin
  if ((S[Length(S)] <> '.') and (S[Length(S)-1] <> '/')) then // slash change by unxed
    {directory name '.' bugfix by piwamoto}
    While (PosChar(S[Length(S)], './') > 0) do SetLength(S, Length(S)-1); // slash change by unxed
end;

function MaxAvail: LongInt;
  begin
  MaxAvail := MemAdjust(System.MaxAvail);
  end;

procedure StdMsg(MsgNo: Byte);
  begin
  Application^.Redraw;
  case MsgNo of
    1:
      ErrMsg(dlArcMsg1);
    4:
      MessageBox(GetString(dlArcMsg4)+''''+
         {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(Cut(ArcFileName,
           40))+'''', nil, mfOKButton or mfError);
    5:
      Msg(dlArcMsg5, nil, mfOKButton or mfInformation);
    6:
      if MsgHelpCtx <> 0 then
        MessageBox(GetString(dlArcMsg6)+GetString(dlPressF1),
          nil, mfOKButton or mfError)
      else
        MessageBox(GetString(dlArcMsg6), nil, mfOKButton or mfError);
    7:
      Msg(dlArcMsg7, nil, mfOKButton or mfInformation);
  end {case};
  end;

procedure TArcDrive.StdMsg4;
  begin
  if TempFile <> '' then
    TempFile := '';
  StdMsg(4);
  Abort := True;
  end;

{-DataCompBoy-}
constructor TArcDrive.Init;
  var
    SR: lSearchRec;
    I: Integer;
    xt, Q: String;
  begin
  TObject.Init;

  I := PosChar(':', Copy(VAName, 3, MaxStringLength))+2;
  if I > 2 then
    VArcName := lFExpand(Copy(VAName, 1, I-1))
  else
    VArcName := lFExpand(VAName);

  I := PosChar(':', Copy(AName, 3, MaxStringLength))+2;
  if I > 2 then
    begin
    Q := Copy(AName, I+1, MaxStringLength);
    if Q[Length(Q)] in ['\', '/'] then
      SetLength(Q, Length(Q)-1);
    ArcName := lFExpand(Copy(AName, 1, I-1));
    end
  else
    begin
    ArcName := lFExpand(AName);
    Q := '/'; // slash change by unxed
    end;

  {lFSplit(ArcName, FreeStr, Nm, Xt);
 if Xt = '' then AddStr(ArcName, '.');}
  xt := GetExt(ArcName);
  if  ( (xt = '') or (xt = '.')) and (ArcName[Length(ArcName)] <> '.')
  then
    ArcName := ArcName+'.';
  lFindFirst(ArcName, AnyFileDir, SR); {JO}
  lFindClose(SR);
  if DosError <> 0 then
    begin
    ArcFileName := ArcName;
    VArcFileName := VArcName;
    StdMsg4;
    lFindClose(SR);
    Fail
    end;
  DriveType := dtArc;
  ColAllowed := PanelFileColAllowed[pcArc];
  if not ReadArchive or (Files = nil) then
    begin
    Done;
    Fail;
    end;
  KillAfterUse := TempFile <> '';
  TempFile := '';
  Password := '';
  {lFindClose(SR);}
  lChDir(Q);
  AddToDirectoryHistory(ArcName+':'+CurDir, Integer(DriveType));
  end { TArcDrive.Init };
{-DataCompBoy-}

{-DataCompBoy-}
constructor TArcDrive.InitCol;
  var
    SR: lSearchRec;
  begin
  TObject.Init;
  ArcName := lFExpand(AName);
  {VArcName := lFExpand(VAName);}
  lFindFirst(ArcName, AnyFileDir, SR); {JO}
  ArcDate := SR.SR.Time;
  ArcSize := SR.SR.Size;
  lFindClose(SR);
  DriveType := dtArc;
  Files := PC;
  if  (Files = nil) then
    Fail;
  KillAfterUse := TempFile <> '';
  TempFile := '';
  Password := '';
  if ExistFile(ArcName) then
    New(ArcFile, Init(ArcName, stOpenRead, ArcBufSize))
  else
    ArcFile := nil;
  ArcFileName := ArcName;
  VArcFileName := VArcName;
  if  (ArcFile = nil) or (ArcFile^.Status <> stOK) then
    begin
    StdMsg(4);
    Dispose(ArcFile, Done);
    ArcFile := nil;
    if Files <> nil then
      begin
      Dispose(Files, Done);
      Files := nil
      end;
    Fail;
    end;
  SkipSFX;
  AType := DetectArchive;
  FreeObject(ArcFile);
  end { TArcDrive.InitCol };
{-DataCompBoy-}

{-DataCompBoy-}
constructor TArcDrive.Load;
  var
    SR: lSearchRec;
  label
    Failure;
  begin
  inherited Load(S);
  S.ReadStrV(ArcName);
  {S.Read(ArcName[0],1); S.Read(ArcName[1],Length(ArcName));}
  {Cat}
  S.ReadStrV(VArcName);
  {S.Read(VArcName[0],1); S.Read(VArcName[1],Length(VArcName));}
  {/Cat}
  S.Read(FakeKillAfterUse, 1);
  {временно}
  KillAfterUse := False;
  S.ReadStrV(Password);
  {S.Read(Password[0],1); S.Read(Password[1],Length(Password));}
  S.Read(ArcDate, SizeOf(ArcDate)+SizeOf(ArcSize));
  ForceRescan := False;
  DriveType := dtArc;
  ArcFileName := ArcName;
  VArcFileName := VArcName;
  Files := PDirStorage(S.Get);
    { AK155 Данные о файлах надо прочитать из потока независио
    от того, будет ли найден сам архив и надо ли его перечитывать,
    иначе собьётся дальнейшее чтение потока }
  lFindFirst(ArcName, AnyFileDir, SR); {JO}
  lFindClose(SR);
  if DosError <> 0 then
    goto Failure;
  if  (ArcDate <> SR.SR.Time) or (ArcSize <> SR.SR.Size) then
    begin {архив изменился, надо его перечитывать заново}
    CurDir := '/'; // slash change by unxed
    ReadArchive;
    end
  else
    begin
    New(ArcFile, Init(ArcName, stOpenRead, ArcBufSize));
    if  (ArcFile = nil) or (ArcFile^.Status <> stOK) then
      begin
      Dispose(ArcFile, Done);
      ArcFile := nil;
      goto Failure;
      end;
    SkipSFX;
    AType := DetectArchive;
    FreeObject(ArcFile);
    if AType = nil then
      begin
Failure:
      StdMsg(4);
      if Files <> nil then
        Dispose(Files, Done);
      Files := nil;
      S.Read(ForceRescan, 1);
      Fail;
      end;
    end;
  S.Read(ForceRescan, 1);
  end { TArcDrive.Load };
{-DataCompBoy-}

procedure TArcDrive.KillUse;
  begin
  if Prev <> nil then
    Prev^.KillUse;
  if KillAfterUse then
    EraseTempFile(ArcName);
  end;

procedure TArcDrive.Store;
  begin
  inherited Store(S);
  S.WriteStr(@ArcName); {S.Write(ArcName[0],1 + Length(ArcName));}
  S.WriteStr(@VArcName); {S.Write(VArcName[0],1 + Length(VArcName));}
  {Cat}
  S.Write(KillAfterUse, 1);
  S.WriteStr(@Password); {S.Write(Password[0],1 + Length(Password));}
  S.Write(ArcDate, SizeOf(ArcDate)+SizeOf(ArcSize));
  S.Put(Files);
  S.Write(ForceRescan, 1);
  end;

destructor TArcDrive.Done;
  begin
  if Files <> nil then
    Dispose(Files, Done);
  Files := nil;
  if AType <> nil then
    Dispose(AType, Done);
  AType := nil;
  inherited Done;
  end;

{-DataCompBoy-}
function TArcDrive.ReadArchive;
  var
    PF: PArcFile;
    P: PWhileView;
    R: TRect;
    Ln: TFileSize;
    Cancel: Boolean;
    T: TEventTimer;
    SR: lSearchRec;
  begin
  {AK155 26-11-2002 Перечитываем архив тогда и только тогда, когда
 у него изменилась дата/время или длина }
  lFindFirst(ArcName, AnyFileDir, SR);
  lFindClose(SR);
  if  (ArcDate = SR.SR.Time) and (ArcSize = SR.SR.Size) then
    begin
    ReadArchive := True;
    Exit;
    end;
  ArcDate := SR.SR.Time;
  ArcSize := SR.SR.Size;
  {/AK155}
  CtrlBreakHit := False;
  ReadArchive := False;
  New(ArcFile, Init(ArcName, stOpenRead, ArcBufSize));
  ArcFileName := ArcName;
  VArcFileName := VArcName;
  if  (ArcFile = nil) or (ArcFile^.Status <> stOK) then
    begin
    Dispose(ArcFile, Done);
    ArcFile := nil;
    StdMsg4;
    Exit;
    end;
  if Files <> nil then
    Dispose(Files, Done);
  Files := nil;
  Files := nil;
  SkipSFX;
  if AType <> nil then
    Dispose(AType, Done);
  AType := nil; {DataCompBoy}
  AType := DetectArchive;
  if AType = nil then
    begin
    FreeObject(ArcFile);
    Exit;
    end;
  New(Files, Init);
  if Files = nil then
    Exit;
  P := nil;
  R.Assign(1, 1, 30, 10);
  {P := WriteMsg(GetString(dlArcReadArc));}
  Ln := ArcFile^.GetSize+1;
  Cancel := False;
  PReader := nil;
  Inc(SkyEnabled);
  NewTimer(T, 300);
  repeat
    if (ArcFile <> nil{see TUC2Archive.GetFile}) and TimerExpired(T)
    then
      begin
      LongWorkBegin;
      if P = nil then
        begin
        New(P, Init(R));
        PReader := P;
        P^.Top := GetString(dlArcReadArc);
        P^.Write(1, GetString(dlPercentComplete));
        Desktop^.Insert(P);
        end;
      P^.Write(2,
         Copy(Strg(#219, 25 div Trunc(Ln / (ArcFile^.GetPos+1))) +
           Strg(#177, 25),
         1, 25));
      P^.Write(3, ItoS(Files^.Files)+GetString(dlFilesFound));
      NewTimer(T, 300);
      end;
    AType^.GetFile;
    if FileInfo.Last = 0 then
      begin
      //commented by unxed
      //Replace('/', '\', FileInfo.FName);
      if FileInfo.FName[1] <> '/' then // slash change by unxed
        FileInfo.FName := '/'+FileInfo.FName; // slash change by unxed
      if FileInfo.Attr and Directory <> 0 then
        FileInfo.FName := FileInfo.FName+'/'; // slash change by unxed

      if FileInfo.FName[length(FileInfo.FName)] = '/' then // slash change by unxed
        FileInfo.Attr := FileInfo.Attr or Directory;

      {attribute "Hidden" means "with password"}
      Files^.AddFile(FileInfo.FName, FileInfo.USize,
        FileInfo.PSize, FileInfo.Date, FileInfo.Attr);
      if FileInfo.Attr and Directory <> 0
      then
        Files^.AddFile(MakeNormName(FileInfo.FName, '..'),
          FileInfo.USize, FileInfo.PSize, FileInfo.Date, 0);

      if  (P <> nil) and TimerExpired(T) then
        begin
        DispatchEvents(P, Cancel);
        if Cancel then
          begin
          StdMsg(5);
          FileInfo.Last := 1;
          end;
        end;
      end;
  until (FileInfo.Last > 0) or LowMemory or CtrlBreakHit;
  if CtrlBreakHit then
    StdMsg(5);
  CtrlBreakHit := False;
  LongWorkEnd;
  Dec(SkyEnabled);
  if P <> nil then
    P^.Free;
  FreeObject(ArcFile);
  CDir := '';
  if  (FileInfo.Last = 2) or
      ( (AType^.GetID = arcZIP) and not CentralDirRecPresent)
  then
    StdMsg(6);
  ReadArchive := True;
  if Files^.Files = 0 then
    begin
    StdMsg(7);
    ReadArchive := False;
    end;
  end { TArcDrive.ReadArchive };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TArcDrive.lChDir;
  var
    Dr: String;
    Nm: String;
    Xt: String;
  begin
  if ADir = #0 then
    Exit;
  CheckSlashDot(CurDir);
{  if IsDummyDir(ADir) then}
{piwamoto: we check for '..' only}
{'.' is a valid directory name in archive}
{.tar.gz have a lots of './path/filename'}
  if ADir = '..' then
    begin
    if CurDir <> '' then
      while (CurDir <> '')
           and (not (CurDir[Length(CurDir)] in ['\', '/']))
      do
        SetLength(CurDir, Length(CurDir)-1)
    else
      LFNvp.lChDir(GetPath(ArcName));
    Exit;
    end;
  lFSplit(ADir, Dr, Nm, Xt);
  if Xt = '..' then
    begin
    CurDir := Dr;
    while (CurDir <> '') and not (CurDir[Length(CurDir)] in ['\', '/'])
    do
      SetLength(CurDir, Length(CurDir)-1);
    end
  else
    CurDir := ADir;
  MakeNoSlash(CurDir);
  if CurDir[1]<>'/' then // slash change by unxed
    CurDir:='/'+CurDir; // slash change by unxed
  CheckSlashDot(CurDir);
  AddToDirectoryHistory(ArcName+':'+CurDir, Integer(DriveType));
  end { TArcDrive.lChDir };
{-DataCompBoy-}

{-DataCompBoy-}
function TArcDrive.GetDir;
  var
    Dr: String;
    Nm: String;
    Xt: String;
  begin
  CheckSlashDot(CurDir);
  if  (Length(CurDir) > 0) and (not (CurDir[1] in ['\', '/'])) then
    CurDir := '/'+CurDir; // slash change by unxed
  if  (Prev <> nil) and (Prev^.DriveType = dtDisk) then
    lFSplit(VArcName, Dr, Nm, Xt) {JO}
  else
    lFSplit(ArcName, Dr, Nm, Xt);
  GetDir := AType^.GetSign+Nm+Xt+CurDir;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TArcDrive.GetDirectory;
  var
    F: PFileRec;
    I, si: LongInt;
    AllFiles: Boolean;
    AFiles, FD: PFilesCollection;
    TTL, TPL: TSize;
    _USize, _PSize: TSize;
    FR: TFileRec;
    OW: Pointer;
    Dr: String;
    MemReq: LongInt;
    MAvail: LongInt;
  begin
  ReadArchive; {AK155 26-11-2002}
  AFiles := New(PFilesCollection, Init($10, $10));
  {FD := New(PFilesCollection, Init($40, $10));}
  PFilesCollection(AFiles)^.Panel := Panel;
  GetDirectory := AFiles;
  CheckSlashDot(CurDir);
  FD := New(PFilesCollection, Init($40, $10));
  TTL := 0;
  TPL := 0;
  {GetDirectory := AFiles;}AllFiles := (FileMask = x_x)
       or (FileMask = '*');
  FD^.SortMode := psmLongName; {<sort141.001>}
  Files^.ResetPointer('');
  {JO: сначала один pаз опpеделяем объём доступной памяти, а затем по ходу дела}
  {    подсчтитываем насколько тpебования памяти pастут и не пpевысили ли они  }
  {    доступный изначально объём                                              }
  MemReq := LowMemSize;
  MAvail := MaxAvail;
  while not Files^.Last and Files^.GetNextFile and (MAvail > MemReq) do
    begin
    _USize := Files^.CurFile.Size;
    _PSize := Files^.CurFile.CSize;
    if  (UpStrg(CurDir+'/') = UpStrg(Files^.LastDir)) and // slash change by unxed
        (AllFiles or InFilter(Files^.CurFile.Name, FileMask))
    then
      begin
      if Files^.CurFile.Name = '' then
        Continue;
      if Files^.CurFile.Name = '..' then
        Continue;
      with Files^.CurFile do
        begin
        F := NewFileRec(Name, {$IFDEF DualName}GetURZ(Name), {$ENDIF}
            _USize, Date, 0, 0, Attr, @CurDir);
        Inc(MemReq, SizeOf(TFileRec));
        Inc(MemReq, Length(CurDir+Name)+2);
        end;
      F^.PSize := _PSize;
      TTL := TTL+_USize;
      TPL := TPL+_PSize;
      end
    else if (UpStrg(CurDir+'/') = UpStrg(Copy(Files^.LastDir, 1, // slash change by unxed
               Length(CurDir)+1)))
    then
      begin
      Dr := Copy(Files^.LastDir, Length(CurDir)+2, MaxStringLength);
      I := PosChar('/', Dr); // slash change by unxed
      if I = 0 then
        I := Length(Dr)+1;
      SetLength(Dr, I-1);
      if Dr = '' then
        Continue;
      FillChar(FR, SizeOf(FR), 0);
      CopyShortString(Dr, FR.FlName[True]);
      FR.Attr := Directory;
      I := FD^.IndexOf(@FR);
      if I >= 0 then
        with PFileRec(FD^.At(I))^ do
          begin
          Size := Size+_USize;
          PSize := PSize+_PSize;
          TTL := TTL+_USize;
          TPL := TPL+_PSize;
          Continue;
          end;
      F := NewFileRec(Dr, {$IFDEF DualName}GetURZ(Dr), {$ENDIF}_USize,
           ArcDate, 0, 0, $80 or Directory, @CurDir);
      Inc(MemReq, SizeOf(TFileRec));
      Inc(MemReq, Length(CurDir+Dr)+2);
      F^.PSize := _PSize;
      TTL := TTL+_USize;
      TPL := TPL+_PSize;
      if FD^.Search(F, si) then
        begin
        DelFileRec(F);
        Continue;
        end
        {else FD^.Insert(F);}
      else
        FD^.AtInsert(si, F);
      end
    else
      Continue;
    if AFiles^.Search(F, si) then
      DelFileRec(F)
      {else AFiles^.Insert(F);}
    else
      AFiles^.AtInsert(si, F);
    end;

  NoMemory := MAvail <= MemReq;
  TotalInfo := TTL;

  if CurDir = '' then
    OW := @ArcName
  else
    OW := @CurDir;
  (* if TTL > MaxLongInt then F := NewFileRec('..', '..',0, ArcDate, Directory, OW) else
 begin
   F := NewFileRec({$IFNDEF OS2}'..',{$ENDIF} '..',Round(TTL), ArcDate, Directory, OW);
   F^.Attr := $8000 or F^.Attr;
 end;
 if TPL < MaxLongInt then F^.PSize := Round(TPL); *)

  F := NewFileRec('..', {$IFDEF DualName}'..', {$ENDIF} {Round}(TTL), ArcDate,
       0, 0, Directory, OW);
  F^.Attr := $8000 or F^.Attr;
  F^.PSize := {Round}(TPL);
  AFiles^.AtInsert(0, F);
  FD^.DeleteAll;
  Dispose(FD, Done);
  end { TArcDrive.GetDirectory };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TArcDrive.UseFile;
  var
    SS, S, S2, Q: String;
    C: Char;
    Unp: String;
    ATime: LongInt;
    ASize: TSize;
    AAttr: Word;
    RunUnp: Boolean;
    {$IFNDEF OS2}
  label TryAgain;
  {$ENDIF}
  begin
  TempFile := ''; {-$VOL}
  if  (Command = cmEditFile) or (Command = cmFileEdit) or
      (Command = cmIntEditFile) or (Command = cmIntFileEdit)
  then
    Exit;
  case Command of
    cmDBFView:
      C := '=';
    cmWKZView:
      C := '>';
    cmTextView, cmViewText:
      C := '<';
    cmHexView:
      C := '|';
    cmIntFileView:
      C := '-';
    else {case}
      C := '+';
  end {case};
  {if not CheckPassword(AF) then Exit;}
  S := ' ';
  if P^.Attr and Hidden <> 0 then
    begin
    S := '';
    {$IFNDEF OS2}
TryAgain:
    {$ENDIF}
    if ExecResource(dlgSetPassword, S) <> cmOK then
      Exit;
    { Flash >>> }
    if CheckForSpaces(S) then
      S := ' '+CnvString(AType^.Garble)+S+' '
    else
      {$IFNDEF OS2}
     if AType^.UseLFN then
      {$ENDIF}
      S := ' '+CnvString(AType^.Garble)+'"'+S+'"'+' '
        {$IFNDEF OS2}
    else
      begin
      MessageBox(GetString(dlSpacesInPassword), nil, mfWarning+mfOKButton);
      goto TryAgain;
      end
      {$ENDIF}
      ;
    { Flash <<< }
    end;
  SS := MakeNormName(P^.Owner^, P^.FlName[True]);
  if SS[1] = '/' then // slash change by unxed
    Delete(SS, 1, 1); {DelFC(SS);}
  {$IFNDEF OS2}
  {
  if AType^.UseLFN then
    S2 := ArcName
  else
    S2 := lfGetShortFileName(ArcName);
  }
  // commented by unxed
  if ArcName[Length(ArcName)] = '.' then
    S2 := S2+'.';
  S := CnvString(AType^.Extract)+' '+S+
    CnvString(AType^.ForceMode)+' '+
    SquashesName(S2)+' '+SquashesName(SS)+' ';
  {$ELSE}
  S := CnvString(AType^.Extract)+' '+S+
    CnvString(AType^.ForceMode)+' '+
    SquashesName(ArcName)+' '+SquashesName(SS)+' ';
  {$ENDIF}
  {   DelDoubles('  ',S);} {piwamoto: files can have 2 spaces in names}
  TempFile := C+MakeNormName(TempDir, P^.FlName[True]);
  Q := '|'+GetRealName+':'+MakeNormName(CurDir, P^.FlName[True]);

  S2 := Copy(TempFile, 2, MaxStringLength);

  if C in ['<', '-', '|', '+'] then
    TempFile := TempFile+Q;

  GetFTimeSizeAttr(S2, ATime, ASize, AAttr);
  RunUnp := not ExistFile(S2) or (PackedDate(P) <> ATime)
                or (P^.Size <> ASize);
  if RunUnp then
    begin
    Unp := CnvString(AType^.UnPacker);
    if  (AType^.GetID = arcRAR) and (PosChar(';', Unp) > 0) then
      begin
      if PRARArchive(AType)^.VersionToExtr > 20 then
        Unp := Copy(Unp, PosChar(';', Unp)+1, 255)
      else
        Unp := Copy(Unp, 1, PosChar(';', Unp)-1);
      end;
    { Flash 21-01-2004
          Директорию нужно запоминать на том диске, где находится
          временный каталог. А на том, где лежит архив
          с просматриваемым файлом, она запомнится в любом случае. }
    LFNvp.lChDir(Copy(TempDir, 1, 2));
    lGetDir(0, DirToChange);
    LFNvp.lChDir(TempDir);
    Exec(Unp, {$IFDEF RecodeWhenDraw}OemToCharStr {$ENDIF}(S), '', False);
    LFNvp.lChDir(DirToChange);
    DirToChange := '';
    end;
  {$IFDEF DPMI32}
  if not (AType^.SwapWhenExec and RunUnp) then
    begin
  {$ENDIF}
    TempFileSWP := {$IFDEF RecodeWhenDraw}OemToCharStr {$ENDIF}(TempFile);
    TempFile := ''; {-$VOL}
    Message(Application, evCommand, cmRetrieveSwp, nil); {JO}
  {$IFDEF DPMI32}
    end
  else
    TempFile := ''; {-$VOL}
  {$ENDIF}
  end { TArcDrive.UseFile };
{-DataCompBoy-}

function TArcDrive.Exec;
  var
    S: String;
    SS1: AnsiString;
    SM: Word;
    DE: Word;
    CmdLineLim, ListLineLim: AWord;
    CmdLineOK: Boolean;
    I, J: LongInt;
    {$IFDEF DPMI32}
    T: lText;
    EX: String;
    I1: Integer;
    {$ENDIF}

  procedure StdMsg8;
    var
      L: array[0..1] of LongInt;
      ST: String;
    begin
    Application^.Redraw;
    ST := S;
    Pointer(L[0]) := @ST;
    L[1] := DE;
    Msg(dlArcMsg8, @L, mfOKButton or mfError);
    end;

  {AK155 20/12/2001 Если под Win32 пытаться в отладчике прошагать
эту функцию, то получается полная блокировка клавиатуры и мыши
сразу на входе (даже с begin сойти не получается).
Этот эффект исчезает, если параметр AnsiString заменить на String.
Под OS/2 все шагается без проблем. Интересно, чей это глюк -
виндового отладчика или виндовой RTL? Хорошо, если первое. }
  begin { TArcDrive.Exec }
  Exec := True;
  S := Prg+' '+Cmd;
  {$IFDEF DPMI32}
  if AType^.SwapWhenExec then
    begin
    if B then
      begin
      CmdLineLim := 120;
      ListLineLim := CmdLineLim-Length(Prg+Cmd)-7;
      CmdLineOK := False;
      SS1 := Lst; {для перестраховки}
      I1 := 1;
      repeat
        ClrIO;
        EX := SwpDir+'$DN'+ItoS(I1)+'$.BAT';
        lAssignText(T, EX);
        FileMode := $40;
        lResetText(T);
        if IOResult <> 0 then
          Break;
        Close(T.T);
        if InOutRes = 0 then
          Inc(I1);
      until IOResult <> 0;
      ClrIO;
      lAssignText(T, EX);
      lRewriteText(T);
      repeat
        if Length(Lst) >= ListLineLim then
          begin
          for I := ListLineLim downto 1 do
            if Lst[I] = #$14 then
              begin
              SS1 := Copy(Lst, 1, I-1);
              Delete(Lst, 1, I);
              Break;
              end;
          end
        else
          begin
          SS1 := Lst;
          CmdLineOK := True;
          end;
        for J := 1 to Length(SS1) do
          if SS1[J] = #$14 then
            SS1[J] := #$20; {JO: заменяем временный символ на пробелы}
        Writeln(T.T, '@'+S+' '+SS1);
      until CmdLineOK;
      Write(T.T, '@del '+EX);
      Close(T.T);
      S := EX;
      end {if B}
    else
      if Lst <> '' then
        begin
        TempFile := SwpDir+'$DN'+ItoS(DNNumber)+'$.LST';
        S := S + ' ' + Lst;
        end;
    Message(Application, evCommand, cmExecString, @S);
    end
  else {if AType^.SwapWhenExec}
  begin
  {$ENDIF}
  DoneSysError;
  DoneEvents;
  DoneVideo;
  DoneDOSMem;
  DoneMemory;
  {AK155 Под OS/2, во-первых, PATH обычно не умещается
    в 255 символов, во-вторых, нет проблем с памятью,
    в третьих архиватор может оказаться ДОСовым.
    Так что пускай PATH просматривает cmd.exe, а мы не
    будем заниматься самодеятельностью }
  {AK155, дописано позже, чем комментарий к OS/2.
    Под Win32 тоже не следует заниматься самодеятельностью.
    Во-первых, мы отдаем консоль в каком-то не таком состоянии,
    так что консольный rar не может вводить с клавиатуры.
    Во-вторых, стОило ли работать с ansistring, чтобы потом вызвать
    Dos.Exec?}
  if B then
    begin
    {JO: разбираем ту часть командной строки, которая содержит список файлов    }
    {    на куски удобоваримой для командного процессора длины                  }
    {$IFNDEF DPMI32}
    if AType^.ShortCmdLine then
      CmdLineLim := {$IFDEF OS2}95 {$ELSE}127 {$ENDIF}
    else
      CmdLineLim := {$IFDEF OS2}1000 {$ELSE}250 {$ENDIF};
    {$ELSE}
    CmdLineLim := 95;
    {$ENDIF}
    ListLineLim := CmdLineLim-Length(Prg+Cmd)-7;
    CmdLineOK := False;
    SS1 := Lst; {для перестраховки}
    repeat
      if Length(Lst) >= ListLineLim then
        begin
        for I := ListLineLim downto 1 do
          if Lst[I] = #$14 then
            begin
            SS1 := Copy(Lst, 1, I-1);
            Delete(Lst, 1, I);
            Break;
            end;
        end
      else
        begin
        SS1 := Lst;
        CmdLineOK := True;
        end;
      for J := 1 to Length(SS1) do
        if SS1[J] = #$14 then
          SS1[J] := #$20; {JO: заменяем временный символ на пробелы}
      // DelDoubles('  ', S);{files can have 2 spaces in names}
      // AnsiDelDoubles('  ', SS1);
      {JO: AnsiExec - процедура из модуля DNExec , которая }
      {    используется вместо DOS.Exec и в качестве       }
      {    коммандлайна использует строку типа Ansistring  }
      SwapVectors;
      AnsiExec(GetEnv('COMSPEC'), '/c '+S+' '+SS1+' ');
      DE := DosError;
      ClrIO;
      SwapVectors;
    until CmdLineOK;
    end
  else
    begin
    // DelDoubles('  ', S);{files can have 2 spaces in names}
    // AnsiDelDoubles('  ', Lst);
    SwapVectors;
    AnsiExec(GetEnv('COMSPEC'), '/c '+S+' '+Lst+' ');
    DE := DosError;
    ClrIO;
    SwapVectors;
    EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.LST'); {DataCompBoy}
    end;
  InitDOSMem;
  InitMemory;
  InitVideo;
  InitEvents;
  InitSysError;
  case DE of
    0:
      Application^.Redraw;
    8:
      StdMsg(1);
    else {case}
      StdMsg8;
  end {case};
  // JO: закомментарил, т.к. теперь после разархивирования идёт копирование
  //     через временный каталог и после его удаления панель перечитывается
  { GlobalMessage(evCommand, cmPanelReread, nil);
  GlobalMessage(evCommand, cmRereadInfo, nil);}
  {$IFDEF DPMI32}
  end; {if AType^.SwapWhenExec}
  {$ENDIF}
  end { TArcDrive.Exec };

function TArcDrive.isUp;
  begin
  isUp := {CurDir = ''}True;
  end;

procedure TArcDrive.ChangeUp;
  begin
  if CurDir <> '' then
    begin
    S := GetName(CurDir);
    lChDir('..');
    Exit
    end;
  if Panel = nil then
    Exit;
  if Prev = nil then
    begin
    New(Prev, Init(0, Panel));
    if Prev = nil then
      Exit;
    {Prev^.Owner := Owner;}
    end;
  PFilePanel(Panel)^.Drive := Prev;
  if TypeOf(Prev^) = TypeOf(TDrive) then
    Prev^.lChDir(GetPath(VArcName));
  {piwamoto: VArcName is a feature, not a bug :-)}
  if  (Prev^.DriveType = dtDisk) and
      (PView(Panel)^.GetState(sfSelected+sfActive))
  then
    ActivePanel := Panel;
  GlobalMessage(evCommand, cmRereadInfo, nil);
  if  (Prev^.DriveType = dtDisk) then
    S := GetName(VArcName)
  else
    S := GetName(ArcName);
  Prev := nil;
  if KillAfterUse then
    EraseTempFile(ArcName);
  Dispose(PDrive(@Self), Done);
  end { TArcDrive.ChangeUp };

procedure TArcDrive.ChangeRoot;
  begin
  CurDir := '';
  end;

{-DataCompBoy-}
function TArcDrive.MakeListFile;
  var
    F: lText;
    PF: PFileRec;
    PA: PArcFile;
    I: Integer;
    S: AnsiString;
    S1: String;

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

  procedure PutDir(SS: String);
    var
      I: Integer;
      S1: String;
    begin
    if not (SS[Length(SS)] in ['\', '/']) then
      AddStr(SS, '/'); // slash change by unxed
    Files^.ResetPointer('');
    while not Files^.Last and Files^.GetNextFile do
      if  (SS = Copy(Files^.LastDir, 1, Length(SS)))
        and (Files^.CurFile.Name <> '')
      then
        begin
        S1 := Files^.LastDir+Files^.CurFile.Name;
        if S1[1] in ['\', '/'] then
          Delete(S1, 1, 1); {DelFC(S1);}
        if  (Copy(S1, Length(S1)-2, 3) = '\..')
               or (Copy(S1, Length(S1)-2, 3) = '/..')
        then
          SetLength(S1, Length(S1)- {2}3); {JO}
        {JO: нужен ли слэш в конце каталогов - вопрос спорный,          }
        {    но похоже его отсутствие нигде не мешает, а наличие        }
        {    вводит RAR в заблуждение, если не использовать файл-список;}
        {    в дальейшем не исключено, что для каких-то архиваторов     }
        {    потребуется сделать соотв. опцию;                          }
        {    zip работает нормально и с тем, и с другим                 }

        {JO:  используем символ #$14 для временного разделения имён файлов}
        if B then
          S := S+#$14+SquashesName(S1)
        else
          Writeln(F.T, S1);
        end;
    end { PutDir };

  begin { TArcDrive.MakeListFile }
  if UseUnp then
    B := (CnvString(AType^.ExtrListChar) = ' ')
           or (CnvString(AType^.ExtrListChar) = '')
  else
    B := (CnvString(AType^.ComprListChar) = ' ')
           or (CnvString(AType^.ComprListChar) = '');
  if B then
    S := ''
  else
    begin
    S := SwpDir+'$DN'+ItoS(DNNumber)+'$.LST';
    lAssignText(F, S);
    ClrIO;
    lRewriteText(F);
    B := IOResult <> 0;
    if B then
      S := ''
    else
      begin
      if UseUnp then
        S := CnvString(AType^.ExtrListChar)+S
      else
        S := CnvString(AType^.ComprListChar)+S;
      end;
    end;
  for I := 0 to PC^.Count-1 do
    begin
    PF := PC^.At(I);
    {JO: проверка для разархивирования из панели поиска в архивах}
    if PathFoundInArc(PF^.Owner^) then
      S1 := MakeNormName(GetArcOwn(PF^.Owner^), PF^.FlName[True])
    else
      S1 := MakeNormName(PF^.Owner^, PF^.FlName[True]);
    if S1[1] in ['\', '/'] then
      Delete(S1, 1, 1); {DelFC(S1);}

    {JO:  используем символ #$14 для временного разделения имён файлов}
    if PF^.Attr and Directory = 0
    then
      if B then
        S := S+#$14+SquashesName(S1)
      else
        Writeln(F.T, S1)
    else
      PutDir('/'+S1+'/'); // slash change by unxed
    MakeListFile := S;
    end;
  MakeListFile := S;
  if not B then
    Close(F.T);
  end { TArcDrive.MakeListFile };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TArcDrive.ExtractFiles(AFiles: PCollection; ExtrDir: String;
    Own: PView; Options: Byte);
  var
    SS, ArchiveName: AnsiString;
    S, SCr: String;
    DT: record
      S: String;
      W: Word;
      Psw: String[30];
      end;
    ExtrChar: String;
    Nm: String;
    Xt: String;
    Pswd: Boolean;
    B: Boolean;
    SCurDir: String;
    Unp: String;
    DNN: Byte;
    TempExtrDir: String;
    TempDirUsed: Boolean;
    FCT: PFilesCollection;
    FRT: PFileRec;
    OldConfirms: Word;
    PV: PView;
    Inhr: Byte;
    SR: lSearchRec;

    {$IFNDEF OS2}
  label TryAgain;
  {$ENDIF}

  procedure UnSelect(P: PFileRec);
    begin
    Message(Panel, evCommand, cmCopyUnselect, P);
    Pswd := Pswd or (P^.Attr and Hidden <> 0);
    end;

  begin { TArcDrive.ExtractFiles }
  while ExtrDir[Length(ExtrDir)] = ' ' do
    SetLength(ExtrDir, Length(ExtrDir)-1);
  if  (ExtrDir = '') or (ExtrDir = '..') then
    {$IFNDEF OS2}
    if AType^.UseLFN then
      {$ENDIF}
      lFSplit(VArcName, ExtrDir, Nm, Xt)
      {JO: для распаковки по F4 архивов, просмотренных через фильтр}
      {$IFNDEF OS2}
    {
    else
      lFSplit(lfGetShortFileName(ArcName), ExtrDir, Nm, Xt)
    }
    // commented by unxed
      {$ENDIF}
      ;

  if not (ExtrDir[Length(ExtrDir)] in ['\', '/']) then
    ExtrDir := ExtrDir+'/'; // slash change by unxed

  SCurDir := CurDir;
  SCr := '';
  if  (SCurDir <> '') then
    begin
    while (SCurDir[Length(SCurDir)] = '.') do
      SetLength(SCurDir, Length(SCurDir)-1);
    while (SCurDir <> '') and (SCurDir[1] = '/') do // slash change by unxed
      Delete(SCurDir, 1, 1);
    MakeSlash(SCurDir);
    if  (CnvString(AType^.SetPathInside) <> '') then
      begin
      SCr := ' '+ CnvString(AType^.SetPathInside)+
        SquashesName(Copy(SCurDir, 1, Length(SCurDir)-1))+' ';
      SCurDir := '';
      end;
    end;

//JO: если извлекаем без сохpанения путей, то файлы внутpи вpеменного
//    подкаталога оказываются без сохpанения стpуктуpы каталогов, котоpая
//    была внутpи аpхива
  if (Options and 1) = 0 then
    SCurDir := '';

  {JO}
  // проверяем, содержит ли каталог назначения файлы
  DosError := 0;
  lFindFirst(MakeNormName(ExtrDir, x_x), AnyFileDir, SR); {JO}
  if IsDummyDir(SR.FullName) then
    lFindNext(SR);
  if IsDummyDir(SR.FullName) then
    lFindNext(SR);
  lFindClose(SR);
  // для разархивирования на дискеты и тестирования не используем
  // временный подкаталог
  if  ( (Options and 8) = 0) or ((Options and 2) <> 0) or
      ( (DosError <> 0) and (SCurDir = ''))
  then
    begin
    TempExtrDir := ExtrDir;
    TempDirUsed := False;
    end
  else
    begin
    { даём имя временному подкаталогу в каталоге назначения}
    DNN := DNNumber;
    while True do
      begin
      TempExtrDir := ExtrDir+'$DN'+ItoS(DNN)+'$.EDR';
      ClrIO;
      if PathExist(TempExtrDir) then
        Inc(DNN)
      else
        Break;
      end;
    ClrIO;
    TempDirUsed := True;
    end;
  {/JO}

  SS := MakeListFile(AFiles, True, B);
  S := ' ';
  Pswd := False;
  AFiles^.ForEach(@Unselect);
  ExtrChar := CnvString(AType^.ExtractWP);
  if Options and 1 = 0 then
    ExtrChar := CnvString(AType^.Extract);
  if Options and 2 <> 0 then
    ExtrChar := CnvString(AType^.Test);
  if Pswd then
    begin
    if Password = '' then
      {$IFNDEF OS2}
TryAgain:
      {$ENDIF}
      if ExecResource(dlgSetPassword, Password) <> cmOK then
        Exit;
    { Flash >>> } {JO: взял код Flash из Arcview.TArcDrive.UseFile }
    if CheckForSpaces(Password) then
      S := ' '+CnvString(AType^.Garble)+Password+' '
    else
      {$IFNDEF OS2}
     if AType^.UseLFN then
      {$ENDIF}
      S := ' '+CnvString(AType^.Garble)+'"'+Password+'"'+' '
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
  {$IFNDEF OS2}
  if AType^.UseLFN then
    {$ENDIF}
    ArchiveName := SquashesName(ArcName)
      {$IFNDEF OS2}
  {
  else
    ArchiveName := SquashesName(lfGetShortFileName(ArcName))
  }
  // commented by unxed
      {$ENDIF}
      ;
  if  ( (Options and 4 <> 0) or TempDirUsed) and
      (CnvString(AType^.ForceMode) <> '')
  then
    S := S+CnvString(AType^.ForceMode)+' ';
  S := S+SCr; {установка пути внутpи аpхива}
  S := ExtrChar+' '+S+ArchiveName;
  Unp := CnvString(AType^.UnPacker);
  if  (AType^.GetID = arcRAR) and (PosChar(';', Unp) > 0) then
    begin
    if PRARArchive(AType)^.VersionToExtr > 20 then
      Unp := Copy(Unp, PosChar(';', Unp)+1, 255)
    else
      Unp := Copy(Unp, 1, PosChar(';', Unp)-1);
    end;
  Inhr := CreateDirInheritance(ExtrDir, True);
  CreateDirInheritance(TempExtrDir, False);
  //JO: если каталог назначения не создался (напpимеp, если диск доступен
  //    только на чтение), то нет смысла и вызывать аpхиватоp
  if not PathExist(TempExtrDir) then
    Exit;
  { Flash 21-01-2004
    Директорию нужно запоминать на том диске, где находится
    временный каталог. А на том, где лежит архив
    с просматриваемым файлом, она запомнится в любом случае. }
  LFNvp.lChDir(Copy(TempExtrDir,1,2));
  lGetDir(0, DirToChange);
  LFNvp.lChDir(TempExtrDir);
 {$IFDEF DPMI32}
  if AType^.SwapWhenExec and TempDirUsed then
    begin
    DirToMoveContent := TempExtrDir + '|' + SCurDir;
    if Options and 4 <> 0 then
      DirToMoveContent := '<'+ DirToMoveContent;
    end;
 {$ENDIF}
  Exec(Unp, {$IFDEF RecodeWhenDraw}OemToCharStr {$ENDIF}(S), SS, B);
  {JO}
  if not TempDirUsed then
    begin
    LFNvp.lChDir(DirToChange);
    DirToChange := '';
    ExtrDir := '>' + ExtrDir; //признак перечитывания подкаталогов в ветви
    GlobalMessage(evCommand, cmPanelReread, @ExtrDir);
    GlobalMessage(evCommand, cmRereadInfo, nil);
    Exit;
    end
  else
    begin
    { перекидываем файлы из временного подкаталога в каталог назначения}
    PV := New(PUserWindow, Init);
    Desktop^.Insert(PV);
    CopyDirContent(TempExtrDir+SCurDir, ExtrDir, True,
       (Options and 4 <> 0));
    PV^.Free;
    { удаляем временный каталог со всем, что в нём осталось}
    SetLength(TempExtrDir, Length(TempExtrDir)-1);
    S := GetPath(TempExtrDir);
    FRT := NewFileRec(GetName(TempExtrDir),
        {$IFDEF DualName}
        GetName(TempExtrDir),
        {$ENDIF}
        0, 0, 0, 0, Directory,
        @S);
    New(FCT, Init(1, 1));
    FCT^.AtInsert(0, FRT);
    OldConfirms := Confirms;
    Confirms := 0;
    LFNvp.lChDir(S);
    Eraser.EraseFiles(FCT);
    LFNvp.lChDir(DirToChange);
    DirToChange := '';
    Confirms := OldConfirms;
    FCT^.DeleteAll;
    Dispose(FCT, Done);
    if Inhr > 0 then
      begin
      ExtrDir := '>' + ExtrDir; //признак перечитывания подкаталогов в ветви
      GlobalMessage(evCommand, cmPanelReread, @ExtrDir);
      GlobalMessage(evCommand, cmRereadInfo, nil);
      end;
    end;
  {/JO}
  end { TArcDrive.ExtractFiles };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TArcDrive.CopyFiles;
  var
    DT: record
      S: String;
      W: Word;
      Psw: String[30];
      end;
    ExtrDir: String;
    DDr: Char;
  begin
  NotifySuspend; {<fnotify.001>}
  ExtrDir := '';
  DT.S := '';
  DT.Psw := Password;
  DT.W := UnarchiveOpt and not 2; {JO}
  Message(Application, evCommand, cmPushFirstName, @DT.S);
  if CopyDirName <> '' then
    DT.S := CopyDirName;
  {if DT.S = cTEMP_ then DT.S := '';}
  if DT.S = '' then
    GlobalMessageL(evCommand, cmPushName, hsExtract);
  if DT.S = '' then
    DT.S := HistoryStr(hsExtract, 0);
  {if DT.S = cTEMP_ then DT.S := '';}
  CopyDirName := '';
  if DT.S = cTEMP_ then
    begin
    CopyToTempDrive(AFiles, Own, ArcName);
    NotifyResume;
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
      begin
      NotifyResume;
      Exit;
      end;
  {JO}
  if  ( (DT.W and 1) <> (UnarchiveOpt and 1)) or
      ( (DT.W and 4) <> (UnarchiveOpt and 4))
  then
    ConfigModified := True;
  UnarchiveOpt := (DT.W and not 2) or 8;
  {/JO}
  SkipCopyDialog := False;
  ExtrDir := DT.S;
  Password := DT.Psw;
  ExtractFiles(AFiles, ExtrDir, Own, DT.W);
  NotifyResume;
  end { TArcDrive.CopyFiles };
{-DataCompBoy-}

procedure TArcDrive.MakeDir;
  begin
  end;

{-DataCompBoy-}
procedure TArcDrive.EraseFiles;
  var
    SS, S: AnsiString;
    PF: PFileRec;
    J, I: Word;
    O: PView;
    P: PString;
    B: Boolean;
  begin
  if AFiles^.Count = 0 then
    Exit;
  if AFiles^.Count = 1 then
    begin
    PF := AFiles^.At(0);
    S := GetString(dlEraseConfirm1)+PF^.FlName[True]+' ?';
    end
  else
    S := GetString(dlEraseConfirms1);
  I := MessageBox(S, nil, mfConfirmation+mfYesButton+mfNoButton
      {+mfFastButton});
  if  (I <> cmYes) then
    Exit;
  if AFiles^.Count > 1 then
    begin
    S := GetString(dlEraseConfirm2)+ItoS(AFiles^.Count)
        +' '+GetString(dlDIFiles)+' ?';
    J := MessageBox(S, nil, mfConfirmation+mfYesButton+mfNoButton);
    if  (J <> cmYes) then
      Exit;
    end;
  SS := MakeListFile(AFiles, False, B);
  {$IFNDEF OS2}
  if AType^.UseLFN then
    {$ENDIF}
    S := CnvString(AType^.Delete)+' '+SquashesName(ArcName)
      {$IFNDEF OS2}
  {
  else
    S := CnvString(AType^.Delete)+' '+lfGetShortFileName(ArcName)
  }
  // commented by unxed
      {$ENDIF}
      ;

  ForceRescan := True;
  Exec(CnvString(AType^.Packer), S, SS, B);
  ForceRescan := False;
  O := Panel;
  if not ReadArchive then
    begin
    CurDir := '';
    S := Cut(ArcName, 40);
    MessageBox(GetString(dlArcMsg4)+S, nil, mfError+mfOKButton);
    ChangeUp(CurDir);
    PFilePanel(O)^.ReadDirectory;
    end
  else
    PFilePanel(O)^.RereadDir;
  end { TArcDrive.EraseFiles };
{-DataCompBoy-}

function TArcDrive.GetRealName;
  var
    S: String;
  begin
  S := GetDir;
  GetRealName := Copy(S, 1, PosChar(':', S))+ArcName;
  end;

function TArcDrive.GetInternalName;
  var
    IntPath: String;
  begin
  IntPath := GetDir;
  if PosChar('/', IntPath) > 0 then // slash change by unxed
    GetInternalName := Copy(IntPath, PosChar('/', IntPath), 255) // slash change by unxed
  else
    GetInternalName := '';
  end;

procedure TArcDrive.CopyFilesInto;
  begin
  ForceRescan := True;
  ArchiveFiles(GetRealName, AFiles, MoveMode, nil);
  ForceRescan := False;
  end;

procedure TArcDrive.HandleCommand;
  var
    C: PCollection absolute InfoPtr;

  procedure SetPassword;
    var
      S: String;
    begin
    S := Password;
    if ExecResource(dlgSetPassword, S) <> cmOK then
      Exit;
    Password := S;
    end;

  procedure TestFiles;
    begin
    ExtractFiles(C, '', Panel, 2);
    end;

  procedure Extract;
    var
      CDir: String;
      Opts: Byte;
    begin
    // JO: пpовеpяем, находится ли диск в списке дисков, на котоpые надо
    //     pазаpхивиpовать не чеpез вpеменный подкаталог (по умолчанию A: и B:)
    lGetDir(0, CDir);
    if  (UpCase(CDir[1]) in ['A'..'Z']) and
        (SystemData.Drives[UpCase(CDir[1])] and ossUnarcToDirectly <> 0)
    then
      Opts := 1
    else
      Opts := 9;
    ExtractFiles(C, '', Panel, Opts);
    end;

  var
    PDr: PDrive;
    S: String;
    O: PView;

  begin { TArcDrive.HandleCommand }
  case Command of
    cmSetPassword:
      SetPassword;
    cmArcTest:
      TestFiles;
    cmExtractTo:
      Extract;
    cmMakeForced:
      ForceRescan := True;
    cmRereadForced:
      if ForceRescan then
        begin
        ForceRescan := False;
        {$IFNDEF MINARCH}
        if (AType^.GetID = arcUC2) or
           (AType^.GetID = arcAIN) or
           (AType^.GetID = arc7Z) then
          begin
          PFilePanel(Panel)^.ForceReading := True;
          end;
        {$ENDIF}
        O := Panel;
        if not ReadArchive then
          begin
          CurDir := '';
          ChangeUp(CurDir);
          end;
        PFilePanel(O)^.RereadDir;
        end;
  end {case};
  end { TArcDrive.HandleCommand };

procedure TArcDrive.GetFreeSpace;
  begin
  S := '';
  end;

function ArcViewer;
  var
    P: PDrive;
    E: TEvent;
    Xt: String;
    I: Byte;
    PathInside: String;
  begin
  {JO: дабы перейти к найденному файлу в архиве из панели поиска}
  PathInside := AName;
  if PathInside[2] = ':' then
    PathInside[2] := ';'; {JO: меняем двоеточие не важно на что }
  I := PosChar(':', PathInside);
  if I > 0 then
    begin
    PathInside := Copy(AName, I+1, 255);
    AName := Copy(AName, 1, I-1);
    end
  else
    PathInside := '';
  {/JO}
  ArcViewer := False;
  P := New(PArcDrive, Init(AName, VAName));
  if Abort then
    begin
    ArcViewer := True;
    Exit;
    end;
  { AK155 21-06-2002
    Если файл не прочитался в качестве архива, то он и любым другим
    спосбом не прочитается, так что ArcViewer берет ответственность
    на себя, чтобы во вьювере не продолжать бессмысленные попытки. }
  if P = nil then
    begin
    {$IFDEF ARVID}
    Xt := UpStrg(GetExt(AName));
    if  (Xt = '.TDR') or (Xt = '.AVT')
    then
      P := New(PArvidDrive, Init(AName));
    if P = nil then
      {$ENDIF}
      Exit;
    end;
  E.What := evCommand;
  E.Command := cmInsertDrive;
  E.InfoPtr := P;
  Desktop^.HandleEvent(E);
  if E.What <> evNothing then
    begin
    Dispose(P, Done);
    Exit;
    end
  else
    ArcViewer := True;
  {JO: переходим к найденному файлу в архиве}
  if PathInside <> '' then
    begin
    if Copy(PathInside, Length(PathInside)-1, 2) = '\.' then
      SetLength(PathInside, Length(PathInside)-2);
    if  (GetPath(PathInside) <> '/') then // slash change by unxed
      begin
      P^.lChDir(Copy(GetPath(PathInside), 2, 255));
      Message(Application, evCommand, cmPanelReread, nil);
      end;
    end;
  {/JO}
  end { ArcViewer };

function TArcDrive.GetFullFlags;
  begin
  GetFullFlags :=
     psShowSize+psShowDate+psShowTime+psShowPacked+psShowRatio;
  end;

procedure TArcDrive.RereadDirectory;
  begin
  if Prev <> nil then
    Prev^.RereadDirectory(S);
  end;

procedure TArcDrive.GetDirInfo;
  var
    Fl: Integer;
    PSz, USz: TSize;
{
  procedure DoCount(P: PArcFile);
    begin
    if  (P <> nil)
         and (not (P^.FName^[Length(P^.FName^)] in ['\', '/']))
    then
      begin
      Inc(Fl);
      USz := USz+P^.USize;
      PSz := PSz+P^.PSize;
      end;
    end;
}
  begin
  B.Title := NewStr(GetString(dlDICurArchive));
  B.Dir := NewStr(ArcName);

  Fl := Files^.Files;
  PSz := Files^.TotalCLength;
  USz := Files^.TotalLength;
  {
  if Files <> nil then Files^.ForEach(@DoCount);
  }
  B.Files := NewStr(GetString(dlDIArcTotalFiles)+ItoS(Fl)+'~');

  B.Total := NewStr(GetString(dlDIPackedSize)+FStr(PSz)+'~');
  B.Free := NewStr(GetString(dlDIUnpackedSize)+FStr(USz)+'~');

  if AType^.GetID = arcRAR then
    B.VolumeID := NewStr
            (GetString(dlDIVersionToExtract)+RtoS(PRARArchive(AType)^.
          VersionToExtr/10, 4, 2)+'~');
  if AType^.GetID = arcACE then
    B.VolumeID := NewStr
          (GetString(dlDIVersionToExtract)+RtoS(ACEVerToExtr/10, 4,
         2)+'~');

  end { TArcDrive.GetDirInfo };

procedure TArcDrive.GetDirLength(PF: PFileRec);
  begin
  end;

function ESC_Pressed: Boolean;
  var
    E: TEvent;
  begin
  Application^.Idle;
  GetKeyEvent(E);
  ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbESC)
  end;

function TArcDrive.OpenDirectory(const Dir: String;
                                       PutDirs: Boolean): PDrive;
  var
    PDrv: PDrive;
    Dirs: PStringCollection;
    Fils: PFilesCollection;
    FR: PFileRec;
    tmr: TEventTimer;
    _USize, _PSize: TSize;
    L: Integer;
    Root: String;
    PDir: PString;
    LDir, DrName: String;
    I: LongInt;
    PI: PView;
    MemReq: LongInt;
    MAvail: LongInt;
  begin
  LongWorkBegin;
  NewTimer(tmr, 0);
  Dirs := New(PStringCollection, Init($10, $10, False));
  PI := WriteMsg(GetString(dlReadingList));
  New(Fils, Init($10, $10));
  Fils^.SortMode := psmLongName;
  Files^.ResetPointer('');
  Root := UpStrg(CurDir)+'/'; // slash change by unxed
  l := Length(Root);
  {JO: сначала один pаз опpеделяем объём доступной памяти, а затем по ходу дела}
  {    подсчтитываем насколько тpебования памяти pастут и не пpевысили ли они  }
  {    доступный изначально объём                                              }
  MemReq := LowMemSize;
  MAvail := MaxAvail;
  while not Files^.Last and Files^.GetNextFile and (MAvail > MemReq)
  do
    begin
    if  (Root = Copy(UpStrg(Files^.LastDir), 1, L)) then
      begin
      with Files^.CurFile do
        begin
        if  (Name = '') or (Name = '..') then
          Continue;
        _USize := Size;
        _PSize := CSize;
        PDir := NewStr(Files^.LastDir);
        Inc(MemReq, Length(PDir^)+1);
        FR := NewFileRec(Name, {$IFDEF DualName}GetURZ(Name), {$ENDIF}
            _USize, Date, 0, 0, Attr, PDir);
        Inc(MemReq, SizeOf(TFileRec));
        Inc(MemReq, Length(PDir^+Name)+2);
        end;
      if Dirs^.IndexOf(PDir) = -1 then
        Dirs^.Insert(PDir);
      Fils^.AtInsert(Fils^.Count, FR);
      {JO: добавляем каталоги}
      if PutDirs and (Length(Files^.LastDir) > L) then
        begin
        LDir := Files^.LastDir;
        repeat
          SetLength(LDir, Length(LDir)-1);
          for I := Length(LDir) downto L do
            if LDir[I] = '/' then // slash change by unxed
              Break;
          DrName := Copy(LDir, I+1, MaxStringLength);
          SetLength(LDir, I);
          if  (DrName <> '') then
            begin
            PDir := NewStr(Copy(LDir, 1, I));
            Inc(MemReq, Length(PDir^)+1);
            FR := NewFileRec(DrName, {$IFDEF DualName}GetURZ(DrName),
                {$ENDIF}
                0, ArcDate, 0, 0, $80 or Directory, PDir);
            Inc(MemReq, SizeOf(TFileRec));
            Inc(MemReq, Length(PDir^+DrName)+2);
            if Dirs^.IndexOf(PDir) = -1 then
              Dirs^.Insert(PDir);
            if Fils^.Search(FR, I) then
              DelFileRec(FR)
            else
              Fils^.AtInsert(I, FR);
            end;
        until Length(LDir) <= L;
        end; {конец добавления каталогов}
      if TimerExpired(tmr) then
        begin
        NewTimer(tmr, 50);
        if ESC_Pressed then
          Break;
        end;
      end;
    end;
  PI^.Free;
//используем '><' в качестве пpизнака ветви
  PDrv := New(PFindDrive, Init('><'+Dir, Dirs, Fils));
  PDrv^.NoMemory := MAvail <= MemReq;
  OpenDirectory := PDrv;
  LongWorkEnd;
  end { TArcDrive.OpenDirectory };

procedure TArcDrive.DrvFindFile(FC: PFilesCollection);
  var
    I: LongInt;
    PDrv: PFindDrive;
    Dirs: PStringCollection;
    Fils: PFilesCollection;
    FR: PFileRec;
    tmr: TEventTimer;
    _USize, _PSize: TSize;
    L: Integer;
    Root: String;
    PDir: PString;
    LDir, DrName: String;
    DateAfter, DateBefore,
    SizeGreat, SizeLess: LongInt;
    Attr: Byte;
    PI: PView;
    MemReq: LongInt;
    MAvail: LongInt;

  begin
  NewTimer(tmr, 0);
  FindRec.AddChar := '';
//JO: поскольку ArcFindRec по absolute совмещена с FindRec,
//    то после вызова диалога можно использовать просто FindRec
  if ExecResource(dlgArcFileFind, ArcFindRec) = cmCancel then
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

  LongWorkBegin;
  Dirs := New(PStringCollection, Init($10, $10, False));
  PI := WriteMsg(^M^M^C+GetString(dlSearching)+'...');
  New(Fils, Init($10, $10));
  Fils^.SortMode := psmLongName;
  Files^.ResetPointer('');
  Root := UpStrg(CurDir)+'/'; // slash change by unxed
  L := Length(Root);
  {JO: сначала один pаз опpеделяем объём доступной памяти, а затем по ходу дела}
  {    подсчтитываем насколько тpебования памяти pастут и не пpевысили ли они  }
  {    доступный изначально объём                                              }
  MemReq := LowMemSize;
  MAvail := MaxAvail;
  while not Files^.Last and Files^.GetNextFile and (MAvail > MemReq)
  do
    begin
    if  (Root = Copy(UpStrg(Files^.LastDir), 1, L)) then
      begin
      with Files^.CurFile do
        begin
        if  ( (FindRec.Options and ffoAdvanced = 0) or
              (Date >= DateAfter) and (Date <= DateBefore)
            and (Size >= SizeGreat) and (Size <= SizeLess)
            and ((Attr = 0) or (FileInfo.Attr and Attr <> 0)))
          and ((FindRec.Options and ffoRecursive <> 0) or
              (Root = UpStrg(Files^.LastDir)))
          and (Name <> '') and (Name <> '..')
          and InFilter(Name, FindRec.Mask+FindRec.AddChar)
        then
          begin
          _USize := Size;
          _PSize := CSize;
          PDir := NewStr(Files^.LastDir);
          Inc(MemReq, Length(PDir^)+1);
          FR := NewFileRec(Name, {$IFDEF DualName}GetURZ(Name), {$ENDIF}
              _USize, Date, 0, 0, Attr, PDir);
          Inc(MemReq, SizeOf(TFileRec));
          Inc(MemReq, Length(PDir^+Name)+2);
          if Dirs^.IndexOf(PDir) = -1 then
            Dirs^.Insert(PDir);
          Fils^.AtInsert(Fils^.Count, FR);
          end;
        {JO: добавляем каталоги}
        if Length(Files^.LastDir) > L then
          begin
          LDir := Files^.LastDir;
          repeat
            SetLength(LDir, Length(LDir)-1);
            for I := Length(LDir) downto L do
              if LDir[I] = '/' then // slash change by unxed
                Break;
            DrName := Copy(LDir, I+1, MaxStringLength);
            SetLength(LDir, I);
            if  (DrName <> '')
              //JO: всё равно для каталогов дата и атрибуты показываются
              //    весьма условно, размер равен нулю, так что при расширенном
              //    поиске каталоги только мешают
              and (FindRec.Options and ffoAdvanced = 0)
              and ((FindRec.Options and ffoRecursive <> 0) or
                  (Root = UpStrg(LDir)))
              and InFilter(DrName, FindRec.Mask+FindRec.AddChar)
            then
              begin
              PDir := NewStr(Copy(LDir, 1, I));
              Inc(MemReq, Length(PDir^)+1);
              FR := NewFileRec(DrName, {$IFDEF DualName}GetURZ(DrName),
                  {$ENDIF}
                  0, ArcDate, 0, 0, $80 or Directory, PDir);
              Inc(MemReq, SizeOf(TFileRec));
              Inc(MemReq, Length(PDir^+DrName)+2);
              if Dirs^.IndexOf(PDir) = -1 then
                Dirs^.Insert(PDir);
              if Fils^.Search(FR, I) then
                DelFileRec(FR)
              else
                Fils^.AtInsert(I, FR);
              end;
          until Length(LDir) <= L;
          end; {конец добавления каталогов}
        if TimerExpired(tmr) then
          begin
          NewTimer(tmr, 50);
          if ESC_Pressed then
            Break;
          end;
        end;
      end;
    end;
  PI^.Free;
  if Fils^.Count > 0 then
    begin
//используем '<>' в качестве пpизнака панели поиска
    PDrv := New(PFindDrive, Init('<>'+FindRec.Mask,
          Dirs, Fils));
    PDrv^.AMask := NewStr(FindRec.Mask);
    PDrv^.NoMemory := MAvail <= MemReq;
    if (FindRec.Options and ffoNoSort) <> 0 then
      RereadNoSort := True;
    Message(Panel, evCommand, cmInsertDrive, PDrv);
    RereadNoSort := False; //!!!
    end
  else
    MessageBox(^C+GetString(dlNoFilesFound), nil,
                 mfInformation+mfOKButton);
  LongWorkEnd;
  end { TArcDrive.DrvFindFile };

procedure TArcDrive.ReadDescrptions(FilesC: PFilesCollection);
  begin
  end;

function TArcDrive.GetDriveLetter: Char;
  begin
  Result := ArcName[1];
  end;

end.

