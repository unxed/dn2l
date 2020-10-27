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

unit FileCopy;

interface

uses
  vpsyslo2,
  Collect, FilesCol, Defines, Views
  ;

type
  PCopyRec = ^TCopyRec;
    {2` Используется для D&D в InfoPtr информации к событию `}
  TCopyRec = record
    FC: PFilesCollection;
    Owner: PView;  // панель, из которой тащили
    Where: TPoint; // где бросили (глобальные координаты)
    end;
    {`}

const
  SkipCopyDialog: Boolean = False;
  CopyDirName: String = ''; {DataCompBoy}
  RevertBar: Boolean = False; {DataCompBoy}

procedure CopyFiles(Files: PCollection; SourcePanel: PView;
     MoveMode: Boolean; FromTemp: Byte);
function SelectDialog(Select: Boolean; var ST: String;
     var XORSelect: Boolean): Boolean;
procedure BeepAftercopy;

function CopyDialog(var CopyDir: String; var Mask: String;
    {DataCompBoy}
    var CopyOpt: Word; var CopyMode: Word;
    var CopyPrn: Boolean; {var Inheread: Byte;}
    MoveMode: Boolean; Files: PCollection;
    FromTemp: Byte; SourcePanel: PView; Link: Boolean): Boolean;
  { Если диалог завершился удачно и not CopyPrn, то CopyDir
   заведомо не пуста, притом в конце обязательно есть '\'.}

procedure CopyDirContent(Source, Destination: String;
    MoveMode, Forced: Boolean); {JO}

procedure CloseWriteStream;
  {` AK155 20.06.2007
    Эта процедура вызывается при аварийном завершении. Она нужна,
    чтобы, если завершение произошло во время записи, установить
    файлу фактически записанный размер, а не полный размер,
    который устанавливается в начале записи.
    Оставлять полный размер плохо, так как это, во-первых, неправда,
    а во-вторых, под Win NT программа может закрываться очень долго
    для большого файла, так как система зачем-то заполняет нулями
    всю недописанную часть. `}

implementation
uses
  DNApp, Startup, Memory, Messages, HistList, Commands,
  xTime, Validate, TitleSet, UserMenu, Dos, DnIni,
  {$IFDEF MODEM} {$IFDEF LINK}NavyLink, {$ENDIF} {$ENDIF}
  VpSysLow, Filediz {$IFDEF ARVID}, ArvidAvt {$ENDIF},
  fnotify, FlTl, Advance, Advance1, Advance2,
  Gauge, FileFind, VPUtils,
  DNUtil, Tree, Archiver, Drives, DiskInfo
  , ErrMess
  , FlPanelX {JO: PFilePanelRoot нужен чтобы делать недоступным }
  {    копирование описаний }
  {$IFDEF COPYTIMER}
  , CmdLine
  {$ENDIF}
  , Events {AK155 для LongWorkBegin - LongWorkEnd}
  , PDSetup, Lfnvp, Files, Streams, Drivers, Objects2, Dialogs
  ;

const
  Marked = $4000;
  Copied = $8000;
    { AK155 28/11/05.
       Биты Marked и Copied записывается в Attr файловой записи,
    если файл (каталог) надо перемещать, и когда он успешно перемещён.
    Кое-где эти быты пишутся независимо от того, copy или move, но
    в случае copy они нигде не анализируются.
    Использовались только в одном месте, для обновления файлов дерева
    и для перечитывания каталогов. Работа с файлами дерева выброшена.
       Раньше эти магические константы тихо использовалась прямо
    в цифровом виде, и смысл этого был совершенно непонятен. Хуже
    того, $8000 использовалась (flpanel) ещё и для отметки того,
    что размер каталога известен, что могло приводить к путанице.
       Теперь для размера каталога бит атрибута не используется,
    а когда длина неизвестна, просто пишется длина -1, а не 0
    (см. filescol.pas, TFileRec.Size). }

const
  cpmOverwrite = 0;
  cpmAppend = 1;
  cpmResume = 2;
  cpmSkipAll = 3;
  cpmRefresh = 4;
  cpmAskOver = 5;

  cpoCheckFree = $01;
  cpoVerify = $02;
  cpoDesc = $04;
  {$IFDEF OS2}
  cpoEAToName = $08;
  {$ENDIF}
  {$IFDEF WIN32}
  cpoAccRights = $08;
  {$ENDIF}
  cpoMove = $10;

  cpoFromTemp = $800;
  cpoCopyDesc = $400;
  cpoFitAll = $100;

var
  SeekPos: TFileSize;

type
  PDir = ^TDir;
  TDir = record
    Created: (sNone, sCreated, sErased);
    XName, Name: PString;
    end;

  PFileCopyRec = ^TFileCopyRec;
  TFileCopyRec = record
    Name: PString;
    Attr: Byte;
    Size: TSize;
    Dir: PDir;
    Owner: PFileRec;
    DIZ: PDiz;
    end;

  PCopyCollection = ^TCopyCollection;
  TCopyCollection = object(TCollection)
    procedure FreeItem(P: Pointer); virtual;
    end;

  PDirCollection = ^TDirCollection;
  TDirCollection = object(TSortedCollection)
    procedure FreeItem(P: Pointer); virtual;
    function Compare(P1, P2: Pointer): Integer; virtual;
    end;

function MemAvail: LongInt;
  begin
  MemAvail := MemAdjust(System.MemAvail);
  end;

function MaxAvail: LongInt;
  begin
  MaxAvail := MemAdjust(System.MaxAvail);
  end;

{-DataCompBoy-}
procedure TCopyCollection.FreeItem;
  begin
  if  (P <> nil) and (PFileCopyRec(P)^.Name <> nil) then
    DisposeStr(PFileCopyRec(P)^.Name);
  if P <> nil then
    Dispose(PFileCopyRec(P));
  end;
{-DataCompBoy-}

procedure TDirCollection.FreeItem;
  begin
  if P <> nil then
    with PDir(P)^ do
      begin
      DisposeStr(XName);
      DisposeStr(Name);
      Dispose(PDir(P));
      end;
  end;

function TDirCollection.Compare;
  var
    S1, S2: String;
  begin
  Compare := 1;
  if PDir(P1)^.Name <> nil then
    S1 := PDir(P1)^.Name^
  else
    S1 := '';
  if PDir(P2)^.Name <> nil then
    S2 := PDir(P2)^.Name^
  else
    S2 := '';
  if S1 < S2 then
    Compare := -1
  else if S1 = S2 then
    begin
    if PDir(P1)^.XName <> nil then
      S1 := PDir(P1)^.XName^
    else
      S1 := '';
    if PDir(P2)^.XName <> nil then
      S2 := PDir(P2)^.XName^
    else
      S2 := '';
    if S1 < S2 then
      Compare := -1
    else if S1 = S2 then
      Compare := 0
    end
  end { TDirCollection.Compare };

procedure BeepAftercopy;
  var
    C: Char;
  begin
{AK155 Зачем?  DelayTics(1); }
  for C := 'A' to 'D' do
    begin
    SysBeepEx {PlaySound}(1259, 55);
    SysBeepEx {PlaySound}(1400, 55);
    end;
  SysBeepEx {PlaySound}(1259, 110);
  end;

procedure PrepareSelectDialog(P: PDialog);
  var
    S: String;
  begin
  with P^ do
    begin
    if DirectLink[1] <> nil then
      PInputline(DirectLink[1])^.SetValidator(New(PFilterValidator,
          Init([#32..#255] - ['|', '>', '<'])));
    end;
  end;

{-DataCompBoy-}
function SelectDialog;
  var
    P: record
      S: String;
      XorSel: Boolean;
      end;
    Idx: TDlgIdx;
    R: TRect;

  begin
  with P do
    begin
    XorSel := XORSelect;
    SelectDialog := False;
    if LowMemory then
      Exit;
    if Select then
      Idx := dlgSelect
    else
      Idx := dlgUnselect;
    S := HistoryStr(hsSelectBox, 0);
    if S = '' then
      S := x_x;
    @PreExecuteDialog := @PrepareSelectDialog;
    if ExecResource(Idx, P) = cmCancel then
      Exit;
    SelectDialog := True;
    ST := S;
    XORSelect := XorSel;
    end
  end { SelectDialog };
{-DataCompBoy-}

const
  eoStart = $01;
  eoEnd = $02;
  eoAppend = $04;
  eoDir = $08;
  eoCheck = $10;
  eoResume = $20;

  ReadLen = $C000;

var
  ToDo, ToDoCopy, ToDoClusCopy: TSize;
  ToDoClusCopyTemp: TSize;
  MemBuf: PByteArray;
    { буфер копирования }
  MemBufPos, MemBufSize: Longint;
  WriteStream: lFile;

type
  pLine = ^TLine;
  TLine = object(TObject)
    { Элемент CopyQueue}
    Owner: PFileRec;
    OldName: PString;
    NewName: PString;
    Size: TSize; // длина файла
    Date: LongInt;
    len: Word; // длина блока, который будем читать
    Eof: Byte;
    Attr: Byte;
    constructor Init(var ALen: LongInt; AOwner: Pointer;
         const AOldName, ANewName: String;
        ASize: TSize; ADate: LongInt; AAttr: Byte; AEOF: ShortInt);
    procedure PrepareToWrite; virtual;
    destructor Done; virtual;
    end;

  PDirName = ^TDirName;
  TDirName = object(TObject)
    OldName, NewName: PString;
    Check: Boolean;
    CopyIt: Boolean;
    Own: PFileRec;
    Attr: Byte;
    constructor Init(const AOld, ANew: String; ACopy: Boolean;
         AnOwn: PFileRec; AnAttr: Byte);
    function DOld: String;
    function DNew: String;
    destructor Done; virtual;
    end;

  { TDirName }

constructor TDirName.Init;
  begin
  inherited Init;
  OldName := NewStr(AOld);
  NewName := NewStr(fReplace('./', '/', ANew)); // slash change by unxed
  CopyIt := ACopy;
  Own := AnOwn;
  Attr := AnAttr;
  end;

function TDirName.DNew;
  begin
  if NewName = nil then
    DNew := ''
  else
    DNew := NewName^;
  end;

function TDirName.DOld;
  begin
  if OldName = nil then
    DOld := ''
  else
    DOld := OldName^;
  end;

destructor TDirName.Done;
  begin
  DisposeStr(OldName);
  DisposeStr(NewName);
  inherited Done; {DataCompBoy}
  end;

{ TLine }

constructor TLine.Init;
  begin
  inherited Init;
  if MaxAvail < 2048 then
    Fail;
  len := ALen;
  Eof := AEOF;
  Attr := AAttr;
  Date := ADate;
  Size := ASize;
  if len <> 0 then
    begin
    PrepareToWrite;
    if len <= 0 then
      Fail;
    ALen := len;
    end
  else
    Eof := eoStart+eoEnd;
  Owner := AOwner;
  if (Eof and eoStart) <> 0 then
    begin
    OldName := NewStr(AOldName);
    NewName := NewStr(ANewName);
    end;
  end { TLine.Init };

destructor TLine.Done;
  begin
  DisposeStr(PString(NewName));
  DisposeStr(PString(OldName));
  inherited Done;
  end;

procedure TLine.PrepareToWrite;
  begin
  if MemBufPos+len > MemBufSize then
    len := MemBufSize-MemBufPos;
  end;

procedure InitMemBuf;
  var
    Limit: Longint; // in kByte
  begin
  Limit := Max(1, ((PhysMemAvail div 10)*9) div 1024);
  if SystemData.CopyLimitBuf > 0 then
    Limit := Min(Limit, SystemData.CopyLimitBuf);
  while Limit <> 0 do
    begin
    MemBuf := MemAlloc(Limit*1024);
    if MemBuf <> nil then
      begin
      MemBufSize := Limit*1024;
      MemBufPos := 0;
      Exit;
      end;
    Limit:= Limit div 2;
    end;
  end;


{ ----------------------------- File Copy ------------------------------ }

{AK155}
function AppendQuery(const S: String): Word;
  var
    D: PDialog;
    P: PStaticText;
    R: TRect;
  begin
  D := PDialog(LoadResource(dlgAppendQuery));
  D^.Options := D^.Options or ofCentered;
  R.Assign(2, 4, D^.Size.X-2, 5);
  P := New(PStaticText, Init(R, ^C+S));
  D^.Insert(P);
  AppendQuery := Desktop^.ExecView(D);
  end;

function GetPercent(N: TSize): Str12;
  begin
  if ToDo = 0 then
    Result := ' (100%)'
  else
    begin
    N := (N*100)/ToDo;
    Result := ' ('+ZtoS(N)+'%)';
    end;
  end;

var
  StopDlgData: word;

type
  POverriteDialog = ^TOverriteDialog;
  TOverriteDialog = object(TDialog)
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

procedure TOverriteDialog.HandleEvent(var Event: TEvent);
  begin
  inherited HandleEvent(Event);
  if (Event.What = evCommand) and (Event.Command = cmSave) then
     begin { кнопка "Продолжить" }
     EndModal(Event.Command);
     ClearEvent(Event);
     end;
  end;

{-DataCompBoy-}
procedure FilesCopy(Files: PCollection; SourcePanel: PView;
    const CopyDir, Mask: String; CopyMode, CopyOptions: Word;
    CopyPrn, RR: Boolean);

  var
    ReadStream: lFile;
    ReadPos: TSize;
    CopyCancel: Boolean;
    SkipAllBad: Boolean;
    ToRead, ToWrite: TSize;
    CopyStartTime, CopyElapsedTime: LongInt; {John_SW 30-06-2005}
    CopyQueue, Dirs: PCollection;
    iQueue: Integer;
    CurOldName, CurNewName: String;
    CurDate: LongInt;
    CurAttr: Byte;
    Info: PWhileView;
    R: TRect;
    Drv, InhR: Byte;
    ReD: set of Char;
    C: Char;
    SS: String[1];
    Timer: TEventTimer;
    CD_Drives: set of Char;
    AdvCopy: Boolean;
    _Tmr: TEventTimer;
    IOR: Integer;
    SSS: String;
    TargetDirName: String;
    PathBuffer: array[0..255] of Char; {целевой каталог}
    RC: LongInt;
    CondLfn: TUseLFN; {JO}

  function MkName(const Nm: String): String;
    var
      S: String;
    begin
    S := Advance2.MkName(Nm, Mask);
    if  (S[Length(S)] = '.') and (Length(S) > 1) then
      SetLength(S, Length(S)-1); {JO}
    MkName := S;
    end;

  var
    GrdClick: Boolean;
    ExAttr: Word;

  procedure SetDateAttr;
    begin
    case ExAttr shr 8 of
      $FD, $FE:
        ;
      else {case}
        SetFTime(WriteStream.F, CurDate);
    end {case};
    Close(WriteStream.F);
    case ExAttr shr 8 of
      $FF: { Set Source Attribute, if Necessary }
        if CurAttr <> Archive then
          lSetFAttr(WriteStream, CurAttr);
      $FE: { Set Dest' Attribute }
        lSetFAttr(WriteStream, ExAttr and $FF);
      $FD: { Don't Set any attribute }
        ;
      else { Set Source Attribute, Force }
        lSetFAttr(WriteStream, CurAttr);
    end {case};
    end;

  procedure Dispatch;
    var
      CancelAnswer: word;
      C: Boolean;
    procedure DE;
      begin
      GrdClick := True;
      DispatchEvents(Info, C)
      end;

    begin
    C := False;
    if CopyCancel then
      begin
      DE;
      Exit
      end;
    if TimerExpired(_Tmr) then
      begin
      DE;
      if C or CtrlBreakHit then
        begin
        StopDlgData := 0;
        CancelAnswer := ExecResource(dlgQueryAbort, StopDlgData);
        CopyCancel := CancelAnswer <> cmNo;
        end;
      CtrlBreakHit := False;
      NewTimer(_Tmr, 300);
      end;
    end { Dispatch };

  procedure ForceDispatch;
    begin
    NewTimer(_Tmr, 0);
    Dispatch;
    end;

  var
    Wrote: TSize;
    FreeDisk: TSize;

  procedure RemoveFromTemp(P: PFileRec);
    var
      I: LongInt;
    begin
    I := 0;
    if  (P <> nil) and (TempFiles <> nil) and TempFiles^.Search(P, I)
    then
      TempFiles^.AtFree(I);
    end;

  function Overwrite(const NName: String; OldS, NewS: TSize;
       OldT, NewT: LongInt): Word;
    var
      AcceptAll: Word;
      I: Word;
      D: PDialog;
      P: PView;
      R: TRect;
      S: String;
      D1, D2,
//      L1, L2: String[30];
      // removed [30] to fix build by unxed
      L1, L2: String;
      PP: array[1..4] of Pointer;
      DD: LongInt;
      DT: DateTime;
    begin
    AcceptAll := 0;
    Overwrite := cmSkip;
    UnpackTime(OldT, DT);
    MakeDate(DT.Day, DT.Month, DT.Year mod 100, DT.Hour,
       DT.Min, D2);
    L2 := FStr(OldS);
    UnpackTime(NewT, DT);
    MakeDate(DT.Day, DT.Month, DT.Year mod 100, DT.Hour,
       DT.Min, D1);
    L1 := FStr(NewS);
    if Length(L1) < Length(L2)
    then
      L1 := PredSpace(L1, Length(L2))
    else
      L2 := PredSpace(L2, Length(L1));
    D := PDialog(LoadResource(dlgOverwriteQuery));
    D^.GetExtent(R);
    R.Grow(-1, -1);
    Inc(R.A.Y);
    D^.Options := D^.Options or ofCentered;
    DelRight(D1);
    DelRight(D2);
    D1[9] := '(';
    AddStr(D1, ')');
    D2[9] := '(';
    AddStr(D2, ')');
    PP[1] := @D1;
    PP[2] := @L1;
    PP[3] := @D2;
    PP[4] := @L2;
    FormatStr(S, GetString(dlFCOver), PP);
    R.B.Y := R.A.Y+1;
    for I := 1 to Length(S) do
      if S[I] = ^M then
        Inc(R.B.Y);
    P := New(PStaticText, Init(R, ^C+GetString(dlFile)+' '+Cut(NName,
           40)+S));
    D^.Insert(P);

    MsgActive := True;
    D^.SetData(AcceptAll);
    ObjChangeType(D, TypeOf(TOverriteDialog));
    if OldS >= NewS then
      D^.DisableCommands([cmSave]);
    I := Desktop^.ExecView(D);
    D^.EnableCommands([cmSave]);
    MsgActive := False;
    NewTimer(Timer, 0);

    if I <> cmCancel then
      begin
      D^.GetData(AcceptAll);
      if  (AcceptAll and 1 = 1) then
        case I of
          cmYes:
            CopyMode := cpmOverwrite;
          cmSkip:
            CopyMode := cpmSkipAll;
          cmNo:
            CopyMode := cpmAppend;
          cmSave:
            CopyMode := cpmResume;
        end {case};
      Overwrite := I;
      end
    else
      Overwrite := cmSkip;

    Dispose(D, Done);

    CopyCancel := I = cmCancel;
    end { Overwrite };

  var
    SkipRequested: Boolean;

  var
    DOSErrorCode: Word;

  procedure RnFl(N1, N2: String);
    begin
    lChangeFileName(N1, N2);
    DosErrorCode := IOResult;
    end;

{--- start -------- Eugeny Zvyagintzev ---- 30-06-2005 -----}
   function CopyTime: String;
     var
       s1, s2, s3: String;
       M: Array [1..3] Of Pointer;
       Speed: Comp;
       Time: Comp;
       D: Longint;
    begin
    Result := '';
    if (ToWrite = 0) and (ToRead = 0) then
      Exit;
    D := GetCurMSec - CopyStartTime;
    if D - CopyElapsedTime < 1000 then
      Exit;
    CopyElapsedTime := D;
    s1 := SecToStr((CopyElapsedTime + 500) div 1000);
    Time := ((2*ToDo-ToRead-ToWrite)*CopyElapsedTime)/(ToRead+ToWrite);
    s2 := SecToStr(Trunc(Time / 1000));

    Speed := 1000*ToDo/(CopyElapsedTime+Time);
    if Speed > 1024 then
      s3 := ZtoS(Speed/1024)+' K'
    else
      s3 := ZtoS(Speed)+' ';
    M[1] := @s1; M[2] := @s2; M[3] := @s3;
    FormatStr(Result, GetString(dlCopyTimeSpeed), M);
    end;
{--- finish -------- Eugeny Zvyagintzev ---- 30-06-2005 -----}

  procedure MaxWrite;
    label DoRewrite, DoAppend, 1, 2, lbStartWrite, lbStartCheck
      , lRetryToReset;
    var
      iQueue: Integer;
      J: Word;
      P: pLine;
      A: LongInt;
      BB: TSize;
      F: lFile;
      PS: array[1..2] of Pointer;
      S1: String;
      BufCrc: Word;
      Created: Boolean;
      Opened: Integer;
      MsgResult: Integer;
      Ask: Integer;
      {$IFDEF OS2}
      {JO - для переименования файла в .LONGNAME исходного файла}
      LogName, NewDir, NewPrName, NewExt: String;
      I_LN: Byte;
      {$ENDIF}
      LongNameWarn: String[50];
      I_LW: Byte;

    procedure RewriteWriteStrem;
      {Открыть WriteStrem и установить его окончательный размер.
       Размер устанавливается для уменьшения фрагментации и
       ускорения.
         InOutres отражает результат открытия, а результат
       установки размера не анализируется.}
      begin
      lReWriteFile(WriteStream, 1);
      SysFileSetSize(FileRec(WriteStream.F).Handle, CompToFSize(P^.Size));
      end;

    procedure TryToReset;
      var
        OldAttr: Word;
      begin
      {AK155 2-02-2002
Раньше наличие файла проверялось  при помощи lResetFile, что не
вполне корректно. На CD RW под RSJ и, кажется, на некоторых сетевых дисках,
эта операция приводит к созданию файла и ложному запросу о перезаписи
файла. Более того, при отрицательном ответе созданный файл остается
(с нулевой длиной). Проверка при помощи lGetFAttr более чистая.
}
      if CopyMode <> cpmOverwrite then
        begin
        ClrIO;
        lGetFAttr(WriteStream, OldAttr);
        if  (DosError = 0) then
          begin
          lResetFile(WriteStream, 1);
          Opened := IOResult;
          Created := False;
          Exit;
          end;
        end;
      RewriteWriteStrem;
      Opened := IOResult;
      Created := Opened = 0;
      end { TryToReset };

    procedure DoSkip;
      begin
      while (iQueue < CopyQueue^.Count-1)
        and (pLine(CopyQueue^.At(iQueue))^.Eof and eoEnd = 0)
      do
        Inc(iQueue);
      SkipRequested := iQueue >= CopyQueue^.Count-1;
      end;

    function CheckI24Abort: Boolean;
      begin
      CheckI24Abort := Abort;
      if not Abort then
        Exit;
      CopyCancel := True;
      end;
    const
      CrcTable: array[0..255] of Word = (
        $0000, $1021, $2042, $3063, $4084, $50a5, $60c6, $70e7,
        $8108, $9129, $a14a, $b16b, $c18c, $d1ad, $e1ce, $f1ef,
        $1231, $0210, $3273, $2252, $52b5, $4294, $72f7, $62d6,
        $9339, $8318, $b37b, $a35a, $d3bd, $c39c, $f3ff, $e3de,
        $2462, $3443, $0420, $1401, $64e6, $74c7, $44a4, $5485,
        $a56a, $b54b, $8528, $9509, $e5ee, $f5cf, $c5ac, $d58d,
        $3653, $2672, $1611, $0630, $76d7, $66f6, $5695, $46b4,
        $b75b, $a77a, $9719, $8738, $f7df, $e7fe, $d79d, $c7bc,
        $48c4, $58e5, $6886, $78a7, $0840, $1861, $2802, $3823,
        $c9cc, $d9ed, $e98e, $f9af, $8948, $9969, $a90a, $b92b,
        $5af5, $4ad4, $7ab7, $6a96, $1a71, $0a50, $3a33, $2a12,
        $dbfd, $cbdc, $fbbf, $eb9e, $9b79, $8b58, $bb3b, $ab1a,
        $6ca6, $7c87, $4ce4, $5cc5, $2c22, $3c03, $0c60, $1c41,
        $edae, $fd8f, $cdec, $ddcd, $ad2a, $bd0b, $8d68, $9d49,
        $7e97, $6eb6, $5ed5, $4ef4, $3e13, $2e32, $1e51, $0e70,
        $ff9f, $efbe, $dfdd, $cffc, $bf1b, $af3a, $9f59, $8f78,
        $9188, $81a9, $b1ca, $a1eb, $d10c, $c12d, $f14e, $e16f,
        $1080, $00a1, $30c2, $20e3, $5004, $4025, $7046, $6067,
        $83b9, $9398, $a3fb, $b3da, $c33d, $d31c, $e37f, $f35e,
        $02b1, $1290, $22f3, $32d2, $4235, $5214, $6277, $7256,
        $b5ea, $a5cb, $95a8, $8589, $f56e, $e54f, $d52c, $c50d,
        $34e2, $24c3, $14a0, $0481, $7466, $6447, $5424, $4405,
        $a7db, $b7fa, $8799, $97b8, $e75f, $f77e, $c71d, $d73c,
        $26d3, $36f2, $0691, $16b0, $6657, $7676, $4615, $5634,
        $d94c, $c96d, $f90e, $e92f, $99c8, $89e9, $b98a, $a9ab,
        $5844, $4865, $7806, $6827, $18c0, $08e1, $3882, $28a3,
        $cb7d, $db5c, $eb3f, $fb1e, $8bf9, $9bd8, $abbb, $bb9a,
        $4a75, $5a54, $6a37, $7a16, $0af1, $1ad0, $2ab3, $3a92,
        $fd2e, $ed0f, $dd6c, $cd4d, $bdaa, $ad8b, $9de8, $8dc9,
        $7c26, $6c07, $5c64, $4c45, $3ca2, $2c83, $1ce0, $0cc1,
        $ef1f, $ff3e, $cf5d, $df7c, $af9b, $bfba, $8fd9, $9ff8,
        $6e17, $7e36, $4e55, $5e74, $2e93, $3eb2, $0ed1, $1ef0
        );

    function UpdateCrc(CurByte: Byte; CurCrc: Word): Word;
      {-Returns an updated CRC16}
      begin
      UpdateCrc := CrcTable[((CurCrc shr 8) and 255)] xor
          (CurCrc shl 8) xor CurByte;
      end;

    function GetCrc(var Buf; BfLen: Word): Word;
      var
        Data: array[0..$FFF] of Byte absolute Buf;
        I: Word;
        Crc: Word;
      begin
      GetCrc := 0;
      Crc := $1972;
      if BfLen = 0 then
        Exit;
      for I := 0 to pred(BfLen) do
        Crc := UpdateCrc(Data[I], Crc);
      GetCrc := Crc;
      end;

    procedure NoWrite;
      begin
      CopyCancel := True;
      ForceDispatch;
      CantWrite(CurNewName);
      end;

    function WriteCount: String;
      begin
      WriteCount := FStr(ToWrite)+GetString(dlBytes)+GetPercent(ToWrite)
        +GetString(dlFC_Written);
      SetTitle(GetPercent((ToWrite+ToRead)/2)+GetString(dlCopied));
      end;

    procedure TrySetArch;
      begin
      if  (ExAttr and (ReadOnly+SysFile+Hidden) <> 0) then
        begin
        lSetFAttr(WriteStream, Archive);
        ClrIO;
        end;
      end;

    var
      WPWas: Boolean;

    procedure WriteProgress;
      var
        CT: String;
      begin
      Info^.Write(6, StrGrd(P^.Size, Wrote, Info^.Size.X-6, RevertBar));
      Info^.Write(7, WriteCount);
      CT := CopyTime;
      if CT <> '' then
        Info^.Write(9, CT); {John_SW 30-06-2005}
      end;

    { **************** }
    { *** FileCopy *** }
    { **************** }

    var
      NFBigger: Boolean; {John_SW}
      WorkS: String;
      DirInfo : lSearchRec; {John_SW}
    begin { MaxWrite }
    SkipRequested := False;
    iQueue := -1;
    WPWas := False;
    MemBufPos := 0;
    if not Abort or not CopyCancel then
      while True do
        begin
1:
        Inc(iQueue);
        if iQueue = CopyQueue^.Count then
          begin
          if WPWas then
            WriteProgress;
          Break;
          end;
        P := CopyQueue^.At(iQueue);
        if P^.Eof and eoStart <> 0 then
          begin
          CurOldName := CnvString(P^.OldName);
          CurNewName := CnvString(P^.NewName);
          CurDate := P^.Date;
          CurAttr := P^.Attr;
          DisposeStr(P^.OldName);
          P^.OldName := nil;
          DisposeStr(P^.NewName);
          P^.NewName := nil;
          end;
        if CurAttr and Directory <> 0 then
          begin
          SSS := CurOldName;
          MakeNoSlash(SSS);
          ClrIO;
          if CopyOptions and cpoMove <> 0 then
            begin
            DeleteDiz(P^.Owner);
            lRmDir(SSS);
            end;
          if  (P^.Owner <> nil) then
            begin
            if IOResult = 0 then
              P^.Owner^.Attr := P^.Owner^.Attr or Copied;
            if  (SourcePanel <> nil) then
              Message(SourcePanel, evCommand, cmCopyUnselect, P^.Owner);
            end;
          Continue;
          end
          {$IFDEF OS2}
          {JO - для переименования файла в .LONGNAME исходного файла}
        else if (CopyOptions and cpoEAToName <> 0) then
          begin
          LogName := '';
          if  (GetEAString(CurOldName, '.LONGNAME',
                 LogName, False) = 0) and
              (LogName <> '')
          then
            begin
            for I_LN := 1 to Length(LogName) do
              if LogName[I_LN] in [#1..#31, #44, #42, #47, #58, #59,
                 #60, #62, #63, #92, #124]
              then
                LogName[I_LN] := #33;
            lFSplit(CurNewName, NewDir, NewPrName, NewExt);
            CurNewName := NewDir+LogName;
            end;
          end
          {$ENDIF}
          ;
        if P^.Eof and eoStart <> 0 then
          begin
2: { Сюда попадаем, в частности, при переименование файла с одинаковым именем }
          ExAttr := $FFFF;
          Wrote := 0;
          Info^.Write(5,
               GetString(dlFC_Writing)+Cut(CurNewName, 40)+' ');
          Info^.Write(6, Strg(#177, Info^.Size.X-6));
          Info^.Write(7, WriteCount);
          //Dispatch;
          GrdClick := False;
          lAssignFile(WriteStream, CurNewName);
          ClrIO;
          FileMode := $42;

          if  (CopyOptions and cpoCheckFree <> 0)
             and (SysDiskFreeLongX(@PathBuffer) < P^.Size)
          then
            begin
            {--- start -------- Eugeny Zvyagintzev ---- 29-08-2002 ----}
            {We have to check for existing file's size when not enough free space}
            lFindFirst(CurNewName, AnyFileDir, DirInfo);
            NFBigger := (DosError <> 0) or
                        (P^.Size > DirInfo.FullSize +
                                   SysDiskFreeLongX(@PathBuffer));
            lFindClose(DirInfo);
            if NFBigger then
              begin
              {--- finish -------- Eugeny Zvyagintzev ---- 29-08-2002 ----}
              PS[1] := Pointer(LongInt(Drv+64));
              PS[2] := P^.OldName;
              lAssignFile(WriteStream, '');
              ForceDispatch;
              if CopyOptions and cpoFitAll = 0 then
                begin
                MsgResult := Msg(erNoDiskSpace, @PS,
                     mfError+mfYesButton+mfNoButton+mfAllButton);
                CopyCancel := (MsgResult <> cmYes) and (MsgResult <> cmOK);
                if MsgResult = cmOK then
                  CopyOptions := CopyOptions or cpoFitAll
                end
              else
                CopyCancel := False;
              NewTimer(Timer, 0);
              DoSkip;
              if not SkipRequested then
                goto 1;
              if  (not CopyCancel) then
                Continue;
              Break;
              end;
            end;

          if P^.Eof and eoCheck <> 0 then
            begin
            if AdvCopy then
              begin
lRetryToReset:
              TryToReset;
              case Opened of
                0:
                  begin
                  if Created then
                    goto lbStartWrite
                  else
                    goto lbStartCheck;
                  end;
                5:
                  goto lbStartCheck;
                123, 206:
                  begin
                  if Opened = 206 then
                    LongNameWarn :=
                      GetString(dlCE_Filename_Exced_Range)
                  else
                    LongNameWarn :=
                      GetString(dlCE_Filename_Illegal);
                  I_LW := Pos('%', LongNameWarn);
                  if I_LW > 0 then
                    begin
                    LongNameWarn[I_LW] :=
                      UpCase(CurNewName[1]);
                    LongNameWarn[I_LW+1] := ':'
                    end;
                  case MessageBox(^C+LongNameWarn+^M^C+
                    GetString(dlFCRename)+' ?', nil,
                    mfError+mfYesButton+
                    mfNoButton+mfCancelButton) of
                    cmYes:
                      begin
                      S1 := CurNewName;
                      J := InputBox(GetString(dlFCRename),
                          GetString(dlFCRenameNew), S1, 255, 0);
                      if  (J <> cmOK) or (S1 = '') then
                        begin
                        DoSkip;
                        if not SkipRequested then
                          goto 1;
                        Break;
                        end;
                      CurNewName := S1;
                      NewTimer(Timer, 0);
                      goto 2;
                      end;
                    cmNo:
                      begin
                      DoSkip;
                      if not SkipRequested then
                        goto 1;
                      Break;
                      end;
                    else {case}
                      begin
                      CopyCancel := True;
                      ForceDispatch;
                      Break;
                      end;
                  end {case};
                  end;
                else {case}
                  begin
                  Ask := SysErrorFunc(Opened,
                         Byte(CurNewName[1])-65);
                  if Ask = 1 then
                    goto lRetryToReset;
                  NoWrite;
                  Break;
                  end;
              end {case};
              end
            else { not AdvCopy }
              begin
              ClrIO;
              FileMode := $40;
              lResetFile(WriteStream, 1);
              end;

            if InOutRes = 0 then
              begin
lbStartCheck:
              case CopyMode of
                cpmOverwrite:
                  goto DoRewrite;
                cpmAppend:
                  goto DoAppend;
                cpmResume:
                  begin
                  goto DoAppend;
                  end;
                cpmSkipAll:
                  begin
                  Info^.Write(2, GetString(dlFC_Exists));
                  Close(WriteStream.F);
                  DoSkip;
                  if not SkipRequested then
                    goto 1;
                  Break;
                  end;
                cpmRefresh:
                  begin
                  GetFtime(WriteStream.F, A);
                  if A >= P^.Date then
                    begin
                    Info^.Write(2, GetString(dlFC_Older));
                    Close(WriteStream.F);
                    DoSkip;
                    if not SkipRequested then
                      goto 1;
                    Break;
                    end;
                  end;
                else {case}
                  begin
                  ClrIO;
                  Close(WriteStream.F);
                  ClrIO;
                  GetFTimeSizeAttr(CurNewName, A, BB, ExAttr);
                  if DosError <> 0 then
                    begin
                    NoWrite;
                    Break;
                    end;
                  case Overwrite(CurNewName, BB, P^.Size, A,
                     P^.Date) of
                    cmSkip:
                      begin
                      DoSkip;
                      if not SkipRequested then
                        goto 1;
                      Break;
                      end;
                    cmOK:
                      begin
                      S1 := CurNewName;
                      J := InputBox(GetString(dlFCRename),
                          GetString(dlFCRenameNew), S1, 255, 0);
                      if  (J <> cmOK) or (S1 = '') then
                        begin
                        DoSkip;
                        if not SkipRequested then
                          goto 1;
                        Break;
                        end;
                      CurNewName := S1;
                      NewTimer(Timer, 0);
                      goto 2;
                      end;
                    cmNo:
                      goto DoAppend;
                    cmYes:
                      goto DoRewrite;
                    else {case}
                      begin
                      CopyCancel := True;
                      Break;
                      end;
                  end {case};
                  end;
              end {case};
              end;
            end;
          if P^.Eof and (eoAppend or eoResume) <> 0 then
            begin
DoAppend:
            FileMode := $42;
            lResetFile(WriteStream, 1);
            J := IOResult;
            case J of
              0:
                begin
                ExAttr := $FD00;
                Seek(WriteStream.F, FileSize(WriteStream.F));
                Wrote := FileSize(WriteStream.F);
                end;
              2:
                goto DoRewrite;
              5:
                begin
                if ExAttr = $FFFF then
                  begin
                  ClrIO;
                  lGetFAttr(WriteStream, ExAttr);
                  if DosError = 0 then
                    TrySetArch
                  end
                else
                  TrySetArch;
                if CheckI24Abort then
                  Break;
                ExAttr := ExAttr or $FE00;
                if (P^.Eof and eoResume) <> 0 then
                  ExAttr := $FF00;
                lResetFile(WriteStream, 1);
                if IOResult = 0 then
                  Seek(WriteStream.F, FileSize(WriteStream.F));
                end;
            end {case};
            if (P^.Eof and eoResume) <> 0 then
              ExAttr := $FF00;
            end
          else
            begin
DoRewrite:
            ClrIO;
            RewriteWriteStrem;
            if CheckI24Abort then
              Break;
            if  (InOutRes = 5) then
              begin
              ClrIO;
              if ExAttr = $FFFF then
                begin
                lGetFAttr(WriteStream, ExAttr);
                if DosError = 0 then
                  TrySetArch;
                end
              else
                TrySetArch;
              if CheckI24Abort then
                Break;
              RewriteWriteStrem;
              end;
            end;

          if CheckI24Abort then
            Break;
          Opened := IOResult;
          if Opened <> 0 then
            begin
            if  (Opened = 206) or (Opened = 123) then
              begin
              if Opened = 206 then
                LongNameWarn := GetString(dlCE_Filename_Exced_Range)
              else
                LongNameWarn := GetString(dlCE_Filename_Illegal);
              I_LW := Pos('%', LongNameWarn);
              if I_LW > 0 then
                begin
                LongNameWarn[I_LW] := UpCase(CurNewName[1]);
                LongNameWarn[I_LW+1] := ':'
                end;
              case MessageBox(^C+LongNameWarn+^M^C+
                GetString(dlFCRename)+' ?', nil,
                 mfError+mfYesButton+mfNoButton+mfCancelButton) of
                cmYes:
                  begin
                  S1 := CurNewName;
                  J := InputBox(GetString(dlFCRename),
                      GetString(dlFCRenameNew), S1, 255, 0);
                  if  (J <> cmOK) or (S1 = '') then
                    begin
                    DoSkip;
                    if not SkipRequested then
                      goto 1;
                    Break;
                    end;
                  CurNewName := S1;
                  NewTimer(Timer, 0);
                  goto 2;
                  end;
                cmNo:
                  Break;
                else {case}
                  begin
                  CopyCancel := True; {вместо NoWrite}
                  ForceDispatch;
                  Break;
                  end;
              end {case};
              end
            else
              Ask := SysErrorFunc(Opened,
                   Byte(CurNewName[1])-65);
            if Ask = 1 then
              goto DoRewrite;
            NoWrite;
            Break;
            end;
          end;
lbStartWrite:
        ClrIO;
        Dispatch;
        if CopyCancel then
          Break;
        if CopyOptions and cpoVerify <> 0 then
          SeekPos := FilePos(WriteStream.F);
        BlockWrite(WriteStream.F, MemBuf^[MemBufPos], P^.len, J);
        IOR := IOResult;

        if not Abort and (CopyOptions and cpoVerify <> 0) and (J > 0)
        then
          begin
          {$IFDEF DPMI32}
          asm
                     mov ah,0dh
                     int 21h
                   end;
          {$ENDIF}

          BufCrc := GetCrc(MemBuf^[MemBufPos], P^.len);

          Seek(WriteStream.F, SeekPos);
          ClrIO;
          BlockRead(WriteStream.F, MemBuf^[MemBufPos], P^.len, J);

          if CheckI24Abort then
            Break;

          if  (IOResult <> 0) or
              (J <> P^.len) or
              (GetCrc(MemBuf^[MemBufPos], P^.len) <> BufCrc)
          then
            begin
            ForceDispatch;
            MessageBox(GetString(dlFCVerifyFailed), nil,
               mfError+mfOKButton);
            CopyCancel := True;
            Break;
            end;
          end;
        inc(MemBufPos, P^.len);

        //Dispatch;
        if Abort or CopyCancel then
          Break;
        Wrote := Wrote+J;
        ToWrite := ToWrite+J;

        if GrdClick then
          begin
          WriteProgress;
          GrdClick := False;
          WPWas := False
          end
        else
          WPWas := True;

        if  (J <> P^.len) or (IOR <> 0) then
          begin
          CopyCancel := True;
          ForceDispatch;
          if  (CopyPrn) or
              (IOR = 5) or
              (SysDiskSizeLongX(@PathBuffer) > P^.Size)
          then
            NoWrite
          else
            ErrMsg(dlFBBDiskFull2);
          Break;
          end;
        if P^.Eof and eoEnd <> 0 then
          begin
          WriteProgress;
          WPWas := False;
          SetDateAttr;
          if  (P^.Owner <> nil) then
            begin
            P^.Owner^.Attr := P^.Owner^.Attr or Copied;
            if  (SourcePanel <> nil) then
              Message(SourcePanel, evCommand, cmCopyUnselect, P^.Owner);
            if  (P^.Owner^.DIZ <> nil) and
                (CopyOptions and cpoDesc <> 0)
            then
              begin
              Info^.Write(6, GetString(dlExportingDIZ));
              ExportDiz(nil, GetName(CurNewName), P^.Owner^.DIZ, CopyDir);
              end;
            end;
          {JO EAs - files}
          {$IFNDEF DPMI32}
          if not CopyPrn then
            CopyEAs(CurOldName, CurNewName);
          {$ENDIF}
          {Cat SAs - files}
          {$IFDEF WIN32}
          if not CopyPrn and (CopyOptions and cpoAccRights <> 0) then
            CopySAs(CurOldName, CurNewName);
          {$ENDIF}
          if CopyOptions and cpoFromTemp <> 0 then
            RemoveFromTemp(P^.Owner);
          if CopyOptions and cpoMove <> 0 then
            {move выделенных; по F6 на файле сюда не попадаем. }
            begin
            Info^.Write(6, GetString(dlDeletingSource));
            DeleteDiz(P^.Owner);
            EraseFile(CurOldName);
            end;
          end;
        end;
    FreeDisk := -1;
    CopyQueue^.FreeAll;
    MemBufPos := 0;
    end { MaxWrite };

  procedure CopyFile(const FName, AddDir: String; Own: PFileRec;
       ln: TSize; Dtt: LongInt; Attr: Word);
    label 1;
    var
      P: pLine;
      A, BB, WW: LongInt;
      Rd: TSize;
      I, J: Word;
      FFF: Boolean;
      Was: TSize;
      EOF: Byte;
      NName, S1, S2: String;
      SR: lSearchRec;
      PS: array[1..2] of Pointer;
      DEr: Boolean;
      Drive: Byte;
      D: PDialog; {John_SW}{process locked files}
      R: TRect;   {John_SW}
      S: String;  {John_SW}

    procedure RenameFile;
      var
        F: lFile;
        W: Word;
        SR: lSearchRec;
      begin
      DosErrorCode := 0;
      RnFl(S1, S2);
      if DosErrorCode = 17 {NOT_SAME_DEVICE} then
        Exit;  { AK155 14.03.2006 фактически код 17
          я сумел получить только под OS/2 }
      if DosErrorCode in [5, 80, 183] then
          { AK155 14.03.2006
            Код 80 (FILE_EXISTS) бывает, например, под NT 4 при
          "переименовании" между фактически разными сетевыми путями,
          когда на целевом устройстве файл есть. Если его удалить,
          то "переименование" проходит, хотя фактически это копирование
          и удаление. Хорошо ли это - непонятно, так как выполняется
          системная функция независимо от настроек DN.
            А при "переименовании" между каталогами одного диска под NT
          получается код 183 (ALREADY_EXISTS). Интересно, в чём
          глубокая разница между FILE_EXISTS и ALREADY_EXISTS?
            Под OS/2 кода 183 как бы нет, но при совпадении имени на
          сетевом виндовом диске получается 183 даже под OS/2. }
        begin
        lAssignFile(F, S2);
        ClrIO;
        lSetFAttr(F, Archive);
        ClrIO;
        lEraseFile(F);
        RnFl(S1, S2);
        end;
      if DosErrorCode in [5, 183] then
        begin {AK155 Смысл этого блока мне непонятен }
        lAssignFile(F, S1);
        lGetFAttr(F, W);
        lSetFAttr(F, Archive);
        ClrIO;
        RnFl(S1, S2);
        lSetFAttr(F, W);
        ClrIO;
        end;
      if not (DosErrorCode in [0, 18]) then
        begin
        ForceDispatch;
        MessFileNotRename(S1, S2, DosErrorCode);
        CopyCancel := True;
        end;
      end { RenameFile };

    procedure DoRename;
      var
        A: array[0..10] of PString;
        I, J: Integer;
      begin
      for I := 0 to Min(10, Info^.Lines^.Count-1) do
        begin
        A[I] := Info^.Lines^.At(I);
        Info^.Lines^.AtPut(I, nil);
        end;
      Info^.Write(2, Cut(FName, 40));
      Info^.Write(4, GetString(dlFC_To));
      Info^.Write(6, Cut(NName, 40));
      //Dispatch;
      if not CopyCancel then
        RenameFile;
      if DosErrorCode = 17 {NOT_SAME_DEVICE} then
        Exit;
      if not CopyCancel and (Own <> nil) then
        begin
        Own^.Attr := Own^.Attr or Copied;
        if  (Own^.DIZ <> nil) and
            (CopyOptions and cpoDesc <> 0)
        then
          begin
          Info^.Write(6, GetString(dlExportingDIZ));
          ExportDiz(@Own^.FlName, GetName(NName), Own^.DIZ, CopyDir);
          if  (CopyOptions and cpoMove <> 0) and
              (FMSetup.Options and fmoPreserveDesc = 0)
          then
            DeleteDiz(Own);
          end;
        {сюда никаких копирований EA вставлять не надо}

        {$IFDEF DualName}
        Own^.FlName[False] := GetName(lfGetShortFileName(NName));
        {$ENDIF}
{!RLN}        CopyShortString(GetName(NName), Own^.FlName[True]);
        //         ReplaceLongName(Own, {$IFDEF OS2}GetName{$ENDIF}(NName));
   { Это на случай если стоим как раз на этом имени.
   Чтобы при перечитывании каталога курсор ушёл на новое имя,
   а не остался на старой позиции }
        if  (SourcePanel <> nil) then
          Message(SourcePanel, evCommand, cmCopyUnselect, Own);
        ToWrite := ToWrite+ln;
        ToRead := ToRead+ln;
        end;
      Info^.DrawView;
      for I := 0 to Min(10, Info^.Lines^.Count-1) do
        Info^.Lines^.AtReplace(I, A[I]);
      end { DoRename };

    label
      NoRename, FileRead, PrepareResume, PrepareSkip, PrepareAppend;

    function ReadCount: String;
      begin
      ReadCount := FStr(ToRead)+GetString(dlBytes)+GetPercent(ToRead)
        +GetString(dlFC_WasRead);
      SetTitle(GetPercent((ToWrite+ToRead)/2)+GetString(dlCopied));
      end;

    procedure ReadProgress;
      var
        CT: String;
      begin
      Info^.Write(2, StrGrd(ln, ReadPos, Info^.Size.X-6, RevertBar));
      Info^.Write(3, ReadCount);
      CT := CopyTime;
      if CT <> '' then
        Info^.Write(9, CT); {John_SW 30-06-2005}
      end;

    begin { CopyFile }
    {JO: !!! не копируем файлы найденные в архивах  }
    {if PathFoundInArc(FName) then Exit;}
    {/JO}
    if CopyPrn then
      begin
      NName := CopyDir;
      while NName[Length(NName)] = '/' do // slash change by unxed
        SetLength(NName, Length(NName)-1);
      S1 := lFExpand(FName); {Cat}
      S2 := lFExpand(NName); {Cat}
      S2[1] := '';
      {Cat: теперь имя начинается с ":\", поэтому
        дальше процедуре будет казаться, что файл назначения (а на
        самом деле устройство) располагается на другом диске, поэтому
        вместо переименования будет использоваться копирование с
        удалением источника. Однако, нет полной гарантии, что имя
        устройства с полным путём, да ещё и с таким обозначением диска,
        будет нормально воспринято системой}
      end
    else
      begin
      if AddDir = '' then
        NName := MkName(GetName(FName))
      else
        NName := MakeNormName(AddDir, GetName(FName));

      NName := MakeNormName(CopyDir, NName);
1:
      S1 := lFExpand(FName);
      S2 := lFExpand(NName);
      if S1 = S2 then
        begin
        {--- start -------- Eugeny Zvyagintzev ---------------------------------}
        {This 2 line was commented to allow DN display error message            }
        {during _move_ file to itself                                           }
        {Before this comment DN displayed error message only                    }
        {during _copy_ file to itself                                           }
        {            if CopyOptions and cpoMove = 0 then
               begin}
        {--- finish -------- Eugeny Zvyagintzev --------------------------------}
        if CopyQueue^.Count > 0 then
          MaxWrite;
        if CopyCancel then
          Exit;
        ForceDispatch;
        MessageBox(^C+GetString(dlFile)+' '+FName+GetString(dlFCItself),
           nil,
          mfError+mfOKButton);
        CopyCancel := True;
        {             end;} {Eugeny Zvyagintzev}
        Exit;
        end;
      end;

    Was := 0;
    ClrIO;
    lAssignFile(ReadStream, FName);
    if S1[1] in CD_Drives then
      Attr := Attr and not ReadOnly;

    FileMode := $40;
    lResetFile(ReadStream, 1);
    ReadPos := 0;

    if Abort then
      begin
      ClrIO;
      if CopyQueue^.Count > 0 then
        MaxWrite;
      CopyCancel := True;
      Exit;
      end;

    RC := IOResult;
    if RC <> 0 then
      begin
      ClrIO;
      if CopyQueue^.Count > 0 then
        MaxWrite;
      if CopyCancel then
        Exit;
      ForceDispatch;
{--- start -------- Eugeny Zvyagintzev and Max Piwamoto 04-02-2005 ----}
      If SkipAllBad Then Exit;
      D := PDialog(LoadResource(dlgSkipBadFile));
      D^.Options := D^.Options or ofCentered;
      S:=Cut(FName,52);
      R.A.X:=1; R.A.Y:=3; R.B.X:=53; R.B.Y:=4;
      D^.Insert(New(PStaticText, Init(R,^C+S)));
      Case Desktop^.ExecView(D) Of
       cmOK: Goto 1;
       cmYes: begin SkipAllBad := True; Exit; end;
       cmNo: Exit;
       cmCancel: begin CopyCancel := True; Exit; end;
      End;
{--- finish ------- Eugeny Zvyagintzev and Max Piwamoto 04-02-2005 ----}
      end;
    Rd := 0;
//    EOF := eoStart or eoCheck*Byte(CopyMode >= cpmAppend);
    {--- start -------- Eugeny Zvyagintzev -----------------------------------}
    {This 3 line was commented to prevent DN first coping files to buffer     }
    {and _then_ check for file already exists                                 }
    {Now DN will first check for file exists                                  }

    {if (CopyMode >= cpmAppend) and (CopyOptions and cpoMove <> 0)  and
         (S1[1] = S2[1]) then
        begin}
    {--- finish -------- Eugeny Zvyagintzev ----------------------------------}

    {AK155 чтобы можно было файлы и каталоги копировать на nul и другие
устройства, чуть ниже вставлено 3 анализа CopyPrn. }
    ClrIO;
    EOF := eoStart or eoCheck;
    if not CopyPrn then
      {AK155 на устройстве незачем искать файлы}
      begin
      lFindFirst(S2, AnyFileDir, SR); {JO}
      DEr := (DosError <> 0);
      lFindClose(SR);
      end;
    if CopyPrn {AK155} or not DEr then
      begin
      EOF := eoStart;
      Was := SR.FullSize;
      if not CopyPrn {AK155: для одиночного файла} and (SR.SR.Attr and
           Directory <> 0)
      then
        begin
        if CopyQueue^.Count > 0 then
          MaxWrite;
        if CopyCancel then
          Exit;
        ForceDispatch;
        MessageBox(GetString(dlFCNotOverDir)+Cut(S2, 40), nil,
           mfError+mfOKButton);
        Exit;
        end;
      case CopyMode of
        cpmAskOver:
NoRename:
            case Overwrite(NName, SR.FullSize, ln, SR.SR.Time, Dtt) of
              cmSkip:
                goto PrepareSkip;
              cmOK:
                begin
                ClrIO;
                Close(ReadStream.F);
                ClrIO;
                if InputBox(GetString(dlFCRename),
                       GetString(dlFCRenameNew),
                    NName, 255, 0) <> cmOK
                then
                  goto NoRename;
                NewTimer(Timer, 0);
                goto 1;
                end;
              cmNo:
                goto PrepareAppend;
              cmSave:
                goto PrepareResume;
            end {case};
        cpmRefresh:
          if Dtt <= SR.SR.Time then
            goto PrepareSkip;
        cpmSkipAll:
          begin
PrepareSkip:
          Info^.Write(1, GetString(dlFC_Reading)+Cut(FName, 40));
          Info^.Write(2, GetString(dlFC_Exists));
          ClrIO;
          Close(ReadStream.F);
          ClrIO;
          Exit;
          end;
        cpmAppend:
PrepareAppend:
          EOF := EOF or eoAppend;
        cpmResume:
          begin
PrepareResume:
          Rd := SR.FullSize;
          if Rd >= FileSize(ReadStream.F) then
            goto PrepareSkip;
          EOF := EOF or eoResume;
          ReadPos := Rd;
          Seek(ReadStream.F, CompToFSize(Rd));
          end;
      end {case};
      end;
    {       end;} {Eugeny Zvyagintzev}
    if  (CopyOptions and cpoMove <> 0) and (EOF and eoAppend = 0) and
        (S1[1] = S2[1])
          {AK155: для сетевых путей '\'='\'. Но, может, это и хорошо.}
    then
      begin
      ClrIO;
      Close(ReadStream.F);
      ClrIO;
      DoRename;
      if DosErrorCode <> 17 {NOT_SAME_DEVICE} then
        Exit;
      { То, что казалось одним устройством, оказалось разными, и move
       не прошло. Пробуем по-другому: скопировать и удалить.}
      CopyCancel := False;
      lResetFile(ReadStream, 1);
      end;
    Info^.Write(1, GetString(dlFC_Reading)+Cut(FName, 40)+' ');
    Info^.Write(2, Strg(#177, Info^.Size.X-6));
    Info^.Write(3, ReadCount);
    //Dispatch;
    GrdClick := False;
    {Info^.DrawView;}
    ln := FileSize(ReadStream.F); {-$VOL}
    P := nil;
    repeat
      if CopyQueue^.Count > 400 then
        MaxWrite; {John_SW  06-11-2002}
      SkipRequested := False;
      WW := Min(ReadLen, MemBufSize-MemBufPos);
      if WW > ln-Rd then
        WW := Round(ln-Rd);
      P := New(pLine, Init(WW, Own, FName, NName, ln, Dtt, Attr, EOF));
      if P = nil then
        begin { исчерпан буфер }
        ReadProgress;
        MaxWrite;
        if SkipRequested then
          Break;
        Continue
        end;
      J := 0;

FileRead:
      Dispatch;
      if CopyCancel then
        Break;
      if WW > 0 then
        begin
        BlockRead(ReadStream.F, MemBuf^[MemBufPos], WW, J);
        inc(MemBufPos, WW);
        end;

      {AK155}
//      {$IFNDEF DPMI32} {AK155 28.06.2007 в DPMI32 int24 не проявляется }
      { Под OS/2 и Win32 не вызывается, как в ДОС, обработчик критических ошибок,
 так что надо самим анализировать результат и вызывать обработчик }
      I := IOResult;
      if I <> 0 then
        begin
        Drive := Byte('');
        if FName[2] = ':' then
          Drive := Byte(FName[1]);
        SysErrStopButton := True;
        I := SysErrorFunc(I, Drive-Byte('A'));
        SysErrStopButton := False;
        if I = 1 then
          goto FileRead;
        if I = 4 then
          begin
          _Tmr.ExpireMSecs := GetCurMSec-1; // сделать таймер истекшим
          CtrlBreakHit := True;
          goto FileRead;
          end;
        CopyCancel := True;
        Dispose(P, Done);
        P := nil;
        Break;
        end;
//      {$ENDIF}
      {/AK155}
      ReadPos := ReadPos+J;
      //Dispatch;
      if CopyCancel then
        Break;
      if Abort then
        begin
        ClrIO;
        if CopyQueue^.Count > 0 then
          MaxWrite;
        CopyCancel := True;
        Break;
        end;
      if ReadPos >= ln then
        begin
        EOF := EOF or eoEnd;
        if P^.OldName = nil then
          P^.OldName := NewStr(FName);
        end;
      P^.EOF := EOF;
      if WW <> J then
        begin
        {AK155 А здесь надо бы проанализировать IOResult
               и выдать вразумительное сообщение! }
        CopyCancel := True;
        Break;
        end;

      ToRead := ToRead+J;

      //Dispatch;
      if GrdClick then
        begin
        ReadProgress;
        GrdClick := False;
        end;

      if Abort or CopyCancel then
        Break;
      CopyQueue^.Insert(P);
      P := nil;
      EOF := EOF and not eoStart;
      Rd := Rd+WW;
    until CopyCancel or Abort or (Rd = ln) or System.EOF(ReadStream.F)
     or SkipRequested; {-$VOL}
    if P <> nil then
      begin
      Dispose(P, Done); // выскочили по Break после Init, но до Insert
      P := nil;
      end;

    SkipRequested := False;
    ClrIO;
    ReadProgress;
    Close(ReadStream.F);
    if not AdvCopy then
      MaxWrite;
    end { CopyFile };

  procedure CopyDirectory(const DirName, AddDir: String; Own: PFileRec);
    var
      SR: lSearchRec;
      P: pLine;
      L: LongInt;
    begin
    ClrIO;
    lFindFirst(DirName+'/*.*', AnyFileDir, SR); {JO} // slash change by unxed
    while (DosError = 0) and not Abort and not CopyCancel do
      begin
      SSS := MakeNormName(AddDir, SR.FullName);
      if SR.SR.Attr and Directory = 0 then
        begin
        CopyFile(DirName+'/'+SR.FullName, Copy(AddDir, // slash change by unxed
            Length(CopyDir)+1, MaxStringLength), nil, SR.FullSize,
          SR.SR.Time, SR.SR.Attr);
        end;
      if CopyCancel or Abort then
        begin
        lFindClose(SR);
        Exit;
        end;
      ClrIO;
      lFindNext(SR);
      end;
    lFindClose(SR);
    if CopyCancel or Abort then
      Exit;
    L := 0;
    P := New(pLine, Init(L, Own, DirName, '', 0, 0, Directory,
           eoStart+eoDir));
    if P <> nil then
      CopyQueue^.Insert(P);
    end { CopyDirectory };

  procedure MaxRead;
    var
      I: Integer;
      P: PFileRec;
      S: String;

    procedure DoCopyDirectory(P: PDirName);
      begin
      if P^.CopyIt then
        CopyDirectory(P^.DOld, P^.DNew, P^.Own)
      else if P^.Own <> nil then
{!RLN}        CopyShortString(GetName(P^.DOld), P^.Own^.FlName[True]);
   { Это на случай, если стоим как раз на этом каталоге. Чтобы
   при перечитывании каталога курсор ушёл на новое имя,
   а не остался на старой позиции.
     А nil бывает, например, на подкаталогах при переименовании
   объемлющего каталога по F6 при погашенной другой панели.
   При этом, кстати, P^.OldName^ =  P^.NewName^. }
      end;

    begin
    CopyStartTime := GetCurMSec; CopyElapsedTime := 0; {John_SW 30-06-2005}
    for I := 0 to Files^.Count-1 do
      begin
      P := Files^.At(I);
      S := MakeNormName(P^.Owner^, P^.FlName[True]);
      if P^.Attr and Directory = 0 then
        CopyFile(S, '', P, P^.Size, PackedDate(P), P^.Attr);
      if Abort or CopyCancel then
        Exit;
      end;
    if Abort or CopyCancel then
      Exit;
    if Dirs <> nil then
      Dirs^.ForEach(@DoCopyDirectory);
    MaxWrite;
    end { MaxRead };

  procedure DoneMemBuf;
    begin
    if MemBuf <> nil then
      begin
      FreeMem(MemBuf);
      MemBuf := nil;
      end;
    end;

  procedure MakeDirectories;
    var
      P: PFileRec;
      PD: PDirName;
      I: Integer;
      S: String;
      FrPos: Integer;
      KillDescrOf: PFlName;
      {$IFDEF OS2}
      LogName, NewDir, NewPrName, NewExt: String;
      I_LN: Byte;
      {$ENDIF}

    procedure CopyI(Dest{Целевой каталог},
        Source{ полное старое имя }: String;
        Name: String;  {Новое имя}
        o: PFileRec; {тут, вроде, то же самое, что и в Source,
          но бывает o=nil}
        CopyIt: Boolean;
        Attr: Byte);
      label 1, 2, TrueCopy;
      var
        JJ: Integer;
      var
        q: String;
        SSS: String;
        {$IFDEF OS2}
        I_LN: Byte;
        {$ENDIF}
      begin
      Info^.Write(5, GetString(dlFCCheckingDirs));

      if  (not CopyPrn) and (Dest <> '') and (Dest[Length(Dest)] <> '/') // slash change by unxed
      then
        AddStr(Dest, '/'); // slash change by unxed
      q := MakeNormName(Dest, Name);
      if q[Length(q)] = '.' then
        SetLength(q, Length(q)-1);
      if o = nil then
        goto 1;
      if CopyCancel then
        goto 2;
      ClrIO;
      SSS := Source;
      MakeSlash(SSS);
      if SSS = Copy(S, 1, Length(SSS)) then
        begin
        ForceDispatch;
        MessageBox(^C+GetString(dlDirectory)+' '+Cut(Source, 40)+
          GetString(erIntoItself), nil, mfError+mfOKButton);
        CopyCancel := True;
        Exit;
        end;
      SetLength(SSS, Length(SSS)-1);
      if  (o <> nil) and (CopyOptions and cpoMove <> 0)
           and (SSS[1] = q[1])
      then
        begin { Переименование каталога }
        ClrIO;
        RnFl(SSS, q);
        if DosErrorCode in [5, 80, 183] then
          begin {Возможно, целевой каталог есть, но пустой. Тогда
             его можно удалить и переименование всё-таки проделать }
          LFNvp.lRmDir(q);
          ClrIO;
          RnFl(SSS, q);
          end;
        if Abort then
          begin
          CopyCancel := True;
          goto 2;
          end;
        if DosErrorCode in [5, 17, 80, 183] then
          goto TrueCopy;
            { Целевой каталог есть, и непустой, либо переименование
              невозможно в принципе. Так что будем честно копировать
              и удалять. }
        CopyIt := False;
        o^.Attr := o^.Attr or Copied;
        if DosErrorCode <> 0 then
          begin
          Info^.Write(5, '');
          Info^.Write(2, Cut(SSS, 40));
          Info^.Write(4, GetString(dlFC_To));
          Source := lFExpand(q);
          Info^.Write(6, Cut(Source, 40));
          ForceDispatch;
          MessageBox(GetString(dlNotRenameDir)+Source, nil,
            mfError+mfOKButton);
          CopyCancel := True;
          Info^.Write(2, '');
          Info^.Write(4, '');
          Info^.Write(6, '');
          Info^.Write(5, GetString(dlFCCheckingDirs));
          goto 2;
          end;


(*
{! Так копировать LFN нельзя! Длины могут отличаться. Надо ли вообще
 копировать новое длинное имя - надо разобраться. Похоже, что не надо.
 Это ведь запись исходного каталога, а не приёмного. А короткое имя
 вообще надо не копировать, а запрашивать, оно ведь может оказаться
 совсем другим. Так что пока что прибиваю пересылку имён. Вроде,
 ни на что не повлияло.
   Проанализировал подробнее. Это переименование нафиг не нужно.
 Данная файловая запись используется тут только для того, чтобы передать
 её в cmCopyUnselect, а там она служит только для сравнения имён. Кстати,
 абсурд редкий. Просматривается вся коллекция на предмет совпадения имени
 с переданной записью. И, поскольку эта запись является одним из элементов
 коллекции, то совпадение происходит при сравнении её с самой сабой. Так
 что старое имя точно годится, а вот новое, наверно, может и к глюкам
 приводить, если это новое имя совпадает с уже имевшимся. Притом всё это
 нужно только для красивой перерисвоки панеленй и подвала по мере
 копирования. А потом всё равно панель перечитывается.}
        {$IFDEF DualName}
        o^.FlName[False] := GetName
                (lfGetShortFileName(MakeNormName(GetPath(Source), Name)));
        {$ENDIF}
        CopyShortString(MakeNormName(GetPath(Source), Name),
           o^.FlName[True]);
*)
        if SourcePanel <> nil then
          Message(SourcePanel, evCommand, cmCopyUnselect, o);
        KillDescrOf := @o^.FlName;
          { При переименовании каталога описание со старым именем
           надо прибивать }
        goto 1;
        end
      else
        begin { Копирование каталога }
TrueCopy:
        CopyIt := True;
        ClrIO;
        if not CopyPrn then
          begin
          CheckMkDir(q);
          if not Abort then
            JJ := SetFileAttr(q, Attr and not Directory)
          end;
        KillDescrOf := nil;
          { При копировании каталога описание со старым именем
            прибивать не надо }
        end;
      if Abort then
        begin
        CopyCancel := True;
        goto 2;
        end;
1:
      if CopyPrn then
        begin
        if Dest = '' then
          SSS := Name
        else
          SSS := Dest;
        end
      else
        SSS := q;
      {$IFDEF OS2}
      {JO - для переименования подкаталогов в .LONGNAME исходных подкаталогов}
      if  (CopyOptions and cpoEAToName <> 0) then
        begin
        LogName := '';
        if  (GetEAString(Source, '.LONGNAME', LogName, False) = 0) and
            (LogName <> '')
        then
          begin
          for I_LN := 1 to Length(LogName) do
            if LogName[I_LN] in [#1..#31, #44, #42, #47, #58, #59, #60,
               #62, #63, #92, #124]
            then
              LogName[I_LN] := #33;
          lFSplit(SSS, NewDir, NewPrName, NewExt);
          SSS := NewDir+LogName;
          end;
        end;
      {$ENDIF}
      {MessageBox('Dirs^.AtInsert: ' + SSS + ' '+ Source, nil, mfOKButton);}
      Dirs^.AtInsert(FrPos, New(PDirName, Init(Source, SSS, CopyIt, o,
             Attr)));
      Inc(FrPos);
2:
      end { CopyI };

    procedure CopyF(Dest, Source: String; CopyIt: Boolean; Attr: Byte);
      var
        SR: lSearchRec;
        Drive: Byte;
      begin
      if Dest[Length(Dest)] = '/' then // slash change by unxed
        SetLength(Dest, Length(Dest)-1);
      if Dest[Length(Dest)] = '.' then
        SetLength(Dest, Length(Dest)-1);
      ClrIO;
      {AK155 Зачем нужен этот кусок - не сразу понятно. Ведь, вроде,
 все это уже сделано в CopyI. Но на самом деле это таки нужно для
 подкаталогов. Конечно, прямее было бы только для подкаталогов это
 и делать, но так тоже ничего - лишняя операция только одна }
      if not CopyPrn then
        begin
        CheckMkDir(Dest);
        if not Abort then
          SetFileAttr(Dest, Attr and not Directory);
        ClrIO;
        end;
      {/AK155}
      lFindFirst(Source+'/*.*', AnyFileDir, SR); {JO} // slash change by unxed
      while (DosError = 0) and not Abort and not CopyCancel do
        begin
        if  (SR.SR.Attr and Directory <> 0) and
          {!!!} not IsDummyDir(SR.SR.Name)
        then
          begin
            CopyI(Dest, MakeNormName(Source, SR.FullName), SR.FullName,
               nil,
              CopyIt, SR.SR.Attr)
          end
        else
          begin
          ToDo := ToDo+SR.FullSize;
          ToDoCopy := ToDoCopy+SR.FullSize;
          if SR.FullSize > 0 then
            begin
            ToDoClusCopyTemp := SR.FullSize/BytesPerCluster;
            ToDoClusCopy := ToDoClusCopy+
                ( {Round}(ToDoClusCopyTemp)+
                Byte( {Round}(ToDoClusCopyTemp)-ToDoClusCopyTemp <> 0)
                )*BytesPerCluster;
            end;
          end;
        ClrIO;
        //Dispatch;
        if not CopyCancel then
          lFindNext(SR);
        end;
      {AK155 Если при копировании каталога или выделенных файлов
 возникает ошибка чтения, то надо выдать сообщение, а не
 просто молча прекратить копирование. Но цикл нормально
 завершается, когда больше нет файлов, при этом под всеми
 операционками DosError=18. Ошибка 3 возникает на каталоге,
 кторый был успешно перемещён целиком.
 }
      if  (DosError = 18 {ERROR_NO_MORE_FILES}) or
          (DosError = 3 {PATH_NOT_FOUND})
      then
        DosError := 0;
      if DosError <> 0 then
        begin
        Drive := Byte('');
        if Source[2] = ':' then
          Drive := Byte(Source[1]);
        SysErrorFunc(DosError, Drive-Byte('A'));
        {при этом будет установлен Abort}
        end;
      {/AK155}
      lFindClose(SR);
      end { CopyF };

    label 1, 2;

    function NoCheck(P: PDirName): Boolean;
      begin
      Inc(FrPos);
      NoCheck := not P^.Check;
      end;

    function IsNetworkPath(const Path: String): Boolean; {KV}
      begin
      IsNetworkPath := ((Length(Path) > 2) and (Path[1] = '/') and // slash change by unxed
             (Path[2] = '/')); // slash change by unxed
      end;

    label
      TryGetInfo;
    begin { MakeDirectories }
    if not CopyPrn then
      begin
      S := CopyDir;
      Inhr := CreateDirInheritance(S, True);
      if Inhr = 0 then
        Inhr := Length(S);
      Drv := Byte(UpCase(S[1]))-64;
      Red := [S[1]];
      end;

    if CopyPrn or (S[1] = '/') then // slash change by unxed
      FreeSpc := 0 // устройство или сетевой адрес
    else
      begin
TryGetInfo:
      if SysDiskSizeLongX(@PathBuffer) < 0 then
        case SysErrorFunc(21, Drv-1) of
          1:
            goto TryGetInfo;
          3:
            Exit;
        end {case};
      end {if};
    New(Dirs, Init(10, 10));
    for I := 0 to Files^.Count-1 do
      begin
      P := Files^.At(I);
      if P^.Attr and Directory <> 0 then
        begin
        FrPos := Dirs^.Count;
        {$IFDEF OS2}
        {JO - для переименования каталога в .LONGNAME исходного каталога}
        if  (CopyOptions and cpoEAToName <> 0) then
          begin
          LogName := '';
          if  (GetEAString(MakeNormName(P^.Owner^, P^.FlName[True]),
                 '.LONGNAME', LogName, False) = 0) and
              (LogName <> '')
          then
            begin
            for I_LN := 1 to Length(LogName) do
              if LogName[I_LN] in [#1..#31, #44, #42, #47, #58, #59,
                 #60, #62, #63, #92, #124]
              then
                LogName[I_LN] := #33;
            CopyI(CopyDir, MakeNormName(P^.Owner^, P^.FlName[True]),
              LogName, P, True, P^.Attr and $3FFF);
            end
          else
            goto 2;
          end
        else
2:
          {$ENDIF}
        CopyI(CopyDir, MakeNormName(P^.Owner^, P^.FlName[True]),
            MkName(P^.FlName[True]), P, True, P^.Attr and $3FFF);

        if not (Abort or CopyCancel)
          and (P^.DIZ <> nil)
          and (CopyOptions and cpoDesc <> 0)
        then
          begin
          {$IFDEF OS2}
          {JO - для импорта описаний при переименовании каталога в .LONGNAME исходного каталога}
          if  (CopyOptions and cpoEAToName = 0) or (LogName = '') then
            {$ENDIF}
            begin
            ExportDiz(KillDescrOf, MkName(P^.FlName[True]), P^.DIZ, CopyDir);
//            {каталоги!!!}ImportDIZ(LowStrg(P^.FlName[CondLfn]),
//                 MkName(P^.FlName[CondLfn]), P^.DIZ, P^.Owner)
            end
            {$IFDEF OS2}
          else
            begin
            ExportDiz(KillDescrOf, LogName, P^.DIZ, CopyDir);
//            ImportDIZ(LowStrg(P^.FlName[True]), LowStrg(LogName), P^.DIZ,
//               P^.Owner);
            end
        {$ENDIF}
          end;
        ;
        end
      else
        begin
        ToDo := ToDo+P^.Size;
        if  (CopyDir[1] <> P^.Owner^[1]) or (CopyOptions and cpoMove = 0)
        then
          begin
          ToDoCopy := ToDoCopy+P^.Size;
          if P^.Size > 0 then
            begin
            ToDoClusCopyTemp := P^.Size/BytesPerCluster;
            ToDoClusCopy := ToDoClusCopy+
                ( {Round}(ToDoClusCopyTemp)+
                Byte( {Round}(ToDoClusCopyTemp)-ToDoClusCopyTemp <> 0)
                )*BytesPerCluster;
            end;
          end;
        end;
      if CopyCancel then
        Break;
      end;
1:
    repeat
      FrPos := -1;
      if Abort then
        Break; {AK155 Abort может установить SysErrorFunc}
      PD := Dirs^.FirstThat(@NoCheck);
      if PD = nil then
        Break;
      CopyF(PD^.DNew, PD^.DOld, PD^.CopyIt, PD^.Attr and $3FFF0);
      {JO EAs - dirs }
      if PD^.CopyIt then
        begin
        {$IFNDEF DPMI32}
        if not CopyPrn then
          CopyEAs(PD^.DOld, PD^.DNew);
        {$ENDIF}
        {Cat SAs - dirs }
        {$IFDEF WIN32}
        if not CopyPrn and (CopyOptions and cpoAccRights <> 0) then
          CopySAs(PD^.DOld, PD^.DNew);
        {$ENDIF}
        end;
      PD^.Check := True;
    until False;
    Info^.Write(5, '');
    end { MakeDirectories };

  procedure __Remove;
    var
      RRC: PStringCollection;

    procedure DoRemove(P: PFileRec);

      procedure MakeMark(P: PFileRec);
        begin
        if  (Copy(P^.Owner^, 1, Length(SSS)) = SSS) then
          P^.Attr := P^.Attr or Marked;
        end;

      begin
      {Cat: раньше следующий кусок выполнялся только для успешно скопированных
      каталогов, т.е. (P^.Attr and Copied <> 0), а надо бы перечитывать все}
      if P^.Attr and Marked = 0 then
        begin
        SSS := CnvString(P^.Owner);
        if SSS[Length(SSS)] = '/' then // slash change by unxed
          SetLength(SSS, Length(SSS)-1);
        if Copy(CopyDir, 1, Length(SSS)) = SSS then
          Inhr := 0;
        RRC^.Insert(NewStr(SSS));
        if P^.Attr and Directory <> 0 then
          Red := Red+[UpCase(SSS[1])];
        Files^.ForEach(@MakeMark);
        end;
      end { DoRemove };

    procedure DoReread(P: PString);
      begin
      RereadDirectory('>'+P^); //'>' - признак перечитывания подкаталогов
      end;

    begin { __Remove }
    RRC := New(PStringCollection, Init($10, $8, False));
    Files^.ForEach(@DoRemove);
    RRC^.ForEach(@DoReread);
    Dispose(RRC, Done);
    end { __Remove };

  procedure DoReset(P: PFileRec);
    var
      s: String;
      i: Integer;
    begin
    CD_Drives := CD_Drives+[UpCase(P^.Owner^[1])];
    if  (CopyOptions and cpoDesc) <> 0 then
      GetDiz(P);
    end { DoReset };

  label 1;

  var
    {$IFDEF DPMI32}
    Flush: Boolean;
    {$ENDIF}
    DrvC: Integer;
    DriveChar: String[1];

    { ***************** }
    { *** FilesCopy *** }
    { ***************** }
  label qqqq;
  begin { FilesCopy }
  SSS := CopyDir;
  MakeNoSlash(SSS);
  Strings.StrPCopy(@PathBuffer, SSS);
  BytesPerCluster := GetBytesPerCluster(@PathBuffer);
  if BytesPerCluster = 0 then
    BytesPerCluster := 512;
  {$IFDEF DualName}
  CondLfn := (FMSetup.Options and fmoDescrByShortNames) = 0;
  {$ELSE}
  CondLfn := True;
  {$ENDIF}

qqqq:
  Dirs := nil;
  AdvCopy := (not CopyPrn)
       and ((SystemData.Options shl 3) and ossAdvCopy <> 0);
  NewTimer(Timer, 0);
  NewTimer(_Tmr, 0);
  GrdClick := True;
  FreeDisk := -1;
  CopyCancel := False;
  SkipAllBad := False;
  ReD := [];
  CD_Drives := [];
  ToRead := 0;
  ToWrite := 0;
  ToDo := 0;
  ToDoCopy := 0;
  ToDoClusCopy := 0;
  R.Assign(0,0,60,15); {John_SW 30-06-2005}
  New(Info, Init(R));
  if CopyOptions and cpoMove <> 0 then
    Info^.Top := GetString(dlFCMove)
  else
    Info^.Top := GetString(dlFCCopy);
  Desktop^.Insert(Info);
  Files^.ForEach(@DoReset);
  if  ( (SystemData.Options shl 3) and ossRemoveCD_RO <> 0)
  then
    begin
    for C := 'A' to 'Z' do
      if  (C in CD_Drives)
        and ((GetDriveTypeNew(C) <> dtnCDRom) // чтобы зря не дёргать диск
//JO: нам здесь важна проверка именно на тип файловой системы,
//    а не на тип устройства, т.к. именно на CDFS все файлы имеют
//    атрибут "read-only", который нужно снимать при копировании,
//    а например для UDF такой необходимости нет
             or (GetFSString(C) <> 'CDFS')) then
          CD_Drives := CD_Drives-[C];
    end
  else
    CD_Drives := [];
  MakeDirectories;
  if Abort then
    CopyCancel := True;
  if CopyCancel then
    goto 1;
  InitMemBuf;
  if MemBuf = nil then
    begin
    Application^.OutOfMemory;
    goto 1;
    end;
  New(CopyQueue, Init(250, 100));
  Info^.Bottom := GetString(dlFC_Total)+FStr(ToDo)+GetString(dlBytes);
  Info^.DrawView;
  DrvC := Drv+64;
  if  (CopyOptions and cpoCheckFree <> 0) and (Files^.Count > 1)
    and (ToDoClusCopy > SysDiskFreeLongX(@PathBuffer))
    and (MessageBox(GetString(erNoDiskSpacePre),
        @DrvC, mfYesButton+mfNoButton) <> cmYes)
  then
    CopyCancel := True;
  if CopyCancel then
    goto 1;
  MaxRead;
  if Abort or CopyCancel then
    begin
    ClrIO;
    if (StopDlgData and 1) <> 0 then
      begin
      CopyCancel := False; // чтобы произошла запись
      MaxWrite;
      end;
    Close(ReadStream.F);
    ClrIO;
    Truncate(WriteStream.F);
    if (IOResult = 0) and ((StopDlgData and 2) = 0) then
      begin
      Close(WriteStream.F);
      lEraseFile(WriteStream);
      end
    else
      SetDateAttr;
    end
  else if (FMSetup.Options and fmoBeep <> 0) and
      (ElapsedTime(Timer) > 30*1000)
  then
    BeepAftercopy;
  Dispose(CopyQueue, Done);
  CopyQueue := nil;
  DoneMemBuf;
1:
  {$IFDEF DPMI32}
  Flush := ((SystemData.Options shl 3) and ossFlushDsk <> 0);
  if Flush then
    begin
    Info^.ClearInterior;
    Info^.Write(5, GetString(dlFlushingBuffers)+'...');
    end
  else
    {$ENDIF}
    Dispose(Info, Done);

  if Dirs <> nil then
    Dispose(Dirs, Done);
  Dirs := nil;
  if CopyOptions and cpoMove <> 0 then
    __Remove;
  for C := 'A' to 'Z' do
    if C in ReD then
      begin
      SSS := C;
      GlobalMessage(evCommand, cmRereadTree, @SSS);
      end;
  SSS := Copy(lFExpand(CopyDir), 1, InhR);
  if  (InhR <> 0) and RR then
    RereadDirectory('>'+SSS); //'>' - признак перечитывания подкаталогов
  if SourcePanel <> nil then
    begin
    { Nil бывает, например, в случае разархивирования по F4 из панели
    архива. Наверно, просто поставить такое условие - неправильно, а надо
    бы таки добраться до подвала и перерисовать его, но я не сумел
    добраться. 17.01.2005}
    {AK155 10.01.2005 После того, как перерисовка панелей при групповых
операциях стала делаться по таймеру, возникла проблема отрисовки
результатов того, что произошло после последнего тика таймера.
}
    SourcePanel^.DrawView;
    PFilePanelRoot(SourcePanel)^.InfoView^.DrawView;
    end;
  {$IFDEF DPMI32}
  if Flush then
    begin
    asm
        mov ah,0dh
        int 21h
      end;
    Dispose(Info, Done);
    end;
  {$ENDIF}
  end { FilesCopy };
{-DataCompBoy-}

const
  ccCopyMode: Byte = cpmAskOver;
  ccCopyOpt: Byte = 0;

var
  DialogLabel: String;
  DialogMoveMode: Boolean;

  {-DataCompBoy-}
function CopyDialog;
  var
    PF: PFileRec;
    D: PDialog;
    P, P1, P2: PView; {JO: P2 - чекбоксы}
    R: TRect;
    S, S1, S4: String;
    DT: record
      S3: String;
      WW: Word;
      WW1: Word;
      end;
    Nm: String;
    Xt: String;
    hFile: LongInt;
    SR: lSearchRec;
    Idx: TDlgIdx;
    I, l: Integer;
    SSS: String;
    C: Char;
    DEr: Boolean;

    IDDQD: record
      Fl, dr: LongInt;
      end;

  procedure DoCalc(P: PFileRec);
    begin
    with IDDQD do
      if P^.Attr and Directory <> 0 then
        Inc(dr)
      else
        Inc(Fl)
    end;

  procedure PrepareDialog(P: PDialog);
    var
      R: TRect;
    begin
    with P^ do
      begin
      if DialogMoveMode then
        begin
        DisposeStr(Title);
        Title := NewStr(GetString(dlFCMove));
        end;
      R.Assign(2, 1, Size.X-3, 2);
      Insert(New(PLabel, Init(R, DialogLabel, DirectLink[1])));
      {JO}
      if  (FMSetup.Options and fmoAlwaysCopyDesc = 0) and
          (SourcePanel <> nil) and
          (PFilePanelRoot(SourcePanel)^.Drive <> nil) and
          (PFilePanelRoot(SourcePanel)^.Drive^.DriveType = dtDisk) and
          (PFilePanelRoot(SourcePanel)^.
            PanSetup^.Show.ColumnsMask and psShowDescript = 0)
      then
        PCheckBoxes(DirectLink[2])^.SetButtonState(cpoDesc, False);
      {/JO}
      end;
    end;

  begin { CopyDialog }
  CopyDialog := False;
  DialogMoveMode := MoveMode;
  DT.WW := ccCopyMode;
  DT.WW1 := ccCopyOpt;
  DT.WW1 := DT.WW1 and not cpoMove;
  if MoveMode and (FromTemp <> 1) then
    DT.WW1 := DT.WW1 or cpoMove;
  if  (FMSetup.Options and fmoAlwaysCopyDesc <> 0) then
    DT.WW1 := DT.WW1 or cpoDesc; {JO}
  if LowMemory then
    Exit;
  if MoveMode then
    S := GetString(dlFCMove1)
  else
    S := GetString(dlFCCopy1);
  DT.S3 := '';
  if Files^.Count = 1 then
    begin
    PF := Files^.At(0);
    S1 := PF^.FlName[True];
    I := 1;
    while I <= Length(S1) do
      begin
      if S1[I] = '~' then
        begin
        System.Insert(#0, S1, I);
        Inc(I);
        end;
      Inc(I);
      end;
    if MoveMode then
      DT.S3 := S1;
    if PF^.Attr and Directory = 0 then
      S1 := GetString(dlDIFile)+' '+S1
    else
      S1 := GetString(dlDIDirectory)+' '+S1;
    end
  else
    begin
    IDDQD.dr := 0;
    IDDQD.Fl := 0;
    Files^.ForEach(@DoCalc);
    if IDDQD.dr = 0 then
      S1 := ItoS(IDDQD.Fl)+' '+#0+GetString(dlDIFiles)+#0
    else if IDDQD.Fl = 0 then
      S1 := ItoS(IDDQD.dr)+' '+#0+GetString(dlDirectories)+#0
    else
      FormatStr(S1, GetString(dlFilDir), IDDQD);
    end;
  if MoveMode then
    DialogLabel := S+S1+GetString(dlFCMove2)
  else
    DialogLabel := S+S1+GetString(dlFCCopy2);
  if MoveMode and (Files^.Count = 1)
  then
    DT.S3 := PFileRec(Files^.At(0))^.FlName[True]
  else
    GlobalMessageL(evCommand, cmPushName, hsFileCopyName);
  S4 := '';
  if SourcePanel <> nil then
    Message(SourcePanel^.Owner, evCommand, cmPushFirstName, @S4);
  if CopyDirName <> '' then
    S4 := CopyDirName;
  if MoveMode and (S4 = cTEMP_) then
    S4 := ''; // Move на TEMP: не бывает
  CopyDirName := '';
  if S4 <> '' then
    begin
    if  (Files^.Count = 1) and
      ((S4[2] = ':') and (Length(S4) > 2) or (Copy(S4, 1, 2) = '\\'))
        and (PFileRec(Files^.At(0))^.Attr and Directory = 0)
    then
      S4 := MakeNormName(S4, PFileRec(Files^.At(0))^.FlName[True]);
    {$IFDEF RecodeWhenDraw}
    S4 := CharToOemStr(S4);
    {$ENDIF}
    HistoryAdd(hsFileCopyName, GetName(S4));
    end
  else if (Files^.Count = 1) then
    begin
    S4 := PFileRec(Files^.At(0))^.FlName[True];
    {$IFDEF RecodeWhenDraw}
    S4 := CharToOemStr(S4);
    {$ENDIF}
    HistoryAdd(hsFileCopyName, S4);
    end;
  DT.S3 := S4;
  with DT do
    begin
    if DT.S3[2] = ':' then
      C := S3[1]
    else
      C := GetCurDrive;
    {Cat:warn могут быть проблемы с сетевыми путями}
    if  SystemData.Drives[C] and ossVerify <> 0  then
      WW1 := WW1 or cpoVerify
    else
      WW1 := WW1 and not cpoVerify;
    if SystemData.Drives[C] and ossCheckFreeSpace <> 0 then
      WW1 := WW1 or cpoCheckFree
    else
      WW1 := WW1 and not cpoCheckFree;
    end;

  if not SkipCopyDialog then
    begin {AK155 6-12-2001. Собрал до кучи всю работу с диалогом }
    @PreExecuteDialog := @PrepareDialog;
    if ExecResource(dlgCopyDialog, DT) = cmCancel then
      Exit;
    end;
  SkipCopyDialog := False;

  {$IFDEF RecodeWhenDraw}
  DT.S3 := OemToCharStr(DT.S3);
  {$ENDIF}
  S := DT.S3;
  CopyMode := DT.WW;
  CopyOpt := DT.WW1;
  DelRight(S);
  DelLeft(S); {AK155}
  {AK155 22-12-2002
   Следующий странный фрагмент посвящен, прежде всего, борьбе с
завуалированной эквивалентностью целевого каталога исходному
(при копировании каталога). Без этого при существующей реализации
копирования каталогов при наличии такой эквивалентности на диске
возникает зацикленная структура каталогов. Например, попробуйте
закомментировать этот кусок, стать на каталог D, нажать F5 и ввести
'D.' или 'D.\.\' . Лучше делать это на RAM-диске :)
Плохо также, когда задается дурацкий вопрос о перезаписи при
копировании файла типа 'f' в 'f..'
   С другой стороны, точки и слэши могут встречаться во вполне
осмысленных сочетаниях типа '..' (ввести руками, чтобы скопировать
вовне) или D:\D\..\file.ext (D&D одиночного файла на '..') или
D:\D\.. (D&D нескольких файлов на '..')
   К этому вопросу прикладывали руки многие, в частности, JO, Cat,
AK155. При этом то восстанавливалось зацикливание, то становилось
невозможным какое-то осмысленное копирование. Данная коррекция
вызвана тем, что в b09 перестал работать D&D одиночного файла
на точечки UpDir.
}
  SSS := S; {AK155 02-01-2003}
  I := Length(S);
  while True do
    begin
    if GetName(S) = '..' then
      Break;
    l := I;
    while S[I] = '.' do
      Dec(I);
    while S[I] = '/' do // slash change by unxed
      Dec(I);
    if l = I then
      Break;
    SetLength(S, I);
    end;
  {/AK155 22-12-2002}

  {AK155 02-01-2003 В результате коррекции от 22-12-2002 получалось
обрезание последней '\', что приводило, например, к невозможности
скопировать в корень диска ('C:\' превращалось в 'C:' и получалось
копирование в текущий каталог). Востановление '\' сделано через ':=',
а не через 'SetLength(S,I+1)', чтобы не закладывать мину на возможный
будущий переход от ShortString к AnsiString }
  if  (Length(SSS) > I) and (SSS[I+1] = '/') then // slash change by unxed
    S := Copy(SSS, 1, I+1);
  {/AK155 02-01-2003}

  ccCopyMode := DT.WW;
  ccCopyOpt := DT.WW1;

  if  (FMSetup.Options and fmoAlwaysCopyDesc = 0) and
      (SourcePanel <> nil) and
      (PFilePanelRoot(SourcePanel)^.Drive <> nil) and
      (PFilePanelRoot(SourcePanel)^.Drive^.DriveType = dtDisk) and
      (PFilePanelRoot(SourcePanel)^.
        PanSetup^.Show.ColumnsMask and psShowDescript = 0)
  then
    CopyOpt := CopyOpt and not cpoDesc; {JO}

  lFSplit(S, CopyDir, Nm, Xt);
  if S = '' then
    Exit;             {<filecopy.001>}
  if not Link then
    begin
    if ArchiveFiles(S, Files, MoveMode, SourcePanel) then
      Exit;
    {$IFDEF ARVID}
    if CopyFilesToArvid(S, Files, MoveMode, SourcePanel) then
      Exit;
    {$ENDIF}
    if UpStrg(Copy(S, 1, PosChar(':', S))) = cTEMP_ then
      begin
      CopyToTempDrive(Files, SourcePanel, '');
      Exit;
      end;
    {$IFDEF MODEM}
    {$IFDEF LINK}
    if UpStrg(Copy(S, 1, PosChar(':', S))) = cLINK_ then
      begin
      Mask := Nm+Xt;
      if  (PosChar('*', Mask) = 0) and (PosChar('?', Mask) = 0) then
        begin
        CopyDir := CopyDir+Mask;
        Mask := ''
        end;
      Delete(CopyDir, 1, 5);
      CopyToLinkDrive(Files, SourcePanel, CopyDir, Mask);
      Exit;
      end;
    {$ENDIF}
    {$ENDIF}
    end;
  SSS := lFExpand(S);
  if Abort then
    Exit;
  Mask := Nm+Xt;
  if  (Mask = '') then
    Mask := x_x
  else if (PosChar('*', Mask) = 0) and (PosChar('?', Mask) = 0) then
    begin
    ClrIO;
    if Length(SSS) = 3 then
      begin
      CopyDir := S+'/'; // slash change by unxed
      Mask := x_x
      end
    else
      begin
      if not ((Files^.Count = 1) and
            (UpStrg(S) = UpStrg(PFileRec(Files^.At(0))^.FlName[True])))
      then
        begin
        lFindFirst(SSS, AnyFileDir, SR); {JO}
        DEr := (DosError <> 0);
        lFindClose(SR);
        if Abort then
          Exit;
        {/AK155
     Если  при  копировании/перемещении  нескольких  файлов в строке
ввода  ввести  несуществующее  имя,  и  не  ввести флажок дополнения
(append),  то первый файл копировался в это имя, а все последующие -
тоже  в  него, что есть явный абсурд. Теперь в этом случае создается
каталог с введенным именем и файлы копируются в него.
}
        if IsDummyDir(S) or (not DEr and (SR.SR.Attr and Directory <> 0))
        then
          begin
          CopyDir := S+'/'; // slash change by unxed
          Mask := x_x;
          end
        else if (DEr and (Files^.Count <> 1)
            and (ccCopyMode and cpmAppend = 0))
        then
          begin
          case AppendQuery(SSS) of
            cmYes:
              CopyMode := cpmAppend;
            cmNo:
              begin
              CopyDir := S+'/'; // slash change by unxed
              Mask := x_x;
              end;
            else {case}
              begin
              CopyDialog := False;
              Exit;
              end;
          end {case};
          end
        end;
      end;
    end;
  if Nm[Length(Nm)] = ':'
  then
    S := UpStrg(Copy(Nm, 1, Length(Nm)-1))+#0
  else
    S := UpStrg(Nm)+#0;
  I := 0;
  if {$IFDEF DPMI32}LfnVP.{$ENDIF}SysFileOpen(@S[1], Open_Access_ReadOnly or open_share_DenyNone,
       hFile) = 0
  then
    begin
    if SysFileIsDevice(hFile) and $FF <> 0 then
      {AK155: 'and $FF' необходимо, так как хелп к SysFileIsDevice
            неправдивый; см. хелп к DosQueryHType}
      I := 1;
    SysFileClose(hFile);
    end;
  if  (I <> 0) then
    begin
    CopyMode := cpmOverwrite;
    CopyDir := SSS;
    Mask := '';
    CopyPrn := True;
    end
  else
    begin
    CopyDir := lFExpand(CopyDir);
    MakeSlash(CopyDir);
    CopyPrn := False;
    end;
  CopyDialog := True;
  end { CopyDialog };
{-DataCompBoy-}

{-DataCompBoy-}
procedure CopyFiles;

  var
    CopyDir: String;
    Mask: String;
    CopyOpt: Word;
    CopyMode: Word;
    CopyPrn: Boolean;
    {$IFDEF COPYTIMER}
    ev: TEvent;
    {$ENDIF}
  begin
  CtrlBreakHit := False;
  Files^.Pack;
  if Files^.Count <= 0 then
    Exit;
  if not CopyDialog(CopyDir, Mask, CopyOpt, CopyMode, CopyPrn,
      MoveMode, Files, FromTemp, SourcePanel, False)
  then
    Exit;
  if  (FromTemp > 0) and MoveMode then
    begin
    CopyOpt := CopyOpt or cpoFromTemp;
    end;
  {$IFDEF COPYTIMER}
  DDTimer := GetCurMSec;
  {$ENDIF}
  Inc(SkyEnabled);
  LongWorkBegin;
  NotifySuspend; {Cat}
  FilesCopy(Files, SourcePanel, CopyDir, Mask, CopyMode, CopyOpt,
            CopyPrn, True);
  NotifyResume; {Cat}
  LongWorkEnd;
  Dec(SkyEnabled);
  {$IFDEF COPYTIMER}
  DDTimer := GetCurMSec-DDTimer;
  ev.What := evCommand;
  ev.Command := cmShowTimeInfo;
  ev.InfoPtr := nil;
  Application^.PutEvent(ev);
  {$ENDIF}
  end { CopyFiles };

{JO}
procedure CopyDirContent(Source, Destination: String;
    MoveMode, Forced: Boolean);
  var
    SR: lSearchRec;
    FC: PFilesCollection;
    FR: PFileRec;
  begin
  MakeSlash(Source);
  New(FC, Init($10, $10));
  ClrIO;
  lFindFirst(Source+x_x, AnyFileDir, SR);
  while (DosError = 0) and not Abort do
    begin
    if not IsDummyDir(SR.FullName) then
      FC^.AtInsert(FC^.Count, NewFileRec(SR.FullName,
          {$IFDEF DualName}
          SR.SR.Name,
          {$ENDIF}
          SR.FullSize,
          SR.SR.Time,
          SR.SR.CreationTime,
          SR.SR.LastAccessTime,
          SR.SR.Attr,
          @Source));
    lFindNext(SR);
    end;
  lFindClose(SR);
  ClrIO;
  FC^.Pack;
  if FC^.Count <= 0 then
    begin
    Dispose(FC, Done);
    Exit;
    end;

  MakeSlash(Destination);
  Inc(SkyEnabled);
  LongWorkBegin;
  NotifySuspend;
  FilesCopy(FC, nil, Destination, x_x, cpmAskOver*Byte(not Forced),
    cpoMove*Byte(MoveMode), False, False);
  NotifyResume;
  LongWorkEnd;
  Dec(SkyEnabled);
  FC^.DeleteAll;
  Dispose(FC, Done);
  end { CopyDirContent };
{/JO}

procedure CloseWriteStream;
  begin
  Truncate(WriteStream.F);
  Close(WriteStream.F);
  ClrIO;
  end;

end.
