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
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}
{&Delphi+}
unit FlPanel;

interface

uses
  drivers2, math, Defines, Streams, Views, Drivers, FilesCol,
  FlPanelX, Objects, TopView
  ;

type
  PFilePanel = ^TFilePanel;
  TFilePanel = object(TFilePanelRoot)
    procedure Draw; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetPalette: PPalette; virtual;
    procedure DrawTop(var B); virtual;
    end;

  TPanelBottomDnD = record
    TotalY, SelectedY, CurrentY1, CurrentY2: Byte;
    end;

const
  MaxFooterHeight = 4;

type
  PInfoView = ^TInfoView;
  TFooterProc = function(IV: PInfoView): Boolean;
    { Процедура формирования (части) строки подвала. Для каждой
    строки таких процедур может быть задано несколько, они вызываются
    по порядку.
      Результат True обозначает, что формирование строки закончено и
    последующие процедуры не вызываются. }
  TInfoView = object(TView)
    Panel: PFilePanel;
    DnD: TPanelBottomDnD;
    LineMaker: array[0..MaxFooterHeight] of array[0..6] of TFooterProc;
      {` Для каждой строки подвала, начиная с разделителя,
       последовательность процедур формирования этой строки.
       Каждая последовательность завершается nil.`}
    constructor Init(R: TRect);
    procedure Compile(Value: Word;
      FullProc, BriefProc: TFooterProc);
    procedure CompileShowOptions;
    procedure Draw; virtual;
    {AK155: Этот метод нагружен двумя важными побочными эффектами:
        он в соответствии с настройками устанавливает свой Size.Y и
        заполняет DnD координатами строк, из которых возможен D&D.
        Первое используется в TFilePanelRoot.ChangeBounds,
        второе в HandleEvent}
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PDirView = ^TDirView;
  TDirView = object(TTopView)
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetText(MaxWidth: Integer): String; virtual;
    end;

  PDriveLine = ^TDriveLine;
  {`2 }
  TDriveLine = object(TView)
    Panel: PFilePanel;
    DriveLine: String[29];
    ViewLine: String[60];
    CharDelta: AInt;
    LogDrvMap: LongInt; {Cat}
    constructor Init(var R: TRect; APanel: PFilePanel);
    procedure MakeDriveLine;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Draw; virtual;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    procedure Refresh; {Cat}
    procedure Update; virtual;
     {` AK155 18.03.2005 Раньше настройка автообновления строки дисков
     срабатывала только при загрузке. Это было вызвано тем, что
     при смене этой настройки сложно "со стороны" включить полосы дисков
     всех панелй в список автообновления (RegisterToBackground).
       Сейчас я сделал просто, хотя и горбато: RegisterToBackground
     делается при создании и навсегда, а Update фактичсески делает
     опрос и обновление только при включённой настройке. `}
    procedure ShiftLetter(d: Integer);
      {` Сдвинуть Panel^.DriveLetter вправо (d=1) или влево (d=-1),
        циклически. `}
    end;
    {`}

const
  CPanel = #6#7#8#9#10#32#33#34#35#36#37#38#39#40#44#45#46#47#48; {JO}
  CTopView = #11#12;
  CInfoView = #25#26#27#28#29#30#31#49#50;
  CDriveLine = #41#42#43;

(* AK155 16.05.05
   Я не понял, зачем вообще здесь нужны эти определения, поскольку
в uses интерфейсной части FlPanelX есть и все эти переменные видны
без всяких ухищрений. И вдвойне я не понял, зачем нужно было их помещать
в интерфейсной части с типами, отличающимися от оригинальных
(типа Pointer вместо PFilesPanelRoot), так как после этого в прочих
модулях получается, что тип этих пероеменных зависит от порядка
FlPanelX и FlPanel в их uses. Так что убираю эту секцию нафиг.
var
  ActivePanel: Pointer absolute FlPanelX.ActivePanel;
  PassivePanel: Pointer absolute FlPanelX.PassivePanel;
  CtrlWas: Boolean absolute FlPanelX.CtrlWas;
  DirsToChange: array[0..9] of PString absolute FlPanelX.DirsToChange;
var
  CurrentDirectory: String absolute FlPanelX.CurrentDirectory;
/AK155 *)

implementation

uses
  Files, VpSysLow, Dos, Eraser, Drives, DNHelp, TitleSet,
  Lfnvp, DNUtil, DNApp, Advance, Advance1, Advance2, Advance3, Startup,
  Memory, FileCopy, Messages, Menus, DiskInfo, Dialogs, Commands,
  HistList, Tree, FBB, ArcView, CmdLine, Histries, Archiver,
  Gauges, Gauge, FileFind, FLTools, DnIni, XDblWnd, DblWnd, Filediz
  {$IFDEF UUENCODE}
  , UUCode
  {$ELSE}
  {$IFDEF  UUDECODE}
  , UUCode
  {$ENDIF}
  {$ENDIF} {, Crt}
  , xTime
  , PDSetup, VPUtils
  {$IFDEF UUENCODE}
  , netbrwsr
  {$ENDIF}
  ;

var
  QSLastSuccessPos: Integer;
    {` Последняя успешная позиция быстрого поиска. Это собственная
    (для быстрогно поиска) копия LastSuccessPos, который может изменяться
    в результате других сопоставлений с маской (например, при
    автообновлении панелей). `}

constructor TDriveLine.Init;
  begin
  inherited Init(R);
  Panel := APanel;
  EventMask := evMouse or evBroadcast;
  MakeDriveLine;
  CharDelta := 1;
  // fixme: commented by unxed
  //LogDrvMap := SysGetValidDrives;
  //UpdTicks := 3000;
  //RegisterToBackground(@Self);
  end;

constructor TDriveLine.Load(var S: TStream);
  begin
  inherited Load(S);
  MakeDriveLine;
  CharDelta := 1;
  GetPeerViewPtr(S, Panel);
  // fixme: commented by unxed
  //UpdTicks := 3000;
  //RegisterToBackground(@Self);
  end;

procedure TDriveLine.MakeDriveLine;
  var
    C: Char;
  begin
  DriveLine := '';
  for C := 'A' to 'Z' do
    if ValidDrive(C) then
      DriveLine := DriveLine + C;
  DriveLine := DriveLine + chTempDrive
              {$IFDEF NetBrowser}
               + chNetDrive
              {$ENDIF};
  end;

function TDriveLine.GetPalette;
  const
    S: String[Length(CDriveLine)] = CDriveLine;
  begin
  GetPalette := @S;
  end;

procedure TDriveLine.Draw;
  var
    B: TDrawBuffer;
    M: Byte absolute DriveLine;
    I: Integer;
    SDir: String;
  begin
  I := M*2+3;
  if  (Panel^.Size.X >= I) then
    begin
    if  (Size.X <> I) then
      begin
      GrowTo(I, 1);
      Exit;
      end;
    ViewLine := '';
    for I := 1 to Length(DriveLine) do
      ViewLine := ViewLine+' '+DriveLine[I];
    ViewLine := '['+ViewLine+' ]';
    end
  else
    begin
    I := 2+M;
    if Panel^.Size.X-2 >= I then
      if  (Size.X <> I)
      then
        begin
        GrowTo(I, 1);
        Exit;
        end
      else
        ViewLine := '['+DriveLine+']'
    else if Panel^.Size.X <> Size.X+2
    then
      begin
      GrowTo(Panel^.Size.X-2, 1);
      CharDelta := 1;
      Exit;
      end
    else
      begin
      ViewLine := Copy(DriveLine, CharDelta, Size.X-2);
      if Length(ViewLine) < Size.X-2 then
        ViewLine := ViewLine+Copy(DriveLine, 1, Size.X-2-Length(ViewLine));
      ViewLine := '{'+ViewLine+'}';
      end;
    end;
  MoveStr(B, ViewLine, GetColor(1));
  I := PosChar(Panel^.DriveLetter, ViewLine);
  if I > 0 then
    WordRec(B[I-1]).Hi := GetColor(3);

  WordRec(B[0]).Hi := GetColor(2);
  WordRec(B[Size.X-1]).Hi := GetColor(2);
  WriteLine(0, 0, Size.X, 1, B);
  end { TDriveLine.Draw };

procedure TDriveLine.HandleEvent;
  var
    P: TPoint;

  { AK155 29.01.06 }
  procedure BracketClick(T: TPanelNum);
    var
      TargetPanel: PView;
      Manager: PDoubleWindow;
    const
      HideCommand: array[TPanelNum] of Word =
         (cmHideLeft, cmHideRight);
    begin
    Manager := PDoubleWindow(Panel^.Owner);
    { Если панель распахнута, то операция применяется к ней, независимо
      от того, правая или левая скобка была нажата. Так что подгоняем
      T к внутренннему номеру данной панели }
    if Manager^.PanelZoomed then
      T := Manager^.Panel[pRight].AnyPanel^.GetState(sfSelected);
    TargetPanel := Manager^.Panel[T].AnyPanel;
    if (Event.Buttons and mbLeftButton <> 0) or
      not TargetPanel^.GetState(sfVisible)
    then { левая кнопка мыши или операция со скрытой панелью:
           скрытие/показ, как Ctrl-F1/F2}
      Message(Owner, evCommand, HideCommand[T], nil)
    else
      begin { правая кнопка мыши: активизация и
          распахивание/восстановление (как Alt-Ctrl-Z) }
      TargetPanel^.Select;
      Message(TargetPanel^.Owner, evCommand, cmMaxi, nil);
      end;
    end;

  procedure Scroll(D: Integer{+1 или -1});
    begin
    repeat
      CharDelta := 1 + ((CharDelta+D-1+Length(DriveLine)) mod Length(DriveLine));
      DrawView;
    until not MouseEvent(Event, evMouseAuto);
    end;
  { /AK155 }

  begin { TDriveLine.HandleEvent }
  inherited HandleEvent(Event);
  case Event.What of
    evBroadcast:
      case Event.Command of
        cmDropped:
          if MouseInView(PCopyRec(Event.InfoPtr)^.Where) then
            begin
            Event.What := evNothing;
            ClrIO;
            MakeLocal(PCopyRec(Event.InfoPtr)^.Where, P);
            if ViewLine[P.X+1] = ' ' then
              {Dec(P.X);}
              begin { между буквами - игнорируем. Это лучше, чем промахнуться }
              ClearEvent(Event); Exit;
              end;
            case ViewLine[P.X+1] of
              chTempDrive:
                CopyDirName := cTEMP_;
          {$IFDEF NetBrowser}
              chNetDrive:
                begin { на панельку сети ничего не копируем }
                ClearEvent(Event); Exit;
                end;
          {$ENDIF}
              'A'..'Z':
                CopyDirName := ViewLine[P.X+1]+':';
              else {case}
                Exit;
            end {case};
            SkipCopyDialog := Confirms and cfMouseConfirm = 0;
            if ReflectCopyDirection
            then
              RevertBar := (Message(Desktop, evBroadcast,
                     cmIsRightPanel, Panel) <> nil)
            else
              RevertBar := False;
            Message(PCopyRec(Event.InfoPtr)^.Owner,
              evBroadcast, cmCopyCollection,
              PCopyRec(Event.InfoPtr)^.FC);
            SkipCopyDialog := False;
            end;
      end {case};
    evMouseDown:
      begin
      ClrIO;
      MakeLocal(Event.Where, P);
      if ViewLine[P.X+1] = ' ' then
        {Dec(P.X);}
        begin { между буквами - игнорируем. Это лучше, чем промахнуться }
        ClearEvent(Event); Exit;
        end;
      if Event.Double then
        begin
        case ViewLine[P.X+1] of
          'A'..'Z':
            Panel^.ChDir(ViewLine[P.X+1]+':\');
          '}':
             Scroll(+1); // без этого быстрые клики на скобке не работают
          '{':
             Scroll(-1); // аналогично
        end {case};
        end
      else if ViewLine[P.X+1] = Panel^.DirectoryName[1]
      then
        Message(Panel, evCommand, cmRereadDir, @Panel^.DirectoryName)
      else
        case ViewLine[P.X+1] of
          chTempDrive:
            begin
            FreeStr := cTEMP_;
            Message(Panel, evCommand, cmChangeDrv, @FreeStr);
            end;
        {$IFDEF NetBrowser}
          chNetDrive:
            begin
            FreeStr := cNET_;
            Message(Panel, evCommand, cmChangeDrv, @FreeStr);
            end;
        {$ENDIF}
          'A'..'Z':
            begin
            FreeStr := ViewLine[P.X+1]+':';
            Message(Panel, evCommand, cmChangeDrv, @FreeStr);
            end;
          '[':
             BracketClick(pLeft);
          ']':
             BracketClick(pRight);
          '}':
             Scroll(+1);
          '{':
             Scroll(-1);
        end {case};
      ClearEvent(Event);
      end;
  end {case};
  end { TDriveLine.HandleEvent };

procedure TDriveLine.Store(var S: TStream);
  begin
  inherited Store(S);
  PutPeerViewPtr(S, Panel);
  end;

procedure TDriveLine.Refresh;
  var
    newLogDrvMap: LongInt;
  begin
  newLogDrvMap := 0;
  // fixme: commented by unxed
  //newLogDrvMap := SysGetValidDrives;
  if newLogDrvMap <> LogDrvMap then
    begin
    LogDrvMap := newLogDrvMap;
    MakeDriveLine;
    DrawView;
    end;
  end;

procedure TDriveLine.Update;
  begin
  if (FMSetup.Options and fmoAutorefreshDriveLine) <> 0 then
    Refresh;
  end;

procedure TDriveLine.ShiftLetter(d: Integer);
  var
    i, l: Integer;
  begin
  Refresh;
  i := PosChar(Panel^.DriveLetter, DriveLine);
  l := Length(DriveLine);
  if i = 0 then
    begin
    if d < 0 then
      i := PosChar(chTempDrive, DriveLine)
    else
      i := l;
    end;
  Panel^.DriveLetter := DriveLine[1 + (i + l - 1 + d) mod l];
  end;

{                                 TFilePanel                                 }
{----------------------------------------------------------------------------}
function TFilePanel.GetPalette;
  const
    S: String[Length(CPanel)] = CPanel;
  begin
  GetPalette := @S;
  end;

var
  Idx, CurPos, i, j: LongInt;
  C, C1, C2, C3, C4, C5, C6, C7, C8, C9: Byte;
  C2_3, C8_9: AWord;
  B: array[0..300] of AWord;
  B1: array[0..200] of record
    C: Char;
    A: Byte;
    end absolute B;

procedure TFilePanel.DrawTop;
  var
    S: String;
    I, J: Integer;
    C: Word;
  begin
  Drive^.MakeTop(S);
  I := 0;
  C := GetColor($0206);
  J := CStrLen(S);
  while I < Size.X do
    begin
    MoveCStr(TAWordArray(B)[I], S, C);
    Inc(I, J);
    end;
  end;

{-DataCompBoy-}
procedure TFilePanel.SetState;

  procedure MakeChange;
    var
      Event: TEvent;
      {A: Boolean;}
    begin
    CurrentDirectory := DirectoryName;
    ClrIO;
    NeedAbort := True;
    lChDir(CurrentDirectory);
    {A := Abort;}

    DriveState := dsActive;

    if IOResult <> 0 then
      begin
      DriveState := DriveState and dsInvalid;
      Exit;
      end;
    DriveState := dsActive;

    NeedAbort := DriveState and dsInvalid <> 0;

    Message(CommandLine, evCommand, cmRereadInfo, nil);
    {Cat:warn где-то в промежутке от b4.09 к b4.13 произошли какие-то изменения,
      скорее всего, связанные с командной строкой, в результате которых стал
      вылезать этот кусок. Вместе с тем, осуществляемые здесь действия на мой
      взгляд достаточно безумны, так что закомментировал их}
    (*
     if (UpStrg(ActiveDir) <> UpStrg(DirectoryName)) or A then
      begin
       Drive^.lChDir(ActiveDir);
       ReadDirectory;
       CurrentDirectory := DirectoryName;
       Event.What := evKeyDown;
       Event.KeyCode := kbTab;
       Event.InfoPtr := nil;
       PutEvent(Event);
      end;
*)
    Message(Owner, evCommand, cmChangeTree, @DirectoryName);
    end { MakeChange };

  var
    DoDraw: Boolean;

  begin { TFilePanel.SetState }
  inherited SetState(AState, Enable);
  { AK155 Подвал и строку дисков надо прятать синхронно с панелью.
    Раньше этим занимался HideView }
  if (AState and sfVisible) <> 0 then
    begin
    if (InfoView <> nil) then
      InfoView^.SetState(sfVisible, Enable);
    if DriveLine <> nil then
      DriveLine^.SetState(sfVisible,
        Enable and (FMSetup.Show and fmsDriveLine <> 0));
    end;
  {/AK155  3-02-2004, 23-05-2005}
  DoDraw := False;
  if QuickSearch and (AState and
         (sfFocused+sfActive+sfVisible+sfSelected) <> 0) and not Enable
  then
    begin
    DoDraw := True;
    StopQuickSearch;
    end;
  if  (AState and sfActive <> 0) then
    if not Enable then
      begin
      DisableCommands(PanelCommands);
      if not GetState(sfActive+sfSelected) and
          (ScrollBar <> nil) and ScrollBar^.GetState(sfVisible)
      then
        ScrollBar^.Hide;
      DoDraw := True;
      end
    else
      EnableCommands(PanelCommands);
  if  (AState and sfSelected <> 0) and Enable then
    begin
    ActivePanel := @Self;
    if Owner <> nil then { nil бывает во время Load }
      PassivePanel := OtherFilePanel(@Self);
    end;
  if  (AState and sfFocused and State <> 0) then
    if Enable then
      begin
      DoDraw := True;
      if ScrollBar <> nil then
        begin
        ScrollBar^.Show;
        ScrollBar^.Options := ScrollBar^.Options or ofPostProcess;
        end;
      if InfoView <> nil then
        InfoView^.DrawView;
      if DirView <> nil then
        DirView^.DrawView;
      if  (Drive^.DriveType = dtDisk) then
        begin
        AddToDirectoryHistory(DirectoryName, Integer(dtDisk));
        if UpStrg(CurrentDirectory) <> UpStrg(DirectoryName)
        then
          MakeChange;
        end
      else if Drive^.DriveType = dtArc
      then
        Drive^.lChDir(#0);
      EnableCommands(PanelCommands);
      end;
  if GetState(sfFocused) then
    begin
    if AState and sfFocused <> 0 then
      DrawView;
    EnableCommands(PanelCommands);
    if  (ScrollBar <> nil) and not ScrollBar^.GetState(sfVisible) then
      begin
      ScrollBar^.Show;
      ScrollBar^.Options := ScrollBar^.Options or ofPostProcess;
      end;
    end;
  if AState and (sfSelected+sfActive) <> 0 then
    if GetState(sfSelected+sfActive) then
      begin
      if  (Drive^.DriveType = dtDisk)
      then
        MakeChange
      else if Drive^.DriveType = dtArc
      then
        Drive^.lChDir(#0);
      DoDraw := True;
      EnableCommands(PanelCommands)
      end
    else
      begin
      if ScrollBar <> nil then
        begin
        ScrollBar^.Hide;
        ScrollBar^.Options := ScrollBar^.Options and (not ofPostProcess);
        end;
      DoDraw := True;
      if InfoView <> nil then
        InfoView^.DrawView;
      if DirView <> nil then
        DirView^.DrawView;
      end;
  if DoDraw then
    begin
    PosChanged := False;
    DrawView;
    end;
  if GetState(sfFocused) then
    SetTitle(DirectoryName);
  end { TFilePanel.SetState };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFilePanel.Draw;
  label 1;
  var
    P: PFileRec;
    CW: AWord;
    CS: AWord;
    PgS: AWord;
    S, S1: String;
    HLC: array[0..ttUpDir] of Byte; {JO, AK155}
    PB: ^Byte;
    TagC: Char;

  procedure DrawAtIdx;
    var
      P: PFileRec;
      CC: Byte;
      JJ: LongInt;
      CursorPos: Integer;
      S: String;
      RLimit: Integer;
    label Scroll;
    begin
    JJ := j*LineLength;
    if  (Files <> nil) and (Idx < Files^.Count) then
      begin
      P := Files^.At(Idx);
      CC := HLC[P^.TType];
      if  (Idx = CurPos) then
        begin
        LastCurPos.X := JJ;
        LastCurPos.Y := i+1
        end;
      if Idx = CurPos then
        if P^.Selected then
          begin
          C := C5;
          CC := C5
          end
        else
          C := C4
      else if P^.Selected then
        begin
        C := CC;
        CC := C3
        end
      else
        C := CC;
      MoveChar(B[JJ], ' ', C, LineLength);
      if  (Idx = CurPos) and GetState(sfFocused) then
        begin
        Drive^.GetFull(B[JJ], P, C shl 8+C, Cs and $00FF+C shl 8);
        end
      else
        Drive^.GetFull(B[JJ], P, CC shl 8+C, Cs);
      if QuickSearch and (Idx = CurPos) then
        begin
        if LFNLonger250 then
          Inc(JJ, CalcLengthWithoutName);
        if (QSLastSuccessPos > flnNLength) then {курсор в поле расширения }
          begin
          CursorPos := flnDotPos + QSLastSuccessPos - flnNLength - 2;
          if CursorPos >= LFNLen - Byte(flnPanelName[LFNLen] = #16) then
            goto Scroll; { курсор ушёл вправо за границу }
          end
        else if QSLastSuccessPos <= flnNSize then {курсор в поле имени }
          CursorPos := QSLastSuccessPos - 1
        else { курсор за пределами имени }
Scroll:
          begin
          {Символ, на котором должен быть курсор, не виден.
           Выводим имя заново без табуляции и с обрезкой, если надо.
           Если обрезка справа есть, то курсор помещаем не правее
           последнего символа перед обрезкой. Если обрезки справа нет,
           то курсор отпускаем вправо аж до линии за колонкой. }
          S := P^.FlName[uLFN];
          RLimit := Max(-1, QSLastSuccessPos - Length(S));
            {Правый предел позиции курсора относительно LFNLen;
             меняется от -1 до +1 }
          if QSLastSuccessPos > LFNLen  then
            begin
            System.Delete(S, 2, QSLastSuccessPos - LFNLen - RLimit);
            S[1] := #17;
            CursorPos := LFNLen - 1 + RLimit;
            end
          else
            CursorPos := QSLastSuccessPos-1;
          S := FormatLongName(S, LFNLen, 0,
            flnPadRight or flnHandleTildes, nfmNull);
          MoveCStr(B[JJ], S, C);
          end;
        SetCursor(JJ+CursorPos, i);
        ShowCursor;
        NormalCursor
        end;
      end
    else
      begin
      MoveChar(B[JJ], ' ', C1, LineLength);
      GetEmpty(B[JJ], Cs, False);
      end;
    end { DrawAtIdx };

  var
    ColumnTitles: Boolean;

  begin { TFilePanel.Draw }
  if DrawDisableLvl > 0 then
    Exit;
  if DrawDisableLvl < 0 then
    DrawDisableLvl := 0;
  if Loaded then
    RereadDir;
  LineLength := CalcLength;

  if  (DriveState and dsInvalid > 0) then
    begin
    C1 := GetColor(1); { Normal }
    for i := 0 to pred(Owner^.Size.Y) do
      begin
      MoveChar(B, ' ', C1, Size.X);
      WriteLine(0, i, Owner^.Size.X, 1, B[0]);
      end;
    Exit;
    end;

  if Size.X > LineLength-1 then
    DeltaX := 0;
  if UpStrg(OldDirectory) <> UpStrg(DirectoryName) then
    begin
    if  (UpStrg(OldDirectory[1]) <> UpStrg(DirectoryName[1]))
         and (OldDirectory <> '')
    then
      ScrollBar^.SetValue(0);
    PosChanged := False;
    DecDrawDisabled;
    OldDirectory := DirectoryName;
    end;
  C1 := GetColor(1); { Normal }
  C2 := GetColor(2); { Separator }
  C3 := GetColor(3); { Selected }
  C4 := C1;
  C5 := C3;
  HLC[0] := C1;
  HLC[ttUpDir] := C1;
  if FMSetup.TagChar[1] <> ' ' then
    TagC := FMSetup.TagChar[1]
  else
    TagC := #251;
  if Startup.FMSetup.Show and fmsHiliteFiles <> 0 then
    for i := 1 to ttCust10 do
      HLC[i] := GetColor(6+i) {JO}
  else
    for i := 1 to ttCust10 do
      HLC[i] := C1; {JO}
  CS := 179+C2 shl 8;
  CW := 32+C2 shl 8;

  ColumnTitles := (Pansetup^.Show.MiscOptions and 2) <> 0;
  PgS := (Size.Y-Byte(ColumnTitles))
        *((Size.X+1) div LineLength);
  if PgS = 0 then
    PgS := (Size.Y-Byte(ColumnTitles));
  ScrollBar^.PgStep := PgS;
  if Delta < 0 then
    Delta := 0;
  if (Files <> nil {бывает, например, при входе в битый zip-архив} )
    and (Files^.Count > 0)
  then
    begin
    CurPos := ScrollBar^.Value;
    if CurPos < Delta then
      Delta := CurPos;
    if CurPos >= Delta+PgS then
      Delta := CurPos-PgS+1;
    end
  else
    CurPos := -1;
  if GetState(sfFocused) then
    begin
    C4 := GetColor(4); { Normal cursor }
    C5 := GetColor(5); { Selected cursor }
    end
  else if CurPos >= 0 then
    begin
    P := Files^.At(CurPos);
    if Startup.FMSetup.Show and fmsHiliteFiles <> 0 then
      C4 := HLC[P^.TType];
    end;
  if not QuickSearch then
    HideCursor;
  if PosChanged and (OldDelta = Delta) then
    begin
    if CurPos <> OldPos then
      begin
      for i := Byte(ColumnTitles) to Size.Y-1
      do
        for j := 0 to Size.X div LineLength do
          begin
          Idx := i-Byte(ColumnTitles)+
            j*(Size.Y-Byte(ColumnTitles))+Delta;
          if  (Idx = CurPos) or (Idx = OldPos) then
            begin
            MoveChar(B, ' ', C1, Size.X);
            DrawAtIdx;
            Idx := (j+1)*LineLength;
            if Idx < Size.X then
              B[Idx-1] := CS
            else
              B[Idx-1] := CW;
            Idx := j*LineLength;
            WriteLine(Idx, i, LineLength-1, 1, B[Idx+DeltaX]);
            end;
          end;
      end
    else
      goto 1;
    PosChanged := False;
    OldDelta := Delta;
    OldPos := CurPos;
    Exit;
    end;
1:
  OldDelta := Delta;
  OldPos := CurPos;
  PosChanged := False;
  if ColumnTitles then
    begin
    DrawTop(B);
    if WordRec(B[Size.X-1]).Lo = 179 then
      WordRec(B[Size.X-1]).Lo := 32;
    WriteLine(0, 0, Size.X, 1, B[DeltaX]);
    end;
  for i := Byte(ColumnTitles) to Size.Y-1 do
    begin
    MoveChar(B, ' ', C1, Size.X);
    for j := 0 to Size.X div LineLength do
      begin
      Idx := i-Byte(ColumnTitles)+
        j*(Size.Y-Byte(ColumnTitles))+Delta;
      DrawAtIdx;
      Idx := (j+1)*LineLength;
      if Idx < Size.X then
        B[Idx-1] := CS
      else
        B[Idx-1] := CW;
      end;
    WriteLine(0, i, Size.X, 1, B[DeltaX]);
    end;
  if GetState(sfFocused) then
    SetTitle(DirectoryName);
  end { TFilePanel.Draw };
{-DataCompBoy-}

{                                 TInfoView                                  }
{----------------------------------------------------------------------------}
constructor TInfoView.Init;
  begin
  inherited Init(R);
  EventMask := evMouse;
  end;

constructor TInfoView.Load;
  begin
  inherited Load(S);
  GetPeerViewPtr(S, Panel);
  end;

procedure TInfoView.Store;
  begin
  inherited Store(S);
  PutPeerViewPtr(S, Panel);
  end;

procedure TInfoView.HandleEvent;
  var
    P: TPoint;
    Y: Integer;
    Mover: PView;
    S: String;
    FC: PFilesCollection;
    C: TCopyRec;

  procedure CE;
    begin
    ClearEvent(Event)
    end;

  procedure DragCurrent;
    begin
    with Panel^ do
      begin
      S := Cut(PFileRec(Files^.At(ScrollBar^.Value))^.FlName[uLfn], 20);
      {DataCompBoy}
      if S = '..' then
        Exit;
      end;
    New(FC, Init(1, 100));
    FC^.Insert(CopyFileRec(Panel^.Files^.At(Panel^.ScrollBar^.Value)));
    if P.X < Length(S) then
      Dec(P.X);
    MakeGlobal(P, P);
    DragMover(@P, S, FC, @C);
    CE;
    end;

  procedure DragSelected;
    var
      I: LongInt;
      PF: PFileRec; {DataCompBoy}
    begin
    if Panel^.SelNum = 0 then
      Exit;
    New(FC, Init(Panel^.SelNum, 100));
    for I := 1 to Panel^.Files^.Count do
      begin
      PF := Panel^.Files^.At(I-1); {DataCompBoy}
      if PF^.Selected then
        FC^.Insert(CopyFileRec(PF)); {DataCompBoy}
      end;
    MakeGlobal(P, P);
    DragMover(@P, ItoS(Panel^.SelNum)+GetString(dlSelectedFiles), FC, @C);
    CE;
    end;

  procedure DragTotals;
    var
      N, I, Start: LongInt;
    begin
    Start := FirstNameNum(Panel)-1;
    with Panel^.Files^ do
      begin
      N := Count-Start;
      New(FC, Init(N, 100));
      for I := Start to N+Start-1 do
        FC^.AtInsert(FC^.Count, CopyFileRec(PFileRec(At(I))));
      end;
    MakeGlobal(P, P);
    DragMover(@P, ItoS(N)+' '+GetString(dlDIFiles), FC, @C);
    CE;
    end;

  begin {TInfoView.HandleEvent}
  inherited HandleEvent(Event);
  if ((FMSetup.Options and fmoDragAndDrop) <> 0) and
     (Event.What and (evMouseDown+evMouseAuto) <> 0)
  then
    begin
    if Panel^.Files^.Count = 0 then
      Exit;
    C.Owner := Panel;
    MakeLocal(Event.Where, P);
    Y := 0;
    if Y = P.Y then
      begin { мышь в разделителе }
      if Panel^.Files^.Count = 0 then
        Exit;
      if  (P.X >= Panel^.SelectedInfoInDividerMin) and
          (P.X <= Panel^.SelectedInfoInDividerMax)
      then
        begin
        DragSelected;
        Exit;
        end; {AK155}
      if  (P.X >= Panel^.TotalInfoInDividerMin) and
          (P.X <= Panel^.TotalInfoInDividerMax)
      then
        begin
        DragTotals;
        Exit;
        end; {AK155}
      if Panel^.GetState(sfActive) and not Panel^.GetState(sfSelected)
      then
        Panel^.Select; {AK155}
      if Panel^.GetState(sfActive+sfSelected) then
        begin
        with Panel^ do
          begin
          MSelect := Event.Buttons and mbRightButton <> 0;
          if MSelect then
            begin
            SelectFlag := not PFileRec
              (Files^.At(ScrollBar^.Value))^.Selected;
            Message(Panel, evKeyDown, kbIns, nil);
            end;
          end;
        RepeatDelay := 0;
        repeat
          Message(Self.Owner, evKeyDown, kbDown, nil);
        until not MouseEvent(Event, evMouseMove+evMouseAuto);
        RepeatDelay := 2;
        Panel^.MSelect := False;
        end;
      Exit
      end;
    Inc(Y);

    { D&D из подвала панели }
    if  (P.Y = DnD.CurrentY1) or (P.Y = DnD.CurrentY2) then
      DragCurrent
    else if P.Y = DnD.SelectedY then
      DragSelected
    else if P.Y = DnD.TotalY then
      DragTotals;
    CE;
    end;
  end { TInfoView.HandleEvent };

var
  Y: Word;
  PF: PFileRec;  { описатель текущего файла }
  BriefL1: Integer;
    { Длина слева, уже занятая краткой информацией в разделителе}
  CDivdier: Byte;
  LFN_inCurFileLine: Boolean;

function MakeDivider(IV: PInfoView): Boolean;
  var
    C: Word;
    I: Integer;
  begin
  Result := False;
  with IV^ do
    begin
    MoveChar(B, #196, CDivdier, IV^.Size.X+Panel^.DeltaX);
    I := 0;
    C := (CDivdier shl 8) or 193;
    while I < Size.X do
      begin
      Panel^.GetEmpty(B[I], C, True);
      Inc(I, Panel^.LineLength);
      if I = Size.X then
        B[I-1] := (C and $FF00)+196;
      end;
    end;
  end;

function MakeCurFile(IV: PInfoView): Boolean;
  begin
  with IV^ do
    begin
    MoveChar(B, ' ', C1, Size.X+Panel^.DeltaX);
      { Очистить надо безусловно, особенно если файлов нет. }
    Result := False;
    if PF = nil then
      Exit;
    DnD.CurrentY1 := Y;
    Panel^.Drive^.GetDown(B, C1, PF, LFN_inCurFileLine);
    end;
  Result := True;
  end;

function MakeFilter(IV: PInfoView): Boolean;
  var
    S: String;
    I: Integer;
  begin
  S := IV^.Panel^.PanSetup^.FileMask;
  Result := S <> x_x;
  if Result then
    begin
    S := fReplace(' ', '', S);
    if Copy(S, 1, 1) = ';' then
      Delete(S, 1, 1);
    I := 0;
    if BriefL1 <> 0 then
      I := Max(BriefL1+1, IV^.Size.X div 2);
    MoveCStr(B[I], GetString(dlFileMask)+S, C3);
    end;
  end;

function MakeQSMask(IV: PInfoView): Boolean;
  begin
  Result := QuickSearch and (IV^.Panel = ActivePanel);
   { Flash 25-01-2004:
       Если не сравнивать текущую панель с активной, то
    при включённом автообновлении строка с маской быстрого поиска
    может появиться на обеих панелях, а это нонсенс. }
  if Result then
    MoveCStr(B[0], QuickSearchString(IV^.Size.X), Swap(C2_3));
  end;

function MakeSelected(IV: PInfoView): Boolean;
  var
    S: String;
    I: Integer;
  begin
  with IV^ do
    begin
    Result := Panel^.SelNum <> 0;
    if Y <> 0 then
      MoveChar(B, ' ', C3, IV^.Size.X+Panel^.DeltaX);
    if Result then
      begin
      with Panel^ do
        begin
        S := '~'+FStr(SelectedLen);
        if  (Drive^.DriveType = dtArc) and (SelectedLen <> PackedLen) then
          S := S+'('+FStr(PackedLen)+')';

        S := S+GetString(dlBytesIn)+
          +ItoS(SelNum)+'~'+GetString(dlSelectedFiles);
        end;
      DnD.SelectedY := Y;
      end
    else if Y <> 0 then
      S := GetString(dlNoFilesSelected)
    else
      Exit;
    I := Max(0, (Size.X-CStrLen(S)) div 2);
    MoveCStr(B[I], S, Swap(C2_3));
    end;
  end;

function MakeSelectedBrief(IV: PInfoView): Boolean;
  var
    S: String;
    I: Integer;
  begin
  with IV^.Panel^ do
    if SelNum <> 0 then
      begin
      S := FStr(SelectedLen);
      if  (Drive^.DriveType = dtArc) and (SelectedLen <> PackedLen) then
        S := S + '/' + FStr(PackedLen);
      S := S + '(' + ItoS(SelNum) + ')';
      MoveStr(B[1], S, C3);
      BriefL1 := Length(S) + 1;
      SelectedInfoInDividerMin := 1;
      SelectedInfoInDividerMax := BriefL1-1;
      end;
  Result := False;
  end;

function MakeTotals(IV: PInfoView): Boolean;
  var
    S: String;
    FilesCount: LongInt;
    I: Integer;
    C: Word;
  begin
  if BriefL1 <> 0 then
    Exit;
  with IV^ do
    begin
    FilesCount := Panel^.Files^.Count-(FirstNameNum(Panel)-1);
    C := GetColor($0504);
    if FilesCount = 0 then
      S := GetString(dlDINoFiles)
    else
      begin
      S := GetString(dlTotal);
      if FilesCount = 1 then
        S := S+'1~ '+GetString(dlDIFile)
      else
        S := S+ItoS(FilesCount)+'~ '+GetString(dlDIFiles);
      S := S+GetString(dlDIWith)+'~';
      if C = 1 then
        S := S+'1~ '+GetString(dlDIByte)
      else
        S := S+FStr(IV^.Panel^.TotalInfo)+'~ '+GetString(dlDIBytes);
      end;
    I := Max(0, (Size.X-CStrLen(S)) div 2);
    DnD.TotalY := Y;
    end;
  MoveCStr(B[I], S, C);
  Result := True;
  end;

function MakeTotalsBrief(IV: PInfoView): Boolean;
  var
    S: String;
    FilesCount: LongInt;
    I: Integer;
  begin
  with IV^.Panel^ do
    begin
    FilesCount := Files^.Count-(FirstNameNum(IV^.Panel)-1);
    S := FStr(TotalInfo)+'('+ItoS(FilesCount)+')';
    I := IV^.Size.X - Length(S);
    if BriefL1 < I then
      begin
      MoveStr(B[I], S, CDivdier);
      TotalInfoInDividerMin := I;
      TotalInfoInDividerMax := Size.X-1;
      BriefL1 := IV^.Size.X;
      end;
    end;
  Result := False;
  end;

function MakeFreeSpace(IV: PInfoView): Boolean;
  var
    S: String;
    I: Integer;
    C: Word;
  begin
  if BriefL1 <> 0 then
    Exit;
  with IV^ do
    begin
    C := GetColor($0706);
    S := Panel^.FreeSpace;
    I := Max(0, (Size.X-CStrLen(S)) div 2);
    end;
  MoveCStr(B[I], S, C);
  Result := True;
  end;

function MakePathDecr(IV: PInfoView): Boolean;
  var
    S2: String;
    I: Integer;
    Mask: Word;
  begin
  if BriefL1 <> 0 then
    Exit;
  with IV^ do
    begin
    S2 := '';
    if Panel^.Drive^.ColAllowed[psnShowDir] then
      begin { Показ пути }
      if  (PF <> nil) and (PF^.Owner <> nil) then
        S2 := {$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(PF^.Owner^);
      Mask := psShowDir;
      end
    else
      begin { Показ описания }
      if  (PF <> nil) and (PF^.DIZ <> nil) and (PF^.DIZ^.DIZText <> '')
      then
        begin
        S2 := DizMaxLine(PF^.DIZ); {Первую строку не выделяем}
        Replace('~', #0'~', S2); {тильды показывать}
        Mask := psShowDescript;
        end;
      end;
    if Panel^.PanSetup^.Show.ColumnsMask and Mask <> 0
    then
      begin
      { выводим в подвале то, что не поместилось в панели }
      with Panel^ do
        begin
        J := CalcNameLength + CalcColPos(psShowDescript);
        J := Size.X-J+DeltaX-1;
        end;
      System.Delete(S2, 1, J);
      end;
    if Length(S2) > Size.X then
      begin
      SetLength(S2, Size.X);
      S2[Size.X] := FMSetup.RestChar[1];
      end;
    Result := S2 <> '';
    if Result then
      begin
      MoveChar(B, ' ', C1, Size.X+Panel^.DeltaX);
      MoveCStr(B, S2, C1);
      end;
    end;
  end;

function MakePacked(IV: PInfoView): Boolean;
  var
    S: String;
    I: Integer;
    C: Word;
  begin
  Result := (PF <> nil) { бывает на пустом диске } and
      ((PF^.Size > 0) or (PF^.Attr and Directory = 0));
  if Result then
    begin
    C := IV^.GetColor($0405);
    MoveChar(B, ' ', Hi(C), IV^.Size.X+IV^.Panel^.DeltaX);
    S :=
      GetString(dlArcPSize)+' ' +
      AddSpace(FStr(PF^.PSize), 14) +
      GetString(dlArcRatio) +
      Percent(PF^.Size, PF^.PSize);
    I := Max(0, (IV^.Size.X-CStrLen(S)) div 2);
    MoveCStr(B[I], S, C);
    end;
  end;

function MakeRatio(IV: PInfoView): Boolean;
  var
    S: String;
    I: Integer;
  begin
  if (PF <> nil) { бывает на пустом диске } and
     ((PF^.Size > 0) or (PF^.Attr and Directory = 0))
  then
    begin
    S := Percent(PF^.Size, PF^.PSize);
    I := (IV^.Size.X - Length(S)) div 2;
    if I < BriefL1 + 1 then
      I := BriefL1 + 1;
    if I + Length(S) < PF^.Size then
      begin
      MoveStr(B[I], S, CDivdier);
      BriefL1 := I + Length(S);
      end;
    end;
  Result := False;
  end;

procedure PrepareLongName(IV: PInfoView; var S: String; var I: Integer);
  { Длинное имя в подвале или на разделителе. Результат - в S,
    Сдвиг для выравнивания - в I (-1 - центрировать) }
  var
    D: PDrive;
    CutLen: Integer;
    Dif1Start, Dif2Start: Integer;
    ExtPos: Integer; { начало расширения в формируемой строке }
    S1: String; { рабочая переменная для определения общей части }
    Dummy: String;

  procedure DelFromS(DelStart, DelLen: Integer);
    procedure DecPos(var Pos: Integer);
      begin
      if DelStart < Pos then
        Pos := Max(DelStart, Pos-DelLen);
      end;
    begin
    DecPos(ExtPos);
    DecPos(Dif2Start);
    DecPos(Dif1Start);
    Delete(S, DelStart, DelLen);
    Delete(S1, DelStart, DelLen);
    end { DelFromS };

  var
    ShowLFN_Difference: Word;
    l: Integer;
    CutNamePos: Integer;
  label
    ShowRight;
  begin { PrepareLongName }
  with IV^ do
    begin
    if LFN_inCurFileLine and
       (FMSetup.LFN_Autohide <> 0)
    then
      begin
      S := '';
      Exit;
      end;
    D := Panel^.Drive;
    S := PF^.FlName[True];
    S1 := UpStrg(S);
    PFilePanelRoot(D^.Panel)^.FormatName(PF, Dummy, l);
      { Повторяем форматирование для панели, чтобы было с чем сравнивать }
    UpStr(flnPanelName);
      { Сравнивать надо регистронезависимо }

    ExtPos := PosLastDot(S)+1;
    Dif1Start := 255; Dif2Start := 255;
{Длинное имя    ┌─────────────────────┐
  ├─ .......... │     Не выделять     │0
  ├─ .......... │  Показывать бледно  │1
  └─ Общую часть│    Не показывать    │2
                └─────────────────────┘
   Сейчас только находим позиции отличий и отрезаем общие части,
 если это задано. В процессе обрезки по длине позиции могут
 измениться (уменьшиться), а уж после этого будем вставлять тильды
 для раскраски. Если вставить тильды сейчас, это затруднит контроль
 длин для обрезки. }
    Dif2Start := ExtPos;
    l := flnDotPos+1;
    while flnPanelName[l] = S1[Dif2Start] do
      begin
      Inc(Dif2Start); Inc(l);
      end;
    { с Dif2Start до конца - новые }

    { Теперь разбираемся с общими от начала до ExtPos-1}
    Dif1Start := 1;
    l := 1;
    while (Dif1Start < ExtPos) and (flnPanelName[l] = S1[Dif1Start]) do
      begin
      Inc(Dif1Start); Inc(l);
      end;
    if (Dif1Start = ExtPos-1) and
       (S1[Dif1Start] = '.') and (flnPanelName[l] = ' ')
    then
      inc(Dif1Start); { нормально, это табуляция расширения }
    { Окончательно:
      отличия в S с Dif1Start по ExtPos-1 и с Dif2Start до конца }

    if FMSetup.LFN_Autohide <> 0 then
      begin
      if (Dif1Start >= ExtPos-1) and (Dif2Start > Length(S)) then
        begin
        S := '';
        Exit;
        end;
      end;

    ShowLFN_Difference := 0;
    if PF^.TType <> ttUpDir then
      begin
      ShowLFN_Difference := FMSetup.LFN_Difference;
      if ShowLFN_Difference = 2 then
        begin { общую часть слева отбрасываем без символа обрезки }
        if (Dif1Start < ExtPos) then { Есть отличие в имени }
          DelFromS(1, Dif1Start-1)
        else{ Отличие, если и есть, то только в расширении }
          DelFromS(1, Dif2Start-1)
        end;
      end;

    CutLen := Length(S) - Size.X;
    if CutLen <= 0 then
      begin { Варианты прижатия }
{Длинное имя   ┌─────────────────────┐
 ├─ Разместить │        Слева        │ 0
 ├─ .......... │      По центру      │ 1
 └─ .......... │       Справа        │ 2
               └─────────────────────┘ }
        case FMSetup.LFN_Wrap of
          0:
            I := 0;
          1:
            I := -1;
          2:
            I := Size.X - Length(S);
        end {case};
      end
    else
      begin { Варианты обрезания }
      I := 0;
{Длинное имя   ┌─────────────────────┐
 ├─ .......... │ Левую часть и расш. │ 0
 ├─ Показывать │    Правую часть     │ 1
 └─ .......... │     Левую часть     │ 2
               └─────────────────────┘}
      case FMSetup.LFN_Cut of
        0:
          begin
          if Length(S)-ExtPos > Size.X-3 then
            goto ShowRight; { от имени ничего не останется }
          CutNamePos := Min(Length(S), ExtPos-1) - CutLen;
          DelFromS(CutNamePos, CutLen);
          S[CutNamePos] := FMSetup.RestChar[1];
          end;
        1:
          begin
ShowRight:
          DelFromS(1, CutLen);
          S[1] := #17;
          end;
        2:
          begin
          SetLength(S, Size.X);
          S[Length(S)] := FMSetup.RestChar[1];
          end;
      end {case};
      end;
{  Вставляем тильды, не забывая, что в имени могли быть настоящие тильды }
{Длинное имя    ┌─────────────────────┐
  ├─ .......... │    Не выделять      │0
  ├─ .......... │  Выделить цветом    │1
  └─ Общую часть│   Не показывать     │2
                └─────────────────────┘}
    if ShowLFN_Difference <> 0 then
      begin
      if Dif2Start <= Length(S) then
        Insert(#1, S, Dif2Start);
      if (ExtPos > Dif1Start) then
        begin
        Insert(#1, S, ExtPos);
        Insert(#1, S, Dif1Start);
        end;
      end;
    Replace('~', #0'~', S);
    Replace(#1, '~', S);
    DnD.CurrentY2 := Y;
    end;
  end { PrepareLongName };

function MakeLongName(IV: PInfoView): Boolean;
  var
    S: String;
    I: Integer;
  begin
  Result := False;
  if PF = nil then
    Exit;
  PrepareLongName(IV, S, I);
  if S = '' then
    Exit;
  Result := True;
  if Y <> 0 then
    MoveChar(B, ' ', C9, IV^.Size.X+IV^.Panel^.DeltaX);
  if I < 0 then
    I := Max(0, (IV^.Size.X-CStrLen(S)) div 2);
  MoveCStr(B[I], S, Swap(C8_9));
  end;

function Terminate(IV: PInfoView): Boolean;
  begin
  Result := True;
  end;

var
  ElNumber: array[0..MaxFooterHeight] of Byte;
    { Используется во время компиляции настроек вида подвала.
      ElNumber[Y] - число элементов, выводимых в строке Y }

procedure TInfoView.Compile(Value: Word;
    FullProc, BriefProc: TFooterProc);
  var
    Y: Word;
    P: TFooterProc;
  begin
  if Value = 0 then
    Exit;
  @P := @FullProc;
  if Value > MaxFooterHeight then
    begin { На разделителе, На разделителе кратко }
    if Value = MaxFooterHeight+2 then
      @P := @BriefProc;
    Value := 0;
    end;
  if ElNumber[Value] >= High(LineMaker[Value]) then
    Exit; {! Собщение выдать бы, но вряд ли такой абсурд
      в жизни встретится }
  @LineMaker[Value][ElNumber[Value]] := @P;
  inc(ElNumber[Value]);
  end;

procedure TInfoView.CompileShowOptions;
  var
    Y, i: Word;
  begin
  if @Self = nil then
    Exit;
  FillChar(ElNumber, SizeOf(ElNumber), 0);
  with Panel^.PanSetup^.Show do
    begin
    Compile(MaxFooterHeight+1, MakeDivider, nil);
    Compile(ShowCurFile, MakeCurFile, nil);
    Compile(SelectedInfo, MakeQSMask, MakeQSMask);
      { Маска быстрого поиска выводится туда же, куда и
      данные о выделенных, притом маска имеет больший приоритет. }
    Compile(SelectedInfo, MakeSelected, MakeSelectedBrief);
    Compile(FilterInfo, MakeFilter, nil);
    Compile(PathDescrInfo, MakePathDecr, nil);
    if (Panel^.Drive^.DriveType = dtArc) and
      (ColumnsMask and (psShowRatio or psShowPacked) = 0)
    then
      begin
      Compile(PackedSizeInfo, MakePacked, nil);
      Compile(BriefPercentInfo, nil, MakeRatio);
      end;
    Compile(LFN_InFooter, MakeLongName, nil);
    Compile(TotalsInfo, MakeTotals, MakeTotalsBrief);
    Compile(FreeSpaceInfo, MakeFreeSpace, nil);
    if (FilterInfo in [1..MaxFooterHeight]) and
       (ElNumber[FilterInfo] = 1) and
       (Panel^.PanSetup^.FileMask = x_x)
    then
      ElNumber[FilterInfo] := 0;
        { Строка, в которой только тождественный фильтр, не нужна }
    end;
  Y := 0;
  for i := 0 to High(ElNumber) do
    begin
    if ElNumber[i] <> 0 then
      begin
      @LineMaker[i][ElNumber[i]] := @Terminate;
      if i <> Y then
        Move(LineMaker[i], LineMaker[Y], SizeOf(LineMaker[i]));
      inc(Y);
      end;
    end;
  Size.Y := Y;
  end { TInfoView.CompileShowOptions };

procedure TInfoView.Draw;
  const
    NoDnD: TPanelBottomDnD =
      (TotalY: 255; SelectedY: 255; CurrentY1: 255; CurrentY2: 255);
  var
    YCurFileLine: Integer;
      {` Номер строки подвала (разделитель - 0), в которой выводится
      информация от текущем файле. -1, если не выводится нигде.
        Эта строка прорисовывается перовй независимо от её расположения,
      поскольку информация об имени из этой строки влияет на прорисовку
      длинного имени.`}

  procedure DrawAtY;
    var
      I: Integer;
    begin
    BriefL1 := 0;
    I := 0;
    {
    while True do
      begin
      if LineMaker[Y][I](@Self) then
        Break;
      inc(I);
      end;
    }
    // fixme: commented by unxed
    WriteLine(0, Y, Size.X, 1, B);
    MoveChar(B, ' ', C1, Size.X);
    end;

  procedure FindCurFileLine;
    var
      I: Integer;
      P: TFooterProc;
    begin
    for Y := 0 to Size.Y-1 do
      begin
      I := 0;
      while True do
        begin
        @P := @LineMaker[Y][I];
        if @P = @Terminate then
          Break;
        if @P = @MakeCurFile then
          begin
          YCurFileLine := Y;
          Exit;
          end;
        inc(I);
        end;
      end;
    YCurFileLine := -1;
    end;

  begin { TInfoView.Draw }
  C1 := GetColor(1);
  C2_3 := GetColor($0203);
  C3 := Lo(C2_3);
  C2 := Hi(C2_3);
  C8_9 := GetColor($0809);
  C8 := Lo(C8_9);
  C9 := Hi(C8_9);
  CDivdier := Panel^.GetColor(2);
  DnD := NoDnD;
  with Panel^ do
    begin { Очистить координаты D&D из разделителя }
    TotalInfoInDividerMin := 1; TotalInfoInDividerMax := 0;
    SelectedInfoInDividerMin := 1; SelectedInfoInDividerMax := 0;
    end;
  if Panel^.Files <> nil then
      {AK155 nil бывает при запуске DN с сохранённым десктопом,
       когда размер окна не соответствует тому, при котором
       десктоп был сохранён.}
    begin
    PF := Panel^.Files^.At(Panel^.ScrollBar^.Value);
    LFN_inCurFileLine := False;
    FindCurFileLine;
    if YCurFileLine >= 0 then
      DrawAtY;
    for Y := 0 to Size.Y-1 do
      if Y <> YCurFileLine then
        DrawAtY;
    end;
  end { TInfoView.Draw };

function TInfoView.GetPalette;
  const
    S: String[Length(CInfoView)] = CInfoView;
  begin
  GetPalette := @S;
  end;

{ ---------------------------- TDirView ------------------------------ }

function TDirView.GetText(MaxWidth: Integer): String;
  begin
  Result := {$IFDEF RecodeWhenDraw}CharToOemStrPanel{$ENDIF}
    (PFilePanelRoot(Panel)^.DirectoryName);
  Result := Cut(Result, MaxWidth);
  end { TDirView.Draw };

procedure TDirView.HandleEvent;
  var
    S: String;
    I: LongInt;
    P: TPoint;
  begin
  inherited HandleEvent(Event);
  case Event.What of
    evMouseDown:
      begin
      MakeLocal(Event.Where, P);
      S := GetText(255);
      if Length(S) > Size.X then
        I := 0
      else
        I := PosChar(':', S);
      repeat
      until not MouseEvent(Event, evMouseAuto+evMouseMove);
      ClearEvent(Event);
      if  (P.X <= I)
      then
        Message(Panel, evCommand, cmChangeDrive, nil)
      else
        Message(Panel, evCommand, cmChangeDir, nil);
      end;
  end {case};
  end { TDirView.HandleEvent };

{ FilePanel HandleEvent}

{-DataCompBoy-}
procedure TFilePanel.HandleEvent;
  var
    PF: PFileRec;
    CurPos: LongInt;
    PPC: PCollection;
    MPos: TPoint;
    LastRDelay: Word;
    I, J: LongInt;
    PDr: PDrive;
    KeyCode: Word; {Cat}

  procedure CE;
    begin
    ClearEvent(Event)
    end;
  procedure CED;
    begin
    ClearEvent(Event);
    DrawView
    end;

  procedure CM_CopyUnselect;
    label 1;
    var
      PF: PFileRec;
      I, OSM: LongInt;
    begin
    if  (Files^.Count = 0) or (Event.InfoPtr = nil) then
      Exit;
    OSM := PFilesCollection(Files)^.SortMode;
    PFilesCollection(Files)^.SortMode := fcmPreciseCompare;
    for I := 0 to Files^.Count-1 do
      if Files^.FileCompare(Files^.At(I), Event.InfoPtr) = 0 then
        goto 1; {-$VOL}
    PFilesCollection(Files)^.SortMode := OSM;
    Exit;
1:
    PFilesCollection(Files)^.SortMode := OSM;
    PF := {Event.InfoPtr}Files^.At(I);
    with PF^ do
      if Selected then
        begin
        Selected := False;
        if Size > 0 then
          begin
          SelectedLen := SelectedLen-Size;
          PackedLen := PackedLen-PSize;
          end;
        Dec(SelNum)
        end;
    if TimerExpired(_Tmr1) then
      begin
      DrawView;
      if InfoView <> nil then
        InfoView^.DrawView;
      NewTimer(_Tmr1, 500);
      end;
    end { CM_CopyUnselect };

  function MaskSearch(B: Byte): Boolean;
{ Поиск файла, соответствия маске быстрого поиска, начиная с
  текущего (B=0) или со следующего (B=1) файла }
    var
      I: LongInt;
    begin
    MaskSearch := True;
    I := CurPos+B;
    if I >= Files^.Count then
      I := 0;
    repeat
      if InMask(PFileRec(Files^.At(I))^.FlName[uLfn],
           QSMaskPlusStar)
      then
        begin
        ScrollBar^.SetValue(I);
        Exit;
        end;
      Inc(I);
      if I >= Files^.Count then
        I := 0;
    until I = CurPos;
    MaskSearch := False;
    end { MaskSearch };

  label lbMakeUp, GotoKb, lbMakeDown;

  begin { TFilePanel.HandleEvent }
  {$IFDEF DualName}
  uLfn := PanSetup^.Show.ColumnsMask and psLFN_InColumns <> 0;
  {$ENDIF}
  inherited HandleEvent(Event);
  if Event.What = evNothing then
    Exit;
  CurPos := ScrollBar^.Value;
  if Files <> nil
  then
    if Files^.Count > CurPos
    then
      PF := Files^.At(CurPos)
    else
      PF := nil
  else
    PF := nil;
  I := ShiftState;
  KeyCode := Event.KeyCode and $FFFF; {Cat}
  if  (Event.What = evKeyDown) and (I and 3 <> 0) and
      ( (KeyCode = kbCtrlRight and $FFFF) or (KeyCode = kbCtrlLeft and
         $FFFF) or
        ( (KeyCode = kbBack and $FFFF) and not QuickSearch))
  then
    begin
    CommandHandle(Event);
    Exit;
    end;
  if  (Event.What = evKeyDown) and (CommandLine <> nil) then
    if  ( (I and (kbRightShift+kbLeftShift) <> 0) and
          ( (KeyCode = kbDown and $FFFF) or
            (KeyCode = kbUp and $FFFF) or
            ( (KeyCode = kbCtrlIns and $FFFF) and (CmdLine.Str <> '')) or
            (KeyCode = kbGrayAst and $FFFF)
          )
        ) or
        ( ( (Event.ScanCode < Hi(kbAlt1)) or
            (Event.ScanCode > Hi(kbAlt9))
          ) and
          (Event.CharCode = #0) and
          (CmdLine.Str <> '') and
          ( ( (I and 3 <> 0) xor (FMSetup.Options and fmoUseArrows = 0)) and
            ( (KeyCode = kbRight and $FFFF) or (KeyCode = kbLeft and
               $FFFF) or
              (KeyCode = kbHome and $FFFF) or (KeyCode = kbEnd and $FFFF)
            )
          )
        )
    then
      CommandLine^.HandleEvent(Event);
  if Event.What = evNothing then
    Exit;
  I := ShiftState2;
  case Event.What of
    evCommand:
      case Event.Command of
        cmDoSendLocated:
          SendLocated;
        cmGetDirName,
        cmGetName:
          begin
          if Drive^.DriveType = dtDisk then
            begin
            {$IFDEF DualName}
            { AK155 13.02.05 Фактичекси сюда можно попасть только из
            DblWnd при обработке Ctrl-[ и Ctrl-], возможно, с Alt.
            Вот этот самый Alt и используем для инверсии
            признака работы с длинным или коротким именем }
            if (PanSetup^.Show.ColumnsMask and psLFN_InColumns <> 0) =
               (ShiftState and kbAltShift <> 0)
            then
              PString(Event.InfoPtr)^:= lfGetShortFileName(DirectoryName)
            else
              {$ENDIF}
              PString(Event.InfoPtr)^:= DirectoryName;
            end
          else if ScrollBar^.Value < Files^.Count then
            PString(Event.InfoPtr)^:= PFileRec
                (Files^.At(ScrollBar^.Value))^.Owner^;
          ClearEvent(Event);
          end;
        cmKillUsed:
          Drive^.KillUse;
        cmClose:
          CommandEnabling := False;
        cmCopyUnselect:
          begin
          CM_CopyUnselect;
          CE
          end;
        0..3, 5..100:
          ;
        else {case}
          CommandHandle(Event);
      end {case};

    evKeyDown:
      begin
      if QuickSearch then
        begin
        if Event.CharCode = #27 then
          begin
          StopQuickSearch;
          InfoView^.DrawView;
          CED;
          Exit
          end;
        if Event.KeyCode = kbCtrlEnter then
          begin
          MaskSearch(1);
          CE;
          Exit;
          end;
        if  (Event.KeyCode = kbGrayPlus) or (Event.KeyCode = kbGrayMinus) or
            (Event.KeyCode = kbGrayAst) or (Event.KeyCode = kbIns) or
            (Event.KeyCode = kbBackUp)
        then
          goto GotoKb;
        if (Event.CharCode > #31) or
              (Event.KeyCode = kbBack) or
              (Event.KeyCode = kbBackUp) or
              (Event.KeyCode = kbCtrlRight) or
              (Event.KeyCode = kbCtrlLeft)
        then
          begin
          DoQuickSearch(Event.KeyCode);
          if not MaskSearch(0) then
            DoQuickSearch(kbBack)
          else
            InfoView^.DrawView; { надо сменить и маску, и данные о файле }
          QSLastSuccessPos := LastSuccessPos;
          CED;
          Exit;
          end;
        end;

      if  ( ( (Event.KeyCode = kbDoubleAlt)) and (FMSetup.Quick =
           pqsAlt)) or
          ( (Event.KeyCode = kbDoubleCtrl) and (FMSetup.Quick = pqsCtrl)) or
          ( (Event.CharCode >= #32) and (Event.CharCode <= #254) and
            (Event.CharCode <> '/') and (FMSetup.Quick = pqsCaps) and // slash change by unxed
            (I and $40 <> 0)) or
          ( (Event.CharCode >= #32) and (Event.CharCode <= #254) and
            (ShiftState and 3 <> 0) and (ShiftState and 4 = 0) and
            (not CommandLine^.GetState(sfVisible)) and
            (InterfaceData.Options and ouiHideCmdline <> 0))
      then
        begin
        if QuickSearch then
          begin
          StopQuickSearch;
          InfoView^.DrawView; {Cat}
          end
        else
          begin
          InitQuickSearch(@Self);
          QSLastSuccessPos := LastSuccessPos;
          end;
        if  (Event.CharCode >= #32) and (Event.CharCode <= #254) then
          begin
          if not ((ShiftState and 3 <> 0) and (not
                   CommandLine^.GetState(sfVisible)))
          then
            begin
            ShiftState2 := I and $BF;
            ShiftState := ShiftState and kbCapsState;
            end
          else
            ShiftState := ShiftState and $FC;
          DoQuickSearch(Event.KeyCode);
          if QuickSearch then
            begin
            if  (FMSetup.Quick = pqsCaps) and (I and kbCapsState <> 0)
            then
              begin
              (*
                        {$IFDEF VIRTUALPASCAL}
                        SysTVSetShiftState(ShiftState and not kbCapsState);
                        ShiftState := SysTVGetShiftState;
                        {$ENDIF}
                        *)
              end;
            if not MaskSearch(0) then
              DoQuickSearch(kbBack);
            end;
          end;
        DrawView;
        CE;
        end
      else
        begin
        if QuickSearch then
          begin
          if Event.KeyCode and $FFFF = 0 then
            begin { Чисто шифтовое событие. Если его не проигнорировать,
              то во время быстрого поиска невозможно переключить
              раскладку, так как драйверы клавиатуры и OS/2,
              и Windows не проглатывают хоткей смены раскладки, а
              передают его и приложению тоже. }
            ClearEvent(Event);
            Exit;
            end;
          StopQuickSearch;
          InfoView^.DrawView;
          end;
GotoKb:
        case Event.KeyCode of
          kbDel:
            if  ( (CmdLine.Str = '') and (FMSetup.Options and fmoDelErase
                   <> 0))
            then
              Message(@Self, evCommand, cmPanelErase, nil);
          kbShiftDel:
            if  ( (CmdLine.Str = '') and (FMSetup.Options and fmoDelErase
                   <> 0))
            then
              Message(@Self, evCommand, cmSingleDel, nil);
          { Flash >>> }
          kbCtrlHome:
            begin
            CE;
            DeltaX := 0;
            OldDelta := -1;
            Owner^.Redraw
            end;
          kbCtrlEnd:
            begin
            CE;
            DeltaX := LineLength-Size.X-1;
            OldDelta := -1;
            Owner^.Redraw
            end;
          kbHome:
            begin
            CE;
            OldDelta := -1;
            ScrollBar^.SetValue(0);
            Owner^.Redraw
            end;
          { Flash <<< }
          kbEnd:
            begin
            CE;
            OldDelta := -1;
            ScrollBar^.SetValue(Files^.Count-1)
            end;
          kbUp, kbDown, kbCtrlUp, kbCtrlDown, kbCtrlShiftUp,
           kbCtrlShiftDown, kbUpUp, kbDownUp
          :
            begin
            CtrlWas :=  LongRec(Event.KeyCode).Hi = kbCtrlShift;
            if Event.KeyCode = kbCtrlUp then
              Event.KeyCode := kbUp;
            if Event.KeyCode = kbCtrlDown then
              Event.KeyCode := kbDown;
            if  (Event.KeyCode = kbDownUp)
              or (Event.KeyCode = kbUpUp)
            then
              begin
              CE;
              Exit
              end;
            end;
          kbIns, kbSpace:
            if CurPos < Files^.Count then
              begin
              StopQuickSearch;
              if  (Event.CharCode = ' ') and ((CmdLine.Str <> '') or
                    (FMSetup.Options and fmoSpaceToggle = 0))
              then
                Exit;
              CE;
              if Files^.Count = 0 then
                Exit;
              PF := Files^.At(CurPos);
              if PF^.TType <> ttUpDir then
                begin
                PF^.Selected := not PF^.Selected;
                if PF^.Size > 0 then
                  begin
                  SelectedLen := SelectedLen-(1-2*Integer(PF^.Selected))
                    *PF^.Size;
                  PackedLen := PackedLen-(1-2*Integer(PF^.Selected))
                    *PF^.PSize;
                  end;
                Dec(SelNum, 1-2*Integer(PF^.Selected));
                end;
              ScrollBar^.SetValue(CurPos+1);
              if CurPos = ScrollBar^.Value then
                DrawView;
              if InfoView <> nil then
                InfoView^.DrawView;
              end;
          kbAltQuote:
            begin
            FMSetup.Show := FMSetup.Show xor fmsShowHidden;
            ConfigModified := True;
            Message(Application, evCommand, cmUpdateConfig, nil)
            end
          else {case}
            CommandHandle(Event);
        end {case};
        end;
      end; {evKeyDown}
    evBroadcast:
      case Event.Command of
        cmFindForced,
        cmInsertDrive,
        cmUnArchive,
        cmCopyCollection,
        cmDropped:
          CommandHandle(Event);

        cmScrollBarChanged:
          if ScrollBar = Event.InfoPtr then
            begin
            if ScrollBar^.ForceScroll or WheelEvent then
              Inc(Delta, ScrollBar^.Step);
                { Немедленное скроллирование с сохранением позиции
                  курсора относительно окна }
            if MSelect then
              begin
              CE;
              if Files <> nil then
                begin
                if Files^.Count = 0 then
                  Exit;
                PF := Files^.At(ScrollBar^.Value);
                if  (PF^.TType <> ttUpDir)
                  and (PF^.Selected xor SelectFlag)
                then
                  begin
                  PF^.Selected := SelectFlag;
                  if SelectFlag then
                    begin
                    Inc(SelNum);
                    if PF^.Size > 0 then
                      begin
                      SelectedLen := SelectedLen+PF^.Size;
                      PackedLen := PackedLen+PF^.PSize
                      end;
                    end
                  else
                    begin
                    Dec(SelNum);
                    if PF^.Size > 0 then
                      begin
                      SelectedLen := SelectedLen-PF^.Size;
                      PackedLen := PackedLen-PF^.PSize
                      end;
                    end;
                  end;
                end;
              end;
            PosChanged := True;
            if InfoView <> nil then
              InfoView^.DrawView;
            CED;
            PosChanged := False;
            if  (RepeatDelay <> 0) and QuickViewEnabled then
              NeedLocated := GetSTime;
            end;
      end {case};
    evMouseDown:
      CommandHandle(Event);
  end {case};
  end { TFilePanel.HandleEvent };
{-DataCompBoy-}

end.

