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
unit FLTools;

interface
uses
  FilesCol, FlPanelX, PDSetup
  ;

procedure CM_AdvancedFilter(AFP: Pointer);
function GetSelection(P: PFilePanelRoot; Single: Boolean):
  PFilesCollection;
function SelectFiles(AFP: Pointer; Select, XORs: Boolean): Boolean;
procedure CM_CopyFiles(AFP: Pointer; MoveMode, Single: Boolean);
procedure InvertSelection(AFP: Pointer; dr: Boolean);
procedure CM_CompareDirs(AFP, IP: Pointer);
procedure CM_EraseFiles(AFP: Pointer; Single: Boolean);
procedure CM_MakeList(AFP: Pointer);
procedure CM_SetAttributes(AFP: Pointer; Single: Boolean; CurPos: Integer);
procedure CM_SetShowParms(AFP: Pointer);
procedure CM_CopyTemp(AFP: Pointer);
procedure CM_ArchiveFiles(AFP: Pointer);
{$IFDEF Printer}
procedure CM_Print(AFP: Pointer);
{$ENDIF}
procedure CM_ToggleDescriptions(AFP: Pointer);
procedure CM_ToggleLongNames(AFP: Pointer);
procedure CM_ToggleShowMode(AFP: Pointer);
procedure CM_DragDropper(AFP: Pointer; CurPos: Integer; EV: Pointer);
procedure CM_Dropped(AFP, EI: Pointer);
procedure DragMover(AP: Pointer; Text: String; AFC, AC: Pointer);
procedure CM_RenameSingleL(AFP, PEV: Pointer);
procedure CM_RenameSingleDialog(AFP, PEV: Pointer);
procedure CM_SortBy(AFP: Pointer);
procedure CM_PanelSortSetup;
function CM_ChangeDirectory(AFP: Pointer): String;
procedure CM_MakeDir(AFP: Pointer);
procedure CM_LongCopy(AFP: Pointer);
procedure CM_ChangeCase(AFP: Pointer);
function PanelSetupTag(const PSS: TPanelSetupSet;
  {` Сформировать краткое строковое описание настроек панели }
    PC: TPanelClass): String;
  {`}
procedure CM_SelectColumn(AFP: Pointer); {JO}
{$IFDEF OS2}
procedure CM_SetEALongname(AFP: Pointer; CurPos: Integer); {JO}
{$ENDIF}

function FirstNameNum(P: PFilePanelRoot): LongInt;
{` AK155 Получение первого номера (от 1) в коллекции файлов панели,
  соответствующего не фиктивному имени '..', а настоящему имени.
  Возвращает либо 1, если это корень диска и элемента '..' нет,
  либо 2, если он есть. `}

implementation
uses
  fnotify,
  Lfnvp, Files, Objects, xTime, DnIni, HistList,
  Advance, Advance1, Advance2, Dos, Defines, Dialogs,
  Views, DNApp, Commands, Drivers, FlPanel, Drives, FileCopy,
  Gauge, Gauges, Archiver, Startup, SWE, Validate, Messages, Menus, DNHelp,
  FileFind, Tree, FBB, DNUtil, Filediz, Filelst, FlTl, DblWnd,
  ErrMess, Objects2, VPUtils
  ;

type
  PSelectList = ^TSelectList;
  TSelectList = object(TListBox)
    function IsSelected(I: LongInt): Boolean; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    end;

procedure DrawViews(P: PFilePanelRoot);
  begin
  P^.DrawView;
  if P^.InfoView <> nil then
    P^.InfoView^.DrawView;
  end;

function TSelectList.IsSelected(I: LongInt): Boolean;
  begin
  IsSelected := (List <> nil) and
    (Copy(CnvString(List^.At(I)), 1, 1) = FMSetup.TagChar[1]);
  end;

procedure TSelectList.HandleEvent(var Event: TEvent);
  var
    P: PString;

  procedure CE;
    begin
    ClearEvent(Event);
    DrawView;
    end;

  procedure Invert;
    begin
    if  (List <> nil) and (Focused < List^.Count) then
      P := List^.At(Focused)
    else
      P := nil;
    if P <> nil then
      if P^[1] = ' ' then
        P^[1] := FMSetup.TagChar[1]
      else
        P^[1] := ' ';
    CE
    end;

  procedure DoSelect(P: PString);
    begin
    if P <> nil then
      case Event.CharCode of
        '*':
          if P^[1] = ' ' then
            P^[1] := FMSetup.TagChar[1]
          else
            P^[1] := ' ';
        '+':
          P^[1] := FMSetup.TagChar[1];
        '-':
          P^[1] := ' ';
      end {case};
    end;

  begin { TSelectList.HandleEvent }
  case Event.What of
    evMouseDown:
      if Event.Buttons and mbRightButton <> 0 then
        begin
        inherited HandleEvent(Event);
        Invert;
        end;
    evKeyDown:
      case Event.CharCode of
        ' ':
          begin
          Event.KeyCode := kbDown;
          PutEvent(Event);
          Invert;
          end;
        '+', '*', '-':
          if List <> nil then
            begin
            List^.ForEach(@DoSelect);
            CE
            end;
        else {case}
          if Event.KeyCode = kbIns then
            begin
            Event.KeyCode := kbDown;
            PutEvent(Event);
            Invert;
            end;
      end {case};
  end {case};
  if Event.What <> evNothing then
    inherited HandleEvent(Event);
  end { TSelectList.HandleEvent };

function TSelectList.GetText(Item: LongInt; MaxLen: Integer): String;
  var S: String;
      ColWidth: Byte;
  begin
  if List <> nil then
    S := PString(List^.At(Item))^
  else
    S := '';
  ColWidth := (Size.X div NumCols)-1;
  if Length(S) > ColWidth then
    begin
    SetLength(S, ColWidth);
    S[ColWidth] := FMSetup.RestChar[1];
    end;
  Result := S;
  end;

{AK155 29.05.05 Средства "оживления" диалогов записи и чтения
настроек панелей, а также самих диалогов настроек панели.
 }

var
  PanelClass: Word;
    { В какой класс панели будем писать; это значение блока
      радиокнопок, то есть от 0 до High(TPanelClass)+1 ("Все") }
  PresetNum: Word;
    { Номер пресета панели или последнего загруженного.
    Он выбран по умолчанию в диалоге "Записать" }

type
{ Диалог установок вида панелей. Является также базовым типом
  для диалогов установок сортировки и фильтра. }
  PShowDialog = ^TShowDialog;
  TShowDialog = object(TDialog)
    function OwnDataAddress(P: PPanelSetup): Pointer; virtual;
      { Адрес того блока данных внутри P^, с которым работает
      данный диалог, то есть, в данном случае, адрес P^.Show.
      Этот метод перекрывается в диалогах сортировки и фильтра.
      Виртуализация этого метода используется в программе обработки
      нажатия кнопки "записать" TSaveSetupButtonPress. }
    procedure HandleEvent(var Event: TEvent); virtual;
      { Для реакции на хоткеи и кнопку "Записать".
      Это метод общий для всех трёх диалогов}
    end;

{ Радиокнопки класса панели в диалоге "Записать".
  Для реакции на смену выбора в блоке радиокнопок "Тип панели" }
  PPanelClassRB = ^TPanelClassRB;
  TPanelClassRB = object(TRadioButtons)
    procedure MovedTo(Item: Integer); virtual;
    procedure Press(Item: Integer); virtual;
      { Приходится перекрываь и MoveTo, и Press, так как ни одна из
      них не вызывается всегда. Например, при перемещеии курсором
      вызывается только MovedTo, при нажатии мышью - только Press,
      а при нажатии горячей клавишей - обе.}
    procedure ChangeClass(Item: Integer);
    end;

  TSaveSetupButton = object(TButton)
    procedure Press; virtual;
    end;

  TSaveSetupDialg = object(TDialog)
    procedure HandleEvent(var Event: TEvent); virtual;
      { Для реакции на кнопку "Все" }
    end;

  var
    SavePresetData: record { даные диалога "Записать"}
      ForClass: Word; {Класс панели: диск, список и.п.д.}
      Target: Word; { Пресет, эта и та панели.}
      end;

procedure TSaveSetupDialg.HandleEvent(var Event: TEvent);
  var
    T: PCheckBoxes;
  begin
  if (Event.What = evCommand) and (Event.Command = cmSelectAll) then
    begin
    T := PCheckBoxes(DirectLink[2]);
    with T^ do
      begin
      if Value = $FFF then
        Value := 1 shl (PresetNum-1)
      else
        Value := $FFF;
      Draw;
      end;
    ClearEvent(Event);
    end
  else
    inherited HandleEvent(Event);
  end;

procedure PrepareSaveSetupDialog(P: PDialog);
  begin
  ObjChangeType(P, TypeOf(TSaveSetupDialg));
  ObjChangeType(P^.DirectLink[1], TypeOf(TPanelClassRB));
    { Прицепили свою реакцию на смену выбора класса панели }
  PPanelClassRB(P^.DirectLink[1])^.ChangeClass(Ord(PanelClass));
    { Сформировали блок радиокнопок с пресетами }
  end;

procedure TPanelClassRB.Press(Item: Integer);
  begin
  inherited Press(Item);
  if Item <> PanelClass then
    ChangeClass(Item);
  end;

procedure TPanelClassRB.MovedTo(Item: Integer);
  begin
  inherited MovedTo(Item);
  if Item <> PanelClass then
    ChangeClass(Item);
  end;

procedure TPanelClassRB.ChangeClass(Item: Integer);
  { Реакция на смену выбора типа панели: перерисовка блока пресетов }
  var
    T: PCheckBoxes;
    i: Integer;
    S: String;
  begin
  PanelClass := Item;
//  T := PRadioButtons(PDialog(Owner)^.DirectLink[2]);
  T := PCheckBoxes(PDialog(Owner)^.DirectLink[2]);
  with T^.Strings do
    begin
    { Освобождение обозначений десяти пресетов, если они есть.
    Последние два элемента (эта и другая панели) есть всегда. }
    for i := Count-3 downto 0 do
      AtFree(i);
    { Добавление обозначений десяти пресетов. Исходно у Strings
    Delta=0, Limit=2, Count=2, поэтому добавить ничего нельзя.}
    Delta := 10; // нужно добавить ровно столько
    for i := 1 to 10 do
      begin
      S := '~' + ItoS(i mod 10) + '~ ';
      if PanelClass <= Ord(High(TPanelClass)) then
        { нормальный класс, а не "Все"}
        S := S +
          PanelSetupTag(PanSetupPreset[i], TPanelClass(PanelClass));
      AtInsert(i-1, NewStr(S));
      end;
    T^.DrawView;
    end;
  end;

procedure TSaveSetupButton.Press;
  var
    TargetSetupSet: PPanelSetupSet;
    PC: TPanelClass;
    TargetPanel: PFilePanelRoot;
    TargetBlock: Word;
    Cmd: Word;
    SaveTaggedDataOnly: Boolean;
  begin
  PresetNum := ActivePanel^.PresetNum;
  PanelClass := Ord(dt2pc[ActivePanel^.Drive^.DriveType]);
  EnableCommands([cmOK]); { Диалог фильтра мог задизейблить }
  SavePresetData.ForClass := PanelClass;
  SavePresetData.Target := 1 shl (PresetNum-1);
  @PreExecuteDialog := @PrepareSaveSetupDialog;
  SaveTaggedDataOnly := TaggedDataOnly;
  TaggedDataOnly := False; // для самого диалога сохранения
  Cmd := ExecResource(dlgSavePanelSetup, SavePresetData);
  TaggedDataOnly := SaveTaggedDataOnly;
  if Cmd = cmOK then
    begin
    for TargetBlock := 0 to 11 do
      if ((1 shl TargetBlock) and SavePresetData.Target) <> 0 then
        begin
        case TargetBlock of
          0..9: {пресеты 1..10}
            begin
            TargetPanel := nil;
            TargetSetupSet := @PanSetupPreset[TargetBlock+1];
            ConfigModified := True;
            end;
          10: { Эта панель}
            begin
            TargetPanel := ActivePanel;
            TargetSetupSet := @TargetPanel^.PanelSetupSet;
            end;
          11: { Другая панель}
            begin
            TargetPanel := PassivePanel;
            TargetSetupSet := @TargetPanel^.PanelSetupSet;
            end;
        end {case};
        if PanelClass > Ord(High(TPanelClass)) then { "Все"}
          for PC := Low(TPanelClass) to High(TPanelClass) do
            with PShowDialog(Owner)^ do
              GetData(OwnDataAddress(@TargetSetupSet^[PC])^)
        else { один реальный класс }
          begin
          PC := TPanelClass(PanelClass);
          with PShowDialog(Owner)^ do
            GetData(OwnDataAddress(@TargetSetupSet^[PC])^);
          end;
        if TargetPanel <> nil then { записали не в пресет, а в панель }
          begin
          PDoubleWindow(TargetPanel^.Owner)^.SetMaxiState(TargetPanel);
          TargetPanel^.Rebound;
          TargetPanel^.RereadDir;
          end;
        end;
    end;
  end;

function TShowDialog.OwnDataAddress(P: PPanelSetup): Pointer;
  begin
  Result := @P^.Show;
  end;

procedure TShowDialog.HandleEvent(var Event: TEvent);
  var
    N: Integer;
    P: PFilePanelRoot;
    Cmd: Word;
  procedure LoadData(N: Integer; NewSetupSet: PPanelSetupSet);
    var
      PC: TPanelClass;
    begin
    PC := dt2pc[ActivePanel^.Drive^.DriveType];
    PresetNum := N;
    SetData(OwnDataAddress(@NewSetupSet^[PC])^);
    ClearEvent(Event);
    end;
  procedure TagUntagAll(P: PView);
    begin
    Message(P, evCommand, Cmd, nil);
    end;
  begin
  inherited HandleEvent(Event);
  case Event.What of
    evKeyDown:
      case Event.KeyCode of
        kbCtrl1..kbCtrl0:
          begin
          N := 1 + (Event.KeyCode - kbCtrl1) div (kbCtrl2 - kbCtrl1);
          LoadData(N, @PanSetupPreset[N]);
          end;
        kbCtrlEqual:
          begin
          P := OtherFilePanel(ActivePanel);
          LoadData(P^.PresetNum, @P^.PanelSetupSet);
          end;
      end {case};
    evCommand:
      case Event.Command of
        cmSelectAll:
          begin
          if TaggedDataOnly then
            begin
            if TaggedDataCount = 0 then
              Cmd := cmForceTagData
            else
              Cmd := cmForceUntagData;
            ForEach(@TagUntagAll);
            DrawView;
            end;
          end;
      end {case};
  end {case};
  end;

{ ----------------- CM_AdvancedFilter ----------------- }

{ Нужно обеспечить такое поведение диалога. Если активна строка
ввода, то кнопка по умолчанию - "OK", и её нажатие завершает диалог
с занесением содержимого строки ввода. А если активен список
расширений, то кнопка по умолчанию - "Добавить", и её нажатие
добавляет элемент фильтра и возобновляет диалог. Лишние кнопки
должны быть не активны. Лучше бы, конечно, было сделать их
невидимыми, но Hide и Show для TButton работают настолько
нетривиально, что связываться с ними не хочется.
  Для манипуляций с кнопками цепляемся через SetState к событиям
выбора списка и строки ввода. }

var
  OkButton, AddButton: PButton;

type

  PFilterDialog = ^TFilterDialog; { Диалог фильтра }
  TFilterDialog = object(TShowDialog)
    function OwnDataAddress(P: PPanelSetup): Pointer; virtual;
    destructor Done; virtual;
    end;

  PExtSelList = ^TExtSelList; { Список расширений }
  TExtSelList = object(TSelectList)
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    function DataSize: Word; virtual;
    procedure GetData(var Rec); virtual;
      { Чтобы список не участвовал в GetData - SetData}
    end;

  PFilterLine = ^TFilterLine; { Строка ввода фильтра }
  TFilterLine = object(TInputLine)
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    end;

procedure TExtSelList.SetState(AState: Word; Enable: Boolean);
  begin
  if Enable and (Astate and sfSelected <> 0) then
    begin
    Owner^.EnableCommands([cmYes, cmNo]);
    OkButton^.MakeDefault(False);
    AddButton^.MakeDefault(True);
    Owner^.DisableCommands([cmOK]);
    end;
  inherited SetState(AState, Enable);
  end;

function TExtSelList.DataSize: Word;
  begin
  Result := 0;
  end;

procedure TExtSelList.GetData(var Rec);
  begin
  end;

procedure TFilterLine.SetState(AState: Word; Enable: Boolean);
  begin
  if Enable and (Astate and sfSelected <> 0) then
    begin
    Owner^.EnableCommands([cmOK]);
    AddButton^.MakeDefault(False);
    OkButton^.MakeDefault(True);
    Owner^.DisableCommands([cmYes, cmNo]);
    end;
  inherited SetState(AState, Enable);
  end;

function TFilterDialog.OwnDataAddress(P: PPanelSetup): Pointer;
  begin
  Result := @P^.FileMask;
  end;

destructor TFilterDialog.Done;
  begin
  EnableCommands([cmOK, cmYes, cmNo]);
  inherited Done;
  end;

procedure CM_AdvancedFilter;
  var
    P: PFilePanelRoot absolute AFP;
    i: Byte;
    C: PStringCollection;
    ClrPlus, ClrMinus: Boolean;
    S, FileMask: String;
    FItem: Integer;
    SelectFilterLine: Boolean;
    PC: PStringCollection;
      { Коллекция расширений с элементами типа ' *.pas'.
      Первый символ текста элемента - признак отметки:
      пробел - значит не отмечен }

  procedure BuildPC;
  { Построение коллекции расширений для ListBox }
  { Строится коллекция файлов, а потом из неё выбираются расширения.
  Это надо делать именно через методы TDrive, а не через чтение диска,
  так как бывают архивы и т.п.
    Но строить всю коллекцию - это плохой метод, так как он приводит
  к лишней трате памяти  на коллекцию файлов. Лучше было бы, если бы
  у TDrive был итератор (виртуальный), позволяющий для каждой файловой
  записи что-то сделать, не строя сначала всю коллекцию. Кстати, и
  построение коллекции можно было бы сделать через него, то есть
  итератор был бы более элементарной функцией, чем GetDirectory.
  }
    var
      SR: lSearchRec;
      S: String;
      ti: TSize;
      fc: PFilesCollection;
      i: Integer;
    begin
    PC := New(PStringCollection, Init(20, 20, False));
    fc := P^.Drive^.GetDirectory(x_x, ti);
    for i := 0 to fc^.Count-1 do
      with PFileRec(fc^.Items^[i])^ do
        begin
        S := FlName[uLFN];
        S := Copy(S, PosLastDot(S)+1, 255);
        UpStr(S);
        PC^.Insert(NewStr(' *.' + S));
        end;
    Dispose(fc, Done);
    PC^.AtInsert(0, NewStr(' *.*'));
    end;

  function GetMaskSelection: String;
    var
      D: PDialog;
      PL: PListBox;
      IL: PInputline;
      Cmd: Word;

    function MakeDialog: PDialog;
      var
        Dlg: PDialog;
        b: Integer;
      begin
      Dlg := PDialog(LoadResource(dlgAdvancedFilter));
      ObjChangeType(Dlg, TypeOf(TFilterDialog));
      with Dlg^ do
        begin
        PL := PListBox(DirectLink[2]);
        ObjChangeType(PL, TypeOf(TExtSelList));
        PL^.NewLisT(PC);
        IL := PInputLine(DirectLink[1]);
        ObjChangeType(IL, TypeOf(TFilterLine));
        OkButton := PButton(DirectLink[4]);
        AddButton := PButton(DirectLink[3]);
        for b := 3 to 5 do
          with PButton(DirectLink[b]) do
            Options := Options and not ofSelectable;
          { Кнопки "OK", "Добавить" и "Закрыть" по Tab пропускаются }
        ObjChangeType(Dlg^.DirectLink[7], TypeOf(TSaveSetupButton));
          { Подменяем тип кнопки "Записать", чтобы подсунуть свой Press }
        end;
      IL^.SetState(sfSelected, True);
        { Чтобы привести кнопки в нужные состояния, см. TFilterLine }
      if not SelectFilterLine then
        begin
        Dlg^.SelectNext(False);
        end;
      MakeDialog := Dlg;
      end;

    procedure DoMake(P: PString);
      begin
      if P^[1] <> ' ' then
        begin
        AddStr(FreeStr, ';');
        if Cmd = cmNo then
          AddStr(FreeStr, '-');
        FreeStr := FreeStr+Copy(P^, 2, Length(P^)-1);
        P^[1] := ' ';
        end;
      end;

    var
      CurrentExt: PString;
      PS: PString;
      I, l: Integer;
    begin { GetMaskSelection: }
    GetMaskSelection := '';
    D := MakeDialog;
    D^.Options := D^.Options or ofCentered;
    PL^.FocusItem(FItem);
    IL^.Data := FileMask;
    Cmd := Desktop^.ExecView(D);

    SelectFilterLine := True;
    if (Cmd = cmYes) or (Cmd = cmNo) then
      begin
      SelectFilterLine := False;
      FItem := PL^.Focused;
      FreeStr := '';
      PC^.ForEach(@DoMake);
      if FreeStr = '' then
        begin
        CurrentExt := PC^.At(FItem);
        CurrentExt^[1] := FMSetup.TagChar[1];
        DoMake(CurrentExt);
        end;
      if  (Cmd = cmNo) and (FreeStr <> '-*.*') and
          (FreeStr <> '-*')
      then
        Insert('*.*;', FreeStr, 1);
      DelLeft(FreeStr);
      GetMaskSelection := FreeStr;
      end
    else if Cmd = cmOK then
      GetMaskSelection := #20+IL^.Data;
    Dispose(D, Done);
    end { GetMaskSelection };

  procedure InS(const AAS: String);
    var
      Mns: Boolean;
    begin
    if  (AAS = '') then
      Exit;
    Mns := (Copy(AAS, 1, 2) = '- ');
    if  (ClrMinus and Mns) or
        (ClrPlus and not Mns) or
        (C^.IndexOf(@AAS) <> -1)
    then
      Exit;
    C^.Insert(NewStr(AAS));
    end;

  procedure MakeMask(P: PString);
    begin
    if FileMask <> '' then
      FileMask := FileMask+';';
    FileMask := FileMask+P^;
    end;

  var
    NeedRebound: Boolean;
  begin { CM_AdvancedFilter }
  SelectFilterLine := True;
  FileMask := P^.PanSetup^.FileMask;
  FItem := 0;
  BuildPC;
  repeat
    S := GetMaskSelection;
    if S = '' then { Кнопка "Закрыть" или Esc }
      Break;
    if S[1] = #20 then { Кнопка "OK" }
      FileMask := Copy(S, 2, 255)
    else
    if  (S = '- *.*') or (S = x_x) then
      FileMask := S
    else
      begin
      if FileMask = x_x then
        begin
        FileMask := '';
        FreeStr := S+';'
        end
      else
        FreeStr := ';'+FileMask+';'+S+';';
      FileMask := S;
      S := FreeStr;
      ClrMinus := Pos(';- *.*;', S) > 0;
      if ClrMinus then
        Replace('- *.*', '', S);
      ClrPlus := Pos(';*.*;', S) > 0;
      if ClrPlus then
        Replace(x_x, '', S);
      if not (ClrPlus and ClrMinus) then
        begin
        if Copy(FileMask, 1, 2) = '- ' then
          Replace(';'+Copy(FileMask, 3, 5)+';', ';', S)
        else
          Replace(';- '+FileMask+';', ';', S);
        C := New(PStringCollection, Init(4, 4, False));
        repeat
          i := PosChar(';', S);
          if i = 0 then
            begin
            InS(S);
            Break;
            end;
          InS(Copy(S, 1, i-1));
          Delete(S, 1, i);
        until False;
        FileMask := '';
        C^.ForEach(@MakeMask);
        Dispose(C, Done);
        if FileMask <> '' then
          FileMask := ';'+FileMask;
        if ClrPlus then
          FileMask := x_x+FileMask;
        if ClrMinus then
          FileMask := '- *.*'+FileMask;
        end;
      end;
    if FileMask = '*' then
      FileMask := x_x;
    with P^ do
      begin
      NeedRebound := (PanSetup^.FileMask = x_x) <> (FileMask = x_x);
        { Если фильтр стал или перестал быть тождественным, это
          может вызывать изменение числа строк подвала, если в какой-то
          строке нет ничего, кроме фильтра. См. TInfoView.Compile }
      PanSetup^.FileMask := FileMask;
      if NeedRebound then
        Rebound;
      OldDelta := -1;
      RereadDir;
      Delta := -1;
      end;
  until S[1] = #20;
  Dispose(PC, Done);
  P^.ChkNoMem;
  end { CM_AdvancedFilter };
{------------------- CM_AdvancedFilter -----------------------}

function FirstNameNum(P: PFilePanelRoot): LongInt;
  begin
  FirstNameNum := 1;
  with P^.Files^ do
    if  (Count > 0) and (PFileRec(At(0))^.TType = ttUpDir) then
      FirstNameNum := 2;
  end;
{/AK155}

{AK155: оформил эту процедуру по-человечески }
function GetSelection(P: PFilePanelRoot; Single: Boolean):
  PFilesCollection;
  var
    FC: PFilesCollection absolute Result;
    SourceFiles: PFilesCollection;
    I, N: LongInt;
    CurFile, FileI: PFileRec;
  begin
  FC := nil;
  N := P^.Files^.Count;
  if N = 0 then
    Exit;
  SourceFiles := P^.Files;
  if  (P^.SelNum = 0) or Single then
    begin
    CurFile := SourceFiles^.At(P^.ScrollBar^.Value);
    if CurFile^.TType = ttUpDir
    then
      begin {все файлы, кроме элемента 0, который есть UpDir}
      New(FC, Init(N-1, 1));
      FC^.Count := N-1;
      Move(SourceFiles^.Items^[1], FC^.Items^, (N-1)*SizeOf(Pointer));
      end
    else
      begin
      New(FC, Init(1, 1));
      FC^.Insert(CurFile);
      end;
    end
  else
    begin
    New(FC, Init(P^.SelNum, 1));
    for I := FirstNameNum(P)-1 to N-1 do
      begin
      FileI := SourceFiles^.At(I);
      if FileI^.Selected then
        FC^.AtInsert(FC^.Count, FileI);
      end;
    end;
  end { GetSelection };

{-DataCompBoy-}
function SelectFiles;
  var
    S, SN: String;
    I: LongInt;
    P: PFilePanelRoot absolute AFP;
    PF: PFileRec; {DataCompBoy}
    J, K: Byte; {JO}
    {SD: String;} {JO}

  begin
  SelectFiles := False;
  if P^.Files^.Count = 0 then
    Exit;
  if not SelectDialog(Select, S, XORs) then
    Exit;
  UpStr(S);
  SelectFiles := True;
  P^.SelNum := 0;
  P^.SelectedLen := 0;
  P^.PackedLen := 0;
  for I := FirstNameNum(P) to P^.Files^.Count do
    begin
    PF := P^.Files^.At(I-1);
    SN := PF^.FlName[uLfn];
    if  ( (PF^.Attr and Directory = 0) { or not Select}) and
        (InFilter(UpStrg(SN), S) xor XORs)
    then
      PF^.Selected := Select;
    {JO}
    if  (PF^.Attr and Directory <> 0) and
        (InDirFilter(UpStrg(SN), S) xor XORs)
    then
      PF^.Selected := Select;
    {/JO}
    (* {работает, но тормозит}
   if (FMSetup.Options and fmoAlwaysCopyDesc <> 0) then
     begin
      SD := GetPossibleDizOwner(1);
      if SD <> '' then
      {$IFNDEF OS2}
      if (LowStrg(MakeFileName(PF^.Name))
      {$ELSE}
      if (LowStrg(PF^.Name)
      {$ENDIF}
        = LowStrg(GetName(GetDizOwner(PF^.Owner^, SD, false)))) then PF^.Selected := false;
     end;
*)
    if  (FMSetup.Options and fmoAlwaysCopyDesc <> 0)
         and (FMSetup.DIZ <> '')
      and (P^.Drive^.DriveType < dtArcFind)
    then
      begin
      K := 1;
      for J := 1 to Length(FMSetup.DIZ) do
        begin
        if  (FMSetup.DIZ[J] = ';') then
          begin
          if  (LowStrg(PF^.FlName[True]) = LowStrg(Copy(FMSetup.DIZ, K,
                   J-K)))
          then
            PF^.Selected := False;
          K := J+1;
          end;
        if J = Length(FMSetup.DIZ) then
          if  (LowStrg(PF^.FlName[True]) = LowStrg(Copy(FMSetup.DIZ, K,
                   J-K+1)))
          then
            PF^.Selected := False;
        end;
      end;
    {JO}
    P^.AddSelected(PF);
    end;
  DrawViews(P);
  end { SelectFiles };
{-DataCompBoy-}

procedure CM_CopyFiles;
  var
    FC: PFilesCollection;
    P: PFilePanelRoot absolute AFP;
  begin
  CurrentDirectory := P^.Drive^.GetRealName;
  if P^.Files^.Count = 0 then
    Exit;
  FC := GetSelection(P, Single);
  if FC = nil then
    Exit;
  P^.Drive^.CopyFiles(FC, P, MoveMode);
  FC^.DeleteAll;
  Dispose(FC, Done);
  end;

{-DataCompBoy-}
procedure InvertSelection;
  var
    I: LongInt;
    PF: PFileRec;
    P: PFilePanelRoot absolute AFP;
    J, K: Byte;
    {SD: String;} {JO}
  begin
  P^.SelNum := 0;
  P^.SelectedLen := 0;
  P^.PackedLen := 0;
  for I := FirstNameNum(P) to P^.Files^.Count do
    begin
    PF := PFileRec(P^.Files^.At(I-1));
    if dr or (PF^.Attr and Directory = 0) then
      PF^.Selected := not PF^.Selected;

    {JO} {работает, но тормозит}
    (*
   if (FMSetup.Options and fmoAlwaysCopyDesc <> 0) then
     begin
      SD := GetPossibleDizOwner(1);
      if SD <> '' then
      {$IFNDEF OS2}
      if (LowStrg(MakeFileName(PF^.Name))
      {$ELSE}
      if (LowStrg(PF^.Name)
      {$ENDIF}
        = LowStrg(GetName(GetDizOwner(PF^.Owner^, SD, false)))) then PF^.Selected := false;
     end;
*)
    if  (FMSetup.Options and fmoAlwaysCopyDesc <> 0)
         and (FMSetup.DIZ <> '')
      and (P^.Drive^.DriveType < dtArcFind)
    then
      begin
      K := 1;
      for J := 1 to Length(FMSetup.DIZ) do
        begin
        if  (FMSetup.DIZ[J] = ';') then
          begin
          if  (LowStrg(PF^.FlName[True]) = LowStrg(Copy(FMSetup.DIZ, K,
                   J-K)))
          then
            PF^.Selected := False;
          K := J+1;
          end;
        if J = Length(FMSetup.DIZ) then
          if  (LowStrg(PF^.FlName[True]) = LowStrg(Copy(FMSetup.DIZ, K,
                   J-K+1)))
          then
            PF^.Selected := False;
        end;
      end;
    {JO}
    P^.AddSelected(PF);
    end;
  DrawViews(P);
  end { InvertSelection };
{-DataCompBoy-}

{-DataCompBoy-}
procedure CM_CompareDirs(AFP, IP: Pointer);
  var
    DD, InThat: PFilesCollection;
    OSM1, OSM2: Word;
    I, J: LongInt;
    PF: PFileRec;
    DT: record
      o: Word;
      FMask: String;
      S: Word;
      end; {-$VOL added, DataCompBoy changed}
    Info: PView;
    P: PFilePanelRoot absolute AFP;
    {$IFDEF Win32}
    D: PDialog; {JO}
    P1: PView; {JO}
    {$ENDIF}
    DialRes: Byte; {JO}
  const
    CompareName = 1;
    CompareDat = 2;
    CompareLength = 4;
    CompareContens = 8;
    {$IFDEF DualName}
  function IsCheckboxes(P: PView): Boolean;
    begin
    IsCheckboxes := TypeOf(P^) = TypeOf(TCheckBoxes);
    end;
  {$ENDIF}
  procedure Cmp1(PF: PFileRec);
    function Cmp(AP: PFileRec): Boolean;
      begin
      UpdateWriteView(Info);
      Cmp := (AP^.Attr and Directory = 0)
             and (P^.Files^.FileCompare(PF, AP) = 0);
      end;
    begin
    UpdateWriteView(Info);
    if DT.S = 0 then
      PF^.Selected := False;
    if  (PF^.Attr and Directory = 0) and
        (InFilter(PF^.FlName[uLfn], DT.FMask)) and {-$VOL}
        (InThat^.FirstThat(@Cmp) = nil)
    then
      PF^.Selected := DT.S = 0;
    end;

  label 1, 2;

  begin { CM_CompareDirs }
  DT.FMask := ComareDirsOptions.FMask;
  DT.o := ComareDirsOptions.o;
  DT.S := ComareDirsOptions.S;
(* !! AK155 04.05.05 временно - надо разобраться
  {$IFDEF DualName}
  D := PDialog(LoadResource(dlgCompareDirs));
  if D = nil then
    Exit;
  {JO: левое условие означает, что показ коротких имён включен в активной }
  {    панели, правое (закомментированное) - в пассивной                  }
  if  (not PFilesCollection(IP)^.LFNActive)
    {or ((P^.Drive^.Flags and psShowLongName) = 0)}
  then
    begin {JO: делаем недоступным чекбокс регистрочувствительности}
    P1 := D^.FirstThat(@IsCheckboxes);
    PCheckBoxes(P1)^.SetButtonState(16, False);
    end;
  D^.SetData(DT);
  DialRes := Desktop^.ExecView(D);
  if DialRes = cmCancel then
    begin
    Dispose(D, Done);
    Exit
    end;
  D^.GetData(DT);
  {$ELSE}
!! *)
  DialRes := ExecResource(dlgCompareDirs, DT);
  if DialRes = cmCancel then
    Exit;
//!!  {$ENDIF}
  if DialRes = cmYes then
    begin
    ConfigModified := True;
    ComareDirsOptions.FMask := DT.FMask;
    ComareDirsOptions.o := DT.o;
    ComareDirsOptions.S := DT.S;
    end;

  if DelSpaces(DT.FMask) = '' then
    DT.FMask := x_x;

  DD := IP;
  OSM1 := PFilesCollection(P^.Files)^.SortMode;
  OSM2 := DD^.SortMode;
  PFilesCollection(P^.Files)^.SortMode := DT.o;
  DD^.SortMode := DT.o;

  Info := nil;
  if DT.o and 8 <> 0 then
    begin
    Info := WriteMsg(GetString(dlComparing));
    end;

  InThat := DD;
  P^.Files^.ForEach(@Cmp1);
  InThat := P^.Files;
  DD^.ForEach(@Cmp1);

  PFilesCollection(P^.Files)^.SortMode := OSM1;
  DD^.SortMode := OSM2;
  Info^.Free;
  end { CM_CompareDirs };
{-DataCompBoy-}

procedure CM_EraseFiles;
  var
    FC: PFilesCollection;
    P: PFilePanelRoot absolute AFP;
  begin
  FC := GetSelection(P, Single);
  if  (FC = nil) or (P^.Drive = nil) then
    Exit;
  P^.Drive^.EraseFiles(FC);
  FC^.DeleteAll;
  Dispose(FC, Done);
  P^.RedrawPanelInfoDir;
  P^.SendLocated;
  end;

procedure CM_MakeList;
  var
    FC: PCollection;
    P: PFilePanelRoot absolute AFP;
  begin
  if  (P^.Files^.Count = 0) then
    Exit;
  if  (P^.SelNum = 0) then
    while SelectFiles(P, True, False) do
      ;
  if  (P^.SelNum = 0) then
    Exit;
  if  (ActivePanel = P) and (P^.Drive^.DriveType = dtDisk)
  then
    CurrentDirectory := P^.DirectoryName;
  FC := GetSelection(P, False);
  if FC = nil then
    Exit;
  MakeListFile(P, FC);
  FC^.DeleteAll;
  Dispose(FC, Done);
  P^.RedrawPanelInfoDir;
  end { CM_MakeList };

{-DataCompBoy-}
procedure CM_SetAttributes;
  var
    D: record
      T: String[8];
      D: String[10];
      T_Cr: String[8];
      D_Cr: String[10];
      T_LAc: String[8];
      D_LAc: String[10];
      S, C: Word;
      end;
    I, UU, DD: LongInt;
    K: Word;
    S: String;
    R: TRect;
    PInfo: PWhileView;
    DTT, DTT1, DTT_Cr, DTT1_Cr, DTT_LAc, DTT1_LAc: LongInt;
    DT, DT1, DT_Cr, DT1_Cr, DT_LAc, DT1_LAc: DateTime;
    DateSet, TimeSet, CrDateSet, CrTimeSet, LAcDateSet, LAcTimeSet:
     Boolean;
    Res: Word;
    Dlg: PDialog;
    P: PFilePanelRoot absolute AFP;
    PF: PFileRec;
    ok: Boolean;
    CmdDlg: Word; {JO}
  label CurTime, CurTime1; {JO}

  function CutNumber(var S: String): Word;
    var
      S1: String[10];
      i, j: Integer;
    begin
    CutNumber := 0;
    if not Ok then
      Exit;
    if S = '' then
      begin
      Ok := False;
      Exit;
      end;
    S1 := '';
    while (not (S[1] in ['0'..'9'])) and (S <> '') do
      Delete(S, 1, 1); {DelFC(S);}
    while (S[1] in ['0'..'9']) and (S <> '') do
      begin
      S1 := S1+S[1]; {AddStr(S1,S[1]);}
      Delete(S, 1, 1); {DelFC(S);}
      end;
    Val(S1, i, j);
    Ok := j = 0;
    CutNumber := i;
    end { CutNumber };

  procedure Swap(var A, B: Word);
  var
    C: Word;
  begin
  C := A;
  A := B;
  B := C;
  end;

  begin { CM_SetAttributes }
  if  (P^.Files^.Count = 0) or (P^.Drive^.DriveType >= dtArcFind) then
    Exit;
  NotifySuspend; {Cat}
  FillChar(D, SizeOf(D), 0);
  if  (P^.SelNum > 0) and not Single then
    begin
    Dlg := PDialog(LoadResource(dlgFilesAttr));
CurTime1:
    Dlg^.SetData(D);
    CmdDlg := Desktop^.ExecView(Dlg);
    case CmdDlg of
      cmYes:
        begin
        D.T := GetDateTime(True);
        D.D := GetDateTime(False);
        D.T_Cr := GetDateTime(True);
        D.D_Cr := GetDateTime(False);
        D.T_LAc := GetDateTime(True);
        D.D_LAc := GetDateTime(False);
        goto CurTime1;
        end;
      cmNo:
        begin
        D.T := '';
        D.D := '';
        D.T_Cr := '';
        D.D_Cr := '';
        D.T_LAc := '';
        D.D_LAc := '';
        goto CurTime1;
        end;
      cmCancel:
        begin
        Dispose(Dlg, Done);
        NotifyResume; {Cat}
        Exit;
        end;
    end {case};
    Dlg^.GetData(D);
    Dispose(Dlg, Done);
    NotifyResume; {Cat}
    end
  else
    begin
    Single := True;
    PF := P^.Files^.At(CurPos);
    if PF^.TType = ttUpDir then
      begin
      NotifyResume; {Cat}
      Exit;
      end;
    S := MakeNormName(PF^.Owner^, PF^.FlName[uLfn]);
    AddStr(S, #0);
    SetLength(S, Length(S)-1);
    Dlg := PDialog(LoadResource(dlgFileAttr));
    D.C := GetFileAttr(S);
    if GetFileAges(S, DTT, DTT_Cr, DTT_LAc) <> 0 then
      begin
      NotifyResume; {Cat}
      Exit;
      end;
    UnpackTime(DTT, DT);
    UnpackTime(DTT_Cr, DT_Cr);
    UnpackTime(DTT_LAc, DT_LAc);
    D.T := FormatDateTime(DT, True);
    D.D := FormatDateTime(DT, False);
    D.T_Cr := FormatDateTime(DT_Cr, True);
    D.D_Cr := FormatDateTime(DT_Cr, False);
    D.T_LAc := FormatDateTime(DT_LAc, True);
    D.D_LAc := FormatDateTime(DT_LAc, False);
    D.S := D.C;
CurTime:
    Dlg^.SetData(D);
    CmdDlg := Desktop^.ExecView(Dlg);
    case CmdDlg of
      cmYes:
        begin
        D.T := GetDateTime(True);
        D.D := GetDateTime(False);
        D.T_Cr := GetDateTime(True);
        D.D_Cr := GetDateTime(False);
        D.T_LAc := GetDateTime(True);
        D.D_LAc := GetDateTime(False);
        goto CurTime;
        end;
      cmNo:
        begin
        D.T := '';
        D.D := '';
        D.T_Cr := '';
        D.D_Cr := '';
        D.T_LAc := '';
        D.D_LAc := '';
        goto CurTime;
        end;
      (*  работает, но падает после n-ного кол-ва раз
 cmSkip:
      begin
      {$IFNDEF OS2}
       Dlg^.EnableCommands([cmYes, cmNo]);
      {$ENDIF}
       Dispose( Dlg, Done );
       NotifyResume; {Cat}
       Message(Application, evKeyDown, kbDown, nil);
       Message(Application, evCommand, cmSingleAttr, nil);
       Exit;
      end;
*)
      cmCancel:
        begin
        {$IFNDEF OS2}
        Dlg^.EnableCommands([cmYes, cmNo]);
        {$ENDIF}
        Dispose(Dlg, Done);
        NotifyResume; {Cat}
        Exit;
        end;
    end {case};
    Dlg^.GetData(D);
    {$IFNDEF OS2}
    Dlg^.EnableCommands([cmYes, cmNo]);
    {$ENDIF}
    Dispose(Dlg, Done);
    D.C := {39}$FF;
    end;

  DTT1 := $FFFFFF;
  DTT1_Cr := $FFFFFF;
  DTT1_LAc := $FFFFFF;

  DateSet := (D.D <> '');
  CrDateSet := (D.D_Cr <> '');
  LAcDateSet := (D.D_LAc <> '');
  TimeSet := (D.T <> '');
  CrTimeSet := (D.T_Cr <> '');
  LAcTimeSet := (D.T_LAc <> '');

  with DT1 do
    begin
    ok := TimeSet;
    // fixme: commented by unxed
    //Hour := CutNumber(D.T);
    //Min := CutNumber(D.T);
    if ok then
      begin
      // fixme: commented by unxed
      //Sec := CutNumber(D.T);
      ok := True;
      end;
    TimeSet := ok and not ((Hour > 24) or (Min > 59) or (Sec > 59));

    ok := DateSet;
    // fixme: commented by unxed
    //Day := CutNumber(D.D);
    //Month := CutNumber(D.D);
    //Year := CutNumber(D.D);
    case CountryInfo.DateFmt of
      0: {MM-DD-YY}
        Swap(Day, Month);
      2: {YY-MM-DD}
        Swap(Day, Year);
    end;
    DateSet := ok and not ((Day > 31) or (Day < 1) or (Month > 12) or
           (Month < 1));

    if DateSet then
      if Year < 80 then
        Inc(Year, 2000)
      else if Year < 100 then
        Inc(Year, 1900);

    end;

  with DT1_Cr do
    begin
    ok := CrTimeSet;
    // fixme: commented by unxed
    //Hour := CutNumber(D.T_Cr);
    //Min := CutNumber(D.T_Cr);
    if ok then
      begin
      // fixme: commented by unxed
      //Sec := CutNumber(D.T_Cr);
      ok := True;
      end;
    CrTimeSet := ok and not ((Hour > 24) or (Min > 59) or (Sec > 59));

    ok := CrDateSet;
    // fixme: commented by unxed
    //Day := CutNumber(D.D_Cr);
    //Month := CutNumber(D.D_Cr);
    //Year := CutNumber(D.D_Cr);
    case CountryInfo.DateFmt of
      0: {MM-DD-YY}
        Swap(Day, Month);
      2: {YY-MM-DD}
        Swap(Day, Year);
    end;
    CrDateSet := ok and not ((Day > 31) or (Day < 1) or (Month > 12) or
           (Month < 1));

    if CrDateSet then
      if Year < 80 then
        Inc(Year, 2000)
      else if Year < 100 then
        Inc(Year, 1900);

    end;

  with DT1_LAc do
    begin
    ok := LAcTimeSet;
    // fixme: commented by unxed
    //Hour := CutNumber(D.T_LAc);
    //Min := CutNumber(D.T_LAc);
    if ok then
      begin
      // fixme: commented by unxed
      //Sec := CutNumber(D.T_LAc);
      ok := True;
      end;
    LAcTimeSet := ok and not ((Hour > 24) or (Min > 59) or (Sec > 59));

    ok := LAcDateSet;
    // fixme: commented by unxed
    //Day := CutNumber(D.D_LAc);
    //Month := CutNumber(D.D_LAc);
    //Year := CutNumber(D.D_LAc);
    case CountryInfo.DateFmt of
      0: {MM-DD-YY}
        Swap(Day, Month);
      2: {YY-MM-DD}
        Swap(Day, Year);
    end;
    LAcDateSet := ok and not ((Day > 31) or (Day < 1) or (Month > 12) or
           (Month < 1));

    if LAcDateSet then
      if Year < 80 then
        Inc(Year, 2000)
      else if Year < 100 then
        Inc(Year, 1900);

    end;

  R.Assign(1, 1, 26, 8);
  New(PInfo, Init(R));
  PInfo^.Top := GetString(dlSetAttr);
  PInfo^.Bottom := '';
  PInfo^.SetState(sfShadow, True);
  Desktop^.Insert(PInfo);

  if Single then
    begin
    UU := CurPos+1;
    DD := UU
    end
  else
    begin
    UU := 1;
    DD := P^.Files^.Count
    end;
  for I := UU to DD do
    begin
    PF := P^.Files^.At(I-1);
    if PF^.Selected or Single then
      begin
      S := Cut(PF^.FlName[uLfn], 40);
      PInfo^.Write(1, S);
      S := MakeNormName(PF^.Owner^, PF^.FlName[uLfn]);
      AddStr(S, #0);
      SetLength(S, Length(S)-1);
      K := (GetFileAttr(S) and (not D.C)) or D.S;
      if {$IFNDEF OS2}(K and Directory = 0) and {$ENDIF}
        TimeSet or DateSet or CrTimeSet or CrDateSet or LAcTimeSet or
         LAcDateSet
      then
        begin
        GetFileAges(S, DTT, DTT_Cr, DTT_LAc);
        UnpackTime(DTT, DT);
        UnpackTime(DTT_Cr, DT_Cr);
        UnpackTime(DTT_LAc, DT_LAc);
        if TimeSet then
          begin
          DT.Hour := DT1.Hour;
          DT.Sec := DT1.Sec;
          DT.Min := DT1.Min;
          end;
        if CrTimeSet then
          begin
          DT_Cr.Hour := DT1_Cr.Hour;
          DT_Cr.Sec := DT1_Cr.Sec;
          DT_Cr.Min := DT1_Cr.Min;
          end;
        if LAcTimeSet then
          begin
          DT_LAc.Hour := DT1_LAc.Hour;
          DT_LAc.Sec := DT1_LAc.Sec;
          DT_LAc.Min := DT1_LAc.Min;
          end;
        if DateSet then
          begin
          DT.Year := DT1.Year;
          DT.Day := DT1.Day;
          DT.Month := DT1.Month;
          end;
        if CrDateSet then
          begin
          DT_Cr.Year := DT1_Cr.Year;
          DT_Cr.Day := DT1_Cr.Day;
          DT_Cr.Month := DT1_Cr.Month;
          end;
        if LAcDateSet then
          begin
          DT_LAc.Year := DT1_LAc.Year;
          DT_LAc.Day := DT1_LAc.Day;
          DT_LAc.Month := DT1_LAc.Month;
          end;

        PackTime(DT, DTT1);
        PackTime(DT_Cr, DTT1_Cr);
        PackTime(DT_LAc, DTT1_LAc);
        DT.Sec := 0;
        DT_Cr.Sec := 0;
        DT_LAc.Sec := 0;
        UnpackTime(DTT1, DT);
        UnpackTime(DTT1_Cr, DT_Cr);
        UnpackTime(DTT1_LAc, DT_LAc);
        if SetFileAges(S, DTT1, DTT1_Cr, DTT1_LAc) = 0 then
{JO: чтобы обновлялась инфоpмация в панелях поиска/ветви}
          begin
          PF^.Yr := DT.Year;
          PF^.YrCreat := DT_Cr.Year;
          PF^.YrLAcc := DT_LAc.Year;
          PF^.Second := DT.Sec;
          PF^.SecondCreat := DT_Cr.Sec;
          PF^.SecondLAcc  := DT_LAc.Sec;
          with TDate4(PF^.FDate) do
            begin
            Month := DT.Month;
            Day := DT.Day;
            Hour := DT.Hour;
            Minute := DT.Min;
            end;
          with TDate4(PF^.FDateCreat) do
            begin
            Month := DT_Cr.Month;
            Day := DT_Cr.Day;
            Hour := DT_Cr.Hour;
            Minute := DT_Cr.Min;
            end;
          with TDate4(PF^.FDateLAcc) do
            begin
            Month := DT_LAc.Month;
            Day := DT_LAc.Day;
            Hour := DT_LAc.Hour;
            Minute := DT_LAc.Min;
            end;
          end;
{/JO}
        end;
      if SetFileAttr(S, K and not Directory
                     and not VolumeID) = 0 then
        {JO: чтобы обновлялась инфоpмация в панелях поиска/ветви}
         PF^.Attr := GetFileAttr(S);
      if not Single then
        PF^.Selected := False;
      end;
    end;
  Desktop^.Delete(PInfo);
  Dispose(PInfo, Done);
  if  (ActivePanel = P) and (P^.Drive^.DriveType = dtDisk)
  then
    CurrentDirectory := P^.DirectoryName;
  RereadDirectory(P^.DirectoryName);
  if P^.Drive^.DriveType = dtDisk then
    GlobalMessage(evCommand, cmRereadInfo, nil);
  GlobalMessage(evCommand, cmRereadTree, @P^.DirectoryName);
  P^.RedrawPanelInfoDir;
  NotifyResume; {Cat}
  end { CM_SetAttributes };
{-DataCompBoy-}


procedure PrepareShowDialog(P: PDialog);
  var
    i: Integer;
  begin
  ObjChangeType(P, TypeOf(TShowDialog));
    { Подменяем диалогу тип, чтобы подсунуть свой HandleEvent}
  ObjChangeType(P^.DirectLink[4], TypeOf(TSaveSetupButton));
    { Подменяем тип кнопки "Записать", чтобы подсунуть свой Press }
  PComboBox(P^.DirectLink[2])^.Items[1].Flags := miDisabled;
    { Сделали недоступным "Не показывать" для выделенных }
  with PComboBox(P^.DirectLink[3])^ do
    for i := 2 to 6 do
      Items[i].Flags := miDisabled;
    { Сделали недоступным "В подвале" и "На разделителе" % упаковки }
  end;

procedure CM_SetShowParms(AFP: Pointer);
  var
    J, K, Old: Word;
    Idx: TDlgIdx;
    P: PFilePanelRoot absolute AFP;
    R: TRect;
  begin
  @PreExecuteDialog := @PrepareShowDialog;
  with P^ do
    begin
    TaggedDataOnly := True;
    TaggedDataCount := 0;
    J := ExecResource(dlgPanelShowSetup, PanSetup^.Show);
    TaggedDataOnly := False;
    TaggedDataCount := 0;
    case J of
      cmOK:
        begin
        K := Min(252, Max(5, StoI(PanSetup^.Show.LFNLen)));
        LFNLen := K;
        EXTLen := Max(0, Min(StoI(PanSetup^.Show.EXTLen), K-5));
        PanSetup^.Show.LFNLen := ItoS(LFNLen);
        PanSetup^.Show.ExtLen := ItoS(ExtLen);
        LFNLonger250 := (LFNLen >= 250)
          {$IFDEF DualName}
          and (PanSetup^.Show.ColumnsMask and psLFN_InColumns <> 0)
          {$ENDIF};
        PDoubleWindow(P^.Owner)^.SetMaxiState(P);
        end;
      else {case}
        Exit;
    end {case};
    DeltaX := 0;
    if (PanSetup^.Show.PathDescrInfo <> fseNotShow) and
       (Drive^.DizOwner = '')
    then
      Drive^.ReadDescrptions(Files);;
    GetBounds(R);
    R.A.Y := 1;
    R.B.Y := Owner^.Size.Y-1;
    ChangeBounds(R);
    Owner^.Redraw;
    end;
  end { CM_SetShowParms };

procedure CM_CopyTemp;
  var
    FC: PFilesCollection;
    C: TCopyRec;
    P: PFilePanelRoot absolute AFP;
  begin
  FC := GetSelection(P, False);
  C.Owner := P;
  C.FC := FC;
  if FC = nil then
    Exit;
  GlobalEvent(evBroadcast, cmCopyToTemp, @C);
  FC^.DeleteAll;
  Dispose(FC, Done);
  end;

{-DataCompBoy-}
procedure CM_ArchiveFiles;
  var
    PC: PCollection;
    S: String;
    P: PFilePanelRoot absolute AFP;
    Dir: DirStr;
    Name: NameStr;
    Ext: ExtStr;
  begin
  if P^.Drive^.DriveType >= dtArcFind then
    Exit;
  PC := GetSelection(P, False);
  if PC = nil then
    Exit;
  if PC^.Count = 1 then
    S := PFileRec(PC^.At(0))^.FlName[uLfn]
  else if P^.Drive^.DriveType = dtDisk then
    S := GetName(P^.DirectoryName)
  else
    S := '';
  {Cat}
  FSplit(S, Dir, Name, Ext); {AK155: не уверен, что это нужно }
  MakeArchive(Name, PC, False, False, P);
  {/Cat}
  PC^.DeleteAll;
  end { CM_ArchiveFiles };
{-DataCompBoy-}

{$IFDEF Printer}
procedure CM_Print;
  var
    N: Integer;
    P: PFilePanelRoot absolute AFP;
  begin
  if  (P^.Drive^.DriveType in [dtDisk, dtFind, dtTemp, dtList]) then
    PrintFiles(GetSelection(P, False), P);
  end;
{$ENDIF}

procedure CM_ToggleDescriptions;
  var
    P: PFilePanelRoot absolute AFP;
  begin
  case P^.Drive^.DriveType of
    dtDisk,
    dtArvid:
      begin
      P^.PanSetup^.Show.ColumnsMask :=
        P^.PanSetup^.Show.ColumnsMask xor psShowDescript;
      P^.DeltaX := 0;
      P^.RereadDir;
      DrawViews(P);
      end;
    dtFind,
    dtList,
    dtArcFind,
    dtTemp:
      begin
      P^.PanSetup^.Show.ColumnsMask :=
        P^.PanSetup^.Show.ColumnsMask xor psShowDir;
      DrawViews(P);
      end;

  end {case};
  end { CM_ToggleDescriptions };

procedure CM_ToggleLongNames;
  var
    P: PFilePanelRoot absolute AFP;
  begin
  if P^.Drive^.DriveType in [dtArc, dtArcFind] then
    Exit;
  {$IFDEF DualName}
  with P^.PanSetup^.Show do
    ColumnsMask := ColumnsMask xor psLFN_InColumns;
  {$ELSE}
  {$IFDEF OS2}
  with P^.Drive^ do
    ShowLogNames := not ShowLogNames;
  {$ENDIF}
  {$ENDIF}
  P^.DeltaX := 0;
  {  P^.RereadDir; }
  {AK155 22-07-2002
Само по себе перечитывание абсолютно не нужно. Может иметь смысл
только входящая в него сортировка. При сортировке не по имени.расширению,
а также если сортировки по длинным и коротким именам/расширениям
отделены, то перечитывание не дает ничего, кроме тормозов. А если
сортировки унифицированы, то есть отдельных сортировок по длинным и
коротким именам/расширениям нет, то при отсутствии перечитывания
все имена на панели остаются на тех же местах и переключение происходит
мгновенно. Ну а если хочется пересортировать - надо нажать Ctrl-R. }
  DrawViews(P);
  end { CM_ToggleLongNames };

procedure CM_ToggleShowMode;
  var
    P: PFilePanelRoot absolute AFP;
  begin
{!! Это переключени в то ли 2, то ли 1, надо просто выкинуть }
(*
  P^.DeltaX := 0;
  if P^.Drive^.Param <> 2 then
    begin
    P^.Drive^.OldParam := P^.Drive^.Param;
    P^.Drive^.GetParam(2)
    end
  else if P^.Drive^.OldParam <> 2 then
    P^.Drive^.GetParam(P^.Drive^.OldParam)
  else
    P^.Drive^.GetParam(1);
  P^.RereadDir;
  DrawViews(P);
  {Message(Desktop, evCommand, cmMaxi, nil);}
*)
  end;

type
  PDragger = ^TDragger;
  TDragger = object(TView)
    Text: PString;
    constructor Init(R: TRect; AText: String);
    procedure Draw; virtual;
    destructor Done; virtual;
    end;

procedure DragMover;
  var
    R: TRect;
    Mover: PView;
    Event: TEvent;
    P: PPoint absolute AP;
    FC: PFilesCollection absolute AFC;
    C: PCopyRec absolute AC;

  function ContainsMouse(P: PView): Boolean;
    begin
    ContainsMouse := (P^.State and sfVisible <> 0) and
      P^.MouseInView(R.A);
    end;

  begin
  {Application^.BFSpeed;}
  Desktop^.MakeLocal(P^, R.A);
  Mover := New(PDragger, Init(R, Text));
  Desktop^.Insert(Mover);
  Desktop^.GetExtent(R);
  Event.Where := P^;
  Event.What := evMouseDown;
  Event.Buttons := mbLeftButton;
  Event.Double := False;
  Mover^.DragView(Event, dmDragMove, R, Mover^.Size, Mover^.Size);
  R.A := Mover^.Origin;
  Mover^.Free;
  if Event.What = evMouseUp then
    {AK155 13-08-2003 Может быть не evMouseUp, если во время
    перетаскивания нажали Esc, см. TView.DragView}
    begin
    C^.FC := FC;
    Desktop^.MakeGlobal(R.A, R.A);
    C^.Where := R.A;
    Message(Desktop^.FirstThat(@ContainsMouse), evBroadcast, cmDropped, C);
    end;
  Dispose(FC, Done);
  end { DragMover };

constructor TDragger.Init;
  begin
  AText := ' '+AText+' ';
  R.B.X := R.A.X+Length(AText);
  R.B.Y := R.A.Y+1;
  inherited Init(R);
  Options := Options or ofTopSelect;
  Text := NewStr(AText);
  SetState(sfShadow, True);
  end;

procedure TDragger.Draw;
  var
    B: TDrawBuffer;
    C: Word;
  begin
  C := $3B30;
  MoveStr(B, Text^, C);
  WriteLine(0, 0, Size.X, Size.Y, B);
  end;

destructor TDragger.Done;
  begin
  DisposeStr(Text);
  inherited Done;
  end;

procedure CM_DragDropper;
  var
    C: TCopyRec;
    FC: PFilesCollection;
    I: LongInt;
    S: String;
    P: PFilePanelRoot absolute AFP;

  begin
  C.Owner := P;
  if  (P^.Files^.Count = 0) or (P^.Files^.Count <= CurPos) then
    Exit;
  if  (PFileRec(P^.Files^.At(CurPos))^.Selected) or
      (TypeOf(P^) <> TypeOf(TFilePanel))
  then
    FC := GetSelection(P, False)
  else if not (PFileRec(P^.Files^.At(CurPos))^.TType = ttUpDir)
  then
    begin
    New(FC, Init(1, 1)); {AK155: заменил 10,10 на 1,1}
    FC^.Insert(P^.Files^.Items^[CurPos]);
    end
  else
    Exit;
  for I := 1 to FC^.Count do
    FC^.AtPut(I-1, CopyFileRec(PFileRec(FC^.Items^[I-1])));
  if FC^.Count = 1 then
    S := Cut(PFileRec(FC^.At(0))^.FlName[uLfn], 20)
  else
    S := ItoS(FC^.Count)+GetString(dlSelectedFiles);
  DragMover(@TEvent(EV^).Where, S, FC, @C);
  end { CM_DragDropper };

procedure CM_Dropped;
  var
    P: PFilePanelRoot absolute AFP;
    MPos: TPoint;
    I, J, K: LongInt;
    Ev: TEvent;
    S: String;
    ColumnTitles: Boolean;
  begin
  if ReflectCopyDirection
  then
    RevertBar := (Message(Desktop, evBroadcast, cmIsRightPanel, P) <> nil)
  else
    RevertBar := False;
  MPos := PCopyRec(EI)^.Where;
  P^.MakeLocal(MPos, MPos);
  ColumnTitles := (P^.Pansetup^.Show.MiscOptions and 2) <> 0;
  I := P^.Delta+(MPos.X div P^.LineLength)
        *(P^.Size.Y-Byte(ColumnTitles))
    +MPos.Y-Byte(ColumnTitles);
  if  (PCopyRec(EI)^.Owner = AFP) and
      ( (MPos.Y < 0) or (I < 0) or (I >= P^.Files^.Count) or
        ( (MPos.Y = 0) and ColumnTitles) or
        (PFileRec(P^.Files^.At(I))^.Attr and Directory = 0))
  then
    Exit;
  {AK155 5-02-2004}
  if (MPos.Y < 0) then
    i := P^.Files^.Count;
      {MPos.Y < 0 - это значит, Drop на DirView (см.
       TFilePanelRoot.CommandHandle, cmDropped). В этом случае
       надо копировать в каталог данной панели. И такое присвоение
       для i гарантирует, что не попадём случайно на какой-то
       подкаталог}
  {/AK155}
  CopyDirName := P^.DirectoryName;
  if PCopyRec(EI)^.Owner <> nil then
    begin
    if  (P^.Drive^.DriveType = dtArc) then
      begin
      CopyDirName := P^.Drive^.GetRealName;
      SkipCopyDialog := Confirms and cfMouseConfirm = 0;
      Message(PCopyRec(EI)^.Owner, evBroadcast, cmCopyCollection,
        PCopyRec(EI)^.FC);
      SkipCopyDialog := False;
      Exit;
      end;
    Ev.What := evBroadcast;
    Ev.Command := cmUnArchive;
    Ev.InfoPtr := EI;
    PCopyRec(EI)^.Owner^.HandleEvent(Ev);
    if Ev.What = evNothing then
      Exit;
    end;
  if  (I < P^.Files^.Count) and (I >= 0) then
    begin
    S := P^.DirectoryName;
    if  ( (MPos.Y > 0) or (not ColumnTitles and
             (MPos.Y = 0)))
      and (PFileRec(P^.Files^.At(I))^.Attr and Directory <> 0)
    then
      S := MakeNormName(S, PFileRec(P^.Files^.At(I))^.FlName[uLfn]);
    CopyDirName := S;
    end;
  SkipCopyDialog := Confirms and cfMouseConfirm = 0;
  if SkipCopyDialog then
    begin
    S := CopyDirName;
    MakeNoSlash(S);
    UpStr(S);
    for J := 0 to PCopyRec(EI)^.FC^.Count-1 do
      with PFileRec(PCopyRec(EI)^.FC^.At(J))^ do
        if  (Attr and Directory <> 0)
                 and (UpStrg(MakeNormName(Owner^, FlName[uLfn])) = S)
        then
          Exit;
    end;
  P^.Drive^.CopyFilesInto(PCopyRec(EI)^.FC, PCopyRec(EI)^.Owner,
    ShiftState and 7 <> 0);
  SkipCopyDialog := False;
  end { CM_Dropped };

procedure DoRenameSingle(PF: PFileRec; const S, S2: String; DlgRes: Word);
{` Собственно переименование; общее для Alt-F6 и Shift-Alt-F6. `}
  var
    SSS: String;
    DosE: Word;
    F: TUseLfn;
  label
    NameErr;
  begin
  if PosChar('/', S) <> 0 then // slash change by unxed
    begin
    DosE := 123; // ERROR_INVALID_NAME
    goto NameErr;
    end;
  ClrIO;
  SSS := PF^.Owner^;
  MakeSlash(SSS);
  lChangeFileName(SSS+S2, SSS+S);
  DosE := IOResult;
  if DosE <> 0 then
    begin
NameErr:
    MessFileNotRename(S2, S, DosE);
    exit;
    end;
  ExportDiz(@PF^.FlName, S, PF^.DIZ, SSS);
{!RLN}  CopyShortString(S, PF^.FlName[True]);
   { Это на случай если стоим как раз на этом имени.
   Чтобы при перечитывании каталога курсор ушёл на новое имя,
   а не остался на старой позиции }

{AK155 25-01-2004
  if not DnIni.AutoRefreshPanels then}
{Поскольку автообновление отключено, надо перечитать. Кроме того,
даже если бы оно было, перечитывание с задержкой здорово раздражает}
  GlobalMessage(evCommand, cmRereadDir, PF^.Owner);
  ClrIO;
  end;

{-DataCompBoy-}
procedure CM_RenameSingleL;
  var
    PIF: PInputFName;
    R: TRect;
    S, S2: String;
    DosE: Word;
    SSS: String;
    PF: PFileRec;
    Nm: String;
    Sn: String;
    P: PFilePanelRoot absolute AFP;
    Event: PEvent absolute PEV;
    ScrollBarValue: Integer;
    ReEnableCmdLine: Boolean;
    DlgRes: Word;

  begin
  if  (P^.Files = nil) then
    Exit;
  ScrollBarValue := P^.ScrollBar^.Value;
  if ScrollBarValue >= P^.Files^.Count then
    Exit;
  PF := P^.Files^.At(ScrollBarValue);
  if  (PF^.TType = ttUpDir) or
    {JO: для найденных в архиве файлов в панели поиска}
    PathFoundInArc(PF^.Owner^) or
    {/JO}
    not (P^.Drive^.DriveType in [dtDisk, dtTemp, dtFind, dtList])
  then
    Exit;
  S := PF^.FlName[uLfn];
  S2 := S;
  {$IFDEF DualName}
  if (FMSetup.Options and fmoDescrByShortNames) <> 0 then
    Nm := PF^.FlName[False]
  else
    {$ENDIF}
    Nm := PF^.FlName[True];
  {$IFDEF DualName}
  if  (P^.PanSetup^.Show.ColumnsMask and psLFN_InColumns = 0)
  then
    R.Assign(0, 0, 13, 1)
  else
    {$ENDIF}
   if (P^.LFNLen < 250)
  then
    R.Assign(0, 0, P^.LFNLen+1, 1)
  else
    R.Assign(P^.CalcLengthWithoutName, 0, 255, 1);
  R.Move(P^.LastCurPos.X, P^.LastCurPos.Y);
  R.Move(P^.Origin.X, 0);
  Dec(R.A.X);
  if R.B.X > P^.Size.X+P^.Origin.X+1 then
    R.B.X := P^.Size.X+P^.Origin.X+1;
  if R.B.X-P^.Origin.X-P^.Size.X = 0 then
    Inc(R.B.X);
  New(PIF, Init(R, 255));
  PIF^.LC := #179;
  PIF^.RC := #179;
  if P^.Origin.X-R.A.X = 1 then
    PIF^.LC := #186
  else if R.B.X-P^.Origin.X-P^.Size.X = 1 then
    PIF^.RC := #186;
  if  (PF^.TType = 0) or (Startup.FMSetup.Show and fmsHiliteFiles = 0)
  then
    PIF^.C[1] := P^.GetColor(1)
  else
    PIF^.C[1] := P^.GetColor(6+PF^.TType);
  PIF^.C[2] := PIF^.C[1];
  PIF^.C[3] := P^.GetColor(4);
  PIF^.C[4] := P^.GetColor(2);
  {$IFDEF RecodeWhenDraw}
  S := CharToOemStr(S);
  {$ENDIF}
  PIF^.SetData(S);
  PIF^.SetValidator(New(PFilterValidator,
         Init([#32..#255]-IllegalCharSet-['\', '/', '*', '?', '"'])));
  PIF^.SelectAll(False);

  {AK155 Чтобы комстрока не забирала курсор себе, отключаем
         ее на время работы }
  ReEnableCmdLine := (CommandLine <> nil) and
    not CommandLine.GetState(sfDisabled);
  if ReEnableCmdLine then
    begin
    CommandLine^.SetState(sfDisabled, True);
    CommandLine^.Update;
    end;

  NotifySuspend; {AK155 25-01-2004 Если не отключить автообновление
      на время редактирования, то при изменеии состава файлов панель
      перерисовывается очень странно, так как строка ввода имеет
      уже зафиксированную позицию }

  PIF^.HelpCtx := hcRenameFile;
  P^.ScrollBar^.Hide;
  DlgRes := P^.Owner.ExecView(PIF);
  PIF^.GetData(S);

  if ReEnableCmdLine then
    CommandLine^.SetState(sfDisabled, False);

  if S[Length(S)] = '.' then
    SetLength(S, Length(S)-1);
  {$IFDEF RecodeWhenDraw}
  if S = CharToOemStr(S2) then
    DlgRes := cmCancel;
  S := OemToCharStr(S);
  {$ELSE}
  if S = S2 then
    DlgRes := cmCancel;
  {$ENDIF}
  if DlgRes <> cmCancel then
    begin
    HistoryAdd(hsRenameFil, S); { Flash 09-06-2003, AK155 21-08-2003 }
    Dispose(PIF, Done);
    DoRenameSingle(PF, S, S2, DlgRes);
(* AK155
    if DlgRes <> cmOK then
      begin {! Так не бывает. Диалог завершается либо по OK, либо по Cancel.
      Какие-то cmNo и cmYes есть при define UPLOWSTRSINDLG, но на кой они
      нужны - непонятно, когда есть Ctrl-], Ctrl-[. Надо их выкинуть.
      }
      Event^.What := evKeyDown;
      Event^.KeyCode := DlgRes;
      Event^.InfoPtr := nil;
      P^.PutEvent(Event^);
      end;
*)
    end
  else
    PIF^.Free;
  NotifyResume; {AK155 25-01-2004}
  end { CM_RenameSingleL };
{-DataCompBoy-}

{-DataCompBoy-}
procedure CM_RenameSingleDialog;
  var
    R: TRect;
    S, S2: String;
    DosE: Word;
    SSS: String;
    PF: PFileRec;
    Nm: String;
    Sn: String;
    DlgRes: Word;
    P: PFilePanelRoot absolute AFP;
    Event: PEvent absolute PEV;
    ScrollBarValue: Integer;
    U: lFile;

  begin
  if  (P^.Files = nil) then
    Exit;
  ScrollBarValue := P^.ScrollBar^.Value;
  if ScrollBarValue >= P^.Files^.Count then
    Exit;
  PF := P^.Files^.At(ScrollBarValue);
  if  (PF^.TType = ttUpDir) or
    {JO: для найденных в архиве файлов в панели поиска}
    PathFoundInArc(PF^.Owner^) or
    {/JO}
    not (P^.Drive^.DriveType in [dtDisk, dtTemp, dtFind, dtList])
  then
    Exit;
  S := PF^.FlName[uLfn];
  {$IFDEF DualName}
  if (FMSetup.Options and fmoDescrByShortNames) <> 0 then
    Nm := PF^.FlName[False]
  else {$ENDIF}
    Nm := PF^.FlName[True];
  S2 := S;
  {$IFDEF RecodeWhenDraw}
  S := CharToOemStr(S);
  {$ENDIF}
  DlgRes := ExecResource(dlgRenFl, S);
  {$IFDEF RecodeWhenDraw}
  S := OemToCharStr(S);
  {$ENDIF}
  if DlgRes = cmCancel then
    Exit;
  if DlgRes = cmYes then
    UpStr(S);
  if DlgRes = cmNo then
    LowStr(S);
  if S[Length(S)] = '.' then
    SetLength(S, Length(S)-1);

  if S = S2 then
    DlgRes := cmCancel;
  if DlgRes <> cmCancel then
    begin
    DoRenameSingle(PF, S, S2, DlgRes);
    end;
  end { CM_RenameSingleDialog };
{-DataCompBoy-}

procedure CM_SortBy;
  var
    Menu: PMenu;
    PM, DefPM: PMenuItem;
    N, W: Word;
    R: TRect;
    PV: PView;
    P: PFilePanelRoot absolute AFP;
    Idx: TStrIdx;
    i: Word;
    Mode: Word;
    wFlags: Word;
    OnOff: array[Boolean] of String[5];
    FlagMask: Word;
    ToggleItem: array[0..3] of PMenuItem;
    PC: TPanelClass;
    CurrentOnly: Boolean;
    OldSortCurPanTypeOnly: Boolean; {JO}
    ActionMenu: PMenu;
  const
    ItemFlags =
      miSubmenu or
      miExecDefault or
      miDoNotDisposeSubmenu or
      miAllowChangeDefault;

  begin
  ActionMenu := NewMenu(
      NewItem(GetString(dlAllPanelTypes), '', kbNoKey, cmYes, 0,
      NewItem(GetString(dlCurPanelType), '', kbNoKey, cmNo, 0,
      nil)));
  if SortCurPanTypeOnly then
    ActionMenu^.Default := ActionMenu^.Items^.Next;

  OnOff[False] := GetString(dlMenuItemOff);
  OnOff[True] := GetString(dlMenuItemOn);

  PM := nil;
  wFlags := P^.PanSetup^.Sort.SortFlags;
  Idx := dlSortDirsByName;
  FlagMask := 1 shl 3;
  for i := 3 downto 0 do
    begin
    PM := NewSubmenu(GetString(Idx), 0, ActionMenu, PM);
    PM^.Flags := ItemFlags or miParam;
    PM^.Param := NewStr(OnOff[(wFlags and FlagMask) <> 0]);
    PM.Command := cmSortOwnerToggle+i;
    FlagMask := FlagMask shr 1;
    ToggleItem[i] := PM;
    Dec(Idx);
    end;
  PM := NewLine(PM);

  Mode := P^.PanSetup^.Sort.SortMode;
  Idx := dlSortUnsorted;
  for i := NumSortModes-1 downto 0 do
    begin
    PM := NewSubmenu(GetString(Idx), 0, ActionMenu, PM);
    PM^.Flags := ItemFlags;
    PM.Command := cmSortName + i;
    if i = Mode then
      DefPM := PM;
    Dec(Idx);
    end;
  Menu := NewMenu(PM);
  Menu^.Default := DefPM;

  W := 16;
  Desktop^.GetExtent(R);
  R.A := P^.Origin;
  P^.Owner^.MakeGlobal(R.A, R.A);
  Desktop^.MakeLocal(R.A, R.A);
  if R.A.X < 0 then
    R.A.X := 0
  else if R.A.X+W > R.B.X then
    R.A.X := R.B.X-W;
  if R.A.Y < 0 then
    R.A.Y := 0
  else if R.A.Y+NumSortModes+1 > R.B.Y then
    R.A.Y := R.B.Y-NumSortModes-1;
  R.B.X := R.A.X+100;
  R.B.Y := R.A.Y+100; {R.B := R.A;}
  PV := New(PMenuBox, Init(R, Menu, nil));
  PV^.HelpCtx := hcSortBy;
  while true do
    begin
    N := Desktop^.ExecView(PV);
    if N = 0 then
      Break;

    CurrentOnly := (N = cmNo);
    N := ParentItem^.Command;
    with P^ do
      begin
      if N < cmSortOwnerToggle then
        begin
        Mode := N - cmSortName;
        if CurrentOnly then
          PanSetup.Sort.SortMode := Mode
        else
          for PC := Low(TPanelClass) to High(TPanelClass) do
            PanelSetupSet[PC].Sort.SortMode := Mode;
        Reorder;
        RedrawPanelInfoDir;
        Break;
        end;
      end;

    { При переключении режимов не закрываем меню }
    i := N-cmSortOwnerToggle;
    wFlags := wFlags xor (1 shl i);
    i := N-cmSortOwnerToggle;
    Menu^.Default := ToggleItem[i];
    with ToggleItem[i] do
      begin
      DisposeStr(Param);
      Param := NewStr(OnOff[(wFlags and (1 shl i)) <> 0]);
      end;
    with P^ do
      begin
      if CurrentOnly then
        PanSetup.Sort.SortFlags := wFlags
      else
        for PC := Low(TPanelClass) to High(TPanelClass) do
          PanelSetupSet[PC].Sort.SortFlags := wFlags;
      Reorder;
      RedrawPanelInfoDir;
      end;
    end;
  Dispose(PV, Done);
  DisposeMenu(Menu);
 {JO}
  OldSortCurPanTypeOnly := SortCurPanTypeOnly;
  SortCurPanTypeOnly := (ActionMenu^.Default = ActionMenu^.Items^.Next);
  if SortCurPanTypeOnly <> OldSortCurPanTypeOnly then
    ConfigModified := True;
 {/JO}
  DisposeMenu(ActionMenu);
  end { CM_SortBy };


type
  PSortDialog = ^TSortDialog;
  TSortDialog = object(TShowDialog)
    function OwnDataAddress(P: PPanelSetup): Pointer; virtual;
    end;

function TSortDialog.OwnDataAddress(P: PPanelSetup): Pointer;
  begin
  Result := @P^.Sort;
  end;

procedure PrepareSortDialog(P: PDialog);
  begin
  ObjChangeType(P, TypeOf(TSortDialog));
    { Подменяем диалогу тип, чтобы подсунуть свой HandleEvent}
  ObjChangeType(P^.DirectLink[1], TypeOf(TSaveSetupButton));
    { Подменяем тип кнопки "Записать", чтобы подсунуть свой Press }
  end;

procedure CM_PanelSortSetup;
  var
    J: Word;
  begin
  with ActivePanel^ do
    begin
    TaggedDataOnly := True;
    TaggedDataCount := 0;
    @PreExecuteDialog := @PrepareSortDialog;
    J := ExecResource(dlgPanelSortSetup, PanSetup^.Sort);
    TaggedDataOnly := False;
    TaggedDataCount := 0;
    if J = cmOK then
      begin
      Reorder;
      RedrawPanelInfoDir;
      end;
    end;
  end { CM_PanelSortSetup };

{-DataCompBoy-}
function CM_ChangeDirectory;
  var
    P: PFilePanelRoot absolute AFP;
    S: String;
  begin
  CM_ChangeDirectory := '';
  if P^.Drive^.DriveType <> dtDisk then
    begin
    ClrIO; {!!!}
    lGetDir(0, S);
    if Abort then
      Exit;
    end
  else
    S := P^.DirectoryName;
  CM_ChangeDirectory :=
    ChangeDir(GetString(dlChangeDir), Byte(S[1])-64);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure CM_MakeDir;
  var
    Dr: String;
    Nm: String;
    Xt: String;
    PF: PFileRec;
    DN: String;
    I, J: LongInt;
    P: PFilePanelRoot absolute AFP;
  begin
  P^.Drive^.MakeDir;
  if CreatedDir = '' then
    Exit;
  lFSplit(CreatedDir, Dr, Nm, Xt);
  MakeNoSlash(Dr);
  if Dr <> P^.DirectoryName then
    Exit;
  J := -1;
  DN := UpStrg(GetName(CreatedDir));
  for I := 1 to P^.Files^.Count do
    begin
    PF := P^.Files^.At(I-1);
    if UpStrg(PF^.FlName[True]) = GetName(DN) then
      begin
      J := I-1;
      Break
      end;
    end;
  if J >= 0 then
    P^.ScrollBar^.SetValue(J);
  P^.RedrawPanelInfoDir;
  end { CM_MakeDir };
{-DataCompBoy-}

{-DataCompBoy-}
procedure CM_LongCopy;
  var
    S: String;
    P: PFilePanelRoot absolute AFP;
    PF: PFileRec;
  begin
  if  (P^.Files^.Count = 0) or (P^.Drive^.DriveType >= dtArcFind) then
    Exit;
  PF := P^.Files^.At(P^.ScrollBar^.Value);
  if  (PF^.Attr and Directory <> 0)
    {JO: для найденных в архиве файлов в панели поиска}
    or PathFoundInArc(PF^.Owner^)
    {/JO}
    then
    Exit;
  S := MakeNormName(PF^.Owner^, PF^.FlName[uLfn]);
  LongCopy(S);
  if  (ActivePanel = P) and (P^.Drive^.DriveType = dtDisk)
  then
    CurrentDirectory := P^.DirectoryName;
  RereadDirectory(P^.DirectoryName);
  if P^.Drive^.DriveType = dtDisk then
    begin
    GlobalMessage(evCommand, cmRereadInfo, nil);
    GlobalMessage(evCommand, cmRereadTree, @(P^.DirectoryName));
    end;
  end { CM_LongCopy };
{DataCompBoy}

procedure CaseAsIs(var S: String; B, E: Byte);
  begin
  end;

procedure CaseLow(var S: String; B, E: Byte);
  begin
  for B := B to E do
    S[B] := LowCase(S[B]);
  end;

procedure CaseCap(var S: String; B, E: Byte);
  begin
  while (S[B] in BreakChars) and (B <= E) do
    Inc(B);
  S[B] := UpCase(S[B]);
  Inc(B);
  while B <= E do
    begin
    S[B] := LowCase(S[B]);
    Inc(B);
    end;
  end;

procedure CaseCapAll(var S: String; B, E: Byte);
  begin
  repeat
    while (B <= E) and (S[B] in BreakChars) do
      Inc(B);
    if B > E then
      Break;
    S[B] := UpCase(S[B]);
    while (B < E) and (not (S[B] in BreakChars)) do
      begin
      Inc(B);
      S[B] := LowCase(S[B]);
      end;
  until B >= E;
  end;

procedure CaseUp(var S: String; B, E: Byte);
  begin
  for B := B to E do
    S[B] := UpCase(S[B]);
  end;

procedure CM_ChangeCase(AFP: Pointer);
  type
    ChPr = procedure (var S: String; B, E: Byte);

  var
    FC: PFilesCollection;
    L: TNamesCaseOptions;
    P: PFilePanelRoot absolute AFP;
    Info: PWhileView;
    TT: TEventTimer;
    R: TRect;
    NameChange, ExtChange: ChPr;

  function DoChangeCase(PF: PFileRec): Boolean;
    var
      S: String;
      SSS: String;
      NM, Sn: String;
      i: Integer;
      U: lFile;
    begin
    if TimerExpired(TT) then
      begin
      DispatchEvents(Info, Abort);
      NewTimer(TT, 2*1000);
      end;
    DoChangeCase := Abort;
    if Abort then
      Exit;
    {JO: для найденных в архиве файлов в панели поиска}
    if PathFoundInArc(PF^.Owner^) then
      Exit;
    {/JO}
    S := PF^.FlName[True];
    NM := S;
    for i := Length(S) downto 1 do
      if S[i] = '.' then
        Break;
    {$IFDEF RecodeWhenDraw}
    if OemToCharStr(CharToOemStr(S)) = S then
      begin
      S := CharToOemStr(S);
      {$ENDIF}
      if S[i] = '.' then
        begin
        NameChange(S, 1, i);
        ExtChange(S, i, Length(S));
        end
      else
        NameChange(S, 1, Length(S));
      {$IFDEF RecodeWhenDraw}
      S := OemToCharStr(S);
      end;
    {$ENDIF}
    if S = PF^.FlName[True] then
      i := 0
    else
      begin
      SSS := PF^.Owner^;
      MakeSlash(SSS);
      S := SSS + S;
      lChangeFileName(S, S);
      i := IOResult;
      end;
    if i <> 0 then
      begin
      Info^.Hide;
      MessFileNotRename(PF^.FlName[True], S, i);
      MessageBox
          (GetString(dlFCNoRename1)+GetString(dlDIFile)+^M^C+Cut(PF^.
          FlName[True], 20)
        +GetString(dlFCNoRename2)+Cut(S, 20), nil, mfError+mfOKButton);
      DoChangeCase := True;
      end
    else
      begin
      CopyShortString(GetName(S), PF^.FlName[True]);
        { В данном случае копировать можно, так как длина заведомо та же}
      ExportDiz(nil, PF^.FlName[True], PF^.DIZ, PF^.Owner^);
(*!
      {JO: при изменении регистра файла имеет хоть какой-то смысл менять регистр }
      {    его имени в файле описаний только если описания даются по длинным     }
      {    именам, так как для коротких имён регистр не меняется                 }

      {$IFDEF DualName}
      if (FMSetup.Options and fmoDescrByShortNames) = 0 then
      {$ENDIF}
        begin
        Sn := PF^.FlName[True];
        S := GetPossibleDizOwner(1);
        if S <> '' then
          begin
          S := GetDizPath(PF^.Owner^, S);
          ExportDiz(nil, PF, '');
          end;
        end;
*)
      Message(P, evCommand, cmCopyUnselect, PF);
      end;
    end { DoChangeCase };

  begin { CM_ChangeCase }
  if  (P^.Drive = nil) then
    Exit;
  FC := GetSelection(P, False);
  if  (FC = nil) then
    Exit;

  L := ChangeNamesCaseOptions;

  if ExecResource(dlgNameCase, ChangeNamesCaseOptions) = cmCancel then
    begin
    FC^.DeleteAll;
    Dispose(FC, Done);
    Exit;
    end;

  if  (L.Name <> ChangeNamesCaseOptions.Name)
    and (L.ext <> ChangeNamesCaseOptions.ext)
  then
    ConfigModified := True;

  if  (ChangeNamesCaseOptions.Name = 0)
       and (ChangeNamesCaseOptions.ext = 0)
  then
    begin
    FC^.DeleteAll;
    Dispose(FC, Done);
    Exit;
    end;

  case ChangeNamesCaseOptions.Name of
    {LoNg FIle nAMe}0:
      NameChange := CaseAsIs;
    {long file name}1:
      NameChange := CaseLow;
    {Long file name}2:
      NameChange := CaseCap;
    {Long File Name}3:
      NameChange := CaseCapAll;
    {LONG FILE NAME}4:
      NameChange := CaseUp;
    else {case}
      Exit;
  end {case};

  case ChangeNamesCaseOptions.ext of
    {LoNg FIle nAMe}0:
      ExtChange := CaseAsIs;
    {long file name}1:
      ExtChange := CaseLow;
    {Long file name}2:
      ExtChange := CaseCap;
    {Long File Name}3:
      ExtChange := CaseCapAll;
    {LONG FILE NAME}4:
      ExtChange := CaseUp;
    else {case}
      Exit;
  end {case};

  R.Assign(0, 0, 20, 7);
  New(Info, Init(R));
  Info^.Write(1, Copy(GetString(dlPleaseStandBy), 4, MaxStringLength));
  Desktop^.Insert(Info);
  NewTimer(TT, 1000);
  Abort := False;
  FC^.FirstThat(@DoChangeCase);
  Abort := False;
  MessageL(P, evCommand, cmPanelReread, 0);
  Info^.Free;
  FC^.DeleteAll;
  Dispose(FC, Done);
  end { CM_ChangeCase };

function PanelSetupTag(const PSS: TPanelSetupSet;
    PC: TPanelClass): String;
  begin
  with PSS[PC].Show do
    begin
    if ColumnsMask = $7FF then
      Result := GetString(dlFullInfo)
    else if ((ColumnsMask and psShowDescript) <> 0) and
      PanelFileColAllowed[PC][psnShowDescript]
    then
      Result := GetString(dlDescr)
    else if ((ColumnsMask and psShowDir) <> 0) and
      PanelFileColAllowed[PC][psnShowDir]
    then
      Result := GetString(dlPath)
    else
      Result := '';
    Result := LFNLen + '.' + ExtLen + ' ' + Result;
    end;
  {!! AK155 28.04.2005 В программе JO было ещё формирование
    признаков длинного и короткого имени ("Д" и "К"), но это
    я пока делать не стал, глядишь, никто и не заметит пропажу.}
  end;

procedure CM_SelectColumn(AFP: Pointer); {JO}
  {` Меню выбора номера блока настроек панели `}
  var
    I: Integer;
    P: PFilePanelRoot absolute AFP;
    Menu: PMenu;
    Items: PMenuItem;
    SelectedItem: PMenuItem;
    R: TRect;
    PV: PView;
    N, W: Word;
    PSetup: PPanelSetup;
    NameI: String;
    PC: TPanelClass;
    OldFullMenuPanelSetup: Boolean; {JO}
    ActionMenu: PMenu;
  const
    ItemFlags =
      miSubmenu or
      miExecDefault or
      miDoNotDisposeSubmenu or
      miAllowChangeDefault;

  begin
  ActionMenu := NewMenu(
      NewItem(GetString(dlAppearanceOnly), '', kbNoKey, cmYes, 0,
      NewItem(GetString(dlFullPanelSetup), '', kbNoKey, cmNo, 0,
      nil)));
 {JO}
  if FullMenuPanelSetup then
    ActionMenu^.Default := ActionMenu^.Items^.Next;
 {/JO}

  PC := dt2pc[P^.Drive^.DriveType];
  Items := NewSubmenu('~-~ ' + GetString(dlUndoPanelSetup),
      0, ActionMenu, nil);
  Items^.Flags := ItemFlags;
  Items^.Command := 16000 + 12;

  Items := NewSubmenu('~=~ ' + GetString(dlOtherPanel),
      0, ActionMenu, Items);
  Items^.Flags := ItemFlags;
  Items^.Command := 16000 + 11;

  for I := 10 downto 1 do
    begin
    NameI := PanelSetupTag(PanSetupPreset[i], PC);
    Items := NewSubmenu('~'+Char(I mod 10 + Byte('0'))+'~ ' + NameI,
      0, ActionMenu, Items);
    Items^.Flags := ItemFlags;
    Items^.Command := 16000 + i;
    if i = P^.PresetNum then
      SelectedItem := Items;
    end;
  Menu := NewMenu(Items);

  N := 9;
  W := 15;
  Desktop^.GetExtent(R);
  R.A := P^.Origin;
  P^.Owner^.MakeGlobal(R.A, R.A);
  Desktop^.MakeLocal(R.A, R.A);
  if R.A.X < 0 then
    R.A.X := 0
  else if R.A.X+W > R.B.X then
    R.A.X := R.B.X-W;
  if R.A.Y < 0 then
    R.A.Y := 0
  else if R.A.Y+N+2 > R.B.Y then
    R.A.Y := R.B.Y-N-2;
  R.B.X := R.A.X+100;
  R.B.Y := R.A.Y+100; {R.B := R.A;}
  PV := New(PMenuBox, Init(R, Menu, nil));
  PV^.HelpCtx := hcSelectPreset;
  Menu^.Default := SelectedItem;
  N := Desktop^.ExecView(PV);
  Dispose(PV, Done);
  DisposeMenu(Menu);
 {JO}
  OldFullMenuPanelSetup := FullMenuPanelSetup;
  FullMenuPanelSetup := (ActionMenu^.Default = ActionMenu^.Items^.Next);
  if FullMenuPanelSetup <> OldFullMenuPanelSetup then
    ConfigModified := True;
 {/JO}
  DisposeMenu(ActionMenu);
  if N = 0 then
    Exit;
  W := ParentItem^.Command;
  if N <> cmYes then
    Inc(W, $10000{см. GetParam});
  P^.GetParam(W-16000);
  end { CM_SelectColumn }; {JO}

{$IFDEF OS2}

procedure CM_SetEALongname(AFP: Pointer; CurPos: Integer); {JO}
  var
    P: PFilePanelRoot absolute AFP;
    PF: PFileRec;
    S: String;
  begin
  if  (P^.Files^.Count = 0) or (P^.Drive^.DriveType >= dtArcFind) then
    Exit;
  PF := P^.Files^.At(CurPos);
  if  (PF^.TType = ttUpDir)
    {JO: для найденных в архиве файлов в панели поиска}
    or PathFoundInArc(PF^.Owner^)
    {/JO}
    then
    Exit;
  S := MakeNormName(PF^.Owner^, PF^.FlName[True]);
  SetEALongname(S);
  end;

{$ENDIF}

end.

