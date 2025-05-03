{AK155 = Alexey Korop, 2:461/155@fidonet}

unit PDSetup;
  { типы и переменные, связанные с файловыми панелями и
    вставленными в них дисками }

{$I stdefine.inc}

interface

uses
  Commands;

type
  TPanelShowSetup = record
  {` Блок данных диалога настроек вида панели dlgPanelShowSetup }
    ColumnsMask: Word; {Checkbox[11]}
    DirRegister: Word; {Combo}
    FileRegister: Word; {Combo}
    LFNLen: String[3];
    EXTLen: String[3];
    TabulateExt: Word; {Combo}
    NoTabulateDirExt: Word; {Checkbox[1]}
    ShowCurFile: Word; {Checkbox[1]}
    CurFileNameType: Word; {Combo}
    SelectedInfo: Word; {Combo}
    FilterInfo: Word; {Combo}
    PathDescrInfo: Word; {Combo}
    PackedSizeInfo: Word; {Combo}
    BriefPercentInfo: Word; {Combo}
    LFN_InFooter: Word; {Combo}
    TotalsInfo: Word; {Combo}
    FreeSpaceInfo: Word; {Combo}
    MiscOptions: Word; {` Checkbox[2]: ZoomPanel, ShowTitles `}
    end;
  {`}

  TPanelSortSetup = record
  {` Блок данных диалога настроек сортировки панели dlgPanelSortSetup }
    SortMode: Word;
    SortFlags: Word;
    Ups: array[1..4] of Word;
    CompareMethod: Word;
    end;
  {`}

  PPanelSetup = ^TPanelSetup;
  {`2 Блок настроек панели }
  TPanelSetup = record
    Show: TPanelShowSetup;
    Sort: TPanelSortSetup;
    FileMask: String;
    end;
  {`}

  TPanelClass = (pcDisk, pcList, pcArc, pcArvid);
    {` Классы файловых панелей, для каждого из которых имеется
     свой блок настроек типа TPanelSetup `}

  PPanelSetupSet = ^TPanelSetupSet;
    {`2 полный набор настроек для всех классов панелей }
  TPanelSetupSet = array[TPanelClass] of TPanelSetup;
  {`}

  TDriveType = (dtUndefined, dtDisk, dtFind, dtTemp, dtList, dtArcFind,
       dtArc, dtNet, dtLink, dtArvid);

var
  PanSetupPreset: array[1..10] of TPanelSetupSet;
    {` 10 блоков настроек, которые выбираются по Ctrl-цифра `}

const
  dt2pc: array[TDriveType] of TPanelClass =
    (pcDisk, pcDisk, pcList, pcList, pcList, pcList,
     pcArc, pcDisk, pcDisk, pcArvid);

const // Значения
{$IFDEF DualName}
  cfnTypeOther = 0;
    {` Значение CurFileNameType "Не такое, как в панели" `}
  cfnAlwaysLong = 1;
{$ELSE}
  cfnAlwaysLong = 0;
    {` Значение CurFileNameType "Всегда длинное" `}
{$ENDIF}
  cfnHide = cfnAlwaysLong+1;
    {` Значение CurFileNameType "Не показывать" `}

type
  TFileColWidht = array[TFileColNumber] of ShortInt;
    {` Ширина колонок (кроме имени).
      Для колонок неограниченной ширины (путь, описание) ширина -1.
      Для колонки времени, ширина которой зависит от установок страны,
    ширина искусственно -2 (фактически - 6 при 24-часовом и 7 при
    12-часовом формате).
      Необходима согласованность:
       - порядка значений в этом массиве,
       - битовых масок вроде psShowDescript,
       - работы функций, формирующих соответствующие строки в колонках
         (GetFull, MakeDate, FileSizeStr).
      Это обозначает также, что все однотипные колонки (даты и времена)
    имеют одинаковую ширину.
     `}
  TFileColAllowed = array [TFileColNumber] of Boolean;
    {` Допустимость колонок для данного типа панели `}

const
  FileColWidht: TFileColWidht =
{Size  PSize Ratio Date Time CrDate CrTime LaDate LaTime Descr Path}
 (10,  11,   4,    9,   -2,  9,     -2,    9,     -2,    -1,   -1);

  PanelFileColAllowed: array[TPanelClass] of TFileColAllowed =
  ( {pcDisk}
 (True, False,False, True,True,True, True, True,  True,  True,  False)
  , {pcList}
 (True, False,False, True,True,True, True, True,  True,  False, True)
  , {pcArc}
 (True, True, True, True,True,False,False,False, False, False, False)
  , {pcArvid}
 (True, False,False, True,True,False,False,False, False, True,  False)
  );

procedure DefaultInit;
{` Мне было лень выписывать структурные константы со всеми
настройками всех режимов, поэтому я перетащил сюда старые настроки
колонок в виде вспомогательной констаны, а заполнение пресетов
сделал программой DefaultInit. Сейчас она вызывается только при
инициализации, но при случае можно будет сделать её вызов по
(новой) команде "Восстановить умолчания". `}

implementation

const
  ColumnsDefaults: array[TPanelClass] of array[1..10] of
    record
      Param: Word;
      LFNLen: String[3];
      EXTLen: String[3];
      end =
  (
(*
  psShowSize = $0001;
  psShowDate = $0002;
  psShowTime = $0004;
  psShowCrDate = $0008;
  psShowCrTime = $0010;
  psShowLADate = $0020;
  psShowLATime = $0040;
  psShowDescript = $0080;
  psShowDir = $0100;
  psShowPacked = $0200;
  psShowRatio = $0400;
*)
//  ColumnsDefaultsDisk1
    ((Param: 0; LFNLen: '12'; EXTLen: '3'),
     (Param: $7FF; LFNLen: '12'; EXTLen: '3'),
     (Param: 0; LFNLen: '18'; EXTLen: '4'),
     (Param: 0; LFNLen: '18'; EXTLen: '0'),
     (Param: 0; LFNLen: '38'; EXTLen: '4'),
     (Param: 0; LFNLen: '252'; EXTLen: '0'),
     (Param: psShowSize; LFNLen: '252'; EXTLen: '0'),
     (Param: psShowDescript; LFNLen: '12'; EXTLen: '3'),
     (Param: 0; LFNLen: '12'; EXTLen: '3'),
     (Param: 0; LFNLen: '12'; EXTLen: '3')
    ),

//  ColumnsDefaultsFind
    ((Param: psShowDir; LFNLen: '12'; EXTLen: '3'),
     (Param: $7FF; LFNLen: '12'; EXTLen: '3'),
     (Param: 0; LFNLen: '18'; EXTLen: '4'),
     (Param: 0; LFNLen: '18'; EXTLen: '0'),
     (Param: 0; LFNLen: '38'; EXTLen: '4'),
     (Param: 0; LFNLen: '252'; EXTLen: '0'),
     (Param: psShowSize; LFNLen: '252'; EXTLen: '0'),
     (Param: psShowDir; LFNLen: '12'; EXTLen: '3'),
     (Param: 0; LFNLen: '12'; EXTLen: '3'),
     (Param: 0; LFNLen: '12'; EXTLen: '3')
    ),

//  ColumnsDefaultsArch
    ((Param: 0; LFNLen: '12'; EXTLen: '3'),
     (Param: $7FF; LFNLen: '12'; EXTLen: '3'),
     (Param: 0; LFNLen: '18'; EXTLen: '4'),
     (Param: 0; LFNLen: '18'; EXTLen: '0'),
     (Param: 0; LFNLen: '38'; EXTLen: '4'),
     (Param: 0; LFNLen: '252'; EXTLen: '0'),
     (Param: psShowSize+psShowPacked+psShowRatio;
        LFNLen: '252'; EXTLen: '0'),
     (Param: psShowSize+psShowPacked+psShowRatio;
        LFNLen: '12'; EXTLen: '3'),
     (Param: psShowRatio; LFNLen: '12'; EXTLen: '3'),
     (Param: 0; LFNLen: '12'; EXTLen: '3')
    ),

//  ColumnsDefaultsArvd:
    ((Param: 0; LFNLen: '12'; EXTLen: '3'),
     (Param: $7FF; LFNLen: '12'; EXTLen: '3'),
     (Param: 0; LFNLen: '18'; EXTLen: '4'),
     (Param: 0; LFNLen: '18'; EXTLen: '0'),
     (Param: 0; LFNLen: '38'; EXTLen: '4'),
     (Param: 0; LFNLen: '252'; EXTLen: '0'),
     (Param: psShowSize; LFNLen: '252'; EXTLen: '0'),
     (Param: psShowDescript; LFNLen: '12'; EXTLen: '3'),
     (Param: 0; LFNLen: '12'; EXTLen: '3'),
     (Param: 0; LFNLen: '12'; EXTLen: '3')
    )
  );

procedure DefaultInit;
  var
    i: Integer;
    pc: TPanelClass;
  begin
  FillChar(PanSetupPreset, SizeOf(PanSetupPreset), 0);
  for i := 1 to 10 do
    for pc := Low(TPanelClass) to High(TPanelClass) do
      with PanSetupPreset[i][pc] do
        begin
        FileMask := '*.*';
        Show.ColumnsMask  := ColumnsDefaults[pc][i].Param
           {$IFDEF DualName} or psLFN_InColumns {$ENDIF} ;
        Show.LFNLen := ColumnsDefaults[pc][i].LFNLen;
        Show.ExtLen := ColumnsDefaults[pc][i].ExtLen;
        if Show.ColumnsMask <> 0 then
          Show.MiscOptions := 2; { заголовки колонок }

        Show.TabulateExt := 3; {Всегда}
        Show.FilterInfo := fseInDivider;
        Show.ShowCurFile := 1;
        Show.SelectedInfo := fseInDivider;
        Sort.SortMode := psmLongExt;
        Sort.CompareMethod := 2; { на нижнем регистре }
        Sort.Ups[1] := upsDirs;
        end;
  end;

begin
DefaultInit;
end.

