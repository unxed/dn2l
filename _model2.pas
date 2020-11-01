unit _Model2;
(******

DN/2 Plugin Interface - functional model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{ сюда попадают те объекты, которые DN/2 напрямую не экспортирует, но }
{ ссылки на которые всё-таки возможно получить какими-либо способами }

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Model1
  ;

type
  PFilePanel = ^TFilePanel;
  TFilePanel = packed record
    VMT: PViewVMT;
    ObjectIsInited: Boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: Byte;
    DragMode: Byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: LongInt;
    OldSizeX: Integer;
    isValid, MSelect, SelectFlag, Loaded, ChangeLocked: Boolean;
    InfoView, DirView, DriveLine: PView;
    Delta, OldDelta, OldPos, DeltaX: LongInt;
    Files: PFilesCollection;
    SortMode: Byte;
    DirectoryName, OldDirectory: String;
    FileMask: String;
    SearchParam: TQuickSearchData;
    ScrollBar: PScrollBar;
    DrawDisableLvl, SelNum, LineLength: LongInt;
    SelectedLen, PackedLen: TSize;
    WasActive, PosChanged, CommandEnabling,
    ViewEnabled: Boolean;
    TotalInfo, FreeSpace: TSize;
    PanelFlags: Word;
    LastDriveFlags: Word;
    Drive: PDrive;
    ForceReading: Boolean;
    DriveState: Word;
    LastCurPos: TPoint;
    end;

  PCommandLine = ^TCommandLine;
  TCommandLine = packed record
    VMT: PViewVMT;
    ObjectIsInited: Boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: Byte;
    DragMode: Byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: LongInt;
    Dir: String;
    DeltaX, CurX: LongInt;
    Overwrite: Boolean;
    LineType: TLineType;
    end;

implementation

end.
