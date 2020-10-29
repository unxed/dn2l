unit Vars;
(******

Access to global variables from Win32 plugins
Written by Cat 2:5030/1326.13
(for use in DN/2 Plugin Version)

******)

{$I STDEFINE.INC}

interface

uses
  Dos, Objects, Objects, FilesCol, Advance, Views, Dialogs,
   Menus,
  DNApp, Archiver, XDblWnd, DnIni, Startup, Calculat, Plugin
  ;

type
  PBoolean = ^Boolean;

  {Global variables}

function FileMode: PLongInt;
function DosError: Integer;
function Application: PProgram;
function Desktop: PDesktop;
function CommandLine: PView;
function StatusLine: PStatusLine;
function MenuBar: PMenuView;
function LngStream: PStream;
function ResourceStream: PStream;
function LStringList: PStringList;
function Resource: PIdxResource;
function EventCatchers: PEventCatcherArray;
procedure AddEventCatchers;
function EventCatchersCount: SmallWord;
procedure SetEventCatchersCount(EventCatchersCount_: SmallWord);
function ArchiveViewers: PArchiveViewerArray;
function StartupDir: String;
function SourceDir: String;
function TempDir: String;
function ArcFile: PStream;
function FileInfo: PFInfo;
function ArcFileName: String;
function VArcFileName: String;
function EvalueError: PBoolean;

{Create some objects}

function NewCollection(A1, A2: LongInt): PCollection;
function NewStringCollection(A1, A2: LongInt; LS: Boolean)
  : PStringCollection;
function NewLineCollection(A1, A2: LongInt; LS: Boolean): PLineCollection;
function NewFilesCollection(A1, A2: LongInt): PFilesCollection;
function NewDosStream(FileName: FNameStr; Mode: Word): PDosStream;
function NewBufStream(FileName: FNameStr; Mode: Word; Size: Word)
  : PBufStream;
function NewView(X1, Y1, X2, Y2: SmallWord): PView;
function NewWindow(X1, Y1, X2, Y2: SmallWord; Title: String;
     Number: SmallWord): PWindow;
function NewDialog(X1, Y1, X2, Y2: SmallWord; Title: String): PDialog;
function NewButton(X1, Y1, X2, Y2: SmallWord; Title: String;
     cm_, bf_: SmallWord): PButton;
function NewLabel(X1, Y1, X2, Y2: SmallWord; S: String; P: PView): PLabel;
function NewStaticText(X1, Y1, X2, Y2: SmallWord; S: String): PStaticText;
function NewInputLine(X1, Y1, X2, Y2: SmallWord; MaxLen: Word): PInputline;
function NewCheckBoxes(X1, Y1, X2, Y2: SmallWord; Items: PSItem)
  : PCheckBoxes;
function NewRadioButtons(X1, Y1, X2, Y2: SmallWord; Items: PSItem)
  : PRadioButtons;
function NewListBox(X1, Y1, X2, Y2: SmallWord; NumCols: Word;
     ScrollBar: PScrollBar): PListBox;
function NewScrollBar(X1, Y1, X2, Y2: SmallWord): PScrollBar;
function NewMenuBox(X1, Y1, X2, Y2: SmallWord; Menu: PMenu;
     ParentMenu: PMenuView): PMenuBox;

{TypeOf}

function TypeOf_TXDoubleWindow(P: PObject): Boolean;

{INI variables}

type
  PIniVars = ^TIniVars;
  TIniVars = record
    {Interface}
    CutDriveInfo: Boolean;
    WinManagerSelectNext: Boolean;
    DriveSelectVCenter: Boolean;
    SystemMenuChar: Byte;
    HorizScrollBarChars: String[6];
    VertScrollBarChars: String[6];
    ReflectCopyDirection: Boolean;
    ReuseViewers: Byte;
    ReuseEditors: Byte;
    HistoryErrorBeep: Boolean;
    PreserveMenuPositions: Boolean;
    PanelDescrArvid: String[255];
    PanelDescrArc: String[255];
    PanelDescrTemp: String[255];
    PanelDescrFind: String[255];
    PanelDescrDrive: String[255];
    UseEnterInViewer: Byte;
    SkipXLatMenu: Boolean;
    EscForOutputWindow: Boolean;
    {Clock}
    ShowSeconds: Boolean;
    BlinkSeparator: Boolean;
    ShowCentury: Boolean;
    ShowDayOfWeek: Boolean;
    DaysOfWeek: String[23];
    RightAlignClock: Boolean;
    {SmartPad}
    SPInsertDate: Boolean;
    SPLineChar: Byte;
    {Game}
    EnableGame: Boolean;
    {Clipboard}
    cbSize: LongInt;
    cbAutoSave: Boolean;
    {Kernel}
    //  CanUseLFN                : Boolean;
    AutoSave: Boolean;
    ShowKeyCode: Byte;
    CopyLimit: LongInt;
    StoreVideoMode: Byte;
    SmartWindowsBoxClose: LongInt;
    //  DoVESATest               : Boolean;
    ForceDefaultArchiver: String[3];
    {Editor}
    UnlimitUnindent: Boolean;
    DrawRShift: Boolean;
    AutoScopeDetect: Boolean;
    ShowBookmarks: Boolean;
    FastBookmark: Boolean;
    DefCodePageView: String[9];
    DefCodePage: String[9];
    FastSearchDeep: LongInt;
    WinManagerPosToEdit: Boolean;
    AutoBracketPairs: String[255];
    F6_DuplicatesLine: Boolean;
    {FilePanels}
    ShowFileMask: Boolean;
    ShowLongName: Boolean;
    QuickRenameInDialog: Boolean;
    UpperCaseSorting: Boolean;
    AutoRefreshDriveLine: Boolean;
    AutoRefreshPanels: Boolean;
    QuickSearchType: Byte;
    {NetInfo}
    NoLevelsInfo: Boolean;
    {Language}
    ActiveLanguage: String[9];
    HelpLanguageOverride: String[9];
    ShowLanguageMenu: Boolean;
    {RegExp}
    RegExpStr0: String[255];
    RegExpStr1: String[255];
    RegExpStr2: String[255];
    RegExpStr3: String[255];
    RegExpStr4: String[255];
    RegExpStr5: String[255];
    RegExpStr6: String[255];
    RegExpStr7: String[255];
    RegExpStr8: String[255];
    RegExpStr9: String[255];
    {SetupStorage}
    SystemDataOpt: LongInt;
    InterfaceDataOpt: LongInt;
    FMSetupOpt: LongInt;
    EditorDefaultsOpt: LongInt;
    EditorDefaultsOpt2: LongInt;
    ViewerOpt: LongInt;
    StartupDataLoad: LongInt;
    StartupDataUnload: LongInt;
    ConfirmsOpt: LongInt;
    NonVIOScreenMode: LongInt;
    VIOScreenMode: LongInt;
    QDirs1: String[255];
    QDirs2: String[255];
    QDirs3: String[255];
    QDirs4: String[255];
    QDirs5: String[255];
    QDirs6: String[255];
    QDirs7: String[255];
    QDirs8: String[255];
    QDirs9: String[255];
    end;

function IniVars: PIniVars;

{CFG variables}

type
  PTetrisRec = ^TTetrisRec;

  {function ChangeNamesCaseOptions: PNamesCaseOptions;}
  {function UUDecodeOptions: AWord;}
  {function UUEncodeData: PUUEncodeData;}
  {function DriveInfoData: AWord;}
  {function StartupData: PStartupData;}
  {function Confirms: Word;}
  {function TerminalDefaults: PTerminalDefaults;}
  {function Archives: PMaskData;}
  {function CustomMask1: PMaskData;}
  {function CustomMask2: PMaskData;}
  {function CustomMask3: PMaskData;}
  {function CustomMask4: PMaskData;}
  {function CustomMask5: PMaskData;}
  {function CustomMask6: PMaskData;}
  {function CustomMask7: PMaskData;}
  {function CustomMask8: PMaskData;}
  {function CustomMask9: PMaskData;}
  {function CustomMask10:PMaskData;}
  {function InterfaceData: PInterfaceData;}
  {function PanelDefaults: PPanelDefaultsData;}
  {function FMSetup: PFMSetup;}
  {function EditorDefaults: PEditorDefaultsData;}
  {function MouseData: PMouseData;}
  {$IFDEF SS}
  {function SaversData: PSaversData;}
  {$ENDIF}
  {function SystemData: PSystemData;}
  {function ColumnsDefaultsDisk: ???;}
  {function ColumnsDefaultsFind: ???;}
  {function ColumnsFindLast: Byte;}
  {function ColumnsDefaultsTemp: ???;}
  {function ColumnsTempLast: Byte;}
  {function ColumnsDefaultsArch: ???;}
  {function ColumnsDefaultsArvd: ???;}
  {function OldColumnsDefaults: ???;}
  {$IFDEF PRINTMANAGER}
  {function RPrinterSetup: ???;}
  {$ENDIF}
function TetrisRec: PTetrisRec;

implementation

(*** Global variables ***)

function FileMode: PLongInt;
  begin
  FileMode := @System.FileMode;
  end;

function DosError: Integer;
  begin
  DosError := Dos.DosError;
  end;

function Application: PProgram;
  begin
  Application := DNApp.Application;
  end;

function Desktop: PDesktop;
  begin
  Desktop := DNApp.Desktop;
  end;

function CommandLine: PView;
  begin
  CommandLine := DNApp.CommandLine;
  end;

function StatusLine: PStatusLine;
  begin
  StatusLine := DNApp.StatusLine;
  end;

function MenuBar: PMenuView;
  begin
  MenuBar := DNApp.MenuBar;
  end;

function LngStream: PStream;
  begin
  LngStream := DNApp.LngStream;
  end;

function ResourceStream: PStream;
  begin
  ResourceStream := DNApp.ResourceStream;
  end;

function LStringList: PStringList;
  begin
  LStringList := DNApp.LStringList;
  end;

function Resource: PIdxResource;
  begin
  Resource := DNApp.Resource;
  end;

function EventCatchers: PEventCatcherArray;
  begin
  EventCatchers := Plugin.EventCatchers;
  end;

procedure AddEventCatchers;
  begin
  Inc(Plugin.EventCatchersCount);
  ReallocMem(Plugin.EventCatchers,
       Plugin.EventCatchersCount*SizeOf(TEventCatcherInfo));
  end;

function EventCatchersCount: SmallWord;
  begin
  EventCatchersCount := Plugin.EventCatchersCount;
  end;

procedure SetEventCatchersCount(EventCatchersCount_: SmallWord);
  begin
  Plugin.EventCatchersCount := EventCatchersCount_;
  end;

function ArchiveViewers: PArchiveViewerArray;
  begin
  ArchiveViewers := @Plugin.ArchiveViewers;
  end;

function StartupDir: String;
  begin
  StartupDir := Advance.StartupDir;
  end;

function SourceDir: String;
  begin
  SourceDir := Advance.SourceDir;
  end;

function TempDir: String;
  begin
  TempDir := Advance.TempDir;
  end;

function ArcFile: PStream;
  begin
  ArcFile := Archiver.ArcFile;
  end;

function FileInfo: PFInfo;
  begin
  FileInfo := @Archiver.FileInfo;
  end;

function ArcFileName: String;
  begin
  ArcFileName := Archiver.ArcFileName;
  end;

function VArcFileName: String;
  begin
  VArcFileName := Archiver.VArcFileName;
  end;

function EvalueError: PBoolean;
  begin
  EvalueError := @Calculat.EvalueError;
  end;

(*** Create some objects ***)

function NewCollection(A1, A2: LongInt): PCollection;
  begin
  NewCollection := New(PCollection, Init(A1, A2));
  end;

function NewStringCollection(A1, A2: LongInt; LS: Boolean)
  : PStringCollection;
  begin
  NewStringCollection := New(PStringCollection, Init(A1, A2, LS));
  end;

function NewLineCollection(A1, A2: LongInt; LS: Boolean): PLineCollection;
  begin
  NewLineCollection := New(PLineCollection, Init(A1, A2, LS));
  end;

function NewFilesCollection(A1, A2: LongInt): PFilesCollection;
  begin
  NewFilesCollection := New(PFilesCollection, Init(A1, A2));
  end;

function NewDosStream(FileName: FNameStr; Mode: Word): PDosStream;
  begin
  NewDosStream := New(PDosStream, Init(FileName, Mode));
  end;

function NewBufStream(FileName: FNameStr; Mode: Word; Size: Word)
  : PBufStream;
  begin
  NewBufStream := New(PBufStream, Init(FileName, Mode, Size));
  end;

function NewView(X1, Y1, X2, Y2: SmallWord): PView;
  var
    R: TRect;
  begin
  R.Assign(X1, Y1, X2, Y2);
  NewView := New(PView, Init(R));
  end;

function NewWindow(X1, Y1, X2, Y2: SmallWord; Title: String;
     Number: SmallWord): PWindow;
  var
    R: TRect;
  begin
  R.Assign(X1, Y1, X2, Y2);
  NewWindow := New(PWindow, Init(R, Title, Number));
  end;

function NewDialog(X1, Y1, X2, Y2: SmallWord; Title: String): PDialog;
  var
    R: TRect;
  begin
  R.Assign(X1, Y1, X2, Y2);
  NewDialog := New(PDialog, Init(R, Title));
  end;

function NewButton(X1, Y1, X2, Y2: SmallWord; Title: String;
     cm_, bf_: SmallWord): PButton;
  var
    R: TRect;
  begin
  R.Assign(X1, Y1, X2, Y2);
  NewButton := New(PButton, Init(R, Title, cm_, bf_));
  end;

function NewLabel(X1, Y1, X2, Y2: SmallWord; S: String; P: PView): PLabel;
  var
    R: TRect;
  begin
  R.Assign(X1, Y1, X2, Y2);
  NewLabel := New(PLabel, Init(R, S, P));
  end;

function NewStaticText(X1, Y1, X2, Y2: SmallWord; S: String): PStaticText;
  var
    R: TRect;
  begin
  R.Assign(X1, Y1, X2, Y2);
  NewStaticText := New(PStaticText, Init(R, S));
  end;

function NewInputLine(X1, Y1, X2, Y2: SmallWord; MaxLen: Word): PInputline;
  var
    R: TRect;
  begin
  R.Assign(X1, Y1, X2, Y2);
  NewInputLine := New(PInputline, Init(R, MaxLen));
  end;

function NewCheckBoxes(X1, Y1, X2, Y2: SmallWord; Items: PSItem)
  : PCheckBoxes;
  var
    R: TRect;
  begin
  R.Assign(X1, Y1, X2, Y2);
  NewCheckBoxes := New(PCheckBoxes, Init(R, Items));
  end;

function NewRadioButtons(X1, Y1, X2, Y2: SmallWord; Items: PSItem)
  : PRadioButtons;
  var
    R: TRect;
  begin
  R.Assign(X1, Y1, X2, Y2);
  NewRadioButtons := New(PRadioButtons, Init(R, Items));
  end;

function NewListBox(X1, Y1, X2, Y2: SmallWord; NumCols: Word;
     ScrollBar: PScrollBar): PListBox;
  var
    R: TRect;
  begin
  R.Assign(X1, Y1, X2, Y2);
  NewListBox := New(PListBox, Init(R, NumCols, ScrollBar));
  end;

function NewScrollBar(X1, Y1, X2, Y2: SmallWord): PScrollBar;
  var
    R: TRect;
  begin
  R.Assign(X1, Y1, X2, Y2);
  NewScrollBar := New(PScrollBar, Init(R));
  end;

function NewMenuBox(X1, Y1, X2, Y2: SmallWord; Menu: PMenu;
     ParentMenu: PMenuView): PMenuBox;
  var
    R: TRect;
  begin
  R.Assign(X1, Y1, X2, Y2);
  NewMenuBox := New(PMenuBox, Init(R, Menu, ParentMenu));
  end;

(*** TypeOf ***)

function TypeOf_TXDoubleWindow(P: PObject): Boolean;
  begin
  TypeOf_TXDoubleWindow := (TypeOf(P^) = TypeOf(TXDoubleWindow));
  end;

(*** INI variables ***)

function IniVars: PIniVars;
  begin
  IniVars := PIniVars(@DnIni.iniparamblock_START);
  end;

(*** CFG variables ***)

function TetrisRec: PTetrisRec;
  begin
  TetrisRec := @Startup.TetrisRec;
  end;

end.

(** Cat:todo
dnapp
  AppPalette: Integer;

dnutil
  TrashCan: PTrashCan;
  HelpWnd: PHelpWindow;
  HelpInUse: Boolean;
  RunMenu: Boolean;

flpanelx
  ActivePanel: Pointer;
  CtrlWas: Boolean;
  DirsToChange: array [0..9] of PString;
  CurrentDirectory: String;
  PShootState: Word;

fviewer
  SearchString: TViewSearch;
  LastViewerBounds: TRect;
  LastViewerDeskSize: TPoint;
  LastEditDeskSize: TPoint;

histries
  CmdStrings: PCollection;
  DirHistory: PCollection;
  EditHistory: PCollection;
  ViewHistory: PCollection;

menus
  MenuActive: Boolean;

messages
  MsgActive : Boolean;

microed
  Clipboard: PCollection;
  ClipboardStream: PStream;
  SearchData: TSearchData;
  MaxCommands: AInt;
  EditCommands: array [1..110] of TEditCommand;

microed2
  TabStep: Integer;
  SmartWindow: PEditWindow;
  ClipboardWindow: PEditWindow;
  SmartWindowPtr: ^PEditWindow;
  ClipboardWindowPtr: ^PEditWindow;

printman
  Printer: PPrintManager;
**)


