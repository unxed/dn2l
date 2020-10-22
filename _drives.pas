unit _Drives;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Objects, _Streams, _Collect, _Views
  ;

type
  PDrive = ^TDrive;
  TDrive = object(TObject)
    Panel: Pointer;
    Prev: PDrive;
    DriveType: TDriveType;
    CurDir: String;
    DizOwner: String;
    NoMemory: Boolean;
    SizeX: LongInt;
    ColAllowed: array [0..10] of Boolean;
    {$IFDEF OS2}
    ShowLogNames: Boolean;
    {$ENDIF}
    constructor Init(ADrive: Byte; AOwner: Pointer);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); virtual;
    procedure KillUse; virtual;
    procedure lChDir(ADir: String); virtual;
    function GetDir: String; virtual;
    function GetDirectory(const FileMask: String;
        var TotalInfo: TSize): PFilesCollection; virtual;
    procedure CopyFiles(Files: PCollection; Own: PView; MoveMode: Boolean)
      ; virtual;
    procedure CopyFilesInto(Files: PCollection; Own: PView;
         MoveMode: Boolean); virtual;
    procedure EraseFiles(Files: PCollection); virtual;
    procedure UseFile(P: PFileRec; Command: Word); virtual;
    procedure GetFreeSpace(var S: String); virtual;
    function Disposable: Boolean; virtual;
    function GetRealName: String; virtual;
    function GetInternalName: String; virtual;
    procedure GetFull(var B; P: PFileRec; C, Sc: Word); virtual;
    procedure MakeTop(var S: String); virtual;
    procedure RereadDirectory(S: String); virtual;
    procedure GetDown(var B; C: Word; P: PFileRec); virtual;
    procedure HandleCommand(Command: Word; InfoPtr: Pointer); virtual;
    procedure GetDirInfo(var B: TDiskInfoRec); virtual;
    function GetRealDir: String; virtual;
    procedure MakeDir; virtual;
    function isUp: Boolean; virtual;
    procedure ChangeUp(var S: String); virtual;
    procedure ChangeRoot; virtual;
    function GetFullFlags: Word; virtual;
    procedure EditDescription(PF: PFileRec); virtual;
    procedure GetDirLength(PF: PFileRec); virtual;
    destructor Done; virtual;
    function OpenDirectory(const Dir: String;
                                 PutDirs: Boolean): PDrive; virtual;
    procedure DrvFindFile(FC: PFilesCollection); virtual;
    procedure ReadDescrptions(FilesC: PFilesCollection); virtual;
    end;

  PFindDrive = ^TFindDrive;
  TFindDrive = object(TDrive)
    isDisposable: Boolean;
    Files: PFilesCollection;
    Dirs: PSortedCollection;
    ListFile: PString;
    UpFile: PFileRec;
    AMask, AWhat: PString;
    constructor Init(const AName: String; ADirs: PCollection;
         AFiles: PFilesCollection);
    constructor InitList(const AName: String);
    constructor Load(var S: TStream);
    {procedure Store(var S: TStream); virtual;}
    {destructor Done; virtual;}
    procedure NewUpFile;
    {procedure lChDir(ADir: String); virtual;}
    {function GetDirectory(SortMode, PanelFlags: Integer; const FileMask: String; var FreeSpace, TotalInfo: String): PCollection; virtual;}
    {procedure CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {procedure UseFile(P: PFileRec; Command: Word); virtual;}
    {function Disposable: Boolean; virtual;}
    {function GetRealName: String; virtual;}
    {function GetInternalName: String; virtual;}
    {function GetDir: string; virtual;}
    {procedure MakeDir; virtual;}
    {procedure GetFull(var B; P: PFileRec; C, SC: Word); virtual;}
    {procedure GetEmpty(var B; SC: Word); virtual;}
    {procedure GetFreeSpace(var S: String); virtual;}
    {procedure MakeTop(var S: String); virtual;}
    {function IsUp: Boolean; virtual;}
    {procedure ChangeUp(var S: String); virtual;}
    {procedure ChangeRoot; virtual;}
    {procedure RereadDirectory(S: String); virtual;}
    {function GetFullFlags: Word; virtual;}
    {procedure GetDirInfo(var B: TDiskInfoRec); virtual;}
    {procedure GetParam(N: Byte); virtual;}
    end;

  PTempDrive = ^TTempDrive;
  TTempDrive = object(TFindDrive)
    constructor Init;
    constructor Load(var S: TStream);
    {procedure Store(var S: TStream); virtual;}
    {destructor Done; virtual;}
    {procedure CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {function GetRealName: String; virtual;}
    {function GetInternalName: String; virtual;}
    {procedure CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {procedure EraseFiles(AFiles: PCollection); virtual;}
    {procedure GetDirInfo(var B: TDiskInfoRec); virtual;}
    {procedure GetParam(N: Byte); virtual;}
    end;

  PArcDrive = ^TArcDrive;
  TArcDrive = object(TDrive)
    ArcName: String;
    VArcName: String;
    AType: Pointer {PARJArchive};
    Files: Pointer {PDirStorage};
    KillAfterUse: Boolean;
    FakeKillAfterUse: Boolean;
    ArcDate: LongInt;
    ArcSize: Comp;
    ForceRescan: Boolean;
    Password: String;
    constructor Init(const AName, VAName: String; ViewMode: Byte);
    constructor InitCol(PC: Pointer {PDirStorage};
         const AName, VAName: String);
    constructor Load(var S: TStream);
    {procedure Store(var S: TStream); virtual;}
    {destructor Done; virtual;}
    {procedure RereadDirectory(S: String); virtual;}
    {procedure KillUse; virtual;}
    function ReadArchive: Boolean;
    {procedure lChDir(ADir: String); virtual;}
    {function GetDir: String; virtual;}
    {function GetDirectory(SortMode, PanelFlags: Integer; const FileMask: String; var FreeSpace, TotalInfo: String): PCollection; virtual;}
    function Exec(Prg, Cmd: String; Lst: AnsiString; B: Boolean): Boolean;
    {procedure UseFile(P: PFileRec; Command: Word); virtual;}
    function MakeListFile(PC: PCollection; UseUnp: Boolean;
         var B: Boolean): AnsiString;
    {procedure CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {procedure CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {procedure EraseFiles(AFiles: PCollection); virtual;}
    {procedure GetFull(var B; P: PFileRec; C, SC: Word); virtual;}
    {procedure GetEmpty(var B; SC: Word); virtual;}
    {function GetRealName: String; virtual;}
    {function GetInternalName: String; virtual;}
    {procedure MakeTop(var S: String); virtual;}
    {procedure HandleCommand(Command: Word; InfoPtr: Pointer); virtual;}
    {procedure MakeDir; virtual;}
    {function IsUp: Boolean; virtual;}
    {procedure ChangeUp(var S: String); virtual;}
    {procedure ChangeRoot; virtual;}
    procedure ExtractFiles(AFiles: PCollection; ExtrDir: String;
         Own: PView; Options: Byte);
    {procedure GetFreeSpace(var S: String); virtual;}
    {procedure GetDirInfo(var B: TDiskInfoRec); virtual;}
    {function GetFullFlags: Word; virtual;}
    {procedure GetDirLength(PF: PFileRec); virtual;}
    {procedure GetParam(N: Byte); virtual;}
    procedure StdMsg4;
    end;

  PArvidDrive = ^TArvidDrive;
  TArvidDrive = object(TDrive)
    Name: PString;
    Stream: PStream;
    CurFile: LongInt;
    CurDirPos: LongInt;
    PosTableOfs: LongInt;
    CurFileNum: AWord;
    CurLevel: AWord;
    CurDate: LongInt;
    KillAfterUse: Boolean;
    filetype: TAvdType;
    D: TTdrHeader;
    AVT: TAvtHeader;
    TapeFmt: AWord;
    TapeTotalTime: AWord;
    TapeRecordedTime: AWord;
    TotFiles: LongInt;
    TotLen: LongInt;
    CurDirCellPos: LongInt;
    constructor Init(const AName: String);
    {destructor Done; virtual;}
    {procedure lChDir(ADir: String); virtual;}
    {function GetDir: String; virtual;}
    {function GetDirectory(SortMode, PanelFlags: Integer; const FileMask: String; var FreeSpace, TotalInfo: String): PCollection; virtual;}
    {procedure RereadDirectory(S: String); virtual;}
    {procedure KillUse; virtual;}
    {procedure UseFile(P: PFileRec; Command: Word); virtual;}
    {procedure CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {procedure CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {procedure EraseFiles(AFiles: PCollection); virtual;}
    {function  GetRealName: String; virtual;}
    {function  GetInternalName: String; virtual;}
    {procedure HandleCommand(Command: Word; InfoPtr: Pointer); virtual;}
    {procedure MakeDir; virtual;}
    {function  IsUp: Boolean; virtual;}
    {procedure ChangeUp(var S: String); virtual;}
    {procedure ChangeRoot; virtual;}
    {procedure GetFreeSpace(var S: String); virtual;}
    {procedure GetDirInfo(var B: TDiskInfoRec); virtual;}
    {procedure EditDescription(PF: PFileRec); virtual;}
    {procedure GetDirLength(PF: PFileRec); virtual;}
    {procedure GetParam(n: byte); virtual;}
    procedure SeekDirectory;
    end;

  (*
  PXDoubleWindow = ^TXDoubleWindow;
  TXDoubleWindow = object(TWindow)
    isValid: Boolean;
    LeftView, RightView: PHideView;
    RDrive, LDrive: Byte;
    Separator: PSeparator;
    LPanel, RPanel: PFilePanel;
    Info: PHideView;
    NetInfo: PHideView;
    LTree: PHTreeView;
    QView: PFileViewer;
    OldBounds: TRect;
    OldPanelBounds: TRect;
    PanelZoomed: Boolean;
    LType, RType: AInt;
    constructor Init(Bounds: TRect; ANumber, ADrive: Integer);
    procedure InitLeftView(R: TRect);
    procedure InitRightView(R: TRect);
    procedure InitInterior;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    {function Valid(C: Word): Boolean; virtual;}
    {procedure ChangeBounds(var Bounds: TRect); virtual;}
    procedure HandleCommand(var Event: TEvent);
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    {function  GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
  end;
*)

implementation

uses
  _DNFuncs
  ;

constructor TDrive.Init(ADrive: Byte; AOwner: Pointer);
  begin
  _TDrive^.Init(ADrive, AOwner, nil, @Self);
  end;

constructor TDrive.Load(var S: TStream);
  begin
  _TDrive^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TDrive.Store(var S: TStream);
  assembler; {&Frame-}
asm
end;

procedure TDrive.KillUse;
  assembler; {&Frame-}
asm
end;

procedure TDrive.lChDir(ADir: String);
  assembler; {&Frame-}
asm
end;

function TDrive.GetDir: String;
  assembler; {&Frame-}
asm
end;

function TDrive.GetDirectory(const FileMask: String;
        var TotalInfo: TSize): PFilesCollection;
  assembler; {&Frame-}
asm
end;

procedure TDrive.CopyFiles(Files: PCollection; Own: PView;
     MoveMode: Boolean);
  assembler; {&Frame-}
asm
end;

procedure TDrive.CopyFilesInto(Files: PCollection; Own: PView;
     MoveMode: Boolean);
  assembler; {&Frame-}
asm
end;

procedure TDrive.EraseFiles(Files: PCollection);
  assembler; {&Frame-}
asm
end;

procedure TDrive.UseFile(P: PFileRec; Command: Word);
  assembler; {&Frame-}
asm
end;

procedure TDrive.GetFreeSpace(var S: String);
  assembler; {&Frame-}
asm
end;

function TDrive.Disposable: Boolean;
  assembler; {&Frame-}
asm
end;

function TDrive.GetRealName: String;
  assembler; {&Frame-}
asm
end;

function TDrive.GetInternalName: String;
  assembler; {&Frame-}
asm
end;

procedure TDrive.GetFull(var B; P: PFileRec; C, Sc: Word);
  assembler; {&Frame-}
asm
end;

procedure TDrive.RereadDirectory(S: String);
  assembler; {&Frame-}
asm
end;

procedure TDrive.MakeTop(var S: String);
  assembler; {&Frame-}
asm
end;

procedure TDrive.GetDown(var B; C: Word; P: PFileRec);
  assembler; {&Frame-}
asm
end;

procedure TDrive.HandleCommand(Command: Word; InfoPtr: Pointer);
  assembler; {&Frame-}
asm
end;

procedure TDrive.GetDirInfo(var B: TDiskInfoRec);
  assembler; {&Frame-}
asm
end;

function TDrive.GetRealDir: String;
  assembler; {&Frame-}
asm
end;

procedure TDrive.MakeDir;
  assembler; {&Frame-}
asm
end;

function TDrive.isUp: Boolean;
  assembler; {&Frame-}
asm
end;

procedure TDrive.ChangeUp(var S: String);
  assembler; {&Frame-}
asm
end;

procedure TDrive.ChangeRoot;
  assembler; {&Frame-}
asm
end;

function TDrive.GetFullFlags: Word;
  assembler; {&Frame-}
asm
end;

procedure TDrive.EditDescription(PF: PFileRec);
  assembler; {&Frame-}
asm
end;

procedure TDrive.GetDirLength(PF: PFileRec);
  assembler; {&Frame-}
asm
end;

procedure TDrive.ReadDescrptions(FilesC: PFilesCollection);
  assembler; {&Frame-}
asm
end;

function TDrive.OpenDirectory(const Dir: String; PutDirs: Boolean): PDrive;
  assembler; {&Frame-}
asm
end;

procedure DrvFindFile(FC: PFilesCollection);
  assembler; {&Frame-}
asm
end;

destructor  TDrive.Done;
  assembler; {&Frame-}
asm
end;

constructor TFindDrive.Init(const AName: String; ADirs: PCollection;
     AFiles: PFilesCollection);
  begin
  _TFindDrive^.Init(AName, _Model1.PCollection(ADirs),
     _Model1.PFilesCollection(AFiles), nil, @Self);
  end;

constructor TFindDrive.InitList(const AName: String);
  begin
  _TFindDrive^.InitList(AName, nil, @Self);
  end;

constructor TFindDrive.Load(var S: TStream);
  begin
  _TFindDrive^.Load(_Model1.TStream(S), nil, @Self);
  end;

procedure TFindDrive.NewUpFile;
  begin
  _TFindDrive^.NewUpFile(@Self);
  end;

constructor TTempDrive.Init;
  begin
  _TTempDrive^.Init(nil, @Self);
  end;

constructor TTempDrive.Load(var S: TStream);
  begin
  _TTempDrive^.Load(_Model1.TStream(S), nil, @Self);
  end;

constructor TArcDrive.Init(const AName, VAName: String; ViewMode: Byte);
  begin
  _TArcDrive^.Init(AName, VAName, ViewMode, nil, @Self);
  end;

constructor TArcDrive.InitCol(PC: Pointer {PDirStorage};
     const AName, VAName: String);
  begin
  _TArcDrive^.InitCol(PC, AName, VAName, nil, @Self);
  end;

constructor TArcDrive.Load(var S: TStream);
  begin
  _TArcDrive^.Load(_Model1.TStream(S), nil, @Self);
  end;

function TArcDrive.ReadArchive: Boolean;
  begin
  Result := _TArcDrive^.ReadArchive(@Self);
  end;

function TArcDrive.Exec(Prg, Cmd: String; Lst: AnsiString; B: Boolean)
  : Boolean;
  begin
  Result := _TArcDrive^.Exec(Prg, Cmd, Lst, B, @Self);
  end;

function TArcDrive.MakeListFile(PC: PCollection; UseUnp: Boolean;
     var B: Boolean): AnsiString;
  begin
  Result := _TArcDrive^.MakeListFile(_Model1.PCollection(PC), UseUnp, B,
       @Self);
  end;

procedure TArcDrive.ExtractFiles(AFiles: PCollection; ExtrDir: String;
     Own: PView; Options: Byte);
  begin
  _TArcDrive^.ExtractFiles(_Model1.PCollection(AFiles), ExtrDir,
     _Model1.PView(Own), Options, @Self);
  end;

procedure TArcDrive.StdMsg4;
  begin
  _TArcDrive^.StdMsg4(@Self);
  end;

constructor TArvidDrive.Init(const AName: String);
  begin
  _TArvidDrive^.Init(AName, nil, @Self);
  end;

procedure TArvidDrive.SeekDirectory;
  begin
  _TArvidDrive^.SeekDirectory(@Self);
  end;

end.
