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

unit Archiver;

interface

uses
  Files,
  Views, Defines, Objects2, Streams, Collect
  ;

const
  DNARC = 'ARCHIVER.INI'#0;

type
  TStr4 = String[4];
  TStr5 = String[5];
  CompRec = record lo, hi: Longint end;

  PArcFile = ^TArcFile;
  TArcFile = record
    FName: PString;
    Select: Boolean;
    Attr: AWord;
    USize, PSize: Comp;
      {AK155: размер в архиве - такой, какой он есть,
        и он может быть большим, даже если текущая плаформа
        не поддерживает больших файлов. Поэтому тут надо
        использовать именно Comp, а не TFileSize }
    Date: LongInt;
    end;

  PFInfo = ^TFInfo;
  TFInfo = record
    FName: String; {DataCompBoy}
    USize, PSize: Comp;
      {AK155: см. выше }
    Date: LongInt;
    Attr: Byte;
    Last: Byte;
    { 0 - not last    }
    { 1 - archive end }
    { 2 - broken arc  }
    end;

  PARJArchive = ^TARJArchive;
  TARJArchive = object(TObject)
    Packer,
    UnPacker,
    Extract,
    ExtractWP,
    Add, Move, Garble,
    Delete,
    Test,
    IncludePaths,
    ExcludePaths,
    ForceMode,
    RecoveryRec,
    SelfExtract,
    Solid,
    RecurseSubDirs,
    SetPathInside,
    StoreCompression,
    FastestCompression,
    FastCompression,
    NormalCompression,
    GoodCompression,
    UltraCompression,
    ComprListChar, {JO}
    ExtrListChar: PString;
    {OldListChar: Char;} {/JO}
    AllVersion: Boolean; {Checkbox}
    {AK155}
    PutDirs: Boolean; {Checkbox}
    {JO}
    {$IFNDEF DPMI32}
    ShortCmdLine: Boolean; {Checkbox}
    {$ELSE}
    SwapWhenExec: Boolean; {Checkbox}
    {$ENDIF}
    {/JO}
    {$IFNDEF OS2}
    UseLFN: Boolean; {Checkbox}
    {$ENDIF}
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    procedure Save;
    function GetVal(const Sign, AFile, Name, Default: PChar): String;
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
    destructor Done; virtual;
    end;

  PFileInfo = ^TFileInfo;
  TFileInfo = object(TSortedCollection)
    function Compare(P1, P2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    end;

  {$IFDEF PLUGIN} {$UNDEF MINARCH} {$ENDIF}

const
  arcACE = 0;
  sigACE {: TStr4} = 'ACE:';
  arcARJ = 3;
  sigARJ {: TStr4} = 'ARJ:';
  arcCAB = 6;
  sigCAB {: TStr4} = 'CAB:';
  arcHA = 8;
  sigHA {: TStr4} = 'HA:';
  arcLHA = 13;
  sigLHA {: TStr4} = 'LHA:';
  arcRAR = 16;
  sigRAR {: TStr4} = 'RAR:';
  arcZIP = 22;
  sigZIP {: TStr4} = 'ZIP:';
  {$IFNDEF MINARCH} {       }
  arcAIN = 1;
  sigAIN {: TStr4} = 'AIN:';
  arcARC = 2;
  sigARC {: TStr4} = 'ARC:';
  arcBS2 = 4;
  sigBS2 {: TStr4} = 'BS2:';
  arcBSA = 5;
  sigBSA {: TStr4} = 'BSA:';
  arcCHZ = 7;
  sigCHZ {: TStr4} = 'CHZ:';
  arcHAP = 9;
  sigHAP {: TStr4} = 'HAP:';
  arcHPK = 10;
  sigHPK {: TStr4} = 'HPK:';
  arcHYP = 11;
  sigHYP {: TStr4} = 'HYP:';
  arcIS3 = 12;
  sigIS3 {: TStr4} = 'IS3:';
  arcLIM = 14;
  sigLIM {: TStr4} = 'LIM:';
  arcQUARK = 15;
  sigQUARK {: TStr4} = 'QRK:';
  arcSQZ = 17;
  sigSQZ {: TStr4} = 'SQZ:';
  arcTAR = 18;
  sigTAR {: TStr4} = 'TAR:';
  arcTGZ = 19;
  sigTGZ {: TStr4} = 'TGZ:';
  arcUC2 = 20;
  sigUC2 {: TStr4} = 'UC2:';
  arcUFA = 21;
  sigUFA {: TStr4} = 'UFA:';
  arcZOO = 23;
  sigZOO {: TStr4} = 'ZOO:';
  arcZXZ = 24;
  sigZXZ {: TStr4} = 'ZXZ:';
  arc7Z  = 25;
  sig7Z  {: TStr4} = '7Z:';
  arcBZ2 = 26;
  sigBZ2 {: TStr4} = 'BZ2:';
  {$ENDIF}
  arcUNK = 255; {UNKNOWN Archiver}

  NumSupportedArchs = 27;

  PPacker: PChar = 'Packer';
  PUnPacker: PChar = 'Unpacker';
  PExtract: PChar = 'Extract';
  PExtractWP: PChar = 'ExtractWithPathnames';
  PAdd: PChar = 'Add';
  PMove: PChar = 'Move';
  PDelete: PChar = 'Delete';
  PGarble: PChar = 'Garble';
  PTest: PChar = 'Test';
  PIncludePaths: PChar = 'IncludePaths';
  PExcludePaths: PChar = 'ExcludePaths';
  PForceMode: PChar = 'ForceMode';
  PRecoveryRec: PChar = 'RecoveryRecord';
  PSelfExtract: PChar = 'SFX';
  PSolid: PChar = 'Solid';
  PRecurseSubDirs: PChar = 'RecurseSubDirs';
  PSetPathInside: PChar = 'SetPathInsideArchive';
  PStoreCompression: PChar = 'StoreCompression';
  PFastestCompression: PChar = 'FastestCompression';
  PFastCompression: PChar = 'FastCompression';
  PNormalCompression: PChar = 'NormalCompression';
  PGoodCompression: PChar = 'GoodCompression';
  PUltraCompression: PChar = 'BestCompression';
  PComprListChar: PChar = 'ComprListChar';
  PExtrListChar: PChar = 'ExtrListChar';
  PAllVersion: PChar = 'AllVersion';
  PPutDirs: PChar = 'PutDirs';
  {$IFNDEF DPMI32}
  PShortCmdLine: PChar = 'ShortCmdLine';
  {$ELSE}
  PSwapWhenExec: PChar = 'SwapWhenExec';
  {$ENDIF}
  PUseLFN: PChar = 'UseLFN';

  DefaultArchiver: AWord = arcRAR;
  DefaultArcMode: AWord = 256*(64+1)+5;
  UnarchiveOpt: Byte = 1; {JO}
  DefaultAddArchiver: AWord = arcUNK;

const
  ArcFile: PBufStream = nil;
var
  FileInfo: TFInfo;
  ArcPos: TFileSize;

const
  ArcFileName: String = ''; {DataCompBoy}
  VArcFileName: String = ''; {JO}
  PReader: PView = nil;

function ArchiveFiles(const S: String; Files: PCollection;
     MoveMode: Boolean; Owner: Pointer): Boolean;
procedure MakeArchive(S: String; Files: PCollection;
     MoveMode, AddToExisting: Boolean; Owner: Pointer);
procedure UnarchiveFiles(const FName: String);
procedure SkipSFX;
  {` Устанавливает ArcPos на начало собственно архива.
  Перед вызовом ArcFile^ должен быть уже открыт и позиционирован
  на начало. После вызова позиция в ArcFile^ не определена `}
function _Cardinal(L: LongInt): Real; {piwamoto}
function FromOct(S: String): TFileSize; {fixed by piwamoto}
function CheckForSpaces(S: String): Boolean; { Flash }

const
  CDir: String = '';
  ArcBufSize = 512;

implementation

uses
  Lfn, Advance, Advance1, Advance2, DNApp, Commands,
  Dialogs, FilesCol, FViewer, Startup,
  ArcView, FileCopy, HistList, {FStorage,}Menus, ArchDet,
   {UserSavr,}DnIni, Messages,
  {JO}Memory, VideoMan, DnExec {$IFDEF Win32}, VpSysLow {$ENDIF}
  {/JO:  добавил для функции ArcExec}
  , Eraser {JO: для разархивирования через временный подкаталог}
  , UserMenu {JO: для скрывания панелей при разархивировании }
  , Dos, Drivers, profile, Tree
  ;

{ ------------------------------- Collections ----------------------------- }

procedure TFileInfo.FreeItem;
  begin
  if Item <> nil then
    begin
    DisposeStr(PArcFile(Item)^.FName);
    Dispose(PArcFile(Item));
    end;
  end;

function TFileInfo.Compare;
  var
    F1: PArcFile absolute P1;
    F2: PArcFile absolute P2;
  begin
  if UpStrg(F1^.FName^) = UpStrg(F2^.FName^) then
    Compare := 0
  else
    Compare := 1-2*Integer(UpStrg(F1^.FName^) > UpStrg(F2^.FName^));
  end;

procedure TFileInfo.PutItem;
  begin
  S.WriteStr(PArcFile(Item)^.FName);
  S.Write(PArcFile(Item)^.Select,
       SizeOf(Boolean)+SizeOf(AWord)+3*SizeOf(LongInt));
  end;

function TFileInfo.GetItem;
  var
    P: PArcFile;
  begin
  New(P);
  P^.FName := S.ReadStr;
  S.Read(P^.Select, SizeOf(Boolean)+SizeOf(AWord)+3*SizeOf(LongInt));
  GetItem := P;
  end;

{ --------------------------- All archives -------------------------------- }

constructor TARJArchive.Load;
  begin
  Packer := S.ReadStr;
  UnPacker := S.ReadStr;
  Extract := S.ReadStr;
  ExtractWP := S.ReadStr;
  Add := S.ReadStr;
  Move := S.ReadStr;
  Delete := S.ReadStr;
  Garble := S.ReadStr;
  Test := S.ReadStr;
  IncludePaths := S.ReadStr;
  ExcludePaths := S.ReadStr;
  ForceMode := S.ReadStr;
  RecoveryRec := S.ReadStr;
  SelfExtract := S.ReadStr;
  Solid := S.ReadStr;
  RecurseSubDirs := S.ReadStr;
  SetPathInside := S.ReadStr;
  StoreCompression := S.ReadStr;
  FastestCompression := S.ReadStr;
  FastCompression := S.ReadStr;
  NormalCompression := S.ReadStr;
  GoodCompression := S.ReadStr;
  UltraCompression := S.ReadStr;
  ComprListChar := S.ReadStr;
  ExtrListChar := S.ReadStr;
  {S.Read(ListChar, SizeOf(ListChar) +
                   SizeOf(Swap) +
                   SizeOf(UseLFN));}

  end { TARJArchive.Load };

procedure TARJArchive.Store;
  begin
  S.WriteStr(Packer);
  S.WriteStr(UnPacker);
  S.WriteStr(Extract);
  S.WriteStr(ExtractWP);
  S.WriteStr(Add);
  S.WriteStr(Move);
  S.WriteStr(Delete);
  S.WriteStr(Garble);
  S.WriteStr(Test);
  S.WriteStr(IncludePaths);
  S.WriteStr(ExcludePaths);
  S.WriteStr(ForceMode);
  S.WriteStr(RecoveryRec);
  S.WriteStr(SelfExtract);
  S.WriteStr(Solid);
  S.WriteStr(RecurseSubDirs);
  S.WriteStr(SetPathInside);
  S.WriteStr(StoreCompression);
  S.WriteStr(FastestCompression);
  S.WriteStr(FastCompression);
  S.WriteStr(NormalCompression);
  S.WriteStr(GoodCompression);
  S.WriteStr(UltraCompression);
  S.WriteStr(ComprListChar);
  S.WriteStr(ExtrListChar);
  { S.Write(ListChar, SizeOf(ListChar) +
                    SizeOf(Swap) +
                    SizeOf(UseLFN));}

  end { TARJArchive.Store };

{const
  BooleanStr: array[boolean] of string[2] = ('0'#0, '1'#0);}

procedure TARJArchive.Save;
  var
    Sign: TStr5;
    q: String;
  function StoS(P: PString): PChar;
    begin
    if P <> nil then
      Q := P^+#0
    else
      Q := #0;
    StoS := @q[1];
    end;
  begin
  FreeStr := SourceDir+DNARC;
  Sign := GetSign;
  SetLength(Sign, Length(Sign)-1);
  Sign := Sign+#0;
  if AllVersion then
    q := '1'#0
  else
    q := '0'#0;
  WritePrivateProfileString(@Sign[1], PAllVersion, @q[1], @FreeStr[1]);

  if PutDirs then
    q := '1'#0
  else
    q := '0'#0;
  WritePrivateProfileString(@Sign[1], PPutDirs, @q[1], @FreeStr[1]);

  {$IFNDEF DPMI32}
  if ShortCmdLine then
    q := '1'#0
  else
    q := '0'#0;
  WritePrivateProfileString(@Sign[1], PShortCmdLine, @q[1], @FreeStr[1]);
  {$ELSE}
  if SwapWhenExec then
    q := '1'#0
  else
    q := '0'#0;
  WritePrivateProfileString(@Sign[1], PSwapWhenExec, @q[1], @FreeStr[1]);
  {$ENDIF}
  {$IFNDEF OS2}
  if UseLFN then
    q := '1'#0
  else
    q := '0'#0;
  WritePrivateProfileString(@Sign[1], PUseLFN, @q[1], @FreeStr[1]);
  {$ENDIF}
  {q:=ListChar+#0;}
  WritePrivateProfileString(@Sign[1], PComprListChar,
     StoS(ComprListChar), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PExtrListChar, StoS(ExtrListChar),
     @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PUltraCompression,
       StoS(UltraCompression), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PGoodCompression,
       StoS(GoodCompression), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PNormalCompression,
       StoS(NormalCompression), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PFastCompression,
       StoS(FastCompression), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PFastestCompression,
       StoS(FastestCompression), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PStoreCompression,
       StoS(StoreCompression), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PSetPathInside,
     StoS(SetPathInside), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PRecurseSubDirs,
       StoS(RecurseSubDirs), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PSolid, StoS(Solid), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PRecoveryRec, StoS(RecoveryRec),
     @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PSelfExtract, StoS(SelfExtract),
     @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PForceMode, StoS(ForceMode),
     @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PExcludePaths, StoS(ExcludePaths),
     @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PIncludePaths, StoS(IncludePaths),
     @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PTest, StoS(Test), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PDelete, StoS(Delete), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PGarble, StoS(Garble), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PMove, StoS(Move), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PAdd, StoS(Add), @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PExtractWP, StoS(ExtractWP),
     @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PExtract, StoS(Extract),
     @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PUnPacker, StoS(UnPacker),
     @FreeStr[1]);
  WritePrivateProfileString(@Sign[1], PPacker, StoS(Packer), @FreeStr[1]);
  end { TARJArchive.Save };

function TARJArchive.GetVal(const Sign, AFile, Name, Default: PChar)
  : String;
  var
    S: String;
  begin
  SetLength(S, GetPrivateProfileString(Sign, Name, Default, @S[1], 255,
       AFile));
  GetVal := S;
  end;

destructor TARJArchive.Done;
  begin
  DisposeStr(Packer);
  DisposeStr(UnPacker);
  DisposeStr(Extract);
  DisposeStr(ExtractWP);
  DisposeStr(Add);
  DisposeStr(Move);
  DisposeStr(Delete);
  DisposeStr(Garble);
  DisposeStr(Test);
  DisposeStr(IncludePaths);
  DisposeStr(ExcludePaths);
  DisposeStr(ForceMode);
  DisposeStr(RecoveryRec);
  DisposeStr(SelfExtract);
  DisposeStr(Solid);
  DisposeStr(RecurseSubDirs);
  DisposeStr(SetPathInside);
  DisposeStr(StoreCompression);
  DisposeStr(FastestCompression);
  DisposeStr(FastCompression);
  DisposeStr(NormalCompression);
  DisposeStr(GoodCompression);
  DisposeStr(UltraCompression);
  { inherited Done;}
  end { TARJArchive.Done };

{ ----------------------------- ARJ ------------------------------------}

constructor TARJArchive.Init;
  var
    Sign: TStr5;
    q: String;
  begin
  Sign := GetSign;
  SetLength(Sign, Length(Sign)-1);
  Sign := Sign+#0;
  FreeStr := SourceDir+DNARC;
  inherited Init;
  {$IFDEF SEVENZIP}
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, 'ARJ'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, '7Z'));
  Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'e'));
  ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP, 'x'));
  Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a -a -a1'));
  Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'm -a -a1 -p1'));
  Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'd -p1'));
  Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 't'));
  Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, '-g'));
  IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,
         '-e'));
  ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, '-y'));
  RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''));
  SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,
         '-je1'));
  Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
  RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,
         ''));
  SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         ''));
  StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, '-m0'));
  FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, '-m4'));
  FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, '-m3'));
  NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, '-m1'));
  GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, '-jm1 -jh65535'));
  UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, '-jm -jh65535'));
  ComprListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListChar,
         '!'));
  ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListChar,
       '@'));
  {$ELSE}
  Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, 'ARJ'));
  UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker, 'ARJ'));
  Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,
         'e -p1 -v -jycv'));
  ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,
         'x -p1 -v -jycv'));
  Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a -a -a1'));
  Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'm -a -a1 -p1'));
  Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'd -p1'));
  Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 't -v -jycv'));
  Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, '-g'));
  IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths, ''));
  ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,
         '-e'));
  ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, '-y'));
  RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''));
  SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,
         '-je1'));
  Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
  RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,
         ''));
  SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,
         ''));
  StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PStoreCompression, '-m0'));
  FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastestCompression, '-m4'));
  FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PFastCompression, '-m3'));
  NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PNormalCompression, '-m1'));
  GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PGoodCompression, '-jm1 -jh65535'));
  UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
         PUltraCompression, '-jm -jh65535'));
  ComprListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListChar,
         '!'));
  ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListChar,
       '!'));
  {$ENDIF}

  q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
  AllVersion := q <> '0';
  q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '0');
  PutDirs := q <> '0';
  {$IFNDEF DPMI32}
  q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '0');
  ShortCmdLine := q <> '0';
  {$ELSE}
  q := GetVal(@Sign[1], @FreeStr[1], PSwapWhenExec, '0');
  SwapWhenExec := q <> '0';
  {$ENDIF}
  {$IFNDEF OS2}
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
  UseLFN := q <> '0';
  {$ENDIF}
  end { TARJArchive.Init };

function TARJArchive.GetID;
  begin
  GetID := arcARJ;
  end;

function TARJArchive.GetSign;
  begin
  GetSign := sigARJ;
  end;

procedure TARJArchive.GetFile;
  const

    {  побитовые флаги для поля ARJ_Flags заголовка }
    GARBLED_FLAG = $01; // indicates passworded file
    OLD_SECURED_FLAG = $02;
    VOLUME_FLAG = $04; // continued file to next volume (file is split)
    EXTFILE_FLAG = $08;
    // indicates file starting position field (for split files)
    PATHSYM_FLAG = $10; // archive name translated ("\" changed to "/")
    BACKUP_FLAG = $20; // obsolete
    SECURED_FLAG = $40;
    // ALTNAME_FLAG      = $80     indicates dual-name archive

  var
    i: AWord;
    FP: TFileSize;
    Extr: LongInt;
    C: Char;
    h: record
      First_Hdr_Size: Byte;
      Version: Byte;
      MinVer: Byte;
      Host_OS: Byte;
      ARJ_Flags: Byte;
      Method: Byte;
      File_Type: Byte;
      Reserved: Byte;
      Date_Time: LongInt;
      Compressed_Size: LongInt;
      Original_Size: LongInt;
      Original_CRC: LongInt;
      Filespec_Pos: AWord;
      File_Acs_Mode: AWord;
      Host_Data: AWord;
      end;
  begin { TARJArchive.GetFile }
  ArcFile^.Read(i, 2);
  if  (i <> 60000) or (ArcFile^.Status <> 0) then
    begin
    FileInfo.Last := 2;
    Exit;
    end;
  ArcFile^.Read(i, 2);
  if  (i = 0) then
    begin
    FileInfo.Last := 1;
    Exit;
    end;
  FP := ArcFile^.GetPos;

  ArcFile^.Read(h, SizeOf(h));

  with FileInfo do
    begin
    Date := h.Date_Time;
    PSize := h.Compressed_Size;
    USize := h.Original_Size;
    if  (h.ARJ_Flags and
          ( (OLD_SECURED_FLAG or GARBLED_FLAG or SECURED_FLAG))) = 0
    then
      Attr := 0
    else
      Attr := Hidden;
    if h.File_Type = 3 then
      Attr := Attr or Directory;
    end;

  if h.ARJ_Flags and EXTFILE_FLAG <> 0 then
    begin
    ArcFile^.Read(Extr, 4);
    if Extr <> 0 then
      with FileInfo do
        Attr := Attr or SysFile;
    end
  else
    Extr := 0;

  FileInfo.FName := '';
  ArcFile^.Seek(FP+h.First_Hdr_Size);
  repeat
    ArcFile^.Read(C, 1);
    if C <> #0 then
      FileInfo.FName := FileInfo.FName+C
    else
      Break;
  until ArcFile^.Status <> stOK;
  if  (ArcFile^.Status <> stOK) then
    begin
    FileInfo.Last := 2;
    Exit;
    end;
  FileInfo.Last := 0;
  ArcFile^.Seek(FP+i+4);
  repeat
    ArcFile^.Read(i, 2);
    if i > 0 then
      ArcFile^.Seek(ArcFile^.GetPos+i+4);
  until (i = 0) or (ArcFile^.Status <> stOK);
  ArcFile^.Seek(CompToFSize(ArcFile^.GetPos+FileInfo.PSize));
  end { TARJArchive.GetFile };

function ArchiveFiles;
  var
    C: TStr4;
    q: Byte;
  begin
  ArchiveFiles := False;
  if PosChar(':', S) < 3 then
    Exit;
  C := UpStrg(Copy(S, 1, PosChar(':', S)));
  q := GetArchiveTagBySign(C);
  if q = arcUNK then
    Exit;
  DefaultAddArchiver := q;
  ArchiveFiles := True;
  MakeArchive(Copy(S, PosChar(':', S)+1, 255), Files, MoveMode, True,
     Owner);
  DefaultAddArchiver := arcUNK;
  end;

{ Flash >>> }
function CheckForSpaces(S: String): Boolean;
  begin
  CheckForSpaces := (Pos(' ', S) = 0);
  end;
{ Flash <<< }

{-DataCompBoy-}
procedure MakeArchive;

  var
    AID: Word;
    C: String[40];
    CurDir: String;
    Arc: PARJArchive;
    f: lFile;
    D: record
      Name: String;
      Password: String[40];
      Add: String[80];
      Options: Word;
      Archiver: Word;
      Mode: Word;
      end;
    ST1: AnsiString;
    B: Boolean;
    SIntern: String;

  function MakeListFile(var B: Boolean): AnsiString;
    var
      F: lText;
      PF: PFileRec;
      I: Integer;
      S: AnsiString;
      S1: String;
      SR: lSearchRec; {JO}

    procedure PutDir(const SS: String);
      var
        I: Integer;
        S1: String;
        SR: lSearchRec;
      begin
      ClrIO;
      {piwamoto.src.begin}
      {JO:  используем символ #$14 для временного разделения имён файлов}
      if not ((PF^.Attr and Directory <> 0) and (D.Options and 1 = 0))
      then
        if B then
          S := S+#$14+SS
        else
          Writeln(F.T, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(SS));
      {piwamoto.src.end}
      ClrIO;
      lFindFirst(MakeNormName(SS, x_x), AnyFileDir, SR); {JO}
      while (DosError = 0) and not Abort {and (Length(S) < 100)} do
        begin
        {$IFNDEF OS2}
        if Arc^.UseLFN
        then
          S1 := GetLongRelPath(MakeNormName(SS, SR.FullName))
        else
          S1 := GetShortRelPath(MakeNormName(SS, SR.SR.Name));
        {$ELSE}
        S1 := GetLongRelPath(MakeNormName(SS, SR.FullName));
        {$ENDIF}
        {JO:  используем символ #$14 для временного разделения имён файлов}
        if  (SR.SR.Attr and (Directory {$IFNDEF OS2}+VolumeID {$ENDIF}) =
             0)
        then
          {JO}
          if B then
            S := S+#$14+SquashesName(S1)
          else
            Writeln(F.T, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(S1));
        {piwamoto.src.begin}
        if  (SR.SR.Attr and Directory <> 0)
          and (not IsDummyDir( {$IFDEF DPMI32}SR.SR.Name
               {$ELSE}SR.FullName {$ENDIF})) and {JO}
            (D.Options and $40 <> 0)
        then
          {piwamoto.src.end}
          {$IFNDEF OS2}
          if Arc^.UseLFN
          then
            {$ENDIF}
            PutDir(MakeNormName(SS, SR.FullName))
            {$IFNDEF OS2}
          else
            PutDir(MakeNormName(SS, SR.SR.Name))
            {$ENDIF}
            ;
        DosError := 0;
        lFindNext(SR);
        end;
      lFindClose(SR);
      end { PutDir };

    begin { MakeListFile }
    B := (CnvString(Arc^.ComprListChar) = ' ')
           or (CnvString(Arc^.ComprListChar) = '');
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
        S := CnvString(Arc^.ComprListChar)+S;
      end;
    for I := 0 to Files^.Count-1 do
      begin
      PF := Files^.At(I);
      if PathFoundInArc(PF^.Owner^) then
        Continue; {JO}
      {$IFNDEF OS2}
      if Arc^.UseLFN
      then
        {$ENDIF}
        S1 := GetLongRelPath(MakeNormName(PF^.Owner^, PF^.FlName[True]))
          {$IFNDEF OS2}
      else
        S1 := GetShortRelPath(MakeNormName(PF^.Owner^, PF^.FlName[True]))
          {$ENDIF}
          ;
      {JO:  используем символ #$14 для временного разделения имён файлов}
      if PF^.Attr and Directory = 0
      then
        if B then
          S := S+#$14+SquashesName(S1)
        else
          Writeln(F.T, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(S1))
          {piwamoto.src.begin}
      else if B then
        S := S+#$14+SquashesName(S1+'\*.*')
      else if Arc^.PutDirs then
        PutDir(S1)
      else {JO}
        {для пустых каталогов надо обязательно подставлять имя без маски, иначе}
        begin
        {архиваторы их игнорируют; для непустых каталогов такая подстановка приводит}
        ClrIO;
        {с некоторыми архиваторами (ZIP) к тому, что файлы попадают в архив дважды}
        lFindFirst(MakeNormName(S1, x_x), AnyFileDir, SR);
        if IsDummyDir( {$IFDEF DPMI32}SR.SR.Name {$ELSE}SR.FullName
            {$ENDIF})
        then
          lFindNext(SR);
        if IsDummyDir( {$IFDEF DPMI32}SR.SR.Name {$ELSE}SR.FullName
            {$ENDIF})
        then
          lFindNext(SR);
        if  (DosError <> 0) then
          Writeln(F.T, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(S1));
        lFindClose(SR);
        ClrIO;
        Writeln(F.T, {$IFDEF RecodeWhenDraw}CharToOemStr
           {$ENDIF}(S1)+'\*.*');
        end; {/JO}
      {piwamoto.src.end}
      MakeListFile := S;
      {if Length(S) > 100 then Exit;}
      end;
    MakeListFile := S;
    if not B then
      Close(F.T);
    end { MakeListFile };

  procedure UnSelect(PF: PFileRec);
    begin
    Message(Owner, evCommand, cmCopyUnselect, PF);
    end;

  function AddString(P: PString): String;
    begin
    if P = nil then
      AddString := ''
    else
      begin
      if Copy(P^, 1, 1) = '+' then
        if Copy(P^, 1, 2) <> '++' then
          AddString := Copy(P^, 2, MaxStringLength)
        else
          AddString := ' '+Copy(P^, 2, MaxStringLength)
      else
        AddString := ' '+P^;
      end;
    end;

  {JO: ввёл функцию ArcExec по аналогии с TArcDrive.Exec в Arcview для разбора}
  {    длинной командной строки                                               }
  function ArcExec(Prg, Cmd: String; Lst: AnsiString; B: Boolean): Boolean;
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

    const
      NotAPath: Char = #22;

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
    begin { ArcExec }
    ArcExec := True;
    S := Prg+' '+Cmd;
    {$IFDEF DPMI32}
    if Arc^.SwapWhenExec then
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
        S := S + ' ' + Lst;
      Message(Application, evCommand, cmExecString, @S);
      end
    else {if Arc^.SwapWhenExec}
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
      if Arc^.ShortCmdLine then
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
        DelDoubles('  ', S);
        AnsiDelDoubles('  ', SS1);
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
      DelDoubles('  ', S);
      AnsiDelDoubles('  ', Lst);
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
//JO: подсовываем в качестве InfoPtr заведомо несуществующий путь
//    из одного символа #22, чтобы не перечитывались панели производные
//    от TFindDrive, т.к. в них после этой процедуры ничего не изменится
    GlobalMessage(evCommand, cmPanelReread, @NotAPath);
    GlobalMessage(evCommand, cmRereadInfo, nil);
    {$IFDEF DPMI32}
    end; {if Arc^.SwapWhenExec}
    {$ENDIF}
    end { ArcExec };

  label Ex {$IFNDEF OS2}, TryAgain {$ENDIF};
  begin { MakeArchive }
  FillChar(D, SizeOf(D), 0);
  D.Name := {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(S);
  if SystemData.ForceDefArch <> '' then
    begin
    DefaultArchiver := GetArchiveTagBySign
        (UpStrg(SystemData.ForceDefArch)+':');
    if DefaultArchiver = arcUNK then
      begin
      SystemData.ForceDefArch := '';
      ForceDefaultArchiver := '';
      DefaultArchiver := arcRAR;
      end;
    end;
  if DefaultAddArchiver = arcUNK then
    D.Archiver := DefaultArchiver
  else
    D.Archiver := DefaultAddArchiver;
  D.Options := (DefaultArcMode shr 8) and not 2 or (Byte(MoveMode)*2);
  D.Mode := DefaultArcMode and 255;
  D.Add := '';
  repeat
    {$IFNDEF OS2}
TryAgain:
    {$ENDIF}
    if ExecResource(dlgArchiveFiles, D) <> cmOK then
      Exit;
    AID := (D.Mode and 255) or (D.Options shl 8);
    if  (D.Archiver <> DefaultArchiver) or
        (AID <> DefaultArcMode)
    then
      begin
      DefaultArchiver := D.Archiver;
      DefaultArcMode := AID;
      Message(Application, evCommand, cmUpdateConfig, nil);
      end;
    S := '';
    AID := D.Archiver;
    Arc := GetArchiveByTag(AID);
    if Arc = nil then
      Exit;
    { Flash >>> }
    {$IFNDEF OS2}
    if D.Password <> '' then
      if not CheckForSpaces(D.Password) then
        if not Arc^.UseLFN then
          begin
          MessageBox(GetString(dlSpacesInPassword), nil,
             mfWarning+mfOKButton);
          goto TryAgain;
          end;
    {$ENDIF}

  until (D.Password = '') or
    ( (ExecResource(dlgReenterPassword, S) = cmOK) and (S = D.Password));
  if {$IFNDEF OS2}(Arc^.UseLFN) and {$ENDIF}
    not CheckForSpaces(D.Password)
  then
    D.Password := '"'+D.Password+'"';
  { Flash <<< }
  lGetDir(0, CurDir);
  if Abort then
    goto Ex;
  MakeSlash(CurDir);
  if D.Options and 2 = 2 then
    C := CnvString(Arc^.Move)
  else
    C := CnvString(Arc^.Add);
  if D.Options and 1 = 0 then
    C := C+AddString(Arc^.ExcludePaths)
  else
    C := C+AddString(Arc^.IncludePaths);
  if D.Options and $40 <> 0 then
    C := C+AddString(Arc^.RecurseSubDirs);

  if D.Password <> '' then
    C := C+' '+CnvString(Arc^.Garble)+D.Password;
  if D.Options and 4 <> 0 then
    C := C+AddString(Arc^.ForceMode);
  if D.Options and 8 <> 0 then
    C := C+AddString(Arc^.Solid);
  if D.Options and $10 <> 0 then
    C := C+AddString(Arc^.RecoveryRec);
  if D.Options and $20 <> 0 then
    C := C+AddString(Arc^.SelfExtract);
  if D.Mode = 0 then
    C := C+AddString(Arc^.StoreCompression)
  else if D.Mode = 1 then
    C := C+AddString(Arc^.FastestCompression)
  else if D.Mode = 2 then
    C := C+AddString(Arc^.FastCompression)
  else if D.Mode = 3 then
    C := C+AddString(Arc^.NormalCompression)
  else if D.Mode = 4 then
    C := C+AddString(Arc^.GoodCompression)
  else if D.Mode = 5 then
    C := C+AddString(Arc^.UltraCompression);

  SIntern := '';
  if AddToExisting and (CnvString(Arc^.SetPathInside) <> '') then
    begin
    if Owner <> nil then
      Message(PView(Owner)^.Owner, evCommand, cmPushInternalName,
         @SIntern);
    if SIntern <> '' then
      begin
      while (SIntern[Length(SIntern)] = '.') do
        SetLength(SIntern, Length(SIntern)-1);
      while (SIntern[1] = '\') do
        Delete(SIntern, 1, 1);
      MakeNoSlash(SIntern);
      SIntern := CnvString(Arc^.SetPathInside)+
        SquashesName(SIntern)+' ';
      end;
    end;
  S := C+' '+D.Add+' '+SIntern+SquashesName(
       {$IFDEF RecodeWhenDraw}OemToCharStr {$ENDIF}(D.Name));
  ST1 := MakeListFile(B);

  { DelDoubles('  ', S);}
  { GlobalMessage(evCommand, cmMakeForced, nil);}
  if Owner <> nil then
    Files^.ForEach(@Unselect);
  { Message(Application, evCommand, cmExecString, @S);}
  if  (ST1 = '') then
    goto Ex;
  ArcExec(CnvString(Arc^.Packer), S, ST1, B);
Ex:
  Dispose(Arc, Done);
  end { MakeArchive };
{-DataCompBoy-}

{-DataCompBoy-}
procedure UnarchiveFiles;
  var
    AType: PARJArchive;
    S: String;
    DT: record
      S: String;
      W: Word;
      Psw: String[30];
      end;
    ExtrDir: String;
    ExtrChar: String[40];
    Dr: String;
    Nm: String;
    Xt: String;
    Unp: String;
    FMod: String;
    DNN: Byte;
    TempExtrDir: String;
    TempDirUsed: Boolean;
    FCT: PFilesCollection;
    FRT: PFileRec;
    OldConfirms: Word;
    PV: PView;
    Inhr: Byte;
    DDr: Char;
    SR: lSearchRec;

  label ex {$IFNDEF OS2}, TryAgain {$ENDIF};
  begin { UnarchiveFiles }
  lFSplit(FName, Dr, Nm, Xt);
  ArcFileName := FName;
  New(ArcFile, Init(FName, stOpenRead, ArcBufSize));
  if  (ArcFile = nil) or (ArcFile^.Status <> stOK) then
    begin
    if TempFile <> '' then
      TempFile := '';
    StdMsg(4);
    FreeObject(ArcFile);
    Abort := True;
    Exit;
    end;
  SkipSFX;
  AType := DetectArchive;
  FreeObject(ArcFile);
  if AType = nil then
    Exit;
  ExtrDir := '';
  DT.S := '';
  DT.Psw := '';
  {  DT.W := UnarchiveOpt and not 2;}
  DT.W := 1 or (UnarchiveOpt and not 2); {JO}
  Message(Application, evCommand, cmPushFullName, @DT.S);
  if CopyDirName <> '' then
    DT.S := CopyDirName;
  if DT.S = cTEMP_ then
    DT.S := '';
  if DT.S = '' then
    GlobalMessageL(evCommand, cmPushName, hsExtract);
  if DT.S = '' then
    DT.S := HistoryStr(hsExtract, 0);
  if DT.S = cTEMP_ then
    DT.S := '';
  if  (Length(DT.S) > 3) and (DT.S[Length(DT.S)] <> '\') then
    DT.S := DT.S+'\';
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
  CopyDirName := '';
  {$IFNDEF OS2}
TryAgain:
  {$ENDIF}
  if ExecResource(dlgExtract, DT) <> cmOK then
    goto ex;
  {
   if (DT.W and 1) <> (UnarchiveOpt and 1) then ConfigModified := True;
}
  {JO}
  if  (DT.W and 4) <> (UnarchiveOpt and 4) then
    ConfigModified := True;
  UnarchiveOpt := (DT.W and not 2) or 8;
  {/JO}
  if  (DT.S = '') or (DT.S = '.') then
    DT.S := GetPath(FName);
  ExtrDir := DT.S;
  if ExtrDir[Length(ExtrDir)] <> '\' then
    ExtrDir := ExtrDir+'\';

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
  if  ( (DT.W and 8) = 0) or ((DT.W and 2) <> 0) or (DosError <> 0) then
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
  Inhr := CreateDirInheritance(ExtrDir, True);
  CreateDirInheritance(TempExtrDir, False);
  //JO: если каталог назначения не создался (напpимеp, если диск доступен
  //    только на чтение), то нет смысла и вызывать аpхиватоp
  if not PathExist(TempExtrDir) then
    Exit;
  {/JO}

  ExtrChar := CnvString(AType^.ExtractWP);
  if DT.W and 1 = 0 then
    ExtrChar := CnvString(AType^.Extract);
  if DT.W and 2 <> 0 then
    ExtrChar := CnvString(AType^.Test);
  S := '';
  if DT.Psw <> '' then
    { Flash >>> }
    if CheckForSpaces(DT.Psw) then
      S := S+' '+CnvString(AType^.Garble)+DT.Psw+' '
    else
      {$IFNDEF OS2}
     if AType^.UseLFN then
      {$ENDIF}
      S := S+' '+CnvString(AType^.Garble)+'"'+DT.Psw+'"'+' '
        {$IFNDEF OS2}
    else
      begin
      MessageBox(GetString(dlSpacesInPassword), nil, mfWarning+mfOKButton);
      goto TryAgain;
      end
      {$ENDIF}
      ;
  { Flash <<< }
  Unp := CnvString(AType^.UnPacker);
  if  (AType^.GetID = arcRAR) and (PosChar(';', Unp) > 0) then
    {begin
       if PRARArchive(AType)^.VersionToExtr > 20 then }
    Unp := Copy(Unp, PosChar(';', Unp)+1, 255)
      {else Unp := Copy(Unp, 1, PosChar(';', Unp)-1);
     end};
  FMod := CnvString(AType^.ForceMode);
  {JO}
  if  ( (DT.W and 4 <> 0) or TempDirUsed) and (FMod <> '') then
    FMod := FMod+' '
  else
    FMod := '';
  {/JO}
  {$IFNDEF OS2}
  if AType^.UseLFN then
    {$ENDIF}
    S := Unp+' '+ExtrChar+' '+FMod+SquashesName(S+FName)
      {$IFNDEF OS2}
  else
    S := Unp+' '+ExtrChar+' '+FMod+SquashesName
          (S+lfGetShortFileName(FName))
      {$ENDIF}
      ;
  if Xt = '' then
    S := S+'.'; {piwamoto: extracting from extensionless archives}
  lGetDir(0, DirToChange);
  LFN.lChDir(TempExtrDir);
  DelDoubles('  ', S);
  {$IFDEF DPMI32}
  if AType^.SwapWhenExec then
    begin
    if TempDirUsed then
      begin
      DirToMoveContent := TempExtrDir + '|';
      if DT.W and 4 <> 0 then
        DirToMoveContent := '<'+ DirToMoveContent;
      end;
    ExecStringRR(S, '', False);
    end
  else
//JO: поскольку в DPMI32 веpсии ExecStringRR выполняется
//    с выгpузкой DN/2, то вместо неё делаем AnsiExec
    begin
    DoneSysError;
    DoneEvents;
    DoneVideo;
    DoneDOSMem;
    DoneMemory;
    SwapVectors;
    AnsiExec(GetEnv('COMSPEC'), '/c '+S);
    ClrIO;
    SwapVectors;
    InitDOSMem;
    InitMemory;
    InitVideo;
    InitEvents;
    InitSysError;
    Application^.Redraw;
    end;
  {$ELSE}
  ExecStringRR(S, '', False);
  {$ENDIF}
  {JO}
  if not TempDirUsed then
    goto ex
  else
    begin
    { перекидываем файлы из временного подкаталога в каталог назначения}
    PV := New(PUserWindow, Init);
    Desktop^.Insert(PV);
    CopyDirContent(TempExtrDir, ExtrDir, True, (DT.W and 4 <> 0));
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
    LFN.lChDir(S);
    {$IFDEF DPMI32} {освобождаем каталог}
    if ActiveDir[2] = ':' then
      ChDir(Copy(ActiveDir, 1, 2) + '\');
    {$ENDIF}
    Eraser.EraseFiles(FCT);
    Confirms := OldConfirms;
    FCT^.DeleteAll;
    Dispose(FCT, Done);
    {$IFDEF DPMI32}
    ChDir(StartDir);
    {$ENDIF}
    end;
  {/JO}
ex:
  LFN.lChDir(DirToChange);
  DirToChange := '';
  FreeObject(AType);
  if  (not TempDirUsed) or (Inhr > 0) then
    begin
    ExtrDir := '>' + ExtrDir; //признак перечитывания подкаталогов в ветви
    GlobalMessage(evCommand, cmPanelReread, @ExtrDir);
    GlobalMessage(evCommand, cmRereadInfo, nil);
    end;
  end { UnarchiveFiles };
{-DataCompBoy-}

procedure SkipSFX; {piwamoto}
  var
    TempWord: AWord;
    {used for MZExeRec.RelOffs, MZExeRec.NewExe.Type, NEhdrRec.SegmentOffset, PEhdrRec.ObjNum}
    MZExeRec: record
      ExeID: AWord;
      LastB: AWord;
      TotalP: AWord;
      NewExe: LongInt;
      end;
    PEhdrRec: record
      ObjNum: AWord;
      MaxObjNum: AWord;
      MaxOffset: LongInt;
      end;
    PEobjRec: record
      Name: array[0..7] of Char;
      VirtualSize: LongInt;
      RVA: LongInt;
      Size: LongInt;
      Offset: LongInt;
      Reserved1: LongInt;
      Reserved2: LongInt;
      Reserved3: LongInt;
      Reserved4: LongInt;
      end;
    LXhdrRec: record
      NumOfPages : Longint;
      ObjectPageTableOffset : Longint;
      DataPagesOffset : Longint;
      NonResTableOffs: LongInt;
      TableLenght: Byte;
      end;
    LXPage: record
      PageDataOffset : LongInt;
      DataSize : AWord;
{     Flags : AWord;}{we don't need 'em}
      end;
    NEhdrRec: record
      NumOfSegments: AWord; {1c}
      SegTableOffset: AWord; {22}
      ResTableOffset: AWord; {24}
      ResNamesOffset: AWord; {26}
      AlignShiftCount: AWord; {32}
      SegmentSize: AWord;

      ResourceAlign: AWord;
      ResourceType: AWord;
      ResourceTypeNum: AWord;
      ResourceOffset: AWord;
      ResourceLenght: AWord;
      end;
    LEhdrRec: record
      PageCount: LongInt; {14}
      PageSize: LongInt; {28}
      LastPageSize: LongInt; {2c}
      DataPagesOffs: LongInt; {80}
      end;

procedure Check4ArcId; {piwamoto}
  const
    BufferSize = $8000;{ should be enougth for everyone ;-))) }
    {works almost fine with $1000}
    {some of MS-CAB-SFX (service pack for office) requires $5800.}
    {tested at 21.09.2005 by piwamoto}
  var
    ReEntrance: Byte;
    ArcId, ArcPosID{!!s}: LongInt;
    ArcIdArr: array[0..3] of Byte absolute ArcId;
  label
    Recurce;
  begin
  ReEntrance := 0;

Recurce:

  Inc(ReEntrance);
  ArcPosID := i32(ArcPos);
  Repeat
   ArcFile^.Seek(ArcPosID);
   ArcFile^.Read(ArcId, SizeOf(ArcId));
   Inc (ArcPosID);
  Until ((ArcPos + BufferSize - ArcPosID) = 3{SizeOf(ArcId)-1}) or
       (ArcFile^.EOF) or
  {ZIP}(ArcId = $04034b50 {'PK'#3#4}) or
  {RAR}(ArcId = $21726152 {'Rar!'}) or
  {ARJ}((ArcIdArr[0] = $60) and
        (ArcIdArr[1] = $ea) and
        (ArcIdArr[3] < $b)
       ) or

  {$IFNDEF MINARCH}
  {IS3}(ArcId = $8c655d13 {#$13#$5D#$65#$8C}) or
  {ZOO}(ArcId = $fdc4a7dc {#$DC#$A7#$C4#$FD}) or
  {CHZ}(ArcId = $46684353 {'SChF'}) or
  {CHZ}(ArcId = $44684353 {'SChD'}) or
  {LHA}((ArcId and $f8ffffff) = $30686C2D{-lh0...-lh7}) or {LHA/LZH}
(* piwamoto: this code useful only with very small BufferSize
  {LHA}(ArcId = $736F5920) or {' Yos' LHA/LZH .COM SFX}
*)
  {7Z }(ArcId = $AFBC7A37) or {7-Zip}
  {GZ }((ArcIdArr[0] = $1f) and
        (ArcIdArr[1] in [$8b, $9d]) and
        (ArcIdArr[2] = $08) and
        (ArcIdArr[3] < $20)
       ) or {GZip}
  {$ENDIF}

  {COFF}(ArcId = $0003014c {#$4c#$01#$03#$00}) or
  {unix-style COFF executable - GNU C / go32stub 2.02 // DJ Delorie}
  {ACE}(ArcId = $4543412a {'*ACE'}) or
  {ACE}(ArcId = $78667321 {'!sfx'}) or {!sfx - ACE-SFX script}
  {ACE}(ArcId = $5846532a {'*SFX'}) or {**SFX** - ACE-SFX script}
  {CAB}(ArcId = $8648862a) or {digital sign for Microsoft's hotfixes}
  {CAB}(ArcId = $4643534d {'MSCF'});

   if ((ArcPos + BufferSize - ArcPosID) > 3) and not ArcFile^.EOF
     then ArcPos := ArcPosID - 1;

  {$IFNDEF MINARCH}
  if (ArcId and $f8ffffff) = $30686C2D {LHA/LZH} then ArcPos := ArcPos - 2;
(* piwamoto: this code useful only with very small BufferSize
  if (ArcId = $736F5920) {LHA .COM SFX} and (ReEntrance < 2) then
     begin {must be after LHA/LZH ArcPos correction}
     ArcPosID := ArcPos - 8;
     ArcFile^.Seek(ArcPosID);
     repeat
       ArcFile^.Read(ArcID, 1);
       Inc (ArcPosID);
     until (Byte(ArcID) = $29{')'}) or (ArcPosID = ArcPos);
     {don't care if ArcID <> $29}
     ArcFile^.Seek(ArcPosID - 5);
     ArcFile^.Read(ArcId, SizeOf(ArcId));{DataSize}
     if (ArcId = $63282053) or (ArcId = $6328204C) then
       begin
       ArcPos := ArcPos + $4c0;{SFX 'S' edition minlen=4ef, max=686}
       if Byte(ArcId) = $4C {'L'}
         then ArcPos := ArcPos + $200;{SFX 'L' edition minlen=72d, max=7ba}
       goto Recurce;
       end;
     end;
*)
  {$ENDIF}
  if ArcId = $0003014c {#$4c#$01#$03#$00} then
    begin
    {unix-style COFF executable - GNU C / go32stub 2.02 // DJ Delorie}
    ArcFile^.Seek(ArcPos+$68);
    ArcFile^.Read(ArcPosID, SizeOf(ArcPosID)); {DataOffset}
    ArcFile^.Read(ArcId, SizeOf(ArcId)); {DataSize}
    ArcPos := ArcPos+ArcPosID+ArcId;
    Exit;
    end;
  if ArcId = $78667321 {!sfx - ACE-SFX script} then
    begin {must be before '*ACE' & '*SFX' ArcPos correction}
    ArcFile^.Read(ArcPosID, 2); {skip 2 bytes}
    ArcFile^.Read(ArcPosID, SizeOf(ArcPosID)); {size of !sfx! script}
    ArcPos := ArcPos+ArcPosID+8;
    ArcFile^.Read(ArcPosID, SizeOf(ArcPosID)); {skip 4 bytes}
    ArcFile^.Read(ArcPosID, SizeOf(ArcPosID)); {script is not empty?}
    ArcFile^.Seek(ArcPos);
    ArcFile^.Read(ArcId, SizeOf(ArcId));
    if  (ArcPosID <> 0) and (ArcId = $4543412a) then
      ArcId := $5846532a;
    end;
  if ArcId = $4543412a {'*ACE'} then
    ArcPos := ArcPos-8;
  if ArcId = $5846532a {**SFX** - ACE-SFX script} then
    begin
    ArcPos := ArcPos-6;
    ArcFile^.Seek(ArcPos);
    ArcPosID := 0;
    ArcFile^.Read(ArcPosID, 2); {only 2 bytes}
    ArcPos := ArcPos+ArcPosID+4;
    ArcFile^.Seek(ArcPos);
    ArcFile^.Read(ArcPosID, 2); {only 2 bytes}
    ArcPos := ArcPos+ArcPosID+2;
    ArcFile^.Read(ArcPosID, 3); {skip 3 bytes}
    ArcFile^.Read(ArcPosID, SizeOf(ArcPosID)); {size of **SFX** script}
    ArcPos := ArcPos+ArcPosID;
    Exit;
    end;
  if (ArcId = $8648862a) and (ReEntrance < 2) then
    begin {digital sign for Microsoft's hotfixes}
    ArcPos := PEobjRec.Offset + $400{known offset.min=$600, max=$5c00};
    goto Recurce;
    end;
   {last check - uses ArcId}
   if (ArcPos + BufferSize - ArcPosID) = 3 {No known ArcId found} then
     begin {check for PackageForTheWeb script}
     ArcFile^.Seek(ArcPos);
     ArcFile^.Read(ArcPosID, SizeOf(ArcPosID));{size of script}
     if ((ArcPosID and $fffc0000) = 0) then
        {<$40000}{feel free to change it}
        begin
        ArcPosID := i32(ArcPosID + ArcPos + 4);
        if ArcPosID < ArcFile^.GetSize then
          begin
          ArcFile^.Seek(ArcPosID);
          ArcFile^.Read(ArcId, SizeOf(ArcId));
          if ArcID = $4643534d {'MSCF'} then ArcPos := ArcPosID;
          end;
        end;
     end;
  end { Check4ArcId };

  begin { SkipSFX }
  ArcPos := 0;
  ArcFile^.Read(MZExeRec.ExeID,
     SizeOf(MZExeRec.ExeID)+SizeOf(MZExeRec.LastB)+SizeOf(MZExeRec.TotalP));
  if  (MZExeRec.ExeID = $5a4d {'MZ'}) or (MZExeRec.ExeID = $4d5a {'ZM'})
  then
    begin {MZ}
    ArcFile^.Seek($18);
    ArcFile^.Read(TempWord, SizeOf(TempWord));
    ArcFile^.Seek($3c);
    ArcFile^.Read(MZExeRec.NewExe, SizeOf(MZExeRec.NewExe));
    if MZExeRec.LastB <> 0 then
      Dec(MZExeRec.TotalP);
    ArcPos := LongInt(MZExeRec.TotalP)*512+MZExeRec.LastB;
    if  (TempWord >= $40)
         and (_Cardinal(MZExeRec.NewExe+$100) < ArcFile^.GetSize)
    then
      begin
      ArcFile^.Seek(MZExeRec.NewExe);
      ArcFile^.Read(TempWord, SizeOf(TempWord));
      case TempWord of
        $4550:
          begin {PE}
          ArcFile^.Seek(MZExeRec.NewExe+6);
          ArcFile^.Read(PEhdrRec.ObjNum, SizeOf(PEhdrRec.ObjNum));
          PEhdrRec.MaxObjNum := 0;
          PEhdrRec.MaxOffset := 0;
          ArcFile^.Seek(MZExeRec.NewExe+$F8); {offset 2 first object}
          for TempWord := 1 to PEhdrRec.ObjNum do
            begin
            ArcFile^.Read(PEobjRec, SizeOf(PEobjRec));
            if PEobjRec.Offset > PEhdrRec.MaxOffset then
              begin
              PEhdrRec.MaxOffset := PEobjRec.Offset;
              PEhdrRec.MaxObjNum := TempWord;
              end;
            end;
          ArcFile^.Seek(MZExeRec.NewExe+$28*PEhdrRec.MaxObjNum+$0D0);
          {offset 2 object with MaxOffset}
          ArcFile^.Read(PEobjRec, SizeOf(PEobjRec));
          if PEobjRec.Name = '_winzip_' then
            begin
            ArcFile^.Seek(ArcFile^.GetPos-2*SizeOf(PEobjRec));
            ArcFile^.Read(PEobjRec, SizeOf(PEobjRec));
            end;
          ArcPos := PEobjRec.Offset+PEobjRec.Size;
          end;
        $454e:
          begin {NE}
          ArcFile^.Seek(MZExeRec.NewExe+$1c);
          ArcFile^.Read(NEhdrRec.NumOfSegments,
               SizeOf(NEhdrRec.NumOfSegments));
          ArcFile^.Seek(MZExeRec.NewExe+$22);
          ArcFile^.Read(NEhdrRec.SegTableOffset,
               SizeOf(NEhdrRec.SegTableOffset)
            +SizeOf(NEhdrRec.ResTableOffset)
            +SizeOf(NEhdrRec.ResNamesOffset));
          ArcFile^.Seek(MZExeRec.NewExe+$32);
          ArcFile^.Read(NEhdrRec.AlignShiftCount,
               SizeOf(NEhdrRec.AlignShiftCount));
          if NEhdrRec.AlignShiftCount = 0 then
            NEhdrRec.AlignShiftCount := 9;
          if NEhdrRec.ResTableOffset = NEhdrRec.ResNamesOffset then
            begin {no resources present: search 4 last segment}
            ArcFile^.Seek
              (LongInt(NEhdrRec.SegTableOffset)+MZExeRec.NewExe+8*NEhdrRec
              .NumOfSegments-8); {last segment}
            ArcFile^.Read(TempWord, SizeOf(TempWord));
            {NEhdrRec.SegmentOffset}
            ArcFile^.Read(NEhdrRec.SegmentSize,
                 SizeOf(NEhdrRec.SegmentSize));
            ArcPos := LongInt(TempWord)
               shl NEhdrRec.AlignShiftCount+NEhdrRec.SegmentSize;
            end
          else
            begin {exe with resources: search 4 last resource}
            ArcFile^.Seek(MZExeRec.NewExe+NEhdrRec.ResTableOffset);
            ArcFile^.Read(NEhdrRec.ResourceAlign,
                 SizeOf(NEhdrRec.ResourceAlign));
            ArcPos := ArcFile^.GetPos-8; {-8=compensation}
            NEhdrRec.ResourceTypeNum := 0;
            repeat
              {search for last resource}
              ArcPos := ArcPos+NEhdrRec.ResourceTypeNum*12+8;
              ArcFile^.Seek(ArcPos);
              ArcFile^.Read(NEhdrRec.ResourceType,
                 SizeOf(NEhdrRec.ResourceType)+
                 SizeOf(NEhdrRec.ResourceTypeNum));
            until (NEhdrRec.ResourceType = 0) or (ArcFile^.Status <> stOK);
            repeat
              {search for non-empty resource}
              ArcPos := ArcPos-12;
              ArcFile^.Seek(ArcPos);
              ArcFile^.Read(NEhdrRec.ResourceOffset,
                 SizeOf(NEhdrRec.ResourceOffset)+
                 SizeOf(NEhdrRec.ResourceLenght));
            until (NEhdrRec.ResourceOffset <> 0)
               or (ArcFile^.Status <> stOK);
            ArcPos := LongInt
                (NEhdrRec.ResourceOffset+NEhdrRec.ResourceLenght)
               shl NEhdrRec.ResourceAlign;
            end;
          end;
        $584c:
          begin {LX}
          ArcFile^.Seek(MZExeRec.NewExe + $14);
          ArcFile^.Read(LXhdrRec.NumOfPages, SizeOf(LXhdrRec.NumOfPages));
          ArcFile^.Seek(MZExeRec.NewExe + $48);
          ArcFile^.Read(LXhdrRec.ObjectPageTableOffset, SizeOf(LXhdrRec.ObjectPageTableOffset));
          ArcFile^.Seek(MZExeRec.NewExe + $80);
          ArcFile^.Read(LXhdrRec.DataPagesOffset, SizeOf(LXhdrRec.DataPagesOffset));
          ArcFile^.Seek(MZExeRec.NewExe+$88);
          ArcFile^.Read(LXhdrRec.NonResTableOffs,
               SizeOf(LXhdrRec.NonResTableOffs));
          if LXhdrRec.NonResTableOffs <> 0 then
            begin
            ArcFile^.Seek(LXhdrRec.NonResTableOffs);
            ArcFile^.Read(LXhdrRec.TableLenght, SizeOf(LXhdrRec.TableLenght));
            ArcPos := LXhdrRec.NonResTableOffs + LXhdrRec.TableLenght + 4; {4=1+3}
            end
          else
            begin
            ArcFile^.Seek(MZExeRec.NewExe
                         + LXhdrRec.ObjectPageTableOffset
                         + 8 * (LXhdrRec.NumOfPages - 1));{last page offset}
            ArcFile^.Read(LXPage, SizeOf(LXPage));
            ArcPos := LXhdrRec.DataPagesOffset
                     + LXPage.PageDataOffset
                     + LXPage.DataSize;
            end;
          end;
        $454c:
          begin {LE}
          ArcFile^.Seek(MZExeRec.NewExe+$14);
          ArcFile^.Read(LEhdrRec.PageCount, SizeOf(LEhdrRec.PageCount));
          ArcFile^.Seek(MZExeRec.NewExe+$28);
          ArcFile^.Read(LEhdrRec.PageSize,
             SizeOf(LEhdrRec.PageSize)+SizeOf(LEhdrRec.LastPageSize));
          ArcFile^.Seek(MZExeRec.NewExe+$80);
          ArcFile^.Read(LEhdrRec.DataPagesOffs,
               SizeOf(LEhdrRec.DataPagesOffs));
          ArcPos := LEhdrRec.DataPagesOffs+(LEhdrRec.PageCount-1)
            *LEhdrRec.PageSize+LEhdrRec.LastPageSize;
          end;
        {other EXE types}
      end {case}; {Cas.e EXEtype}
      end; {??-EXE}
    end; {it isn't an EXE}
  Check4ArcId;
  ArcFile^.Seek(ArcPos);
  end { SkipSFX };

function _Cardinal(L: LongInt): Real; {piwamoto}
  {-Return the unsigned equivalent of L as a real}
  begin
  if L < 0 then
    _Cardinal := 4294967296.0+L
  else
    _Cardinal := L;
  end; {_Cardinal}

function FromOct(S: String): TFileSize; {fixed by piwamoto}
  var
    I: Byte;
  begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] in ['0'..'7'] then
      Result := Result * 8+Byte(S[I])-48;
  end;

end.
