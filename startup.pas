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

unit Startup;

interface

uses
  Defines, Dos, Commands,
  Drivers, Objects
  ;

procedure FatalError(const S: String);

const
  MaxSignLen = 30;
type
  TSignRec = record
    Sign: String[MaxSignLen];
    SignLen: Byte;
    SignVer: AWord;
    HavVer: Boolean; {Is DN version word written after sign}
    end;

const
  FBRPBSignature: array[1..5] of Char = 'FBRPB';
  NumSupportedConfigs = 9;
  ConfigSigns: array[1..NumSupportedConfigs] of TSignRec =
    ( (Sign: 'DN2 Configuration'#26#001#36; SignLen: 20; SignVer: 00136),
    {Base configuration}
      (Sign: 'DN2 Configuration'#26#151#05; SignLen: 20; SignVer: 15104),
    {Configuration for 1.51.04-0}
      (Sign: 'DN2 Configuration'#26#151#42; SignLen: 20; SignVer: 15104),
    {Configuration for 1.51.04-1}
      (Sign: 'DN2 Configuration'#26#151#42; SignLen: 20; SignVer: 15104),
    {Configuration for 1.51.04-2}
      (Sign: 'DN OSP Configuration'#26#151#42; SignLen: 23;
       SignVer: 15104),
      (Sign: 'DN OSP Configuration'#26#151#43; SignLen: 23;
       SignVer: 15104),
      (Sign: 'DN OSP Configuration'#26#151#50; SignLen: 23;
       SignVer: 15105),
    {Configuration for 1.51.05}
      (Sign: 'DN OSP Configuration'#26#151#51; SignLen: 23;
       SignVer: 15105),
    {Configuration for 1.51.05-2}
      (Sign: 'DN OSP Configuration'#26; SignLen: 21; SignVer: 15106;
       HavVer: True));
  {Configuration for 1.51.06+}
  DskSign: TSignRec =
  {$IFDEF LITE}(Sign: 'Lite DN OSP Desktop'#26#151#80; SignLen: 22;
     SignVer: 15180);
  {$ELSE}(Sign: 'DN OSP Desktop'#26#151#80; SignLen: 17; SignVer: 15180)
  ; {$ENDIF}
  {Desktop for 1.51.08}
  Security: Boolean = True;
  ConfigModified: Boolean = False;
  SkyDelay: Byte = 1;
  CmdExt: String[4] = '.CMD'; {расширение командных файлов}

type


  TSystemData = record
    Options: Word;
    Mode1: String[5];
    Mode2: String[5];
    Temp: String;
    CopyLimitBuf: Word;
    ForceDefArch: String[3];
    Drives: array['A'..'Z'] of Byte;
    end;

  TOld2SystemData = record
    Options: AWord;
    LFNContainer: AWord;
    Mode1: String[5];
    Mode2: String[5];
    Temp: String; {DataCompBoy}
    Drives: array['A'..'Z'] of Byte;
    end;

  TOldSystemData = record
    Options: AWord;
    Mode1: String[5];
    Mode2: String[5];
    Temp: String; {DataCompBoy}
    Drives: array['A'..'Z'] of Byte;
    end;

  TOld1SystemData = record
    Options: AWord;
    Mode1: String[5];
    Mode2: String[5];
    Temp: PathStr;
    Drives: array['A'..'Z'] of Byte;
    end;

  TMouseData = record
    HSense: Word;
    VSense: Word;
    Options: Word;
    end;

  TDriveInfoType = record
    ForDrives: Word;
    ExceptDrv: String[30];
    TypeShowFor: Word;
      FSShowFor: Word;
    VLabShowFor: Word;
    FreeSpShowFor: Word;
    AddInfo: Word;
    Tabulated: Word;
    end;

  TInterfaceData = record
    Options: Word;
    DrvInfType: TDriveInfoType;
    end;

  TFMSetup = record
    Options: Word; // Поведение
    Quick: Word;
    LRCtrlInDriveLine: Word;
    Show: Word;    // Внешний вид
    LFN_Wrap: Word; {Combo}
    LFN_Cut: Word; {Combo}
    LFN_Autohide: Word; {Checkbox[1]}
    LFN_Difference: Word; {Combo}
    TagChar: String[1]; // Если не нужен, то TagChar[1]= ' ';
    RestChar: String[1]; // RestChar[1] всегда корректен
    DIZ: String[250];
    NewPanelPreset: Word;
    LeftPanelType: Word;
    end;

  TEditorDefaultsData = record
    EdOpt: Word;
    EdOpt2: Word;
    ViOpt: Word;
    LM,
    RM,
    PM: String[3];
    NewLine: Word;
    TabSize: String[3];
    end;

  TOldEditorDefaultsData = record
    EdOpt: AWord;
    ViOpt: AWord;
    LM,
    RM,
    PM: String[3];
    NewLine: AWord;
    TabSize: String[3];
    end;

  TTerminalDefaults = record
    Emulation: Word;
    Options: Word;
    end;

  TOldStartupData = record
    Load, Unload, Slice, OvrSize: AWord;
    end;

  TStartupData = record
    Load, Unload: Word;
    end;

  TUUEncodeData = record
    Name: String;
    Prefix, CheckSum: Word;
    NLines: String[5];
    Format: Word;
    end;

  TNamesCaseOptions = record
    Name: Word;
    ext: Word;
    end;

  TTetrisRec = record
    l: Word;
    S: Word;
    P: Word;
    end;

  TComareDirsOptions = record
    o: Word;
    FMask: String;
    S: Word;
    end;

var
  TempBounds: TRect;

const
  ChangeNamesCaseOptions: TNamesCaseOptions = (Name: 0; ext: 0); {JO}
  {было Longint и делилось на два Word,}
  {но это не годится для 32-битной платформы}
  ComareDirsOptions: TComareDirsOptions = (o: 3; FMask: '*.*'; S: 0);
  UUDecodeOptions: AWord = 3;

  UUEncodeData: TUUEncodeData =
    (Name: '';
    Prefix: ckFileTime+ckStatistic;
    CheckSum: ckStd;
    NLines: '100';
    Format: 0);

  DriveInfoData: AWord = $FFFF;

  StartupData: TStartupData =
    (Load: osuRestoreScrMode;
    Unload: 0
    );

  Confirms: Word = cfSingleErase+cfMultiErase+cfEraseReadonly+
  cfEraseSubDir+cfMouseConfirm+cfExitConfirm;

  TerminalDefaults: TTerminalDefaults = (Emulation: emANSIBBS;
     Options: toCTRLCLS+toAutoZModem);

const

  InterfaceData: TInterfaceData = (
    Options: ouiClock+
    ouiEsc+
    ouiStoreEditorPosition+
    ouiStoreViewerPosition+
    ouiTrackEditors+
    ouiTrackViewers+
    ouiTrackDirs;
    DrvInfType: (
      ForDrives: 0;
      ExceptDrv: 'AB';
      TypeShowFor: ditHDD+ditFloppy+ditCDMO+ditProgr+ditNet;
      FSShowFor: ditHDD;
      VLabShowFor: ditHDD+ditProgr;
      FreeSpShowFor: ditHDD+ditProgr;
      AddInfo: 0;
      Tabulated: 1)
    );

  FMSetup: TFMSetup =
    (Options: fmoDragAndDrop+
      fmoBeep+
      fmoEnterArchives+
      fmoSpaceToggle+
      fmoUseArrows+
      fmoBackGoesBack+
      fmoKillContainer+
      fmoAlwaysCopyDesc;
    Quick: pqsAlt;
    LRCtrlInDriveLine: fdlPassive;
    Show: fmsSortIndicator+
      fmsDriveLine+
      fmsShowScrollBar+
      fmsHiliteFiles;
    LFN_Wrap: 0; {Combo}
    LFN_Cut: 0; {Combo}
    LFN_Autohide: 0; {Checkbox[1]}
    LFN_Difference: 0; {Combo}
    TagChar: #251;
    RestChar: #16;
    DIZ: 'descript.ion;files.bbs';
    NewPanelPreset: 2; {нумерация от нуля, то есть 2 - это как Ctrl-3}
    LeftPanelType: fdoDriveDrive
    );

  AutoRefreshPanels: Boolean = False;

  EditorDefaults: TEditorDefaultsData = (
    EdOpt: ebfBSU+ebfTRp;
    EdOpt2: ebfHlt+ebfSmt;
    ViOpt: vbfScrollAfterEOF;
    LM: '0';
    RM: '78';
    PM: '5';
    NewLine: 0;
    TabSize: '8'
    );

  MouseData: TMouseData = (
    HSense: 22;
    VSense: 22;
    Options: omsCursor
    );

  SystemData: TSystemData = (
    Options: ossEditor+
    ossUseSysClip+
    ossRemoveCD_RO shr 3;
    Mode1: '147';
    Mode2: '148';
    Temp: '';
    CopyLimitBuf: 8192;
    ForceDefArch: '';
    Drives: (
      {A}ossCheckFreeSpace + ossVerify + ossUnarcToDirectly,
      {B}ossCheckFreeSpace + ossVerify + ossUnarcToDirectly,
      {C}0, {D}0, {E}0, {F}0, {G}0, {H}0, {I}0, {J}0, {K}0, {L}0, {M}0,
      {N}0,
      {O}0, {P}0, {Q}0, {R}0, {S}0, {T}0, {U}0, {V}0, {W}0, {X}0, {Y}0,
      {Z}0
      )
    );

  {$IFDEF PRINTMANAGER}
  RPrinterSetup: record
    Device: Word;
    CustomDev1, CustomDev2, InitPrinter, AfterFile: String;
    end =
    (Device: 0; CustomDev1: 'PRN'; CustomDev2: '';
     InitPrinter: ''; AfterFile: #12);
  {$ENDIF}

  TetrisRec: TTetrisRec =
    (l: 4; S: 0; P: 0);

  rfDescriptions = $0002;
  rfPhoneBook = $0004;
  rfDiskCopy = $0008;
  rfFormat = $0010;
  rfArchives = $0020;
  rfAdvFind = $0040;

  SortCurPanTypeOnly: Boolean = False;
  FullMenuPanelSetup: Boolean = False;

implementation
uses
  VpSysLow, Advance1, Advance2
  ;

procedure FatalError;
  begin
  InOutRes := 0;
  Writeln(S);
  SysTVInitCursor;
  Halt(1);
  end;

{-DataCompBoy-} {piwamoto} {JO}
{ note: BIN, DAT, RAW are NOT included in any group, }
{ becouse there is TOO MUCH filetypes with these extensions }

begin
if  (SysPlatformId <> -1) and (SysPlatformId <> 2) then
  CmdExt := '.BAT'
end.

