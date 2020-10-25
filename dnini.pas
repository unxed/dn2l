{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.6.RC1
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
//////////////////////////////////////////////////////////////////////////
//
//  Ini Configuration by VIV (C) 1999
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
unit DnIni;

interface

uses
  Commands
  ;

type
  TBlockMarker = record
    end;

const

  { DO NOT CHANGE THIS LINE }iniparamblock_START: TBlockMarker = ();
  { ------------------------------ PLACE ALL INI VARIABLES BELOW }

  {Cat: Внимание!!!
      При добавлении новых переменных необходимо соответствующим
      образом изменять структуру TIniVars в модуле Vars
      Добавлять новые переменные обязательно в конец - для
      совместимости со старыми версиями плагинов}

  { NOTE! }
  { To declare a short string that must have fixed length add at least }
  { one extra character to its length for better error checking.       }

  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {; Variable name           ; Type       ; Default value      ;          Comments ;}
  { '._______________________;____________;____________________;___________________'}
  {} {Interface}

  {}MenuOnError: Boolean = True;
  {}CutDriveInfo: Boolean = True;
  {}WinManagerSelectNext: Boolean = True;
  {}DriveSelectVCenter: Boolean = False; {-$X-Man}
  {}SystemMenuChar: Byte = 4;
  {}HorizScrollBarChars: String[6] = #17#16#177#254#178; {DataCompBoy}
  {}VertScrollBarChars: String[6] = #30#31#177#254#178; {DataCompBoy}
  {}ReflectCopyDirection: Boolean = False;
  {}ReuseViewers: Byte = 0; { 0 - always open new}
  {}ReuseEditors: Byte = 0; { 1 - prompt for open}
  {} { 2 - do not open new}
  {}HistoryErrorBeep: Boolean = True; {DataCompBoy}
  {}PreserveMenuPositions: Boolean = False; {DataCompBoy}
  {}PanelDescrArvid: String = ''; {JO}
  {}PanelDescrArc: String = ''; {JO}
  {}PanelDescrTemp: String = ''; {JO}
  {}PanelDescrFind: String = ''; {JO}
  {}PanelDescrDrive: String = ''; {JO}
  {}UseEnterInViewer: Byte = 0; {JO}
  {}SkipXLatMenu: Boolean = False; {JO}
  {}EscForOutputWindow: Boolean = False; {JO}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  { '._______________________;____________;____________________;___________________'}
  {} {Clock}
  {}ShowSeconds: Boolean = True;
  {}BlinkSeparator: Boolean = True; {JO}
  {}ShowCentury: Boolean = True;
  {}ShowDayOfWeek: Boolean = True;
  {}DaysOfWeek: String[23] = '(use language default)';
  {}RightAlignClock: Boolean = False; {FY 13-03-2000}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  { '._______________________;____________;____________________;___________________'}
  {} {SmartPad}
  {}SPInsertDate: Boolean = True;
  {}SPLineChar: Byte = 196; {X-Man}
  {SYR}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  { '._______________________;____________;____________________;___________________'}
  {} {Game}
  {}EnableGame: Boolean = True;
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  { '._______________________;____________;____________________;___________________'}
  {} {Clipboard}
  {}cbSize: LongInt = 4096; {-$VOL}
  {}cbAutoSave: Boolean = False; {-$VOL}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  { '._______________________;____________;____________________;___________________'}
  {} {Kernel}
  //CanUseLFN                : Boolean    = True;
  {}AutoSave: Boolean = True;
  {}ShowKeyCode: Byte = 0;
  {}CopyLimit: LongInt = 8192; {DataCompBoy}
  {piwamoto}
  {}StoreVideoMode: Byte = 0; {PZ}
  {DataCompBoy}
  {}SmartWindowsBoxClose: LongInt = 0; {DataCompBoy, AK155}
  {}CtrlBreakKill: Boolean = False; {AK155}
  //DoVESATest               : Boolean    = False;                      {DataCompBoy}
  {}ForceDefaultArchiver: String[3] = ''; {JO}
  {$IFDEF WIN32}
  {}AltGrAsAlt               : Boolean    = True; {JO} {JO: эта переменная дублируется в VpKbdW32}
  {$ENDIF}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  { '._______________________;____________;____________________;___________________'}
  {} {Editor}
  {}UnlimitUnindent: Boolean = False;
  {}DrawRShift: Boolean = True;
  {}AutoScopeDetect: Boolean = True;
  {}ShowBookmarks: Boolean = True;
  {}FastBookmark: Boolean = True;
  {}DefCodePageView: String[9] = 'Dos';
  {}DefCodePage: String[9] = 'Dos';
  {}FastSearchDeep: LongInt = 0;
  {}WinManagerPosToEdit: Boolean = True;
  {}AutoBracketPairs: String = '()[]{}<>';
  {}F6_DuplicatesLine: Boolean = False; {JO}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  { '._______________________;____________;____________________;___________________'}
  {} {FilePanels}
  {}QuickRenameInDialog: Boolean = False;
  {}UpperCaseSorting: Boolean = True; {JO}
  {$IFDEF OS2}
  {}ExecWin32GUI: String[30] = 'start /n /f pe';
  {}ExecWin32CUI: String[30] = 'pec';
  {$ENDIF}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  { '._______________________;____________;____________________;___________________'}
  {} {NetInfo}
  {}NoLevelsInfo: Boolean = False;
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  { '._______________________;____________;____________________;___________________'}
  {} {TaskList}
  {}KillAllowed: Boolean = False; {JO}
  {}ShowExePaths: Boolean = False; {JO}
  {}FilteredList: Boolean = True; {JO}
  {$IFDEF OS2}
  {}UserTaskFilter: String = 'pmshell.exe#filebar.exe#clipvdm.exe';
  {JO}
  {$ENDIF}
  {$IFDEF Win32}
  {}UserTaskFilter: String =
  'msgsrv32.exe#mmtask.tsk#mstask.exe#systray.exe#internat.exe#winoa386.mod'
  ; {JO}
  {$ENDIF}
  {$IFDEF DPMI32}
  {}UserTaskFilter: String = ''; {JO}
  {$ENDIF}
  {$IFDEF LINUX}
  UserTaskFilter: String = ''; // by unxed
  {$ENDIF}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  { '._______________________;____________;____________________;___________________'}
  {} {Language}
  {}ActiveLanguage: String[9] = '';
  {}HelpLanguageOverride: String[9] = '';
  {}ShowLanguageMenu: Boolean = False;
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  { '._______________________;____________;____________________;___________________'}
  {}{Calendar}
  {}CalSundayFirst : Byte = 2; {piwamoto}{PZ}
  {}CalOptionFlags : LongInt = 8; {PZ}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  { `._______________________;____________;____________________;___________________'}
  {} {RegExp}
  {}RegExpStr0: String = ''; {PZ}
  {}RegExpStr1: String = ''; {PZ}
  {}RegExpStr2: String = ''; {PZ}
  {}RegExpStr3: String = ''; {PZ}
  {}RegExpStr4: String = ''; {PZ}
  {}RegExpStr5: String = ''; {PZ}
  {}RegExpStr6: String = ''; {PZ}
  {}RegExpStr7: String = ''; {PZ}
  {}RegExpStr8: String = ''; {PZ}
  {}RegExpStr9: String = ''; {PZ}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type  ; Default value  ;          Comments ;}
  { '._______________________;_________;________________;___________________'}
  {} {QuickDirs}
  {}QDirs1: String = ''; {Quick Dirs - 1}
  {}QDirs2: String = ''; {Quick Dirs - 2}
  {}QDirs3: String = ''; {Quick Dirs - 3}
  {}QDirs4: String = ''; {Quick Dirs - 4}
  {}QDirs5: String = ''; {Quick Dirs - 5}
  {}QDirs6: String = ''; {Quick Dirs - 6}
  {}QDirs7: String = ''; {Quick Dirs - 7}
  {}QDirs8: String = ''; {Quick Dirs - 8}
  {}QDirs9: String = ''; {Quick Dirs - 9}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type  ; Default value  ;          Comments ;}
  { '._______________________;_________;________________;___________________'}

  {} {SetupStorage}
  {}SystemDataOpt: LongInt = 65535; {SystemData.Options}
  {}InterfaceDataOpt: LongInt = 65535; {InterfaceData.Options}
  {}FMSetupOpt: LongInt = 65535; {FMSetup.Options}
  {}EditorDefaultsOpt: LongInt = 65535; {EditorDefaults.EdOpt}
  {}EditorDefaultsOpt2: LongInt = 65535; {EditorDefaults.EdOpt2}
  {}ViewerOpt: LongInt = 65535; {EditorDefaults.ViOpt}
  {}StartupDataLoad: LongInt = 65535; {StartupData.Load}
  {}StartupDataUnload: LongInt = 65535; {StartupData.Unload}
  {}ConfirmsOpt: LongInt = 65535; {Confirms}
  {}NonVIOScreenMode: LongInt = 65535; {JO}
  {последний видеорежим в полноэкранной сессии}
  {}VIOScreenMode: LongInt = 65535; {JO}
  {последний видеорежим в оконной сессии}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`~~~~~~~~~~~~`,}
  {;                   This is end of parameters definitions                       ;}
  { '._____________________________________________________________________________'}

  { ----------------------------- NO INI VARIABLES BEYOND HERE }
  { DO NOT CHANGE THIS LINE }iniparamblock_END: TBlockMarker = ();

implementation

end.
