{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08-JO/DPMI
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
//     "Based on Dos Navigator by RIT Research Labs"
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
unit DNHelp;

interface

const

  hcNoContext            = 00000;
  hcIndex                = 00002;
  hcHelpOnHelp           = 00003;
  hcCommonInfo           = 00004;
  hcFeaturesOverview     = 00005;
  hcWindowsFeature       = 00006;
  hcHotOverview          = 00007;
  hcEditorFeature        = 00008;
  hcMainMenu             = 00009;
  hcMenuBox              = 00010;
  hcExtendedVideo        = 00011;
  hcFileMenu             = 00012;
  hcRenameMove           = 00013;
  hcDiskMenu             = 00014;
  hcUtilMenu             = 00015;
  hcPanelMenu            = 00016;
  hcQuickRun             = 00017;
  hcOptionsMenu          = 00018;
  hcNavigatorsWindows    = 00019;
  hcFileManager          = 00020;
  hcManagerCommands      = 00021;
  hcWindows              = 00022;
  hcWindowsType          = 00023;
  hcFiles                = 00024;
  hcDeleteFiles          = 00025;
  hcSplitFiles           = 00026;
  hcCombineDialog        = 00027;
  hcSplitDialog          = 00028;
  hcCopyFiles            = 00029;
  hcCopyDialog           = 00030;
  hcAppendFiles          = 00031;
  hcFilePanel            = 01100;
  hcLinkPanel            = 01101;
  hcEditor               = 01102;
  hcQuickSearch          = 01210;
  hcEditorWindow         = 10000;
  hcEditorCommands       = 10001;
  hcBlockCommands        = 10002;
  hcMovementCommands     = 10003;
  hcInsDelCommands       = 10004;
  hcMiscCommands         = 10005;
  hcDiskOperations       = 10006;
  hcPanelView            = 11000;
  hcPanelSortSetup       = 11001;
  hcPanelShowSetup       = 11002;
  hcPanelHotKeys         = 11003;
  hcDriveInfoSetup       = 11004;
  hcExtFile              = 11005;
  hcInterface            = 11006;
  hcDirTree              = 11007;
  hcReanimator           = 11008;
  hcFormat               = 11009;
  hcVolLabel             = 11010;
  hcDiskEditor           = 11011;
  hcCommunication        = 11012;
  hcAdvPortSetup         = 11013;
  hcArchives             = 11014;
  hcSpreadSheet          = 11015;
  hcCalculator           = 11016;
  hcTempList             = 11017;
  hcTerminal             = 11018;
  hcArcSetup             = 11019;
  hcPhone                = 11020;
  hcView                 = 11021;
  hcHighlight            = 11022;
  hcClipboard            = 11023;
  hcScreenGrabber        = 11024;
  hcTetris               = 16543;
  hcPentix               = 16544;
  hcUserMenu             = 16545;
  hcLUserMenu            = 16546;
  hcGUserMenu            = 16547;
  hcMenuTips             = 16548;
  hcMenuExample          = 16549;
  hcMenuEdit             = 16550;
  hcMenuFormat           = 16551;
  hcExtMacros            = 16552;
  hcAboutDialog          = 16553;
  hcTeam                 = 16554;
  hcCDplayer             = 16555;
  hcCDplayerOptions      = 16556;
  hcCDplayerTitle        = 16557;
  hcTreeDialog           = 16558;
  hcEnvEditor            = 16559;
  hcNetInfo              = 16560;
  hcDiskInfo             = 16561;
  hcPrintManager         = 16562;
  hcConfirmations        = 16563;
  hcCountrySetup         = 16564;
  hcReenterPassword      = 16565;
  hcMouseSetup           = 16567;
  hcInterfaceSetup       = 16568;
  hcSetupTerminal        = 16569;
  hcAppendPhoneNumber    = 16570;
  hcEditPhoneNumber      = 16571;
  hcAppendPhoneDirectory = 16572;
  hcEditPhoneDirectory   = 16573;
  hcPrinterSetup         = 16574;
  hcGameSetup            = 16575;
  hcDBSearch             = 16576;
  hcGotoLine             = 16577;
  hcGotoAddress          = 16578;
  hcDiskError            = 16579;
  hcSavePanelSetup       = 16581;
  hcPanelSetup           = 16582;
  hcFileAttr             = 16583;
  hcFilesAttr            = 16584;
  hcSetPassword          = 16585;
  hcCmdHistory           = 16586;
  hcFileMask             = 16587;
  hcExtendedFileMask     = 16588;
  hcSelect               = 16589;
  hcUnselect             = 16590;
  hcMemoryInfo           = 16591;
  hcSystemInfo           = 16592;
  hcAsciiChart           = 16595;
  hcPanelShowSetup1      = 16601;
  hcPanelShowSetup2      = 16602;
  hcSelectDrive          = 21000;
  hcSelectDriveExtended  = 21001;
  hcAdvanceSearch        = 21002;
  hcFileFind             = 21003;
  hcWinMan               = 21004;
  hcGame                 = 21005;
  hcArcFileFind          = 21006;
  hcFoundFileFind        = 21007;
  hcFoundArcFileFind     = 21008;
  hcAllContainingDialog  = 48000;
  hcYesNoDialog          = 48001;
  hcYesNoCancelDialog    = 48002;
  hcWarningDialog        = 48003;
  hcErrorDialog          = 48004;
  hcInformationDialog    = 48005;
  hcConfirmationDialog   = 48006;
  hcQueryDialog          = 48007;
  hcInputBox             = 48008;
  hcConfiguration        = 48009;
  hcSubMenuConfig        = 48010;
  hcSubMenuFileMgr       = 48011;
  hcOpenFileDialog       = 48012;
  hcSelectFileDialog     = 48013;
  hcManualDial           = 48014;
  hcTetrisScore          = 48015;
  hcTetrisWinner         = 48016;
  hcMkDir                = 48017;
  hcSortBy               = 48018;
  hcQuickDirs            = 48019;
  hcedFile               = 48020;
  hcedEdit               = 48021;
  hcedSearch             = 48022;
  hcedParagraph          = 48023;
  hcedOptions            = 48024;
  hcUUEncode             = 48025;
  hcUUDecode             = 48026;
  hcCompareDirs          = 48027;
  hcMakeListFile         = 48028;
  hcAdvancedFilter       = 48029;
  hcExtract              = 48030;
  hcArchiveFiles         = 48031;
  hcExtViewers           = 48032;
  hcExtEditors           = 48033;
  hcPanelPresets         = 48034;
  hcDrivesSetup          = 48035;
  hcFileMgrSetup         = 48036;
  hcSystemSetup          = 48037;
  hcOptionsStartup       = 48038;
  hcSavers               = 48039;
  hcEditorDefaults       = 48040;
  hcFloppyFormat         = 48041;
  hcAdvancedFloppyFormat = 48042;
  hcScrollBack           = 48043;
  hcEditorHilite         = 48044;
  hcColorDialog          = 48045;
  hcOverwriteQuery       = 48046;
  hcViewerFind           = 48047;
  hcDBView               = 48048;
  hcHelp                 = 48049;
  hcDesktop              = 48050;
  hcDialogWindow         = 48051;
  hcDialogInput          = 48052;
  hcDialogHistory        = 48053;
  hcDialogButton         = 48054;
  hcDialogCase           = 48055;
  hcDialogMessage        = 48056;
  hcViewFile             = 48057;
  hcWildcards            = 48058;
  hcCustomVideo          = 48059;
  hcOS2Support           = 48060;
  hcFilesBBS             = 48061;
  hcLinkStatusWnd        = 48062;
  hcNavyLink             = 48063;
  hcSerialInterface      = 48064;
  hcUnpackDiskImg        = 48065;
  hcReplaceCell          = 48066;
  hcFindCell             = 48067;
  hcGotoCellNumber       = 48068;
  hcSetCellFormat        = 48069;
  hcFormatMargins        = 48070;
  hcEditorFind           = 48071;
  hcEditorReplace        = 48072;
  hcDirHistory           = 48073;
  hcEditHistory          = 48074;
  hcViewHistory          = 48075;
  hcArvidFindResults     = 48076;
  hcChLngId              = 48077;
  hcCPUFlagInfo          = 48078;
  hcEditMiscMenu         = 48079;
  hcMiscUpper            = 48080;
  hcMiscLower            = 48081;
  hcMiscCapitalize       = 48082;
  hcRenameFile           = 48083;
  hcCalendar             = 48084;
  hcEditBlockMenu        = 48085;
  hcSpreadFileMenu       = 48086;
  hcSpreadTableMenu      = 48087;
  hcSpreadEditMenu       = 48088;
  hcSpreadSearchMenu     = 48089;
  hcFTNInfo              = 48090;
  hcSrchFailed           = 48091;
  hcIniFile              = 48092;
  hcChangeNameCase       = 48093;
  hcPath                 = 48094;
  hcDialer               = 48095;
  hcPktListDialog        = 48096;
  hcLineViewer           = 48097;
  hcMsgViewer            = 48098;
  hcPktMsgViewer         = 48099;
  hcTaskList             = 48100;
  hcDBFGoto              = 48101;
  hcMiscToggleCase       = 48102;
  hcMiscKbdLayout        = 48103;
  hcSelectPreset         = 48104;
  hcDialogItems          = 48105;
  hcSkipBadFile          = 49009;
  hcQueryAbort           = 49010;
  hc_                    = 65535;

implementation

end.
