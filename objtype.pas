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
unit ObjType;

interface

const

  { --- Archiver }

  otACEArchiver = 9201;
  otAINArchiver = 9202;
  otARCArchiver = 9203;
  otARJArchiver = 9204;
  otBS2Archiver = 9205;
  otBSAArchiver = 9206;
  otCABArchiver = 9207;
  otCHZArchiver = 9208;
  otHAArchiver = 9209;
  otHAPArchiver = 9210;
  otHPKArchiver = 9211;
  otHYPArchiver = 9212;
  otLHAArchiver = 9213;
  otLIMArchiver = 9214;
  otQUARKArchiver = 9215;
  otRARArchiver = 9216;
  otSQZArchiver = 9217;
  otTARArchiver = 9218;
  otUC2Archiver = 9219;
  otUFAArchiver = 9220;
  otZIPArchiver = 9221;
  otZOOArchiver = 9222;
  otZXZArchiver = 9223;
  otIS3Archiver = 9224;
  otTGZArchiver = 9225;
  otS7ZArchiver = 9226;
  otBZ2Archiver = 9227;

  { --- ArcView }

  otArcDrive = 4356;
  otFileInfo = 7339;
  otUserSaver = 7356;

  { --- Network }

  otNetDrive = 4359;

  { --- Arvid }

  otArvidDrive = 4358;

  { --- ASCIITab }

  otTable = 910;
  otReport = 911;
  otASCIIChart = 912;

  { --- Calc }

  otCalcWindow = 6333;
  otCalcView = 6334;
  otCalcInfo = 6335;
  otInfoView = 16336;

  { --- CCalc }

  otCalcLine = 9301;
  otIndicator = 9302;

  { --- CDPlayer }

  otCdplayer = 15547;
  otCDCounter = 15548;

  { --- CellsCol }

  otCellCollection = 1060;

  { --- ColorSel }

  otColorSelector = 21;
  otMonoSelector = 22;
  otColorDisplay = 23;
  otColorGroupList = 24;
  otColorItemList = 25;
  otColorDialog = 26;
  otR_BWSelector = 28;

  { --- SWE }
  otColorPoint = 9;

  { --- DBView }

  otDBWindow = 15549;
  otDBViewer = 15550;
  otDBIndicator = 15551;
  otFieldListBox = 15553;

  { --- Dialogs }

  otDialog = 10;
  otInputLine = 11;
  otLongInputLine = 33; {JO}
  otButton = 12;
  otCluster = 13;
  otRadioButtons = 14;
  otCheckBoxes = 15;
  otListBox = 16;
  otStaticText = 17;
  otLabel = 18;
  otHistory = 19;
  otParamText = 20;
  otMultiCheckBoxes = 27;
  otHexLine = 29;
  otComboBox = 34;
  otNotepad = 35;
  otPage = 36;
  otBookmark = 37;
  otPageFrame = 38;
  otNotepadFrame = 39;

  { --- DiskInfo }

  otDiskInfo = 3361;
  otDriveView = 3363;

  { --- NetInfo }

  otNetInfo = 3362; {-$VIV}

  { --- DnApp }

  otBackground = 30;
  otDesktop = 31;
  otStringCache = 32;

  { --- DnStdDlg }

  otFileInputLine = 60;
  otFileCollection = 61;
  otFileList = 62;
  otFileInfoPane = 63;
  otFileDialog = 64;
  otSortedListBox = 68;


  { --- DnUtil }

  otDataSaver = 20300;

  { --- Drives }

  otDrive = 4354;

  { --- FileFind }

  otFindDrive = 4355;
  otTempDrive = 4357;

  { --- FilesCol }

  otFilesCollection = 3360;

  { --- FlPanel }

  otFilePanel = 3350;
  otFlPInfoView = 3351;
  otDirView = 3352;
  otSeparator = 3353;
  otSpecScroll = 3354;
  otDriveLine = 14354;
  otSortView = 14355;

  { --- FStorage }

  otDirStorage = 20303;

  { --- FViewer }

  otFileViewer = 3370;
  otFileWindow = 3371;
  otViewScroll = 3372;
  otQFileViewer = 3373;
  otDFileViewer = 3374;
  otViewInfo = 3570;

  { --- Gauges }

  otTrashCan = 4090;
  otKeyMacros = 4091;

  { --- HelpKern }

  otHelpTopic = 10000;
  otHelpIndex = 10001;

  { --- Histries }

  otEditHistoryCol = 17570;
  otViewHistoryCol = 17571;

  { --- Menus }

  otMenuBar = 40;
  otMenuBox = 41;
  otStatusLine = 42;
  otMenuPopup = 43;

  { --- MicroEd }

  otFileEditor = 6340;
  otInfoLine = 6341;
  otEditWindow = 6342;
  otEditScrollBar = 6343;
  otEditFrame = 6344;
  otBookLine = 6345;
  otXFileEditor = 6346;

  otLineCollection = 16344;

  { --- NavyLink }

  otLinker = 20301;
  otLinkDrive = 20302;

  { --- Objects }

  otCollection = 50;
  otStringCollection = 51;
  otStringList = 52;
  otStrListMaker = 52;
  otStrCollection = 69;

  { --- Phones }

  otAutoDialer = 7331;
  otDialBox = 8331;
  otDStringView = 8332;
  otPhone = 7330;
  otPhoneDir = 8330;
  otPhoneCollection = 5331;

  { --- PrintMan }

  otStringCol = 5356;
  otPrintManager = 5357;
  otPrintStatus = 5358;
  otPMWindow = 5359;

  { --- ScrollBk }

  otScrollBackWindow = 6353;
  otScrollBack = 6354;

  { --- Scroller }

  otScroller = 4;
  otListViewer = 5;

  { --- Setups }

  otSysDialog = 15001;
  otCurrDriveInfo = 15002;
  otMouseDialog = 15003;
  otMouseBar = 15004;
  otSaversDialog = 15005;
  otSaversListBox = 15006;

  { --- Startup }

  otTextCollection = 17455;

  { --- Terminal }

  otTerminalWindow = 6351;
  otTerminal = 6352;
  otPortInfo = 6652;

  { --- Tetris }

  otGameWindow = 6371;
  otGameView = 6372;
  otGameInfo = 6373;

  { --- Tree }

  otTreeView = 3330;
  otTreeReader = 3331;
  otTreeWindow = 3332;
  otTreePanel = 3333;
  otTreeDialog = 3334;
  otTreeInfoView = 3335;
  otHTreeView = 3336;
  otDirCollection = 16337;

  { --- UserKey }

  otCover = 23323;
  otDCoder = 21831;
  otGCoder = 21830;
  otXCoder = 21832;
  otInfo = 21961;
  otInfo1 = 22005;
  otInfo2 = 29297;
  otInfo3 = 21922;
  otInfo4 = 21927;
  otInfo5 = 26034;
  otInfo6 = 54017;
  otInfo7 = 47029;
  otInfo8 = 47030;
  otInfo9 = 21769;
  otInfo10 = 21770;

  { --- UserMenu }

  otUserWindow = 6361;
  otUserView = 6362;

  { --- Validate }

  otFilterValidator = 81;
  otRangeValidator = 82;

  { --- Views }

  otView = 1;
  otFrame = 2;
  otScrollBar = 3;
  otMyScrollBar = 8;
  otGroup = 6;
  otWindow = 7;

  { --- XDblWnd }

  otDoubleWindow = 3340;

  { 12335 used internally in MicroEd2 }

  otEditStorer = 12335;

  { --- Calendar } {JO}

  otCalendarView = 27000;
  otCalendarWindow = 27001; {JO}

  { --- Plugins }
  otPlugins = 30000; {Cat}
  {..........занято...........} {Cat}
  otPluginsEnd = 60000; {Cat}

implementation

end.
