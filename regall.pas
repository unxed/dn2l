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
unit RegAll;

interface

procedure RegisterAll;

implementation

uses
  {$IFNDEF RCP}
  archZip, archLHA, archRAR, archACE, archHA, archCAB,
  {$IFNDEF MINARCH}
  archARC, archBSA, archBS2, archHYP, archLIM, archHPK, archTAR,
  archZXZ, archQRK, archAIN, archCHZ, archHAP, archIS3, archSQZ,
  archUC2, archUFA, archZOO, archTGZ, arch7Z,  archBZ2,
  {$ENDIF}
  {$IFDEF ARVID}
  Arvid,
  {$ENDIF}
  Archiver, ArcView, ASCIITab, CCalc, Collect, DiskInfo, DNApp,
  DNStdDlg, DNUtil, Drives, ed2, Editor, FileFind, FilesCol,
  FlPanel, FStorage, FViewer, Gauges, Histries, Microed, Startup,
  Tree, UniWin, UserMenu, XDblWnd, HelpKern,
  {$IFDEF SpreadSheet}Calc, CellsCol, {$ENDIF}
  {$IFDEF Calendar}Calendar, {$ENDIF}
  {$IFDEF DBView}DBView, {$ENDIF}
  {$IFDEF MODEM}ScrollBk, Terminal, uDialer,
  {$IFDEF LINK}NavyLink, {$ENDIF} {$ENDIF}
  {$IFDEF PrintManager}PrintMan, {$ENDIF}
  {$IFDEF Game}Tetris, {$ENDIF}
  {$IFDEF NetInfo}NetInfo, {$ENDIF}
  {$IFDEF PHONES}Phones, {$ENDIF}
  {$IFDEF NetBrowser}NetBrwsr, {$ENDIF}
  {$ENDIF !RCP}
  ColorSel,
  Dialogs, Menus, Streams, ObjType, Scroller, Setups,
  Validate, Views, SWE {$IFDEF UserSaver}, UserSavr {$ENDIF}
  , EdWin
  ;

const
    { Validate }
{first TStreamRec used in RegisterAll}
RFilterValidator: TStreamRec = (
      ObjType: otFilterValidator;
      VmtLink: (TypeOf(Validate.TFilterValidator));
      Load: @Validate.TFilterValidator.Load;
      Store: @Validate.TFilterValidator.Store);
{second TStreamRec used in RegisterAll}
RRangeValidator: TStreamRec = (
      ObjType: otRangeValidator;
      VmtLink: (TypeOf(Validate.TRangeValidator));
      Load: @Validate.TRangeValidator.Load;
      Store: @Validate.TRangeValidator.Store);
    { Views }
RView: TStreamRec = (
      ObjType: otView;
      VmtLink: (TypeOf(Views.TView));
      Load: @Views.TView.Load;
      Store: @Views.TView.Store);
RFrame : TStreamRec = (
      ObjType: otFrame;
      VmtLink: (TypeOf(Views.TFrame));
      Load: @Views.TFrame.Load;
      Store: @Views.TFrame.Store);
RScrollBar : TStreamRec = (
      ObjType: otScrollBar;
      VmtLink: (TypeOf(Views.TScrollBar));
      Load: @Views.TScrollBar.Load;
      Store: @Views.TScrollBar.Store);
RGroup : TStreamRec = (
      ObjType: otGroup;
      VmtLink: (TypeOf(Views.TGroup));
      Load: @Views.TGroup.Load;
      Store: @Views.TGroup.Store);
RWindow : TStreamRec = (
      ObjType: otWindow;
      VmtLink: (TypeOf(Views.TWindow));
      Load: @Views.TWindow.Load;
      Store: @Views.TWindow.Store);
    {$IFNDEF RCP}
    { archZIP }
RZIPArchiver : TStreamRec = (
      ObjType: otZIPArchiver;
      VmtLink: (TypeOf(archZip.TZIPArchive));
      Load: @archZIP.TZIPArchive.Load;
      Store: @archZIP.TZIPArchive.Store);
    { archLHA }
RLHAArchiver : TStreamRec = (
      ObjType: otLHAArchiver;
      VmtLink: (TypeOf(archLHA.TLHAArchive));
      Load: @archLHA.TLHAArchive.Load;
      Store: @archLHA.TLHAArchive.Store);
    { archRAR }
RRARArchiver : TStreamRec = (
      ObjType: otRARArchiver;
      VmtLink: (TypeOf(archRAR.TRARArchive));
      Load: @archRAR.TRARArchive.Load;
      Store: @archRAR.TRARArchive.Store);
    { archCAB }
RCABArchiver : TStreamRec = (
      ObjType: otCABArchiver;
      VmtLink: (TypeOf(archCAB.TCABArchive));
      Load: @archCAB.TCABArchive.Load;
      Store: @archCAB.TCABArchive.Store);
    { archACE }
RACEArchiver : TStreamRec = (
      ObjType: otACEArchiver;
      VmtLink: (TypeOf(archACE.TACEArchive));
      Load: @archACE.TACEArchive.Load;
      Store: @archACE.TACEArchive.Store);
    { archHA }
RHAArchiver : TStreamRec = (
      ObjType: otHAArchiver;
      VmtLink: (TypeOf(archHA.THAArchive));
      Load: @archHA.THAArchive.Load;
      Store: @archHA.THAArchive.Store);
    {$IFNDEF MINARCH}
    { archarc }
RARCArchiver : TStreamRec = (
      ObjType: otARCArchiver;
      VmtLink: (TypeOf(archARC.TARCArchive));
      Load: @archarc.TARCArchive.Load;
      Store: @archarc.TARCArchive.Store);
    { archbsa }
RBSAArchiver : TStreamRec = (
      ObjType: otBSAArchiver;
      VmtLink: (TypeOf(archBSA.TBSAArchive));
      Load: @archbsa.TBSAArchive.Load;
      Store: @archbsa.TBSAArchive.Store);
    { archbs2 }
RBS2Archiver : TStreamRec = (
      ObjType: otBS2Archiver;
      VmtLink: (TypeOf(archBS2.TBS2Archive));
      Load: @archbs2.TBS2Archive.Load;
      Store: @archbs2.TBS2Archive.Store);
    { archhyp }
RHYPArchiver : TStreamRec = (
      ObjType: otHYPArchiver;
      VmtLink: (TypeOf(archHYP.THYPArchive));
      Load: @archhyp.THYPArchive.Load;
      Store: @archhyp.THYPArchive.Store);
    { archlim }
RLIMArchiver : TStreamRec = (
      ObjType: otLIMArchiver;
      VmtLink: (TypeOf(archLIM.TLIMArchive));
      Load: @archlim.TLIMArchive.Load;
      Store: @archlim.TLIMArchive.Store);
    { archhpk }
RHPKArchiver : TStreamRec = (
      ObjType: otHPKArchiver;
      VmtLink: (TypeOf(archHPK.THPKArchive));
      Load: @archHpk.THPKArchive.Load;
      Store: @archhpk.THPKArchive.Store);
    { archTAR }
RTARArchiver : TStreamRec = (
      ObjType: otTARArchiver;
      VmtLink: (TypeOf(archTAR.TTARArchive));
      Load: @archTAR.TTARArchive.Load;
      Store: @archTAR.TTARArchive.Store);
    { archTGZ }
RTGZArchiver : TStreamRec = (
      ObjType: otTGZArchiver;
      VmtLink: (TypeOf(archTGZ.TTGZArchive));
      Load: @archTGZ.TTGZArchive.Load;
      Store: @archTGZ.TTGZArchive.Store);
    { archZXZ }
RZXZArchiver : TStreamRec = (
      ObjType: otZXZArchiver;
      VmtLink: (TypeOf(archZXZ.TZXZArchive));
      Load: @archZXZ.TZXZArchive.Load;
      Store: @archZXZ.TZXZArchive.Store);
    { archQRK }
RQUARKArchiver : TStreamRec = (
      ObjType: otQUARKArchiver;
      VmtLink: (TypeOf(archQRK.TQuArkArchive));
      Load: @archQRK.TQuArkArchive.Load;
      Store: @archQRK.TQuArkArchive.Store);
    { archUFA }
RUFAArchiver : TStreamRec = (
      ObjType: otUFAArchiver;
      VmtLink: (TypeOf(archUFA.TUFAArchive));
      Load: @archUFA.TUFAArchive.Load;
      Store: @archUFA.TUFAArchive.Store);
    { archIS3 }
RIS3Archiver : TStreamRec = (
      ObjType: otIS3Archiver;
      VmtLink: (TypeOf(archIS3.TIS3Archive));
      Load: @archIS3.TIS3Archive.Load;
      Store: @archIS3.TIS3Archive.Store);
    { archSQZ }
RSQZArchiver : TStreamRec = (
      ObjType: otSQZArchiver;
      VmtLink: (TypeOf(archSQZ.TSQZArchive));
      Load: @archSQZ.TSQZArchive.Load;
      Store: @archSQZ.TSQZArchive.Store);
    { archHAP }
RHAPArchiver : TStreamRec = (
      ObjType: otHAPArchiver;
      VmtLink: (TypeOf(archHAP.THAPArchive));
      Load: @archHAP.THAPArchive.Load;
      Store: @archHAP.THAPArchive.Store);
    { archZOO }
RZOOArchiver : TStreamRec = (
      ObjType: otZOOArchiver;
      VmtLink: (TypeOf(archZOO.TZOOArchive));
      Load: @archZOO.TZOOArchive.Load;
      Store: @archZOO.TZOOArchive.Store);
    { archCHZ }
RCHZArchiver : TStreamRec = (
      ObjType: otCHZArchiver;
      VmtLink: (TypeOf(archCHZ.TCHZArchive));
      Load: @archCHZ.TCHZArchive.Load;
      Store: @archCHZ.TCHZArchive.Store);
    { archUC2 }
RUC2Archiver : TStreamRec = (
      ObjType: otUC2Archiver;
      VmtLink: (TypeOf(archUC2.TUC2Archive));
      Load: @archUC2.TUC2Archive.Load;
      Store: @archUC2.TUC2Archive.Store);
    { archAIN }
RAINArchiver : TStreamRec = (
      ObjType: otAINArchiver;
      VmtLink: (TypeOf(archAIN.TAINArchive));
      Load: @archAIN.TAINArchive.Load;
      Store: @archAIN.TAINArchive.Store);
    { arch7Z }
RS7ZArchiver : TStreamRec = (
      ObjType: otS7ZArchiver;
      VmtLink: (TypeOf(arch7Z.TS7ZArchive));
      Load: @arch7Z.TS7ZArchive.Load;
      Store: @arch7Z.TS7ZArchive.Store);
    { archBZ2 }
RBZ2Archiver : TStreamRec = (
      ObjType: otBZ2Archiver;
      VmtLink: (TypeOf(archBZ2.TBZ2Archive));
      Load: @archBZ2.TBZ2Archive.Load;
      Store: @archBZ2.TBZ2Archive.Store);
    {$ENDIF MINARCH}
    { Archiver }
RARJArchiver : TStreamRec = (
      ObjType: otARJArchiver;
      VmtLink: (TypeOf(Archiver.TARJArchive));
      Load: @Archiver.TARJArchive.Load;
      Store: @Archiver.TARJArchive.Store);
RFileInfo : TStreamRec = (
      ObjType: otFileInfo;
      VmtLink: (TypeOf(Archiver.TFileInfo));
      Load: @Archiver.TFileInfo.Load;
      Store: @Archiver.TFileInfo.Store);
    {$IFDEF UserSaver}
RUserSaver : TStreamRec = (
      ObjType: otUserSaver;
      VmtLink: (TypeOf(UserSavr.TUserSaver));
      Load: @UserSavr.TUserSaver.Load;
      Store: @UserSavr.TUserSaver.Store);
    {$ENDIF UserSaver}
    { ArcView }
RArcDrive : TStreamRec = (
      ObjType: otArcDrive;
      VmtLink: (TypeOf(ArcView.TArcDrive));
      Load: @ArcView.TArcDrive.Load;
      Store: @ArcView.TArcDrive.Store);
    {$IFDEF ARVID}
    { Arvid }
RArvidDrive : TStreamRec = (
      ObjType: otArvidDrive;
      VmtLink: (TypeOf(Arvid.TArvidDrive));
      Load: @Arvid.TArvidDrive.Load;
      Store: @Arvid.TArvidDrive.Store);
    {$ENDIF}
    { AsciiTab }
RTable : TStreamRec = (
      ObjType: otTable;
      VmtLink: (TypeOf(ASCIITab.TTable));
      Load: @AsciiTab.TTable.Load;
      Store: @AsciiTab.TTable.Store);
RReport : TStreamRec = (
      ObjType: otReport;
      VmtLink: (TypeOf(ASCIITab.TReport));
      Load: @AsciiTab.TReport.Load;
      Store: @AsciiTab.TReport.Store);
RASCIIChart : TStreamRec = (
      ObjType: otASCIIChart;
      VmtLink: (TypeOf(ASCIITab.TASCIIChart));
      Load: @AsciiTab.TASCIIChart.Load;
      Store: @AsciiTab.TASCIIChart.Store);
    { Calc }
    {$IFDEF SpreadSheet}
RCalcWindow : TStreamRec = (
      ObjType: otCalcWindow;
      VmtLink: (TypeOf(Calc.TCalcWindow));
      Load: @Calc.TCalcWindow.Load;
      Store: @Calc.TCalcWindow.Store);
RCalcView : TStreamRec = (
      ObjType: otCalcView;
      VmtLink: (TypeOf(Calc.TCalcView));
      Load: @Calc.TCalcView.Load;
      Store: @Calc.TCalcView.Store);
RCalcInfo : TStreamRec = (
      ObjType: otCalcInfo;
      VmtLink: (TypeOf(Calc.TCalcInput));
      Load: @Calc.TCalcInput.Load;
      Store: @Calc.TCalcInput.Store);
RInfoView : TStreamRec = (
      ObjType: otInfoView;
      VmtLink: (TypeOf(Calc.TInfoView));
      Load: @Calc.TInfoView.Load;
      Store: @Calc.TInfoView.Store);
    { CellsCol }
RCellCollection : TStreamRec = (
      ObjType: otCellCollection;
      VmtLink: (TypeOf(CellsCol.TCellCollection));
      Load: @CellsCol.TCellCollection.Load;
      Store: @CellsCol.TCellCollection.Store);
    {$ENDIF SpreadSheet}
    {$IFDEF Calendar}
    { Calendar }
RCalendarView : TStreamRec = (
      ObjType: otCalendarView;
      VmtLink: (TypeOf(Calendar.TCalendarView));
      Load: @Calendar.TCalendarView.Load;
      Store: @Calendar.TCalendarView.Store);
RCalendarWindow : TStreamRec = (
      ObjType: otCalendarWindow;
      VmtLink: (TypeOf(Calendar.TCalendarWindow));
      Load: @Calendar.TCalendarWindow.Load;
      Store: @Calendar.TCalendarWindow.Store);
    {$ENDIF Calendar}
    { CCalc }
RCalcLine : TStreamRec = (
      ObjType: otCalcLine;
      VmtLink: (TypeOf(CCalc.TCalcLine));
      Load: @CCalc.TCalcLine.Load;
      Store: @CCalc.TCalcLine.Store);
RIndicator : TStreamRec = (
      ObjType: otIndicator;
      VmtLink: (TypeOf(CCalc.TIndicator));
      Load: @CCalc.TIndicator.Load;
      Store: @CCalc.TIndicator.Store);
    { Collect }
RCollection : TStreamRec = (
      ObjType: otCollection;
      VmtLink: (TypeOf(Collect.TCollection));
      Load: @Collect.TCollection.Load;
      Store: @Collect.TCollection.Store);
RLineCollection : TStreamRec = (
      ObjType: otLineCollection;
      VmtLink: (TypeOf(Collect.TLineCollection));
      Load: @Collect.TLineCollection.Load;
      Store: @Collect.TLineCollection.Store);
RStringCollection : TStreamRec = (
      ObjType: otStringCollection;
      VmtLink: (TypeOf(Collect.TStringCollection));
      Load: @Collect.TStringCollection.Load;
      Store: @Collect.TStringCollection.Store);
RStrCollection : TStreamRec = (
      ObjType: otStrCollection;
      VmtLink: (TypeOf(Collect.TStrCollection));
      Load: @Collect.TStrCollection.Load;
      Store: @Collect.TStrCollection.Store);
RStringList : TStreamRec = (
      ObjType: otStringList;
      VmtLink: (TypeOf(Collect.TStringList));
      Load: @Collect.TStringList.Load;
      Store: nil);
    {$ENDIF !RCP}
    { ColorSel }
RColorSelector : TStreamRec = (
      ObjType: otColorSelector;
      VmtLink: (TypeOf(ColorSel.TColorSelector));
      Load: @ColorSel.TColorSelector.Load;
      Store: @ColorSel.TColorSelector.Store);
RMonoSelector : TStreamRec = (
      ObjType: otMonoSelector;
      VmtLink: (TypeOf(ColorSel.TMonoSelector));
      Load: @ColorSel.TMonoSelector.Load;
      Store: @ColorSel.TMonoSelector.Store);
RColorDisplay : TStreamRec = (
      ObjType: otColorDisplay;
      VmtLink: (TypeOf(ColorSel.TColorDisplay));
      Load: @ColorSel.TColorDisplay.Load;
      Store: @ColorSel.TColorDisplay.Store);
RColorGroupList : TStreamRec = (
      ObjType: otColorGroupList;
      VmtLink: (TypeOf(ColorSel.TColorGroupList));
      Load: @ColorSel.TColorGroupList.Load;
      Store: @ColorSel.TColorGroupList.Store);
RColorItemList : TStreamRec = (
      ObjType: otColorItemList;
      VmtLink: (TypeOf(ColorSel.TColorItemList));
      Load: @ColorSel.TColorItemList.Load;
      Store: @ColorSel.TColorItemList.Store);
RColorDialog : TStreamRec = (
      ObjType: otColorDialog;
      VmtLink: (TypeOf(ColorSel.TColorDialog));
      Load: @ColorSel.TColorDialog.Load;
      Store: @ColorSel.TColorDialog.Store);
RR_BWSelector : TStreamRec = (
      ObjType: otR_BWSelector;
      VmtLink: (TypeOf(ColorSel.T_BWSelector));
      Load: @ColorSel.T_BWSelector.Load;
      Store: @ColorSel.T_BWSelector.Store);
    {$IFNDEF RCP}
    {$IFDEF DBView}
    { DBView }
RDBWindow : TStreamRec = (
      ObjType: otDBWindow;
      VmtLink: (TypeOf(DBView.TDBWindow));
      Load: @DBView.TDBWindow.Load;
      Store: @DBView.TDBWindow.Store);
RDBViewer : TStreamRec = (
      ObjType: otDBViewer;
      VmtLink: (TypeOf(DBView.TDBViewer));
      Load: @DBView.TDBViewer.Load;
      Store: @DBView.TDBViewer.Store);
RDBIndicator : TStreamRec = (
      ObjType: otDBIndicator;
      VmtLink: (TypeOf(DBView.TDBIndicator));
      Load: @DBView.TDBIndicator.Load;
      Store: @DBView.TDBIndicator.Store);
RFieldListBox : TStreamRec = (
      ObjType: otFieldListBox;
      VmtLink: (TypeOf(DBView.TFieldListBox));
      Load: @DBView.TFieldListBox.Load;
      Store: @DBView.TFieldListBox.Store);
    {$ENDIF DBView}
    {$ENDIF !RCP}
    { Dialogs }
RDialog : TStreamRec = (
      ObjType: otDialog;
      VmtLink: (TypeOf(Dialogs.TDialog));
      Load: @Dialogs.TDialog.Load;
      Store: @Dialogs.TDialog.Store);
RInputLine : TStreamRec = (
      ObjType: otInputLine;
      VmtLink: (TypeOf(Dialogs.TInputLine));
      Load: @Dialogs.TInputLine.Load;
      Store: @Dialogs.TInputLine.Store);
RHexLine : TStreamRec = (
      ObjType: otHexLine;
      VmtLink: (TypeOf(Dialogs.THexLine));
      Load: @Dialogs.THexLine.Load;
      Store: @Dialogs.THexLine.Store);
RLongInputLine : TStreamRec = (
      ObjType: otLongInputLine;
      VmtLink: (TypeOf(Dialogs.TLongInputLine));
      Load: @Dialogs.TLongInputLine.Load;
      Store: @Dialogs.TLongInputLine.Store);
RButton : TStreamRec = (
      ObjType: otButton;
      VmtLink: (TypeOf(Dialogs.TButton));
      Load: @Dialogs.TButton.Load;
      Store: @Dialogs.TButton.Store);
RCluster : TStreamRec = (
      ObjType: otCluster;
      VmtLink: (TypeOf(Dialogs.TCluster));
      Load: @Dialogs.TCluster.Load;
      Store: @Dialogs.TCluster.Store);
RRadioButtons : TStreamRec = (
      ObjType: otRadioButtons;
      VmtLink: (TypeOf(Dialogs.TRadioButtons));
      Load: @Dialogs.TRadioButtons.Load;
      Store: @Dialogs.TRadioButtons.Store);
RComboBox : TStreamRec = (
      ObjType: otComboBox;
      VmtLink: (TypeOf(Dialogs.TComboBox));
      Load: @Dialogs.TComboBox.Load;
      Store: @Dialogs.TComboBox.Store);
RCheckBoxes : TStreamRec = (
      ObjType: otCheckBoxes;
      VmtLink: (TypeOf(Dialogs.TCheckBoxes));
      Load: @Dialogs.TCheckBoxes.Load;
      Store: @Dialogs.TCheckBoxes.Store);
RMultiCheckBoxes : TStreamRec = (
      ObjType: otMultiCheckBoxes;
      VmtLink: (TypeOf(Dialogs.TMultiCheckBoxes));
      Load: @Dialogs.TMultiCheckBoxes.Load;
      Store: @Dialogs.TMultiCheckBoxes.Store);
RListBox : TStreamRec = (
      ObjType: otListBox;
      VmtLink: (TypeOf(Dialogs.TListBox));
      Load: @Dialogs.TListBox.Load;
      Store: @Dialogs.TListBox.Store);
RStaticText : TStreamRec = (
      ObjType: otStaticText;
      VmtLink: (TypeOf(Dialogs.TStaticText));
      Load: @Dialogs.TStaticText.Load;
      Store: @Dialogs.TStaticText.Store);
RLabel : TStreamRec = (
      ObjType: otLabel;
      VmtLink: (TypeOf(Dialogs.TLabel));
      Load: @Dialogs.TLabel.Load;
      Store: @Dialogs.TLabel.Store);
RHistory : TStreamRec = (
      ObjType: otHistory;
      VmtLink: (TypeOf(Dialogs.THistory));
      Load: @Dialogs.THistory.Load;
      Store: @Dialogs.THistory.Store);
RParamText : TStreamRec = (
      ObjType: otParamText;
      VmtLink: (TypeOf(Dialogs.TParamText));
      Load: @Dialogs.TParamText.Load;
      Store: @Dialogs.TParamText.Store);
RNotepad : TStreamRec = (
      ObjType: otNotepad;
      VmtLink: (TypeOf(Dialogs.TNotepad));
      Load: @Dialogs.TNotepad.Load;
      Store: @Dialogs.TNotepad.Store);
RPage : TStreamRec = (
      ObjType: otPage;
      VmtLink: (TypeOf(Dialogs.TPage));
      Load: @Dialogs.TPage.Load;
      Store: @Dialogs.TPage.Store);
RBookmark : TStreamRec = (
      ObjType: otBookmark;
      VmtLink: (TypeOf(Dialogs.TBookmark));
      Load: @Dialogs.TBookmark.Load;
      Store: @Dialogs.TBookmark.Store);
RPageFrame : TStreamRec = (
      ObjType: otPageFrame;
      VmtLink: (TypeOf(Dialogs.TPageFrame));
      Load: @Dialogs.TPageFrame.Load;
      Store: @Dialogs.TPageFrame.Store);
RNotepadFrame : TStreamRec = (
      ObjType: otNotepadFrame;
      VmtLink: (TypeOf(Dialogs.TNotepadFrame));
      Load: @Dialogs.TNotepadFrame.Load;
      Store: @Dialogs.TNotepadFrame.Store);
    {$IFNDEF RCP}
    { DiskInfo }
RDiskInfo : TStreamRec = (
      ObjType: otDiskInfo;
      VmtLink: (TypeOf(DiskInfo.TDiskInfo));
      Load: @DiskInfo.TDiskInfo.Load;
      Store: @DiskInfo.TDiskInfo.Store);
RDriveView : TStreamRec = (
      ObjType: otDriveView;
      VmtLink: (TypeOf(DiskInfo.TDriveView));
      Load: @DiskInfo.TDriveView.Load;
      Store: @DiskInfo.TDriveView.Store);
    { DnApp }
RBackground : TStreamRec = (
      ObjType: otBackground;
      VmtLink: (TypeOf(DNApp.TBackground));
      Load: @DnApp.TBackground.Load;
      Store: @DnApp.TBackground.Store);
RDesktop : TStreamRec = (
      ObjType: otDesktop;
      VmtLink: (TypeOf(DNApp.TDesktop));
      Load: @DnApp.TDesktop.Load;
      Store: @DnApp.TDesktop.Store);
    { DnStdDlg }
RFileInputLine : TStreamRec = (
      ObjType: otFileInputLine;
      VmtLink: (TypeOf(DNStdDlg.TFileInputLine));
      Load: @DnStdDlg.TFileInputLine.Load;
      Store: @DnStdDlg.TFileInputLine.Store);
RFileCollection : TStreamRec = (
      ObjType: otFileCollection;
      VmtLink: (TypeOf(DNStdDlg.TFileCollection));
      Load: @DnStdDlg.TFileCollection.Load;
      Store: @DnStdDlg.TFileCollection.Store);
RFileList : TStreamRec = (
      ObjType: otFileList;
      VmtLink: (TypeOf(DNStdDlg.TFileList));
      Load: @DnStdDlg.TFileList.Load;
      Store: @DnStdDlg.TFileList.Store);
RFileInfoPane : TStreamRec = (
      ObjType: otFileInfoPane;
      VmtLink: (TypeOf(DNStdDlg.TFileInfoPane));
      Load: @DnStdDlg.TFileInfoPane.Load;
      Store: @DnStdDlg.TFileInfoPane.Store);
RFileDialog : TStreamRec = (
      ObjType: otFileDialog;
      VmtLink: (TypeOf(DNStdDlg.TFileDialog));
      Load: @DnStdDlg.TFileDialog.Load;
      Store: @DnStdDlg.TFileDialog.Store);
RSortedListBox : TStreamRec = (
      ObjType: otSortedListBox;
      VmtLink: (TypeOf(DNStdDlg.TSortedListBox));
      Load: @DnStdDlg.TSortedListBox.Load;
      Store: @DnStdDlg.TSortedListBox.Store);
    { DNUtil }
RDataSaver : TStreamRec = (
      ObjType: otDataSaver;
      VmtLink: (TypeOf(DNUtil.TDataSaver));
      Load: @DNUtil.TDataSaver.Load;
      Store: @DNUtil.TDataSaver.Store);
    { Drives }
RDrive : TStreamRec = (
      ObjType: otDrive;
      VmtLink: (TypeOf(Drives.TDrive));
      Load: @Drives.TDrive.Load;
      Store: @Drives.TDrive.Store);
    { Ed2 }
RInfoLine : TStreamRec = (
      ObjType: otInfoLine;
      VmtLink: (TypeOf(ed2.TInfoLine));
      Load: @Ed2.TInfoLine.Load;
      Store: @Ed2.TInfoLine.Store);
RBookLine : TStreamRec = (
      ObjType: otBookLine;
      VmtLink: (TypeOf(ed2.TBookmarkLine));
      Load: @Ed2.TBookmarkLine.Load;
      Store: @Ed2.TBookmarkLine.Store);
    { Editor }
RXFileEditor : TStreamRec = (
      ObjType: otXFileEditor;
      VmtLink: (TypeOf(Editor.TXFileEditor));
      Load: @Editor.TXFileEditor.Load;
      Store: @Editor.TXFileEditor.Store);
    { FileFind }
RFindDrive : TStreamRec = (
      ObjType: otFindDrive;
      VmtLink: (TypeOf(FileFind.TFindDrive));
      Load: @FileFind.TFindDrive.Load;
      Store: @FileFind.TFindDrive.Store);
RTempDrive : TStreamRec = (
      ObjType: otTempDrive;
      VmtLink: (TypeOf(FileFind.TTempDrive));
      Load: @FileFind.TTempDrive.Load;
      Store: @FileFind.TTempDrive.Store);
    {$IFDEF NetBrowser}
    { NetBrwsr }
RNetDrive : TStreamRec = (
     ObjType: otNetDrive;
     VmtLink: (TypeOf(NetBrwsr.TNetDrive));
     Load: @NetBrwsr.TNetDrive.Load;
     Store: @NetBrwsr.TNetDrive.Store);
    {$ENDIF}
    { FilesCol }
RFilesCollection : TStreamRec = (
      ObjType: otFilesCollection;
      VmtLink: (TypeOf(FilesCol.TFilesCollection));
      Load: @FilesCol.TFilesCollection.Load;
      Store: @FilesCol.TFilesCollection.Store);
    { FlPanel }
RFilePanel : TStreamRec = (
      ObjType: otFilePanel;
      VmtLink: (TypeOf(FlPanel.TFilePanel));
      Load: @FlPanel.TFilePanel.Load;
      Store: @FlPanel.TFilePanel.Store);
RFlPInfoView : TStreamRec = (
      ObjType: otFlPInfoView;
      VmtLink: (TypeOf(FlPanel.TInfoView));
      Load: @FlPanel.TInfoView.Load;
      Store: @FlPanel.TInfoView.Store);
RDirView : TStreamRec = (
      ObjType: otDirView;
      VmtLink: (TypeOf(FlPanel.TDirView));
      Load: @FlPanel.TDirView.Load;
      Store: @FlPanel.TDirView.Store);
RSortView : TStreamRec = (
      ObjType: otSortView;
      VmtLink: (TypeOf(TopView.TSortView));
      Load: @TopView.TSortView.Load;
      Store: @TopView.TSortView.Store);
RSeparator : TStreamRec = (
      ObjType: otSeparator;
      VmtLink: (TypeOf(DblWnd.TSeparator));
      Load: @DblWnd.TSeparator.Load;
      Store: @DblWnd.TSeparator.Store);
RDriveLine : TStreamRec = (
      ObjType: otDriveLine;
      VmtLink: (TypeOf(FlPanel.TDriveLine));
      Load: @FlPanel.TDriveLine.Load;
      Store: @FlPanel.TDriveLine.Store);
    { FStorage }
RDirStorage : TStreamRec = (
      ObjType: otDirStorage;
      VmtLink: (TypeOf(FStorage.TDirStorage));
      Load: @FStorage.TDirStorage.Load;
      Store: @FStorage.TDirStorage.Store);
    { FViewer }
RFileViewer : TStreamRec = (
      ObjType: otFileViewer;
      VmtLink: (TypeOf(FViewer.TFileViewer));
      Load: @FViewer.TFileViewer.Load;
      Store: @FViewer.TFileViewer.Store);
RFileWindow : TStreamRec = (
      ObjType: otFileWindow;
      VmtLink: (TypeOf(FViewer.TFileWindow));
      Load: @FViewer.TFileWindow.Load;
      Store: @FViewer.TFileWindow.Store);
RViewScroll : TStreamRec = (
      ObjType: otViewScroll;
      VmtLink: (TypeOf(FViewer.TViewScroll));
      Load: @FViewer.TViewScroll.Load;
      Store: @FViewer.TViewScroll.Store);
RQFileViewer : TStreamRec = (
      ObjType: otQFileViewer;
      VmtLink: (TypeOf(FViewer.TQFileViewer));
      Load: @FViewer.TQFileViewer.Load;
      Store: @FViewer.TQFileViewer.Store);
RDFileViewer : TStreamRec = (
      ObjType: otDFileViewer;
      VmtLink: (TypeOf(FViewer.TDFileViewer));
      Load: @FViewer.TDFileViewer.Load;
      Store: @FViewer.TDFileViewer.Store);
RViewInfo : TStreamRec = (
      ObjType: otViewInfo;
      VmtLink: (TypeOf(FViewer.TViewInfo));
      Load: @FViewer.TViewInfo.Load;
      Store: @FViewer.TViewInfo.Store);
    { Gauges }
    {$IFDEF TrashCan}
RTrashCan : TStreamRec = (
      ObjType: otTrashCan;
      VmtLink: (TypeOf(Gauges.TTrashCan));
      Load: @Gauges.TTrashCan.Load;
      Store: @Gauges.TTrashCan.Store);
    {$ENDIF TrashCan}
RKeyMacros : TStreamRec = (
      ObjType: otKeyMacros;
      VmtLink: (TypeOf(Gauges.TKeyMacros));
      Load: @Gauges.TKeyMacros.Load;
      Store: @Gauges.TKeyMacros.Store);
    { HelpKern }
RHelpTopic : TStreamRec = (
      ObjType: otHelpTopic;
      VmtLink: (TypeOf(HelpKern.THelpTopic));
      Load: @HelpKern.THelpTopic.Load;
      Store: @HelpKern.THelpTopic.Store);
RHelpIndex : TStreamRec = (
      ObjType: otHelpIndex;
      VmtLink: (TypeOf(HelpKern.THelpIndex));
      Load: @HelpKern.THelpIndex.Load;
      Store: @HelpKern.THelpIndex.Store);
    { Histries }
REditHistoryCol : TStreamRec = (
      ObjType: otEditHistoryCol;
      VmtLink: (TypeOf(Histries.TEditHistoryCol));
      Load: @Histries.TEditHistoryCol.Load;
      Store: @Histries.TEditHistoryCol.Store);
RViewHistoryCol : TStreamRec = (
      ObjType: otViewHistoryCol;
      VmtLink: (TypeOf(Histries.TViewHistoryCol));
      Load: @Histries.TViewHistoryCol.Load;
      Store: @Histries.TViewHistoryCol.Store);
    { Menus }
    {$ENDIF !RCP}
RMenuBar : TStreamRec = (
      ObjType: otMenuBar;
      VmtLink: (TypeOf(Menus.TMenuBar));
      Load: @Menus.TMenuBar.Load;
      Store: @Menus.TMenuBar.Store);
RMenuBox : TStreamRec = (
      ObjType: otMenuBox;
      VmtLink: (TypeOf(Menus.TMenuBox));
      Load: @Menus.TMenuBox.Load;
      Store: @Menus.TMenuBox.Store);
RStatusLine : TStreamRec = (
      ObjType: otStatusLine;
      VmtLink: (TypeOf(Menus.TStatusLine));
      Load: @Menus.TStatusLine.Load;
      Store: @Menus.TStatusLine.Store);
RMenuPopup : TStreamRec = (
      ObjType: otMenuPopup;
      VmtLink: (TypeOf(Menus.TMenuPopup));
      Load: @Menus.TMenuPopup.Load;
      Store: @Menus.TMenuPopup.Store);
    {$IFNDEF RCP}
    { Microed }
RFileEditor : TStreamRec = (
      ObjType: otFileEditor;
      VmtLink: (TypeOf(Microed.TFileEditor));
      Load: @Microed.TFileEditor.Load;
      Store: @Microed.TFileEditor.Store);
REditWindow : TStreamRec = (
      ObjType: otEditWindow;
      VmtLink: (TypeOf(EdWin.TEditWindow));
      Load: @EdWin.TEditWindow.Load;
      Store: @EdWin.TEditWindow.Store);
    {$IFDEF MODEM}
    {$IFDEF LINK}
    { NavyLink }
RLinker : TStreamRec = (
      ObjType: otLinker;
      VmtLink: (TypeOf(NavyLink.TLinker));
      Load: @NavyLink.TLinker.Load;
      Store: @NavyLink.TLinker.Store);
RLinkDrive : TStreamRec = (
      ObjType: otLinkDrive;
      VmtLink: (TypeOf(NavyLink.TLinkDrive));
      Load: @NavyLink.TLinkDrive.Load;
      Store: @NavyLink.TLinkDrive.Store);
    {$ENDIF LINK IN MODEM}
    { Dialer }
RAutoDialer : TStreamRec = (
      ObjType: otAutoDialer;
      VmtLink: (TypeOf(uDialer.TAutoDialer));
      Load: @uDialer.TAutoDialer.Load;
      Store: @uDialer.TAutoDialer.Store);
RDialBox : TStreamRec = (
      ObjType: otDialBox;
      VmtLink: (TypeOf(uDialer.TDialBox));
      Load: @uDialer.TDialBox.Load;
      Store: @uDialer.TDialBox.Store);
    { ScrollBk }
RScrollBackWindow : TStreamRec = (
      ObjType: otScrollBackWindow;
      VmtLink: (TypeOf(ScrollBk.TScrollBackWindow));
      Load: @ScrollBk.TScrollBackWindow.Load;
      Store: @ScrollBk.TScrollBackWindow.Store);
RScrollBack : TStreamRec = (
      ObjType: otScrollBack;
      VmtLink: (TypeOf(ScrollBk.TScrollBack));
      Load: @ScrollBk.TScrollBack.Load;
      Store: @ScrollBk.TScrollBack.Store);
    {$ENDIF MODEM}
    {$IFDEF NETINFO}
    { NetInfo }
RNetInfo : TStreamRec = (
      ObjType: otNetInfo;
      VmtLink: (TypeOf(NetInfo.TNetInfo));
      Load: @NetInfo.TNetInfo.Load;
      Store: @NetInfo.TNetInfo.Store);
    {$ENDIF NETINFO}
    {$IFDEF PHONES}
RDStringView : TStreamRec = (
      ObjType: otDStringView;
      VmtLink: (TypeOf(StrView.TDStringView));
      Load: @StrView.TDStringView.Load;
      Store: @StrView.TDStringView.Store);
RPhone : TStreamRec = (
      ObjType: otPhone;
      VmtLink: (TypeOf(Phones.TPhone));
      Load: @Phones.TPhone.Load;
      Store: @Phones.TPhone.Store);
RPhoneDir : TStreamRec = (
      ObjType: otPhoneDir;
      VmtLink: (TypeOf(Phones.TPhoneDir));
      Load: @Phones.TPhoneDir.Load;
      Store: @Phones.TPhoneDir.Store);
RPhoneCollection : TStreamRec = (
      ObjType: otPhoneCollection;
      VmtLink: (TypeOf(Phones.TPhoneCollection));
      Load: @Phones.TPhoneCollection.Load;
      Store: @Phones.TPhoneCollection.Store);
    {$ENDIF PHONES}
    {$IFDEF PrintManager}
    { PrintManager }
RStringCol : TStreamRec = (
      ObjType: otStringCol;
      VmtLink: (TypeOf(PrintMan.TStringCol));
      Load: @PrintMan.TStringCol.Load;
      Store: @PrintMan.TStringCol.Store);
RPrintManager : TStreamRec = (
      ObjType: otPrintManager;
      VmtLink: (TypeOf(PrintMan.TPrintManager));
      Load: @PrintMan.TPrintManager.Load;
      Store: @PrintMan.TPrintManager.Store);
RPrintStatus : TStreamRec = (
      ObjType: otPrintStatus;
      VmtLink: (TypeOf(PrintMan.TPrintStatus));
      Load: @PrintMan.TPrintStatus.Load;
      Store: @PrintMan.TPrintStatus.Store);
RPMWindow : TStreamRec = (
      ObjType: otPMWindow;
      VmtLink: (TypeOf(PrintMan.TPMWindow));
      Load: @PrintMan.TPMWindow.Load;
      Store: @PrintMan.TPMWindow.Store);
    {$ENDIF PrintManager}
    {$ENDIF !RCP}
    { Scroller }
RScroller : TStreamRec = (
      ObjType: otScroller;
      VmtLink: (TypeOf(Scroller.TScroller));
      Load: @Scroller.TScroller.Load;
      Store: @Scroller.TScroller.Store);
RListViewer : TStreamRec = (
      ObjType: otListViewer;
      VmtLink: (TypeOf(Scroller.TListViewer));
      Load: @Scroller.TListViewer.Load;
      Store: @Scroller.TListViewer.Store);
    { Setups }
RSysDialog : TStreamRec = (
      ObjType: otSysDialog;
      VmtLink: (TypeOf(Setups.TSysDialog));
      Load: @Setups.TSysDialog.Load;
      Store: @Setups.TSysDialog.Store);
RCurrDriveInfo : TStreamRec = (
      ObjType: otCurrDriveInfo;
      VmtLink: (TypeOf(Setups.TCurrDriveInfo));
      Load: @Setups.TCurrDriveInfo.Load;
      Store: @Setups.TCurrDriveInfo.Store);
RMouseBar : TStreamRec = (
      ObjType: otMouseBar;
      VmtLink: (TypeOf(Setups.TMouseBar));
      Load: @Setups.TMouseBar.Load;
      Store: @Setups.TMouseBar.Store);
    {$IFDEF SS}
RSaversDialog : TStreamRec = (
      ObjType: otSaversDialog;
      VmtLink: (TypeOf(Setups.TSaversDialog));
      Load: @Setups.TSaversDialog.Load;
      Store: @Setups.TSaversDialog.Store);
RSaversListBox : TStreamRec = (
      ObjType: otSaversListBox;
      VmtLink: (TypeOf(Setups.TSaversListBox));
      Load: @Setups.TSaversListBox.Load;
      Store: @Setups.TSaversListBox.Store);
    {$ENDIF SS}
    {$IFNDEF RCP}
    { Startup }
RTextCollection : TStreamRec = (
      ObjType: otTextCollection;
      VmtLink: (TypeOf(Startupp.TTextCollection));
      Load: @Startupp.TTextCollection.Load;
      Store: @Startupp.TTextCollection.Store);
    { Terminal }
    {$IFDEF Modem}
RTerminalWindow : TStreamRec = (
      ObjType: otTerminalWindow;
      VmtLink: (TypeOf(Terminal.TTerminalWindow));
      Load: @Terminal.TTerminalWindow.Load;
      Store: @Terminal.TTerminalWindow.Store);
RTerminal : TStreamRec = (
      ObjType: otTerminal;
      VmtLink: (TypeOf(Terminal.TTerminal));
      Load: @Terminal.TTerminal.Load;
      Store: @Terminal.TTerminal.Store);
RPortInfo : TStreamRec = (
      ObjType: otPortInfo;
      VmtLink: (TypeOf(Terminal.TPortInfo));
      Load: @Terminal.TPortInfo.Load;
      Store: @Terminal.TPortInfo.Store);
    {$ENDIF Modem}
    {$IFDEF Game}
    { Tetris }
RGameWindow : TStreamRec = (
      ObjType: otGameWindow;
      VmtLink: (TypeOf(Tetris.TGameWindow));
      Load: @Tetris.TGameWindow.Load;
      Store: @Tetris.TGameWindow.Store);
RGameView : TStreamRec = (
      ObjType: otGameView;
      VmtLink: (TypeOf(Tetris.TGameView));
      Load: @Tetris.TGameView.Load;
      Store: @Tetris.TGameView.Store);
RGameInfo : TStreamRec = (
      ObjType: otGameInfo;
      VmtLink: (TypeOf(Tetris.TGameInfo));
      Load: @Tetris.TGameInfo.Load;
      Store: @Tetris.TGameInfo.Store);
    {$ENDIF Game}
    { Tree }
RTreeView : TStreamRec = (
      ObjType: otTreeView;
      VmtLink: (TypeOf(Tree.TTreeView));
      Load: @Tree.TTreeView.Load;
      Store: @Tree.TTreeView.Store);
RTreeReader : TStreamRec = (
      ObjType: otTreeReader;
      VmtLink: (TypeOf(Tree.TTreeReader));
      Load: @Tree.TTreeReader.Load;
      Store: @Tree.TTreeReader.Store);
RTreeWindow : TStreamRec = (
      ObjType: otTreeWindow;
      VmtLink: (TypeOf(Tree.TTreeWindow));
      Load: @Tree.TTreeWindow.Load;
      Store: @Tree.TTreeWindow.Store);
RTreePanel : TStreamRec = (
      ObjType: otTreePanel;
      VmtLink: (TypeOf(Tree.TTreePanel));
      Load: @Tree.TTreePanel.Load;
      Store: @Tree.TTreePanel.Store);
RTreeDialog : TStreamRec = (
      ObjType: otTreeDialog;
      VmtLink: (TypeOf(Tree.TTreeDialog));
      Load: @Tree.TTreeDialog.Load;
      Store: @Tree.TTreeDialog.Store);
RTreeInfoView : TStreamRec = (
      ObjType: otTreeInfoView;
      VmtLink: (TypeOf(Tree.TTreeInfoView));
      Load: @Tree.TTreeInfoView.Load;
      Store: @Tree.TTreeInfoView.Store);
RHTreeView : TStreamRec = (
      ObjType: otHTreeView;
      VmtLink: (TypeOf(Tree.THTreeView));
      Load: @Tree.THTreeView.Load;
      Store: @Tree.THTreeView.Store);
RDirCollection : TStreamRec = (
      ObjType: otDirCollection;
      VmtLink: (TypeOf(Tree.TDirCollection));
      Load: @Tree.TDirCollection.Load;
      Store: @Tree.TDirCollection.Store);
    { UniWin }
REditScrollBar : TStreamRec = (
      ObjType: otEditScrollBar;
      VmtLink: (TypeOf(UniWin.TEditScrollBar));
      Load: @UniWin.TEditScrollBar.Load;
      Store: @UniWin.TEditScrollBar.Store);
REditFrame : TStreamRec = (
      ObjType: otEditFrame;
      VmtLink: (TypeOf(UniWin.TEditFrame));
      Load: @UniWin.TEditFrame.Load;
      Store: @UniWin.TEditFrame.Store);
    { UserMenu }
RUserWindow : TStreamRec = (
      ObjType: otUserWindow;
      VmtLink: (TypeOf(UserMenu.TUserWindow));
      Load: @UserMenu.TUserWindow.Load;
      Store: @UserMenu.TUserWindow.Store);
RUserView : TStreamRec = (
      ObjType: otUserView;
      VmtLink: (TypeOf(UserMenu.TUserView));
      Load: @UserMenu.TUserView.Load;
      Store: @UserMenu.TUserView.Store);
RMyScrollBar : TStreamRec = (
      ObjType: otMyScrollBar;
      VmtLink: (TypeOf(Views.TMyScrollBar));
      Load: @Views.TMyScrollBar.Load;
      Store: @Views.TMyScrollBar.Store);
    { XDblWnd }
RDoubleWindow : TStreamRec = (
      ObjType: otDoubleWindow;
      VmtLink: (TypeOf(XDblWnd.TXDoubleWindow));
      Load: @XDblWnd.TXDoubleWindow.Load;
      Store: @XDblWnd.TXDoubleWindow.Store);
    {$ENDIF !RCP}
{last TStreamRec used in RegisterAll}
RColorPoint : TStreamRec = (
      ObjType: otColorPoint;
      VmtLink: (TypeOf(SWE.TColorPoint));
      Load: @SWE.TColorPoint.Load;
      Store: @SWE.TColorPoint.Store);

procedure RegisterAll;
  var
    P: PStreamRec;
    I: Integer;
  type
    PtrRec = record
      Ofs: LongInt;
      end;
  begin
    P := @RFilterValidator;
    I := Ofs(RRangeValidator) - Ofs(RFilterValidator);
    {use it instead of SizeOf() because of compiler's data align engine}
    repeat
      RegisterType(P^);
      Inc(PtrRec(P).Ofs, I);
    until PtrRec(P).Ofs > Ofs(RColorPoint);
  end;

end.
