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
unit DnIni_p;
{AK155  3.06.2007 Для уменьшения циклических ссылок между модулями
 бывший dnini.pas разбит на два маодуля: dnini.pas и dnini_p.pas,
 при том первый из них содержит бОьшую часть того на что ссылаются
 другие модулиЮ но имеет почти пустой uses-список }

interface

uses
  Commands, DnIni
  ;


function DnIniFileName: String;

procedure LoadDnIniSettings;
procedure SaveDnIniSettings(PVar: Pointer);
procedure DoneIniEngine;

function ProbeINI(var INItime, INIsize: LongInt): Boolean;

procedure ShowIniErrors;
procedure ClearIniErrors;

{JO: 21-04-2005 - кэш INI в отдельном файле}
procedure WriteIniCache(INItime, INIsize: LongInt);
function ReadIniCache(INItime, INIsize: LongInt): Boolean;
{/JO}

implementation
uses
  Dos, Lfn, profile, Advance, Advance1, Collect, Messages, DNApp,
  {$IFDEF WIN32}VpSysLow, {$ENDIF}
  U_KeyMap, Country_,
  Strings, Streams, Advance2
  ;

const
  IniCacheSign: array[1..60] of Char =
    'This file is compiled binary cache of dn.ini; see dnini.txt ';
  INIModified: Boolean = False; {JO}

type

  TSInt8 = ShortInt;
  TSInt16 = SmallInt;
  TSInt32 = LongInt;
  TUInt8 = Byte;
  TUInt16 = Word;

  TIniItemKind = (
    ikBool,
    ikEnum,
    ikStr,
    ikSInt,
    ikUInt,
    ikChar
    );

  TDoProc = procedure (Group, Parameter: PChar; Kind: TIniItemKind;
     Size: Word; PVar: Pointer);

const
  {Group names}
  CSInterface: PChar = 'Interface';
  CSClock: PChar = 'Clock';
  CSSmartPad: PChar = 'SmartPad';
  CSGame: PChar = 'Game';
  CSClipboard: PChar = 'Clipboard';
  CSKernel: PChar = 'Kernel';
  CSEditor: PChar = 'Editor';
  CSFilePanels: PChar = 'FilePanels';
  CSNetInfo: PChar = 'NetInfo';
  CSTaskList: PChar = 'TaskList';
  CSLanguage: PChar = 'Language';
  CSCalendar: PChar = 'Calendar';
  CSRegExp: PChar = 'RegExp';
  CSQuickDirs: PChar = 'QuickDirs';
  CSSetupStorage: PChar = 'SetupStorage';

type
  PIniError = ^TIniError;
  TIniError = record
    Size: Byte;
    Group: PChar;
    Parameter: PChar;
    end;

  PIniErrors = ^TIniErrors;
  TIniErrors = object(TCollection)
    procedure FreeItem(Item: Pointer); virtual;
    end;

const
  IniErrors: PIniErrors = nil;

procedure TIniErrors.FreeItem(Item: Pointer);
  var
    P: PIniError absolute Item;
  begin
  Dispose(P);
  end;

procedure AddIniError(Size: Byte; Group: PChar; Parameter: PChar);
  var
    p: PIniError;
  begin
  if IniErrors = nil then
    IniErrors := New(PIniErrors, Init(1, 1));
  New(p);
  p^.Size := Size;
  p^.Group := Group;
  p^.Parameter := Parameter;
  IniErrors^.Insert(p);
  end;

procedure ShowIniErrors;

  procedure Show(P: Pointer);
    type
      TParams = record
        Size: LongInt;
        Group: PChar;
        Parameter: PChar;
        end;
    var
      I: PIniError absolute P;
      sg: String[10];
      sp: String[21];
      id: TStrIdx;
      flg: Word;
      q: Pointer;
      par: TParams;
    begin
    if P = nil then
      Exit;
    sg := StrPas(I^.Group);
    sp := StrPas(I^.Parameter);
    flg := mfOKButton;
    par.Group := @sg;
    par.Parameter := @sp;
    if I^.Size > 0 then
      begin
      par.Size := I^.Size;
      Inc(flg, mfWarning);
      id := dlIniDataTooLong;
      q := @par.Size;
      end
    else
      begin
      Inc(flg, mfError);
      id := dlIniDataReadError;
      q := @par.Group;
      end;
    Msg(id, q, flg);
    end { Show };

  begin { ShowIniErrors }
  if IniErrors <> nil then
    begin
    IniErrors^.ForEach(@Show);
    ClearIniErrors;
    end;
  end { ShowIniErrors };

procedure ClearIniErrors;
  begin
  if IniErrors <> nil then
    begin
    IniErrors^.FreeAll;
    Dispose(IniErrors, Done);
    IniErrors := nil;
    end;
  end;

function DnIniFileName: String;
  begin
  DnIniFileName := SourceDir+'DN.INI';
  end;

procedure Proceed(RegisterVar: TDoProc);
  begin
  {Interface}
  RegisterVar(CSInterface, 'MenuOnError', ikBool, SizeOf(MenuOnError),
     @MenuOnError);
  RegisterVar(CSInterface, 'CutDriveInfo', ikBool, SizeOf(CutDriveInfo),
     @CutDriveInfo);
  RegisterVar(CSInterface, 'WinManagerSelectNext', ikBool,
       SizeOf(WinManagerSelectNext), @WinManagerSelectNext);
  RegisterVar(CSInterface, 'DriveSelectVCenter', ikBool,
       SizeOf(DriveSelectVCenter), @DriveSelectVCenter); {-$X-Man}
  RegisterVar(CSInterface, 'SystemMenuChar', ikUInt,
       SizeOf(SystemMenuChar), @SystemMenuChar);
  RegisterVar(CSInterface, 'HorizScrollBarChars', ikStr,
       SizeOf(HorizScrollBarChars), @HorizScrollBarChars);
  RegisterVar(CSInterface, 'VertScrollBarChars', ikStr,
       SizeOf(VertScrollBarChars), @VertScrollBarChars);
  RegisterVar(CSInterface, 'ReflectCopyDirection', ikBool,
       SizeOf(ReflectCopyDirection), @ReflectCopyDirection);
  RegisterVar(CSInterface, 'ReuseViewers', ikUInt, SizeOf(ReuseViewers),
     @ReuseViewers);
  RegisterVar(CSInterface, 'ReuseEditors', ikUInt, SizeOf(ReuseEditors),
     @ReuseEditors);
  RegisterVar(CSInterface, 'HistoryErrorBeep', ikBool,
       SizeOf(HistoryErrorBeep), @HistoryErrorBeep);
  RegisterVar(CSInterface, 'PreserveMenuPositions', ikBool,
       SizeOf(PreserveMenuPositions), @PreserveMenuPositions);
  //  RegisterVar(CSInterface, 'DescriptionInBottom',  ikBool, SizeOf(DescriptionInBottom),  @DescriptionInBottom);{JO}
  RegisterVar(CSInterface, 'PanelDescrArvid', ikStr,
       SizeOf(PanelDescrArvid), @PanelDescrArvid); {JO}
  RegisterVar(CSInterface, 'PanelDescrArc', ikStr,
     SizeOf(PanelDescrArc), @PanelDescrArc); {JO}
  RegisterVar(CSInterface, 'PanelDescrTemp', ikStr,
     SizeOf(PanelDescrTemp), @PanelDescrTemp); {JO}
  RegisterVar(CSInterface, 'PanelDescrFind', ikStr,
     SizeOf(PanelDescrFind), @PanelDescrFind); {JO}
  RegisterVar(CSInterface, 'PanelDescrDrive', ikStr,
       SizeOf(PanelDescrDrive), @PanelDescrDrive); {JO}
  RegisterVar(CSInterface, 'UseEnterInViewer', ikUInt,
       SizeOf(UseEnterInViewer), @UseEnterInViewer); {JO}
  RegisterVar(CSInterface, 'SkipXLatMenu', ikBool, SizeOf(SkipXLatMenu),
     @SkipXLatMenu); {JO}
  RegisterVar(CSInterface, 'EscForOutputWindow', ikBool,
       SizeOf(EscForOutputWindow), @EscForOutputWindow); {JO}
  {Clock}
  RegisterVar(CSClock, 'ShowSeconds', ikBool, SizeOf(ShowSeconds),
     @ShowSeconds);
  RegisterVar(CSClock, 'BlinkSeparator', ikBool, SizeOf(BlinkSeparator),
     @BlinkSeparator);
  RegisterVar(CSClock, 'ShowCentury', ikBool, SizeOf(ShowCentury),
     @ShowCentury);
  RegisterVar(CSClock, 'ShowDayOfWeek', ikBool, SizeOf(ShowDayOfWeek),
     @ShowDayOfWeek);
  RegisterVar(CSClock, 'DaysOfWeek', ikStr, SizeOf(DaysOfWeek),
     @DaysOfWeek);
  RegisterVar(CSClock, 'RightAlignClock', ikBool,
     SizeOf(RightAlignClock), @RightAlignClock); {FY 13-03-2000}
  {SmartPad}
  RegisterVar(CSSmartPad, 'InsertDate', ikBool, SizeOf(SPInsertDate),
     @SPInsertDate);
  RegisterVar(CSSmartPad, 'LineChar', ikUInt, SizeOf(SPLineChar),
     @SPLineChar); {SYR}
  {Game}
  RegisterVar(CSGame, 'EnableGame', ikBool, SizeOf(EnableGame),
     @EnableGame);
  {Clipboard}
  RegisterVar(CSClipboard, 'SaveClipboardOnExit', ikBool,
       SizeOf(cbAutoSave), @CBAutoSave);
  RegisterVar(CSClipboard, 'MaxClipboardSize', ikSInt, SizeOf(cbSize),
     @CBSize);
  {Kernel}
  //RegisterVar(CSKernel,    'UseLFN',               ikBool, SizeOf(CanUseLFN),            @CanUseLFN);
  RegisterVar(CSKernel, 'AutoSave', ikBool, SizeOf(AutoSave), @AutoSave);
  RegisterVar(CSKernel, 'ShowKeyCode', ikUInt, SizeOf(ShowKeyCode),
     @ShowKeyCode);
  RegisterVar(CSKernel, 'CopyLimit', ikSInt, SizeOf(CopyLimit),
     @CopyLimit);
  RegisterVar(CSKernel, 'StoreVideoMode', ikUInt,
     SizeOf(StoreVideoMode), @StoreVideoMode); {PZ}
  {DataCompBoy}
  RegisterVar(CSKernel, 'SmartWindowsBoxClose', ikSInt,
       SizeOf(SmartWindowsBoxClose), @SmartWindowsBoxClose);
  {DataCompBoy}
  RegisterVar(CSKernel, 'CtrlBreakKill', ikBool, SizeOf(CtrlBreakKill),
     @CtrlBreakKill); {AK155}
  //RegisterVar(CSKernel,    'DoVESATest',           ikBool, SizeOf(DoVESATest),           @DoVESATest);           {DataCompBoy}
  RegisterVar(CSKernel, 'ForceDefaultArchiver', ikStr,
       SizeOf(ForceDefaultArchiver), @ForceDefaultArchiver); {JO}
  {$IFDEF WIN32}
  RegisterVar(CSKernel, 'AltGrAsAlt', ikBool,
      SizeOf(AltGrAsAlt), @AltGrAsAlt); {JO}
  {$ENDIF}
  {Editor}
  RegisterVar(CSEditor, 'UnlimitUnindent', ikBool,
     SizeOf(UnlimitUnindent), @UnlimitUnindent);
  RegisterVar(CSEditor, 'DrawRShift', ikBool, SizeOf(DrawRShift),
     @DrawRShift);
  RegisterVar(CSEditor, 'AutoScopeDetect', ikBool,
     SizeOf(AutoScopeDetect), @AutoScopeDetect);
  RegisterVar(CSEditor, 'ShowBookmarks', ikBool, SizeOf(ShowBookmarks),
     @ShowBookmarks);
  RegisterVar(CSEditor, 'FastBookmark', ikBool, SizeOf(FastBookmark),
     @FastBookmark);
  RegisterVar(CSEditor, 'DefCodePageView', ikStr,
   SizeOf(DefCodePageView), @DefCodePageView);
  RegisterVar(CSEditor, 'DefCodePage', ikStr, SizeOf(DefCodePage),
     @DefCodePage);
  RegisterVar(CSEditor, 'FastSearchDeep', ikSInt,
     SizeOf(FastSearchDeep), @FastSearchDeep);
  RegisterVar(CSEditor, 'WinManagerPosToEdit', ikBool,
       SizeOf(WinManagerPosToEdit), @WinManagerPosToEdit);
  RegisterVar(CSEditor, 'AutoBracketPairs', ikStr,
       SizeOf(AutoBracketPairs), @AutoBracketPairs);
  RegisterVar(CSEditor, 'F6_DuplicatesLine', ikBool,
       SizeOf(F6_DuplicatesLine), @F6_DuplicatesLine); {JO}
  {FilePanels}
  RegisterVar(CSFilePanels, 'QuickRenameInDialog', ikBool,
       SizeOf(QuickRenameInDialog), @QuickRenameInDialog);
  RegisterVar(CSFilePanels, 'UpperCaseSorting', ikBool,
       SizeOf(UpperCaseSorting), @UpperCaseSorting); {JO}
  {$IFDEF OS2}
  RegisterVar(CSFilePanels, 'ExecWin32GUI', ikStr, SizeOf(ExecWin32GUI),
     @ExecWin32GUI);
  RegisterVar(CSFilePanels, 'ExecWin32CUI', ikStr, SizeOf(ExecWin32CUI),
     @ExecWin32CUI);
  {$ENDIF}
  {NetInfo}
  RegisterVar(CSNetInfo, 'NoLevelsInfo', ikBool, SizeOf(NoLevelsInfo),
     @NoLevelsInfo);
  {TaskList}
  RegisterVar(CSTaskList, 'KillAllowed', ikBool, SizeOf(KillAllowed),
     @KillAllowed); {JO}
  RegisterVar(CSTaskList, 'ShowExePaths', ikBool, SizeOf(ShowExePaths),
     @ShowExePaths); {JO}
  RegisterVar(CSTaskList, 'FilteredList', ikBool, SizeOf(FilteredList),
     @FilteredList); {JO}
  RegisterVar(CSTaskList, 'UserTaskFilter', ikStr,
     SizeOf(UserTaskFilter), @UserTaskFilter); {JO}
  {Language}
  RegisterVar(CSLanguage, 'ActiveLanguage', ikStr,
     SizeOf(ActiveLanguage), @ActiveLanguage);
  RegisterVar(CSLanguage, 'HelpLanguageOverride', ikStr,
       SizeOf(HelpLanguageOverride), @HelpLanguageOverride);
  RegisterVar(CSLanguage, 'ShowLanguageMenu', ikBool,
       SizeOf(ShowLanguageMenu), @ShowLanguageMenu);
  {Calendar}
  RegisterVar(CSCalendar, 'SundayFirst', ikUInt,
       SizeOf(CalSundayFirst), @CalSundayFirst);
  RegisterVar(CSCalendar, 'OptionFlags', ikSInt,
       SizeOf(CalOptionFlags), @CalOptionFlags);
  {$IFDEF REGEXP}
  {RegExp}
  RegisterVar(CSRegExp, 'RegExpStr0', ikStr, SizeOf(RegExpStr0),
     @RegExpStr0); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr1', ikStr, SizeOf(RegExpStr1),
     @RegExpStr1); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr2', ikStr, SizeOf(RegExpStr2),
     @RegExpStr2); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr3', ikStr, SizeOf(RegExpStr3),
     @RegExpStr3); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr4', ikStr, SizeOf(RegExpStr4),
     @RegExpStr4); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr5', ikStr, SizeOf(RegExpStr5),
     @RegExpStr5); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr6', ikStr, SizeOf(RegExpStr6),
     @RegExpStr6); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr7', ikStr, SizeOf(RegExpStr7),
     @RegExpStr7); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr8', ikStr, SizeOf(RegExpStr8),
     @RegExpStr8); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr9', ikStr, SizeOf(RegExpStr9),
     @RegExpStr9); {PZ}
  {$ENDIF}
  {QuickDirs}
  RegisterVar(CSQuickDirs, 'QDirs1', ikStr, SizeOf(QDirs1), @QDirs1);
  RegisterVar(CSQuickDirs, 'QDirs2', ikStr, SizeOf(QDirs2), @QDirs2);
  RegisterVar(CSQuickDirs, 'QDirs3', ikStr, SizeOf(QDirs3), @QDirs3);
  RegisterVar(CSQuickDirs, 'QDirs4', ikStr, SizeOf(QDirs4), @QDirs4);
  RegisterVar(CSQuickDirs, 'QDirs5', ikStr, SizeOf(QDirs5), @QDirs5);
  RegisterVar(CSQuickDirs, 'QDirs6', ikStr, SizeOf(QDirs6), @QDirs6);
  RegisterVar(CSQuickDirs, 'QDirs7', ikStr, SizeOf(QDirs7), @QDirs7);
  RegisterVar(CSQuickDirs, 'QDirs8', ikStr, SizeOf(QDirs8), @QDirs8);
  RegisterVar(CSQuickDirs, 'QDirs9', ikStr, SizeOf(QDirs9), @QDirs9);
  {SetupStorage}
  RegisterVar(CSSetupStorage, 'SystemDataOpt', ikSInt,
       SizeOf(SystemDataOpt), @SystemDataOpt);
  RegisterVar(CSSetupStorage, 'InterfaceDataOpt', ikSInt,
       SizeOf(InterfaceDataOpt), @InterfaceDataOpt);
  RegisterVar(CSSetupStorage, 'FMSetupOpt', ikSInt, SizeOf(FMSetupOpt),
     @FMSetupOpt);
  RegisterVar(CSSetupStorage, 'EditorDefaultsOpt', ikSInt,
       SizeOf(EditorDefaultsOpt), @EditorDefaultsOpt);
  RegisterVar(CSSetupStorage, 'EditorDefaultsOpt2', ikSInt,
       SizeOf(EditorDefaultsOpt2), @EditorDefaultsOpt2);
  RegisterVar(CSSetupStorage, 'ViewerOpt', ikSInt, SizeOf(ViewerOpt),
     @ViewerOpt);
  RegisterVar(CSSetupStorage, 'StartupDataLoad', ikSInt,
       SizeOf(StartupDataLoad), @StartupDataLoad);
  RegisterVar(CSSetupStorage, 'StartupDataUnload', ikSInt,
       SizeOf(StartupDataUnload), @StartupDataUnload);
  RegisterVar(CSSetupStorage, 'ConfirmsOpt', ikSInt,
     SizeOf(ConfirmsOpt), @ConfirmsOpt);
  RegisterVar(CSSetupStorage, 'NonVIOScreenMode', ikSInt,
       SizeOf(NonVIOScreenMode), @NonVIOScreenMode);
  RegisterVar(CSSetupStorage, 'VIOScreenMode', ikSInt,
       SizeOf(VIOScreenMode), @VIOScreenMode);
  end { Proceed };

function IniVarToStr(Kind: TIniItemKind; Size: Word; PVar: Pointer)
  : String;
  var
    b: Boolean;
    i: Integer;
  begin
  Result := '';

  case Kind of

    ikBool:
      begin
      case Size of
        1:
          b := Boolean(PVar^);
        2:
          b := Wordbool(PVar^);
        4:
          b := Longbool(PVar^);
        else {case}
          Exit;
      end {case};
      if b then
        Result := '1'
      else
        Result := '0';
      end;

    ikEnum:
      begin
      if Size <= High(TUInt8) then
        Str(TUInt8(PVar^), Result)
      else
        Str(TUInt16(PVar^), Result);
      end;

    ikStr:
      begin
      Result := Copy(String(PVar^), 1, Size-1);
      end;

    ikSInt:
      begin
      case Size of
        1:
          Str(TSInt8(PVar^), Result);
        2:
          Str(TSInt16(PVar^), Result);
        4:
          Str(TSInt32(PVar^), Result);
      end {case};
      end;

    ikUInt:
      begin
      case Size of
        1:
          Str(TUInt8(PVar^), Result);
        2:
          Str(TUInt16(PVar^), Result);
      end {case};
      end;

    ikChar:
      begin
      Move(PVar^, Result[1], Size);
      SetLength(Result, Size) {result[0] := Chr(size);}
      end;

  end {case};
  end { IniVarToStr };

function IniStrToVar(Text: String; Kind: TIniItemKind; Size: Word;
     PVar: Pointer): Boolean;
  var
    i: Integer;
    buffer: array[0..3] of Byte;
    b: Boolean absolute buffer;
    i32: TSInt32 absolute buffer;
    i16: TSInt16 absolute buffer;
    i8: TSInt8 absolute buffer;
    u16: TUInt16 absolute buffer;
    u8: TUInt8 absolute buffer;
  begin
  IniStrToVar := False;
  case Kind of

    ikBool:
      begin
      UpStr(Text);
      if  (Text = '1') or (Text = 'TRUE') or (Text = 'YES')
           or (Text = 'ON')
      then
        b := True
      else if (Text = '0') or (Text = 'FALSE') or (Text = 'NO')
           or (Text = 'OFF')
      then
        b := False
      else
        Exit;
      IniStrToVar := True;
      case Size of
        1:
          Boolean(PVar^) := b;
        2:
          Wordbool(PVar^) := b;
        4:
          Longbool(PVar^) := b;
      end {case};
      end;

    ikEnum:
      begin
      if Size <= High(u8) then
        begin
        Val(Text, u8, i);
        if  (i = 0) and (u8 <= Size) then
          begin
          IniStrToVar := True;
          Move(buffer, PVar^, 1);
          end;
        end
      else
        begin
        Val(Text, u16, i);
        if  (i = 0) and (u16 <= Size) then
          begin
          IniStrToVar := True;
          Move(buffer, PVar^, 2);
          end;
        end;
      end;

    ikStr:
      begin
      String(PVar^) := Copy(Text, 1, Size-1);
      IniStrToVar := Length(Text) < Size;
      end;

    ikSInt:
      begin
      case Size of
        1:
          Val(Text, i8, i);
        2:
          Val(Text, i16, i);
        4:
          Val(Text, i32, i);
      end {case};
      if i = 0 then
        begin
        IniStrToVar := True;
        Move(buffer, PVar^, Size);
        end;
      end;

    ikUInt:
      begin
      case Size of
        1:
          Val(Text, u8, i);
        2:
          Val(Text, u16, i);
      end {case};
      if i = 0 then
        begin
        IniStrToVar := True;
        Move(buffer, PVar^, Size);
        end;
      end;

    ikChar:
      begin
      if Text = '' then
        Exit;
      i := Size;
      if Length(Text) < Size then
        begin
        IniStrToVar := True;
        i := Length(Text);
        end;
      Move(Text[1], PVar^, i);
      if i < Size then
        FillChar(PChar(PVar)[i], Size-i, 0);
      end;

  end {case};
  end { IniStrToVar };

procedure Loader(Group, Parameter: PChar; Kind: TIniItemKind;
     Size: Word; PVar: Pointer);
  var
    s: String;
    b: Byte;
  begin
  s := IniVarToStr(Kind, Size, PVar)+#0;
  s[0] := Chr(GetPrivateProfileString(
        Group,
        Parameter,
        @s[1],
        @s[1],
        255,
        @FreeStr[1]
        ));
  if not IniStrToVar(s, Kind, Size, PVar) then
    begin
    b := 0;
    if  (Kind = ikStr) or (Kind = ikChar) then
      b := Length(s)+1-Size;
    AddIniError(b, Group, Parameter);
    end;
  end { Loader };

const
  SaveVar: Pointer = nil;

procedure Saver(Group, Parameter: PChar; Kind: TIniItemKind; Size: Word;
     PVar: Pointer);
  var
    s: String;
    b: array[0..255] of Char;
  begin
  if  (SaveVar <> nil) and (SaveVar <> PVar) then
    Exit;
  s := IniVarToStr(Kind, Size, PVar)+#0;
  { This code is provided only to not cut longer string if only a part }
  { was read in Loader procedure.                                      }
  if  ( (Kind = ikStr) and (Length(s) = (Size-1)))
    or ((Kind = ikChar) and (Length(s) = Size))
  then
    begin
    FillChar(b, SizeOf(b), 0);
    GetPrivateProfileString(
      Group,
      Parameter,
      @b[255], { default = empty string (null-terminated) }
      @b[0],
      255,
      @FreeStr[1]
      );
    if StrLComp(@b, @s[1], Length(s)) = 0 then
      Exit;
    end;
  StrPCopy(b, s);
  WritePrivateProfileString(
    Group,
    Parameter,
    @b[0],
    @FreeStr[1]
    );
  end { Saver };

procedure LoadDnIniSettings;
  begin
  FreeStr := DnIniFileName+#0;
  Proceed(Loader);
  CloseProfile;
  end;

procedure SaveDnIniSettings(PVar: Pointer);
  begin
  SaveVar := PVar;
  FreeStr := DnIniFileName+#0;
  Proceed(Saver);
  CloseProfile;
  end;

procedure DoneIniEngine;
  begin
  CloseProfile;
  INIModified := True;
  end;

{-DataCompBoy-}
function ProbeINI(var INItime, INIsize: LongInt): Boolean;
  var
    SR: lSearchRec;
    Pos: LongInt;
    S: Word;
  begin
  INItime := 0;
  INIsize := 0;
  lFindFirst(DnIniFileName, AnyFileDir, SR);
  if  (DosError = 0) and ((SR.SR.Attr and Directory) = 0) then
    begin
    ProbeINI := True;
    INItime := SR.SR.Time;
    INIsize := 0; {AK155 на случай DPMI, где Size: longint}
    Move(SR.SR.Size, INIsize, SizeOf(INIsize)); {AK155 Size: comp}
    end
  else
    ProbeINI := False;
  lFindClose(SR);
  end { ProbeINI };
{-DataCompBoy-}

{JO: 21-04-2005 }
procedure WriteIniCache(INItime, INIsize: LongInt);
  var
    S: TBufStream;
  begin
  if INIModified then
    begin
    INIModified := False;
    S.Init(SourceDir+'dnini.in_', stCreate, 8192);
    if S.Status <> stOK then
      begin
      S.Done;
      Exit;
      end;
    S.Write(IniCacheSign, SizeOf(IniCacheSign));
    S.Write(INItime, SizeOf(INItime));
    S.Write(INIsize, SizeOf(INIsize));
    S.Write(iniparamblock_START,
         Ofs(iniparamblock_END)-Ofs(iniparamblock_START));
    S.Done;
    end;
  end;

function ReadIniCache(INItime, INIsize: LongInt): Boolean;
  var
    S: TBufStream;
    I: LongInt;
    Sign: array[1..60] of Char;
  begin
  Result := False;
  S.Init(SourceDir+'dnini.in_', stOpenRead, 8192);
  if  (S.Status <> stOK) or (S.GetSize = 0) then
    begin
    S.Done;
    Exit;
    end;
  S.Read(Sign, SizeOf(Sign));
  if Sign <> IniCacheSign then begin S.Done; Exit; end;
  S.Read(I, SizeOf(I));
  if I <> INItime then begin S.Done; Exit; end;
  S.Read(I, SizeOf(I));
  if I <> INIsize then begin S.Done; Exit; end;
  if (S.GetSize - S.GetPos) <>
      (Ofs(iniparamblock_END)-Ofs(iniparamblock_START)) then
    begin
    S.Done;
    Exit;
    end;
  S.Read(iniparamblock_START,
         Ofs(iniparamblock_END)-Ofs(iniparamblock_START));
  if S.Status = stOK then
    Result := True;
  S.Done;
  end;
{/JO}
end.
