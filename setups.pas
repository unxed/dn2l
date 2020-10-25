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

unit Setups;

interface

uses
  Defines, Drivers, Views, Dialogs, Collect,
  Commands, Startup, Startupp
  ;

{                                System Setup                                }
{----------------------------------------------------------------------------}
type
  TSysData = record
    Options: Word;
    Mode1: String[5];
    Mode2: String[5];
    Temp: String;
    Drives: TTextListboxRec;
    Current: Word;
    CopyLimitBuf: String[5];
    ForceDefArch: String[3];
    end;

  PSysDialog = ^TSysDialog;
  TSysDialog = object(TDialog)
    LocalData: TSystemData;
    SysData: TSysData;
    {constructor Init;}
    procedure Awaken; virtual;
    destructor Done; virtual;
    procedure GetData(var Rec); virtual;
    end;

  PCurrDriveInfo = ^TCurrDriveInfo;
  TCurrDriveInfo = object(TCheckBoxes)
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Press(Item: Integer); virtual;
    end;

  PMouseBar = ^TMouseBar;
  TMouseBar = object(TScrollBar)
    constructor Init(var Bounds: TRect);
    procedure SetData(var Rec); virtual;
    procedure GetData(var Rec); virtual;
    function DataSize: Word; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  {$IFDEF SS}
  PSaversDialog = ^TSaversDialog;
  TSaversDialog = object(TDialog)
    constructor Init;
    procedure HandleEvent(var Event: TEvent); virtual;
    destructor Done; virtual;
    procedure Awaken; virtual;
    end;

  PSaversListBox = ^TSaversListBox;
  TSaversListBox = object(TListBox)
    procedure HandleEvent(var Event: TEvent); virtual;
    end;
  {$ENDIF}

procedure SetupCountryInfo;
  {` Диалог настоек страны `}
function ApplyCodetables: Integer;
  {` Применить настройки кодировок из CountryInfo. Результат:
     0 - нет ошибок,
     1 - ошибка в KbdToggleLayout
     2 - ошибка в ABCSortTable,
     3 - ошибка в WinCodeTable,
     4 - ошибка в Codetables,
     `}
procedure DoFMSetup;
procedure DriveInfoSetup;
procedure SetupEditorDefaults;
procedure SystemSetup;
procedure InterfaceSetup;
procedure StartupSetup;
procedure MouseSetup;
procedure ConfirmSetup;
function TerminalSetup: Boolean;
{$IFDEF SS}
procedure SaversSetup;
function MakeSaversDialog: PDialog;
{$ENDIF}

const
  CodeErrMessage: array[1..4] of TStrIdx =
 {`AK155 12.01.2004 Собщения об ошибках перекодировочных настроек.}
   (dlLayoutErr, dlSortError, dlWinErr, dlCodetablesErr);
   {`}

implementation
uses
  Dos, Tree, Drives, Advance, Advance1, Advance2, Messages, DNHelp,
  Advance6, DnIni, DnInip, Country, UKeyMap, fnotify
  , lfn, DNApp, Validate
  ;

procedure ConfirmSetup;
  var
    D: Word;
  begin
  D := ExecResource(dlgConfirmations, Confirms);
  if D <> cmOK then
    Exit;
  ConfigModified := True;

  ConfirmsOpt := Confirms;
  SaveDnIniSettings(@ConfirmsOpt);
  DoneIniEngine;
  end;

function TerminalSetup;
  begin
  TerminalSetup := False;
  if ExecResource(dlgSetupTerminal, TerminalDefaults) <> cmOK then
    Exit;
  TerminalSetup := True;
  Message(Application, evCommand, cmUpdateConfig, nil);
  end;

procedure SystemSetup;
  var
    W: Word;
    D: PDialog;
    B: Boolean;
    Data: TSysData;
    i: Char;
  begin
  OpenResource;
  if Resource = nil then
    Exit;
  D := PDialog
            (Application^.ValidView(PDialog(Resource^.Get(dlgSystemSetup))
        ));
  if D = nil then
    Exit;
  W := Desktop^.ExecView(D);
  if W <> cmCancel then
    begin
    D^.GetData(Data);
    SystemData := PSysDialog(D)^.LocalData;
    Message(Application, evCommand, cmUpdateConfig, nil);
    end;
  Dispose(D, Done);
  SystemDataOpt := SystemData.Options;
  CopyLimit := SystemData.CopyLimitBuf;
  ForceDefaultArchiver := SystemData.ForceDefArch;

  SaveDnIniSettings(@SystemDataOpt);
  SaveDnIniSettings(@CopyLimit);
  SaveDnIniSettings(@ForceDefaultArchiver);
  DoneIniEngine;
  end { SystemSetup };

procedure InterfaceSetup;
  var
    AltTab: Boolean;
    R: TRect;
  begin
  with PApplication(Application)^ do
    if ExecResource(dlgInterfaceSetup, InterfaceData) <> cmCancel then
      begin
      GetExtent(R);
      if InterfaceData.Options and ouiHideMenu = 0 then
        Inc(R.A.Y);
      if InterfaceData.Options and ouiHideStatus = 0 then
        Dec(R.B.Y);
      if InterfaceData.Options and ouiHideCmdline = 0 then
        Dec(R.B.Y);
      Desktop^.Locate(R);
      R.A.Y := R.B.Y;
      R.B.Y := R.A.Y+Byte(InterfaceData.Options and ouiHideCmdline = 0);
      CommandLine^.Locate(R);
      CommandLine^.SetState(sfVisible, InterfaceData.Options and
         ouiHideCmdline = 0);
      Message(Application, evCommand, cmUpdateConfig, nil);
      if InterfaceData.Options and ouiClock <> 0 then
        if not Clock^.GetState(sfVisible) then
          Clock^.Show;
      if InterfaceData.Options and ouiClock = 0 then
        if Clock^.GetState(sfVisible) then
          Clock^.Hide;
      end;
  InterfaceDataOpt := InterfaceData.Options;
  SaveDnIniSettings(@InterfaceDataOpt);
  DoneIniEngine;
  end { InterfaceSetup };

procedure StartupSetup;
  var
    Data: record
      Load, Unload: Word;
      end;
  begin
  Data.Load := StartupData.Load;
  Data.Unload := StartupData.Unload;
  if ExecResource(dlgStartupSetup, Data) <> cmCancel then
    begin
    StartupData.Load := Data.Load;
    StartupData.Unload := Data.Unload;
    LSliceCnt := -3;
    Message(Application, evCommand, cmUpdateConfig, nil);

    StartupDataLoad := StartupData.Load;
    StartupDataUnload := StartupData.Unload;
    SaveDnIniSettings(@StartupDataLoad);
    SaveDnIniSettings(@StartupDataUnload);
    DoneIniEngine;
    end;
  end { StartupSetup };

procedure MouseSetup;
  begin
  if ExecResource(dlgMouseSetup, MouseData) <> cmOK then
    Exit;
  if MouseVisible xor (MouseData.Options and omsCursor <> 0) then
    begin
    DoneEvents;
    InitEvents;
    end;
  MouseReverse := MouseData.Options and omsReverse <> 0;
  SetMouseSpeed(MouseData.HSense, MouseData.VSense);
  Message(Application, evCommand, cmUpdateConfig, nil);
  end;

{$IFDEF SS}
procedure SaversSetup;
  var
    W: Word;
    D: PDialog;
    B: Boolean;
  begin
  OpenResource;
  if Resource = nil then
    Exit;
  D := PDialog
            (Application^.ValidView(PDialog(Resource^.Get(dlgSaversSetup))
        ));
  if D = nil then
    Exit;
  W := Desktop^.ExecView(D);
  if W <> cmCancel then
    begin
    D^.GetData(SaversData);
    Message(Application, evCommand, cmUpdateConfig, nil);
    end;
  Dispose(D, Done);
  end;
{$ENDIF}

function ApplyCodetables: Integer;
  var
    CP: Word;
    Err: Integer;
  begin
  with CountryInfo do
    begin
    {$IFDEF Win32}
    if WinCodetable = '' then
      WinCodetable := '0'{CP_ACP};
    {$ENDIF}
    if not BuildLayoutConvXlat(KbdToggleLayout) then
      begin
      Result := 1;
      Exit;
      end;
    if ABCSortTable = '' then
      ABCSortTable := '0';
    Val(ABCSortTable, CP, Err);
    if (Err = 0) and QueryABCSort(CP, ABCSortXlat) then
      begin { запрос таблицы у ОС удовлетворён }
      end
    else if not BuildABCSortXlat(ABCSortTable) then
      begin
      Result := 2;
      Exit;
      end;

    FreeCodetables;
    if (WinCodetable <> '') and not BuildWinCodeTable(WinCodeTable)
    then
      begin
      Result := 3;
      Exit;
      end;
    if not InitCodeTables(CodeTables) then
      begin
      Result := 4;
      Exit;
      end;
    end;
  Result := 0;
  end;

procedure SetupCountryInfo;
  var
    SaveCountryInfo: TCountryInfo;
    C: Word;
    Err: Integer;
  label
    TryDialog;
  begin
  SaveCountryInfo := CountryInfo;
TryDialog:
  while True do
    begin
    C := ExecResource(dlgCountrySetup, CountryInfo);
    if C = cmYes then
      GetSysCountryInfo
    else
      Break;
    end;
  if C <> cmOK then
    begin
    CountryInfo := SaveCountryInfo;
     // CountryInfo мог измениться по cmYes
    ApplyCodetables;
    Exit;
    end;
  Err := ApplyCodetables;
  if Err <> 0 then
    begin
    MessageBox(^C+GetString(CodeErrMessage[Err]), nil, mfError+mfYesButton);
    goto TryDialog;
    end;
  GlobalMessage(evCommand, cmReboundPanel, nil);
  ConfigModified := True;
  end;

procedure DoFMSetup;
  begin
  if ExecResource(dlgFMSetup, Startup.FMSetup) <> cmOK then
    Exit;
  if Startup.FMSetup.TagChar = '' then
    Startup.FMSetup.TagChar[1] := ' ';
  if (Startup.FMSetup.RestChar = '') or
     (Startup.FMSetup.RestChar[1] = ' ')
  then
    Startup.FMSetup.RestChar := #16; { символа обрезки не может не быть }
  Message(Application, evCommand, cmUpdateConfig, nil);
  GlobalMessage(evCommand, cmReboundPanel, nil);

  FMSetupOpt := Startup.FMSetup.Options;
  SaveDnIniSettings(@FMSetupOpt);
  DoneIniEngine;
  if AutoRefreshPanels <>
     (Startup.FMSetup.Options and fmoAutorefreshPanels <> 0)
  then
    begin
    AutoRefreshPanels := not AutoRefreshPanels;
    if AutoRefreshPanels then
      NotifyInit
    else
      NotifyDone;
    end;
  end;

procedure DriveInfoSetup;
  var
    W: Word;
  begin
  Startup.DriveInfoData := Startup.DriveInfoData and $001F+
    Startup.DriveInfoData and $07E0 shl 2+
    Startup.DriveInfoData and $1800 shr 6;

  if ExecResource(dlgDriveInfoSetup, Startup.DriveInfoData) <> cmOK then
    Exit;
  Startup.DriveInfoData := Startup.DriveInfoData and $001F+
    Startup.DriveInfoData and $FF80 shr 2+
    Startup.DriveInfoData and $0060 shl 6;
  Message(Application, evCommand, cmUpdateConfig, nil);
  GlobalMessage(evCommand, cmReboundPanel, nil);
  end;

procedure SetupEditorDefaults;
  begin
  if ExecResource(dlgEditorDefaults, EditorDefaults) = cmOK then
    begin
    if StoI(EditorDefaults.TabSize) < 2 then
      EditorDefaults.TabSize := '2'; {-$VOL}
    Message(Application, evCommand, cmUpdateConfig, nil);
    EditorDefaultsOpt := EditorDefaults.EdOpt;
    EditorDefaultsOpt2 := EditorDefaults.EdOpt2;
    ViewerOpt := EditorDefaults.ViOpt;
    SaveDnIniSettings(@EditorDefaultsOpt);
    SaveDnIniSettings(@EditorDefaultsOpt2);
    SaveDnIniSettings(@ViewerOpt);
    DoneIniEngine;
    end;
  end;

procedure TCurrDriveInfo.HandleEvent;
  var
    W: Word;
    Data: TSysData;
  begin
  inherited HandleEvent(Event);
  if  (Event.What = evBroadcast)
       and (Event.Command = cmScrollBarChanged)
  then
    begin
    W := PSysDialog(Owner)^.LocalData.Drives[Char
          (Byte('A')+PScrollBar(Event.InfoPtr)^.Value)];
    SetData(W);
    end
  else if (Event.What = evKeyDown) and (Event.CharCode = ' ')
         and (TypeOf(Owner^.Current^) = TypeOf(TListBox))
  then
    Press(0);
  end;

procedure TCurrDriveInfo.Press;
  var
    Data: TSysData;
  begin
  inherited Press(Item);
  Owner^.GetData(Data);
  PSysDialog(Owner)^.LocalData.Drives[Char(Byte('A')+Data.Drives.Focus)
  ] := Value;
  end;

procedure TSysDialog.Awaken;
  var
    C: Char;
  begin
  LocalData := SystemData;
  New(SysData.Drives.List, Init(26, 1, False));
  for C := 'A' to 'Z' do
    SysData.Drives.List^.Insert(NewStr(C+':'));
  Move(SystemData, SysData,
     SizeOf(SysData.Options)+SizeOf(SysData.Mode1)*2);
  SysData.Temp := SystemData.Temp;
  SysData.Drives.Focus := 2;
  SysData.Current := LocalData.Drives['C'];
  SysData.CopyLimitBuf := ItoS(SystemData.CopyLimitBuf);
  SysData.ForceDefArch := SystemData.ForceDefArch;
  SetData(SysData);
  end;

destructor TSysDialog.Done;
  var
    Data: TSysData;
  begin
  GetData(Data);
  inherited Done;
  Dispose(Data.Drives.List, Done);
  end;

procedure TSysDialog.GetData;
  var
    Data: TSysData;
  begin
  inherited GetData(Data);
  TSysData(Rec) := Data;
  LocalData.Options := Data.Options;
  LocalData.Mode1 := Data.Mode1;
  LocalData.Mode2 := Data.Mode2;
  LocalData.Temp := Data.Temp;
  LocalData.CopyLimitBuf := StoI(Data.CopyLimitBuf);
  LocalData.ForceDefArch := Data.ForceDefArch;
  end;

{----------------------------------------------------------------------------}
{                                 Mouse Setup                                }
{----------------------------------------------------------------------------}
constructor TMouseBar.Init;
  begin
  inherited Init(Bounds);
  Options := Options or ofSelectable;
  SetParams(0, 0, Size.X-1, 3, 1);
  end;

const
  HSenseY = 3;

function TMouseBar.DataSize;
  assembler;
asm mov eax,4 end;

procedure TMouseBar.SetData;
  begin
  SetValue(Integer(Rec))
  end;

procedure TMouseBar.GetData;
  begin
  Integer(Rec) := Value
  end;

procedure TMouseBar.HandleEvent;
  begin
  inherited HandleEvent(Event);
  end;

{$IFDEF SS}
{----------------------------------------------------------------------------}
{                                Savers Setup                                }
{----------------------------------------------------------------------------}
procedure TSaversListBox.HandleEvent;
  var
    PS: PString;
    F: Integer;
    A, S: PCollection;
    LocalData: TSaversData;
  function SeekStr(P: PString): Boolean;
    begin
    SeekStr := (P <> nil) and (P^ = PS^);
    end;
  begin
  if Event.What = evBroadcast then
    case Event.Command of
      cmYes:
        begin
        Owner^.GetData(LocalData);
        A := LocalData.Available.List;
        if A^.Count > 0 then
          begin
          PS := A^.At(LocalData.Available.Focus);
          if  (PS <> nil) and (List^.FirstThat(@SeekStr) = nil) then
            begin
            List^.Insert(NewStr(PS^));
            S := List;
            List := nil;
            NewLisT(S);
            end;
          end;
        ClearEvent(Event);
        end;
      cmNo:
        begin
        F := Focused;
        if F < List^.Count then
          begin
          S := List;
          S^.AtFree(F);
          List := nil;
          Owner^.Lock;
          NewLisT(S);
          if  (F > 0) and (F >= List^.Count) then
            Dec(F);
          FocusItem(F);
          Owner^.UnLock;
          end;
        ClearEvent(Event);
        end;
    end {case};
  inherited HandleEvent(Event);
  end { TSaversListBox.HandleEvent };

constructor TSaversDialog.Init;
  var
    R: TRect;
    D: PDialog;
    Control, Labl, Histry: PView;
  begin
  R.Assign(0, 0, 57, 20);
  inherited Init(R, GetString(dlScreenSaverSetup));
  Options := Options or ofCentered or ofValidate;
  HelpCtx := hcSavers;
  R.Assign(19, 3, 20, 13);
  Control := New(PScrollBar, Init(R));
  Insert(Control);

  R.Assign(2, 3, 19, 13);
  Control := New(PSaversListBox, Init(R, 1, PScrollBar(Control)));
  Insert(Control);

  R.Assign(2, 2, 18, 3);
  Labl := New(PLabel, Init(R, GetString(dlSS_S_electedSavers), Control));
  Insert(Labl);

  R.Assign(20, 6, 36, 8);
  Control := New(PButton, Init(R, GetString(dlSS_A_dd), cmYes,
         bfNormal+bfBroadcast));
  Insert(Control);

  R.Assign(20, 8, 36, 10);
  Control := New(PButton, Init(R, GetString(dlSS_R_emove), cmNo,
         bfNormal+bfBroadcast));
  Insert(Control);

  R.Assign(54, 3, 55, 13);
  Control := New(PScrollBar, Init(R));
  Insert(Control);

  R.Assign(37, 3, 54, 13);
  Control := New(PListBox, Init(R, 1, PScrollBar(Control)));
  Insert(Control);

  R.Assign(37, 2, 54, 3);
  Labl := New(PLabel, Init(R, GetString(dlSSA_v_ailableSavers), Control));
  Insert(Labl);

  R.Assign(2, 15, 18, 16);
  Control := New(PInputline, Init(R, 3));
  PInputline(Control)^.SetValidator(New(PRangeValidator, Init(1, 254)));
  { X-Man }
  Control^.Options := Control^.Options or ofValidate;
  Insert(Control);

  R.Assign(2, 14, 18, 15);
  Labl := New(PLabel, Init(R, GetString(dlSS_T_ime), Control));
  Insert(Labl);

  R.Assign(20, 15, 55, 16);
  Control := New(PCheckBoxes, Init(R,
        NewSItem(GetString(dlSSUse_M_ouse), nil)));
  Insert(Control);

  R.Assign(7, 17, 17, 19);
  Control := New(PButton, Init(R, GetString(dlOKButton), cmOK, bfDefault));
  Insert(Control);

  R.Assign(17, 17, 28, 19);
  Control := New(PButton, Init(R, GetString(dlCancelButton), cmCancel,
         bfNormal));
  Insert(Control);

  R.Assign(28, 17, 40, 19);
  Control := New(PButton, Init(R, GetString(dlHelpButton), cmHelp,
         bfNormal));
  Insert(Control);

  R.Assign(40, 17, 50, 19);
  Control := New(PButton, Init(R, GetString(dlTestButton), cmTest,
         bfNormal));
  Insert(Control);

  SelectNext(False);
  end { TSaversDialog.Init };

procedure TSaversDialog.HandleEvent(var Event: TEvent);
  var
    Data: TSaversData;
  begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      case Event.Command of
        cmTest:
          begin
          ClearEvent(Event);
          GetData(Data);
          Application^.InsertAvIdlerN(Data, Data.Available.Focus);
          end;
      end {case};
  end {case};
  end;

{-DataCompBoy-}
procedure TSaversDialog.Awaken;
  var
    lSR: lSearchRec;
    Data: TSaversData;
  begin
  Data := SaversData;
  Data.Available.Focus := 0;
  Data.Selected.Focus := 0;
  New(Data.Available.List, Init(5, 5, False));
  if Data.Selected.List = nil
  then
    New(Data.Selected.List, Init(5, 5, False));
  with Data.Available.List^ do
    begin
    Insert(NewStr(#249' Star flight'));
    Insert(NewStr(#249' Flash-light'));
    Insert(NewStr(#249' Clock'));
    Insert(NewStr(#249' Blackness'));
    lFindFirst(SourceDir+'SSAVERS\*.SS', AnyFileDir, lSR);
    while DosError = 0 do
      begin
      Insert(NewStr(lSR.FullName));
      lFindNext(lSR);
      end;
    lFindClose(lSR);
    end;
  SetData(Data);
  end { TSaversDialog.Awaken };
{-DataCompBoy-}

destructor TSaversDialog.Done;
  var
    Data: TSaversData;
  begin
  GetData(Data);
  inherited Done;
  if  (Data.Available.List <> nil) then
    Dispose(Data.Available.List, Done);
  end;

function MakeSaversDialog: PDialog;
  begin
  MakeSaversDialog := New(PSaversDialog, Init);
  end;
{$ENDIF}

end.

