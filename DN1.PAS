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
{.$DEFINE GRABPalette}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

unit DN1;

interface

procedure InvalidateTempDir;
procedure UpdateConfig;
procedure DoStartup;

procedure RUN_IT;
{$IFDEF LINEPOSIT}
procedure Error(const FileName: String; LineNo, Addr, Code: LongInt);
{$ENDIF}

implementation
uses
  {$IFDEF DEBUGMEM}DebugMem, {$ENDIF} {Cat}
  {$IFDEF OS2}Os2Def, Os2Base, {AK155 for killer} {$ENDIF}
  {$IFDEF WIN95_HIGHPRIORITY}Windows, {Cat for SetPriority} {$ENDIF}
  Advance, Advance1, Advance2, Advance3, Advance4,
  Startup, Startupp, Defines, Streams,
  Setups, DNUtil, Drivers, Commands, DNApp, Messages, Lfn, Dos, FlPanelX,
  UserMenu, CmdLine, FilesCol, Views, ArcView, FileFind,
  DnIni, DnIni_p, CopyIni, Archiver,
  U_MyApp, Microed, ArchSet, Advance6, RegAll, DnExec, Histries, Menus,
   VideoMan, Events,
  fnotify,
  {$IFNDEF DPMI32}Killer, {$ENDIF}
  {$IFDEF DPMI32} lfnvp, Dpmi32, Dpmi32df, {$ENDIF}
  Tree
  , filetype, PDSetup
  ;

{AK155 Мало проверить, что имя временного каталога непусто, надо
еще проверить, что он существует, и что в нем можно создавать и
уничтожать файлы. Побочным эффектом этой функции является обязательное
наличие '\' в конце s }
function BadTemp(var s: String): Boolean;
  var
    f: file;
  begin
  BadTemp := True;
  if  (s = '') then
    Exit;
  if not (s[Length(s)] in ['\', '/']) then
    s := s+'\';
  ClrIO;
  if not PathExist(s) then
    Exit;
  Assign(f, s+'$DNTEST.SWP');
  Rewrite(f);
  if IOResult = 0 then
    begin
    Close(f);
    Erase(f);
    { Под Win NT бывает и так, что создать файл можно, а удалить - нет}
    if IOResult = 0 then
      BadTemp := False;
    end;
  end { BadTemp };

{-DataCompBoy-}
procedure InvalidateTempDir;
  var
    I: Integer;
  begin
  NoTempDir := False;
  TempDir := SystemData.Temp;
  I := PosChar('%', TempDir);
  if I > 0 then
    begin
    Delete(TempDir, 1, I);
    I := PosChar('%', TempDir);
    if I > 0 then
      Delete(TempDir, I, 255);
    TempDir := GetEnv(TempDir);
    end;
  if not BadTemp(TempDir) then
    Exit;
  TempDir := GetEnv('TEMP');
  if not BadTemp(TempDir) then
    Exit;
  TempDir := GetEnv('TMP');
  if not BadTemp(TempDir) then
    Exit;
  TempDir := SourceDir;
  if not BadTemp(TempDir) then
    begin
    TempDir := TempDir+'TEMP';
    MkDir(Copy(TempDir, 1, Length(TempDir)-1));
    MakeSlash(TempDir);
    ClrIO;
    if not BadTemp(TempDir) then
      Exit;
    end;
  NoTempDir := True;
  end { InvalidateTempDir };
{-DataCompBoy-}

procedure UpdateConfig;
  var
    OldSecurity: Boolean;
    TempInteger: Integer; {DataCompBoy}
    R: TRect;
    const
      NotAPath: Char = #22;
  begin
  InvalidateTempDir;
  OldSecurity := Security;
  smSVGALo := StoI(SystemData.Mode1);
  smSVGAHi := StoI(SystemData.Mode2);
  if smSVGALo = 0 then
    smSVGALo := sm80x25;
  if smSVGAHi = 0 then
    smSVGAHi := sm80x25;
  SystemData.Mode1 := ItoS(smSVGALo);
  SystemData.Mode2 := ItoS(smSVGAHi);
  {$IFDEF SS}Val(SaversData.Time, SkyDelay, TempInteger); {$ENDIF}
  if SkyDelay = 0 then
    SkyDelay := 255; {DataCompBoy}

  {TempDir := SystemData.Temp;}

  MouseReverse := MouseData.Options and omsReverse <> 0;
  Security := Startup.FMSetup.Show and fmsShowHidden = 0;

  if OldSecurity xor Security then
    begin
    if Application <> nil then
      GlobalMessage(evCommand, cmPanelReread, @NotAPath);
    end;

  SetBlink(CurrentBlink);
  end { UpdateConfig };

{-DataCompBoy-}
procedure DoStartup;
  var
    SavePos, SPos1: LongInt;
    {JO}
  procedure ReadHighlite;
    var
      F: PTextReader;
    begin
    FileMode := $40;
    F := New(PTextReader, Init(SourceDir+'dnhgl.grp'));
    if F = nil then
      Exit;
    if not F^.Eof then
      CustomMask1 := F^.GetStr;
    if not F^.Eof then
      CustomMask2 := F^.GetStr;
    if not F^.Eof then
      CustomMask3 := F^.GetStr;
    if not F^.Eof then
      CustomMask4 := F^.GetStr;
    if not F^.Eof then
      CustomMask5 := F^.GetStr;
    if not F^.Eof then
      CustomMask6 := F^.GetStr;
    if not F^.Eof then
      CustomMask7 := F^.GetStr;
    if not F^.Eof then
      CustomMask8 := F^.GetStr;
    if not F^.Eof then
      CustomMask9 := F^.GetStr;
    if not F^.Eof then
      CustomMask10 := F^.GetStr;
    if not F^.Eof then
      Archives := F^.GetStr;
    Dispose(F, Done);
    end { ReadHighlite };
  {JO}

  function ReadConfig: LongInt;
    var
      S: TBufStream;
      CFGVer: AWord;
      ID: AWord;
      L: AWord;
      p: Pointer;
      I: LongInt;

    procedure SRead(var Buf);
      begin
      S.Read(Buf, l);
      end;

    procedure SSkip;
      begin
      S.Seek(S.GetPos + L);
//JO: пытаться сделать локализацию нижележащего сообщения бесполезно,
//    т.к. оно выдаётся до загрузки языковых установок
      WriteLn('Local error in config file, ID =  ' + ItoS(ID));
      end;

    procedure GetVer;
      var
        i: Byte;
        Chk: String;
      begin
      CFGVer := 0;
      for i := NumSupportedConfigs downto 1 do
        begin
        S.Seek(0);
        S.Read(Chk[1], ConfigSigns[i].SignLen);
        SetLength(Chk, ConfigSigns[i].SignLen);
        {Chk[0] := Char( ConfigSigns[i].SignLen );}
        if Chk = ConfigSigns[i].Sign then
          begin
          CFGVer := ConfigSigns[i].SignVer;
          if ConfigSigns[i].HavVer then
            S.Read(CFGVer, SizeOf(VersionWord));
          Break;
          end;
        end;
      end;

    begin { ReadConfig: }
    ReadConfig := -1;
    CFGVer := 0;
    S.Init(SourceDir+'DN'+GetEnv('DNCFG')+'.CFG', stOpenRead, 16384);
    if  (S.Status = stOK) and (S.GetSize <> 0) then
      GetVer;
    if  (CFGVer  = 0) or (CFGVer > VersionWord) then
      begin
      S.Done;
      Virgin := True;
      Exit;
      end;
    while S.GetPos < S.GetSize do
      begin
      S.Status := stOK;
      S.Read(ID, SizeOf(AWord));
      S.Read(L, SizeOf(AWord));

      case ID of
        0:
          begin
          {Virgin := True;}
          Break;
          end;
        cfgNewSystemData:
          if SizeOf(SystemData) = L then
            SRead(SystemData) else SSkip;
        cfgOld2SystemData: {Back compatibility}
          if SizeOf(TOld2SystemData) = L then
            begin
            GetMem(p, SizeOf(TOld2SystemData));
            SRead(p^);
            with TOld2SystemData(p^) do
              begin
              SystemData.Options := Options;
              SystemData.Mode1 := Mode1;
              SystemData.Mode2 := Mode2;
              SystemData.Temp := Temp;
              Move(Drives, SystemData.Drives, SizeOf(Drives));
              end;
            FreeMem(p, SizeOf(TOld2SystemData));
            end
          else SSkip;
        cfgOldSystemData: {Back compatibility}
          if SizeOf(TOldSystemData) = L then
            begin
            GetMem(p, SizeOf(TOldSystemData));
            SRead(p^);
            with TOldSystemData(p^) do
              begin
              SystemData.Options := Options;
              SystemData.Mode1 := Mode1;
              SystemData.Mode2 := Mode2;
              SystemData.Temp := Temp;
              Move(Drives, SystemData.Drives, SizeOf(Drives));
              end;
            FreeMem(p, SizeOf(TOldSystemData));
            end
          else SSkip;
        cfgSystemData: {Back compatibility}
          if SizeOf(TOld1SystemData) = L then
            begin
            GetMem(p, SizeOf(TOld1SystemData));
            SRead(p^);
            with TOld1SystemData(p^) do
              begin
              SystemData.Options := Options;
              SystemData.Mode1 := Mode1;
              SystemData.Mode2 := Mode2;
              SystemData.Temp := Temp;
              Move(Drives, SystemData.Drives, SizeOf(Drives));
              end;
            FreeMem(p, SizeOf(TOld1SystemData));
            end
          else SSkip;
        cfgStartupData: {Back compatibility}
          if SizeOf(TOldStartupData) = L then
            begin
            GetMem(p, SizeOf(TOldStartupData));
            SRead(p^);
            with TOldStartupData(p^) do
              begin
              StartupData.Load := Load;
              StartupData.Unload := Unload;
              end;
            FreeMem(p, SizeOf(TOldStartupData));
            end
          else SSkip;
        cfgNewStartupData:
          if SizeOf(StartupData) = L then
            SRead(StartupData) else SSkip;
        cfgMouseData:
          begin
          if SizeOf(MouseData) = L then SRead(MouseData) else SSkip;
          XSens := MouseData.HSense;
          YSens := MouseData.VSense;
          end;
        cfgInterfaceData:
          if SizeOf(InterfaceData) = L then
            SRead(InterfaceData) else SSkip;
        {$IFDEF SS}
        cfgNewSaversData:
          if (SizeOf(SaversData)-SizeOf(SaversData.Selected)*2) = L then
            SRead(SaversData.Time) else SSkip;
        cfgSaversData: {Back compatibility}
          if (SizeOf(TOldSaversData)-
              SizeOf((TOldSaversData(p^).Selected))*2) = L then
            begin
            GetMem(p, SizeOf(TOldSaversData));
            S.Read(TOldSaversData(p^).Time,
                 SizeOf(TOldSaversData)-
                 SizeOf((TOldSaversData(p^).Selected))*2);
            with TOldSaversData(p^) do
              begin
              SaversData.Time := ItoS(Time);
              SaversData.Mouse := Mouse;
              SaversData._ := _;
              end;
            FreeMem(p, SizeOf(TOldSaversData));
            end
          else SSkip;
        {$ENDIF}
        cfgSystemColors:
          if SizeOf(SystemColors) = L then
            SRead(SystemColors) else SSkip;
        cfgEditorDefaults:
          if SizeOf(EditorDefaults) = L then
            SRead(EditorDefaults) else SSkip;
        cfgOldEditorDefaults: {Back compatibility}
          if SizeOf(TOldEditorDefaultsData) = L then
            begin
            GetMem(p, SizeOf(TOldEditorDefaultsData));
            SRead(p^);
            with TOldEditorDefaultsData(p^) do
              begin
              EditorDefaults.EdOpt := EdOpt;
              EditorDefaults.EdOpt2 := ebfHlt+ebfSmt;
              EditorDefaults.ViOpt := ViOpt;
              EditorDefaults.LM := LM;
              EditorDefaults.RM := RM;
              EditorDefaults.NewLine := NewLine;
              EditorDefaults.TabSize := TabSize;
              end;
            FreeMem(p, SizeOf(TOldEditorDefaultsData));
            end
          else SSkip;
        cfgFFindOptions:
          if SizeOf(Word)*2 = L then {см. сохранение соотв. структуры}
            SRead(FileFind.FindRec.Options) else SSkip;
        cfgTetrisRec:
          if SizeOf(TetrisRec) = L then SRead(TetrisRec) else SSkip;
        {$IFDEF PrintManager}
        cfgPrinterSetup:
          if SizeOf(RPrinterSetup) = L then
            SRead(RPrinterSetup) else SSkip;
        {$ENDIF}
        cfgPanSetupPreset:
          if SizeOf(PanSetupPreset) = L then
            SRead(PanSetupPreset) else SSkip;
        cfgDriveInfoData:
          if SizeOf(DriveInfoData) = L then
            SRead(DriveInfoData) else SSkip;
        cfgCountryInfo:
          begin
          if SizeOf(CountryInfo) = L then
            SRead(CountryInfo) else SSkip;
          end;
        cfgConfirms:
          begin
          if SizeOf(Confirms) = L then SRead(Confirms) else SSkip;
          {if CfgVer<$15106 then
                              Confirms:=Confirms or cfFmtOs2Warning;}
          end;
        cfgUUEData:
          if SizeOf(UUDecodeOptions) +
            SizeOf(TUUEncodeData) = L then {см. сохранение соотв. структуры}
              SRead(UUDecodeOptions) else SSkip;
        cfgTermDefaults:
          if SizeOf(TerminalDefaults) = L then
            SRead(TerminalDefaults) else SSkip;
        cfgFMSetup:
          if SizeOf(Startup.FMSetup) = L then
            SRead(Startup.FMSetup) else SSkip;
        cfgBlink:
          begin
          if SizeOf(CurrentBlink) = L then
            SRead(CurrentBlink) else SSkip;
          SetBlink(CurrentBlink);
          end;
        cfgVGAPalette:
          begin
          if SizeOf(VGA_palette) = L then
            SRead(VGA_palette) else SSkip;
          if  (StartupData.Load and osuResetPalette <> 0)
            and VGASystem
          then
            SetPalette(VGA_palette);
          end;
        cfgDefaultArchiver:
          if SizeOf(DefaultArchiver) = L then
            SRead(DefaultArchiver) else SSkip;
        cfgDefaultArchiverMode:
          if SizeOf(DefaultArcMode) = L then
            SRead(DefaultArcMode) else SSkip;
        {$IFDEF SS}
        cfgSavers:
          begin
          I := i32(S.GetPos);
          SaversData.Selected.List := PTextCollection(S.Get);
          if (S.Status <> stOK) or (S.GetPos <> I + L) then
            begin
            S.Reset;
            S.Seek(I + L);
            WriteLn('Local error in config file, ID =  ' + ItoS(ID));
            end;
          end;
        {$ENDIF}
        cfgExtractOptions:
          if SizeOf(UnarchiveOpt) = L then
             SRead(UnarchiveOpt) else SSkip; {JO}
        cfgChangeCaseOptions:
          if SizeOf(ChangeNamesCaseOptions) = L then
            SRead(ChangeNamesCaseOptions) else SSkip;
        cfgAppPalette:
          if SizeOf(appPalette) = L then
            SRead(appPalette) else SSkip;
        cfgComareDirsOptions:
          if SizeOf(ComareDirsOptions) = L then
            SRead(ComareDirsOptions) else SSkip;
        cfgSortCurPanTypeOnly:
          if SizeOf(SortCurPanTypeOnly) = L then
            SRead(SortCurPanTypeOnly) else SSkip;
        cfgFullMenuPanelSetup:
          if SizeOf(FullMenuPanelSetup) = L then
            SRead(FullMenuPanelSetup) else SSkip;
        cfgCalcFormat:
          if SizeOf(CalcFormat) = L then
            SRead(CalcFormat) else SSkip;
        else {case}
          S.Seek(S.GetPos+L);
      end {case};
      end;
    S.Done;
    Security := Startup.FMSetup.Show and fmsShowHidden = 0;

    SystemDataOpt := SystemData.Options;
    InterfaceDataOpt := InterfaceData.Options;
    FMSetupOpt := Startup.FMSetup.Options;
    EditorDefaultsOpt := EditorDefaults.EdOpt;
    EditorDefaultsOpt2 := EditorDefaults.EdOpt2;
    ViewerOpt := EditorDefaults.ViOpt;
    StartupDataLoad := StartupData.Load;
    StartupDataUnload := StartupData.Unload;
    ConfirmsOpt := Confirms;
    CopyLimit := SystemData.CopyLimitBuf;
    ForceDefaultArchiver := SystemData.ForceDefArch;
    end { ReadConfig: };

  procedure SetOverlay;
    var
      S: String;
      I: LongInt;
    begin
    SwpDir := GetEnv('DNSWP');
    if BadTemp(SwpDir) then
      begin
      I := FindParam('/S');
      if I > 0 then
        SwpDir := Copy(ParamStr(I), 3, MaxStringLength);
      end;
    if BadTemp(SwpDir) and NoTempDir then
      SwpDir := ''
    else
      SwpDir := TempDir;
    if RunFirst then
      EraseFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
    end;

  procedure ReadIni;
    var
      INItime, INIsize: LongInt;
    begin
    if ProbeINI(INItime, INIsize) then
      begin {ini есть}
      if (not ReadIniCache(INItime, INIsize)) {не удалось пpочесть файл-кэш}
//JO: нижележащее условие - нет конфига, или сменилась его веpсия -
//    с наибольшей веpоятностью говоpит о том, что DN новой веpсии
//    только что установлен, и читать кэш ini-файла в такой ситуации
//    нежелательно
        or Virgin then
        begin
        LoadDnIniSettings;
        if DnIni.AutoSave then
          SaveDnIniSettings(nil);
        end;
      CopyIniVarsToCfgVars;
      end
    else
      SaveDnIniSettings(nil); {создаем ini}
    DoneIniEngine;
    end { ReadIni };

  var
    i: Integer;
  begin { DoStartup }
  {AK155 10.03.2005 }
  i := FindParam('/DNHIS=');
  if i <> 0 then
    HistNameSuffix := Copy(ParamStr(i), Length('/DNHIS=')+1, 255)
  else
    HistNameSuffix := GetEnv('DNHIS');
  {/AK155}

  (*  RegisterType( RTextCollection );*)
  SavePos := ReadConfig;
  ReadHighlite; {JO}
  UpdateConfig;

  MouseVisible := MouseData.Options and omsCursor <> 0;
  if OS2exec or (opSys and opWNT <> 0)
  then
    Executables := Executables+';cmd';
  {$IFDEF DPMI32}
  if Chk4Dos
  then
    Executables := Executables+';btm';
  {$ENDIF}
  PrepareExtCollection;

  RunMenu := (StartupData.Load and osuAutoMenu <> 0);
  SetOverlay;
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT');
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.MNU');
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'.LST');
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.LST');
  {$IFDEF OS2}
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.CMD');
  {$ENDIF}
  ReadIni;
  if ApplyCodetables <> 0 then
    writeln(GetString(dlCoutrySetupErr));
  {$IFDEF SS}Val(SaversData.Time, SkyDelay, Integer(SPos1)); {$ENDIF}
  if SkyDelay = 0 then
    SkyDelay := 255; { X-Man }
  {ExecDNAutoexec;}
  {Cat}
  Startup.AutoRefreshPanels :=
    (Startup.FMSetup.Options and fmoAutorefreshPanels <> 0);
  if Startup.AutoRefreshPanels then
    NotifyInit;
  {/Cat}
  end { DoStartup };
{-DataCompBoy-}

procedure CrLf;
  {$IFNDEF DPMI32}
  begin
  Writeln;
  end;
  {$ELSE}
  assembler;
asm
  MOV  DL,0DH
  MOV  AH,2
  INT  21H
  MOV  DL,0AH
  MOV  AH,2
  INT  21H
end;
{$ENDIF}

procedure RUN_IT;
  var
    ShiftRec: record
      ScrH, CurY: Byte
      end absolute FreeStr; { just to reduce DS }
    {$IFDEF OS2}
  var
    RegRec: ExceptionRegistrationRecord; {AK155 for killer}
    {this data MUST be located in stack}
    {$ENDIF}
  var
    Ev: TEvent;
    {$IFDEF DPMI32}
    Regs: real_mode_call_structure_typ;
    {$ENDIF}
  begin
  {$IFDEF WIN95_HIGHPRIORITY}
  if opSys = opWin then
    {Win9x}
    SetPriorityClass(GetCurrentProcess, High_Priority_Class);
  {Cat: чтоб не тормозило}
  {$ENDIF}

  Randomize;

  LSliceCnt := -3;

  {$IFNDEF DPMI32}
  if RestartOnExit then
    RunFirst := False
  else
    RunFirst := True;
  RestartOnExit := False;
  TottalExit := False;
  {$ELSE}
  RunFirst := True;

  //get info from Dn.Com
  LoaderSeg := 0;
  init_register(Regs);
  Regs.ax_ := $9900;
  intr_realmode(Regs, $2F);

  if Regs.bx_= $444E{'DN'} then
    begin
    DnNumber := Regs.al_;
    RunFirst := boolean(Regs.ah_);

    init_register(Regs);
    Regs.ax_ := $9901;
    Regs.es_ := 0;
    intr_realmode(Regs, $2F);
    CommandOfs := Regs.bx_;
    LoaderSeg  := Regs.es_;

    init_register(Regs);
    Regs.ax_ := $9905;
    Regs.dx_ := 0;
    Regs.cx_ := 0;
    intr_realmode(Regs, $2F);
    DDTimer := Regs.cx_;
    DDTimer := DDTimer shl 16 + Regs.dx_;
  end;
  {$ENDIF}{dpmi32}

  if DDTimer > 0 then
    DDTimer := GetCurMSec - DDTimer;

  if RunFirst then Writeln
    ('Dos Navigator /2 Open Source  '+VersionName+'  Based on DN by RIT Labs'
    )
  ;

  TempBounds.Assign(0, 0, 0, 0);

  RegisterAll;
  DoStartup;
  {$IFNDEF DPMI32}
  SetKillHandler {$IFDEF OS2}(RegRec) {$ENDIF}; {AK155}
  {$ENDIF}
  with ShiftRec do
    begin
    if  (CurY = ScrH) and (InterfaceData.Options and ouiHideStatus = 0)
    then
      begin
      CrLf;
      end
    end;

  SetBlink(CurrentBlink);

  (* InitLFNCol; *)
  MyApplication.Init;

  if RunFirst then
    ShowIniErrors;

  if RunFirst then
    if  (StartupData.Load and osuKillHistory <> 0) then
      ClearHistories;
  if not RunFirst then
    EraseFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
  if RunFirst then
    begin
    if  (Message(@MyApplication, evBroadcast, cmLookForPanels, nil) = nil)
    then
      Message(@MyApplication, evCommand, cmFirstTimePanel, nil);

    FreeStr[1] := Char(FindParam('/P'));
    if  (FreeStr[1] > #0) then
      LoadPalFromFile(Copy(ParamStr(Byte(FreeStr[1])), 3,
         MaxStringLength));
{JO}
    if Virgin then
      begin
      ConfigModified := True; {создаём новый конфиг}
      Message(@MyApplication, evCommand, cmAbout, nil);
      end;
{/JO}
    if NoTempDir then
      begin
      CreateDirInheritance(TempDir, False);
      NoTempDir := False;
      end; {JO}
    ExecDNAutoexec;
    end;

  if DDTimer > 0 then
    begin
    Ev.What := evCommand;
    Ev.Command := cmShowTimeInfo;
    MyApplication.PutEvent(Ev);
    end;

  with MyApplication do
    begin
    Lock;
    MenuBar^.MakeFirst;
    Desktop^.MakeFirst;
    Clock^.MakeFirst;
    UnLock;
    end;
  {$IFDEF DPMI32}
  w95QuitInit; {Gimly}
  {$ENDIF}
  MyApplication.Run;
  {$IFNDEF DPMI32}
  UnsetKillHandler {$IFDEF OS2}(RegRec) {$ENDIF}; {AK155}
  {$ENDIF}
  ClearIniErrors;
  GlobalMessage(evCommand, cmKillUsed, nil);
  TottalExit := True;
  MyApplication.Done;
  {Cat}
  DoneHistories;
  if ColorIndexes <> nil then
    begin
    FreeMem(ColorIndexes, 2+ColorIndexes^.ColorSize);
    ColorIndexes := nil;
    end;
  {/Cat}
  end { RUN_IT };

{Cat}
{$IFDEF LINEPOSIT}
procedure Error(const FileName: String; LineNo, Addr, Code: LongInt);
  begin
  if Addr <> 0 then
    begin
    SourceFileName := @FileName;
    SourceLineNo := LineNo;
    ErrorAddr := Pointer(Addr);
    ExitCode := Code;
    EndFatalError;
    end;
  end;
{$ENDIF}
{/Cat}

end.

