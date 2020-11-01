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

unit UMyApp;

interface

uses
  {$IFDEF PLUGIN}Plugin, {$ENDIF} {Cat}
  DNUtil, Drivers, Views,
  xTime, Defines, Objects2
  ;

type
  MyApp = object(TDNApplication)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure GetEvent(var Event: TEvent); virtual;
    procedure Idle; virtual;
    end;

var
  MyApplication: MyApp;

implementation

uses
  {$IFNDEF DPMI32}Killer, {$ENDIF}
  VpSysLow, fnotify, Advance2, DNApp, Gauges,
  Drives, Advance, Advance3, Commands,
  DN1, Events, UserMenu, Messages, Startup,
  FlPanelX, Macro
  ;

(*{$I  runcmd.inc}*)

{$IFDEF DPMI32}
{Gimly}
procedure PostQuitMessage;
  var
    Event: TEvent;
  begin
  w95locked := True;
  Event.What := evCommand;
  Event.Command := cmQuit;
  PDNApplication(Application)^.HandleCommand(Event);
  if w95locked then
    MyApplication.HandleEvent(Event);
  end;
{$ENDIF}

procedure MyApp.GetEvent;
  var
    W: Word;
    WW: Word;
    PM: PKeyMacros;

  const
    MacroPlaying: Boolean = False;
    MacroKey: Integer = 0;
    CurrentMacro: PKeyMacros = nil;
    QuitEvent: TEvent = (What: evKeyDown; KeyCode: kbAltX);
  begin
  {$IFNDEF DPMI32}
  if Killer.fQuit then
    begin
    PutEvent(QuitEvent);
    Killer.fQuit := False;
    end;
  {$ENDIF}
  {$IFDEF DPMI32}
  if  (not w95locked) and w95QuitCheck then
    PostQuitMessage; {Gimly}
  {$ENDIF}

  inherited GetEvent(Event);
  if MacroPlaying and ((Event.What = evKeyDown) or (Event.What =
         evNothing))
  then
    begin
    Event.What := evKeyDown;
    Event.KeyCode := CurrentMacro^.Keys^[MacroKey];
    Inc(MacroKey);
    MacroPlaying := MacroKey < CurrentMacro^.Count;
    if not MacroPlaying then LongWorkEnd; {JO}
    end;
  case Event.What of
    evNothing:
      if  (NeedLocated > 0) and (GetSTime-NeedLocated > 30) then
        begin
        NeedLocated := 0;
        Message(Desktop, evCommand, cmDoSendLocated, nil);
        end;
    evKeyDown:
      begin
      {          if (Event.KeyCode = kbAltQ) and Desktop^.GetState(sfFocused) then begin OpenSmartpad; ClearEvent(Event) end;}
      if Event.KeyCode = kbNoKey then
        begin
        Event.What := evNothing;
        Exit
        end
      else if (not MacroPlaying)
        and (Event.KeyCode = kbAltShiftIns)
        and (ShiftState and kbCtrlShift = 0)
      then
        begin
        Event.What := evNothing;
        ScreenGrabber(False);
        Exit
        end;
      if  (Event.ScanCode >= Hi(kbCtrlF1))
             and (Event.ScanCode <= Hi(kbCtrlF10))
        and (Pointer(Current) = Pointer(Desktop))
           and (ShiftState and 3 <> 0)
      then
        begin
        if QuickExecExternal(Event.ScanCode-Hi(kbCtrlF1)+1) then
          begin
          Event.What := evCommand;
          Event.Command := cmExecString;
          Event.InfoPtr := @QuickExecExternalStr;
          end
        else
          Event.What := evNothing;
        Exit;
        end;
      if  (ShiftState and 7 <> 0)
             and ((ShiftState and 4 = 0) or (ShiftState and 3 = 0)) and
          (Event.ScanCode >= Hi(kbAlt1))
           and (Event.ScanCode <= Hi(kbAlt9))
      then
        begin
        WW := Event.ScanCode-Hi(kbAlt1);
        if ShiftState and 3 <> 0 then
          begin
          if KeyMacroses = nil then
            begin
            New(KeyMacroses, Init(10, 10));
            for W := 1 to 10 do
              KeyMacroses^.Insert(nil);
            end;
          if MacroRecord then
            begin
            MacroRecord := False;
            ClearEvent(Event);
            Exit;
            end;
          MacroRecord := True;
          KeyMacroses^.AtFree(WW);
          New(PM, Init);
          KeyMacroses^.AtInsert(WW, PM);
          CurrentMacro := PM;
          end
        else if KeyMacroses <> nil then
          begin
          if MacroRecord then
            begin
            MacroRecord := False;
            ClearEvent(Event);
            Exit;
            end;
          CurrentMacro := KeyMacroses^.At(WW);
          MacroPlaying := CurrentMacro <> nil;
          if MacroPlaying then LongWorkBegin else LongWorkEnd; {JO}
          MacroKey := 0;
          end;
        ClearEvent(Event);
        end;

      {AK155: Alt-Shift-F3 -> cmFileTextView}
      {if (Event.KeyCode = kbAltF3) and
              (ShiftState and kbAltShift <> 0) and
              (ShiftState and (kbRightShift or kbLeftShift) <> 0) then}
      if Event.KeyCode = kbAltShiftF3 then
        {Cat}
        begin
        Event.What := evCommand;
        Event.Command := cmFileTextView;
        Exit;
        end;
      {/AK155}
      {if (Event.KeyCode = kbAlt0) and (ShiftState and 3 <> 0) then}
      if Event.KeyCode = kbAltShift0 then
        {Cat}
        begin
        Event.What := evCommand;
        Event.Command := cmListOfDirs;
        Exit;
        end;
      if MsgActive then
        if Event.KeyCode = kbLeft then
          Event.KeyCode := kbShiftTab
        else if Event.KeyCode = kbRight then
          Event.KeyCode := kbTab;
      if  (Event.What = evKeyDown) then
        begin
        if MacroRecord and (CurrentMacro <> nil) then
          CurrentMacro^.PutKey(Event.KeyCode);
        if  (StatusLine <> nil) then
          StatusLine^.HandleEvent(Event);
        end;
      end;
  end {case};
  case Event.What of
    evCommand:
      case Event.Command of
        cmTree,
        cmGetTeam,
        cmHelp,
        cmClearData,
        cmSearchAdvance,
        cmAdvancePortSetup,
        cmNavyLinkSetup,
        cmQuit:
          HandleCommand(Event);
        {Cat: проверяем, не пора ли запускать плагины - EventCatcher-ы}
        {$IFDEF PLUGIN}
        else {case}
          CatchersHandleCommand(Event.Command);
        {$ENDIF}
        {/Cat}
      end {case};
  end {case};

  end { MyApp.GetEvent };

var
  L_Tmr: TEventTimer;
  OldNotify, NewNotify: String; {Cat}

procedure MyApp.Idle;

  procedure L_On;
    begin
    xTime.NewTimer(L_Tmr, 100)
    end;
  procedure NLS;
    begin
    xTime.NewTimer(LSliceTimer, 150)
    end;

  var
    Event: TEvent;

  begin
  {  Put IdleEvt after IdleClick Expired  }
  with IdleEvt do
    if What <> evNothing then
      if xTime.TimerExpired(IdleClick) then
        begin
        PutEvent(IdleEvt);
        if What = evCommand then
          begin
          What := evBroadcast;
          PutEvent(IdleEvt);
          end;
        ClearEvent(IdleEvt);
        end;

  TApplication.Idle;
  {Cat}
  if Startup.AutoRefreshPanels
    and xTime.TimerExpired(NotifyTmr)
  then
    {JO}
    begin
    if NotifyAsk(NewNotify) and
       (NewNotify <> OldNotify) and (OldNotify <> '')
    then
      {$IFDEF OS2}
      FileChanged(OldNotify);
      {$ELSE}
      RereadDirectory(OldNotify);
    {$ENDIF}
    OldNotify := NewNotify;
    xTime.NewTimer(NotifyTmr, 1000); {JO}
    end;
  {/Cat}

  UpdateAll(True);

  if CtrlWas then
    if ShiftState and kbCtrlShift = 0 then
      begin
      CtrlWas := False;
      {if DelSpaces(CmdLine.Str) = '' then}
      Message(@Self, evCommand, cmTouchFile, nil);
      end;

  IdleWas := True;

  end { MyApp.Idle };

procedure MyApp.HandleEvent;
  var
    s: Word;

  procedure UpView(P: PView);
    begin
    P^.MakeFirst;
    Clock^.MakeFirst;
    end;

  begin
  if Event.What = evMouseDown
  then
    if  (Event.Where.Y = 0) and (Event.Buttons and mbLeftButton <> 0)
    then
      MenuBar^.HandleEvent(Event);
  if Event.What <> evNothing then
    inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      case Event.Command of
        cmUpdateConfig:
          begin
          UpdateConfig;
          WriteConfig;
          end;
        cmMenuOn:
          if  (Event.InfoPtr = MenuBar) then
            UpView(MenuBar);
        cmMenuOff:
          if  (Event.InfoPtr = MenuBar) then
            UpView(Desktop);
        {$IFNDEF PLUGIN}
        cmEnvEdit:
          EditDOSEnvironment(Environment);
        {$ELSE}
        cmEnvEdit:
          EditDOSEnvironment(Environment);
        {$ENDIF}
        else {case}
          HandleCommand(Event);
      end {case};
  end {case};
  end { MyApp.HandleEvent };

end.
