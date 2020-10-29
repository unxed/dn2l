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
{AK155 = Alexey Korop, 2:461/155@fidonet}
{$I STDEFINE.INC}
{$DEFINE NOASM}
unit UserMenu;

interface

uses
  Defines, Views, Scroller, Drivers, FilesCol
  ;

var
  QuickExecExternalStr: String;

function QuickExecExternal(N: Integer): Boolean;
  {` Исполнитель программ быстрого запуска (типа Ctr-F1).
    N=ScanCode-Hi(kbCtrlF1)+1`}
function ExecUserMenu(DoGlobal: Boolean): Boolean;
procedure ScreenGrabber(ShowMessage: Boolean);
function MakeString(S: String; UserParams: PUserParams;
    HandleTildes: Boolean; TM: PString): String;

type
  PUserView = ^TUserView;
  TUserView = object(TScroller)
    Grabbing: Boolean;
    constructor Init(var R: TRect; H, V: PScrollBar);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure ChangeBounds(var Bounds: TRect); virtual;
    end;

  PUserWindow = ^TUserWindow;
  TUserWindow = object(TWindow)
    OldScreenWidth: Word;
    constructor Init;
    procedure CalcBounds(var Bounds: TRect; Delta: TPoint); virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    end;

const
  RunFrom2E: Boolean = False;

implementation
uses
  Lfnvp, {DataCompBoy}
  DNApp, Advance, Advance1, Advance2, Startup, Messages, Menus,
  Commands, Microed, {WinClp, // commented by unxed} DNHelp, Dos, Memory, Dialogs, Tree
  , filediz, Objects, VPUtils
  ;

type
  PUserMenuItem = ^TUserMenuItem;
  TUserMenuItem = record
    Text: PString;
    Level: Word;
    Line: Word;
    RunFrom2E: Boolean;
    end;

  PUserCollection = ^TUserCollection;
  TUserCollection = object(TCollection)
    procedure FreeItem(P: Pointer); virtual;
    end;

constructor TUserWindow.Init;
  var
    R: TRect;
  begin
  Desktop^.GetExtent(R);
  R.Grow(1, 1);
  inherited Init(R, GetString(dlOutputTitle), 0);
  OldScreenWidth := ScreenWidth;
  GetExtent(R);
  R.Grow(-1, -1);
  Insert(New(PUserView, Init(R,
           StandardScrollBar(sbHorizontal+sbHandleKeyboard),
        StandardScrollBar(sbVertical+sbHandleKeyboard))));
  ClearPositionalEvents := False;
    { пусть единообразно обработается в dnapp }
  end;

procedure TUserWindow.CalcBounds(var Bounds: TRect; Delta: TPoint);
  begin
  if  (Size.X <= ScreenWidth) and (ScreenWidth = OldScreenWidth) then
    begin
    inherited CalcBounds(Bounds, Delta);
    Exit
    end;
  Desktop^.GetExtent(Bounds);
  Bounds.Grow(1, 2);
  Inc(Bounds.B.Y);
  OldScreenWidth := ScreenWidth;
  end;

procedure TUserWindow.SetState(AState: Word; Enable: Boolean);
  var
    WindowCommands: TCommandSet;
  begin
  inherited SetState(AState, Enable);
  if AState = sfSelected then
    begin
    SetState(sfActive, Enable);
    if Enable then
      EnableCommands([cmHideLeft, cmHideRight, cmHideInactive])
    else
      DisableCommands([cmHideLeft, cmHideRight, cmHideInactive]);
    end;
  end;

constructor TUserView.Init;
  begin
  inherited Init(R, H, V);
  SetLimit(ScreenWidth, ScreenHeight);
  GrowMode := gfGrowHiX+gfGrowHiY;
  EventMask := $FFFF;
  Options := Options or ofSelectable;
  Grabbing := False;
  ScrollTo(0,
    Byte(CommandLine^.GetState(sfVisible))+
    Byte(InterfaceData.Options and ouiHideMenu = 0)
    { +
   Byte(InterfaceData.Options and ouiHideStatus = 0)});
  Delta.Y := VScrollBar^.Value;
  end;

procedure TUserView.ChangeBounds;
  begin
  inherited ChangeBounds(Bounds);
  end;

procedure TUserView.Draw;
  var
    I: Integer;
    B: TDrawBuffer;
  begin
  for I := 0 to Size.Y-1 do
    begin
    MoveChar(B, ' ', 07, SizeOf(B) div 2);
    if I+Delta.Y < UserScreenSize div (UserScreenWidth*2)
    then
      Move(PAWordArray(UserScreen)^[(I+Delta.Y)*UserScreenWidth], B,
         UserScreenWidth*2);
    WriteLine(0, I, Size.X, 1, B[Delta.X]);
    end;
  end;

procedure TUserView.HandleEvent;
  begin
  inherited HandleEvent(Event);
  SetLimit(ScreenWidth, ScreenHeight);
  case Event.What of
    evCommand:
      case Event.Command of
        {cmGrabscreen: begin ClearEvent(Event); if Execute = cmOK then  end;}
        cmGetName:
          PString(Event.InfoPtr)^:= GetString(dlOutputTitle);
        cmShowOutput, cmHideRight, cmHideLeft:
          begin
          if Event.Command <> cmShowOutput then
            begin
            if Event.Command = cmHideLeft then
              Event.Command := cmPostHideLeft
            else
              Event.Command := cmPostHideRight;
            PutEvent(Event);
            end;
          if Owner^.GetState(sfSelected) then
            Message(Owner, evCommand, cmClose, nil)
          else
            PView(Event.InfoPtr^) := Owner;
          ClearEvent(Event);
          end;
      end {case};
    evKeyDown:
      begin
      { Flash >>> }
      if  (Event.KeyCode = kbShiftUp) or (Event.KeyCode = kbShiftDown)
      then
        CommandLine^.HandleEvent(Event);
      { Flash <<< }
      if Size.X = ScreenWidth then
        CommandLine^.HandleEvent(Event);
      end;
  end {case};
  end { TUserView.HandleEvent };

procedure TUserCollection.FreeItem(P: Pointer);
  begin
  DisposeStr(PUserMenuItem(P)^.Text);
  Dispose(PUserMenuItem(P));
  end;

{AK155 17-12-2002}
procedure NameAndExt(HandleTildes: Boolean;
    TS: String; Macro: Char; var S: String);
  var
    l: Integer;
  begin
  if HandleTildes then
    Replace('~', #0'~', TS);
  Replace('$', #4, TS);
  Replace('!', #1, TS);
  Replace('&', #2, TS);
  Replace('#', #3, TS);
  l := PosLastDot(TS);
  Replace('.'+Macro, Copy(TS, l, 255), S);
  Replace(Macro, Copy(TS, 1, l-1), S);
  end;

{-DataCompBoy-}
{AK155 Тут была куча обкладываний строк символом #0,
непонятно зачем нужными. В ритлабовском DN этого не было,
так что я убрал, а вставил замену в именах файлов '~' на #0'~'
(см. MoveCStr в drivers._vp)}

function MakeString(S: String; UserParams: PUserParams;
     HandleTildes: Boolean; TM: PString): String;
  var
    ts: String;
    tz: String;
    DA, DP: TDate4; { дата файла в активной и пассивной панели }
  begin
  {  if HandleTildes then zs:=#0 else zs:='';}
  Replace('!!', #1, S);
  Replace('&&', #2, S);
  Replace('##', #3, S);
  Replace('$$', #4, S);

  {$IFDEF OS2}
  Replace('!', '#', S);
  Replace('$', '&', S);
  {$ENDIF}

  if UserParams^.Active <> nil then
    DA := TDate4(UserParams^.Active^.FDate);
  if UserParams^.Passive <> nil then
    DP := TDate4(UserParams^.Passive^.FDate);

  {$IFDEF DualName}
  if  (Pos('!\', S) > 0) or (Pos('!:', S) > 0) or (Pos('!/', S) > 0)
  then
    begin
    if UserParams^.Active = nil then
      ts := '' {KSNK}
    else
      begin
      {
      if UserParams^.Active^.TType = ttUpDir
      then
        ts := MakeNormName
            (lfGetShortFileName(UserParams^.Active^.Owner^), '')
      else

        ts := MakeNormName(GetPath(lfGetShortFileName(MakeNormName(
                  UserParams^.Active^.Owner^,
                   UserParams^.Active^.FlName[False]))), '');
      }
      end;
    if HandleTildes then
      Replace('~', #0'~', ts);
    Replace('$', #4, ts);
    Replace('!', #1, ts);
    Replace('&', #2, ts);
    Replace('#', #3, ts);
    Replace('!:', Copy(ts, 1, 2), S);
    Replace('!\', Copy(ts, 3, MaxStringLength), S);
    Replace('!/', Copy(ts, 3, Length(ts)-3), S);
    end;
  {$ENDIF}

  if  (Pos('#\', S) > 0) or (Pos('#:', S) > 0) or (Pos('#/', S) > 0)
  then
    begin
    if UserParams^.Active = nil then
      ts := '' {KSNK}
    else

     if UserParams^.Active^.TType = ttUpDir
    then
      ts := MakeNormName(UserParams^.Active^.Owner^, '')
    else
      ts := MakeNormName(GetPath(MakeNormName(
              UserParams^.Active^.Owner^,
               UserParams^.Active^.FlName[True])), '');
    if HandleTildes then
      Replace('~', #0'~', ts);
    Replace('$', #4, ts);
    Replace('!', #1, ts);
    Replace('&', #2, ts);
    Replace('#', #3, ts);
    Replace('#:', Copy(ts, 1, 2), S);
    Replace('#\', Copy(ts, 3, MaxStringLength), S);
    Replace('#/', Copy(ts, 3, Length(ts)-3), S);
    end;

  {$IFDEF DualName}
  if  (Pos('$\', S) > 0) or (Pos('$:', S) > 0) or (Pos('$/', S) > 0)
  then
    begin
    (*
    if UserParams^.Passive = nil then
      ts := '' {KSNK}
    else if UserParams^.Passive^.TType = ttUpDir
    then
      ts := MakeNormName(lfGetShortFileName(UserParams^.Passive^.Owner^),
           '')
    else
      ts := MakeNormName(GetPath(lfGetShortFileName(MakeNormName(
                UserParams^.Passive^.Owner^,
                 UserParams^.Passive^.FlName[False]))), '');
    *)
    if HandleTildes then
      Replace('~', #0'~', ts);
    Replace('$', #4, ts);
    Replace('!', #1, ts);
    Replace('&', #2, ts);
    Replace('#', #3, ts);
    Replace('$:', Copy(ts, 1, 2), S);
    Replace('$\', Copy(ts, 3, MaxStringLength), S);
    Replace('$/', Copy(ts, 3, Length(ts)-3), S);
    end;
  {$ENDIF}

  if  (Pos('&\', S) > 0) or (Pos('&:', S) > 0) or (Pos('&/', S) > 0)
  then
    begin
    if UserParams^.Passive = nil then
      ts := '' {KSNK}
    else if UserParams^.Passive^.TType = ttUpDir
    then
      ts := MakeNormName(UserParams^.Passive^.Owner^, '')
    else
      ts := MakeNormName(GetPath(MakeNormName(
              UserParams^.Passive^.Owner^,
               UserParams^.Passive^.FlName[True])), '');
    if HandleTildes then
      Replace('~', #0'~', ts);
    Replace('$', #4, ts);
    Replace('!', #1, ts);
    Replace('&', #2, ts);
    Replace('#', #3, ts);
    Replace('&:', Copy(ts, 1, 2), S);
    Replace('&\', Copy(ts, 3, MaxStringLength), S);
    Replace('&/', Copy(ts, 3, Length(ts)-3), S);
    end;

  if Replace('!%', #5, S) or Replace('#%', #5, S) then
    begin
    if  (Pos(#5'A', S) > 0) or (Pos(#5'a', S) > 0) then
      begin
      if UserParams^.Active = nil then
        ts := '' {KSNK}
      else
        ts := GetAttrStr(UserParams^.Active^.Attr);
      Replace(#5'A', ts, S);
      Replace(#5'a', ts, S);
      end;

    if  (Pos(#5'C', S) > 0) or (Pos(#5'c', S) > 0) then
      begin
      if UserParams^.Active = nil then
        ts := '' {KSNK}
      else if UserParams^.Active^.Attr and Directory <> 0
      then
        ts := ''
      else
        ts := ZtoS(UserParams^.Active^.PSize);
      Replace(#5'C', ts, S);
      Replace(#5'c', ts, S);
      end;

    if  (Pos(#5'D', S) > 0) or (Pos(#5'd', S) > 0) then
      begin
      if UserParams^.Active = nil then
        ts := '' {KSNK}
      else
        with UserParams^.Active^, DA do
          MakeDateFull(Day, Month, Yr, Hour, Minute, ts, True);
      SetLength(ts, 10);
      Replace(#5'D', ts, S);
      Replace(#5'd', ts, S);
      end;

    if  (Pos(#5'R', S) > 0) or (Pos(#5'r', S) > 0) then
      begin
      if UserParams^.Active = nil then
        ts := '' {KSNK}
      else
        ts := ItoS
                  (Round(((UserParams^.Active^.PSize/UserParams^.Active^.
                  Size)*100)+0.5))+'%';
      Replace(#5'R', ts, S);
      Replace(#5'r', ts, S);
      end;

    if  (Pos(#5'S', S) > 0) or (Pos(#5's', S) > 0) then
      begin
      if UserParams^.Active = nil then
        ts := '' {KSNK}
      else if UserParams^.Active^.Attr and Directory <> 0
      then
        ts := ''
      else
        ts := ZtoS(UserParams^.Active^.Size);
      Replace(#5'S', ts, S);
      Replace(#5's', ts, S);
      end;

    if  (Pos(#5'T', S) > 0) or (Pos(#5't', S) > 0) then
      begin
      if UserParams^.Active = nil then
        ts := '' {KSNK}
      else
        with UserParams^.Active^, DA do
          ts := FormatTimeStr(Hour, Minute, Second);
      Replace(#5'T', ts, S);
      Replace(#5't', ts, S);
      end;

    if  (Pos(#5'Z', S) > 0) or (Pos(#5'z', S) > 0) then
      begin
      if UserParams^.Active = nil then
        ts := '' {KSNK}
      else
        ts := DizFirstLine(UserParams^.Active^.DIZ);
      if HandleTildes then
        Replace('~', #0'~', ts);
      Replace('$', #4, ts);
      Replace('!', #1, ts);
      Replace('&', #2, ts);
      Replace('#', #3, ts);
      Replace(#5'Z', ts, S);
      Replace(#5'z', ts, S);
      end;

    if  (Pos(#5'F', S) > 0) or (Pos(#5'f', S) > 0) then
      begin
      (*
      {$IFNDEF OS2}
      ts := MakeNormName(lfGetShortFileName(SwpDir),
          GetName(CalcTmpFName(CalcTmpId, 'flt', True)));
      {$ELSE}
      ts := MakeNormName(SwpDir, GetName(CalcTmpFName(CalcTmpId, 'flt',
               True)));
      {$ENDIF}
      *)
      if TM <> nil then
        TM^:= ts;
      if HandleTildes then
        Replace('~', #0'~', ts);
      Replace('$', #4, ts);
      Replace('!', #1, ts);
      Replace('&', #2, ts);
      Replace('#', #3, ts);
      Replace(#5'F', ts, S);
      Replace(#5'f', ts, S);
      end;
    end;

  if Replace('$%', #6, S) or Replace('&%', #6, S) then
    begin
    if  (Pos(#6'A', S) > 0) or (Pos(#6'a', S) > 0) then
      begin
      if UserParams^.Passive = nil then
        ts := '' {KSNK}
      else
        ts := GetAttrStr(UserParams^.Passive^.Attr);
      Replace(#6'A', ts, S);
      Replace(#6'a', ts, S);
      end;

    if  (Pos(#6'C', S) > 0) or (Pos(#6'c', S) > 0) then
      begin
      if UserParams^.Passive = nil then
        ts := '' {KSNK}
      else if UserParams^.Passive^.Attr and Directory <> 0
      then
        ts := ''
      else
        ts := ZtoS(UserParams^.Passive^.PSize);
      if HandleTildes then
        Replace('~', #0'~', ts);
      Replace(#6'C', ts, S);
      Replace(#6'c', ts, S);
      end;

    if  (Pos(#6'D', S) > 0) or (Pos(#6'd', S) > 0) then
      begin
      if UserParams^.Passive = nil then
        ts := '' {KSNK}
      else
        with UserParams^.Passive^, DP do
          MakeDateFull(Day, Month, Yr, Hour, Minute, ts, True);
      SetLength(ts, 10);
      Replace(#6'D', ts, S);
      Replace(#6'd', ts, S);
      end;

    if  (Pos(#6'R', S) > 0) or (Pos(#6'r', S) > 0) then
      begin
      if UserParams^.Passive = nil then
        ts := '' {KSNK}
      else
        ts := ItoS
                  (Round(((UserParams^.Passive^.PSize/UserParams^.Passive^.
                  Size)*100)+0.5))+'%';
      Replace(#6'R', ts, S);
      Replace(#6'r', ts, S);
      end;

    if  (Pos(#6'S', S) > 0) or (Pos(#6's', S) > 0) then
      begin
      if UserParams^.Passive = nil then
        ts := '' {KSNK}
      else if UserParams^.Passive^.Attr and Directory <> 0
      then
        ts := ''
      else
        ts := ZtoS(UserParams^.Passive^.Size);
      Replace(#6'S', ts, S);
      Replace(#6's', ts, S);
      end;

    if  (Pos(#6'T', S) > 0) or (Pos(#6't', S) > 0) then
      begin
      if UserParams^.Passive = nil then
        ts := '' {KSNK}
      else
        with UserParams^.Passive^, DP do
          ts := FormatTimeStr(Hour, Minute, Second);
      Replace(#6'T', ts, S);
      Replace(#6't', ts, S);
      end;

    if  (Pos(#6'Z', S) > 0) or (Pos(#6'z', S) > 0) then
      begin
      if UserParams^.Passive = nil then
        ts := '' {KSNK}
      else
        ts := DizFirstLine(UserParams^.Passive^.DIZ);
      Replace('$', #4, ts);
      Replace('!', #1, ts);
      Replace('&', #2, ts);
      Replace('#', #3, ts);
      Replace(#6'Z', ts, S);
      Replace(#6'z', ts, S);
      end;
    end;

  {$IFDEF DualName}
  if  (PosChar('!', S) > 0) then
    begin
    if UserParams^.Active = nil then
      ts := '' {KSNK}
    else
      ts := UserParams^.Active^.FlName[False];
    NameAndExt(HandleTildes, ts, '!', S);
    end;

  if  (PosChar('$', S) > 0) then
    begin
    if UserParams^.Passive = nil then
      ts := '' {KSNK}
    else
      ts := UserParams^.Passive^.FlName[False];
    NameAndExt(HandleTildes, ts, '$', S);
    end;
  {$ENDIF}

  if  (PosChar('#', S) > 0) then
    begin
    if UserParams^.Active = nil then
      ts := '' {KSNK}
    else
      ts := UserParams^.Active^.FlName[True];
    NameAndExt(HandleTildes, ts, '#', S);
    end;

  if  (PosChar('&', S) > 0) then
    begin
    if UserParams^.Passive = nil then
      ts := '' {KSNK}
    else
      ts := UserParams^.Passive^.FlName[True];
    NameAndExt(HandleTildes, ts, '&', S);
    end;

  Replace(#1, '!', S);
  Replace(#2, '&', S);
  Replace(#3, '#', S);
  Replace(#4, '$', S);

  MakeString := S;
  end { MakeString };
{/AK155}
{-DataCompBoy-}

function ExecUserMenu;
  var
    F: PTextReader;
    F1: lText; {DataCompBoy}
    P: PUserCollection;
    S, S1: String;
    I: LongInt;
    UI: PUserMenuItem;
    StartPos: Integer;
    Items, OItems, SItems: PMenuItem;
    Menu: PMenu;
    PV: PView;
    R: TRect;
    NI, NW: Word;
    EnterParms: Boolean;
    TitleStr: String[30];
    DefStr: String;
    RF2E: Boolean;
    Event: TEvent;
    {$IFDEF OS2}
    WriteEcho: Boolean;
    {$ENDIF}
  label 1, 2;

  function CheckFKeys(S: String): Word;
    var
      I: Integer;
      S1: String;
    begin
    CheckFKeys := kbNoKey;
    for I := 1 to 12 do
      begin
      S1 := 'F'+ItoS(I)+' ';
      if UpStrg(Copy(S, 1, Length(S1))) = S1 then
        begin
        CheckFKeys := kbF1+(I-1) shl 8;
        Exit
        end;
      end;
    end;

  var
    MenuLast: PMenuItem;

  function DoSubMenu(Main: Boolean): PMenuItem;
    var
      P1, P2: PMenuItem;
      UI, PU: PUserMenuItem;
      KB: Word;
    begin
    DoSubMenu := nil;
    if i = P^.Count then
      Exit;
    UI := P^.At(i);
    {if UI^.Text = nil then begin DoSubMenu := NewLine(DoSubMenu); Exit end;}
    KB := kbNoKey;
    P2 := nil;
    if UI^.Text <> nil then
      P1 := NewItem(UI^.Text^, '', CheckFKeys(UI^.Text^), 1000+i,
          hcLUserMenu+Byte(DoGlobal), nil)
    else
      P1 := NewItem('Empty line', '', kbNoKey, 1000+i,
          hcLUserMenu+Byte(DoGlobal), nil);
    DoSubMenu := P1;
    while (i < P^.Count-1) do
      begin
      Inc(i);
      PU := P^.At(i);
      MenuLast := P1;
      if PU^.Level < UI^.Level then
        begin
        Dec(i);
        Exit;
        end;
      if PU^.Level > UI^.Level then
        begin
        P1^.SubMenu := NewMenu(DoSubMenu(False));
        P1^.Command := 0;
        P1^.Flags := miSubmenu;
        end
      else
        begin
        if PU^.Text <> nil
        then
          P1^.Next := NewItem(PU^.Text^, '', CheckFKeys(PU^.Text^),
              1000+i, hcLUserMenu+Byte(DoGlobal), nil)
        else
          P1^.Next := NewLine(nil);
        if Main then
          Inc(NI);
        P1 := P1^.Next;
        end;
      MenuLast := P1;
      end;
    end { DoSubMenu };

  {-DataCompBoy-}
  var
    UserParams: tUserParams;

  begin { ExecUserMenu }
  FillChar(UserParams, SizeOf(UserParams), 0);
  Message(Desktop, evBroadcast, cmGetUserParamsWL, @UserParams); {.11}
  RunFrom2E := False;
  ExecUserMenu := False;
  TitleStr := '';
  DefStr := '';
  EnterParms := False;
  if DoGlobal then
    goto 2;

  F := New(PTextReader, Init(SwpDir+'$dn'+ItoS(DNNumber)+'$.mnu'));
  if F <> nil then
    goto 1;

  S := lFExpand('dn.mnu');
  I := Length(S);
  if Abort then
    goto 2;
  while (I > 0) and (S[I] <> '/') do // slash change by unxed
    Dec(I);
  if I = 0 then
    goto 2;
  repeat
    F := New(PTextReader, Init(S));
    if F <> nil then
      goto 1;

    while (I > 2) and (S[I-1] <> '/') do // slash change by unxed
      begin
      Delete(S, I-1, 1);
      Dec(I);
      end;
    Delete(S, I-1, 1);
    Dec(I);
  until I < 3;
2:
  DoGlobal := True;
  F := New(PTextReader, Init(SourceDir+'dn.mnu'));
  if F = nil then
    begin
    ErrMsg(dlMNUNotFound);
    Exit;
    end;
1:
  Message(Desktop, evBroadcast, cmGetUserParamsWL, @UserParams);
  ClrIO;
  New(P, Init(10, 10));
  I := 0;
  NW := 30;
  while (not F^.Eof) and (IOResult = 0) do
    begin
    S := F^.GetStr;
    Inc(I);
    DelLeft(S);
    DelRight(S);
    if  (S <> '') and (S[1] = '>') then
      begin
      RF2E := False;
      Delete(S, 1, 1); {DelFC(S);}
      if S[1] = '>' then
        begin
        Delete(S, 1, 1); {DelFC(S);}
        RF2E := True
        end;
      S1 := '';
      repeat
        S1 := S1+S[1];
        Delete(S, 1, 1); {DelFC(S);}
      until (S = '') or (S[1] = ' ');
      Delete(S, 1, 1); {DelFC(S);}
      if  (StoI(S1) > 0) then
        begin
        New(UI);
        S := MakeString(S, @UserParams, True, nil);
        UI^.Text := NewStr(S);
        UI^.Level := StoI(S1);
        if  (CStrLen(S) > NW) and (UI^.Level = 1) then
          NW := CStrLen(S);
        UI^.Line := I;
        UI^.RunFrom2E := RF2E;
        P^.Insert(UI);
        end;
      end;
    end;
  S := F^.FileName;
  Dispose(F, Done);
  if P^.Count > 0 then
    begin
    StartPos := -1;
    repeat
      I := 0;
      OItems := nil;
      SItems := nil;
      NI := 3;
      SItems := DoSubMenu(True);
      if SItems = nil then
        Break;
      Menu := NewMenu(SItems);
      Application^.GetExtent(R);
      R.A.X := 0;
      R.B.X := NW;
      R.B.Y := Min(R.B.Y, NI);
      PV := New(PMenuBox, Init(R, Menu, nil));
      PV^.Options := PV^.Options or ofCentered;
      I := Application^.ExecView(PV);
      Dispose(PV, Done);
      DisposeMenu(Menu);
      if I < 1000 then
        begin
        if UserParams.ActiveList <> '' then
          EraseFile(UserParams.ActiveList);
        if UserParams.PassiveList <> '' then
          EraseFile(UserParams.PassiveList);
        if I = cmEditMenu then
          begin
          Event.What := evCommand;
          Event.Command := Byte(not DoGlobal)*cmLocalMenuFileEdit+
            Byte(DoGlobal)*cmMenuFileEdit;
          Event.InfoPtr := nil;
          Application^.PutEvent(Event);
          end;
        Break;
        end;
      StartPos := I-999;
      if  (StartPos >= P^.Count-1) or
          (PUserMenuItem(P^.At(StartPos-1))^.Level >=
               PUserMenuItem(P^.At(StartPos))^.Level)
      then
        begin
        ExecUserMenu := True;
        I := I-1000;
        NW := I;
        UI := P^.At(I);
        F := New(PTextReader, Init(S));
        lAssignText(F1, SwpDir+'$DN'+ItoS(DNNumber)+'$'+CmdExt);
        lRewriteText(F1);
        {$IFNDEF OS2}
        Writeln(F1.T, '@Echo off');
        {$ELSE}
        WriteEcho := True;
        {$ENDIF}
        I := I-1001;
        if F <> nil then
          begin
          for I := 0 to UI^.Line-1 do
            S := F^.GetStr;
          while not F^.Eof do
            begin
            S := F^.GetStr;
            DelLeft(S);
            DelRight(S);
            if  (S <> '') then
              if  (S[1] = '>') then
                Break
              else if (S[1] = ';') then
                Continue
              else if (S[1] = '<') then
                begin
                EnterParms := True;
                Delete(S, 1, 1); {DelFC(S);}
                if  (S <> '') and (S[1] = '=') then
                  begin
                  Delete(S, 1, 1); {DelFC(S);}
                  {Knave 29.08.99}S := MakeString(S, @UserParams, False,
                       nil);
                  DefStr := S;
                  end
                else
                  begin
                  DelLeft(S);
                  TitleStr := S;
                  end;
                Continue;
                end;
            EnterParms := EnterParms or (Pos('%3', S) > 0);
            {$IFDEF OS2}
            {JO: под осью если в меню строка на REXX'е или Perl'е, то не нужно добавлять @Echo off}
            if WriteEcho and (Copy(S, 1, 2) <> '/*')
                 and (Copy(S, 1, 2) <> '#!')
            then
              Writeln(F1.T, '@Echo off');
            WriteEcho := False;
            {$ENDIF}
            Writeln(F1.T, MakeString(S, @UserParams, False, nil));
            RunFrom2E := UI^.RunFrom2E;
            end;
          end;
        Close(F1.T);
        Dispose(F, Done);
        Break;
        end;
    until False;
    end;
  Dispose(P, Done);
  FreeStr := DefStr;
  if TitleStr = '' then
    TitleStr := GetString(dlMenuParamLabel);
  if EnterParms then
    ExecUserMenu := InputBox(GetString(dlMenuParams), TitleStr, FreeStr,
         125, hsInputParams) = cmOK;
  with UserParams do
    begin
    if ActiveList = '' then
      ActiveList := '-';
    if PassiveList = '' then
      PassiveList := '-';
    FreeStr := ActiveList+' '+PassiveList+' '+FreeStr;
    end;
  EraseFile(SwpDir+'$dn'+ItoS(DNNumber)+'$.mnu');
  end { ExecUserMenu };
{-DataCompBoy-}

type
  PGrabber = ^TGrabber;
  TGrabber = object(TView)
    ModalEnd: Boolean;
    Screen: PAWordArray;
    R: TRect;
    BufSize: Word;
    constructor Init;
    destructor Done; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Draw; virtual;
    function Execute: Word; virtual;
    end;

const
  Top: TPoint = (X: 10; Y: 5);
  Bot: TPoint = (X: 21; Y: 6);
  NotMessage: Boolean = True;
  Here: Boolean = False;

procedure ScreenGrabber(ShowMessage: Boolean);
  var
    P: PGrabber;
    B: Boolean;
  begin
  if Here then
    Exit;
  Here := True;
  if NotMessage and ShowMessage then
    begin
    MessageBox(GetString(dlGrabWelcome),
      nil, mfOKButton+mfInformation);
    NotMessage := False;
    end;
  B := MsgActive;
  MsgActive := False;
  P := New(PGrabber, Init);
  Application^.ExecView(P);
  Dispose(P, Done);
  Here := False;
  MsgActive := B;
  end;

constructor TGrabber.Init;
  var
    BB: TRect;
  begin
  Application^.GetExtent(BB);
  inherited Init(BB);
  R.A := Top;
  R.B := Bot;
  BufSize := Application^.Size.X*2*Application^.Size.Y;
  Screen := MemAlloc(BufSize);
  if Screen = nil then
    Fail;
  HideMouse;
  Move(ScreenBuffer^, Screen^, BufSize);
  ShowMouse;
  if Screen = nil then
    Fail;
  Options := Options or ofSelectable or ofTopSelect;
  {EventMask := $FFFF;}
  end;

destructor TGrabber.Done;
  begin
  if Screen <> nil then
    FreeMem(Screen, BufSize);
  inherited Done;
  end;

procedure TGrabber.Draw;

  var
    B: TDrawBuffer;
    I: Integer;

    {$IFNDEF NOASM}
  procedure Invert(var B; K: Integer);
    assembler;
    {&Frame-} {$USES EBX, ECX}
  asm
         mov ECX, K
         lea EBX, B
        @@1:
         mov AX, word ptr [EBX]
         xor AH, $7F
         and AH, $7F
         jnz @@2
         mov AH, 7
       @@2:
         mov word ptr [EBX], AX
         add EBX, 2
         loop @@1
       end;
    {$ELSE}
  procedure Invert(K1, K2: Integer);
    var
      I: Integer;
    begin
    for I := K1 to K1+K2-1 do
      begin
      B[I] := ((B[I] xor $7F00) and $7F00) or Lo(B[I]);
      end
    end;
  {$ENDIF}
  begin { TGrabber.Draw }
  with R do
    begin
    if A.X < 0 then
      Move(-A.X, 0);
    if A.Y < 0 then
      Move(0, -A.Y);
    if B.X-A.X > Size.X then
      B.X := A.X+Size.X;
    if B.Y-A.Y > Size.Y then
      B.Y := A.Y+Size.Y;
    if B.X > Size.X then
      Move(Size.X-B.X, 0);
    if B.Y > Size.Y then
      Move(0, Size.Y-B.Y);
    end;
  Top := R.A;
  Bot := R.B;
  for I := 0 to Size.Y-1 do
    begin
    Move(Screen^[Size.X*I], B, Size.X*2);
    if  (I >= R.A.Y) and (I < R.B.Y) and (R.A.X < R.B.X) then
      {$IFNDEF NOASM}
      Invert(B[R.A.X], R.B.X-R.A.X);
      {$ELSE}
      Invert(R.A.X, R.B.X-R.A.X);
    {$ENDIF}
    WriteLine(0, I, Size.X, 1, B);
    end;
  end { TGrabber.Draw };

function TGrabber.Execute;
  var
    E: TEvent;
  begin
  ModalEnd := False;
  DrawView;
  repeat
    GetEvent(E);
    if E.What <> evNothing then
      HandleEvent(E)
    else
      TinySlice;
  until ModalEnd;
  end;

procedure TGrabber.HandleEvent;
  var
    B: Boolean;
    Sh: Boolean;
    Stp: Integer;
  procedure CED;
    begin
    ClearEvent(Event);
    DrawView
    end;

  procedure MakeClip;
    var
      I, J: Integer;
      S: String;
      C: Char;
    begin
    if ClipBoard <> nil then
      Dispose(ClipBoard, Done);
    ClipBoard := New(PLineCollection, Init(R.B.Y-R.A.Y, 10, True));
    for I := R.A.Y to R.B.Y-1 do
      begin
      S := '';
      for J := R.A.X to R.B.X-1 do
        begin
        C := Char(WordRec(Screen^[I*Size.X+J]).Lo);
        S := S+C;
        end;
      ClipBoard^.Insert(NewLongStr(S));
      end;
    // fixme: commented by unxed
//    if SystemData.Options and ossUseSysClip <> 0 then
//      SyncClipIn;
    if ClipBoardStream <> nil
    then
      ClipBoardStream^.Seek(Positive(ClipBoardStream^.GetPos-4));
    // fixme: commented by unxed
//    CopyLines2Stream(ClipBoard, ClipBoardStream);
    end { MakeClip };

  begin { TGrabber.HandleEvent }
  B := True;
  inherited HandleEvent(Event);
  case Event.What of
    evKeyDown:
      begin
      Sh := ShiftState and 3 <> 0;
      if ShiftState and 4 <> 0 then
        Stp := 8
      else
        Stp := 1;
      case Event.KeyCode and $FFFF of
        {Cat}
        kbPgUp and $FFFF, kbCtrlPgUp and $FFFF:
          if Sh then
            R.A.Y := 0
          else
            R.Move(0, -R.A.Y);
        kbHome and $FFFF, kbCtrlHome and $FFFF:
          if Sh then
            R.A.X := 0
          else
            R.Move(-R.A.X, 0);
        kbEnd and $FFFF, kbCtrlEnd and $FFFF:
          if Sh then
            R.B.X := Size.X
          else
            R.Move(Size.X, 0);
        kbPgDn and $FFFF, kbCtrlPgDn and $FFFF:
          if Sh then
            R.B.Y := Size.Y
          else
            R.Move(0, Size.Y);
        kbRight and $FFFF, kbCtrlRight and $FFFF:
          if Sh then
            begin
            Inc(R.B.X, Stp);
            R.B.X := Min(R.B.X, Size.X)
            end
          else
            R.Move(Stp, 0);
        kbLeft and $FFFF, kbCtrlLeft and $FFFF:
          if Sh then
            begin
            if R.B.X > R.A.X+Stp then
              Dec(R.B.X, Stp)
            else
              R.B.X := R.A.X+1;
            end
          else
            R.Move(-Stp, 0);
        kbEnter and $FFFF:
          begin
          ModalEnd := True;
          MakeClip;
          end;
        kbESC and $FFFF:
          begin
          ModalEnd := True;
          end;
        else {case}
          begin
          if Stp > 1 then
            Stp := Stp div 2;
          case Event.KeyCode and $FFFF of
            {Cat}
            kbUp and $FFFF, kbCtrlUp and $FFFF:
              if Sh then
                begin
                if R.B.Y > R.A.Y+Stp then
                  Dec(R.B.Y, Stp)
                else
                  R.B.Y := R.A.Y+1;
                end
              else
                R.Move(0, -Stp);
            kbDown and $FFFF, kbCtrlDown and $FFFF:
              if Sh then
                begin
                Inc(R.B.Y, Stp);
                R.B.Y := Min(R.B.Y, Size.Y)
                end
              else
                R.Move(0, Stp);
            else {case}
              B := False;
          end {case};
          end;
      end {case};
      end
    else {case}
      B := False;
  end {case};
  if B then
    CED;
  end { TGrabber.HandleEvent };

function QuickExecExternal(N: Integer): Boolean;

  label 1;

  var
    UserParams: tUserParams;
    TitleStr, DefStr, S: String;
    F: PTextReader;
    F1: lText;
    I: Integer;
    OS2: Char;
    SR: lSearchRec;
    EnterParms, Local: Boolean;
  label RL;
  begin
  QuickExecExternal := False;

  Local := True;
  F := New(PTextReader, Init('DN.XRN'));

  if F = nil then
    begin
RL:
    Local := False;
    F := New(PTextReader, Init(SourceDir+'DN.XRN'));
    end;
  if F = nil then
    Exit;
  while not F^.Eof do
    begin
    OS2 := #0;
    TitleStr := F^.GetStr;

    I := PosChar(';', TitleStr);
    if I > 0 then
      SetLength(TitleStr, I-1);
    if TitleStr[1] = '>' then
      begin
      if PosChar(TitleStr[2], '[]><') > 0 then
        OS2 := TitleStr[2];
      {OS2 := Byte(TitleStr[2]=']')+2*Byte(TitleStr[2]='[');}
      if StoI(DelSpaces(Copy(TitleStr, 2+Byte(OS2 <> #0),
             MaxStringLength))) = N
      then
        goto 1;
      end;
    end;
  Dispose(F, Done);
  if Local then
    goto RL;
  Exit;
1:
  I := DNNumber;
  if OS2 = #0 then
    FreeStr := {$IFNDEF OS2}'BAT' {$ELSE}'CMD' {$ENDIF}
  else
    begin
    I := 0;
    repeat
      ClrIO;
      Inc(I);
      lFindFirst(SwpDir+'$DN'+ItoS(I)+'$.CMD', AnyFileDir-Directory, SR); {JO}
      lFindClose(SR);
    until (DosError <> 0) or (I = 99) or Abort;
    ClrIO;
    FreeStr := 'CMD';
    end;

  FreeStr := SwpDir+'$DN'+ItoS(I)+'$.'+FreeStr;
  lAssignText(F1, FreeStr);
  ClrIO;
  lRewriteText(F1);

  if IOResult <> 0 then
    begin
    Dispose(F, Done);
    Exit
    end;
  Writeln(F1.T, '@Echo off');

  if OS2 <> #0 then
    begin
    lGetDir(0, TitleStr); {GetDir(0, TitleStr);}
    {Cat}
    Writeln(F1.T, '@', Copy(TitleStr, 1, 2));
    Writeln(F1.T, '@cd ', TitleStr);
    end;

  TitleStr := '';
  DefStr := '';
  EnterParms := False;
  Message(Desktop, evBroadcast, cmGetUserParamsWL, @UserParams);
  ClrIO;
  while not F^.Eof do
    begin
    S := F^.GetStr;
    DelRight(S);
    DelLeft(S);
    if  (S <> '') and (S[1] = ';') then
      Continue; {!!SF}
    if  (S <> '') then
      if  (S[1] = '>') then
        Break
      else if (S[1] = '<') then
        begin
        EnterParms := True;
        Delete(S, 1, 1); {DelFC(S);}
        if  (S <> '') and (S[1] = '=') then
          begin
          Delete(S, 1, 1); {DelFC(S);}
          {Knave 29.08.99}S := MakeString(S, @UserParams, False, nil);
          DefStr := S;
          end
        else
          begin
          DelLeft(S);
          TitleStr := S;
          end;
        Continue;
        end;
    EnterParms := EnterParms or (Pos('%3', S) > 0);
    Writeln(F1.T, MakeString(S, @UserParams, False, nil));
    end;

  with UserParams do
    begin
    if ActiveList <> '' then
      Writeln(F1.T, '@DEL ', ActiveList);
    if PassiveList <> '' then
      Writeln(F1.T, '@DEL ', PassiveList);
    end;

  Write(F1.T, '@DEL ', FreeStr);
  Close(F1.T);
  Dispose(F, Done);

  if OS2 <> #0 then
    S := OS2+'call '+FreeStr
  else
    S := FreeStr;

  if TitleStr = '' then
    TitleStr := GetString(dlMenuParamLabel);

  QuickExecExternal := not EnterParms or
        (InputBox(GetString(dlMenuParams), TitleStr, DefStr, 125,
         hsInputParams) = cmOK);

  with UserParams do
    begin
    if ActiveList = '' then
      ActiveList := '-';
    if PassiveList = '' then
      PassiveList := '-';
    QuickExecExternalStr := S+' '+ActiveList+' '+PassiveList+' '+DefStr;
    end;

  end { QuickExecExternal };
{-DataCompBoy-}

end.
