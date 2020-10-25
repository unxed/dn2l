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

unit CmdLine;

interface

uses
  Drivers, Defines, Views
  ;

type
  PCommandLine = ^TCommandLine;
  TCommandLine = object(TView)
    Dir: String;
    DeltaX, CurX: LongInt;
    Overwrite: Boolean;
    LineType: (ltNormal, ltFullScreen, ltWindow, ltTimer);
    constructor Init(R: TRect);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Update; virtual;
    procedure GetDir;
    procedure GetData(var S); virtual;
    procedure SetData(var S); virtual;
    function DataSize: Word; virtual;
    procedure SetDirShape;
    procedure QueryCursorVisible; {AK155}
    end;

  (*
     PCmdLine = ^TCmdLine;
     TCmdLine = object(TView)
      procedure Draw; virtual;
     end;

     PCmdWindow = ^TCmdWindow;
     TCmdWindow = object(TWindow)
      constructor Init(R: TRect);
     end;
*)

const
  Separators = [':', '.', ',', '/', '\', '[', ']', '+', '>', '<', '|',
   ';', ' ', '@', '"'];
  StrModified: Boolean = False;
  DDTimer: LongInt = 0;
  TimerMark: Boolean = False;
  CurString: LongInt = 0;
  CmdDisabled: Boolean = False;
  {     CanProcess:  Boolean = True;}
  HideCommandLine: Boolean = False;
  Str: String = '';
  StrCleared: Boolean = False;

implementation
uses
  Dos, Commands, DNApp, Dialogs, Advance, Advance1, Advance2,
  XDblWnd, {$IFDEF TrashCan}Gauges, {$ENDIF}
  Startup, xTime, Messages, DNUtil
  , Microed, Histries, FViewer, FlPanelX
  {$IFDEF SS}, Idlers {$ENDIF}
  , VpSysLow, Lfnvp, UserMenu, Menus
  , DnIni, VPUtils
  ;

const
  CursorMustBeVisible: Boolean = False;
  PrevCmdLineCursorVisible: Boolean = False;

constructor TCommandLine.Init;
  begin
  inherited Init(R);
  EventMask := $FFFF;
  Options := Options {or ofSelectable} or ofPostProcess
    or ofFirstClick {or ofTopSelect};
  DragMode := dmDragMove;
  GetDir;
  Str := '';
  DeltaX := 0;
  CurX := 0;
  GrowMode := gfGrowLoY+gfGrowHiX+gfGrowHiY;
  Overwrite := False;
  LineType := ltNormal;
  end;

function TCommandLine.DataSize;
  begin
  DataSize := SizeOf(String)
  end;

procedure TCommandLine.GetData;
  begin
  String(S) := Str;
  end;
procedure TCommandLine.SetData;
  begin
  Str := String(S);
  DeltaX := 0;
  CurX := 0
  end;

procedure TCommandLine.GetDir;
  var
    MM: record
      case Byte of
        //1: (l: LongInt; S: String[1]);
        1: (l: LongInt; S: String); // fixme: removed [1] to fix build by unxed
        2: (C: Char);
      end;
    D: PDialog;
  begin
  Inc(SkyEnabled);
  repeat
    Abort := False;
    NeedAbort := True;
    lGetDir(0, Dir); {DataCompBoy}
    if Abort then
      begin
      repeat
        MM.l := 0;
        MM.C := GetCurDrive;
        MM.S := MM.C;
        D := PDialog(LoadResource(dlgDiskError));
        if D <> nil then
          begin
          D^.SetData(MM);
          Application^.ExecView(D);
          D^.GetData(MM);
          Dispose(D, Done);
          end;
        UpStr(MM.S);
        if ValidDrive(MM.S[1]) then
          Break;
      until False;
      Abort := True;
      end;
  until not Abort;
  Dec(SkyEnabled);
  NeedAbort := False;
  MakeNoSlash(Dir);
  SetDirShape;
  end { TCommandLine.GetDir };

procedure TCommandLine.SetDirShape;
  begin
  TimerMark := LineType = ltTimer;
  if Dir[1] in ['[', '(', '{'] then
    Dir := Copy(Dir, 2, Length(Dir)-2)
  else if Dir[Length(Dir)] = '>' then
    SetLength(Dir, Length(Dir)-1);
  case LineType of
    ltFullScreen:
      Dir := '['+Dir+']';
    ltWindow:
      Dir := '('+Dir+')';
    ltTimer:
      Dir := '{'+Dir+'}';
    else {case}
      Dir := Dir+'>';
  end {case};
  end;

procedure TCommandLine.QueryCursorVisible; {AK155}
  begin
  CursorMustBeVisible :=
      (State and sfDisabled = 0) and not QuickSearch and
      ( (Desktop^.Current = nil)
      or (TypeOf((Desktop^.Current^)) = TypeOf(TXDoubleWindow))
      or (TypeOf((Desktop^.Current^)) = TypeOf(TUserWindow))
      {$IFDEF TrashCan} or (TypeOf((Desktop^.Current^)) =
           TypeOf(TTrashCan)) {$ENDIF}
      );
  end;

procedure TCommandLine.Update;
  var
    P: TPoint;
    A1, A2: SmallWord;
    //     BB: Boolean;
    CursorStartScanLine, CursorEndScanLine: Integer; //ak155
    CursorVisible: Boolean; //ak155
    CursorMaxY, CursorMinY: Integer;
  const
    OldOverwrite: LongInt = 2;
  begin
  QueryCursorVisible;
  if  (CursorMustBeVisible <> PrevCmdLineCursorVisible)
    { есть повод для работы }
    and (State and (sfDisabled or sfVisible) <> sfVisible)
    { но не надо гасить чужой курсор }
    then
    ResetCursor;
  PrevCmdLineCursorVisible := CursorMustBeVisible;
  if not CursorMustBeVisible then
    Exit;

  { А теперь делаем, чтобы курсор действиельно имел нужный вид }
  SysGetCurPos(A1, A2);
  SysTVGetCurType(CursorStartScanLine, CursorEndScanLine, CursorVisible);
  if
    {$IFDEF SS}(SSaver <> nil) or {$ENDIF}
      (Size.X = 0) or (Size.Y = 0) or
      (Desktop^.GetState(sfActive) and
        (Desktop^.Current <> nil) and
        (Desktop^.Current^.GetState(sfCursorVis) or
        Desktop^.Current^.GetState(sfModal))) or
    MenuActive
  then
    Exit;
  P.X := CurX+Min(Length(Dir), 50)-DeltaX;
  P.Y := 0;
  MakeGlobal(P, P);
  //AK155  BB := not Overwrite  xor (InterfaceData.Options and ouiBlockInsertCursor <> 0);
  {AK155 вызовы программ установки режимов курсора безобидны под OS/2,
но под Win32 приводят к мерзкому миганию курсора. Поэтому я попытался
не вызывать их без надобности. 18.10.2001 }
  if  (OldOverwrite <> Ord(Overwrite)) or not CursorVisible
  then
    begin
    CursorMaxY := Lo(Drivers.CursorLines);
    CursorMinY := 0;
    if not Overwrite then
      CursorMinY := CursorMaxY-1;
    SysTVSetCurType(CursorMinY, CursorMaxY, True);
    OldOverwrite := Ord(Overwrite);
    end;
  if  (A1 <> P.X) or (A2 <> Origin.Y) then
    SysTVSetCurPos(P.X, Origin.Y);
  {/AK155}
  end { TCommandLine.Update };

procedure TCommandLine.SetState;
  begin
  TView.SetState(AState, Enable);
  if AState and (sfActive or sfFocused) <> 0 then
    begin
    DrawView;
    if Enable then
      EnableCommands([cmNext, cmPrev])
    else
      DisableCommands([cmNext, cmPrev])
    end
  end;

procedure TCommandLine.Draw;
  var
    B: array[0..200] of record
      C: Char;
      A: Byte;
      end;
    C1, C2, C3: Word;
    S: ^Str50;
  begin
  if CmdDisabled then
    Exit;
  New(S);
  S^:= {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(Cut(Copy(Dir, 1,
           Length(Dir)-1), 49)+Dir[Length(Dir)]);
  C3 := $0F;
  C1 := $07;
  if Overwrite then
    C2 := $4F
  else
    C2 := $70;
  if CurX < 0 then
    CurX := 0;
  if CurX > Length(Str) then
    CurX := Length(Str);
  if DeltaX > CurX then
    DeltaX := CurX;
  if CurX-DeltaX > Size.X-Min(Length(Dir), 50)-1 then
    DeltaX := CurX-Size.X+Min(Length(Dir), 50)+1;
  MoveChar(B, ' ', C1, Size.X);
  MoveStr(B, S^, C3);
  MoveStr(B[Length(S^)], Copy(Str, DeltaX+1, Size.X-Length(S^)), C1);
  if not MenuActive then
    ShowCursor
  else
    HideCursor;
  if Overwrite xor (InterfaceData.Options and ouiBlockInsertCursor <> 0)
  then
    BlockCursor
  else
    NormalCursor;
  Update;
  WriteLine(0, 0, Size.X, Size.Y, B);
  Dispose(S)
  end { TCommandLine.Draw };

procedure TCommandLine.HandleEvent;
  procedure CE;
    begin
    ClearEvent(Event)
    end;
  procedure CE2;
    begin
    DrawView;
    ClearEvent(Event)
    end;

  procedure CheckSize;
    begin
    if not GetState(sfVisible) and (Str <> '') then
      begin
      ToggleCommandLine(True);
      end;
    end;

  var
    R: TRect;
    P: TPoint;
    i, l, c, ls: Integer;
    s1: String;
    S: String;
    Changed: Boolean;
  label
    EndLFN;
  begin { TCommandLine.HandleEvent }
  inherited HandleEvent(Event);
  if Event.What = evNothing then
    Exit;
  QueryCursorVisible;
  S := Str;
  Changed := False;
  if not CursorMustBeVisible then
    Exit;

  case Event.What of
    evMouseDown, evMouseAuto:
      begin
      if Event.Double then
        begin
        Message(Application, evCommand, cmHistoryList, nil);
        CE
        end;
      MakeLocal(Event.Where, P);
      if P.X >= Min(Length(Dir), 50) then
        if Event.Buttons and mbRightButton <> 0 then
          begin
          if P.X < (Size.X-Min(Length(Dir), 50)) div 2
          then
            Message(@Self, evKeyDown, kbLeft, nil)
          else
            Message(@Self, evKeyDown, kbRight, nil);
          CE2;
          end
        else
          begin
          CurX := DeltaX+P.X-Min(Length(Dir), 50);
          CE2
          end;
      end;
    evCommand:
      case Event.Command of
        cmRereadInfo:
          begin
          GetDir;
          DrawView;
          Update
          end;
        cmInsertName:
          if InterfaceData.Options and ouiHideCmdline = 0 then
            begin
            S := String(Event.InfoPtr^);
            {AK155: обработка длинных имен с пробелами.}
            if  (CurX > 0) and not (Str[CurX] in Separators)
              {and not (S[1] = '"')}
              then
              begin
              Insert(' ', Str, CurX+1);
              Inc(CurX)
              end;
            ls := Length(S);
            c := CurX;
            l := Length(Str);
            if  (S[Length(S)] <> '/') and (Copy(S, Length(S)-1, 2) <> '\"')
            then
              S := S+' ';
            Insert(S, Str, CurX+1);
            Inc(CurX, Length(S));
            if c > 0 then
              begin
              s1 := Copy(Str, c, 2);
              if s1 = '""' then
                begin
                i := c+Length(S)+1;
                while (i <= Length(Str)) and (Str[i] <> '"') do
                  Inc(i);
                if i > Length(Str) then
                  begin
                  Delete(Str, c, 2);
                  Dec(c, 1);
                  Dec(l, 1);
                  Dec(ls, 1);
                  Dec(CurX, 2);
                  end
                else {(Str[i] = '"')}
                  begin
                  Delete(Str, c+ls, 1);
                  Delete(Str, c, 1);
                  Dec(c, 1);
                  Dec(l, 1);
                  Dec(ls, 1);
                  Dec(CurX, 2);
                  goto EndLFN; { чтобы не обрабатывать конец S}
                  end;
                end
              else if s1 = '\"' then
                begin
                i := c-1;
                while (i >= 1) and (Str[i] <> ' ') do
                  Dec(i);
                Delete(Str, c+1, 1);
                Insert('"', Str, i+1);
                Inc(c);
                Inc(l);
                Dec(ls);
                end
              else if (c > 1) and (Copy(Str, c-1, 2) = '\"') then
                begin
                Delete(Str, c, 1);
                Dec(c);
                Dec(l);
                Insert('"', Str, c+Length(S));
                Inc(ls);
                end;
              end;
            if c <> l then
              begin
              s1 := Copy(Str, c+Length(S), 2);
              if s1 = '""' then
                begin
                Delete(Str, c+Length(S), 2);
                Dec(CurX);
                end
              else if s1 = '\"' then
                begin
                i := c+Length(S)-1;
                while (i > c) and (Str[i] <> ' ') do
                  Dec(i);
                Delete(Str, c+Length(S)+1, 1);
                Insert('"', Str, i+1);
                Inc(c);
                Inc(l);
                Dec(ls);
                end
              else if s1[1] = '"' then
                begin
                i := c+Length(S);
                while (i <= Length(Str)) and (Str[i] <> ' ') do
                  Inc(i);
                Insert('"', Str, i);
                Delete(Str, c+Length(S), 1);
                Dec(CurX);
                end;
              end;
EndLFN:
            {/AK155}
            CE2;
            CheckSize;
            end;
        cmClearCommandLine:
          begin
          Str := '';
          CurX := 0;
          DeltaX := 0;
          CE2
          end;
        cmExecCommandLine:
          if InterfaceData.Options and ouiHideCmdline = 0 then
            begin
            if DelSpaces(Str) = '' then
              Exit;
            {AK155: Шатания, которые здесь были (Up-Down), приводили
у тому, что после выполнения команды по Ctrl-E вызывалась из
истории не эта команда, а предыдущая. В чем смысл,
я не понял, поэтому сделал по-простому. В ритлабовском DN
шатания тоже были, но вызывалось, вроде, правильно.
                         StrModified := True;
                         Message(@Self, evKeyDown, kbDown, nil);
                         Message(@Self, evKeyDown, kbUp, nil);
}
            AddCommand(Str);
            CurString := CmdStrings^.Count;
            StrModified := False;
            {/AK155}
            end;
      end {case};
    evKeyDown:
      begin
      if InterfaceData.Options and ouiHideCmdline = 0 then
        case Event.CharCode of
          ^V:
            begin
            Overwrite := not Overwrite;
            CE2
            end;
          ^J:
            begin
            Message(@Self, evKeyDown, kbEnter, nil);
            CE
            end;
          ^A:
            begin
            Message(@Self, evKeyDown, kbCtrlLeft, nil);
            CE
            end;
          ^F:
            begin
            Message(@Self, evKeyDown, kbCtrlRight, nil);
            CE
            end;
          #32..#126, #128..#255:
            begin
            if Overwrite then
              if CurX >= Length(Str) then
                Str := Str+Event.CharCode
              else
                Str[CurX+1] := Event.CharCode
            else
              Insert(Event.CharCode, Str, CurX+1);
            Inc(CurX);
            StrModified := True;
            CE2;
            end;
          else {case}
            case Event.KeyCode of
              kbCtrlBack:
                begin
                while (CurX > 0) and not (Str[CurX] in Separators) do
                  begin
                  Delete(Str, CurX, 1);
                  Dec(CurX)
                  end;
                while (CurX > 0) and (Str[CurX] in Separators) do
                  begin
                  Delete(Str, CurX, 1);
                  Dec(CurX)
                  end;
                CE2;
                end;
              kbAltSlash,
              kbAltShiftSlash
             {$IFDEF Win32}
              , kbAltGraySlash
              , kbAltShiftGraySlash
             {$ENDIF}
              :
                begin
                if (Event.KeyCode = kbAltShiftSlash)
                 {$IFDEF Win32}
                  or (Event.KeyCode = kbAltShiftGraySlash)
                 {$ENDIF}
                   then
                  Dec(LineType)
                else
                  Inc(LineType);
                while (not (OS2exec or Win32exec)
                       and (LineType in [ltWindow, ltFullScreen]))
                     //под виндой в F/S из комстpоки не запустишь
                     or (Win32exec and (LineType = ltFullScreen))
                do
                if (Event.KeyCode = kbAltShiftSlash)
                 {$IFDEF Win32}
                  or (Event.KeyCode = kbAltShiftGraySlash)
                 {$ENDIF}
                  then
                    Dec(LineType)
                  else
                    Inc(LineType);

                if LineType > ltTimer then
                  LineType := ltNormal;
                if LineType < ltNormal then
                  LineType := ltTimer;
                SetDirShape;
                if  (InterfaceData.Options and ouiHideCmdline = 0) and
                  not GetState(sfVisible) and (Str = '')
                then
                  begin
                  Str := ' ';
                  ToggleCommandLine(True);
                  Str := '';
                  end;
                CE2
                end;
              kbESC:
                begin
                if  (Str = '') and (InterfaceData.Options and ouiEsc <> 0)
                then
                  begin
                  if EscForOutputWindow then
                    Message(Application, evCommand, cmShowOutput, nil)
                  else
                    Message(Application, evCommand, cmShowUserScreen, nil);
                  end
                else
                  Str := '';
                CurX := 0;
                DeltaX := 0;
                CE2
                end;
              kbBack:
                begin
                if CurX > 0 then
                  begin
                  Delete(Str, CurX, 1);
                  Dec(CurX);
                  if CurX = 0 then
                    StrCleared := True;
                  CE2
                  end;
                end;
              kbBackUp:
                StrCleared := False;
              kbUp, kbShiftUp, kbCtrlE:
                begin
                if  (Event.KeyCode = kbUp)
                     and (ShiftState and kbCtrlShift <> 0)
                then
                  Exit;
                if StrModified then
                  begin
                  AddCommand(Str);
                  CurString := CmdStrings^.Count;
                  StrModified := False;
                  end;
                if CurString > 0 then
                  Dec(CurString);
                Str := GetCommand(CurString);
                Changed := True;
                CurX := Length(Str);
                if CurX < Size.X-Min(Length(Dir), 50)-1 then
                  DeltaX := 0; {John_SW}
                CE2;
                end;
              kbDown, kbShiftDown, kbCtrlX:
                begin
                if  (Event.KeyCode = kbDown)
                     and (ShiftState and kbCtrlShift <> 0)
                then
                  Exit;
                if StrModified then
                  begin
                  AddCommand(Str);
                  CurString := CmdStrings^.Count;
                  StrModified := False;
                  end;
                Str := GetCommand(CurString);
                if Str <> '' then
                  Inc(CurString);
                Str := GetCommand(CurString);
                Changed := True;
                CurX := Length(Str);
                CE2;
                if CurX < Size.X-Min(Length(Dir), 50)-1 then
                  DeltaX := 0; {John_SW}
                end;
              kbLeft, kbCtrlS, kbShiftLeft:
                begin
                if CurX > 0 then
                  Dec(CurX);
                CE2
                end;
              kbRight, kbCtrlD, kbShiftRight:
                begin
                if CurX < Length(Str) then
                  Inc(CurX);
                CE2
                end;
              kbCtrlIns, kbCtrlShiftIns:
                if Str <> '' then
                  begin
                  PutInClip(Str);
                  CE;
                  end;
              kbShiftIns: {if ShiftState and 4 = 0 then}
                {Cat}
                begin
                GetFromClip(S);
                if S <> '' then
                  Message(@Self, evCommand, cmInsertName, @S);
                CE2;
                end
                {else
                               begin
                                 PutInClip(Str);
                                 CE;
                               end};
              kbIns:
                begin
                Overwrite := not Overwrite;
                CE2
                end;
              kbDel:
                begin
                Delete(Str, CurX+1, 1);
                CE2
                end;
              kbEnd, kbShiftEnd, kbCtrlEnd:
                begin
                CurX := Length(Str);
                CE2
                end;
              kbHome, kbShiftHome, kbCtrlHome:
                begin
                CurX := 0;
                CE2
                end;
              kbCtrlLeft:
                begin
                if not (Str[CurX] in Separators) then
                  while (CurX > 0) and not (Str[CurX] in Separators) do
                    Dec(CurX)

                else
                  begin
                  while (CurX > 0) and (Str[CurX] in Separators) do
                    Dec(CurX);
                  while (CurX > 0) and not (Str[CurX] in Separators) do
                    Dec(CurX)
                  end;
                CE2;
                end;
              kbEnter:
                Message(Application, evCommand, cmExecCommandLine, nil);
              kbCtrlRight:
                begin
                if not (Str[CurX+1] in Separators) then
                  begin
                  while (CurX < Length(Str))
                       and not (Str[CurX+1] in Separators)
                  do
                    Inc(CurX);
                  while (CurX < Length(Str))
                       and (Str[CurX+1] in Separators)
                  do
                    Inc(CurX);
                  end
                else
                  while (CurX < Length(Str))
                       and (Str[CurX+1] in Separators)
                  do
                    Inc(CurX);
                CE2;
                end;
            end {case};
        end {case};
      CheckSize;
      end;
  end {case};
  if Changed then
    StrModified := False
  else
    StrModified := (S <> Str);
  end { TCommandLine.HandleEvent };

(*
constructor TCmdWindow.Init;
begin
 inherited Init(R, 'Command Line', 0);
 GetExtent(R);
 Palette := wpCyanWindow;
 R.Grow(-1, -1);
 Insert(New(PCmdLine, Init(R)));
end;

procedure TCmdLine.Draw;
 var B: TDrawBuffer;
     I: Integer;
     S: String;
begin
end;
*)

end.
