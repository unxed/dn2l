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
unit VideoMan;

interface

var
  ScreenMirror: Pointer;
   {` Выводимая информаци сравнивается с содержимым, и, если всё совпадает,
    вывод в консоль не производится. См. Views._vp.ShowBuffer `}

procedure DetectVideo;

procedure SetScrMode(Mode: Word);
procedure SetBlink(Mode: Boolean);

{ ******** SCREEN MANAGER ******** }

type

  vga_pal = array[1..3, 0..15] of Byte;

const

  { VGA palette }

  VGA_palette: vga_pal = (
    { red   }(0, 0, 0, 7, 42, 42, 42, 42, 19, 21, 0, 19, 63, 63, 53, 63),
    { green }(0, 21, 42, 42, 7, 7, 28, 42, 19, 21, 63, 55, 21, 21, 57, 63),
    { blue  }(0, 42, 21, 42, 7, 42, 0, 42, 19, 63, 32, 55, 21, 63, 18, 63)
    );
  { default values }

  vga_default: vga_pal = (
    { red   }(0, 0, 0, 0, 42, 42, 42, 42, 21, 21, 21, 21, 63, 63, 63, 63),
    { green }(0, 0, 42, 42, 0, 0, 21, 42, 21, 21, 63, 63, 21, 21, 63, 63),
    { blue  }(0, 42, 0, 42, 0, 42, 0, 42, 21, 63, 21, 63, 21, 63, 21, 63)
    );
  { palette slot }
  SL: array[0..15] of Byte = (0, 1, 2, 3, 4, 5, 20, 7, 56, 57, 58, 59,
     60, 61, 62, 63);

  (*****************************************************************
 *
 * Video Device Type
 *
 *****************************************************************)

type

  TVideoType = (
    vtUnknown,
    vtMONO,
    vtCGA,
    vtEGA,
    vtVGA,
    vtXGA,
    vtSVGA,
    vtVBE2
    );

var

  VideoType: TVideoType;

  (*****************************************************************
 *
 * Screen Modes
 *
 *****************************************************************)

const

  smBW40 = $0000;
  smCO40 = $0001; { CGA }
  smBW80 = $0002;
  smCO80 = $0003; { CGA,EGA,VGA }
  smMono = $0007; { CGA,EGA,VGA }

  sm40x12 = $80; { VGA }
  sm40x14 = $81; { VGA }
  sm40x25 = $82; { CGA,EGA,VGA }
  sm40x30 = $83; { VGA }
  sm40x34 = $84; { VGA }
  sm40x43 = $85; { VGA }
  sm40x50 = $86; { VGA }
  sm40x60 = $87; { VGA }

  sm80x12 = $90; { VGA }
  sm80x14 = $91; { VGA }
  sm80x25 = $92; { VGA }
  sm80x30 = $93; { VGA }
  sm80x34 = $94; { VGA }
  sm80x43 = $95; { EGA,VGA }
  sm80x50 = $96; { VGA }
  sm80x60 = $97; { VGA }

  sm94x12 = $A0; { VGA }
  sm94x14 = $A1; { VGA }
  sm94x25 = $A2; { VGA }
  sm94x30 = $A3; { VGA }
  sm94x34 = $A4; { VGA }
  sm94x43 = $A5; { VGA }
  sm94x50 = $A6; { VGA }
  sm94x60 = $A7; { VGA }
  smNonStandard = $00FF;

  NonStandardModes = True;

const

  smSVGALo: Word = $109;
  smSVGAHi: Word = $10C;

  { Screen manager routines }

procedure InitVideo;
procedure DoneVideo;
function SetVideoMode(Mode: Word): Boolean;
procedure ClearScreen;

procedure ResetVGApalette(Update: Boolean); { reset palette to default}
procedure GetPalette(var Buf); { fill buff with palette 64 bytes   }
procedure SetPalette(var Buf); { set palette using buf 64 bytes    }
function VGASystem: Boolean;
{Knave begin}

procedure Set_palette(color, r, g, b: Byte);
procedure Get_palette(color: Byte; var R, G, B: Byte);
{Knave end}

const
  EquipmentOfs = $0010; { Word }
  CrtColsOfs = $004A; { Byte }
  CrtRowsOfs = $0084; { Byte }
  CrtCharOfs = $0085; { Byte }
  CrtInfoOfs = $0087; { Byte }
  CrtPSizOfs = $004C; { Word }

implementation
uses
  {$IFDEF WIN32}Windows, {$ENDIF}Dos, VpSysLow, Drivers, Defines, DNApp,
  DnIni, DnInip, Startup,
  Commands, VPUtils {$IFDEF OS2}, Os2Base {$ENDIF}, Messages
  {$IFDEF DPMI32},Dpmi32, Dpmi32df{$ENDIF}
  ;

var
  StrtCurY1: Integer;
  StrtCurY2: Integer;
  StrtCurVisible: Boolean;

  { ******** SCREEN MANAGER ******** }

procedure DetectVideoType; {JO}
  {$IFDEF OS2}
  var
    VideoConfig: VioConfigInfo;
  begin
  VideoConfig.cb := SizeOf(VideoConfig); {AK155}
  if VioGetConfig(0, VideoConfig, 0) = 0 then
    begin
    case VideoConfig.Adapter of
      0:
        VideoType := vtMONO;
      1:
        VideoType := vtCGA;
      2:
        VideoType := vtEGA;
      3:
        VideoType := vtVGA;
      4..8:
        VideoType := vtUnknown;
      9:
        VideoType := vtXGA;
      else {case}
        VideoType := vtSVGA; {???}
    end {case};
    end
  else
    VideoType := vtUnknown;
  end { DetectVideoType }; {/JO}
{$ENDIF}
{$IFDEF WIN32}
  begin { DetectVideoType }
  VideoType := vtUnknown;
  end { DetectVideoType };
{$ENDIF}
{$IFDEF DPMI32}
  var
    regs: real_mode_call_structure_typ;
  begin { DetectVideoType }
  init_register(regs);
  regs.ax_ := $1C00;
  regs.cx_ := 7;
  intr_realmode(regs, $10);
  if regs.al_ = $1C{VGA} then begin VideoType := vtVga; Exit; end;

  init_register(regs);
  regs.ax_ := $1200;
  regs.bl_ := $32;
  intr_realmode(regs, $10);
  if regs.al_ = $12{MCGA}
  then VideoType := vtVga
  else VideoType := vtEga;

  end { DetectVideoType };
{$ENDIF}

{  vtUnknown, vtMONO, vtCGA, vtEGA, vtVGA, vtXGA, vtSVGA, vtVBE2 }

// Fixes the CRT mode if required

function FixCrtMode(Mode: Word): Word;
  begin
  case Lo(Mode) of
    smMono, smCO80, smBW80:
      FixCrtMode := Mode;
    smNonStandard:
      if NonStandardModes then
        FixCrtMode := Mode
      else
        FixCrtMode := smCO80;
    else {case}
      FixCrtMode := smCO80;
  end {case};
  end;

procedure ReallocBuffers;
  var
    NewUserScreenSize: Word;
  begin
  NewUserScreenSize := ScreenWidth*ScreenHeight*2;
  if NewUserScreenSize <> UserScreenSize then
    begin
    UserScreenSize := NewUserScreenSize;
    if UserScreen <> nil then
      begin
      FreeMem(UserScreen);
      UserScreen := nil;
      end;
    if ScreenMirror <> nil then
      begin
      FreeMem(ScreenMirror);
      ScreenMirror := nil;
      end;
    end;
  UserScreenWidth := ScreenWidth;
  if UserScreen = nil then
    GetMem(UserScreen, UserScreenSize);
  if ScreenMirror = nil then
    GetMem(ScreenMirror, UserScreenSize);
  FillChar(ScreenMirror^, UserScreenSize, 0);
  end;

// Updates the CRT-related variables

procedure SetCrtData;
  var
    BufSize: SmallWord;
    Y1, Y2: Integer;
    Visible: Boolean;
    SrcSize: TSysPoint;
  begin
  SysTvGetScrMode(@SrcSize, True);
(*
  {AK155 при SrcSize.Y=300 (w2k, wXP) DN падает}
  if  (SrcSize.Y > 100) or (SrcSize.X*SrcSize.Y*2 > 32768) then
    begin
    SysTVSetScrMode(3);
    SysTvGetScrMode(@SrcSize, True);
    end;
  {/AK155}
*)
  ScreenHeight := SrcSize.Y;
  ScreenWidth := SrcSize.X;
  ReallocBuffers;
  {JO}
  if SrcSize.X = 80 then
    case SrcSize.Y of
      25:
        ScreenMode := sm80x25;
      43:
        ScreenMode := sm80x43;
      50:
        ScreenMode := sm80x50;
      30:
        ScreenMode := sm80x30;
      34:
        ScreenMode := sm80x34;
      60:
        ScreenMode := sm80x60;
      else {case}
        ScreenMode := SrcSize.X*256+SrcSize.Y;
    end
  else
    ScreenMode := SrcSize.X*256+SrcSize.Y;
  {/JO}
  ShowMouse;
  HiResScreen := True;
  ScreenBuffer := SysTVGetSrcBuf;
  SysTVGetCurType(Y1, Y2, Visible);
  WordRec(CursorLines).Hi := Y1;
  WordRec(CursorLines).Lo := Y2;
  SysTVSetCurType(Y1, Y2, False); // Hide cursor
  end { SetCrtData };

// Detects video modes

procedure DetectVideo;
  begin
  ScreenMode := FixCrtMode(SysTvGetScrMode(nil, True));
  end;

// Sets the video mode. Mode is one of the constants smCO80, smBW80, or smMono,
// optionally with smFont8x8 added to select 43- or 50-line mode on an EGA or
// VGA. SetVideoMode initializes the same variables as InitVideo (except for
// the StartupMode variable, which isn't affected).

function SetVideoMode(Mode: Word): Boolean;
  var
    Cols, Rows: Word;
    CursorSize: Word;
  begin
  Result := False;
  {$IFDEF OS2}
  if VideoType < vtEGA then
    Exit;
  {$ENDIF}
  Cols := 80;
  Rows := 0;
  case Mode of
    sm80x25:
      Rows := 25;
    sm80x43:
      Rows := 43;
    sm80x50:
      Rows := 50;
    sm80x30:
      Rows := 30;
    sm80x34:
      Rows := 34;
    sm80x60:
      Rows := 60;
    $140A..$FFFE:
      begin {минимальный размер окна 20x10, меньше просто нет смысла}
      Rows := Lo(Mode);
      Cols := Hi(Mode);
      if Rows < 10 then
        Rows := 10;
      end;
  end {case};
{$IFDEF DPMI32}
{
piwamoto: current mode == target mode
VPSYSD32.SysSetVideoMode всегда чистит экран при смене видеорежима и делает
кучу проверок, что тормозит и не нужно нам в случае если размер экрана до
запуска DN и его рабочий размер совпадают
}
  if (Rows = Byte(MemL[seg0040+$84] + 1)) and
     (Cols = Byte(MemL[seg0040+$4A]))
    then Exit;
{$ENDIF}
  if Rows <> 0 then
    if SysSetVideoMode(Cols, Rows) then
      begin
      Result := True;
      ScreenHeight := Rows;
      ScreenWidth := Cols;
      ScreenMode := Mode;
      {AK155 Число видеострок курсора зависит от видеорежима, а в окне - еще и
от операционки. Но сразу после установки режима курсор прижат к нижнему
краю знакоместа, так что lo(Drivers.CursorLines) равно максимальному
номеру видеостроки знака. Это используется при изменениях вида курсора.}
      Drivers.CursorLines := GetCursorSize;
      {/AK155}
      end;
//  ReallocBuffers;
  end { SetVideoMode };

// Initializes Turbo Vision's video manager. Saves the current screen
// mode in StartupMode, and switches the screen to the mode indicated by
// ScreenMode. The ScreenWidth, ScreenHeight, HiResScreen, ScreenBuffer,
// and CursorLines variables are updated accordingly.InitVideo is called
// automatically by TApplication.Init.

procedure InitVideo;
  var
    X, Y: SmallWord; {KV}
    {$IFDEF WIN32}s, c: TCOORD;
    r: TSMALLRECT;
    Bf: Pointer;

  procedure Rebuf(Bf: Pointer);
    assembler; {$USES ESI, EDI, ECX}
  asm
   cld
   mov esi,Bf
   mov edi,UserScreen
   mov ecx,UserScreenSize
@@1:
   lodsw  { символ в AL, мусор в AH }
   stosb

{AK155 нам не нужна шапка-невидимка, когда атрибуты нулевые
 (такое бывает под W98 при чтении экрана новой консоли) }
   lodsw  { атрибут в AL, мусор в AH }
   dec  ecx
   or  AL,AL
   jnz  @@2
   mov  al, 7
@@2:
   stosb

   loop @@1
  end;
  {$ENDIF}
  begin { InitVideo }
  SysTVGetCurType(StrtCurY1, StrtCurY2, StrtCurVisible);
  SysTVInitCursor; {KV}
  SysGetCurPos(X, Y); {KV}
  WordRec(OldCursorPos).Lo := X; {KV}
  WordRec(OldCursorPos).Hi := Y; {KV}
  {JO}
  DetectVideoType;
//  ReallocBuffers;
  if  (StartupData.Load and osuRestoreScrMode <> 0) then
    begin
    {$IFDEF OS2}
    if not PMWindowed then
      begin
      {$ENDIF}
      ScreenMode := NonVIOScreenMode;
      {$IFDEF OS2}
      end
    else
      ScreenMode := VIOScreenMode;
    {$ENDIF}
    SetVideoMode(ScreenMode);
    end;
  {/JO}
  SetCrtData;
  if not ScreenSaved then
    begin
    {$IFDEF WIN32} {?} {DataCompBoy: how to do this in OS/2 ???}
    GetMem(Bf, UserScreenSize shl 1);
    s.X := ScreenWidth;
    s.Y := ScreenHeight;
    c.X := 0;
    c.Y := 0;
    r.Left := 0;
    r.Right := ScreenWidth;
    r.Top := 0;
    r.Bottom := ScreenHeight;
    ReadConsoleOutput(SysFileStdOut, Bf, s, c, r);
    Rebuf(Bf);
    FreeMem(Bf, UserScreenSize shl 1);
    {$ELSE}
    Move(ScreenBuffer^, UserScreen^, UserScreenSize); {JO}
    {$ENDIF}
    ScreenSaved := True;
    end;
  if  (StartupData.Load and osuResetPalette <> 0) and VGASystem
  then
    SetPalette(VGA_palette);
  SetBlink(CurrentBlink);
  end { InitVideo };

// Terminates Turbo Vision's video manager by restoring the initial
// screen mode, clearing the screen, and restoring the cursor. Called
// automatically by TApplication.Done.

procedure DoneVideo;
  begin
  FillChar(ScreenBuffer^, ScreenWidth*ScreenHeight*2, 0);
  {JO: нужно, чтобы куски панелей не "линяли" в UserScreen}
  if UserScreen <> nil then
    Move(UserScreen^, ScreenBuffer^, UserScreenSize);
  FreeMem(UserScreen, UserScreenSize); {Cat}
  UserScreen := nil; {Cat}
  ScreenSaved := False; {Cat}
  {$IFDEF Win32}
  Sleep(1); {AK155}
  { Без этого под Win9x что-то куда-то не успевает переписаться и при
     уменьшении высоты окна в пользовательский экран попадают клочья
     от нижней части старого (бОльшего) окна}
  {$ENDIF}
  SysTvShowBuf(0, UserScreenSize);
  SysTVSetCurType(StrtCurY1, StrtCurY2, StrtCurVisible);
  if WordRec(OldCursorPos).Hi > ScreenHeight-1 then
    WordRec(OldCursorPos).Hi := ScreenHeight-1; {KV}
  if WordRec(OldCursorPos).Lo > ScreenWidth-1 then
    WordRec(OldCursorPos).Lo := ScreenWidth-1; {KV}
  {JO: под OS/2 после смены видеорежима SysGetCurPos даёт нулевые координаты}
  {    как с этим бороться - пока не знаю                                   }
  if OldCursorPos <> 0 then
    SysTVSetCurPos(WordRec(OldCursorPos).Lo, WordRec(OldCursorPos).Hi);
  {KV}
  {$IFDEF Win32}
  SysCtrlSleep(1); {KV}
  {$ENDIF}
  {$IFDEF OS2}
  {JO: это работает действеннее, чем SysTVSetCurPos, который }
  {перестаёт нормально работать после смены видеорежима      }
  if OldCursorPos = 0 then
    Writeln;
  {$ENDIF}
  end { DoneVideo };

// Clears the screen, moves cursor to the top left corner

procedure ClearScreen;
  begin
  SysTVClrScr;
  {$IFDEF Win32}
  SysCtrlSleep(50);
  {Cat: даём время курсорной нитке установить курсор в угол}
  {$ENDIF}
  end;

{$IFDEF OS2} {JO} {установка VGA-палитры в окне}

procedure ResetVGApalette(Update: Boolean);
  begin
  if Update then
    VGA_palette := vga_default;
  SetPalette(vga_default);
  end;

function VGASystem: Boolean;
  begin
  VGASystem := VideoType >= vtVGA; { PZ 2000.06.14 }
  end;

type
  RGB = record
    Red, green, Blue: Byte
    end;
  VGAPalette = array[Byte] of RGB;

procedure SetGetVGAPal(var p: VGAPalette; Get: Boolean);
  const
    ColorReg: VioColorReg = (
      cb: SizeOf(VioColorReg); // Size of this structure
      RType: 3; // 3 = Color registers
      FirstColorReg: 0; // Specifies the first color registers
      NumColorRegs: 256; // Number of color registers
      ColorRegAddr: nil // Pointer to array with color values
      );
  begin
  with ColorReg do
    begin
    ColorRegAddr := @p;
    FLatToSel(ColorRegAddr);
    end;
  if Get then
    VioGetState(ColorReg, 0)
  else
    VioSetState(ColorReg, 0)
  end;

procedure Set_palette(color, r, g, b: Byte);
  var
    curpal: VGAPalette;
  begin
  SetGetVGAPal(curpal, True);
  with curpal[color] do
    begin
    Red := r;
    green := g;
    Blue := b;
    end;
  SetGetVGAPal(curpal, False);
  end;

procedure Get_palette(color: Byte; var R, G, B: Byte);
  var
    curpal: VGAPalette;
  begin
  SetGetVGAPal(curpal, True);
  with curpal[color] do
    begin
    R := Red;
    G := green;
    B := Blue;
    end;
  end;

procedure GetPalette(var Buf);
  var
    PAL: vga_pal absolute Buf;
    curpal: VGAPalette;
    I: Byte;
  begin
  if not (VGASystem and not PMWindowed) then
    Exit;
  PAL := vga_default;
  SetGetVGAPal(curpal, True);
  for I := 0 to 15 do
    with curpal[SL[I]] do
      begin
      PAL[1, I] := Red;
      PAL[2, I] := green;
      PAL[3, I] := Blue;
      end;
  end;

procedure SetPalette(var Buf);
  var
    PAL: vga_pal absolute Buf;
    curpal: VGAPalette;
    I: Byte;
  begin
  if not (VGASystem and not PMWindowed) then
    Exit;
  SetGetVGAPal(curpal, True);
  for I := 0 to 15 do
    with curpal[SL[I]] do
      begin
      Red := PAL[1, I];
      green := PAL[2, I];
      Blue := PAL[3, I];
      end;
  SetGetVGAPal(curpal, False);
  end;

procedure SetBlink(Mode: Boolean); {JO}
  var
    I: VioIntensity;
  begin
  with I do
    begin
    cb := SizeOf(VioIntensity);
    RType := 2;
    if Mode then
      fs := 0
    else
      fs := 1;
    end;
  VioSetState(I, TVVioHandle);
  end;

{$ENDIF}

{$IFDEF WIN32}
procedure ResetVGApalette(Update: Boolean);
  begin
  end;
procedure GetPalette(var Buf);
  begin
  end;
procedure SetPalette(var Buf);
  begin
  end;
function VGASystem: Boolean;
  begin
  end;
procedure Set_palette(color, r, g, b: Byte);
  begin
  end;
procedure Get_palette(color: Byte; var R, G, B: Byte);
  begin
  end;
procedure SetBlink(Mode: Boolean);
  begin
  end;
{$ENDIF}

{$IFDEF DPMI32}
procedure ResetVGApalette(Update: Boolean);
begin
  if Update then VGA_palette := VGA_default;
  SetPalette(VGA_default);
end;

procedure GetPalette(var Buf);
var
  PAL: VGA_pal absolute Buf;
  I: byte;
begin
  PAL:=VGA_default;
  if not VGAsystem then Exit;
  for I:=0 to 15 do
    Get_palette(sl[I], pal[1,i], pal[2,i], pal[3,i]);
end;

procedure SetPalette(var Buf);
var
  PAL: VGA_pal absolute Buf;
  I: byte;
begin
  if not VGAsystem then Exit;
  for i:=0 to 15 do
    Set_palette(sl[i], pal[1,i], pal[2,i], pal[3,i]);
end;

function VGASystem: Boolean;
begin
  VGASystem := VideoType >= vtVGA;
end;

procedure Set_palette(color, r, g, b: Byte);
var
  regs: real_mode_call_structure_typ;
begin
  init_register(regs);
  regs.ax_ := $1010;
  regs.bx_ := color;
  regs.dh_ := r;
  regs.ch_ := g;
  regs.cl_ := b;
  intr_realmode(regs, $10);
end;

procedure Get_palette(color: Byte; var R, G, B: Byte);
var
  regs: real_mode_call_structure_typ;
begin
  init_register(regs);
  regs.ax_ := $1015;
  regs.bx_ := color;
  intr_realmode(regs, $10);
  r := regs.dh_;
  g := regs.ch_;
  b := regs.cl_;
end;

procedure SetBlink(Mode: Boolean);
var
  regs: real_mode_call_structure_typ;
begin
  init_register(regs);
  regs.ax_ := $1003;
  regs.bl_ := Byte(Mode);
  intr_realmode(regs, $10);
end;
{$ENDIF}

{Procedure GetCrtMode;                                begin end;}

procedure SetScrMode(Mode: Word);
  var
    R, R1, A: TRect;
  label Ex;
  begin
  with PApplication(Application)^ do
    begin
    if Mode = ScreenMode then
      goto Ex;
    GetExtent(R1);
    Clock^.GetBounds(A);
    if not SetScreenMode(Mode) then
      begin
      SetBlink(CurrentBlink);
      if  (StartupData.Load and osuResetPalette <> 0) and VGASystem
      then
        SetPalette(VGA_palette);
      Redraw;
      MessageBox(GetString(dlNotValidForCurSession), nil,
         mfError+mfOKButton);
      Exit;
      end;
    GetExtent(R);
    A.A.X := Round(A.A.X*R.B.X/R1.B.X);
    if A.B.Y = R1.B.Y then
      A.A.Y := R.B.Y-1
    else
      A.A.Y := Round(A.A.Y*R.B.Y/R1.B.Y);
    if ShowSeconds then
      A.B.X := A.A.X+10
    else
      A.B.X := A.A.X+7;
    A.B.Y := A.A.Y+1;
    Clock^.Locate(A);
    SetBlink(CurrentBlink);
    if  (StartupData.Load and osuResetPalette <> 0) and VGASystem
    then
      SetPalette(VGA_palette);
    Redraw;
    end;
Ex:
  {$IFDEF OS2}
  if not PMWindowed then
    begin
    {$ENDIF}
    NonVIOScreenMode := ScreenMode;
    SaveDnIniSettings(@NonVIOScreenMode)
    {$IFDEF OS2}
    end
  else
    begin
    VIOScreenMode := ScreenMode;
    SaveDnIniSettings(@VIOScreenMode);
    end
    {$ENDIF};
  DoneIniEngine;
  end { SetScrMode };

end.
