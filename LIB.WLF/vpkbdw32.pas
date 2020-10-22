//
// This source file is included as a unit used by VpSysW32.Pas, and
// implements default keyboard handling for Win32 Virtual Pascal programs.
//
// By defining the conditional define KEYDLL, compiling this file results
// in a DLL, VpKbdW32.Dll to be produced.  When this DLL is present, the
// keyboard handling functions of the DLL supercede those statically
// linked into the program, allowing for advanced customization of the
// keyboard handler.
// Some change for russian by Peter S. Voronov aka Chem O'Dun
// Some change by Jaroslaw Osadtchiy (JO) and Alexey Korop (AK155)
// Some change by Aleksej Kozlov (Cat)
// Some change by Sergey Biryukov (Flash)

{$Ifdef KeyDll} library {$Else} unit {$Endif}
  VpKbdW32;

{$I ..\STDEFINE.INC}
{&Delphi+,S-,R-,Optimize+,J+,X+}

{$Ifndef KeyDll}
interface
{$Endif}

uses
  Windows;

const AltGreyAsAlt: Boolean = True; {JO}

{$Ifndef KeyDll}
procedure KbdInit(var _pSysKeyCount, _pSysKeyQue, _pSysShiftState, _pSysMouCount, _pSysMouQue);
procedure KbdUpdateEventQueues;
function GetWinShiftState2: byte;

var
  WindowNotFocused: Boolean;
    { Эта переменная отслеживает потерю и возврат фокуса окном DN }
const
  WheelScrollLines: Integer = 3;
    { число "вертикальных стрелок", генерируемых из одного щелчка
    мышиного колеса. Если -1 - генерировать не стрелки, а страницы.
    Можно установить снаружи, значение желательно брать из
    HCU\Control Panel\Desktop\WheelScrollLines. }

implementation
{$Endif}

const
  Ctrl_Pressed = Right_Ctrl_Pressed or Left_Ctrl_Pressed;
  Alt_Pressed = Right_Alt_Pressed or Left_Alt_Pressed;
  AltCtrl_Pressed = Alt_Pressed or Ctrl_Pressed;

// Type definitions duplicated from VpSysLow
type
  PSysPoint = ^TSysPoint;
  TSysPoint = packed record
    X,Y: SmallInt;
  end;

  TSysMouseEvent = packed record
    smeTime:    Longint;
    smePos:     TSysPoint;
    smeButtons: Byte;
  end;

  TSysKeyEvent = packed record
    skeKeyCode:    SmallWord;
    skeShiftState: Byte;
  end;

  TSysMouQUeue = array[0..15] of TSysMouseEvent;
  PSysMouQueue = ^TSysMouQueue;
  TSysKeyQueue = array[0..15] of TSysKeyEvent;
  PSysKeyQueue = ^tSysKeyQueue;

// Variables holding the keyboard and mouse queues
var
  SysKeyQue: TSysKeyQueue;
  SysMouQue: TSysMouQueue;

const
  SysKeyCount  : Integer = 0;
  SysMouCount  : Integer = 0;
  SysShiftState: Byte = 0;
  SysPlatform  : Integer = 0;

// Initialisation - set VpSysW32 pointer variables to point to to here
procedure KbdInit(var _pSysKeyCount, _pSysKeyQue, _pSysShiftState, _pSysMouCount, _pSysMouQue);
var
  OSVersionInfo: TOSVersionInfo;
begin
  // Link VpSysW32 variables to the ones defined here
  Pointer(_pSysKeyCount)   := @SysKeyCount;
  Pointer(_pSysKeyQue)     := @SysKeyQue;
  Pointer(_pSysShiftState) := @SysShiftState;
  Pointer(_pSysMouCount)   := @SysMouCount;
  Pointer(_pSysMouQue)     := @SysMouQue;

  // Determine the operating system (Win95/98 or Windows NT)
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  SysPlatform := OSVersionInfo.dwPlatformId;
  SetConsoleMode(SysFileStdIn, ENABLE_MOUSE_INPUT); {Alexey Suhinin}
end;

{ Translate Windows CtrlKeys field to the low byte of OS/2-compatible
  ShiftStates, with the following valid values:
  Bit Value
   0  0001 Right Shift
   1  0002 Left shift
   2  0004 Either Ctrl
   3  0008 Either Alt
   4  0010 ScrollLock
   5  0020 NumLock
   6  0040 CapsLock
   --- unused:
   7  0080 Insert
   8  0100 Left Ctrl
   9  0200 Left Alt
  10  0400 Right Ctrl
  11  0800 Right Alt (AltGr)
  12  1000 ScrollLock down
  13  2000 NumLock down
  14  4000 CapsLock down
  15  8000 SysReq down
}

const
  WinShiftState2: byte = 0; {как старший байт KbdGetStatus для OS/2}
const
  AdvShiftKeys: array[0..6] of byte =
    ( LEFT_CTRL_PRESSED
    , LEFT_ALT_PRESSED
    , RIGHT_CTRL_PRESSED
    , RIGHT_ALT_PRESSED
    , SCROLLLOCK_ON
    , NUMLOCK_ON
    , CAPSLOCK_ON
    );

function GetWinShiftState2: byte;
  begin Result := WinShiftState2 end;

function CtrlKeysToShiftState(CtrlKeys: Integer): Byte;
var
  i: integer;
begin
  WinShiftState2 := 0;
  for i := 0 to High(AdvShiftKeys) do
    if CtrlKeys and AdvShiftKeys[i] <> 0 then
      WinShiftState2 := WinShiftState2 or (1 shl i);
  Result := 0;
  if CtrlKeys and (RIGHT_ALT_PRESSED + LEFT_ALT_PRESSED) <> 0 then
    Result := $0008;
  if CtrlKeys and (RIGHT_CTRL_PRESSED + LEFT_CTRL_PRESSED) <> 0 then
    Inc(Result, $0004);
  if CtrlKeys and SHIFT_PRESSED <> 0 then
    Inc(Result, $0003);
  if CtrlKeys and SCROLLLOCK_ON <> 0 then
    Inc(Result, $0010);
  if CtrlKeys and NUMLOCK_ON <> 0 then
    Inc(Result, $0020);
  if CtrlKeys and CAPSLOCK_ON <> 0 then
    Inc(Result, $0040);
end;

{$IFNDEF OLDCRT}

{
  Revised implemenatation of TranslateKeyCode, where every
  effort has been made to ensure that the keys returned are identical
  for Win32 and OS/2, and that national language characters are supported.

  In Win32, "dead" keys (diacritics) are returned as normal characters,
  with nothing to distinguish them from normal keys.  This means that when
  pressing a dead key, a character will appear - but the dead key will
  still be in memory and be activated by the next keystroke.  A solution
  is wanted!
}

function TranslateKeyCode(KeyCode,ScanCode,CharCode: Byte; ShiftState: Integer): Word;
const
  // Table for CTRL + some special keys
  CtrlTable: array[71..83] of Byte =
    (119, 141, 132, 142, 115, {71..75: Home, Up, PgUp, GrayMinus, Left}
    143, 116, 144, 117, 145,{76..80: NumPad5, Right, GrayPlus, End, Down}
    118, $92, 147); {PgDn, Ins, Del}
  // Table for ALT (or Alt+Ctrl) + some special keys
  AltTable: array[71..83] of Byte =
    ($97, $98, $99, $4A, $9B,
    $4C, $9D, $4E, $9F, $A0,
    $A1, $A2, $A3);
begin

  if  (ScanCode  = $1C) then
    begin { Enter }
    if ((ShiftState and Ctrl_Pressed) <> 0) then
      begin
      if (ShiftState and Shift_Pressed) <> 0 then
        CharCode := $A  {AK155  Ctrl-Shift-Enter under NT}
      else if (ShiftState and Alt_Pressed) <> 0 then
        CharCode := 0;  {AK155  Ctrl-Alt-Enter under W98}
      end;
    Result := ScanCode shl 8 or CharCode;
    Exit;
    end;

  if  ScanCode  in [$1A, $1B] then
    begin { '[', ']' }
    if (ShiftState and Alt_Pressed) <> 0 then
      CharCode := 0;  {AK155  Ctrl-Alt-[ under W98}
    Result := ScanCode shl 8 or CharCode;
    Exit;
    end;

  // First we check whether the system suggests a printable character
  if (CharCode <> 0) and (ShiftState and Left_Alt_Pressed = 0) and
  // JO: On some national keyboard layouts we need to treat Right Alt
  //     as an auxiliary key to enter national letters
     ((AltGreyAsAlt = False) or (ShiftState and Right_Alt_Pressed = 0)) {JO}
  then
    begin
      {Flash 07-02-2004, AK155 29-02-04:
        Для работы эмуляции нажатия BS, например, в Punto Switcher}
      if (CharCode = 8) and (ScanCode = 0) then
        begin // Special treatment for BackSpace
          Result := $E08;
          exit;
        end;
      if (CharCode = 9) and (ShiftState and Shift_Pressed <> 0) then
        begin
          // Special treatment for SHIFT + TAB
          Result := 15 shl 8;
          Exit;
        end
      else
        // Special treatment for Ctrl-keys
        if (CharCode <> $E0) or ((CharCode = $E0) and (KeyCode = $48) and
            ((ScanCode = $23) or
            {Flash 07-02-2004: Для работы эмуляции нажатия русской "р"}
            (ScanCode = 0)) and
            (ShiftState and AltCtrl_Pressed = 0))
        then
          begin
            if ShiftState and $100 <> 0 then
              ScanCode := $E0; // Signal special character
            if (ShiftState and Right_Alt_Pressed <> 0) and (CharCode = $F0) then
              CharCode := $00;
            // Ordinary characters
            Result := ScanCode shl 8 or CharCode;
            Exit;
          end;
    end;

  // Default: No recognised key pressed: Return zero. The
  // calling function will not insert this into the input buffer.

  Result := 0;

  // No printable character. Return an extended keystroke based on the scan code
  case ScanCode of
        1: {AK155 Win9x: Esc with CapsLock contain CharCode=0 }
      if ShiftState = CAPSLOCK_ON then
        Result := $11B;

    2..13: // ALT + number keys, ALT + EQ, ALT + MINUS, ALT + =
     if ShiftState and Alt_Pressed <> 0 then
       Result := (ScanCode + 118) shl 8
     else
       Result := ScanCode shl 8 + CharCode;

    15: // AK155 TurboVision code for Ctrl-Tab
     if ShiftState and Ctrl_Pressed <> 0 then
       Result := $9400
     else if ShiftState and Alt_Pressed <> 0 then {JO: Alt-Tab}
       Result := $A500;

{
  // Test code; should not be necessary
    27:
    begin
      // ∙^~
      if ShiftState and Left_Alt_Pressed <> 0 then
        Result := ScanCode shl 8
      else if ShiftState and Right_Alt_Pressed <> 0 then
        Result := Ord ('~')
      else if ShiftState and Shift_Pressed <> 0 then
        Result := Ord ('^')
      else
        Result := Ord ('∙');

      Exit;
    end;
}
    14, 16..27, 29..52:
      // ALT + BS, ALT + characters, ALT + COLON, ALT + DOT
      if ShiftState and Alt_Pressed <> 0 then
        Result := ScanCode shl 8;

    53:
      // ALT + grey DIV
      if ShiftState and Alt_Pressed <> 0 then
        Result := $a400
      else if ShiftState and Ctrl_Pressed <> 0 then
        Result := $9500
      else
        Result := ScanCode shl 8;

    55:
      // ALT + grey MUL
      if ShiftState and Alt_Pressed <> 0 then
        Result := $3700
      else if ShiftState and Ctrl_Pressed <> 0 then
        Result := $9600
      else
        Result := ScanCode shl 8;

    57: // JO: Alt-Space
     if ShiftState and Alt_Pressed <> 0 then
       Result := $3920;

    59..68:
      // FN keys 1 to 10
      if ShiftState and Alt_Pressed <> 0 then
        Result := (ScanCode + 45) shl 8
      else if ShiftState and Ctrl_Pressed <> 0 then
        Result := (ScanCode + 35) shl 8
      else if ShiftState and Shift_Pressed > 0 then
        Result := (ScanCode + 25) shl 8
      else
        Result := ScanCode shl 8;

    71..83:
      // INS, DEL, HOME, END, PGUP, PGDN, Gray+, Gray-, Num5 and CURSOR keys
      if ShiftState and Alt_Pressed <> 0 then
        Result := AltTable[ScanCode] shl 8 // Alt or Alt+Ctrl
      else if (ShiftState and Ctrl_Pressed <> 0) then
        Result := CtrlTable[ScanCode] shl 8 // Ctrl w/o Alt
      else
        Result := ScanCode shl 8;

    87, 88:
      // FN keys 11 and 12
      if ShiftState and Alt_Pressed <> 0 then
        Result := (ScanCode + 52) shl 8
      else if ShiftState and Ctrl_Pressed <> 0 then
        Result := (ScanCode + 50) shl 8
      else if ShiftState and Shift_Pressed <> 0 then
        Result := (ScanCode + 48) shl 8
      else
        Result := (ScanCode + 46) shl 8;

    91: // Cat: LeftSuxx
      Result := $EC00;
    92: // Cat: RightSuxx
      Result := $ED00;
    93: // Cat: MenuSuxx
      Result := $EE00;
  end; // Case ScanCode
end;

{$ELSE}

function TranslateKeyCode(KeyCode,ScanCode,CharCode,ShiftState: Byte): Word;
const
  FnKeys: array[0..12*4-1] of Byte =
   // F1  F2   F3   F4   F5   F6   F7   F8   F9   F10  F11  F12
    ($3B, $3C, $3D, $3E, $3F, $40, $41, $42, $43, $44, $85, $86,  // None
     $54, $55, $56, $57, $58, $59, $5A, $5B, $5C, $5D, $87, $88,  // Shift
     $5E, $5F, $60, $61, $62, $63, $64, $65, $66, $67, $89, $8A,  // Ctrl
     $68, $69, $6A, $6B, $6C, $6D, $6E, $6F, $70, $71, $8B, $8C); // Alt
  CtrlNumKeys: array[Ord('0')..Ord('9')] of Byte =
   // 0   1    2    3    4    5    6    7    8    9
    ($FF, $FF, $00, $FF, $FF, $FF, $1E, $FF, $FF, $FF);
  AltNumKeys: array[Ord('0')..Ord('9')] of Byte =
    ($81, $78, $79, $7A, $7B, $7C, $7D, $7E, $7F, $80);
  CtrlGreyKeys: array[VK_NUMPAD0..VK_DIVIDE] of Byte =
   // 0   1    2    3    4    5    6    7    8    9    *    +    SEP   -   .    /
    ($92, $75, $91, $76, $73, $8F, $74, $77, $8D, $84, $96, $90, $00, $8E, $93, $95);
  AltGreyKeys: array[VK_NUMPAD0..VK_DIVIDE] of Byte =
    ($A2, $9F, $A0, $A1, $9B, $00, $9D, $97, $98, $99, $37, $4E, $00, $4A, $A3, $A4);
  TransTable1: array[0..13] of record
    VK: Word;
    NS: Word;
    SS: Word;
    CS: Word;
    AL: Word;
  end =
   //Virtual Key        None      Shift     Ctrl      Alt
   ((VK:VK_BACK       ; NS:$0E08; SS:$0E08; CS:$0E7F; AL:$0E00),
    (VK:VK_LEFT       ; NS:$4BE0; SS:$4BE0; CS:$73E0; AL:$9B00),
    (VK:VK_RIGHT      ; NS:$4DE0; SS:$4DE0; CS:$74E0; AL:$9D00),
    (VK:VK_END        ; NS:$4FE0; SS:$4FE0; CS:$75E0; AL:$9F00),
    (VK:VK_NEXT       ; NS:$51E0; SS:$51E0; CS:$76E0; AL:$A100),
    (VK:VK_HOME       ; NS:$47E0; SS:$47E0; CS:$77E0; AL:$9700),
    (VK:VK_PRIOR      ; NS:$49E0; SS:$49E0; CS:$84E0; AL:$9900),
    (VK:VK_UP         ; NS:$48E0; SS:$48E0; CS:$8DE0; AL:$9800),
    (VK:VK_DOWN       ; NS:$50E0; SS:$50E0; CS:$91E0; AL:$A000),
    (VK:VK_INSERT     ; NS:$52E0; SS:$52E0; CS:$92E0; AL:$A200),
    (VK:VK_DELETE     ; NS:$53E0; SS:$53E0; CS:$93E0; AL:$A300),
    (VK:VK_TAB        ; NS:$0F09; SS:$0F00; CS:$9400; AL:$A500),
    (VK:VK_SPACE      ; NS:$3920; SS:$3920; CS:$3920; AL:$3920),
    (VK:VK_RETURN     ; NS:$1C0D; SS:$1C0D; CS:$1C0A; AL:$1C00)
   );
  TransTable2: array[0..10] of record
    VK: Word;
    CS: Word;
    AL: Word;
  end =
{    VirtKey Ctrl      Alt  }
   ((VK:$DF; CS:$291C; AL:$2900), // '`'
    (VK:$BD; CS:$0C1F; AL:$8200), // '-'
    (VK:$BB; CS:$0DFF; AL:$8300), // '='
    (VK:$DB; CS:$1A1B; AL:$1A00), // '['
    (VK:$DD; CS:$1B1D; AL:$1B00), // ']'
    (VK:$BA; CS:$27FF; AL:$2700), // ';'
    (VK:$C0; CS:$28FF; AL:$2800), // '@'
    (VK:$DE; CS:$2BFF; AL:$2B00), // '#'
    (VK:$BC; CS:$332C; AL:$3300), // '<'
    (VK:$BE; CS:$34FF; AL:$3400), // '>'
    (VK:$BF; CS:$35FF; AL:$3500)  // '?'
   );
var
  I: Integer;
begin
  Result := 0;
  case KeyCode of
    VK_F1..VK_F12:
      begin
        Dec(KeyCode, VK_F1);
        if ShiftState and $0003 <> 0 then
          Inc(KeyCode, 12*1);             // Shift
        if ShiftState and $0004 <> 0 then
          Inc(KeyCode, 12*2);             // Ctrl
        if ShiftState and $0008 <> 0 then
          Inc(KeyCode, 12*3);             // Alt
        Result := FnKeys[KeyCode] shl 8;
      end;
    Ord('A')..Ord('Z'):
      begin
        if ShiftState and $0004 <> 0 then // Ctrl
          Result := ScanCode shl 8 + KeyCode - Ord('A') + 1
        else
          if ShiftState and $0008 <> 0 then
            Result := ScanCode shl 8      // Alt
          else
            Result := ScanCode shl 8 + CharCode;
      end;
    Ord('0')..Ord('9'):
      begin
        if ShiftState and $000C = 0 then
          Result := ScanCode shl 8 + CharCode
        else
          if ShiftState and $0008 <> 0 then // Alt
            if CharCode <> KeyCode then
              Result := ScanCode shl 8 + CharCode
            else
              Result := AltNumKeys[KeyCode] shl 8
          else // Ctrl
            Result := ScanCode shl 8 + CtrlNumKeys[KeyCode];
      end;
    VK_NUMPAD0..VK_DIVIDE:
      begin
        if ShiftState and $000C = 0 then
          Result := ScanCode shl 8 + CharCode
        else
          if ShiftState and $0004 <> 0 then
            Result := CtrlGreyKeys[KeyCode]
          else
            Result := AltGreyKeys[KeyCode]
      end;
    else
      begin
        // Scan translation table #1
        I := Low(TransTable1);
        while I < High(TransTable1) do
        begin
          with TransTable1[I] do
          if KeyCode = VK then
          begin
            if ShiftState and $0004 <> 0 then
              Result := CS
            else
              if ShiftState and $0003 <> 0 then
                Result := SS
              else
                if ShiftState and $0008 <> 0 then
                  Result := AL
                else
                  Result := NS;
            Exit;
          end;
          Inc(I);
        end;
        // Scan translation table #2
        I := Low(TransTable2);
        while I < High(TransTable2) do
        begin
          with TransTable2[I] do
          if KeyCode = VK then
          begin
            if ShiftState and $0004 <> 0 then
              Result := CS
            else
              if ShiftState and $0008 <> 0 then
                Result := AL
            else
              Break;
            Exit;
          end;
          Inc(I);
        end;
        Result := ScanCode shl 8 + CharCode;
      end;
  end;
end;
{$ENDIF}

procedure KbdUpdateEventQueues;
const
  AltNumericKeys : array[0..9] of record
    VK: Smallword;
    Value: Byte
  end = (
    (VK:$9B00; Value: 4),
    (VK:$9D00; Value: 6),
    (VK:$9F00; Value: 1),
    (VK:$A100; Value: 3),
    (VK:$9700; Value: 7),
    (VK:$9900; Value: 9),
    (VK:$9800; Value: 8),
    (VK:$A000; Value: 2),
    (VK:$A200; Value: 0),
    (VK:$4C00; Value: 5));
const
  AltNumeric: Byte = 0;
  {Platform: Longint = -1;}
var
  EventCount: DWord;
  InRec: TInputRecord;
  i: Integer;
  FoundAlt: Boolean;
begin
  if SysKeyCount > High(SysKeyQue) then
    exit;
  repeat
    GetNumberOfConsoleInputEvents(SysFileStdIn, EventCount);
    if EventCount > 0 then
     PeekConsoleInput(SysFileStdIn, InRec, 1, EventCount);
    if EventCount = 0 then
     Exit;
    {$IFDEF WIN95_HIGHPRIORITY}
    if SysPlatform = 1 then {Cat: если сидим в Win9x, значит для нормализации работы с консолью у нас увеличен приоритет}
      Sleep(1);       {     и тогда здесь нужно отдать ConAgent-у время, чтобы не "глотались" нажатия клавиш}
    {$ENDIF}
    with InRec do
      case EventType of
        FOCUS_EVENT:
          begin
          WindowNotFocused := not InRec.FocusEvent.bSetFocus;
          ReadConsoleInput(SysFileStdIn, InRec, 1, EventCount);
          end;
        key_Event:
          if SysKeyCount <= High(SysKeyQue) then
          with SysKeyQue[SysKeyCount], KeyEvent do
          begin
            {Cat: переделал так, чтобы при отпускании клавиш тоже обновлять ShiftState}
            SysShiftState := CtrlKeysToShiftState(dwControlKeyState); {Cat}
            if bKeyDown then
              begin
                skeKeyCode:=0;
{AK155 Под Win9x 'дочитывание' при помощи readConsole необходимо для
"нормальных" символов, недопустимо для Shift-Tab, а для стрелок, Home,
End и т.п. на _некоторых_ компьютерах тоже недопустимо при использовании
стандартной переключалки keyb (от чего это зависит - неизвестно,
возможно, от BIOS. Условие wVirtualKeyCode >= $30, вроде, накрывает все.}
                if not( (SysPlatform = 1)
                    and (wVirtualKeyCode >= $30){AK155}
                    and (AsciiChar <> #0)
                    and ((dwControlKeyState and AltCtrl_Pressed) = 0)
                    and  ReadConsole(SysFileStdIn, @UnicodeChar, 1, EventCount,nil)
                   )
                then
                  ReadConsoleInput(SysFileStdIn, InRec, 1, EventCount);
                {skeShiftState := CtrlKeysToShiftState(dwControlKeyState);} {Cat}
                {SysShiftState := skeShiftState;} {Cat}
                skeShiftState := SysShiftState; {Cat}

                {$IFDEF OLDCRT}
                skeKeyCode := TranslateKeyCode(wVirtualKeyCode, wVirtualScanCode, Ord(AsciiChar), skeShiftState);
                {$ELSE}
                skeKeyCode := TranslateKeyCode(wVirtualKeyCode, wVirtualScanCode, Ord(AsciiChar), dwControlKeyState);
                {$ENDIF}
                if skeKeyCode = 0 then
                  exit;                 // Do not report non-event

                // If numeric keypad, Alt pressed: record keys
                FoundAlt := False;
                if (skeShiftState and $8 = $8) and (dwControlKeyState and $100 = 0) then
                  if SysPlatform = 1 then // Windows 95
                    begin
                      for i := 0 to 9 do
                        if skeKeyCode = AltNumericKeys[I].VK then
                        begin
                          AltNumeric := AltNumeric*10 + AltNumericKeys[I].Value;
                          FoundAlt := True;
                        end;
                    end
                  else // Windows NT
                    if wVirtualKeyCode in [VK_NUMPAD0..VK_NUMPAD9] then
                      begin
                        AltNumeric := AltNumeric*10 + wVirtualKeyCode-VK_NUMPAD0;
                        FoundAlt := True;
                      end;

                if not FoundAlt then
                  begin
                    Inc(SysKeyCount);
                    AltNumeric := 0;
                  end;
              end
            else
             begin
              case KeyEvent.wVirtualKeyCode of
                VK_MENU:
                  if (AltNumeric <> 0) then
                    begin
                      skeKeyCode := AltNumeric;
                      AltNumeric := 0;
                      inc(SysKeyCount);
                    end;
              end {case};
              ReadConsoleInput(SysFileStdIn, InRec, 1, EventCount);
             end;
          end;
        _mouse_Event:
         begin
          if InRec.MouseEvent.dwEventFlags = 4{MOUSE_WHEELED} then
            begin { Имитируем клавиши Up/Dn или PgUp/PgDn,
             но сопровождаем их "регистром" $80. }
            if SysKeyCount <= High(SysKeyQue) then
              with SysKeyQue[SysKeyCount] do
                begin
                skeShiftState := SysShiftState or $80;
                if WheelScrollLines = -1 then
                  begin
                  if (MouseEvent.dwButtonState and $80000000) <> 0 then
                    skeKeyCode := $005100{kbPgDn}
                  else
                    skeKeyCode := $004900{kbPgUp};
                  Inc(SysKeyCount);
                  end
                else
                  begin
                  if (MouseEvent.dwButtonState and $80000000) <> 0 then
                    skeKeyCode := $005000{kbDown}
                  else
                    skeKeyCode := $004800{kbUp};
                  Inc(SysKeyCount);
                  for i := 2 to WheelScrollLines do
                    if SysKeyCount <= High(SysKeyQue) then
                      begin
                      SysKeyQue[SysKeyCount] := SysKeyQue[SysKeyCount-1];
                      Inc(SysKeyCount);
                      end;
                  end;
                end;
            end
          else if SysMouCount <= High(SysMouQue) then
          with SysMouQue[SysMouCount] do
          begin
            smePos.X := MouseEvent.dwMousePosition.X;
            smePos.Y := MouseEvent.dwMousePosition.Y;
            smeButtons := MouseEvent.dwButtonState and $0003;
            smeTime := SysSysMsCount;
            Inc(SysMouCount);
          end;
          ReadConsoleInput(SysFileStdIn, InRec, 1, EventCount);
         end;
        else
         ReadConsoleInput(SysFileStdIn, InRec, 1, EventCount);
      end;
    until (False) OR (SysKeyCount > High(SysKeyQue));
end;

{$Ifdef KeyDll}
Exports
  KbdInit              name 'KbdInit',
  KbdUpdateEventQueues name 'KbdUpdateEventQueues';
{$Endif}

begin
end.
