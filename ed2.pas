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
unit ed2;

interface

uses
  Commands, UKeyMap, Objects, Views, Drivers, Defines, Streams,
  Lfnvp {, SBlocks}
  ;

{ TDoCollection }
const
  maxUndo = 100;

  udDelChar = 1;
  udInsChar = 2;
  udDelLine = 3;
  udInsLine = 4;
  udDelBlock = 5;
  udInsBlock = 6;
  udBackDel = 7;
  udSubDel = 8;
  udSubDelLine = 9;
  udIndentBlock = 10;
  udUnindentBlock = 11;
  udInsVertBlock = 12;
  udReplace = 13;
  udReplaceChar = 14;
  udReplaceBlock = 15;
  udClearBlock = 16;
  udStrModified = 17;
  udDupeLine = 18;
  udFormatBlock = 19;
  udReplaceAll = 20; {-$VOL}

type
  TDoKind = (dkRedo, dkUndo); {-$VOL}

  PUndoRec = ^TUndoRec;
  TUndoRec = record
    What: Word;
    Where: TPoint;
    KeyMap: TKeyMap; {-$VIV}
    case Word of
      udDelChar: (Str: PLongString);
      udInsChar: (Count, Width: Integer; Block: TRect);
      udDelLine: (Lines: PCollection; Vertical, InsM: Boolean);
    end;

  PDoCollection = ^TDoCollection;
  TDoCollection = object(TCollection)
    DoKind: TDoKind; {-$VOL}
    procedure FreeItem(P: Pointer); virtual;
    constructor Init(ReOrUn_do: TDoKind); {-$VOL}
    end;

  { TInfoLine }

  PInfoLine = ^TInfoLine;
  TInfoLine = object(TView)
    constructor Init(var R: TRect);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  { TBookmarkLine }

  PBookmarkLine = ^TBookmarkLine;
  TBookmarkLine = object(TView)
    procedure Draw; virtual;
    end;

  { TAttrBufStream }

  PAttrBufStream = ^TAttrBufStream;
  TAttrBufStream = object(TBufStream)
    OldAttr: Word;
    F: lFile;
    constructor Init(FileName: String; Mode, Size: Word);
    destructor Done; virtual;
    end;

function CheckForOver(Name: String): PStream;
procedure WriteBlock(Hint: String; S: PStream; C: PLineCollection
    {PCollector}; ForcedCRLF: TCRLF; AOptimalFill: Boolean); {-SBlocks}

implementation
uses
  Advance, Advance1, Advance2
  , Messages, Dos, DNApp, Microed, Startup, DnIni, EdWin
  ;

{ TDoCollection }
procedure TDoCollection.FreeItem(P: Pointer);
  var
    T: PUndoRec;
  begin
  T := P;
  if P = nil then
    Exit;
  case DoKind of
    dkUndo:
      case T^.What of
        udDelChar, udInsLine, udSubDelLine, udReplace, udReplaceChar,
         udInsBlock,
        udFormatBlock, udStrModified, udSubDel, udBackDel, udReplaceAll:
          DisposeLongStr(T^.Str);
        udDelLine, udDelBlock, udReplaceBlock,
        udClearBlock:
          begin
          Dispose(T^.Lines, Done);
          T^.Lines := nil
          end;
        udInsChar:
          begin
          end;
      end {case};
    dkRedo:
      case T^.What of
        udInsChar, udSubDel, udReplaceChar, udSubDelLine, udStrModified,
         udReplace,
        udReplaceAll:
          DisposeLongStr(T^.Str);
        udInsLine, udInsBlock, udFormatBlock, udDelBlock, udInsVertBlock,
        udReplaceBlock, udClearBlock:
          begin
          Dispose(T^.Lines, Done);
          T^.Lines := nil
          end;
        udDelChar, udDelLine, udBackDel:
          begin
          end;
      end {case};
  end {case};
  Dispose(T);
  end { TDoCollection.FreeItem };

constructor TDoCollection.Init(ReOrUn_do: TDoKind); {-$VOL}
  begin
  inherited Init($100, $80);
  DoKind := ReOrUn_do;
  end; {-$VOL}

{ TInfoLine }

constructor TInfoLine.Init;
  begin
  inherited Init(R);
  EventMask := evMouseDown;
  GrowMode := gfGrowHiX+gfGrowHiY+gfGrowLoY;
  end;

procedure TInfoLine.HandleEvent;
  var
    T: TPoint;
    { lS: byte; }
    P: PFileEditor;
    Ev: TEvent;
    BookMark: Byte;
  begin
  inherited HandleEvent(Event);
  if Event.What = evMouseDown then
    begin
    Owner^.MakeLocal(Event.Where, T);
    if T.X >= Owner^.Size.X-2 then
      begin
      PWindow(Owner)^.Frame^.HandleEvent(Event);
      Exit;
      end;
    MakeLocal(Event.Where, T);
    Event.What := evCommand;
    {==0000000:000=[000 00]=(-)==CrLf=DOS==<1........>}
    {00000000001111111111222222222233333333334444444444}
    {01234567890123456789012345678901234567890123456789}
    P := PFileEditor(PEditWindow(Owner)^.Intern);
    if  (T.X > 1) and (T.X < 12) then
      Event.Command := cmGotoLineNumber
    else if (T.X > 12) and (T.X < 21) then
      Event.Command := cmSpecChar
    else if (T.X > 21) and (T.X < 25) then
      Event.Command := cmSwitchBlock
    else if (T.X > 26) and (T.X < 31) then
      case P^.EdOpt.ForcedCRLF of
        cfCRLF:
          Event.Command := cmEditLfMode;
        cfLF:
          Event.Command := cmEditCrMode;
        cfCR:
          Event.Command := cmEditCrLfMode;
        else {case}
          Event.Command := cmEditCrLfMode;
      end
    else if (T.X > 31) and (T.X < 35) then
      Event.Command := cmSwitchKeyMapping
    else if (T.X = 37) then
      PFileEditor(Owner^.Current)^.ScrollTo(0, 0) {AK155}
    else if FastBookmark and ((T.X > 37) and (T.X < 47)) then
      begin
      BookMark := T.X-38;
      if  (Event.Buttons and mbRightButton <> 0) or
          (not P^.MarkPos[Event.InfoByte].EqualsXY(-1, -1))
      then
        with Event do
          begin
          What := evCommand;
          if  (Event.Buttons and mbRightButton <> 0)
          then
            Command := cmPlaceMarker1+BookMark
          else
            Command := cmGoToMarker1+BookMark;
          end
      else
        Event.What := evNothing;
      end
    else if (T.X = 47) then
      with PFileEditor(Owner^.Current) do
        ScrollTo(0, FileLines^.Count) {AK155}
    else
      Event.What := evNothing;
    if Event.What <> evNothing then
      PutEvent(Event);
    ClearEvent(Event);
    end;
  end { TInfoLine.HandleEvent };

procedure TInfoLine.Draw;
  var
    P: PFileEditor;
    X, Y: LongInt;
    C: Char;
    S: String;
    CharNum: Byte; {-$VIV}
    Ch: Char;
    Ch2: Char;
    Color: Byte;
    qwe: Byte;
    B: TDrawBuffer;
  begin
  P := PFileEditor(PEditWindow(Owner)^.Intern);
  if Owner^.GetState(sfDragging) or not Owner^.GetState(sfActive) then
    begin
    if Owner^.GetState(sfDragging)
    then
      Color := PWindow(Owner)^.Frame^.GetColor(5)
    else
      Color := PWindow(Owner)^.Frame^.GetColor(2);
    Ch2 := #196;
    end
  else
    begin
    Color := PWindow(Owner)^.Frame^.GetColor(3);
    Ch2 := #205;
    end;
  if not Owner^.GetState(sfActive) then
    SetLength(S, 0)
  else
    begin
    with P^ do
      begin
      X := Delta.X+1;
      Y := Delta.Y+1;
      if X <= Length(WorkString) then
        C := WorkString[X]
      else
        C := #0;
      end;
    CharNum := Byte(KeyMapDescr[P^.KeyMap].XLatCP^[FromAscii][C]);
    if P^.Modified then
      S := #15+Ch2
    else
      S := Ch2+Ch2;
    S := S+SStr(Y, 5, Ch2)+':'+SSt2(X, 4, Ch2)
        +Ch2+'['+SStr(CharNum, 3, '0')+'·'+Hex2(CharNum)+']'+Ch2;
    {-$VIV}
    if P^.DrawMode = 1 then
      S := S+'{┼'
    else if P^.DrawMode = 2 then
      S := S+'{╬'
    else if P^.VertBlock then
      S := S+'('#18
    else
      S := S+'('#29;
    if P^.DrawMode = 0 then
      if P^.OptimalFill then
        S := S+'F)'
      else
        S := S+')═'
    else if P^.OptimalFill then
      S := S+'F}'
    else
      S := S+'}═';

    if P^.EdOpt.ForcedCRLF = cfNone then
      begin
      P^.EdOpt.ForcedCRLF := cfNone;
      for qwe := 0 to EditorDefaults.NewLine do
        P^.EdOpt.ForcedCRLF := Succ(P^.EdOpt.ForcedCRLF);
      end;
    if P^.EdOpt.ForcedCRLF = cfCR then
      S := S+Ch2+Ch2+'Cr'+Ch2
    else if P^.EdOpt.ForcedCRLF = cfLF then
      S := S+Ch2+Ch2+'Lf'+Ch2
    else if P^.EdOpt.ForcedCRLF = cfCRLF then
      S := S+Ch2+'CrLf'
    else
      S := S+Ch2+'::::';
    S := S+Ch2+KeyMapDescr[P^.KeyMap].Tag+Ch2;
    if FastBookmark then
      begin
      S := S+Ch2+'<'; {-$VIV 20.05.99--}
      for X := 1 to 9 do
        if not P^.MarkPos[X].EqualsXY(-1, -1)
        then
          S := S+Char(X+48)
        else
          S := S+#250;
      S := S+'>'; {-$VIV::}
      end;
    end;
  MoveChar(B, Ch2, Color, Size.X);
  MoveStr(B, S, Color);
  WriteLine(0, 0, Size.X, 1, B);
  end { TInfoLine.Draw };

{TBookmarkLine}
procedure TBookmarkLine.Draw;
  var
    P: PFileEditor;
    Col: Byte;
    I: Integer;
    Mrk: Char;
    Ch: Char;
    B: array[0..20] of AWord;

  function IsMarker(pLine: LongInt): Char;
    var
      I: Byte;
    begin
    IsMarker := #0;
    for I := 1 to 9 do
      if P^.MarkPos[I].Y = pLine then
        begin
        IsMarker := Char(I+48);
        Break;
        end;
    end;

  function SwitchHalfs(B: Byte): Byte;
    begin
    SwitchHalfs := ((B and $0F) shl 4) or ((B and $F0) shr 4);
    end;

  begin { TBookmarkLine.Draw }
  P := PFileEditor(PEditWindow(Owner)^.Intern);
  if Owner^.GetState(sfDragging) or not Owner^.GetState(sfActive) then
    begin
    if Owner^.GetState(sfDragging)
    then
      Col := PWindow(Owner)^.Frame^.GetColor(5)
    else
      Col := PWindow(Owner)^.Frame^.GetColor(2);
    Ch := #179;
    end
  else
    begin
    Col := PWindow(Owner)^.Frame^.GetColor(3);
    Ch := #186;
    end;
  if not ShowBookmarks then
    begin
    MoveChar(B, Ch, Col, 1); {SYR}
    WriteLine(0, 0, Size.X, Size.Y, B);
    Exit;
    end;
  for I := 0 to Size.Y do
    begin
    Mrk := IsMarker(P^.Pos.Y+I);
    if Mrk = #0 then
      MoveChar(B, Ch, Col, 1) {SYR}
    else
      MoveChar(B, Mrk, SwitchHalfs(Col), 1);
    WriteLine(0, I, Size.X, 1, B);
    end;
  end { TBookmarkLine.Draw };

constructor TAttrBufStream.Init(FileName: String; Mode, Size: Word);
  begin
  inherited Init(FileName, Mode, Size);
  OldAttr := $FFFF;
  end;

destructor TAttrBufStream.Done;
  begin
  inherited Done;
  if OldAttr <> $FFFF then
    lSetFAttr(F, OldAttr);
  end;

{-DataCompBoy-}
function CheckForOver(Name: String): PStream; {<Microed2.001>}
  var
    S: PAttrBufStream;
    F: lFile;
    W: Word;
    L: array[0..0] of LongInt;
    Attr: Word;

  procedure CreateBackup;
    var
      Dr: String;
      Nm: String;
      Xt: String;
    begin
    lFSplit(Name, Dr, Nm, Xt);
    ClrIO;
    EraseFile(Dr+Nm+'.BAK');
    lChangeFileName(Name, Dr+Nm+'.BAK');
    ClrIO;
    end;

  procedure OverQuery;
    var
      P: PString;
      Dr: String[30];
    begin
    P := @Dr;
    Dr := Cut(Name, 30);
    W := Msg(dlED_OverQuery, @P,
         mfYesButton+mfCancelButton+mfAppendButton+mfWarning);
    end;

  begin { CheckForOver }
  CheckForOver := nil;
  Abort := False;
  S := nil;
  lAssignFile(F, Name);
  ClrIO;
  lGetFAttr(F, Attr);
  if Abort then
    Exit;
  if  (DosError = 0) and (Attr and ReadOnly <> 0) then
    begin
    OverQuery;
    case W of
      cmYes, cmOK:
        ;
      else {case}
        Exit
    end {case};
    Pointer(L[0]) := @Name;
    if Msg(dlED_ModifyRO, @L, mfConfirmation+mfOKCancel) <> cmOK then
      Exit;
    lSetFAttr(F, Archive);
    if Abort or (DosError <> 0) then
      begin
      CantWrite(Name);
      Exit;
      end;
    end
  else
    begin
    W := $FFFF;
    Attr := Archive
    end;
  New(S, Init(Name, stOpen, 4096));
  if S = nil then
    Exit;
  if Abort or (S^.Status = stOK) then
    begin
    if W = $FFFF then
      OverQuery;
    case W of
      cmYes, cmOK:
        begin
        if Attr and ReadOnly <> 0 then
          begin
          S^.OldAttr := Attr;
          lAssignFile(S^.F, lFileNameOf(F));
          end;
        case W of
          cmYes:
            S^.Truncate;
          cmOK:
            S^.Seek(S^.GetSize);
        end {case};
        CheckForOver := S;
        end;
      else {case}
        begin
        Dispose(S, Done);
        S := nil;
        end;
    end {case};
    Exit;
    end;
  Dispose(S, Done);
  S := nil;
  if Abort then
    Exit;
{AK155 13/03/2006
  Полезность CreateBackup очень сомнительна. Сюда можно попасть
только если при открытии с stOpen получился Status <> 0, что, скорее
всего, обозначает, что файла нет, то есть Backup не нужен. Из других
причин Status <> 0 приходит в голову только нехватка памяти на буфер.
Так в этом случае надо не бэкап создавать, а что-то с памятью
придумывать, поскольку при открытии с stCreate памяти опять таки
не хватит, но пользователю мы этого не скажем, и, более того,
можно, наверно, упасть в CantWrite или не выдать никакого сообщения.
  Так что лучше бы проанализировать причину Status <> 0 и сделать
что-то более осмысленное, чем то, что тут имеется сейчас. }
  if EditorDefaults.EdOpt and ebfCBF <> 0 then
    CreateBackup;
  New(S, Init(Name, stCreate, 4096));
  if Abort or (S = nil) or (S^.Status <> stOK) then
    begin
    CantWrite(Name);
    Dispose(S, Done);
    S := nil;
    Exit
    end;
  CheckForOver := S;
  end { CheckForOver };
{-DataCompBoy-}

procedure WriteBlock(Hint: String; S: PStream; C: PLineCollection
    {PCollector}; ForcedCRLF: TCRLF; AOptimalFill: Boolean); {-SBlocks}
  var
    I: LongInt;
    M: LongInt;
    SST: LongString;
    P: PLongString;

    {Cat: эта процедура теперь умеет работать с длинными строками
      и находится в модуле Advance1}
    (*
  procedure CompressString; {та, кот. при сохранении файла}
  var PP: Pointer;
      TSt: Integer;
  begin
   PP := @SST;
   TSt := StoI(EditorDefaults.TabSize);
   if TSt = 0 then TSt := 8;
 {$IFNDEF BIT_32}
   asm
      les bx, PP
      mov cl, es:[bx]
      inc bx
      xor ch, ch
      jcxz @@Ex
      xor di, di
      xor si, si
      mov byte ptr es:[bx-1], ch
    @@1:
      mov ah, byte ptr TSt
      xor dx, dx
    @@2:
      mov al, es:[bx][si]
      mov es:[bx][di], al
      inc si
      cmp si, cx
      ja  @@Ex
      inc di
      inc byte ptr es:[bx-1]
      cmp al, ' '
      jne @@3
      inc dl
      jmp @@4
     @@3:
      xor Dl, dl
     @@4:
      dec ah
      jnz @@2
      or  dl, dl
      jz @@5
      dec dl
      jz @@5
      sub di, dx
      sub byte ptr es:[bx-1], dl
      mov al, 9
      mov es:[bx][di-1], al
     @@5:
      jmp @@1
    @@Ex:
   end;
 {$ELSE}
   asm
      push ebx
      push edx
      push edi
      push esi
      mov ebx, PP
      xor ecx, ecx
      mov cl, [ebx]
      inc ebx
      jcxz @@Ex
      xor edi, edi
      xor esi, esi
      mov byte ptr [ebx-1], ch
    @@1:
      mov ah, byte ptr TSt
      xor edx, edx
    @@2:
      mov al, [ebx+esi]
      mov [ebx+edi], al
      inc esi
      cmp esi, ecx
      ja  @@Ex
      inc edi
      inc byte ptr [ebx-1]
      cmp al, ' '
      jne @@3
      inc dl
      jmp @@4
     @@3:
      xor dl, dl
     @@4:
      dec ah
      jnz @@2
      or  dl, dl
      jz @@5
      dec dl
      jz @@5
      sub edi, edx
      sub byte ptr [ebx-1], dl
      mov al, 9
      mov [ebx+edi-1], al
     @@5:
      jmp @@1
    @@Ex:
      pop esi
      pop edi
      pop edx
      pop ebx
   end;
 {$ENDIF}
  end;
*)

  var
    PP: PView;
    CrLf: String[2];
    qwe: Byte;
  begin { WriteBlock }

  if ForcedCRLF = cfNone then
    begin
    ForcedCRLF := cfNone;
    for qwe := 0 to EditorDefaults.NewLine do
      ForcedCRLF := Succ(ForcedCRLF);
    end;

  if ForcedCRLF = cfCR then
    CrLf := #13
  else if ForcedCRLF = cfLF then
    CrLf := #10
  else if ForcedCRLF = cfCRLF then
    CrLf := #13#10;

  I := 1;
  if  (S = nil) or (C = nil) then
    Exit;
  PP := WriteMsg(^M^M^C+GetString(dlWritingFile));
  while not Abort and (S^.Status = stOK) and (I < C^.Count) do
    begin
    UpdateWriteView(PP);
    P := C^.At(I-1);
    if P <> nil then
      {JO} {!!!}
      SST := P^+CrLf
    else
      SST := CrLf; {JO}
    if AOptimalFill then
      CompressString(SST);
    S^.Write(SST[1], Length(SST));
    Inc(I);
    end;
  {HintString := '';}Application^.Idle;
  P := C^.At(I-1);
  if P <> nil then
    SST := P^
  else
    SST := '';
  if AOptimalFill then
    CompressString(SST); {Cat: про последнюю строку тоже не забываем}
  S^.Write(SST[1], Length(SST));
  if PP <> nil then
    PP^.Free;
  end { WriteBlock };

end.
