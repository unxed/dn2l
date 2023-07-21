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
unit MicroEd2;

interface

uses
  SysUtils, Classes, Defines, Microed, EdWin,
  Objects2
  ;

const
  TabStep: Integer = 8;

procedure MISaveFileAs(AED: PFileEditor);
procedure MISaveFile(AED: PFileEditor);
procedure MIOpenFile(AED: PFileEditor);
procedure MILoadFile(AED: PFileEditor; Name: String);
function MIReadBlock(AED: PFileEditor; var FileName: String;
    RetCollector: Boolean): Pointer;
procedure MILockFile(AED: PFileEditor);
procedure MIUnLockFile(AED: PFileEditor);

//procedure MIStore(AED: PFileEditor; var S: TStream);
//procedure MILoad(AED: PFileEditor; var S: TStream);
procedure MIAwaken(AED: PFileEditor);

const
  SmartWindow: PEditWindow = nil;
  ClipboardWindow: PEditWindow = nil;
  SmartWindowPtr: ^PEditWindow = @SmartWindow;
  ClipboardWindowPtr: ^PEditWindow = @ClipboardWindow;

implementation
uses
  DNStdDlg, Advance, DNApp, Commands, Lfnvp, Advance2, ed2, Advance1, Views,
  Objects, {WinClp, // commented by unxed} Dos, Messages, Startup, DnIni, DnInip, CopyIni,
  {SBlocks,}UKeyMap, Macro,
  xTime, Memory, Drivers,
  FlTl,
  fnotify,
  {$IFDEF PLUGIN}Plugin, {$ENDIF}
  ErrMess
  , Events {AK155 для LongWorkBegin - LongWorkEnd}
  ;

type
  ByteArray = array[1..MaxBytes] of Byte;
const
  FBufSize = 8192;

function ESC_Pressed: Boolean;
  var
    E: TEvent;
  begin
  Application^.Idle;
  GetKeyEvent(E);
  ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbESC)
  end;

{-DataCompBoy-}
procedure MISaveFileAs(AED: PFileEditor);
  var
    FileName: String;
    S: PStream;
    {$IFDEF PLUGIN}
    Event: TEvent; {Cat}
    {$ENDIF}
  begin
  {Cat}
  {$IFDEF PLUGIN}
  Event.What := evCommand;
  Event.Command := 65005;
  ProcessEditorEventHook(Event, AED);
  {$ENDIF}
  {/Cat}

  with AED^ do
    begin
    (*
    FileName := GetFileNameDialog(x_x, GetString(dlSaveFileAs),
        GetString(dlSaveFileAsName),
        fdOKButton+fdHelpButton, hsEditSave);
    *) // fixme: commented by unxed
    if FileName <> '' then
      begin
      MIUnLockFile(AED);
      S := CheckForOver(FileName);
      if S = nil then
        begin
        MILockFile(AED);
        Exit;
        end;
      EditName := lFExpand(FileName);
      if EditName[Length(EditName)] = '.' then
        SetLength(EditName, Length(EditName)-1);
      WriteBlock(EditName, S, FileLines, EdOpt.ForcedCRLF, OptimalFill);
      Dispose(S, Done);
      FileChanged(EditName);
      DisposeStr(PWindow(Owner)^.Title);
      if EditName = ''
      then
        PWindow(Owner)^.Title := NewStr(GetString(dlEditTitle))
      else
        PWindow(Owner)^.Title := NewStr(GetString(dlEditTitle)+' - '+
            {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(EditName));
      Owner^.Redraw;
      Modified := False;
      MILockFile(AED);
      end;
    end
  end { MISaveFileAs };
{-DataCompBoy-}

{-DataCompBoy-}
procedure MIOpenFile(AED: PFileEditor);
  var
    FileName: String;
  begin
  with AED^ do
    begin
    (*
    FileName := GetFileNameDialog(x_x, GetString(dlOpenFile),
        GetString(dlOpenFileName),
        fdOpenButton+fdHelpButton, hsEditOpen);
    *)
    // fixme: commented by unxed
    if FileName <> '' then
      begin
      MIUnLockFile(AED);
      MILoadFile(AED, lFExpand(FileName));
      BlockVisible := False; {Cat}
      Mark.Assign(0, 0, 0, 0); {Cat}
      if not isValid then
        begin
        isValid := True;
        Message(Owner, evCommand, cmClose, nil);
        MILockFile(AED);
        Exit;
        end;
      ScrollTo(0, 0);
      Owner^.Redraw;
      end;
    end
  end { MIOpenFile };
{-DataCompBoy-}

{-DataCompBoy-}
procedure MISaveFile(AED: PFileEditor);
  var
    S: PBufStream;
    I: LongInt;
    F: lFile;
    Dr: String;
    Nm: String;
    Xt: String;
    OldAttr: Word;
    L: array[0..0] of LongInt;
    PC: PLineCollection;
    FileExist: Boolean;
    TempEAContainerName: String;
    TempEAContainer: lFile;
    {$IFDEF PLUGIN}
    Event: TEvent; {Cat}
    {$ENDIF}
  begin
  {Cat}
  {$IFDEF PLUGIN}
  Event.What := evCommand;
  Event.Command := 65004;
  ProcessEditorEventHook(Event, AED);
  {$ENDIF}
  {/Cat}

  with AED^ do
    begin
    MIUnLockFile(AED);
    if ClipBrd then
      begin {-$VOL begin}
      if ClipBoardStream <> nil then
        Dispose(ClipBoardStream, Done);
      ClipBoardStream := nil;
      PC := New(PLineCollection, Init(100, 5, True));
      for I := 0 to FileLines^.Count-1 do
        PC^.Insert(NewLongStr(CnvLongString(FileLines^.At(I))));
      // fixme: commented by unxed
      //CopyLines2Stream(PC, ClipBoardStream);
      Dispose(PC, Done);
      end
    else
      begin {-$VOL end}
      if EditName = '' then
        begin
        MISaveFileAs(AED);
        Exit
        end;
      FileExist := False;
      lAssignFile(F, EditName);
      ClrIO;
      OldAttr := 0;
      lGetFAttr(F, OldAttr);
      if  (DosError = 0) then
        FileExist := True;
      if FileExist and (OldAttr and ReadOnly <> 0) then
        begin
        L[0] := LongInt(PtrInt(@EditName));
        //Pointer(L[0]) := @EditName; // fixme: changed by unxed
        if Msg(dlED_ModifyRO, @L, mfConfirmation+mfOKCancel) <> cmOK
        then
          begin
          MILockFile(AED);
          Exit;
          end;
        end;
      ClrIO;
      lSetFAttr(F, Archive);
      if Abort then
        begin
        MILockFile(AED);
        Exit;
        end;
      if FileExist then  {<MicroEd2.001>}
        begin
        TempEAContainerName := SwpDir+'DN'+ItoS(DNNumber)+'.EA_';
        lAssignFile(TempEAContainer, TempEAContainerName);
        lReWriteFile(TempEAContainer, 0);
        Close(TempEAContainer.F);
        {$IFNDEF DPMI32}
        CopyEAs(EditName, TempEAContainerName);
        {$ENDIF}
        {$IFDEF WIN32}
        CopySAs(EditName, TempEAContainerName); {Cat}
        {$ENDIF}
        end;
      if EditorDefaults.EdOpt and ebfCBF <> 0 then
        begin
        lFSplit(EditName, Dr, Nm, Xt);
        ClrIO;
        EraseFile(Dr+Nm+'.BAK');
        lChangeFileName(EditName, Dr+Nm+'.BAK');
        ClrIO;
        end;
      New(S, Init(EditName, stCreate, 4096));
      if S = nil then
        begin
        MILockFile(AED);
        Exit;
        end;
      {Cat: раньше почему-то проверка статуса происходила в этом месте,
      т.е. считалось, что если поток создан успешно, то и записан
      он также успешно, что неверно (например, когда место на диске
      заканчивается); теперь проверка осуществляется после записи блока
      кроме того, добавил вызов FileChanged - в случае неуспешной записи
      содержимое файла может поменяться}
      WriteBlock(EditName, S, FileLines, EdOpt.ForcedCRLF, OptimalFill);
      if S^.Status <> stOK then
        begin
        CantWrite(EditName);
        Dispose(S, Done);
        MILockFile(AED);
        if not (SmartPad or ClipBrd) then
          FileChanged(EditName);
        Exit;
        end;
      {/Cat}
      Dispose(S, Done);
      lAssignFile(F, EditName);
      ClrIO;
      if  (OldAttr <> Archive) and (OldAttr <> $FFFF) then
        lSetFAttr(F, OldAttr or Archive);
      ClrIO;
      if FileExist then
        begin
        {$IFNDEF DPMI32}
        CopyEAs(TempEAContainerName, EditName);
        {$ENDIF}
        {$IFDEF WIN32}
        CopySAs(TempEAContainerName, EditName); {Cat}
        {$ENDIF}
        EraseFile(TempEAContainerName);
        end;
      end;
    MILockFile(AED);
    Modified := False;
    JustSaved := True;
    LastSaveUndoTimes := UndoTimes; {piwamoto}
    Owner^.Redraw;
    if not (SmartPad or ClipBrd) then
      FileChanged(EditName);
    if UpStrg(EditName) = UpStrg(MakeNormName(SourceDir, 'DN.INI')) then
      begin
      LoadDnIniSettings;
      DoneIniEngine;

      CopyIniVarsToCfgVars;

      ShowIniErrors;
      end;
    end
  end { MISaveFile };
{-DataCompBoy-}

{-DataCompBoy-}
procedure MILoadFile(AED: PFileEditor; Name: String);
  label
    1;
  var
    Nm: String;
    Xt: String;
    PC: PCollection; {-$VOL}
    {$IFDEF PLUGIN}
    Event: TEvent; {Cat}
    {$ENDIF}
  begin
  with AED^ do
    begin
    if FileLines <> nil then
      Dispose(FileLines, Done);
    FileLines := nil;
    if UndoInfo <> nil then
      Dispose(UndoInfo, Done);
    UndoInfo := nil;
    if RedoInfo <> nil then
      Dispose(RedoInfo, Done);
    RedoInfo := nil;
    UndoTimes := 0;
    LastDir := -1; {DrawMode := 0;}
    LastSaveUndoTimes := 0;
    JustSaved := False; {piwamoto}
    SearchOnDisplay := False;
    EdOpt.ForcedCRLF := cfNone;
1:
    if Name = '' then
      begin
      {FileLines := GetCollector(1000, 100);}
      FileLines := New(PLineCollection, Init(300, 1000, True));
      {-SBlocks}
      if ClipBrd then
        begin {-$VOL begin}
        {Cat:warn лишние переприсваивания}
        PC := nil;
        // fixme: commented by unxed
        //CopyStream2Lines(ClipBoardStream, PC);
        if PC <> nil then
          with PC^ do
            begin
            while Count > 0 do
              begin
              FileLines^.Insert(At(Count-1));
              AtDelete(Count-1);
              end;
            Dispose(PC, Done);
            end;
        end; {-$VOL end}
      if FileLines^.Count = 0 then
        FileLines^.Insert(NewLongStr(''))
      end
    else
      begin
      LongWorkBegin;
      FileLines := MIReadBlock(AED, Name, True);
      {/Cat}
      LongWorkEnd;
      if not isValid then
        Exit;
      if FileLines = nil then
        begin
        {FileLines := GetCollector(1000, 100);}
        FileLines := New(PLineCollection, Init(300, 1000, True));
        {-SBlocks}
        FileLines^.Insert(NewLongStr(''));
        KeyMap := kmAscii;
        end;
      if Name <> '' then
        Name := lFExpand(Name);
      end;
    SetLimits;
    Pos.X := 0;
    Pos.Y := 0;
    Delta := Pos;
    {InsertMode := true;}
    {BlockVisible := false;}
    {Mark.Assign(0,0,0,0);}
    Modified := False; {DrawMode := 0;}
    WorkModified := False;
    LastLine := -1;
    SpecChar := False;
    Marking := False;
    MouseMark := False;
    RulerVisible := False;
    EnableMarking := True;
    WorkString := GetLine(0);
    if Name <> '' then
      Name := lFExpand(Name);
    EditName := Name;
    DisposeStr(PWindow(Owner)^.Title);
    {Cat:warn а не бред ли это?}
    if '*^&'+EditName = TempFile then
      begin
      EditName := '';
      TempFile := '';
      end;
    if SmartPad then
      begin
      PWindow(Owner)^.Title := NewStr('SmartPad(TM) - '+EditName);
      end
    else if ClipBrd then
      begin
      PWindow(Owner)^.Title := NewStr('Clipboard');
      end
    else if EditName <> ''
    then
      PWindow(Owner)^.Title := NewStr(GetString(dlEditTitle)+' - '+
          {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(EditName))
    else
      PWindow(Owner)^.Title := NewStr(GetString(dlEditTitle));
    MILockFile(AED);
    lFSplit(EditName, FreeStr, Nm, Xt);
    {PZ 2000.06.09}
    EdOpt.HiLite := Macro.InitHighLight(Nm+Xt, HiLitePar, Macros, @EdOpt);
    if EdOpt.HiLite then
      { check if highlighting is not disabled }
      EdOpt.HiLite := (EditorDefaults.EdOpt2 and ebfHlt) <> 0;
    {PZ end}
    end;

  {Cat}
  {$IFDEF PLUGIN}
  Event.What := evCommand;
  Event.Command := 65003;
  ProcessEditorEventHook(Event, AED);
  {$ENDIF}
  {/Cat}
  end { MILoadFile };
{-DataCompBoy-}

{-DataCompBoy-}
function MIReadBlock(AED: PFileEditor; var FileName: String;
    RetCollector: Boolean): Pointer;
  var
    S: PDosStream;
    B: ^ByteArray;
    I: LongInt;
    FFSize: LongInt; {!!s}
    J: LongInt; // длина прочитанного в буфер куска
    K: LongInt;
    LCount: LongInt;
    Lines: PLineCollection {PCollector}; {-SBlocks}
    S1, ST, S2: LongString;
    L: LongInt;
    KeyMapDetecting: Boolean;
    CodePageDetector: TCodePageDetector;
    OD, OA, ODOA: LongInt;

{AK155  15-02-2006 Полностью переписал, выкинув ассемблер и исправив
   ошибку в развороте табуляции в конце промежуточного буфера. }
  procedure SearchLines;
    var
      L: LongInt;
      MMM: array[0..1124] of Char;
        // промежуточный буфер, чтобы не дёргать зря AnsiString
      TS: LongInt; // ширина табуляции
      i: Integer; // заполнение MMM
      InChar, BufEnd: PChar;
      C: Char;
    label
      WrChar;
    begin
    if KeyMapDetecting then
      CodePageDetector.CheckString(PChar(B), J);
    with AED^ do
      begin
      TS := TabStep;
      InChar := PChar(B);
      BufEnd := PChar(B)+J;
      while True do
        begin { цикл строк; выход - катапультирование по концу буфера }
        i := 0;
        while True do { цикл символов; выход по разделителю строк или
            катапультирование по концу буфера }
          begin
          if InChar = BufEnd then
            begin // катапультирование по концу буфера
            if i <> 0 then
              begin
              L := Length(ST);
              SetLength(ST, L + i);
              Move(MMM, ST[L+1], i);
              end;
            Exit;
            end;
          C := InChar^;
          inc(InChar);
          case C of
           #$0D:
             begin
             if InChar^ = #$0A then
               begin
               inc(InChar);
               inc(ODOA);
               Break
               end;
             inc(OD);
             Break;
             end;
           #$0A:
             begin
             inc(OA);
             Break;
             end;
          end {case};
          { Запись символа в промежуточный буфер }
          if (C <> #$09) or (TS = 0) then
            begin { Просто запись символа }
            MMM[i] := C;
            inc(i);
            { Анализируем переполнение промежуточного буфера. В конце
              буфера оставляем резерв на максимальный размер TabStep}
            if i > SizeOf(MMM)-101 then
              begin
              L := Length(ST);
              SetLength(ST, L + i);
              Move(MMM, ST[L+1], i);
              i := 0;
              end;
            end
          else
            begin { Замена табуляции пробелами. Тут переполнение
              буфера не анализируем, хватит запаса (см. выше). }
            L := (i div TS + 1)*TS;
            while i <> L do
              begin
              MMM[i] := ' ';
              inc(i);
              end;
            end
          end;

        if i <> 0 then
          begin
          L := Length(ST);
          SetLength(ST, L + i);
          Move(MMM, ST[L+1], i);
          end;

{!! Отбрасывание хвостовых пробелов. Надо сделать опциональным }
        if (ST <> '') and (ST[Length(ST)] = ' ') then
          LongDelRight(ST);

        Lines^.Insert(NewLongStr(ST));
        ST := '';
        end;
      end
    end { SearchLines };
{/AK155}
  var
    Info: PView;
    ep: Boolean;
    tmr: TEventTimer;
  begin { MIReadBlock }
  with AED^ do
    begin
    MIReadBlock := nil;
    Abort := False;
    if LowMemory then
      begin
      FileName := '';
      isValid := False;
      Exit;
      end;
    CodePageDetector.Init;
    KeyMap := ProcessDefCodepage(DefCodePage);
    KeyMapDetecting := (KeyMap = kmNone);
    ODOA := 0;
    OD := 0;
    OA := 0;
    S := New(PBufStream, Init(FileName, stOpenRead, 1024));
    if  (S^.Status <> stOK) then
      begin
      K := S^.ErrorInfo;
      isValid := (K = 2) or (K = 3) or (K = 110);
      if  (K <> 2) and (K <> 3) and (K <> 110) then
        MessFileNotOpen(FileName, K); {JO, AK155}
      Dispose(S, Done);
      Exit
      end;
    if  (S^.GetSize > MemAvail-$4000)
    then
      begin
      Application^.OutOfMemory;
      Dispose(S, Done);
      FileName := '';
      isValid := False;
      Exit
      end;
    B := MemAlloc(FBufSize);
    if B = nil then
      begin
      Dispose(S, Done);
      FileName := '';
      Exit
      end;
    Info := nil;
    I := 0;
    FFSize := i32(S^.GetSize);
    Lines := New(PLineCollection, Init(1000 + (FFSize div 20), 1000, True));
    {-$VOL begin}
    if EditorDefaults.EdOpt and ebfTRp = 0
    then
      TabStep := 0
    else
      begin
      TabStep := StoI(EditorDefaults.TabSize);
      if TabStep = 0 then
        TabStep := 8;
      end;
    if TabStep > 100 then
      TabStep := 100;
    {-$VOL end}
    S^.Seek(0);
    I := 0;
    FFSize := i32(S^.GetSize);
    LCount := 1;
    ep := False;
    NewTimer(tmr, 150);
    ST := '';
    if FFSize > FBufSize then
      J := FBufSize
    else
      J := FFSize;
    while I < FFSize do
      begin
      UpdateWriteView(Info);
      S^.Read(B^, J);
      if TimerExpired(tmr) then
        begin
        NewTimer(tmr, 150);
        if Info = nil then
          Info := WriteMsg(^M^M^C+GetString(dlReadingFile));
        ep := ESC_Pressed;
        end;
      if  (S^.Status <> stOK) or ep or Abort or (MemAvail < $4000) or
        LowMemory or (Lines^.Count > MaxCollectionSize)
      then
        begin
        Dispose(Lines, Done);
        Lines := nil;
        FileName := '';
        FreeMem(B, FBufSize);
        Dispose(S, Done);
        Info^.Free;
        Application^.OutOfMemory;
        isValid := False;
        Exit
        end;
      if  (B^[J] = 13) and (S^.GetPos < S^.GetSize) then
        begin
        Dec(J);
        S^.Seek(S^.GetPos-1);
        end;
      SearchLines;
      I := i32(S^.GetPos){!!s};
      if FFSize-I > FBufSize then
        J := FBufSize
      else
        J := FFSize-I;
      end;
    if  (ST <> '') and (ST[1] = #10) then
      System.Delete(ST, 1, 1);
    while (ST <> '') and (ST[Length(ST)] = ' ') do
      SetLength(ST, Length(ST)-1);
    Lines^.Insert(NewLongStr(ST));
    if KeyMapDetecting then
      KeyMap := CodePageDetector.DetectedCodePage;
    if  (ODOA shl 1 >= OD+OA) then
      EdOpt.ForcedCRLF := cfCRLF
    else if (OD shl 1 >= ODOA+OA) then
      EdOpt.ForcedCRLF := cfCR
    else if (OA shl 1 >= ODOA+OD) then
      EdOpt.ForcedCRLF := cfLF;
    if RetCollector then
      MIReadBlock := Lines
    else
      begin
      {MIReadBlock := PStdCollector(Lines)^.Collection;}
      {PStdCollector(Lines)^.Collection := nil;}
      {Dispose(Lines,Done); Lines:=nil;}
      MIReadBlock := PLineCollection(Lines); {-SBlocks}
      Lines := nil; {-SBlocks}
      end;
    Dispose(S, Done);
    FreeMem(B, FBufSize);
    Info^.Free;
    end
  end { MIReadBlock };
{-DataCompBoy-}

procedure MILockFile(AED: PFileEditor);
  begin
  with AED^ do
    begin
    if EditorDefaults.EdOpt and ebfLck = 0 then
      Exit;
    if Locker <> nil then
      Dispose(Locker, Done);
    Locker := New(PDosStream, Init(EditName, (stOpenRead and $FF0F) or // fmDeny to $FF0F changed by unxed
           fmShareDenyWrite));
    end
  end;

procedure MIUnLockFile(AED: PFileEditor);
  begin
  with AED^ do
    begin
    if EditorDefaults.EdOpt and ebfLck = 0 then
      Exit;
    if Locker = nil then
      Exit; { на всякий случай }
    Dispose(Locker, Done);
    Locker := nil;
    end
  end;

(*
procedure MIStore(AED: PFileEditor; var S: TStream);
  begin
  with AED^ do
    begin
    PutPeerViewPtr(S, HScroll);
    PutPeerViewPtr(S, VScroll);
    PutPeerViewPtr(S, InfoL);
    PutPeerViewPtr(S, BMrk);
    S.WriteStr(@EditName);
    S.Write(SmartPad, SizeOf(SmartPad));
    S.Write(ClipBrd, SizeOf(ClipBrd));
    S.Write(MarkPos, SizeOf(MarkPos));
    S.Write(EdOpt.LeftSide, 6);
    S.Write(EdOpt.HiLite, 1);
    S.Write(EdOpt.HiliteColumn, SizeOf(EdOpt.HiliteColumn));
    S.Write(EdOpt.HiliteLine, SizeOf(EdOpt.HiliteLine));
    S.Write(EdOpt.AutoIndent, SizeOf(EdOpt.AutoIndent));
    S.Write(VertBlock, SizeOf(VertBlock));
    S.Write(EdOpt.BackIndent, SizeOf(EdOpt.BackIndent));
    S.Write(EdOpt.AutoJustify, SizeOf(EdOpt.AutoJustify));
    S.Write(OptimalFill, SizeOf(OptimalFill));
    S.Write(EdOpt.AutoWrap, SizeOf(EdOpt.AutoWrap));
    S.Write(EdOpt.AutoBrackets, SizeOf(EdOpt.AutoBrackets));
    S.Write(KeyMap, SizeOf(KeyMap)); {-$VIV}
    S.Write(TabReplace, SizeOf(TabReplace)); {-$VOL}
    S.Write(EdOpt.SmartTab, SizeOf(EdOpt.SmartTab)); {-$VOL}
    S.Write(Pos, SizeOf(Pos)); {Cat}
    S.Write(InsertMode, SizeOf(InsertMode)); {Cat}
    S.Write(DrawMode, SizeOf(DrawMode)); {Cat}
    S.Write(Mark, SizeOf(Mark)); {Cat}
    S.Write(BlockVisible, SizeOf(BlockVisible)); {Cat}
    S.Write(EdOpt.ForcedCRLF, SizeOf(EdOpt.ForcedCRLF)); {Cat}
    end
  end { MIStore };

procedure MILoad(AED: PFileEditor; var S: Objects.TStream);
  var
    SS: PString;
  begin
  with AED^ do
    begin
    GetPeerViewPtr(S, HScroll);
    GetPeerViewPtr(S, VScroll);
    GetPeerViewPtr(S, InfoL);
    GetPeerViewPtr(S, BMrk);
    SS := S.ReadStr;
    if SS = nil then
      EditName := ''
    else
      begin
      EditName := SS^;
      DisposeStr(SS);
      end;
    S.Read(SmartPad, SizeOf(SmartPad));
    S.Read(ClipBrd, SizeOf(ClipBrd));
    {Cat}
    if SmartPad then
      SmartWindowPtr := @Owner;
    if ClipBrd then
      ClipboardWindowPtr := @Owner;
    {/Cat}
    S.Read(MarkPos, SizeOf(MarkPos));
    S.Read(EdOpt.LeftSide, 6);
    S.Read(EdOpt.HiLite, 1);
    S.Read(EdOpt.HiliteColumn, SizeOf(EdOpt.HiliteColumn));
    S.Read(EdOpt.HiliteLine, SizeOf(EdOpt.HiliteLine));
    S.Read(EdOpt.AutoIndent, SizeOf(EdOpt.AutoIndent));
    S.Read(VertBlock, SizeOf(VertBlock));
    S.Read(EdOpt.BackIndent, SizeOf(EdOpt.BackIndent));
    S.Read(EdOpt.AutoJustify, SizeOf(EdOpt.AutoJustify));
    S.Read(OptimalFill, SizeOf(OptimalFill));
    S.Read(EdOpt.AutoWrap, SizeOf(EdOpt.AutoWrap));
    S.Read(EdOpt.AutoBrackets, SizeOf(EdOpt.AutoBrackets));
    S.Read(KeyMap, SizeOf(KeyMap)); {-$VIV}
    S.Read(TabReplace, SizeOf(TabReplace)); {-$VOL}
    S.Read(EdOpt.SmartTab, SizeOf(EdOpt.SmartTab)); {-$VOL}
    S.Read(Pos, SizeOf(Pos)); {Cat}
    S.Read(InsertMode, SizeOf(InsertMode)); {Cat}
    S.Read(DrawMode, SizeOf(DrawMode)); {Cat}
    S.Read(Mark, SizeOf(Mark)); {Cat}
    S.Read(BlockVisible, SizeOf(BlockVisible)); {Cat}
    S.Read(EdOpt.ForcedCRLF, SizeOf(EdOpt.ForcedCRLF)); {Cat}
    isValid := True;
    Macros := New(PCollection, Init(10, 10));
    LastDir := -1;
    MenuItemStr[True] := NewStr(GetString(dlMenuItemOn));
    MenuItemStr[False] := NewStr(GetString(dlMenuItemOff));
    end
  end { MILoad };
*)

procedure MIAwaken(AED: PFileEditor);
  var
    X, Y: LongInt;
    XD: TPoint;
    Hi: Boolean;
    _KeyMap: TKeyMap;
    _ForcedCrLf: TCRLF;
  begin
  with AED^ do
    begin
    X := HScroll^.Value;
    Y := VScroll^.Value;
    XD := Pos;
    Hi := EdOpt.HiLite;
    _KeyMap := KeyMap; {Cat}
    _ForcedCrLf := EdOpt.ForcedCRLF; {Cat}
    MILoadFile(AED, EditName);
    KeyMap := _KeyMap; {Cat}
    EdOpt.ForcedCRLF := _ForcedCrLf; {Cat}
    EdOpt.HiLite := Hi;
    ScrollTo(X, Y);
    Pos := XD;
    ChPosition := False;
    Owner^.Redraw;
    end
  end { MIAwaken };

end.
