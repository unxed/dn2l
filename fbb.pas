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

unit FBB;

interface

procedure LongCopy(Fn1: String);

implementation
uses
  Lfnvp, Dos, Tree, VPSysLow, Advance, Advance1, Advance2, FileCopy, DNApp,
  Messages, Views, Defines, Dialogs, Commands, Drivers, Memory, HistList,
  Gauge, Startup, xTime, ErrMess, VPUtils
  ;

type
  Ar65000 = array[1..65000] of Byte;
  ArPtr = ^Ar65000;

  HRec = record
    Num: Word;
    NR: Word;
    NW: Word;
    Buf: ArPtr;
    end;

const
  MaxBF = 20;
var
  Bf: array[1..MaxBF] of HRec;
  dr1, dr2: Byte;
  FreeSpc: TSize;
  Timer: TEventTimer;
const
  NBf: Byte = 0;

function IsLabel(P: PView): Boolean;
  begin
  IsLabel := TypeOf(P^) = TypeOf(TLabel);
  end;

procedure GetMaxMem;
  var
    l: LongInt;
  begin
  NBf := 0;
  repeat
    l := MaxAvail-15000;
    if l > 0 then
      begin
      Inc(NBf);
      if l > 65000 then
        l := 65000;
      Bf[NBf].Buf := MemAlloc(l);
      Bf[NBf].Num := l;
      if Bf[NBf].Buf = nil then
        Dec(NBf);
      end;
  until (MaxAvail < 16000) or LowMemory or (NBf >= MaxBF);
  end;

procedure ClearMem;
  var
    i: Byte;
  begin
  for i := 1 to NBf do
    FreeMem(Bf[i].Buf, Bf[i].Num);
  NBf := 0;
  end;

procedure LongCopy(Fn1: String);
  label Ex, _Abort_;
  type
    OHdr = record
      Nm: array[1..23] of Char;
      XT: array[1..3] of Char;
      ln: LongInt;
      end;
    NHdr = record
      Nm: array[1..23] of Char;
      ln: LongInt;
      XT: String;
      end;

  const
    OldStdHdr : array[1..23] of Char='Navigator Long Copy '#13#10#26;
    NewStdHdr : array[1..23] of Char='Navigator NLong Copy'#13#10#26;

  var
    F1, F2: lFile;
    Cs, Cd: LongInt;
    Ls: LongInt;
    Tuda: Boolean;
    i: Byte;
    NumW: Word;
    Count: Byte;
    wx, wy: Integer;
    wl: Byte;
    d, Fn2: String;
    n, x: String;
    PathBuffer: array[0..255] of Char; {целевой каталог}
    nhd: NHdr;
    ohd: OHdr absolute nhd;
    PInfo: PWhileView;
    CopyCancelled: Boolean;
    BinarySplit: Boolean;
    NewMode: Boolean;
    DiskVerify: Boolean;
    LastPos: TFileSize;
    Olddate: LongInt;
    PartSize: LongInt;
    PartWrt: LongInt;
    CopyDir: String;
    R: TRect;
    PP: Pointer;
    RC: LongInt;
    csv: Byte;

  procedure DsplInfo;
    begin
    PInfo^.Write(5, GetString(dlRead)+
      Copy(Strg(#219, (LongInt(WL)*Cs) div (Ls + Byte(Ls=0)))+Strg(#177, WL), 1, WL));
    PInfo^.Write(6, GetString(dlWrite)+
      Copy(Strg(#219, (LongInt(WL)*CD) div (Ls + Byte(Ls=0)))+Strg(#177, WL), 1, WL));
    end;

  procedure MaxRead;
    var
      i: Byte;
      b: Boolean;
    begin
    b := True;
    DsplInfo;
    for i := 1 to NBf do
      begin
      if b then
        BlockRead(F1.F, Bf[i].Buf^, Bf[i].Num, Bf[i].NR)
      else
        Bf[i].NR := 0;
      if IOResult <> 0 then
        begin
        MessageBox(GetString(dlFBBNotReadSource)+Fn1, nil,
           mfError+mfOKButton);
        Abort := True;
        NewTimer(Timer, 0);
        end;
      DispatchEvents(PInfo, CopyCancelled);
      if Abort or CopyCancelled then
        begin
        Close(F2.F);
        lEraseFile(F2);
        Exit;
        end;
      Bf[i].NW := 0;
      Inc(Cs, Bf[i].NR);
      DsplInfo;
      b := Bf[i].NR <> 0;
      end;
    end { MaxRead };

  function OverWr: Boolean;
    var
      SR: lSearchRec;
      lF: lFile;
    begin
    FreeSpc := SysDiskSizeLongX(@PathBuffer);
    DosError := 0;
    OverWr := True;
    Abort := False;
    lFindFirst(D+n+'.d??', AnyFileDir, SR); {JO}
    while (DosError = 0)
         and not (SR.FullName[Length(SR.FullName)] in ['0'..'9'])
      and not (SR.FullName[Length(SR.FullName)-1] in ['0'..'9'])
    do
      lFindNext(SR);
    if  (DosError = 0) and ((PartSize = 0) or (FreeSpc < PartSize)) then
      begin
      i := MessageBox
            (GetString(dlFBBOver1)+SR.FullName+GetString(dlFBBOver2), nil,
          mfQuery+mfYesButton+mfNextDButton+mfCancelButton);
      NewTimer(Timer, 0);
      Abort := i = cmCancel;
      if i = cmYes
      then
        EraseFile(D+SR.FullName)
      else
        OverWr := i <> cmCancel;
      end;
    lFindClose(SR);
    end { OverWr: };

  procedure RequireDisk;
    begin
    Abort := MessageBox(GetString(dlFBBInsertDisk)+Dec2(Count),
         nil,
        mfConfirmation+mfOKButton+mfCancelButton) <> cmOK;
    NewTimer(Timer, 0);
    end;

  procedure RequireNew;
    label 1;
    begin
    PartWrt := 0;
    Inc(Count);
1:
    repeat
      if Count > 1 then
        begin
        SetFTime(F2.F, Olddate);
        Close(F2.F);
        ClrIO;
        FreeSpc := SysDiskSizeLongX(@PathBuffer);
        if  (PartSize = 0) or (FreeSpc < PartSize) then
          RequireDisk;
        end;
      lFSplit(Fn2, d, n, X);
      Strings.StrPCopy(@PathBuffer, d);
      Fn2 := d+n+'.d'+Dec2(Count);
    until OverWr or Abort or CopyCancelled;
    if Abort or CopyCancelled then
      Exit;
    FreeSpc := SysDiskSizeLongX(@PathBuffer);
    while (FreeSpc < 512) do
      begin
      i := MessageBox(GetString(dlFBBDiskFull1), nil,
           mfWarning+mfOKButton+mfCancelButton);
      NewTimer(Timer, 0);
      FreeSpc := SysDiskSizeLongX(@PathBuffer);
      if i <> cmOK then
        begin
        Abort := True;
        Exit;
        end;
      end;
    CreateDirInheritance(D, False);
    lAssignFile(F2, Fn2);
    PInfo^.Write(3, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(Fn2));
    ClrIO;
    lReWriteFile(F2, 1);
    if not BinarySplit then
      if not NewMode then
        BlockWrite(F2.F, ohd, SizeOf(OHdr))
      else
        BlockWrite(F2.F, nhd, SizeOf(NHdr)-254+Length(nhd.XT));
    if IOResult <> 0 then
      begin
      i := MessageBox(GetString(dlFBBNoCreate)+Dec2(Count), nil,
          mfError+mfOKButton+mfCancelButton);
      NewTimer(Timer, 0);
      if i <> cmOK then
        begin
        Abort := True;
        Exit;
        end;
      goto 1;
      end;
    end { RequireNew };

  procedure MaxWrite;
    label Rep, 1, 2, 33;
    var
      i: Byte;
      wr: LongInt;
    begin
Rep:
    if Abort or CopyCancelled then
      Exit;
    for i := 1 to NBf do
      if Bf[i].NW < Bf[i].NR then
        begin
        wr := LongInt(Bf[i].NR)-LongInt(Bf[i].NW);
2:
        LastPos := FilePos(F2.F);
        if PartSize <> 0
        then
          BlockWrite(F2.F, Bf[i].Buf^[Bf[i].NW+1], Min(wr,
               PartSize-PartWrt), NumW)
        else
          BlockWrite(F2.F, Bf[i].Buf^[Bf[i].NW+1], Word(wr), NumW);
        if DiskVerify then
          begin
          {$IFDEF DPMI32}
          asm
             mov ah,0dh
             int 21h
           end;
          {$ENDIF}
          Seek(F2.F, LastPos);
          if PartSize <> 0
          then
            BlockRead(F2.F, Bf[i].Buf^[Bf[i].NW+1], Min(wr,
                 PartSize-PartWrt), NumW)
          else
            BlockRead(F2.F, Bf[i].Buf^[Bf[i].NW+1], Word(wr), NumW);
          end;
        if  (IOResult <> 0) then
          begin
          MessageBox(GetString(dlFBBNoWrite)+Fn2, nil, mfError+mfOKButton);
          Abort := True;
          NewTimer(Timer, 0);
          end;
        if Abort then
          goto 33;
        if  (NumW = 0) or ((PartSize <> 0) and (PartWrt = PartSize))
        then
          begin
          {Cat:warn}
          wr := DiskFree(dr2);
          if  (wr = 0) or (PartWrt = PartSize) then
            goto 1
          else
            goto 2;
          end;
        Inc(Bf[i].NW, NumW);
        Inc(CD, NumW);
        Inc(PartWrt, NumW);
        DsplInfo;
        DispatchEvents(PInfo, CopyCancelled);
        if Abort or CopyCancelled then
          begin
33:
          CopyCancelled := True;
          Close(F2.F);
          lEraseFile(F2);
          Exit;
          end;
1:
        if  (NumW = 0) or ((PartWrt = PartSize) and (PartSize <> 0))
        then
          if Tuda then
            begin
            MessageBox(GetString(dlFBBDiskFull2), nil, mfError+mfOKButton);
            NewTimer(Timer, 0);
            Abort := True;
            Exit;
            end
          else
            begin
            if (ElapsedTime(Timer) > 20*1000) and
                (FMSetup.Options and fmoBeep <> 0)
            then
              BeepAftercopy;
            if Cd < Ls then ReQuireNew;
            goto Rep;
            end;
        if Bf[i].NR > Bf[i].NW then
          Dec(i);
        end;
    end { MaxWrite };

  function CheckSvoi: Byte;
    var
      b: Byte;
    begin
    b := 0;
    FillChar(nhd, SizeOf(NHdr), 0);
    Seek(F1.F, 0);
    BlockRead(F1.F, nhd, MinBufSize(FileSize(F1.F), SizeOf(NHdr)));
    if ohd.nm=OldStdHdr then
      begin
       b:=1;
       Seek(F1.f, SizeOf(OHdr));
      end;
    if nhd.nm=NewStdHdr then
      begin
       b:=2;
       Seek(F1.f, SizeOf(NHdr) - 254 + Length(nhd.xt));
      end;
    CheckSvoi := b;
    end { CheckSvoi: };

  procedure RequirePath;
    var
      D: PDialog;
      V: PLabel;
      S, K, L: String;
    begin
    D := PDialog(LoadResource(dlgNextSection));
    V := PLabel(D^.FirstThat(@IsLabel));
    S := V^.Text^;
    DisposeStr(V^.Text);
    Replace('%D', ItoS(Count), S);
    V^.Text := NewStr(S);
    lFSplit(Fn1, S, K, L);
    D^.SetData(S);
    if Desktop^.ExecView(D) = cmCancel then
      begin
      Dispose(D, Done);
      Abort := True;
      Exit
      end;
    Abort := False;
    D^.GetData(S);
    Dispose(D, Done);
    Fn1 := MakeNormName(S, K+L);
    NewTimer(Timer, 0);
    end { RequirePath };

  procedure InsCor;
    var
      j: Byte;
    begin
    Inc(Count);
    j := 0;
    repeat
      Close(F1.F);
      if  (Count > 1) and (j > 0) then
        begin
        ClrIO;
        RequirePath;
        if Abort then
          Exit;
        end;
      Inc(j);
      ClrIO;
      lFSplit(Fn1, D, n, X);
      Fn1 := D+n+'.d'+Dec2(Count);
      FileMode := 0;
      lAssignFile(F1, Fn1);
      lResetFile(F1, 1);
      PInfo^.Write(1, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(Fn1));
    until ((IOResult = 0) and (CheckSvoi <> 0))
     or Abort or CopyCancelled;
    end { InsCor };

  procedure Nedochit;
    var
      i: Byte;
      b: Boolean;
    begin
    b := False;
    for i := 1 to NBf do
      b := b or (Bf[i].NR = 0);
    if b and (CD < LS) then
      InsCor;
    end;

  function CopyDialog: Boolean;
    var
      D: PDialog;
      P: PView;
      R: TRect;
      S, Mask: String;
      SR: lSearchRec;
      I: Integer;
      L: Longint;
      F: lFile;
      DTA: record
        S: String;
        W: Word;
        SPLSiz, NumSect: String[10]
        end;
      a, aa, aaa: String;

    const
      x_x = '*.';
    begin
    lFSplit(Fn1, a, aa, aaa);
    CopyDialog := False;
    if Tuda
    then
      D := PDialog(LoadResource(dlgCombineFile))
    else
      D := PDialog(LoadResource(dlgSplitFile));

    // Строка 1 в диалоге должна быть пустой - туда вставляется имя файла
    R.Assign(0, 1, Length(aa)+Length(aaa), 2);
    P := New(PStaticText, Init(R,
          {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(aa+aaa)));
    P^.Options := P^.Options or ofCenterX;
    D^.Insert(P);

    S := '';
    HistoryAdd(hsFBBCopy, lFExpand(a));
    GlobalMessage(evCommand, cmPushFirstName, @S);
    GlobalMessageL(evCommand, cmPushName, hsFBBCopy);
    if S[2] <> ':' then
      S := '';
    if S = '' then
      S := HistoryStr(hsFBBCopy, 0);
    DTA.S := {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(S);
    {    DTA.W := 0; }
    DTA.SPLSiz := '';
    DTA.NumSect := '';

    I := Byte(UpCase(S[1]));
    if I in [65..90] then
      begin
      if  (SystemData.Drives[Char(I)] and 16) > 0 then
        DTA.W := 2 or 4
      else
        DTA.W := 0 or 4;
      end;

    D^.SetData(DTA);
    if Desktop^.ExecView(D) = cmCancel then
      begin
      Dispose(D, Done);
      Exit
      end;
    D^.GetData(DTA);
    BinarySplit := DTA.W and 1 <> 0;
    DiskVerify := DTA.W and 2 <> 0;
    if not Tuda then
      begin
      NewMode := DTA.W and 4 <> 0;
      Val(DTA.SPLSiz, PartSize, I);
      if I <> 0 then
        begin
        PartSize := 0;
        Val(DTA.NumSect, PartSize, I);
        if  (I = 0) and (PartSize > 0) then
          begin
          ClrIO;
          lAssignFile(F, Fn1);
          lResetFile(F, 1);
          L := i32(FileSize(F.F)); {!!s}
          PartSize := (L div PartSize)+Byte((L mod PartSize) <> 0);
          Close(F.F);
          end;
        end
      else
        begin
        if BinarySplit
        then
          if PartSize < 0
          then
            PartSize := 0
          else
        else if NewMode
        then
          if PartSize <= SizeOf(NHdr)-253+Length(aaa)
          then
            PartSize := 0
          else
            Dec(PartSize, SizeOf(NHdr)-253+Length(aaa))
        else if PartSize <= SizeOf(OHdr)
        then
          PartSize := 0
        else
          Dec(PartSize, SizeOf(OHdr));
        end;
      end
    else
      PartSize := 0;
    S := {$IFDEF RecodeWhenDraw}OemToCharStr {$ENDIF}(DTA.S);
    Dispose(D, Done);
    if S = '' then
      S := '.';
    S := lFExpand(S);
    if (DTA.S[Length(DTA.S)] = '\') or IsDir(S) then
      MakeSlash(S);
    if S[Length(S)] = '\' then
      begin
      CopyDir := S;
      Mask := x_x
      end
    else
      begin
      lFSplit(S, CopyDir, n, X);
      Mask := n+X
      end;
    Fn1 := lFExpand(Fn1);
    lFSplit(Fn1, S, n, X);
    S := n+X;
    Fn2 := MakeNormName(CopyDir, MkName(S, Mask));
    CopyDialog := True;
    S := Copy(GetExt(Fn1), 2, MaxStringLength);
    if Tuda then
      begin
      if GetExt(Fn2) = '.' then
        if NewMode then
          if Fn2[Length(Fn2)] = '.' then
            Fn2 := Fn2+nhd.XT
          else
            Fn2 := Fn2+'.'+nhd.XT
        else if Fn2[Length(Fn2)] = '.' then
          Fn2 := Fn2+ohd.XT
        else
          Fn2 := Fn2+'.'+ohd.XT;
      end
    else if NewMode then
      begin
      nhd.XT := S;
      Move(NewStdHdr, nhd.nm, SizeOf(NewStdHdr));
      end
    else
      begin
      Move(S[1], ohd.XT, 3);
      Move(OldStdHdr, ohd.nm, SizeOf(OldStdHdr));
      end;
    end { CopyDialog: };

  begin { LongCopy }
  NewTimer(Timer, 0);
  Count := 0;
  Tuda := False;
  Abort := False;
  CopyCancelled := False;
  BinarySplit := False;
  NewMode := True;
  Fn1 := lFExpand(Fn1);
  lFSplit(Fn1, d, n, x);
  dr1 := Byte(Fn1[1])-64;
  Cs := 0;
  Cd := 0;
  FileMode := $40;
  lAssignFile(F1, Fn1);
  ClrIO;
  lResetFile(F1, 1);
  RC := IOResult;
  if RC <> 0 then
    begin
    MessFileNotOpen(Fn1, RC);
    Exit;
    end;
  GetFtime(F1.F, Olddate);
  lFSplit(Fn1, d, n, x);

  Tuda := (Length(x) = 4) and (UpStrg(x[2]) = 'D')
         and (StoI(Copy(x, 3, 2)) > 0);
  csv := CheckSvoi;
  if Tuda and (csv = 0) then
    begin
    Close(F1.F);
    Message(Desktop, evCommand, cmSingleCopy, nil);
    Exit;
    end;

  if Tuda then
    Count := 1;
  if Tuda and ((x[4] <> '1') or (x[3] <> '0')) then
    begin
    x[3] := '0';
    x[4] := '1';
    Fn1 := d+n+x;
    repeat
      Close(F1.F);
      ClrIO;
      Count := 1;
      RequirePath;
      lAssignFile(F1, Fn1);
      if Abort then
        Exit;
      FileMode := 0;
      lResetFile(F1, 1);
      csv := CheckSvoi;
    until (IOResult = 0) and (csv <> 0);
    end;
  if csv = 1 then
    NewMode := False
  else
    NewMode := True;
  if not Tuda then
    begin
    Ls := i32(FileSize(F1.F)); {!!s}
    Seek(F1.F, 0);
    end
  else if NewMode then
    Ls := nhd.ln
  else
    Ls := ohd.ln;

  if  (not CopyDialog) or Abort then
_Abort_:
    begin
    Close(F1.F);
    Exit;
    end;

  if NewMode then
    nhd.ln := Ls
  else
    ohd.ln := Ls;

  Fn2 := lFExpand(Fn2);
  if Abort then
    goto _Abort_;
  dr2 := Byte(Fn2[1])-64;
  if Fn1 = Fn2 then
    goto _Abort_;

  R.Assign(1, 1, 36, 13);
  New(PInfo, Init(R));
  if PInfo = nil then
    goto _Abort_;
  if Tuda then
    PInfo^.Top := GetString(dlFBBDEFragment)
  else
    PInfo^.Top := GetString(dlFBBFragment);
  PInfo^.Bottom := '';
  PInfo^.SetState(sfShadow, True);
  Desktop^.Insert(PInfo);
  PInfo^.Write(1, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(Fn1));
  lFSplit(Fn2, d, n, x);
  if Tuda and (UpCase(x[2]) = 'D') and (StoI(Copy(x, 3, 2)) > 0) then
    case csv of
      1:
        Fn2 := d+n+'.'+ohd.XT;
      2:
        Fn2 := d+n+'.'+nhd.XT;
    end {case};
  lAssignFile(F2, Fn2);
  ClrIO;
  PInfo^.Write(2, GetString(dlFBBFragmentTo));
  PInfo^.Write(3, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(Fn2));
  wl := PInfo^.Size.x-8;
  if not Tuda then
    RequireNew
  else
    begin
    CreateDirInheritance(d, False);
    lResetFile(F2, 1);
    if IOResult = 0 then
      begin
      PP := @Fn2;
      if MessageBox(GetString(dlFileExist)+^M, @PP,
           mfQuery+mfYesButton+mfNoButton+mfCancelButton)
        <> cmYes
      then
        begin
        Desktop^.Delete(PInfo);
        Dispose(PInfo, Done);
        Close(F2.F);
        goto _Abort_;
        end;
      end;
    ClrIO;
    lReWriteFile(F2, 1);
    if  (IOResult <> 0) or Abort then
      begin
      MessageBox(GetString(erCantCreateFile)+Fn2, nil, mfError+mfOKButton);
      Desktop^.Delete(PInfo);
      Dispose(PInfo, Done);
      goto _Abort_;
      end;
    end;
  GetMaxMem;
  if NBf = 0 then
    begin
    Close(F2.F);
    lEraseFile(F2);
    goto _Abort_;
    end;
  Inc(SkyEnabled);
  PartWrt := 0;
  repeat
    MaxRead;
    MaxWrite;
    if Tuda then
      Nedochit;
  until (Cd >= Ls) or Abort or CopyCancelled;
  Dec(SkyEnabled);
Ex:
  Desktop^.Delete(PInfo);
  Dispose(PInfo, Done);
  Close(F1.F);
  SetFTime(F2.F, Olddate);
  Close(F2.F);
  if  (Abort or CopyCancelled) and Tuda then
    lEraseFile(F2);
  ClearMem;
  Abort := False;
  if not Startup.AutoRefreshPanels then
    begin
    GlobalMessage(evCommand, cmPanelReread, @CopyDir);
    {GlobalMessage(evCommand, cmRereadInfo, nil);}
    GlobalMessage(evCommand, cmRereadTree, @CopyDir);
    end;
  if not (Abort or CopyCancelled) then

    if  (FMSetup.Options and fmoBeep <> 0) and
        (ElapsedTime(Timer) > 20*1000)
    then
      BeepAftercopy;
  end { LongCopy };
{-DataCompBoy-}

end.
