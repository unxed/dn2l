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

unit Eraser;

interface

uses
  Collect
  ;

procedure EraseFiles(Files: PCollection);
procedure SetVLabel;
function ValidErase(Files: PCollection): Boolean;

implementation
uses
  Defines, Files, Filediz,
  Dos, Lfnvp {DataCompBoy}, FilesCol, Commands, Advance, Advance1, Advance2,
  Startup, Messages, xTime, Drivers, Tree, Memory,
  DNApp, Gauge, Views, Dialogs, Drives, FileCopy
  , fnotify, Events
  {JO} {$IFDEF OS2}, VPUtils {$ENDIF}, FlTl

  ;

{-DataCompBoy-}
function ValidErase;
  var
    PF: PFileRec;
    S: String;
    I: Integer;
  begin
  ValidErase := False;
  if Files = nil then
    Exit;
  if Files^.Count = 0 then
    Exit;
  if Files^.Count = 1 then
    begin
    PF := Files^.At(0);
    if PF^.Attr and Directory <> 0 then
      S := GetString(dlEraseConfirmDir)
    else
      S := GetString(dlEraseConfirm1);
    S := S+'"'+ {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}
          (Cut(PF^.FlName[True], 40))+'"'+' ?';

    end
  else
    S := GetString(dlEraseConfirms1);
  if  (Files^.Count = 1) and (Confirms and cfSingleErase = 0) then
    I := cmYes
  else
    I := MessageBox(S, nil, mfConfirmation+mfYesButton+mfNoButton);
  if  (I <> cmYes) then
    Exit;
  if  (Files^.Count > 1) and (Confirms and cfMultiErase <> 0) then
    begin
    S := GetString(dlEraseConfirm2)+ItoS(Files^.Count)
        +' '+GetString(dlDIFiles)+' ?';
    if MessageBox(S, nil, mfConfirmation+mfYesButton+mfNoButton)
       <> cmYes
    then
      Exit;
    end;
  ValidErase := True;
  end { ValidErase };
{-DataCompBoy-}

{-DataCompBoy-}
procedure EraseFiles;
  var
    PInfo: PWhileView;
    R: TRect;
    EraseCancel: Boolean;
    Tmr: TEventTimer;
    I, J: Integer;
    S, Dr: String;
    ch: Char;
    F: lFile;
    DOSMode, DirEnd, DirModified: Boolean;
    PF: PFileRec;
    //    DrivesSet: Set of Char;
    RereadCollection: PStringCollection; {Cat}
    PS: PString; {Cat}
    DeleteAllFiles: Boolean;
    Params: record
      {AK155}
      FName: PString;
      RC: LongInt;
      end;

  procedure InfoWrite(N: Integer; const S: Str40);
    begin
    PInfo^.Write(N, S);
    end;

  procedure CalcCancel;
    begin
    if EraseCancel then
      Exit;
    if TimerExpired(Tmr) then
      begin
      DispatchEvents(PInfo, EraseCancel);
      EraseCancel := (EraseCancel or CtrlBreakHit)
             and (MessageBox(GetString(dlQueryAbort), nil, mfYesNoConfirm)
           = cmYes);
      CtrlBreakHit := False;
      NewTimer(Tmr, 50);
      end;
    end;

  function Over(S: String): Word;
    var
      I: Integer;
    begin
    if Confirms and cfEraseReadonly = 0 then
      begin
      Over := cmYes;
      Exit
      end;
    I := MessageBox(^C+GetString(dlFile)+' "'+Cut(S,
           40)+'"'+GetString(dlEraseRO), nil, mfYesNoConfirm+mfAllButton);
    if I = cmOK then
      DeleteAllFiles := True;
    Over := I;
    if I = cmOK then
      Over := cmYes;
    end;

  {AK155 При отказе (DOSDelDir=false) Params.RC возвращает код
завершения неудачной операции, если причина отказа - ошибка
операции с файлом или каталогом. При посторонних причинах отказа
(например, Abort) будет Params.RC=0}
  function DosDelDir: Boolean;
    var
      DC: PDirCol;
      TD: PDirCol;
      SR: lSearchRec;
      s: String;
    label TryDel;
    begin
    DosDelDir := False;
    Abort := False;
    Params.RC := 0;
    New(DC, Init($10, $10, False));
    New(TD, Init($10, $10, False));
    DC^.Insert(NewStr(FreeStr));

    while (DC^.Count>0) and (not EraseCancel) and (not Abort) do
      begin
      FreeStr := PString(DC^.At(0))^;
      DC^.AtFree(0);
      PInfo^.Write(2, Cut(GetName(FreeStr), 40));

      if Word(Length(FreeStr))+Word(Length(x_x)) > MaxPathLen-1 then
        begin
        Abort := True;
        Break
        end;

      lFindFirst(MakeNormName(FreeStr, x_x), AnyFileDir, SR); {JO}
      while DosError = 0 do
        begin
        CalcCancel; {JOHN_SW}
        if Abort or EraseCancel then
          Break; {JOHN_SW}
        if  (SR.FullName = '.') or (SR.FullName = '..') then
          begin
          lFindNext(SR);
          Continue
          end;
        if  (Word(Length(FreeStr))+Word(Length(SR.FullName)))
           > MaxPathLen-1
        then
          begin
          Abort := True;
          Break;
          end;
        if  (SR.SR.Attr and Directory) <> 0 then
          begin
          s := MakeNormName(FreeStr, SR.FullName);
          {$IFDEF Win32}
          if SR.SR.FindData.dwFileAttributes and
            FILE_ATTRIBUTE_REPARSE_POINT <> 0
          then
            begin { Симлинк: удаляем сам симлинк }
            lRmDir(s);
            Params.RC := IOResult;
            if Params.RC <> 0 then
              begin
              Abort := True;
              Break;
              end;
            end
          else
            {$ENDIF}
            begin
            if MaxAvail < Length(s)+$40
            then
              begin
              Abort := True;
              Break;
              end;
            DC^.AtInsert(0, NewStr(s));
            end;
          end
        else
          begin
          if  (Word(Length(FreeStr))+Word(Length(SR.FullName)))
             > MaxPathLen-1
          then
            begin
            Abort := True;
            Break;
            end;
          ClrIO;
          lAssignFile(F, MakeNormName(FreeStr, SR.FullName));
          lSetFAttr(F, 0);
          lEraseFile(F);
          Params.RC := IOResult;
          if Params.RC <> 0 then
            begin
            Abort := True;
            Break;
            end;
          end;
        lFindNext(SR);
        end;
      lFindClose(SR);
      if not Abort then
        begin
        if MaxAvail < Length(FreeStr)+$40 then
          begin
          Abort := True;
          Break
          end;
        if FreeStr[Length(FreeStr)] = '\'
        then
          TD^.AtInsert(0, NewStr(Copy(FreeStr, 1, Length(FreeStr)-1)))
        else
          TD^.AtInsert(0, NewStr(FreeStr));
        end
      else
        Break;
      end;
    DC^.FreeAll;
    Dispose(DC, Done);
    DosDelDir := not Abort;
    if not Abort then
      while (TD^.Count > 0) and (not EraseCancel) and (not Abort) do
        begin
        CalcCancel; {JOHN_SW}
        ClrIO;
        s := PString(TD^.At(0))^;
TryDel:
        lRmDir(s);
        Params.RC := IOResult;
        if Params.RC <> 0 then
          if SysErrorFunc(Params.RC, Byte(s[1])-Byte('A')) = 1 then
            goto TryDel;
        if Params.RC <> 0 then
          begin
          DosDelDir := False;
          FreeStr := PString(TD^.At(0))^;
          Break;
          end;
        TD^.AtFree(0);
        end;
    TD^.FreeAll;
    Dispose(TD, Done);
    end { DosDelDir: };

  function DeleteDirectory(Dir: String; Cluster: Word): Boolean;
    var
      S: String;
      dr: Word;
      j123qwe: Boolean;
      SR: lSearchRec;
    begin
    DeleteDirectory := False;
    Dir := lFExpand(Dir);
    if Dir[Length(Dir)] = '.' then
      SetLength(Dir, Length(Dir)-1);
    if not DeleteAllFiles then
      begin
      S := MakeNormName(Dir, x_x);
      DosError := 0;
      lFindFirst(S, AnyFileDir, SR); {JO}
      if IsDummyDir( {$IFDEF DPMI32}SR.SR.Name {$ELSE}SR.FullName
           {$ENDIF})
      then
        lFindNext(SR);
      if IsDummyDir( {$IFDEF DPMI32}SR.SR.Name {$ELSE}SR.FullName
           {$ENDIF})
      then
        lFindNext(SR);
      lFindClose(SR);
      if Abort then
        Exit;
      dr := cmOK;
      if  (DosError = 0) and (not DeleteAllFiles) then
        begin
        S := Dir;
        Dec(SkyEnabled);
        if Confirms and cfEraseSubDir = 0 then
          dr := cmYes
        else
          dr := MessageBox(^C+GetString(dlDirectory)+' '+Cut(S,
               40)+GetString(dlEraseDirNotEmpty),
              nil,

               mfConfirmation+mfNoButton+mfAllButton+mf2YesButton+mfCancelButton)
            ;
        Inc(SkyEnabled);
        DeleteAllFiles := dr = cmOK;
        Abort := dr = cmCancel;
        end;
      if not (dr in [cmYes, cmOK]) then
        Exit;
      end;
    FreeStr := Dir;
    j123qwe := not DosDelDir;
    if j123qwe then
      begin
      FreeStr := Cut(FreeStr, 40);
      Params.FName := @FreeStr;
      S := GetString(dlEraseCantDelDir);
      if Params.RC <> 0 then
        S := S+^M^C'(RC=%d)';
      MessageBox(S, @Params, mfError+mfOKButton);
      Abort := True;
      end;
    PInfo^.DrawView;
    end { DeleteDirectory };

  var
    Fls: PCollection;
    {$IFDEF DPMI32}
    Flush: Boolean;
    {$ENDIF}
    Ask: Integer;
    iLfn: TUseLFN;
    SkipAll: Boolean;
    D: PDialog;
    PStr1: PString;
  label LLL, DeleteDirDIZ;

  begin { EraseFiles }
  NotifySuspend; {<fnotify.001>}
  if not ValidErase(Files) then
    begin
    NotifyResume;
    Exit;
    end;
  LongWorkBegin;
  DeleteAllFiles := False;
  SkipAll := False;
  Abort := False;
  ClrIO;
  CtrlBreakHit := False; {JO}
  R.Assign(1, 1, 26, 9);
  New(PInfo, Init(R));
  PInfo^.Top := GetString(dlErase);
  DOSMode := True;

  EraseCancel := False;
  Abort := False;
  NewTimer(Tmr, 0);
  CalcCancel;

  if Abort then
    begin
    Dispose(PInfo, Done);
    NotifyResume; {Cat}
    LongWorkEnd;
    Exit;
    end;
  Inc(SkyEnabled);
  Desktop^.Insert(PInfo);

  for I := 1 to Files^.Count do
    begin
    PF := Files^.At(I-1);
    {JO: файлы, найденные в архивах нельзя удалить из панели поиска}
    if PathFoundInArc(PF^.Owner^) then
      Continue;
    {/JO}
    S := PF^.FlName[True];
    CalcCancel;
    if  (PF <> nil) and not EraseCancel and not Abort then
      if  (PF^.Attr and Directory = 0) then
        begin
        InfoWrite(1, GetString(dlErasingFile));
        InfoWrite(2, Cut(S, 40));
        J := cmYes;
        if not DeleteAllFiles and (PF^.Attr and ReadOnly <> 0) then
          J := Over(S);
        EraseCancel := EraseCancel or (J = cmCancel);
        if J = cmYes then
          begin
          S := MakeNormName(PF^.Owner^, S);
          lAssignFile(F, S);
          ClrIO;
          if PF^.Attr and ReadOnly <> 0 then
            begin
LLL:
            lSetFAttr(F, Archive);
            ClrIO;
            lEraseFile(F);
            end
          else
            begin
            lEraseFile(F);
            if IOResult <> 0 then
              goto LLL;
            end;
          if IOResult <> 0 then
            begin
            S := lFExpand(S);
            GlobalMessage(evBroadcast, cmReleaseFile, @S);
            ClrIO;
            lEraseFile(F);
            end;
          Ask := IOResult;
          {if IOResult = 0 then}
          if Ask = 0 then
            begin
            if  (PF^.DIZ <> nil) and
                (FMSetup.Options and fmoPreserveDesc = 0)
            then
              begin
              InfoWrite(1, GetString(dlDeletingDIZ));
              DeleteDiz(PF);
              end;
            end
          else
            if not SkipAll then begin
              D := PDialog(LoadResource(dlgSkipBadFile));
              D^.Options := D^.Options or ofCentered;
              R.A.X := 1; R.A.Y := 2; R.B.X := 53; R.B.Y := 3;
              D^.Insert(New(PStaticText, Init(R,GetString(dlErasingNoFile))));
              inc(R.A.Y); inc(R.B.Y);
              S := FormatLongName(S,50,0,0,nfmNull);
              D^.Insert(New(PStaticText, Init(R,^C+S)));
              Case Desktop^.ExecView(D) Of
               cmOK:     Goto LLL {Dec(I)};
               cmYes:    SkipAll := True;
               cmCancel: Break;
              End;
            end;
          end;
        end
      else
        begin
        InfoWrite(1, GetString(dlErasingDir));
        InfoWrite(2, Cut(S, 40));
        {$IFDEF DPMI32}
        S := lfGetLongFileName(MakeNormName(PF^.Owner^, S));
        {$ELSE}
        S := MakeNormName(PF^.Owner^, S);
        {$ENDIF}
        {$IFDEF WIN32}
        if PF^.Attr and FILE_ATTRIBUTE_REPARSE_POINT <> 0 then
          begin { Симлинк: удаляем без подтверждений сам симлинк}
          lRmDir(S);
          if IOResult = 0 then
            goto DeleteDirDIZ;
          end
        else
          {$ENDIF}
         if DeleteDirectory(S, 0) then
          begin
DeleteDirDIZ:
          DeleteDiz(PF);
          end;
        end
    else
      Break;
    end;
  {$IFDEF DPMI32}
  Flush := ((SystemData.Options shl 3) and ossFlushDsk <> 0);

  if Flush then
    begin
    PInfo^.ClearInterior;
    InfoWrite(1, GetString(dlFlushingBuffers));
    end
  else
    {$ENDIF}
    Dispose(PInfo, Done);

  {Cat: Во-первых, совершенно непонятно, зачем перечитывать _все_ диски
      Во-вторых, такой способ не работает с сетевыми путями}
  (*
  DrivesSet := [];
  for I := 0 to Files^.Count-1 do
      DrivesSet := [UpCase(PFileRec(Files^.At(I))^.Owner^[1])] + DrivesSet;
  S := 'A:\';
  for ch := 'A' to 'Z' do
    if ch in DrivesSet then
      begin
        S[1]:=ch;
        GlobalMessage(evCommand, cmRereadTree, @S);
        RereadDirectory(S);
      end;
*)
  RereadCollection := New(PStringCollection, Init(32, 32, False));
  {сортированная, без повторов}
  for I := 0 to Files^.Count-1 do
    begin
    RereadCollection^.Insert(PFileRec(Files^.At(I))^.Owner);
    if PFileRec(Files^.At(I))^.Attr and Directory <> 0 then
      begin
      PStr1 := NewStr('>'+MakeNormName(PFileRec(Files^.At(I))^.Owner^,
            PFileRec(Files^.At(I))^.FlName[True])+'\');
      RereadCollection^.Insert(PStr1);
      end;
    end;
  S := #0;
  for I := 0 to RereadCollection^.Count-1 do
    begin
    PS := RereadCollection^.At(I);
    if S <> Copy(PS^, 1, Length(S)) then
      {если уже перечитали вышележащий каталог, то этот перечитывать не надо}
      begin
      S := PS^;
      if S[1] <> '>' then
        GlobalMessage(evCommand, cmRereadTree, PS);
      RereadDirectory(S);
      end;
    end;
  RereadCollection^.DeleteAll;
  Dispose(RereadCollection, Done);
  {/Cat}
  GlobalMessage(evCommand, cmRereadInfo, nil);
  {$IFDEF DPMI32}
  if Flush then
    begin
    asm
     mov ah, 0dh
     int 21h
    end;
    Dispose(PInfo, Done);
    end;
  {$ENDIF}
  Dec(SkyEnabled);
  NotifyResume; {Cat}
  LongWorkEnd;
  end { EraseFiles };
{-DataCompBoy-}

procedure SetVLabel;
  {$IFNDEF OS2}
  begin
  end;
  {$ELSE}
  var
    S: String;
    rc: LongInt;
    Dr: Char;
  begin
  Dr := GetCurDrive;
  if Dr = '\' then
    Exit; {!! Это очень некрасивое решение. Лучше бы сделать неактивной
      команду cmSetVolumeLabel, если на активной панели сетевой диск.
      А заодно и если там архив. }
  S := {$IFDEF WIN32}CharToOemStr {$ENDIF}(GetVolumeLabel(Dr));
  {Cat}
  if ExecResource(dlgVolumeLabel, S) = cmOK then
    begin
    rc := SetVolume(GetDrive+1, S);
    if rc <> 0 then
      MessageBox
        (GetString(dl_Failed_to_set_volume_label)+^M^C', (RC=%d)', @rc,
         mfOKButton);
    end;
  end;
{$ENDIF}

end.
