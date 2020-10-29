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
unit Filelst;

interface

uses
  Objects
  ;

procedure MakeListFile(APP: Pointer; Files: PCollection);
function ParseAddress(Address: String; var Zone, Net, Node, Point: Word)
  : Boolean;

implementation
uses
  Startup, Lfnvp, Messages, Defines, FilesCol, Advance2, Advance1, UserMenu,
  Advance, HistList, Commands, DNApp, DNUtil, Tree, Views, Drivers, Drives
  {, dnfuncs} {надо вставлять до Dos}
  , Dos, Dialogs, Objects2
  , ErrMess, FlPanelX
  ;
type
  { Диалог создания списка файлов. В ресурсе должны быть
  DirectLink на строку ввода имени файла (1) и строку ввода
  шаблона строки файла (2) }
  PMakeListDlg = ^TMakeListDlg;
  TMakeListDlg = object(TDialog)
    procedure HandleEvent(var Event: TEvent); virtual;
      { Для реакции на кнопки }
    end;

procedure InpLineReplace(P: PInputLine; const S: String);
  begin
  with P^ do
    begin
    Select;
    SelStart := CurPos;
    SelEnd := CurPos;
    System.Insert(S, Data, 1+CurPos);
    Inc(CurPos, Length(S));
    SelStart := CurPos;
    SelEnd := CurPos;
    Draw;
    end;
  end;

procedure TMakeListDlg.HandleEvent(var Event: TEvent);
  var
    i: Integer;
  begin
  if Event.What = evCommand then
    begin
    case Event.Command of
      cmYes:
        begin
        InpLineReplace(PInputLine(DirectLink[2]), '!:!\!.!');
        ClearEvent(Event);
        Exit;
        end;
      cmNo:
        begin
        InpLineReplace(PInputLine(DirectLink[2]), '#:#\#.#');
        ClearEvent(Event);
        Exit;
        end;
      cmOK:
        begin { Не выпускаем с пустым именем списка или
          с пустым шаблоном обработки файла }
        for i := 1 to 2 do
        if PInputLine(DirectLink[i])^.Data = '' then
          begin
          PInputLine(DirectLink[i])^.Select;
          ClearEvent(Event);
          Exit;
          end;
        end;
    end {case};
    end;
  inherited HandleEvent(Event);
  end;

procedure PrepareMakeListDialog(P: PDialog);
  begin
  ObjChangeType(P, TypeOf(TMakeListDlg));
  end;

function ParseAddress(Address: String; var Zone, Net, Node, Point: Word)
  : Boolean;
  var
    I, J: Integer;
  begin
  ParseAddress := False;
  Point := 0;
  I := PosChar('@', Address);
  if I > 0 then
    Delete(Address, I, 255);
  I := PosChar(':', Address);
  if I > 0 then
    begin
    Val(Copy(Address, 1, I-1), Zone, J);
    if J <> 0 then
      Exit;
    Delete(Address, 1, I);
    end;
  I := PosChar('/', Address);
  if I > 0 then
    begin
    Val(Copy(Address, 1, I-1), Net, J);
    if J <> 0 then
      Exit;
    Delete(Address, 1, I);
    end;
  I := PosChar('.', Address);
  if I > 0 then
    begin
    Val(Copy(Address, I+1, MaxStringLength), Point, J);
    if J <> 0 then
      Exit;
    Delete(Address, I, MaxStringLength);
    end;
  if Length(Address) > 0 then
    begin
    Val(Address, Node, J);
    if J <> 0 then
      Exit;
    end;
  ParseAddress := True;
  end { ParseAddress };

procedure MakeListFile;
  label AddrError, Retry;
  var
    I, J, K: Integer;
    D, Dr, SR, Sn: String;
    P: PFileRec;
    S: TMakeListRec;
    T, HF: lText;
    Nm: String;
    Xt: String;
    FidoMode: Boolean;
    SS: String;
    PP: PString;
    ZZ, NN, ND, PT: Word;
    BB: Boolean;
    FLD, Dr_: String;
    CurPos: Integer;
    UPr: tUserParams;
    RC: LongInt;

  function GetNextName(var TheString, TheName: String): Boolean;
    var
      j: Boolean;
    begin
    while (CurPos <= Length(TheString))
         and (TheString[CurPos] in [';', ' '])
    do
      Inc(CurPos);
    TheName := '';
    j := False;
    while (CurPos <= Length(TheString))
         and ((TheString[CurPos] <> ';') or j)
    do
      begin
      if TheString[CurPos] = '"' then
        j := not j;
      TheName := TheName+TheString[CurPos];
      Inc(CurPos)
      end;
    DelRight(TheName);
    GetNextName := TheName <> ''
    end { GetNextName };

  function SomeFilesExist(var NameList: String): Boolean;
    var
      TheName: String;
    begin
    SomeFilesExist := False;
    CurPos := 1;
    while GetNextName(NameList, TheName) do
      if ExistFile(TheName)
      then
        begin
        SomeFilesExist := True;
        Exit
        end
    end;

  procedure MakeStr(D: String);
    label Fail;
    var
      Drr: Boolean;
    begin
    { BB means "Is filename occurs in Action?" }
    Replace('!!', #1, D);
    Replace('##', #2, D);
    Replace('$$', #3, D);
    Replace('&&', #4, D);
    Replace(#1, '!!', D);
    Replace(#2, '##', D);
    Replace(#3, '$$', D);
    Replace(#4, '&&', D);
    D := MakeString(D, @UPr, False, nil);
    Writeln(T.T, D);
    end { MakeStr };

  begin { MakeListFile }
  if Files^.Count = 0 then
    Exit;
  Message(APP, evBroadcast, cmGetUserParams, @UPr);
  FillChar(S, SizeOf(S), 0);
  S.FileName := HistoryStr(hsMakeList, 0);
  if S.FileName = '' then
    S.FileName := 'DNLIST'+CmdExt;
  S.Action := HistoryStr(hsExecDOSCmd, 0);
  S.Header := HistoryStr(hsMakeListHeader, 0); {JO}
  {S.Header := '';}S.HeaderMode := hfmAuto;
  S.Footer := HistoryStr(hsMakeListFooter, 0); {JO}
  {S.Footer := '';}S.FooterMode := hfmAuto;
  @PreExecuteDialog := @PrepareMakeListDialog;
  if  (ExecResource(dlgMakeList, S) <> cmOK) then
    Exit;
  DelRight(S.Action);
  DelRight(S.Header);
  DelRight(S.Footer);
  FileMode := 2;
  Abort := False;
  if S.FileName[1] in ['+', '%', '/'] then
    begin
Retry:
    FidoMode := True;
    SS := FMSetup.DIZ;
    I := Pos('/FIDO=', SS);
    if I = 0 then
      begin
      SS := '';
AddrError:
      if ExecResource(dlgFTNInfo, SS) = cmCancel then
        Exit;
      if PosChar(',', SS) > 0 then
        I := Pos('/FIDO=', FMSetup.DIZ);
      if I > 0 then
        begin
        for J := I to 255 do
          if FMSetup.DIZ[J] = ';' then
            Break;
        Delete(FMSetup.DIZ, I, J-I+1);
        end;
      if FMSetup.DIZ[Length(FMSetup.DIZ)] <> ';' then
        FMSetup.DIZ := FMSetup.DIZ+';';
      FMSetup.DIZ := FMSetup.DIZ+'/FIDO='+SS;
      Message(Application, evCommand, cmUpdateConfig, nil);
      goto Retry;
      end;
    Delete(SS, 1, I+5);
    I := PosChar(';', SS);
    if I = 0 then
      I := Length(SS)+1;
    SetLength(SS, I-1);
    I := PosChar(',', SS);
    if I = 0 then
      goto AddrError;
    ParseAddress(Copy(SS, 1, I-1), ZZ, NN, ND, PT);
    K := ZZ;
    ParseAddress(Copy(S.FileName, 2, MaxStringLength), ZZ, NN, ND, PT);
    Delete(SS, 1, I);
    MakeNoSlash(SS);
    lFSplit(SS, Dr, Nm, Xt);
    if K <> ZZ then
      Xt := '.'+Copy(Hex4(ZZ), 2, 3);
    SS := Dr+Nm+Xt;
    if PT = 0 then
      Nm := Hex4(NN)+Hex4(ND)
    else
      begin
      SS := MakeNormName(SS, Hex4(NN)+Hex4(ND)+'.PNT\');
      Nm := Hex8(PT);
      end;
    Xt := '.hlo';
    case S.FileName[1] of
      '+':
        Xt[2] := 'c';
      '%':
        Xt[2] := 'f';
    end {case};
    SS := MakeNormName(SS, Nm+Xt);
    S.FileName := SS;
    end
  else
    FidoMode := False;
  D := lFExpand(S.FileName);
  lFSplit(D, Dr, Nm, Xt);
  ClrIO;
  FLD := Dr;
  CreateDirInheritance(Dr, False);
  if Abort then
    Exit;
  lAssignText(T, D);
  ClrIO;
  lResetText(T);
  if IOResult = 0 then
    begin
    Close(T.T);
    PP := @SS;
    SS := Cut(D, 40);
    if FidoMode then
      I := cmOK
    else
      I := MessageBox(GetString(dlED_OverQuery)
          , @PP, mfYesButton+mfCancelButton+mfAppendButton+mfWarning);
    case I of
      cmOK:
        lAppendText(T);
      cmYes:
        lRewriteText(T);
      else {case}
        Exit;
    end {case};
    end
  else
    lRewriteText(T);
  if Abort then
    begin
    Close(T.T);
    Exit;
    end;
  RC := IOResult;
  if RC <> 0 then
    begin
    MessFileNotOpen(Cut(S.FileName, 40), RC);
    Exit;
    end;
  if  (S.Header <> '') and not FidoMode then
    begin
    if  (S.HeaderMode = hfmInsertText) or
        ( (S.HeaderMode = hfmAuto) and not SomeFilesExist(S.Header))
    then
      Writeln(T.T, S.Header)
    else
      begin
      CurPos := 1;
      while GetNextName(S.Header, SS) do
        begin
        lAssignText(HF, SS);
        ClrIO;
        lResetText(HF);
        RC := IOResult;
        if  (RC <> 0) and (S.HeaderMode = hfmInsertFiles) then
          begin
          MessFileNotOpen(Cut(SS, 40), RC);
          end;
        while (IOResult = 0) and not Eof(HF.T) do
          begin
          Readln(HF.T, SS);
          Writeln(T.T, SS);
          end;
        Close(HF.T)
        end;
      ClrIO;
      end;
    end;
  Message(Desktop, evBroadcast, cmGetUserParams, @UPr);
  for I := 1 to Files^.Count do
    begin
    P := Files^.At(I-1);
    UPr.Active := P;
    {AK155 23-09-2003: разотметка по одному файлу тормозит страшно при
большом числе файлов
    Message(APP, evCommand, cmCopyUnselect, P);
/AK155}
    BB := False;
    SR := P^.Owner^;
    Replace('!', #0, SR);
    MakeSlash(SR);
    SS := S.Action;
    Replace(';;', #1, SS);
    while SS <> '' do
      begin
      J := PosChar(';', SS);
      if J = 0 then
        J := Length(SS)+1;
      MakeStr(fReplace(#1, ';', Copy(SS, 1, J-1)));
      Delete(SS, 1, J);
      end;
    end;
  if  (S.Footer <> '') and not FidoMode then
    begin
    if  (S.FooterMode = hfmInsertText) or
        ( (S.FooterMode = hfmAuto) and not SomeFilesExist(S.Footer))
    then
      Writeln(T.T, S.Footer)
    else
      begin
      CurPos := 1;
      while GetNextName(S.Footer, SS) do
        begin
        lAssignText(HF, SS);
        ClrIO;
        lResetText(HF);
        RC := IOResult;
        if  (RC <> 0) and (S.FooterMode = hfmInsertFiles) then
          begin
          MessFileNotOpen(Cut(SS, 40), RC);
          end;
        while (IOResult = 0) and not Eof(HF.T) do
          begin
          Readln(HF.T, SS);
          Writeln(T.T, SS);
          end;
        Close(HF.T)
        end;
      ClrIO;
      end;
    end;
  Close(T.T);
  { AK155 23-09-2003 Теперь скопом снимаем всю отметку. Делать это надо
обязательно до RereadDirectory, так как она страшно тормзит при большом
числе отмеченных файлов. }
  ClearSelection(APP, PFilePanelRoot(APP)^.Files);
  {/AK155}
  RereadDirectory(Dr);
  GlobalMessage(evCommand, cmRereadInfo, nil);
  GlobalMessage(evCommand, cmRereadTree, @Dr);
  end { MakeListFile };
{-DataCompBoy-}

end.
