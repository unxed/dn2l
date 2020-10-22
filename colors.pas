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

unit Colors;

interface

uses
  Dialogs, Collect
  ;

procedure ChangeColors;
procedure WindowManager;
procedure SetHighlightGroups;

type

  PWindowList = ^TWindowList;
  TWindowList = object(TListBox)
    function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    end;

  PWindowCol = ^TWindowCol;
  TWindowCol = object(TCollection)
    procedure FreeItem(Item: Pointer); virtual;
    end;

implementation
uses
  Messages, Drives, Startup, DnIni, VideoMan, Defines, DNApp,
  Drivers, Views, Memory, Commands, DNStdDlg, Advance, DNHelp, Advance2
  , FileType
  ;

procedure SetHighlightGroups;
  var
    D: record
      S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11: String[250];
      end;
  begin
  with D do
    begin
    S1 := CustomMask1;
    S2 := CustomMask2;
    S3 := CustomMask3;
    S4 := CustomMask4;
    S5 := CustomMask5;
    S6 := CustomMask6;
    S7 := CustomMask7;
    S8 := CustomMask8;
    S9 := CustomMask9;
    S10 := CustomMask10;
    S11 := Archives;
    if ExecResource(dlgHighlightGroups, D) <> cmOK then
      Exit;

    while S1[Length(S1)] in [' ', ';'] do
      SetLength(S1, Length(S1)-1);
    CustomMask1 := S1;
    while S2[Length(S2)] in [' ', ';'] do
      SetLength(S2, Length(S2)-1);
    CustomMask2 := S2;
    while S3[Length(S3)] in [' ', ';'] do
      SetLength(S3, Length(S3)-1);
    CustomMask3 := S3;
    while S4[Length(S4)] in [' ', ';'] do
      SetLength(S4, Length(S4)-1);
    CustomMask4 := S4;
    while S5[Length(S5)] in [' ', ';'] do
      SetLength(S5, Length(S5)-1);
    CustomMask5 := S5;
    {JO} while S6[Length(S6)] in [' ', ';'] do
      SetLength(S6, Length(S6)-1);
    CustomMask6 := S6;
    while S7[Length(S7)] in [' ', ';'] do
      SetLength(S7, Length(S7)-1);
    CustomMask7 := S7;
    while S8[Length(S8)] in [' ', ';'] do
      SetLength(S8, Length(S8)-1);
    CustomMask8 := S8;
    while S9[Length(S9)] in [' ', ';'] do
      SetLength(S9, Length(S9)-1);
    CustomMask9 := S9;
    {JO} while S10[Length(S10)] in [' ', ';'] do
      SetLength(S10, Length(S10)-1);
    CustomMask10 := S10;
    while S11[Length(S11)] in [' ', ';'] do
      SetLength(S11, Length(S11)-1);
    Archives := S11;
    end;
  PrepareExtCollection;
  Message(Application, evCommand, cmUpdateConfig, nil);
 {JO: перечитываем группы для всех панелей}
  GlobalMessage(evCommand, cmUpdateHighlight, nil);
  end { SetHighlightGroups };

procedure ChangeColors;
  var
    CurPal: String;
  begin
  CurPal := Application^.GetPalette^;
  if ExecResource(dlgColors, CurPal) <> cmCancel then
    begin
    SystemColors[appPalette] := CurPal;
    DoneMemory; { Dispose all group buffers }
    Application^.Redraw; { Redraw application with new palette }
    end;
  if VGASystem then
    GetPalette(VGA_palette);
  end;

procedure TWindowCol.FreeItem;
  begin
  end;

function TWindowList.GetText;
  var
    S: String;
    P: PView;
  begin
  P := List^.At(Item);
  S := GetString(dlUnknownWindowType);
  Message(P, evCommand, cmGetName, @S);
  if PWindow(P)^.Number in [1..9]
  then
    GetText := Char($30+PWindow(P)^.Number)+' '+S
  else
    GetText := '  '+S;
  end;

procedure WindowManager;
  label 1;
  var
    D: PDialog;
    R: TRect;
    PC: PWindowCol;
    PV: PView;
    S: String;
    DT: record
      P: PCollection;
      n: Word;
      end;
    I, Num: Integer;
    Cmd: Word;

  procedure InsView(P: PView);
    begin
    if P = nil then
      Exit;
    S := '';
    if  (P^.GetState(sfVisible)) then
      begin
      Message(P, evCommand, cmGetName, @S);
      if  (S <> '') then
        PC^.Insert(P);
      end;
    end;

  begin { WindowManager }
  New(PC, Init(10, 10));
  Desktop^.ForEach(@InsView);
(* AK155 Контроль на Count = 0 не нужен, так как если окон нет, то
cmWindowManager задизейблена.
  if PC^.Count = 0 then
    begin
    Dispose(PC, Done);
    Exit
    end;
*)
  D := PDialog(LoadResource(dlgWindowManager));

  R.Assign(D^.Size.X-13, 3, D^.Size.X-12, D^.Size.Y-2);
  PV := New(PScrollBar, Init(R));
  PV^.Options := PV^.Options or ofPostProcess or ofSecurity;
  D^.Insert(PV);

  R.Assign(2, 3, D^.Size.X-13, D^.Size.Y-2);
  PV := New(PWindowList, Init(R, 1, PScrollBar(PV)));
  PV^.Options := PV^.Options or ofPostProcess or ofSecurity;
  PListBox(PV)^.NewLisT(PC);
  if PC^.Count > 1 then
    PListBox(PV)^.Focused := 1;
  Num := 0; {-$VIV 28.05.99--}
  if  (WinManagerPosToEdit) and (PC^.Count > 0) and
      (Pos(GetString(dlEditTitle)+' -', PWindowList(PV)^.GetText(0, 255))
       > 0)
  then
    for I := 1 to PC^.Count-1 do
      if  (Pos(GetString(dlEditTitle)+' -', PWindowList(PV)^.GetText(I,
               255)) > 0)
      then
        begin
        Num := I;
        Break;
        end;
  if  (WinManagerSelectNext) and (Num = 0) and (PC^.Count > 1) then
    Num := 1;
  PListBox(PV)^.Focused := Num; {-$VIV--}
  D^.Insert(PV);

  R.Assign(2, 2, 45, 3);
  PV := New(PLabel, Init(R, GetString(dlWindowsLabel), PV));
  D^.Insert(PV);

while true do
  begin
  Cmd := Desktop^.ExecView(D);
  D^.GetData(DT); { Теперь DT.P = PC }
  PV := PView(DT.P^.At(DT.n));
  if Cmd = cmCancel then
    Break;
  if Cmd = cmOK then { "Select" }
    begin
    PV^.Select;
    Break;
    end;
  if Cmd = cmNo then { "Close" }
    begin
    if PV^.Valid(cmClose) then
      begin
      PV^.Free;
      DT.P := nil;
      D^.SetData(DT); {при этом NewList освободит PC }
      New(PC, Init(10, 10));
      Desktop^.ForEach(@InsView);
      if PC^.Count = 0 then
        Break;
      if DT.n >= PC^.Count then
        Dec(DT.n);
      DT.P := PC;
      D^.SetData(DT);
      end;
    end
  end;
  Dispose(D, Done);
  Dispose(PC, Done);
  end { WindowManager };

end.
