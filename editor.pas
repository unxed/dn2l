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

unit Editor;

interface

uses
  Drivers, Microed
  ;

type
  PXFileEditor = ^TXFileEditor;
  TXFileEditor = object(TFileEditor)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    function HandleCommand(var Event: TEvent): Boolean; virtual;
    end;

implementation

uses
  Lfnvp, Views, Defines, Streams, UKeyMap, Objects, ed2,
  Advance, Advance1, Advance2, Dos, Dialogs, DNApp,
  {SBlocks,}Memory, Gauge, Startup, {WinClp, // commented by unxed} Messages, Commands, Macro,
  EdWin, xTime, DnIni, DNUtil, Advance6, Calculat, FViewer {AK155}
  {$IFDEF REGEXP}, RegExp {$ENDIF}
  ;

type
  PSortCollection = ^TSortCollection;
  TSortCollection = object(TSortedCollection)
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    end;

var
  BoundStart, BoundEnd: LongInt;

function TSortCollection.Compare;
  var
    P1: PLongString absolute Key1;
    P2: PLongString absolute Key2;
  begin
  if P1 = nil then
    Compare := -1
  else if P2 = nil then
    Compare := 1
  else if Copy(P1^, 3, MaxLongStringLength)
       > Copy(P2^, 3, MaxLongStringLength)
  then
    Compare := 1
  else
    Compare := -1
  end;

{Added by PaSkol -> }
{$F+}
function Less(const L, R: LongString): Boolean;
  begin
  Less := (L < R);
  end;

function Bigger(const L, R: LongString): Boolean;
  begin
  Bigger := (L > R);
  end;
{$F-}
{ <- Added by PaSkol}

function TXFileEditor.HandleCommand;

  var
    LastX, LastY: LongInt;
    T: TPoint;

  procedure StartSearch(Replace: Boolean);
    var
      Y: LongInt;
      S: LongString;

    procedure Mix2And3(var M: Word);
      const
        SwapBits = $14; { swap bits: (...X.Y..) -> (...Y.X..) }
        {Cat:  efoCaseSens       =  1;}
        {      efoWholeWords     =  2;}
        { ┌─>  efoReplacePrompt  =  4;}
        { │    efoRegExp         =  8;}
        { └─>  efoAllCP          = 16;}
      begin
      case (M and SwapBits) of
        0, SwapBits:
        else {case}
          M := M xor SwapBits;
      end {case};
      end;

    begin { StartSearch }
    S := '';
    if  (LastX < Length(WorkString))
         and not (WorkString[LastX+1] in BreakChars)
    then
      begin
      while (LastX > 0) and
        not (WorkString[LastX] in BreakChars)
      do
        Dec(LastX);
      while not (WorkString[LastX+1] in BreakChars)
        and (LastX < Length(WorkString))
      do
        begin
        S := S+WorkString[LastX+1];
        Inc(LastX);
        end;
      end;
    SearchData.Line := S;
    if AutoScopeDetect then
      {-$VIV 19.05.99--}
      SearchData.Scope := Word(Marking and ((Mark.A.X <> Mark.B.X) or
               (Mark.A.Y <> Mark.B.Y)));
    if not Replace then
      SearchData.What := #0
    else
      SetLength(SearchData.What, 0);
    if Replace then
      Y := ExecResource(dlgEditorReplace, SearchData)
    else
      begin
      Mix2And3(SearchData.Options);
      Y := ExecResource(dlgEditorFind, SearchData);
      Mix2And3(SearchData.Options);
      end;

    if Y = cmCancel then
      Exit;
    ReplaceAll := Replace and (Y = cmYes);
    {$IFDEF REGEXP}
    if SearchData.Options and {efoRegExp}16 = 0 then
      {обычный поиск}
      {$ENDIF}
      if SearchData.Origin = 0 then
        if SearchData.Dir = 0 then
          Search(0, 0)
        else
          Search(MaxLongStringLength, FileLines^.Count-1)
      else
        Search(Delta.X, Delta.Y)
        {$IFDEF REGEXP}
    else {поиск регэкспа}
      begin
      if not RegExp^.Compile(@SearchData.Line[1],
           Length(SearchData.Line))
      then
        Exit;
      Search(0, 0);
      end;
    {$ENDIF}
    end { StartSearch };

  procedure MakeUndo;
    var
      P: PUndoRec;
      S, S1: LongString;
      V, M: Boolean;
      I: LongInt;
      PS: PLongString;
      RPT: Boolean;
      L, J: LongInt;
    begin
    repeat
      RPT := True;
      ChangeLine;
      Mark.B := Mark.A;
      Marking := False;
      BlockVisible := False;
      UnMark := True;
      if  (UndoInfo = nil) or (UndoInfo^.Count = 0) then
        Exit;
      P := UndoInfo^.At(UndoInfo^.Count-1);
      if  (P^.Where.X <> Delta.X) or (P^.Where.Y <> Delta.Y) then
        begin
        ScrollTo(P^.Where.X, P^.Where.Y);
        if P^.What <> udReplaceAll then
          Exit;
        end;
      KeyMap := P^.KeyMap;
      Convert4Do(P, dkRedo); {-$VOL}
      case P^.What of
        udDelBlock:
          begin
          V := VertBlock;
          VertBlock := P^.Vertical;
          M := InsertMode;
          InsertMode := P^.InsM; {-$VOL}
          InsertBlock(P^.Lines, False);
          VertBlock := V;
          InsertMode := M; {-$VOL}
          SetLimits;
          end;
        udDelChar:
          begin
          S := GetLine(P^.Where.Y);
          if Length(S) < P^.Where.X+1 then
            S := S+LongStrg(' ', P^.Where.X-Length(S)+1);
          Insert(P^.Str^, S, P^.Where.X+1);
          ModifyLine(P^.Where.Y, S, False);
          end;
        udInsChar:
          begin
          S := GetLine(P^.Where.Y);
          Delete(S, P^.Where.X-P^.Count+1, P^.Count);
          ModifyLine(P^.Where.Y, S, False);
          ScrollTo(P^.Where.X-P^.Count, P^.Where.Y);
          end;
        udInsLine:
          begin
          ModifyLine(P^.Where.Y, CnvLongString(P^.Str), False);
          FileLines^.AtFree(P^.Where.Y+1);
          for L := 1 to 9 do
            if MarkPos[L].Y >= P^.Where.Y then
              Dec(MarkPos[L].Y);
          SetLimits;
          end;
        udSubDel:
          ModifyLine(P^.Where.Y, CnvLongString(P^.Str), False);
        udSubDelLine:
          begin
          S := GetLine(P^.Where.Y);
          if P^.Str <> nil then
            SetLength(S, Length(S)-Length(P^.Str^));
          ModifyLine(P^.Where.Y, S, False);
          if P^.Str = nil then
            S := ''
          else
            S := P^.Str^;
          KeyMapAtInsert(P^.Where.Y+1, NewLongStr(S)); {-$VIV}
          for L := 1 to 9 do
            if MarkPos[L].Y > P^.Where.Y then
              Inc(MarkPos[L].Y);
          SetLimits;
          end;
        udInsVertBlock:
          begin
          for I := 1 to P^.Count do
            begin
            S := GetLine(I+Delta.Y-1);
            Delete(S, Delta.X+1, P^.Width);
            ModifyLine(I+Delta.Y-1, S, False);
            end;
          end;
        udInsBlock, udFormatBlock:
          begin
          S := P^.Str^;
          J := Byte(S[2])*256+Byte(S[1]);
          for I := 1 to J do
            FileLines^.AtFree(Delta.Y);
          for L := 1 to 9 do
            if MarkPos[L].Y >= Delta.Y then
              Dec(MarkPos[L].Y, J);
          Delete(S, 1, 2);
          KeyMapAtInsert(Delta.Y, NewLongStr(S)); {-$VIV}
          for L := 1 to 9 do
            if MarkPos[L].Y >= Delta.Y then
              Inc(MarkPos[L].Y);
          SetLimits;
          RPT := P^.What <> udFormatBlock;
          end;
        udReplaceChar:
          begin
          S := GetLine(P^.Where.Y);
          Delete(S, P^.Where.X+1-Length(P^.Str^), Length(P^.Str^));
          Insert(P^.Str^, S, P^.Where.X+1-Length(P^.Str^));
          ModifyLine(P^.Where.Y, S, False);
          end;
        udBackDel:
          begin
          S := LongAddSpace(GetLine(P^.Where.Y), LastX+1);
          Insert(P^.Str^, S, P^.Where.X+1);
          ModifyLine(P^.Where.Y, S, False);
          ScrollTo(P^.Where.X+Length(P^.Str^), P^.Where.Y);
          end;
        udIndentBlock:
          begin
          for I := P^.Block.A.Y to P^.Block.B.Y-Byte(P^.Block.B.X = 0)
          do
            begin
            S := GetLine(I);
            Delete(S, 1, P^.Count);
            ModifyLine(I, S, False);
            end;
          end;
        udUnindentBlock:
          begin
          for I := P^.Block.A.Y to P^.Block.B.Y-Byte(P^.Block.B.X = 0)
          do
            begin
            S := LongStrg(' ', P^.Count)+GetLine(I);
            ModifyLine(I, S, False);
            end;
          end;
        udReplace, udReplaceAll:
          begin
          S := GetLine(Delta.Y);
          Delete(S, Delta.X+1, Byte(P^.Str^[1]));
          Insert(Copy(P^.Str^, 2, MaxLongStringLength), S, Delta.X+1);
          ModifyLine(Delta.Y, S, False);
          with UndoInfo^ do
            RPT := (P^.What <> udReplaceAll) or (Count <= 1)
                   or (PUndoRec(At(Count-2))^.What <> udReplaceAll);
          end;
        udDupeLine:
          begin
          FileLines^.AtFree(P^.Where.Y+1);
          for L := 1 to 9 do
            if MarkPos[L].Y >= P^.Where.Y then
              Dec(MarkPos[L].Y);
          SetLimits;
          end;
        udStrModified:
          begin
          if P^.Str = nil then
            S := ''
          else
            S := P^.Str^;
          ModifyLine(Delta.Y, S, False);
          end;
        udReplaceBlock, udClearBlock:
          begin
          for I := Delta.Y to P^.Lines^.Count+Delta.Y-1 do
            begin
            S := GetLine(I);
            PS := P^.Lines^.At(I-Delta.Y);
            if PS <> nil then
              S1 := PS^
            else
              S1 := '';
            Delete(S, Delta.X+1, Length(S1));
            if Length(S) < Delta.X+1 then
              S := S+LongStrg(' ', Delta.X+1-Length(S));
            Insert(S1, S, Delta.X+1);
            ModifyLine(I, S, False);
            end;
          end;
        udDelLine:
          begin
          J := 0;
          while P^.Lines^.Count > 0 do
            begin
            KeyMapAtInsert(P^.Where.Y, P^.Lines^.At(P^.Lines^.Count-1));
            {-$VIV}
            P^.Lines^.AtDelete(P^.Lines^.Count-1);
            Inc(J);
            end;
          for L := 1 to 9 do
            if MarkPos[L].Y >= P^.Where.Y then
              Dec(MarkPos[L].Y, J);
          SetLimits;
          end;
      end {case};
      WorkModified := False;
      ChangeLine;
      UndoInfo^.AtFree(UndoInfo^.Count-1);
      Dec(UndoTimes);
      Modified := True;
      if not JustSaved and (UndoTimes = LastSaveUndoTimes) then
        begin
        Modified := False;
        if PEditWindow(Owner)^.AInfo <> nil then
          PEditWindow(Owner)^.AInfo^.DrawView;
        if PEditWindow(Owner)^.ABookLine <> nil then
          PEditWindow(Owner)^.ABookLine^.DrawView; {-$VIV}
        end;
      JustSaved := False;
    until RPT;
    end { MakeUndo };

  procedure MakeRedo; {-$VOL}
    var
      P: PUndoRec;
      S, S1: LongString;
      I: LongInt;
      V, M: Boolean;
      RPT: Boolean;
      L, J: LongInt;
    begin
    repeat
      RPT := True;
      ChangeLine;
      Mark.B := Mark.A;
      Marking := False;
      BlockVisible := False;
      UnMark := True;
      if  (RedoInfo = nil) or (RedoInfo^.Count = 0) then
        Exit;
      P := RedoInfo^.At(RedoInfo^.Count-1);
      if  (P^.Where.X <> Delta.X) or (P^.Where.Y <> Delta.Y) then
        begin
        ScrollTo(P^.Where.X, P^.Where.Y); {Exit;}
        end;
      KeyMap := P^.KeyMap;

      Convert4Do(P, dkUndo);
      case P^.What of
        udDelChar:
          begin
          S := GetLine(P^.Where.Y);
          Delete(S, P^.Where.X-P^.Count+1, P^.Count);
          ModifyLine(P^.Where.Y, S, False);
          ScrollTo(P^.Where.X-P^.Count, P^.Where.Y);
          end;
        udInsChar:
          begin
          S := GetLine(P^.Where.Y);
          if Length(S) < P^.Where.X+1 then
            S := S+LongStrg(' ', P^.Where.X-Length(S)+1);
          if P^.Str <> nil then
            Insert(P^.Str^, S, P^.Where.X+1);
          ModifyLine(P^.Where.Y, S, False);
          end;
        udDelLine:
          begin
          for I := 1 to P^.Count do
            FileLines^.AtFree(P^.Where.Y);
          J := P^.Count;
          for L := 1 to 9 do
            if MarkPos[L].Y >= P^.Where.Y then
              Dec(MarkPos[L].Y, J);
          SetLimits;
          end;
        udInsLine:
          begin
          if P^.Lines^.Count > 0 then
            begin
            ModifyLine(P^.Where.Y, CnvLongString(P^.Lines^.At(0)), False);
            P^.Lines^.AtDelete(0);
            end;
          J := 0;
          while P^.Lines^.Count > 0 do
            begin
            KeyMapAtInsert(P^.Where.Y+P^.Lines^.Count,
                 P^.Lines^.At(P^.Lines^.Count-1));
            P^.Lines^.AtDelete(P^.Lines^.Count-1);
            Inc(J);
            end;
          for L := 1 to 9 do
            if MarkPos[L].Y >= P^.Where.Y then
              Dec(MarkPos[L].Y, J);
          SetLimits;
          end;
        udDelBlock:
          begin
          V := VertBlock;
          VertBlock := P^.Vertical;
          M := InsertMode;
          InsertMode := P^.InsM;
          if VertBlock then
            if InsertMode then
              begin
              for I := 0 to P^.Lines^.Count-1 do
                begin
                S := CnvLongString(P^.Lines^.At(I));
                S1 := GetLine(P^.Where.Y+I);
                Delete(S1, P^.Where.X+1, Length(S));
                ModifyLine(P^.Where.Y+I, S1, False);
                end;
              end
            else
              InsertBlock(P^.Lines, False)
          else
            begin
            J := P^.Lines^.Count-1;
            for I := 0 to J do
              FileLines^.AtFree(P^.Where.Y);
            Inc(J);
            for L := 1 to 9 do
              if MarkPos[L].Y >= P^.Where.Y then
                Dec(MarkPos[L].Y, J);
            if J > 0 then
              S := CnvLongString(P^.Lines^.At(0))
            else
              S := '';
            KeyMapAtInsert(P^.Where.Y, NewLongStr(S));
            for L := 1 to 9 do
              if MarkPos[L].Y >= P^.Where.Y then
                Inc(MarkPos[L].Y);
            end;
          VertBlock := V;
          InsertMode := M;
          SetLimits;
          end;
        udInsBlock, udFormatBlock:
          begin
          FileLines^.AtFree(Delta.Y);
          J := -1;
          while P^.Lines^.Count > 0 do
            begin
            KeyMapAtInsert(Delta.Y, P^.Lines^.At(P^.Lines^.Count-1));
            P^.Lines^.AtDelete(P^.Lines^.Count-1);
            Inc(J);
            end;
          for L := 1 to 9 do
            if MarkPos[L].Y >= P^.Where.Y then
              Inc(MarkPos[L].Y, J);
          SetLimits;
          RPT := P^.What <> udFormatBlock;
          end;
        udBackDel:
          begin
          S := GetLine(P^.Where.Y);
          S := Copy(S, 1, P^.Where.X)
              +Copy(S, P^.Where.X+P^.Count+1, MaxLongStringLength);
          ModifyLine(P^.Where.Y, S, False);
          end;
        udSubDel:
          begin
          ModifyLine(P^.Where.Y, CnvLongString(P^.Str), False);
          end;
        udSubDelLine:
          begin
          ModifyLine(P^.Where.Y, CnvLongString(P^.Str), False);
          FileLines^.AtFree(P^.Where.Y+1);
          SetLimits;
          end;
        udIndentBlock:
          begin
          for I := P^.Block.A.Y to P^.Block.B.Y-Byte(P^.Block.B.X = 0)
          do
            begin
            S := LongStrg(' ', P^.Count)+GetLine(I);
            ModifyLine(I, S, False);
            end;
          end;
        udUnindentBlock:
          begin
          for I := P^.Block.A.Y to P^.Block.B.Y-Byte(P^.Block.B.X = 0)
          do
            begin
            S := GetLine(I);
            Delete(S, 1, P^.Count);
            ModifyLine(I, S, False);
            end;
          end;
        udInsVertBlock:
          begin
          V := VertBlock;
          VertBlock := P^.Vertical;
          M := InsertMode;
          InsertMode := P^.InsM;
          InsertBlock(P^.Lines, False);
          VertBlock := V;
          InsertMode := M;
          SetLimits;
          end;
        udReplace, udReplaceAll:
          begin
          S := GetLine(Delta.Y);
          Delete(S, Delta.X+1, Byte(P^.Str^[1]));
          Insert(Copy(P^.Str^, 2, MaxLongStringLength), S, Delta.X+1);
          ModifyLine(Delta.Y, S, False);
          with RedoInfo^ do
            RPT := (P^.What <> udReplaceAll) or (Count <= 1)
                   or (PUndoRec(At(Count-2))^.What <> udReplaceAll);
          end;
        udReplaceChar:
          begin
          S := GetLine(P^.Where.Y);
          Delete(S, P^.Where.X+1, Length(P^.Str^));
          Insert(P^.Str^, S, P^.Where.X+1);
          ModifyLine(P^.Where.Y, S, False);
          end;
        udReplaceBlock, udClearBlock:
          begin
          for I := Delta.Y to P^.Lines^.Count+Delta.Y-1 do
            begin
            S := GetLine(I);
            S1 := CnvLongString(P^.Lines^.At(I-Delta.Y));
            Delete(S, Delta.X+1, Length(S1));
            if Length(S) < Delta.X+1 then
              S := S+LongStrg(' ', Delta.X+1-Length(S));
            Insert(S1, S, Delta.X+1);
            ModifyLine(I, S, False);
            end;
          end;
        udStrModified:
          begin
          ModifyLine(P^.Where.Y, CnvLongString(P^.Str), False);
          end;
        udDupeLine:
          begin
          S := GetLine(P^.Where.Y);
          KeyMapAtInsert(P^.Where.Y+1, NewLongStr(S));
          for L := 1 to 9 do
            if MarkPos[L].Y >= P^.Where.Y then
              Inc(MarkPos[L].Y);
          SetLimits;
          end;
      end {case};

      WorkModified := False;
      ChangeLine;
      RedoInfo^.AtFree(RedoInfo^.Count-1);
      Inc(UndoTimes);
      Modified := True;
      if not JustSaved and (UndoTimes = LastSaveUndoTimes) then
        begin
        Modified := False;
        if PEditWindow(Owner)^.AInfo <> nil then
          PEditWindow(Owner)^.AInfo^.DrawView;
        if PEditWindow(Owner)^.ABookLine <> nil then
          PEditWindow(Owner)^.ABookLine^.DrawView; {-$VIV}
        end;
      JustSaved := False;
    until RPT;
    end { MakeRedo }; {-$VOL}

  procedure UnIndentBlock(Undo: Boolean);
    var
      I: LongInt;
      S: LongString;
    begin
    Modified := True;
    if not (BlockVisible and ValidBlock) then
      Exit;
    ChangeLine;
    if not UnlimitUnindent then
      for I := Mark.A.Y to Mark.B.Y-Integer(Mark.B.X = 0) do
        {-$VIV 13.05.99}
        begin
        S := GetLine(I);
        if  (S <> '') and (S[1] <> ' ') then
          Exit;
        end;
    T.X := 0;
    T.Y := Mark.A.Y;
    if Undo then
      StoreUndoInfo(udUnindentBlock, T, Mark);
    for I := Mark.A.Y to Mark.B.Y-Integer(Mark.B.X = 0) do
      {-$VIV 13.05.99}
      begin
      S := GetLine(I);
      if  (S = '') or (S[1] <> ' ') then
        Continue;
      Delete(S, 1, 1); {DelFC(S);}
      ModifyLine(I, S, True);
      end;
    EnableMarking := False;
    Marking := False;
    Sel := Mark;
    ScrollTo(0, Mark.A.Y);
    Mark := Sel;
    DrawView;
    ChangeLine;
    EnableMarking := True;
    end { UnIndentBlock };

  procedure IndentBlock(Undo: Boolean);
    var
      I: LongInt;
      S: LongString;
      M: TRect;
    begin
    Modified := True;
    if not (BlockVisible and ValidBlock) then
      Exit;
    ChangeLine;
    T.X := 0;
    T.Y := Mark.A.Y;
    if Undo then
      StoreUndoInfo(udIndentBlock, T, Mark);
    for I := Mark.A.Y to Mark.B.Y-Integer(Mark.B.X = 0) do
      begin
      S := ' '+GetLine(I);
      ModifyLine(I, S, True)
      end;
    EnableMarking := False;
    Marking := False;
    M := Mark;
    ScrollTo(0, Mark.A.Y);
    Mark := M;
    DrawView;
    ChangeLine;
    EnableMarking := True;
    end { IndentBlock };

  procedure NextBreak(S: LongString; var i: LongInt);
    begin
    while (i <= Length(S)) and not (S[i] = ' ') do
      Inc(i);
    while (i <= Length(S)) and (S[i] = ' ') do
      Inc(i);
    end;

  function FirstToken(var S: LongString): LongString;
    var
      SS: LongString;
      I: LongInt;
    begin
    FirstToken := '';
    I := 1;
    while (S <> '') and (S[Length(S)] = ' ') do
      SetLength(S, Length(S)-1);
    if S = '' then
      Exit;
    while (S[1] = ' ') do
      Delete(S, 1, 1); {DelFC(S);}
    NextBreak(S, I);
    SS := Copy(S, 1, I-1);
    Delete(S, 1, I-1);
    FirstToken := SS;
    end;

  procedure FormatBlock(Command: Word);
    var
      S, Left, W: LongString;
      P: PLineCollection;
      PS: PLongString;
      LS, I, J, LG: LongInt;
      Abort, LastLine: Boolean;
      TP: TPoint;
      Q, L: LongInt;

    label 1;

    procedure WriteLeft;
      var
        I: LongInt;
      begin
      while (Left <> '') and (Left[Length(Left)] = ' ') do
        SetLength(Left, Length(Left)-1);
      LongDelLeft(Left);
      case Command of
        cmFRight, cmLRight:
          begin
          j := EdOpt.RightSide-Length(Left);
          if j > 0 then
            Left := LongStrg(' ', j)+Left
          else
            Left := LongStrg(' ', EdOpt.LeftSide)+Left
          end;
        cmFLeft, cmLLeft:
          Left := LongStrg(' ', EdOpt.LeftSide)+Left;
        cmFCenter, cmLCenter:
          begin
          j := (LS-Length(Left)) div 2;
          if j < 0 then
            j := 0;
          Left := LongStrg(' ', EdOpt.LeftSide+j)+Left
          end;
        cmFJustify, cmLJustify:
          begin
          I := 1;
          j := 0;
          while (Length(Left) < LS) and (j <> I) and not LastLine do
            begin
            Inc(I);
            j := I;
            NextBreak(Left, I);
            if I < Length(Left) then
              Insert(' ', Left, I)
            else if j = 2 then
              Break
            else
              I := 1;
            end;
          j := EdOpt.LeftSide;
          if P^.Count = 0 then
            j := EdOpt.InSide;
          Left := LongStrg(' ', j)+Left;
          end;
      end {case};
      PS := NewLongStr(Left);
      if PS = nil then
        begin
        Abort := True;
        Exit
        end;
      P^.Insert(PS);
      if LowMemory then
        begin
        Abort := True;
        Exit
        end;
      Left := W;
      LS := EdOpt.RightSide-EdOpt.LeftSide;
      end { WriteLeft };

    procedure CompressLine(N: Integer);
      var
        MS: LongString;
      begin
      S := GetLine(N);
      LongDelDoubles('  ', S);
      if  (S <> '') and (S[1] = ' ') then
        Delete(S, 1, 1); {DelFC(S);}
      if N-1 < FileLines^.Count then
        MS := GetLine(N+1)
      else
        MS := '';
      LongDelDoubles('  ', MS);
      if  (MS <> '') and (MS[1] = ' ') then
        Delete(MS, 1, 1); {DelFC(MS);}
      if  (N < LG) and (Length(S) < EdOpt.RightSide)
           and (Length(MS)+Length(S) < MaxLongStringLength)
      then
        begin
        S := S+' '+MS;
        Inc(i);
        end;
      end { CompressLine };

    begin { FormatBlock }
    Abort := False;
    LastLine := False;
    Modified := True;
    Q := 0;
    if not (ValidBlock and BlockVisible) or VertBlock then
      Exit;
    if LowMemory then
      Exit;
    ChangeLine;
    {if UndoInfo <> nil then UndoInfo^.FreeAll; Inc(UndoTimes);}
    New(P, Init((Mark.B.Y-Mark.A.Y+1)*2, 10, True));
    for I := Mark.A.Y to Mark.B.Y-Integer(Mark.B.X = 0) do
      P^.Insert(NewLongStr(GetLine(I)));
    TP.Y := Mark.A.Y;
    TP.X := 0;
    StoreUndoInfo(udDelBlock, TP, P);
    New(P, Init((Mark.B.Y-Mark.A.Y+1)*2, 10, True));
    if P = nil then
      Exit;
    Left := '';
    LS := EdOpt.RightSide-EdOpt.LeftSide;
    W := '';
    if  (Command = cmFJustify) or (Command = cmLJustify) then
      LS := EdOpt.RightSide-EdOpt.InSide;
    LG := Mark.B.Y-Integer(Mark.B.X = 0);
    I := Mark.A.Y;
    CompressLine(I);
    repeat
      W := FirstToken(S);
      if W = '' then
        begin
        Left := Left+' ';
        if I < LG then
          CompressLine(I+1);
        Inc(I);
        goto 1
        end;
      J := Integer((Left <> '') and (Left[Length(Left)] = ' '));
      LongDelRight(W);
      LongDelDoubles('  ', W);
      if W[1] = ' ' then
        Delete(W, 1, 1); {DelFC(W);}
      if Length(Left)+Length(W)+J < LS then
        if J = 0 then
          Left := Left+' '+W
        else
          Left := Left+W
      else
        WriteLeft;
      if  (Left <> '') and (Left[1] = ' ') then
        Delete(Left, 1, 1); {DelFC(Left);}
1:
      if Abort then
        begin
        Dispose(P, Done);
        Exit
        end;
    until I > LG+1;
    LastLine := True;
    WriteLeft;
    if Abort then
      begin
      Dispose(P, Done);
      Exit
      end;
    Marking := False;
    EnableMarking := False;
    Delta.Y := Mark.A.Y;
    Delta.X := 0;
    for I := Mark.A.Y to LG do
      FileLines^.AtFree(Delta.Y);
    Q := LG-Mark.A.Y;
    Mark.A.X := 0;
    Mark.B.Y := Mark.A.Y+P^.Count;
    Mark.B.X := 0;
    S := Char(Lo(P^.Count))+Char(Hi(P^.Count));
    StoreUndoInfo(udFormatBlock, TP, S);
    while P^.Count > 0 do
      {-$VIV}
      begin
      Dec(Q);
      KeyMapAtInsert(Delta.Y, P^.At(P^.Count-1));
      P^.AtDelete(P^.Count-1);
      end;
    if Q > 0 then
      for L := 1 to 9 do
        if MarkPos[L].Y >= Mark.A.Y then
          Dec(MarkPos[L].Y, Q);
    SetLimits;
    DrawView;
    Dispose(P, Done);
    ChangeLine;
    EnableMarking := True;
    end { FormatBlock };

  {-DataCompBoy-}
  procedure Print(Block: Boolean);
    var
      S: TDOSStream;
      L: PLineCollection {PCollector}; {-SBlocks}
      {PC: PCollection;}
      M: String;
      P: PWhileView;
      I: LongInt;
      R: TRect;
      E: TEvent;
      PS: PLongString;
      Cancel: Boolean;
      LL: array[1..3] of LongInt;
      SR: lSearchRec;
      FName: String;
    label 1;
    begin
    ChangeLine;
    if Block then
      begin
      {PC := GetSelection;}
      {if PC = nil then Exit;}
      {L := New(PStdCollector, Init(10));}
      {Dispose(PStdCollector(L)^.Collection, Done);}
      {PStdCollector(L)^.Collection := PC;}
      {PStdCollector(L)^.Count := PC^.Count;}
      L := PLineCollection(GetSelection); {-SBlocks}
      end
    else
      L := FileLines;
    LL[1] := L^.Count;
    FormatStr(M, GetString(dlED_PrintQuery), LL);
    if MessageBox(M, nil, mfYesNoConfirm) <> cmYes
    then
      begin
      if Block then
        Dispose(L, Done);
      Exit;
      end;
    R.Assign(1, 1, 30, 10);
    New(P, Init(R));
    P^.Top := GetString(dlED_Print);
    Desktop^.Insert(P);
    ClrIO;
    Cancel := False;
    for I := 1 to 1000 do
      begin
      FName := SwpDir+'$DN'+SStr(I, 4, '0')+'$.PRN';
      ClrIO;
      lFindFirst(FName, Archive, SR);
      if DOSError <> 0 then Break;
      lFindClose(SR);
      end;
    lFindClose(SR);
    S.Init(FName, stCreate);
    if S.Status <> stOK then
      begin
      S.Done;
      {Cat:warn}
      PS := @FName;
      Msg(erCantCreateFile, @PS, mfError+mfOKButton);
      goto 1;
      end;
    P^.Write(1, GetString(dlED_Printed));
    for I := 0 to L^.Count-1 do
      begin
      PS := L^.At(I);
      P^.Write(2, Copy(LongStrg(#219, ((I+1)*25) div
             L^.Count)+LongStrg(#177, 25), 1, 25));
      LL[1] := I+1;
      LL[2] := L^.Count+1;
      FormatStr(M, GetString(dlED_PrintLine), LL);
      P^.Write(3, M);
      if PS = nil then
        M := ''
      else
        M := PS^;
      M := M+#13#10;
      S.Write(M[1], Length(M));
      DispatchEvents(P, Cancel);
      if Cancel or Abort then
        Goto 1;
      end;
    if not Block then
      begin
      M := #12;
      S.Write(M[1], 1);
      end;
    S.Done;
    Message(Application, evCommand, cmFilePrint, @FName);
1:
    P^.Free;
    if Block then
      Dispose(L, Done);
    end { Print };
  {-DataCompBoy-}

  procedure SetFormat;
    var
      Data: record
        S1, S2, S3: String[6];
        end;
      I: LongInt;
      J: Integer;
    begin
    Data.S1 := ItoS(EdOpt.LeftSide);
    Data.S2 := ItoS(EdOpt.RightSide);
    Data.S3 := ItoS(EdOpt.InSide);

    if ExecResource(dlgEditorFormat, Data) <> cmCancel then
      begin
      Val(Data.S1, I, J);
      if J = 0 then
        EdOpt.LeftSide := I;
      Val(Data.S2, I, J);
      if J = 0 then
        EdOpt.RightSide := I;
      Val(Data.S3, I, J);
      if J = 0 then
        EdOpt.InSide := I;

      if  (EdOpt.LeftSide > EdOpt.RightSide) or (EdOpt.LeftSide < 0)
      then
        EdOpt.LeftSide := 0;
      if EdOpt.RightSide < 2 then
        EdOpt.RightSide := 2;
      if  (EdOpt.InSide >= EdOpt.RightSide) or (EdOpt.InSide < 0) then
        EdOpt.InSide := EdOpt.LeftSide;
      end;
    end { SetFormat };

  procedure GotoLine;
    const
      S: String = '';
    var
      I: LongInt;
      J: Integer;
    begin
    if ExecResource(dlgGotoLine, S) <> cmOK then
      Exit;
    Val(S, I, J);
    if  (J = 0) and (I > 0) then
      ScrollTo(Delta.X, I-1);
    end;

  procedure SortBlock(Reverse: Boolean);
    var
      {PC: PSortCollection;}
      {I, J, K: Integer;
        B: Boolean;}
      S: LongString;
      Info: PView;
      Tmr: TEventTimer;
      Cmpr: function (const l, R: LongString): Boolean;

      {AK155 QuickSort имеет существенный недостаток: при равных
ключах он может переставлять строки. Поэтому я заменил его на
метод Шелла, который этого недостака не имеет. По быстродействию
разницы особой не будет: при числе элементов до 20000 Шелл если
и проигрывает, то не сильно.}
      (*
   procedure QuickSort(L, R: LongInt);
   var
     I, J: LongInt;
     P1, P2: Pointer; {-SBlocks}

     function Ln(AI: LongInt): LongString;
     begin
       if TimerExpired(Tmr) then
       begin
         UpdateWriteView(Info);
         Application^.Idle;
         NewTimer(Tmr, 150);
       end;
       Ln := Copy(GetLine(AI), BoundStart, BoundEnd);
     end;

   begin
     repeat
       I := L;
       J := R;
       S := Ln((L + R) shr 1);
       repeat
         while Cmpr(Ln(I),S) do Inc(I);
         while Cmpr(S,Ln(J)) do Dec(J);
         if I <= J then
         begin
          {if I <> J then FileLines^.SwapItems(I, J);}
           if I <> J then               {-SBlocks}
             begin                      {-SBlocks}
               P1 := FileLines^.At(I);  {-SBlocks}
               P2 := FileLines^.At(J);  {-SBlocks}
               FileLines^.AtPut(I, P2); {-SBlocks}
               FileLines^.AtPut(J, P1); {-SBlocks}
             end;                       {-SBlocks}
           Inc(I);
           Dec(J);
         end;
       until I > J;
       if L < J then QuickSort(L, J);
       L := I;
     until I >= R;
   end;
*)

    procedure ShellSort(L, R: LongInt);
      var
        i, j, step, n: LongInt;
        P1, P2: PLongString;
        x: PLongString;
      label
        1;

      function ln(AI: LongInt): LongString;
        begin
        if TimerExpired(Tmr) then
          begin
          UpdateWriteView(Info);
          Application^.Idle;
          NewTimer(Tmr, 150);
          end;
        ln := Copy(GetLine(AI), BoundStart, BoundEnd);
        end;
      begin
      n := R-L+1;
      j := 13;
      while j < n do
        j := j*3+1;
      step := j div 3;
      while step <> 1 do
        begin
        step := step div 3;
        for i := L+step to R do
          begin
          S := ln(i);
          if Cmpr(S, ln(i-step)) then
            begin
            x := FileLines^.At(i);
            j := i-step;
            repeat
              P1 := FileLines^.At(j); {-SBlocks}
              FileLines^.AtPut(j+step, P1);
              if j <= L+step-1 then
                goto 1;
              j := j-step;
            until Cmpr(ln(j), S);
            j := j+step;
1:
            FileLines^.AtPut(j, x);
            end;
          end;
        end;
      end { ShellSort };
    {/AK155}

    begin { SortBlock }
    if not VertBlock then
      begin
      ErrMsg(dlED_VertNeed);
      Exit;
      end;
    if Reverse then
      Cmpr := Bigger
    else
      Cmpr := Less;
    Info := WriteMsg(GetString(dlPleaseStandBy));
    NewTimer(Tmr, 100);
    ChangeLine;
    BoundStart := Mark.A.X+1;
    BoundEnd := Mark.B.X-Mark.A.X;
    {AK155}ShellSort(Mark.A.Y, Mark.B.Y);
    {    QuickSort(Mark.A.Y, Mark.B.Y);}
    {/AK155}
    if UndoInfo <> nil then
      UndoInfo^.FreeAll;
    Inc(UndoTimes);
    Modified := True;
    Dispose(Info, Done);
    end { SortBlock };

  procedure CalcBlock;
    var
      I: LongInt;
      J: Integer;
      R1, R2: Extended;
      S: LongString;
      R: Real;
    begin
    if not VertBlock then
      begin
      ChangeLine;
      S := DelSpaces(Copy(GetLine(Mark.A.Y), Mark.A.X+1,
             Mark.B.X-Mark.A.X));
      if S <> '' then
        begin
        EvalueError := False; {??? в .11 этой строки нет}
        R := Evalue(S, nil);
        if not EvalueError then
          begin
          Str(R: 0: 20, S);
          while S[Length(S)] = '0' do
            SetLength(S, Length(S)-1);
          if S[Length(S)] = '.' then
            SetLength(S, Length(S)-1);
          PutInClipLong(S);
          end;
        end;
      end
    else
      begin
      ChangeLine;
      BoundStart := Mark.A.X;
      BoundEnd := Mark.B.X-Mark.A.X;
      R1 := 0;
      for I := Mark.A.Y to Mark.B.Y do
        begin
        R2 := 0;
        Val(DelSpaces(Copy(GetLine(I), BoundStart+1, BoundEnd)), R2, J);
        R1 := R1+R2;
        end;
      Str(R1: 0: 20, S);
      while S[Length(S)] = '0' do
        SetLength(S, Length(S)-1);
      if S[Length(S)] = '.' then
        SetLength(S, Length(S)-1);
      PutInClipLong(S);
      end;
    end { CalcBlock };

  procedure InsertText(const S: String);
    var
      SS: Integer;
    begin
    ChangeLine;
    WorkModify;
    WorkString := LongAddSpace(WorkString, LastX+1);
    System.Insert(S, WorkString, LastX+1);
    for SS := 1 to Length(S) do
      begin
      StoreUndoInfo(udInsChar, Delta, WorkString);
      if Delta.X < MaxLongStringLength then
        Inc(Delta.X);
      end;
    ScrollTo(Delta.X, Delta.Y);
    ChangeLine;
    DrawView;
    end;

  procedure PlayMacro;
    var
      P: PEditMacros;

    function DoFind(P: PEditMacros): Boolean;
      begin
      if UpStrg(P^.Name^) = UpCase(Event.InfoChar) then
        begin
        P^.Play(@Self);
        DoFind := True;
        end
      else
        DoFind := False;
      end;

    begin
    if Event.InfoChar < #32 then
      Inc(Event.InfoChar, 64);
    Macros^.FirstThat(@DoFind);
    end;

  procedure SelectMacro;
    begin
    end;

  procedure SearchFwd;
    var
      I: LongInt;
      C: String[2];
    begin
    I := 1;
    Inc(LastX);
    case WorkString[LastX] of
      '[':
        C := '[]';
      '{':
        C := '{}';
      '(':
        C := '()';
      '<':
        C := '<>'; {-$VIV}
    end {case};
    FreeLongStr := WorkString;
    while (LastY < FileLines^.Count) do
      begin
      while LastX < Length(FreeLongStr) do
        begin
        if FreeLongStr[LastX+1] = C[1] then
          Inc(I)
        else if FreeLongStr[LastX+1] = C[2] then
          Dec(I);
        if I = 0 then
          begin
          ScrollTo(LastX, LastY);
          HandleCommand := True;
          Exit;
          end;
        Inc(LastX);
        end;
      Inc(LastY);
      LastX := 0;
      FreeLongStr := GetLine(LastY);
      end;
    end { SearchFwd };

  procedure SearchBwd;
    var
      I: LongInt;
      C: String[2];
    begin
    I := 1;
    case WorkString[LastX+1] of
      ']':
        C := '[]';
      '}':
        C := '{}';
      ')':
        C := '()';
      '>':
        C := '<>'; {-$VIV}
    end {case};
    Dec(LastX);
    FreeLongStr := WorkString;
    while (LastY >= 0) do
      begin
      while LastX >= 0 do
        begin
        if FreeLongStr[LastX+1] = C[2] then
          Inc(I)
        else if FreeLongStr[LastX+1] = C[1] then
          Dec(I);
        if I = 0 then
          begin
          ScrollTo(LastX, LastY);
          HandleCommand := True;
          Exit;
          end;
        Dec(LastX);
        end;
      Dec(LastY);
      FreeLongStr := GetLine(LastY);
      LastX := Length(FreeLongStr)-1;
      end;
    end { SearchBwd };

  procedure CopyWinBlock;
    var
      R: TRect;
    begin
    ChangeLine;
    if  (ClipBoard <> nil) then
      Dispose(ClipBoard, Done);
    ClipBoard := GetSelection;
    // fixme: commented by unxed
    //SetWinClip(PLineCollection(ClipBoard));
    end;

  {-$VIV 20.05.99--}

(*1  function WordsIsEqual(pWord1, pManyWords: LongString): Boolean;
    begin
    WordsIsEqual := System.Pos(';'+UpLongStrg(pWord1)+';',
        ';'+UpLongStrg(pManyWords)+';') <> 0;
    end;
  function CurrentWordIs(pWord: LongString): Boolean;
    var
      S: LongString;
      Max, I: Integer;
    begin
    CurrentWordIs := False;
    S := WorkString;
    Max := Delta.X+1;
    if Max > Length(S) then
      Max := Length(S)
    else if Max = 0 then
      Max := 1;
    if  (S[Max] in BreakChars) then
      Exit;
    for I := Max downto 1 do
      if  (S[I] in BreakChars) then
        begin
        S := Copy(S, I+1, MaxLongStringLength);
        Break;
        end;
    for I := 1 to Length(S) do
      if  (S[I] in BreakChars) then
        begin
        S := Copy(S, 1, I-1);
        Break;
        end;
    CurrentWordIs := WordsIsEqual(S, pWord);
    end { CurrentWordIs };
1*)

  { FromPos - search start position (last search stop position)  }
  { WrdPos - first character potition in th word found           }
  { pWord - the word found                                       }
  { result - found a word ?                                      }

  function GetPrevWordViv(var FromPos, WrdPos: TPoint;
       var pWord: LongString): Boolean;
    var
      S, Buf: LongString;
      WasBreak, DoStore, MakeExit: Boolean;
    begin
    GetPrevWordViv := False;
    if  (FromPos.Y > FileLines^.Count-1) or (FromPos.Y < 0) then
      Exit;
    Buf := '';
    S := GetLine(FromPos.Y);
    MakeExit := False;
    WasBreak := False;
    DoStore := (FromPos.X >= 0) and (FromPos.X < Length(S))
         and (S[FromPos.X+1] in BreakChars);
    repeat
      if  (FromPos.X >= 0) then
        begin
        if  (S[FromPos.X+1] in BreakChars) then
          {BreakChars}
          begin
          if Buf <> '' then
            begin
            pWord := Buf;
            GetPrevWordViv := True;
            MakeExit := True;
            if FromPos.X < Length(S)-1 then
              Inc(FromPos.X);
            end;
          WasBreak := True;
          end
        else
          begin {not BreakChars}
          if WasBreak then
            DoStore := True;
          if DoStore then
            begin
            Buf := S[FromPos.X+1]+Buf;
            WrdPos := FromPos;
            end;
          WasBreak := False;
          end;
        Dec(FromPos.X);
        end;
      if  (FromPos.X < 0) then
        begin
        if Buf <> '' then
          begin
          pWord := Buf;
          GetPrevWordViv := True;
          MakeExit := True;
          end
        else
          begin
          Dec(FromPos.Y);
          FromPos.X := 0;
          WasBreak := True;
          if FromPos.Y < 0 then
            MakeExit := True
          else if not MakeExit then
            begin
            S := GetLine(FromPos.Y);
            FromPos.X := Length(S)-1;
            end;
          end;
        end;
    until MakeExit;
    end { GetPrevWordViv };

  function GetNextWordViv(var FromPos, WrdPos: TPoint;
       var pWord: LongString): Boolean;
    var
      S, Buf: LongString;
      WasBreak, DoStore, MakeExit: Boolean;
    begin
    GetNextWordViv := False;
    if  (FromPos.Y > FileLines^.Count-1) or (FromPos.Y < 0) then
      Exit;
    Buf := '';
    S := GetLine(FromPos.Y);
    MakeExit := False;
    WasBreak := False;
    DoStore := (FromPos.X >= 0) and (FromPos.X < Length(S))
         and (S[FromPos.X+1] in BreakChars);
    repeat
      if  (FromPos.X < Length(S)) then
        begin
        if  (S[FromPos.X+1] in BreakChars) then
          {BreakChars}
          begin
          if Buf <> '' then
            begin
            pWord := Buf;
            GetNextWordViv := True;
            MakeExit := True;
            if FromPos.X > 0 then
              Dec(FromPos.X);
            end;
          WasBreak := True;
          end
        else
          begin {not BreakChars}
          if WasBreak then
            begin
            DoStore := True;
            WrdPos := FromPos;
            end;
          if DoStore then
            Buf := Buf+S[FromPos.X+1];
          WasBreak := False;
          end;
        Inc(FromPos.X);
        end;
      if  (FromPos.X >= Length(S)) then
        begin
        if Buf <> '' then
          begin
          pWord := Buf;
          GetNextWordViv := True;
          MakeExit := True;
          end
        else
          begin
          Inc(FromPos.Y);
          FromPos.X := 0;
          WasBreak := True;
          if FromPos.Y > FileLines^.Count-1 then
            MakeExit := True
          else if not MakeExit then
            S := GetLine(FromPos.Y);
          end;
        end;
    until MakeExit;
    end { GetNextWordViv };
(*1
  procedure SearchWordViv(Frw: Boolean; pIncWord, pWord: LongString);
    var
      Pnt, Wrd: TPoint;
      Res: Boolean;
      sWord: LongString;
      I, PrevLine: LongInt;
      DeepLevel: LongInt;
      IsDeepRange: Boolean;
    begin
    Pnt := Delta;
    I := 1;
    DeepLevel := FastSearchDeep;
    IsDeepRange := (DeepLevel <> 0);
    PrevLine := Pnt.Y;
    repeat
      if Frw then
        Res := GetNextWordViv(Pnt, Wrd, sWord)
      else
        Res := GetPrevWordViv(Pnt, Wrd, sWord);
      if IsDeepRange and (PrevLine <> Pnt.Y) then
        begin
        Dec(DeepLevel);
        PrevLine := Pnt.Y;
        end;
      if Res and (pIncWord <> '') and WordsIsEqual(sWord, pIncWord) then
        Inc(I)
      else if Res and (pIncWord <> '') and WordsIsEqual(sWord, pWord)
      then
        Dec(I)
      else if Res and (pIncWord = '') and WordsIsEqual(sWord, pWord)
      then
        I := 0;
      if I = 0 then
        begin
        ScrollTo(Wrd.X, Wrd.Y);
        HandleCommand := True;
        Break;
        end;
      if IsDeepRange and (DeepLevel <= 0) then
        begin
        if MessageBox(GetString(dlVeryDeepSearch), nil, mfYesNoConfirm)
           = cmYes
        then
          IsDeepRange := False
        else
          Break;
        end;
    until (not Res);
    end { SearchWordViv };

  function CheckPairsViv(pPair1, pPair2: LongString): Boolean;
    const
      ForwardDir = True;
      BackwardDir = False;
    begin
    CheckPairsViv := True;
    if CurrentWordIs(pPair1) then
      SearchWordViv(ForwardDir, pPair1, pPair2)
    else if CurrentWordIs(pPair2) then
      SearchWordViv(BackwardDir, pPair2, pPair1)
    else
      CheckPairsViv := False;
    end;

  {-$VIV--}
1*)
  begin { TXFileEditor.HandleCommand }
  LastX := Delta.X;
  LastY := Delta.Y;
  HandleCommand := False;
  case Event.Command of
    cmWindowsCopy:
      CopyWinBlock;
    cmSyncClipIn:
      begin
      // fixme: commented by unxed
      //SyncClipIn;
      if ClipBoardStream <> nil then
        ClipBoardStream^.Seek(Positive(ClipBoardStream^.GetPos-1));
      // fixme: commented by unxed
      //CopyLines2Stream(ClipBoard, ClipBoardStream);
      end;
    cmSyncClipOut:
      begin
          // fixme: commented by unxed
          //SyncClipOut {(true)};
      end;
    cmPlayMacro:
      PlayMacro;
    cmSelectMacro:
      SelectMacro;
    cmSetMargins:
      SetFormat;
    cmGotoLineNumber:
      GotoLine;
    cmGotoLineNumber2:
      if LongInt(Event.InfoPtr) > 0 then
        begin
        ScrollTo(Delta.X, LongInt(Event.InfoPtr)+20);
        ScrollTo(Delta.X, LongInt(Event.InfoPtr)-1);
        end;
    cmRevSortBlock:
      begin
      if ValidBlock then
        SortBlock(True);
      HandleCommand := True;
      end;
    cmSortBlock:
      begin
      if ValidBlock then
        SortBlock(False);
      HandleCommand := True;
      end;
    cmCalcBlock:
      begin
      if ValidBlock then
        CalcBlock;
      HandleCommand := True;
      end;
    cmInsertText:
      InsertText(InterfaceStr);
    cmInsertDate, cmInsertTime:
      InsertText(GetDateTime(Event.Command = cmInsertTime));
    cmStartSearch:
      begin
      ChangeLine;
      StartSearch(False);
      ChangeLine;
      end;
    cmReplace:
      begin
      ChangeLine;
      StartSearch(True);
      ChangeLine;
      end;
    cmUndo:
      begin
      MakeUndo;
      HandleCommand := True;
      end;
    cmRedo:
      begin
      MakeRedo;
      HandleCommand := True;
      end; {-$VOL}
    cmIndentBlock:
      begin
      UnMark := True;
      IndentBlock(True)
      end;
    cmUnIndentBlock:
      begin
      UnMark := True;
      UnIndentBlock(True)
      end;
    cmFRight, cmFLeft, cmFJustify, cmFCenter:
      begin
      FormatBlock(Event.Command)
      end;
    cmLRight, cmLLeft, cmLJustify, cmLCenter:
      begin
      ChangeLine;
      Mark.A := Delta;
      Mark.B := Delta;
      Mark.A.X := 0;
      Mark.B.X := Length(GetLine(Delta.Y));
      Sel := Mark;
      BlockVisible := True;
      FormatBlock(Event.Command)
      end;
    cmViewFile:
      if Valid(cmClose) and (EditName <> '') then
        begin
        if Desktop <> nil then
          Desktop^.Lock;
        if Application <> nil then
          begin
          {AK155}Application^.InsertWindow(New(PFileWindow,
              Init(EditName, EditName, False)));
          Message(Application, evCommand, cmViewText,
            @EditName);
          MessageL(Application, evCommand, cmScrollBarChanged,
            GetOffsetForLineNumber(EditName, Pos.Y+1));
          end;
        ClearEvent(Event);
        Message(Owner, evCommand, cmClose, nil);
        if Desktop <> nil then
          Desktop^.UnLock;
        Exit;
        end;
    cmPrintBlock:
      Print(True);
    cmPrintFileEd:
      Print(False);
    cmBracketPair:
      begin
      ChangeLine;
      if  (LastX < Length(WorkString)) then
        if  (WorkString[LastX+1] in ['[', '{', '(', '<']) then
          SearchFwd
        else if (WorkString[LastX+1] in [')', '}', ']', '>']) then
          SearchBwd
(*1
          {-$VIV}
        else if CheckPairsViv('begi'+'n;ca'+'se;a'+'sm', 'e'+'nd') then
          {no action}
        else if CheckPairsViv('repeat', 'until') then
          {no action}
        else if CheckPairsViv('if', 'then') then
          {no action}
1*)
          ;
      {-$VIV::}
      end;
    cmSwitchKeyMapping: {-$VIV}
      begin
      FlushWorkString;
      KeyMap := RollKeyMap[KeyMap];
      HandleCommand := True;
      WorkString := GetLine(Delta.Y); { Прочитать в новой кодировке }
      end;
  end {case};
  end { TXFileEditor.HandleCommand };

end.
