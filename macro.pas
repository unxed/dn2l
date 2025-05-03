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

unit Macro;

interface

uses
  Advance, Defines, Objects2, Views, Objects,
  Microed, highlite, Strings
  ;

type
  PEditMacros = ^TEditMacros;
  TEditMacros = object(TObject)
    Name: PString;
    Commands: PCollection;
    constructor Init(S: String; var F: PTextReader);
    procedure Play(Editor: PView);
    destructor Done; virtual;
    end;

  PMacroCommand = ^TMacroCommand;
  TMacroCommand = object(TObject)
    Command: Integer;
    Repetitions: Integer;
    constructor Init(ACommand, ARepetitions: Word);
    procedure ExecCommand(Editor: PView); virtual;
    end;

  PMacroGoto = ^TMacroGoto;
  TMacroGoto = object(TMacroCommand)
    procedure ExecCommand(Editor: PView); virtual;
    end;

  PMacroMark = ^TMacroMark;
  TMacroMark = object(TMacroCommand)
    Mark: Boolean;
    constructor Init(AN: Integer; AMark: Boolean);
    procedure ExecCommand(Editor: PView); virtual;
    end;

  PMacroString = ^TMacroString;
  TMacroString = object(TMacroCommand)
    S: PString;
    constructor Init(const AString: String; ARepetitions: Word);
    procedure ExecCommand(Editor: PView); virtual;
    destructor Done; virtual;
    end;

  PIDCollection = ^TIDCollection;
  TIDCollection = object(TSortedCollection)
    function Compare(P1, P2: Pointer): Integer; virtual;
    end;

procedure EditDOSEnvironment(Env: PByteArray);
function InitHighLight
    (const FName: String;
    var HiLitePar: THighliteParams;
    Macros: PCollection;
    EdOptions: PEditOptions): Boolean;

implementation
uses
  Messages, Drivers, Dialogs, Commands, DNHelp, DNApp,
  Startup, Advance1, Advance2, VPUtils
  , VPSysLow {JO: for SysBeepEx}
  ;

type
  PLngWord = ^TLngWord;
  TLngWord = object(TObject)
    Name: String;
    // removed [30] by unxed to fix build
    //Name: String[30];
    l: Word;
    constructor Init(AL: Word; const AName: String);
    end;

function Token(const S: String; var Pos: LongInt): String;
  var
    A: String;
    B, Cmd: Boolean;
    M: String;
    J: Integer;
  begin
  Token := '';
  A := '';
  B := False;
  Cmd := False;
  while (S[Pos] = ' ') and (Length(S) >= Pos) do
    Inc(Pos);
  while Pos <= Length(S) do
    begin
    case S[Pos] of
      '''':
        if  (S[Pos+1] = '''') and B and (Pos < Length(S)) then
          begin
          Inc(Pos);
          AddStr(A, '''')
          end
        else
          B := not B;
      ',', ' ':
        if not B or Cmd then
          Break
        else if B then
          AddStr(A, S[Pos]);
      '^':
        if B then
          AddStr(A, S[Pos])
        else
          begin
          Inc(Pos);
          AddStr(A, Char(Byte(UpCase(S[Pos]))-64));
          end;
      '#':
        if B then
          AddStr(A, S[Pos])
        else
          begin
          J := 1;
          M := '';
          repeat
            Inc(Pos);
            AddStr(M, S[Pos]);
          until not (S[Pos+1] in ['0'..'9']) or (Pos >= Length(S));
          AddStr(A, Char(StoI(M)));
          end;
      else {case}
        if B then
          AddStr(A, S[Pos])
        else
          begin
          B := True;
          Cmd := True;
          AddStr(A, S[Pos])
          end;
    end {case};
    Inc(Pos);
    end;
  Inc(Pos);
  Token := A;
  end { Token };

function TIDCollection.Compare;
  begin
  if PLngWord(P1)^.Name > PLngWord(P2)^.Name then
    Compare := 1
  else if PLngWord(P1)^.Name < PLngWord(P2)^.Name then
    Compare := -1
  else
    Compare := 0;
  end;

constructor TLngWord.Init;
  begin
  Name := AName;
  UpStr(Name);
  l := AL;
  end;

constructor TEditMacros.Init;
  var
    I, J: LongInt;
    IDs: PIDCollection;
    Error: Boolean;

  function GetID(const S: String): LongInt;
    var
      T: TLngWord;
      I: LongInt;
      L: LongInt;
      qq: Integer;
    begin
    FreeStr := S;
    DelRight(FreeStr);
    if S[Length(S)] = ',' then
      SetLength(FreeStr, Length(FreeStr)-1);
    DelRight(FreeStr);
    T.Init(0, FreeStr);
    if IDs^.Search(@T, I) then
      begin
      GetID := PLngWord(IDs^.At(I))^.L;
      end
    else
      begin
      Val(FreeStr, L, qq);
      {if I > 0 then MessageBox(^M^C'Unknown identifier ('+FreeStr+')', nil, mfError+mfOKButton);}
      GetID := L;
      end;
    end { GetID };

  function Found(const SS: String): Boolean;
    begin
    if UpStrg(Copy(S, 1, Length(SS))) = SS then
      begin
      Found := True;
      Delete(S, 1, Length(SS));
      DelLeft(S);
      end
    else
      Found := False;
    end;

  procedure MakePrint;
    begin
    Commands^.Insert(New(PMacroString, Init(Token(S, i), GetID(Token(S,
             i)))));
    end;

  procedure MakeGotoXY(X, Y: Integer);
    begin
    Commands^.Insert(New(PMacroGoto, Init(X, Y)));
    end;

  procedure MakeIDs;

    procedure Add(N: Word; const S: String);
      begin
      IDs^.Insert(New(PLngWord, Init(N, S)))
      end;

    begin
    Add(cmSaveText, 'SaveText');
    Add(cmLoadText, 'LoadText');
    Add(cmSaveTextAs, 'SaveTextAs');
    Add(cmSwitchBlock, 'SwitchBlock');
    Add(cmSwitchIndent, 'SwitchIndent');
    Add(cmSwitchFill, 'SwitchFill');
    Add(cmSwitchBack, 'SwitchBack');
    Add(cmSwitchSave, 'SwitchSave');

    Add(cmWordLeft, 'WordLeft');
    Add(cmWordRight, 'WordRight');
    Add(cmDelWordRight, 'DelWordRight');
    Add(cmDelWordLeft, 'DelWordLeft');
    Add(cmScrollUp, 'ScrollUp');
    Add(cmScrollDn, 'ScrollDn');
    Add(cmCenter, 'Center');
    Add(cmStartSearch, 'StartSearch');
    Add(cmContSearch, 'ContSearch');
    Add(cmDelBackChar, 'DelBackChar');
    Add(cmDelChar, 'DelChar');
    Add(cmSwitchIns, 'SwitchIns');
    Add(cmPgUp, 'PgUp');
    Add(cmPgDn, 'PgDn');
    Add(cmMoveUp, 'MoveUp');
    Add(cmMoveLeft, 'MoveLeft');
    Add(cmMoveRight, 'MoveRight');
    Add(cmMoveDown, 'MoveDown');
    Add(cmBlockStart, 'BlockStart');
    Add(cmBlockEnd, 'BlockEnd');
    Add(cmMarkWord, 'MarkWord');
    Add(cmMarkLine, 'MarkLine');
    Add(cmBlockRead, 'BlockRead');
    Add(cmBlockWrite, 'BlockWrite');
    {$IFDEF Printer}
    Add(cmBlockPrint, 'BlockPrint');
    {$ENDIF}
    Add(cmIndentBlock, 'IndentBlock');
    Add(cmUnIndentBlock, 'UnIndentBlock');
    Add(cmTab, 'Tab');
    Add(cmPlaceMarker1, 'PlaceMarker1');
    Add(cmPlaceMarker2, 'PlaceMarker2');
    Add(cmPlaceMarker3, 'PlaceMarker3');
    Add(cmPlaceMarker4, 'PlaceMarker4');
    Add(cmPlaceMarker5, 'PlaceMarker5');
    Add(cmPlaceMarker6, 'PlaceMarker6');
    Add(cmPlaceMarker7, 'PlaceMarker7');
    Add(cmPlaceMarker8, 'PlaceMarker8');
    Add(cmPlaceMarker9, 'PlaceMarker9');
    Add(cmGoToMarker1, 'GoToMarker1');
    Add(cmGoToMarker2, 'GoToMarker2');
    Add(cmGoToMarker3, 'GoToMarker3');
    Add(cmGoToMarker4, 'GoToMarker4');
    Add(cmGoToMarker5, 'GoToMarker5');
    Add(cmGoToMarker6, 'GoToMarker6');
    Add(cmGoToMarker7, 'GoToMarker7');
    Add(cmGoToMarker8, 'GoToMarker8');
    Add(cmGoToMarker9, 'GoToMarker9');
    Add(cmSpecChar, 'SpecChar');
    Add(cmReplace, 'Replace');
    Add(cmEnd, 'End');
    Add(cmEnter, 'Enter');
    Add(cmInsLine, 'InsLine');
    Add(cmFJustify, 'FJustify');
    Add(cmFCenter, 'FCenter');
    Add(cmFLeft, 'FLeft');
    Add(cmFRight, 'FRight');
    Add(cmLJustify, 'LJustify');
    Add(cmLCenter, 'LCenter');
    Add(cmLLeft, 'LLeft');
    Add(cmLRight, 'LRight');
    Add(cmDeltoEOLN, 'DeltoEOLN');
    Add(cmSetMargins, 'SetMargins');
    Add(cmCtrlHome, 'CtrlHome');
    Add(cmCtrlEnd, 'CtrlEnd');
    Add(cmSwitchHighLight, 'SwitchHighLight');
    Add(cmGotoLineNumber, 'GotoLineNumber');
    Add(cmGotoLineNumber2, 'GotoLineNumber2');
    Add(cmSwitchWrap, 'SwitchWrap');
    Add(cmSwitchHiLine, 'SwitchHiLine');
    Add(cmSwitchHiColumn, 'SwitchHiColumn');
    Add(cmUpString, 'UpString');
    Add(cmLowString, 'LowString');
    Add(cmCapString, 'CapString');
    Add(cmUpcaseBlock, 'UpBlock');
    Add(cmLowcaseBlock, 'LowBlock');
    Add(cmCapitalizeBlock, 'CapBlock');
    Add(cmInsertTime, 'InsertTime');
    Add(cmInsertDate, 'InsertDate');
    Add(cmSortBlock, 'SortBlock');
    Add(cmCalcBlock, 'CalcBlock');
    Add(cmCopyBlock, 'CopyBlock');
    Add(cmMoveBlock, 'MoveBlock');
    Add(cmHideBlock, 'HideBlock');
    Add(cmMoveBlockStart, 'MoveBlockStart');
    Add(cmMoveBlockEnd, 'MoveBlockEnd');
    Add(cmInsertOn, 'InsertOn');
    Add(cmInsertOff, 'InsertOff');
    Add(cmIndentOn, 'IndentOn');
    Add(cmIndentOff, 'IndentOff');
    end { MakeIDs };

  begin { TEditMacros.Init }
  inherited Init;
  New(Commands, Init(10, 10));
  New(IDs, Init(100, 10));
  MakeIDs;
  while (S <> '') and (S[1] = ' ') do
    Delete(FreeStr, 1, 1);
  if S = '' then
    S := '0';
  Name := NewStr(S);
  while not F^.Eof do
    begin
    S := F^.GetStr;
    DelRight(S);
    DelLeft(S);
    if  (S[1] <> ';') and (S <> '') then
      begin
      I := 1;
      if Found('ENDMACRO') then
        Break;
      if Found('PRINT ') then
        MakePrint
      else if Found('GOTOXY ') then
        MakeGotoXY(GetID(Token(S, I)), GetID(Token(S, I)))
      else if Found('GOTOX ') then
        MakeGotoXY(GetID(Token(S, I)), -1)
      else if Found('GOTOY ') then
        MakeGotoXY(-1, GetID(Token(S, I)))
      else if Found('GOTO ') then
        Commands^.Insert(New(PMacroMark, Init(GetID(Token(S, I)),
           False)))
      else if Found('MARK ') then
        Commands^.Insert(New(PMacroMark, Init(GetID(Token(S, I)),
           True)))
      else
        begin
        J := GetID(Token(S, I));
        if J > 0 then
          Commands^.Insert(New(PMacroCommand, Init(J, GetID(Token(S,
             I)))));
        end;
      end;
    end;
  Dispose(IDs, Done);
  IDs := nil;
  end { TEditMacros.Init };

procedure TEditMacros.Play;
  procedure DoPlay(P: PMacroCommand);
    begin
    P^.ExecCommand(Editor);
    end;
  begin
  Commands^.ForEach(@DoPlay);
  end;

destructor TEditMacros.Done;
  begin
  DisposeStr(Name);
  Dispose(Commands, Done);
  Commands := nil;
  end;

constructor TMacroCommand.Init;
  begin
  inherited Init;
  Command := ACommand;
  Repetitions := ARepetitions;
  end;

procedure TMacroCommand.ExecCommand;
  var
    I: Integer;
  begin
  for I := 1 to Max(1, Repetitions) do
    Message(Editor^.Owner, evCommand, Command, nil);
  end;

procedure TMacroGoto.ExecCommand;
  begin
  if  (Command < 0) then
    PFileEditor(Editor)^.ScrollTo(PFileEditor(Editor)^.Delta.X,
       Repetitions)
  else if (Repetitions < 0) then
    PFileEditor(Editor)^.ScrollTo(Command, PFileEditor(Editor)^.Delta.Y)
  else
    PFileEditor(Editor)^.ScrollTo(Command, Repetitions);
  end;

constructor TMacroMark.Init;
  begin
  TObject.Init;
  Command := Max(1, Min(AN, 9));
  Mark := AMark;
  end;

procedure TMacroMark.ExecCommand;
  begin
  if Mark then
    with PFileEditor(Editor)^ do
      MarkPos[Command] := Delta
  else
    with PFileEditor(Editor)^ do
      ScrollTo(MarkPos[Command].X, MarkPos[Command].Y);
  end;

constructor TMacroString.Init;
  begin
  TObject.Init;
  S := NewStr(AString);
  Repetitions := ARepetitions;
  end;

procedure TMacroString.ExecCommand;
  var
    I, J: Integer;
  begin
  if S = nil then
    Exit;
  for J := 1 to Max(1, Repetitions) do
    for I := 1 to Length(S^) do
      Message(Application, evKeyDown, Byte(S^[I]), nil);
  end;

destructor TMacroString.Done;
  begin
  DisposeStr(S);
  end;

{---------------------------- EditDOSEnvironment -----------------------------}

//JO: 29-01-2005 - поддеpжка значений пеpеменных окpужения
//                 длиной более 255 символов
//Пpим.: в настоящий момент попытка использовать стpоку ввода длиной более
//       18517 символов пpиводит к падению DN/2. Пока на всякий случай
//       огpаничил длину стpоки ввода 16383 символами (см. dn.dnr, pесуpсы
//       диалога dlgEditEnvironment)
type
  PDOSVar = ^TDOSVar;
  TDOSVar = object(TObject)
    Name: PString;
    Value: AnsiString; {JO}
    constructor Init(P: PChar; NameLen, ValueLen: Longint);
    destructor Done; virtual;
    end;

  PVarList = ^TVarList;
  TVarList = object(TListBox)
    Line: PLongInputLine;
    PrevFocused: Integer;
    SText: PView;
    procedure FocusItem(Item: LongInt); virtual;
    function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  { TDOSVar }

constructor TDOSVar.Init(P: PChar; NameLen, ValueLen: Longint);
   {P - определение одной переменной окружения вида 'LANG=ru_RU',
    #0 в конце не гарантируется.
    Hапример, для 'LANG=ru_RU' будет NameLen=4, ValueLen=5 }
  begin
  Inherited Init;
  { Тут несподручно вызывать NewStr, поэтоу дин. строку формируем сами }
  GetMem(Name, NameLen+1);
  SetLength(Name^, NameLen);
  Move((P)^, Name^[1], NameLen);

  SetLength(Value, ValueLen);
  StrLCopy(@Value[1], P+NameLen+1, ValueLen)
  end;

destructor TDOSVar.Done;
  begin
  DisposeStr(Name);
  inherited Done;
  end;

{ TVarList }

procedure TVarList.FocusItem;
  var
    P: PDOSVar;
  begin
  if  (PrevFocused >= 0) and
      (List <> nil) and (Focused < List^.Count)
  then
    P := List^.At(Focused)
  else
    P := nil;
  if P <> nil then
    Line^.GetData(P^.Value);
  PrevFocused := Focused;
  inherited FocusItem(Item);
  if  (List <> nil) and (Focused < List^.Count) then
    P := List^.At(Focused)
  else
    P := nil;
  if  P <> nil then
    begin
    Line^.SetData(P^.Value);
    Line^.DrawView;
    end;
  end { TVarList.FocusItem };

function TVarList.GetText;
  var
    P: PDOSVar;
  begin
  P := List^.At(Item);
  if P <> nil then
    GetText := CnvString(P^.Name)
  else
    GetText := '';
  end;

procedure TVarList.HandleEvent;
  var
    P: PDOSVar;

  procedure DeleteVar;
    begin
    if  (P <> nil) and (MessageBox(GetString(dlEnvDelConfirm),
          @P^.Name, mfYesNoConfirm) = cmYes)
    then
      begin
      PrevFocused := -1;
      List^.AtFree(Focused);
      SetRange(List^.Count);
      FocusItem(Focused);
      end;
    ClearEvent(Event);
    DrawView;
    end;

  procedure AppendVar;
    var
      S: String;
    begin
    ClearEvent(Event);
    S := '';
    if InputBox(GetString(dlEnvAddTitle), GetString(dlEnvVariable), S,
         255, hsNewVariable) <> cmOK
    then
      Exit;
    PrevFocused := -1;
    UpStr(S);
    List^.AtInsert(Focused,
      New(PDOSVar, Init(@s[1], Length(S), 0)));
    SetRange(List^.Count);
    FocusItem(Focused);
    DrawView;
    end;

  procedure RenameVar;
    var
      S: String;
    begin
    ClearEvent(Event);
    if  (P = nil) then
      Exit;
    S := CnvString(P^.Name);
    FormatStr(FreeStr, GetString(dlEnvRenVar), P^.Name);
    if InputBox(FreeStr, GetString(dlFCRenameNew), S, 255, hsNewVariable)
       <> cmOK
    then
      Exit;
    UpStr(S);
    DisposeStr(P^.Name);
    P^.Name := NewStr(S);
    DrawView;
    end;

  begin { TVarList.HandleEvent }
  inherited HandleEvent(Event);
  if  (List <> nil) and (Focused < List^.Count) then
    P := List^.At(Focused)
  else
    P := nil;
  case Event.What of
    evCommand:
      case Event.Command of
        cmOK:
          begin
          if P <> nil then
            Line^.GetData(P^.Value);
          end;
        cmAddVariable:
          AppendVar;
        cmRenVariable:
          RenameVar;
        cmDelVariable:
          DeleteVar;
      end {case};
    evBroadcast:
      case Event.Command of
        cmOK:
          AppendVar;
        cmNo:
          DeleteVar;
        cmYes:
          RenameVar;
      end {case};
  end {case};
  end { TVarList.HandleEvent };

procedure EditDOSEnvironment;
  var
    D: PDialog;
    P: PView;
    R: TRect;
    PC: PCollection;
    I: LongInt;

  procedure MakeDialog;
    var
      R: TRect;
      Control, Labl, Histry: PView;
      PL: PVarList;
      PI: PLongInputLine;
    function IsLongInputLine(P: PView): Boolean;
      begin
      IsLongInputLine := TypeOf(P^) = TypeOf(TLongInputLine)
      end;
    function IsButton(P: PView): Boolean;
      begin
      IsButton := TypeOf(P^) = TypeOf(TButton)
      end;
    begin
    D := PDialog(LoadResource(dlgEditEnvironment));
    Control := D^.FirstThat(@IsButton);

    R.A.X := Control^.Origin.X-1;
    R.B.X := R.A.X+1;
    R.A.Y := 3;
    R.B.Y := 14;
    Control := New(PScrollBar, Init(R));
    D^.Insert(Control);

    R.B.X := R.A.X;
    R.A.X := 3;

    PL := New(PVarList, Init(R, 1, PScrollBar(Control)));
    D^.Insert(PL);

    PI := PLongInputLine(D^.FirstThat(@IsLongInputLine));
    if PI = nil then
      begin
      D^.Free;
      D := nil;
      exit;
      end;

    R.Assign(2, 2, 43, 3);
    Labl := New(PLabel, Init(R, GetString(dlEnvVarLabel), PL));
    D^.Insert(Labl);

    PL^.Options := PL^.Options or ofPostProcess;
    PL^.PrevFocused := -1;
    PL^.Line := PI;
    PL^.NewLisT(PC);
    end { MakeDialog };

  procedure DoPut(P: PDOSVar);
    var
      J: LongInt;

    procedure Put(C: Char);
      begin
      Env^[i] := Byte(C);
      Inc(i);
      end;

    begin
    if  (P^.Name <> nil) then
      begin
      for J := 1 to Length(P^.Name^) do
        Put(P^.Name^[J]);
      Put('=');
      for J := 1 to Length(P^.Value) do
        Put(P^.Value[J]);
      Put(#0);
      end;
    end;

  var
    n1, n2: Longint;
  begin { EditDOSEnvironment }
  New(PC, Init(10, 10));

  I := 0;
  while Env^[I] <> 0 do
    begin
    n1 := I; n2 := I;
    while Env^[I] <> 0 do
      begin
      if Char(Env^[I]) = '=' then
        n2 := I;
      Inc(I)
      end;
// JO: имя пеpеменной окpужения не может быть длиннее 255 символов,
//     по кpайней меpе мне такие извpаты неизвестны. Так что если
//     что-то такое стpанное встpетится (что пpактически маловеpоятно),
//     то pазумнее будет такую пеpеменную окpужения пpосто пpоигноpиpовать
    if (n1 <> n2) and (n2 - n1 < 255) then
      PC^.Insert(New(PDOSVar, Init(PChar(Env) + n1, n2-n1, I-n2-1)));
    Inc(I);
    end;

  MakeDialog;

  if Desktop^.ExecView(D) = cmOK then
    begin
    I := 0;
    PC^.ForEach(@DoPut);
    Env^[I] := 0;
    Env^[I+1] := 0;
    Env^[I+2] := 0;
    Env^[I+3] := 0;
    Env^[I+4] := 0;
    Env^[I+5] := 0;
    end;

  D^.Free;
  Dispose(PC, Done);
  end { EditDOSEnvironment };

(*****************************************************************
 *
 * FUNCTION:    InitHighLight
 *
 * PURPOSE:     This function initializes highlight parameters and
 *              editor options defined in DN.HGL file.
 *
 * INPUTS:      FName     - Source file openend with viewer of
 *                          editor.
 *              HiLitePar - Buffer for parameters used during
 *                          coloring text.
 *              Macros    - A collection for editor macros.
 *                          Can be nil for viewers.
 *              EdOptions - Pointer to buffer for additional editor
 *                          options. Can be nil for viewers.
 *
 * OUTPUTS:     HiLitePar - Buffer with highlighting data.
 *              Macros    - With new macros.
 *              EdOptions - With updated options.
 *
 * RETURNS:     FALSE - When file is not defined in DN.HGL or
 *                      if there is a definition 'HIGHLIGHT OFF'.
 *              TRUE  - Elsewhere.
 *
 * NOTES:
 *
 * HISTORY:     2000.06.09 - PZ - Changed interface.
 *
 *****************************************************************)

function InitHighLight
    (const FName: String;
    var HiLitePar: THighliteParams;
    Macros: PCollection;
    EdOptions: PEditOptions): Boolean;
  var
    F: PTextReader;
    I: Integer;
    CaseStr: String; {PZ - 2000.04.11}

  function Found(const S: String): Boolean;
    begin
    if Copy(FreeStr, 1, Length(S)) = S then
      begin
      Found := True;
      Delete(FreeStr, 1, Length(S));
      DelLeft(FreeStr);
      Delete(CaseStr, 1, Length(S)); {PZ - 2000.04.11}
      DelLeft(CaseStr); {PZ}
      end
    else
      Found := False;
    end;

  procedure InsertMacro;
    var
      P: PEditMacros;
    begin
    if Macros = nil then
      Exit;
    New(P, Init(FreeStr, F));
    if  (P^.Commands^.Count = 0) or (Macros^.IndexOf(P) >= 0)
    then
      Dispose(P, Done)
    else
      Macros^.Insert(P);
    end;

  function IsOn: Boolean;
    begin
    IsOn := (FreeStr = 'YES') or (FreeStr = 'ON');
    end;

  procedure MakeValue(var V: Word);
    var
      J: Integer;
    begin
    System.Val(FreeStr, J, i);
    if  (J >= 0) and (i = 0) then
      V := J;
    end;

  {PZ - 2000.05.13 - begin }
  procedure MakeStrings(Index: THighliteRule);
    begin
    while CaseStr <> '' do
      begin
      i := System.Pos(',', CaseStr);
      if i = 0 then
        i := Length(CaseStr)+1;
      if i > 1 then
        InsertHighliteRule(HiLitePar, Index, Copy(CaseStr, 1, i-1));
      Delete(CaseStr, 1, i);
      DelLeft(CaseStr);
      end;
    end;
  {PZ - end }

  procedure MakeDefaults;
    var
      j: Integer;
    begin
    while not F^.Eof do
      begin
      CaseStr := fDelLeft(fDelRight(F^.GetStr));
      FreeStr := UpStrg(CaseStr);
      if  (FreeStr = '') or (FreeStr[1] = ';') then
        Continue;
      if Found('FILES ') or Found('END') then
        Break;
      if Found('MACRO ') then
        InsertMacro
      else
        {PZ - 2000.04.14 - begin }
       if Found('COMMENT ') then
        begin
        while CaseStr <> '' do
          begin
          i := System.Pos(',', CaseStr);
          j := System.Pos(' ', CaseStr);
          if i = 0 then
            i := Length(CaseStr)+1;
          if  (j > 1) and (j < i-1) then
            begin
            FreeStr := Copy(CaseStr, 1, j-1)+#13;
            Delete(CaseStr, 1, j);
            DelLeft(CaseStr);
            i := System.Pos(',', CaseStr);
            if i = 0 then
              i := Length(CaseStr)+1;
            FreeStr := FreeStr+Copy(CaseStr, 1, i-1);
            DelRight(FreeStr);
            InsertHighliteRule(HiLitePar, hrCommentStrings, FreeStr);
            end;
          Delete(CaseStr, 1, i);
          DelLeft(CaseStr);
          end;
        end
      else if Found('COMMENTSTART ') then
        MakeStrings(hrCommentStarts)
      else if Found('COMMENTSTRING ') then
        MakeStrings(hrCommentStrings)
      else if Found('KEYWORDS1 ') then
        MakeStrings(hrKeywords1)
      else if Found('KEYWORDS2 ') then
        MakeStrings(hrKeywords2)
      else if Found('GENERALFLAGS ') then
        MakeValue(HiLitePar.GenFlags)
      else if Found('HEXNUMFLAGS ') then
        MakeValue(HiLitePar.HexFlags)
      else if Found('DECNUMFLAGS ') then
        MakeValue(HiLitePar.DecFlags)
      else if Found('OCTONUMFLAGS ') then
        MakeValue(HiLitePar.OctFlagsO)
      else if Found('OCTQNUMFLAGS ') then
        MakeValue(HiLitePar.OctFlagsQ)
      else if Found('BINNUMFLAGS ') then
        MakeValue(HiLitePar.BinFlags)
      else if Found('STRINGFLAGS ') then
        MakeValue(HiLitePar.StrFlags)
      else
        {PZ - end }
        {PZ - 2000.06.09 - begin }
       if Found('HIGHLIGHT ') then
        InitHighLight := IsOn
      else if EdOptions <> nil then
        begin
        if Found('H_LINE ') or Found('H_ROW ') then
          EdOptions^.HiliteLine := IsOn
        else if Found('H_COLUMN ') then
          EdOptions^.HiliteColumn := IsOn
        else if Found('WRAPJUSTIFY ') then
          EdOptions^.AutoJustify := IsOn
        else if Found('AUTOWRAP ') then
          EdOptions^.AutoWrap := IsOn
        else if Found('LEFTMARGIN ') then
          MakeValue(EdOptions^.LeftSide)
        else if Found('RIGHTMARGIN ') then
          MakeValue(EdOptions^.RightSide)
        else if Found('PARAGRAPH ') then
          MakeValue(EdOptions^.InSide)
        else if Found('FORCECRLF ') then
          EdOptions^.ForcedCRLF := cfCRLF
        else if Found('FORCECR ') then
          EdOptions^.ForcedCRLF := cfCR
        else if Found('FORCELF ') then
          EdOptions^.ForcedCRLF := cfLF
        end
        {PZ - end }
      end
    end { MakeDefaults };

  begin { InitHighLight }
  if Macros <> nil then
    Macros^.FreeAll;
  InitHighLight := False; {PZ 2000.06.09 Default is No Highlight }
  FillChar(HiLitePar, SizeOf(HiLitePar), 0);
  F := New(PTextReader, Init(SourceDir+'DN.HGL'));
  if F = nil then
    Exit;
  while not F^.Eof do
    begin
    FreeStr := UpStrg(fDelLeft(fDelRight(F^.GetStr)));
    if  (FreeStr = '') or (FreeStr[1] = ';') then
      Continue;
    if Found('FILES ') and InFilter(FName, FreeStr) then
      begin
      InitHighLight := True;
      {PZ 2000.06.09 Default is ON if file is defined }
      MakeDefaults;
      FixHighliteParams(HiLitePar); {PZ - 2000.04.11}
      Break;
      end
    else if Found('DEFAULT') then
      begin
      InitHighLight := True;
      {PZ 2000.06.09 Default is ON if default is defined }
      MakeDefaults;
      end;
    end;
  Dispose(F, Done);
  end { InitHighLight };

end.
