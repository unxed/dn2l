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
{AK155 = Alexey Korop, 2:461/155@fidonet}
{KV = Kirill Vodonosov}
{20.08.2002 AK Почти весь текст в той или иной степени переработан.
  См. также CellsCol. Основное нововведение - TSort и новый Recalc }
unit Calc;
{&Delphi+}
interface

uses
  Defines, Streams, Views, CellsCol, Drivers, Dialogs,
  UniWin, Commands, DNHelp, Calculat
  ;

const
  CellClipboard: PCellCollection = nil;
  ClipRect: record
    A, B: TPoint;
    end = (A: (X: 0; Y: 0); B: (X: 0; Y: 0));
  FalseStr: String[10] = 'False';
  TrueStr: String[10] = 'True';

type
  PCalcView = ^TCalcView;
  PCalcInput = ^TCalcInput;
  PInfoView = ^TInfoView;

  PCalcWindow = ^TCalcWindow;
  TCalcWindow = object(TUniWindow)
    CalcView: PCalcView;
    constructor Init(Bounds: TRect; AName: String); {DataCompBoy}
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    procedure HandleEvent(var Event: TEvent); virtual;
    destructor Done; virtual;
    end;

  TCalcView = object(TView)

    FocusEvent: TEvent;
    CalcInput: PCalcInput;
    CellInfo: PInfoView;

    HScroll, VScroll: PScrollBar;
    Delta, Cur, Mark: TPoint;
    Cells: PCellCollection;
    NumC: Byte;
    Marking, BlockDraw, Modified: Boolean;
    ShowSeparators: Boolean;
    ColWidth: array[Byte] of Byte;
    CurrentCalc, SearchPos, ErrorCell: TPoint;
    SName: PString; {DataCompBoy}
    constructor Init(Bounds: TRect;
        AInfo: PCalcInput; ACellInfo: PInfoView;
        AHScroll, AVScroll: PScrollBar);
    destructor Done; virtual;

    constructor Load(var S: TStream);
    procedure Store(var S: TStream);

    procedure Draw; virtual;
    function Valid(Command: Word): Boolean; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetCellValue(const S: String): Boolean;
    {S - имя ячейки; результат - в calculat.Res}
    function GetFuncValue(S: String): Boolean;
    {S - функция SUM или MUL, результат - в calculat.Res}
    procedure CalcError(Index: TStrIdx);
    procedure LoadSheet(FName: String); {DataCompBoy}
    procedure SaveSheet;
    procedure SaveSheetAs;
    function AskSave: Boolean;
    procedure Copy;
    procedure Paste;
    procedure Clear;
    function CalcEval(const s: String; var Value: CReal): Boolean;
    procedure ReCalc(Full: Boolean);
    procedure InsertLine;
    procedure InsertCol;
    procedure DeleteLine;
    procedure DeleteCol;
    procedure GotoCell(Cell: String);
    procedure SearchCell;
    procedure RewriteFormula(var P: PCellrec; LX, LY, DX, DY: Integer);
    end;

  TCalcInput = object(TInputLine)
    CalcView: PCalcView;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  TInfoView = object(TView)
    InfoStr: String;
    InfoAttr: Byte;
    procedure SetInfo(S: String; Attr: Byte);
    procedure Draw; virtual;
    end;

function GetFileName(var FileName: String; Mask, Title, ALabel: String;
     Buttons: Word): Word;

implementation

uses
  Advance, Lfnvp, Messages, HistList, VPUtils,
  Objects, Strings, ErrMess,
  Memory, Dos, DNApp, DNStdDlg, Advance1, Advance2,
  MicroEd2, Histries
  ;

type
  THeaderDBF = record
    {KV}
    DBFIdent: Char; { $03 - Нет MEMO; $83 - Есть MEMO }
    { FoxBase+, FoxPro, dBaseIII+, dBaseIV, no memo - 0x03 }
    { FoxBase+, dBaseIII+ with memo - 0x83 }
    { FoxPro with memo - 0xF5 }
    { dBaseIV with memo - 0x8B }
    { dBaseIV with SQL Table - 0x8E }

    Yar: Byte; { две последние цифры года }
    Month: Byte; { month number }
    Day: Byte; { day number }
    LastRecord: LongInt; { номер последней записи }
    DataOffset: AWord;
    { смещение первой записи относительно начала файла}
    RecSize: AWord; { размер записи с учетом символа удаления }
    Reserv1: AWord;
    WaitTrans: Byte; { dB IV }
    Reserv2: array[0..12] of Byte;
    Multiindex: Byte;
    Reserv3: array[0..2] of Byte;
    end;
  TDBFLength = record
    { длина поля DBF } {KV}
    case Integer of
      0: (FieldLength: AWord);
      1: (NumericLength: Byte; Decimals: Byte);
    end;
  PDBFField = ^TDBFField; {KV}
  TDBFField = record
    { dB field descriptor Length = 32 bytes }
    FieldName: array[0..10] of Char; { имя поля }
    FieldType: Char; { тип: C=$43, D=$44, L=$4C, $M=$4D, N=$4E }
    Reserved: array[0..3] of Char; { Расположение поля внутри записи }
    FLength: TDBFLength; { длина поля }
    Reserved2: array[0..12] of Byte;
    Tag: Byte; { for multiindex, only dB IV }
    end;

  PDbfFieldCollection = ^TDbfFieldCollection;
  TDbfFieldCollection = object(TCollection)
    {KV}
    procedure FreeItem(Item: Pointer); virtual;
    end;

  TDbaseWriter = object(TBufStream)
    {KV}
    Header: THeaderDBF;
    Fields: PDbfFieldCollection;
    CurRecord: LongInt;
    EofFlag: Boolean;
    constructor Init(FileName: FNameStr; Mode: Word; Size: SW_Word);
    destructor Done; virtual;
    procedure AddField(const NameField: String; TypeField: Char;
        LenField: Integer; DecField: Integer);
    procedure AddRecord;
    procedure CreateFile;
    procedure DeleteRecord;
    function FCount: Integer;
    function FieldAsString(FieldIndex: Integer): String;
    function FieldAsInteger(FieldIndex: Integer): Integer;
    function FieldAsFloat(FieldIndex: Integer): Double;
    procedure FieldPutString(FieldIndex: Integer; FieldValue: String);
    procedure FieldPutInteger(FieldIndex: Integer; FieldValue: Integer);
    procedure FieldPutFloat(FieldIndex: Integer; FieldValue: Double);
    function FieldName(FieldIndex: Integer): String;
    // Warning! FieldIndex from 1
    function FieldDec(FieldIndex: Integer): Byte; // FieldIndex >= 1
    function FieldLen(FieldIndex: Integer): AWord;
    // Warning! FieldIndex from 1
    function FieldPos(NameField: String): Integer; // FieldIndex >= 1
    function FieldType(FieldIndex: Integer): Char;
    function FieldOffsetInBuffer(FieldIndex: Integer): Integer;
    // FieldIndex >= 1
    function FieldOffsetInFile(FieldIndex: Integer): LongInt;
    // FieldIndex >= 1
    procedure First;
    procedure DBGoTo(NewRecord: Integer);
    procedure Next;
    procedure ReadFile;
    procedure RecallRecord;
    procedure WriteEndOfFile;
    end;

  TExcelWriter = object(TBufStream)
    {KV}
    procedure WriteBOF;
    procedure WriteEOF;
    procedure WriteBLANK(const Col, Row: AWord; const XF: AWord);
    procedure WriteLABEL(const Data: String; const Col, Row: AWord;
         const XF: AWord);
    procedure WriteNUMBER(const Data: Double; const Col, Row: AWord;
         const XF: AWord);
    procedure WriteBOOL(const Data: Boolean; const Col, Row: AWord;
         const XF: AWord);
    procedure WriteERROR(const Data: Byte; const Col, Row: AWord;
         const XF: AWord);
    end;

const
  SearchData: record
    S: String[240];
    CellOptions, TxtOptions: Word;
    end =
    (S: ''; CellOptions: 0; TxtOptions: 0);
  ReplaceData: record
    S, S1: String[240];
    CellOptions, TxtOptions: Word;
    end =
    (S: ''; S1: ''; CellOptions: 0; TxtOptions: 0);
  WasReplace: Boolean = False;
  ContSearch: Boolean = False;
  SearchCanceled: Boolean = False;
  DefaultColWidth = 11;
  UntitledName = 'Untitled.WKZ';

  {-DataCompBoy-}
function GetFileName(var FileName: String; Mask, Title, ALabel: String;
     Buttons: Word): Word;
  var
    S: String;
    D: PFileDialog;
    B: Boolean;
    C: Word;
  begin
  S := '';
  B := False;
  if Mask = '' then
    begin
    Mask := x_x;
    B := False
    end;
  D := PFileDialog(Application^.ValidView(New(PFileDialog,
          Init(Mask, Title, ALabel, Buttons, 211))));
  if D = nil then
    Exit;
  if B then
    D^.SetData(S);
  C := Desktop^.ExecView(D);
  if C <> cmCancel then
    D^.GetFileName(S);
  GetFileName := C;
  Dispose(D, Done);
  {$IFDEF DPMI32}
  FileName := lfGetLongFileName(S);
  {$ELSE}
  FileName := S;
  {$ENDIF}
  end { GetFileName };
{-DataCompBoy-}

{-------------------------       TInfoView       -------------------------}

procedure TInfoView.SetInfo;
  begin
  InfoStr := Copy(S, 1, Size.X);
  InfoAttr := Attr;
  DrawView;
  end;

procedure TInfoView.Draw;
  var
    B: TDrawBuffer;
  begin
  MoveChar(B, ' ', InfoAttr, Size.X);
  MoveStr(B[Size.X-Length(InfoStr)], InfoStr, InfoAttr);
  WriteLine(0, 0, Size.X, 1, B);
  end;

{-------------------------      TCalcWindow      -------------------------}

{-DataCompBoy-}
constructor TCalcWindow.Init;
  var
    R: TRect;
    P: PView;
    P1: PInfoView;
  begin
  if  (AName = '') or (GetFileAttr(AName+#0) and Directory <> 0) then
    AName := UntitledName
  else
    AName := lFExpand(AName);
  TWindow.Init(Bounds, Cut(AName, Bounds.B.X-Bounds.A.X-12), 0);
  Options := Options or ofTileable;
  Flags := Flags or wfMaxi;
  if LowMemory then
    Exit;

  R.A.X := 1;
  R.A.Y := 5;
  R.B.X := 7;
  R.B.Y := Size.Y-1;
  P := New(PStaticText, Init(R, ''));
  P^.Options := P^.Options or ofFramed;
  P^.GrowMode := gfGrowHiY;
  Insert(P);

  R.A.X := 8;
  R.A.Y := 2;
  R.B.X := Size.X-1;
  R.B.Y := 4;
  P := New(PStaticText, Init(R, ''));
  P^.Options := P^.Options or ofFramed;
  P^.GrowMode := gfGrowHiX;
  Insert(P);

  GetExtent(R);
  R.Grow(-1, -1);
  R.B.Y := R.A.Y+1;
  P := PView(LoadResource(dlgWkzMenuBar));
  P^.Locate(R);
  P^.GrowMode := gfGrowHiX;
  Insert(P);

  GetExtent(R);
  R.Grow(-1, -1);
  Inc(R.A.Y);
  R.B.Y := R.A.Y+1;
  Inc(R.A.X, 7);
  P := New(PCalcInput, Init(R, 240));
  P^.GrowMode := gfGrowHiX;
  P^.Options := P^.Options or ofSelectable;
  Insert(P);

  R.Assign(1, 2, 7, 4);
  P1 := New(PInfoView, Init(R));
  PInfoView(P1)^.InfoStr := '';
  PInfoView(P1)^.InfoAttr := GetColor(9);
  Insert(P1);

  GetExtent(R);
  R.Grow(-1, -1);
  Inc(R.A.Y, 2);
  CalcView := New(PCalcView, Init(R, PCalcInput(P), P1,
        MakeScrollBar(sbHorizontal+sbHandleKeyboard),
        MakeScrollBar(sbVertical+sbHandleKeyboard)));
  Insert(CalcView);
  CalcView^.LoadSheet(AName);
  end { TCalcWindow.Init };
{-DataCompBoy-}

constructor TCalcWindow.Load;
  var
    R: TRect;
    P: PView;
    P1: PInfoView;
  begin
  inherited Load(S);
  GetSubViewPtr(S, CalcView);
  if CalcView = nil then
    {Cat}
    Fail;
  end;

procedure TCalcWindow.Store;
  begin
  inherited Store(S);
  PutSubViewPtr(S, CalcView);
  end;

procedure TCalcWindow.HandleEvent;
  begin
  case Event.What of
    evCommand:
      case Event.Command of
        cmClose, cmQuit:
          if not CalcView^.AskSave then
            ClearEvent(Event);
      end {case};
    evBroadcast:
      case Event.Command of
        cmFindView:
          if PString(Event.InfoPtr)^ = CnvString(CalcView^.SName) then
            begin
            Self.Select;
            ClearEvent(Event);
            end;
      end {case};
  end {case};
  inherited HandleEvent(Event);
  end;

destructor TCalcWindow.Done;
  begin
  StoreViewInfo(@Self);
  inherited Done;
  end;

{-----------------------------    TCalcInput     ---------------------------}

constructor TCalcInput.Load;
  begin
  inherited Load(S);
  GetPeerViewPtr(S, CalcView);
  end;

procedure TCalcInput.Store;
  begin
  inherited Store(S);
  PutPeerViewPtr(S, CalcView);
  end;

function TCalcInput.GetPalette;
  const
    S: String[4] = #13#13#14#13;
  begin
  GetPalette := @S;
  end;

procedure MakeDefaultOptions(const S: String;
    var o: AWord; var D: Byte; var R: CReal);
  var
    I: Integer;
  begin
  Val(S, R, I);
  D := 2; { 2 десятичных знака по умолчанию }
  if I = 0 then
    { число: вправо, как есть }
    o := coValue or coRight
  else if (S[1] = '=') and (Length(S) > 1) then
    { формула: вправо, десятичное, 2 знака }
    o := coFormula or coDec or coRight
  else { текст: влево, как есть }
    begin
    o := 0;
    R := 0;
    end;
  end;

procedure TCalcInput.HandleEvent;

  function EndEdit: Boolean;
    label 2;
    var
      NewS: ShortString;
      O: AWord;
      D: Byte;
      R: CReal;
      I: Integer;
      P: PCellrec;
      TypeChanged: Boolean;
    begin
    GetData(NewS);
    EndEdit := True;
    with CalcView^ do
      begin
      Modified := True;
      CurrentCalc.X := Delta.X+Cur.X;
      CurrentCalc.Y := Delta.Y+Cur.Y;
      DelRight(NewS);
      P := Cells^.Get(Delta.X+Cur.X, Delta.Y+Cur.Y);
      if  (P <> nil) and (NewS = P^.S) then
        Exit;
      if NewS = '' then
        begin
        if  (P = nil) then
          Exit;
        if MessageBox(GetString(dlWkzWarningClearCell),
            nil, mfOKCancel) <> cmOK
        then
          begin
          EndEdit := False;
          Exit;
          end;
        Cells^.DelItem(Delta.X+Cur.X, Delta.Y+Cur.Y);
        goto 2;
        end;

      MakeDefaultOptions(NewS, O, D, R);
      TypeChanged := True; // на случай P=nil
      if P <> nil then
        with P^ do
          begin
          if  (Options and coTypeMask) = coTypeMask then
            Options := (Options and not coTypeMask) or coFormula;
          TypeChanged := (Options xor O) and coTypeMask <> 0;
          if TypeChanged then
            begin {Изменен тип ячейки}
            if MessageBox(GetString(dlWkzWarningCellTypeChange),
                nil, mfOKCancel) <> cmOK
            then
              begin
              EndEdit := False;
              Exit;
              end;
            end;
          end;
      with Cells^.ReplaceItem(Delta.X+Cur.X, Delta.Y+Cur.Y, NewS)^ do
        begin
        if TypeChanged then
          begin
          Options := O;
          Decimals := D;
          end;
        Value := R;
        end;
2:
      ReCalc(False);
      end {with CalcView^};
    end { EndEdit: };

  procedure Transfer;
    begin
    CalcView^.FocusEvent := Event;
    if EndEdit then
      PWindow(Owner)^.SelectNext(False);
    ClearEvent(Event)
    end;

  begin { TCalcInput.HandleEvent }
  case Event.What of
    evKeyDown:
      case Event.KeyCode of
        kbUp, kbDown, kbPgUp, kbPgDn, kbCtrlPgUp,
        kbCtrlPgDn, kbEnter:
          Transfer;
        kbLeft:
          if CurPos = 0 then
            Transfer;
        kbRight:
          if CurPos > Length(Data)-1 then
            Transfer;
        kbTab:
          if not EndEdit then
            ClearEvent(Event);
      end {case};
  end {case};
  TInputLine.HandleEvent(Event);
  end { TCalcInput.HandleEvent };

{-----------------------------    TCalcView     ---------------------------}

constructor TCalcView.Init;
  var
    I, J: Integer;
  begin
  TView.Init(Bounds);
  HelpCtx := hcSpreadSheet;
  HScroll := AHScroll;
  VScroll := AVScroll;
  Options := Options or ofSelectable;
  GrowMode := gfGrowHiX+gfGrowHiY;
  EventMask := $FFFF;
  CalcInput := AInfo;
  CellInfo := ACellInfo;
  PCalcInput(CalcInput)^.CalcView := @Self;
  FocusEvent.What := evNothing;
  FillChar(ColWidth, SizeOf(ColWidth), DefaultColWidth);
  Marking := False;
  BlockDraw := False;
  Modified := False;
  Cells := nil;
  Delta.X := 0;
  Delta.Y := 0;
  Cur := Delta;
  Mark := Cur;
  HScroll^.SetParams(0, 0, 255, 1, 1);
  VScroll^.SetParams(0, 0, MaxCellY-1, Size.Y-3, 1);
  SName := nil;
  end { TCalcView.Init };

constructor TCalcView.Load;
  var
    R: TRect;
  begin
  inherited Load(S);
  GetPeerViewPtr(S, CalcInput);
  GetPeerViewPtr(S, CellInfo);
  GetPeerViewPtr(S, HScroll);
  GetPeerViewPtr(S, VScroll);
  S.Read(Delta, SizeOf(Delta)*3);
  S.Read(NumC, 6); {###}
  SName := S.ReadStr;
  LoadSheet(SName^);
  FocusEvent.What := evNothing;
  ReCalc(True);
  end;

procedure TCalcView.Store;
  begin
  inherited Store(S);
  PutPeerViewPtr(S, CalcInput);
  PutPeerViewPtr(S, CellInfo);
  PutPeerViewPtr(S, HScroll);
  PutPeerViewPtr(S, VScroll);
  S.Write(Delta, SizeOf(Delta)*3);
  S.Write(NumC, 6); {###}
  S.WriteStr(SName);
  end;

function TCalcView.Valid;
  begin
  Valid := Cells <> nil;
  end;

procedure TCalcView.SetState;
  var
    Bounds: TRect;
  begin
  TView.SetState(AState, Enable);
  if Enable and (AState and sfFocused <> 0) then
    DrawView;
  if  (AState and sfActive <> 0) then
    if Enable then
      begin
      HScroll^.GrowTo(HScroll^.Size.X, 1);
      VScroll^.GrowTo(1, VScroll^.Size.Y);
      EnableCommands([cmUndo, cmCut, cmCopy, cmPaste, cmClear]);
      end
    else
      begin
      HScroll^.GrowTo(HScroll^.Size.X, 0);
      VScroll^.GrowTo(0, VScroll^.Size.Y);
      end;
  if Owner <> nil then
    begin
    GetBounds(Bounds);
    DisposeStr(PWindow(Owner)^.Title);
    if SName <> nil then
      PWindow(Owner)^.Title := NewStr(Cut(SName^,
             Bounds.B.X-Bounds.A.X-12));
    end;
  end { TCalcView.SetState };

function TCalcView.GetPalette;
  const
    S: String[4] = #8#13#14;
  begin
  GetPalette := @S;
  end;

type
  TQArray = array[0..80, 0..60] of Integer;
  PQArray = ^TQArray;

const
  Q: PQArray = nil;

function MakeCellText(P: PCellrec): String; {KV}
  var
    S1: String absolute Result;
    i, A1, A2: Integer;
  begin
  S1 := '';
  case (P^.Options shr 4) and 7 of
    0:
      S1 := P^.S;
    1:
      Str(P^.Value: 0: P^.Decimals, S1);
    2:
      begin
      Str(P^.Value: 0: 20, S1);
      i := Length(S1);
      while (i > 0) and (S1[i] = '0') do
        Dec(i);
      SetLength(S1, i);
      if S1[Length(S1)] = '.' then
        SetLength(S1, Length(S1)-1);
      S1 := MakeComma(S1);
      end;
    3:
      begin
      Str(P^.Value, S1);
      A1 := Pos('E', S1);
      if A1 > 0 then
        begin
        A2 := A1-1;
        while (S1[A2] = '0') and (S1[A2-1] <> '.') do
          Dec(A2);
        Move(S1[A1], S1[A2+1], Length(S1)-A1+1);
        SetLength(S1, Length(S1)-(A1-A2-1));
        end;
      end;
    4:
      if P^.Value = 0 then
        S1 := FalseStr
      else
        S1 := TrueStr;
    5:
      MakeCurrency(P^.Value, S1);
    6:
      begin
      Str((P^.Value*100): 0: 2, S1);
      S1 := MakeComma(S1)+'%';
      end;
    7:
      S1 := '';
  end {case};
  end { MakeCellText };

procedure FormatCellText(const S1: String; k: Integer;
    ShowSeparators: Boolean;
    Options: AWord; var S: String);
  var
    T: Integer;
  begin
  if Length(S1) >= k then
    begin
    if ShowSeparators then
      begin
      Move(S1[1], S[1], k-1);
      S[k] := #16;
      end
    else
      Move(S1[1], S[1], Length(S1))
    end
  else
    begin
    T := 1-Ord(ShowSeparators);
    if Options and $C = 0 then
      Move(S1[1], S[1], Length(S1))
    else if (Options and $C = coRight) then
      Move(S1[1], S[k-Length(S1)+T], Length(S1))
    else
      Move(S1[1], S[1+(k-Length(S1)) div 2], Length(S1));
    end;
  end { FormatCellText };

procedure TCalcView.Draw;

  var
    B, B1: TDrawBuffer;
    BC: array[0..High(B)] of record
      C: Char;
      A: Byte;
      end absolute B;
    I, J, K, L, T: Integer;
    C1, C2, C3: Byte;
    S, S1: String;
    P: PCellrec;
    X1, X2, Y1, Y2: Integer;
    CurPos: TPoint;
    StX, EnX, A1, A2: Byte;
    Wrk: Integer;
    EmptyLine: String;
  begin
  if BlockDraw then
    Exit;
  C1 := GetColor(1);
  C2 := GetColor(2);
  C3 := GetColor(3);
  if Q = nil then
    New(Q);
  CurPos.X := Delta.X+Cur.X;
  CurPos.Y := Delta.Y+Cur.Y;
  if CurPos.X > 255 then
    Cur.X := 255-Delta.X;
  if CurPos.Y >= MaxCellY then
    Cur.Y := MaxCellY-1-Delta.Y;
  if Cur.Y < 0 then
    begin
    J := Cur.Y;
    Cur.Y := 0;
    VScroll^.SetValue(Delta.Y+J);
    Exit
    end;
  if Cur.X < 0 then
    begin
    J := Cur.X;
    Cur.X := 0;
    HScroll^.SetValue(Delta.X+J);
    Exit
    end;
  CurPos.X := Delta.X+Cur.X;
  CurPos.Y := Delta.Y+Cur.Y;
  X1 := CurPos.X;
  X2 := Mark.X;
  Y1 := CurPos.Y;
  Y2 := Mark.Y;
  if X1 > X2 then
    begin
    Wrk := X1;
    X1 := X2;
    X2 := Wrk;
    end;
  if Y1 > Y2 then
    begin
    Wrk := Y1;
    Y1 := Y2;
    Y2 := Wrk;
    end;
  if Y2 > 4095 then
    Y2 := 4095;
  SetLength(EmptyLine, Size.X);
  FillChar(EmptyLine[1], Size.X, ' ');
  MoveChar(B, ' ', C1, Size.X);
  BC[6].C := '│';
  MoveChar(B1, '─', C1, Size.X);
  WordRec(B1[6]).Lo := Byte('┼');
  NumC := 0;
  I := 7;
  J := Delta.X;
  StX := I;
  EnX := Size.X;

  {Заголовки колонок и верхняя черта}
  while (I < Size.X) and (J <= HScroll^.Max) do
    begin
    K := ColWidth[J];
    L := ColWidth[J+1];
    SetLength(S, K);
    FillChar(S[1], K, ' ');
    S[K] := '│';
    S1 := GetColName(J);
    Move(S1[1], S[K div 2], Length(S1));
    WordRec(B1[I+K-1]).Lo := Byte('┼');
    MoveStr(B[I], S, C1);
    if J < X1 then
      Inc(StX, K);
    if  (J > X2) and (EnX > I) then
      EnX := I;
    if  (J = HScroll^.Max) and (J <= X2) then
      EnX := I+K;
    Inc(NumC);
    Inc(J);
    Inc(I, K);
    end;

  if NumC = 0 then
    NumC := 1;
  if Cur.X >= NumC then
    begin
    J := Cur.X-NumC+1;
    Cur.X := NumC-1;
    HScroll^.SetValue(Delta.X+J);
    Delta.X := HScroll^.Value;
    Exit;
    end;
  PInfoView(CellInfo)^.SetInfo(GetCellName(CurPos.X, CurPos.Y),
     Owner^.GetColor(9));
  FillChar(Q^, SizeOf(Q^), 255);
  for I := 1 to Cells^.Count do
    begin
    P := Cells^.At(I-1);
    if  (P^.Row >= Delta.Y) and (P^.Row < Delta.Y+Size.Y-2) and
        (P^.Col >= Delta.X) and (P^.Col < Delta.X+NumC+3)
    then
      Q^[P^.Col-Delta.X, P^.Row-Delta.Y] := I-1;
    end;
  WriteBuf(6, 0, Size.X, 1, B[6]);
  WriteBuf(0, 1, Size.X, 1, B1);

  for I := 0 to Size.Y-2 do
    begin
    MoveChar(B, ' ', C2, Size.X);
    if Delta.Y+I <= VScroll^.Max then
      begin
      Str((Delta.Y+I+1): 5, S);
      MoveStr(B, S, C1);
      L := 7;
      for J := 0 to Min(NumC-1, 255-Delta.X) do
        begin
        K := ColWidth[Delta.X+J];
        if ShowSeparators then
          begin { ограничиваем длину по колонке }
          SetLength(S, K);
          FillChar(S[1], K, ' ');
          S[K] := '│';
          end
        else
          begin { длина до конца экрана }
          T := Size.X-L+1;
          SetLength(S, T);
          FillChar(S[1], T, ' ');
          end;
        P := nil;
        if Q^[J, I] >= 0 then
          begin
          P := Cells^.At(Q^[J, I]);
          S1 := MakeCellText(P);
          FormatCellText(S1, K, ShowSeparators, P^.Options, S);
          end;
        if ShowSeparators or (P <> nil) then
          MoveStr(B[L], S, C2);
        if  (Cur.X = J) and (Cur.Y = I) then
          begin
          if P <> nil then
            begin
            S := P^.S;
            case P^.Options and 3 of
              coText:
                WriteStr(0, 0, '   T  ', 2);
              coValue:
                WriteStr(0, 0, '   V  ', 2);
              else {case}
                WriteStr(0, 0, '   F  ', 2);
            end {case};
            end
          else
            begin
            WriteStr(0, 0, GetString(dlWKZ_Empty), 2);
            S := '';
            end;
          PCalcInput(CalcInput)^.SetData(S);
          CalcInput^.Draw;
          end;
        L := L+K;
        end;
      end;
    BC[6].C := '│';
    BC[6].A := C1;
    if  (Delta.Y+I >= Y1) and (Delta.Y+I <= Y2) and (StX < EnX)
    then
      MoveColor(B[StX], EnX-StX-Ord(ShowSeparators), C3);
    WriteBuf(0, I+2, Size.X, 1, B);
    end;
  end { TCalcView.Draw };

{-DataCompBoy-}
procedure TCalcView.HandleEvent;
  type
    TColIndexArray = array[0..255] of Integer; {KV}
    TColTypeAndLengthRec = record
      {KV}
      T: Char;
      l: AWord;
      D: Byte;
      end;
    PColTypeAndLengthArray = ^TColTypeAndLengthArray;
    TColTypeAndLengthArray = array[0..255] of TColTypeAndLengthRec; {KV}

  var
    Inf: PCalcInput;
    S: String;
    St: PStream;
    MaxX, MaxY, K, L: Integer;
    P: PCellrec;
    FName: String;
    X1, X2, Y1, Y2: Integer;
    R: record
      Display, Justify: Word;
      Dec: String[2];
      Protect: Word;
      end;
    I: Integer;
    Wrk: Integer;

  procedure kvGetMaxColRow(var MaxCol, MaxRow: Integer); {KV}
    var
      i: Integer;
      P: PCellrec;
    begin
    MaxCol := 0;
    MaxRow := 0;
    for i := 0 to Cells^.Count-1 do
      begin
      P := Cells^.At(i);
      if MaxCol < P^.Col then
        MaxCol := P^.Col;
      if MaxRow < P^.Row then
        MaxRow := P^.Row;
      end;
    end;

  procedure kvFillColIndexArray(CurRow: Integer;
       var ColIndexArray: TColIndexArray); {KV}
    var
      i: Integer;
      P: PCellrec;
    begin
    FillChar(ColIndexArray, SizeOf(ColIndexArray), 255);
    for i := 0 to Cells^.Count-1 do
      begin
      P := Cells^.At(i);
      if P^.Row = CurRow then
        ColIndexArray[P^.Col] := i;
      end;
    end;

  procedure ImportFromCsv; {KV}
    var
      S: String;
      FileName: String;
      CellValue: String;
      ColSeparator: Char;
      F: lText;
      CurCol, CurRow: Integer;
      StartCh, EndCh: Integer;
      P: PCellrec;
      R: CReal;
      i: Integer;
    begin
    if Cells^.Count > 0 then
      begin
      if MessageBox(GetString(dlWkzWarningClear), nil,
          mfYesNoCancel or mfConfirmation) <> cmYes
      then
        Exit;
      end;
    if GetFileName(FileName, '*.CSV', GetString(dlOpenFile),
           GetString(dlOpenFileName),
        fdOKButton+fdHelpButton) = cmCancel
    then
      Exit;
    {KV: разделитель колонок можно определить автоматически, однако при этом
         возникает несколько проблем: если в файле только одна строка нельзя
         определить какие символы встречаются во второй строке столько-же раз
         сколько и в первой. Выполнять проверку только на ',',';' неправильно}
    S := ';';
    if InputBox(GetString(dlWkzQuerySeparatorTitle),
           GetString(dlWkzQuerySeparatorLabel),
        S, 1, hcSpreadSheet) <> cmOK
    then
      Exit;
    if S = '' then
      ColSeparator := ';'
    else
      ColSeparator := S[1];

    lAssignText(F, FileName);
    lSetTAttr(F, Archive);
    lResetTextReadOnly(F);
    i := IOResult;
    if i <> 0 then
      begin
      MessFileNotOpen(FileName, i);
      Exit;
      end;
    Cells^.FreeAll;
    CurRow := 0;
    while not Eof(F.T) do
      begin
      Readln(F.T, S);
      if S <> '' then
        begin {KV: Просто на всякий случай}
        CurCol := 0;
        StartCh := 1;
        while StartCh <= Length(S) do
          begin
          EndCh := StartCh;
          while (EndCh <= Length(S)) and (S[EndCh] <> ColSeparator) do
            Inc(EndCh);
          if EndCh > StartCh then
            begin
            CellValue := System.Copy(S, StartCh, EndCh-StartCh);
            P := Cells^.NewItem(CurCol, CurRow, CellValue);
            MakeDefaultOptions(CellValue, P^.Options, P^.Decimals,
               P^.Value);
            end;
          StartCh := EndCh+1;
          Inc(CurCol);
          end;
        end;
      Inc(CurRow);
      end;
    Close(F.T);
    ReCalc(True);
    Modified := True;
    if Owner <> nil then
      Owner^.Redraw;
    end { ImportFromCsv };

  procedure ImportFromDbf; {KV}
    var
      DBF: TDbaseWriter;
      FileName: FNameStr;
      CellValue: String;
      P: PCellrec;
      R: CReal;
      i: Integer;
    begin
    if Cells^.Count > 0 then
      begin
      if MessageBox(GetString(dlWkzWarningClear), nil,
          mfYesNoCancel or mfConfirmation) <> cmYes
      then
        Exit;
      end;
    if GetFileName(FileName, '*.DBF', GetString(dlOpenFile),
           GetString(dlOpenFileName),
        fdOKButton+fdHelpButton) = cmCancel
    then
      Exit;
    DBF.Init(FileName, stOpenRead, 16384);
    if DBF.Status <> stOK then
      begin
      MessFileNotOpen(FileName, DBF.ErrorInfo);
      DBF.Done;
      Exit;
      end;

    Cells^.FreeAll;
    DBF.First;
    while not DBF.EofFlag do
      begin
      for i := 1 to DBF.FCount do
        begin
        CellValue := DBF.FieldAsString(i);
        DelRight(CellValue);
        if CellValue <> '' then
          with Cells^.NewItem(i-1, DBF.CurRecord-1, CellValue) do
            case DBF.FieldType(i) of
              'N': {число}
                begin
                DelLeft(S);
                Options := coValue or coRight;
                Decimals := DBF.FieldDec(i);
                end;
              'L': {логическое}
                begin
                if UpCase(CellValue[1]) = 'T' then
                  S := '-1'
                else
                  S := '0';
                Options := coValue or coBool or coRight;
                end;
              else {текст, дата, мемо...}
                if  (CellValue[1] = '=') and (Length(CellValue) > 1)
                then
                  begin
                  Options := coFormula or coDec or coRight;
                  Decimals := 2;
                  end;
            end {case};
        end;
      DBF.Next;
      end;
    DBF.Done;
    ReCalc(True);
    Modified := True;
    Owner^.Redraw;
    end { ImportFromDbf };

  procedure ExportToCsv; {KV}
    var
      S: String;
      PS: PString;
      ColSeparator: Char;
      UseFormatFlag: Boolean;
      CalcFormulaFlag: Boolean;
      F: lText;
      MaxCol, MaxRow, CurCol, CurRow: Integer;
      P: PCellrec;
      ColIndexArray: TColIndexArray;
      R: record
        Separator: String[1];
        ValueFormat: Word;
        FormulaFormat: Word;
        end;
    begin
    if GetFileName(S, '*.CSV', GetString(dlExportTitle),
           GetString(dlExportLabel),
        fdOKButton+fdHelpButton) = cmCancel
    then
      Exit;
    if ExistFile(S) then
      begin
      PS := @S;
      if MessageBox(GetString(dlED_OverQuery), @PS,
          mfYesNoCancel+mfWarning) <> cmYes
      then
        Exit;
      end;

    FillChar(R, SizeOf(R), 0);
    R.Separator := ';';
    if ExecResource(dlgSetExportCsvFormat, R) = cmOK then
      begin
      if Length(R.Separator) > 0 then
        ColSeparator := R.Separator[1]
      else
        ColSeparator := ';';
      UseFormatFlag := (R.ValueFormat = 1);
      CalcFormulaFlag := (R.FormulaFormat = 1);
      lAssignText(F, S);
      lSetTAttr(F, Archive);
      lRewriteText(F);
      if IOResult <> 0 then
        begin
        PS := @S;
        Msg(erCantCreateFile, @PS, mfError+mfOKButton);
        Exit;
        end;
      kvGetMaxColRow(MaxCol, MaxRow);
      for CurRow := 0 to MaxRow do
        begin
        kvFillColIndexArray(CurRow, ColIndexArray); {KV}
        for CurCol := 0 to MaxCol do
          begin
          if ColIndexArray[CurCol] >= 0 then
            begin
            P := Cells^.At(ColIndexArray[CurCol]);
            if  ( (P^.Options and 3) = coText) or
                ( ( (P^.Options and coFormula) <> 0) and (not
                   CalcFormulaFlag))
            then
              Write(F.T, P^.S)
            else if ((P^.Options and 3) = coValue) or
                ( ( (P^.Options and coFormula) <> 0) and
                 CalcFormulaFlag)
            then
              begin
              if UseFormatFlag then
                S := MakeCellText(P)
              else
                begin
                // Оставляем 5 знаков после запятой, на случай если
                // неправильно указан формат
                if P^.Decimals <= 5 then
                  Str(P^.Value: 0: 5, S)
                else
                  Str(P^.Value: 0: P^.Decimals, S);
                while S[Length(S)] = '0' do
                  SetLength(S, Length(S)-1);
                if S[Length(S)] = '.' then
                  SetLength(S, Length(S)-1);
                end;
              Write(F.T, S);
              end
            else
              Write(F.T, P^.S); {This code not use}
            end;
          if CurCol < MaxCol then
            Write(F.T, ColSeparator);
          end;
        Writeln(F.T);
        end;
      Close(F.T);
      end;
    end { ExportToCsv };

  procedure kvCalcFieldTypeAndLength(var TypeLenArr:
       TColTypeAndLengthArray);
    var
      i: Integer;
      P: PCellrec;
      S1: String;
    begin
    FillChar(TypeLenArr, SizeOf(TypeLenArr), 0);
    for i := 0 to Cells^.Count-1 do
      begin
      P := Cells^.At(i);
      with TypeLenArr[P^.Col] do
        begin
        if  (P^.Options and coValue) <> coValue then
          // Если в колонке текст или ошибка в формуле,
          // то эта колонка сохраняется как текстовая
          T := 'C'
        else if (T = #0) and ((P^.Options and $F0) = coBool) then
          // Если тип колонки еще не оределен и первое найденное значение
          // имеет формат Boolean, то устанавливаем тип Boolean
          T := 'L'
        else if T = #0 then
          T := 'N'; // По умолчанию - число
        end;
      end;
    // Делаем второй проход когда типы полей уже известны
    // для определения размерности
    for i := 0 to Cells^.Count-1 do
      begin
      P := Cells^.At(i);
      with TypeLenArr[P^.Col] do
        begin
        case T of
          'C':
            begin
            if l < Length(P^.S) then
              l := Length(P^.S);
            end;
          'N':
            begin
            Str(P^.Value: 0: P^.Decimals, S1);
            if D < P^.Decimals then
              begin
              if l > 0 then
                Inc(l, P^.Decimals-D);
              D := P^.Decimals;
              end;
            if l < Length(S1) then
              l := Length(S1);
            end;
          'L':
            begin
            l := 1;
            D := 0;
            end;
        end {case}; { case }
        end; { with }
      end; { for }
    end { kvCalcFieldTypeAndLength };

  procedure ExportToDbf; {KV}
    var
      FileName: FNameStr;
      PS: PString;
      TypeLenArr: TColTypeAndLengthArray;
      i, MaxCol, MaxRow, CurCol, CurRow: Integer;
      DbfHeader: THeaderDBF;
      S: TDbaseWriter;
      Buf: PByteArray;
      TmpS: String;
      FieldName: String;
      ColIndexArray: TColIndexArray;
      P: PCellrec;
    begin
    if GetFileName(FileName, '*.DBF', GetString(dlExportTitle),
           GetString(dlExportLabel),
        fdOKButton+fdHelpButton) = cmCancel
    then
      Exit;

    if ExistFile(FileName) then
      begin
      PS := @FileName;
      if MessageBox(GetString(dlED_OverQuery), @PS,
          mfYesNoCancel+mfWarning) <> cmYes
      then
        Exit;
      end;

    S.Init(FileName, stCreate, 16384);
    if S.Status <> stOK then
      begin
      PS := @FileName;
      Msg(erCantCreateFile, @PS, mfError+mfOKButton);
      S.Done;
      Exit;
      end;

    kvGetMaxColRow(MaxCol, MaxRow);
    kvCalcFieldTypeAndLength(TypeLenArr);

    for i := 0 to MaxCol do
      begin
      with TypeLenArr[i] do
        begin
        FieldName := GetColName(i);
        if T = #0 then
          begin
          T := 'N';
          l := 5;
          D := 0;
          end;
        if l = 0 then
          l := 1;
        if  (T = 'L') and (l > 1) then
          l := 1;
        if  (T <> 'N') and (D <> 0) then
          D := 0;
        S.AddField(FieldName, T, l, D);
        end;
      end;

    S.CreateFile;

    for CurRow := 0 to MaxRow do
      begin
      kvFillColIndexArray(CurRow, ColIndexArray);
      i := 1; // Номер текущего поля внутри записи
      S.AddRecord; // Добавляем новую пустую запись
      for CurCol := 0 to MaxCol do
        begin
        if ColIndexArray[CurCol] >= 0 then
          begin
          P := Cells^.At(ColIndexArray[CurCol]);
          if TypeLenArr[CurCol].T = 'L' then
            begin
            if P^.Value = 0 then
              TmpS := 'F'
            else
              TmpS := 'T';
            S.FieldPutString(i, TmpS);
            end
          else if TypeLenArr[CurCol].T = 'N' then
            S.FieldPutFloat(i, P^.Value)
          else
            S.FieldPutString(i, P^.S);
          end;
        Inc(i);
        end;
      end;
    S.WriteEndOfFile;
    S.Done;
    end { ExportToDbf };

  procedure ExportToXls; {KV}
    var
      FileName: FNameStr;
      PS: PString;
      i, MaxCol, MaxRow, CurCol, CurRow: Integer;
      S: TExcelWriter;
      ColIndexArray: TColIndexArray;
      P: PCellrec;
    begin
    if GetFileName(FileName, '*.XLS', GetString(dlExportTitle),
           GetString(dlExportLabel),
        fdOKButton+fdHelpButton) = cmCancel
    then
      Exit;
    if ExistFile(FileName) then
      begin
      PS := @FileName;
      if MessageBox(GetString(dlED_OverQuery), @PS,
          mfYesNoCancel+mfWarning) <> cmYes
      then
        Exit;
      end;

    S.Init(FileName, stCreate, 16384);
    if S.Status <> stOK then
      begin
      PS := @FileName;
      Msg(erCantCreateFile, @PS, mfError+mfOKButton);
      S.Done;
      Exit;
      end;

    kvGetMaxColRow(MaxCol, MaxRow);
    S.WriteBOF;
    for CurRow := 0 to MaxRow do
      begin
      kvFillColIndexArray(CurRow, ColIndexArray);
      for CurCol := 0 to MaxCol do
        begin
        if ColIndexArray[CurCol] < 0 then
          S.WriteBLANK(CurCol, CurRow, 0)
        else
          begin
          P := Cells^.At(ColIndexArray[CurCol]);
          if  (P^.Options and coFormula) <> 0 then
            S.WriteLABEL(P^.S, CurCol, CurRow, 0)
          else if (P.Options and coValue) = coValue then
            begin
            if  (P^.Options and $F0) = coBool then
              S.WriteBOOL(P^.Value <> 0, CurCol, CurRow, 0)
            else
              S.WriteNUMBER(P^.Value, CurCol, CurRow, 0);
            end
          else
            S.WriteLABEL(P^.S, CurCol, CurRow, 0);
          end;
        end;
      end;
    S.WriteEOF;
    S.Done;
    end { ExportToXls };

  procedure ExportToFile;
    var
      S, S1: String;
      C: array[0..255] of Integer;
      P: PCellrec;
      MaxLv, Maxr, I, J, K, L, L1: Integer;
      F: lText;
    begin
    if GetFileName(S, x_x, GetString(dlExportTitle),
           GetString(dlExportLabel),
        fdOKButton+fdHelpButton) = cmCancel
    then
      Exit;
    lAssignText(F, S);
    lSetTAttr(F, Archive);
    lRewriteText(F);
    MaxLv := 0;
    for I := 1 to Cells^.Count do
      begin
      P := Cells^.At(I-1);
      if MaxLv < P^.Row then
        MaxLv := P^.Row;
      end;
    for J := 0 to MaxLv do
      begin
      FillChar(C, SizeOf(C), 255);
      Maxr := 0;
      for I := 1 to Cells^.Count do
        begin
        P := Cells^.At(I-1);
        if J = P^.Row then
          C[P^.Col] := I-1;
        if Maxr < P^.Col then
          Maxr := P^.Col;
        end;
      for I := 0 to Maxr do
        begin
        K := ColWidth[I];
        if ShowSeparators then
          S := Strg(' ', K-1)+'│'
        else
          S := Strg(' ', 255);
        if C[I] >= 0 then
          begin
          P := Cells^.At(C[I]);
          S1 := MakeCellText(P);
          L1 := Length(S1);
          FormatCellText(S1, K, ShowSeparators, P^.Options, S);
          end
        else
          L1 := Length(S);
        if not ShowSeparators then
          while (I < Maxr) and (C[I+1] < 0) do
            begin
            Inc(I);
            Inc(K, ColWidth[I]);
            end;
        if K <= L1 then
          Write(F.T, System.Copy(S, 1, K))
        else
          begin
          if K > 255 then
            L := 255
          else
            L := K;
          Write(F.T, System.Copy(S, 1, L));
          repeat
            Dec(K, L);
            if K > 255 then
              L := 255
            else
              L := K;
            if L <= 0 then
              Break;
            Write(F.T, Strg(' ', L));
          until K <= 0;
          end;
        end;
      Writeln(F.T);
      end;
    Close(F.T);
    end { ExportToFile };

  procedure SetMark;
    begin
    if Marking then
      Exit;
    Mark.X := Delta.X+Cur.X;
    Mark.Y := Delta.Y+Cur.Y;
    end;

  procedure CheckMark;
    begin
    Marking := ShiftState and $3 <> 0
    end;

  procedure EndMarking;
    begin
    Marking := False;
    SetMark;
    DrawView;
    ClearEvent(Event)
    end;

  procedure ChangeFormat;
    const
      J: Integer = 0;
      NewOptions: AWord = 0; {см. комментарий к ForRectangle}
    procedure SetFormat(Item: PCellrec);
      begin
      with Item^ do
        begin
        Options := (Options and 3) or NewOptions;
        Decimals := j;
        end;
      end;

    begin {ChangeFormat}
    ClearEvent(Event);
    P := Cells^.Get(Delta.X+Cur.X, Delta.Y+Cur.Y);
    if P = nil then
      begin
      FillChar(R, SizeOf(R), 0);
      R.Dec := '2'
      end
    else
      begin
      R.Display := (P^.Options shr 4) and 7;
      R.Justify := (P^.Options shr 2) and 3;
      Str(P^.Decimals, R.Dec);
      R.Protect := (P^.Options shr 7) and 1;
      end;
    if ExecResource(dlgSetCellFormat, R) = cmOK then
      begin
      X1 := Delta.X+Cur.X;
      X2 := Mark.X;
      Y1 := Delta.Y+Cur.Y;
      Y2 := Mark.Y;
      if X1 > X2 then
        begin
        Wrk := X1;
        X1 := X2;
        X2 := Wrk;
        end;
      if Y1 > Y2 then
        begin
        Wrk := Y1;
        Y1 := Y2;
        Y2 := Wrk;
        end;
      Val(R.Dec, J, i);
      NewOptions := ((R.Justify and 3) shl 2)
        or ((R.Display and 7) shl 4)
        or ((R.Protect and 1) shl 7);
      Cells^.ForRectangle(X1, Y1, X2, Y2, @SetFormat);
      end;
    Modified := True;
    DrawView;
    end { ChangeFormat };

  procedure CE;
    begin
    ClearEvent(Event)
    end;

  procedure ExpandCol(CurCol: Integer);
    begin
    if ColWidth[CurCol] < ScreenWidth then
      begin
      Inc(ColWidth[CurCol]);
      DrawView
      end;
    Modified := True;
    end;

  procedure ShrinkCol(CurCol: Integer);
    begin
    if ColWidth[CurCol] > 1 then
      begin
      Dec(ColWidth[CurCol]);
      DrawView
      end;
    Modified := True;
    end;

  var
    SaveCellWidth: Byte;
    CurCol: AInt;

  begin {TCalcView.HandleEvent}
  Inf := PCalcInput(CalcInput);
  if GetState(sfFocused) and (FocusEvent.What <> evNothing) then
    begin
    PutEvent(Event);
    Event := FocusEvent;
    FocusEvent.What := evNothing;
    end;
  TView.HandleEvent(Event);
  CurCol := Delta.X+Cur.X;
  case Event.What of
    evCommand:
      case Event.Command of
        cmGetName:
          PString(Event.InfoPtr)^:= 'Speadsheet - '+SName^;
        cmImportToFile:
          begin
          ExportToFile;
          ClearEvent(Event)
          end;
        cmImportFromCsv:
          begin
          ImportFromCsv;
          ClearEvent(Event);
          end;
        cmImportFromDbf:
          begin
          ImportFromDbf;
          ClearEvent(Event);
          end;
        cmExportToCsv:
          begin
          ExportToCsv;
          ClearEvent(Event);
          end;
        cmExportToDbf:
          begin
          ExportToDbf;
          ClearEvent(Event);
          end;
        cmExportToXls:
          begin
          ExportToXls;
          ClearEvent(Event);
          end;
        cmSaveSheetAs:
          begin
          SaveSheetAs;
          CE;
          Owner^.Redraw;
          end;
        cmSave:
          begin
          SaveSheet;
          CE;
          Owner^.Redraw;
          end;
        cmOpen:
          begin
          CE;
          K := GetFileName(FName, '*.WKZ', GetString(dlOpenFile),
              GetString(dlOpenFileName), fdOpenButton
              { + fdReplaceButton, hsLoadSheet, 0});
          if K = cmCancel then
            Exit;
          LoadSheet(FName);
          end;
        cmChangeWidth:
          begin
          SaveCellWidth := ColWidth[CurCol];
          while True do
            begin
            CE;
            GetKeyEvent(Event);
            if Event.What = evKeyDown then
              case Event.KeyCode of
                kbLeft:
                  ShrinkCol(CurCol);
                kbRight:
                  ExpandCol(CurCol);
                kbEnter:
                  Break;
                kbESC:
                  begin
                  ColWidth[CurCol] := SaveCellWidth;
                  DrawView;
                  Break;
                  end;
              end {case};
            Application^.Idle;
            end;
          CE;
          end;
        cmChangeFormat:
          begin
          ChangeFormat;
          CE;
          end;
        cmPaste:
          begin
          Paste;
          EndMarking
          end;
        cmCopy:
          begin
          Copy;
          EndMarking
          end;
        cmCut:
          begin
          Copy;
          Clear;
          EndMarking
          end;
        cmClear:
          begin
          Clear;
          EndMarking
          end;
        cmInsertLine:
          begin
          InsertLine;
          EndMarking
          end;
        cmInsertColumn:
          begin
          InsertCol;
          EndMarking
          end;
        cmDeleteLine:
          begin
          DeleteLine;
          EndMarking
          end;
        cmDeleteColumn:
          begin
          DeleteCol;
          EndMarking
          end;
        cmToggleShowMode:
          begin
          ShowSeparators := not ShowSeparators;
          DrawView;
          CE;
          end;
        cmRecalc:
          begin
          ReCalc(True);
          DrawView;
          CE
          end;
        cmGotoCell:
          begin
          if HistoryCount(hsGotoCell) = 0 then
            S := 'A1'
          else
            S := HistoryStr(hsGotoCell, 0);
          if ExecResource(dlgGotoCellNumber, S) = cmOK
          then
            GotoCell(S);
          ClearEvent(Event);
          Exit
          end;
        cmFindCell:
          begin
          if ExecResource(dlgFindCell, SearchData) = cmOK then
            begin
            SearchPos.X := -1;
            SearchPos.Y := 0;
            WasReplace := False;
            SearchCell;
            end;
          CE;
          end;
        cmSearchAgain:
          begin
          SearchCanceled := False;
          SearchPos.X := CurCol;
          SearchPos.Y := Delta.Y+Cur.Y;
          repeat
            SearchCell
          until not WasReplace or not ContSearch or SearchCanceled;
          CE;
          end;
        cmReplaceCell:
          begin
          L := ExecResource(dlgReplaceCell, ReplaceData);
          if  (L = cmYes) or (L = cmOK) then
            begin
            SearchData.S := ReplaceData.S;
            SearchData.TxtOptions := ReplaceData.TxtOptions;
            SearchData.CellOptions := ReplaceData.CellOptions;
            SearchCanceled := False;
            ContSearch := L = cmYes;
            SearchPos.X := -1;
            SearchPos.Y := 0;
            WasReplace := True;
            repeat
              SearchCell
            until not WasReplace or not ContSearch or SearchCanceled;
            end;
          CE;
          end;
        cmMainMenu:
          begin
          Message(DNApp.MenuBar, evCommand, cmMenu, nil);
          CE;
          end;
      end {case};
    evKeyDown:
      case Event.KeyCode of
        kbDoubleAlt, kbDoubleCtrl:
          CE;
        kbDel:
          begin
          Clear;
          EndMarking
          end;
        kbShiftLeft,
        kbLeft:
          if Cur.X > 0 then
            begin
            CheckMark;
            Dec(Cur.X);
            SetMark;
            DrawView;
            CE;
            Exit
            end
          else
            Exit;
        kbShiftRight,
        kbRight:
          if  (Cur.X < NumC) then
            begin
            CheckMark;
            if  (CurCol = HScroll^.Max) then
              begin
              if Cur.X > 0 then
                Dec(Cur.X);
              SetMark;
              Exit
              end;
            Inc(Cur.X);
            SetMark;
            DrawView;
            ClearEvent(Event);
            Exit
            end
          else if (CurCol = HScroll^.Max) then
            begin
            CheckMark;
            if Cur.X > 0 then
              Dec(Cur.X);
            SetMark;
            Exit
            end
          else
            Exit;
        kbShiftUp, kbUp:
          if Cur.Y > 0 then
            begin
            CheckMark;
            Dec(Cur.Y);
            SetMark;
            DrawView;
            ClearEvent(Event);
            Exit
            end
          else
            Exit;
        kbShiftHome, kbHome:
          begin
          CheckMark;
          Cur.Y := 0;
          Cur.X := 0;
          SetMark;
          HScroll^.SetValue(0);
          VScroll^.SetValue(0);
          DrawView;
          ClearEvent(Event);
          Exit
          end;
        kbShiftEnd, kbEnd:
          begin
          CheckMark;
          MaxX := 0;
          MaxY := 0;
          Cur.Y := 0;
          Cur.X := 0;
          for L := 1 to Cells^.Count do
            begin
            P := Cells^.At(L-1);
            if P^.Col > MaxX then
              MaxX := P^.Col;
            end;
          if Cells^.Count <> 0 then
            MaxY := P^.Row;
          if  (L+ColWidth[CurCol] < Size.X) and (MaxX > 0) then
            begin
            L := 7;
            while (L <= Size.X) and (MaxX >= 0) do
              begin
              K := ColWidth[CurCol];
              if MaxX >= 0 then
                begin
                Inc(Cur.X);
                Dec(MaxX)
                end;
              Inc(L, K);
              end;
            Dec(Cur.X);
            Inc(MaxX);
            end;
          VScroll^.SetValue(MaxY-Size.Y+3);
          Delta.Y := VScroll^.Value;
          Cur.Y := MaxY-Delta.Y;
          HScroll^.SetValue(MaxX);
          Delta.X := HScroll^.Value;
          SetMark;
          DrawView;
          ClearEvent(Event);
          Exit
          end;
        kbShiftDown, kbDown:
          if  (Cur.Y < Size.Y-3) then
            begin
            CheckMark;
            if  (Delta.Y+Cur.Y >= VScroll^.Max) then
              begin
              if Cur.Y > 0 then
                Dec(Cur.Y);
              SetMark;
              Exit
              end;
            Inc(Cur.Y);
            SetMark;
            DrawView;
            ClearEvent(Event);
            Exit
            end
          else if (Delta.Y+Cur.Y >= VScroll^.Max) then
            begin
            if Cur.Y > 0 then
              Dec(Cur.Y);
            SetMark;
            Exit
            end
          else
            Exit;
        kbEnter:
          begin
          Marking := False;
          SetMark; //DrawView;
          PWindow(Owner)^.SelectNext(False);
          ClearEvent(Event);
          end;
        kbTab:
          begin
          Marking := False;
          SetMark;
          DrawView;
          end;
        kbAltLeft:
          ShrinkCol(CurCol);
        kbAltRight:
          ExpandCol(CurCol);
        else {case}
          if Event.CharCode > #31 then
            begin
            Marking := False;
            SetMark;
            DrawView;
            PWindow(Owner)^.SelectNext(True);
            Event.InfoPtr := CalcInput;
            CalcInput^.PutEvent(Event);
            ClearEvent(Event)
            end;
      end {case};
    evBroadcast:
      case Event.Command of
        cmScrollBarChanged:
          if Event.InfoPtr = HScroll then
            begin
            CheckMark;
            Delta.X := HScroll^.Value;
            SetMark;
            DrawView;
            Exit
            end
          else if Event.InfoPtr = VScroll then
            begin
            CheckMark;
            Delta.Y := VScroll^.Value;
            SetMark;
            DrawView;
            Exit
            end
      end {case};
  end {case};
  Marking := False;
  end { TCalcView.HandleEvent };
{-DataCompBoy-}

function TCalcView.GetCellValue(const S: String): Boolean;
  var
    I: Integer;
    SR: TCellSearcRec;
  begin
  Result := False;
  // DelSpaces(S); UpStr(S);
  if GetCellCoord(S, SR.Col, SR.Row) then
    begin
    if not Cells^.Search(@SR, I) then
      begin
      Res := 0;
      Result := True;
      end
    else
      with PCellrec(Cells^.At(I))^ do
        if Options and 3 <> 0 then
          begin
          Res := Value;
          Result := True;
          end;
    end;
  end { TCalcView.GetCellValue };

{  Вычисление функций SUM и MUL. В скобках через запятую могут быть
диапазоны, ячейки и числа. Пробелы игнорируются. Наличие закрывающей
скобки должно быть проверено перед вызовом.
   Пример: =SUM(A 1:B7, C1 8, 1).
   Вычисленное число кладется в Calculat.Res}
function TCalcView.GetFuncValue(S: String): Boolean;
  procedure DoSum(PP: PCellrec);
    begin
    with PP^ do
      if  (Options and 3 <> 0) then
        Res := Res+PP^.Value
    end;

  procedure DoMul(PP: PCellrec);
    begin
    with PP^ do
      if  (Options and 3 <> 0) then
        Res := Res*PP^.Value
    end;

  var
    I: Integer;
    AFromX, AToX: Byte;
    AFromY, AToY: AInt;
    Op: Pointer;
    SR: TCellSearcRec;
    t0, t: Integer;
    S1: String;
    R: CReal;
  begin {TCalcView.GetFuncValue}
  Result := False;
  DelSpaces(S);
  UpStr(S);
  if  (System.Copy(S, 1, 4) = 'SUM(') then
    begin
    Op := @DoSum;
    Res := 0;
    end
  else if (System.Copy(S, 1, 4) = 'MUL(') then
    begin
    Op := @DoMul;
    Res := 1;
    end
  else
    Exit;
  t := 5;
  while t <= Length(S) do
    begin
    t0 := t;
    while not (S[t] in [',', ')']) do
      Inc(t);
    if t = t0 then
      Exit;
    S1 := System.Copy(S, t0, t-t0);
    I := Pos(':', S1);
    if I <> 0 then
      begin
      if not GetCellCoord(System.Copy(S1, 1, I-1), AFromX, AFromY) or
        not GetCellCoord(System.Copy(S1, I+1, MaxStringLength), AToX, AToY)
      then
        Exit;
      Cells^.ForRectangle(AFromX, AFromY, AToX, AToY, Op);
      end
    else
      begin
      if not GetCellCoord(S1, AFromX, AFromY) then
        Exit;
      Cells^.ForRectangle(AFromX, AFromY, AFromX, AFromY, Op);
      end;
    Inc(t); { пропустить запятую или скобку }
    end;
  Result := True;
  end { TCalcView.GetFuncValue };

procedure TCalcView.CalcError;
  var
    L: Integer;
    S: String;
    err: String;
    c: Word;
  begin
  S := GetString(Index)+' '+GetCellName(CurrentCalc.X, CurrentCalc.Y);
  if EvalueError then
    begin
    err := GetErrOp(L);
    if  (err = '') and (CalcSym <> #3) then
      err := ' '+CalcSym;
    S := S+^M^C+GetString(CalcErrMess)+err;
    EvalueError := False;
    end;
  c := MessageBox(S, @Self, {mfYesNoCancel}mfYesButton+mfError);
  end;

destructor TCalcView.Done;
  begin
  if Cells <> nil then
    Dispose(Cells, Done);
  Cells := nil;
  if Q <> nil then
    Dispose(Q);
  Q := nil;
  DisposeStr(SName);
  TView.Done;
  end;

function TCalcView.AskSave;
  var
    A: Word;
  begin
  AskSave := True;
  if not Modified then
    Exit;
  A := MessageBox(GetString(dlWorkSheet)+' '+Cut(SName^,
         40)+GetString(dlNotSaved),
      nil, mfWarning+mfYesNoCancel);
  if A <> cmCancel then
    Modified := False;
  if A = cmYes then
    SaveSheet
  else
    AskSave := A <> cmCancel;
  end;

{-DataCompBoy-}
procedure TCalcView.LoadSheet;
  var
    S: PStream;
    Bounds: TRect;
  begin
  GetBounds(Bounds);
  if Modified and not AskSave then
    Exit;
  if Cells <> nil then
    Dispose(Cells, Done);
  Cells := nil;
  if  (FName = '') or (FName = UntitledName) then
    begin
    DisposeStr(SName);
    SName := NewStr(UntitledName);
    if Owner <> nil then
      begin
      DisposeStr(PWindow(Owner)^.Title);
      PWindow(Owner)^.Title := NewStr(Cut(SName^,
             Bounds.B.X-Bounds.A.X-12));
      Owner^.Redraw;
      end;
    New(Cells, Init(10, 10));
    Exit
    end;
  {$IFDEF DPMI32}
  FName := lfGetLongFileName(lFExpand(FName));
  {$ELSE}
  FName := lFExpand(FName);
  {$ENDIF}
  S := New(PBufStream, Init(FName, stOpenRead, 2048));
  DisposeStr(SName);
  SName := NewStr(FName);
  if Owner <> nil then
    begin
    DisposeStr(PWindow(Owner)^.Title);
    PWindow(Owner)^.Title := NewStr(Cut(SName^, Bounds.B.X-Bounds.A.X-12));
    end;
  if S^.Status <> stOK then
    begin
    Dispose(S, Done);
    New(Cells, Init(10, 10));
    FillChar(ColWidth, SizeOf(ColWidth), DefaultColWidth);
    //   Owner^.Redraw;
    Exit
    end;
  S^.Read(ColWidth, SizeOf(ColWidth));
  { Cells := PCellCollection(S^.Get);}
  New(Cells, ShortLoad(S^));
  Dispose(S, Done);
  if Cells = nil then
    begin
    ErrMsg(erInvalidFileFormat);
    DisposeStr(SName);
    SName := NewStr(UntitledName);
    if Owner <> nil then
      begin
      DisposeStr(PWindow(Owner)^.Title);
      PWindow(Owner)^.Title := NewStr(Cut(SName^,
             Bounds.B.X-Bounds.A.X-12));
      end;
    New(Cells, Init(10, 10));
    FillChar(ColWidth, SizeOf(ColWidth), DefaultColWidth);
    end;
  Modified := False;
  ReCalc(True);
  end { TCalcView.LoadSheet };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TCalcView.SaveSheetAs;
  var
    S: PStream;
    FName: String;
    W: Word;
    PP: Pointer;
    Bounds: TRect;
  begin
  GetBounds(Bounds);
  PP := @FName;
  if GetFileName(FName, '*.WKZ', GetString(dlSaveFileAs),
      GetString(dlSaveFileAs), fdOKButton {, hsSaveSheetAs, 0})
     = cmCancel
  then
    Exit;
  {$IFDEF DPMI32}
  FName := lfGetLongFileName(lFExpand(FName));
  {$ELSE}
  FName := lFExpand(FName);
  {$ENDIF}
  S := New(PDosStream, Init(FName, stOpen));
  W := S^.Status;
  Dispose(S, Done);
  if W = 0 then
    begin
    if Msg(dlFileExist, @PP, mfYesButton+mfNoButton+mfWarning) <> cmYes
    then
      Exit;
    end;
  DisposeStr(SName);
  SName := NewStr(FName);
  if Owner <> nil then
    begin
    DisposeStr(PWindow(Owner)^.Title);
    PWindow(Owner)^.Title := NewStr(Cut(SName^, Bounds.B.X-Bounds.A.X-12));
    Owner^.Redraw;
    end;
  {PInfoView(CellInfo)^.SetInfo(' WORK ', Owner^.GetColor(12));}
  S := New(PBufStream, Init(FName, stCreate, 2048));
  S^.Write(ColWidth, SizeOf(ColWidth));
  { S^.Put(Cells);}
  Cells^.ShortStore(S^);
  if S^.Status <> stOK then
    begin
    Msg(dlCanNotWrite, @PP, mfError+mfOKButton);
    end;
  Modified := False;
  Dispose(S, Done);
  FileChanged(FName);
  end { TCalcView.SaveSheetAs };
{-DataCompBoy-}

procedure TCalcView.SaveSheet;
  var
    S: PStream;
  begin
  if SName^ = UntitledName then
    begin
    SaveSheetAs;
    Exit
    end;
  {PInfoView(CellInfo)^.SetInfo(' WORK ', Owner^.GetColor(12));}
  S := New(PBufStream, Init(SName^, stCreate, 2048));
  S^.Write(ColWidth, SizeOf(ColWidth));
  {S^.Put(Cells);}
  Cells^.ShortStore(S^);
  if S^.Status <> stOK then
    Msg(dlCanNotWrite, SName, mfError+mfOKButton)
  else
    Modified := False;
  Dispose(S, Done);
  FileChanged(SName^);
  end;

procedure TCalcView.Copy;
  var
    X1, Y1, X2, Y2: Integer;
    Wrk: Integer;

  procedure ToClip(P: PCellrec);
    begin
    with CellClipboard^.ReplaceItem(P^.Col, P^.Row, P^.S)^ do
      begin
      Options := P^.Options;
      Value := P^.Value;
      Decimals := P^.Decimals;
      end;
    end;

  begin
  X1 := Delta.X+Cur.X;
  X2 := Mark.X;
  Y1 := Delta.Y+Cur.Y;
  Y2 := Mark.Y;
  if X1 > X2 then
    begin
    Wrk := X1;
    X1 := X2;
    X2 := Wrk;
    end;
  if Y1 > Y2 then
    begin
    Wrk := Y1;
    Y1 := Y2;
    Y2 := Wrk;
    end;
  if CellClipboard <> nil then
    Dispose(CellClipboard, Done);
  CellClipboard := nil;
  New(CellClipboard, Init(10, 10));
  Cells^.ForRectangle(X1, Y1, X2, Y2, @ToClip);
  ClipRect.A.X := X1;
  ClipRect.A.Y := Y1;
  ClipRect.B.X := X2;
  ClipRect.B.Y := Y2;
  end { TCalcView.Copy };

{В формуле S ссылки на ячейки (>=LX,>=LY) сдвигать на (DX,DY) }
function ReformFormula(const S: String; LX, LY, DX, DY: Integer): String;
  const
    Signs = [';', '[', ']', '{', '}', #39, ':', '"', '.', '<',
    '>', ',', '/', '?', '\', '-', '=', '|', '_', '(',
    ')', '*', '&', '^', '%', '$', '!', '~', '+', ' '];
  var
    S1, S2: String;
    X, X1: Byte;
    I, l: Integer;
    Y, Y1: AInt;
  label EndRowName;
  begin
  Result := '=';
  I := 2;
  while (I <= Length(S)) do
    begin
    S1 := '';
    S2 := '';
    while (I <= Length(S)) and (S[I] in Signs) do
      begin
      Result := Result+S[I];
      Inc(I)
      end;
    while (I <= Length(S)) and not (S[I] in Signs) do
      begin
      S1 := S1+S[I];
      Inc(I)
      end;
    if GetCellCoord(S1, X, Y) then
      if  (X >= LX) and (Y >= LY) then
        begin
        if S1[1] = '@' then
          S2 := '@'+GetColName(X)
        else if (X+DX < 0) or (X+DX > 255) then
          S2 := '?'
        else
          S2 := GetColName(X+DX);
        { сейчас S2 - имя колонки}

        for l := 2 to Length(S1)-1 do
          if S1[l] = '@' then
            begin
            S1 := '@'+GetRowName(Y);
            goto EndRowName;
            end;
        if  (Y+DY < 0) or (Y+DY > MaxCellY) then
          S1 := '?'
        else
          S1 := GetRowName(Y+DY);
        end;
EndRowName: { сейчас S1 - имя строки }

    Result := Result+S2+S1;
    end;
  end { ReformFormula };

procedure TCalcView.Paste;
  var
    I, J: Integer;

  procedure RewriteFormulaClip(P: PCellrec; DX, DY: Integer);
    var
      S: String;
    begin
    if  (P^.Options and coFormula) <> 0 then
      S := ReformFormula(P^.S, 0, 0, DX, DY)
    else
      S := P^.S;
    with Cells^.ReplaceItem(P^.Col+DX, P^.Row+DY, S)^ do
      begin
      Options := P^.Options;
      Value := P^.Value;
      Decimals := P^.Decimals;
      end;
    end;

  begin
  if CellClipboard = nil then
    Exit;
  Mark.X := Delta.X+Cur.X+ClipRect.B.X-ClipRect.A.X;
  Mark.Y := Delta.Y+Cur.Y+ClipRect.B.Y-ClipRect.A.Y;
  Clear;
  for I := 1 to CellClipboard^.Count do
    RewriteFormulaClip(CellClipboard^.At(I-1),
      -ClipRect.A.X+Delta.X+Cur.X,
      -ClipRect.A.Y+Delta.Y+Cur.Y);
  ReCalc(False);
  Modified := True;
  end { TCalcView.Paste };

procedure TCalcView.Clear;
  var
    X1, X2, Y1, Y2, K, L: Integer;
    Wrk: Integer;
  begin
  X1 := Delta.X+Cur.X;
  X2 := Mark.X;
  Y1 := Delta.Y+Cur.Y;
  Y2 := Mark.Y;
  if X1 > X2 then
    begin
    Wrk := X1;
    X1 := X2;
    X2 := Wrk;
    end;
  if Y1 > Y2 then
    begin
    Wrk := Y1;
    Y1 := Y2;
    Y2 := Wrk;
    end;
  for K := X1 to X2 do
    for L := Y1 to Y2 do
      Cells^.DelItem(K, L);
  ReCalc(False);
  Modified := True;
  end { TCalcView.Clear };

procedure TCalcView.RewriteFormula(var P: PCellrec;
     LX, LY, DX, DY: Integer);
  var
    S: String;
    O, D: Integer;
    V: CReal;
  begin
  if  (P^.Options and coFormula) = 0 then
    Exit;
  S := ReformFormula(P^.S, LX, LY, DX, DY);
  O := P^.Options;
  D := P^.Decimals;
  V := P^.Value;
  P := Cells^.ReplaceItem(P^.Col, P^.Row, S);
  with P^ do
    begin
    Options := O;
    Decimals := D;
    Value := V
    end;
  end;

procedure TCalcView.InsertLine;
  var
    I, L: Integer;
    P: PCellrec;
  begin
  L := Delta.Y+Cur.Y;
  for I := 1 to Cells^.Count do
    begin
    P := Cells^.At(I-1);
    if P^.Row >= VScroll^.Max then
      Cells^.AtFree(I-1)
    else
      begin
      if P^.Row >= L then
        Inc(P^.Row);
      RewriteFormula(P, 0, L, 0, 1);
      end;
    end;
  ReCalc(False);
  Modified := True;
  end;

procedure TCalcView.DeleteLine;
  var
    I, L: Integer;
    P: PCellrec;
  begin
  L := Delta.Y+Cur.Y+1;
  for I := 1 to Cells^.Count do
    begin
    P := Cells^.At(I-1);
    RewriteFormula(P, 0, L, 0, -1);
    if P^.Row = L-1 then
      begin
      Cells^.FreeItem(P);
      Cells^.AtPut(I-1, nil)
      end
    else if P^.Row >= L then
      Dec(P^.Row);
    end;
  Cells^.Pack;
  ReCalc(False);
  Modified := True;
  end;

procedure TCalcView.InsertCol;
  var
    I, J, L: Integer;
    P: PCellrec;
  begin
  L := Delta.X+Cur.X;
  for I := 1 to Cells^.Count do
    begin
    P := Cells^.At(I-1);
    if P^.Col = HScroll^.Max then
      Cells^.AtFree(I-1)
    else
      begin
      if P^.Col >= L then
        Inc(P^.Col);
      RewriteFormula(P, L, 0, 1, 0);
      end;
    end;
  Move(ColWidth[L], ColWidth[L+1],
    SizeOf(ColWidth[0])*(High(ColWidth)-L));
  ColWidth[L] := DefaultColWidth;
  ReCalc(False);
  Modified := True;
  end { TCalcView.InsertCol };

procedure TCalcView.DeleteCol;
  var
    I, J, L: Integer;
    P: PCellrec;
  begin
  L := Delta.X+Cur.X+1;
  for I := 1 to Cells^.Count do
    begin
    P := Cells^.At(I-1);
    if P^.Col = L-1 then
      begin
      Cells^.FreeItem(P);
      Cells^.AtPut(I-1, nil)
      end
    else
      begin
      if P^.Col >= L then
        Dec(P^.Col);
      RewriteFormula(P, L, 0, -1, 0);
      end;
    end;
  Move(ColWidth[L], ColWidth[L-1],
    SizeOf(ColWidth[0])*(High(ColWidth)-L+1));
  ColWidth[High(ColWidth)] := DefaultColWidth;
  Cells^.Pack;
  ReCalc(False);
  Modified := True;
  end { TCalcView.DeleteCol };

{ вычисление формулы; s[1] = '=' }
function TCalcView.CalcEval(const s: String; var Value: CReal): Boolean;
  var
    R: CReal;
  begin
  R := Evalue(System.Copy(s, 2, MaxStringLength), @self);
  if EvalueError then
    begin
    CalcError(erInvalidFormula);
    Value := 0;
    CalcEval := False;
    end
  else
    begin
    Value := R;
    CalcEval := True;
    end;
  end;

procedure TCalcView.ReCalc(Full: Boolean);
  var
    StartRecalc: Integer;
    ierr: Integer;
  begin
  if not Cells^.TSort(StartRecalc) then
    begin
    with PCellrec(Cells^.At(StartRecalc-1)) do
      begin
      CurrentCalc.X := Col;
      CurrentCalc.Y := Row;
      end;
    CalcError(erRecurseTooDeep);
    Exit;
    end;
  while StartRecalc <> 0 do
    begin
    with PCellrec(Cells^.At(StartRecalc-1))^ do
      begin
      CurrentCalc.X := Col;
      CurrentCalc.Y := Row;
      if  (Options and coFormula) <> 0 then
        begin
        if not CalcEval(S, Value) then
          Exit;
        end
      else if Full and ((Options and coValue) <> 0) then
        if S[1] = '=' then
          begin
          {После некоторых старых версий DN бывает флаг coValue
          без флага coFormula для формул, включающих только константные
          операнды. То, что TSort не воспринимал их, как формулы, не
          страшно, так как от других ячеек они не зависят. }
          Value := Evalue(System.Copy(S, 2, 255), @self);
          Options := Options or coValue;
          end
        else
          begin
          Val(S, Value, ierr);
          if ierr <> 0 then
            Exit;
          end;
      StartRecalc := NextC;
      end;
    end;
  end { TCalcView.ReCalc };

procedure TCalcView.GotoCell;
  var
    X: Byte;
    Y: AInt;
    L: Integer;
  begin
  if not GetCellCoord(Cell, X, Y) then
    begin
    ErrMsg(erGotoInvalidNumber);
    Exit;
    end;
  Marking := False;
  BlockDraw := True;
  Delta.X := X;
  Delta.Y := Y-(Size.Y-2) div 2;
  VScroll^.SetValue(Delta.Y);
  Delta.Y := VScroll^.Value;
  Cur.X := 0;
  Cur.Y := Y-Delta.Y;
  L := 0;
  repeat
    Inc(L, ColWidth[Cur.X]);
    if Delta.X > 0 then
      begin
      Inc(Cur.X);
      Dec(Delta.X)
      end;
  until (Delta.X = 0) or (L > Size.X);
  if L > Size.X div 2 then
    begin
    Dec(Cur.X);
    Inc(Delta.X)
    end;
  HScroll^.SetValue(Delta.X);
  BlockDraw := False;
  Mark.X := Cur.X+Delta.X;
  Mark.Y := Cur.Y+Delta.Y;
  DrawView;
  end { TCalcView.GotoCell };

procedure TCalcView.SearchCell;
  const
    Signs = [#0..#128]-['A'..'Z', 'a'..'z', '0'..'9', '_'];
  var
    P: PCellrec;
    I, J: Integer;
    V: CReal;
    K, L: Byte;
    S, S1: String;

  function Found: Boolean;
    begin
    Found := True;
    SearchPos.Y := P^.Row;
    SearchPos.X := P^.Col;
    GotoCell(GetCellName(P^.Col, P^.Row));
    if WasReplace then
      begin
      if  (SearchData.TxtOptions and 4 <> 0) then
        if  (SearchData.CellOptions = 0) then
          begin
          S := P^.S;
          Delete(S, k+l, Length(S1));
          Insert(ReplaceData.S1, S, k+l);
          Cells^.ReplaceItem(P^.Col, P^.Row, S);
          DrawView;
          Exit
          end
        else
          begin
          if  (P^.Options and coFormula) <> 0 then
            begin
            Found := False;
            Exit
            end;
          Val(ReplaceData.S1, V, j);
          Cells^.ReplaceItem(P^.Col, P^.Row, ReplaceData.S1)^.Value := V;
          DrawView;
          Exit
          end;
      end;
    end { Found: };

  procedure SearchSign;
    begin
    while (k <= Length(S)) and not (S[k] in Signs) do
      Inc(k);
    Inc(l, k);
    S := System.Copy(S, k, MaxStringLength);
    end;

  begin { TCalcView.SearchCell }
  if SearchData.S = '' then
    Exit;
  Val(SearchData.S, V, I);
  if SearchData.TxtOptions and 1 = 0 then
    S1 := UpStrg(SearchData.S)
  else
    S1 := SearchData.S;
  for I := 1 to Cells^.Count do
    begin
    P := Cells^.At(I-1);
    if  (P^.Row >= SearchPos.Y) and (P^.Col > SearchPos.X) then
      if  (SearchData.CellOptions = 0) then
        begin
        if SearchData.TxtOptions and 1 = 0 then
          S := UpStrg(P^.S)
        else
          S := P^.S;
        K := Pos(S1, S);
        L := 0;
        if SearchData.TxtOptions and 2 = 0 then
          begin
          if K > 0 then
            begin
            if Found then
              Exit
            end
          end
        else
          repeat
            K := Pos(S1, S);
            L := 0;
            if K = 0 then
            else if (K = 1) or (Length(S) = Length(S1)) then
              begin
              if  (Length(S) = Length(S1)) or (Length(S) <> Length(S1))
                and (S[Length(S1)+1] in Signs)
              then
                begin
                if Found then
                  Exit
                end
              else
                SearchSign;
              end
            else if (K = Length(S)-Length(S1)+1) then
              begin
              if  (S[K-1] in Signs) then
                begin
                if Found then
                  Exit
                end
              else
                SearchSign;
              end
            else if (S[K-1] in Signs) and (S[Length(S1)+K] in Signs)
            then
              begin
              if Found then
                Exit
              end
            else
              SearchSign;
          until (K = 0) or (Length(S) < Length(S1));
        end
      else if (P^.Options and 3 <> 0) and (V = P^.Value) then
        begin
        if Found then
          Exit
        end;
    end;
  if not ContSearch and not WasReplace then
    ErrMsg(erTextNotFound);
  SearchCanceled := True;
  end { TCalcView.SearchCell };

{------------------------------------------------------------------------------}
{ Write dBase Dbf }

procedure TDbfFieldCollection.FreeItem(Item: Pointer); {KV}
  begin
  if Item <> nil then
    Dispose(PDBFField(Item));
  end;

constructor TDbaseWriter.Init(FileName: FNameStr; Mode: Word;
     Size: SW_Word); {KV}
  begin
  inherited Init(FileName, Mode, Size);
  CurRecord := 0;
  EofFlag := False;
  New(Fields, Init(10, 10));
  FillChar(Header, SizeOf(Header), 0);
  Header.DBFIdent := #3;
  if  (Mode = stOpen) or (Mode = stOpenRead) then
    ReadFile;
  end;

destructor TDbaseWriter.Done; {KV}
  begin
  if Fields <> nil then
    Dispose(Fields, Done);
  Fields := nil;
  inherited Done;
  end;

procedure TDbaseWriter.AddField(const NameField: String; TypeField: Char;
    LenField: Integer; DecField: Integer); {KV}
  var
    P: PDBFField;
  begin
  New(P);
  FillChar(P^, SizeOf(TDBFField), 0);
  StrPCopy(P^.FieldName, System.Copy(NameField, 1, 10));
  P^.FieldType := TypeField;
  if TypeField = 'C' then
    P^.FLength.FieldLength := LenField
  else
    begin
    P^.FLength.NumericLength := LenField;
    P^.FLength.Decimals := DecField;
    end;
  Fields^.Insert(P);
  end;

procedure TDbaseWriter.AddRecord; {KV}
  var
    Buf: PChar;
  begin
  if Status <> stOK then
    Exit;
  with Header do
    begin
    inherited Seek(DataOffset+RecSize*LastRecord);
    GetMem(Buf, RecSize);
    FillChar(Buf^, RecSize, ' ');
    inherited Write(Buf^, RecSize);
    FreeMem(Buf, RecSize);
    if Status = stOK then
      begin
      Inc(LastRecord);
      inherited Seek(0);
      inherited Write(Header, SizeOf(Header));
      end;
    CurRecord := LastRecord;
    EofFlag := False;
    end;
  end { TDbaseWriter.AddRecord };

procedure TDbaseWriter.CreateFile; {KV}
  var
    i: Integer;
  begin
  Header.RecSize := 1;
  for i := 0 to Fields^.Count-1 do
    begin
    with PDBFField(Fields^.At(i))^ do
      begin
      if FieldType = 'C' then
        Inc(Header.RecSize, FLength.FieldLength)
      else
        Inc(Header.RecSize, FLength.NumericLength);
      end;
    end;
  Header.DataOffset := SizeOf(THeaderDBF)+(Fields^.Count)
      *SizeOf(TDBFField)+1;
  inherited Seek(0);
  inherited Write(Header, SizeOf(Header));
  for i := 0 to Fields^.Count-1 do
    begin
    inherited Write(PDBFField(Fields^.At(i))^, SizeOf(TDBFField));
    end;
  i := $1A0D;
  inherited Write(i, 2);
  end { TDbaseWriter.CreateFile };

procedure TDbaseWriter.DeleteRecord; {KV}
  var
    Ch: Char;
  begin
  if  (CurRecord < 1) or (CurRecord > Header.LastRecord) then
    Exit;
  inherited Seek(FieldOffsetInFile(1)-1);
  Ch := #$2A;
  inherited Write(Ch, 1);
  end;

function TDbaseWriter.FCount: Integer;
  begin
  FCount := Fields^.Count;
  end;

function TDbaseWriter.FieldAsString(FieldIndex: Integer): String; {KV}
  var
    Buf: array[0..2047] of Char;
    i: Integer;
  begin
  inherited Seek(FieldOffsetInFile(FieldIndex));
  FillChar(Buf, SizeOf(Buf), ' ');
  inherited Read(Buf, FieldLen(FieldIndex));
  i := FieldLen(FieldIndex)-1;
  while (i >= 0) and (Buf[i] = ' ') do
    Dec(i);
  Buf[i+1] := #0;
  FieldAsString := StrPas(Buf);
  end;

function TDbaseWriter.FieldAsInteger(FieldIndex: Integer): Integer;
  {KV}
  var
    S: String;
    Value: Integer;
    i: Integer;
  begin
  S := FieldAsString(FieldIndex);
  if S = '' then
    Value := 0
  else
    begin
    Val(S, Value, i);
    if i <> 0 then
      Value := 0;
    end;
  FieldAsInteger := Value;
  end;

function TDbaseWriter.FieldAsFloat(FieldIndex: Integer): Double; {KV}
  var
    S: String;
    Value: Double;
    i: Integer;
  begin
  S := FieldAsString(FieldIndex);
  if S = '' then
    Value := 0.0
  else
    begin
    Val(S, Value, i);
    if i <> 0 then
      Value := 0.0;
    end;
  FieldAsFloat := Value;
  end;

procedure TDbaseWriter.FieldPutString(FieldIndex: Integer;
     FieldValue: String); {KV}
  var
    P: PDBFField;
    Buf: array[0..2047] of Char;
    L: AWord;
    StrLen: Integer;
  begin
  if  (FieldIndex < 1) or (FieldIndex > Fields^.Count) then
    Exit;
  P := PDBFField(Fields^.At(FieldIndex-1));
  FillChar(Buf, SizeOf(Buf), ' ');
  L := FieldLen(FieldIndex);
  if L > 2047 then
    L := 2047;
  StrLen := Length(FieldValue);
  if StrLen > L then
    begin
    StrLen := L;
    FieldValue := System.Copy(FieldValue, 1, StrLen);
    end;
  StrPCopy(Buf, FieldValue); // Можно было использовать Move,
  // но при работе с длинными строками
  // так безопаснее
  Buf[StrLen] := ' '; // Для удаления #0, который вставляет StrPCopy
  inherited Seek(FieldOffsetInFile(FieldIndex));
  inherited Write(Buf, L);
  end { TDbaseWriter.FieldPutString };

procedure TDbaseWriter.FieldPutInteger(FieldIndex: Integer;
     FieldValue: Integer); {KV}
  var
    S: String;
    L: AWord;
    D: Byte;
    R: AWord;
  begin
  if  (FieldIndex < 1) or (FieldIndex > Fields^.Count) then
    Exit;
  L := FieldLen(FieldIndex);
  D := FieldDec(FieldIndex);
  if D > 0 then
    R := L-D-1
  else
    R := L;
  Str(FieldValue: L, S);
  if D > 0 then
    begin
    S := S+'.';
    Inc(R);
    while R < L do
      begin
      S := S+'0';
      Inc(R);
      end;
    end;
  FieldPutString(FieldIndex, S);
  end { TDbaseWriter.FieldPutInteger };

procedure TDbaseWriter.FieldPutFloat(FieldIndex: Integer;
     FieldValue: Double); {KV}
  var
    S: String;
  begin
  if  (FieldIndex < 1) or (FieldIndex > Fields^.Count) then
    Exit;
  Str(FieldValue: FieldLen(FieldIndex): FieldDec(FieldIndex), S);
  FieldPutString(FieldIndex, S);
  end;

function TDbaseWriter.FieldName(FieldIndex: Integer): String; {KV}
  begin
  FieldName := StrPas(PDBFField(Fields^.At(FieldIndex-1))^.FieldName);
  end;

function TDbaseWriter.FieldLen(FieldIndex: Integer): AWord; {KV}
  begin
  with PDBFField(Fields^.At(FieldIndex-1))^ do
    begin
    if FieldType = 'C' then
      FieldLen := FLength.FieldLength
    else
      FieldLen := FLength.NumericLength;
    end;
  end;

function TDbaseWriter.FieldDec(FieldIndex: Integer): Byte; {KV}
  begin
  with PDBFField(Fields^.At(FieldIndex-1))^ do
    begin
    if FieldType = 'C' then
      FieldDec := 0
    else
      FieldDec := FLength.Decimals;
    end;
  end;

function TDbaseWriter.FieldPos(NameField: String): Integer; {KV}
  var
    FieldIndex, i: Integer;
  begin
  UpStr(NameField);
  FieldIndex := 0;
  for i := 0 to Fields^.Count-1 do
    begin
    if UpStrg(StrPas(PDBFField(Fields^.At(i))^.FieldName)) = NameField
    then
      begin
      FieldIndex := i+1;
      Break;
      end;
    end;
  FieldPos := FieldIndex;
  end;

function TDbaseWriter.FieldType(FieldIndex: Integer): Char; {KV}
  begin
  FieldType := PDBFField(Fields^.At(FieldIndex-1))^.FieldType;
  end;

function TDbaseWriter.FieldOffsetInBuffer(FieldIndex: Integer): Integer;
  {KV}
  var
    Offset: Integer;
    i: Integer;
  begin
  Offset := 1;
  for i := 0 to FieldIndex-2 do
    begin
    with PDBFField(Fields^.At(i))^ do
      begin
      if FieldType = 'C' then
        Inc(Offset, FLength.FieldLength)
      else
        Inc(Offset, FLength.NumericLength);
      end;
    end;
  FieldOffsetInBuffer := Offset;
  end;

function TDbaseWriter.FieldOffsetInFile(FieldIndex: Integer): LongInt;
  {KV}
  begin
  FieldOffsetInFile := Header.DataOffset+(CurRecord-1)*Header.RecSize+
    FieldOffsetInBuffer(FieldIndex);
  end;

procedure TDbaseWriter.First; {KV}
  begin
  CurRecord := 1;
  EofFlag := (Header.LastRecord = 0);
  end;

procedure TDbaseWriter.DBGoTo(NewRecord: Integer); {KV}
  begin
  CurRecord := NewRecord;
  EofFlag := (Header.LastRecord = 0) or (Header.LastRecord < CurRecord);
  end;

procedure TDbaseWriter.Next; {KV}
  begin
  if CurRecord < Header.LastRecord then
    begin
    Inc(CurRecord);
    EofFlag := False;
    end
  else
    EofFlag := True;
  end;

procedure TDbaseWriter.ReadFile; {KV}
  var
    R: TDBFField;
    P: PDBFField;
  begin
  inherited Seek(0);
  FillChar(Header, SizeOf(Header), 0);
  Fields^.FreeAll;
  inherited Read(Header, SizeOf(Header));
  if Status <> stOK then
    Exit;
  inherited Seek(SizeOf(Header));
  repeat
    inherited Read(R, 1);
    if R.FieldName[0] <> #$0D then
      begin
      inherited Read(R.FieldName[1], SizeOf(R)-1);
      New(P);
      Move(R, P^, SizeOf(R));
      Fields^.Insert(P);
      end;
  until (R.FieldName[0] = #$0D) or (Status <> stOK);
  EofFlag := (Header.LastRecord = 0);
  CurRecord := 1;
  end { TDbaseWriter.ReadFile };

procedure TDbaseWriter.RecallRecord; {KV}
  var
    Ch: Char;
  begin
  if  (CurRecord < 1) or (CurRecord > Header.LastRecord) then
    Exit;
  inherited Seek(FieldOffsetInFile(1)-1);
  Ch := #$20;
  inherited Write(Ch, 1);
  end;

procedure TDbaseWriter.WriteEndOfFile; {KV}
  var
    Ch: Char;
  begin
  Ch := #$1A;
  inherited Seek(Header.DataOffset+Header.LastRecord*Header.RecSize);
  inherited Write(Ch, 1);
  end;

{------------------------------------------------------------------------------}
{ Write Excel Xls }

type
  TXlsBOF = packed record
    RecNo: AWord; // Record number
    RecSize: AWord; // Record size
    Vers: AWord; // Version number (0500 for BIFF5 and BIFF7)
    DT: AWord; // Substream type: 0005h = Workbook globals
    //                 0006h = Visual Basic module
    //                 0010h = Worksheet or dialog sheet
    //                 0020h = Chart
    //                 0040h = Microsoft Excel 4.0 macro sheet
    //                 0100h = Workspace file
    rupBuild: AWord; // Build identifier (internal use only)
    rupYear: AWord; // Build year (internal use only)
    end;

  TXlsEOF = packed record
    RecNo: AWord; // Record number
    Res1: AWord;
    end;

  // LABEL: Cell Value, String Constant (204h)
  // A LABEL record describes a cell that contains a string constant.
  // The rw field contains the 0-based row number.
  // The col field contains the 0-based column number.
  // The string length is contained in the cch field and must be in the range of
  // 0000h..00FFh (0..255). The string itself is contained in the rgch field.
  TXlsLABEL = packed record
    RecNo: AWord; // Record number
    RecSize: AWord; // Record size
    Rw: AWord; // Cell Row
    Col: AWord; // Cell Col
    ixfe: AWord; // Index to the XF record
    cch: AWord; // Length of the string
    (* rgch: var *) // The string
    end;

  // BLANK: Cell Value, Blank Cell (201h)
  // A BLANK record describes an empty cell.
  // The rw field contains the 0-based row number.
  // The col field contains the 0-based column number.
  TXlsBLANK = packed record
    RecNo: AWord; // Record number
    RecSize: AWord; // Record size
    Rw: AWord; // Cell Row
    Col: AWord; // Cell Col
    ixfe: AWord; // Index to the XF record
    end;

  // NUMBER: Cell Value, Floating-Point Number (203h)
  // A NUMBER record describes a cell containing a constant floating-point number.
  // The rw field contains the 0-based row number.
  // The col field contains the 0-based column number.
  // The number is contained in the num field in 8-byte IEEE floating-point format.
  TXlsNUMBER = packed record
    RecNo: AWord; // Record number
    RecSize: AWord; // Record size
    Rw: AWord; // Cell Row
    Col: AWord; // Cell Col
    ixfe: AWord; // Index to the XF record
    Num: Double; // Floating-point number value
    end;

  // BOOLERR: Cell Value, Boolean or Error (205h)
  // A BOOLERR record describes a cell that contains a constant Boolean or error value.
  // The rw field contains the 0-based row number.
  // The col field contains the 0-based column number.
  TXlsBOOLERR = packed record
    RecNo: AWord; // Record number
    RecSize: AWord; // Record size
    Rw: AWord; // Cell Row
    Col: AWord; // Cell Col
    ixfe: AWord; // Index to the XF record
    bBoolErr: Byte; // Boolean value or error value
    fError: Byte; // Boolean/error flag
    end;

const
  CXlsBOF = $809;
  CXlsEOF = $0A;
  CXlsLABEL = $204;
  CXlsBLANK = $201;
  CXlsNUMBER = $203;
  CXlsBOOLERR = $205;

procedure TExcelWriter.WriteBLANK(const Col, Row, XF: AWord); {KV}
  var
    CDefXlsBLANK: TXlsBLANK;
  begin
  CDefXlsBLANK.RecNo := CXlsBLANK;
  CDefXlsBLANK.RecSize := SizeOf(TXlsBLANK)-2*SizeOf(AWord);
  CDefXlsBLANK.Rw := Row;
  CDefXlsBLANK.Col := Col;
  CDefXlsBLANK.ixfe := XF;
  inherited Write(CDefXlsBLANK, SizeOf(CDefXlsBLANK));
  end;

procedure TExcelWriter.WriteBOF; {KV}
  const
    CDefXlsBOF: TXlsBOF =
      (RecNo: CXlsBOF;
      RecSize: SizeOf(TXlsBOF)-2*SizeOf(AWord);
      Vers: 0;
      DT: $10;
      rupBuild: 0;
      rupYear: 0);
  begin
  inherited Write(CDefXlsBOF, SizeOf(CDefXlsBOF));
  end;

procedure TExcelWriter.WriteBOOL(const Data: Boolean;
     const Col, Row, XF: AWord); {KV}
  const
    Values: array[Boolean] of Byte = (0, 1);
  var
    CDefXlsBOOLERR: TXlsBOOLERR;
  begin
  CDefXlsBOOLERR.RecNo := CXlsBOOLERR;
  CDefXlsBOOLERR.RecSize := SizeOf(TXlsBOOLERR)-2*SizeOf(AWord);
  CDefXlsBOOLERR.Rw := Row;
  CDefXlsBOOLERR.Col := Col;
  CDefXlsBOOLERR.ixfe := XF;
  CDefXlsBOOLERR.bBoolErr := Values[Data];
  CDefXlsBOOLERR.fError := 0;
  inherited Write(CDefXlsBOOLERR, SizeOf(CDefXlsBOOLERR));
  end;

procedure TExcelWriter.WriteEOF;
  const
    CDefXlsEOF: TXlsEOF =
      (RecNo: CXlsEOF;
      Res1: 0);
  begin
  inherited Write(CDefXlsEOF, SizeOf(CDefXlsEOF));
  end;

procedure TExcelWriter.WriteERROR(const Data: Byte;
     const Col, Row, XF: AWord); {KV}
  var
    CDefXlsBOOLERR: TXlsBOOLERR;
  begin
  CDefXlsBOOLERR.RecNo := CXlsBOOLERR;
  CDefXlsBOOLERR.RecSize := SizeOf(TXlsBOOLERR)-2*SizeOf(AWord);
  CDefXlsBOOLERR.Rw := Row;
  CDefXlsBOOLERR.Col := Col;
  CDefXlsBOOLERR.ixfe := XF;
  CDefXlsBOOLERR.bBoolErr := Data;
  CDefXlsBOOLERR.fError := 1;
  inherited Write(CDefXlsBOOLERR, SizeOf(CDefXlsBOOLERR));
  end;

procedure TExcelWriter.WriteLABEL(const Data: String;
     const Col, Row, XF: AWord); {KV}
  var
    S: String;
    CDefXlsLABEL: TXlsLABEL;
  begin
  CDefXlsLABEL.RecNo := CXlsLABEL;
  S := Data;
  CDefXlsLABEL.cch := Length(S);
  if CDefXlsLABEL.cch > 255 then
    begin
    CDefXlsLABEL.cch := 255;
    SetLength(S, CDefXlsLABEL.cch);
    end;
  CDefXlsLABEL.Rw := Row;
  CDefXlsLABEL.Col := Col;
  CDefXlsLABEL.ixfe := XF;
  CDefXlsLABEL.RecSize := (SizeOf(TXlsLABEL)-2*SizeOf(AWord))
    +CDefXlsLABEL.cch;
  inherited Write(CDefXlsLABEL, SizeOf(CDefXlsLABEL));
  inherited Write(S[1], CDefXlsLABEL.cch);
  end;

procedure TExcelWriter.WriteNUMBER(const Data: Double;
     const Col, Row, XF: AWord); {KV}
  var
    CDefXlsNUMBER: TXlsNUMBER;
  begin
  CDefXlsNUMBER.RecNo := CXlsNUMBER;
  CDefXlsNUMBER.RecSize := SizeOf(TXlsNUMBER)-2*SizeOf(AWord);
  CDefXlsNUMBER.Rw := Row;
  CDefXlsNUMBER.Col := Col;
  CDefXlsNUMBER.ixfe := XF;
  CDefXlsNUMBER.Num := Data;
  inherited Write(CDefXlsNUMBER, SizeOf(CDefXlsNUMBER));
  end;

end.
