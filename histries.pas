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
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

unit Histries;

interface

uses
  Objects, Drivers, Defines, Objects2, Streams, Views,
  Drives, Advance, UKeyMap
  {$IFDEF DBView}, DBView {$ENDIF}
  ;

type
  PViewRecord = ^TViewRecord;
  TViewRecord = record
    FName: PString;
    fOrigin: TPoint;
    fSize: TPoint;
    fDeskSize: TPoint;
    fViewMode: AInt;
    fKeyMap: TKeyMap;
    fToAscii: TXLat; // нужно только при fKeyMap = kmXlat
    fCodeTag: Str8;
    case Byte of
      0: (fPos: Comp;
        fBufPos: AWord;
        fFilter: Byte;
        fHexEdit: Boolean;
        fWrap: Byte; {DataCompBoy}
        fXDelta: AInt;
        fHexPos: AInt;
        fCur: TPoint;
        fMarks: TFPosArray;
        );
      {$IFDEF DBView}
      1: (fdDelta: TDBPoint;
        fdPos: TDBPoint;
        );
      {$ENDIF}
      {$IFDEF SpreadSheet}
      2: (fsDelta: TPoint;
        fsCur: TPoint;
        fsMark: TPoint;
        fsCurrentCalc: TPoint;
        fsSearchPos: TPoint;
        fsErrorCell: TPoint);
      {$ENDIF}
    end;

  PEditRecord = ^TEditRecord;
  TEditRecord = record
    FName: PString;
    fOrigin: TPoint;
    fSize: TPoint;
    fDeskSize: TPoint;
    fPos: TPoint;
    fDelta: TPoint;
    fMarks: TPosArray;
    fBlockStart: TPoint;
    fBlockEnd: TPoint;
    fBlockVisible: Boolean;
    fVerticalBlock: Boolean;
    fHighlight: Boolean;
    fHiliteColumn: Boolean;
    fHiliteLine: Boolean;
    fAutoIndent: Boolean;
    fAutoJustify: Boolean;
    fAutoBrackets: Boolean;
    fInsMode: Boolean;
    fLeftSide,
    fRightSide,
    fInSide: AInt;
    fKeyMap: TKeyMap; {-$VIV}
    { Flash >>> }
    fBackIndent: Boolean;
    fAutoWrap: Boolean;
    fOptimalFill: Boolean;
    fTabReplace: Boolean;
    fSmartTab: Boolean;
    { Flash <<< }
    end;

  PEditHistoryCol = ^TEditHistoryCol;
  TEditHistoryCol = object(TCollection)
    function IndexOf(P: Pointer): LongInt; virtual;
    procedure PutItem(var S: TStream; P: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    procedure FreeItem(P: Pointer); virtual;
    end;

  PViewHistoryCol = ^TViewHistoryCol;
  TViewHistoryCol = object(TEditHistoryCol)
    procedure PutItem(var S: TStream; P: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    procedure FreeItem(P: Pointer); virtual;
    end;

procedure AddToDirectoryHistory(S: String; DriveType: Integer);
procedure SaveCommands(var S: TStream);
procedure LoadCommands(var S: TStream);
procedure CmdHistory;
{DataCompBoy
procedure InitCommands;
}
function DirHistoryMenu: String;
procedure EditHistoryMenu;
procedure ViewHistoryMenu;
procedure StoreEditInfo(P: Pointer);
procedure StoreViewInfo(P: Pointer);
procedure StoreExtViewer(const FileName: String);
procedure AddCommand(const LastCommand: String);
function GetCommand(Idx: Integer): String;
procedure LoadHistories;
procedure SaveHistories;
procedure ClearHistories;
procedure DoneHistories; {Cat}

const
  MaxDirHistorySize = 40;
  MaxEditHistorySize = 20;

  CmdStrings: PCollection = nil;
  DirHistory: PCollection = nil;
  EditHistory: PCollection = nil;
  ViewHistory: PCollection = nil;

var
  HistNameSuffix: string;
    {` Добавки к имени файла истории. Например, если эта переменная имеет
       значение 'Wrk', то для истории будет использован файл DNWrk.HIS `}

implementation
uses
  Lfnvp, Dos, Commands, DNApp, Dialogs, HistList,
  Startup, xTime, Messages, DNUtil, DnIni,
  VpSysLow, EdWin, Advance1,  Advance2,
  {$IFDEF SS}
  Idlers,
  {$ENDIF}
  FViewer, CmdLine, PDSetup, Microed
  , FlPanelX {для ActivePanel}
  {$IFDEF SpreadSheet}, Calc {$ENDIF}
  ;

procedure FreeLastUnmarked(C: PCollection);

  var
    I: Integer;

  function IsThat(P: PEditRecord): Boolean;
    begin
    Dec(i);
    IsThat := (P^.FName <> nil) and (P^.FName^[1] = ' ');
    end;

  begin
  if  (C = nil) or (C^.Count < 1) then
    Exit;
  I := C^.Count;
  if C^.LastThat(@IsThat) <> nil then
    C^.AtFree(I);
  end;

function TEditHistoryCol.IndexOf;
  var
    S, S1: String;
    I: Integer;
  begin
  IndexOf := -1;
  S := CnvString(PEditRecord(P)^.FName);
  S[1] := ' ';
  UpStr(S); {AK155}
  for I := 0 to Count-1 do
    begin
    S1 := CnvString(PEditRecord(At(I))^.FName);
    S1[1] := ' ';
    UpStr(S1); {AK155}
    if S1 = S then
      {-$VOL}
      begin
      IndexOf := I;
      Exit
      end;
    end;
  end;

procedure TEditHistoryCol.PutItem;
  begin
  S.WriteStr(PEditRecord(P)^.FName);
  S.Write(PEditRecord(P)^.fOrigin, SizeOf(TEditRecord)-SizeOf(PString));
  end;

function TEditHistoryCol.GetItem;
  var
    R: PEditRecord;
  begin
  New(R);
  GetItem := R;
  R^.FName := S.ReadStr;
  S.Read(R^.fOrigin, SizeOf(TEditRecord)-SizeOf(PString));
  end;

procedure TEditHistoryCol.FreeItem;
  begin
  if P <> nil then
    begin
    DisposeStr(PEditRecord(P)^.FName);
    Dispose(PEditRecord(P));
    end;
  end;

procedure TViewHistoryCol.PutItem;
  begin
  S.WriteStr(PViewRecord(P)^.FName);
  S.Write(PViewRecord(P)^.fOrigin, SizeOf(TViewRecord)-SizeOf(PString));
  end;

function TViewHistoryCol.GetItem;
  var
    R: PViewRecord;
  begin
  New(R);
  GetItem := R;
  R^.FName := S.ReadStr;
  S.Read(R^.fOrigin, SizeOf(TViewRecord)-SizeOf(PString));
  end;

procedure TViewHistoryCol.FreeItem;
  begin
  if P <> nil then
    begin
    DisposeStr(PViewRecord(P)^.FName);
    Dispose(PViewRecord(P));
    end;
  end;

procedure StoreViewInfo(P: Pointer);
  var
    Viewer: PFileWindow absolute P;
    {$IFDEF DBView}
    DBView: PDBWindow absolute P;
    {$ENDIF}
    {$IFDEF SpreadSheet}
    SSView: PCalcWindow absolute P;
    {$ENDIF}
    R: PViewRecord;
    I: Integer;
  label Q;
  begin
  if  (InterfaceData.Options and ouiTrackViewers = 0) or (P = nil) then
    Exit;
  if ViewHistory = nil then
    ViewHistory := New(PViewHistoryCol, Init(30, 30));
  New(R);

  if TypeOf((PObject(P)^)) = TypeOf(TFileWindow) then
    with PFileViewer(Viewer^.Current)^, R^ do
      begin
      if VFileName = '' then
        goto Q;
      {$IFDEF DPMI32}
      FName := NewStr(' '+lfGetLongFileName(VFileName)); {DataCompBoy}
      {$ELSE}
      FName := NewStr(' '+VFileName);
      {$ENDIF}
      fOrigin := Viewer^.Origin;
      fSize := Viewer^.Size;
      fDeskSize := Desktop^.Size;
      if Filtr then
        fViewMode := ViewMode
      else
        fViewMode := ViewMode or vmInternal;

      {AK155 25-03-2003 После того, как BufPos стало longint, в
fBufPos: AWord оно может не помещаться. Но, с другой стороны,
непонятно, зачем вообще разделять запоминание FilePos и BufPos.
Работают же закладки без такого разделения.  }
      (*
     fPos      := FilePos;
     fBufPos   := BufPos;
     end;
*)
      fPos := FilePos+BufPos;
      fBufPos := 0;
      {/AK155 25-03-2003}

      fFilter := Filter;
      fHexEdit := HexEdit;
      fWrap := Wrap;
      fXDelta := XDelta;
      fHexPos := HexPos;
      fCur := Cur;
      fMarks := MarkPos;
      XCoder.ToHistory(fKeyMap, fToAscii, fCodeTag);
      end
      {$IFDEF DBView} {-DataCompBoy-}
  else if TypeOf((PObject(P)^)) = TypeOf(TDBWindow) then
    with DBView^, R^ do
      begin
      if DBView^.RealName = '' then
        goto Q;
      FName := NewStr(' '+DBView^.RealName);
      fOrigin := Origin;
      fSize := Size;
      fDeskSize := Desktop^.Size;
      fViewMode := vmDB;
      with P^ do
        begin
        fdDelta := Delta;
        fdPos := Pos;
        XCoder.ToHistory(fKeyMap, fToAscii, fCodeTag);
        end;
      end
      {$ENDIF}
      {$IFDEF SpreadSheet}
  else if TypeOf((PObject(P)^)) = TypeOf(TCalcWindow) then
    with SSView^, R^ do
      begin
      {$IFDEF DPMI32}
      FName := NewStr(' '+lfGetLongFileName(CnvString(CalcView^.SName)));
      {$ELSE}
      FName := NewStr(' '+CnvString(CalcView^.SName));
      {$ENDIF}
      fOrigin := Origin;
      fSize := Size;
      fDeskSize := Desktop^.Size;
      with CalcView^ do
        begin
        if ShowSeparators then
          fViewMode := vmSpreadSL {AK155}
        else
          fViewMode := vmSpread;
        fsDelta := Delta;
        fsCur := Cur;
        fsMark := Mark;
        fsCurrentCalc := CurrentCalc;
        fsSearchPos := SearchPos;
        fsErrorCell := ErrorCell;
        end;
      end
      {$ENDIF}
  else
Q:
    begin
    Dispose(R);
    R := nil
    end;
  {-DataCompBoy-}
  if R <> nil then
    begin
    I := ViewHistory^.IndexOf(R);
    if I >= 0 then
      begin
      R^.FName^[1] := PViewRecord(ViewHistory^.At(I))^.FName^[1];
      ViewHistory^.AtFree(I);
      end;
    ViewHistory^.AtInsert(0, R);
    end;
  if ViewHistory^.Count > MaxEditHistorySize then
    FreeLastUnmarked(ViewHistory);
  SaveHistories; {AK155}
  end { StoreViewInfo };

procedure StoreExtViewer(const FileName: String);
  var
    R: PViewRecord;
    I: Integer;
  begin
  if  (InterfaceData.Options and ouiTrackViewers = 0) or (FileName = '')
  then
    Exit;
  if ViewHistory = nil then
    ViewHistory := New(PViewHistoryCol, Init(30, 30));
  New(R);

  with R^ do
    begin
    {$IFDEF DPMI32}
    FName := NewStr(' '+lfGetLongFileName(FileName)); {DataCompBoy}
    {$ELSE}
    FName := NewStr(' '+FileName);
    {$ENDIF}
    fViewMode := vmExternal;
    end;
  I := ViewHistory^.IndexOf(R);
  if I >= 0 then
    begin
    R^.FName^[1] := PViewRecord(ViewHistory^.At(I))^.FName^[1];
    ViewHistory^.AtFree(I);
    end;
  ViewHistory^.AtInsert(0, R);
  if ViewHistory^.Count > MaxEditHistorySize then
    FreeLastUnmarked(ViewHistory);
  end { StoreExtViewer };

procedure StoreEditInfo(P: Pointer);
  var
    E: PEditWindow absolute P;
    I: Integer;
    R: PEditRecord;
    PP: PEditRecord;

  begin
  if  (InterfaceData.Options and ouiTrackEditors = 0) or
      (PFileEditor(E^.Intern)^.EditName = '')
  then
    Exit;
  if EditHistory = nil then
    EditHistory := New(PEditHistoryCol, Init(30, 30));
  New(R);
  with PFileEditor(E^.Intern)^, R^ do
    begin
    {$IFDEF DPMI32}
    FName := NewStr(' '+lfGetLongFileName(EditName)); {DataCompBoy}
    {$ELSE}
    FName := NewStr(' '+EditName);
    {$ENDIF}
    fOrigin := Owner^.Origin;
    fSize := Owner^.Size;
    fDeskSize := Desktop^.Size;
    fMarks := MarkPos;
    fBlockStart := Mark.A;
    fBlockEnd := Mark.B;
    fPos := Pos;
    fDelta := Delta;
    fBlockVisible := BlockVisible;
    fVerticalBlock := VertBlock;
    fHighlight := EdOpt.HiLite;
    fHiliteColumn := EdOpt.HiliteColumn;
    fHiliteLine := EdOpt.HiliteLine;
    fAutoIndent := EdOpt.AutoIndent;
    fAutoJustify := EdOpt.AutoJustify;
    fAutoBrackets := EdOpt.AutoBrackets;
    fLeftSide := EdOpt.LeftSide;
    fRightSide := EdOpt.RightSide;
    fInSide := EdOpt.InSide;
    fInsMode := InsertMode;
    fKeyMap := KeyMap; {-$VIV}
    { Flash >>> }
    fBackIndent := EdOpt.BackIndent;
    fAutoWrap := EdOpt.AutoWrap;
    fOptimalFill := OptimalFill;
    fTabReplace := TabReplace;
    fSmartTab := EdOpt.SmartTab;
    { Flash <<< }
    end;
  I := EditHistory^.IndexOf(R);
  if I >= 0 then
    begin
    R^.FName^[1] := PViewRecord(EditHistory^.At(I))^.FName^[1];
    EditHistory^.AtFree(I);
    end;
  EditHistory^.AtInsert(0, R);
  if EditHistory^.Count > MaxEditHistorySize then
    FreeLastUnmarked(EditHistory);
  SaveHistories; {AK155}
  end { StoreEditInfo };

procedure AddCommand(const LastCommand: String);
  var
    I: Integer;
    P: PString;
  label 1;
  begin
  if LastCommand <> '' then
    begin
    if CmdStrings = nil then
      CmdStrings := New(PLineCollection, Init(40, 40, False));
    for I := 0 to CmdStrings^.Count-1 do
      begin
      P := CmdStrings^.At(I);
      if Copy(CnvString(P), 2, MaxStringLength) = LastCommand then
        begin
        CmdStrings^.AtDelete(I);
        CmdStrings^.Insert(P);
        goto 1;
        end;
      end;
    CmdStrings^.Insert(NewStr(' '+LastCommand));
1:
    I := 0;
    while (CmdStrings^.Count > 50) and
        (I < CmdStrings^.Count)
    do
      begin
      FreeStr := CnvString(CmdStrings^.At(I));
      if FreeStr[1] <> '+' then
        begin
        {if LastTHistPos > I then Dec(LastTHistPos);}
        CmdStrings^.AtFree(I);
        end
      else
        Inc(I);
      end;
    SaveHistories; {AK155}
    end;
  end { AddCommand };
{DataCompBoy
procedure InitCommands;
begin
 if CmdStrings <> nil then Dispose(CmdStrings,Done);
 CmdStrings := New(PLineCollection, Init(40, 10));
 StrModified := False;
 CurString := 0;
end;
}
procedure SaveCommands(var S: TStream);
  var
    I, J: Integer;
    S1, S2: String;
    M: PCollection;
  begin
  {AK155 зачем это Message - непонятно. Выполнение пустой команды
делает ровно ничего (см. cmdline.pas, поиск по cmExecCommandLine)
Message(CommandLine, evCommand, cmExecCommandLine, nil);
/AK155}
  if  (CmdStrings <> nil) and (CmdStrings^.Count >= 50) then
    begin
    M := New(PLineCollection, Init(50, 10, False));
    for I := 1 to 40 do
      begin
      if CmdStrings^.Count <= 0 then
        Break;
      M^.AtInsert(0, CmdStrings^.At(CmdStrings^.Count-1));
      CmdStrings^.AtDelete(CmdStrings^.Count-1);
      end;
    Dispose(CmdStrings, Done);
    CmdStrings := M;
    end;
  S.Put(CmdStrings);
  if InterfaceData.Options and ouiTrackDirs <> 0 then
    S.Put(DirHistory)
  else
    S.Put(nil);
  if InterfaceData.Options and ouiTrackEditors <> 0 then
    S.Put(EditHistory)
  else
    S.Put(nil);
  if InterfaceData.Options and ouiTrackViewers <> 0 then
    S.Put(ViewHistory)
  else
    S.Put(nil);
  end { SaveCommands };

procedure LoadCommands(var S: TStream);
  var
    I: Integer;
  begin
  CmdStrings := PCollection(S.Get);
  DirHistory := PCollection(S.Get);
  EditHistory := PCollection(S.Get);
  ViewHistory := PCollection(S.Get);

  if CmdStrings <> nil then
    CurString := CmdStrings^.Count
  else
    CurString := 0;
  {AK155 Перерисовка комстроки не нужна, а очистка даже мешает}
  (*
 StrModified := False;
 if CommandLine <> nil then CommandLine^.DrawView;
 Str := '';
*)
  {/AK155}
  end;

function GetCommand(Idx: Integer): String;
  begin
  GetCommand := '';
  if  (CmdStrings = nil) or (CmdStrings^.Count <= Idx)
         or (CmdStrings^.At(Idx) = nil)
  then
    Exit;
  GetCommand := Copy(PString(CmdStrings^.At(Idx))^, 2, MaxStringLength);
  end;

type
  PTHistList = ^TTHistList;
  TTHistList = object(TListBox)
    EVHistory, CommandHistory, RolledFwd: Boolean;
    Dlg: TDlgIdx; {AK155}
    function ItemStr(I: LongInt): PString;
    function IsSelected(I: LongInt): Boolean; virtual;
    function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    procedure SelectItem(Item: LongInt); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    destructor Done; virtual;
    end;

function TTHistList.IsSelected;
  var
    P: PString;
  begin
  P := ItemStr(I);
  IsSelected := (P <> nil) and (P^[1] = '+');
  end;

function TTHistList.ItemStr;
  begin
  if EVHistory then
    ItemStr := PEditRecord(List^.At(I))^.FName
  else
    ItemStr := List^.At(I);
  end;

function TTHistList.GetText;
  var
    WasSlash: Boolean;
  begin
  FreeStr := CnvString(ItemStr(Item));
  if not RolledFwd then
    begin
    WasSlash := False;
    if FreeStr[Length(FreeStr)] = '/' then // slash change by unxed
      begin
      SetLength(FreeStr, Length(FreeStr)-1);
      WasSlash := True;
      end;
    if WasSlash then
      FreeStr := Cut(FreeStr, Size.X-2) + '/' // slash change by unxed
    else
      FreeStr := Cut(FreeStr, Size.X-1);
    end
  else
    begin
    if Length(FreeStr) > Size.X-1 then
      FreeStr := FreeStr[1] + #17 +
        Copy(FreeStr, Length(FreeStr) - (Size.X-4), Size.X-3);
    end;
  if FreeStr[1] = '+' then
    FreeStr[1] := #254
  else
    FreeStr[1] := ' ';
  GetText := FreeStr;
  end;

procedure TTHistList.SelectItem;
  var
    P: PString;
  begin
  P := ItemStr(Focused);
  if P <> nil then
    if P^[1] <> '+' then
      P^[1] := '+'
    else
      P^[1] := ' ';
  DrawView;
  end;

procedure TTHistList.HandleEvent;
  label 1;
  var
    P: Pointer;

  procedure ScanMarked(D: Integer);
    var
      P: PString;
      I: Integer;
    begin
    ClearEvent(Event);
    I := Focused;
    repeat
      Inc(I, D);
      if  (I < 0) or (I >= List^.Count) or (ItemStr(I)^[1] <> ' ') then
        Break;
    until False;
    if  (I >= 0) and (I < List^.Count) then
      begin
      FocusItem(I);
      DrawView
      end;
    end;

  procedure SwapItems(i: Integer);
    var
      P: Pointer;
    begin
    with List^ do
      begin
      P := Items^[i];
      Items^[i] := Items^[i+1];
      Items^[i+1] := P;
      end;
    DrawView;
    end;

  var
    DirToGo: String;

  begin { TTHistList.HandleEvent }
  if  (Event.What = evKeyDown) and (List <> nil) then
    case Event.KeyCode of
      kbCtrlEnter: {AK155: делаем как у всех: CtrlEnter = Drop}
        begin
        EndModal(cmYes);
        ClearEvent(Event);
        Exit;
        end;
      kbDel:
        goto 1;
      kbShiftDown:
        if {(ShiftState and 3 <> 0) and}(Focused < List^.Count-1) then
          SwapItems(Focused);
      kbShiftUp:
        if {(ShiftState and 3 <> 0) and}(Focused > 0) then
          SwapItems(Focused-1);
      kbRight, kbAltDown:
        ScanMarked(1);
      kbLeft, kbAltUp:
        ScanMarked(-1);
      kbCtrlRight:
        begin
        RolledFwd := True;
        DrawView;
        end;
      kbCtrlLeft:
        begin
        RolledFwd := False;
        DrawView;
        end;
    end
  else if (Event.What = evBroadcast) then
    case Event.Command of
      cmOK:
        begin
        ClearEvent(Event);
        if Focused >= List^.Count then
          Exit;
        FreeStr := fDelLeft(fDelRight(Copy(CnvString(List^.At(Focused)),
                 2, 255))); {-$VIV}
        {DataCompBoy}
        if InputBox(GetString(dlEditHistory),
             GetString(dlFindCellString), FreeStr, 255, hsEditHistory)
           <> cmOK
        then
          Exit;
        Insert(Copy(CnvString(List^.At(Focused)), 1, 1), FreeStr, 1);
        List^.AtReplace(Focused, NewStr(FreeStr));
        DrawView;
        end;
      cmYes: { удаление }
1:
          begin
          ClearEvent(Event);
          if  (Dlg = dlgDirectoryHistory) and (Focused = 0) then
            begin
            MessageBox(GetString(dlHistDelCurDir), nil, mfOKButton);
            {John_SW}
            Exit;
            {AK155: 0 - это текущий каталог,
                  его удалять бесполезно: он опять вставится, но при этом
                  собьет нумерацию элементов }
            end;
          if Focused >= List^.Count then
            Exit;
          if Copy(CnvString(ItemStr(Focused)), 1, 1) = '+' then
            begin
            if HistoryErrorBeep then
              begin
              SysBeepEx {PlaySound}(500, 110);
              end;
            Exit;
            end;
          if CommandHistory and (Focused <= CurString)
               and (CurString > 0)
          then
            Dec(CurString);
          List^.AtFree(Focused);
          SetRange(List^.Count);
          DrawView;
          end;

       cmNo: {встать на}
          begin
          ClearEvent(Event);
          if Focused >= List^.Count then
            Exit;
          if (Dlg = dlgDirectoryHistory) then
            DirToGo := Copy(CnvString(List^.At(Focused)), 2,
                            MaxStringLength)
          else
            DirToGo := Copy(PViewRecord(List^.At(Focused))^.FName^, 2,
                            MaxStringLength);
          MakeNoSlash(DirToGo);
          Message(ActivePanel, evCommand, cmStandAt, @DirToGo);
          DrawView;
          EndModal(cmCancel); //Не важно, какая команда, главное завершить
          end;

    end {case};
  inherited HandleEvent(Event);
  end { TTHistList.HandleEvent };

destructor TTHistList.Done;
  begin
  List := nil;
  inherited Done;
  end;

procedure AddToDirectoryHistory(S: String; DriveType: Integer);
  var
    I: Integer;
    P: PString;

  function IsThat(P: PString): Boolean;
    begin
    Inc(i);
    IsThat := S = Copy(P^, 2, MaxStringLength);
    end;

  function IsThis(P: PString): Boolean;
    begin
    IsThis := P^[1] = ' ';
    end;

  begin
  if InterfaceData.Options and ouiTrackDirs = 0 then
    Exit;
  if  (S = '') or ((S[2] <> ':') and ((S[1] <> '/') or (S[2] <> '/'))) // slash change by unxed
  then
    Exit;
  {Cat: добавил проверку на сетевые пути}
  {$IFDEF DPMI32}
  S := lfGetLongFileName(S);
  {$ENDIF}
  if DirHistory = nil then
    DirHistory := New(PLineCollection, Init(40, 40, False));
  if  (DriveType <> Integer(dtList)) and
      (DriveType <> Integer(dtFind)) and
      (DriveType <> Integer(dtArcFind)) and
      (DriveType <> Integer(dtArvid)) and
      (DriveType <> Integer(dtArc))
  then
    MakeSlash(S)
  else
    begin
    MakeNoSlash(S);
    if  (DriveType = Integer(dtArvid)) or
        (DriveType = Integer(dtArc))
    then
      AddStr(S, '/'); // slash change by unxed
    end;
  I := -1;
  P := DirHistory^.FirstThat(@IsThat);
  if P <> nil then
    DirHistory^.AtDelete(I)
  else
    P := NewStr(' '+S);
  if P <> nil then
    DirHistory^.AtInsert(0, P);
  if DirHistory^.Count > MaxDirHistorySize then
    begin
    P := DirHistory^.LastThat(@IsThis);
    if P <> nil then
      DirHistory^.Free(P);
    end;
  SaveHistories; {AK155}
  end { AddToDirectoryHistory };

function GetDialog(Dlg: TDlgIdx; var List: Pointer): PDialog;
  var
    D: PDialog;
    L: PTHistList; {AK155}
    P: PView;
    R: TRect;
  begin
  D := PDialog(LoadResource(Dlg));

  R.Assign(D^.Size.X-3, 2, D^.Size.X-2, 13);
  P := New(PScrollBar, Init(R));
  D^.Insert(P);

  R.Assign(2, 2, D^.Size.X-3, 13);
  L := New(PTHistList, Init(R, 1, PScrollBar(P)));
  L^.Dlg := Dlg; {AK155: см. TTHistList.HandleEvent, cmYes }
  D^.Insert(L);
  List := L;

  GetDialog := D;
  end;

procedure EditHistoryMenu;
  var
    D: PDialog;
    P: PTHistList;
    I: Integer;
  begin
  if InterfaceData.Options and ouiTrackEditors = 0 then
    begin
    Msg(dlSetEditHistory, nil, mfError+mfOKButton);
    Exit;
    end;
  ClearHistories;
  LoadHistories; {AK155}
  if EditHistory = nil then
    EditHistory := New(PEditHistoryCol, Init(30, 30));
  {  if EditHistory^.Count = 0 then Exit;}
  D := GetDialog(dlgEditHistory, Pointer(P));
  P^.NewLisT(EditHistory);
  P^.EVHistory := True;
  if Desktop^.ExecView(D) = cmOK then
    I := P^.Focused
  else
    I := -1;
  Dispose(D, Done);
  if EditHistory^.Count = 0 then
    Exit; {Proverka, esli udalyali, zarazy:}
  if  (I >= 0) then
    begin
    if  (PViewRecord(EditHistory^.At(I))^.FName = nil) then
      Exit;
    {A eto tak, na vsyakiy sluchay proverka, esli eto ne DPMI :}
    PDNApplication(Application)^.EditFile(
      SystemData.Options and ossEditor <> 0,
      {AK155 28.09.2002:
             это чтобы через историю всегда вызывался внутренний
             редактор, поскольку вызов внешнего в историю не попадает}
      Copy(PViewRecord(EditHistory^.At(I))^.FName^, 2, MaxStringLength));
    end;
  end { EditHistoryMenu };

procedure ViewHistoryMenu;
  var
    D: PDialog;
    P: PTHistList;
    I: Integer;
  begin
  if InterfaceData.Options and ouiTrackViewers = 0 then
    begin
    Msg(dlSetViewHistory, nil, mfError+mfOKButton);
    Exit;
    end;
  ClearHistories;
  LoadHistories; {AK155}
  if ViewHistory = nil then
    ViewHistory := New(PViewHistoryCol, Init(30, 30));
  {  if ViewHistory^.Count = 0 then Exit;}
  D := GetDialog(dlgViewHistory, Pointer(P));
  P^.NewLisT(ViewHistory);
  P^.EVHistory := True;
  if Desktop^.ExecView(D) = cmOK then
    I := P^.Focused
  else
    I := -1;
  Dispose(D, Done);
  if ViewHistory^.Count = 0 then
    Exit; {Proverim, esli udalyali, pa**y}
  if I >= 0 then
    begin
    if  (PViewRecord(ViewHistory^.At(I))^.FName = nil) then
      Exit; {Ku :}
    PDNApplication(Application)^.ViewFile(False, True, {AK155}
      Copy(PViewRecord(ViewHistory^.At(I))^.FName^, 2, MaxStringLength));
    end;
  end { ViewHistoryMenu };

function DirHistoryMenu: String;
  var
    PC: PLineCollection;
    D: PDialog;
    P: PView;
    R: TRect;
    I: Integer;
    DT: record
      PC: PCollection;
      I: Integer;
      end;
  begin
  ClearHistories;
  LoadHistories; {AK155}
  DirHistoryMenu := '';

  if InterfaceData.Options and ouiTrackDirs = 0 then
    begin
    Msg(dlSetDirHistory, nil, mfError+mfOKButton);
    Exit;
    end;

  if DirHistory = nil then
    DirHistory := New(PLineCollection, Init(40, 40, False));
  {  if DirHistory^.Count = 0 then Exit;}

  D := GetDialog(dlgDirectoryHistory, Pointer(P));

  PListBox(P)^.NewLisT(DirHistory);
  if DirHistory^.Count > 1 then
    PListBox(P)^.Focused := 1;

  I := Desktop^.ExecView(D);

  DT.I := PListBox(P)^.Focused;
  Dispose(D, Done);
  if I = cmOK then
    DirHistoryMenu := Copy(CnvString(DirHistory^.At(DT.I)), 2,
         MaxStringLength);
  end { DirHistoryMenu: };

procedure CmdHistory;
  var
    PC: PLineCollection;
    D: PDialog;
    P: PView;
    R: TRect;
    I: Integer;
    DT: record
      PC: PCollection;
      I: Integer;
      end;
  begin
  ClearHistories;
  LoadHistories; {AK155}
  if CmdStrings = nil then
    CmdStrings := New(PLineCollection, Init(40, 40, False));

  D := GetDialog(dlgCommandsHistory, Pointer(P));

  PListBox(P)^.NewLisT(CmdStrings);
  PListBox(P)^.FocusItem(CmdStrings^.Count-1);
  PTHistList(P)^.CommandHistory := True;
  if CmdStrings^.Count > 0 then
    PListBox(P)^.FocusItem(CmdStrings^.Count-1);

  I := Desktop^.ExecView(D);

  DT.I := PListBox(P)^.Focused;
  Dispose(D, Done);

  if I = cmCancel then
    Exit;
  Message(CommandLine, evKeyDown, kbDown, nil);

  CurString := DT.I;
  Str := GetCommand(DT.I);
  CommandLine^.DrawView;
  Message(CommandLine, evKeyDown, kbEnd, nil);
  if I <> cmYes then
    Message(CommandLine, evKeyDown, kbEnter, nil);
  end { CmdHistory };

const
  HistoryFileSign = 'DN OSP History file'#13#10#26#1#51#05;

  {-DataCompBoy-}
procedure LoadHistories;
  var
    S: TBufStream;
    A: AWord;
  begin
  S.Init(SourceDir+'DN'+HistNameSuffix+'.HIS', stOpenRead, 2048);
  if S.Status = stOK then
    begin
    S.Read(FreeStr[1], Length(HistoryFileSign));
    SetLength(FreeStr, Length(HistoryFileSign));
    if FreeStr = HistoryFileSign then
      begin
      S.Read(A, SizeOf(A));
      if HistorySize < A+256 then
        begin
        DoneHistory;
        HistorySize := A+256;
        InitHistory;
        end;
      HistoryUsed := A+Word(HistoryBlock);
      S.Read(HistoryBlock^, A);
      LoadCommands(S);
      end
    else
      MessageBox('Can''t load histories!', nil, mfOKButton);
    end;
  S.Done;
  end { LoadHistories };
{-DataCompBoy-}

{-DataCompBoy-}
procedure SaveHistories;
  var
    S: TBufStream;
    A: AWord;
  begin
  S.Init(SourceDir+'DN'+HistNameSuffix+'.HIS', stCreate, 2048);
  if S.Status = stOK then
    begin
    FreeStr := HistoryFileSign;
    S.Write(FreeStr[1], Length(HistoryFileSign));
    A := HistoryUsed-Word(HistoryBlock);
    S.Write(A, SizeOf(A));
    S.Write(HistoryBlock^, A);
    SaveCommands(S);
    end;
  S.Done;
  end;
{-DataCompBoy-}

procedure ClearHistories;
  var
    I, J, K: Integer;
    B: PChar;

  procedure ClearStrCollection(C: PCollection);
    begin
    if C = nil then
      Exit;
    C^.Pack;
    i := 0;
    while i < C^.Count do
      begin
      FreeStr := CnvString(C^.At(i));
      if FreeStr[1] = ' ' then
        C^.AtFree(i)
      else
        Inc(i);
      end;
    end;

  procedure ClearCollection(C: PCollection);
    begin
    if C = nil then
      Exit;
    C^.Pack;
    i := 0;
    while i < C^.Count do
      begin
      FreeStr := CnvString(PEditRecord(C^.At(i))^.FName);
      if FreeStr[1] = ' ' then
        C^.AtFree(i)
      else
        Inc(i);
      end;
    end;

  var
    A: AWord;
  begin { ClearHistories }
  if HistoryBlock = nil then
    Exit;
  B := PChar(HistoryBlock);
  I := 1;
  A := HistoryUsed-Word(HistoryBlock);
  while I < A do
    begin
    if B[I+1] = #0 then
      Break;
    Move(B[I+2], FreeStr, Byte(B[I+2])+1);
    J := I+Byte(B[I+2])+3;
    if FreeStr[Length(FreeStr)] <> ' ' then
      I := J
    else
      begin
      Move(B[J], B[I], A-J+1);
      Dec(A, J-I);
      end;
    end;
  if (HistoryUsed - Word(HistoryBlock)) < 4 then
    HistoryUsed := Word(HistoryBlock);
  I := 0;
  ClearStrCollection(CmdStrings);
  ClearStrCollection(DirHistory);
  ClearCollection(EditHistory);
  ClearCollection(ViewHistory);
  end { ClearHistories };

{Cat}
procedure DoneHistories;
  begin
  if CmdStrings <> nil then
    Dispose(CmdStrings, Done);
  if DirHistory <> nil then
    Dispose(DirHistory, Done);
  if EditHistory <> nil then
    Dispose(EditHistory, Done);
  if ViewHistory <> nil then
    Dispose(ViewHistory, Done);
  end;
{/Cat}

end.
