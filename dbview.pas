{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
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
//////////////////////////////////////////////////////////////////////////
//
//  Version history:
//
//  1.6.RC1
//  dn16rc1-deleting_temporary_files_safety_fix.patch
//  dn16rc1-dbf_scrollbar_colors_fix.patch
//  dn16rc1-vp_noasm_compatible.patch
//  dn16rc1-edit_long_db_field.patch
//
//  2.0.0
//  dn200-delete_file_from_temp_fix-diff161byMV.patch
//  dn200-dbf_wkz-history.patch
//  dn200-dbf_wkz_hscroll_bar.patch
//  dn200-empty_dbf.patch
//  dn223-dbf_indicator_and_hscrollbar.patch
//  dn2215-windows_manager_changes_2.patch
//
//  2.3.0
//  dn247-text_and_file_find_improve.patch
//  dn230-dbf_history_improve.patch
//  dn230-dbf_view_long_field_improve.patch
//  dn230-dbf_viewer_scrollbars_fix.patch
//
//  2.7.0
//  dn270-dbf_window_activate_fix.patch
//  dn270_dbf_goto_row_column_added.patch
//  dn281-dbf_wkz_view_from_archive_fix.patch
//  dn281_dbf_detect_fix.patch
//  dn281-dbf_view_Clipper_memo_fix.patch
//  dn2911-dbf_search_in_current_line_fix.patch
//  dn2106-dbf_viewer_search_date_fix.patch
//  dn2106-dbf_update_after_country_settings_changed.patch
//  dn21013-dbf_search_fix.patch
//  dn21029-dbf_find_empty_fields.patch
//  dn3216-DBF_Viewer(fn)-change_code_page_ability_added.patch
//  dn3315-DBF_Viewer(i)-indicator_and_code_page_improve.patch
//  dn3421-DBF_WKZ(f)-find_settings_store_fix.patch
//
//  3.7.0
//  dn31005-bp_to_vp_on_off_true_false.patch
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit DBView;

interface

uses
  DBWatch, Defines, Objects2, Streams,
  Views, Drivers, Dialogs,
  FViewer, Commands, XCode
  ;

type

  TDBPoint = record
    X: Integer;
    Y: LongInt;
    end;

  PDBIndicator = ^TDBIndicator;
  PDBViewer = ^TDBViewer;

  { -------- Eugeny Zvyagintzev ---------}
  PDBScrollBar = ^TDBScrollBar;
  TDBScrollBar = object(TScrollBar)
    ScrollBarType: Integer;
    DBViewer: PDBViewer;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetPalette: PPalette; virtual;
    end;
  { -------- Eugeny Zvyagintzev ---------}

  TDBViewer = object(TView)
    FileName: PString;
    DBFile: PDBFile;
    isValid, KillAfterUse: Boolean;
    Buf: Pointer;
    SearchString: PString;
    StartRec: LongInt;
    NumRec: Word;
    Indicator: PDBIndicator;
    Delta, Pos: TDBPoint;
    VerticalScrollBar: PDBScrollBar;
    HorizontalScrollBar: PDBScrollBar;
    XCoder: TXCoder;
    constructor Init(R: TRect; FName: String; var FileIsDBF: Boolean);
    {DataCompBoy}
    function Valid(Command: Word): Boolean; virtual;
    function GetRecord(N: LongInt): Pointer;
    procedure GetInfo(StrIdx: TStrIdx);
    procedure HandleEvent(var Event: TEvent); virtual;
    destructor Done; virtual;
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    function Failed(var FileIsDBF: Boolean): Boolean;
    end;

  PMemoStream = ^TMemoStream;
  TMemoStream = object(TStream)
    StartPos: LongInt;
    Length: LongInt;
    BaseStream: PStream;
    constructor Init(AStream: PStream; Start, len: LongInt);
    destructor Done; virtual;
    function GetPos: TFileSize; virtual;
    function GetSize: TFileSize; virtual;
    procedure Read(var Buf; Count: Word); virtual;
    procedure Seek(Pos: TFileSize); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: Word); virtual;
    end;

  TDBIndicator = object(TView)
    DBViewer: PDBViewer;
    procedure Draw; virtual;
    {Constructor Load(var S : Tstream);
       Procedure Store(var S : TStream);}
    end;

  PFieldListBox = ^TFieldListBox;
  TFieldListBox = object(TListBox)
    function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    end;

  PDBWindow = ^TDBWindow;
  TDBWindow = object(TFileWindow)
    P: PDBViewer;
    P1: PDBIndicator;
    VSB: PDBScrollBar;
    HSB: PDBScrollBar;
    RealName: String;
    {--- start -------- Eugeny Zvyagintzev ---------}
    constructor Init(FName: String; var FileIsDBF: Boolean);
    {--- finish -------- Eugeny Zvyagintzev ---------}
    function GetPalette: PPalette; virtual;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    {John_SW 14-03-2003}
    destructor Done; virtual;
    end;

const
  CDBWindow = #166#167#168#169#170#171#212#213;
  CDBViewer = #4#5#6;
  CDBScrollBar = #7#8#8;

const
  SearchData: record
    S: String[250]; {Inputline}
    OPT: Word; {Checkbox}
    Scope: Word; {Radiobuttons}
    Direction: Integer; {Radiobuttons}
    end = (S: ''; OPT: 0; Scope: 0; Direction: 0);

implementation
uses
  Advance, Advance1, Advance2, Startup, DNHelp, DNApp, Messages,
  Histries, Gauge, xTime, DnIni
  , Events, lfnvp, Dos, Memory, UKeyMap
  ;

function MaxAvail: LongInt;
  begin
  MaxAvail := MemAdjust(System.MaxAvail);
  end;

function TFieldListBox.GetText;
  var
    P: PFieldRec;
    S: String;
    S1, S2, S3, S4: String[20];
    M: array[0..4] of Pointer;
  begin
  P := List^.At(Item);
  LongInt(M[0]) := Item+1;
  M[1] := @P^.Name;
  M[2] := @S1;
  M[3] := @S2;
  M[4] := @S3;
  case P^.Who of
    'N':
      S1 := GetString(dlDBNumeric);
    'C':
      S1 := GetString(dlDBCharacter);
    'M':
      S1 := GetString(dlDBMemo);
    'L':
      S1 := GetString(dlDBLogical);
    'D':
      S1 := GetString(dlDBDate);
    'F':
      S1 := GetString(dlDBFloat);
    'P':
      S1 := GetString(dlDBPicture);
  end {case};
  S2 := ItoS(P^.len);
  if  (P^.Who = 'N') or (P^.Who = 'F') then
    S3 := ItoS(P^.Dec)
  else
    S3 := '';
  if  (P^.Who = 'C') and (P^.Dec <> 0) then
    S3 := ItoS(P^.Dec); {JOHN_SW}
  FormatStr(S, ' %-4d%-12s%-13s%-11s%-8s', M);
  GetText := S;
  end { TFieldListBox.GetText };

{ -------- Eugeny Zvyagintzev ---------}
function TDBScrollBar.GetPalette: PPalette;
  const
    S: String[Length(CDBScrollBar)] = CDBScrollBar;
  begin
  GetPalette := @S;
  end;

procedure TDBScrollBar.HandleEvent(var Event: TEvent);
  begin
  inherited HandleEvent(Event);
  case ScrollBarType of
    sbVertical:
      if DBViewer^.Delta.Y <> Value then
        begin
        DBViewer^.Delta.Y := Value;
        DBViewer^.DrawView;
        DBViewer^.Indicator^.Draw;
        end;
    sbHorizontal:
      if DBViewer^.Delta.X <> Value then
        begin
        DBViewer^.Delta.X := Value;
        DBViewer^.DrawView;
        DBViewer^.Indicator^.Draw;
        end;
  end {case};
  end;
{ -------- Eugeny Zvyagintzev ---------}

constructor TMemoStream.Init;
  begin
  TObject.Init;
  StartPos := Start;
  Length := len;
  BaseStream := AStream;
  if AStream^.Status = stOK then
    AStream^.Seek(StartPos);
  ErrorInfo := BaseStream^.ErrorInfo;
  Status := BaseStream^.Status;
  end;

destructor TMemoStream.Done;
  begin
  Dispose(BaseStream, Done);
  BaseStream := nil;
  TObject.Done;
  end;

function TMemoStream.GetPos: TFileSize;
  var
    L: TFileSize;
  begin
  L := BaseStream^.GetPos-StartPos;
  if L < 0 then
    begin
    BaseStream^.Seek(StartPos);
    L := 0;
    end;
  GetPos := L;
  ErrorInfo := BaseStream^.ErrorInfo;
  Status := BaseStream^.Status;
  end;

function TMemoStream.GetSize: TFileSize;
  begin
  GetSize := Length;
  end;

procedure TMemoStream.Read(var Buf; Count: Word);
  begin
  BaseStream^.Read(Buf, Count);
  ErrorInfo := BaseStream^.ErrorInfo;
  Status := BaseStream^.Status;
  end;

procedure TMemoStream.Seek(Pos: TFileSize);
  var
    L: TFileSize;
  begin
  L := Pos+StartPos;
  BaseStream^.Seek(L);
  ErrorInfo := BaseStream^.ErrorInfo;
  Status := BaseStream^.Status;
  end;

procedure TMemoStream.Truncate;
  begin
  BaseStream^.Truncate;
  ErrorInfo := BaseStream^.ErrorInfo;
  Status := BaseStream^.Status;
  end;

procedure TMemoStream.Write(const Buf; Count: Word);
  begin
  BaseStream^.Write(Buf, Count);
  ErrorInfo := BaseStream^.ErrorInfo;
  Status := BaseStream^.Status;

  {Move(AStream^, Self, sizeof(TStream));}
  end;

{-DataCompBoy-}
constructor TDBViewer.Init;
  begin
  inherited Init(R);
  KillAfterUse := TempFile <> '';
  Options := Options or ofSelectable;
  GrowMode := gfGrowHiX+gfGrowHiY;
  EventMask := $FFFF;
  SearchString := nil;
  HelpCtx := hcDBView;
  Buf := nil;
  FileName := NewStr(lFExpand(FName));
  if Failed(FileIsDBF) then
    isValid := False
  else if isValid then
    TempFile := '';
  XCoder.Init(8);
  end;
{-DataCompBoy-}

procedure TDBViewer.Store;
  begin
  inherited Store(S);
  S.Write(KillAfterUse, SizeOf(KillAfterUse));
  S.WriteStr(SearchString);
  S.WriteStr(FileName);
  S.Write(Delta, SizeOf(Delta));
  S.Write(Pos, SizeOf(Pos));
  XCoder.Store(S);
  end;

function DateToHuman(const S: String): String;
  var
    I: Integer;
    C: Char;
    BB: Byte;
    D, M: String[2];
    Y: String[4];
  begin
  Y := Copy(S, 1, 4);
  M := Copy(S, 5, 2);
  D := Copy(S, 7, 2);
  case CountryInfo.DateFmt of
    dfMDY:
      DateToHuman := M+CountryInfo.DateSep+D+CountryInfo.DateSep+Y;
    dfDMY:
      DateToHuman := D+CountryInfo.DateSep+M+CountryInfo.DateSep+Y;
    dfYMD:
      DateToHuman := Y+CountryInfo.DateSep+M+CountryInfo.DateSep+D;
  end {case};
  end;

function TDBViewer.Failed(var FileIsDBF: Boolean): Boolean;
  var
    I: LongInt;
  begin
  Failed := False;
  New(DBFile, Init(CnvString(FileName)));

  if DBFile <> nil then
    begin
    I := 65520;
    if DBFile^.NumRec = 0 then
      begin
      GetInfo(dlDBEmptyStruc);
      {     DisposeStr(SearchString);
      Dispose(DBFile,Done); DBFile:=nil;
}TempFile := '';
      if KillAfterUse then
        EraseTempFile(FileName^);
      {     DisposeStr(FileName);}
      Failed := True;
      FileIsDBF := True;
      Exit;
      end;
    if MaxAvail < I then
      I := MaxAvail;
    if LongInt(I) > DBFile^.NumRec*LongInt(DBFile^.RecLen) then
      I := DBFile^.NumRec*LongInt(DBFile^.RecLen);
    NumRec := I div DBFile^.RecLen;
    I := NumRec*DBFile^.RecLen;
    StartRec := 0;
    if I <> 0 then
      GetMem(Buf, I);
    end;
  isValid := (DBFile <> nil) and (Buf <> nil);
  if isValid then
    begin
    DBFile^.Seek(0);
    DBFile^.Read(Buf^, NumRec);
    end;
  FileIsDBF := isValid;
  end { TDBViewer.Failed };

constructor TDBViewer.Load;
  var
    FileIsDBF: Boolean;
  begin
  inherited Load(S);
  S.Read(KillAfterUse, SizeOf(KillAfterUse));
  SearchString := S.ReadStr;
  FileName := S.ReadStr;

  if Failed(FileIsDBF) then
    Fail;

  S.Read(Delta, SizeOf(Delta));
  S.Read(Pos, SizeOf(Pos));

  XCoder.Load(S);
  end;

destructor TDBViewer.Done;
  begin
  DisposeStr(SearchString);
  if Buf <> nil then
    FreeMem(Buf, NumRec*DBFile^.RecLen);
  if DBFile <> nil then
    Dispose(DBFile, Done);
  DBFile := nil;
  {--- start -------- Eugeny Zvyagintzev ---- 07-08-2002 -----}
  if not (TottalExit or Exiting) and KillAfterUse then
    begin
    EraseTempFile(FileName^);
    KillAfterUse := False;
    end;
  {--- finish -------- Eugeny Zvyagintzev ---- 07-08-2002 -----}
  DisposeStr(FileName);
  inherited Done;
  end;

function TDBViewer.GetPalette;
  const
    S: String[Length(CDBViewer)] = CDBViewer;
  begin
  GetPalette := @S;
  end;

function TDBWindow.GetPalette;
  const
    S: String[Length(CDBWindow)] = CDBWindow;
  begin
  GetPalette := @S;
  end;

function TDBViewer.GetRecord;
  var
    I: Integer;
  begin
  GetRecord := nil;
  if N < 0 then
    N := 0;
  if N >= DBFile^.NumRec then
    Exit;
  if  (N < StartRec) or (N >= StartRec+NumRec) then
    begin
    StartRec := N-(NumRec div 2);
    if StartRec < 0 then
      StartRec := 0;
    DBFile^.Seek(StartRec);
    DBFile^.Read(Buf^, NumRec);
    end;
  GetRecord := @PByteArray(Buf)^[(N-StartRec)*DBFile^.RecLen];
  end;

function TDBViewer.Valid;
  begin
  Valid := inherited Valid(Command) and isValid;
  end;

procedure TDBViewer.GetInfo;
  var
    R: TRect;
    D: PDialog;
    P: PView;
    Data: record
      List: Pointer {PCollection};
      Focused: Integer;
      end;
  begin
  R.Assign(1, 1, 50, 18);
  D := New(PDialog, Init(R, GetString(StrIdx)+Cut(FileName^, 20)));
  D^.Options := D^.Options or ofCentered;

  R.Assign(2, 2, 48, 3);
  P := New(PStaticText, Init(R, GetString(dlDBViewInfoString)));
  D^.Insert(P);

  R.Assign(46, 3, 47, 13);
  P := New(PScrollBar, Init(R));
  P^.Options := P^.Options or ofPostProcess;
  D^.Insert(P);

  R.Assign(2, 3, 46, 13);
  P := New(PFieldListBox, Init(R, 1, PScrollBar(P)));
  D^.Insert(P);

  R.Assign(30, 14, 40, 16);
  P := New(PButton, Init(R, GetString(dlOKButton), cmOK, bfDefault));
  P^.Options := P^.Options or ofCenterX;
  D^.Insert(P);

  D^.SelectNext(False);

  D := PDialog(Application^.ValidView(D));
  if D = nil then
    Exit;

  Data.List := DBFile^.Fields;
  Data.Focused := Delta.X;
  D^.SetData(Data);
  if Desktop^.ExecView(D) = cmOK then
    begin
    D^.GetData(Data);
    if Delta.X <> Data.Focused then
      begin
      Delta.X := Data.Focused;
      DrawView;
      end;
    end;
  Dispose(D, Done);
  end { TDBViewer.GetInfo };

procedure TDBViewer.HandleEvent;

  {--- start -------- Eugeny Zvyagintzev ---- 31-07-2002 ----}
  procedure GotoRecFld;
    const
      GotoRec: record
        S1: String[10]; {Line Number}
        S2: String[10]; {Field Name}
        end = (S1: ''; S2: '');
    var
      FldCnt: Integer;
      Done: Boolean;
      PFR: PFieldRec;
    begin
    PFR := DBFile^.GetFieldRec(Delta.X);
    GotoRec.S1 := ItoS(Delta.Y+1);
    GotoRec.S2 := PFR^.Name;
    if  (ExecResource(dlgDBFGoto, GotoRec) <> cmOK) or
        ( (GotoRec.S1 = ItoS(Delta.Y+1)) and (GotoRec.S2 = PFR^.Name))
    then
      Exit;
    FldCnt := 0;
    GotoRec.S2 := UpStrg(DelSpaces(GotoRec.S2));
    Done := False;
    if PFR^.Name <> GotoRec.S2 then
      while (not Done) and (FldCnt < DBFile^.NumFields) do
        begin
        PFR := DBFile^.GetFieldRec(FldCnt);
        if PFR^.Name = GotoRec.S2 then
          Done := True
        else
          Inc(FldCnt);
        end;
    if Done then
      Delta.X := FldCnt;
    if GotoRec.S1 <> ItoS(Delta.Y+1) then
      begin
      Done := True;
      Delta.Y := StoI(GotoRec.S1)-1;
      end;
    if Done then
      DrawView;
    end { GotoRecFld };
  {--- finish -------- Eugeny Zvyagintzev ---- 31-07-2002 ----}

  {-DataCompBoy-}
  procedure ViewMemo;
    var
      P: PByteArray;
      PFR: PFieldRec;
      S: String;
      L: LongInt;
      ML: LongInt;{!!s}
      I: Integer;
      D: PDialog;
      R: TRect;
      PV: PView;
      MemoStream: PStream;
      Dr: String;
      Nm: String;
      Xt: String;
    begin
    P := GetRecord(Delta.Y);
    if P = nil then
      Exit;
    PFR := DBFile^.GetFieldRec(Delta.X);
    if PFR = nil then
      Exit;
    if  (PFR^.Who <> 'M') then
      Exit;
    Move(P^[PFR^.Pos], S[1], PFR^.len);
    S[0] := Char(PFR^.len);
    Val(DelSpaces(S), L, I);
    ML := 0;
    lFSplit(FileName^, Dr, Nm, Xt);
    if  (Xt[0] = #4) and (UpStrg(Xt) <> '.DBF') then
      Xt[4] := 'T'
    else
      Xt := '.FPT';
    MemoStream := New(PDosStream, Init(Dr+Nm+Xt, stOpenRead));
    if  (MemoStream = nil) or (MemoStream^.Status <> stOK) then
      begin
      if MemoStream <> nil then
        Dispose(MemoStream, Done);
      MemoStream := New(PBufStream, Init(Dr+Nm+'.dbt', stOpenRead, 512));
      if  (MemoStream = nil) or (MemoStream^.Status <> stOK) then
        begin
        if MemoStream <> nil then
          Dispose(MemoStream, Done);
        MemoStream := nil;
        Exit;
        end;
      if L <> 0 then
        {JOHN_SW}
        begin
        L := L*512;
        MemoStream^.Seek(L);
        repeat
          MemoStream^.Read(Nm[1], 1);
          if Nm[1] <> #$1A then
            Inc(ML)
          else
            Break
        until MemoStream^.Status <> stOK;
        if  (MemoStream = nil) or (MemoStream^.Status <> stOK) then
          begin
          ErrMsg(dlDBViewNoMemo);
          if MemoStream <> nil then
            Dispose(MemoStream, Done);
          MemoStream := nil;
          Exit;
          end;
        end;
      end
    else
      begin
      if L <> 0 then
        {JOHN_SW}
        begin
        MemoStream^.Seek(4);
        MemoStream^.Read(ML, 4);
        asm
       mov ax, word ptr ML
       mov bx, word ptr ML+2
       xchg al, ah
       xchg bl, bh
       mov word ptr ML, bx
       mov word ptr ML+2, ax
      end;

        L := L*ML;
        MemoStream^.Seek(L+4);
        ML := i32(MemoStream^.GetPos);
        ML := 0;
        MemoStream^.Read(ML, 4);
        asm
       mov ax, word ptr ML
       mov bx, word ptr ML+2
       xchg al, ah
       xchg bl, bh
       mov word ptr ML, bx
       mov word ptr ML+2, ax
      end;
        Inc(L, 8);
        end;
      end;

    MemoStream := New(PMemoStream, Init(MemoStream, L, ML));
    if MemoStream^.Status <> stOK then
      begin
      Dispose(MemoStream, Done);
      MemoStream := nil;
      Exit
      end;
    Desktop^.GetExtent(R);
    R.Grow(-2, -2);
    R.Assign(1, 1, 70, 20);
    D := New(PDialog, Init(R, GetString(dlDBViewViewMemo)));
    D^.Options := D^.Options or ofCentered;

    R.Assign(D^.Size.X-2, 2, D^.Size.X-1, D^.Size.Y-4);
    PV := New(PViewScroll, Init(R));
    D^.Insert(PV);
    PV^.GetPalette^:= CScrollBar;

    R.Assign(2, 2, D^.Size.X-2, D^.Size.Y-4);
    PV := New(PNFileViewer, Init(R, MemoStream, '', '', PV, False, False));

    D^.Insert(PV);

    R.Assign(30, D^.Size.Y-3, 40, D^.Size.Y-1);
    PV := New(PButton, Init(R, GetString(dlOKButton), cmOK, bfDefault));
    PV^.Options := PV^.Options or ofCenterX;
    D^.Insert(PV);

    D^.SelectNext(False);

    D := PDialog(Application^.ValidView(D));
    if D = nil then
      Exit;
    Desktop^.ExecView(D);
    Dispose(D, Done);
    end { ViewMemo };
  {-DataCompBoy-}

  procedure ContSearch(StartSearch: Boolean);
    var
      FirstFld, LastFld, CurFld, CurRow: LongInt;
      PFR: PFieldRec;
      S, SS: String;
      P: PByteArray;
      V: PWhileView;
      R: TRect;
      T: TEventTimer;
    begin
    LongWorkBegin;
    SS := SearchData.S;
    if SearchData.OPT and 1 = 0 then
      UpStr(SS);
    {--- start -------- Eugeny Zvyagintzev ---- 19-10-2002 ----}
    if  (SearchData.Direction = 2) and (StartSearch) then
      CurRow := 0
    else
      CurRow := Delta.Y;

    if SearchData.Scope = 0 then
      begin
      FirstFld := Delta.X;
      LastFld := Delta.X
      end
    else
      begin
      FirstFld := 0;
      LastFld := DBFile^.NumFields-1;
      end;
    CurFld := Delta.X;
    if not StartSearch then
      if SearchData.Scope = 0 then
        if SearchData.Direction = 1 then
          Dec(CurRow)
        else
          Inc(CurRow)
      else if SearchData.Direction = 1 then
        Dec(CurFld)
      else
        Inc(CurFld);
    {--- finish -------- Eugeny Zvyagintzev ---- 19-10-2002 ----}

    R.Assign(1, 1, 30, 9);
    New(V, Init(R));
    V^.Top := GetString(dlSearching)+' "'+Cut(SearchData.S, 40)+'"';
    V^.Write(1, GetString(dlPercentComplete));
    Desktop^.Insert(V);
    Abort := False;
    NewTimer(T, 50);
    while ((CurRow < DBFile^.NumRec) and (SearchData.Direction <> 1)) or
        ( (SearchData.Direction = 1) and (CurRow >= 0))
    do
      begin
      if TimerExpired(T) then
        begin
        DispatchEvents(V, Abort);
        if Abort then
          Break;
        V^.Write(1, StrGrd(DBFile^.NumRec, CurRow, 30, False));
        V^.Write(2, Percent(DBFile^.NumRec, CurRow)+'%');
        NewTimer(T, 50);
        end;
      P := GetRecord(CurRow);
      while ((CurFld <= LastFld) and (SearchData.Direction <> 1)) or
          ( (SearchData.Direction = 1) and (CurFld >= FirstFld))
      do
        begin
        if Abort then
          Break;
        PFR := DBFile^.GetFieldRec(CurFld);
        Move(P^[PFR^.Pos], S[1], PFR^.len);
        S[0] := Char(PFR^.len);

        if PFR^.Who = 'D' then
          {John_SW  10-10-2002}
          S := DateToHuman(S); {We have to convert date field}

        if SS = '' then
          {John_SW  14-11-2002}
          S := DelSpaces(S);
        {String to find is empty-searching empty field}

        XLatStr(S, XCoder.XLatCP[SearchData.OPT and 1]);
        if  (System.Pos(SS, S) > 0) or (SS = S) then
          begin
          Delta.Y := CurRow;
          Delta.X := CurFld;
          DrawView;
          V^.Free;
          LongWorkEnd;
          Exit;
          end;
        if SearchData.Direction = 1 then
          Dec(CurFld)
        else
          Inc(CurFld);
        end;
      if SearchData.Direction = 1 then
        CurFld := LastFld {John_SW 20-10-2002}
      else
        CurFld := FirstFld; {John_SW 20-10-2002}
      if SearchData.Direction = 1 then
        Dec(CurRow)
      else
        Inc(CurRow);
      end;
    V^.Free;
    if not Abort then
      ErrMsg(dlDBViewSearchNot);
    LongWorkEnd;
    end { ContSearch };

  procedure StartSearch;
    begin
    if ExecResource(dlgDbFind, SearchData) <> cmOK then
      Exit;
    ContSearch(True);
    end;

  { ------ Kirill Vodonosov Start -------}
  function ReOpenInWriteMode : boolean;
  var
    RetValue : boolean;
  begin
    RetValue := false;
    repeat
      case DBFile^.WriteMode of
        0 : begin
              DBFile^.BaseFile.Done;
              DBFile^.OpenMode(stOpen);
              Inc(DBFile^.WriteMode);
              if DBFile^.BaseFile.Status = stOK then Inc(DBFile^.WriteMode)
              else DBFile^.OpenMode(stOpenRead);
            end;
        1 : begin
              ErrMsg(dlDBCantEdit);
              break;
            end;
        2 : RetValue := true;
      end {case};
    until RetValue;
    ReOpenInWriteMode := RetValue;
  end;

  procedure InsertRecord;
  var
    BaseRec : LongInt;
    CurRec : LongInt;
    p : PByteArray;
  begin
    if not ReOpenInWriteMode then Exit;
    BaseRec := Delta.Y;
    CurRec := DBFile^.NumRec-1;
    p := GetRecord(BaseRec);

    while CurRec >= BaseRec do begin
      DBFile^.Seek(CurRec);
      DBFile^.BaseFile.Read(p^[0],DBFile^.RecLen);
      DBFile^.Seek(CurRec);   { TBufStream bug ? }
      DBFile^.Seek(CurRec+1); { если перед этим не сделать Seek(CurRec),        }
                              { то позиционирование идёт на один байт дальше    }
                              { чем нужо. Почему - так и не понял. Особенность. }
      DBFile^.BaseFile.Write(p^[0], DBFile^.RecLen);
      dec(CurRec);
    end;
    FillChar(p^[0], DBFile^.RecLen, ' ');
    DBFile^.Seek(BaseRec);
    DBFile^.BaseFile.Write(p^[0], DBFile^.RecLen);

    inc(DBFile^.NumRec);
    DBFile^.BaseFile.Seek(0);
    DBFile^.BaseFile.Write(DBFile^.Date,32);
    StartRec := DBFile^.NumRec; { for refresh records buffer }
    p := GetRecord(BaseRec);
  end;

  procedure MoveRecordUpDown(UpFlag : boolean);
  var
    BaseRec : LongInt;
    i : LongInt;
    b : byte;
  begin
    if (UpFlag and (Delta.Y <= 0)) or
       ((not UpFlag) and (Delta.Y >= (DBFile^.NumRec-1))) then
      Exit;
    if NumRec < 2 then Exit;
    if not ReOpenInWriteMode then Exit;
    BaseRec := Delta.Y;
    if UpFlag then dec(BaseRec);
    DBFile^.Seek(BaseRec);
    DBFile^.BaseFile.Read(Buf^, DBFile^.RecLen * 2);
    for i := 0 to DBFile^.RecLen - 1 do begin
      b := PByteArray(Buf)^[i];
      PByteArray(Buf)^[i] := PByteArray(Buf)^[i + DBFile^.RecLen];
      PByteArray(Buf)^[i + DBFile^.RecLen] := b;
    end;
    DBFile^.Seek(BaseRec);
    DBFile^.BaseFile.Write(Buf^, DBFile^.RecLen * 2);
    StartRec := DBFile^.NumRec; { for refresh records buffer }
    if UpFlag then dec(Delta.Y) else inc(Delta.Y);
    GetRecord(Delta.Y);
  end;
  { ------- Kirill Vodonosov End --------}

  { -------- Eugeny Zvyagintzev ---------}

  procedure DeleteRecord;
    var
      p: PByteArray;
    begin
    if not ReOpenInWriteMode then Exit; { KV }
    p := GetRecord(Delta.Y);
    if p^[0] = $2A then
      p^[0] := $20
    else
      p^[0] := $2A;
    DBFile^.Seek(Delta.Y);
    DBFile^.BaseFile.Write(p^[0], DBFile^.RecLen);
    end { DeleteRecord };

  { -------- Eugeny Zvyagintzev ---------}

  procedure EditField;
    var
      P: PChar;
      PFR: PFieldRec;
      S: String;
      R: Real;
      I: Integer;
      MM, DD, YY: Word;
      II, N1, N2, N3: Word;
      LL: array[0..3] of LongInt;
      FldLen: AWord; { Kirill }

    procedure StoreField;
      begin
      S := AddSpace(S, FldLen);
      XLatStr(S, XCoder.XlatCP[FromAscii]); {John_SW 14-03-2003}
      Move(S[1], P[PFR^.Pos], FldLen);
      DBFile^.Seek(Delta.Y);
      DBFile^.BaseFile.Seek(DBFile^.BaseFile.GetPos+PFR^.Pos);
      DBFile^.BaseFile.Write(S[1], FldLen);
      end;

    function GetDig: Word;
      var
        D: String;
      begin
      D := '';
      while (II <= Length(S)) and ((S[II] < '0') or (S[II] > '9')) do
        Inc(II);
      while (II <= Length(S)) and ((S[II] >= '0') and (S[II] <= '9')) do
        begin
        AddStr(D, S[II]);
        Inc(II);
        end;
      GetDig := StoI(D);
      end;

    label redat;
    begin { EditField }
    if not ReOpenInWriteMode then Exit; { KV }
    PFR := DBFile^.GetFieldRec(Delta.X);
    if PFR^.Who = 'M' then
      Exit;
    P := GetRecord(Delta.Y);
    FldLen := PFR^.len;
    if FldLen > 252 then
      { Больше чем 252 нельзя, иначе будет вылетать  }
      FldLen := 252; { Kirill }
    { Здесь необходимо вывести предупреждение }
    { о том, что будут отредактированы только }
    { первые 252 символа длинного поля.       }
    Move(P[PFR^.Pos], S[1], FldLen);
    S[0] := Char(FldLen); { Kirill }
    if PFR^.Who <> 'D' then
      begin
      if PFR^.Who <> 'C' then
        DelLeft(S);
      DelRight(S); {Oleg Redut}
      end;
    XLatStr(S, XCoder.XLatCP[ToAscii]); {John_SW 14-03-2003}
    case PFR^.Who of
      'C', 'N':
        if InputBox(GetString(dlDBEditField), GetString(dlDBValue), S,
             FldLen, hsEditDBF) = cmOK
        then
          begin
          if PFR^.Who = 'N' then
            begin
            DelSpace(S);
            Val(S, R, I);
            Str(R:
              FldLen:
              PFR^.Dec, S);
            S := PredSpace(S, FldLen);
            end;
          StoreField;
          end;
      'D':
        begin
        S := DateToHuman(S); {!}
redat:
        if InputBox(GetString(dlDBEditField), GetString(dlDBValue), S,
             10, hsEditDBF) = cmOK
        then
          begin
          II := 1;
          N1 := GetDig;
          N2 := GetDig;
          N3 := GetDig;
          GetDate(II, MM, DD, YY);
          case CountryInfo.DateFmt of
            dfMDY:
              begin
              MM := N1;
              DD := N2;
              YY := N3;
              end;
            dfDMY:
              begin
              MM := N2;
              DD := N1;
              YY := N3;
              end;
            dfYMD:
              begin
              MM := N2;
              DD := N3;
              YY := N1;
              end;
          end {case};
          if  ( (MM = 0) and (DD = 0) and (YY = 0)) then
            S := ' '+' '+' '+' '+' '+' '+' '+' ' {8 spaces!}
          else
            begin
            if YY < 100 then
              YY := (II div 100)*100+YY;
            if  (MM < 1) or (MM > 12) or (DD < 1) or
                (DD > 31) or ((DD = 31) and (MM in [4, 6, 9, 11])) or
                ( (MM = 2) and ((DD = 30) or ((DD = 29) and (YY mod 4 <>
                       0))))
            then
              goto redat;
            LL[0] := YY mod 10000;
            LL[1] := MM mod 100;
            LL[2] := DD mod 100;
            FormatStr(S, '%04d%02d%02d', LL);
            end;
          StoreField;
          end;
        end;
      'L':
        begin
        if UpStrg(S) = 'T' then
          S := 'F'
        else
          S := 'T';
        StoreField;
        end;
    end {case};
    end { EditField };

  procedure CE;
    begin
    ClearEvent(Event)
    end;
  procedure CED;
    begin
    CE;
    DrawView
    end;

  var
    F: lFile; {DataCompBoy}
    P: TPoint;
    RD: Word;
    TempPath: String;

  begin { TDBViewer.HandleEvent }
  inherited HandleEvent(Event);
  case Event.What of
    evBroadcast:
      case Event.Command of
        cmFindView:
          if PString(Event.InfoPtr)^ = FileName^ then
            begin
            if Owner <> nil then
              begin
              Owner^.Select; { Flash }
              ClearEvent(Event);
              end;
            end;
      end {case};
    evCommand:
      case Event.Command of
        cmReboundPanel:
          DrawView; {John_SW 10-10-2002 after country settings changed}
        cmClose, cmKillUsed:
          if KillAfterUse then
            begin
            if DBFile <> nil then
              begin
              if Buf <> nil then
                FreeMem(Buf, NumRec*DBFile^.RecLen);
              Dispose(DBFile, Done);
              Buf := nil;
              DBFile := nil;
              end;
            EraseTempFile(FileName^);
            KillAfterUse := False;
            TempPath := GetPath(FileName^);
            if Event.Command = cmClose then
              GlobalMessage(evCommand, cmPanelReread, @TempPath);
            end;
        cmGetName:
          PString(Event.InfoPtr)^:= GetString(dlDBViewName)+' - '
            +Cut(FileName^, 49-Length(GetString(dlDBViewName)));
        cmShowFields:
          begin
          GetInfo(dlDBViewInfo);
          CE
          end;
        cmShowMemo:
          begin
          ViewMemo;
          CE
          end;
        cmDBFGoto:
          begin
          GotoRecFld;
          CE;
          end;
        cmSearchFor:
          begin
          StartSearch;
          CE;
          end;
        cmContinueSearch:
          begin
          ContSearch(False);
          CE;
          end;
        cmBackSearch:
          begin
          if SearchData.Direction = 1 then
            begin
            SearchData.Direction := 0;
            ContSearch(False);
            SearchData.Direction := 1;
            end
          else
            begin
            RD := SearchData.Direction;
            SearchData.Direction := 1;
            ContSearch(False);
            SearchData.Direction := RD;
            end;
          CE;
          end;
        cmEditDBField:
          begin
          EditField;
          CED
          end;
        cmDeleteDBRec:
          begin
          DeleteRecord;
          CED;
          end;
        cmInsertDBRec:
          begin
          InsertRecord;
          CED;
          end;
        cmUpDBRec:
          begin
          MoveRecordUpDown(true);
          CED;
          end;
        cmDownDBRec:
          begin
          MoveRecordUpDown(false);
          CED;
          end;
        cmSwitchKeyMapping:
          begin
          XCoder.NextXLat;
          CED;
          Indicator^.Draw;
          end; {John_SW 14-03-2003}
        cmLoadXlatTable:
          begin
          XCoder.LoadXlatTable;
          Indicator^.Draw;
          CED;
          end; {John_SW 18-03-2003}
      end {case};
    evKeyDown:
      case Event.KeyCode of
        kbEnter:
          begin
          if Delta.X < DBFile^.NumFields-1 then
            Inc(Delta.X)
          else
            begin
            Inc(Delta.Y);
            Delta.X := 0;
            end;
          CED;
          end;
        kbLeft:
          begin
          if Delta.X > 0 then
            Dec(Delta.X);
          CED;
          end;
        kbRight:
          begin
          if Delta.X < DBFile^.NumFields-1 then
            Inc(Delta.X);
          CED;
          end;
        kbHome:
          begin
          Delta.X := 0;
          CED;
          end;
        kbEnd:
          begin
          Delta.X := DBFile^.NumFields-1;
          CED;
          end;
        kbUp:
          begin
          Dec(Delta.Y);
          CED;
          end;
        kbDown:
          begin
          Inc(Delta.Y);
          CED;
          end;
        kbPgUp:
          begin
          Dec(Delta.Y, Size.Y-2);
          CED;
          end;
        kbPgDn:
          begin
          Inc(Delta.Y, Size.Y-2);
          CED;
          end;
        kbCtrlPgUp:
          begin
          Delta.Y := 0;
          CED;
          end;
        kbCtrlPgDn:
          begin
          Delta.Y := DBFile^.NumRec-1;
          CED;
          end;
        {              kbCtrlD: begin DeleteRecord; CED; end;}
        kbESC:
          begin
          Message(Owner, evCommand, cmClose, nil);
          CE
          end;
      end {case};
    evMouseDown:
      begin
      RD := RepeatDelay;
      RepeatDelay := 0;
      repeat
        MakeLocal(Event.Where, P);
        if MouseInView(Event.Where) then
          if P.X < Size.X div 4 then
            Message(@Self, evKeyDown, kbLeft, nil)
          else if P.X >= (Size.X*3) div 4 then
            Message(@Self, evKeyDown, kbRight, nil)
          else if P.Y < Size.Y div 2 then
            Message(@Self, evKeyDown, kbUp, nil)
          else
            Message(@Self, evKeyDown, kbDown, nil)
      until not MouseEvent(Event, evMouseMove+evMouseAuto);
      RepeatDelay := RD;
      CE
      end;
  end {case};
  end { TDBViewer.HandleEvent };

procedure TDBViewer.Draw;
  var
    B: array[0..1024] of record
      C: Char;
      A: Byte;
      end;
    FN: array[0..132] of PFieldRec;
    C1, C2, C3: Word;
    C: Byte;
    I, J, K, NFN: Integer;
    FldLen: Word; { Kirill }
    FldPart: Byte;
    Idx: LongInt;
    PFR: PFieldRec;
    P: PByteArray;
    S: String;
  begin
  C1 := GetColor(1);
  C2 := GetColor(2);
  C3 := GetColor(3);
  if Delta.Y < 0 then
    Delta.Y := 0;
  if Delta.Y >= DBFile^.NumRec then
    Delta.Y := DBFile^.NumRec-1;
  if Pos.Y > Delta.Y then
    Pos.Y := Delta.Y;
  if Pos.Y+Size.Y-2 < Delta.Y then
    Pos.Y := Delta.Y-Size.Y+2;
  if Pos.Y < 0 then
    Pos.Y := 0;
  if Delta.X > DBFile^.NumFields-1 then
    Delta.X := DBFile^.NumFields-1;
  if Pos.X > Delta.X then
    Pos.X := Delta.X;
  {if Indicator <> nil then Indicator^.Draw;}
  { -------- Eugeny Zvyagintzev ---------}
  if  (VerticalScrollBar <> nil)
       and (VerticalScrollBar^.Value <> Delta.Y)
  then
    begin
    VerticalScrollBar^.SetValue(Delta.Y);
    if Indicator <> nil then
      Indicator^.Draw;
    end;
  if  (HorizontalScrollBar <> nil)
       and (HorizontalScrollBar^.Value <> Delta.X)
  then
    begin
    HorizontalScrollBar^.SetValue(Delta.X);
    if Indicator <> nil then
      Indicator^.Draw;
    end;
  { -------- Eugeny Zvyagintzev ---------}
  repeat
    MoveChar(B, ' ', C1, Size.X);
    J := Pos.X;
    NFN := 0;
    I := 1;
    FldPart := 0;
    while (I < Size.X) and (J < DBFile^.NumFields) do
      begin
      PFR := DBFile^.GetFieldRec(J);
      FN[NFN] := PFR;
      if PFR <> nil then
        begin
        K := PFR^.ln;
        if K > Size.X-I then
          K := Size.X-I; { Kirill }
        MoveStr(B[I], CenterStr(PFR^.Name, {PFR^.Ln}K), C1);
        Inc(I, PFR^.ln+1);
        end;
      if I < Size.X then
        Inc(NFN)
      else
        FldPart := 1;
      Inc(J);
      end;
    if  (Delta.X > Pos.X+NFN-1) and (Pos.X <> Delta.X) then
      Inc(Pos.X)
    else
      Break;
  until False;

  WriteLine(0, 0, Size.X, 1, B);
  for K := 1 to Size.Y-1 do
    begin
    MoveChar(B, ' ', C2, Size.X);
    if DBFile^.NumRec > 0 then
      begin
      J := Delta.X;
      I := 1;
      Idx := Pos.Y+LongInt(K)-1;
      P := GetRecord(Idx);
      {if (NFN = 0) and (P <> nil) then NFN := 1;}
      if P <> nil then
        B[0].C := Char(P^[0]);
      if P <> nil then

        for J := 0 to NFN+FldPart-1 do
          begin
          PFR := FN[J];
          if  (Delta.Y = Idx) and (Pos.X+J = Delta.X) then
            C := C3
          else
            C := C2;
          if PFR <> nil then
            if PFR^.Who = 'M' then
              begin
              Move(P^[PFR^.Pos], S[1], PFR^.len);
              S[0] := Char(PFR^.len);
              if StoI(S) > 0 then
                S := '  Memo  '
              else
                S := '  memo  ';
              MoveStr(B[I], S, C);
              Inc(I, PFR^.ln+1);
              end
            else if PFR^.Who = 'D' then
              begin
              Move(P^[PFR^.Pos], S[1], PFR^.len);
              S[0] := Char(PFR^.len);
              MoveStr(B[I], DateToHuman(S), C);
              Inc(I, PFR^.ln+1);
              end
            else
              begin { Kirill }
              FldLen := PFR^.len;
              if FldLen > Size.X-I then
                FldLen := Size.X-I;
              Move(P^[PFR^.Pos], S[1], FldLen);
              S[0] := Char(FldLen);
              XLatStr(S, XCoder.XLatCP[ToAscii]); {John_SW 14-03-2003}
              MoveStr(B[I], S, C);
              Inc(I, PFR^.ln+1);
              end;
          end;
      end;
    WriteLine(0, K, Size.X, 1, B);
    end;
  end { TDBViewer.Draw };

procedure TDBIndicator.Draw;
  var
    B: TDrawBuffer;
    S, S1: String; {John_SW 18-03-2003}
    C: Byte;
    PFR: PFieldRec;
  begin
  {10171114/10171114═══[C 10,10]═WIN═}
  if  (DBViewer = nil) or (DBViewer^.DBFile = nil) then
    Exit;
  if DBViewer^.Delta.Y < 0 then
    DBViewer^.Delta.Y := 0;
  if DBViewer^.Delta.X > DBViewer^.DBFile^.NumFields-1 then
    DBViewer^.Delta.X := DBViewer^.DBFile^.NumFields-1;
  if Origin.Y <> Owner^.Size.Y-1 then
    MoveTo(Origin.X, Owner^.Size.Y-1);
  PFR := DBViewer^.DBFile^.GetFieldRec(DBViewer^.Delta.X);
  if  (State and sfDragging <> 0) or (State and sfActive = 0) then
    C := 196
  else
    C := 205;
  S := AddSpace
        (ItoS(DBViewer^.Delta.Y+1)+'/'+ItoS(DBViewer^.DBFile^.NumRec), 10);
  Replace(' ', Char(C), S);
  S1 := AddSpace(ItoS(PFR^.len)+','+ItoS(PFR^.Dec)+']', 7);
  Replace(' ', Char(C), S1);
  S := S+Char(C)+'['+PFR^.Who+' '+S1;
  S := S+Char(C)+DBViewer^.XCoder.CodeTag;
  S := Copy(S, 1, Owner^.Size.X-4);
  if Byte(S[0]) <> Size.X then
    begin
    GrowTo(Byte(S[0]), 1);
    Exit;
    end;
  {--- start -------- Eugeny Zvyagintzev ---- 14-03-2003 -----}
  if State and sfDragging <> 0 then
    C := PWindow(Owner)^.Frame^.GetColor($05)
  else if State and sfActive = 0 then
    C := PWindow(Owner)^.Frame^.GetColor($01)
  else
    C := PWindow(Owner)^.Frame^.GetColor($03);
  {--- finish -------- Eugeny Zvyagintzev ---- 14-03-2003 -----}
  MoveStr(B, S, C);
  WriteLine(0, 0, Size.X, 1, B);
  end { TDBIndicator.Draw };

constructor TDBWindow.Init;
  var
    R: TRect;
    I: Integer;
    s: String;
  begin
  Desktop^.GetExtent(R);
  I := PosChar('|', FName);
  if I > 0 then
    begin
    s := Copy(FName, 1, I-1);
    FName := Copy(FName, I+1, 255);
    end;
  FName := lFExpand(FName);
  if I <= 0 then
    s := FName;
  TWindow.Init(R, s, 0);
  RealName := s;
  Options := Options or ofTileable;
  Flags := Flags or wfMaxi;
  GetExtent(R);
  R.Grow(-1, -1);
  P := New(PDBViewer, Init(R, FName, FileIsDBF));
  if P = nil then
    Fail;
  Insert(P);
  if not P^.isValid then
    begin
    Done;
    Fail;
    end;
  { -------- Eugeny Zvyagintzev ---------}
  R.A.X := R.B.X;
  R.B.X := R.B.X+1;
  VSB := New(PDBScrollBar, Init(R));
  VSB^.Max := P^.DBFile^.NumRec-1;
  VSB^.GrowMode := gfGrowLoX+gfGrowHiX+gfGrowHiY;
  VSB^.DBViewer := P;
  VSB^.SetStep(Size.Y-4, 1);
  VSB^.ScrollBarType := sbVertical;
  Insert(VSB);
  P^.VerticalScrollBar := VSB;
  GetBounds(R);
  R.A.X := R.A.X+49;
  R.B.X := R.B.X-2;
  R.A.Y := R.B.Y-1;
  HSB := New(PDBScrollBar, Init(R));
  HSB^.Max := P^.DBFile^.NumFields-1;
  HSB^.GrowMode := gfGrowLoY+gfGrowHiX+gfGrowHiY;
  HSB^.DBViewer := P;
  HSB^.ScrollBarType := sbHorizontal;
  Insert(HSB);
  P^.HorizontalScrollBar := HSB;
  { -------- Eugeny Zvyagintzev ---------}
  R.A.X := 2;
  R.B.X := 34;
  R.A.Y := Size.Y-1;
  R.B.Y := Size.Y;
  P1 := New(PDBIndicator, Init(R));
  P1^.DBViewer := P;
  P1^.GrowMode := gfGrowLoY+gfGrowHiX+gfGrowHiY;
  Insert(P1);
  P^.Indicator := P1;
  end { TDBWindow.Init };

constructor TDBWindow.Load;
  begin
  inherited Load(S);
  GetSubViewPtr(S, P);
  GetSubViewPtr(S, P1);
  P1^.DBViewer := P;
  P^.Indicator := P1;
  GetSubViewPtr(S, VSB);
  VSB^.DBViewer := P;
  VSB^.ScrollBarType := sbVertical;
  P^.VerticalScrollBar := VSB;
  GetSubViewPtr(S, HSB);
  HSB^.DBViewer := P;
  HSB^.ScrollBarType := sbHorizontal;
  P^.HorizontalScrollBar := HSB;

  Redraw;
  end;

procedure TDBWindow.Store;
  begin
  inherited Store(S);
  PutSubViewPtr(S, P);
  PutSubViewPtr(S, P1);
  PutSubViewPtr(S, VSB);
  PutSubViewPtr(S, HSB);
  end;

{--- start -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}
procedure TDBWindow.SetState(AState: Word; Enable: Boolean);
  begin
  inherited SetState(AState, Enable);
  if P1 <> nil then
    begin
    P1^.SetState(AState, Enable);
    P1^.Draw;
    end;
  end;
{--- finish -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}

destructor TDBWindow.Done;
  begin
  StoreViewInfo(@Self);
  inherited Done;
  end;



end.
