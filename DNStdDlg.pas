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
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

unit DNStdDlg;

interface

uses
  FilesCol, Collect, Defines, Streams, Drivers, Views, Dialogs
  ;

const

  { Commands }

  cmFileOpen = 800; { Returned from TFileDialog when Open pressed }
  cmFileReplace = 801;
  { Returned from TFileDialog when Replace pressed }
  cmFileClear = 802; { Returned from TFileDialog when Clear pressed }
  cmFileInit = 803; { Used by TFileDialog internally }
  (*cmChangeDir   = 804;   { Used by TChDirDialog internally }*)
  cmRevert = 805; { Used by TChDirDialog internally }

  { Messages }

  cmFileFocused = 806; { A new file was focused in the TFileList }
  cmFileDoubleClicked { A file was selected in the TFileList }
  = 807;

type

  { TSearchRec }

  {  Record used to store directory information by TFileDialog }

  {DataCompBoy}
  TSearchRec = record
    Attr: Byte;
    Time: LongInt;
    Size: TSize;
    Name: String;
    end;
  {DataCompBoy}

type

  { TFileInputLine is a special input line that is used by      }
  { TFileDialog that will update its contents in response to a  }
  { cmFileFocused command from a TFileList.                     }

  PFileInputLine = ^TFileInputLine;
  TFileInputLine = object(TInputLine)
    constructor Init(var Bounds: TRect; AMaxLen: Integer);
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  { TFileCollection is a collection of TSearchRec's.            }

  PFileCollection = ^TFileCollection;
  TFileCollection = object(TSortedCollection)
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    end;

  { TSortedListBox is a TListBox that assumes it has a          }
  { TSortedCollection instead of just a TCollection.  It will   }
  { perform an incremental search on the contents.              }

  PSortedListBox = ^TSortedListBox;
  TSortedListBox = object(TListBox)
    SearchPos: Word;
    constructor Init(var Bounds: TRect; ANumCols: Word;
        AScrollBar: PScrollBar);
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetKey(var S: String): Pointer; virtual;
    procedure NewLisT(AList: PCollection); virtual;
    end;

  { TFileList is a TSortedList box that assumes it contains     }
  { a TFileCollection as its collection.  It also communicates  }
  { through broadcast messages to TFileInput and TInfoPane      }
  { what file is currently selected.                            }

  PFileList = ^TFileList;
  TFileList = object(TSortedListBox)
    constructor Init(var Bounds: TRect; const AWildCard: String;
        {DataCompBoy}
        AScrollBar: PScrollBar);
    destructor Done; virtual;
    function DataSize: Word; virtual;
    procedure FocusItem(Item: LongInt); virtual;
    function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    function GetKey(var S: String): Pointer; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure ReadDirectory(AWildCard: String);
    procedure SetData(var Rec); virtual;
    procedure Select; virtual;
    end;

  { TDirectoryList }
  PDirectoryList = ^TDirectoryList;
  TDirectoryList = object(TFileList)
    function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    function GetKey(var S: String): Pointer; virtual;
    procedure ReadDirectory(AWildCard: String);
    end;

  { TFileInfoPane is a TView that displays the information      }
  { about the currently selected file in the TFileList          }
  { of a TFileDialog.                                           }

  PFileInfoPane = ^TFileInfoPane;
  TFileInfoPane = object(TView)
    S: TSearchRec; {DataCompBoy}
    constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  { TFileDialog is a standard file name input dialog            }

const
  fdOKButton = $0001; { Put an OK button in the dialog }
  fdOpenButton = $0002; { Put an Open button in the dialog }
  fdReplaceButton = $0004; { Put a Replace button in the dialog }
  fdClearButton = $0008; { Put a Clear button in the dialog }
  fdHelpButton = $0010; { Put a Help button in the dialog }
  fdNoLoadDir = $0100; { Do not load the current directory }
  { contents into the dialog at Init. }
  { This means you intend to change the }
  { WildCard by using SetData or store }
  { the dialog on a stream. }

type

  PFileDialog = ^TFileDialog;
  TFileDialog = object(TDialog)
    FileName: PFileInputLine;
    FileList: PFileList;
    DirList: PDirectoryList;
    WildCard: PString; {DataCompBoy}
    Directory: PString;
    constructor Init(const AWildCard: String; ATitle: String;
        InputName: String; AOptions: Word; HistoryId: Byte);
    {DataCompBoy}
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure GetData(var Rec); virtual;
    procedure GetFileName(var S: String); {DataCompBoy}
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
    function Valid(Command: Word): Boolean; virtual;
  private
    procedure ReadDirectory;
    end;

  { TDirEntry }

  PDirEntry = ^TDirEntry;
  TDirEntry = record
    DisplayText: PString;
    Directory: PString;
    end;

  { TDirCollection is a collection of TDirEntry's used by       }
  { TDirListBox.                                                }

  PDirCollection = ^TDirCollection;
  TDirCollection = object(TCollection)
    function GetItem(var S: TStream): Pointer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    end;

const

  CInfoPane = #30;

function GetFileNameDialog(Mask, Title, Name: String;
     Buttons, HistoryId: Word): String; {DataCompBoy}
function GetFileNameMenu(Path, Mask, Default: String;
     PutNumbers: Boolean; var More, None: Boolean): String; {JO}
{DataCompBoy: Mask now an standard DN filter, like "*.Ops;-Pas.*;p*.d"}

implementation

uses
  Lfn, Dos, Memory, Messages, DNApp,
  HistList, Advance, Advance1, Advance2, Commands,
  Startup, DNHelp, {JO}Menus {JO}
  ;

{-DataCompBoy-}
function ValidFileName(var FileName: String): Boolean;
  var
    Dir: String;
    Name: String;
    Ext: String;

    { Contains returns true if S1 contains any characters in S2 }
  function Contains(const S1, S2: String): Boolean;
    {$IFNDEF NOASM}
    near;
    assembler;
    {$Frame-} {$USES ESI, EDI, EDX, EBX, ECX}
  asm
        CLD
        MOV     ESI,S1
        MOV     EDI,S2
        MOV     EDX,EDI
        XOR     EAX,EAX
        LODSB
        MOV     EBX,EAX
        OR      EBX,EBX
        JZ      @@2
        MOV     AL,[EDI]
        XCHG    EAX,ECX
 @@1:   PUSH    ECX
        MOV     EDI,EDX
        LODSB
        REPNE   SCASB
        POP     ECX
        JE      @@3
        DEC     EBX
        JNZ     @@1
 @@2:   XOR     AL,AL
        JMP     @@4
 @@3:   MOV     AL,1
 @@4:
 end;
    {$ELSE}
    var
      q: Integer;
    begin { Contains }
    Contains := True;
    for q := 1 to Length(S2) do
      if PosChar(S2[q], S1) > 0 then
        Exit;
    Contains := False;
    end { Contains };
  {$ENDIF}

  begin { ValidFileName }
  ValidFileName := True;
  lTrueName(FileName, FileName);
  lFSplit(FileName, Dir, Name, Ext);
  if not ((Dir = '') or PathExist(Dir)) or Contains(Name, IllegalChars) or
    Contains(Dir, IllegalChars)
  then
    ValidFileName := False;
  lTrueName(FileName, Dir);
  if FileName <> Dir then
    ValidFileName := False;
  end { ValidFileName };
{-DataCompBoy-}

{-DataCompBoy-}
{DataCompBoy
function GetCurDir: String;
var
  CurDir: String;
begin
  lGetDir(0, CurDir);
  if Length(CurDir) > 3 then CurDir:=CurDir+'\';
  GetCurDir := CurDir;
end;
}
{-DataCompBoy-}

type
  PSearchRec = ^TSearchRec; {DataCompBoy}

function IsWild(var S: String): Boolean;
  begin
  IsWild := (Pos('?', S) > 0) or (Pos('*', S) > 0);
  end;

{ TFileInputLine }

constructor TFileInputLine.Init(var Bounds: TRect; AMaxLen: Integer);
  begin
  TInputLine.Init(Bounds, AMaxLen);
  EventMask := EventMask or evBroadcast;
  end;

{-DataCompBoy-}
procedure TFileInputLine.HandleEvent(var Event: TEvent);
  var
    Dir: String;
    Name: String;
    Ext: String;
  begin
  TInputLine.HandleEvent(Event);
  if  (Event.What = evBroadcast) and (Event.Command = cmFileFocused) and
      (State and sfSelected = 0)
  then
    begin
    Dir := PSearchRec(Event.InfoPtr)^.Name+'\';
    if PSearchRec(Event.InfoPtr)^.Attr and Directory <> 0 then
      Data := Dir+
        PFileDialog(Owner)^.WildCard^
    else
      Data := PSearchRec(Event.InfoPtr)^.Name;
    DrawView;
    end;
  end;
{-DataCompBoy-}

{ TFileCollection }

{-DataCompBoy-}
function TFileCollection.Compare(Key1, Key2: Pointer): Integer;
  var
    K1: PSearchRec absolute Key1;
    K2: PSearchRec absolute Key2;
    S1, S2: String;
  begin
  S1 := UpLowStrg(K1^.Name);
  S2 := UpLowStrg(K2^.Name);
  if S1 = S2 then
    Compare := 0
  else if S1[2] = ':' then
    Compare := +1
  else if S2[2] = ':' then
    Compare := -1
  else if S1 = '..' then
    Compare := -1
  else if S2 = '..' then
    Compare := 1
  else if (K1^.Attr and Directory <> 0) and
      (K2^.Attr and Directory = 0)
  then
    Compare := 1
  else if (K2^.Attr and Directory <> 0) and
      (K1^.Attr and Directory = 0)
  then
    Compare := -1
  else if S1 > S2 then
    Compare := 1
  else
    Compare := -1;
  end { TFileCollection.Compare };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileCollection.FreeItem(Item: Pointer);
  begin
  Dispose(PSearchRec(Item));
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TFileCollection.GetItem(var S: TStream): Pointer;
  var
    Item: PSearchRec;
    B: Byte;
  begin
  New(Item);
  S.Read(Item^, SizeOf(TSearchRec));
  GetItem := Item;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileCollection.PutItem(var S: TStream; Item: Pointer);
  begin
  S.Write(Item^, SizeOf(TSearchRec));
  end;
{-DataCompBoy-}


{ TSortedListBox }

constructor TSortedListBox.Init(var Bounds: TRect; ANumCols: Word;
    AScrollBar: PScrollBar);
  begin
  TListBox.Init(Bounds, ANumCols, AScrollBar);
  SearchPos := 0;
  ShowCursor;
  SetCursor(1, 0);
  end;

procedure TSortedListBox.HandleEvent(var Event: TEvent);
  var
    CurString, NewString: String;
    K: Pointer;
    Value, OldPos, OldValue: LongInt;
    T: Boolean;

  function Equal(var S1: String; var S2: String; Count: Word): Boolean;
    var
      I: Word;
    begin
    Equal := False;
    if  (Length(S1) < Count) or (Length(S2) < Count) then
      Exit;
    for I := 1 to Count do
      if UpCase(S1[I]) <> UpCase(S2[I]) then
        Exit;
    Equal := True;
    end;

  begin
  OldValue := Focused;
  TListBox.HandleEvent(Event);
  if OldValue <> Focused then
    SearchPos := 0;
  if Event.What = evKeyDown then
    begin
    if Event.CharCode <> #0 then
      begin
      Value := Focused;
      if Value < Range then
        CurString := GetText(Value, 255)
      else
        CurString := '';
      OldPos := SearchPos;
      if Event.KeyCode = kbBack then
        begin
        if SearchPos = 0 then
          Exit;
        Dec(SearchPos);
        SetLength(CurString, SearchPos);
        end
      else if (Event.CharCode = '.') then
        SearchPos := Pos('.', CurString)
      else
        begin
        Inc(SearchPos);
        SetLength(CurString, SearchPos);
        CurString[SearchPos] := Event.CharCode;
        end;
      K := GetKey(CurString);
      if List <> nil then
        T := PSortedCollection(List)^.Search(K, Value)
      else
        Value := Range;
      if Value < Range then
        begin
        if Value < Range then
          NewString := GetText(Value, 255)
        else
          NewString := '';
        if Equal(NewString, CurString, SearchPos) then
          begin
          if Value <> OldValue then
            begin
            FocusItem(Value);
            { Assumes ListControl will set the cursor to the first character }
            { of the sfFocused item }
            SetCursor(Cursor.X+SearchPos, Cursor.Y);
            end
          else
            SetCursor(Cursor.X+(SearchPos-OldPos), Cursor.Y);
          end
        else
          SearchPos := OldPos;
        end
      else
        SearchPos := OldPos;
      if  (SearchPos <> OldPos)
           or (Event.CharCode in ['A'..'Z', 'a'..'z'])
      then
        ClearEvent(Event);
      end;
    end;
  end { TSortedListBox.HandleEvent };

function TSortedListBox.GetKey(var S: String): Pointer;
  begin
  GetKey := @S;
  end;

procedure TSortedListBox.NewLisT(AList: PCollection);
  begin
  TListBox.NewLisT(AList);
  SearchPos := 0;
  end;

{ TFileList }

{-DataCompBoy-}
constructor TFileList.Init(var Bounds: TRect; const AWildCard: String;
    AScrollBar: PScrollBar);
  begin
  TSortedListBox.Init(Bounds, 1, AScrollBar);
  end;
{-DataCompBoy-}

destructor TFileList.Done;
  begin
  if List <> nil then
    Dispose(List, Done);
  List := nil;
  TListBox.Done;
  end;

procedure TFileList.Select;
  var
    Event: TEvent;
  begin
  if List^.Count > 0
  then
    begin
    Event.What := evBroadcast;
    Event.Command := cmFileFocused;
    Event.InfoPtr := List^.At(Focused);
    Owner^.HandleEvent(Event);
    inherited Select;
    end
  else {Owner^.SelectNext(False)}
    ;
  end;

function TFileList.DataSize: Word;
  begin
  DataSize := 0;
  end;

procedure TFileList.FocusItem(Item: LongInt);
  begin
  TSortedListBox.FocusItem(Item);
  Message(Owner, evBroadcast, cmFileFocused, List^.At(Item));
  end;

{-DataCompBoy-}
function TFileList.GetKey(var S: String): Pointer;
  const
    SR: TSearchRec = ();
  begin
  SR.Name := S;
  SR.Attr := 0;
  UpStr(SR.Name);
  GetKey := @SR;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TFileList.GetText(Item: LongInt; MaxLen: Integer): String;
  var
    SR: PSearchRec;
    S: String;
  begin
  SR := PSearchRec(List^.At(Item));
  if IsMixedCase(SR^.Name) then
    S := SR^.Name
  else
    S := LowStrg(SR^.Name);
  if Length(S) > 13 then
    begin
    SetLength(S, 13);
    S[13] := FMSetup.RestChar[1];
    end;
  GetText := S;
  end;
{-DataCompBoy-}

procedure TFileList.HandleEvent(var Event: TEvent);
  begin
  if  (Event.What = evMouseDown) and (Event.Double) then
    begin
    Event.What := evCommand;
    Event.Command := cmOK;
    PutEvent(Event);
    ClearEvent(Event);
    end
  else
    TSortedListBox.HandleEvent(Event);
  end;

{-DataCompBoy-}
procedure TFileList.ReadDirectory(AWildCard: String);
  const
    FindAttr = ReadOnly+Archive+SysFile;
  var
    S: lSearchRec;
    P: PSearchRec;
    FileList: PFileCollection;
    Dir: String;
    Name: String;
    Ext: String;
    Event: TEvent;
  begin
  AWildCard := lFExpand(AWildCard);
  lFSplit(AWildCard, Dir, Name, Ext);
  FileList := New(PFileCollection, Init(5, 5));
  lFindFirst(AWildCard, FindAttr + Byte(not Security)*Hidden, S);
  P := @P;
  while (P <> nil) and (DosError = 0) do
    begin
    if  (S.SR.Attr and Directory = 0) then
      begin
      P := MemAlloc(SizeOf(P^));
      if P <> nil then
        begin
        with P^ do
          begin
          Attr := S.SR.Attr;
          Time := S.SR.Time;
          Size := S.FullSize;
          Name := S.SR.Name;
          end;
        FileList^.Insert(P);
        end;
      end;
    lFindNext(S);
    end;
  lFindClose(S);
  if P = nil then
    Msg(dlStdDlgManyFiles, nil, mfOKButton+mfWarning);
  NewLisT(FileList);
  {  if List^.Count > 0 then
  begin
    Event.What := evBroadcast;
    Event.Command := cmFileFocused;
    Event.InfoPtr := List^.At(0);
    Owner^.HandleEvent(Event);
  end;  }
  end { TFileList.ReadDirectory };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileList.SetData(var Rec);
  begin
  {with PFileDialog(Owner)^ do}
  ReadDirectory
    (PFileDialog(Owner)^.Directory^+PFileDialog(Owner)^.WildCard^);
  end;
{-DataCompBoy-}


{ TDirectoryList }

{-DataCompBoy-}
function TDirectoryList.GetKey(var S: String): Pointer;
  const
    SR: TSearchRec = ();
  begin
  SR.Name := S;
  SR.Attr := Directory;
  UpStr(SR.Name);
  GetKey := @SR;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TDirectoryList.GetText(Item: LongInt; MaxLen: Integer): String;
  var
    SR: PSearchRec;
    S: String;
  begin
  SR := PSearchRec(List^.At(Item));
  if IsMixedCase(SR^.Name) then
    S := SR^.Name
  else
    S := UpStrg(SR^.Name);
  if Length(S) > 12 then
    begin
    SetLength(S, 12);
    S[12] := FMSetup.RestChar[1];
    end;
  if SR^.Name[2] <> ':' then
    GetText := S+'\'
  else
    GetText := SR^.Name;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDirectoryList.ReadDirectory(AWildCard: String);
  const
    FindAttr = Directory+ReadOnly+Archive+SysFile;
  var
    S: lSearchRec;
    P: PSearchRec;
    FileList: PFileCollection;
    Dir: String;
    Name: String;
    Ext: String;
    Tmp: String;
    Event: TEvent;
    C: Char;
  begin
  AWildCard := lFExpand(AWildCard);
  lFSplit(AWildCard, Dir, Name, Ext);
  FileList := New(PFileCollection, Init(5, 5));

  { Диски помещаем в список заранее на случай проблем с памятью }
  for C := 'Z' downto 'A' do
    if ValidDrive(C) then
      begin
      P := MemAlloc(SizeOf(P^));
      // JO: сделаем просто и без выпендрёжа с "красивыми" скобочками, что
      //     позволит избежать нелепых проблем на ровном месте
      P^.Name := C+':';
      P^.Size := 0;
      P^.Time := $210000;
      P^.Attr := Directory;
      FileList^.Insert(P);
      end;

  Tmp := Dir+x_x;
  lFindFirst(Tmp, FindAttr + Byte(not Security)*Hidden, S);
  {AK155 Тут надо учитывать, что под WinNT каталога '.'
     может не быть вообще, и что FindNextFile приводит к падению,
     если FindFirstFile не был успешным из-за несуществующего
     каталога. Такое бывает, например, в диалоге загрузки палитры,
     если каталога COLORS вообще нет. }
  while DosError = 0 do
    begin
    if  (S.SR.Attr and Directory <> 0) and not (S.SR.Name = '.') and
        ( (Length(Dir) > 4) or (S.SR.Name <> '..'))
    then
      begin
      P := MemAlloc(SizeOf(P^));
      if P = nil then
        begin
        Msg(dlStdDlgManyFiles, nil, mfOKButton+mfWarning);
        Break;
        end;
      with P^ do
        begin
        Attr := S.SR.Attr;
        Time := S.SR.Time;
        Size := S.FullSize;
        Name := S.FullName;
        end;
      FileList^.Insert(P);
      end;
    lFindNext(S);
    end;
  lFindClose(S);
  NewLisT(FileList);
  end { TDirectoryList.ReadDirectory };
{-DataCompBoy-}

{ TFileInfoPane }

constructor TFileInfoPane.Init(var Bounds: TRect);
  begin
  TView.Init(Bounds);
  EventMask := EventMask or evBroadcast;
  end;

{-DataCompBoy-}
procedure TFileInfoPane.Draw;
  var
    B: TDrawBuffer;
    D: String[10];
    M: String[20];
    PM: Boolean;
    Color: Word;
    Time: DateTime;
    Path: String;
    FmtId: String;
    Params: array[0..7] of LongInt;
    Str: String;
  const
    {$IFNDEF OS2}
    sDirectoryLine = ' %-12s %-10s  %sm';
    sFileLine = ' %-12s %-10s  %sm';
    {$ELSE}
    sDirectoryLine = '          %-10s  %sm         ';
    sFileLine = '          %-10s  %sm         ';
    {$ENDIF}
  begin
  { Display path }
  {$IFDEF DPMI32}
  Path := lfGetLongFileName(lFExpand(PFileDialog(Owner)^.Directory^))+
      PFileDialog(Owner)^.WildCard^;
  {$ELSE}
  Path := PFileDialog(Owner)^.Directory^+PFileDialog(Owner)^.WildCard^;
  {$ENDIF}
  Color := GetColor($01);
  MoveChar(B, ' ', Color, Size.X);
  MoveStr(B[1], Cut(Path, Size.X-1), Color);
  WriteLine(0, 0, Size.X, 1, B);

  { Display file }
  {$IFNDEF OS2}
  Params[0] := LongInt(@S.Name);
  MoveChar(B, ' ', Color, Size.X);
  Params[0] := LongInt(@S.Name);
  {$ENDIF}
  if S.Attr and Directory <> 0 then
    begin
    FmtId := sDirectoryLine;
    if S.Name[2] <> ':' then
      D := GetString(dlDirectory)
    else
      D := GetString(dlDrive);
    Params {$IFNDEF OS2}[1] {$ELSE}[0] {$ENDIF}:= LongInt(@D);

    end
  else
    begin
    FmtId := sFileLine;
    D := FileSizeStr(S.Size);
    Params {$IFNDEF OS2}[1] {$ELSE}[0] {$ENDIF}:= LongInt(@D);
    end;
  UnpackTime(S.Time, Time);
  with Time do
    MakeDate(Day, Month, Year mod 100, Hour, Min, M);
  {  M := GetString( TStrIdx( Integer( dlJanuary ) + Time.Month - 1 ));}
  Params {$IFNDEF OS2}[2] {$ELSE}[1] {$ENDIF}:= LongInt(@M);
  {  Params[3] := Time.Day;
  Params[4] := Time.Year;}
  {  PM := Time.Hour >= 12;
  Time.Hour := Time.Hour mod 12;
  if Time.Hour = 0 then Time.Hour := 12;
  Params[5] := Time.Hour;
  Params[6] := Time.Min;
  if PM then Params[7] := Byte('p')
  else Params[7] := Byte('a');}
  FormatStr(Str, FmtId, Params);
  if CountryInfo.TimeFmt = 1 then
    SetLength(Str, Length(Str)-1);
  MoveStr(B, Str, Color);
  WriteLine(0, 2, Size.X, 1, B);
  Path := Cut(GetName(S.Name), Size.X-1);
  Color := GetColor($01);
  MoveChar(B, ' ', Color, Size.X);
  MoveStr(B[1], Path, Color);
  WriteLine(0, 1, Size.X, 1, B);

  { Fill in rest of rectangle }
  MoveChar(B, ' ', Color, Size.X);
  WriteLine(0, 3, Size.X, Size.Y-2, B);

  end { TFileInfoPane.Draw };
{-DataCompBoy-}

function TFileInfoPane.GetPalette: PPalette;
  const
    P: String[Length(CInfoPane)] = CInfoPane;
  begin
  GetPalette := @P;
  end;

procedure TFileInfoPane.HandleEvent(var Event: TEvent);
  begin
  TView.HandleEvent(Event);
  if  (Event.What = evBroadcast) and (Event.Command = cmFileFocused)
  then
    begin
    S := PSearchRec(Event.InfoPtr)^;
    DrawView;
    end;
  end;

{ TFileDialog }

{-DataCompBoy-}
constructor TFileDialog.Init;
  var
    Control: PView;
    R: TRect;
    S: String;
    Opt: Word;
    ACurDir: String;
  begin
  R.Assign(15, 1, 63, 21);
  TDialog.Init(R, ATitle);
  Options := Options or ofCentered;
  if AOptions and fdOpenButton <> 0
  then
    HelpCtx := hcOpenFileDialog
  else
    HelpCtx := hcSelectFileDialog;
  WildCard := NewStr(AWildCard);
  Directory := nil;

  R.Assign(3, 3, 31, 4);
  FileName := New(PFileInputLine, Init(R, MaxPathLen));
  FileName^.Data := WildCard^;
  Insert(FileName);
  R.Assign(2, 2, 3+CStrLen(InputName), 3);
  Control := New(PLabel, Init(R, InputName, FileName));
  Insert(Control);
  R.Assign(31, 3, 34, 4);
  Control := New(PHistory, Init(R, FileName, HistoryId));
  Insert(Control);

  R.Assign(17, 6, 18, 15);
  Control := New(PScrollBar, Init(R));
  Insert(Control);
  R.Assign(3, 6, 17, 15);
  FileList := New(PFileList, Init(R, WildCard^, PScrollBar(Control)));
  FileList^.HelpCtx := {hcOpen_Files}0;
  Insert(FileList);
  R.Assign(2, 5, 8, 6);
  Control := New(PLabel, Init(R, GetString(dlFiles), FileList));
  Insert(Control);

  R.Assign(34, 6, 35, 15);
  Control := New(PScrollBar, Init(R));
  Insert(Control);
  R.Assign(20, 6, 34, 15);
  DirList := New(PDirectoryList, Init(R, WildCard^, PScrollBar(Control)));
  DirList^.HelpCtx := {hcOpen_Director}0;
  Insert(DirList);
  R.Assign(19, 5, 34, 6);
  Control := New(PLabel, Init(R, GetString(dlDirectoriesLabel), DirList));
  Insert(Control);

  R.Assign(35, 3, 47, 5);
  Opt := bfDefault;
  if AOptions and fdOpenButton <> 0 then
    begin
    Control := New(PButton, Init(R, GetString(dlOpen), cmFileOpen, Opt));
    Control^.HelpCtx := 0;
    Insert(Control);
    Opt := bfNormal;
    Inc(R.A.Y, 3);
    Inc(R.B.Y, 3);
    end;
  if AOptions and fdOKButton <> 0 then
    begin
    Control := New(PButton, Init(R, GetString(dlOKButton), cmFileOpen,
           Opt));
    Control^.HelpCtx := {hcOK}0;
    Insert(Control);
    Opt := bfNormal;
    Inc(R.A.Y, 3);
    Inc(R.B.Y, 3);
    end;
  if AOptions and fdReplaceButton <> 0 then
    begin
    Insert(New(PButton, Init(R, GetString(dlReplace), cmFileReplace,
       Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3);
    Inc(R.B.Y, 3);
    end;
  if AOptions and fdClearButton <> 0 then
    begin
    Insert(New(PButton, Init(R, GetString(dlClear), cmFileClear, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3);
    Inc(R.B.Y, 3);
    end;
  Control := New(PButton, Init(R, GetString(dlCancelButton), cmCancel,
         bfNormal));
  Control^.HelpCtx := 0;
  Insert(Control);
  Inc(R.A.Y, 3);
  Inc(R.B.Y, 3);
  if AOptions and fdHelpButton <> 0 then
    begin
    Control := New(PButton, Init(R, GetString(dlHelpButton), cmHelp,
           bfNormal));
    Insert(Control);
    Inc(R.A.Y, 3);
    Inc(R.B.Y, 3);
    end;

  R.Assign(2, 16, 46, 19);
  Control := New(PFileInfoPane, Init(R));
  Insert(Control);

  SelectNext(False);

  if AOptions and fdNoLoadDir = 0 then
    ReadDirectory;
  end { TFileDialog.Init };
{-DataCompBoy-}

{-DataCompBoy-}
constructor TFileDialog.Load(var S: TStream);
  begin
  TDialog.Load(S);
  WildCard := S.ReadStr;
  GetSubViewPtr(S, FileName);
  GetSubViewPtr(S, FileList);
  if  (FileName = nil) or (FileList = nil) then
    {Cat}
    Fail;

  ReadDirectory;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
destructor TFileDialog.Done;
  begin
  DisposeStr(WildCard);
  DisposeStr(Directory);
  TDialog.Done;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileDialog.GetData(var Rec);
  begin
  GetFileName(String(Rec));
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileDialog.GetFileName(var S: String);
  var
    Path: String;
    Name: String;
    Ext: String;
    TPath: String;
    TName: String;
    TExt: String;

  function LTrim(S: String): String;
    var
      I: Integer;
    begin
    I := 1;
    while (I < Length(S)) and (S[I] = ' ') do
      Inc(I);
    LTrim := Copy(S, I, MaxStringLength);
    end;

  function RTrim(S: String): String;
    var
      I: Integer;
    begin
    while S[Length(S)] = ' ' do
      SetLength(S, Length(S)-1);
    RTrim := S;
    end;

  (* function NoWildChars(S: String): String; assembler;
{$IFNDEF BIT_32}
 asm
        PUSH    DS
        LDS     SI,S
        XOR     AX,AX
        LODSB
        XCHG    AX,CX
        LES     DI,@Result
        INC     DI
 @@1:   LODSB
        CMP     AL,'?'
        JE      @@2
        CMP     AL,'*'
        JE      @@2
        STOSB
 @@2:   LOOP    @@1
        XCHG    AX,DI
        MOV     DI,WORD PTR @Result
        SUB     AX,DI
        DEC     AX
        STOSB
        POP     DS
 end;
{$ELSE BIT_32}{&Frame-}{$USES ESI, EDI, ECX}
 asm
        LEA     ESI,S
        XOR     EAX,EAX
        LODSB
        XCHG    EAX,ECX
        MOV     EDI,@Result
        INC     EDI
 @@1:   LODSB
        CMP     AL,'?'
        JE      @@2
        CMP     AL,'*'
        JE      @@2
        STOSB
 @@2:   LOOP    @@1
        XCHG    EAX,EDI
        MOV     EDI,@Result
        SUB     EAX,EDI
        DEC     EAX
        STOSB
 end;
{$ENDIF}
*)
  {AK155: тут было нечто ассемблерное с условной трансляцией,
глючившее под VP. Эта функция вызывается в единственном месте
в связи с диалогом, так что ассемблер здесь совершенно не нужен.
Поэтому переписал на Паскале: и понятно, и платформонезависимо.}
  function NoWildChars(S: String): String;
    var
      i, j: Integer;
      c: Char;
      {Result: string;}
    begin
    j := 0;
    for i := 1 to Length(S) do
      begin
      c := S[i];
      if  (c <> '*') and (c <> '?') then
        begin
        Inc(j);
        Result[j] := c;
        end;
      end;
    SetLength(Result, j);
    if Result = '.' then
      Result := '';
    {Cat: чтобы к именам без расширения не добавлялась точка}
    {NoWildChars := Result;}
    end { NoWildChars };
  {/AK155}

  begin { TFileDialog.GetFileName }
  S := FileName^.Data;
  ClrIO;
   {AK155 разворачиваем S до полного пути, но не относительно
   текущего каталога активной панели, а относительно каталога,
   показанного сейчас в данном диалоге }
  Path := ActiveDir;
  ActiveDir := Directory^;
  S := lFExpand(S);
  ActiveDir := Path;

  if Abort then
    Exit;
  lFSplit(S, Path, Name, Ext);
  if  (Ext = '') and not IsDir(S) then
    begin
    lFSplit(WildCard^, TPath, TName, TExt);
    if  ( (Name = '') and (Ext = '')) then
      S := Path+TName+TExt
    else if Ext = '' then
      begin
      if IsWild(Name) then
        S := Path+Name+TExt
      else
        S := Path+Name+NoWildChars(TExt);
      end;
    end;
  S := DelSquashes(S);
  {$IFDEF DPMI32}
  S := lfGetLongFileName(lfGetLongFileName(GetPath(S))+GetName(S));
  {$ENDIF}
  end { TFileDialog.GetFileName };
{-DataCompBoy-}

procedure TFileDialog.HandleEvent(var Event: TEvent);

  procedure Go;
    procedure SuperFocus(var List: PCollection; curent: Integer);
      begin
      if List^.Count > 0 then
        begin
        Event.What := evBroadcast;
        Event.Command := cmFileFocused;
        Event.InfoPtr := List^.At(curent);
        Owner^.HandleEvent(Event);
        end;
      end;

    begin
    if  (FileList^.State and sfSelected > 0) then
      SuperFocus(FileList^.List, FileList^.Focused);
    if  (DirList^.State and sfSelected > 0) then
      SuperFocus(DirList^.List, DirList^.Focused);
    ClearEvent(Event);
    end;

  begin { TFileDialog.HandleEvent }
  if Event.What = evKeyDown then
    begin
    if  (FileName^.State and sfSelected) = 0 then
      case Event.KeyCode of
        kbRight:
          begin
          if  (Current = PView(FileName))
          then
            if  (FileList^.List^.Count = 0)
            then
              DirList^.Select
            else
              SelectNext(False)
          else
            SelectNext(False);
          Go;
          end;
        kbShiftTab, kbLeft:
          begin
          if  (Current = PView(DirList))
          then
            if  (FileList^.List^.Count = 0)
            then
              FileName^.Select
            else
              SelectNext(True)
          else
            SelectNext(True);
          Go;
          end;
      end {case};
    case Event.KeyCode of
      kbTab:
        begin
        if  (Current = PView(FileName))
        then
          if  (FileList^.List^.Count = 0)
          then
            DirList^.Select
          else
            SelectNext(False)
        else
          SelectNext(False);
        Go;
        end;
    end {case};

    end;
  TDialog.HandleEvent(Event);

  if Event.What = evCommand then
    case Event.Command of
      cmFileOpen, cmFileReplace, cmFileClear:
        begin
        EndModal(Event.Command);
        ClearEvent(Event);
        end;
    end {case};
  end { TFileDialog.HandleEvent };

{-DataCompBoy-}
procedure TFileDialog.SetData(var Rec);
  begin
  FileName^.SetData(Rec);
  if  (String(Rec) <> '') and (IsWild(String(Rec))) then
    begin
    Valid(cmFileInit);
    FileName^.Select;
    end;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileDialog.ReadDirectory;
  var
    D: String;
    N: String;
    X: String;
  begin
  FileList^.ReadDirectory(WildCard^);
  DirList^.ReadDirectory(WildCard^);
  lFSplit(lFExpand(WildCard^), D, N, X);
  DisposeStr(WildCard);
  WildCard := NewStr(N+X);
  DisposeStr(Directory);
  Directory := NewStr(D);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileDialog.Store(var S: TStream);
  begin
  TDialog.Store(S);
  S.WriteStr(WildCard);
  PutSubViewPtr(S, FileName);
  PutSubViewPtr(S, FileList);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TFileDialog.Valid(Command: Word): Boolean;
  var
    T: Boolean;
    lFName: String;
    Dir: String;
    Name: String;
    Ext: String;

  function CheckDirectory(var S: String): Boolean;
    begin
    if not PathExist(S) then
      begin
      ErrMsg(erInvalidDrive);
      FileName^.Select;
      CheckDirectory := False;
      end
    else
      CheckDirectory := True;
    end;
  var
    Event: TEvent;
  begin
  if Command = 0 then
    begin
    Valid := True;
    Exit;
    end
  else
    Valid := False;
  if TDialog.Valid(Command) then
    begin
    if Command = cmCancel then
      begin
      Valid := True;
      Exit;
      end;
    GetFileName(lFName);
    if Abort then
      begin
      Valid := False;
      Exit;
      end;
    if  (Command <> cmCancel) and (Command <> cmFileClear) then
      begin
      if IsWild(lFName) then
        begin
        lFSplit(lFName, Dir, Name, Ext);
        if CheckDirectory(Dir) then
          begin
          DisposeStr(Directory);
          Directory := NewStr(Dir);
          DisposeStr(WildCard);
          WildCard := NewStr(Name+Ext);
          DirList^.ReadDirectory(Directory^+WildCard^);
          FileList^.ReadDirectory(Directory^+WildCard^);
          if Command <> cmFileInit then
            { FileList^.Select;}
            if  ( (DirList^.State and sfSelected) <> 0) or
                (FileList^.List^.Count = 0)
            then
              begin
              begin
              Event.What := evBroadcast;
              Event.Command := cmFileFocused;
              Event.InfoPtr := DirList^.List^.At(0);
              Owner^.HandleEvent(Event);
              end;
              end
            else
              FileList^.Select;
          end
        end
      else if IsDir(lFName) then
        begin
        if CheckDirectory(lFName) then
          begin
          DisposeStr(Directory);
          Directory := NewStr(lFName+'\');
          DirList^.ReadDirectory(Directory^+WildCard^);
          FileList^.ReadDirectory(Directory^+WildCard^);
          if Command <> cmFileInit then
            if  ( (DirList^.State and sfSelected) <> 0) or
                (FileList^.List^.Count = 0)
            then
              begin
              begin
              Event.What := evBroadcast;
              Event.Command := cmFileFocused;
              Event.InfoPtr := DirList^.List^.At(0);
              Owner^.HandleEvent(Event);
              end;
              end
            else
              FileList^.Select;
          end
        end
      else if ValidFileName(lFName) then
        Valid := True
      else
        begin
        ErrMsg(erInvalidFileName);
        Valid := False;
        end
      end
    else
      Valid := True;
    end;
  end { TFileDialog.Valid };

{ TDirCollection }

procedure TDirCollection.FreeItem(Item: Pointer);
  var
    DirItem: PDirEntry absolute Item;
  begin
  DisposeStr(DirItem^.DisplayText);
  DisposeStr(DirItem^.Directory);
  Dispose(DirItem);
  end;

function TDirCollection.GetItem(var S: TStream): Pointer;
  var
    DirItem: PDirEntry;
  begin
  New(DirItem);
  DirItem^.DisplayText := S.ReadStr;
  DirItem^.Directory := S.ReadStr;
  GetItem := DirItem;
  end;

procedure TDirCollection.PutItem(var S: TStream; Item: Pointer);
  var
    DirItem: PDirEntry absolute Item;
  begin
  S.WriteStr(DirItem^.DisplayText);
  S.WriteStr(DirItem^.Directory);
  end;

{-DataCompBoy-}
function GetFileNameDialog(Mask, Title, Name: String;
     Buttons, HistoryId: Word): String;
  var
    S: String;
    D: PFileDialog;
    B: Boolean;
  begin
  S := '';
  B := False;
  if Mask = '' then
    begin
    Mask := x_x;
    B := True
    end;
  D := PFileDialog(Application^.ValidView(New(PFileDialog,
          Init(Mask, Title, Name, Buttons, HistoryId))));
  if D = nil then
    Exit;
  if B then
    D^.SetData(S);
  if Desktop^.ExecView(D) <> cmCancel then
    D^.GetFileName(S);
  Dispose(D, Done);
  if S <> '' then
    begin
    {$IFDEF DPMI32}
    S := lFExpand(lfGetLongFileName(S));
    {$ENDIF}
    HistoryAdd(HistoryId, S);
    end;
  GetFileNameDialog := S
  end { GetFileNameDialog };

function GetFileNameMenu(Path, Mask, Default: String;
     PutNumbers: Boolean; var More, None: Boolean): String; {JO}

  var
    S: String;
    L: TLineCollection;
    SR: lSearchRec;
    Current, Def: PMenuItem;
    Menu: PMenu;
    HMB: PMenuBox;
    CurIdx, i: Integer;
    R: TRect;
    W: Word;

  function LngMixCase(P: String): String;
    var
      i: Integer;
    begin
    for i := 1 to Length(P) do
      if  (i = 1) or not (P[i-1] in ['a'..'z',
          'A'..'Z'])
      then
        P[i] := UpCase(P[i])
      else if P[i] in ['A'..'Z']
      then
        P[i] := Chr(Ord(P[i])+Ord('a')-Ord('A'));
    LngMixCase := P
    end;

  begin { GetFileNameMenu }
  UpStr(Default);
  L.Init(5, 5, False);
  lFindFirst(MakeNormName(Path, '*.*'), AnyFileDir, SR);
  while DosError = 0 do
    begin
    if  ( (SR.SR.Attr and Directory) = 0) and (SR.FullSize > 0) and
        (InFilter(SR.FullName, Mask) or InFilter(SR.SR.Name, Mask))
    then
      begin
      SetLength(S, 0);
      S := SR.FullName;
      S := LngMixCase(S);
      L.Insert(NewStr(S))
      end;
    lFindNext(SR)
    end;
  lFindClose(SR);
  Def := nil;
  Current := nil;
  if L.Count > 0 then
    begin
    for i := L.Count-1 downto 0 do
      begin
      if PutNumbers then
        case i of
          0..8:
            S := '~'+Chr(Ord('1')+i)+'~ ';
          9:
            S := '~0~ ';
          10..35:
            S := '~'+Chr(55+i)+'~ ';
          else {case}
            S := '  ';
        end
      else
        S := '';
      Current := NewItem(S+CutH(PString(L.At(i))^, 40), '', kbNoKey,
          cmSelectMenuCommands+i, hcNoContext, Current);
      if UpStrg(PString(L.At(i))^) = Default then
        Def := Current
      end;
    end;
  if More then
    Current := NewItem('  '+GetString(dlMore), '', kbSpace,
        cmMoreCommand, hcNoContext, Current);
  if None then
    Current := NewItem('~'#27'~ '+GetString(dlNone), '', kbBack,
        cmNoneCommand, hcNoContext, Current);
  Menu := NewMenu(Current);
  if Def <> nil then
    Menu^.Default := Def;
  Application^.GetExtent(R);
  R.A.X := ((R.A.X+R.B.X) div 2)-8;
  R.B.X := R.A.X+16;
  R.A.Y := ((R.A.Y+R.B.Y-L.Count) div 2)-1;
  R.B.Y := R.A.Y+L.Count+2;
  if R.A.Y < Application^.Origin.Y+1 then
    R.A.Y := Application^.Origin.Y+1;
  if R.B.Y > Application^.Origin.Y+Application^.Size.Y-1
  then
    R.B.Y := Application^.Origin.Y+Application^.Size.Y-1;
  New(HMB, Init(R, Menu, nil));
  W := Application^.ExecView(HMB);
  Dispose(HMB, Done);
  DisposeMenu(Menu);
  More := False;
  None := False;
  GetFileNameMenu := '';
  case W of
    cmCancel:
      ;
    cmClose:
      ;
    cmNoneCommand:
      None := True;
    cmMoreCommand:
      More := True;
    else {case}
      begin
      CurIdx := W-cmSelectMenuCommands;
      if  (CurIdx >= 0) and (CurIdx < L.Count) then
        GetFileNameMenu := MakeNormName(Path, PString(L.At(CurIdx))^);
      end;
  end {case};
  L.FreeAll;
  L.Done;
  end { GetFileNameMenu }; {JO}

end.
