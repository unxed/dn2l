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

unit Tree;

interface

uses
  Objects, Drivers, Defines, Streams,
  Dialogs, Views, FilesCol
  ;

type
  PTreeWindow = ^TTreeWindow;
  TTreeWindow = object( {TStd}TWindow)
    constructor Init(var Bounds: TRect);
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    end;

  PTreeReader = ^TTreeReader;
  TTreeReader = object(TView)
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PTreeDialog = ^TTreeDialog;
  TTreeDialog = object(TDialog)
    Tree: PView;
    isValid: Boolean;
    constructor Init(R: TRect; const ATitle: String; ADrive: Byte);
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function GetPalette: PPalette; virtual;
    function Valid(Command: Word): Boolean; virtual;
    end;

  PDirRec = ^TDirRec;
  TDirRec = record
    Level: Byte;
    Cluster: Word;
    Size: TSize;
    Attr: Byte;
    NumFiles: LongInt;
    Date: LongInt;
    Number: Integer;
    DirName: TFlName;
    Dummy: array[1..SizeOf(ShortString)-SizeOf(TShortName)] of Char;
    {см. комментарий к TFileRec}
    end;

  PTreeView = ^TTreeView;
  TTreeView = object(TView)
    ScrollBar: PScrollBar;
    CurPtr: PDirRec;
    isValid {, QuickSearch}: Boolean;
    Drive, SearchPos: Byte;
    CurPath, LastPath: String; {DataCompBoy}
    DC, Dirs: PCollection;
    Delta: TPoint;
    CurNum: Integer;
    Parital, DrawDisabled,
    LocateEnabled, MouseTracking, WasChanged: Boolean;
    InfoView: PView;
    constructor Init(R: TRect; ADrive: Integer; ParitalView: Boolean;
        ScrBar: PScrollBar);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    function Valid(Command: Word): Boolean; virtual;
    function Expanded(P: PDirRec; i: Integer): Boolean;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure ReadTree(CountLen: Boolean);
    procedure Reread(CountLen: Boolean);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure HandleCommand(var Event: TEvent);
    function GetDirName(N: Integer): String;
    procedure SetData(var Rec); virtual;
    procedure GetData(var Rec); virtual;
    procedure CollapseBranch(N: Integer);
    // fixme: commented by unxed
    //function DataSize: Word; virtual;
    function FindDir(Dir: String): Integer; {DataCompBoy}
    procedure ReadAfterLoad;
    function GetPalette: PPalette; virtual;
    procedure Draw; virtual;
    destructor Done; virtual;
    end;

  PTreePanel = ^TTreePanel;
  TTreePanel = object(TTreeView)
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PTreeInfoView = ^TTreeInfoView;
  TTreeInfoView = object(TView)
    Tree: PTreeView;
    Down: String;
    Loaded: Boolean;
    constructor Init(R: TRect; ATree: PTreeView);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure MakeDown;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    function GetPalette: PPalette; virtual;
    destructor Done; virtual;
    end;

  PDTreeInfoView = ^TDTreeInfoView; { дерево в диалоге }
  TDTreeInfoView = object(TTreeInfoView)
    function GetPalette: PPalette; virtual;
    end;

  PHTreeView = ^THTreeView;
   {`2 панель дерева с подвалом `}
  THTreeView = object(TTreePanel)
    Info: PView;
    constructor Init(R: TRect; ADrive: Integer; ParitalView: Boolean;
        ScrBar: PScrollBar);
    procedure ChangeBounds(var Bounds: TRect); virtual;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    function GetPalette: PPalette; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    destructor Done; virtual;
    end;

  PDirCollection = ^TDirCollection;
  TDirCollection = object(TCollection)
    procedure FreeItem(P: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    end;

function ChangeDir(ATitle: TTitleStr; Drv: Byte): String; {DataCompBoy}
procedure CheckMkDir(const Path: String);
 {` MkDir and check result AK155 `}
procedure MakeDirectory;
 {` Создать каталог(и) в диалоге. Фактически, это TDrive.MakeDir.
  В введённой строке можнт быть несколько каталогов, разделённых
  точками с запятой.
  Имя первого созданного каталог с полным путём без слэша помещается в
  переменную CreatedDir. Если каталог не создался - CreatedDir=''.
  Переменная CreatedDir используется в ARVIDAVT.PAS, и подозреваю, что
  в каком-то другом смысле. `}
procedure FreeTree(C: Char);
function GetDirLen(Dir: String): TSize; {DataCompBoy}
function CreateDirInheritance(var S: String; Confirm: Boolean): Byte;
  {` Создать каталог любой вложенности. S разворачивается при
   помощи lFExpand и дополняется '\' в конце, и это значение
   остаётся после вызова.
     Результат - длина пути (то есть подстроки S) каталога, в
   котором находится самый внешний созданный каталог (без слэша).
   Например, если s='C:\TEMP\AAA\BBB' и каталог C:\TEMP существовал,
   а C:\TEMP\AAA был создан, то результат - 7. Смысл этого в том,
   что если C:\TEMP открыт на какой-то панели, то эту панель надо
   перечитать, чтобы на ней появился AAA.
     Если каталоги не создавались - результат 0. `}

const
  CreatedDir: String = '';
    {` Результат MakeDirectory `}
  TreeError: Boolean = False;

  CHTreeView = #15#16#17#18#19#20#21;
  CTreeInfoView = #22;
  CDTreeInfoView = #30;
  CTreeView = #35#36#37#38#39#40#41;
  CTreeDialog = CDialog+#104#105#106#107#108#109#110;

var
  DrvTrees: array['A'..'Z'] of record
    C: PCollection;
    len: Boolean;
    end;

implementation
uses
  Lfnvp, Files, Memory, Startup, Dos, DnIni, DNHelp,
  Advance, Advance1, Advance2, Advance3,
  FlPanelX, DNApp, Messages, Commands, Drives, Eraser, Menus,
  xTime, FileCopy
  ;

const
  trExpanded = $02;
  trHasBranch = $04;
  cmRevert = 200;
  cmDirChanged = 201;

function ESC_Pressed: Boolean;
  var
    E: TEvent;
  begin
  Application^.Idle;
  GetKeyEvent(E);
  ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbESC)
  end;
{DataCompBoy
function NotStr(S: String): String;
 var I: Integer;
begin
  for I := 1 to Length(S) do S[I] := Char(not Byte(S[I]) xor I);
  NotStr := S;
end;
}
procedure ReadTree(C: Char; CountLen: Boolean);
  label Rep, DRep;
  var
    P: PDirRec;
    DCEntry: Integer;
    Idx: Integer;
    D, S: String; {DataCompBoy}
    Lv, I: Integer;
    DSize: Word;
    Dr: array[1..255] of Integer;
    DC: PDirCollection;
    Tmr: TEventTimer;

  procedure ChkESC;
    begin
    if Abort then
      Exit;
    if TimerExpired(Tmr) then
      begin
      NewTimer(Tmr, 150);
      Abort := ESC_Pressed;
      end;
    end;

  {-DataCompBoy-}
  procedure DosReadDirectory(PD: PDirRec);
    var
      I: Integer;
      Lv: Integer;
      S: String;
      SR: lSearchRec;
      P: PDirRec;
    begin
    I := DC^.IndexOf(PD);
    Lv := PD^.Level;
    PD^.Size := 0;
    P := PD;
    S := MakeNormName(PD^.DirName[uLfn], '');
    repeat
      while (I > 0) and (P^.Level <= PDirRec(DC^.At(I))^.Level) do
        Dec(I);
      if DC^.IndexOf(PD) <> I then
        begin
        P := DC^.At(I);
        S := MakeNormName(P^.DirName[uLfn], S);
        end;
    until I = 0;
    ClrIO;
    if Abort then
      Exit;
    I := DC^.IndexOf(PD);
    lFindFirst(S+x_x, AnyFileDir, SR); {JO}
    while (DosError = 0) and not Abort do
      begin
      if not IsDummyDir(SR.SR.Name) then
        begin
        if SR.SR.Attr and Directory <> 0 then
          begin
          New(P);
          CopyShortString(SR.SR.Name, P^.DirName[uLfn]);
          P^.Cluster := 0;
          P^.NumFiles := 0;
          P^.Size := -1;
          P^.Level := PD^.Level+1;
          Inc(I);
          DC^.AtInsert(I, P);
          end
        else
          begin
          PD^.Size := PD^.Size+SR.FullSize;
          Inc(PD^.NumFiles);
          end;
        end;
      lFindNext(SR);
      end;
    lFindClose(SR);
    DosError := 0;
    ClrIO;
    end { DosReadDirectory };
  {-DataCompBoy-}

  var
    PD: PDirRec;
    RemoteDrive: Boolean;
    Info: PView;

    {-DataCompBoy-}
  begin { ReadTree }
  {Cat:warn надо добавить вывод дерева для сетевых путей}
  NewTimer(Tmr, 1);
  C := UpCase(C);
  if not (C in ['A'..'Z']) then
    Exit;
  if DrvTrees[C].C <> nil then
    Dispose(DrvTrees[C].C, Done);
  DrvTrees[C].C := nil;
  TreeError := True;
  if LowMemory then
    Exit;
  New(DC, Init(10, 10));
  New(P);
  FillChar(P^, SizeOf(P^), 0);
  P^.DirName[uLfn] := C+':\';
  P^.Cluster := 0;
  P^.Level := 0;
  P^.Size := -1;
  P^.Attr := 0;
  DC^.Insert(P);
  Abort := False;
  RemoteDrive := False;
  begin
  Info := WriteMsg(GetString(dlScanningDirs));
  DrvTrees[C].C := DC;
  DrvTrees[C].len := True;
DRep:
  ChkESC;
  if not Abort then
    for DCEntry := 1 to DC^.Count do
      begin
      UpdateWriteView(Info);
      P := DC^.At(DCEntry-1);
      if P^.Size < 0 then
        if  (not Abort) and (P^.Level < 40) then
          begin
          DosReadDirectory(P);
          goto DRep
          end
        else
          P^.Size := 0;
      end;
  Dispose(Info, Done);
  if Abort then
    begin
    Dispose(DC, Done);
    DrvTrees[C].C := nil;
    TreeError := True;
    Exit;
    end;
  end
  ;
  for I := 1 to DC^.Count do
    begin
    P := DC^.At(I-1);
    Lv := I;
    P^.Number := I-1;
    P^.Attr := 0;
    while (Lv < DC^.Count) and (PDirRec(DC^.At(Lv))^.Level > P^.Level)
    do
      begin
      PD := DC^.At(Lv);
      Inc(P^.NumFiles, PD^.NumFiles);
      P^.Size := P^.Size+PD^.Size;
      Inc(Lv);
      end;
    if  (Lv < DC^.Count) and (PDirRec(DC^.At(Lv))^.Level = P^.Level)
    then
      P^.Attr := 1;
    if  (I < DC^.Count) and (PDirRec(DC^.At(I))^.Level > P^.Level) then
      P^.Attr := P^.Attr or trHasBranch;
    end;
  TreeError := False;
  end { ReadTree };
{-DataCompBoy-}

function GetDirCollection(C: Char; CountLen: Boolean): PCollection;
  begin
  GetDirCollection := nil;
  C := UpCase(C);
  if not (C in ['A'..'Z']) then
    Exit;
  if  (DrvTrees[C].C = nil) or (not DrvTrees[C].len and CountLen)
  then
    ReadTree(C, CountLen);
  GetDirCollection := DrvTrees[C].C;
  end;

{-DataCompBoy-}
function FindDir(DC: PCollection; const Dir: String): Integer;
  var
    I, Lv: Integer;
    S, D: String;
    P: PDirRec;
  begin
  FindDir := -1;
  if DC = nil then
    Exit;
  D := Dir;
  Lv := 1;
  I := 1;
  Delete(D, 1, 3);
  if D = '' then
    begin
    FindDir := 0;
    Exit
    end;
  repeat
    S := '';
    while (D[1] <> '/') and (D <> '') do // slash change by unxed
      begin
      S := S+D[1];
      Delete(D, 1, 1); {DelFC(D)}
      end;
    Delete(D, 1, 1); {DelFC(D);}
    UpStr(S);
    if  (S <> '') and (I < DC^.Count) then
      begin
      repeat
        if I < DC^.Count then
          P := DC^.At(I);
        while (I < DC^.Count) and (P^.Level > Lv) do
          begin
          Inc(I);
          if I < DC^.Count then
            P := DC^.At(I);
          end;
        while (I < DC^.Count) and (P^.Level = Lv)
          and (UpStrg(P^.DirName[uLfn]) <> S)
        do
          begin
          Inc(I);
          if I < DC^.Count then
            P := DC^.At(I);
          end
      until (I >= DC^.Count) or
        ( (UpStrg(P^.DirName[uLfn]) = S)
        and (P^.Level = Lv)) or (P^.Level < Lv)
      end
    else if S = '' then
      I := 1;
    if D <> '' then
      Inc(I);
    Inc(Lv);
  until (D = '') or (I >= DC^.Count) or (Lv > 128) { :) };
  if  (I >= DC^.Count) or (UpStrg(P^.DirName[uLfn]) <> S)
  then
    FindDir := -1
  else
    FindDir := I
  end { FindDir };
{-DataCompBoy-}

function GetDirLen(Dir: String): TSize; {DataCompBoy}
  var
    DC: PCollection;
    I: Integer;
    R: Boolean;

  procedure MakeReread(P: PView);
    var
      Event: TEvent;
    begin
    Event.What := evCommand;
    Event.Command := cmRereadTree;
    Event.InfoPtr := @Dir;
    if P <> nil then
      P^.HandleEvent(Event);
    end;

  begin
  GetDirLen := 0;
  Dir[1] := UpCase(Dir[1]);
  R := (DrvTrees[Dir[1]].C <> nil) and not (DrvTrees[Dir[1]].len);
  DC := GetDirCollection(Dir[1], True);
  if DC = nil then
    Exit;
  if R then
    Desktop^.ForEach(@MakeReread);
  I := FindDir(DC, Dir);
  if I < 0 then
    Exit;
  GetDirLen := PDirRec(DC^.At(I))^.Size;
  end { GetDirLen };

procedure FreeTree(C: Char);
  begin
  C := UpCase(C);
  if not (C in ['A'..'Z']) then
    Exit;
  Dispose(DrvTrees[C].C, Done);
  DrvTrees[C].C := nil;
  end;

procedure TTreeReader.HandleEvent;
  var
    C: Char;
    S: String;

  begin
  inherited HandleEvent(Event);

  if  (Event.What = evCommand)
  then
    case Event.Command of
      cmRereadTree:
        begin
        S := UpStrg(PString(Event.InfoPtr)^);
        C := S[1];
        if  (C in ['A'..'Z']) and (DrvTrees[C].C <> nil) then
          begin
          Dispose(DrvTrees[C].C, Done);
          DrvTrees[C].C := nil;
          GlobalMessage(evCommand, cmFindTree, @C);
          if C = #0 then
            begin
            C := S[1];
            ReadTree(C, DrvTrees[C].len)
            end
          else
            DrvTrees[C].len := False;
          end;
        end;
    end {case}; { case }

  end { TTreeReader.HandleEvent };

procedure CheckMkDir(const Path: String);
  var
    rc: LongInt;
  label Start;
  begin
Start:
  lMkDir(Path);
  rc := IOResult;
  {AK155 29-05-2002
При создании существующего каталога (что не есть ошибка)
под OS/2 rc=5, а под WinNT - rc=183.
При создании каталога на CD (что есть ошибка)
под OS/2 rc=19, а под WinNT - rc=5.
Как оно будет под Win9x или DPMI - тоже еще вопрос.
Поэтому проще и аккуратнее проверить фактическое наличие, а не
анализировать rc }
  if not PathExist(Path) then
    (*  if (rc <> 0) and (rc <> 5 {каталог уже существует}) {$IFDEF Win32} and (rc <> 183) {$ENDIF} then*)
    begin
    if SysErrorFunc(rc, Byte(Path[1])-Byte('A')) = 1 then
      goto Start;
    rc := MessageBox(GetString(dlFCNoCreateDir)+Path, nil,
         mfError+mfOKButton);
    Abort := True;
    end;
  end { CheckMkDir };

{-DataCompBoy-}
procedure MakeDirectory;
  var
    S, S1: String;
    Dr: String;
    Nm: String;
    XT: String;
    B: Byte;
    I: Integer;
    j: Boolean;
    W: Word;
  begin
  CreatedDir := '';
  if LowMemory then
    Exit;
  S := '';
  W := ExecResource(dlgMkDir, S);
  if W = cmYes then
    UpStr(S)
  else if W = cmNo then
    LowStr(S)
  else if W <> cmOK then
    Exit;
  DelRight(S);
  {$IFDEF DPMI32}
  DelLeft(S);
  {$ENDIF}
  if S = '' then
    Exit;
  {$IFDEF RecodeWhenDraw}
  S := OemToCharStr(S);
  {$ENDIF}
  CreatedDir := '';
  while S <> '' do
    begin
    j := False;
    I := 0;
    while (I < Length(S)) do
      begin
      Inc(I);
      if not j and (S[I] = ';') then
        Break;
      if S[I] = '"' then
        j := not j;
      end;
    if I = Length(S) then
      Inc(I);
    if I = 0 then
      I := Length(S)+1;
    S1 := DelSquashes(Copy(S, 1, I-1));
    Delete(S, 1, I);
    if S1 = '' then
      Continue;
    B := CreateDirInheritance(S1, False);
    if Abort or (IOResult <> 0) then
      Exit;
    SetLength(S1, Length(S1)-1); // удалить слэш
    if CreatedDir = '' then
      CreatedDir := S1;
    { определение каталога для перечитывания }
    if B > 0 then
      SetLength(S1, B)
    else
      begin
      lFSplit(S1, S1, Nm, XT);
      SetLength(S1, Length(S1)-1); // удалить слэш
      end;
    RereadDirectory(S1);
    GlobalMessage(evCommand, cmRereadTree, @Dr);
    GlobalMessage(evCommand, cmRereadInfo, nil);
    end;
  end { MakeDirectory };
{-DataCompBoy-}

function ChangeDir;
  var
    D: PTreeDialog;
    S: String;
    R: TRect;
  begin
  R.Assign(1, 1, 50, 18);
  Abort := False;
  ChangeDir := '';
  New(D, Init(R, ATitle, Drv));
  D^.Options := D^.Options or ofCentered;
  S := '';
  D := PTreeDialog(Application^.ValidView(D));
  if D <> nil then
    if Desktop^.ExecView(D) = cmOK then
      D^.GetData(S);
  ChangeDir := S;
  end;

destructor TTreeInfoView.Done;
  begin
  PHTreeView(Tree).Info := nil;
  inherited Done;
  end;

constructor TTreeInfoView.Init;
  begin
  inherited Init(R);
  Tree := ATree;
  Options := Options or ofPostProcess;
  EventMask := evBroadcast;
  GrowMode := gfGrowHiX+gfGrowHiY+gfGrowLoY;
  MakeDown;
  Loaded := False;
  end;

procedure TTreeInfoView.HandleEvent;
  begin
  inherited HandleEvent(Event);
  if  (Event.What = evBroadcast) and (Event.Command = cmDirChanged) then
    begin
    MakeDown;
    DrawView
    end;
  end;

constructor TTreeInfoView.Load;
  begin
  inherited Load(S);
  GetPeerViewPtr(S, Tree);
  Loaded := True;
  end;

procedure TTreeInfoView.Store;
  begin
  inherited Store(S);
  PutPeerViewPtr(S, Tree);
  end;

function TTreeInfoView.GetPalette;
  const
    S: String[Length(CTreeInfoView)] = CTreeInfoView;
  begin
  GetPalette := @S;
  end;

function TDTreeInfoView.GetPalette;
  const
    S: String[Length(CDTreeInfoView)] = CDTreeInfoView;
  begin
  GetPalette := @S;
  end;

procedure TTreeInfoView.Draw;
  var
    B: TDrawBuffer;
    C: Byte;
  begin
  C := GetColor(1);
  if Loaded then
    MakeDown;
  Loaded := False;
  MoveChar(B, ' ', C, Size.X);
  MoveStr(B[1], Cut(Tree^.CurPath, Size.X), C);
  WriteLine(0, 0, Size.X, 1, B);
  MoveChar(B, ' ', C, Size.X);
  MoveStr(B[1], Down, C);
  WriteLine(0, 1, Size.X, 1, B);
  end;

procedure TTreeInfoView.MakeDown;
  {var L: Array [1..5] of Longint;}
  var
    L1: LongInt;
    L2: TSize;
  begin
  {L[1] := Tree^.CurPtr^.NumFiles;}
  {L[2] := Tree^.CurPtr^.Size;}
  if  (Tree <> nil) and (Tree^.CurPtr <> nil) then
    begin
    L1 := Tree^.CurPtr^.NumFiles;
    L2 := Tree^.CurPtr^.Size;
    if L1 <> 1 then
      Down := ItoS(L1)+GetString(dlTreeFilesWith)
    else
      Down := GetString(dlTree1FileWith);
    if L2 <> 1 then
      Down := Down+FStr(L2)+' '+GetString(dlDIBytes)
    else
      Down := Down+' 1'+GetString(dlDIByte);
    end;
  end;

constructor TTreeDialog.Init;
  var
    R1, R2: TRect;
    P: PView;
  begin
  inherited Init(R, ATitle);
  HelpCtx := hcTreeDialog;
  isValid := True;
  if R.B.X-R.A.X < 24 then
    R.Grow(24+R.A.X-R.B.X, 0);
  if R.B.Y-R.A.Y < 8 then
    R.B.Y := R.A.Y+8;
  GetExtent(R);
  R.Grow(-1, -1);
  R1 := R;
  Dec(R1.B.X, 14);
  P := StandardScrollBar(sbVertical+sbHandleKeyboard);
  Dec(P^.Origin.X, 14);
  Dec(R1.B.Y);
  Tree := New(PTreeView, Init(R1, ADrive, False, PScrollBar(P)));
  if Tree^.Valid(0) then
    Insert(Tree)
  else
    begin
    Dispose(Tree, Done);
    Tree := nil;
    isValid := False;
    Exit;
    end;

  R1.A.Y := R1.B.Y;
  Inc(R1.B.Y);
  P := New(PDTreeInfoView, Init(R1, PTreeView(Tree)));
  Insert(P);

  R1.Assign(R.B.X-13, R.A.Y+1, R.B.X-1, R.A.Y+3);
  P := New(PButton, Init(R1, GetString(dlOKButton), cmOK, bfDefault));
  Insert(P);
  R1.Assign(R.B.X-13, R.A.Y+4, R.B.X-1, R.A.Y+6);
  P := New(PButton, Init(R1, GetString(dlDriveButton), cmChangeDrive,
         bfBroadcast));
  {P^.Options := P^.Options and not ofSelectable;}
  Insert(P);
  R1.Assign(R.B.X-13, R.A.Y+7, R.B.X-1, R.A.Y+9);
  P := New(PButton, Init(R1, GetString(dlRereadButton), cmPanelReread,
         bfBroadcast));
  Insert(P);
  R1.Assign(R.B.X-13, R.A.Y+10, R.B.X-1, R.A.Y+12);
  P := New(PButton, Init(R1, GetString(dlMkDirButton), cmPanelMkDir,
         bfBroadcast));
  Insert(P);
  R1.Assign(R.B.X-13, R.A.Y+13, R.B.X-1, R.A.Y+15);
  P := New(PButton, Init(R1, GetString(dlCancelButton), cmCancel, 0));
  Insert(P);
  SelectNext(False);
  Options := Options or ofTopSelect;
  end { TTreeDialog.Init };

function TTreeDialog.GetPalette;
  const
    S: String[Length(CTreeDialog)] = CTreeDialog;
  begin
  GetPalette := @S;
  end;

function TTreeDialog.Valid;
  begin
  Valid := isValid and inherited Valid(Command)
  end;

(*procedure TTreeDialog.HandleEvent;
begin
 {if Event.What = evCommand then Tree^.HandleEvent(Event);}
 inherited HandleEvent(Event);
end;*)

constructor TTreeWindow.Init;
  var
    R: TRect;
    P: PView;
    S: PScrollBar;
  begin
  inherited Init(Bounds, GetString(dlTreeTitle), 0);
  GetExtent(R);
  R.Grow(-1, -1);
  R.A.X := R.B.X;
  Inc(R.B.X);
  S := StandardScrollBar(sbVertical+sbHandleKeyboard);
  GetExtent(R);
  R.Grow(-1, -1);
  Dec(R.B.Y, 2);
  P := New(PTreePanel, Init(R, 0, True, S));
  Insert(P);

  GetExtent(R);
  R.Grow(-1, -1);
  R.A.Y := R.B.Y-2;
  P := New(PTreeInfoView, Init(R, PTreeView(P)));
  Insert(P);
  end { TTreeWindow.Init };

constructor TTreeWindow.Load;
  begin
  inherited Load(S);
  PTreeView(Current)^.ReadAfterLoad;
  end;

procedure TTreeWindow.Store;
  begin
  inherited Store(S);
  end;

function TTreeWindow.GetPalette;
  const
    S: String[Length(CTreeDialog)] = CTreeDialog;
  begin
  GetPalette := @S;
  end;

procedure TTreeWindow.HandleEvent(var Event: TEvent);
  begin
  case Event.What of
    evKeyDown:
      if  (Event.KeyCode = kbESC) then
        begin
        Message(Application, evCommand, cmClose, nil);
        ClearEvent(Event);
        end;
  end {case};
  inherited HandleEvent(Event);
  end;

constructor TTreeView.Init;
  var
    I, Lv: Integer;
    S, D: String;
    P: PDirRec;
  begin
  inherited Init(R);
  Abort := False;
  if ParitalView then
    HelpCtx := hcDirTree;
  EventMask := $FFFF;
  lGetDir(ADrive, CurPath); {DataCompBoy}
  ScrollBar := ScrBar;
  Drive := ADrive;
  LastPath := CurPath;
  Options := Options or ofSelectable {or ofTopSelect} or ofFirstClick;
  Parital := ParitalView;
  if Parital then
    Options := Options or ofTopSelect;
  GrowMode := gfGrowHiX+gfGrowHiY;
  WasChanged := False;
  Dirs := nil;
  DC := nil;
  MouseTracking := False;
  LocateEnabled := True;
  if not Abort then
    ReadTree(False);
  StopQuickSearch;
  DrawDisabled := False;
  isValid := not Abort and (ScrollBar <> nil);
  end { TTreeView.Init };

destructor TTreeView.Done;
  begin
  if DC <> nil then
    begin
    DC^.DeleteAll;
    Dispose(DC, Done);
    DC := nil;
    end;
  inherited Done;
  end;

function TTreeView.GetPalette;
  const
    S: String[Length(CTreeView)] = CTreeView;
  begin
  GetPalette := @S;
  end;

constructor TTreeView.Load;
  var
    P: Pointer;
  begin
  inherited Load(S);
  GetPeerViewPtr(S, ScrollBar);
  S.Read(Parital, 1);
  S.ReadStrV(LastPath);
  {S.Read(LastPath[0], 1); S.Read(LastPath[1], Length(LastPath));}
  CurPath := LastPath;
  Drive := Byte(LastPath[1])-64;
  StopQuickSearch;
  DrawDisabled := False;
  LocateEnabled := True;
  Dirs := nil;
  DC := nil;
  isValid := True;
  WasChanged := False;
  MouseTracking := False;
  end;

procedure TTreeView.ReadAfterLoad;
  begin
  Abort := False;
  ReadTree(False);
  isValid := not Abort and (ScrollBar <> nil);
  end;

function THTreeView.GetPalette;
  const
    S: String[Length(CHTreeView)] = CHTreeView;
  begin
  GetPalette := @S;
  end;

procedure TTreeView.Store;
  begin
  inherited Store(S);
  PutPeerViewPtr(S, ScrollBar);
  S.Write(Parital, 1);
  S.WriteStr(@LastPath); {S.Write(LastPath, Length(LastPath)+1);}
  end;

function TTreeView.Valid;
  begin
  Valid := isValid and inherited Valid(Command)
  end;

procedure TTreeView.SetData;
  begin
  end;

{-DataCompBoy-}
procedure TTreeView.GetData;
  var
    S: String;
  begin
  S := GetDirName(ScrollBar^.Value);
  String(Rec) := S;
  LastPath := S;
  end;
{-DataCompBoy-}

function TTreeView.GetDirName;
  var
    S: String;
    I: Integer;
    P: PDirRec;
  begin
  I := N;
  P := DC^.At(I);
  if I = 0 then
    S := ''
  else
    S := P^.DirName[uLfn];
  repeat
    if I > 0 then
      repeat
        Dec(I)
      until (I < 0) or (PDirRec(DC^.At(I))^.Level < P^.Level);
    if I >= 0 then
      P := DC^.At(I);
    S := MakeNormName(P^.DirName[uLfn], S); {DataCompBoy}

  until I <= 0;
  GetDirName := S;
  end { TTreeView.GetDirName };

function TTreeView.DataSize;
  begin
  DataSize := 256;
  end;

procedure TTreeView.CollapseBranch;
  var
    L, I: Integer;
    P, P1: PDirRec;

  function Find(N: Integer): Integer;
    var
      I: Integer;
    begin
    Find := -1;
    for I := 1 to DC^.Count do
      if PDirRec(DC^.At(I-1))^.Number = N then
        begin
        Find := I-1;
        Exit
        end;
    end;

  begin
  I := Find(N);
  if  (I <= 0) or not Parital then
    Exit;
  P := DC^.At(I);
  if Expanded(P, I+1) then
    begin
    while (I < DC^.Count-1) and (P^.Level < PDirRec(DC^.At(I+1))^.Level)
    do
      DC^.AtDelete(I+1);
    end
  else
    begin
    L := N+1;
    while (L < Dirs^.Count) and (PDirRec(Dirs^.At(L))^.Level > P^.Level)
    do
      begin
      if PDirRec(Dirs^.At(L))^.Level = P^.Level+1 then
        begin
        DC^.AtInsert(I+1, Dirs^.At(L));
        Inc(I);
        end;
      Inc(L);
      end;
    end;
  ScrollBar^.SetParams(ScrollBar^.Value, 0, DC^.Count-1, DC^.Count, 1);
  end { TTreeView.CollapseBranch };

procedure TTreeView.HandleEvent;
  begin
  if Valid(0) then
    begin
    inherited HandleEvent(Event);
    HandleCommand(Event);
    if not Valid(0) then
      Message(Owner, evCommand, cmCancel, nil)
    end;
  end;

{AK155}
function MkFcFromDirRec(D: PDirRec; var FullName: String)
  : PFilesCollection;
  var
    l: LongInt;
    fr: PFileRec;
  begin
  l := Length(FullName);
  while FullName[l] <> '/' do // slash change by unxed
    Dec(l);
  SetLength(FullName, l-1);
  New(Result, Init(1, 1));
  l := Length(D^.DirName[True]);
  GetMem(fr, TFileRecFixedSize+l);
  with fr^ do
    begin
    Move(D^.DirName, FlName, SizeOf(FlName[True])+1+l);
    Attr := Directory;
    Owner := @FullName;
    DIZ := nil;
    end;
  Result^.Insert(fr);
  end;

procedure TTreeView.HandleCommand;
  label NoLoc;
  var
    CurPos, I: Integer;
    PD: PDirRec;
    C: Char;
    DR: String;
    MP: TPoint;

  procedure CE;
    begin
    ClearEvent(Event)
    end;

  function SearchForMask(Delta: Integer): Boolean;
    var
      I: Integer;
    begin
    SearchForMask := False;
    if QSMask[1] = '/' then // slash change by unxed
      begin
      ScrollBar^.SetValue(0);
      SearchForMask := True;
      InitQuickSearch(@Self);
      Exit;
      end;
    I := ScrollBar^.Value+Delta;
    if I = 0 then
      Inc(I);
    while (I < DC^.Count) and
      not InMask(PDirRec(DC^.At(I))^.DirName[uLfn],
         QSMask)
    do
      Inc(I); {DataCompBoy}
    if I >= DC^.Count then
      begin
      I := 0;
      while (I < ScrollBar^.Value) and
        not InMask(PDirRec(DC^.At(I))^.DirName[uLfn],
           QSMask)
      do
        Inc(I); {DataCompBoy}
      if I >= ScrollBar^.Value then
        I := 0;
      end;
    if I > 0 then
      begin
      SearchForMask := True;
      if I = ScrollBar^.Value then
        DrawView
      else
        ScrollBar^.SetValue(I);
      end;
    end { SearchForMask };

  procedure CancelSearch;
    begin
    if not QuickSearch then
      Exit;
    StopQuickSearch;
    DrawView;
    end;

  procedure ChangeDrive;
    var
      T: TPoint;
      S: String;
    begin
    T.X := Size.X div 2;
    T.Y := 1;
    MakeGlobal(T, T);
    Desktop^.MakeLocal(T, T);
    S := SelectDrive(T.X, T.Y, CurPath[1], False);
    if S = '' then
      Exit;
    ClrIO;
    lGetDir(Byte(S[1])-64, S);
    if Abort then
      Exit; {DataCompBoy}
    CurPath := S;
    LastPath := S;
    if not Abort then
      begin
      ReadTree(False);
      if isValid then
        DrawView;
      end;
    end { ChangeDrive };

  procedure SendLocated;
    begin
    if LocateEnabled then
      Message(Owner, evCommand, cmChangeDirectory, @CurPath);
    end;

  procedure ExpandBranches;
    var
      B: Boolean;
      I: Integer;
      S: String;
      P: PDirRec;
    begin
    B := True;
    GetData(S);
    for I := 1 to DC^.Count do
      begin
      P := DC^.At(I-1);
      B := B and Expanded(P, I);
      end;
    if not B then
      begin
      DC^.DeleteAll;
      for I := 1 to Dirs^.Count do
        DC^.Insert(Dirs^.At(I-1));
      end
    else
      Reread(False);
    if Valid(0) then
      begin
      I := FindDir(S);
      if I < 0 then
        I := 0;
      ScrollBar^.SetParams(I, 0, DC^.Count-1, DC^.Count, 1);
      DrawView;
      end;
    end { ExpandBranches };

  {-DataCompBoy-}
  procedure MkDirectory;
    var
      OldDir, NewDir: String;
    begin
    lGetDir(0, OldDir);
    GetData(NewDir);
    lChDir(NewDir);
    MakeDirectory;
    lChDir(OldDir);
    GlobalMessage(evCommand, cmRereadInfo, nil);
    end;
  {-DataCompBoy-}

  {-DataCompBoy-}
  procedure EraseDir;
    var
      FC: PFilesCollection;
      D: PDirRec;
      S: String;
      OldDir: String;
    begin
    CE;
    if ScrollBar^.Value < 1 then
      Exit;
    GetData(S);
    ClrIO;
    lGetDir(0, OldDir);
    if Abort then
      Exit;
    lChDir(GetPath(S));
    D := DC^.At(ScrollBar^.Value);
    FC := MkFcFromDirRec(D, S);
    EraseFiles(FC);
    Dispose(FC, Done);
    lChDir(OldDir);
    ClrIO;
    end { EraseDir };
  {-DataCompBoy-}

  procedure QuickChange(const SS: String);
    begin
    if SS <> '' then
      Message(@Self, evCommand, cmChangeTree, @SS);
    end;

  var
    Ev: TEvent;

  begin { TTreeView.HandleCommand }
  CurPos := ScrollBar^.Value;
  {P := DC^.At(CurPos);}
  if QuickSearch and (Event.What = evKeyDown) and (Event.CharCode < #32)
    and (Event.KeyCode <> kbBack) and (Event.KeyCode <> kbCtrlEnter)
  then
    CancelSearch;
  case Event.What of
    evCommand:
      case Event.Command of

        cmPanelErase:
          EraseDir;
        cmDoSendLocated:
          begin
          SendLocated;
          CE
          end;
        cmPanelMkDir:
          begin
          MkDirectory;
          CE
          end;
        cmChangeDrive:
          begin
          ChangeDrive;
          CE;
          end;
        cmFindTree:
          if Char(Event.InfoPtr^) = LastPath[1] then
            Char(Event.InfoPtr^) := #0;
        cmRevert:
          begin
          ScrollBar^.SetValue(CurNum);
          CurPtr := DC^.At(ScrollBar^.Value);
          Message(Owner, evBroadcast, cmDirChanged, @CurPath);
          CE
          end;
        cmRereadTree:
          begin
          if Dirs = nil then
            Exit;
          if CurPath[1] = PString(Event.InfoPtr)^[1] then
            Reread(False);
          if Valid(0) then
            begin
            CurPtr := DC^.At(ScrollBar^.Value);
            Message(Owner, evBroadcast, cmDirChanged, @CurPath);
            end;
          end;
        cmPanelReread, cmRereadForced, cmForceRescan
        :
          begin
          Reread(True);
          if Valid(0) then
            begin
            CurPtr := DC^.At(ScrollBar^.Value);
            Message(Owner, evBroadcast, cmDirChanged, @CurPath);
            Message(Application, evBroadcast, cmTreeChanged, @CurPath);
            end;
          end;
        cmChangeTree:
          begin
          if PString(Event.InfoPtr)^[2] <> ':' then
            {Cat:warn могут быть проблемы с сетевыми путями}
            begin
            CE;
            Exit;
            end;
          CurPath[1] := UpCase(CurPath[1]);
          LastPath[1] := UpCase(LastPath[1]);
          LocateEnabled := True;
          LastPath := PString(Event.InfoPtr)^;
          if Dirs = nil then
            begin
            WasChanged := CurPath[1] <> LastPath[1];
            CurPath := LastPath;
            Exit;
            end;
          LocateEnabled := False;
          if  ( (LastPath[1] <> CurPath[1]) or WasChanged)
            and (ValidDrive(LastPath[1]))
          then
            begin
            LocateEnabled := False;
            WasChanged := False;
            CurPath := LastPath;
            Reread(False);
            if DC = nil then
              begin
              CE;
              Exit;
              end; {JO}
            if Valid(0) then
              begin
              if Dirs = nil then
                Exit;
              CurPtr := DC^.At(ScrollBar^.Value);
              Message(Owner, evBroadcast, cmDirChanged, @CurPath);
              LocateEnabled := True;
              CE;
              Exit;
              end;
            end;
          CurPath := LastPath;
          CurNum := Tree.FindDir(Dirs, CurPath);
          I := FindDir(CurPath);
          if I >= 0 then
            ScrollBar^.SetValue(I);
          LocateEnabled := True;
          CE;
          DrawView;
          CurPtr := DC^.At(ScrollBar^.Value);
          Message(Owner, evBroadcast, cmDirChanged, @CurPath);
          end;
        cmGetName:
          PString(Event.InfoPtr)^:= GetString(dlTreeTitle);
        cmGetDirName:
          PString(Event.InfoPtr)^:= GetDirName(ScrollBar^.Value);
      end {case};
    evKeyDown:
      case Event.KeyCode of
        kbAlt1, kbAlt2, kbAlt3, kbAlt4, kbAlt5, kbAlt6, kbAlt7, kbAlt8,
         kbAlt9:
          begin
          QuickChange(CnvString(DirsToChange[Event.ScanCode-Hi(kbAlt1)]))
          ;
          CE
          end;
        kbEnter:
          if Parital then
            begin
            SendLocated;
            CE
            end;
        kbDel:
          if  (FMSetup.Options and fmoDelErase <> 0) then
            EraseDir;
        kbGrayAst:
          begin
          if Parital then
            ExpandBranches;
          CE;
          end;
        kbSpace, kbGrayPlus, kbGrayMinus:
          begin
          StopQuickSearch;
          if Parital then
            begin
            CollapseBranch(PDirRec(DC^.At(CurPos))^.Number);
            DrawView;
            end;
          CE;
          end;
        kbRight:
          Event.KeyCode := kbDown;
        kbLeft:
          Event.KeyCode := kbUp;
        kbCtrlEnter:
          begin
          if QuickSearch then
            begin
            SearchForMask(1);
            CE;
            end;
          Exit;
          end;
        {-DataCompBoy-}
        kbBack:
          if QuickSearch then
            if QSMask = '' then
              CancelSearch
            else
              begin
              DoQuickSearch(Event.KeyCode);
              ScrollBar^.SetValue(0);
              SearchForMask(0);
              DrawView;
              CE
              end;
        else {case}
          if Event.CharCode >= #32 then
            begin
            if QuickSearch then
              begin
              if  (Event.CharCode = '/') and (QSMask <> '') // slash change by unxed
              then
                begin
                CE;
                PD := DC^.At(CurPos);
                while (PD^.Attr and trHasBranch = 0) do
                  begin
                  if not SearchForMask(1) then
                    Exit;
                  PD := DC^.At(ScrollBar^.Value);
                  end;
                ScrollBar^.SetValue(ScrollBar^.Value+1);
                if Parital and (PD^.Attr and trExpanded = 0) then
                  CollapseBranch(PDirRec(DC^.At(CurPos))^.Number);
                InitQuickSearch(@Self);
                DrawView;
                Exit;
                end;
              DoQuickSearch(Event.KeyCode);
              if not SearchForMask(0) then
                DoQuickSearch(kbBack);
              end
            else
              begin
              InitQuickSearch(@Self);
              DoQuickSearch(Event.KeyCode);
              SearchForMask(0);
              if QSMask = '/' then // slash change by unxed
                InitQuickSearch(@Self);
              end;
            CE;
            end;
      end {case};
    {-DataCompBoy-}
    evBroadcast:
      case Event.Command of
        cmPanelReread,
        cmPanelMkDir,
        cmChangeDrive:
          Message(@Self, evCommand, Event.Command, nil);
        cmTreeChanged:
          begin
          if  (Dirs <> nil)
               and (PString(Event.InfoPtr)^[1] = CurPath[1])
          then
            Reread(False);
          end;
        cmDropped:
          begin
          MP := PCopyRec(Event.InfoPtr)^.Where;
          if not MouseInView(MP) then
            begin
            CE;
            Exit;
            end;
          MakeLocal(MP, MP);
          I := Delta.Y+MP.Y;
          if I >= DC^.Count then
            begin
            CE;
            Exit;
            end;
          CopyDirName := GetDirName(I);
          if PCopyRec(Event.InfoPtr)^.Owner <> nil then
            begin
            Ev.What := evBroadcast;
            Ev.Command := cmUnArchive;
            Ev.InfoPtr := Event.InfoPtr;
            PCopyRec(Event.InfoPtr)^.Owner^.HandleEvent(Ev);
            if Ev.What = evNothing then
              begin
              CE;
              Exit;
              end;
            end;
          if ReflectCopyDirection
          then
            RevertBar := not (Message(Desktop, evBroadcast,
                   cmIsRightPanel, @Self) <> nil)
          else
            RevertBar := False;
          CopyFiles(PCopyRec(Event.InfoPtr)^.FC,
             PCopyRec(Event.InfoPtr)^.Owner,
            ShiftState and 3 <> 0, 0);
          CE;
          end;
        cmScrollBarChanged:
          if ScrollBar = Event.InfoPtr then
            begin
            DrawView;
            CE;
            GetData(CurPath);
            CurPtr := DC^.At(CurPos);
            Message(Owner, evBroadcast, cmDirChanged, @CurPath);
            if not MouseTracking and (FMSetup.Options and
                 fmoAutoChangeDir <> 0)
            then
              NeedLocated := GetSTime;
            end;
      end {case};
    evMouseDown:
      begin
      MakeLocal(Event.Where, MP);
      if MP.Y+Delta.Y < DC^.Count then
        begin
        PD := DC^.At(MP.Y+Delta.Y);
        if  (PD^.Level > 0) and
            (MP.X >= PD^.Level*3+1+Delta.X) and
            (MP.X <= PD^.Level*3+3+Delta.X)
        then
          begin
          ScrollBar^.SetValue(MP.Y+Delta.Y);
          Message(@Self, evKeyDown, $3920, nil);
          while MouseEvent(Event, evMouseAuto+evMouseMove) do
            ;
          end
        else
          {if (MP.X >= P^.Level*3 + 1 + 3*Byte(P^.Level>0) + Delta.X) and
                      (MP.X <= P^.Level*3 + 2 + 3*Byte(P^.Level>0) + Delta.X + Length(P^.Name)) then}
          begin
          if Event.Double and not Parital then
            begin
            ScrollBar^.SetValue(MP.Y+Delta.Y);
            Message(Owner, evCommand, cmOK, nil);
            CE
            end;
          CurPos := RepeatDelay;
          RepeatDelay := 0;
          MouseTracking := True;
          repeat
            MakeLocal(Event.Where, MP);
            if  (MP.X > 0) and (MP.X < Size.X) then
              ScrollBar^.SetValue(MP.Y+Delta.Y);
          until not MouseEvent(Event, evMouseAuto+evMouseMove);
          MouseTracking := False;
          SendLocated;
          RepeatDelay := CurPos;
          end;
        CE;
        end;
      end;
  end {case};
  end { TTreeView.HandleCommand };

function TTreeView.Expanded(P: PDirRec; i: Integer): Boolean;
  begin
  Expanded := True;
  if  (i >= DC^.Count) then
    begin
    if  (P^.Number+1 <> Dirs^.Count) then
      Expanded := False;
    end
  else if PDirRec(DC^.At(i))^.Number <> P^.Number+1 then
    Expanded := False;
  end;

procedure TTreeView.Draw;
  var
    Levels: array[0..255] of Boolean;
    I, J, K, CurPos, Idx: Integer;
    C: Char;
    S, Q: String;
    B: TDrawBuffer;
    C1, C2, C3, C4, CC: Byte;
    P: PDirRec;
  begin
  if  (DrawDisabled) or (DC = nil) then
    Exit;
  FillChar(Levels, 255, 0);
  C1 := GetColor(1);
  C2 := GetColor(2);
  C3 := GetColor(3);
  C4 := GetColor(4);
  ScrollBar^.PgStep := Size.Y*((Size.X+1) div 13);
  if Owner^.GetState(sfActive) and GetState(sfSelected) then
    C3 := GetColor(3)
  else
    C3 := GetColor(6);
  CurPos := ScrollBar^.Value;
  if CurPos < Delta.Y then
    Delta.Y := CurPos;
  if CurPos >= Delta.Y+Size.Y then
    Delta.Y := CurPos-Size.Y+1;
  P := DC^.At(CurPos);
  if  (P^.Number = CurNum) and not Parital then
    if Owner^.GetState(sfActive) and GetState(sfSelected) then
      C3 := GetColor(5)
    else
      C3 := GetColor(7);
  if P^.Level*3+6+Length(P^.DirName[uLfn])+Delta.X > Size.X then
    Delta.X := P^.Level*3+6+Length(P^.DirName[uLfn])-Size.X;
  if P^.Level*3-4 < Delta.X then
    Delta.X := P^.Level*3-4;
  if Delta.X < 0 then
    Delta.X := 0;
  for I := 0 to Delta.Y-1 do
    begin
    P := DC^.At(I);
    Levels[P^.Level] := (P^.Attr and 1 = 1);
    end;
  for I := 1 to Size.Y do
    begin
    MoveChar(B, ' ', C1, 200);
    Idx := I+Delta.Y-1;
    if Idx < DC^.Count then
      begin
      P := DC^.At(Idx);
      if P^.Attr and 1 = 1 then
        C := #195
      else
        C := #192;
      if P^.Level = 0 then
        S := ''
      else
        begin
        {if Parital then S := C + #196'[ ] ' else}S :=
           C+#196#196#196#196' ';
        if  (P^.Attr and trHasBranch <> 0) and
            (PDirRec(Dirs^.At(P^.Number+1))^.Level > P^.Level)
        then
          if Parital then
            if Expanded(P, Idx+1) then
              S := C+#196'[-] '
            else
              S := C+#196'[+] '
          else
            S[4] := #194;
        end;
      K := 2;
      if P^.Level > 0 then
        for J := 1 to P^.Level-1 do
          begin
          if Levels[J] then
            MoveChar(B[K], #179, C1, 1);
          Inc(K, 3);
          end;
      Levels[P^.Level] := (P^.Attr and 1 = 1);
      MoveStr(B[K], S, C1);
      if Idx = CurPos then
        CC := C3
      else if (P^.Number = CurNum) and not Parital then
        CC := C4
      else
        CC := C2;
      {-DataCompBoy-}
      Q := P^.DirName[uLfn];
      if CurPos = Idx then
        begin
        MoveStr(B[K+Length(S)], {' '#0+}Q {+#0' '}, CC);
        {JO: зачем нужна была эта оторочка имён нулями - тайна,}
        if QuickSearch then
          {    покрытая мраком, но из-за неё каталоги в панели}
          begin {    оторочены мерзкими точечками или чем ещё похуже}
          ShowCursor;
          NormalCursor;
          SetCursor(K+Length(S)+LastSuccessPos-1, I-1)
          end
        else
          HideCursor;
        end
      else
        begin {q:=#0+q+#0;}
        MoveStr(B[K+Length(S)], Q, CC)
        end;
      {-DataCompBoy-}
      end;
    WriteLine(0, I-1, Size.X, 1, B[Delta.X]);
    end;
  end { TTreeView.Draw };

{-DataCompBoy-}
procedure TDirCollection.FreeItem;
  begin
  Dispose(PDirRec(P));
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDirCollection.PutItem;
  begin
  S.Write(Item^, SizeOf(TDirRec));
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TDirCollection.GetItem;
  var
    Item: PDirRec;
  begin
  New(Item);
  S.Read(Item^, SizeOf(TDirRec));
  if  (Item^.NumFiles < 0) or (Item^.Size < 0) then
    begin
    Item^.NumFiles := 0;
    Item^.NumFiles := 0;
    end;
  GetItem := Item;
  end;
{-DataCompBoy-}

procedure TTreeView.ReadTree;
  label Rep;
  var
    P: PDirRec;
    DCEntry: Integer;
    Idx: Integer;
    D, S: String; {DataCompBoy}
    Lv, I: Integer;
    Dr: array[1..255] of Integer;
    PD: PDirRec;
    iLFN: TUseLFN;
  begin
  Abort := False;
  if DC <> nil then
    begin
    DC^.DeleteAll;
    Dispose(DC, Done);
    end;
  DC := GetDirCollection(CurPath[1], CountLen);
  isValid := not Abort;
  if  (DC = nil) then
    begin
    Abort := True;
    isValid := False;
    Exit
    end;
  if DC^.Count = 0 then
    begin
    New(PD);
    PD^.Cluster := 0;
    PD^.Level := 0;
    PD^.Size := -1;
    PD^.Attr := 0;
    S := Copy(CurPath, 1, 3);
    for iLFN := Low(TUseLFN) to High(TUseLFN) do
      CopyShortString(S, PD^.DirName[iLFN]);
    DC^.Insert(PD);
    end;
  if LowMemory then
    begin
    Abort := True;
    Exit
    end;
  if not Abort and (ScrollBar <> nil) and (DC^.Count > 0) then
    begin
    lGetDir(Byte(CurPath[1])-64, CurPath);
    if Abort then
      Exit; {DataCompBoy}
    D := CurPath;
    Delete(D, 1, 3);
    Dirs := DC;
    DC := New(PDirCollection, Init(Dirs^.Count, 10));
    for I := 1 to Dirs^.Count do
      DC^.Insert(Dirs^.At(I-1));
    DrawDisabled := True;
    if Parital then
      begin
      I := 1;
      while (I < DC^.Count) do
        begin
        CollapseBranch(PDirRec(DC^.At(I))^.Number);
        Inc(I);
        end;
      end;
    DrawDisabled := False;
    I := FindDir(CurPath);
    CurNum := I;
    Delta.Y := 0;
    Delta.X := 0;
    Lv := CurNum;
    ScrollBar^.SetParams(Lv, 0, DC^.Count-1, DC^.Count, 1);
    Lv := ScrollBar^.Value;
    if Lv < 0 then
      Lv := 0;
    CurPtr := DC^.At(Lv);
    end
  else
    isValid := False;
  isValid := not Abort;
  end { TTreeView.ReadTree };

function TTreeView.FindDir;
  var
    N, I: Integer;

  function Find(N: Integer): Integer;
    var
      I: Integer;
    begin
    Find := -1;
    for I := 1 to DC^.Count do
      if PDirRec(DC^.At(I-1))^.Number = N then
        begin
        Find := I-1;
        Exit
        end;
    end;

  procedure ExpandFor(N: Integer);
    var
      I, Lv, CurLv: Integer;
      Lvs: array[1..255] of Integer;
    begin
    I := N-1;
    CurLv := PDirRec(Dirs^.At(N))^.Level;
    Lvs[CurLv] := N;
    Lv := CurLv;
    repeat
      Dec(Lv);
      while (I >= 0) and (PDirRec(Dirs^.At(I))^.Level > Lv) do
        Dec(I);
      Lvs[Lv] := I;
    until (I = 0) or (Find(I) >= 0);
    for I := Lv to CurLv-1 do
      CollapseBranch(Lvs[I]);
    end;

  begin { TTreeView.FindDir }
  N := Tree.FindDir(Dirs, Dir);
  if Parital and (N >= 0) then
    begin
    I := Find(N);
    FindDir := I;
    if I >= 0 then
      Exit;
    ExpandFor(N);
    FindDir := Find(N);
    end
  else
    FindDir := N;
  end { TTreeView.FindDir };

procedure TTreeView.SetState;
  begin
  inherited SetState(AState, Enable);
  if  (AState and sfFocused <> 0) and not Enable then
    if Parital then
      DisableCommands([cmCopyFiles, cmPanelErase, cmMoveFiles,
         cmPanelMkDir,
        cmChangeDrive, cmPanelReread]);
  if AState and (sfFocused or sfActive or sfSelected) <> 0 then
    if Owner^.GetState(sfActive) and GetState(sfSelected) then
      begin
      if ScrollBar <> nil then
        ScrollBar^.Show;
      if Parital then
        EnableCommands([cmCopyFiles, cmPanelErase, cmMoveFiles,
           cmPanelReread,
          cmPanelMkDir, cmChangeDrive]);
      {EventMask := EventMask or evBroadcast;}
      DrawView
      end
    else
      begin
      if ScrollBar <> nil then
        ScrollBar^.Hide;
      {EventMask := EventMask and (not evBroadcast);}
      DrawView
      end;
  end { TTreeView.SetState };

procedure TTreeView.Reread;
  var
    S: String;
    I, M: Integer;
  begin
  DrawDisabled := True;
  DC^.DeleteAll;
  Dispose(DC, Done);
  DC := nil;
  M := ScrollBar^.Value;
  LocateEnabled := False;
  ReadTree(CountLen);
  if Valid(0) then
    begin
    I := FindDir(LastPath);
    if I < 0 then
      I := M;
    LocateEnabled := True;
    ScrollBar^.SetValue(I);
    DrawDisabled := False;
    DrawView;
    end;
  end { TTreeView.Reread };

procedure TTreePanel.HandleEvent;
  procedure CE;
    begin
    ClearEvent(Event)
    end;

  {-DataCompBoy-}
  procedure CopyDir;
    var
      FC: PFilesCollection;
      D: PDirRec;
      S: String;
      OldDir, NewDir: String;
    begin
    lGetDir(0, OldDir);
    GetData(NewDir);
    lChDir(MakeNormName(NewDir, '..'));
    CE;
    if ScrollBar^.Value < 1 then
      Exit;
    GetData(S);
    D := DC^.At(ScrollBar^.Value);
    FC := MkFcFromDirRec(D, S);
    if ReflectCopyDirection
    then
      RevertBar := (Message(Desktop, evBroadcast, cmIsRightPanel, @Self)
           <> nil)
    else
      RevertBar := False;
    CopyFiles(FC, nil, Event.Command = cmMoveFiles, 0);
    Dispose(FC, Done);
    FC := nil;
    lChDir(OldDir);
    GlobalMessage(evCommand, cmRereadInfo, nil);
    end { CopyDir };
  {-DataCompBoy-}

  begin { TTreePanel.HandleEvent }
  inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      case Event.Command of
        cmMoveFiles, cmCopyFiles:
          CopyDir;
      end {case};
  end {case};
  end { TTreePanel.HandleEvent };

constructor THTreeView.Init;
  begin
  inherited Init(R, ADrive, True, ScrBar);
  Info := nil;
  end;

constructor THTreeView.Load;
  begin
  inherited Load(S);
  GetPeerViewPtr(S, Info);
  end;

procedure THTreeView.Store;
  begin
  inherited Store(S);
  PutPeerViewPtr(S, Info);
  end;

procedure THTreeView.ChangeBounds;
  var
    R: TRect;
  begin
  Dec(Bounds.B.Y, 2);
  SetBounds(Bounds);
  R := Bounds;
  R.A.Y := R.B.Y;
  Inc(R.B.Y, 2);
  if Info <> nil then
    Info^.SetBounds(R);
  if ScrollBar <> nil then
    begin
    R := Bounds;
    R.A.X := R.B.X;
    Inc(R.B.X);
    ScrollBar^.SetBounds(R)
    end
  end;

{ AK155 10.06.05. Подвал надо прятать и показывать вместе с панелью }
procedure THTreeView.SetState(AState: Word; Enable: Boolean);
  begin
  inherited SetState(AState, Enable);
  if (AState and sfVisible <> 0) and (Info <> nil) then
     Info^.SetState(sfVisible, Enable);
  end;

{ AK155 26-01-2003. Раньше Info не освобождалось вообще }
destructor THTreeView.Done;
  begin
  if Info <> nil then
    Info^.Free;
  inherited Done;
  end;

{-DataCompBoy-}
function CreateDirInheritance;
  var
    I, J: Integer;
    SR: lSearchRec;
    M: String;
  begin
  Result := 0;
  ClrIO;
  S := lFExpand(S);
  MakeSlash(S);
  if Abort then
    Exit;
  I := GetRootStart(S);
  if I > Length(S) then
    Exit;
  while I < Length(S) do
    begin
    J := I;  // указывает на '\' перед началом имени на очередном уровне
    repeat
      Inc(I);
    until (S[I] = '/'); // slash change by unxed
     // I указывает на первый символ за концом имени
    M := Copy(S, 1, I-1); // полный путь очередного уровня
    ClrIO;
    lFindFirst(M, AnyFileDir, SR); {JO}
    lFindClose(SR);
    if Abort then
      Exit;
    if DosError <> 0 then
      begin // каталог не найден, надо создавать
      if Confirm and (Confirms and cfCreateSubdir <> 0) then
        begin
        if  (MessageBox(GetString(dlQueryCreateDir)+Cut(S, 40)+' ?',
               nil, mfYesNoConfirm) <> cmYes)
        then
          Exit;
        end;
      ClrIO;
      CheckMkDir(M);
      if Abort then
        Exit;
      // Каталог создан успешно
      if Result = 0 then // это был первый созданный каталог
        Result := J-1;
      end;
    end;
  end { CreateDirInheritance };
{-DataCompBoy-}

end.
