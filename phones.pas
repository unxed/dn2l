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

unit Phones;

interface

uses
  Defines, Objects2, Streams, Drivers, Dialogs, Menus,
  Views, DNStdDlg, Objects, StrView
  ;

type
  PPhoneCollection = ^TPhoneCollection;
  TPhoneCollection = object(TSortedCollection)
    constructor Init(ALimit, ADelta: LongInt);
    constructor Load(var S: TStream);
    constructor ShortLoad(var S: TStream);
    procedure ShortStore(var S: TStream);
    function Compare(P1, P2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    end;

  PPhoneDir = ^TPhoneDir;
  TPhoneDir = object(TObject)
    Name: String[30];
    Memo1: PString;
    Memo2: PString;
    Password: String[15];
    Phones: PCollection;
    Encrypted: Boolean;
    constructor Init(const APassword, AName, AMemo1, AMemo2: String);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    destructor Done; virtual;
    end;

  PPhone = ^TPhone;
  TPhone = object(TObject)
    Name: String[30];
    Memo1: PString;
    Memo2: PString;
    Number: PString;
    constructor Init(const ANumber, AName, AMemo1, AMemo2: String);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    destructor Done; virtual;
    end;

  PPhoneBox = ^TPhoneBox;
  TPhoneBox = object(TSortedListBox)
    GroupLabel, ItemLabel: PLabel;
    AlphaMode, SearchMode: Boolean;
    Phones: PCollection;
    Active: PPhoneDir;
    Info: PDStringView;
    destructor Done; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    function GetKey(var S: String): Pointer; virtual;
    procedure SetList(Alpha: Boolean);
    {      procedure SetState(AState: Word; Enable: Boolean); virtual;}
    end;

procedure PhoneBook(Manual: Boolean);

implementation
uses
  DNApp, Startup, Commands, Messages, ObjType
  , DNHelp, Advance, Advance1, Advance2
  {$IFDEF Modem}, modemio, Terminal, uDialer {$ENDIF}
  ;

var
  EnterButton, ReturnButton: PButton;

function CryptWith(Pass, S: String): String;
  var
    I: Integer;
  begin
  if Pass <> '' then
    begin
    for I := 1 to Length(S) do
      S[I] := Char(Byte(S[I]) xor Byte(Pass[1+I mod Length(Pass)]) xor
             (I xor $AA));
    end;
  CryptWith := S;
  end;

var
  PPH: PPhone;

procedure PhoneBook(Manual: Boolean);
  var
    D: PDialog;
    R: TRect;
    PV: PView;
    PL: PPhoneBox;
    PC: PCollection;
    S: TBufStream;
    DT: record
      C: PCollection;
      n: Word;
      end;
    W: PWindow;
    I: Integer;
    SS: String;

  procedure DoSearchButton(P: PView);
    begin
    if  (TypeOf(P^) = TypeOf(TButton)) then
      if PButton(P)^.Command = cmOK then
        EnterButton := PButton(P)
      else if PButton(P)^.Command = cmNo then
        ReturnButton := PButton(P);
    end;

  begin { PhoneBook }
  PPH := nil;
  SS := '';
  if Manual then
    begin
    if  (ExecResource(dlgManualDial, SS) <> cmOK)
         or (DelSpaces(SS) = '')
    then
      Exit;
    New(PPH, Init(SS, GetString(dlMD_Manual)+SS, '', ''));
    end
  else
    begin
    D := PDialog(LoadResource(dlgPhoneBook));
    D^.ForEach(@DoSearchButton);
    ReturnButton^.Hide;
    PV := D^.StandardScrollBar(sbVertical+sbHandleKeyboard);
    R.Assign(D^.Size.X-3, 3, D^.Size.X-2, 12);
    PV^.Locate(R);

    R.Assign(2, 3, D^.Size.X-3, 12);
    PL := New(PPhoneBox, Init(R, 1, PScrollBar(PV)));
    PL^.Options := PL^.Options or ofPostProcess;
    S.Init(SourceDir+'DN.PHN', stOpenRead, 1024);
    PC := nil;
    PC := PCollection(S.Get);
    if PC = nil then
      begin
      S.Reset;
      S.Seek(0);
      PC := PCollection(New(PPhoneCollection, ShortLoad(S)));
      end;
    S.Done;
    if PC = nil then
      PC := New(PPhoneCollection, Init(10, 10));
    PC^.Pack;
    PL^.Phones := PC;
    PL^.Active := nil;
    PL^.AlphaMode := False;
    PL^.SearchMode := False;
    PL^.NewLisT(PC);

    D^.Insert(PL);
    R.Assign(2, 2, 53, 3);

    {    PV := New(PLabel, Init(R, GetString(dlPhonesLabel),PL));}
    PL^.GroupLabel := New(PLabel, Init(R, GetString(dlPhonesLabelGroup),
           PL));
    D^.Insert(PL^.GroupLabel);

    PL^.ItemLabel := New(PLabel, Init(R, GetString(dlPhonesLabelPhones),
           PL));
    PL^.ItemLabel^.Hide;
    D^.Insert(PL^.ItemLabel);


    D^.Insert(PL);
    {D^.Insert(PV);}
    R.Assign(2, 12, D^.Size.X-2, 14);
    PV := New(PDStringView, Init(R));
    PDStringView(PV)^.S1 := '';
    PDStringView(PV)^.S2 := '';
    D^.Insert(PV);
    PL^.Info := PDStringView(PV);

    R.A.X := Desktop^.ExecView(D);
    D^.GetData(DT);
    Dispose(D, Done);
    if R.A.X <> cmDialPhone then
      Exit;
    end;
  if PPH = nil then
    Exit;
  {$IFDEF Modem}
  if Dialer = nil then
    begin
    W := New(PAutoDialer, Init(PPH^.Name, PPH^.Number^));
    Application^.InsertWindow(W);
    end
  else
    Dialer^.SetPhone(PPH^.Name, PPH^.Number^);
  {$ENDIF}
  Dispose(PPH, Done);
  PPH := nil;
  end { PhoneBook };

function TPhoneBox.GetKey;
  const
    B: Byte = 0;
    ST: String[30] = '';
  begin
  ST := UpStrg(S);
  GetKey := @B;
  end;

{procedure TPhoneBox.SetState(AState: Word; Enable: Boolean);
begin
 Inherited SetState(AState, Enable);
 if (Active <> nil) and ((State And sfFocused)<>0) then
   if (Focused=0) then DisableCommands([cmDialPhone, cmImportPhones])
                  else EnableCommands([cmDialPhone, cmImportPhones])
end;
}
procedure TPhoneBox.SetList;
  var
    PC: PPhoneCollection;
    S: TBufStream;

  procedure DoPhones(Ph: PPhoneDir);

    procedure InsertPhone(P: PPhone);
      var
        PP: PPhoneDir;
        I: LongInt;
      begin
      if P <> nil then
        begin
        FreeStr := UpStrg(P^.Name);
        if  (Length(FreeStr) = 2) and (FreeStr = '..') then
          Exit;
        PP := New(PPhoneDir, Init('', FreeStr[1], '', ''));
        if PC^.Search(PP, I) then
          begin
          Dispose(PP, Done);
          PP := PC^.At(I);
          end
        else
          PC^.AtInsert(I, PP);
        if PP^.Phones = nil then
          PP^.Phones := New(PPhoneCollection, Init(10, 10));
        PSortedCollection(PP^.Phones)^.Duplicates := True;
        PP^.Phones^.Insert(P);
        end;
      end { InsertPhone };

    begin { DoPhones }
    if  (Ph <> nil) and (Ph^.Phones <> nil)
           and ((Ph^.Password = '') or (Ph^.Encrypted))
    then
      Ph^.Phones^.ForEach(@InsertPhone);
    Ph^.Phones^.DeleteAll;
    end { DoPhones };

  begin { TPhoneBox.SetList }
  if Phones = nil then
    Exit;
  if  (AlphaMode xor Alpha) then
    begin
    if Alpha then
      begin
      PC := New(PPhoneCollection, Init(10, 10));
      Phones^.ForEach(@DoPhones);
      end
    else
      begin
      S.Init(SourceDir+'DN.PHN', stOpenRead, 1024);
      PC := nil;
      PC := PPhoneCollection(S.Get);
      if PC = nil then
        begin
        S.Reset;
        S.Seek(0);
        PC := New(PPhoneCollection, ShortLoad(S));
        end;
      S.Done;
      if PC = nil then
        PC := New(PPhoneCollection, Init(10, 10));
      end;
    Dispose(Phones, Done);
    Phones := PC;
    end;
  AlphaMode := Alpha;
  List := nil;
  NewLisT(Phones);
  end { TPhoneBox.SetList };

procedure TPhoneBox.HandleEvent;
  var
    Dt: record
      Name: String[30];
      Number: String[100];
      Memo1, Memo2: String[50];
      end;
    C: PCollection;
    Stream: TBufStream;
    Ph: PPhone;

  function CheckPassword(Ph: PPhoneDir): Boolean;
    var
      S: String;
    begin
    CheckPassword := True;
    if  (Ph^.Password = '') or Ph^.Encrypted then
      Exit;
    repeat
      S := '';
      if ExecResource(dlgSetPassword, S) = cmCancel then
        begin
        CheckPassword := False;
        Exit
        end;
    until Ph^.Password = S;
    Ph^.Encrypted := True;
    end;

  procedure EditDirectory(Append: Boolean);
    var
      D: PDialog;
      Dt: record
        Name: String[30];
        Password: String[15];
        Memo1, Memo2: String[50];
        end;
      R: TRect;
      P: PView;
      Idx: TDlgIdx;
      Ph: PPhoneDir;
      {      PC: PCollection;}
    begin
    FillChar(Dt, SizeOf(Dt), 0);
    if not Append then
      begin
      Ph := List^.At(Focused);
      if not CheckPassword(Ph) then
        Exit;
      Dt.Name := Ph^.Name;
      Dt.Password := Ph^.Password;
      if Ph^.Memo1 <> nil then
        Dt.Memo1 := Ph^.Memo1^;
      if Ph^.Memo2 <> nil then
        Dt.Memo2 := Ph^.Memo2^;
      Idx := dlgEditDirectory;
      end
    else
      Idx := dlgAppendDirectory;

    if ExecResource(Idx, Dt) <> cmOK then
      Exit;

    C := List;
    if C = nil then
      C := New(PPhoneCollection, Init(10, 10));
    List := nil;
    R.A.X := Focused;
    if Append then
      C^.Insert(New(PPhoneDir, Init(Dt.Password, Dt.Name, Dt.Memo1,
             Dt.Memo2)))
    else
      begin
      Ph^.Name := Dt.Name;
      Ph^.Password := Dt.Password;
      DisposeStr(Ph^.Memo1);
      Ph^.Memo1 := NewStr(Dt.Memo1);
      DisposeStr(Ph^.Memo2);
      Ph^.Memo2 := NewStr(Dt.Memo2);
      C^.AtDelete(Focused);
      PSortedCollection(C)^.Search(Ph, Focused);
      C^.AtInsert(Focused, Ph);
      end;
    Owner^.Lock;
    NewLisT(C);
    FocusItem(R.A.X);
    Owner^.UnLock;
    Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
    Phones := C;
    if Phones^.Count > 16380 then
      Stream.Put(Phones)
    else
      PPhoneCollection(Phones)^.ShortStore(Stream);
    Stream.Done;
    end { EditDirectory };

  procedure EditNumber(Append: Boolean);
    var
      D: PDialog;
      R: TRect;
      P: PView;
      Idx: TDlgIdx;
    begin
    if Active = nil then
      begin
      EditDirectory(Append);
      Exit
      end;
    FillChar(DT, SizeOf(DT), 0);
    if not Append then
      begin
      Ph := List^.At(Focused);
      DT.Name := Ph^.Name;
      if Ph^.Number <> nil then
        DT.Number := Ph^.Number^;
      if Ph^.Memo1 <> nil then
        DT.Memo1 := Ph^.Memo1^;
      if Ph^.Memo2 <> nil then
        DT.Memo2 := Ph^.Memo2^;
      Idx := dlgEditNumber;
      end
    else
      Idx := dlgAppendNumber;

    repeat
      if ExecResource(Idx, DT) <> cmOK then
        Exit;
    until (DT.Number <> '');

    if  (DT.Number = '') or (DT.Name = '..') {or (DT.Name='')} then
      Exit;
    C := List;
    if C = nil then
      C := New(PPhoneCollection, Init(10, 10));
    List := nil;
    R.A.X := Focused;
    if not Append and (C^.Count > Focused) then
      C^.AtFree(Focused);
    C^.Insert(New(PPhone, Init(DT.Number, DT.Name, DT.Memo1, DT.Memo2)));
    Owner^.Lock;
    NewLisT(C);
    FocusItem(R.A.X);
    Owner^.UnLock;
    Active^.Phones := C;
    Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
    if Phones^.Count > 16380 then
      Stream.Put(Phones)
    else
      PPhoneCollection(Phones)^.ShortStore(Stream);
    Stream.Done;
    end { EditNumber };

  procedure CopyDirectory;
    var
      Dt: record
        Name: String[30];
        Password: String[15];
        Memo1, Memo2: String[50];
        end;
      Ph, P1: PPhoneDir;
      I: Integer;
    begin
    if  (List = nil) or (Focused >= List^.Count) then
      Exit;
    FillChar(Dt, SizeOf(Dt), 0);
    Ph := List^.At(Focused);
    if Ph = nil then
      Exit;
    if not CheckPassword(Ph) then
      Exit;
    Dt.Name := Ph^.Name;
    Dt.Password := Ph^.Password;
    if Ph^.Memo1 <> nil then
      Dt.Memo1 := Ph^.Memo1^;
    if Ph^.Memo2 <> nil then
      Dt.Memo2 := Ph^.Memo2^;

    C := List;
    if C = nil then
      C := New(PPhoneCollection, Init(10, 10));
    List := nil;
    I := Focused;

    P1 := New(PPhoneDir, Init(Dt.Password, Dt.Name, Dt.Memo1, Dt.Memo2));
    P1^.Encrypted := False;
    C^.Insert(P1);
    Owner^.Lock;
    NewLisT(C);
    FocusItem(I);
    Owner^.UnLock;
    Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
    Phones := C;
    if Phones^.Count > 16380 then
      Stream.Put(Phones)
    else
      PPhoneCollection(Phones)^.ShortStore(Stream);
    Stream.Done;
    end { CopyDirectory };

  procedure CopyPhones;
    var
      I: Integer;
    begin
    if  (List = nil) or (Focused >= List^.Count) then
      Exit;
    if Active = nil then
      begin
      CopyDirectory;
      Exit
      end;
    FillChar(DT, SizeOf(DT), 0);
    Ph := List^.At(Focused);
    if Ph = nil then
      Exit;
    DT.Name := Ph^.Name;
    if Ph^.Number <> nil then
      DT.Number := Ph^.Number^;
    if Ph^.Memo1 <> nil then
      DT.Memo1 := Ph^.Memo1^;
    if Ph^.Memo2 <> nil then
      DT.Memo2 := Ph^.Memo2^;

    C := List;
    if C = nil then
      C := New(PPhoneCollection, Init(10, 10));
    List := nil;
    I := Focused;
    C^.Insert(New(PPhone, Init(DT.Number, DT.Name, DT.Memo1, DT.Memo2)));
    Owner^.Lock;
    NewLisT(C);
    FocusItem(I);
    Owner^.UnLock;
    Active^.Phones := C;
    Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
    if Phones^.Count > 16380 then
      Stream.Put(Phones)
    else
      PPhoneCollection(Phones)^.ShortStore(Stream);
    Stream.Done;
    end { CopyPhones };

  procedure CE;
    begin
    ClearEvent(Event)
    end;

  procedure DialPhone;
    var
      P: PPhone;
      W: PWindow;
    begin
    CE;
    if Focused = 0 then
      Exit;
    if  (Active = nil) or (List = nil) or (List^.Count = 0) then
      Exit;
    P := List^.At(Focused);
    New(PPH, Init(P^.Number^, P^.Name, '', ''));
    Owner^.EndModal(cmDialPhone);
    end;

  procedure EnterDir;
    var
      I: Integer;
      S: String;
      PD: PPhoneDir;
      P: PPhone;
    begin
    if Active = nil then
      begin
      if List^.Count = 0 then
        Exit;

      PD := List^.At(Focused);
      if not CheckPassword(PD) then
        Exit;

      DisableCommands([cmDialPhone, cmImportPhones]);
      GroupLabel^.Hide;
      ItemLabel^.Show;
      EnterButton^.Hide;
      ReturnButton^.Show;

      (*     if{ SearchMode and} (List <> nil) then
       begin
        List^.DeleteAll;
        Dispose(List,Done);
        Lisr:=nil;
       end;
  *)
      Active := PD;
      List := nil;
      NewLisT(PD^.Phones);
      if  (List = nil) then
        begin
        C := New(PPhoneCollection, Init(10, 10));
        C^.Insert(New(PPhone, Init(' ', '..', GetString(dlPhonesUpDir),
               '')));
        Owner^.Lock;
        NewLisT(C);
        Owner^.UnLock;
        Active^.Phones := C;
        Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
        if Phones^.Count > 16380 then
          Stream.Put(Phones)
        else
          PPhoneCollection(Phones)^.ShortStore(Stream);
        Stream.Done;
        DrawView;
        Exit;
        end;
      PSortedCollection(List)^.Sort;
      if  (PPhone(List^.At(0))^.Name <> '..') and (List^.Count > 0) then
        begin
        P := New(PPhone, Init(' ', '..', GetString(dlPhonesUpDir), ''));
        List^.AtInsert(0, P);
        SetRange(List^.Count);
        DrawView;
        end;
      {HideCursor;}
      end
    else
      begin
      GroupLabel^.Show;
      ItemLabel^.Hide;
      EnterButton^.Show;
      ReturnButton^.Hide;
      if Focused > 0 then
        begin
        DialPhone;
        Exit;
        end;

      if SearchMode and (List <> nil) then
        begin
        List^.DeleteAll;
        Dispose(List, Done);
        List := nil;
        end;
      SearchMode := False;
      List := nil;
      NewLisT(Phones);
      FocusItem(Phones^.IndexOf(Active));
      Active := nil;
      {HideCursor;}
      end;
    Message(@self, evBroadcast, cmValid, nil);
    end { EnterDir };

  procedure SearchPhone;
    var
      PC: PPhoneCollection;
      S: String;

    procedure SearchDir(Ph: PPhoneDir);
      procedure DoPhone(P: PPhone);
        var
          I: Integer;
        begin
        if  (P <> nil)
                 and ((Pos(S, UpStrg(P^.Name)) <> 0) or (Pos(S,
                   UpStrg(P^.Number^)) <> 0))
        then
          PC^.Insert(P);
        end;
      begin
      if  (Ph <> nil) and (Ph^.Phones <> nil)
             and ((Ph^.Password = '') or (Ph^.Encrypted))
      then
        Ph^.Phones^.ForEach(@DoPhone);
      end;

    begin { SearchPhone }
    if  (Phones = nil) or (Phones^.Count = 0) then
      Exit;
    S := '';
    if InputBox(GetString(dlPB_SearchPhone),
         GetString(dlPB_S_earchString), S, 30, 0) <> cmOK
    then
      Exit;
    UpStr(S);
    New(PC, Init(10, 10));
    PC^.Duplicates := True;
    Phones^.ForEach(@SearchDir);
    if PC^.Count = 0 then
      begin
      MessageBox(GetString(dlPB_NoFind), nil, mfError+mfOKButton);
      Exit;
      end;
    if SearchMode then
      begin
      List^.DeleteAll;
      Dispose(List, Done);
      List := nil
      end;
    SearchMode := True;
    List := nil;
    if Active = nil then
      Active := Phones^.At(Focused);
    PC^.AtInsert(0, New(PPhone, Init(' ', '..', GetString(dlPhonesUpDir),
           '')));
    NewLisT(PC);
    HideCursor;
    EnterButton^.Hide;
    ReturnButton^.Show;
    GroupLabel^.Hide;
    ItemLabel^.Show;
    end { SearchPhone };

  procedure ImportPhones;
    var
      F: PTextReader;
      S: String;
      P: String;
      Ph: PPhone;
      C: Char;
      I, J, K, M: Integer;
      PV: PView;
      Fr: Boolean;
      Template: String;

    procedure MakePhone;
      var
        I, J, K: Integer;
      begin
      for I := 2 to Length(S) do
        if  (S[I] >= '0') and (S[I] <= '9') and (S[I-1] = ' ') then
          begin
          P := '';
          K := I;
          J := I;
          while (J <= Length(S))
               and (S[J] in ['0'..'9', 'W', 'w', '-', ',', '(', ')'])
          do
            begin
            P := P+S[J];
            Inc(J)
            end;
          if Length(P) < 3 then
            Continue;
          S := Copy(S, 1, K-1);
          DelLeft(S);
          DelRight(S);
          if S <> '' then
            begin
            New(Ph, Init(P, S, '', ''));
            if Active^.Phones = nil then
              Active^.Phones := New(PPhoneCollection, Init(10, 10));
            Active^.Phones^.Insert(Ph);
            Inc(M);
            end;
          Break;
          end;
      end { MakePhone };

    procedure MakeTPhone;
      begin
      MakePhone;
      end;

    begin { ImportPhones }
    M := 0;
    if  (Active = nil) or AlphaMode or SearchMode then
      Exit;
    S := GetFileNameDialog(x_x, GetString(dlPB_ImportPhones),
        GetString(dlFileName), fdOKButton+fdHelpButton, hsImportPhones);
    if S = '' then
      Exit;
    F := New(PTextReader, Init(S));
    if F = nil then
      begin
      MessageBox(GetString(dlFBBNoOpen)+S, nil, mfError+mfOKButton);
      Exit
      end;
    PV := WriteMsg(GetString(dlPB_Working));
    Fr := True;
    while not F^.Eof do
      begin
      UpdateWriteView(PV);
      S := F^.GetStr;
      if S <> '' then
        begin
        if Fr and (S[1] = '|') then
          begin
          Fr := False;
          Template := S;
          Continue;
          end;
        if Template = '' then
          MakePhone
        else
          MakeTPhone;
        end;
      end;
    if PV <> nil then
      PV^.Free;
    MessageBox(^C+ItoS(M)+GetString(dlPB_CnvReport), nil,
       mfInformation+mfOKButton);
    List := nil;
    NewLisT(Active^.Phones);
    Dispose(F, Done);
    Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
    if Phones^.Count > 16380 then
      Stream.Put(Phones)
    else
      PPhoneCollection(Phones)^.ShortStore(Stream);
    Stream.Done;
    end { ImportPhones };

  procedure CopyItem;
    var
      S: String[230];
    begin
    {  FillChar(S[1], 230, 0);
  S[0]:=#230;
  FillChar(S[1], 30, 0);
  Ph := List^.At(Focused);
  Ph^.Name;              30
  Ph^.Number^;           100
  Ph^.Memo1^;            50
  Ph^.Memo2^;            50
  PutInClip(Str);}
    end;

  var
    WasBroad: Boolean;

  begin { TPhoneBox.HandleEvent }
  WasBroad := Event.What = evBroadcast;
  case Event.What of
    evMouseDown:
      if Event.Double then
        begin
        if Event.Buttons and mbRightButton = 0 then
          Message(Owner, evKeyDown, kbEnter, nil)
        else
          Message(Owner, evKeyDown, kbSpace, nil);
        CE;
        end;
    evCommand:
      case Event.KeyCode of
        cmCopyPhone:
          begin
          CE;
          CopyPhones;
          CE;
          end;
        cmImportPhones:
          begin
          CE;
          ImportPhones;
          CE;
          end;
        cmDialPhone:
          begin
          CE;
          DialPhone
          end;
        cmSearchPhone:
          begin
          CE;
          SearchPhone;
          CE;
          end;
        cmOK, cmNo:
          begin
          if  (EnterButton^.State and sfVisible) = 0 then
            Focused := 0;
          EnterDir;
          CE
          end;
        cmInsertPhone:
          begin
          if not AlphaMode or SearchMode then
            EditNumber(True);
          CE
          end;
        cmEditPhone:
          begin
          if List^.Count > 0 then
            if not AlphaMode or SearchMode then
              if not ((Active <> nil) and (Focused = 0)) then
                EditNumber(False);
          CE
          end;
        cmPhoneBookMode:
          begin
          SetList(not AlphaMode);
          CE
          end;
        cmDeletePhone:
          begin
          CE;
          if  (List = nil) or (List^.Count = 0) then
            Exit;
          if AlphaMode or SearchMode then
            Exit;
          if  (Active <> nil) and (Focused = 0) then
            Exit;
          if Active = nil then
            begin
            if Msg(dlPhoneDirDelete, nil,
                 mf2YesButton+mfNoButton+mfConfirmation) <> cmYes
            then
              Exit;
            if not CheckPassword(List^.At(Focused)) then
              Exit;
            end
          else if (List^.Count > 0) and
              (MessageBox(GetString(dlPhoneDeleteQuery), nil,
                mfYesButton+mfNoButton+mfConfirmation) <> cmYes)
          then
            Exit;
          if  (List <> nil) and (Focused < List^.Count) then
            begin
            List^.AtFree(Focused);
            SetRange(List^.Count);
            DrawView;
            end;
          Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
          if Phones^.Count > 16380 then
            Stream.Put(Phones)
          else
            PPhoneCollection(Phones)^.ShortStore(Stream);
          Stream.Done;
          end;
      end {case};
    evKeyDown:
      if Event.CharCode = ' ' then
        DialPhone
      else if (Active <> nil) then
        case Event.KeyCode of
          kbCtrlIns:
            CopyItem;
          kbCtrlPgUp, kbCtrlSlash:
            begin
            Focused := 0;
            EnterDir;
            CE;
            Exit;
            end;
          kbEnter:
            begin
            EnterDir;
            CE
            end;
        end {case};
  end {case};
  inherited HandleEvent(Event);
  if WasBroad then
    begin
    if  (Info <> nil) and (List <> nil) and (List^.Count > 0) then
      begin
      Ph := List^.At(Focused);
      if Ph^.Memo1 <> nil then
        Info^.S1 := Ph^.Memo1^
      else
        Info^.S1 := '';
      if Ph^.Memo2 <> nil then
        Info^.S2 := Ph^.Memo2^
      else
        Info^.S2 := '';
      Info^.DrawView;
      end
    else if Info <> nil then
      begin
      Info^.S1 := '';
      Info^.S2 := '';
      Info^.DrawView;
      end;
    end;
  if Active = nil then
    begin
    DisableCommands([cmDialPhone, cmImportPhones]);
    EnableCommands([cmPhoneBookMode, cmCopyPhone]);
    Owner^.Redraw
    end
  else
    begin
    DisableCommands([cmPhoneBookMode]);
    EnableCommands([cmImportPhones]);
    if Focused = 0 then
      DisableCommands([cmDialPhone, cmCopyPhone])
    else
      EnableCommands([cmDialPhone, cmCopyPhone]);
    Owner^.Redraw
    end;

  if SearchMode then
    DisableCommands([cmInsertPhone, cmDeletePhone, cmEditPhone,
       cmImportPhones])
  else
    EnableCommands([cmInsertPhone, cmDeletePhone, cmEditPhone]);

  end { TPhoneBox.HandleEvent };

destructor TPhoneBox.Done;
  begin
  { if (List <> nil) and (List <> Phones) }
  {                  then List^.DeleteAll;}
  if Phones <> nil then
    Dispose(Phones, Done);
  Phones := nil;
  { if Active <> nil then Dispose(Active,Done); Active:=nil;}
  if Info <> nil then
    Dispose(Info, Done);
  Info := nil;

  inherited Done;
  end;

function TPhoneBox.GetText;
  var
    S: String;
    P: PPhone;
  begin
  P := List^.At(Item);
  S := AddSpace(P^.Name, 30);
  if TypeOf(P^) = TypeOf(TPhone) then
    S := S+'  '+AddSpace(P^.Number^, 23);
  GetText := S;
  end;

constructor TPhoneCollection.Init;
  begin
  inherited Init(ALimit, ADelta);
  Duplicates := True;
  end;

constructor TPhoneCollection.Load;
  begin
  if not inherited Load(S) then
    Fail;
  Duplicates := True;
  Sort;
  end;

const
  VObjType: AWord = otPhone;
  VObjType2: AWord = otPhoneDir;
  VObjType3: AWord = otPhoneCollection;
procedure TPhoneCollection.ShortStore(var S: TStream);
  var
    TCount, TLimit, TDelta: AInt;
  procedure DoPutItem(P: Pointer);
    begin
    if TypeOf((PObject(P)^)) = TypeOf(TPhone) then
      begin
      S.Write(VObjType, 2);
      PPhone(P)^.Store(S);
      end
    else if TypeOf((PObject(P)^)) = TypeOf(TPhoneDir) then
      begin
      S.Write(VObjType2, 2);
      PPhoneDir(P)^.Store(S);
      end;
    end;
  begin
  TCount := Count;
  TLimit := Limit;
  TDelta := Delta;
  S.Write(VObjType3, 2);
  S.Write(Count, 2);
  S.Write(Limit, 2);
  S.Write(Delta, 2);
  ForEach(@DoPutItem);
  S.Write(Duplicates, SizeOf(Duplicates));
  end { TPhoneCollection.ShortStore };

constructor TPhoneCollection.ShortLoad(var S: TStream);
  var
    q: AWord;
    C, I: Integer;
    ACount, ALimit, ADelta: AInt;
  begin
  S.Read(q, 2);
  if q <> otPhoneCollection then
    Fail;
  S.Read(ACount, SizeOf(AInt));
  S.Read(ALimit, SizeOf(AInt));
  S.Read(ADelta, SizeOf(AInt));
  inherited Init(ALimit, ADelta);
  SetLimit(ACount);
  for I := 0 to ACount-1 do
    begin
    S.Read(q, 2);
    if q = otPhone then
      AtInsert(I, New(PPhone, Load(S)))
    else if q = otPhoneDir then
      AtInsert(I, New(PPhoneDir, Load(S)))
    else
      Break;
    end;
  S.Read(Duplicates, SizeOf(Boolean));
  Sort;
  end { TPhoneCollection.ShortLoad };

function TPhoneCollection.Compare;
  begin
  if PPhone(P1)^.Name = PPhone(P2)^.Name then
    Compare := 0
  else if (PPhone(P1)^.Name = '..')
       or (PPhone(P1)^.Name < PPhone(P2)^.Name)
  then
    Compare := -1
  else
    Compare := 1;
  end;

procedure TPhoneCollection.FreeItem(Item: Pointer);
  var
    PP: PPhone;
  begin
  PP := Item;
  if Item <> nil then
    Dispose(PP, Done);
  end;

constructor TPhone.Init;
  begin
  inherited Init;
  Name := AName;
  Number := NewStr(ANumber);
  Memo1 := NewStr(AMemo1);
  Memo2 := NewStr(AMemo2);
  end;

constructor TPhone.Load;
  begin
  Number := S.ReadStr;

  // fixme: commented by unxed
  //S.ReadStrV(Name);

  {S.Read(Name[0],1); S.Read(Name[1], Byte(Name[0]));}
  Memo1 := S.ReadStr;
  Memo2 := S.ReadStr;
  end;

procedure TPhone.Store;
  begin
  S.WriteStr(Number);
  S.WriteStr(@Name);
  S.WriteStr(Memo1);
  S.WriteStr(Memo2);
  end;

destructor TPhone.Done;
  begin
  DisposeStr(Number);
  DisposeStr(Memo1);
  DisposeStr(Memo2);
  inherited Done;
  end;

constructor TPhoneDir.Init;
  begin
  inherited Init;
  Name := AName;
  Memo1 := NewStr(AMemo1);
  Memo2 := NewStr(AMemo2);
  Password := APassword;
  Phones := nil;
  end;

procedure CryptCol(Col: PCollection; Pass: String);
  procedure CryptPhone(P: PPhone);
    begin
    if P <> nil then
      with P^ do
        begin
        Name := CryptWith(Pass, Name);
        if Number <> nil then
          Number^:= CryptWith(Pass, Number^);
        if Memo1 <> nil then
          Memo1^:= CryptWith(Pass, Memo1^);
        if Memo2 <> nil then
          Memo2^:= CryptWith(Pass, Memo2^);
        end;
    end;
  begin
  if Col <> nil then
    Col^.ForEach(@CryptPhone);
  end;

constructor TPhoneDir.Load;
  var
    Q: TFileSize;
  begin

  // fixme: commented by unxed
  //S.ReadStrV(Name);

  {S.Read(Name[0],1); S.Read(Name[1], Byte(Name[0]));}
  Memo1 := S.ReadStr;
  Memo2 := S.ReadStr;
  S.Read(Password, 1);
  S.Read(Password[1], Byte(Password[0]));
  Password := CryptWith('NaViGaToR', Password);
  Q := S.GetPos;
  Phones := nil;
  Phones := PCollection(S.Get);
  if Phones = nil then
    begin
    S.Status := stOK;
    S.Seek(Q);
    Phones := PCollection(New(PPhoneCollection, ShortLoad(S)));
    end;
  if Phones <> nil then
    CryptCol(Phones, Password);
  Encrypted := False;
  end { TPhoneDir.Load };

procedure TPhoneDir.Store;
  begin
  S.WriteStr(@Name);
  S.WriteStr(Memo1);
  S.WriteStr(Memo2);
  Password := CryptWith('NaViGaToR', Password);
  S.Write(Password, 1);
  S.Write(Password[1], Byte(Password[0]));
  Password := CryptWith('NaViGaToR', Password);
  if Phones <> nil then
    CryptCol(Phones, Password);
  if Phones <> nil then
    if Phones^.Count > 16380 then
      S.Put(Phones)
    else
      PPhoneCollection(Phones)^.ShortStore(S);
  if Phones <> nil then
    CryptCol(Phones, Password);
  end;

destructor TPhoneDir.Done;
  begin
  DisposeStr(Memo1);
  DisposeStr(Memo2);
  if Phones <> nil then
    Dispose(Phones, Done);
  Phones := nil;
  inherited Done;
  end;

end.
