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

unit
IniFiles;

interface

uses
  Defines, Streams, Objects
  ;

type
  { Collection of strings like "video=vesa.bgi"  }
  { Note, "=" must be directly between key and   }
  {                   value, no blanks allowed   }
  { Empty key means insertion of ordinary string }
  { It may be used to implement indexed logfiles }
  { Of course, these lines must not contain      }
  { strings with "=" or starting with "[".       }
  PIniSection = ^TIniSection;
  TIniSection = object(TCollection)
    TheName: PString;
    constructor Init(const AName: String);
    destructor Done; virtual;
    function GetIndexOf(Key: String): Integer;
    function Get(const Key: String): String;
    procedure Put(const Key, Value: String; var Modified: Boolean);
    function GetKeyAt(const Index: Integer): String;
    function GetValueAt(const Index: Integer): String;
    function Name: String;
    procedure FreeItem(Item: Pointer); virtual;
    procedure Insert(Item: Pointer); virtual;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); virtual;
    end;

  { Collection of TIniSections }
  PIniFile = ^TIniFile;
  TIniFile = object(TCollection)
    Modified: Boolean;
    Name: PString;
    constructor Init(FileName: String; var AStatus: Integer);
    destructor Done; virtual;
    function Get(const Section, Key: String): String;
    procedure Put(const Section, Key, Value: String);
    function GetSection(Section: String): PIniSection;
    end;

implementation

uses
  Advance, Advance1, Advance2, Commands {Cat}
  ;

const
  CrLf: array[0..1] of Char = (#13, #10);

function PStr2Str(P: PString): String;
  begin
  if P <> nil
  then
    PStr2Str := P^
  else
    PStr2Str := ''
  end;

function ReadString(var Stream: TStream): String;
  var
    S: String;
    C: Char;
  label
    Loop, Failure;
  begin
  S := '';
Loop:
  Stream.Read(C, 1);
  if  (C <> #13) and (Stream.Status = stOK) then
    begin
    S := S+C;
    goto Loop;
    end;
  ReadString := S;
  if Stream.Status <> stOK then
Failure:
    begin
    Stream.Reset;
    Exit;
    end;
  Stream.Read(C, 1);
  if  (C <> #10) or (Stream.Status <> stOK) then
    begin
    Stream.Reset;
    Stream.Seek(Stream.GetPos-1);
    end;
  end { ReadString };

{                                INI Section                                 }
{----------------------------------------------------------------------------}
constructor TIniSection.Init;
  begin
  inherited Init(5, 5);
  TheName := NewStr(AName);
  end;

function RemoveLeadSpaces(const S: String): String;
  var
    I: Integer;
  begin
  I := 1;
  while (I <= Length(S)) and (S[I] = ' ') do
    Inc(I);
  RemoveLeadSpaces := Copy(S, I, MaxStringLength);
  end;

function TruncSpace(S: String): String;
  var
    I: Byte;
  begin
  I := Length(S);
  while (I > 0) and (S[I] = ' ') do
    Dec(I);
  SetLength(S, I);
  TruncSpace := S
  end;

constructor TIniSection.Load;
  var
    Base, L: TFileSize;
    Str: String;
  function Simplify(S: String): String;
    var
      I: Integer;
    begin
    I := Pos('=', S);
    if I <> 0 then
      begin
      Simplify := RemoveLeadSpaces(TruncSpace(Copy(S, 1, I-1)))
        +'='+RemoveLeadSpaces(TruncSpace(Copy(S, I+1, MaxStringLength)));
      end
    else
      Simplify := RemoveLeadSpaces(TruncSpace(S));
    end;
  label
    Loop, Skip;
  begin
  TCollection.Init(5, 5);
  Base := S.GetPos;
  Str := ReadString(S);
  if  (Str <> '') and (Str[1] = '[') and (Str[Length(Str)] = ']') then
    begin
    TheName := NewStr(Copy(Str, 2, Length(Str)-2));
    end
  else
    begin
    TheName := nil;
    S.Seek(Base);
    end;

Loop:
  Str := Simplify(ReadString(S));
  if Str <> '' then
    begin
    Insert(NewStr(Str));
    goto Loop;
    end;

Skip:
  L := S.GetPos;
  Str := ReadString(S);
  if Str <> ''
  then
    S.Seek(L)
  else if S.GetPos <> L then
    goto Skip;

  if S.GetPos = Base then
    begin
    inherited Done;
    Fail
    end;
  end { TIniSection.Load };

procedure TIniSection.FreeItem;
  begin
  DisposeStr(PString(Item));
  end;

procedure TIniSection.Insert;
  begin
  if Item <> nil then
    inherited Insert(Item);
  end;

procedure TIniSection.Store;
  var
    Str: String;
  procedure DoPutItem(P: PString);
    begin
    Str := PStr2Str(P)+#13#10;
    S.Write(Str[1], Length(Str));
    end;
  begin
  Str := PStr2Str(TheName);
  if Str <> '' then
    begin
    Str := '['+Str+']'+#13#10;
    S.Write(Str[1], Length(Str));
    end;
  ForEach(@DoPutItem);
  S.Write(CrLf, SizeOf(CrLf));
  end;

destructor TIniSection.Done;
  begin
  inherited Done;
  DisposeStr(TheName);
  end;

function TIniSection.Name;
  begin
  Name := PStr2Str(TheName)
  end;

function TIniSection.GetIndexOf;
  var
    P: PString;
    S: String;
    I: Integer;
  function Search(P: PString): Boolean;
    var
      S: String;
      I: Integer;
    begin
    S := UpStrg(PStr2Str(P));
    I := Pos(Key, S);
    Search := (I = 1) and (S[Length(Key)+1] = '=');
    end;
  begin
  UpStr(Key);
  GetIndexOf := IndexOf(FirstThat(@Search));
  end;

function TIniSection.Get;
  var
    I: Integer;
    S: String;
  begin
  I := GetIndexOf(Key);
  if I >= 0 then
    begin
    S := PStr2Str(At(I));
    I := Pos('=', S);
    if I > 0 then
      begin
      Get := Copy(S, I+1, MaxStringLength);
      Exit
      end;
    end;
  Get := ''
  end;

procedure TIniSection.Put;
  var
    I: Integer;
    S: String;
    NewValue: String;
  begin
  if Key = '' then
    begin
    Insert(NewStr(Value));
    Exit;
    end;
  I := GetIndexOf(Key);
  NewValue := Key+'='+Value;
  if I >= 0 then
    begin
    S := PStr2Str(At(I));
    if S <> NewValue then
      begin
      AtReplace(I, NewStr(NewValue));
      Modified := True;
      end;
    end
  else
    begin
    Insert(NewStr(NewValue));
    Modified := True;
    end;
  end { TIniSection.Put };

function TIniSection.GetKeyAt;
  var
    S: String;
    I: Integer;
  begin
  S := PStr2Str(At(Index));
  I := PosChar('=', S);
  if I = 0
  then
    GetKeyAt := ''
  else
    GetKeyAt := TruncSpace(Copy(S, 1, I-1));
  end;

function TIniSection.GetValueAt;
  var
    S: String;
    I: Integer;
  begin
  S := PStr2Str(At(Index));
  I := PosChar('=', S);
  if I = 0
  then
    GetValueAt := ''
  else
    GetValueAt := RemoveLeadSpaces(Copy(S, I+1, MaxStringLength));
  end;

{                                  INI File                                  }
{----------------------------------------------------------------------------}
constructor TIniFile.Init;
  var
    T: TBufStream;
    P: PIniSection;
  begin
  inherited Init(5, 5);
  Name := NewStr(FileName);
  T.Init(FileName, stOpenRead, 512);
  if T.Status = stOK then
    repeat
      P := New(PIniSection, Load(T));
      if P <> nil
      then
        Insert(P);
    until P = nil;
  AStatus := T.Status;
  T.Done;
  end;

destructor TIniFile.Done;
  var
    T: TBufStream;
  procedure DoPutItem(P: PIniSection);
    begin
    if P <> nil then
      P^.Store(T);
    end;
  begin
  if Modified then
    begin
    T.Init(PStr2Str(Name), stCreate, 512);
    ForEach(@DoPutItem);
    T.Done;
    end;
  DisposeStr(Name);
  inherited Done;
  end;

function TIniFile.GetSection;
  function Search(P: PIniSection): Boolean;
    begin
    Search := UpStrg(P^.Name) = Section
    end;
  begin
  UpStr(Section);
  GetSection := FirstThat(@Search)
  end;

function TIniFile.Get;
  var
    P: PIniSection;
  begin
  P := GetSection(Section);
  if P <> nil
  then
    Get := P^.Get(Key)
  else
    Get := ''
  end;

procedure TIniFile.Put;
  var
    P: PIniSection;
  begin
  P := GetSection(Section);
  if P = nil then
    begin
    New(P, Init(Key));
    Insert(P);
    Modified := True;
    end;
  P^.Put(Key, Value, Modified);
  end;

end.
