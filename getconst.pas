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
GetConst;

interface

uses
  Objects, Objects2
  ;

type
  PValuesHolder = ^TValuesHolder;
  PTypeHolder = ^TTypeHolder;
  PLngWord = ^TLngWord;
  TTypeMode = (tmConst, tmEnum);

  { Collection of PTypeHolders }
  TValuesHolder = object(TCollection)
    function GetType(ID: String): PTypeHolder;
    function GetValue(S: String; var Complete: Boolean): Word;
    procedure Show;
    end;

  { Collection of PLngWords }
  TTypeHolder = object(TSortedCollection)
    TypeID: String[10];
    Mode: TTypeMode;
    constructor Init(ID: String; AMode: TTypeMode);
    function Compare(P1, P2: Pointer): Integer; virtual;
    procedure Show;
    end;

  TLngWord = object(TObject)
    Name: String[30];
    l: Word;
    Mark: Byte;
    constructor Init(AL: Word; const AName: String);
    end;

procedure ProcessFile(const FileName: String; Types: PValuesHolder);

implementation

uses
  Dos, lfnvp,
  Advance, Advance1, Advance2, Commands
  ;

procedure Error(const S: String);
  begin
  Writeln(S);
  Halt(2);
  end;

function TValuesHolder.GetType;
  function LookForType(P: PTypeHolder): Boolean;
    begin
    LookForType := P^.TypeID = Id;
    end;
  begin
  GetType := FirstThat(@LookForType);
  end;

function TValuesHolder.GetValue;
  function DoScanValue(P: PTypeHolder): Boolean;
    function DoScan(P: PLngWord): Boolean;
      begin
      if P^.Name = S then
        begin
        GetValue := P^.l;
        DoScan := True;
        end
      else
        DoScan := False
      end;
    begin
    DoScanValue := P^.FirstThat(@DoScan) <> nil;
    end;
  begin
  UpStr(S);
  Complete := FirstThat(@DoScanValue) <> nil;
  end;

procedure TTypeHolder.Show;
  procedure DoScan(P: PLngWord);
    begin
    Writeln(P^.Name, '=', P^.l);
    end;
  begin
  ForEach(@DoScan);
  end;

function TTypeHolder.Compare;
  begin
  if PLngWord(P1)^.Name > PLngWord(P2)^.Name then
    Compare := 1
  else if PLngWord(P1)^.Name < PLngWord(P2)^.Name then
    Compare := -1
  else
    Compare := 0;
  end;


procedure TValuesHolder.Show;
  procedure DoScan(P: PTypeHolder);
    begin
    Writeln('TYPE ', P^.TypeID);
    Writeln(Strg(#196, 5+Length(P^.TypeID)));
    P^.Show;
    end;
  begin
  ForEach(@DoScan);
  end;

constructor TTypeHolder.Init;
  begin
  inherited Init(50, 50);
  TypeID := Copy(ID, 1, 10);
  Mode := AMode;
  end;

constructor TLngWord.Init;
  begin
  Name := UpStrg(Copy(AName, 1, 30));
  l := Word(AL);
  Mark := 0;
  end;

const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  Digits = ['0'..'9'];
  Hexs = ['0'..'9', 'A'..'F', 'a'..'f'];
  Blank = [#0..#32];

  {-DataCompBoy-}
procedure ProcessFile;
  type
    TParseMode = (pmSkip, pmComment, pmConst, pmStartType, pmSeekType,
       pmType);
  var
    F: lText;
    US, S, ID: String;
    Eq: Integer;
    P: PTypeHolder;
    CurrentType: PTypeHolder;
    TypeCount: Word;
    CmtMode, Mode: TParseMode;
    CmtB, CmtE: Integer;
    CmtS: String[2];

  function CalcValue(var S: String): Word;
    var
      W: Word;
      V: String;
      ParseMode: (pmNone, pmAlpha, pmWord, pmHex);
      I: Integer;
      Complete: Boolean;
    label Loop;
    begin
    I := 0;
    W := 0;
    ParseMode := pmNone;
    while I < Length(S) do
      begin
      Inc(I);
Loop:
      case ParseMode of
        pmNone:
          if S[I] in Blank then
            Continue
          else if S[I] in Digits then
            begin
            ParseMode := pmWord;
            W := 0;
            goto Loop
            end
          else if S[I] = '$' then
            begin
            ParseMode := pmHex;
            W := 0;
            end
          else if S[I] in Alpha then
            begin
            ParseMode := pmAlpha;
            V := S[I];
            end
          else if S[I] = '+' then
            begin
            S := Copy(S, I+1, MaxStringLength);
            W := W+CalcValue(S);
            Break;
            end
          else if S[I] = '-' then
            begin
            S := Copy(S, I+1, MaxStringLength);
            W := W-CalcValue(S);
            Break;
            end;
        pmWord:
          if S[I] in Digits
          then
            W := W*10+Ord(S[I])-Ord('0')
          else
            Break;
        pmHex:
          if S[I] in Hexs
          then
            W := W*16+Ord(UpCase(S[I]))-Ord('0')-7*Byte(S[I] > '9')
          else
            Break;
        pmAlpha:
          if S[I] in (Alpha+Digits)
          then
            V := V+S[I]
          else
            begin
            W := Types^.GetValue(V, Complete);
            if not Complete then
              begin
              Writeln('Undefined variable ', V);
              end
            else
              Break;
            end;
      end { case };
      end { while };
    S := Copy(S, I, MaxStringLength);
    CalcValue := W;
    end { CalcValue };

  function TruncSpace(S: String): String;
    var
      I: Byte;
    begin
    I := Length(S);
    while S[I] = ' ' do
      Dec(I);
    SetLength(S, I);
    TruncSpace := S
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

  function Ident(N: Integer): String;
    var
      R: String;
      I: Integer;
    begin
    I := N;
    if not (S[I] in Alpha) then
      begin
      Ident := '';
      Exit;
      end;
    while (I <= Length(S)) and (S[I] in (Alpha+Digits)) do
      Inc(I);
    Dec(I);
    Ident := Copy(S, N, I-N+1);
    S := RemoveLeadSpaces(Copy(S, I+1, MaxStringLength));
    US := UpStrg(S);
    end;

  function LookForType(P: PTypeHolder): Boolean;
    begin
    LookForType := (Mode = pmConst) and (P^.Mode = tmConst) and
        (Pos(Copy(Id, 1, 2)+#0, P^.TypeID) > 0)
      or
        (Mode = pmStartType) and (P^.Mode = tmEnum) and
        (UpStrg(Id) = UpStrg(P^.TypeID));
    end;
  {DataCompBoy
  function DelDblSpc( S: String ): String;
  begin
    DelDoubles( '  ', S );
    DelDblSpc := S;
  end;
}
  label
    Reparse, StartType;
  begin { ProcessFile }
  Mode := pmSkip;
  lAssignText(F, FileName);
  lResetText(F);
  while not Eof(F.T) do
    begin
    Readln(F.T, S);
Reparse:
    S := RemoveLeadSpaces(TruncSpace(S));
    DelDoubles('  ', S);
    if S = '' then
      Continue;
    US := UpStrg(S);

    CmtB := Pos('{', S);
    if CmtB > 0 then
      begin
      CmtS := '}';
      CmtE := Pos('}', S);
      if CmtE > 0 then
        begin
        Delete(S, CmtB, CmtE-CmtB+1);
        goto Reparse;
        end
      else
        begin
        Delete(S, CmtB, 1);
        CmtMode := Mode;
        Mode := pmComment;
        goto Reparse;
        end;
      end;
    CmtB := Pos('(*', S);
    if CmtB > 0 then
      begin
      CmtS := '*)';
      CmtE := Pos('*)', S);
      if CmtE > 0 then
        begin
        Delete(S, CmtB, CmtE-CmtB+2);
        goto Reparse;
        end
      else
        begin
        Delete(S, CmtB, 2);
        CmtMode := Mode;
        Mode := pmComment;
        goto Reparse;
        end;
      end;

    case Mode of
      pmComment:
        begin
        CmtE := Pos(CmtS, S);
        if CmtE > 0 then
          begin
          Delete(S, 1, CmtE+Length(CmtS));
          Mode := CmtMode;
          goto Reparse;
          end;
        end;
      pmSkip:
        begin
        if Pos('CONST', US) = 1 then
          begin
          Mode := pmConst;
          Delete(S, 1, 5);
          goto Reparse;
          end
        else if Pos('TYPE', US) = 1
        then
          goto StartType;
        end;
      pmConst:
        if Pos('TYPE', US) = 1 then
StartType:
            begin
            Mode := pmStartType;
            Delete(S, 1, 4);
            goto Reparse;
            end
        else
          begin
          if Pos('CONST', US) = 1 then
            begin
            Delete(S, 1, 5);
            goto Reparse;
            end;
          Eq := Pos('=', S);
          if Eq = 0 then
            begin
            Mode := pmSkip;
            Continue;
            end;
          ID := Ident(1);
          P := Types^.FirstThat(@LookForType);
          if P = nil then
            Continue;
          Eq := Pos('=', S);
          S := RemoveLeadSpaces(Copy(S, Eq+1, MaxStringLength));
          US := RemoveLeadSpaces(Copy(US, Eq+1, MaxStringLength));
          P^.Insert(New(PLngWord, Init(CalcValue(US), ID)));
          US := RemoveLeadSpaces(US);
          if US[1] = ';' then
            US := Copy(US, 2, MaxStringLength);
          Delete(S, 1, Length(S)-Length(US));
          goto Reparse;
          end;
      pmStartType:
        begin
        ID := Ident(1);
        CurrentType := Types^.FirstThat(@LookForType);
        if CurrentType = nil then
          begin
          if Pos(')', S) > 0 then
            while not Eof(F.T) do
              if Pos(')', S) > 0
              then
                Break
              else
                Readln(F.T, S);
          Mode := pmSeekType;
          end
        else
          begin
          TypeCount := 0;
          Mode := pmType;
          Delete(S, 1, Length(ID));
          goto Reparse;
          end
        end;
      pmSeekType:
        if Pos('CONST', US) = 1 then
          begin
          Mode := pmSkip;
          goto Reparse;
          end
        else if Pos('TYPE', US) = 1 then
          begin
          Mode := pmSkip;
          goto Reparse;
          end
        else
          begin
          Mode := pmStartType;
          goto Reparse;
          end;
      pmType:
        begin
        ID := Ident(1);
        if ID <> '' then
          begin
          CurrentType^.Insert(New(PLngWord, Init(TypeCount, ID)));
          Inc(TypeCount);
          if S[1] = ',' then
            begin
            Delete(S, 1, 1);
            goto Reparse;
            end;
          end;
        if Pos(')', S) > 0
        then
          Mode := pmSeekType
        else
          goto Reparse;
        end
    end {case};
    end;
  Close(F.T);
  end { ProcessFile };
{-DataCompBoy-}

end.
