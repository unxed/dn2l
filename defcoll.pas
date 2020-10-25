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
unit DefColl;

interface

uses
  Collect
  ;

type
  PDefCollection = ^TDefCollection;
  TDefCollection = object(TStringCollection)
    constructor Init(ALimit, ADelta: LongInt); {Initialization }
    procedure ProceedFile(FName: String); {Read defines   }
    {from pascal-   }
    {style defines  }
    {file           }
    function IsDef(Name: String): Boolean; {Is Name defined}
    procedure Def(Name: String); {Define Name    }
    procedure Undef(Name: String); {Undefine Name  }
    function ProceedStr(Str: String): String; {Proceed pascal-}
    {style string   }
    (*{$.....}' and *)
    {(*$.....*)     }
    function ProceedStr2(Str: String): String; {Proceed RCP-   }
    {style string   }
    {;$.....        }
  private
    InComm: Byte;
    DefStack: array[0..20] of Boolean;
    SP: Byte;
    procedure Push(const ADef: String; IfDef: Boolean);
    procedure Pop(EndIf: Boolean);
    function TextOk: Boolean;
    end;

implementation
uses
  Lfnvp, Advance1, Advance
  ;

constructor TDefCollection.Init(ALimit, ADelta: LongInt);
  var
    i: LongInt;
  begin
  inherited Init(ALimit, ADelta, False);
  InComm := 0;
  SP := 0;
  DefStack[SP] := True;
  Def('VER20');
  Def('VIRTUALPASCAL');

  {AK155  символы можно задавать в комстроке в параметрах, начиная
со второго. При этом второй параметр - целевая платформа.}
  if ParamStr(2) = '' then
    begin
    {$IFDEF OS2}Def('OS2'); {$ENDIF}
    {$IFDEF WIN32}Def('WIN32'); {$ENDIF}
    {$IFDEF DPMI32}Def('DPMI32'); {$ENDIF}
    end
  else
    begin
    for i := 2 to ParamCount do
      Def(ParamStr(i));
    end;
  end { TDefCollection.Init };

procedure TDefCollection.ProceedFile;
  var
    F: lText;
    S: String;
    Q: String;
    I, j: Integer;
    B: Boolean;
  begin
  lAssignText(F, FName);
  lResetText(F);
  if IOResult <> 0 then
    Exit;
  while not Eof(F.T) do
    begin
    Readln(F.T, S);
    S := ProceedStr(S);
    if S = '' then
      Continue;
    repeat
      I := Pos('{$DEFINE', S);
      if I = 0 then
        I := Pos('(*$DEFINE', S);
      if I > 0 then
        begin
        B := (S[I] = '(');
        System.Delete(S, I, 9+Byte(B));
        DelLeft(S);
        for I := 1 to Length(S) do
          if S[I] in BreakChars then
            Break;
        if  (S[I] in BreakChars) then
          Dec(I);
        Q := Copy(S, 1, I);
        Def(Q);
        System.Delete(S, 1, I);
        I := 0;
        repeat
          Inc(I)
        until (I >= Length(S)) or
          (B and (S[I] = '*') and (I < Length(S)) and (S[I+1] = ')')) or
          (not B and (S[I] = '}')) or
          (I = Length(S));
        System.Delete(S, 1, I);
        end;

      I := Pos('{$UNDEF', S);
      if I = 0 then
        I := Pos('(*$UNDEF', S);
      if I > 0 then
        begin
        B := (S[I] = '(');
        System.Delete(S, I, 8+Byte(B));
        DelLeft(S);
        for I := 1 to Length(S) do
          if S[I] in BreakChars then
            Break;
        if  (S[I] in BreakChars) then
          Dec(I);
        Q := Copy(S, 1, I);
        Undef(Q);
        System.Delete(S, 1, I);
        I := 0;
        repeat
          Inc(I)
        until (I >= Length(S)) or
          (B and (S[I] = '*') and (I < Length(S)) and (S[I+1] = ')')) or
          (not B and (S[I] = '}')) or
          (I = Length(S));
        System.Delete(S, 1, I);
        end;
    until I = 0;
    end;
  Close(F.T);
  end { TDefCollection.ProceedFile };

function TDefCollection.IsDef(Name: String): Boolean;
  var
    I: LongInt;
  begin
  UpStr(Name);
  IsDef := Search(@Name, I);
  end;

procedure TDefCollection.Def(Name: String);
  var
    I: LongInt;
  begin
  UpStr(Name);
  if not Search(@Name, I) then
    AtInsert(I, NewStr(Name));
  end;

procedure TDefCollection.Undef(Name: String);
  var
    I: LongInt;
  begin
  UpStr(Name);
  if Search(@Name, I) then
    AtFree(I);
  end;

function TDefCollection.ProceedStr(Str: String): String;
  var
    i: Integer;
    j: Integer;
    k: Integer;
    b: Boolean;
    S: String;
    Q: String;
    Z: String;
  begin
  S := '';
  i := 0;
  while i < Length(Str) do
    begin
    Inc(i);
    b := False;
    if InComm = 0 then
      begin
      if  (Str[i] = '{') and ((i = Length(Str)) or (Str[i+1] <> '$'))
      then
        InComm := 1
      else if ((Str[i] = '(') and (i < Length(Str)) and (Str[i+1] <>
           '*')) and
          ( (i = Length(Str)-1) or (Str[i+2] <> '$'))
      then
        InComm := 2
      else if Str[i] = '{' then
        begin
        k := i;
        Inc(i);
        Inc(i);
        j := i+1;
        while not ((j = Length(Str)) or (Str[j] in BreakChars)) do
          Inc(j);
        if not (Str[j] in BreakChars) then
          Inc(j);
        Q := UpStrg(Copy(Str, i, j-i));
        i := j;
        if  (Q = 'IFDEF') or (Q = 'IFNDEF') then
          begin
          while (Str[i] in BreakChars) do
            Inc(i);
          j := i;
          while not (Str[j] in BreakChars) do
            Inc(j);
          Z := Copy(Str, i, j-i);
          i := j;
          Push(Z, Q = 'IFDEF');
          end
        else if Q = 'ELSE' then
          Pop(False)
        else if Q = 'ENDIF' then
          Pop(True)
        else
          b := True;
        while (i <= Length(Str)) and (Str[i] <> '}') do
          Inc(i);
        if b and TextOk then
          S := S+Copy(Str, k, i-k+1);
        if Str[i] <> '}' then
          InComm := 1;
        end
      else if (i < Length(Str)) and (Str[i] = '(') and (Str[i+1] = '*')
      then
        begin
        k := i;
        Inc(i);
        Inc(i);
        Inc(i);
        j := i+1;
        while not ((j = Length(Str)) or (Str[j] in BreakChars)) do
          Inc(j);
        if not (Str[j] in BreakChars) then
          Inc(j);
        Q := UpStrg(Copy(Str, i, j-i));
        i := j;
        if  (Q = 'IFDEF') or (Q = 'IFNDEF') then
          begin
          while (Str[i] in BreakChars) do
            Inc(i);
          j := i;
          while not (Str[j] in BreakChars) do
            Inc(j);
          Z := Copy(Str, i, j-i);
          i := j;
          Push(Z, Q = 'IFDEF');
          end
        else if Q = 'ELSE' then
          Pop(False)
        else if Q = 'ENDIF' then
          Pop(True)
        else
          b := True;
        while (i < Length(Str)) and (Str[i] <> '*') and (Str[i+1] <> ')')
        do
          Inc(i);
        if b and TextOk then
          S := S+Copy(Str, k, i-k+1);
        if  (i = Length(Str)) or ((Str[i] <> '*') and (Str[i+1] <> ')'))
        then
          InComm := 2;
        end
      else if TextOk then
        S := S+Str[i];
      end
    else if InComm = 1 then
      if Str[i] = '}' then
        InComm := 0
      else
    else if InComm = 2 then
      if  (i < Length(Str)) and (Str[i] = '*') and (Str[i+1] = ')') then
        InComm := 0;
    end;
  ProceedStr := S;
  end { TDefCollection.ProceedStr };

function TDefCollection.ProceedStr2(Str: String): String;
  var
    i: Integer;
    j: Integer;
    k: Integer;
    S: String;
    Q: String;
    Z: String;
  begin
  ProceedStr2 := '';
  S := Str;
  while (Str <> '') and (Str[1] = ' ') do
    System.Delete(Str, 1, 1);
  if  (Length(Str) >= 2) and (Str[1] = ';') and (Str[2] = '$') then
    begin
    i := 1;
    k := i;
    Inc(i);
    Inc(i);
    j := i+1;
    while not ((j = Length(Str)) or (Str[j] in BreakChars)) do
      Inc(j);
    if not (Str[j] in BreakChars) then
      Inc(j);
    Q := UpStrg(Copy(Str, i, j-i));
    i := j;
    if  (Q = 'IFDEF') or (Q = 'IFNDEF') then
      begin
      while (Str[i] in BreakChars) do
        Inc(i);
      j := i;
      while not (Str[j] in BreakChars) do
        Inc(j);
      Z := Copy(Str, i, j-i);
      i := j;
      Push(Z, Q = 'IFDEF');
      end
    else if Q = 'ELSE' then
      Pop(False)
    else if Q = 'ENDIF' then
      Pop(True);
    S := '';
    end;
  if TextOk then
    ProceedStr2 := S;
  end { TDefCollection.ProceedStr2 };

procedure TDefCollection.Push(const ADef: String; IfDef: Boolean);
  begin
  Inc(SP);
  if DefStack[SP-1]
  then
    DefStack[SP] := not (IfDef xor IsDef(ADef))
  else
    DefStack[SP] := False;
  end;

procedure TDefCollection.Pop(EndIf: Boolean);
  begin
  if EndIf
  then
    if SP > 0 then
      Dec(SP)
    else
      DefStack[0] := True
  else
    DefStack[SP] := not DefStack[SP];
  end;

function TDefCollection.TextOk: Boolean;
  begin
  TextOk := DefStack[SP];
  end;

end.
