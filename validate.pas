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

unit Validate;

{-----------------------------------------------------}
{ This module is based on Turbo Vision Validate Unit  }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}

interface

uses
  Defines, Objects2, Streams
  ;

const

  { TValidator Status constants }

  vsOk = 0;
  vsSyntax = 1;
  { Error in the syntax of either a TPXPictureValidator
                        or a TDBPictureValidator }

  { Validator option flags }
  voFill = $0001;
  voTransfer = $0002;
  voReserved = $00FC;

  { TVTransfer constants }

type
  TVTransfer = (vtDataSize, vtSetData, vtGetData);

  { Abstract TValidator object }

  PValidator = ^TValidator;
  TValidator = object(TObject)
    Status: Word;
    Options: AWord;
    constructor Init;
    constructor Load(var S: TStream);
    procedure Error; virtual;
    function IsValidInput(var S: AnsiString;
        SuppressFill: Boolean): Boolean; virtual;
    function isValid(const S: AnsiString): Boolean; virtual;
    procedure Store(var S: TStream);
    function Transfer(var S: AnsiString; Buffer: Pointer;
        Flag: TVTransfer): Word; virtual;
    function Valid(const S: AnsiString): Boolean;
    end;

  { TPXPictureValidator result type }

  TPicResult = (prComplete, prIncomplete, prEmpty, prError, prSyntax,
    prAmbiguous, prIncompNoFill);

  { TFilterValidator }

  PFilterValidator = ^TFilterValidator;
  TFilterValidator = object(TValidator)
    ValidChars: TCharSet;
    constructor Init(AValidChars: TCharSet);
    constructor Load(var S: TStream);
    procedure Error; virtual;
    function isValid(const S: AnsiString): Boolean; virtual;
    function IsValidInput(var S: AnsiString;
        SuppressFill: Boolean): Boolean; virtual;
    procedure Store(var S: TStream);
    end;

  { TRangeValidator }

  PRangeValidator = ^TRangeValidator;
  TRangeValidator = object(TFilterValidator)
    Min, Max: LongInt;
    constructor Init(AMin, AMax: LongInt);
    constructor Load(var S: TStream);
    procedure Error; virtual;
    function isValid(const S: AnsiString): Boolean; virtual;
    procedure Store(var S: TStream);
    function Transfer(var S: AnsiString; Buffer: Pointer;
        Flag: TVTransfer): Word; virtual;
    end;

implementation

uses
  Commands, DNApp, Messages, Advance1, Advance2
  ;

{ TValidator }

constructor TValidator.Init;
  begin
  inherited Init;
  Status := 0;
  Options := 0;
  end;

constructor TValidator.Load(var S: TStream);
  begin
  Init;
  Status := 0;
  S.Read(Options, SizeOf(Options));
  end;

procedure TValidator.Error;
  begin
  end;

function TValidator.IsValidInput(var S: AnsiString; SuppressFill: Boolean):
  Boolean;
  begin
  IsValidInput := True;
  end;

function TValidator.isValid(const S: AnsiString): Boolean;
  begin
  isValid := True;
  end;

procedure TValidator.Store(var S: TStream);
  begin
  S.Write(Options, SizeOf(Options));
  end;

function TValidator.Transfer(var S: AnsiString; Buffer: Pointer;
    Flag: TVTransfer): Word;
  begin
  Transfer := 0;
  end;

function TValidator.Valid(const S: AnsiString): Boolean;
  begin
  Valid := False;
  if not isValid(S) then
    begin
    Error;
    Exit;
    end;
  Valid := True;
  end;

(*

{ TPXPictureValidator }

constructor TPXPictureValidator.Init(const APic: AnsiString;
  AutoFill: Boolean);
var
  S: String;
begin
  inherited Init;
  Pic := NewStr(APic);
  if AutoFill then Options := Options or voFill;
  S := '';
  if Picture(S, False) <> prEmpty then
    Status := vsSyntax;
end;

constructor TPXPictureValidator.Load(var S: TStream);
begin
  inherited Load(S);
  Pic := S.ReadStr;
end;

destructor TPXPictureValidator.Done;
begin
  DisposeStr(Pic);
  inherited Done;
end;

{$IFDEF Windows}

procedure TPXPictureValidator.Error;
var
  MsgStr: array[0..255] of Char;
  PicSz: array[0..255] of Char;
begin
  StrPCopy(@PicSz, Pic^);
  wvsprintf(MsgStr, 'Error in picture format.'#13' %s', PicSz);
  MessageBox(0, MsgStr, 'Validator', mb_IconExclamation or mb_Ok);
end;

{$ELSE}

procedure TPXPictureValidator.Error;
begin
  MessageBox('Error in picture format.'#13' %s', @Pic,
    mfError + mfOKButton);
end;

{$ENDIF Windows}

function TPXPictureValidator.IsValidInput(var S: string;
  SuppressFill: Boolean): Boolean;
begin
  IsValidInput := (Pic = nil) or
     (Picture(S, (Options and voFill <> 0)  and not SuppressFill) <> prError);
end;

function TPXPictureValidator.IsValid(const S: string): Boolean;
var
  Str: String;
begin
  Str := S;
  IsValid := (Pic = nil) or (Picture(Str, False) = prComplete);
end;

function IsNumber(Chr: Char): Boolean; near; assembler;
asm
        XOR     AL,AL
        MOV     Ch,Chr
        CMP     Ch,'0'
        JB      @@1
        CMP     Ch,'9'
        JA      @@1
        INC     AL
@@1:
end;

function IsLetter(Chr: Char): Boolean; near; assembler;
asm
        XOR     AL,AL
        MOV     Cl,Chr
        AND     Cl,0DFH
        CMP     Cl,'A'
        JB      @@2
        CMP     Cl,'Z'
        JA      @@2
@@1:    INC     AL
@@2:
end;

function IsSpecial(Chr: Char; const Special: string): Boolean; near;
  assembler;
asm
        XOR     AH,AH
        LES     DI,Special
        MOV     AL,ES:[DI]
        INC     DI
        MOV     CH,AH
        MOV     CL,AL
        MOV     AL,Chr
        REPNE   SCASB
        JCXZ    @@1
        INC     AH
@@1:    MOV     AL,AH
end;

{ This helper function will be used for a persistant TInputLine mask.
  It will be moved to DIALOGS.PAS when needed. }
{DataCompBoy
function NumChar(Chr: Char; const S: string): Byte; near; assembler;
asm
        XOR     AH,AH
        LES     DI,S
        MOV     AL,ES:[DI]
        INC     DI
        MOV     CH,AH
        MOV     CL,AL
        MOV     AL,Chr
@@1:    REPNE   SCASB
        JCXZ    @@2
        INC     AH
        JMP     @@1
@@2:    MOV     AL,AH
end;
}
function IsComplete(Rslt: TPicResult): Boolean;
begin
  IsComplete := Rslt in [prComplete, prAmbiguous];
end;

function IsIncomplete(Rslt: TPicResult): Boolean;
begin
  IsIncomplete := Rslt in [prIncomplete, prIncompNoFill];
end;

function TPXPictureValidator.Picture(var Input: string;
  AutoFill: Boolean): TPicResult;
var
  I, J: Byte;
  Rslt: TPicResult;
  Reprocess: Boolean;

  function Process(TermCh: Byte): TPicResult;
  var
    Rslt: TPicResult;
    Incomp: Boolean;
    OldI, OldJ, IncompJ, IncompI: Byte;

    { Consume input }

    procedure Consume(Ch: Char);
    begin
      Input[J] := Ch;
      Inc(J);
      Inc(I);
    end;

    { Skip a character or a picture group }

    procedure ToGroupEnd(var I: Byte);
    var
      BrkLevel, BrcLevel: Integer;
    begin
      BrkLevel := 0;
      BrcLevel := 0;
      repeat
        if I = TermCh then Exit;
        case Pic^[I] of
          '[': Inc(BrkLevel);
          ']': Dec(BrkLevel);
          '{': Inc(BrcLevel);
          '}': Dec(BrcLevel);
          ';': Inc(I);
          '*':
            begin
              Inc(I);
              while IsNumber(Pic^[I]) do Inc(I);
            end;
        end;
        Inc(I);
      until (BrkLevel = 0) and (BrcLevel = 0);
    end;

    { Find the a comma separator }

    function SkipToComma: Boolean;
    begin
      repeat ToGroupEnd(I) until (I = TermCh) or (Pic^[I] = ',');
      if Pic^[I] = ',' then Inc(I);
      SkipToComma := I < TermCh;
    end;

    { Calclate the end of a group }

    function CalcTerm: Byte;
    var
      K: Byte;
    begin
      K := I;
      ToGroupEnd(K);
      CalcTerm := K;
    end;

    { The next group is repeated X times }

    function Iteration: TPicResult;
    var
      SubPic: String;
      Itr, K, L, OldJ: Byte;
      Rslt: TPicResult;
      TermCh: Byte;
    begin
      Itr := 0;
      Iteration := prError;

      Inc(I);  { Skip '*' }

      { Retrieve number }

      while IsNumber(Pic^[I]) do
      begin
        Itr := Itr * 10 + Byte(Pic^[I]) - Byte('0');
        Inc(I);
      end;

      K := I;
      TermCh := CalcTerm;

      if Pic^[I] = #0 then
      begin
        Iteration := prSyntax;
        Exit;
      end;

      { If Itr is 0 allow any number, otherwise enforce the number }
      if Itr <> 0 then
      begin
        for L := 1 to Itr do
        begin
          I := K;
          Rslt := Process(TermCh);
          if not IsComplete(Rslt) then
          begin
            { Empty means incomplete since all are required }
            if Rslt = prEmpty then Rslt := prIncomplete;
            Iteration := Rslt;
            Exit;
          end;
        end;
      end
      else
      begin
        repeat
          I := K;
          OldJ := J;
          Rslt := Process(TermCh);
        until Rslt <> prComplete;
        if (Rslt = prEmpty) or (Rslt = prError) then
        begin
          Inc(I);
          Rslt := prAmbiguous;
        end;
      end;
      I := TermCh;
      Iteration := Rslt;
    end;

    { Process a picture group }

    function Group: TPicResult;
    var
      Rslt: TPicResult;
      TermCh: Byte;
    begin
      TermCh := CalcTerm;
      Inc(I);
      Rslt := Process(TermCh - 1);
      if not IsIncomplete(Rslt) then I := TermCh;
      Group := Rslt;
    end;

    function CheckComplete(Rslt: TPicResult): TPicResult;
    var
      J: Byte;
    begin
      J := I;
      if IsIncomplete(Rslt) then
      begin
        { Skip optional pieces }
        while True do
          case Pic^[J] of
            '[': ToGroupEnd(J);
            '*':
              if not IsNumber(Pic^[J + 1]) then
              begin
                Inc(J);
                ToGroupEnd(J);
              end
              else
                Break;
          else
            Break;
          end;

        if J = TermCh then Rslt := prAmbiguous;
      end;
      CheckComplete := Rslt;
    end;

    function Scan: TPicResult;
    var
      Ch: Char;
      Rslt: TPicResult;
    begin
      Scan := prError;
      Rslt := prEmpty;
      while (I <> TermCh) and (Pic^[I] <> ',') do
      begin
        if J > Length(Input) then
        begin
          Scan := CheckComplete(Rslt);
          Exit;
        end;

        Ch := Input[J];
        case Pic^[I] of
          '#': if not IsNumber(Ch) then Exit
               else Consume(Ch);
          '?': if not IsLetter(Ch) then Exit
               else Consume(Ch);
          '&': if not IsLetter(Ch) then Exit
               else Consume(UpCase(Ch));
          '!': Consume(UpCase(Ch));
          '@': Consume(Ch);
          '*':
            begin
              Rslt := Iteration;
              if not IsComplete(Rslt) then
              begin
                Scan := Rslt;
                Exit;
              end;
              if Rslt = prError then Rslt := prAmbiguous;
            end;
          '{':
            begin
              Rslt := Group;
              if not IsComplete(Rslt) then
              begin
                Scan := Rslt;
                Exit;
              end;
            end;
          '[':
            begin
              Rslt := Group;
              if IsIncomplete(Rslt) then
              begin
                Scan := Rslt;
                Exit;
              end;
              if Rslt = prError then Rslt := prAmbiguous;
            end;
        else
          if Pic^[I] = ';' then Inc(I);
          if UpCase(Pic^[I]) <> UpCase(Ch) then
            if Ch = ' ' then Ch := Pic^[I]
            else Exit;
          Consume(Pic^[I]);
        end;

        if Rslt = prAmbiguous then
          Rslt := prIncompNoFill
        else
          Rslt := prIncomplete;
      end;

      if Rslt = prIncompNoFill then
        Scan := prAmbiguous
      else
        Scan := prComplete;
    end;

  begin
    Incomp := False;
    OldI := I;
    OldJ := J;
    repeat
      Rslt := Scan;

      { Only accept completes if they make it farther in the input
        stream from the last incomplete }
      if (Rslt = prComplete) and Incomp and (J < IncompJ) then
      begin
        Rslt := prIncomplete;
        J := IncompJ;
      end;

      if (Rslt = prError) or (Rslt = prIncomplete) then
      begin
        Process := Rslt;
        if not Incomp and (Rslt = prIncomplete) then
        begin
          Incomp := True;
          IncompI := I;
          IncompJ := J;
        end;
        I := OldI;
        J := OldJ;
        if not SkipToComma then
        begin
          if Incomp then
          begin
            Process := prIncomplete;
            I := IncompI;
            J := IncompJ;
          end;
          Exit;
        end;
        OldI := I;
      end;
    until (Rslt <> prError) and (Rslt <> prIncomplete);

    if (Rslt = prComplete) and Incomp then
      Process := prAmbiguous
    else
      Process := Rslt;
  end;

  function SyntaxCheck: Boolean;
  var
    I: Integer;
    BrkLevel, BrcLevel: Integer;
  begin
    SyntaxCheck := False;

    if Pic^ = '' then Exit;

    if Pic^[Length(Pic^)] = ';' then Exit;

    I := 1;
    BrkLevel := 0;
    BrcLevel := 0;
    while I <= Length(Pic^) do
    begin
      case Pic^[I] of
        '[': Inc(BrkLevel);
        ']': Dec(BrkLevel);
        '{': Inc(BrcLevel);
        '}': Dec(BrcLevel);
        ';': Inc(I);
      end;
      Inc(I);
    end;
    if (BrkLevel <> 0) or (BrcLevel <> 0) then Exit;

    SyntaxCheck := True;
  end;


begin
  Picture := prSyntax;
  if not SyntaxCheck then Exit;

  Picture := prEmpty;
  if Input = '' then Exit;

  J := 1;
  I := 1;

  Rslt := Process(Length(Pic^) + 1);
  if (Rslt <> prError) and (Rslt <> prSyntax) and (J <= Length(Input)) then
    Rslt := prError;

  if (Rslt = prIncomplete) and AutoFill then
  begin
    Reprocess := False;
    while (I <= Length(Pic^)) and
      not IsSpecial(Pic^[I], '#?&!@*{}[],'#0) do
    begin
      if Pic^[I] = ';' then Inc(I);
      Input := Input + Pic^[I];
      Inc(I);
      Reprocess := True;
    end;
    J := 1;
    I := 1;
    if Reprocess then
      Rslt := Process(Length(Pic^) + 1)
  end;

  if Rslt = prAmbiguous then
    Picture := prComplete
  else if Rslt = prIncompNoFill then
    Picture := prIncomplete
  else
    Picture := Rslt;
end;

procedure TPXPictureValidator.Store(var S: TStream);
begin
  inherited Store(S);
  S.WriteStr(Pic);
end;

*)

{ TFilterValidator }

constructor TFilterValidator.Init(AValidChars: TCharSet);
  begin
  inherited Init;
  ValidChars := AValidChars;
  end;

constructor TFilterValidator.Load(var S: TStream);
  begin
  inherited Load(S);
  S.Read(ValidChars, SizeOf(TCharSet));
  end;

function TFilterValidator.isValid(const S: AnsiString): Boolean;
  var
    I: Integer;
  begin
  I := 1;
  while S[I] in ValidChars do
    Inc(I);
  isValid := I > Length(S);
  end;

function TFilterValidator.IsValidInput(var S: AnsiString;
     SuppressFill: Boolean): Boolean;
  var
    I: Integer;
  begin
  Result := True;
  for I := 1 to Length(S) do
    if not (S[I] in ValidChars) then
      begin
      Result := False;
      Break;
      end;
{AK155 Что-то я не понял, почеу SuppressFill никак не используется }
  end;

procedure TFilterValidator.Store(var S: TStream);
  begin
  inherited Store(S);
  S.Write(ValidChars, SizeOf(TCharSet));
  end;

procedure TFilterValidator.Error;
  begin
  MessageBox('Invalid character in input', nil, mfError+mfOKButton);
  end;

{ TRangeValidator }

constructor TRangeValidator.Init(AMin, AMax: LongInt);
  begin
  inherited Init(['0'..'9', '+', '-']);
  if AMin >= 0 then
    ValidChars := ValidChars-['-'];
  Min := AMin;
  Max := AMax;
  end;

constructor TRangeValidator.Load(var S: TStream);
  begin
  inherited Load(S);
  S.Read(Min, SizeOf(Max)+SizeOf(Min));
  end;

procedure TRangeValidator.Error;
  var
    Params: array[0..1] of LongInt;
  begin
  Params[0] := Min;
  Params[1] := Max;
  MessageBox(GetString(dlValueNotInRange), @Params,
    mfError+mfOKButton);
  end;

function TRangeValidator.isValid(const S: AnsiString): Boolean;
  var
    Value: LongInt;
    Code: Integer;
  begin
  isValid := False;
  if inherited isValid(S) then
    begin
    Val(S, Value, Code);
    if  (Code = 0) and (Value >= Min) and (Value <= Max) then
      isValid := True;
    end;
  end;

procedure TRangeValidator.Store(var S: TStream);
  begin
  inherited Store(S);
  S.Write(Min, SizeOf(Max)+SizeOf(Min));
  end;

function TRangeValidator.Transfer(var S: AnsiString; Buffer: Pointer;
    Flag: TVTransfer): Word;
  var
    Value: LongInt;
    Code: Integer;
  begin
  if Options and voTransfer <> 0 then
    begin
    Transfer := SizeOf(Value);
    case Flag of
      vtGetData:
        begin
        Val(S, Value, Code);
        LongInt(Buffer^) := Value;
        end;
      vtSetData:
        Str(LongInt(Buffer^), S);
    end {case};
    end
  else
    Transfer := 0;
  end;

(*
{ TLookupValidator }

function TLookupValidator.IsValid(const S: string): Boolean;
begin
  IsValid := Lookup(S);
end;

function TLookupValidator.Lookup(const S: string): Boolean;
begin
  Lookup := True;
end;

*)

(*

{ TStringLookupValidator }

constructor TStringLookupValidator.Init(AStrings: PStringCollection);
begin
  inherited Init;
  Strings := AStrings;
end;

constructor TStringLookupValidator.Load(var S: TStream);
begin
  inherited Load(S);
  Strings := PStringCollection(S.Get);
end;

destructor TStringLookupValidator.Done;
begin
  NewStringList(nil);
  inherited Done;
end;

{$IFDEF Windows}

procedure TStringLookupValidator.Error;
begin
  MessageBox(0, 'Input not in valid-list', 'Validator',
    mb_IconExclamation or mb_Ok);
end;

{$ELSE}

procedure TStringLookupValidator.Error;
begin
  MessageBox('Input not in valid-list', nil, mfError + mfOKButton);
end;

{$ENDIF Windows}

function TStringLookupValidator.Lookup(const S: string): Boolean;
var
  Index: Integer;
  Str: PString;
begin
  asm
        LES     DI,S
        MOV     Str.Word[0], DI
        MOV     Str.Word[2], ES
  end;
  Lookup := False;
  if Strings <> nil then
    Lookup := Strings^.Search(Str, Index);
end;

procedure TStringLookupValidator.NewStringList(AStrings: PStringCollection);
begin
  if Strings <> nil then Dispose(Strings,Done);
  Strings := AStrings;
end;

procedure TStringLookupValidator.Store(var S: TStream);
begin
  inherited Store(S);
  S.Put(Strings);
end;

*)

end.
