unit _RegExp;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Objects
  ;

type
  PRegExp = ^TRegExp;
  TRegExp = object(TObject)
    FStatus: TRegExpStatus;
    FStart: Integer;
    FLength: Integer;
    constructor Init;
    {destructor Done; virtual;}
    procedure Reset;
    function CompileString(const AExpression: String): Boolean;
    function CompileStr(AExpression: PChar): Boolean;
    function Compile(AExpression: PChar; ALength: Integer): Boolean;
    function Execute(AString: PChar; ALength: Integer): Boolean;
    function SubstituteString(ASrc: PChar; const AReplace: String;
         var ADest: String): Boolean;
    function SubstituteStr(ASrc, AReplace: PChar; ADest: PChar;
         var ALength: Integer): Boolean;
    function Substitute(ASrc, AReplace: PChar; ARLen: Integer;
         ADest: PChar; var ADLen: Integer): Boolean;
    procedure Error(AStatus: TRegExpStatus); virtual;
    function CheckBreak: Boolean; virtual;
    procedure Escape(AChar: Char; var ASubExp: PChar; var ALen: Integer)
      ; virtual;
  private
    FFlags: set of
      (
      ffCompiled,
      ffAnchored,
      ffStart,
      ffParsing,
      ffMatchNext,
      ffBreak,
      ffAutoTag
      );
    FCodeSize: Word;
    FCodeData: PChar;
    FStartP: array[1..9] of Integer;
    FEndP: array[1..9] of Integer;
    FStartCh: Char;
    FMust: PString;
    FInput: PChar;
    FInputBol: PChar;
    FInputEol: PChar;
    FLStack: array[1..10] of Integer;
    FLStackId: Integer;
    end;

implementation

uses
  _DNFuncs
  ;

constructor TRegExp.Init;
  begin
  _TRegExp^.Init(nil, @Self);
  end;

procedure TRegExp.Reset;
  begin
  _TRegExp^.Reset(@Self);
  end;

function TRegExp.CompileString(const AExpression: String): Boolean;
  begin
  Result := _TRegExp^.CompileString(AExpression, @Self);
  end;

function TRegExp.CompileStr(AExpression: PChar): Boolean;
  begin
  Result := _TRegExp^.CompileStr(AExpression, @Self);
  end;

function TRegExp.Compile(AExpression: PChar; ALength: Integer): Boolean;
  begin
  Result := _TRegExp^.Compile(AExpression, ALength, @Self);
  end;

function TRegExp.Execute(AString: PChar; ALength: Integer): Boolean;
  begin
  Result := _TRegExp^.Execute(AString, ALength, @Self);
  end;

function TRegExp.SubstituteString(ASrc: PChar; const AReplace: String;
     var ADest: String): Boolean;
  begin
  Result := _TRegExp^.SubstituteString(ASrc, AReplace, ADest, @Self);
  end;

function TRegExp.SubstituteStr(ASrc, AReplace: PChar; ADest: PChar;
     var ALength: Integer): Boolean;
  begin
  Result := _TRegExp^.SubstituteStr(ASrc, AReplace, ADest, ALength, @Self);
  end;

function TRegExp.Substitute(ASrc, AReplace: PChar; ARLen: Integer;
     ADest: PChar; var ADLen: Integer): Boolean;
  begin
  Result := _TRegExp^.Substitute(ASrc, AReplace, ARLen, ADest, ADLen,
       @Self);
  end;

procedure TRegExp.Error(AStatus: TRegExpStatus);
  assembler; {&Frame-}
asm
end;

function TRegExp.CheckBreak: Boolean;
  assembler; {&Frame-}
asm
end;

procedure TRegExp.Escape(AChar: Char; var ASubExp: PChar;
     var ALen: Integer);
  assembler; {&Frame-}
asm
end;

end.
