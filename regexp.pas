(*****************************************************************
 * Copyright (C), 2001 Pawel Ziemian, All rights reserved        *
 *****************************************************************)

(*****************************************************************
 *
 * SOURCE FILE: REGEXP.PAS
 *
 * MODULE NAME: Regular Expression for Objects
 *
 * PURPOSE:
 *
 * AUTHOR:      Pawel Ziemian (PZ)
 *
 * REVIEWED BY:
 *
 * ORIGINAL NOTES:
 * ===============================================================
 * Based on: REGEX.C, REGEX.H                           2001.09.16
 * ---------------------------------------------------------------
 * Copyright (c) 1986 by University of Toronto.
 * Written by Henry Spencer.  Not derived from licensed software.
 *
 * Permission is granted to anyone to use this software for any
 * purpose on any computer system, and to redistribute it freely,
 * subject to the following restrictions:
 *
 * 1. The author is not responsible for the consequences of use of
 *    this software, no matter how awful, even if they arise
 *    from defects in it.
 *
 * 2. The origin of this software must not be misrepresented, either
 *    by explicit claim or by omission.
 *
 * 3. Altered versions must be plainly marked as such, and must not
 *    be misrepresented as being the original software.
 *
 * Beware that some of this code is subtly aware of the way operator
 * precedence is structured in regular expressions.  Serious changes
 * in regular-expression syntax might require a total rethink.
 * ===============================================================
 * Based on: REGEXPR.PAS v. 0.942                       2001.10.24
 * ---------------------------------------------------------------
 * Copyright (c) 1999-00 by Andrey V. Sorokin <anso@mail.ru>
 *
 * This software is provided as it is, without any kind of warranty
 * given. Use it at your own risk.
 *
 * You may use this software in any kind of development, including
 * comercial, redistribute, and modify it freely, under the
 * following restrictions :
 * 1. The origin of this software may not be mispresented, you must
 *    not claim that you wrote the original software. If you use
 *    this software in any kind of product, it would be appreciated
 *    that there in a information box, or in the documentation would
 *    be an acknowledgmnent like this
 *           Partial Copyright (c) 2000 by Andrey V. Sorokin
 * 2. You may not have any income from distributing this source
 *    to other developers. When you use this product in a comercial
 *    package, the source may not be charged seperatly.
 * ===============================================================
 *
 * HISTORY:
 *
 * Ver   Date       Sign   Description
 *
 * 0.01  2001.09.16 PZ     Created based on REGEX.C
 * 0.02  2001.09.20 PZ     Object version
 * 0.03  2001.09.23 PZ     Optimized
 * 0.04  2001.09.26 PZ     Added '\:#' escapes
 * 0.05  2001.09.27 PZ     Optimized
 * 0.06  2001.10.01 PZ     Fixed bugs in '\:#'
 * 0.07  2001.10.02 PZ     Added '\a', '\b' and '\v'
 * 0.08  2001.10.03 PZ     Added posibility to define
 *                         user '\:#' escapes ('#' is 0..9)
 * 0.09  2001.10.18 PZ     Added Substitution methods
 *                         ('\0'..'\9' for '(..)')
 * 0.10  2001.10.22 PZ     Added tagged expressions
 *                         - '(?dX)' where d is 1..9
 *                         - '(?:X)' no tagged
 *                         - '\1'..'\9' escapes
 * 0.11  2001.10.24 PZ     Added '{n,m}' syntax (by REGEXPR.PAS).
 * 0.12  2001.11.13 PZ     Fixed bugs
 *
 *****************************************************************)

{$I STDEFINE.INC}
{$UNDEF DEBUG}
{$DEFINE ComplexBraces}
{ define for beta-version of braces                  }
{ (in stable version it works only for simple cases) }

unit RegExp;

interface

uses
  Objects2
  ;

type

  TRegExpStatus = (
    resOK,
    resCanceled,
    resNilArgument,
    resInvalidArgument,
    resRegExpTooBig,
    resOutOfSpace,
    resCorruptedProgram,
    resUnmatchedParenthesis,
    resJunkOnEnd,
    resStarPlusOperandCouldBeEmpty,
    resNestedStarQuotePlus,
    resInvalidEscape,
    resInvalidPredefinedExp,
    resUndefinedPredefinedExp,
    resStackOverflow,
    resInvalidSetRange,
    resUnmatchedSquareBracket,
    resInternalUrp,
    resOperatorFollowsNothing,
    resTrailingBackSlash,
    resInternalDisaster,
    resNoExpression,
    resMemoryCorruption,
    resCorruptedPointers,
    resInternalFoulup,

    resDuplicatedTaggedExp,
    resInvalidTaggedExp,
    resComplexBracesNotImplemented,
    resInvalidBraces,
    resLoopStackExceeded,
    resLoopWithoutEntry
    );

type

  (*
   * Flags to be passed up and down.
   *)
  TRegExpFlag = (
    refHasWidth, (* Known never to match null string.      *)
    refSimple, (* Simple enough to be STAR/PLUS operand. *)
    refSpStart (* Starts with * or +.                    *)
    );
  TRegExpFlags = set of TRegExpFlag;

const
  LoopStackMax = 10; { max depth of loops stack }

type

  PRegExp = ^TRegExp;
  TRegExp = object(TObject)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    {$IFDEF DEBUG}
    Narrate: Boolean;
    {$ENDIF DEBUG}
    FStatus: TRegExpStatus;
    FStart: Integer;
    FLength: Integer;
    constructor Init;
    destructor Done; virtual;
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
    {$IFDEF DEBUG}
    procedure Dump;
    {$ENDIF DEBUG}
  private
    (*
   * The "internal use only" fields in regexp.h are present to pass info
   * from compile to execute that permits the execute phase to run lots
   * faster on simple cases.  They are:
   *
   * regstart char that must begin a match; '\0' if none obvious
   * reganch  is the match anchored (at beginning-of-line only)?
   * regmust  string (pointer into program) that match must include, or NULL
   * regmlen  length of regmust string
   *
   * Regstart and reganch permit very fast decisions on suitable starting
   * points for a match, cutting down the work a lot.  Regmust permits fast
   * rejection of lines that cannot possibly match.  The regmust tests are
   * costly enough that regcomp() supplies a regmust only if the r.e. contains
   * something potentially expensive (at present, the only such thing detected
   * is * or + at the start of the r.e., which can involve a lot of backup).
   * Regmlen is supplied because the test in regexec() needs it and regcomp()
   * is computing it anyway.
   *)
    FFlags: set of (
      ffCompiled, { expression is compiled }
      ffAnchored, { expression is anchored }
      ffStart, { expression starts with FStartCh }
      ffParsing, { calculating code size }
      ffMatchNext,
      ffBreak, { 'Execute' is cancelled }
      ffAutoTag { automatic tagged expressions }
      );
    FCodeSize: Word; { *1* }
    FCodeData: PChar; { *2* }
    FStartP: array[1..9] of Integer; { founded expr starting points }
    FEndP: array[1..9] of Integer; { founded expr end points      }
    FStartCh: Char;
    FMust: PString; { *3* }
    FInput: PChar; { *4* }
    FInputBol: PChar; { *5* }
    FInputEol: PChar; { *6* }
    FLStack: array[1..LoopStackMax] of Integer;
    { state before entering loop }
    FLStackId: Integer; { 0 - out of all loops       }
    {
    // The following variables have different meaning while parsing or
    // compiling the regular expression:
    //
    // *1* - Calculates the program size while parsing.
    // *2* - Not used while parsing except it must be non-nil.
    //       Points to the next emited code byte while compiling.
    // *3* - Points to internal stack while parsing or compiling.
    // *4* - Points to the next character of being parsed or compiled
    //       (sub)expression.
    // *5* - Points to the being parsed or compiled (sub)expression.
    // *6* - Points just beyond the being parsed/compiled expression.
  }
    { Compilation routines }
    function RegCompile(AExpression: PChar; ALength: Integer): Boolean;
    procedure IncRegSize(I: Integer);
    function Reg(Paren: Integer; var FlagP: TRegExpFlags): PChar;
    function RegBranch(var FlagP: TRegExpFlags): PChar;
    function RegPiece(var FlagP: TRegExpFlags): PChar;
    function RegAtom(var FlagP: TRegExpFlags): PChar;
    function RegEnter(Op: Char; Tagged: Boolean): Boolean;
    procedure RegLeave;
    function RegLoadNumber(Base: Integer; Digits: Integer; var AResult)
      : Boolean;
    function RegEscape(var AResult: Char): Boolean;
    function RegSet: PChar;
    function RegExactly(var FlagP: TRegExpFlags): PChar;
    function RegNode(Op: Char): PChar;
    procedure RegC(B: Char);
    function RegInsert(Op: Char; Opnd: PChar; ASize: Integer): PChar;
    procedure RegTail(P: PChar; Val: PChar);
    procedure RegOpTail(P: PChar; Val: PChar);
    { Execute routines }
    function RegExecute(AString: PChar; ALength: Integer): Boolean;
    function RegTry(AString: PChar): Boolean;
    function RegMatch(Prog: PChar): Boolean;
    function RegRepeat(P: PChar; AMax: Integer): Integer;
    { Common routines }
    function RegNext(P: PChar): PChar;
    procedure RegClearTags;
    { Replace support }
    function RegSub(ASrc, AReplace: PChar; ARLen: Integer; ADest: PChar;
         var ADLen: Integer): Boolean;
    end;

implementation

uses
  Strings,
  Memory, Advance
  ;

(*****************************************************************
 *
 * FUNCTION:    StrScan2
 *
 * PURPOSE:     This function returns a pointer to the first
 *              occurrence of a character in a string.
 *
 * INPUTS:      P  - A string to scan.
 *              C  - A character to scan.
 *              P2 - An address just beyond the string.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     nil   - A character not found.
 *              Other - An address of the character in P.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function StrScan2(P: PChar; C: Char; P2: PChar): PChar;
  {$IFDEF NOASM}
  begin
  StrScan2 := nil;
  while P < P2 do
    begin
    if P[0] = C then
      begin
      StrScan2 := P;
      Exit;
      end;
    Inc(P);
    end;
  end;
  {$ELSE ASM}
  assembler;
  {&Frame-} {&USES EDI, ECX}
asm
        MOV     EDI,P
        MOV     ECX,P2
        XOR     EAX,EAX         { default nil result }
        SUB     ECX,EDI         { calculate length }
        JBE     @@1             { P2 must be greater than P }
        MOV     AL,C
        CLD
        REPNE   SCASB
        MOV     AL,0            { restore the nil }
        JNE     @@1
        LEA     EAX,[EDI-1]     { get the address }
@@1:
end;
{$ENDIF ASM}

(*****************************************************************
 *
 * TRegExp support types and routines.
 *
 *****************************************************************)

(*****************************************************************
 *
 * Structure for regexp "program".  This is essentially a linear
 * encoding of a nondeterministic finite-state machine (aka syntax
 * charts or "railroad normal form" in parsing technology).  Each
 * node is an opcode plus a "next" pointer, possibly plus an operand.
 * "Next" pointers of all nodes except BRANCH implement concatenation;
 * a "next" pointer with a BRANCH on both ends of it is connecting
 * two alternatives.  (Here we have one of the subtle syntax
 * dependencies:  an individual BRANCH (as opposed to a collection of
 * them) is never concatenated with anything because of operator
 * precedence.)  The operand of some types of node is a literal
 * string; for others, it is a node leading into a sub-FSM.
 * In particular, the operand of a BRANCH node is the first node
 * of the branch. (NB this is *not* a tree structure:  the tail of
 * the branch connects to the thing following the set of BRANCHes.)
 * The opcodes are:
 *)

const
  (* definition    number   opnd? meaning *)
  ropEND = #00; (* no    End of program. *)
  ropBOL = #01; (* no    Match "" at beginning of line. *)
  ropEOL = #02; (* no    Match "" at end of line. *)
  ropANY = #03; (* no    Match any one character. *)
  ropANYOF = #04; (* str   Match any character in this string. *)
  ropANYBUT = #05; (* str   Match any character not in this string. *)
  ropBRANCH = #06; (* node  Match this alternative, or the next... *)
  ropBACK = #07; (* no    Match "", "next" ptr points backward. *)
  ropEXACTLY = #08; (* str   Match this string. *)
  ropNOTHING = #09; (* no    Match empty string. *)
  ropSTAR = #10; (* node  Match this (simple) thing 0 or more times. *)
  ropPLUS = #11; (* node  Match this (simple) thing 1 or more times. *)
  ropOPEN = #12; (* no    Mark this point in input as start of #. *)
  ropCLOSE = #13; (* no    Analogous to OPEN. *)
  ropBRACES = #14;
  {  n,m   Match this (simple) thing from n to m times. }
  ropSTARNG = #15; {        Same as START but in non-greedy mode  }
  ropPLUSNG = #16; {        Same as PLUS but in non-greedy mode }
  ropBRACESNG = #17; {        Same as BRACES but in non-greedy mode }
  ropLOOPENTRY = #18;
  {  node  Start of loop (LOOP - Node for this loop) }
  ropLOOP = #19; {  n,m   Back jump for LOOPENTRY. }
  ropLOOPNG = #20; {        Same as LOOP but in non-greedy mode }
  ropOPEN0 = #30; (* no    Mark this point in input as start of #n. *)
  ropOPEN9 = #39;
  ropCLOSE0 = #40; (* no    Analogous to OPEN0. *)
  ropCLOSE9 = #49;

  (*
 * Opcode notes:
 *
 * BRANCH   The set of branches constituting a single choice are hooked
 *    together with their "next" pointers, since precedence prevents
 *    anything being concatenated to any individual branch.  The
 *    "next" pointer of the last BRANCH in a choice points to the
 *    thing following the whole choice.  This is also where the
 *    final "next" pointer of each individual branch points; each
 *    branch starts with the operand node of a BRANCH node.
 *
 * BACK     Normal "next" pointers all implicitly point forward; BACK
 *    exists to make loop structures possible.
 *
 * STAR,PLUS   '?', and complex '*' and '+', are implemented as circular
 *    BRANCH structures using BACK.  Simple cases (one character
 *    per match) are implemented with STAR and PLUS for speed
 *    and to minimize recursive plunges.
 *
 * OPEN,CLOSE
 *
 *****************************************************************)

  (*****************************************************************
 *
 * The first byte of the regexp internal "program" is actually
 * this magic number; the start node begins in the second byte.
 *
 *****************************************************************)

const
  magic = #$9C; { 0234 }

  (*****************************************************************
 *
 * This is the maximum size of an compiled regexp program.
 * The limit is caused by size of "Next" pointer in the regexp
 * instruction which takes only two bytes.
 *
 *****************************************************************)

const
  MaxRegSize = 32768;

  (*****************************************************************
 *
 * The internal stack is introduced to allow compile '\:#' escape
 * sequences. The predefined sequence can be called recursively
 * in regular expression. There is already defined one recursive
 * call for '\:p' which uses '\:f' in their definition.
 *
 * The current stack size is two items, which is enough for '\:p'
 * (One for entry to subexpression and one for nested '\:f').
 *
 * 2001.10.03
 * The current stack is 10 due to user escapes '\:0'..'\:9' are
 * introduced. Any of them can call recursively other escape.
 *
 *****************************************************************)

const
  MaxRegExpStack = 10;

const
  ParseNumBaseMask = $00FF;
  ParseNumInteger = $0100;

const
  MaxBracesArg = Ord(High(Char));

type
  TRegExpStackItem = record
    Entry: Integer;
    bol: PChar;
    Cur: PChar;
    Eol: PChar;
    end;

  PRegExpStack = ^TRegExpStack;
  TRegExpStack = record
    Entries: Integer;
    Entry: Integer;
    FNPar: Integer;
    Level: Integer;
    Stack: array[0..MaxRegExpStack-1] of TRegExpStackItem;
    TagStarts: array[1..9] of PChar;
    TagEnds: array[1..9] of PChar;
    end;

  {$IFDEF DEBUG}

  (*****************************************************************
 *
 * FUNCTION:    RegProp
 *
 * PURPOSE:     This function returns printable representation of
 *              opcode.
 *
 * INPUTS:      Op - An opcode.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function RegProp(Op: PChar): String;
  var
    s: String;
  begin
  case Op[0] of
    ropBOL:
      s := 'BOL';
    ropEOL:
      s := 'EOL';
    ropANY:
      s := 'ANY';
    ropANYOF:
      s := 'ANYOF';
    ropANYBUT:
      s := 'ANYBUT';
    ropBRANCH:
      s := 'BRANCH';
    ropEXACTLY:
      s := 'EXACTLY';
    ropNOTHING:
      s := 'NOTHING';
    ropBACK:
      s := 'BACK';
    ropEND:
      s := 'END';
    ropOPEN:
      s := 'OPEN';
    ropCLOSE:
      s := 'CLOSE';
    ropSTAR:
      s := 'STAR';
    ropPLUS:
      s := 'PLUS';
    ropBRACES:
      s := 'BRACES';
    ropSTARNG:
      s := 'STARNG';
    ropPLUSNG:
      s := 'PLUSNG';
    ropBRACESNG:
      s := 'BRACESNG';
    ropLOOPENTRY:
      s := 'LOOPENTRY';
    ropLOOP:
      s := 'LOOP';
    ropLOOPNG:
      s := 'LOOPNG';
    else {case}
      if Op[0] in [ropOPEN0..ropOPEN9] then
        begin
        Str(Ord(Op[0])-Ord(ropOPEN0), s);
        s := 'OPEN'+s;
        end
      else if Op[0] in [ropCLOSE0..ropCLOSE9] then
        begin
        Str(Ord(Op[0])-Ord(ropCLOSE0), s);
        s := 'CLOSE'+s;
        end
      else
        begin
        Str(Ord(Op[0]), s);
        s := '#'+s;
        end;
  end {case};
  RegProp := ':'+s;
  end { RegProp };

{$ENDIF DEBUG}

(*****************************************************************
 *
 * TRegExp
 *
 *****************************************************************)

constructor TRegExp.Init;
  begin
  inherited Init;
  FCodeSize := 0;
  FCodeData := nil;
  FFlags := [];
  FStatus := resOK;
  end;

destructor TRegExp.Done;
  begin
  Reset;
  inherited Done;
  end;

procedure TRegExp.Reset;
  begin
  if FCodeSize > 0 then
    begin
    if FCodeData <> nil then
      FreeMem(FCodeData, FCodeSize);
    FCodeSize := 0;
    FCodeData := nil;
    Exclude(FFlags, ffCompiled);
    end;
  FStatus := resOK;
  end;

function TRegExp.CompileString(const AExpression: String): Boolean;
  begin
  CompileString := Compile(@AExpression[1], Length(AExpression));
  end;

function TRegExp.CompileStr(AExpression: PChar): Boolean;
  begin
  CompileStr := Compile(AExpression, StrLen(AExpression));
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.Compile
 *
 * PURPOSE:     This function compile a regular expression into
 *              internal code and invoke Error if required.
 *
 * INPUTS:      AExpression - A regular expspression string.
 *              ALength     - A length of the expression string.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     False - Error.
 *              True  - Successful.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.Compile(AExpression: PChar; ALength: Integer): Boolean;
  begin
  Compile := RegCompile(AExpression, ALength);
  if FStatus <> resOK then
    Error(FStatus);
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.Execute
 *
 * PURPOSE:     This function match a regexp against a string
 *              and invokes Error method if required.
 *
 * INPUTS:      AString - A source string.
 *              ALength - A string length.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     True  - The given string match.
 *              False - The given string does not match.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.Execute(AString: PChar; ALength: Integer): Boolean;
  begin
  Execute := RegExecute(AString, ALength);
  if FStatus <> resOK then
    Error(FStatus);
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.Error
 *
 * PURPOSE:     This function is called is status is not resOK
 *              after compiling or executing regular expression.
 *
 * INPUTS:      AStatus - The status of an operation.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.Error(AStatus: TRegExpStatus);
  begin
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.CheckBreak
 *
 * PURPOSE:     This function is called periodically to check if
 *              if current searching is canceled.
 *
 * INPUTS:      None.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     True  - Cancel the search.
 *              False - Continue search.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.CheckBreak: Boolean;
  begin
  CheckBreak := False;
  end;

(*****************************************************************
 *
 * PROCEDURE:   Escape
 *
 * PURPOSE:     This procedure is called to obtain user defined
 *              escape sequence.
 *
 * INPUTS:      AChar   - A '#' character in '\:#' escape code.
 *              ASubExp - Buffer for an expression address.
 *              ALen    - Buffer for a length of the expression.
 *
 * OUTPUTS:     ASubExp - Buffer with address of the expression.
 *              ALen    - Buffer with length of the expression.
 *
 * NOTES:       1. If ASubExp is nil then the escape code is
 *                 invalid.
 *              2. If ALen is zero then expression is not defined.
 *              3. The method must be overide in derived objects
 *                 to provide its own escapes.
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.Escape(AChar: Char; var ASubExp: PChar;
     var ALen: Integer);
  begin
  ASubExp := nil;
  ALen := 0;
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegCompile
 *
 * PURPOSE:     This function compile a regular expression into
 *              internal code.
 *
 * INPUTS:      AExpression - A regular expspression string.
 *              ALength     - A length of the expression string.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     True  - Successful.
 *              False - Error.
 *
 * NOTES:       We can't allocate space until we know how big the
 *              compiled form will be, but we can't compile it
 *              (and thus know how big it is) until we've got a
 *              place to put the code.  So we cheat:  we compile
 *              it twice, once with code generation turned off and
 *              size counting turned on, and once "for real".
 *              This also means that we don't allocate space until
 *              we are sure that the thing really will compile
 *              successfully, and we never have to move the code
 *              and thus invalidate pointers into it.
 *              (Note that it has to be in one piece because free()
 *              must be able to free it all.)
 *
 *              Beware that the optimization-preparation code in
 *              here knows about some of the structure of the
 *              compiled regexp.
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegCompile(AExpression: PChar; ALength: Integer): Boolean;
  var
    scan: PChar;
    len: Integer;
    flags: TRegExpFlags;
    p: Pointer;
    dummy: Char;
    stack: TRegExpStack;
  begin
  {$IFDEF DEBUG}
  FillChar(stack, SizeOf(stack), 0);
  {$ENDIF DEBUG}
  RegCompile := False;
  Reset;
  if AExpression = nil then
    begin
    FStatus := resNilArgument;
    Exit;
    end;
  if ALength <= 0 then
    begin
    FStatus := resInvalidArgument;
    Exit;
    end;
  FMust := @stack;
  FInputBol := AExpression;
  FInputEol := AExpression+ALength;
  (* First pass: determine size, legality. *)
  FInput := AExpression;
  FCodeSize := 0;
  FCodeData := @dummy; { must be non-nil }
  stack.Entries := 0;
  stack.Entry := 0;
  stack.Level := 0;
  stack.FNPar := 1;
  RegC(magic);
  FFlags := FFlags+[ffParsing, ffAutoTag];
  RegClearTags;
  p := Reg(0, flags);
  Exclude(FFlags, ffParsing);
  if p = nil then
    begin
    FCodeSize := 0;
    FCodeData := nil;
    Exit;
    end;
  (* Small enough for pointer-storage convention? *)
  if FCodeSize >= MaxRegSize then
    begin (* Probably could be 65535. *)
    FStatus := resRegExpTooBig;
    Exit;
    end;
  (* Allocate space. *)
  p := MemAlloc(FCodeSize);
  if p = nil then
    begin
    FStatus := resOutOfSpace;
    Exit;
    end;
  {$IFDEF DEBUG}
  FillChar(p^, FCodeSize, $FF);
  {$ENDIF DEBUG}
  (* Second pass: emit code. *)
  FInput := AExpression;
  FCodeData := PChar(p);
  stack.Entries := 0;
  stack.Entry := 0;
  stack.Level := 0;
  stack.FNPar := 1;
  RegC(magic);
  RegClearTags;
  Reg(0, flags); { this should return with non-nil result }
  FCodeData := p;

  (* Dig out information for optimizations. *)
  FFlags := [];
  FMust := nil;
  scan := @FCodeData[1]; (* First BRANCH. *)
  if RegNext(scan)[0] = ropEND then
    begin
    (* Only one top-level choice. *)
    scan := @scan[3];

    (* Starting-point info. *)
    if scan[0] = ropEXACTLY then
      begin
      FStartCh := scan[4];
      Include(FFlags, ffStart);
      end
    else if scan[0] = ropBOL then
      begin
      Include(FFlags, ffAnchored);
      end;

    (*
     * If there's something expensive in the r.e., find the
     * longest literal string that must appear and make it the
     * regmust.  Resolve ties in favor of later strings, since
     * the regstart check works with the beginning of the r.e.
     * and avoiding duplication strengthens checking.  Not a
     * strong reason, but sufficient in the absence of others.
     *)
    if refSpStart in flags then
      begin
      p := nil;
      len := 0;
      while scan <> nil do
        begin
        if  (scan[0] = ropEXACTLY) and (Ord(scan[3]) >= len) then
          begin
          p := @scan[3];
          len := Ord(scan[3]);
          end;
        scan := RegNext(scan);
        end;
      FMust := PString(p);
      end;
    end;
  Include(FFlags, ffCompiled);
  RegCompile := True;
  end { TRegExp.RegCompile };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.IncRegSize
 *
 * PURPOSE:     This function is used to calculate size of compiled
 *              expression.
 *
 * INPUTS:      I - Size of next chunk of code.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.IncRegSize(I: Integer);
  begin
  if FCodeSize <= MaxRegSize then
    Inc(FCodeSize, I);
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.Reg
 *
 * PURPOSE:     This function parses regular expression, i.e.
 *              main body or parenthesized thing.
 *
 * INPUTS:      Paren - 0    = main body;
 *                      -1   = parenthesized (auto-tag);
 *                      1..9 = tagged expression 1..9;
 *              FlagP - Buffer with flags.
 *
 * OUTPUTS:     FlagP - Buffer with flags.
 *
 * RETURNS:     nil  - If error.
 *              !nil - Address of RegExp byte code.
 *
 * NOTES:       1. Caller must absorb opening parenthesis.
 *
 *              2. Combining parenthesis handling with the base
 *                 level of regular expression is a trifle forced,
 *                 but the need to tie the tails of the branches
 *                 to what follows makes it hard to avoid.
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.Reg(Paren: Integer; var FlagP: TRegExpFlags): PChar;
  var
    ret: PChar;
    br: PChar;
    ender: PChar;
    flags: TRegExpFlags;
    parno: Integer;
    stack: PRegExpStack absolute FMust;
  begin
  Reg := nil;
  FlagP := [refHasWidth]; (* Tentatively. *)

  (* Make an OPEN node, if parenthesized. *)
  if Paren <> 0 then
    begin
    parno := Paren; { use selected tagged expression }
    if ffAutoTag in FFlags then
      begin
      if parno = -1 then
        begin
        parno := stack^.FNPar;
        Inc(stack^.FNPar);
        end
      else
        begin
        RegClearTags; { clear currently defined tags }
        Exclude(FFlags, ffAutoTag);
        end;
      end;
    if not (parno in [1..9]) then
      begin
      ret := RegNode(ropOPEN);
      end
    else
      begin
      ret := RegNode(Char(Ord(ropOPEN0)+parno));
      if stack^.TagStarts[parno] <> nil then
        begin
        FStatus := resDuplicatedTaggedExp;
        Exit;
        end;
      stack^.TagStarts[parno] := FInput;
      FStartP[parno] := stack^.Entry;
      end;
    end
  else
    begin
    ret := nil;
    end;

  (* Pick up the branches, linking them together. *)
  br := RegBranch(flags);
  if br = nil then
    Exit;
  if ret <> nil then
    RegTail(ret, br) (* OPEN -> first. *)
  else
    ret := br;
  if not (refHasWidth in flags) then
    Exclude(FlagP, refHasWidth);
  FlagP := FlagP+(flags*[refSpStart]);
  while (FInput < FInputEol) and (FInput[0] = '|') do
    begin
    Inc(FInput);
    RegLeave;
    br := RegBranch(flags);
    if br = nil then
      Exit;
    RegTail(ret, br); (* BRANCH -> BRANCH. *)
    if not (refHasWidth in flags) then
      Exclude(FlagP, refHasWidth);
    FlagP := FlagP+(flags*[refSpStart]);
    end;

  (* Make a closing node, and hook it on the end. *)
  if Paren <> 0 then
    begin
    if not (parno in [1..9]) then
      begin
      ender := RegNode(ropCLOSE)
      end
    else
      begin
      ender := RegNode(Char(Ord(ropCLOSE0)+parno));
      if stack^.TagEnds[parno] <> nil then
        begin
        FStatus := resDuplicatedTaggedExp;
        Exit;
        end;
      stack^.TagEnds[parno] := FInput;
      FEndP[parno] := stack^.Entry;
      end;
    end
  else
    begin
    ender := RegNode(ropEND);
    end;
  RegTail(ret, ender);

  (* Hook the tails of the branches to the closing node. *)
  br := ret;
  while br <> nil do
    begin
    RegOpTail(br, ender);
    br := RegNext(br);
    end;

  (* Check for proper termination. *)
  if Paren <> 0 then
    begin
    if  (FInput >= FInputEol) or (FInput[0] <> ')') then
      begin
      FStatus := resUnmatchedParenthesis;
      Exit;
      end;
    Inc(FInput);
    RegLeave;
    end
  else
    begin
    if FInput < FInputEol then
      begin
      if FInput[0] = ')' then
        FStatus := resUnmatchedParenthesis
      else (* "Can't happen". *)
        FStatus := resJunkOnEnd; (* NOTREACHED *)
      Exit;
      end;
    end;

  Reg := ret;
  end { TRegExp.Reg };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegBranch
 *
 * PURPOSE:     This function implements the concatenation
 *              operator. It parses one alternative of an '|'
 *              operator.
 *
 * INPUTS:      FlagP - Buffer with flags.
 *
 * OUTPUTS:     FlagP - Buffer with flags.
 *
 * RETURNS:     nil  - If error.
 *              !nil - Address of RegExp byte code.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegBranch(var FlagP: TRegExpFlags): PChar;
  var
    ret: PChar;
    chain: PChar;
    latest: PChar;
    flags: TRegExpFlags;
  begin
  RegBranch := nil;
  FlagP := []; (* Tentatively. *)

  ret := RegNode(ropBRANCH);
  chain := nil;
  while (FInput < FInputEol) and (FInput[0] <> '|')
       and (FInput[0] <> ')')
  do
    begin
    latest := RegPiece(flags);
    if latest = nil then
      Exit;
    FlagP := FlagP+(flags*[refHasWidth]);
    if chain = nil then
      (* First piece. *)
      FlagP := FlagP+(flags*[refSpStart])
    else
      RegTail(chain, latest);
    chain := latest;
    end;
  if chain = nil then
    (* Loop ran zero times. *)
    RegNode(ropNOTHING);

  RegBranch := ret;
  end { TRegExp.RegBranch };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegPiece
 *
 * PURPOSE:     This function parses something followed by
 *              possible '*', '+', '?' or '{n,m}' in both  maximal
 *              and minimal (followed by additional '?') variants.
 *
 * INPUTS:      FlagP - Buffer with flags.
 *
 * OUTPUTS:     FlagP - Buffer with flags.
 *
 * RETURNS:     nil  - If error.
 *              !nil - Address of RegExp byte code.
 *
 * NOTES:       The branching code sequences used for '?' and the
 *              general cases of '*' and '+' are somewhat optimized:
 *              they use the same NOTHING node as both the endmarker
 *              for their branch list and the body of the last branch.
 *              It might seem that this node could be dispensed with
 *              entirely, but the endmarker role is not redundant.
 *
 * HISTORY:
 *
 * Ver   Date       Description
 *
 * 0.11  2001.10.24 Added '{n,m}' syntax is based on REGEXPR.PAS
 *
 *****************************************************************)

function TRegExp.RegPiece(var FlagP: TRegExpFlags): PChar;
  label
    braces_error;
  var
    ret: PChar;
    op: Char;
    ngop: Boolean; { non-greedy operand }
    next: PChar;
    flags: TRegExpFlags;
    n: Integer;
    m: Integer;
    sinput: PChar;

  procedure EmitComplexBraces(AMin, AMax: Integer; ANonGreedy: Boolean);
    var
      rop: Char;
      r: PChar;
      off: Integer;
      next: PChar;
    begin
    rop := ropLOOP;
    if ANonGreedy then
      rop := ropLOOPNG;
    r := RegInsert(ropLOOPENTRY, ret, 0);
    next := RegNode(rop);
    off := next-r; { back to atom after LOOPENTRY }
    RegC(Chr((off shr 8) and 255));
    RegC(Chr(off and 255));
    RegC(Chr(AMin));
    RegC(Chr(AMax));
    RegTail(ret, next); { LOOPENTRY -> LOOP }
    if r <> nil then
      RegTail(r, next); { Atom -> LOOP }
    end;

  procedure EmitSimpleBraces(AMin, AMax: Integer; ANonGreedy: Boolean);
    var
      rop: Char;
      args: PChar;
    begin
    rop := ropBRACES;
    if ANonGreedy then
      rop := ropBRACESNG;
    args := RegInsert(rop, ret, 2);
    if args <> nil then
      begin
      ret[3] := Chr(AMin);
      ret[4] := Chr(AMax);
      end;
    end;

  begin { TRegExp.RegPiece }
  RegPiece := nil;
  ret := RegAtom(flags);
  if ret = nil then
    Exit;

  { remember the source               }
  { it will be restored on error exit }
  sinput := FInput;

  op := #0;
  if FInput < FInputEol then
    begin
    op := FInput[0];
    Inc(FInput);
    end;
  if not ((op = '*') or (op = '+') or (op = '?') or (op = '{')) then
    begin
    FInput := sinput;
    FlagP := flags;
    RegPiece := ret;
    Exit;
    end;

  if not (refHasWidth in flags) and (op <> '?') then
    begin
    FInput := sinput;
    FStatus := resStarPlusOperandCouldBeEmpty;
    Exit;
    end;

  (* parse {n,m} statement for further use          *)
  {  'while' is used instead of 'if' - do not change }
  {  the pseudo-if statement ends with 'Break'       }
  {  after that is error handling code               }
  while op = '{' do
    begin
    (* case {,m} *)
    if not RegLoadNumber(ParseNumInteger or 10, 5, n) then
      begin
      { assume the lowest limit }
      n := 0;
      { a comma must follow }
      if  (FInput >= FInputEol) or (FInput[0] <> ',') then
        goto braces_error;
      Inc(FInput);
      if not RegLoadNumber(ParseNumInteger or 10, 5, m) then
        goto braces_error;
      (* case {n} or {n,} or {n,m} *)
      end
    else
      begin
      (* assume simple case {n} *)
      m := n;
      { eat a followed comma }
      if  (FInput < FInputEol) and (FInput[0] = ',') then
        begin
        Inc(FInput);
        { assign maximum }
        if RegLoadNumber(ParseNumInteger or 10, 5, m) then
          begin
          if n > m then
            goto braces_error;
          end
        else
          begin
          m := -1; { temporary use for infinity }
          end;
        end;
      end;
    (* at this stage '}' must follow *)
    if  (FInput >= FInputEol) or (FInput[0] <> '}') or (m = 0)
         or (m > MaxBracesArg)
    then
      goto braces_error;
    Inc(FInput);
    { some optimisations }
    if m = -1 then
      begin
      if n = 0 then
        op := '*'
      else if n = 1 then
        op := '+'
      else
        m := MaxBracesArg; { convert infinity to a proper value }
      end;
    Break; { skip error - exit loop }
braces_error:
    FInput := sinput;
    FStatus := resInvalidBraces;
    Exit;
    end;

  ngop := False;
  if  (FInput < FInputEol) and (FInput[0] = '?') then
    begin
    ngop := True;
    Inc(FInput);
    end;

  case op of

    '*':
      begin
      FlagP := [refSpStart];
      if refSimple in flags then
        begin
        if ngop then
          RegInsert(ropSTARNG, ret, 0)
        else
          RegInsert(ropSTAR, ret, 0);
        end
      else
        begin
        if ngop then
          begin
          EmitComplexBraces(0, MaxBracesArg, True)
          end
        else
          begin
          (* Emit x* as (x&|), where & means "self". *)
          RegInsert(ropBRANCH, ret, 0); (* Either x *)
          RegOpTail(ret, RegNode(ropBACK)); (* and loop *)
          RegOpTail(ret, ret); (* back *)
          RegTail(ret, RegNode(ropBRANCH)); (* or *)
          RegTail(ret, RegNode(ropNOTHING)); (* null. *)
          end;
        end;
      end;

    '+':
      begin
      FlagP := [refSpStart, refHasWidth];
      if refSimple in flags then
        begin
        if ngop then
          RegInsert(ropPLUSNG, ret, 0)
        else
          RegInsert(ropPLUS, ret, 0);
        end
      else
        begin
        if ngop then
          begin
          EmitComplexBraces(1, MaxBracesArg, True)
          end
        else
          begin
          (* Emit x+ as x(&|), where & means "self". *)
          next := RegNode(ropBRANCH); (* Either *)
          RegTail(ret, next);
          RegTail(RegNode(ropBACK), ret); (* loop back *)
          RegTail(next, RegNode(ropBRANCH)); (* or *)
          RegTail(ret, RegNode(ropNOTHING)); (* null. *)
          end;
        end;
      end;

    '?':
      begin
      if ngop then
        begin
        if refSimple in flags then
          EmitSimpleBraces(0, 1, True)
        else
          EmitComplexBraces(0, 1, True);
        end
      else
        begin
        (* Emit x? as (x|) *)
        RegInsert(ropBRANCH, ret, 0); (* Either x *)
        RegTail(ret, RegNode(ropBRANCH)); (* or *)
        next := RegNode(ropNOTHING); (* null. *)
        RegTail(ret, next);
        RegOpTail(ret, next);
        end;
      end;

    '{':
      begin
      if n > 0 then
        FlagP := [];
      if m > 0 then
        FlagP := FlagP+[refSpStart, refHasWidth];
      if refSimple in flags then
        EmitSimpleBraces(n, m, ngop)
      else
        EmitComplexBraces(n, m, ngop);
      end;

  end {case};
  RegLeave;

  op := #0;
  if FInput < FInputEol then
    op := FInput[0];
  if  (op = '*') or (op = '+') or (op = '?') or (op = '{') then
    begin
    FStatus := resNestedStarQuotePlus;
    Exit;
    end;

  RegPiece := ret;
  end { TRegExp.RegPiece };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegAtom
 *
 * PURPOSE:     This function parses lowest level of regular
 *              expression.
 *
 * INPUTS:      FlagP - Buffer with flags.
 *
 * OUTPUTS:     FlagP - Buffer with flags.
 *
 * RETURNS:     nil  - If error.
 *              !nil - Address of RegExp byte code.
 *
 * NOTES:       Optimization:  gobbles an entire sequence of ordinary
 *              characters so that it can turn them into a single node,
 *              which is smaller to store and faster to run.
 *              Backslashed characters are exceptions, each becoming
 *              a separate node; the code is simpler that way and
 *              it's not worth fixing.
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegAtom(var FlagP: TRegExpFlags): PChar;
  label
    1, exactly;
  var
    ret: PChar;
    flags: TRegExpFlags;
    op: Char;
  begin
  RegAtom := nil; { initialize result }
  FlagP := []; (* Tentatively. *)
1: { restart '\:#' subexpresion }
  op := FInput[0];
  Inc(FInput);
  case op of

    '^':
      begin
      ret := RegNode(ropBOL);
      end;

    '$':
      begin
      ret := RegNode(ropEOL);
      end;

    '.':
      begin
      ret := RegNode(ropANY);
      FlagP := FlagP+[refHasWidth, refSimple];
      end;

    '[':
      begin
      ret := RegSet;
      if ret = nil then
        Exit;
      FlagP := FlagP+[refHasWidth, refSimple];
      end;

    '(':
      begin
      if  (FInput < FInputEol) and (FInput[0] = '?') then
        begin
        Inc(FInput);
        if FInput > FInputEol then
          begin
          FStatus := resInvalidTaggedExp;
          Exit;
          end;
        op := FInput[0];
        Inc(FInput);
        if op = ':' then
          begin
          ret := Reg(-2, flags)
          end
        else if op in ['1'..'9'] then
          begin
          ret := Reg(Ord(op)-Ord('0'), flags)
          end
        else
          begin
          FStatus := resInvalidTaggedExp;
          Exit;
          end;
        end
      else
        begin
        ret := Reg(-1, flags);
        end;
      if ret = nil then
        Exit;
      FlagP := FlagP+(flags*[refHasWidth, refSpStart]);
      end;

    '|', ')':
      begin
      FStatus := resInternalUrp; (* Supposed to be caught earlier. *)
      Exit;
      end;

    '?', '+', '*', '{':
      begin
      FStatus := resOperatorFollowsNothing;
      Exit;
      end;

    else { EXACTLY or PREDEFINED or TAGGED }
exactly:
        if  (op = '/') and (FInput < FInputEol) then // slash change by unxed
          begin
          op := FInput[0];
          Inc(FInput);
          { predefined }
          if op = ':' then
            begin
            if FInput >= FInputEol then
              begin
              FStatus := resInvalidPredefinedExp;
              Exit;
              end;
            op := FInput[0];
            Inc(FInput);
            if not RegEnter(op, False) then
              Exit;
            goto 1;
            end;
          { tagged }
          if op in ['1'..'9'] then
            begin
            if not RegEnter(op, True) then
              Exit;
            goto 1;
            end;
          { exactly }
          Dec(FInput);
          end;
    Dec(FInput);
    ret := RegExactly(FlagP);
    if ret = nil then
      Exit;
  end {case};
  RegLeave;

  RegAtom := ret;
  end { TRegExp.RegAtom };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegEnter
 *
 * PURPOSE:     This function saves current expression parsing
 *              state and switches to the new one.
 *
 * INPUTS:      Op     - The escape code of the predefined
 *                       sequence.
 *              Tagged - Use tagged expression 'Op'.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     False - Invalid escape code or stack overflow.
 *              True  - Successful.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegEnter(Op: Char; Tagged: Boolean): Boolean;
  {$IFNDEF REGENTER_ASM}
  const
    eAlpha = '[A-Za-z0-9]';
    eBlanks = '([ \t]+)';
    eAlphabetic = '[A-Za-z]';
    eDigit = '[0-9]';
    eFilename = '([^\[\]\:\\/<>|=+;, \t]+)';
    eHex = '([0-9A-Fa-f]+)';
    eInteger = '([0-9]+)';
    eFloat = '(([0-9]+(\.[0-9]+|)|\.[0-9]+)([Ee](\+|-|)[0-9]+|))';
    ePath = '(([A-Za-z]:|)(\\|/|)(\:f(\\|/|))*\:f)';
    eQuotedStr = '(\"[^\"]*\")|''[^'']*'')';
    eCVariable = '([A-Za-z_$][A-Za-z0-9_$]*)';
    eWord = '([A-Za-z]+)';
    {$ENDIF !REGENTER_ASM}
  var
    stack: PRegExpStack absolute FMust;
    p: PChar;
    l: Integer;
  begin
  RegEnter := False;
  p := nil;
  l := 0;
  if not Tagged then
    begin
    {$IFDEF REGENTER_ASM}
    { This peace of code is provided only to move constant strings from DATA }
    { segment (there is a limit to 64KB for that) to CODE segment (there is  }
    { also limit to 64KB but there can be more than one such a segment).     }
    asm
                MOV     AL,Op
                MOV     DX,OFFSET @@A
                CMP     AL,'a'
                JE      @@1
                MOV     DX,OFFSET @@B
                CMP     AL,'b'
                JE      @@1
                MOV     DX,OFFSET @@C
                CMP     AL,'c'
                JE      @@1
                MOV     DX,OFFSET @@D
                CMP     AL,'d'
                JE      @@1
                MOV     DX,OFFSET @@F
                CMP     AL,'f'
                JE      @@1
                MOV     DX,OFFSET @@H
                CMP     AL,'h'
                JE      @@1
                MOV     DX,OFFSET @@I
                CMP     AL,'i'
                JE      @@1
                MOV     DX,OFFSET @@N
                CMP     AL,'n'
                JE      @@1
                MOV     DX,OFFSET @@P
                CMP     AL,'p'
                JE      @@1
                MOV     DX,OFFSET @@Q
                CMP     AL,'q'
                JE      @@1
                MOV     DX,OFFSET @@V
                CMP     AL,'v'
                JE      @@1
                MOV     DX,OFFSET @@W
                CMP     AL,'w'
                JNE     @@2
        @@1:
                MOV     AX,CS
                MOV     WORD PTR p[0],DX
                MOV     WORD PTR p[2],CS
                JMP     @@2
        @@A:    DB      '[A-Za-z0-9]',0
        @@B:    DB      '([ \t]+)',0
        @@C:    DB      '[A-Za-z]',0
        @@D:    DB      '[0-9]',0
        @@F:    DB      '([^\[\]\:\\/<>|=+;, \t]+)',0
        @@H:    DB      '([0-9A-Fa-f]+)',0
        @@I:    DB      '([0-9]+)',0
        @@N:    DB      '(([0-9]+(\.[0-9]+|)|\.[0-9]+)([Ee](\+|-|)[0-9]+|))',0
        @@P:    DB      '(([A-Za-z]:|)(\\|/|)(\:f(\\|/|))*\:f)',0
        @@Q:    DB      '(\"[^\"]*\")|''[^'']*'')',0
        @@V:    DB      '([A-Za-z_$][A-Za-z0-9_$]*)',0
        @@W:    DB      '([A-Za-z]+)',0
        @@2:
    end;
  {$ELSE !REGENTER_ASM}
    case Op of
      'a': p := eAlpha;
      'b': p := eBlanks;
      'c': p := eAlphabetic;
      'd': p := eDigit;
      'f': p := eFilename;
      'h': p := eHex;
      'i': p := eInteger;
      'n': p := eFloat;
      'p': p := ePath;
      'q': p := eQuotedStr;
      'v': p := eCVariable;
      'w': p := eWord;
    end;
  {$ENDIF !REGENTER_ASM}
    if p = nil then begin
      Escape ( Op, p, l );
      if (p = nil) or (l < 0) then begin
        FStatus := resInvalidPredefinedExp;
        Exit;
      end;
      if l = 0 then begin
        FStatus := resUndefinedPredefinedExp;
        Exit;
      end;
    end;
    if l = 0 then
      l := StrLen ( p );
  end else begin
    l := Ord(op) - Ord('0');
    if (l < 1) or (l > 9) then begin
      FStatus := resInvalidTaggedExp;
      Exit;
    end;
    if (stack^.TagStarts[l] = nil)          { check if the start  }
    or (stack^.TagEnds[l] = nil)            { and the end}
    or (FStartP[l] <> FEndP[l])
    then
      begin { have the same entry }
      FStatus := resInvalidTaggedExp;
      Exit;
      end;
  p := stack^.TagStarts[l];
  l := stack^.TagEnds[l]-p;
  end { TRegExp.RegEnter };
with Stack^ do
  begin
  if Level >= MaxRegExpStack then
    begin
    FStatus := resStackOverflow;
    Exit;
    end;
  Stack[Level].Entry := Entry;
  Stack[Level].Cur := FInput;
  Stack[Level].bol := FInputBol;
  Stack[Level].Eol := FInputEol;
  Inc(Level);
  Inc(Entries);
  end;
FInput := P;
FInputBol := P;
FInputEol := P+l;
RegEnter := True;
end;

(*****************************************************************
 *
 * PROCEDURE:   TRegExp.RegLeave
 *
 * PURPOSE:     This function exits from nested expression entered
 *              previously via RegEnter.
 *
 * INPUTS:      None.
 *
 * OUTPUTS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.RegLeave;
  var
    stack: PRegExpStack absolute FMust;
  begin
  with stack^ do
    begin
    while (Level > 0) and (FInput = FInputEol) do
      begin
      Dec(Level);
      Entry := stack[Level].Entry;
      FInput := stack[Level].Cur;
      FInputBol := stack[Level].bol;
      FInputEol := stack[Level].Eol;
      end;
    end;
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegLoadNumber
 *
 * PURPOSE:     This function parses a number of any base between
 *              2 and 16 and treats it as an character opcode.
 *
 * INPUTS:      Base    - A number between 2 and 16 + Flags.
 *              Digits  - Maximum number of digits.
 *              AResult - Buffer for result.
 *
 * OUTPUTS:     AResult - Buffer with result.
 *
 * RETURNS:     False - Error.
 *              True  - Successful.
 *
 * NOTES:       Flags:
 *
 *              ParseNumInteger - Return Integer instead of Char
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegLoadNumber(Base: Integer; Digits: Integer;
     var AResult): Boolean;
  var
    v: Integer;
    i: Integer;
    j: Integer;
    h: Integer;
  begin
  RegLoadNumber := False;
  v := 0;
  i := 0;
  h := Ord(High(Char));
  if  (Base and ParseNumInteger) <> 0 then
    h := High(Integer);
  repeat
    if FInput >= FInputEol then
      Exit;
    j := Pos(UpCase(FInput[0]), HexStr);
    if  (j = 0) or (j > (Base and $FF)) then
      Break;
    v := v*(Base and ParseNumBaseMask)+j-1;
    Inc(FInput);
    Inc(i);
  until i >= Digits;
  if  (i = 0) or (v > h) or (v < 0) then
    begin
    FStatus := resInvalidEscape;
    Exit;
    end;
  if  (Base and ParseNumInteger) = 0 then
    Char(AResult) := Chr(v)
  else
    Integer(AResult) := v;
  RegLoadNumber := True;
  end { TRegExp.RegLoadNumber };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegEscape
 *
 * PURPOSE:     This function parses an escaped character.
 *
 * INPUTS:      AResult - Buffer for result.
 *
 * OUTPUTS:     AResult - Buffer with result.
 *
 * RETURNS:     False - Error.
 *              True  - Successful.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegEscape(var AResult: Char): Boolean;
  var
    c: Char;
  begin
  RegEscape := False;
  if FInput >= FInputEol then
    begin
    FStatus := resTrailingBackSlash;
    Exit;
    end;
  c := FInput[0];
  Inc(FInput);
  case c of
    'a':
      AResult := #7; { alarm/bell      }
    'b':
      AResult := #8; { backspace       }
    't':
      AResult := #9; { tabulator       }
    'n':
      AResult := #10; { line feed       }
    'v':
      AResult := #11; { vertical tab    }
    'f':
      AResult := #12; { form feed       }
    'r':
      AResult := #13; { carriage return }
    'x':
      if not RegLoadNumber(16, 2, AResult) then
        Exit;
    'd':
      if not RegLoadNumber(10, 3, AResult) then
        Exit;
    else {case}
      AResult := c;
  end {case};
  RegEscape := True;
  end { TRegExp.RegEscape };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegSet
 *
 * PURPOSE:     This function parses a set expression.
 *
 * INPUTS:      None.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     non-nil - The new node.
 *              nil     - Error.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegSet: PChar;
  var
    ret: PChar;
    len: Integer;
    op: Char;
    opset: Char;
    charset: set of Char;

  function ParseSet: Boolean;
    var
      char1: Char;
      char2: Char;
      c: Char;
    begin
    ParseSet := False;
    if  (FInput < FInputEol) and (FInput[0] = '^') then
      begin (* Complement of range. *)
      opset := ropANYBUT;
      Inc(FInput);
      end
    else
      begin
      opset := ropANYOF;
      end;
    Charset := [];
    { Exception for '[]]' }
    if  (FInput+1 < FInputEol) and (FInput[0] = ']')
         and (FInput[1] = ']')
    then
      begin
      ParseSet := True;
      Inc(FInput, 2);
      Include(Charset, ']');
      Exit;
      end;
    { standard parsing }
    while (FInput < FInputEol) and (FInput[0] <> ']') do
      begin
      char1 := FInput[0];
      Inc(FInput);
      if  (char1 = '/') and not RegEscape(char1) then // slash change by unxed
        Exit;
      if  (FInput < FInputEol) and (FInput[0] = '-') then
        begin
        Inc(FInput);
        if  (FInput >= FInputEol) or (FInput[0] = ']') then
          begin
          FStatus := resInvalidSetRange;
          Exit;
          end;
        char2 := FInput[0];
        Inc(FInput);
        if  (char2 = '/') and not RegEscape(char2) then // slash change by unxed
          Exit;
        end
      else
        begin
        char2 := char1;
        end;
      if char1 > char2 then
        begin
        FStatus := resInvalidSetRange;
        Exit;
        end;
      for c := char1 to char2 do
        Include(Charset, c);
      end;
    if  (FInput >= FInputEol) or (FInput[0] <> ']') then
      begin
      FStatus := resUnmatchedSquareBracket;
      Exit;
      end;
    Inc(FInput);
    { Exception for '[^]' }
    if  (Charset = []) and (opset = ropANYBUT) then
      begin
      opset := ropANYOF;
      Include(Charset, '^');
      end;
    ParseSet := True;
    end { ParseSet: };

  begin { TRegExp.RegSet: }
  RegSet := nil;
  if not ParseSet then
    Exit;
  len := 0;
  for op := Low(Char) to High(Char) do
    begin
    if op in charset then
      Inc(len);
    end;
  if  ( (len = 0) and (opset = ropANYOF))
    or ((len = 256) and (opset = ropANYBUT))
  then
    begin
    FStatus := resInvalidSetRange;
    Exit;
    end;
  if  ( (len = 256) and (opset = ropANYOF))
    or ((len = 0) and (opset = ropANYBUT))
  then
    begin
    ret := RegNode(ropANY);
    end
  else
    begin
    ret := RegNode(opset);
    RegC(Chr(len));
    for op := Low(Char) to High(Char) do
      begin
      if op in charset then
        RegC(op);
      end;
    end;
  RegSet := ret;
  end { TRegExp.RegSet: };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegExactly
 *
 * PURPOSE:     This function parses a directly defined string.
 *
 * INPUTS:      FlagP - Buffer with flags.
 *
 * OUTPUTS:     FlagP - Buffer with flags.
 *
 * RETURNS:     non-nil - The new node.
 *              nil     - Error.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegExactly(var FlagP: TRegExpFlags): PChar;
  var
    ret: PChar;
    len: Integer;
    exactly: String;

  function ParseExactly: Boolean;
    var
      c: Char;
      b0: PChar;
      b1: PChar;
    begin
    ParseExactly := False;
    exactly := '';
    b1 := nil;
    repeat
      b0 := b1;
      b1 := FInput;
      if b1 >= FInputEol then
        Break;
      c := b1[0];
      if  (c = '/') then // slash change by unxed
        begin
        if  (b1+1 < FInputEol) and (b1[1] = ':') then
          Break;
        Inc(FInput);
        if not RegEscape(c) then
          Exit;
        end
      else
        begin
        { METACHARS }
        if c in ['^', '$', '.', '[', '(', ')', '|', '?', '+', '*', '{']
        then
          begin
          if  (Length(exactly) > 1) and (c in ['*', '+', '?', '{']) then
            begin
            (* Back off clear of ?+* operand. *)
            Delete(exactly, Length(exactly), 1);
            if b0 <> nil then
              FInput := b0;
            end;
          Break;
          end;
        Inc(FInput);
        end;
      exactly := exactly+c;
    until Length(exactly) = Ord(High(Char));
    ParseExactly := True;
    end { ParseExactly: };

  begin { TRegExp.RegExactly }
  RegExactly := nil;
  if not ParseExactly then
    Exit;
  Include(FlagP, refHasWidth);
  if Length(exactly) = 1 then
    Include(FlagP, refSimple);
  ret := RegNode(ropEXACTLY);
  for len := 0 to Length(exactly) do
    RegC(exactly[len]);
  RegExactly := ret;
  end { TRegExp.RegExactly };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegNode
 *
 * PURPOSE:     This function emits a node.
 *
 * INPUTS:      Op - Operand (byte code).
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     An address of a node in a byte code.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegNode(Op: Char): PChar;
  begin
  RegNode := FCodeData;
  if ffParsing in FFlags then
    begin
    IncRegSize(3);
    Exit;
    end;
  FCodeData[0] := Op;
  FCodeData[1] := #0; (* Null "next" pointer. *)
  FCodeData[2] := #0;
  Inc(FCodeData, 3);
  end;

(*****************************************************************
 *
 * PROCEDURE:   TRegExp.RegC
 *
 * PURPOSE:     This procedure emits (if appropriate a byte of
 *              code.
 *
 * INPUTS:      B - A byte of code (argument of operand).
 *
 * OUTPUTS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.RegC(B: Char);
  begin
  if not (ffParsing in FFlags) then
    begin
    FCodeData[0] := B;
    Inc(FCodeData);
    end
  else
    begin
    IncRegSize(1);
    end;
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegInsert
 *
 * PURPOSE:     This procedure inserts an operator in front of
 *              already-emitted operand. Means relocating the
 *              operand.
 *
 * INPUTS:      Op    - An operator.
 *              Opnd  - Destination address.
 *              ASize - Size of auxiliary data.
 *
 * OUTPUTS:     None.
 *
 * RESULT:      Address of auxiliary buffer (ASize bytes)
 *              or next operand (if ASize = 0).
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegInsert(Op: Char; Opnd: PChar; ASize: Integer): PChar;
  begin
  RegInsert := nil;

  if ffParsing in FFlags then
    begin
    IncRegSize(3+ASize);
    Exit;
    end;

  Move(Opnd[0], Opnd[3+ASize], FCodeData-Opnd);
  Inc(FCodeData, 3+ASize);

  FillChar(Opnd[0], 3+ASize, 0);
  Opnd[0] := Op; (* Op node, where operand used to be. *)
  RegInsert := Opnd+3; { Reserved data or next operand }
  end;

(*****************************************************************
 *
 * PROCEDURE:   TRegExp.RegTail
 *
 * PURPOSE:     This procedure sets the next-pointer at the end
 *              of a node chain.
 *
 * INPUTS:      P   - The current node.
 *              Val - The target node.
 *
 * OUTPUTS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.RegTail(P: PChar; Val: PChar);
  var
    scan: PChar;
    temp: PChar;
    offset: Integer;
  begin
  if ffParsing in FFlags then
    Exit;

  (* Find last node. *)
  scan := P;
  while True do
    begin
    temp := RegNext(scan);
    if temp = nil then
      Break;
    scan := temp;
    end;

  if scan[0] = ropBACK then
    offset := scan-Val
  else
    offset := Val-scan;
  scan[1] := Chr((offset shr 8) and 255);
  scan[2] := Chr(offset and 255);
  end { TRegExp.RegTail };

(*****************************************************************
 *
 * PROCEDURE:   TRegExp.RegOpTail
 *
 * PURPOSE:     This procedure perform RegTail on operand of first
 *              argument; nop if operandless.
 *
 * INPUTS:      P   - The current node.
 *              Val - The target node.
 *
 * OUTPUTS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.RegOpTail(P: PChar; Val: PChar);
  begin
  (* "Operandless" and "op != BRANCH" are synonymous in practice. *)
  if  (ffParsing in FFlags) or (P = nil) or (P[0] <> ropBRANCH) then
    Exit;
  RegTail(@P[3], Val);
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegExecute
 *
 * PURPOSE:     This function match a regexp against a string.
 *
 * INPUTS:      AString - A source string.
 *              ALength - A string length.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     True  - The given string match.
 *              False - The given string does not match.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegExecute(AString: PChar; ALength: Integer): Boolean;
  var
    s: PChar;
    t: PChar;
  begin
  RegExecute := False;
  FStatus := resOK;
  Exclude(FFlags, ffBreak);

  { cannot execute if there was an error }
  if not (ffCompiled in FFlags) then
    begin
    FStatus := resNoExpression;
    Exit;
    end;

  (* Be paranoid... *)
  if AString = nil then
    begin
    FStatus := resNilArgument;
    Exit;
    end;
  if ALength = -1 then
    ALength := StrLen(AString);

  (* Check validity of program. *)
  if  (FCodeData[0] <> magic) then
    begin
    FStatus := resCorruptedProgram;
    Exit;
    end;

  (* If there is a "must appear" string, look for it. *)
  if  (FMust <> nil) and (ALength >= Length(FMust^)) then
    begin
    s := AString;
    t := AString+ALength-Length(FMust^);
    repeat
      s := StrScan2(s, FMust^[1], t);
      if s = nil then
        Exit; (* Not present. *)
      if StrLComp(s, @FMust^[1], Length(FMust^)) = 0 then
        Break; (* Found it. *)
      Inc(s);
    until False;
    end;

  (* Remember the source *)
  FInputBol := AString;
  FInputEol := AString+ALength;

  (* Simplest case:  anchored match need be tried only once. *)
  if ffAnchored in FFlags then
    begin
    RegExecute := RegTry(AString);
    Exit;
    end;

  (* Messy cases:  unanchored match. *)
  s := AString;
  t := AString+ALength;
  if  (ffStart in FFlags) then
    begin
    (* We know what char it must start with. *)
    repeat
      s := StrScan2(s, FStartCh, t);
      if s = nil then
        Exit; (* Not present. *)
      if RegTry(s) then
        begin
        RegExecute := True;
        Exit;
        end;
      Inc(s);
    until False;
    end
  else
    begin
    (* We don't -- general case. *)
    repeat
      if RegTry(s) then
        begin
        RegExecute := True;
        Exit;
        end;
      { Check the limit }
      if s >= t then
        Exit;
      Inc(s);
    until False;
    end;
  (* Failure. *)
  end { TRegExp.RegExecute };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegTry
 *
 * PURPOSE:     This function tries match at specific point.
 *
 * INPUTS:      AString - The string to match.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     True  - Success.
 *              False - Failure.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegTry(AString: PChar): Boolean;
  var
    i: Integer;
  begin
  for i := 1 to 9 do
    begin
    FStartP[i] := -1;
    FEndP[i] := -1;
    end;
  FInput := AString;
  FLStackId := 0;
  if RegMatch(@FCodeData[1]) then
    begin
    FStart := AString-FInputBol;
    FLength := FInput-AString;
    RegTry := True;
    Exit;
    end;
  RegTry := False;
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegMatch
 *
 * PURPOSE:     This function is a main matching routine.
 *
 * INPUTS:      Prog - Program (byte code) pointer.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     None.
 *
 * NOTES:       Conceptually the strategy is simple:  check to see
 *              whether the current node matches, call self
 *              recursively to see whether the rest matches,
 *              and then act accordingly.  In practice we make
 *              some effort to avoid recursion, in particular by
 *              going through "ordinary" nodes (that don't need to
 *              know whether the rest of the match failed) by a
 *              loop instead of by recursion.
 *
 * HISTORY:
 *
 * Ver   Date       Description
 *
 * 0.11  2001.10.24 Added '{n,m}' syntax is based on REGEXPR.PAS
 *
 *****************************************************************)

function TRegExp.RegMatch(Prog: PChar): Boolean;
  var
    scan: PChar; (* Current node. *)
    next: PChar; (* Next node. *)
    no: Integer; { EXACTLY, STAR, PLUS }
    save: PChar; { EXACTLY, BRANCH, STAR, PLUS }
    min: Integer; { STAR, PLUS }
    max: Integer;
    nextch: Char; { STAR, PLUS }
    opnd: PChar;

  function BracesMatch: Boolean;
    var
      sloops: array[1..LoopStackMax] of Integer;
      { :(( very bad for recursion }
      sloopi: Integer;
    begin
    BracesMatch := False;
    if  (ffMatchNext in FFlags) or (FInput[0] = nextch) then
      begin
      Move(FLStack, sloops, SizeOf(FLStack));
      sloopi := FLStackId;
      if RegMatch(Next) then
        begin
        RegMatch := True;
        BracesMatch := True;
        Exit;
        end;
      Move(sloops, FLStack, SizeOf(FLStack));
      FLStackId := sloopi;
      if ffBreak in FFlags then
        BracesMatch := True;
      end;
    end { BracesMatch: };

  function LoopMatch: Boolean;
    begin
    Inc(FLStack[FLStackId]);
    LoopMatch := False;
    No := FLStackId;
    if RegMatch(Opnd) then
      begin
      RegMatch := True;
      LoopMatch := True;
      end;
    FLStackId := No;
    end;

  begin { TRegExp.RegMatch }
  RegMatch := False;
  scan := Prog;
  {$IFDEF DEBUG}
  if  (scan <> nil) and Narrate then
    Writeln(RegProp(scan), '(');
  {$ENDIF DEBUG}
  while scan <> nil do
    begin
    {$IFDEF DEBUG}
    if Narrate then
      Writeln(RegProp(scan), '...');
    {$ENDIF DEBUG}
    if ffBreak in FFlags then
      Exit;
    if CheckBreak then
      begin
      FStatus := resCanceled;
      Include(FFlags, ffBreak);
      Exit;
      end;
    next := RegNext(scan);

    case scan[0] of

      ropBOL:
        begin
        if FInput <> FInputBol then
          Exit;
        end;

      ropEOL:
        begin { should be checked for new-line }
        if FInput <> FInputEol then
          Exit;
        end;

      ropANY:
        begin { should be any except new-line }
        if FInput = FInputEol then
          Exit;
        Inc(FInput);
        end;

      ropEXACTLY:
        begin
        save := @scan[4];
        (* Inline the first character, for speed. *)
        if save[0] <> FInput[0] then
          Exit;
        no := Ord(scan[3]);
        { input is too short }
        if FInput+no > FInputEol then
          Exit;
        if  (no > 1) and (StrLComp(save, FInput, no) <> 0) then
          Exit;
        Inc(FInput, no);
        end;

      ropANYOF:
        begin
        if FInput = FInputEol then
          Exit;
        no := Ord(scan[3]);
        if StrScan2(@scan[4], FInput[0], @scan[4+no]) = nil then
          Exit;
        Inc(FInput);
        end;

      ropANYBUT:
        begin
        if FInput = FInputEol then
          Exit;
        no := Ord(scan[3]);
        if StrScan2(@scan[4], FInput[0], @scan[4+no]) <> nil then
          Exit;
        Inc(FInput);
        end;

      ropNOTHING:
        begin
        end;

      ropBACK:
        begin
        end;

      ropOPEN:
        begin
        if RegMatch(next) then
          RegMatch := True;
        Exit;
        end;

      Succ(ropOPEN0)..ropOPEN9:
        begin
        no := Ord(scan[0])-Ord(ropOPEN0);
        save := FInput;
        if RegMatch(next) then
          begin
          RegMatch := True;
          { Don't set startp if some later invocation of the same }
          { parentheses already has.                              }
          if FStartP[no] = -1 then
            FStartP[no] := save-FInputBol;
          end;
        Exit;
        end;

      ropCLOSE:
        begin
        if RegMatch(next) then
          RegMatch := True;
        Exit;
        end;

      Succ(ropCLOSE0)..ropCLOSE9:
        begin
        no := Ord(scan[0])-Ord(ropCLOSE0);
        save := FInput;
        if RegMatch(next) then
          begin
          RegMatch := True;
          { Don't set startp if some later invocation of the same }
          { parentheses already has.                              }
          if FEndP[no] = -1 then
            FEndP[no] := save-FInputBol;
          end;
        Exit;
        end;

      ropBRANCH:
        begin
        if next[0] <> ropBRANCH then
          begin (* No choice. *)
          next := @scan[3]; (* Avoid recursion. *)
          end
        else
          begin
          repeat
            save := FInput;
            if RegMatch(@scan[3]) then
              begin
              RegMatch := True;
              Exit;
              end;
            FInput := save;
            scan := RegNext(scan);
          until (ffBreak in FFlags) or (scan = nil)
             or (scan[0] <> ropBRANCH);
          Exit;
          (* NOTREACHED *)
          end;
        end;

      ropLOOPENTRY:
        begin
        no := FLStackId;
        Inc(FLStackId);
        if FLStackId > LoopStackMax then
          begin
          FStatus := resLoopStackExceeded;
          Exit;
          end;
        save := FInput;
        FLStack[FLStackId] := 0; { init loop counter }
        if not RegMatch(next) then
          { execute loop      }
          FInput := save;
        FLStackId := no; { cleanup           }
        Exit;
        end;

      ropLOOP, ropLOOPNG:
        begin
        if FLStackId <= 0 then
          begin
          FStatus := resLoopWithoutEntry;
          Exit;
          end;
        opnd := scan-(Ord(scan[3])*256+Ord(scan[4])); { back atom }
        min := Ord(scan[5]);
        max := Ord(scan[6]);
        save := FInput;
        if FLStack[FLStackId] >= min then
          begin
          { Min alredy matched - we can work }
          if scan[0] = ropLOOP then
            begin
            { greedy way - first try to max deep of greed ;) }
            if FLStack[FLStackId] < max then
              begin
              if LoopMatch then
                Exit;
              FInput := save;
              end;
            { Fail. May be we are too greedy? ;) }
            Dec(FLStackId);
            if RegMatch(next) then
              begin
              RegMatch := True;
              end
            else
              begin
              FInput := save;
              end;
            Exit;
            end
          else
            begin
            { non-greedy - try just now }
            if RegMatch(next) then
              Exit;
            { failed - move next and try again }
            FInput := save;
            if FLStack[FLStackId] < max then
              begin
              if LoopMatch then
                Exit;
              FInput := save;
              end;
            { Failed - back up }
            Dec(FLStackId);
            Exit;
            end;
          end
        else
          begin
          { first match a min_cnt times }
          if LoopMatch then
            Exit;
          Dec(FLStack[FLStackId]);
          FInput := save;
          Exit;
          end;
        end;

      ropSTAR, ropPLUS, ropBRACES, ropSTARNG, ropPLUSNG, ropBRACESNG:
        begin
        (*
         * Lookahead to avoid useless match attempts
         * when we know what character comes next.
         *)
        Include(FFlags, ffMatchNext);
        if next[0] = ropEXACTLY then
          begin
          nextch := next[4];
          Exclude(FFlags, ffMatchNext);
          end;
        max := MaxInt; { infinite loop for * and + }
        if  (scan[0] = ropSTAR) or (scan[0] = ropSTARNG) then
          min := 0 { STAR }
        else if (scan[0] = ropPLUS) or (scan[0] = ropPLUSNG) then
          min := 1 { PLUS }
        else
          begin { BRACES }
          min := Ord(scan[3]);
          max := Ord(scan[4]);
          end;
        save := FInput;
        opnd := @scan[3];
        if  (scan[0] = ropBRACES) or (scan[0] = ropBRACESNG) then
          Inc(opnd, 2);
        if  (scan[0] = ropPLUSNG) or (scan[0] = ropSTARNG)
             or (scan[0] = ropBRACESNG)
        then
          begin
          { non-greedy mode }
          max := RegRepeat(opnd, max); { don't repeat more than Max }
          { Now we know real Max limit to move forward (for recursion 'back up') }
          { In some cases it can be faster to check only Min positions first,    }
          { but after that we have to check every position separtely instead     }
          { of fast scannig in loop.                                             }
          no := min;
          while no <= min do
            begin
            FInput := save+no;
            { If it could work, try it. }
            if BracesMatch then
              Exit;
            { Couldn't or didn't - move forward. }
            Inc(no);
            end;
          end
        else
          begin
          no := RegRepeat(opnd, max); { don't repeat more than Max }
          while no >= min do
            begin
            { If it could work, try it. }
            if BracesMatch then
              Exit;
            (* Couldn't or didn't -- back up. *)
            Dec(no);
            FInput := save+no;
            end;
          end;
        Exit;
        end;

      ropEND:
        begin
        RegMatch := True; (* Success! *)
        Exit;
        end;

      else {case}
        FStatus := resMemoryCorruption;
      Exit;
    end {case};

    scan := next;
    end;

  (*
   * We get here only if there's trouble -- normally "case END" is
   * the terminating point.
   *)
  FStatus := resCorruptedPointers;
  end { TRegExp.RegMatch };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegRepeat
 *
 * PURPOSE:     This function repeatedly matches something simple
 *              and reports how many.
 *
 * INPUTS:      P    - Program (byte code) pointer.
 *              AMax - Maximum number or repetition.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     Number of repetitions.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegRepeat(P: PChar; AMax: Integer): Integer;
  var
    count: Integer;
    scan: PChar;
    opnd: PChar;
    opnde: PChar;
    max: Integer;
  begin
  count := 0;
  scan := FInput;
  if P[0] <> ropANY then
    begin
    opnd := @P[4];
    opnde := opnd+Ord(P[3]);
    end;
  max := FInputEol-scan;
  if max > AMax then
    max := AMax;
  case P[0] of

    ropANY:
      begin
      count := max;
      Inc(scan, max);
      end;

    ropEXACTLY:
      begin
      while (count < max) and (opnd[0] = scan[0]) do
        begin
        Inc(count);
        Inc(scan);
        end;
      end;

    ropANYOF:
      begin
      while (count < max) and (StrScan2(opnd, scan[0], opnde) <> nil) do
        begin
        Inc(count);
        Inc(scan);
        end;
      end;

    ropANYBUT:
      begin
      while (count < max) and (StrScan2(opnd, scan[0], opnde) = nil) do
        begin
        Inc(count);
        Inc(scan);
        end;
      end;

    else (* Oh dear.  Called inappropriately. *)
      FStatus := resInternalFoulup;
    count := 0; (* Best compromise. *)
  end {case};
  FInput := scan;

  RegRepeat := count;
  end { TRegExp.RegRepeat };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegNext
 *
 * PURPOSE:     This function digs the "next" pointer out of a
 *              node.
 *
 * INPUTS:      P - Program (byte code) pointer.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegNext(P: PChar): PChar;
  var
    offset: Integer;
  begin
  RegNext := nil; { initialize result }

  if ffParsing in FFlags then
    Exit;

  offset := (((Ord(P[1]) and 255) shl 8)+(Ord(P[2]) and 255));
  if offset = 0 then
    Exit;

  if P[0] = ropBACK then
    RegNext := P-offset
  else
    RegNext := P+offset;
  end;

(*****************************************************************
 *
 * PROCEDURE:   TRegExp.RegClearTags
 *
 * PURPOSE:     This procedure sets all tags to 'uninitialized'.
 *
 * INPUTS:      None.
 *
 * OUTPUTS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.RegClearTags;
  var
    i: Integer;
    stack: PRegExpStack absolute FMust;
  begin
  for i := 1 to 9 do
    with stack^ do
      begin
      TagStarts[i] := nil;
      TagEnds[i] := nil;
      end;
  end;

{$IFDEF DEBUG}

(*****************************************************************
 *
 * PROCEDURE:   Dump
 *
 * PURPOSE:     This procedure dumps a regular expression byte
 *              code onto standard output in vaguely comprehensible
 *              form.
 *
 * INPUTS:      None.
 *
 * OUTPUTS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.Dump;
  var
    s: PChar;
    op: Char;
    next: PChar;
    off: Integer;
  begin
  if FCodeSize = 0 then
    Exit;
  op := ropEXACTLY; (* Arbitrary non-END op. *)

  s := @FCodeData[1];
  while op <> ropEND do
    begin
    (* While that wasn't END last time... *)
    op := s[0];
    off := s-FCodeData;
    Write(s-FCodeData: 4, RegProp(s)); (* Where, what. *)
    next := RegNext(s);
    if next = nil then
      (* Next ptr. *)
      Write('(0)')
    else
      Write('(', (s-FCodeData)+(next-s), ')');
    Inc(s, 3);
    if  (op = ropANYOF) or (op = ropANYBUT) or (op = ropEXACTLY) then
      begin
      (* Literal string, where present. *)
      Write(PString(s)^);
      Inc(s, Length(PString(s)^)+1);
      end
    else if (op = ropLOOP) or (op = ropLOOPNG) then
      begin
      Write('[', off-(Ord(s[0])*256+Ord(s[1])), ']{', Ord(s[2]), ',',
           Ord(s[3]), '}');
      Inc(s, 4);
      end
    else if (op = ropBRACES) or (op = ropBRACESNG) then
      begin
      Write('{', Ord(s[0]), ',', Ord(s[1]), '}');
      Inc(s, 2);
      end;
    Writeln;
    end;

  (* Header fields of interest. *)
  if ffStart in FFlags then
    Write('start `', FStartCh, ''' ');
  if ffAnchored in FFlags then
    Write('anchored ');
  if FMust <> nil then
    Write('must have "', FMust^, '"');
  Writeln;
  end { TRegExp.Dump };

{$ENDIF DEBUG}

function TRegExp.SubstituteString(ASrc: PChar; const AReplace: String;
     var ADest: String): Boolean;
  var
    len: Integer;
    ret: Boolean;
  begin
  SubstituteString := False;
  len := High(ADest);
  if Substitute(ASrc, @AReplace[1], Length(AReplace), @ADest[1], len)
  then
    begin
    ADest[0] := Chr(len);
    SubstituteString := True;
    end;
  end;

function TRegExp.SubstituteStr(ASrc, AReplace: PChar; ADest: PChar;
     var ALength: Integer): Boolean;
  begin
  SubstituteStr := Substitute(ASrc, AReplace, -1, ADest, ALength);
  end;

function TRegExp.Substitute(ASrc, AReplace: PChar; ARLen: Integer;
     ADest: PChar; var ADLen: Integer): Boolean;
  begin
  Substitute := RegSub(ASrc, AReplace, ARLen, ADest, ADLen);
  if FStatus <> resOK then
    Error(FStatus);
  end;

function TRegExp.RegSub(ASrc, AReplace: PChar; ARLen: Integer;
     ADest: PChar; var ADLen: Integer): Boolean;
  var
    j: Integer;
    l: Integer;
    c: Char;
    no: Integer;
    len: Integer;
    start: Integer;
  begin
  RegSub := False;
  FStatus := resOK;

  { cannot execute if there was an error }
  if not (ffCompiled in FFlags) then
    begin
    FStatus := resNoExpression;
    Exit;
    end;

  (* Be paranoid... *)
  if AReplace = nil then
    begin
    FStatus := resNilArgument;
    Exit;
    end;
  if ARLen = -1 then
    ARLen := StrLen(AReplace);

  { destination buffer must have valid size }
  if  (ADest <> nil) and (ADLen <= 0) then
    begin
    FStatus := resInvalidArgument;
    Exit;
    end;

  (* Check validity of program. *)
  if  (FCodeData[0] <> magic) then
    begin
    FStatus := resCorruptedProgram;
    Exit;
    end;

  FInput := AReplace;
  FInputEol := AReplace+ARLen;

  l := ADLen;
  j := 0;
  while FInput < FInputEol do
    begin
    c := FInput[0];
    Inc(FInput);
    no := -1;
    if c = '&' then
      begin
      no := 0;
      end
    else if (c = '/') then // slash change by unxed
      begin
      if  (FInput < FInputEol) and (FInput[0] in ['0'..'9']) then
        begin
        no := Ord(FInput[0])-Ord('0');
        Inc(FInput);
        end
      else
        begin
        if not RegEscape(c) then
          Exit;
        end;
      end;
    if no < 0 then
      begin
      { ordinary character }
      if ADest <> nil then
        begin
        if j < l then
          ADest[j] := c;
        end;
      Inc(j);
      end
    else
      begin
      { copy subexpression }
      start := -1;
      len := 0;
      if no = 0 then
        begin
        { a whole expression }
        start := FStart;
        len := FLength;
        end
      else if (FStartP[no] <> -1) and (FEndP[no] <> -1) then
        begin
        { a subexpression }
        start := FStartP[no];
        len := FEndP[no]-start;
        end;
      if  (start >= 0) and (len > 0) then
        begin
        if ADest <> nil then
          begin
          if j < l then
            begin
            no := len;
            if j+len > l then
              no := l-j;
            Move(ASrc[start], ADest[j], no);
            end;
          end;
        Inc(j, len);
        end;
      end;
    end;

  { return number of characters to replace }
  ADLen := j;

  RegSub := True;
  end { TRegExp.RegSub };

end.
