{&Delphi-}
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
{****************************************************************************}
{***       Unit for reassembling instruction for Borland Pascal 7.0       ***}
{****               Reassembling from 8088 up to Pentium                 ****}
{***        (C) by Grumbler, Ver. 1.02, 10-05-2000, Sumy, Ukraine.        ***}
{                                  *  *  *                                   }
{     If you'll find any bugs in decoding routings, send me buglist, plz,    }
{                  via FIDOnet: 2:4614/9.25, 2:4614/24.99                    }
{****************************************************************************}

{$Q-,W-,E-,R-,T-,Y-,I-,A+,S-,F-,G+,X-,V-,B-,N-}

unit Decoder;

interface

type
  XCHGData = record
    { Must be predefined by calling program }
    MemBuff: Pointer; { Pointer to decoding instruction }
    InstrMaxLen: LongInt;
    { Max instruction lenght in bytes -
                                   must be predefined for down decoding }
    Offset: LongInt;
    { Current instruction offset from
                                   segment start. Must be predifined
                                   before first call of procedure.
                                   After  decoding this value will be
                                   corrected automatically }

    { Filled by unit after decoding }
    Command: String; { Decoded name of instruction     }
    Operands: String; { Decoded instruction operands    }
    CodeStr: String; { Instruction dump code           }
    InstrLen: Byte; { Instruction code lenght         }
    end;

procedure ScanCode(var InstrData: XCHGData);

procedure ScanUp(var InstrData: XCHGData);

implementation

type
  Data = array[0..20] of Byte; {Temporary buffer for instruction}
  CodeN = String[9]; {Lenght of instruction name}
  Code = record
    {Simple Code record}
    C: Byte;
    S: Byte;
    end;

const
  Sg: array[0..5] of String[2] = ('es', 'cs', 'ss', 'ds', 'fs', 'gs');
  Rb: array[0..7] of String[2] = ('al', 'cl', 'dl', 'bl', 'ah', 'ch',
     'dh', 'bh');
  Rw: array[0..7] of String[2] = ('ax', 'cx', 'dx', 'bx', 'sp', 'bp',
     'si', 'di');

  AddrType: array[0..7] of String[6] = ('[bx+si', '[bx+di', '[bp+si',
     '[bp+di',
    '[si', '[di', '[bp', '[bx');

  CodeName: array[1..164] of CodeN =
  {1}('rep', 'repnz', 'lock', 'SS:', 'CS:', 'DS:', 'ES:', 'FS:', 'GS:',
     'aaa',
    {11}'aad', 'aam', 'aas', 'adc', 'add', 'and', 'arpl', 'bound',
     'bsf', 'bsr',
    {21}'bswap', 'bt', 'btc', 'btr', 'bts', 'call', 'cbw', 'cdq', 'clc',
     'cld',
    {31}'cli', 'clts', 'cmc', 'cmp', 'cmps', 'cmpxchg', 'cwd', 'cwde',
     'daa', 'das',
    {41}'dec', 'div', 'enter', 'hlt', 'idiv', 'imul', 'in', 'inc',
     'ins', 'int',
    {51}'into', 'invd', 'invlpg', 'iret', 'ja', 'jae', 'jb', 'jbe',
     'jcxz', 'jecxz',
    {61}'jz', 'jg', 'jge', 'jl', 'jle', 'jne', 'jno', 'jnp', 'jns', 'jo',
    {71}'jp', 'js', 'jmp', 'lahf', 'lar', 'lea', 'leave', 'lgdt',
     'lidt', 'lgs',
    {81}'lss', 'lfs', 'lds', 'les', 'lldt', 'lmsw', 'lods', 'loop',
     'loopnz', 'lsl',
    {91}'ltr', 'mov', 'movs', 'movsx', 'movzx', 'mul', 'neg', 'nop',
     'not', 'or',
    {101}'out', 'outs', 'pop', 'popa', 'popf', 'push', 'pusha', 'pushf',
     'rcl', 'rcr',
    {111}'rol', 'ror', 'ret', 'retf', 'sahf', 'sar', 'shl', 'shr',
     'sbb', 'scas',
    {121}'seta', 'setae', 'setb', 'setbe', 'setz', 'setg', 'setge',
     'setl', 'setle', 'setnz',
    {131}'setno', 'setnp', 'setns', 'setno', 'sets', 'sgdt', 'sidt',
     'shld', 'shrd', 'sldt',
    {141}'smsw', 'stc', 'std', 'sti', 'stos', 'str', 'sub', 'test',
     'verr', 'verw',
    {151}'wait', 'wbinvd', 'xadd', 'xchg', 'xlat', 'xor', 'int 3',
     'loopz', 'wrmsr', 'rdmsr',
    {161}'cpuid', 'rsm', 'cmpxchg8b', 'setp');

  _Rep = 1;
  _RepNZ = 2;
  _Lock = 3;
  _SS = 4;
  _CS = 5;
  _DS = 6;
  _ES = 7;
  _FS = 8;
  _GS = 9;
  _AAA = 10;
  _AAD = 11;
  _AAM = 12;
  _AAS = 13;
  _ADC = 14;
  _ADD = 15;
  _AND = 16;
  _ARPL = 17;
  _BOUND = 18;
  _BSF = 19;
  _BSR = 20;
  _BSWAP = 21;
  _BT = 22;
  _BTC = 23;
  _BTR = 24;
  _BTS = 25;
  _CALL = 26;
  _CBW = 27;
  _CDQ = 28;
  _CLC = 29;
  _CLD = 30;
  _CLI = 31;
  _CLTS = 32;
  _CMC = 33;
  _CMP = 34;
  _CMPS = 35;
  _CMPXCHG = 36;
  _CWD = 37;
  _CWDE = 38;
  _DAA = 39;
  _DAS = 40;
  _DEC = 41;
  _DIV = 42;
  _ENTER = 43;
  _HLT = 44;
  _IDIV = 45;
  _IMUL = 46;
  _IN = 47;
  _INC = 48;
  _INS = 49;
  _INT = 50;
  _INTO = 51;
  _INVD = 52;
  _INVLPG = 53;
  _IRET = 54;
  _JA = 55;
  _JAE = 56;
  _JB = 57;
  _JBE = 58;
  _JCXZ = 59;
  _JECXZ = 60;
  _JZ = 61;
  _JG = 62;
  _JGE = 63;
  _JL = 64;
  _JLE = 65;
  _JNE = 66;
  _JNO = 67;
  _JNP = 68;
  _JNS = 69;
  _JO = 70;
  _JP = 71;
  _JS = 72;
  _JMP = 73;
  _LAHF = 74;
  _LAR = 75;
  _LEA = 76;
  _LEAVE = 77;
  _LGDT = 78;
  _LIDT = 79;
  _LGS = 80;
  _LSS = 81;
  _LFS = 82;
  _LDS = 83;
  _LES = 84;
  _LLDT = 85;
  _LMSW = 86;
  _LODS = 87;
  _LOOP = 88;
  _LOOPNZ = 89;
  _LSL = 90;
  _LTR = 91;
  _MOV = 92;
  _MOVS = 93;
  _MOVSX = 94;
  _MOVZX = 95;
  _MUL = 96;
  _NEG = 97;
  _NOP = 98;
  _NOT = 99;
  _OR = 100;
  _OUT = 101;
  _OUTS = 102;
  _POP = 103;
  _POPA = 104;
  _POPF = 105;
  _PUSH = 106;
  _PUSHA = 107;
  _PUSHF = 108;
  _RCL = 109;
  _RCR = 110;
  _ROL = 111;
  _ROR = 112;
  _RET = 113;
  _RETF = 114;
  _SAHF = 115;
  _SAR = 116;
  _SHL = 117;
  _SHR = 118;
  _SBB = 119;
  _SCAS = 120;
  _SETA = 121;
  _SETAE = 122;
  _SETB = 123;
  _SETBE = 124;
  _SETZ = 125;
  _SETG = 126;
  _SETGE = 127;
  _SETL = 128;
  _SETLE = 129;
  _SETNZ = 130;
  _SETO = 131;
  _SETNP = 132;
  _SETNS = 133;
  _SETNO = 134;
  _SETS = 135;
  _SGDT = 136;
  _SIDT = 137;
  _SHLD = 138;
  _SHRD = 139;
  _SLDT = 140;
  _SMSW = 141;
  _STC = 142;
  _STD = 143;
  _STI = 144;
  _STOS = 145;
  _STR = 146;
  _SUB = 147;
  _TEST = 148;
  _VERR = 149;
  _VERW = 150;
  _WAIT = 151;
  _WBINVD = 152;
  _XADD = 153;
  _XCHG = 154;
  _XLAT = 155;
  _XOR = 156;
  _INT3 = 157;
  _LOOPZ = 158;
  _WRMSR = 159;
  _RDMSR = 160;
  _CPUID = 161;
  _RSM = 162;
  _CMPXCHG8B = 163;
  _SETP = 164;

  PrefN = 10; { All prefixes counts }
  IPB = 0; { IP begin }
  IPE = 2; { IP end   }
  SPB = 3; { SP begin }
  SPE = 8; { SP end   }
  os = 9;
  as = 10;
  Prefixes: array[0..PrefN] of Code =
    ( (C: $F3; S: _Rep), {0 - Instruction prefixes }
      (C: $F2; S: _RepNZ),
      (C: $F0; S: _Lock),

      (C: $2E; S: _CS), {3 - Segment prefixes }
      (C: $36; S: _SS),
      (C: $3E; S: _DS),
      (C: $26; S: _ES),
      (C: $64; S: _FS),
      (C: $65; S: _GS),

      (C: $66; S: 0), {9  - Operand-size override}
      (C: $67; S: 0) {10 - Address-size override}
    );

  Codes1N = 28;
  Codes1: array[0..Codes1N] of Code =
    ( (C: $37; S: _AAA), {0 - 1 byte Instructions }
      (C: $3F; S: _AAS),
      (C: $98; S: _CBW),
      (C: $99; S: _CDQ),
      (C: $F8; S: _CLC),
      (C: $FC; S: _CLD),
      (C: $FA; S: _CLI),
      (C: $F5; S: _CMC),
      (C: $27; S: _DAA),
      (C: $2F; S: _DAS),
      (C: $F4; S: _HLT),
      (C: $CC; S: _INT3),
      (C: $CE; S: _INTO),
      (C: $CF; S: _IRET),
      (C: $9F; S: _LAHF),
      (C: $C9; S: _LEAVE),
      (C: $90; S: _NOP),
      (C: $61; S: _POPA),
      (C: $9D; S: _POPF),
      (C: $60; S: _PUSHA),
      (C: $9C; S: _PUSHF),
      (C: $C3; S: _RET),
      (C: $CB; S: _RETF),
      (C: $9E; S: _SAHF),
      (C: $F9; S: _STC),
      (C: $FD; S: _STD),
      (C: $FB; S: _STI),
      (C: $9B; S: _WAIT),
      (C: $D7; S: _XLAT)
    );

  CodeStrN = 6;
  CodeStr: array[0..CodeStrN] of Code =
    ( (C: $A6; S: _CMPS),
      (C: $A4; S: _MOVS),
      (C: $6E; S: _OUTS),
      (C: $6C; S: _INS),
      (C: $AE; S: _SCAS),
      (C: $AC; S: _LODS),
      (C: $AA; S: _STOS));

  RegsN = 6;
  Regs: array[0..RegsN] of Code =
    ( (C: $48; S: _DEC),
      (C: $40; S: _INC),
      (C: $58; S: _POP),
      (C: $50; S: _PUSH),
      (C: $90; S: _XCHG),
      (C: $B0; S: _MOV),
      (C: $B8; S: _MOV)
    );

  JMPN = 27;
  JMP: array[0..JMPN] of Code =
    ( (C: $70; S: _JO), {Jxx}
      (C: $71; S: _JNO),
      (C: $72; S: _JB),
      (C: $73; S: _JAE),
      (C: $74; S: _JZ),
      (C: $75; S: _JNE),
      (C: $76; S: _JBE),
      (C: $77; S: _JA),
      (C: $78; S: _JS),
      (C: $79; S: _JNS),
      (C: $7A; S: _JP),
      (C: $7B; S: _JNP),
      (C: $7C; S: _JL),
      (C: $7D; S: _JGE),
      (C: $7E; S: _JLE),
      (C: $7F; S: _JG),
      (C: $E0; S: _LOOPNZ), {LOOPxx}
      (C: $E1; S: _LOOPZ),
      (C: $E2; S: _LOOP),
      (C: $E3; S: _JCXZ),
      (C: $E9; S: _JMP),
      (C: $EB; S: _JMP),
      (C: $E8; S: _CALL),
      (C: $CD; S: _INT),
      (C: $C2; S: _RET),
      (C: $CA; S: _RETF),
      (C: $6A; S: _PUSH),
      (C: $68; S: _PUSH)
    );

  IAccN = 10;
  IAcc: array[0..IAccN] of Code =
    ( (C: $14; S: _ADC), {xxx AX, imm}
      (C: $04; S: _ADD),
      (C: $24; S: _AND),
      (C: $3C; S: _CMP),
      (C: $E4; S: _IN),
      (C: $0C; S: _OR),
      (C: $E6; S: _OUT),
      (C: $1C; S: _SBB),
      (C: $2C; S: _SUB),
      (C: $A8; S: _TEST),
      (C: $34; S: _XOR)
    );

  Exept1N = 12;
  Exept1: array[0..Exept1N] of Code =
    ( (C: $EC; S: _IN), {xxx AX, imm}
      (C: $EE; S: _OUT),
      (C: $1F; S: _POP),
      (C: $07; S: _POP),
      (C: $17; S: _POP),
      (C: $0E; S: _PUSH),
      (C: $16; S: _PUSH),
      (C: $1E; S: _PUSH),
      (C: $06; S: _PUSH),
      (C: $D4; S: _AAM),
      (C: $C6; S: _MOV),
      (C: $A0; S: _MOV),
      (C: $A2; S: _MOV)
    );

  Exept2N = 12;
  Exept2: array[0..Exept2N] of Code =
    ( (C: $62; S: _BOUND),
      (C: $63; S: _ARPL),
      (C: $9A; S: _CALL),
      (C: $EA; S: _JMP),
      (C: $C8; S: _ENTER),
      (C: $69; S: _IMUL),
      (C: $6B; S: _IMUL),
      (C: $C4; S: _LES),
      (C: $C5; S: _LDS),
      (C: $8F; S: _POP),
      (C: $8D; S: _LEA),
      (C: $8C; S: _MOV),
      (C: $8E; S: _MOV)
    );

  _Codes1N = 10;
  _Codes1: array[0.._Codes1N] of Code =
    ( (C: $06; S: _CLTS), {with 0F prefix}
      (C: $08; S: _INVD),
      (C: $09; S: _WBINVD),
      (C: $A1; S: _POP),
      (C: $A9; S: _POP),
      (C: $A0; S: _PUSH),
      (C: $A8; S: _PUSH),
      (C: $30; S: _WRMSR),
      (C: $32; S: _RDMSR),
      (C: $A2; S: _CPUID),
      (C: $AA; S: _RSM)
    );

  _RegFEN = 6;
  _RegFE: array[0.._RegFEN] of Code =
    ( (C: $1; S: _DEC),
      (C: $0; S: _INC),
      (C: $4; S: _JMP),
      (C: $5; S: _JMP),
      (C: $6; S: _PUSH),
      (C: $2; S: _CALL),
      (C: $3; S: _CALL)
    );

  _RegF6N = 6;
  _RegF6: array[0.._RegF6N] of Code =
    ( (C: $6; S: _DIV),
      (C: $7; S: _IDIV),
      (C: $5; S: _IMUL),
      (C: $4; S: _MUL),
      (C: $3; S: _NEG),
      (C: $2; S: _NOT),
      (C: $0; S: _TEST)
    );

  _Reg80N = 6;
  _Reg80: array[0.._Reg80N] of Code =
    ( (C: $0; S: _ADD),
      (C: $4; S: _AND),
      (C: $7; S: _CMP),
      (C: $1; S: _OR),
      (C: $3; S: _SBB),
      (C: $5; S: _SUB),
      (C: $6; S: _XOR)
    );

  _RegD0N = 6;
  _RegD0: array[0.._RegD0N] of Code =
    ( (C: $0; S: _ROL),
      (C: $1; S: _ROR),
      (C: $2; S: _RCL),
      (C: $3; S: _RCR),
      (C: $4; S: _SHL),
      (C: $5; S: _SHR),
      (C: $7; S: _SAR)
    );

  _Reg00N = 5;
  _Reg00: array[0.._Reg00N] of Code =
    ( (C: $0; S: _SLDT),
      (C: $1; S: _STR),
      (C: $2; S: _LLDT),
      (C: $3; S: _LTR),
      (C: $4; S: _VERR),
      (C: $5; S: _VERW)
    );

  _Reg01N = 6;
  _Reg01: array[0.._Reg01N] of Code =
    ( (C: $0; S: _SGDT),
      (C: $1; S: _SIDT),
      (C: $2; S: _LGDT),
      (C: $3; S: _LIDT),
      (C: $4; S: _SMSW),
      (C: $6; S: _LMSW),
      (C: $7; S: _INVLPG)
    );

  _RegBAN = 3;
  _RegBA: array[0.._RegBAN] of Code =
    ( (C: $4; S: _BT),
      (C: $5; S: _BTS),
      (C: $6; S: _BTR),
      (C: $7; S: _BTC)
    );

  _CompR_MN = 19;
  _CompR_M: array[0.._CompR_MN] of Code =
    ( (C: $10; S: _ADC),
      (C: $12; S: _ADC),
      (C: $00; S: _ADD),
      (C: $02; S: _ADD),
      (C: $20; S: _AND),
      (C: $22; S: _AND),
      (C: $38; S: _CMP),
      (C: $3A; S: _CMP),
      (C: $88; S: _MOV),
      (C: $8A; S: _MOV),
      (C: $08; S: _OR),
      (C: $0A; S: _OR),
      (C: $28; S: _SUB),
      (C: $2A; S: _SUB),
      (C: $18; S: _SBB),
      (C: $1A; S: _SBB),
      (C: $84; S: _TEST),
      (C: $86; S: _XCHG),
      (C: $30; S: _XOR),
      (C: $32; S: _XOR)
    );

  _Group2_N = 4;
  _Group2: array[0.._Group2_N] of Code =
    ( (C: $02; S: _LAR), {reg,r/m}
      (C: $03; S: _LSL),
      (C: $BC; S: _BSF),
      (C: $BD; S: _BSR),
      (C: $AF; S: _IMUL)
    );

  _Group3_N = 3;
  _Group3: array[0.._Group3_N] of Code =
    ( (C: $A3; S: _BT), {r/m,reg}
      (C: $BB; S: _BTC),
      (C: $B3; S: _BTR),
      (C: $AB; S: _BTS)
    );

  _Other_N = 5;
  _Other_: array[0.._Other_N] of Code =
    ( (C: $A4; S: _SHLD),
      (C: $A6; S: _CMPXCHG),
      (C: $BE; S: _MOVSX),
      (C: $B6; S: _MOVZX),
      (C: $AC; S: _SHRD),
      (C: $C0; S: _XADD)
    );

  _SETxx_N = 15;
  _SETxx: array[0.._SETxx_N] of Code =
    ( (C: $90; S: _SETO), {Jxx}
      (C: $91; S: _SETNO),
      (C: $92; S: _SETB),
      (C: $93; S: _SETAE),
      (C: $94; S: _SETZ),
      (C: $95; S: _SETNZ),
      (C: $96; S: _SETBE),
      (C: $97; S: _SETA),
      (C: $98; S: _SETS),
      (C: $99; S: _SETNS),
      (C: $9A; S: _SETP),
      (C: $9B; S: _SETNP),
      (C: $9C; S: _SETL),
      (C: $9D; S: _SETGE),
      (C: $9E; S: _SETLE),
      (C: $9F; S: _SETG)
    );

procedure ScanUp;
  var
    D: XCHGData;
    TMP: ^data; {Temp buffer containing instruction}
    COfs, i: Byte;
    MaxUpBytes: LongInt;
    OldOffset: LongInt;
    OldMemBuff: Pointer;
  begin
  with InstrData do
    begin
    Command := '';
    CodeStr := '';
    Operands := '';
    InstrLen := 0;
    if  (MemBuff = nil) or (Offset <= 0) then
      Exit; { No more code }
    if Offset > 25 then
      MaxUpBytes := 25
    else
      MaxUpBytes := Offset;
    TMP := Pointer(LongInt(MemBuff)+Offset-MaxUpBytes);
    {Start of decoding block}
    OldOffset := Offset;
    OldMemBuff := MemBuff;
    COfs := 0;
    i := COfs;
    repeat
      D.Offset := 0;
      D.MemBuff := @TMP^[i];
      D.InstrMaxLen := MaxUpBytes-i;
      ScanCode(D);
      i := i+D.InstrLen;
      if  (i = MaxUpBytes) and (D.InstrLen > InstrLen) then
        InstrData := D;
      if i >= MaxUpBytes then
        begin
        Inc(COfs);
        i := COfs;
        end;
    until (COfs > MaxUpBytes);
    if Command = '' then
      InstrData := D;
    Offset := OldOffset-InstrLen;
    MemBuff := OldMemBuff;
    end;
  end { ScanUp };

function HexB(A: Byte): String;
  var
    M1, M2: Byte;
    S: String[2];
  begin
  M1 := A div 16;
  M2 := A-(M1*16);
  SetLength(S, 2);
  if M1 < 10 then
    S[1] := Chr(M1+$30)
  else
    S[1] := Chr(M1+55);
  if M2 < 10 then
    S[2] := Chr(M2+$30)
  else
    S[2] := Chr(M2+55);
  HexB := S;
  end;

function HexWp(A: Pointer): String;
  begin
  HexWp := HexB(Hi(Word(A^)))+HexB(Lo(Word(A^)));
  end;

type
  ___ = ^integer;
function SHexWp(A: ___): String;
  var
    S: Char;
  begin
  if A^ < 0 then
    S := '-'
  else
    S := '+';
  SHexWp := S+HexB(Hi(Abs(A^)))+HexB(Lo(Abs(A^)));
  end;

function HexW(A: Word): String;
  begin
  HexW := HexB(Hi(A))+HexB(Lo(A));
  end;

function HexDp(A: Pointer): String;
  begin
  HexDp := HexW(LongInt(A^) div $FFFF)+
    HexW(LongInt(A^) and $FFFF);
  end;

type
  ____ = ^longint;
function SHexDp(A: ____): String;
  var
    S: Char;
  begin
  if A^ < 0 then
    S := '-'
  else
    S := '+';
  SHexDp := S+HexW(Abs(A^) div $FFFF)+
    HexW(Abs(A^) and $FFFF);
  end;

function HexD(A: LongInt): String;
  begin
  HexD := HexW(A div $FFFF)+
    HexW(A and $FFFF);
  end;

function SHexB(A: ShortInt): String;
  var
    S: Char;
  begin
  if A < 0 then
    S := '-'
  else
    S := '+';
  SHexB := S+HexB(Abs(A));
  end;

{***************************************************************************}
{***                      Main subprogram                                ***}
{***************************************************************************}
procedure ScanCode(var InstrData: XCHGData);
  label i386, Use_Segment, _Use_Segment, Done;

  var
    Cur, {Current byte offset}
    CurB, {*}
    SPPos, {Position of SegPrefix in code}
    Reg, {Register code in instruction}
    i: Byte; {*}
    k: ShortInt; {*}
    SPp, {Segment prefix present}
    OSp, {Operand-size override present}
    ASp, {Address-size override present}
    Exeption, {Exeption in code}
    Stop: Boolean; {Data decoded - terminate work}
    SPCode: Byte; {Segment prefix code (if present)}
    W: Boolean; {Word size}
    Com, {Instruction name}
    Op: String[30]; {Instruction operand(s)}

    S: String; {* Temp for pefixes}
    M: Data; {Temp buffer containing instruction}
    CMax: Byte; {Max instr. lenght}
    Offset: {Instruction offset form segment bound}
    LongInt;

  function Get3(A: Byte): Byte;
    begin
    Get3 := (A and $38) shr 3;
    end;

  procedure CheckOS(A: Byte);
    begin
    if OSp and (A in [$60, $61, $9C, $9D, $CF]) then
      Com := Com+'d';
    end;

  function Hex: String;
    begin
    if not W then
      begin
      Hex := HexB(M[Cur]);
      Inc(Cur)
      end
    else if not OSp then
      begin
      Hex := HexWp(@M[Cur]);
      Inc(Cur, 2)
      end
    else
      begin
      Hex := HexDp(@M[Cur]);
      Inc(Cur, 4)
      end
    end;

  function RelB: String;
    begin
    RelB := HexD(Offset+Cur+1+ShortInt(M[Cur]));
    Inc(Cur)
    end;

  function RelW: String;
    begin
    if OSp then
      begin
      RelW := HexD(Offset+Cur+4+LongInt((@M[Cur])^));
      Inc(Cur, 4);
      end
    else
      begin
      RelW := HexD(Offset+Cur+2+Integer((@M[Cur])^));
      Inc(Cur, 2)
      end;
    end;

  procedure AddSpc;
    var
      i: Byte;
    begin
    if Length(S) < 8 then
      for i := 1 to 8-Length(S) do
        S := S+' '
    else
      S := S+' ';
    end;

  function R(T: Byte): String;
    begin
    if  (not W) then
      R := Rb[T]
    else if OSp then
      R := 'e'+Rw[T]
    else
      R := Rw[T]
    end;

  procedure ModR_M; { Decode R/M & SIB byte(s) }
    label _386, SIB, SIB8, SIB32;
    var
      _m, SIBMode: Byte;
      _type: String[10];
    begin
    SIBMode := 0;
    if W then
      if OSp then
        Op := 'dword ptr '
      else
        Op := 'word ptr '
    else
      Op := 'byte ptr ';
    Reg := Get3(M[Cur]);
    _m := M[Cur] and 7;
    Inc(Cur);

    if ASp then
      goto _386; {it's  32-bit x386}

    {16-bit x86}
    {*******************************************************}
    case (M[Cur-1] and $C0) shr 6 of
      0:
        begin
        if SPp then
          Op := Op+CodeName[SPCode];
        SPp := False;
        if _m = 6 then
          begin
          Op := Op+'['+HexWp(@M[Cur])+']';
          Inc(Cur, 2);
          end
        else
          Op := Op+AddrType[_m]+']';
        end;
      1:
        begin
        if SPp then
          Op := Op+CodeName[SPCode];
        SPp := False;
        Op := Op+AddrType[_m]+SHexB(M[Cur])+']';
        Inc(Cur);
        end;
      2:
        begin
        if SPp then
          Op := Op+CodeName[SPCode];
        SPp := False;
        Op := Op+AddrType[_m]+SHexWp(@M[Cur])+']';
        Inc(Cur, 2);
        end;
      3:
        Op := R(_m);
    end {case};
    Exit; {All done with 16-bit}
    {******************************************************}

_386: {32-bit x386}
    {******************************************************}
    case (M[Cur-1] and $C0) shr 6 of
      {16-bit x86}
      0:
        begin
        if SPp then
          Op := Op+CodeName[SPCode];
        SPp := False;
        if _m = 4 then
          goto SIB
        else if _m = 5 then
          begin
          Op := Op+'['+HexDp(@M[Cur])+']';
          Inc(Cur, 4);
          end
        else
          Op := Op+'[e'+Rw[_m]+']';
        end;
      1:
        begin
        if SPp then
          Op := Op+CodeName[SPCode];
        SPp := False;
        if _m = 4 then
          goto SIB8;
        Op := Op+'[e'+Rw[_m]+SHexB(M[Cur])+']';
        Inc(Cur);
        end;
      2:
        begin
        if SPp then
          Op := Op+CodeName[SPCode];
        SPp := False;
        if _m = 4 then
          goto SIB32;
        Op := Op+'[e'+Rw[_m]+SHexDp(@M[Cur])+']';
        Inc(Cur, 4);
        end;
      3:
        Op := R(_m);
    end {case};
    Exit;

    {SIB 32-bit x386}
    {******************************************************}

SIB32:
    Inc(SIBMode);
SIB8:
    Inc(SIBMode);
SIB:
    Inc(Cur);

    if Get3(M[Cur-1]) = 4 then
      {SIB exeption}
      begin
      Exeption := True;
      Exit
      end;

    Op := Op+'['; {SIB decoding exeption on MOD=00}
    if  (SIBMode = 0) and (M[Cur-1] and 7 = 5) then
      SIBMode := 2
    else
      Op := Op+'e'+Rw[M[Cur-1] and 7]+'+';

    case (M[Cur-1] and $C0) shr 6 of
      {Get Scaling factor}
      1:
        Op := Op+'2*';
      2:
        Op := Op+'4*';
      3:
        Op := Op+'8*';
    end {case};

    Op := Op+'e'+Rw[Get3(M[Cur-1])];

    case SIBMode of
      1:
        begin
        Op := Op+SHexB(M[Cur]);
        Inc(Cur);
        end;
      2:
        begin
        Op := Op+SHexDp(@M[Cur]);
        Inc(Cur, 4);
        end;
    end {case};
    Op := Op+']';
    end { ModR_M };

  procedure IPrefixes;
    var
      i: Byte;
    begin
    for i := 0 to PrefN do
      {Scanning for Prefixes}
      if Prefixes[i].C = M[Cur] then
        begin
        case i of
          IPB..IPE:
            S := S+CodeName[Prefixes[i].S]+' ';
          SPB..SPE:
            begin
            if SPp then
              Exeption := True
            else
              begin
              SPp := True;
              SPCode := Prefixes[i].S;
              SPPos := Cur;
              end;
            end;
          os:
            OSp := True;
          as:
            ASp := True;
        end {case};
        Inc(Cur); {Start again}
        i := 255;
        end;
    end { IPrefixes };

  procedure I1Byte;
    var
      i: Byte;
    begin
    for i := 0 to Codes1N do
      {Scanning for 1 byte instructions}
      if Codes1[i].C = M[Cur] then
        begin
        if OSp and (M[Cur] in [$98, $99]) then
          begin
          if  (M[Cur] = $98) then
            Com := CodeName[_CWDE];
          if  (M[Cur] = $99) then
            Com := CodeName[_CWD];
          end
        else
          Com := CodeName[Codes1[i].S];
        CheckOS(M[Cur]);
        Inc(Cur);
        Stop := True;
        Break
        end;
    end { I1Byte };

  procedure IStr;
    var
      i: Byte;
    begin
    CurB := (M[Cur] and $FE);
    W := (M[Cur] and 1) = 1;
    for i := 0 to CodeStrN do
      {Scanning for string instructions}
      if CodeStr[i].C = CurB then
        begin
        Com := CodeName[CodeStr[i].S];
        if not W then
          Com := Com+'b'
        else if OSp then
          Com := Com+'d'
        else
          Com := Com+'w';
        Stop := True;
        Inc(Cur);
        Break
        end;
    end;

  procedure xxxReg;
    var
      i: Byte;
    begin
    CurB := (M[Cur] and $F8);
    W := True;
    for i := 0 to RegsN do
      {Scanning for xxxReg instructions}
      if Regs[i].C = CurB then
        begin
        Com := CodeName[Regs[i].S];
        Op := R(M[Cur] and 7);
        case CurB of
          $90:
            Op := Op+','+R(0);
          $B0:
            begin
            W := False;
            Op := R(M[Cur] and 7)+','+HexB(M[Cur+1]);
            Inc(Cur);
            end;
          $B8:
            begin
            Inc(Cur);
            Op := Op+','+Hex;
            Dec(Cur);
            end;
        end {case};
        Stop := True;
        Inc(Cur);
        Break
        end;
    end { xxxReg };

  procedure JMPxx;
    var
      i: Byte;
    begin
    W := True;
    for i := 0 to JMPN do
      {Scanning for JMPxx instructions}
      if JMP[i].C = M[Cur] then
        begin
        Com := CodeName[JMP[i].S];
        Inc(Cur);
        case JMP[i].C of
          $CD, $6A:
            begin
            W := False;
            Op := Hex; {INT, PUSH}
            end;

          $C2,
          $CA,
          $68:
            Op := Hex; {RET xx, RETF xx}

          $E8, {Call, JMP}
          $E9:
            Op := RelW;

          else {case}
            Op := RelB;
        end {case};
        Stop := True;
        Break
        end;
    end { JMPxx };

  procedure IAccum;
    var
      i: Byte;
    begin
    CurB := (M[Cur] and $FE);
    W := (M[Cur] and $1) = 1;
    for i := 0 to IAccN do
      if CurB = IAcc[i].C then
        begin
        Com := CodeName[IAcc[i].S];
        Inc(Cur);
        case CurB of
          $E4:
            begin
            Op := R(0)+','+HexB(M[Cur]);
            Inc(Cur)
            end;
          $E6:
            begin
            Op := HexB(M[Cur])+R(0)+',';
            Inc(Cur)
            end;
          else {case}
            Op := R(0)+','+Hex;
        end {case};
        Stop := True;
        Break
        end;
    end { IAccum };

  procedure Exeptions;
    var
      i: Byte;
      Fl: Boolean;
    begin
    CurB := (M[Cur] and $FE);
    W := (M[Cur] and $1) = 1;
    for i := 0 to Exept1N do
      {1-st exeptions table}
      if CurB = Exept1[i].C then
        begin
        if  (CurB = $D4) and (M[Cur+1] <> $0A) then
          Break;
        if M[Cur] = $D5 then
          Com := CodeName[_AAD]
        else
          Com := CodeName[Exept1[i].S];
        case CurB of
          $EC,
          $EE:
            Op := R(0)+',dx';
          $D4:
            Inc(Cur);
          $C6:
            begin
            ModR_M;
            Inc(Cur);
            Op := Op+','+Hex;
            Dec(Cur);
            end;
          $A0:
            begin
            Op := R(0)+',';
            if SPp then
              Op := CodeName[SPCode];
            SPp := False;
            Inc(Cur);
            W := True;
            Op := Op+'['+Hex+']';
            Dec(Cur);
            end;
          $A2:
            begin
            if SPp then
              Op := CodeName[SPCode];
            SPp := False;
            Inc(Cur);
            Fl := W;
            W := True;
            Op := Op+'['+Hex+'],';
            W := Fl;
            Op := Op+R(0);
            Dec(Cur);
            end;
          else {case}
            Op := Sg[Get3(M[Cur])];
        end {case};

        Stop := True;
        Inc(Cur);
        Break
        end;

    if not Stop then
      for i := 0 to Exept2N do
        {2-nd exeptions table}
        if M[Cur] = Exept2[i].C then
          begin
          Com := CodeName[Exept2[i].S];
          Inc(Cur);
          case M[Cur-1] of
            $8C: {mov ...,sseg}
              begin
              ModR_M;
              Op := Op+','+Sg[Reg];
              end;

            $8D:
              begin {lea }
              ModR_M;
              if Pos('[', Op) = 0 then
                Exeption := True;
              Op := R(Reg)+','+Op;
              end;

            $8E: {mov sseg,...}
              begin
              ModR_M;
              Op := Sg[Reg]+','+Op;
              end;

            $8F:
              begin {POP}
              ModR_M;
              if Reg <> 0 then
                Exeption := True;
              end;

            $C4, {les}
            $C5: {lds}
              begin
              ModR_M;
              if Pos('[', Op) = 0 then
                Exeption := True
              else
                Delete(Op, 1, 10);

              Op := R(Reg)+','+Op;
              end;

            $69:
              begin
              ModR_M;
              if R(Reg) = Op then
                Op := R(Reg)+','+Hex
              else
                Op := R(Reg)+','+Op+','+Hex;
              end;

            $6B:
              begin
              ModR_M;
              if R(Reg) = Op then
                Op := R(Reg)+','+HexB(M[Cur])
              else
                Op := R(Reg)+','+Op+','+HexB(M[Cur]);
              Inc(Cur);
              end;
            $C8:
              begin
              Op := HexWp(@M[Cur])+','+HexB(M[Cur+2]);
              Inc(Cur, 3)
              end;

            $63:
              begin
              W := True;
              ModR_M;
              Op := Op+','+R(Reg);
              end;

            $62:
              begin
              W := True;
              ModR_M;
              Op := R(Reg)+','+Op;
              end;

            $9A,
            $EA:
              begin
              W := True;
              Op := HexWp(@M[Cur])+':';
              Inc(Cur, 2);
              Op := Op+Hex;
              end;

          end {case};
          Stop := True;
          Break
          end;
    end { Exeptions };

  procedure _I1Byte;
    var
      i: Byte;
    begin
    if M[Cur] in [$C8..$CF] then
      begin
      Com := CodeName[_BSWAP];
      Op := R(M[Cur] and 7);
      Inc(Cur);
      Stop := True;
      Exit
      end;
    for i := 0 to _Codes1N do
      {Scanning for 1 byte instructions}
      if _Codes1[i].C = M[Cur] then
        begin
        Com := CodeName[_Codes1[i].S];
        if M[Cur] in [$A0, $A1, $A8, $A9] then
          Op := Sg[Get3(M[Cur])];
        Inc(Cur);
        Stop := True;
        Break
        end;
    end { _I1Byte };

  procedure _eJumps;
    var
      i: Byte;
    begin
    CurB := M[Cur]-$10;
    for i := 0 to JMPN-12 do
      {Scanning for Jxx instructions}
      if JMP[i].C = CurB then
        begin
        Com := CodeName[JMP[i].S];
        Inc(Cur);
        Op := RelW;
        Stop := True;
        Break
        end;
    end;

  procedure SimpR_M;
    var
      i, command: Byte;
      fl: Boolean;
    begin
    CurB := Get3(M[Cur+1]);
    W := M[Cur] and 1 = 1;
    command := M[Cur] and $FE;
    case command of
      $F6:
        for i := 0 to _RegF6N do
          {Scanning for R/M instructions}
          if _RegF6[i].C = CurB then
            begin
            Com := CodeName[_RegF6[i].S];
            Inc(Cur);
            ModR_M;
            if _RegF6[i].C = 0 then
              Op := Op+','+Hex; {TEST extend}
            Stop := True;
            Break
            end;

      $80, $82:
        for i := 0 to _Reg80N do
          {Scanning for R/M instructions}
          if _Reg80[i].C = CurB then
            begin
            Com := CodeName[_Reg80[i].S];
            Inc(Cur);
            ModR_M;
            if command = $80 then
              Op := Op+','+Hex {Add Operand}
            else
              begin
              Op := Op+','+HexB(M[Cur]);
              Inc(Cur);
              end;
            Stop := True;
            Break
            end;

      $FE:
        for i := 0 to _RegFEN do
          {Scanning for R/M instructions}
          if _RegFE[i].C = CurB then
            begin
            Com := CodeName[_RegFE[i].S];
            Inc(Cur);
            if _RegFE[i].C in [3, 5] then
              begin
              {Check for bugs...}
              if M[Cur-1] = $FE then
                Exeption := True;

              W := True; {Modify instruction for JMP & CALL}
              fl := OSp;
              OSp := True;
              end;
            ModR_M;
            {Modify instruction back after JMP & CALL}
            if _RegFE[i].C in [3, 5] then
              OSp := fl;
            Stop := True;
            Break
            end;

      $D0, $D2, $C0: {Scanning for R/M Shift-instructions}
        for i := 0 to _RegD0N do
          if _RegD0[i].C = CurB then
            begin
            Com := CodeName[_RegD0[i].S];
            Inc(Cur);
            ModR_M;
            Op := Op+',';
            case command of
              $D0:
                Op := Op+'1';
              $D2:
                Op := Op+'cl';
              else {case}
                begin
                Op := Op+HexB(M[Cur]); {shl ax,5!}
                Inc(Cur);
                end;
            end {case};
            Stop := True;
            Break
            end;

    end {case};
    end { SimpR_M };

  procedure CompR_M;
    var
      i: Byte;
    begin
    Reg := Get3(M[Cur+1]);
    W := M[Cur] and 1 = 1;
    CurB := M[Cur] and $FE;
    for i := 0 to _CompR_MN do
      if _CompR_M[i].C = CurB then
        begin
        Com := CodeName[_CompR_M[i].S];
        Inc(Cur);
        ModR_M;
        if CurB and $0F in [2, $A]
        then
          Op := R(Reg)+','+Op
        else
          Op := Op+','+R(Reg);
        Stop := True;
        Break
        end;
    end;

  procedure _Group1;
    var
      i: Byte;
    begin
    CurB := Get3(M[Cur+1]);
    W := True;
    case M[Cur] of
      $00:
        for i := 0 to _Reg00N do
          {Scanning for R/M instructions}
          if _Reg00[i].C = CurB then
            begin
            Com := CodeName[_Reg00[i].S];
            Inc(Cur);
            ModR_M;
            Stop := True;
            Break
            end;
      $01:
        for i := 0 to _Reg01N do
          {Scanning for R/M instructions}
          if _Reg01[i].C = CurB then
            begin
            Com := CodeName[_Reg01[i].S];
            Inc(Cur);
            ModR_M;
            Stop := True;
            Break
            end;

      $BA:
        for i := 0 to _RegBAN do
          {Scanning for R/M instructions}
          if _RegBA[i].C = CurB then
            begin
            Com := CodeName[_RegBA[i].S];
            Inc(Cur);
            ModR_M;
            Op := Op+','+HexB(M[Cur]);
            Inc(Cur);
            Stop := True;
            Break
            end;
    end {case};
    end { _Group1 };

  procedure _Group2_3;
    var
      i: Byte;
    begin
    Reg := Get3(M[Cur+1]);
    W := True;
    for i := 0 to _Group2_N do
      if _Group2[i].C = M[Cur] then
        begin
        Com := CodeName[_Group2[i].S];
        Inc(Cur);
        ModR_M;
        Op := R(Reg)+','+Op;
        Stop := True;
        Break
        end;
    if not Stop then
      for i := 0 to _Group3_N do
        if _Group3[i].C = M[Cur] then
          begin
          Com := CodeName[_Group3[i].S];
          Inc(Cur);
          ModR_M;
          Op := Op+','+R(Reg);
          Stop := True;
          Break
          end;
    end { _Group2_3 };

  procedure _Other;
    var
      i: Byte;
      Fl,
      OldOSp: Boolean;
    begin
    case M[Cur] of
      $C7:
        begin
        Com := CodeName[_CMPXCHG8B];
        Inc(Cur);
        ModR_M;
        Stop := True;
        {inc(Cur);}
        Exit
        end;

      $B2, $B5, $B4: {LxS}
        begin
        if M[Cur] = $B2 then
          Com := CodeName[_LSS]
        else if M[Cur] = $B4 then
          Com := CodeName[_LFS]
        else
          Com := CodeName[_LGS];
        ModR_M;
        if Pos('[', Op) = 0 then
          Exeption := True
        else
          Delete(Op, 1, 10);
        Op := R(Reg)+','+Op;
        Stop := True;
        Inc(Cur);
        Exit
        end;

      $20..$24, {MOV}
      $26:
        begin
        Com := CodeName[_MOV];
        CurB := M[Cur];
        Inc(Cur);
        Reg := Get3(M[Cur]);
        case CurB of
          $20,
          $22:
            if not (Reg in [0, 2, 3]) then
              Exeption := True
            else
              Op := 'CR';
          $21,
          $23:
            if not (Reg in [0..3, 6, 7]) then
              Exeption := True
            else
              Op := 'DR';
          $24,
          $26:
            if not (Reg in [3..7]) then
              Exeption := True
            else
              Op := 'TR';
        end {case};
        Op := Op+Chr(Reg+48);
        W := True;
        Fl := OSp;
        OSp := True;
        Reg := M[Cur] and 7;
        if CurB in [$20, $21, $24] then
          Op := R(Reg)+','+Op
        else
          Op := Op+','+R(Reg);
        OSp := Fl;
        Stop := True;
        Inc(Cur);
        Exit
        end;
    end {case};
    Reg := Get3(M[Cur+1]);
    Fl := M[Cur] and 1 = 1;
    CurB := M[Cur] and $FE;
    for i := 0 to _Other_N do
      if _Other_[i].C = CurB then
        begin
        Com := CodeName[_Other_[i].S];
        Inc(Cur);
        W := True;
        case CurB of
          $AC, {SHxD}
          $A4:
            begin
            ModR_M;
            Op := Op+','+R(Reg)+',';
            if not Fl then
              begin
              Op := Op+HexB(M[Cur]);
              Inc(Cur);
              end
            else
              Op := Op+'cl'
            end;

          $BE, {MOVxX}
          $B6:
            begin
            OldOSp := OSp;
            if not Fl then
              W := False
            else if OSp then
              OSp := False;
            ModR_M;
            OSp := OldOSp;
            W := True;
            Op := R(Reg)+','+Op;
            end;
          $A6, {CMPXCHG}
          $C0: {XADD}
            begin
            W := Fl;
            ModR_M;
            Op := Op+','+R(Reg);
            end;
        end {case};
        Stop := True;
        Break
        end;
    end { _Other };

  procedure _eSETxx;
    var
      i: Byte;
    begin
    for i := 0 to _SETxx_N do
      {Scanning for Jxx instructions}
      if _SETxx[i].C = M[Cur] then
        begin
        Com := CodeName[_SETxx[i].S];
        Inc(Cur);
        W := False;
        ModR_M;
        Stop := True;
        Break
        end;
    end;

  begin { ScanCode }
  Cur := 0; {Current byte}
  SPp := False; {Segment prefix not present}
  OSp := False; {No Operand-size override}
  ASp := False; {No Address-size override}
  SPCode := 0; {No Segment prefix code}
  Stop := False; {Data not decoded}
  Exeption := False; {No exeptions}
  Com := '';
  Op := '';
  W := True;
  S := '';
  Offset := InstrData.Offset;
  with InstrData do
    begin
    CodeStr := '';
    Command := '';
    Operands := '';
    InstrLen := 0;
    if  (Offset < 0) or (MemBuff = nil) or (InstrMaxLen <= 0) then
      Exit;
    if InstrMaxLen > 15 then
      CMax := 15
    else
      CMax := InstrMaxLen;
    Move(Pointer(LongInt(MemBuff)+Offset)^, M, CMax);
    end;

  IPrefixes; {Scan for segment & instruction Prefixes}

  if M[Cur] = $0F then
    { It must be 386 or high! }
    begin
    Inc(Cur);
    goto i386;
    end;

  if SPp then
    goto Use_Segment;

  I1Byte;
  if not Stop then
    IStr;
  if not Stop then
    xxxReg;
  if not Stop then
    JMPxx;
  if not Stop then
    IAccum;

Use_Segment:
  if not Stop then
    Exeptions;
  if not Stop then
    SimpR_M;
  if not Stop then
    CompR_M;

  goto Done; { x86 (& some 286) instructions proccessed }
  {*******************************************************************}

  { It must be 386 or high! }
i386:
  if SPp then
    goto _Use_Segment; {Optimize decoding}
  _I1Byte;
  if not Stop then
    _eJumps;

_Use_Segment:
  if not Stop then
    _Group1;
  if not Stop then
    _Group2_3;
  if not Stop then
    _eSETxx;
  if not Stop then
    _Other;
  {*******************************************************************}

Done: {All tables passed}
  if  (not Stop or SPp or Exeption) or (CMax < Cur) then
    begin
    Cur := 1;
    Op := '';
    Com := '';
    if S <> '' then
      Stop := True
    else
      Stop := False;
    if not Stop then
      begin
      Com := 'db';
      Op := HexB(M[Cur-1]);
      if  (SPp and (SPPos = Cur-1)) then
        Op := Op+'  ; '+CodeName[SPCode]+'/"'+Chr(M[Cur-1])+'"'
      else
        Op := Op+'  ; "'+Chr(M[Cur-1])+'"';
      end;
    end;

  with InstrData do
    begin
    Command := S+Com;
    Operands := Op;
    InstrLen := Cur;
    Offset := Offset+InstrLen;
    for i := 0 to Cur-1 do
      CodeStr := CodeStr+HexB(M[i]);
    end;
  end { ScanCode };

end.
