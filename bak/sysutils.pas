{*******************************************************}
{                                                       }
{       Delphi Runtime Library                          }
{       System Utilities Unit                           }
{                                                       }
{       Copyright (C) 1995,96 Borland International     }
{                                                       }
{       Virtual Pascal v2.1                             }
{       Copyright (C) 1996-2000 vpascal.com             }
{                                                       }
{*******************************************************}
{AK155 25-06-2002
    В этом модуле оставлено только то, что необходимо для
функционирования исключений. В том, что осталсь, Format и
ShowException упрощены и реализован через Drivers.FormatStr.
}

unit SysUtils;

{$H-,J+,P+,S-,T-,W-,R-,Z-,X+}
{&Delphi+,AlignRec-,AlignData+,CDecl-,Use32-,Open32-}

{$IFDEF LINUX}
  {$UNDEF Win32}
{$ENDIF}

interface

uses
{$IFDEF Win32}
  Windows,
{$ENDIF}
  VpSysLow;

{определения Integer и MaxInt нужны для модуля Math }
type
  Integer = Longint;
const
  MaxInt = MaxLongint;

{ Exceptions }

type
  Exception = class(TObject)
  private
    FMessage: String;
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreateRes(Ident: Longint);
    constructor CreateResFmt(Ident: Longint; const Args: array of const);
    property Message: string read FMessage write FMessage;
  end;

  ExceptClass = class of Exception;

  EAbort = class(Exception);

  EOutOfMemory = class(Exception)
  private
    AllowFree: Boolean;
  public
    destructor Destroy; override;
    procedure FreeInstance; override;
  end;

  EInOutError = class(Exception)
  public
    ErrorCode: Longint;
  end;

  EIntError = class(Exception);
  EDivByZero = class(EIntError);
  ERangeError = class(EIntError);
  EIntOverflow = class(EIntError);

  EMathError = class(Exception);
  EInvalidOp = class(EMathError);
  EZeroDivide = class(EMathError);
  EOverflow = class(EMathError);
  EUnderflow = class(EMathError);

  EInvalidPointer = class(Exception);

  EInvalidCast = class(Exception);

  EConvertError = class(Exception);

  EAccessViolation = class(Exception);
  EPrivilege = class(Exception);
  EStackOverflow = class(Exception);
  EControlC = class(Exception);

  EPropReadOnly = class(Exception);
  EPropWriteOnly = class(Exception);

  EExternalException = class(Exception)
  public
    ExceptionRecord: POSExceptionRecord;
  end;


{ Exception handling routines }

function ExceptObject: TObject;
function ExceptAddr: Pointer;
function ReturnAddr: Pointer;

procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);

function Format(const Fmt: String; const Args: array of const): String;

{$R SYSUTILS.RES}

implementation
uses
  Strings,
  Drivers;

{$I SYSUTILS.INC}

(* !!!
{$IFNDEF PM_VERSION}
  {$I SYSUTILS.PA1}
{$ENDIF}
*)

{ Get return address of caller }

function ReturnAddr: Pointer; {&USES None} {&FRAME-}
asm
                mov     eax,[ebp+4]
end;

{ Exception handling routines }

var
  OutOfMemory: EOutOfMemory;

type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: POSExceptionRecord;
  end;

{ Return current exception object }

function ExceptObject: TObject;
begin
  if RaiseList <> nil then
    Result := PRaiseFrame(RaiseList)^.ExceptObject else
    Result := nil;
end;

{ Return current exception address }

function ExceptAddr: Pointer;
begin
  if RaiseList <> nil then
    Result := PRaiseFrame(RaiseList)^.ExceptAddr else
    Result := nil;
end;


{ Raise abort exception }

{$W+}

procedure Abort;
begin
  raise EAbort.CreateRes(SOperationAborted) at ReturnAddr;
end;

{ Raise out of memory exception }

procedure OutOfMemoryError;
begin
  raise OutOfMemory at ReturnAddr;
end;

{$W-}

{Cat}
function Format(const Fmt: String; const Args: array of const): String;
var
  I: Integer;
  Params: array[0..63] of Integer;
begin
  for I := Low(Args) to High(Args) do
    Params[I] := TVarRec(Args[I]).VInteger;
  FormatStr(Result, Fmt, Params);
end;
{/Cat}

function LoadStr(Ident: Longint): string;
var
  Buffer: array[0..1023] of Char;
begin
  Result := StrPas(SysLoadResourceString(Ident, Buffer, SizeOf(Buffer)));
end;

{ Exception class }

constructor Exception.Create(const Msg: string);
begin
  FMessage := Msg;
end;

constructor Exception.CreateFmt(const Msg: string;
  const Args: array of const);
begin
  FMessage := Format(Msg, Args);
end;

constructor Exception.CreateRes(Ident: Longint);
begin
  FMessage := LoadStr(Ident);
end;

constructor Exception.CreateResFmt(Ident: Longint;
  const Args: array of const);
begin
  FMessage := Format(LoadStr(Ident), Args);
end;

{ EOutOfMemory class }

destructor EOutOfMemory.Destroy;
begin
end;

procedure EOutOfMemory.FreeInstance;
begin
  if AllowFree then
    inherited FreeInstance;
end;

{ Create I/O exception }

function CreateInOutError: EInOutError;
type
  TErrorRec = record
    Code: Byte;
    Ident: Word;
  end;
const
  ErrorMap: array[0..6] of TErrorRec = (
    (Code: 2; Ident: SFileNotFound),
    (Code: 3; Ident: SInvalidFilename),
    (Code: 4; Ident: STooManyOpenFiles),
    (Code: 5; Ident: SAccessDenied),
    (Code: 100; Ident: SEndOfFile),
    (Code: 101; Ident: SDiskFull),
    (Code: 106; Ident: SInvalidInput));
var
  I: Longint;
begin
  I := Low(ErrorMap);
  while (I <= High(ErrorMap)) and (ErrorMap[I].Code <> InOutRes) do Inc(I);
  if I <= High(ErrorMap) then
    Result := EInOutError.CreateRes(ErrorMap[I].Ident) else
    Result := EInOutError.CreateResFmt(SInOutError, [InOutRes]);
  Result.ErrorCode := InOutRes;
  InOutRes := 0;
end;

{ RTL error handler }

type
  TExceptRec = record
    EClass: ExceptClass;
    EIdent: Word;
  end;
const
  ExceptMap: array[2..15] of TExceptRec = (
    (EClass: EInvalidPointer; EIdent: SInvalidPointer),
    (EClass: EDivByZero; EIdent: SDivByZero),
    (EClass: ERangeError; EIdent: SRangeError),
    (EClass: EIntOverflow; EIdent: SIntOverflow),
    (EClass: EInvalidOp; EIdent: SInvalidOp),
    (EClass: EZeroDivide; EIdent: SZeroDivide),
    (EClass: EOverflow; EIdent: SOverflow),
    (EClass: EUnderflow; EIdent: SUnderflow),
    (EClass: EInvalidCast; EIdent: SInvalidCast),
    (EClass: EAccessViolation; EIdent: SAccessViolation),
    (EClass: EPrivilege; EIdent: SPrivilege),
    (EClass: EControlC; EIdent: SControlC),
    (EClass: EStackOverflow; EIdent: SStackOverflow),
    (EClass: EExternalException; EIdent: SExternalException));

procedure ErrorHandler(ErrorCode: Longint; ErrorAddr: Pointer);
var
  E: Exception;
begin
  case ErrorCode of
    1: E := OutOfMemory;
    2..15: with ExceptMap[ErrorCode] do E := EClass.CreateRes(EIdent);
  else
    E := CreateInOutError;
  end;
  raise E at ErrorAddr;
end;

function MapException(P: POSExceptionRecord):Byte;
begin
  case P^.fExceptionNum of
    xcpt_Integer_Divide_By_Zero:  Result := 3;
    xcpt_Array_Bounds_Exceeded:   Result := 4;
    xcpt_Integer_Overflow:        Result := 5;
    xcpt_Float_Inexact_Result,
    xcpt_Float_Invalid_Operation,
    xcpt_Float_Stack_Check:       Result := 6;
    xcpt_Float_Divide_By_Zero:    Result := 7;
    xcpt_Float_Overflow:          Result := 8;
    xcpt_Float_Underflow,
    xcpt_Float_Denormal_Operand:  Result := 9;
    {$IFDEF LINUX}
    xcpt_In_Page_Error,
    {$ENDIF}
    {$IFDEF DPMI32} // Exception E
    xcpt_In_Page_Error,
    {$ENDIF}
    xcpt_Access_Violation:        Result := 11;
    xcpt_Privileged_Instruction:  Result := 12;
    xcpt_Signal_Ctrl_C:           Result := 13;
    xcpt_Unable_To_Grow_Stack:    Result := 14;
  else                            Result := 15;
  end;
end;

function GetExceptionClass(P: POSExceptionRecord): ExceptClass;
var
  ErrorCode: Byte;
begin
  ErrorCode := MapException(P);
  Result := ExceptMap[ErrorCode].EClass;
end;

function GetExceptionObject(P: POSExceptionRecord): Exception;
var
  ErrorCode: Longint;
  AccessOp: Longint; // string ID indicating the access type READ or WRITE
  AccessAddress: Pointer;
begin
  ErrorCode := MapException(P);
  case ErrorCode of
    3..10,12..14:
      with ExceptMap[ErrorCode] do Result := EClass.CreateRes(EIdent);
    11:
      begin
        with P^ do
        begin
          {$IfDef Win32}
          if fExceptionInfo[0] = 0 {xcpt_Read_Access} then
          {$Else}
          { OS/2, DPMI32 and Linux:
            hardware 0:unknown 1:read 2:write 4:execute 8:space 16:limit }
          if (fExceptionInfo[0] and $2) = 0 then
          {$EndIf}
            AccessOp := sReadAccess
          else
            AccessOp := sWriteAccess;
          AccessAddress := Pointer(fExceptionInfo[1]);
          Result := EAccessViolation.CreateResFmt(sAccessViolation,
            [fExceptionAddress, LoadStr(AccessOp), AccessAddress]);
        end;
      end;
  else
    Result := EExternalException.CreateResFmt(SExternalException,
      [P^.fExceptionNum]);
    EExternalException(Result).ExceptionRecord := P;
  end;
end;

{ RTL exception handler }

procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);
  var
    LineNo: Longint;
    FileName: ShortString;
    s: ShortString;
  begin
  if ExceptObject is Exception then
    writeln(Exception(ExceptObject).Message);

  if GetLocationInfo(ExceptAddr, FileName, LineNo) <> nil then
    writeln(Format('File: %s Line#: %d'#10, [FileName, LineNo]));
  end;

procedure ExceptHandler(ExceptObject: TObject; ExceptAddr: Pointer);
begin
  ShowException(ExceptObject, ExceptAddr);
  Halt(1);
end;

procedure InitExceptions;
begin
  OutOfMemory := EOutOfMemory.CreateRes(SOutOfMemory);
  ErrorProc := @ErrorHandler;
  ExceptProc := @ExceptHandler;
  ExceptionClass := Exception;
  ExceptClsProc := @GetExceptionClass;
  ExceptObjProc := @GetExceptionObject;
end;

initialization
  InitExceptions;
end.
