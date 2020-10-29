unit RTPatch;
(******

Runtime Patch (for Virtual Pascal)
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}

interface

//function UnlockMemory(Base: Pointer; Size: LongInt): Boolean;
function RuntimePatch(OldFunc, NewFunc: Pointer): Boolean;

implementation

{$IFDEF OS2}
uses
  Os2Base
  ;

function UnlockMemory(Base: Pointer; Size: LongInt): Boolean;
  begin
  UnlockMemory := (DosSetMem(Base, Size, pag_Execute+pag_Read+pag_Write) =
       0);
  end;
{$ENDIF}

{$IFDEF Win32}
uses
  Windows
  ;

function UnlockMemory(Base: Pointer; Size: LongInt): Boolean;
  var
    OldProtect: LongInt;
  begin
  UnlockMemory := VirtualProtect(Base, Size, page_Execute_ReadWrite,
       @OldProtect);
  end;
{$ENDIF}

type
  PJMP = ^TJMP;
  TJMP = packed record
    JMP: Byte;
    Offset: LongInt;
    end;

function RuntimePatch(OldFunc, NewFunc: Pointer): Boolean;
  begin
  Result := UnlockMemory(OldFunc, SizeOf(TJMP));
  if Result then
    with PJMP(OldFunc)^ do
      begin
      JMP := $E9;
      Offset := LongInt(NewFunc)-LongInt(OldFunc)-5;
      end;
  end;

end.
