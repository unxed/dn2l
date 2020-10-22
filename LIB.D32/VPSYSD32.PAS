// |---------------------------------------------------------|
// |                                                         |
// |     Virtual Pascal Runtime Library.  Version 2.1.       |
// |     System Interface Layer for DOS/DPMI32               |
// |     ----------------------------------------------------|
// |     Copyright (C) 1995-2004 vpascal.com                 |
// |                                                         |
// |---------------------------------------------------------|

{$DEFINE THREAD_SUPPORT}        // SysCtrlCreateThread
{$DEFINE CLIP_SUPPORT}          // SysClipCopy
{$DEFINE PE2LE_P2}              // relocation depacker code

Function Max( a,b : Longint ) : Longint; inline;
  begin
    if a > b then
      Max := a
    else
      Max := b;
  end;

Function Min( a,b : Longint ) : Longint; inline;
  begin
    if a < b then
      Min := a
    else
      Min := b;
  end;

// 16:16 real <-> 0:32 flat
procedure SysSysSelToFlat(var P: Pointer);
  {&Frame-}{Uses eax,edx,esi}
  asm
    mov esi,P
    movzx eax,SmallWord [esi+0]
    movzx edx,SmallWord [esi+2]
    shl edx,4
    add eax,edx                         // assumes A20 enabled :)
    mov [esi],eax
  end;

procedure SysSysFlatToSel(var P: Pointer);
  {&Frame-}{&Uses eax,edx,esi}
  asm
    mov esi,P
    mov eax,[esi]
    cmp eax,$ffff0+$ffff
    jbe @valid
    push 201                            // range check error..
    call _RunError
  @valid:
    mov edx,eax                         // try to conbert to ssss:000o
    and edx,$fffffff0
    cmp edx,1 shr 20                    // > 1M?
    jb @below_1MiB
    mov edx,$ffff0                      // then use          FFFF:oooo
  @below_1MiB:
    sub eax,edx
    shr edx,4
    mov Smallword [esi+0],ax
    mov Smallword [esi+2],dx
  end;


function SysGetTextVideoMode:Longint;
  {&Frame-}{&Uses None}
  asm
    mov al,byte [Seg0040+$49]        // video mode 0..127, bit 7 is no-clear-screen
    and eax,$7f
  end;

// return linear address of video mem
function SysGetTextVideoMemBase:Longint;
  {&Frame-}{&Uses None}
  asm
    call SysGetTextVideoMode
    cmp al,$07                          // 7=mono
    mov eax,SegB000
    je @ret
    mov eax,SegB800
  @ret:
  end;


function SysGetTextVideoColumns:Longint;
  {&Frame-}{&Uses None}
  asm
    movzx eax,SmallWord [Seg0040+$4a]   // number of visible screen columns
  end;

function SysGetTextVideoRows:Longint;
  {&Frame-}{&Uses None}
  asm
    mov eax,25
    cmp video_adapter_found,ega_found   // error 3 means you have to set 'primary file' to VpSysLow.pas
    jb @ret
    movzx eax,byte [Seg0040+$84]        // number of visible lines-1 (EGA+)
    inc eax
  @ret:
  end;

function SysGetTextFontHeight:Longint;
  {&Frame-}{&Uses None}
  asm
    mov eax,16
    cmp video_adapter_found,mda_found
    je @ret
    mov eax,8
    cmp video_adapter_found,ega_found
    jb @ret
    movzx eax,Byte [Seg0040+$85]        // documnetation says SmallWord
  @ret:
  end;

function calculate_video_address(x,y:Longint):Longint;
  {&Frame-}{&Uses edx}
  asm
    call SysGetTextVideoColumns         // eax:=number of columns (80)
    movzx edx,[y].SmallWord
    mul edx                             // eax:=number of columns*y
    movzx edx,[x].SmallWord
    add edx,eax                         // edx:=number of columns*y+x
    add edx,edx                         // edx:=2*(number of columns*y+x)
    call SysGetTextVideoMemBase         // -> eax
    add eax,edx                         // SegB800+2*(number of columns*y+x)
  end;

function SysFileStdIn: Longint;
  begin
    SysFileStdIn := 0;
  end;

function SysFileStdOut: Longint;
  begin
    SysFileStdOut := 1;
  end;

function SysFileStdErr: Longint;
  begin
    SysFileStdErr := 2;
  end;

function SysFileOpen(FileName: PChar; Mode: Longint; var Handle: Longint): Longint;
  {&Frame-}{&Uses ebx,ecx,edx}
  asm
    sub eax,eax                         // Bit 31..16=0
    mov ah,$3d                          // OPEN FILE
    mov al,byte [Mode]
    mov edx,FileName
    int $21
    jc @ret

    mov edx,Handle
    mov [edx],eax
    sub eax,eax

  @ret:
  end;

function SysFileCreate(FileName: PChar; Mode,Attr: Longint; var Handle: Longint): Longint;
  {&Frame-}{&Uses ecx,edx}
  asm
    sub eax,eax                         // Bit 31..16=0
    mov ah,$3c                          // CREATE OR TRUNCATE
    mov ecx,Attr
    mov edx,FileName
    int $21
    jc @ret

    mov edx,Handle
    mov [edx],eax
    sub eax,eax

  @ret:
  end;

function SysFileCreateNEW(FileName: PChar; Attr: Longint; var Handle: Longint): Longint;
  {&Frame-}{&Uses ecx,edx}
  asm
    sub eax,eax                         // Bit 31..16=0
    mov ah,$5b                          // CREATE NEW FILE
    mov ecx,Attr
    mov edx,FileName
    int $21
    jc @ret

    mov edx,Handle
    mov [edx],eax
    sub eax,eax

  @ret:
  end;

function SysFileSeek(Handle,Distance,Method: Longint; var Actual: Longint): Longint;
  {&Frame-}{&Uses ebx,ecx,edx}
  asm
    sub eax,eax                         // Bit 31..16=0
    mov ah,$42                          // SET CURRENT FILE POSITION
    mov al,byte [Method]
    mov ebx,Handle
    mov dx,SmallWord [Distance+0]       // CX:DX
    mov cx,SmallWord [Distance+2]
    int $21
    jc @ret

    // DX:AX
    mov ecx,Actual
    mov [ecx  ],ax
    mov [ecx+2],dx
    sub eax,eax

  @ret:
  end;

function SysFileRead(Handle: Longint; var Buffer; Count: Longint; var Actual: Longint): Longint;
  {&Frame-}{&Uses ebx,ecx,edx}
  asm
    sub eax,eax                         // Bit 31..16=0
    mov ah,$3f
    mov ebx,Handle
    mov ecx,Count
    mov edx,Buffer
    int $21
    mov edx,Actual
    jc @error

    // eax=actual read
    mov [edx],eax  // Actual:=eax
    sub eax,eax
    jmp @ret

  @error:
    // eax=error code
    sub ecx,ecx    // Actual:=0
    mov [edx],ecx

  @ret:
    call Test_Ctrl_C_Sensed
  end;

function SysFileWrite(Handle: Longint; const Buffer; Count: Longint; var Actual: Longint): Longint;
  {&Frame-}{&Uses ebx,ecx,edx}
  asm
    sub eax,eax                         // Bit 31..16=0
    mov ah,$40
    mov ebx,Handle
    mov ecx,Count
    mov edx,Buffer
    int $21
    mov edx,Actual
    jc @error

    mov [edx],eax
    sub eax,eax
    jmp @ret

  @error:
    sub ecx,ecx
    mov [edx],ecx

  @ret:
    call Test_Ctrl_C_Sensed
  end;

function SysFileSetSize(Handle,NewSize: Longint): Longint;assembler;
  {&Frame+}{&Uses None}
  var
    current_position,
    actual              :Longint;
  asm
    // remember file position
    push Handle
    push 0                      // +-0
    push 1                      // from current position
    lea eax,current_position
    push eax
    call SysFileSeek
    test eax,eax
    jnz @ret                    // error

    // seek to
    mov eax,NewSize
    cmp eax,current_position
    je @seek_NewSize_done
    push Handle
    push eax                    // offset
    push 0                      // from begin of file
    lea eax,actual
    push eax
    call SysFileSeek
    test eax,eax
    jnz @restore_ret            // error

  @seek_NewSize_done:
    // write 0 Byte to truncate:
    push Handle                 // Handle
    push 0                      // Buffer (@nil^)
    push 0                      // Count
    lea eax,actual              // @Actual
    push eax
    call SysFileWrite
    test eax,eax
    jnz @restore_ret            // error

    // null byte written can be an error...
    // if filesize<>NewSize then error occured:

    push Handle
    push 0                      // no offset
    push 2                      // to end of file
    lea eax,actual
    push eax
    call SysFileSeek
    test eax,eax
    jnz @restore_ret            // error

    mov eax,[actual]            // final SysFileSetSize check
    cmp eax,[NewSize]
    mov eax,0
    setne al                    // eax=0=equal eax=1=error

  @restore_ret:
    // restore old file position - ignore errors here!
    push eax

    mov eax,current_position
    cmp eax,NewSize
    je @seek_current_position_done

    push Handle
    push eax                    // offset
    push 0                      // from begin of file
    lea eax,actual
    push eax
    call SysFileSeek

  @seek_current_position_done:

    pop eax

  @ret:
  end;

function SysFileClose(Handle: Longint): Longint;
  {&Frame-}{&Uses ebx}
  asm
    sub eax,eax
    mov ebx,Handle

    // if (Handle > 2) or (Handle < 0) then Exit
    cmp ebx,0
    jl @1
    cmp ebx,2
    jle @ret

  @1:
    mov ah,$3e                          // close file using handle
    int $21
    jc @ret

    sub eax,eax                         // SysFileClose:=0

  @ret:
  end;

function SysFileFlushBuffers(Handle: Longint): Longint;
  {&Frame-}{&Uses ebx}
  asm
    sub eax,eax
    mov ah,$68
    mov ebx,Handle
    int $21
    jc @ret

    sub eax,eax

  @ret:
  end;

function SysFileDelete(FileName: PChar): Longint;
  {&Frame-}{&Uses edx}
  asm
    sub eax,eax
    mov ah,$41
    mov edx,FileName
    int $21
    jc @ret

    sub eax,eax

  @ret:
  end;

function SysFileMove(OldName,NewName: PChar): Longint;
  {&Frame-}{&Uses edx,edi}
  asm
    sub eax,eax
    mov ah,$56
    mov edx,OldName // DS:EDX
    mov edi,NewName // ES:EDI
    int $21
    jc @ret

    sub eax,eax

  @ret:
  end;

function SysFileIsDevice(Handle: Longint): Longint;
  {&Frame-}{&Uses ebx,edx}
  asm
    mov eax,$4400
    mov ebx,Handle
    int $21

    jc @error

    sub eax,eax                         // 0 .. file
    test dl,$80                         // Bit 7 = 1 -> Char/Block DEV
    setnz al                            // 1 .. device
    jmp @ret

  @error:
    sub eax,eax

  @ret:
  end;

function SysDirGetCurrent(Drive: Longint; Path: PChar): Longint;
  {&Frame-}{&Uses edx,esi,edi}
  asm
    mov eax,Drive
    mov edi,Path

    test eax,eax
    jnz @drive_known

    mov ah,$19
    int $21       // -> AL: 0=A 1=B ..
    inc eax

  @drive_known:
    cld
    mov dl,al
    add al,'A'-1
    stosb
    mov ax,'\:'   // 'X:\'
    stosw

    mov esi,edi
    sub eax,eax
    stosd
    // LFN extension by piwamoto
(*
INT 21 - Windows95 - LONG FILENAME - GET CURRENT DIRECTORY
        AX = 7147h
        DL = drive number (00h = current, 01h = A:, etc.)
        DS:SI -> buffer for ASCIZ directory name
Return: CF clear if successful
        CF set on error
            AX = error code (see #01680)
                7100h if function not supported
Notes:  the returned pathname does not include the drive letter, colon, or
          leading backslash, and is not necessarily a long filename -- this
          function returns whatever path was used when changing to the
          current directory, and may include a mixture of long and short
          components
        the provided buffer must be at least as large as the value indicated
          by AX=71A0h
        for compatibility with DOS versions prior to v7.00, the carry flag
          should be set on call to ensure that it is set on exit
SeeAlso: AH=47h,AX=713Bh,AX=7160h,AX=71A0h
*)
    mov ah,$47    // get current directory -> ESI (without 'X:\')
    int $21       // DL 1=A 2=B ..
    jc @ret
// LFN extension by piwamoto
// we don't care if int21(ax=7147) is not supported by OS
// buffer already filled with int21(ah=47)
    mov ax,$7147  // get current directory -> ESI (without 'X:\')
    int $21       // DL 1=A 2=B ..

    sub eax,eax   // SysDirGetCurrent:=0

  @ret:
  end;

function SysDirSetCurrent(Path: PChar): Longint;
  {&Frame-}{&Uses ebx,edx,esi}
  asm
    mov esi,Path
    mov edx,[esi]
    sub eax,eax                         // SysDirSetCurrent:=0
    cmp dl,0                            // Path=''?
    je @ret                             // yes, success

    cmp dh,':'                          // Path[1]=':'?
    jne @no_drive

    // drive change
    movzx eax,dl                        // Drive:=UpCase(Path[0])
    push eax
    call _upcase
    lea edx,[eax-'A']                   // dl:=Ord(Drive)-Ord('A')

    mov ah,$0e                          // set drive dl (0..31)
    int $21

    mov ah,$19                          // get drive (0..31)
    int $21

    cmp al,dl                           // success ?
    mov eax,15                          // invalid drive
    jne @ret                            // no -> exit
                                        // yes -> set path

    inc esi                             // skip Length('A:')
    inc esi

  @no_drive:

    // path change?
    sub eax,eax
    cmp byte [esi],al                   // ('A:')
    je @ret                             // no, success

    mov ah,$3b                          // change current directory
    mov edx,esi                         // DS:DX
    int $21
    jc @ret

    sub eax,eax

  @ret:

  end;


function SysDirCreate(Path: PChar): Longint;
  {&Frame-}{&Uses edx}
  asm
    sub eax,eax
    mov ah,$39
    mov edx,Path
    int $21
    jc @ret

    sub eax,eax

  @ret:
  end;

function SysDirDelete(Path: PChar): Longint;
  {&Frame-}{&Uses edx}
  asm
    sub eax,eax
    mov ah,$3a
    mov edx,Path
    int $21
    jc @ret

    sub eax,eax

  @ret:
  end;

function SysMemAvail: Longint;
  var
    dpmimeminfo         :dpmimeminfo09_type;
  begin
    dpmi_getmeminfo09(dpmimeminfo);

    with dpmimeminfo do
      begin
        if (free_pages<>$ffffffff) then
          begin
            if free_pages>(500*1024) then
              SysMemAvail:=500*1024*4096 // ~~ max(Longint)
            else
              if free_pages>1 then
                SysMemAvail:=(free_pages-1)*4096
              else
                SysMemAvail:=0;
          end
        else
          begin
            if largest_available_block_in_bytes=$ffffffff then
              SysMemAvail:=512*1024 // no informations - guess 512K free
            else
              if largest_available_block_in_bytes>2000 then
                SysMemAvail:=largest_available_block_in_bytes-2000
              else
                SysMemAvail:=0;
          end;
      end;

  end;


function SysMemAlloc(Size,Flags: Longint; var MemPtr: Pointer): Longint;
  {&Frame-}{&Uses ebx,ecx,esi,edi}
  asm
    // Flags is ignored

    mov eax,$0501       // DPMI 0.9+ ALLOCATE MEMORY BLOCK
    mov ecx,[Size]      // BX:CX
    add ecx,(15+4)      // + alignment to 16 bytes
                        // + need 5 bytes to store handle

    mov ebx,ecx
    shr ebx,16
    int $31
    jc @error

    // BX:CX = Address
    // SI:DI = Handle

    shl ebx,16
    mov bx,cx

    // align to 16 bytes, room for Handle
    lea eax,[ebx+(15+4)]
    and eax,$fffffff0

    // remeber Handle SI:DI
    mov [eax-4+2],si
    mov [eax-4+0],di

    mov edi,MemPtr                      // MemPtr:=eax
    mov [edi],eax
    sub eax,eax                         // Result:=0
    jmp @ret

  @error:
    mov edi,MemPtr                      // MemPtr :=nil
    and Longint [edi],0
    mov eax,8                           // Result:=8

  @ret:
  end;

function SysMemFree(MemPtr: Pointer): Longint;
  {&Frame-}{&Uses esi,edi}
  asm
    mov eax,[MemPtr]

    // get Handle (SI:DI)
    mov esi,[eax-4+2]
    mov edi,[eax-4+0]

    mov eax,$00000502    // DPMI 0.9+ FREE MEMORY BLOCK
    int $31
    jc @ret

    sub eax,eax

  @ret:
  end;


function SysSysMsCount: Longint;
  {&Frame-}{&Uses None}
  asm
    // SysSysMsCount:=MemL[seg0040+$6c]*55;
    // 1024/18.2
    imul eax,[Seg0040+$6c].Longint,55
  end;

function SysFileOpen_Create(Open: Boolean;FileName: PChar; Mode,Attr,Action: Longint; var Handle: Longint): Longint;
  {&Frame+}{&Uses ebx,ecx,edx}
  asm
    sub eax,eax
    mov edx,FileName

    cmp Open,false
    je @open_false
    cmp Open,true
    jne @invalid_option

    cmp Action,0
    jne @1

    // Open=true, Action=0
    // SysFileOpen(FileName,Mode,Handle);
    push edx
    push Mode
    push Handle
    call SysFileOpen
    // SysFileOpen_Create:=eax
    jmp @ret

  @1:
    cmp Action,open_CreateIfNew
    jne @2
    // Open=true, Action=open_CreateIfNew
    // SysFileOpen(FileName,Mode,Handle);
    push edx
    push Mode
    push Handle
    call SysFileOpen
    test eax,eax
    // SysFileOpen_Create:=eax (0)
    jz @ret

    // SysFileCreate
    push edx
    push Mode
    push Attr
    push Handle
    call SysFileCreate
    jmp @ret

  @2:
    cmp Action,open_TruncateIfExists
    jne @invalid_option
    // Open=true, Action=open_TruncateIfExist
    // SysFileOpen(FileName,Mode,Handle);
    push edx
    push Mode
    push Handle
    call SysFileOpen
    test eax,eax
    jnz @ret // does not exist? -> exit
    // truncate
    // SysFileSetSize(Handle,0)
    mov eax,Handle
    push Longint [eax] // Handle
    push 0
    call SysFileSetSize
    jmp @ret

    //*** open = false ***

  @open_false:
    cmp Action,0
    jne @3

    // Open=false, Action=0
    // SysFileCreateNEW(FileName,Attr,Handle) ; fails for existing file
    push edx
    push Attr
    push Handle
    call SysFileCreateNEW
    jmp @ret

  @3:
    cmp Action,create_TruncateIfExists
    jne @invalid_option
    // Open=false, Action=create_TruncateIfExist
    // SysFileCreate(FileName,Mode,Attr,Handle)
    push edx
    push Mode
    push Attr
    push Handle
    call SysFileCreate
    jmp @ret

  @invalid_option:
    mov eax,1 // function number invalid

  @ret:
  end;

function SysFileCopy(_Old, _New: PChar; _Overwrite: Boolean): Boolean;
  var
    d1,d2       :file;
    p           :array[0..8*1024-1] of byte;
    org_FileMode:Longint;
    size,
    pos,
    block,
    actual      :Longint;
    error       :Longint;
  begin
    SysFileCopy:=false;

    Assign(d1,_old);
    Assign(d2,_new);

    org_FileMode:=FileMode;
    FileMode:=open_access_WriteOnly;

    if _overwrite then
      begin
        {$I-}
        Rewrite(d2,1);
        {$I+}
        FileMode:=org_FileMode;
        if IOResult<>0 then
          Exit;
      end
    else
      begin
        {$I-}
        Reset(d2,1);
        {$I+}
        if IOResult=0 then
          begin
            FileMode:=org_FileMode;
            Exit;
          end;

        {$I-}
        Rewrite(d2,1);
        FileMode:=org_FileMode;
        {$I+}
        if IOResult<>0 then
          Exit;
      end;


    {$I-}
    FileMode:=open_access_ReadOnly+open_share_DenyNone;
    Reset(d1,1);
    FileMode:=org_FileMode;
    {$I+}

    if IOResult<>0 then
      begin
        Close(d2);
        Exit;
      end;

    size:=FileSize(d1);
    pos:=0;
    repeat
      if pos=size then
        begin
          SysFileCopy:=true;
          Break;
        end;

      block:=size-pos;
      if block>SizeOf(p) then
        block:=SizeOf(p);

      BlockRead(d1,p,block,actual);
      if block<>actual then Break;
      BlockWrite(d2,p,block,actual);
      if block<>actual then Break;

      Inc(pos,block);
    until false;

    Close(d1);
    Close(d2);
  end;

function SysCtrlSelfAppType: Longint;
  begin
    // full-screen text mode ... should be 0 (Online Help is wrong)
    SysCtrlSelfAppType := 1;
  end;

function SysCtrlCreateThread(Attrs: Pointer; StackSize: Longint; Func,Param: Pointer; Flags: Longint; var Tid: Longint): Longint;
  begin
    {$IfDef THREAD_SUPPORT}
    dpmi32_install_thread_code;
    SysCtrlCreateThread:=
      Dpmi32CreateThread(tid,Func,Ofs(Param),Flags,StackSize);
    {$Else}
    SysCtrlCreateThread:=-1;
    {$EndIf}
  end;

function SysCtrlKillThread(Handle: Longint): Longint;
  begin
    {$IfDef THREAD_SUPPORT}
    SysCtrlKillThread:=
      Dpmi32KillThread(Handle,0);
    {$Else}
    SysCtrlKillThread:=-1;
    {$EndIf}
  end;

function SysCtrlSuspendThread(Handle: Longint): Longint;
  begin
    {$IfDef THREAD_SUPPORT}
    SysCtrlSuspendThread:=
      Dpmi32SuspendThread(Handle);
    {$Else}
    SysCtrlSuspendThread:=-1;
    {$EndIf}
  end;

function SysCtrlResumeThread(Handle: Longint): Longint;
  begin
    {$IfDef THREAD_SUPPORT}
    SysCtrlResumeThread:=
      Dpmi32ResumeThread(Handle);
    {$Else}
    SysCtrlResumeThread:=-1;
    {$EndIf}
  end;

function SysGetThreadId: Longint;
  begin
    {$IfDef THREAD_SUPPORT}
    SysGetThreadId:=
      Dpmi32GetThreadId;
    {$Else}
    // -> GetThreadId
    SysGetThreadId:=1;
    {$EndIf}
  end;

procedure SysCtrlExitThread(ExitCode: Longint);
  begin
    {$IfDef THREAD_SUPPORT}
    Dpmi32KillThread(Dpmi32GetThreadId,ExitCode);
    {$Else}
    // Not implemented
    {$EndIf}
  end;

procedure SysCtrlExitProcess(ExitCode: Longint);
  {&Frame-}{&Uses None}
  asm
    // debug support:  See  deb_link.pas
    nop
    mov ah,$4c
    mov al,Byte [ExitCode] // lo(ExitCode)
    int $21
  end;

function SysCtrlGetModuleName(Handle: Longint; Buffer: PChar): Longint;
  {&Frame-}{&Uses None}
  asm
    // not used ?
    @1:
    mov eax,[Buffer]
    push offset @1
    push eax
    push 260
    call SysGetModuleName
    sub eax,eax
  end;

procedure SysCtrlEnterCritSec;
  {&Frame-}{&Uses None}
  asm
    {$IfDef THREAD_SUPPORT}
    jmp Dpmi32EnterCritSec
    {$Else}
    //cli
    {$EndIf}
  end;

procedure SysCtrlLeaveCritSec;
  {&Frame-}{&Uses None}
  asm
    {$IfDef THREAD_SUPPORT}
    jmp Dpmi32LeaveCritSec
    {$Else}
    //sti
    {$EndIf}
  end;


function SysGetProcessId: Longint;
  begin
    Result := sel_psp;
  end;

var
  dpmi32_tlsmapmem      :array[0..SharedMemSize-1] of byte;

const
  tls1                  :integer=0;

function SysCtrlGetTlsMapMem: Pointer;

begin
  Result:=@dpmi32_tlsmapmem;
  if tls1=0 then
  begin
    tls1:=1;
    FillChar(Result^, SharedMemSize, $FF);
    FillChar(Result^, SizeOf(TSharedMem), 0);
    System.GetMemoryManager( PSharedMem(Result)^.TlsMemMgr );
  end;
end;


var
  syscmdln_var          :array[0..512] of Char;
  parax_length_array    :array[0..100] of Longint;
  SysCmdlnCount_var     :Longint;

function SysCmdln: PChar;
  begin
    SysCmdln := @syscmdln_var;
  end;

function SysCmdlnCount: Longint;
  begin
    SysCmdlnCount := SysCmdlnCount_var;
  end;

procedure SysCmdlnParam(Index: Longint; var Param: ShortString);
  {&Frame-}{&Uses eax,ecx,edx,esi,edi}
  asm
    // Param:=''
    mov edi,Param
    mov byte [edi],0

    mov edx,Index
    mov esi,offset parax_length_array
    mov edi,offset syscmdln_var
    sub ecx,ecx
    cld

  @param_loop:
    lodsd

    // out of Paramstr ?
    test eax,eax // 0 ?
    jz @ret

    // counter=index ?
    cmp edx,ecx
    je @found

    // skip eax chars, increment counter
    add edi,eax
    inc edi     // length(' ')
    inc ecx     // inc(counter)
    jmp @param_loop

  @found:

    // Parameter like "Demo"
    cmp byte [edi],'"'
    jne @not_quote

    // Length('"DEMO"')-2*Length('"')
    dec eax
    dec eax
    // skip leading '"'
    inc edi

  @not_quote:

    mov esi,edi
    mov ecx,eax
    mov edi,Param
    mov al,cl
    stosb
    jecxz @ret
    rep movsb

    @ret:
  end;

function SysGetEnvironment: PChar;
  begin
    SysGetEnvironment := Environment;
  end;

procedure SysFreeEnvironment(_Env: PChar);
  begin
    // Nothing - the environment does not need freeing
  end;

// pre-calculate command line
procedure Setup_Commandline;
  var
    source,
    target              :PChar;
    l1                  :Longint;
  begin
    FillChar(syscmdln_var      ,SizeOf(syscmdln_var      ),0);
    FillChar(parax_length_array,SizeOf(parax_length_array),0);
    SysCmdlnCount_var:=0;

    // Paramstr(0)
    source:=Environment;
    while MemW[Ofs(source^)]<>0 do
      Inc(source);

    Inc(source,4); // skip byte $00 $00 $01 $00

    l1:=StrEnd(source)-source;

    Move(source^,syscmdln_var,l1);
    target:=@syscmdln_var[l1+1];  // Cmdline=Paramstr(0)+#0+Paramstr(1)+' '...
                                  //                       ^
    parax_length_array[SysCmdlnCount_var]:=l1;

    // ParamStr(1+)
    source:=Ptr(seg_psp+$81);
    while source[0] in [#9,' '] do
      Inc(source);

    repeat
      // End of Parameters?
      if source[0] in [#0,#10,#13] then
        Break;

      // skip spaces
      if source[0] in [#9,' '] then
        begin
          Inc(source);
          Continue;
        end;

      // found ..
      Inc(SysCmdlnCount_var);

      if SysCmdlnCount_var>1 then
        begin
          // separate : ' '
          target[0]:=' ';
          Inc(target);
        end;

      if source[0]='"' then
        begin
          // search terminating '"'
          l1:=1;
          while not (source[l1] in [#0,#10,#13,'"']) do
            Inc(l1);
          // not including '"'

          // copy calculated para ('"DEMO')
          Move(source^,target^,l1);
          Inc(target,l1);
          // move start
          Inc(source,l1);

          // trailing '"'
          target[0]:='"';
          Inc(target);

          // store length('"DEMO"')
          parax_length_array[SysCmdlnCount_var]:=l1+1;

          // skip trailing '"'
          if source[0]='"' then
            Inc(source);
        end
      else
        begin
          // search terminating ' '
          l1:=0;
          while not (source[l1] in [#0,#9,#10,#13,' ']) do
            Inc(l1);

          // copy calculated para
          Move(source^,target^,l1);
          Inc(target,l1);
          // move start
          Inc(source,l1);

          parax_length_array[SysCmdlnCount_var]:=l1;
        end;

    until false;

  end;


function SysOsVersion: Longint;
  {&Frame-}{&Uses ebx,ecx}
  asm
    sub eax,eax
    mov ah,$30
    int $21
  end;

procedure write_value_edi;
  {&Frame-}{&Uses None}
  asm
    test edi,edi                        // if Assigned(edi) then
    jz @ret
    mov [edi],eax                       // MemL[edi]:=value(eax)
  @ret:
  end;

procedure SysGetDateTime(Year,Month,Day,DayOfWeek,Hour,Minute,Second,MSec: PLongint);assembler;
  {&Frame-}{&Uses All}
  asm
    mov ebp,Offset write_value_edi

    mov ah,$2a // get system date
    int $21

    mov edi,DayOfWeek
    movzx eax,al
    call ebp

    mov edi,Year
    movzx eax,cx
    call ebp

    mov edi,Month
    movzx eax,dh
    call ebp

    mov edi,Day
    movzx eax,dl
    call ebp

    mov ah,$2c // get system time
    int $21

    mov edi,Hour
    movzx eax,ch
    call ebp

    mov edi,Minute
    movzx eax,cl
    call ebp

    mov edi,Second
    movzx eax,dh
    call ebp

    mov edi,MSec
    movzx eax,dl        // eax=h
    shl eax,1           // eax=2*h
    lea eax,[eax+eax*4] // eax=10*h
    movzx eax,dl
    call ebp
  end;

procedure SysSetDateTime(Year,Month,Day,Hour,Minute,Second,MSec: PLongint);
  {&Frame-}{&Uses All}
  asm
    mov ah,$2a // get system date
    int $21

    mov esi,Year
    test esi,esi
    jz @1
    mov cx,SmallWord [esi]
  @1:

    mov esi,Month
    test esi,esi
    jz @2
    mov dh,Byte [esi]
  @2:

    mov esi,Day
    test esi,esi
    jz @3
    mov dl,Byte [esi]
  @3:

    mov ah,$2b // set system date
    int $21


    mov ah,$2c // get system time
    int $21

    mov esi,Hour
    test esi,esi
    jz @4
    mov ch,Byte [esi]
  @4:

    mov esi,Minute
    test esi,esi
    jz @5
    mov cl,Byte [esi]
  @5:

    mov esi,Second
    test esi,esi
    jz @6
    mov dh,Byte [esi]
  @6:

    mov esi,MSec
    or esi,esi
    jz @7

    push ecx
      push edx
        mov eax,[esi]
        cdq
        mov ecx,10
        div ecx
      pop edx
    pop ecx
    mov dl,al
  @7:

    mov ah,$2d // set system time
    int $21

  end;

function SysVerify(SetValue: Boolean; Value: Boolean): Boolean;
  {&Frame-}{&Uses edx}
  asm
    cmp setvalue,true
    jne @verify_read

    sub eax,eax // off
    cmp value,true
    sete al    // on

    mov ah,$2e // al=verify
    mov dl,0
    int $21

  @verify_read:

    mov ah,$54
    int $21

    // al Verify  boolean  ord
    //  0  off    false    0
    //  1  on     true     1
  end;

function SysDiskFreeLong(Drive: Byte): TQuad;assembler;
  {&Frame-}{&Uses All}
  var
    DiskFree            :comp;
  asm
    mov ah,$36
    mov dl,drive
    int $21

    cmp ax,$ffff
    je @error

    movzx eax,ax
    movzx ebx,bx
    movzx ecx,cx
    mul ebx
    mul ecx
    // -> EDX:EAX

    jmp @ret

  @error:
    // return -1
    or eax,-1
    mov edx,eax

  @ret:
    // return edx:eax
    mov Longint [DiskFree+0],eax
    mov Longint [DiskFree+4],edx

    // SysDiskFreeLong:=DiskFree;
    fild qword ptr [DiskFree]
    fwait
  end;

function SysDiskSizeLong(Drive: Byte): TQuad;assembler;
  {&Frame-}{&Uses All}
  var
    DiskSize            :comp;
  asm
    mov ah,$36
    mov dl,drive
    int $21

    cmp ax,$ffff
    je @error

    movzx eax,ax
    movzx edx,dx
    movzx ecx,cx
    mul edx
    mul ecx
    // -> EDX:EAX

    jmp @ret

  @error:
    // return -1
    or eax,-1
    mov edx,eax

  @ret:
    // return edx:eax
    mov Longint [DiskSize+0],eax
    mov Longint [DiskSize+4],edx

    // SysDiskSizeLong:=q4;
    fild qword ptr [DiskSize]
    fwait
  end;

{Cat: пока что просто заглушки, потом надо переделать для поддержки сетевых путей}
function SysDiskFreeLongX(Path: PChar): TQuad;
var
  Regs: real_mode_call_structure_typ;
begin
  fillchar(Mem[segdossyslow32],
           SizeOf(DriveData) + SizeOf(Path) + 1{#0}, #0);
  move(Path^, Mem[segdossyslow32 + SizeOf(DriveData)], SizeOf(Path));
  init_register(regs);
  regs.ax_ := $7303;
  regs.ds_ := segdossyslow16;
  regs.dx_ := SizeOf(DriveData); //ds:dx=path
  regs.es_ := segdossyslow16;
//  regs.di_ := 0;
  regs.cx_ := SizeOf(DriveData);
  intr_realmode(Regs, $21);
  move(Mem[segdossyslow32], DriveData.RecSize, SizeOf(DriveData));
  if DriveData.RecSize <> 0
  then begin
       Result := DriveData.SectorsPerCluster * DriveData.BytesPerSector;
       Result := Result * DriveData.AvailClusters;
       end
  else Result := SysDiskFreeLong(Byte(UpCase(Path^))-Byte('A')+1);
end;

function SysDiskSizeLongX(Path: PChar): TQuad;
var
  Regs: real_mode_call_structure_typ;
begin
  fillchar(Mem[segdossyslow32],
           SizeOf(DriveData) + SizeOf(Path) + 1{#0}, #0);
  move(Path^, Mem[segdossyslow32 + SizeOf(DriveData)], SizeOf(Path));
  init_register(regs);
  regs.ax_ := $7303;
  regs.ds_ := segdossyslow16;
  regs.dx_ := SizeOf(DriveData); //ds:dx=path
  regs.es_ := segdossyslow16;
//  regs.di_ := 0;
  regs.cx_ := SizeOf(DriveData);
  intr_realmode(Regs, $21);
  move(Mem[segdossyslow32], DriveData.RecSize, SizeOf(DriveData));
  if DriveData.RecSize <> 0
  then begin
       Result := DriveData.SectorsPerCluster * DriveData.BytesPerSector;
       Result := Result * DriveData.TotalClusters;
       end
  else Result := SysDiskSizeLong(Byte(UpCase(Path^))-Byte('A')+1);
end;
{/Cat}

function SysGetFileAttr(FileName: PChar; var Attr: Longint): Longint;
  {&Frame-}{&Uses ecx,edx}
  asm
    mov eax,$00004300
    mov edx,FileName
    sub ecx,ecx
    int $21
    jc @ret

    mov edx,Attr
    mov [edx],ecx
    sub eax,eax

  @ret:
  end;

function SysSetFileAttr(FileName: PChar; Attr: Longint): Longint;
  {&Frame-}{&Uses ecx,edx}
  asm
    mov eax,$00004301
    mov ecx,Attr
    mov edx,FileName
    int $21
    jc @ret

    sub eax,eax

    @ret:
  end;

function SysGetFileTime(Handle: Longint; var Time: Longint): Longint;
  {&Frame-}{&Uses ebx,ecx,edx}
  asm
    sub ecx,ecx
    sub edx,edx

    mov eax,$00005700
    mov ebx,Handle
    int $21
    jc @ret

    shl edx,16 // CX=Zeit DX=Datum
    or ecx,edx

    mov edx,time
    mov [edx],ecx
    sub eax,eax

  @ret:
  end;

function SysSetFileTime(Handle: Longint; Time: Longint): Longint;
  {&Frame-}{&Uses ebx,ecx,edx}
  asm
    mov eax,$00005701
    mov ebx,Handle

    mov cx,SmallWord [Time+0]
    mov dx,SmallWord [Time+2]
    int $21
    jc @ret

    sub eax,eax

  @ret:
  end;

procedure get_DTA(var org_dta: real_mode_ptr_type);assembler;
  {&Frame-}{&Uses eax,esi,edi}
  var
    regs                :real_mode_call_structure_type;
  asm
    mov [regs.ah_],$2f  // GET DTA
    lea eax,regs
    push eax
    push $21
    call intr_realmode

    mov edi,org_dta
    mov ax,[regs.ds_]   // -> ES:BX
    mov [edi+real_mode_ptr_type.seg_],ax
    mov eax,[regs.ebx_]
    mov [edi+real_mode_ptr_type.ofs_],ax
  end;

procedure set_DTA(var dta: real_mode_ptr_type);assembler;
  {&Frame-}{&Uses eax,esi,edi}
  var
    regs                :real_mode_call_structure_type;
  asm
    mov edi,dta
    mov [regs.ah_],$1a  // SET DTA DS:DX
    mov ax,[edi+real_mode_ptr_type.seg_]
    mov [regs.ds_],ax
    mov ax,[edi+real_mode_ptr_type.ofs_]
    mov [regs.edx_],eax
    lea eax,regs
    push eax
    push $21
    call intr_realmode
  end;


procedure process_search_record(var F: TOSSearchRec; IsPChar: Boolean);
  begin
    with F do
      begin
        attr:=dos_dta.attr;
        time:=dos_dta.time;
        size:=dos_dta.size;

        if ispchar then
          StrCopy(PChar(@name),dos_dta.name)
        else
          name:=StrPas(dos_dta.name);
      end;
  end;

const
  ofs_searchmask=$200; // room in segdossyslow32 reserved for TOSSearchRec

function SysFindFirst(Path: PChar; Attr: Longint; var F: TOSSearchRec; IsPChar: Boolean): Longint;
  var
    reg                 :real_mode_call_structure_type;
    org_dta             :real_mode_ptr_type;
  begin
    init_register(reg);
    F.attr_must:=Attr shr 8;

    // copy '*.*'#0
    StrCopy(Ptr(segdossyslow32+ofs_searchmask),Path);

    get_DTA(org_dta);
    set_DTA(work_dta);
    FillChar(Mem[segdossyslow32],SizeOf(F.dos_dta),0);

    with reg do
      begin
        ah_:=$4e;            // FINDFIRST
        ds_:=segdossyslow16; // DS:DX
        dx_:=ofs_searchmask;
        cx_:=attr;
        intr_realmode(reg,$21);
      end;

    set_DTA(org_dta);

    // copy work_dta -> F

    Move(Mem[segdossyslow32],F.dos_dta,SizeOf(F.dos_dta));
    process_search_record(F,IsPChar);

    // Error on FindFirst?
    if (reg.flags_ and flags_carry)<>0 then
      begin
        SysFindFirst:=reg.ax_;
        Exit;
      end;

    // success ... but matches attr?
    if (F.attr and F.attr_must)=F.attr_must then
      begin
        SysFindFirst:=0; // success!
        Exit;
      end;

    // let FindNext do the work ...
    SysFindFirst:=SysFindNext(F,ispchar);
  end;

function SysFindNext(var F: TOSSearchRec; IsPChar: Boolean): Longint;
  var
    reg                 :real_mode_call_structure_type;
    org_dta             :real_mode_ptr_type;
  begin
    Move(F.dos_dta,Mem[segdossyslow32],SizeOf(F.dos_dta));
    init_register(reg);

    repeat

      get_DTA(org_dta);
      set_DTA(work_dta);

      reg.ah_:=$4f;            // FINDNEXT
      intr_realmode(reg,$21);

      set_DTA(org_dta);

      // copy arbeits_dta -> F
      Move(Mem[segdossyslow32],F.dos_dta,SizeOf(F.dos_dta));
      process_search_record(F,IsPChar);

      // found?
      if (reg.flags_ and flags_carry)<>0 then
        begin
          SysFindNext:=reg.ax_;
          Exit;
        end;

      // success ... but matches attr?
      if (F.attr and F.attr_must)=F.attr_must then
        begin
          SysFindNext:=0; // success!
          Exit;
        end;

    until false;
  end;

function SysFindClose(var F: TOSSearchRec): Longint;
  begin
    SysFindClose := 0;
  end;

// Check if file exists; if it does, update FileName parameter
// to include correct case of existing file
// VpSysOs2.PAS
function SysFileAsOS(FileName: PChar): Boolean;
  var
    SRec: TOSSearchRec;
    P: PChar;
  begin
    Result := False;
    if SysFindFirst(FileName, $37, SRec, False) = 0 then
      begin
        if SRec.Name[1] <> #0 then
          begin
            // Replace filename part with data returned by OS
            P := StrRScan(FileName, '\');
            if P = nil then
              P := FileName
            else
              inc(P); // Point to first character of file name
            strPcopy(P, SRec.Name);
          end;
        SysFindClose(SRec );
        Result := True;
      end;
  end;

function SysFileSearch(Dest,Name,List: PChar): PChar;
  var
    sr                  :TOSSearchRec;
    ListPos,
    ListLimit           :PChar;
    FileNameTest        :array[0..260] of Char;
    IsDirectory,
    found               :boolean;

  begin
    Result := Dest;
    Result [0] := #0;

    if Name[0] = #0 then // Name='' ?
      Exit;

    // Directory or File?
    // $10** - else OS/2 will find volume labels everywhere..
    IsDirectory:=SysFindFirst(Name, $10ff, sr, true) = 0; // true=(sr.name=pchar)
    SysFindClose(sr);

    if IsDirectory then
      SysFileExpand(Result, Name)
    else
      begin
        // first test: in current directory
        StrCopy(FileNameTest, '.\');
        ListPos := List;
        repeat
          StrCat(FileNameTest, Name);

          found:=SysFindFirst(FileNameTest, $ff and (not $10), sr, true)=0;
          SysFindClose(sr);
          if found then
            begin
              SysFileExpand(Result, FileNameTest);
              Break;
            end;

          while ListPos[0] = ';' do Inc(ListPos);

          if ListPos[0] = #0 then
            begin
              Result[0] := #0;
              Break;
            end;

          if ListPos[0] = '"' then
            begin
              FileNameTest[0] := #0;
              Inc(ListPos);                     // Length('"');
              ListLimit := StrPos(ListPos, '"');
              if ListLimit = nil then
                ListLimit := StrEnd(ListPos);
              StrLCopy(FileNameTest, ListPos, ListLimit-ListPos);
              ListPos := ListLimit;
              if ListPos[0] = '"' then
                 Inc(ListPos);                  // Length('"');
            end
          else
            begin
              ListLimit := StrPos(ListPos, ';');
              if ListLimit = nil then
                ListLimit := StrEnd(ListPos);

              StrLCopy(FileNameTest, ListPos, ListLimit-ListPos);
              ListPos := ListLimit;
            end;

          if FileNameTest[0] <> #0 then
            if not (PChar(StrEnd(FileNameTest)-1)[0] in [':','\','/']) then
              StrCat(FileNameTest, '\');

        until false;
      end;

  end;

// aus VpSysOs2.PAS
function SysFileExpand(Dest,Name: PChar): PChar;
  var
    I,J,L               : Integer;
    C                   : Char;
    CurDir              : array[0..259] of Char;

  procedure AdjustPath;
    begin
      if (Dest[J-2] = '\') and (Dest[J-1] = '.') then
        Dec(J,2)
      else
        if (j>3) and (Dest[J-3] = '\') and (Dest[J-2] = '.') and (Dest[J-1] = '.') then
          begin
            Dec(J, 3);
            if Dest[J-1] <> ':' then
              repeat
                Dec(J);
              until Dest[J] = '\';
          end;
    end;

  begin
    L := StrLen(Name);

    if (L >= 2) and ((Name[1] = ':') or (Name[0] = '\') and (Name[1] = '\')) then
      begin                         // Path is already in form 'X:\Path'
        if (L >= 3) and (Name[2] in ['\','/']) then
          StrCopy(Dest, Name)
        else
          begin                     // Path is in form 'X:Path'
            SysDirGetCurrent(Ord(UpCase(Name[0])) - (Ord('A') - 1), CurDir);
            if StrLen(CurDir) > 3 then
              StrCat(CurDir, '\');
            StrLCat(StrCopy(Dest, CurDir), PChar(@Name[2]), 259);
          end;
      end
    else
      begin                         // Path is without drive letter
        SysDirGetCurrent(0, CurDir);// Get default drive & directory
        if StrLen(CurDir) > 3 then
          StrCat(CurDir, '\');
        if Name[0] in ['\','/'] then
          StrLCopy(Dest, PChar(@CurDir[0]), 2) // 'X:' only
        else
          StrCopy(Dest, CurDir);
        StrLCat(Dest, Name, 259);
      end;

    I := 0; J := 0;
    for I := 0 to StrLen(Dest)-1 do
      begin
        C := Dest[I];
        if C='/' then
          begin
            C:='\';
            Dest[I]:=C;
          end;
        if C = '\' then AdjustPath;
        Dest[J] := C;
        Inc(J);
      end;

    AdjustPath;
    if Dest[J-1] = ':' then
      begin
        Dest[J] := '\';
        Inc(J);
      end;

    Dest[J] := #0;
    Result := Dest;
  end;

// will be ignored:
// * Env (when Async=true)
// * PID (when Async=true or Async=false)
// * StdIn,StdOut,StdErr (when Async=true)

threadvar
  LastAsync             :Boolean;
  last_exitcode         :Longint;

function SysExecute(Path,CmdLine,Env: PChar; Async: Boolean; PID: PLongint; StdIn,StdOut,StdErr: Longint): Longint;
  type
    dos_exe_parablock_type=
      packed record
        env_seg         :SmallWord;
        cmdline_ptr     :real_mode_ptr_type;
        fcb5c           :real_mode_ptr_type;
        fcb6c           :real_mode_ptr_type;
        res_sssp        :real_mode_ptr_type;
        res_csip        :real_mode_ptr_type;
      end;

    os2_startdata_type=
      packed record
        cb              :SmallWord;             // length of structure (must be 0018h,001Eh,0020h,0032h,or 003Ch)
        relation        :SmallWord;             // relation of new process to caller (00h independent, 01h child)
        foreback        :SmallWord;             // fore/background (00h foreground, 01h background)
        trace           :SmallWord;             // trace options (00h-02h, 00h = no trace)
        title           :real_mode_ptr_type;    // pointer to ASCIZ program title (max 62 chars) or 0000h:0000h
        name            :real_mode_ptr_type;    // pointer to ASCIZ program name (max 128 chars) or 0000h:0000h
        args            :real_mode_ptr_type;    // pointer to ASCIZ program args (max 144 chars) or 0000h:0000h
        termq           :Longint;               // "TermQ" (currently reserved, must be 00000000h)
        // $18
        env             :real_mode_ptr_type;    // pointer to environment (max 486 bytes) or 0000h:0000h
        inheritance     :SmallWord;             // inheritance (00h or 01h)
        // $1e
        session_type    :SmallWord;             // session type 0,1,2,3,4,7
        // $20
        icon            :real_mode_ptr_type;    // pointer to ASCIZ icon filename (max 128 chars) or 0000h:0000h
        pgmhandle       :Longint;
        pgmcontrol      :SmallWord;
        initial_column  :SmallWord;
        initial_row     :SmallWord;
        initial_with    :SmallWord;
        initial_height  :SmallWord;
        // $32
        reserved1       :SmallWord;
        objectbuffer    :Longint;
        objectbufferlen :Longint;
      end;

  var
    l1                  :Longint;
    regs                :real_mode_call_structure_type;
    env_laenge          :Longint;
    env_low16           :SmallWord;
    env_low32           :Longint;
    StdHandles          :array[0..2] of Longint;
    NewHandles          :array[0..2] of Longint;
    OldHandles          :array[0..2] of Longint;
    I                   :Longint;
  begin
    LastAsync := Async;
    last_exitcode:=-1;

    init_register(regs);

    StrCopy(PChar(@Mem[segdossyslow32]),Path);
    l1:=StrLen(CmdLine);
    Mem[segdossyslow32+$100]:=l1+1;
    Mem[segdossyslow32+$101]:=Ord(' ');
    Move(CmdLine^,Mem[segdossyslow32+$102],l1);
    Mem[segdossyslow32+$102+l1  ]:=$0d;
    Mem[segdossyslow32+$102+l1+1]:=$00;

    FillChar(dos_exe_parablock_type(Mem[segdossyslow32+$200]),SizeOf(dos_exe_parablock_type),0);
    with     dos_exe_parablock_type(Mem[segdossyslow32+$200]) do
      begin
        cmdline_ptr.seg_:=segdossyslow16;
        cmdline_ptr.ofs_:=$100;
      end;


    if async then
      begin
        if taskmgr then (*** DR-DOS TASKMGR ***)
          begin
            with regs do
              begin
                ax_:=$2707; // create new task
                bx_:=$0200; // ES:BX = PARABLOCK
                dx_:=$0000; // DS:DX = Programm
                cx_:=0;     // switch
                ds_:=segdossyslow16;
                es_:=segdossyslow16;
              end;
            intr_realmode(regs,$2f);
            SysExecute:=0;
            last_exitcode:=0; // unknown
          end
        else if os2 then (*** OS/2 VDM/VMB ***)
          begin
            FillChar(os2_startdata_type(Mem[segdossyslow32+$200]),SizeOf(os2_startdata_type),0);
            with     os2_startdata_type(Mem[segdossyslow32+$200]) do
              begin
                cb:=$18;
                name.seg_:=segdossyslow16;
                name.ofs_:=0;
                args.seg_:=segdossyslow16;
                args.ofs_:=$102;
              end;

            with regs do
              begin
                ah_:=$64;
                bx_:=$0025;
                cx_:=$636c;
                si_:=$200; // DS:SI
                ds_:=segdossyslow16;
              end;
            intr_realmode(regs,$21);

            SysExecute:=regs.ax_;
            last_exitcode:=0; // unknown
          end
        else (*** no support for async ***)
          begin
            SysExecute:=-1;
            last_exitcode:=0;
          end
      end
    else (*** async =false -> DOS 2.x+ ***)
      begin
        // copy environment to <1 MB
        // 1: calculate length
        if Env=nil then env:=Environment;
        env_laenge:=0;
        while MemW[ofs(Env^)+env_laenge]<>0 do
          Inc(env_laenge);
        Inc(env_laenge,2);
        // 2: allocate memory
        if getdosmem(env_low16,env_laenge)<>0 then
          begin
            SysExecute:=8; // Mem ...
            last_exitcode:=0;
          end
        else
          begin
            // 3: copy environment
            env_low32:=dosseg_linear(env_low16);
            Move(Env^,Mem[env_low32],env_laenge);
            dos_exe_parablock_type(Mem[segdossyslow32+$200]).env_seg:=env_low16;


            with regs do
              begin
                ax_:=$4b00; // EXEC
                bx_:=$0200; // ES:BX = PARABLOCK
                dx_:=$0000; // DS:DX = Programm
                ds_:=segdossyslow16;
                es_:=segdossyslow16;
              end;


            StdHandles[0] := StdIn;
            StdHandles[1] := StdOut;
            StdHandles[2] := StdErr;

            for I := 0 to 2 do
              if StdHandles[I] <> -1 then
                begin
                  OldHandles[I] := $FFFFFFFF;       // Save original StdIn to OldIn
                  NewHandles[I] := I;
                  DPMI32DupHandle(NewHandles[I], OldHandles[I]);
                  DPMI32DupHandle(StdHandles[I], NewHandles[I]);
                end;

            // swapvectors
            RemoveDpmi32Exceptionhandlers;
            // exec
            intr_realmode(regs,$21);
            // swapvectors
            InstallDpmi32Exceptionhandlers;

            for I := 0 to 2 do
              if StdHandles[I] <> -1 then
                begin
                  DPMI32DupHandle(OldHandles[I], NewHandles[I]);
                  SysFileClose(OldHandles[I]);
                end;


            freedosmem(env_low16);

            if (regs.flags_ and flags_carry)<>0 then // ERROR
              begin
                SysExecute:=regs.ax_;
                last_exitcode:=-1;
              end
            else
              begin
                SysExecute:=0;
                // determine exitcode
                {$Alters eax,ecx}
                asm
                  mov ah,$4d
                  int $21
                  movzx ecx,ax // hi(ax)=0/1/2/3 lo(ax)=exit
                  push offset last_exitcode
                  call _GetTlsVar
                  mov [eax],ecx
                end;
              end
          end; (* else getdosmem(env_low16) *)
      end; (* else async *)
  end;

function SysExitCode: Longint;
  begin
    SysExitCode := last_exitcode;
  end;

const
  max_sem               =100;
  max_sem_namelen       =80;
  mutexsem_sig          =Ord('M')
                        +Ord('U') shl 8
                        +Ord('T') shl 16
                        +Ord('X') shl 24;
  eventsem_sig          =Ord('E')
                        +Ord('V') shl 8
                        +Ord('S') shl 16
                        +Ord('E') shl 24;

type
  sem_types             =(sem_type_unused,sem_type_mutex,sem_type_event);

  mutexsem_type=
    packed record
      sig               :Longint;
      status            :Longint;
      eigentuemer       :Longint;
    end;

  eventsem_type=
    packed record
      sig               :Longint;
      status            :Longint;
    end;


  mutexsem_z_type       =^mutexsem_type;
  eventsem_z_type       =^eventsem_type;


var
  sem_array                     :
    array[1..max_sem] of
      record
        sem_name                :array[0..max_sem_namelen] of Char;
        opencount               :Longint;
        case used:sem_types of
          sem_type_unused       :(private_data:byte  );
          sem_type_mutex        :(mutex:mutexsem_type);
          sem_type_event        :(event:eventsem_type);
        end;

const
  sem_init_complete             :boolean=false;

procedure init_semaphore_table;
  begin
    if sem_init_complete then Exit;

    FillChar(sem_array,SizeOf(sem_array),0);
    sem_init_complete:=true;
  end;



function Search_MutexSem(_Name:pChar):Longint;
  var
    i:Longint;
  begin
    Result:=0;
    if StrLen(_Name)=0 then Exit;

    for i:=Low(sem_array) to High(sem_array) do
      with sem_array[i] do
        if used=sem_type_mutex then
          if StrComp(sem_name,_Name)=0 then
            begin
              Result:=i;
              Exit;
            end;
  end;

function Search_EventSem(_Name:pChar):Longint;
  var
    i:Longint;
  begin
    Result:=0;

    i:=StrLen(_Name);
    if (i=0) or (i>max_sem_namelen) then Exit;

    for i:=Low(sem_array) to High(sem_array) do
      with sem_array[i] do
        if used=sem_type_event then
          if StrComp(sem_name,_Name)=0 then
            begin
              Result:=i;
              Exit;
            end;
  end;

function Search_Unused_Sem:Longint;
  var
    i:Longint;
  begin
    Result:=0;
    for i:=Low(sem_array) to High(sem_array) do
      if sem_array[i].used=sem_type_unused then
        begin
          Result:=i;
          Exit;
        end;
  end;


function Search_Used_Sem(const _Handle: TSemhandle):Longint;
  var
    i:Longint;
  begin
    Result:=0;
    for i:=Low(sem_array) to High(sem_array) do
      if Ofs(sem_array[i].private_data)=_Handle then
        begin
          Result:=i;
          Exit;
        end;
  end;

procedure Free_Sem(const i:Longint);
  begin
    FillChar(sem_array[i],SizeOf(sem_array[i]),0);
  end;


(*** Sem*Event *****************************************************)

function SemCreateEvent(_Name: pChar; _Shared, _State: Boolean): TSemHandle;
  var
    i:Longint;
  begin
    Result:=-1;

    // already exist ?
    if Search_EventSem(_Name)<>0 then Exit;

    // get free entry
    i:=Search_Unused_Sem;
    if i=0 then Exit;

    with sem_array[i] do
      begin
        if _Name<>nil then
          StrCopy(sem_name,_Name);
        opencount:=1;
        used:=sem_type_event;
        event.sig:=eventsem_sig;
        event.status:=Ord(_State);
        Result:=Ofs(event);
      end;

  end;

function SemAccessEvent(_Name: pChar): TSemHandle;
  var
    i:Longint;
  begin
    Result:=-1;

    // already exist ?
    i:=Search_EventSem(_Name);
    if i=0 then Exit;


    with sem_array[i] do
      begin
        SysCtrlEnterCritSec;
        Inc(opencount);
        SysCtrlLeaveCritSec;
        Result:=Ofs(event);
      end;
  end;

function SemPostEvent(_Handle: TSemhandle): Boolean;
  begin
    with eventsem_z_type(_Handle)^ do
      begin
        if sig<>eventsem_sig then
          RunError(204);

        SysCtrlEnterCritSec;

        Inc(status);

        SysCtrlLeaveCritSec;

        Result:=true;
      end;
  end;

function SemWaitEvent(_Handle: TSemHandle; _TimeOut: Longint): Boolean;
  var
    t0,t1:Longint;
  begin
    with eventsem_z_type(_Handle)^ do
      begin
        if sig<>eventsem_sig then
          RunError(204);

        t0:=SysSysMsCount;


        repeat
          SysCtrlEnterCritSec;

          if status>0 then
            begin
              Dec(status);
              SysCtrlLeaveCritSec;
              Result:=true;
              Exit;
            end;

          SysCtrlLeaveCritSec;

          Dpmi32MultiThread;

          t1:=SysSysMsCount;

          if t1<t0 then Inc(t1,$1800b0);
        until (_TimeOut<>-1) and (t0+_TimeOut>=t1);
      end;

    Result:=false;
  end;

function SemResetEvent(_Handle: TSemhandle; var _PostCount: Longint): Boolean;
begin
  // Not implemented
  Result := True;
  _PostCount := 0;
end;

procedure SemCloseEvent(_Handle: TSemHandle);
  var
    i:Longint;
  begin
    i:=Search_Used_Sem(_Handle);
    if i=0 then
      RunError(204);

    with eventsem_z_type(_Handle)^ do
      begin
        if sig<>eventsem_sig then
          RunError(204);

        SysCtrlEnterCritSec;

        with sem_array[i] do
          begin
            Dec(opencount);
            if opencount=0 then
              Free_Sem(i);
          end;

        SysCtrlLeaveCritSec;
      end;
  end;

(*** Sem*Mutex *****************************************************)

function SemCreateMutex(_Name: PChar; _Shared, _State: Boolean): TSemHandle;
  var
    i:Longint;
  begin
    Result:=-1;

    // already exist ?
    if Search_MutexSem(_Name)<>0 then Exit;

    // get free entry
    i:=Search_Unused_Sem;
    if i=0 then Exit;

    with sem_array[i] do
      begin
        if _Name<>nil then
          StrCopy(sem_name,_Name);
        opencount:=1;
        used:=sem_type_mutex;
        mutex.sig:=mutexsem_sig;
        mutex.status:=Ord(_State);
        mutex.eigentuemer:=SysGetThreadId;
        Result:=Ofs(mutex);
      end;

  end;

function SemAccessMutex(_Name: PChar): TSemHandle;
  var
    i:Longint;
  begin
    init_semaphore_table; // called by init_exe...

    Result:=-1;

    // already exist ?
    i:=Search_MutexSem(_Name);
    if i=0 then Exit;


    with sem_array[i] do
      begin
        SysCtrlEnterCritSec;
        Inc(opencount);
        SysCtrlLeaveCritSec;
        Result:=Ofs(mutex);
      end;
  end;

function SemRequestMutex(_Handle: TSemHandle; _TimeOut: Longint): Boolean;
  var
    t0,t1:Longint;
  begin
    with mutexsem_z_type(_Handle)^ do
      begin
        if sig<>mutexsem_sig then
          RunError(204);

        t0:=SysSysMsCount;


        repeat
          SysCtrlEnterCritSec;

          if (status=0) or (eigentuemer=SysGetThreadId) then
            begin
              Inc(status);
              eigentuemer:=SysGetThreadId;
              SysCtrlLeaveCritSec;
              Result:=true;
              Exit;
            end;

          SysCtrlLeaveCritSec;

          Dpmi32MultiThread;

          t1:=SysSysMsCount;

          if t1<t0 then Inc(t1,$1800b0);
        until (_TimeOut<>-1) and (t0+_TimeOut>=t1);
      end;

    Result:=false;
  end;

function SemReleaseMutex(_Handle: TSemHandle): Boolean;
  begin
    with mutexsem_z_type(_Handle)^ do
      begin
        if sig<>mutexsem_sig then
          RunError(204);

        SysCtrlEnterCritSec;

        if status=0 then
          Result:=false
        else
          begin
            Dec(status);        // release ..
            Result:=true;
          end;

        SysCtrlLeaveCritSec;
      end;
  end;

procedure SemCloseMutex(_Handle: TSemHandle);
  var
    i:integer;
  begin
    i:=Search_Used_Sem(_Handle);
    if i=0 then
      RunError(204);

    with mutexsem_z_type(_Handle)^ do
      begin
        if sig<>mutexsem_sig then
          RunError(204);

        SysCtrlEnterCritSec;

        with sem_array[i] do
          begin
            Dec(opencount);
            if opencount=0 then
              Free_Sem(i);
          end;

        SysCtrlLeaveCritSec;
      end;
  end;

function SysMemInfo(_Base: Pointer; _Size: Longint; var _Flags: Longint): Boolean;
  begin
    // not implemented
    SysMemInfo := false;
  end;

function SysSetMemProtection(_Base: Pointer; _Size: Longint; _Flags: Longint): Boolean;
  begin
    // not implemented
    SysSetMemProtection := false;
  end;

procedure SysMessageBox(_Msg, _Title: PChar; _Error: Boolean);
  begin
    SysDisplayConsoleError(true, _Title, _Msg);
  end;

function SysGetVolumeLabel(Drive: Char): ShortString;
  var
    sr                  :TOSSearchRec;
    root                :array[0..6] of char;
  const
    suchmaske=$0800  // must  volume
             +$0008  // allow volume
//             +$0020  // allow archive
// 'allow archive' commented by piwamoto
// it fixes searching on network drives
             ;
  begin
    root:='@:\*.*'#0;
    root[0]:=drive;

    if SysFindFirst(root,suchmaske,sr,false)=0 then
      begin
        SysGetVolumeLabel:=sr.name;
        if Length(Result)>=9 then
          if Result[9]='.' then
            Delete(Result,9,1);
      end
    else
      SysGetVolumeLabel:='';
  end;

function SysSetVolumeLabel(Drive: Char; _Label: ShortString): Boolean;
  var
    OrgDir              :string;        // current directory of <Drive>
    OrgDrive            :string;        // current drive and diretory
    rc                  :Longint;
    regs                :real_mode_call_structure_type;

  type
    extended_fcb=
      packed record
        sign_ext        :byte; // $ff
        res01           :array[1..5] of byte;
        attr            :byte;
        drive_number    :byte; // 1=A
        filename_ext    :array[1..8+3] of char;
        rest            :array[1..25] of byte;
      end;


  begin
    SysSetVolumeLabel:=false;
    Drive:=UpCase(Drive);
    if Length(_Label)>8+3 then Exit;

    // 1. get old drive and directory
    GetDir(0                    ,OrgDrive);
    GetDir(Ord(Drive)-Ord('A')+1,OrgDir  );

    // 2. go to root
    ChDir(Drive+':\');

    // 3. delete old label
    FillChar(extended_fcb(Mem[segdossyslow32]),SizeOf(extended_fcb),0);
    with extended_fcb(Mem[segdossyslow32]) do
      begin
        sign_ext        :=$ff;
        attr            :=$08; // directory mask
        drive_number    :=Ord(Drive)-Ord('A')+1;
        filename_ext    :='????????'+'???';
      end;

    with regs do
      begin
        ah_:=$13;                       // DELETE FILE USING FCB
        ds_:=segdossyslow16;            // DS:DX
        dx_:=0;
      end;
    intr_realmode(regs,$21);


    // 4. create new label
    FillChar(extended_fcb(Mem[segdossyslow32]),SizeOf(extended_fcb),0);
    with extended_fcb(Mem[segdossyslow32]) do
      begin
        sign_ext        :=$ff;
        attr            :=$08; // directory bit
        drive_number    :=Ord(Drive)-Ord('A')+1;
        filename_ext    :='        '+'   ';
        Move(_Label[1],filename_ext,Length(_Label));
      end;

    with regs do
      begin
        ah_:=$16;                       // CREATE OR TRUNCATE FILE USING FCB
        ds_:=segdossyslow16;            // DS:DX
        dx_:=0;
        intr_realmode(regs,$21);
        SysSetVolumeLabel:=(al_=0) or (_Label='')
      end;


    // 5. restore directory and drive
    ChDir(OrgDir  );
    ChDir(OrgDrive);

  end;

function SysGetForegroundProcessId: Longint;
  begin
    // not implemented
    SysGetForegroundProcessId := -1;
  end;

function SysGetBootDrive: Char;
  {&Frame-}{&Uses edx}
  asm
    mov eax,$3305
    mov dl,1
    int $21
    add dl,'A'-1
    movzx eax,dl
  end;


//  TDriveType = ( dtFloppy, dtHDFAT, dtHDHPFS, dtInvalid,
//                 dtNovellNet, dtCDRom, dtLAN, dtHDNTFS, dtUnknown,
//                 dtTVFS, dtHDExt2, dtJFS );

function SysGetDriveType(Drive: Char): TDriveType;assembler;
  {&Frame-}{&Uses ecx,esi,edi,edx}
  var
    regs                :real_mode_call_structure_type;
  asm
    push Longint [Drive]
    call _upcase
    sub al,'A'
    movzx esi,al        // esi=drive0 ( // 0=A: ... )

    lea edi,regs        // edi=@regs

    // ******************************* Valid Drive ?
    mov ah,$19  // Get Current Default Drive -> al
    int $21
    push eax

    mov ah,$0e  // Select Disk
    mov edx,esi // dl:=Drive
    int $21

    mov ah,$19  // Get Current Default Drive
    int $21
    cmp al,dl   // result is in flags.ZF

    pop edx     // restore current drive..
    pushf       // but remember compare result

    mov ah,$0e  // Select Disk
  //mov edx,edx // dl:=Drive
    int $21

    popf        // restore compare result again
    mov eax,dtInvalid
    jne @ret    // not selectable -- return dtInvalid

    // drive letter is valid.
    // ******************************* CD-ROM ?
    // INT $2F,AX=$150b
    mov [regs.ax_],$150b
    mov [regs.bx_],$eeee
    mov [regs.ecx_],esi // drive0

    push edi
    push $2f
    call intr_realmode

    cmp [regs.bx_],$eeee
    je @not_cdrom

    cmp [regs.ax_],0    // 0=unsupported
    je @not_cdrom

    mov eax,dtCDRom
    jmp @ret

  @not_cdrom:
    // ******************************* NOVELL NETWARE
    // INT $21,AX=$EF01 .. Novell NetWare - WORKSTATION - GET DRIVE FLAG TABLE
    mov [regs.ax_],$ef01

    push edi
    push $21
    call intr_realmode

    cmp [regs.ax_],0
    jne @not_novell_netware

    // ES:SI -> network shell's 32-byte drive flag table
    movzx eax,[regs.ds_]
    shl eax,4           // dosseg_linear
    movzx ecx,[regs.si_]
    add ecx,eax
    add ecx,esi         // drive0
    cmp byte [ecx],$00  // drive is not mapped
    je @not_novell_netware
    cmp byte [ecx],$80  // mapped to local drive
    je @not_novell_netware

    mov eax,dtNovellNet
    jmp @ret

  @not_novell_netware:
    // ******************************* GENERIC NETWORK
    // INT $21,AX=$4409
    mov [regs.ax_],$4409 // CHECK IF BLOCK DEVICE REMOTE
    lea eax,[esi+1]
    mov [regs.bl_],al    // drive: A:=1 ..
    and [regs.edx_],0

    push edi
    push $21
    call intr_realmode
    test [regs.flags_],(1 shl 0) // Carry
    jnz @not_remote_drive
    test [regs.edx_],(1 shl 12) // Bit 12 =1  -> remote
    jz @not_remote_drive

    mov eax,dtLAN
    jmp @ret

  @not_remote_drive:
    // ************************* check removable drive
    mov [regs.ax_],$4408 // CHECK IF BLOCK DEVICE REMOVABLE
    lea eax,[esi+1]
    mov [regs.bl_],al   // drive: A:=1 ..
    and [regs.edx_],0
    push edi
    push $21
    call intr_realmode
    mov eax,dtFloppy
    test [regs.flags_],(1 shl 0) // Carry
    jnz @ret
    cmp [regs.ax_],1    // ax=1: fixed
    jne @ret

    mov eax,dtHDFAT     // anything that is local, harddisk

  @ret:
  end;

function SysGetSystemSettings: Longint;
  {&Frame-}{&Uses None}
  asm
    mov ah,$09 // GET KEYBOARD FUNCTIONALITY
    int $16    // al Bit 5
    shr al,5
    and eax,1
  end;

const
  CrtScanCode: Byte = 0;

function SysKeyPressed: Boolean;
  {&Frame-}{&Uses edx}
  asm
    call Test_Ctrl_C_Sensed
    {$IfDef THREAD_SUPPORT}
    call Dpmi32MultiThread
    {$EndIf}

    mov dl,true
    cmp CrtScanCode,0
    jne @ret

    mov ah,$11
    int $16
    jnz @ret

    mov dl,false

  @ret:
    mov al,dl
  end;

procedure SysFlushKeyBuf;
  {&Frame-}{&Uses eax}
  asm
  @loop_flush:

    call Test_Ctrl_C_Sensed
    call SysKeyPressed
    cmp al,false
    je @ret

    call SysReadKey
    jmp @loop_flush

  @ret:
    {$IfDef THREAD_SUPPORT}
    call Dpmi32MultiThread
    {$EndIf}
  end;

function SysReadKey: Char;
  var
    scan                :byte;
    ascii               :char;
  begin
    Test_Ctrl_C_Sensed;

    If CrtScanCode <> 0 then
      begin
        SysReadKey:=Chr(CrtScanCode);
        CrtScanCode:=0;
      end
    else
      begin
        {$IfDef THREAD_SUPPORT}
        if IsMultiThread then
          while not SysKeyPressed do
            Dpmi32MultiThread;
        {$EndIf}

        asm {$Alters EAX}
        @again:
          mov ah,$10
          int $16
          mov scan,ah
          mov ascii,al
          cmp ax,$2e03 // ^C
          jne @not_ctrl_c

          mov Ctrl_C_Sensed,true
          call Test_Ctrl_C_Sensed
          jmp @again

        @not_ctrl_c:
        end;

        case ascii of
          #$00:CrtScanCode:=scan;
          #$E0:           {   Up, Dn, Left Rt Ins Del Home End PgUp PgDn C-Home C-End C-PgUp C-PgDn C-Left C-Right C-Up C-Dn }
            if scan       in [$48,$50,$4B,$4D,$52,$53,$47, $4F,$49, $51, $77,   $75,  $84,   $76,   $73,   $74,    $8D, $91] then
            begin
              CrtScanCode:=scan;
              ascii:=#0;
            end;
        end;
        SysReadKey:=ascii;
      end;
    end;

// VPUTILS.PeekKey
function SysPeekKey(Var Ch:Char):boolean;
  var
    scan                :byte;
    ascii               :char;
    keypress_avail      :boolean;
  begin
    Test_Ctrl_C_Sensed;
    {$IfDef THREAD_SUPPORT}
    Dpmi32MultiThread;
    {$EndIf}

    // 2*ReadKey for "F1"
    if CrtScanCode<>0 then
      begin
        Ch:=chr(CrtScanCode);
        SysPeekKey:=true;
        Exit;
      end;

    asm {$Alters EAX}
      mov ah,$11
      int $16
      setnz keypress_avail
      mov scan,ah
      mov ascii,al
    end;

    if not keypress_avail then
      begin
        SysPeekKey:=false;
        Exit;
      end;

    case ascii of
      #$E0:           {   Up, Dn, Left Rt Ins Del Home End PgUp PgDn C-Home C-End C-PgUp C-PgDn C-Left C-Right C-Up C-Dn }
        if scan       in [$48,$50,$4B,$4D,$52,$53,$47, $4F,$49, $51, $77,   $75,  $84,   $76,   $73,   $74,    $8D, $91] then
          ascii:=#0;
    end;

    Ch:=ascii;
    SysPeekKey:=true;

  end;

const
  video_page_0=0;

procedure SysGetCurPos(var X, Y: SmallWord);
  {&Frame-}{&Uses eax,esi}
  asm
    movzx eax,byte [Seg0040+$50+2*video_page_0]
    mov esi,[x]
    mov [esi],ax
    movzx eax,byte [Seg0040+$51+2*video_page_0]
    mov esi,[y]
    mov [esi],ax
  end;

procedure SysSetCurPos(X,Y: SmallWord);
  {&Frame-}{&Uses eax,ebx,edx}
  asm
    mov ah,$02
    mov bh,video_page_0
    mov dh,byte [x]
    mov dl,byte [y]
    int $10
  end;

procedure SysWrtCharStrAtt(CharStr: Pointer; Len,X,Y: SmallWord; var Attr: Byte);
  {&Frame-}{&Uses All}
  asm
    call Test_Ctrl_C_Sensed

    movzx ecx,[Len]
    jecxz @ret

    push [X+0].Longint
      push [Y+4].Longint
        call calculate_video_address
    mov edi,eax
    mov esi,[Attr]
    mov ah,[esi]
    mov esi,[CharStr]
    cld

  @l1:

    lodsb
    stosw
    loop @l1

  @ret:

  end;

procedure SysScrollUp(X1,Y1,X2,Y2,Lines,Cell: SmallWord);
  {&Frame-}{&Uses All}
  asm
    movzx eax,[Lines]
    // nothing to do?
    test eax,eax
    jz @ret
    // 32 Bit-expand Lines
    mov [Lines].Longint,eax

    // size(bytes) of a screen line in video memory
    call SysGetTextVideoColumns
    shl eax,1 // *2
    mov ebp,eax

    // target row
    push [X1].Longint
      push [Y1+4].Longint
        call calculate_video_address
    mov edi,eax

    // count 16 Bit words per line
    mov eax,[X2].Longint
    sub eax,[X1].Longint
    inc eax
    movzx ecx,ax

    // number of lines of window
    mov eax,[Y2].Longint
    sub eax,[Y1].Longint
    inc eax
    movzx ebx,ax

    // scroll more than avilable lines?
    mov eax,[Lines].Longint
    cmp ebx,eax
    jae @no_correcture
    mov eax,ebx
    mov [Lines].Longint,eax

  @no_correcture:

    // read position
    // eax=[Lines]
    mul ebp
    lea esi,[edi+eax]

    // scroll needed?
    mov eax,ebx
    sub eax,[Lines].Longint     // eax:=number of lines to move (if <0 then 0)



    // eax = number of lines to move
    // ecx = number of SmallWord to move per line
    // esi/edi = source/target of move
    // ebx = size of video window

    // ebp = number of byte until next start of window
    sub ebp,ecx
    sub ebp,ecx

    cld

  @move_loop:
    cmp eax,0           // <=0 ?
    jle @do_fill

    push ecx
      push ecx
        shr ecx,1
        rep movsd
      pop ecx
      and ecx,1
      rep movsw
    pop ecx
    add esi,ebp
    add edi,ebp
    dec eax             // Anzahl Zeilen zu verschieben
    dec ebx             // Anzahl Zeilen Бbrig zu fБllen
    jmp @move_loop

  @do_fill:
    mov ax,[Cell]
    shl eax,16
    mov ax,[Cell]

  @fill_loop:
    cmp ebx,0
    jle @break_fill_loop

    push ecx
      push ecx
        shr ecx,1
        rep stosd
      pop ecx
      and ecx,1
      rep stosw
    pop ecx
    add edi,ebp
    dec ebx
    jmp @fill_loop

  @break_fill_loop:
  @ret:
end;

procedure SysScrollDn(X1,Y1,X2,Y2,Lines,Cell: SmallWord);
  {&Frame-}{&Uses All}
  asm
    movzx eax,[Lines]
   // nothing to do?
    test eax,eax
    jz @ret
    // 32 Bit-expand Lines
    mov [Lines].Longint,eax

    // size(bytes) of a screen line in video memory
    call SysGetTextVideoColumns
    shl eax,1 // *2
    mov ebp,eax

    // target row
    push [X1].Longint
      push [Y2+4].Longint
        call calculate_video_address
    mov edi,eax

    // count 16 Bit words per line
    mov eax,[X2].Longint
    sub eax,[X1].Longint
    inc eax
    movzx ecx,ax

    // number of lines of window
    mov eax,[Y2].Longint
    sub eax,[Y1].Longint
    inc eax
    movzx ebx,ax

    // scroll more than avilable lines?
    mov eax,[Lines].Longint
    cmp ebx,eax
    jae @no_correcture
    mov eax,ebx
    mov [Lines].Longint,eax

  @no_correcture:

    // read position
    // eax=[Lines]
    mul ebp
    mov esi,edi
    sub esi,eax

    // scroll needed?
    mov eax,ebx
    sub eax,[Lines].Longint     // eax:=number of lines to move (if <0 then 0)



    // eax = number of lines to move
    // ecx = number of SmallWord to move per line
    // esi/edi = source/target of move
    // ebx = size of video window

    // ebp = number of byte until next start of window
    add ebp,ecx
    add ebp,ecx

    cld

  @move_loop:
    cmp eax,0           // <=0 ?
    jle @do_fill

    push ecx
      push ecx
        shr ecx,1
        rep movsd
      pop ecx
      and ecx,1
      rep movsw
    pop ecx
    sub esi,ebp
    sub edi,ebp
    dec eax             // Anzahl Zeilen zu verschieben
    dec ebx             // Anzahl Zeilen Бbrig zu fБllen
    jmp @move_loop

  @do_fill:
    mov ax,[Cell]
    shl eax,16
    mov ax,[Cell]

  @fill_loop:
    cmp ebx,0
    jle @break_fill_loop

    push ecx
      push ecx
        shr ecx,1
        rep stosd
      pop ecx
      and ecx,1
      rep stosw
    pop ecx
    sub edi,ebp
    dec ebx
    jmp @fill_loop

  @break_fill_loop:
  @ret:

  end;


type
  TCharCaseTable = array[0..255] of Char;

var
  UpperCaseTable: TCharCaseTable;
  LowerCaseTable: TCharCaseTable;
  AnsiUpperCaseTable: TCharCaseTable;
  AnsiLowerCaseTable: TCharCaseTable;
  WeightTable: TCharCaseTable;

const
  CaseTablesInitialized: Boolean = False;

procedure InitCaseTables;
var
  I,J: Integer;
begin
  for I := 0 to 255 do
  begin
    UpperCaseTable[I] := Chr(I);
    LowerCaseTable[I] := Chr(I);
    AnsiUpperCaseTable[I] := Chr(I);
    AnsiLowerCaseTable[I] := Chr(I);
    if I in [Ord('A')..Ord('Z')] then
      LowerCaseTable[I] := Chr(I + (Ord('a')-Ord('A')));
    if I in [Ord('a')..Ord('z')] then
      UpperCaseTable[I] := Chr(I - (Ord('a')-Ord('A')));
  end;
  SysGetCaseMap(SizeOf(AnsiUpperCaseTable), AnsiUpperCaseTable);
  for I := 255 downto 0 do
  begin
    J := Ord(AnsiUpperCaseTable[I]);
    if (J <> I) {and (AnsiLowerCaseTable[J] <> chr(J))} then
      AnsiLowerCaseTable[J] := Chr(I);
  end;
  SysGetWeightTable(SizeOf(WeightTable), WeightTable);
  CaseTablesInitialized := True;
end;

procedure ConvertCase(S1,S2: PChar; Count: Integer; var Table: TCharCaseTable);
{&USES esi,edi} {&FRAME-}
asm
                cmp     CaseTablesInitialized,0
                jne     @@1
                Call    InitCaseTables
              @@1:
                xor     eax,eax
                mov     esi,S1
                mov     edi,S2
                mov     ecx,Count
                mov     edx,Table
                jecxz   @@3
              @@2:
                dec     ecx
                mov     al,[esi+ecx]
                mov     al,[edx+eax]
                mov     [edi+ecx],al
                jnz     @@2
              @@3:
end;

procedure SysChangeCase(Source, Dest: PChar; Len: Longint; NewCase: TCharCase);
begin
  case NewCase of
    ccLower:     ConvertCase(Source, Dest, Len, LowerCaseTable);
    ccUpper:     ConvertCase(Source, Dest, Len, UpperCaseTable);
    ccAnsiLower: ConvertCase(Source, Dest, Len, AnsiLowerCaseTable);
    ccAnsiUpper: ConvertCase(Source, Dest, Len, AnsiUpperCaseTable);
  end;
end;

function SysLowerCase(s: PChar): PChar;
begin
  ConvertCase(s, s, strlen(s), AnsiLowerCaseTable);
  Result := s;
end;

function SysUpperCase(s: PChar): PChar;
begin
  ConvertCase(s, s, strlen(s), AnsiUpperCaseTable);
  Result := s;
end;

function MemComp(P1,P2: Pointer; L1,L2: Integer; T1,T2: PChar): Integer;
  {&USES ebx,esi,edi,ebp} {&FRAME-}
asm
                cmp     CaseTablesInitialized,0
                jne     @@0
                Call    InitCaseTables
              @@0:
                mov     ecx,L1
                mov     eax,L2
                mov     esi,P1
                mov     edi,P2
                cmp     ecx,eax
                jbe     @@1
                mov     ecx,eax
              @@1:
                mov     ebx,T1
                mov     ebp,T2
                xor     eax,eax
                xor     edx,edx
                test    ecx,ecx
                jz      @@5
              @@2:
                mov     al,[esi]
                mov     dl,[edi]
                inc     esi
                inc     edi
                test    ebp,ebp
                mov     al,[ebx+eax]    // Table1
                mov     dl,[ebx+edx]
                jz      @@3
                mov     al,[ebp+eax]    // Table2
                mov     dl,[ebp+edx]
              @@3:
                cmp     al,dl
                jne     @@RET
                dec     ecx
                jnz     @@2
              @@5:
                mov     eax,L1
                mov     edx,L2
              @@RET:
                sub     eax,edx
end;

function SysCompareStrings(s1, s2: PChar; l1, l2: Longint; IgnoreCase: Boolean): Longint;
begin
  if IgnoreCase then
    Result := MemComp(s1, s2, l1, l2, PChar(@WeightTable), nil)
  else
    Result := MemComp(s1, s2, l1, l2, PChar(@AnsiUpperCaseTable), PChar(@WeightTable));
end;

procedure SysGetCaseMap(TblLen: Longint; Tbl: PChar );assembler;
  {&Frame-}{&Uses eax,ecx,esi,edi}
  var
    regs                :real_mode_call_structure_type;
  asm
    // DOS 3.3+
    mov [regs.ax_],$6502        // get pointer to uppercase table
    or eax,-1                   // ax:=$ffff
    mov [regs.ebx_],eax         // code page (FFFFh=global code page)
    mov [regs.edx_],eax         // country ID (FFFFh=current country)
    mov ax,segdossyslow16
    mov [regs.es_],ax
    and [regs.edi_],0           // -> country information buffer
    mov [regs.cx_],1+4          // size of buffer

    lea eax,regs
    push eax
    push $21
    call intr_realmode

    mov edi,[segdossyslow32]

    //  00  byte        info ID
    //  01  DWORD       pointer to uppercase table

    //  00  word        table size ($80)
    //  02  128 byte    upper case equivalents

    movzx eax,SmallWord [edi+1+2]
    // 16:16->32 Bit
    shl eax,4
    movzx esi,SmallWord [edi+1+0]
    lea esi,[eax+esi+2]

    // process Tbl ..
    mov ecx,TblLen
    mov edi,Tbl
    sub eax,eax
    sub esi,128
    jecxz @ret

    cld

  @Tbl_loop:

    mov al,[edi]
    cmp al,$80
    jb @ascii

    mov al,[esi+eax]
    mov [edi],al
    jmp @continue

  @ascii:
    cmp al,'a'
    jb @continue

    cmp al,'z'
    ja @continue

    sub al,'a'-'A'

  @continue:
    stosb
    loop @Tbl_loop

  @ret:
  end;

procedure SysGetWeightTable(TblLen: Longint; WeightTable: PChar);assembler;
  {&Frame-}{&Uses eax,ecx,esi,edi}
  var
    regs                :real_mode_call_structure_type;
  asm
    // DOS 3.3+
    mov [regs.ax_],$6506        // get pointer to collating sequence table
    or eax,-1                   // ax:=$ffff
    mov [regs.ebx_],eax         // code page (FFFFh=global code page)
    mov [regs.edx_],eax         // country ID (FFFFh=current country)
    mov ax,segdossyslow16
    mov [regs.es_],ax
    and [regs.edi_],0           // -> country information buffer
    mov [regs.cx_],1+4          // size of buffer

    lea eax,regs
    push eax
    push $21
    call intr_realmode

    mov edi,[segdossyslow32]
    movzx eax,SmallWord [edi+1+2]  //   01h    DWORD   pointer to collating table

    // 16:16->32 Bit
    shl eax,4
    // copy
    movzx esi,SmallWord [edi+1+0]
    lea esi,[eax+esi+2]
    mov edi,WeightTable
    mov ecx,TblLen
    cld
    rep movsb
  end;

// copy from VpSysOS2.Pas
var
  PrevXcptProc: Pointer = Ptr(-1);

function SignalHandler(Report:       PExceptionReportRecord;
                       Registration: PExceptionRegistrationRecord;
                       Context:      PContextRecord;
                       P:            Pointer): Longint; stdcall;
  begin
    Result := xcpt_Continue_Search;
    if Report^.ExceptionNum = xcpt_Ctrl_Break then
      if Assigned(CtrlBreakHandler) and CtrlBreakHandler then
        Result := xcpt_Continue_Execution;
    XcptProc := PrevXcptProc;
  end;

procedure SysCtrlSetCBreakHandler;
  begin
    if PrevXcptProc=Ptr(-1) then
      begin
        PrevXcptProc:=XcptProc;
        XcptProc := @SignalHandler;
      end;
  end;


const
  big_jft16             :SmallWord=0;
  big_jft32             :Longint  =0;

function SysFileIncHandleCount(Count: Longint): Longint;
  const
    count_255=255;
  begin
    Result:=0;

    //   PSP:$0018=Default JFT [20]
    //   DOS 3.X:
    //   PSP:$0032=SIZE(JFT)
    //   PSP:$0034=ADDR(JFT)

    if  (big_jft16=0)                   // not allocated
    and (MemW[seg_psp+$34]=$0018)       // not moved
     then
      begin
        if GetDosMem(big_jft16,count_255)=0 then
          begin
            big_jft32:=DosSeg_Linear(big_jft16);
            FillChar(Mem[big_jft32],count_255,$ff);

            // copy old table
            Move(Mem[DosSeg_Linear(MemW[seg_psp+$36])+MemW[seg_psp+$34]],Mem[big_jft32],20);

            // install new table
            MemW[seg_psp+$34]:=0;
            MemW[seg_psp+$36]:=big_jft16;
            MemW[seg_psp+$32]:=count_255;
          end
        else
          Result:=8;            // no memory
      end
    else
      Result:=1;                // 2. Call
  end;

function SysGetCodePage: Longint;assembler;
  {&Frame-}{&Uses ebx,edx}
  asm
    sub ebx,ebx

    mov eax,$6601               // DOS 3.3+ - GET GLOBAL CODE PAGE TABLE
    int $21

    mov eax,ebx
  end;

function SysReadAttributesAt(x,y: SmallWord): Byte;
  {&Frame-}{&Uses None}
  asm
    push [x+0].Longint
    push [y+4].Longint
    call calculate_video_address
    mov al,[eax+1]
  end;

function SysReadCharAt(x,y: SmallWord): Char;
  {&Frame-}{&Uses None}
  asm
    push [x+0].Longint
    push [y+4].Longint
    call calculate_video_address
    mov al,[eax]
  end;


procedure SysSound(freq:Longint);
  {&Frame-}{&Uses eax,edx}
  asm
    mov eax,$001234dd // $ffff * 18.2 -> 1.19318 Mhz
    sub edx,edx
    cmp [freq],edx
    jne @not_zero

    inc [freq]

  @not_zero:
    div [freq]

    push eax

      in al,$61                 // timer 2 speaker enable
      test al,$03
      jnz @already_enabled

      or al,$03
      out $61,al

  @already_enabled:

      // set counter 2,both low+high byte,sqare wave generator, 16 bit counter
      mov al,(2 shl 6)+(3 shl 4)+(3 shl 1)+(0 shl 0)
      out $43,al

    pop eax

    // counter low, high
    out $42,al
    mov al,ah
    out $42,al

  end;

procedure SysNoSound;
  {&Frame-}{&Uses eax}
  asm
    in al,$61
    and al,(not $03)            // timer 2 speaker disable
    out $61,al
  end;

procedure delay_loop;
  {&Frame-}{&Uses None}                 // EAX,EDI,EBX
  asm
  @delay_loop_2:

    out $ed,al                          // waste time
    dec eax
    jz @exit

    cmp ebx,[edi]                       // system clock change?
    je @delay_loop_2                    // no, wait

  @exit:
  end;

const // initialised because can be called before SysLowInit
  thousandth_part_loop  :Longint=1;

procedure SysCtrlSleep(Delay: Integer);assembler;
  {&Frame-}{&Uses All}
  const
    half_day=$1800b0 div 2;
  asm
    {$IfDef THREAD_SUPPORT}
    cmp IsMultiThread,true
    jne @singe_thread_code

    mov eax,[Delay]
    cdq
    mov ebx,55                          // 1/1000 s = 55 * 1/18 s
    div ebx
    mov esi,Seg0040+$006c               // MEML[$40:$6c]= 1/18 timer tick
    add eax,[esi]

  @still_wait:
    call Dpmi32MultiThread

    mov edx,[esi]
    sub edx,eax

    jae @ret                            // time limit expiried

    cmp edx,-half_day                   // more ticks than a half day ? missing

    ja @still_wait                      // no overflow
    jmp @ret

    //***************************************************************

  @singe_thread_code:
    {$EndIf}
    (*
    mov ah,$86
    mov ecx,[Delay]
    shl ecx,10                          //milli->mikro
    mov edx,ecx
    shr ecx,16
    stc
    int $15
    jmp @ret*)

    mov edi,Seg0040                     // [edi] does not change
    mov ebx,[edi]
    mov ecx,[Delay]                     // Milliseconds
    jecxz @ret

  @loop_milli:
    cmp Ctrl_C_Sensed,true
    je @ret

    mov eax,thousandth_part_loop
    call delay_loop
    loop @loop_milli

  @ret:

    cmp [Seg0040+$0070].Longint,0
    je @no_day_update

    sub eax,eax                         // ask BIOS to fix the time
    int $1a

  @no_day_update:
  end;

procedure Calibrate_Delayloop;
  {&Frame-}{&Uses All}
  asm
    mov eax,1
    mov edi,Seg0040+$6c                 // 1/18s timer ticker
    mov edx,[edi]
    // warm up..
    call delay_loop

  @wait_for_change:
    mov ebx,[edi]
    cmp ebx,edx
    je @wait_for_change

    sub eax,eax
    call delay_loop
    neg eax
    sub edx,edx
    mov ecx,55 // 1000 / 18.2
  //        56 // 1024 / 18.2
    div ecx
    test eax,eax
    jnz @g0
    inc eax
  @g0:
    mov thousandth_part_loop,eax
  end;


procedure SysBeepEx(Freq,Dur: Longint);
  {&Frame-}{&Uses None}
  asm
    push Freq
    call SysSound
    push Dur
    call SysCtrlSleep
    call SysNoSound
    call Test_Ctrl_C_Sensed
  end;

function SysGetVideoModeInfo( Var Cols, Rows, Colours : Word ): Boolean;
  {&Frame-}{&Uses esi,edi}
  asm
    call SysGetTextVideoColumns // Cols:=SysGetTextVideoColumns
    mov edi,Cols
    mov [edi],eax

    call SysGetTextVideoRows    // Rows:=SysGetTextVideoRows
    mov edi,Rows
    mov [edi],eax

    mov edi,Colours             // Colours:=16
    mov [edi].Longint,16        // Co80 -> 16 colours
                                // Bw80 -> 2 colours but blink/underline/inverse
    mov al,true
  end;


function SysGetVisibleLines( var Top, Bottom: Longint ): Boolean;
  {&Frame-}{&Uses edi}
  asm
    mov edi,Top
    sub eax,eax;inc eax         // Top:=1
    mov [edi],eax

    mov edi,Bottom
    call SysGetTextVideoRows    // Bottom:=number of rows
    mov [edi],eax

    mov al,True                 // never fails
  end;


function SysSetVideoMode(Cols, Rows: Word): Boolean;

  function SetVesaMode(ModeNumber, Rows: Longint ): Boolean;
    {&Frame+}{&Uses None}
    asm
      mov eax,ModeNumber

      pushad

      cmp ah,0                          // Mode 0..3/7?
      je @use_int10_00

      mov ebx,eax                       // bx:=ModeNumber (VESA)
      mov ax,$4f02
      mov ebx,ModeNumber
      int $10

      cmp ax,$004f                      // success+supported?
      jmp @done_mode

   @use_int10_00:

      // 350/400 scan lines...
      push eax

      // EGA 80x43 hack: Set 350 lines mode...
      cmp al,3
      jne @not_ega43
      cmp Rows,43
      jne @not_ega43
      mov ax,$1201                      // select 350 scan lines for next mode
      jmp @set_scanlines

   @not_ega43:
      mov ax,$1202                      // select 400 scan lines for next mode

   @set_scanlines:
      mov bl,$30
      int $10

      pop eax


    //mov ah,ah                         // ah=0 (set mode) al=mode number/bit7=0(clear screen)
      int $10
      call SysGetTextVideoMode          // check set mode result
      cmp al,Byte [ModeNumber]

   @done_mode:
      jnz @done_font                    // skip if mode failed

      call SysGetTextVideoRows          // already have correct Row count?
      cmp Rows, eax
      je @done_font                     // yes, no font loading needed.

      mov edx,eax                       // current Rows
      call SysGetTextFontHeight
      mul edx                           // => vertical resolution
      div Rows                          // => needed font size

      mov bl,-1                         // invalid
      cmp eax,8
      jne @not_set_RomFont8
      mov bl,$12
      jmp @set_font

   @not_set_RomFont8:
      cmp eax,14
      jne @not_set_RomFont14
      mov bl,$11
      jmp @set_font

   @not_set_RomFont14:
      cmp eax,16
      jne @done_font                    // no matching font size, exit with ZF=0
      mov bl,$14

   @set_font:
      mov ah,$11                        // char gen
      mov al,bl                         // subfunction
      mov bl,0                          // table to load into
      int $10

      call SysGetTextVideoRows          // correct Row count?
      cmp Rows, eax

   @done_font:

      popad

      sete al
    end;

  begin
    SysSetVideoMode:=false;

    if Cols=40 then
      SysSetVideoMode:=SetVesaMode($01,Rows)
    else
    if (Cols=80) and (Rows=25) and (video_adapter_found=mda_found) then
      SysSetVideoMode:=SetVesaMode($07,Rows)
    else
    if (Cols=80) and (Rows=60) then
      SysSetVideoMode:=SetVesaMode($108,Rows)
    else
    if Cols=80 then
      SysSetVideoMode:=SetVesaMode($03,Rows)
    else
    if Cols=132 then
      case Rows of
        25:SysSetVideoMode:=SetVesaMode($109,Rows);
        43:SysSetVideoMode:=SetVesaMode($10a,Rows);
        50:SysSetVideoMode:=SetVesaMode($10b,Rows);
        60:SysSetVideoMode:=SetVesaMode($10c,Rows);
      end;

  end;

procedure Detect_MdaCgaEgaVga;
  {&Frame-}{&Uses All}
  asm
    sub edx,edx                         // mda_found
    call SysGetTextVideoMode
    cmp al,7                            // 7=MDA/Hercules
    je @ret

    inc edx                             // cga_found
    mov ah,$12
    mov bl,$10
    int $10
    cmp bl,$10
    je @ret

    inc edx                             // ega_found
    mov ax,$1a00
    int $10
    cmp al,$1a
    jne @ret

    cmp bl,7 // VGA mono
    jl @ret

    cmp bl,8 // VGA colour
    ja @ret

    inc edx                             // vga_found

  @ret:
    {$IfOpt Z+}
    mov video_adapter_found,edx
    {$Else}
    mov video_adapter_found, dl
    {$EndIf}
  end;

const
  wheel_supported: Boolean = false;

function SysTVDetectMouse: Longint;
  {&Frame-}{&Uses ebx}
  asm

    // test ctmouse driver wheel support presence
    mov ax,$0011
    int $33
    cmp ax,'WM'
    jne @end_test_wm
    test cx,1
    jz @end_test_wm
    mov wheel_supported,true
@end_test_wm:

    mov ax,$0021 // Software Reset
    sub ebx,ebx
    int $33
    cmp bx,$ffff // $ffff -> 2
    jne @1

    mov ebx,2

  @1:
    mov eax,ebx // SysTVDetectMouse:=ebx
  end;


const
  // not implemented as an qeue: only one element
  mouse_event_avail     :boolean=false;

var
  mouse_event           :TSysMouseEvent;
  mouse_rmcbs           :real_mode_call_structure_type;
  mouse_rm_ptr          :real_mode_ptr_type;

procedure mouse_eventhandler;
  {&Frame-}{&Uses None}
  asm
    // DS:ESI = DOS SS:SP
    // ES:EDI = mouse_rmcbs

    push eax
      push ds

        // WDOSX 2000.07.13
        mov eax,ds
        lsl eax,eax
        cmp eax,esi
        ja @ntBugDone

        movzx esi,si
        movzx edi,di

      @ntBugDone:

        mov ds,cs:[seldata]

        // process it
        cmp mouse_event_avail,false
        jne @1

        cmp wheel_supported,false
        je @end_wheel
        movsx eax,es:[edi+real_mode_call_structure_type.bh_]
        imul eax,3 // 3 lines scroll
        add wheel_counter,eax
@end_wheel:

        mov mouse_event_avail,true

        call SysSysMsCount
        mov [mouse_event.smeTime   ],eax

        // Y
        mov eax,es:[edi+real_mode_call_structure_type.edx_]
        shl eax,16
        // X
        mov  ax,es:[edi+real_mode_call_structure_type.cx_]
        // y/8, x/8
        shr eax,3
        mov [mouse_event.smePos],eax

        // BUTTONS
        mov al,es:[edi+real_mode_call_structure_type.bl_]
        mov [mouse_event.smeButtons],al

      @1:

      pop ds
    pop eax

    // simulate RETF
    cld
    lodsw  // RETF:IP
    mov es:[edi+real_mode_call_structure_type.ip_],ax
    lodsw  // RETF:CS
    mov es:[edi+real_mode_call_structure_type.cs_],ax

    add es:[edi+real_mode_call_structure_type.sp_],2*2

    // REAL MODE
    iretd
  end;

procedure SysTVInitMouse(var X,Y: Integer);assembler;
  {&Frame-}{&Uses All}
  const
    mousemask           =$ffff;
  var
    regs                :real_mode_call_structure_type;
  asm
    mov mouse_event_avail,false

    // mouse_goto(0,0)
    mov ax,$0004
    sub ecx,ecx
    sub edx,edx
    int $33

    // initialize mouse_rm_ptr
    and mouse_rm_ptr,0

    mov eax,$0303 // ALLOCATE REAL MODE CALLBACK ADDRESS
    mov esi,offset mouse_eventhandler // DS:ESI
    mov edi,offset mouse_rmcbs        // ES:EDI
    push ds
      push cs
      pop ds
      int $31
    pop ds
    jc @dpmi_err

    // -> CX:DX
    mov [mouse_rm_ptr.ofs_],dx
    mov [mouse_rm_ptr.seg_],cx

    mov [regs.ax_],$000c        // SET ALTERNATE MOUSE USER HANDLER
    mov [regs.es_],cx
    mov [regs.edx_],edx
    mov [regs.cx_],mousemask    // call mask .. Bit 0..5

    lea eax,regs
    push eax
    push $33
    call intr_realmode

    @dpmi_err:

    // SysTVUpdateMouseWhere
    sub ecx,ecx // x
    sub edx,edx // y
    mov ax,$0003
    int $33
    mov edi,x
    shr ecx,3
    mov [edi],ecx
    mov edi,y
    shr edx,3
    mov [edi],edx
  end;

procedure SysTVDoneMouse(Close: Boolean);assembler;
  {&Frame-}{&Uses All}
  var
    regs                :real_mode_call_structure_type;
  asm
    cmp mouse_rm_ptr,0
    je @no_mouse

    mov [regs.ax_],$0021        // Software Reset

    lea eax,regs
    push eax
    push $33                    // Mouse driver
    call intr_realmode

    mov dx,[mouse_rm_ptr.ofs_]
    mov cx,[mouse_rm_ptr.seg_]
    mov eax,$0304 // FREE REAL MODE CALLBACK ADDRESS CX:DX
    int $31

    and mouse_rm_ptr,0

  @no_mouse:
  end;

procedure SysTVShowMouse;
  {&Frame-}{&Uses eax}
  asm
    sub eax,eax    // mov ax,$0001
    inc eax
    int $33
  end;

procedure SysTVHideMouse;
  {&Frame-}{&Uses eax}
  asm
    mov ax,$0002
    int $33
  end;

procedure SysTVUpdateMouseWhere(var X,Y: Integer);
  {&Frame-}{&Uses All}
  asm
    sub ecx,ecx // x
    sub edx,edx // y
    mov ax,$0003
    int $33
    mov edi,X
    shr ecx,3
    mov [edi],ecx
    mov edi,Y
    shr edx,3
    mov [edi],edx
  end;

function SysTVGetMouseEvent(var Event: TSysMouseEvent): Boolean;
  {&Frame-}{&Uses esi,edi}
  asm
    mov al,false
    cmp mouse_event_avail,false
    je @ret

    mov esi,Offset mouse_event
    mov edi,Event
    cld
    movsd // TSysMouseEvent.smeTime
    movsd // TSysMouseEvent.smePos
    movsb // TSysMouseEvent.smeButtons

    mov mouse_event_avail,false
    mov al,true

  @ret:
  end;

procedure SysTVKbdInit;
  begin
  end;

function SysTVGetKeyEvent(var Event: TSysKeyEvent): Boolean;
  {&Frame-}{&Uses edi}
  asm
    mov dl,false

    mov edi,Event
    sub eax,eax
    mov [edi+TSysKeyEvent.skeKeyCode],ax

    call SysTVGetShiftState
    mov [edi+TSysKeyEvent.skeShiftState],al

    mov ah,$11
    int $16
    jz @no_update

    mov ah,$10
    int $16
    mov [edi+TSysKeyEvent.skeKeyCode],ax
    mov dl,true

  @no_update:
    mov al,dl
  end;

function SysTVPeekKeyEvent(var Event: TSysKeyEvent): Boolean;
  {&Frame-}{&Uses edx,edi}
  asm
    mov dl,false
    mov edi,Event
    sub eax,eax
    mov [edi+TSysKeyEvent.skeKeyCode],ax

    call SysTVGetShiftState
    mov [edi+TSysKeyEvent.skeShiftState],al

    mov ah,$11
    int $16
    jz @no_update

    mov [edi+TSysKeyEvent.skeKeyCode],ax
    mov dl,true

  @no_update:
    mov al,dl
  end;

function SysTVGetShiftState: Byte;
  {&Frame-}{&Uses None}
  asm
    mov ah,$02
    int $16
  end;

procedure SysTVSetCurPos(X,Y: Integer);
  {&Frame-}{&Uses eax,ebx,edx}
  asm
    mov ah,$02
    mov bh,video_page_0
    mov dh,byte [Y]
    mov dl,byte [X]
    int $10
  end;

procedure SysTVSetCurType(Y1,Y2: Integer; Show: Boolean);
  begin

    if Y1<0 then Y1:=(-Y1*(SysGetTextFontHeight-1)) div 100;
    if Y2<0 then Y2:=(-Y2*(SysGetTextFontHeight-1)) div 100;

    asm {$Alters eax,ecx}
      mov ah,$01
      mov ch,byte [Y1]
      mov cl,byte [Y2]
      cmp Show,true
      je @1
      or ch,$20 // or cx,$2000
    @1:
      int $10
    end;
  end;

procedure SysTVGetCurType(var Y1,Y2: Integer; var Visible: Boolean);
  {&Frame-}{&Uses All}
  asm
    mov ah,$03
    mov bh,video_page_0
    int $10

    mov edi,Y1
    movzx eax,ch
    and eax,$1f  // Bit 4..0
    mov [edi],eax

    mov edi,Y2
    movzx eax,cl
    and eax,$1f  // Bit 4..0
    mov [edi],eax

    test ch,$20   // Bit 5(13) invisible
    setz al
    mov edi,Visible
    mov [edi],al
  end;

procedure SysTVShowBuf(Pos,Size: Integer);
  begin
  end;

procedure SysTVClrScr;
  {&Frame-}{&Uses eax,ecx,edi}
  asm
    call SysGetTextVideoMemBase
    mov edi,eax

    movzx ecx,SmallWord [Seg0040+$4c]   // Size Video MEM
    shr ecx,1                           // div 2
    cld
    mov ax,$0720                        // Space character, white on black
    rep stosw
  end;

function SysTVGetScrMode(_Size: PSysPoint; _Align: Boolean): Integer;
  begin
    if _Size <> nil then
      with _Size do
        begin
          X:=SysGetTextVideoColumns;
          Y:=SysGetTextVideoRows;
        end;

    if (SysGetTextVideoMode in [$00,$01,$02,$03,$07]) then
      begin
       if SysGetTextVideoRows=25 then           // default row count?
         SysTVGetScrMode:=SysGetTextVideoMode
       else                                     // changed font
         SysTVGetScrMode:=SysGetTextVideoMode+$100;
      end
    else
      SysTVGetScrMode:=$00ff; // Non-standard

  end;

procedure SysTVSetScrMode(Mode: Integer);
  {&Frame-}{&Uses eax,ebx}
  asm
    mov eax,Mode
    push eax
      mov ah,$00
      int $10
    pop eax
    cmp ah,0
    je @ret

    mov ax,$1112 // Font 8x8
    mov bl,0
    int $10

    @ret:
  end;

function SysTVGetSrcBuf: Pointer;
  begin
    SysTVGetSrcBuf := Pointer(SysGetTextVideoMemBase);
  end;

procedure SysTVInitCursor;
  begin
  end;

procedure SysTvDoneCursor;
  begin
  end;

function SysPlatformID: Longint;
  begin
    SysPlatformID := -2; // "DPMI32"
  end;

procedure SysBeep;
  {&Frame-}{&Uses eax}
  asm
    mov al,$07
    int $29
  end;

function SysGetValidDrives: Longint;
  {&Frame-}{&Uses ebx,ecx,edx}
  asm
    sub ebx,ebx  // Bits

    movzx eax,SmallWord [Seg0040+$10] // Equipment list
    test al, 1
    jz  @CountCtoZ
    mov ebx,$C0000000 //2 floppies maximum
    and al, $40       //mask out 'A' and 'B' floppies
    jnz @CountCtoZ
    shr ebx,1         //only 'A' floppy present

   @CountCtoZ:

    mov ah,$19   // Get Current Default Drive -> al
    int $21
    mov cl,al    // -> cl
    mov edx,2    //drive counter == 'C'

  @0:
    mov ah,$0e   // Select Disk
    int $21

    mov ah,$19   // Get Current Default Drive
    int $21
    cmp al,dl
    jne @1

    inc ebx

  @1:
    ror ebx,1
    inc edx
    cmp edx,26 //don't call int21 for nonexisting drives, i.e. > 'Z'
    jne @0

    ror ebx,6  //correct bitmask

    mov ah,$0e   // Select Disk
    mov dl,cl
    int $21

    mov eax,ebx
  end;

{$IfDef CLIP_SUPPORT}
const
  clip_buffer_size      =64*1024-1;             // max transfer size
  clip_buffer_16        :SmallWord=0;           // realmode segment
  clip_buffer_32        :Longint=0;             // protmode offset
  clip_type             =7;                     // OEM text
{$EndIf}


function SysClipCanPaste: Boolean;
  {$IfDef CLIP_SUPPORT}
  {&Frame-}{&Uses esi}
  asm
    cmp [clip_buffer_32],0              // already assigned ?
    mov al,True
    jne @ret

    mov esi,$00001700                   // MS Windows "WINOLDAP"
    mov eax,esi
    int $2f
    cmp eax,esi
    mov al,False
    je @ret                             // not installed

    lea eax,clip_buffer_16
    push eax
    push clip_buffer_size
    call GetDosMem
    test eax,eax
    mov al,False
    jnz @ret

    push [clip_buffer_16].Longint
    call dosseg_linear
    mov [clip_buffer_32],eax

    mov al,True
  @ret:
  end;
  {$Else}
  begin
    SysClipCanPaste := false;
  end;
  {$EndIf}

function SysClipCopy(P: PChar; Size: Longint): Boolean;
  {$IfDef CLIP_SUPPORT}
  var
    r                   :real_mode_call_structure_type;
  begin
    SysClipCopy:=false;

    if  SysClipCanPaste                 // installed?
    and (Size<clip_buffer_size) then    // Size not to large?
      with r do
        begin
          Move(P^,Mem[clip_buffer_32],Size); // -> 1. MB

          init_register(r);

          eax_:=$1701;                  // OPEN CLIPBOARD
          intr_realmode(r,$2f);

          eax_:=$1703;                  // SET CLIPBOARD DATA
          edx_:=clip_type;
          es_:=clip_buffer_16;          // ES:BX
          ebx_:=0;
          esi_:=0;                      // SI shl 16+CX
          ecx_:=Size;
          intr_realmode(r,$2f);

          if r.ax_<>0 then
            SysClipCopy:=true;

          eax_:=$1708;                  // Close Clipboard
          intr_realmode(r,$2f);

        end;
  end;
  {$Else}
  begin
    SysClipCopy := false;
  end;
  {$EndIf}

function SysClipPaste(var Size: Integer): Pointer;
  {$IfDef CLIP_SUPPORT}
  var
    r                   :real_mode_call_structure_type;
    s                   :Longint;
    p                   :PChar;
  begin
    SysClipPaste:=nil;
    Size:=0;

    if SysClipCanPaste then             // installed?
      with r do
        begin
          init_register(r);

          eax_:=$1701;                  // OPEN CLIPBOARD
          intr_realmode(r,$2f);

          eax_:=$1704;
          edx_:=clip_type;
          intr_realmode(r,$2f);         // GET CLIPBOARD DATA SIZE
          s:=r.ax_+r.dx_ shl 16;

          if (s>0) and (s<=clip_buffer_size) then
            begin

              eax_:=$1705;                  // GET CLIPBOARD DATA
              edx_:=clip_type;
              es_:=clip_buffer_16;          // ES:BX
              ebx_:=0;
              intr_realmode(r,$2f);

              if r.ax_=0 then               // error, or no data in this format in Clipboard
                Exit;

              GetMem(p,s+1);
              Move(Mem[clip_buffer_32],p^,s);
              p[s]:=#0;
              Size:=StrLen(p)+1;
              SysClipPaste:=p;
            end;

          eax_:=$1708;                  // Close Clipboard
          intr_realmode(r,$2f);
        end;
  end;
  {$Else}
  begin
    Size := 0;
    SysClipPaste := nil;
  end;
  {$EndIf}

function SysLoadResourceString(ID: Longint; Buffer: PChar; BufSize: Longint): PChar;
  {&Frame+}{&Uses ecx}
  asm
    // Buffer^:=#0
    mov eax,[Buffer]
    mov byte [eax],0

    push [ID]
    call d32_resource_query_address_resourcestring
    test eax,eax
    je @ret


    movzx ecx,SmallWord [eax]
    inc eax                     // skip .len
    inc eax

    // min(ecx,eax)
    cmp ecx,[BufSize]
    jle @weniger
    mov ecx,[BufSize]
  @weniger:
    pushad // &Frame+
      push [Buffer]             // Dest
      push eax                  // Source (.str)
      push ecx                  // MaxLen
      call StrLCopy
    popad

  @ret:
    // SysLoadResourceString:=Buffer;
    mov eax,[Buffer]
  end;


// this functionality is not available in DOS
// but it can be emulated by including resources from
// unit esyserr into executable

function SysGetSystemError(Code: Longint; Buffer: PChar; BufSize: Longint; var MsgLen: Longint): PChar;
  const
    System_Error        :array[0..13] of char='System Error #';
  var
    num                 :string[10];
    res                 :PD32ResourceString;
  begin
    res:=d32_resource_query_address_resourcestring(Code);
    if Assigned(res) then
      begin
        MsgLen:=Min(res^.len,BufSize);
        StrLCopy(Buffer,res^.str,MsgLen);
      end
    else
      begin
        Str(Code,num);
        if BufSize>Length(System_Error)+Length(num) then
          begin
            Move(System_Error,Buffer^,Length(System_Error));
            Move(num[1],Mem[Ofs(Buffer^)+Length(System_Error)],Length(num));
            MsgLen:=Length(System_Error)+Length(num);
          end
        else
          begin
            Buffer^:='?';
            MsgLen:=1;
          end;
      end;
    SysGetSystemError:=Buffer;
  end;


function SysGetModuleName(var Address: Pointer; Buffer: PChar; BufSize: Longint): PChar;
  begin
    // ParamStr(0) - until we support DLLs
    SysGetModuleName := StrLCopy(Buffer,CmdLine,BufSize);
  end;

function SysFileUNCExpand(Dest,Name: PChar): PChar;
  var
    regs                :real_mode_call_structure_type;
    tmp                 :array[0..260] of char;
  begin
    SysFileExpand(tmp,Name);
    StrCopy(PChar(@Mem[segdossyslow32]),tmp);
    with regs do
      begin
        ah_:=$60;             // "TRUENAME" - CANONICALIZE FILENAME OR PATH
        ds_:=segdossyslow16;  // DS:SI -> ES:DI
        si_:=$0000;
        es_:=segdossyslow16;
        di_:=$0200;
        intr_realmode(regs,$21);
        if (flags_ and flags_carry)=0 then
          StrCopy(Dest,PChar(@Mem[segdossyslow32+$0200]))
        else
          StrCopy(Dest,tmp);
      end;
    SysFileUNCExpand:=Dest;
  end;

type
  country_info_type=
    packed record
      date_format               :SmallWord;
        //        0 = USA    mm dd yy
        //        1 = Europe dd mm yy
        //        2 = Japan  yy mm dd
      currency_symbol           :array[0..5-1] of Char;
      thousands_separator       :array[0..2-1] of Char;
      decimal_separator         :array[0..2-1] of Char;
      date_separator            :array[0..2-1] of Char;
      time_separator            :array[0..2-1] of Char;
      currency_format           :Byte;
        //        bit 2 = set if currency symbol replaces decimal point
        //        bit 1 = number of spaces between value and currency symbol
        //        bit 0 = 0 if currency symbol precedes value
        //                1 if currency symbol follows value
      number_of_digits_after_decimal_in_currency:byte;
      time_format               :byte;
        //        bit 0 = 0 if 12-hour clock
        //                1 if 24-hour clock
      address_of_case_map_routine:real_mode_ptr_type;
      data_list_separator       :array[0..1] of char;
      reserved                  :array[1..10] of byte;
    end;

var
  country_info          :country_info_type;

procedure get_country_info;
  var
    regs                :real_mode_call_structure_type;
  begin
    FillChar(Mem[segdossyslow32],SizeOf(country_info_type),0);
    with regs do
      begin
        ax_:=$3800;
        ds_:=segdossyslow16; // -> DS:DX
        dx_:=0;
        intr_realmode(regs,$21);
      end;
    Move(Mem[segdossyslow32],country_info,SizeOf(country_info_type));
  end;

procedure SysGetCurrencyFormat(CString: PChar; var CFormat, CNegFormat, CDecimals: Byte; var CThousandSep, CDecimalSep: Char);
  begin
    get_country_info;
    StrCopy(CString,country_info.currency_symbol);
    CFormat             :=country_info.currency_format and $3;
                              // '$1'
                              // '1$'
                              // '$ 1'
                              // '1 $'
    case country_info.currency_format and $3 of // not available
      0:CNegFormat      := 3; //  '$1-'
      1:CNegFormat      := 5; //  '-1$'
      2:CNegFormat      :=11; //  '$ 1-'
      3:CNegFormat      := 8; //  '-1 $'
    end;
    CDecimals           :=country_info.number_of_digits_after_decimal_in_currency;
    CThousandSep        :=country_info.thousands_separator[0];
    CDecimalSep         :=country_info.decimal_separator[0];
  end;

const
  ShortDateFormat_tab:array[0..2] of pchar=
    ('mm/dd/yy',
     'dd/mm/yy',
     'yy/mm/dd');
  LongDateFormat_tab:array[0..2] of pchar=
    ('mmmm d, yyyy',
     'dd mmmm yyyy',
     'yyyy mmmm d');

procedure SysGetDateFormat(var DateSeparator: Char; ShortDateFormat,LongDateFormat: PChar);
  begin
    get_country_info;
    DateSeparator:=country_info.date_separator[0];
    if country_info.date_format>2 then
      country_info.date_format:=0;
    StrCopy(ShortDateFormat,ShortDateFormat_tab[country_info.date_format]);
    StrCopy(LongDateFormat ,LongDateFormat_tab [country_info.date_format]);
  end;

procedure SysGetTimeFormat(var TimeSeparator: Char; TimeAMString,TimePMString,ShortTimeFormat,LongTimeFormat: PChar);
  begin
    get_country_info;
    TimeSeparator:=country_info.time_separator[0];
    StrCopy(TimeAMString    ,'am');
    StrCopy(TimePMString    ,'pm');
    StrCopy(ShortTimeFormat ,'hh:mm');
    StrCopy(LongTimeFormat  ,'hh:mm:ss');
  end;

procedure SysDisplayConsoleError(PopupErrors: Boolean; Title, Msg: PChar);assembler;
  {&Frame-}{&Uses All}

  procedure NewLine;
    {&Frame-}{&Uses None}
    asm
      mov al,13
      int $29
      mov al,10
      int $29
    end;

  procedure DisplayPChar;
    {&Frame-}{&Uses None}
    asm
    @1:
      lodsb
      cmp al,0
      je @2
      int $29
      jmp @1
    @2:
    end;

  asm
    // could be impoved : PopupErrors is ignored

    call NewLine
    mov esi,Title
    cld

    call DisplayPChar
    call NewLine
    // length of title+1
    mov ecx,esi
    sub ecx,Title
    mov al,'='
  @3:
    int $29
    loop @3

    call NewLine
    mov esi,Msg
    call DisplayPChar
    call NewLine
  end;

procedure SysDisableHardErrors;
  begin
    DPMI32.install_i24;
  end;

function SysKillProcess(Process: Longint): Longint;
  begin
    // Not Implemented
    SysKillProcess:=-1;
  end;

function SysAllocSharedMemory(var _Base: Pointer; _Name: pChar; _Size: Longint): Longint;
begin
  // Not Implemented
  SysAllocSharedMemory := -1;
end;

function SysAccessSharedMemory(var _Base: Pointer; _Name: pChar): Longint;
begin
  // Not Implemented
  SysAccessSharedMemory := -1;
end;

procedure SysFreeSharedMemory(_Base: Pointer; _Handle: Longint);
begin
  // Not Implemented
end;

function SysAllocSharedMem(Size: Longint; var MemPtr: Pointer): Longint;
  begin
    // Not implemented          DPMI 1.0 has shared memory...
    SysAllocSharedMem:=-1;
  end;

function SysGiveSharedMem(MemPtr: Pointer): Longint;
  begin
    // Not implemented
    SysGiveSharedMem:=-1;
  end;

function SysPipeCreate(var ReadHandle,WriteHandle: Longint; Size: Longint): Longint;
  begin
    // Not implemented
    SysPipeCreate:=-1;
  end;

function SysPipePeek(Pipe: Longint; Buffer: Pointer; BufSize: Longint; var BytesRead: Longint; var IsClosing: Boolean): Longint;
  begin
    // Not implemented
    SysPipePeek:=-1;
  end;

function SysPipeClose(Pipe: Longint): Longint;
  begin
    // Not implemented
    SysPipeClose:=-1;
  end;

procedure SysDisplayGUIError(Title, Msg: PChar);
  begin
    SysDisplayConsoleError(true,Title,Msg);
  end;

// allocate memory for doing 16:16 real mode things, and for FS:0 Tib emulation
procedure GetDosStack_and_FS;
  {&Frame-}{&Uses eax,ebx,edx}
  asm
    mov eax,$0100               // DPMI 0.9+ ALLOCATE DOS MEMORY BLOCK
    mov bx,(4096+100)/16        // in 16 Byte Blocks
    int $31
    jnc @1

  @error:
    push 8
    call _RunError
    //************

  @1:
    mov segdossyslow16,ax
    mov work_dta.seg_,ax
    mov work_dta.ofs_,0

    movzx eax,ax
    shl eax,4                   // dosseg_linear
    mov segdossyslow32,eax


    // Allocate real mode stack
    mov eax,$0100               // DPMI 0.9+ ALLOCATE DOS MEMORY BLOCK
    mov bx,2048/16
    int $31
    jc @error

    mov real_mode_stack.seg_,ax
    mov real_mode_stack.ofs_,2048-4


    // Allocate Memory for TIB-Emulation (FS:[0]..)
    mov eax,$0100
    mov bx,4                    // 4*16 Byte is more than sufficient
    int $31
    jc @error

    // -> Selector DX
    mov sel_fs,dx
    mov fs,dx
  end;

procedure Detect_Multitaskers;
  {&Frame-}{&Uses All}
  asm
    mov ax,$2700 // NW/DR-DOS Taskmgr
    int $2f      // 00->false
    cmp al,$ff   // ff->true
    sete taskmgr

    mov ax,$4010 // OS/2 2.0+
    int $2f
    cmp ax,$4010
    setne os2

  end;

procedure Calculate_Segments;assembler;
  {&Frame-}{&Uses All}
  var
    regs                :real_mode_call_structure_type;
  asm

//--    movzx eax,sel_psp   // PSP: SEL->OFS
//--    push eax
//--    call protsel_linear
//--    mov seg_psp,eax
//--
//--    movzx eax,SmallWord [eax+$2c] // ENV= PSP:002c
//--    push eax
//--    call protsel_linear
//--    cmp eax,-1
//--    jne @1
//--
//--    // eclipse os has sel 0 for env ...
//--    push 216
//--    call _runerror
//--
//--  @1:
//--    mov Environment,eax

    lea esi,[regs]
    push esi
    call init_register

    mov [regs.ah_],$62             // get PSP seg
    and [regs.ebx_],0

    push esi
    push $21
    call intr_realmode

    mov eax,[regs.ebx_]
    shl eax,4
    cmp [eax].SmallWord,$20cd
    je @valid_psp

    push 216                            // RTE_Access_Violation
    call _RunError

  @valid_psp:
    mov [seg_psp],eax

    movzx eax,SmallWord [eax+$2c]       // environment selector= PSP:$002c
    push eax
    call protsel_linear

    cmp eax,-1
    je @invalid_env_sel

    cmp [eax-$10].byte,'M'              // valid memory control block before env ?
    je @valid_env
    cmp [eax-$10].byte,'Z'
    je @valid_env

  @invalid_env_sel:                     // causeway+dos+himem.sys ...

    mov eax,[seg_psp]
    movzx eax,SmallWord [eax+$2c]       // environment segement = PSP:$002c
    shl eax,4

  @valid_env:
    mov [Environment],eax

  end;

procedure Determine_DPMI_Version;
  {&Frame-}{&Uses All}
  asm
    mov eax,$0400
    int $31
    cmp ax,$0090
    jne @1
    mov al,   90
    @1:
    mov dpmi_version,eax
    dec cl          // 3=386 4=486 ..
    mov Test8086,cl

    // switch CauseWay into DOS4G mode
    mov eax,$ff00
    mov edx,$0078
    int $21
  end;

procedure SysLowInitPreTLS;
  begin
    // Nothing
  end;

procedure SysLowInitPostTLS;assembler;
  {&Frame-}{&Uses None}
  const
    stack_push= 4 // push esi
              + 4 // push eax
              + 4 // call VpSysLow.SysLowInit
              +16 // SizeOf(TExcFrame)
              + 4;// call System.Initexe
  asm
    // query DPMI version and 386/486.. processor
    call Determine_DPMI_Version

    // get memory in first megabyte for FS:[0] Tib emulation
    // and for real mode DOS API calls
    call GetDosStack_and_FS

    // initialize TIB-Emulation
    push eax
      push esi

        sub esi,esi
        // Head of exception handler chain
        or fs:[esi+Tib.Tib_PExchain],-1

        // bigger end of Stack
        lea eax,[esp+stack_push]
        mov fs:[esi+Tib.Tib_PStackLimit],eax
        // lower end of Stack
        sub eax,stacksize
        mov fs:[esi+Tib.Tib_PStack],eax

        // unused
        sub eax,eax
        mov fs:[esi+Tib.Tib_PTib2],eax
        mov fs:[esi+Tib.Tib_Version],eax

        // thread #1
        inc eax
        mov fs:[esi+Tib.Tib_Ordinal],eax

      pop esi
    pop eax

    // calculate some real mode memory locations
    call Calculate_Segments
    // prepare SysCmdln,SysCmdlnCount
    call Setup_Commandline
    // detect video adapter type
    call Detect_MdaCgaEgaVga
    // for SysCtrlSleep
    call Calibrate_Delayloop
    // check for Multitaskers
    call Detect_Multitaskers
    // install handlers for exceptions and ^C
    call InstallDpmi32Exceptionhandlers

    {$IfDef PE2LE_P2}
    jmp @ret

    //*****************************************************************
    // copy from pe2le: start.pas (procedure start2;)
    // if included here pe2le will find it
    // and save nearly 4096 bytes (1 page alignment)

    call @pop_edx
  @call_pop_edx:
    db 'WATCOM',0
  @pop_edx:
    pop edx
    sub edx,'5555'      // @call_pop_edx-@start2_virtuapl_pascal_code...

    mov eax,'EIP '
    add eax,edx
    push eax

      mov esi,'RELO'
      add esi,edx
      push esi          // remember starting address of memory area to clean

        cld
        lodsd
        lea edi,[eax+edx]
  @relocation_loop:
        add [edi],edx
        add edi,4

        sub eax,eax
        lodsb

        // more than 253 bytes difference?
        cmp al,254
        jb @small_delta
        // end of relocation data?
        // (al=255)
        ja @relocations_done

        // >253
        lodsd
  @small_delta:
        add edi,eax
        jmp @relocation_loop

  @relocations_done:

      pop edi           // FillChar(Mem[edi],ecx,0)
      push es           // ES (PSP) store
        push ds
        pop es          // ES:=data area

        sub eax,eax
        mov ecx,'VAR0'
        push ecx
          shr ecx,2     // div 4
          rep stosd
        pop ecx
        and ecx,3       // mod 4
        rep stosb
      pop es

      ret               // to program entry point

    db '****'           // supply a save place for a fixup that points to [1:0]
    db '****'           // some more space to avoid fixups on a page break.

    //
    //*****************************************************************

    @ret:
    {$EndIf PE2LE_P2}
  end; (* SysLowInitPostTLS *)

function PhysMemAvail: Longint;
  begin
  Result := SysMemAvail;
  end;

procedure SysLowInit;
  begin
  // Nothing
  end;
