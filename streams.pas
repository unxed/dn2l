unit Streams;

{$I STDEFINE.INC}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{Cat
   28/08/2001 - переделал чтение/запись строк в потоки для совместимости с
   типом AnsiString; добавил чтение/запись длинных строк (LongString) в потоки
}

interface

uses
  VpSysLow, Defines, Objects2
  ;

const
  { TStream access modes }
  stCreate = $FFFF; { Create new file }
  stOpenRead = Open_Access_ReadOnly or open_share_DenyNone;
  { Read access only }
  stOpenWrite = Open_Access_WriteOnly or open_share_DenyNone;
  { Write access only }
  stOpen = Open_Access_ReadWrite or open_share_DenyNone;
  { Read and write access }
  stOpenPacked = Open_Access_ReadWrite+1;
  { Read access only, packed files too }

  { File share mode constants }
  fmClean = $FF00;
  { Mask to clean low byte of file mode constatns }

  fmOpenMode = $FFF0;
  { Mask to apply fmReadOnly/fmWriteOnly/fmReadWrite }
  fmReadOnly = Open_Access_ReadOnly; { Open read-only file }
  fmWriteOnly = Open_Access_WriteOnly; { Open file for write only }
  fmReadWrite = Open_Access_ReadWrite;
  { Open file as for read, as for write }
  fmPacked = Open_Access_ReadWrite+1; { Open a packed file, if can }

  fmDeny = $FF0F; { Mask to apply fmDenyXXX }
  fmDenyAll = Open_Share_DenyReadWrite; { Exclusive file use }
  fmDenyWrite = Open_Share_DenyWrite; { Deny write access }
  fmDenyRead = Open_Share_DenyRead; { Deny read access }
  fmDenyNone = open_share_DenyNone; { Deny no access }
  fmDenyChild = open_share_DenyNone; { Don't give right's to child }

  { TStream error codes }
  stOK = 0; { No error }
  stError = -1; { Access error }
  stInitError = -2; { Cannot initialize stream }
  stReadError = -3; { Read beyond end of stream }
  stWriteError = -4; { Cannot expand stream }
  stGetError = -5; { Get of unregistered object type }
  stPutError = -6; { Put of unregistered object type }
  stSeekError = -7; { Stream seek error }
  stOpenError = -8; { Stream open error }

type
  PStreamRec = ^TStreamRec;
  TStreamRec = record
    ObjType: Word;
    VmtLink: Pointer;
    Load: Pointer;
    Store: Pointer;
    Next: PStreamRec;
    end;

  PStream = ^TStream;
  TStream = object(TObject)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Status: Integer;
    ErrorInfo: Integer;
    StreamSize: TFileSize; { Stream current size }
    Position: TFileSize; { Current Position }
    procedure CopyFrom(var S: TStream; Count: TFileSize);
    procedure Error(Code, Info: Integer); virtual;
    procedure Flush; virtual;
    function Get: PObject;
    function GetPos: TFileSize; virtual;
    function GetSize: TFileSize; virtual;
    procedure Put(P: PObject);
    procedure Read(var Buf; Count: SW_Word); virtual;
    function ReadStr: PString;
    function ReadLongStr: PLongString;
    procedure ReadStrV(var S: String);
    procedure ReadLongStrV(var S: LongString);
    procedure Reset;
    procedure Seek(Pos: TFileSize); virtual;
    function StrRead: PChar;
    procedure StrWrite(P: PChar);
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: SW_Word); virtual;
    procedure WriteStr(P: PString);
    procedure WriteLongStr(P: PLongString);
    function Eof: Boolean;
    procedure DoOpen(OpenMode: Word); virtual;
    procedure Close; virtual;
    end;

  PDosStream = ^TDosStream;
  TDOSStream = object(TStream)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Handle: Integer;
    FName: AsciiZ;
    FMode: Word;
    constructor Init(FileName: FNameStr; Mode: Word);
    procedure Open(FileName: FNameStr; Mode: Word);
    destructor Done; virtual;
    procedure Read(var Buf; Count: SW_Word); virtual;
    procedure ReadBlock(var Buf; Count: SW_Word; var BytesRead: Word);
     virtual;
    procedure Seek(Pos: TFileSize); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: SW_Word); virtual;
    procedure DoOpen(OpenMode: Word); virtual;
    procedure Close; virtual;
    end;

  PBufStream = ^TBufStream;
  TBufStream = object(TDOSStream)
  {`2}
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Buffer: PByteArray;
    BufSize: SW_Word;
    BufPtr: SW_Word;
      {` Индекс в Buffer^ первого необработанного байта, то есть
      непрочитанного при чтении, не записанного при записи`}
    BufEnd: SW_Word;
      {` Индекс в Buffer^ первого незаполненного байта `}
    ModBufStart, ModBufEnd: SW_Word;
      {` в буфере модифицированы байты с ModBufStart по ModBufEnd-1 `}
    constructor Init(FileName: FNameStr; Mode: Word; Size: SW_Word);
    destructor Done; virtual;
    procedure Flush; virtual;
    procedure Read(var Buf; Count: SW_Word); virtual;
    procedure Seek(Pos: TFileSize); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: SW_Word); virtual;
    procedure DoOpen(OpenMode: Word); virtual;
    procedure Close; virtual;
    end;
  {`}

  PMemoryStream = ^TMemoryStream;
  TMemoryStream = object(TStream)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    BlkCount: LongInt;
     {` Number of segments `}
    BlkSize: Word;
     {` Memory block size `}
    MemSize: LongInt;
     {` Memory alloc size `}
    BlkList: PPointerArray;
     {` Memory block list `}
    constructor Init(ALimit: LongInt; ABlockSize: Word);
    destructor Done; virtual;
    procedure Read(var Buf; Count: SW_Word); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: SW_Word); virtual;
  private
    function ChangeListSize(ALimit: LongInt): Boolean;
    end;

procedure RegisterType(var S: TStreamRec);
procedure ReRegisterType(var S: TStreamRec);

function GetAnyMemoryStream: PStream;

const
  StreamError: Pointer = nil;

implementation

uses
  {$IFDEF PLUGIN}Plugin, {$ENDIF}
  Strings, Memory, Advance1
  {$IFDEF DPMI32}, LfnVP {$ENDIF}
  ;

const
  StreamTypes: PStreamRec = nil;

procedure RegisterError;
  begin
  end;

procedure RegisterType(var S: TStreamRec);
  const
    SRegisterError = 'RegisterError, ObjType=';
  var
    P: PStreamRec;
  begin
  if S.ObjType = 0 then
    begin
    Writeln(SRegisterError, S.ObjType);
    RegisterError;
    end;
  P := StreamTypes;
  while (P <> nil) and (P^.ObjType <> S.ObjType) do
    P := P^.Next;
  if P = nil then
    begin
    S.Next := StreamTypes;
    StreamTypes := @S;
    end
  else if (P^.VmtLink <> S.VmtLink) or (P^.Load <> S.Load)
       or (P^.Store <> S.Store)
  then
    begin
    Writeln(SRegisterError, S.ObjType);
    RegisterError;
    end;
  end { RegisterType };

procedure ReRegisterType(var S: TStreamRec);
  var
    P, L: PStreamRec;
  begin
  if S.ObjType = 0 then
    RegisterError;
  P := StreamTypes;
  L := nil;
  while (P <> nil) and (P^.ObjType <> S.ObjType) do
    begin
    L := P;
    P := P^.Next;
    end;
  if P <> nil then
    if L <> nil then
      L^.Next := P^.Next
    else
      StreamTypes := P^.Next;
  S.Next := StreamTypes;
  StreamTypes := @S;
  end;

const
  TStream_Error = vmtHeaderSize+$04;
  TStream_Flush = vmtHeaderSize+$08;
  TStream_Read = vmtHeaderSize+$14;
  TStream_Write = vmtHeaderSize+$20;

  StreamMagic = $590C5CF1;

  { Stream error handler                                  }
  { In    eax   = Error info                              }
  {       dl    = Error code                              }
  {       ecx   = Stream object pointer                   }
  { Uses  eax,edx                                         }

procedure DoStreamError;
  assembler; {$USES ecx}
  {$FRAME-}
asm
  movsx   edx,dl
  push    edx             { [1]:Integer = Code    }
  push    eax             { [2]:Integer = Info    }
  push    ecx             { [3]:Pointer = Self    }
  mov     eax,[ecx]
  call    DWord Ptr [eax].TStream_Error
end;

procedure TStream.CopyFrom(var S: TStream; Count: TFileSize);
  var
    N: Word;
    Buffer: PByteArray;
    BufSize: Word;
    Allocated: Boolean;
    TTempBuf: array[0..255] of Byte;
  begin
  BufSize := MinBufSize(Count, 32768);
  GetMem(Buffer, BufSize);
  if Buffer = nil then
    begin
    Allocated := False;
    Buffer := PByteArray(@TTempBuf);
    BufSize := 256;
    end
  else
    Allocated := True;
  while Count > 0 do
    begin
    N := MinBufSize(Count, BufSize);
    S.Read(Buffer^, N);
    Write(Buffer^, N);
    Count := Count - N;
    end;
  if Allocated then
    FreeMem(Buffer, BufSize);
  end { TStream.CopyFrom };

procedure TStream.Error(Code, Info: Integer);
  type
    TErrorProc = procedure (var S: TStream);
  begin
  Status := Code;
  ErrorInfo := Info;
  if StreamError <> nil then
    TErrorProc(StreamError)(Self);
  end;

procedure TStream.Flush;
  begin
  end;

function TStream.Get: PObject;
  assembler; {$USES None}
  {$FRAME+}
asm
{Cat: добавил проверку StreamMagic}
  push    eax
  mov     eax,esp
  push    eax                     { [1]:Pointer = Buf   }
  push    4                       { [2]:DWord   = Count }
  mov     eax,Self
  push    eax                     { [3]:Pointer = Self  }
  mov     eax,[eax]
  call    DWord Ptr [eax].TStream_Read
  pop     eax
  cmp     eax,StreamMagic
  jne     @@Error
{/Cat}
  push    eax
  mov     eax,esp
  push    eax                     { [1]:Pointer = Buf   }
  push    4                       { [2]:DWord   = Count }
  mov     eax,Self
  push    eax                     { [3]:Pointer = Self  }
  mov     eax,[eax]
  call    DWord Ptr [eax].TStream_Read
  pop     eax
  test    eax,eax                 { Return nil }
  jz      @@4
{Cat: если пытаемся прочитать объект, регистрируемый плагином, то надо
      сначала запустить этот плагин, чтобы он провёл регистрацию}
  {$IFDEF PLUGIN}
  push    eax
  push    eax
  call    PluginRegisterObject
  pop     eax
  {$ENDIF}
{/Cat}
  mov     edx,StreamTypes
  jmp     @@2
  @@1:
  cmp     eax,[edx].TStreamRec.ObjType
  je      @@3
  mov     edx,[edx].TStreamRec.Next
  @@2:
  test    edx,edx
  jnz     @@1
  @@Error:
  mov     ecx,Self
  mov     dl,stGetError
  call    DoStreamError
  xor     eax,eax                 { Return nil }
  jmp     @@4
  @@3:
  push    Self
  { [1]:Pointer = TStream }
  push    [edx].TStreamRec.VmtLink
  { [2]:DWord   = VMT     }
  push    0
  { [3]:Pointer = Self = nil: allocate in dynamic memory }
  call    [edx].TStreamRec.Load
  @@4:
  { Return Self or nil }
end;

function TStream.GetPos: TFileSize;
  begin
  if  (Status = stOK) then
    GetPos := Position
  else
    GetPos := -1;
  end;

function TStream.GetSize: TFileSize;
  begin
  if  (Status = stOK) then
    GetSize := StreamSize
  else
    GetSize := -1;
  end;

procedure TStream.Put(P: PObject);
  assembler; {$USES None}
  {$FRAME+}
asm
{Cat: добавил запись StreamMagic}
  push    StreamMagic
  mov     eax,esp
  push    eax                     { [1]:Pointer = Buf  }
  push    4                       { [2]:DWord   = Size }
  mov     eax,Self                { [3]:Pointer = Self }
  push    eax
  mov     eax,[eax]
  call    DWord Ptr [eax].TStream_Write
  pop     ecx
{/Cat}
  mov     ecx,P
  jecxz   @@4
  mov     eax,[ecx]               { VMT pointer }
  mov     edx,StreamTypes
  jmp     @@2
  @@1:
  cmp     eax,[edx].TStreamRec.VmtLink
  je      @@3
  mov     edx,[edx].TStreamRec.Next
  @@2:
  test    edx,edx
  jne     @@1
  mov     ecx,Self
  mov     dl,stPutError
  call    DoStreamError
  jmp     @@5
{/Cat}
  @@3:
  mov     ecx,[edx].TStreamRec.ObjType
  @@4:
  push    edx
  push    ecx                     { Write object type  }
  mov     eax,esp
  push    eax                     { [1]:Pointer = Buf  }
  push    4                       { [2]:DWord   = Size }
  mov     eax,Self                { [3]:Pointer = Self }
  push    eax
  mov     eax,[eax]
  call    DWord Ptr [eax].TStream_Write
  pop     ecx
  pop     edx
  jecxz   @@5
  push    Self
  { [1]:Pointer = TStream }
  push    P
  { [2]:Pointer = Self    }
  call    [edx].TStreamRec.Store
  @@5:
end;

procedure TStream.Read(var Buf; Count: SW_Word);
  begin
  end;

{Cat}
function TStream.ReadStr: PString;
  var
    L: LongInt;
    P: PString;
  begin
  L := 0;
  Read(L, 1);
  if L > 0 then
    begin
    GetMem(P, L+1);
    SetLength(P^, L);
    Read(P^[1], L);
    ReadStr := P;
    end
  else
    ReadStr := nil;
  end;

function TStream.ReadLongStr: PLongString;
  var
    L: LongInt;
    P: PLongString;
  begin
  Read(L, SizeOf(L));
  if L > 0 then
    if L > MaxLongStringLength then
      begin
      New(P);
      SetLength(P^, MaxLongStringLength);
      Read(P^[1], MaxLongStringLength);
      Seek(Position-MaxLongStringLength+L);
      ReadLongStr := P;
      end
    else
      begin
      New(P);
      SetLength(P^, L);
      Read(P^[1], L);
      ReadLongStr := P;
      end
  else
    ReadLongStr := nil;
  end { TStream.ReadLongStr: };

procedure TStream.ReadStrV(var S: String);
  var
    L: LongInt;
  begin
  L := 0;
  Read(L, 1);
  if L > 0 then
    begin
    SetLength(S, L);
    Read(S[1], L);
    end
  else
    S := '';
  end;

procedure TStream.ReadLongStrV(var S: LongString);
  var
    L: LongInt;
  begin
  Read(L, SizeOf(L));
  if L > 0 then
    if L > MaxLongStringLength then
      begin
      SetLength(S, MaxLongStringLength);
      Read(S[1], MaxLongStringLength);
      Seek(Position-MaxLongStringLength+L);
      end
    else
      begin
      SetLength(S, L);
      Read(S[1], L);
      end
  else
    S := '';
  end;
{/Cat}

procedure TStream.Reset;
  begin
  Status := 0;
  ErrorInfo := 0;
  end;

procedure TStream.Seek(Pos: TFileSize);
  begin
  if Status = stOK then
    if Pos < 0 then
      Pos := 0
    else if Pos <= StreamSize then
      Position := Pos
    else
      Error(stSeekError, 0{!!Pos});
  end;

function TStream.StrRead: PChar;
  var
    L: Word;
    P: PChar;
  begin
  Read(L, SizeOf(L));
  if L = 0 then
    StrRead := nil
  else
    begin
    GetMem(P, L+1);
    Read(P[0], L);
    P[L] := #0;
    StrRead := P;
    end;
  end;

procedure TStream.StrWrite(P: PChar);
  var
    L: Word;
  begin
  if P = nil then
    L := 0
  else
    L := StrLen(P);
  Write(L, SizeOf(L));
  if P <> nil then
    Write(P[0], L);
  end;

procedure TStream.Truncate;
  begin
  end;

procedure TStream.Write(const Buf; Count: SW_Word);
  begin
  end;

{Cat}
procedure TStream.WriteStr(P: PString);
  var
    L: LongInt;
  begin
  if P <> nil then
    Write(P^[0], Length(P^)+1)
  else
    begin
    L := 0;
    Write(L, 1);
    end;
  end;

procedure TStream.WriteLongStr(P: PLongString);
  var
    L: LongInt;
  begin
  if P <> nil then
    begin
    L := Length(P^);
    Write(L, 4);
    Write(P^[1], L);
    end
  else
    begin
    L := 0;
    Write(L, SizeOf(L));
    end
  end;
{/Cat}

function TStream.Eof: Boolean;
  begin
  Eof := (GetPos >= GetSize);
  end;

procedure TStream.DoOpen(OpenMode: Word);
  begin
  end;

procedure TStream.Close;
  begin
  end;

function AFileClose(Handle: Word): Boolean;
  begin
  AFileClose := (SysFileClose(Handle) = 0);
  end;

function AFileOpen(var FileName: AsciiZ; Mode: Word; var Handle: Word)
  : Word;
  begin
  if Mode = stCreate then
    AFileOpen := {$IFDEF DPMI32}LfnVP.{$ENDIF}SysFileCreate(@FileName, stOpen, $20 {Archive}, Handle)
  else
    AFileOpen := {$IFDEF DPMI32}LfnVP.{$ENDIF}SysFileOpen(@FileName, Mode, Handle);
  end;

function AFileRead(Handle: Word; var Buf; Count: SW_Word;
     var Actual: SW_Word): Word;
  begin
  Actual := 0;
  AFileRead := SysFileRead(Handle, Buf, Count, Actual);
  end;

function AFileWrite(Handle: Word; var Buf; Count: SW_Word;
     var Actual: SW_Word): Word;
  begin
  Actual := 0;
  AFileWrite := SysFileWrite(Handle, Buf, Count, Actual);
  end;

function ASetFilePos(Handle: Word; Pos: TFileSize; MoveType: Word;
     var Actual: TFileSize): Longint;
  begin
  Actual := 0;
  ASetFilePos := SysFileSeek(Handle, Pos, MoveType, Actual);
  end;

{AK155}
function ASetFileSize(Handle: Word; FileSize: TFileSize): Word;
  begin
  ASetFileSize := 0;
  if  (ASetFilePos(Handle, FileSize, 0, FileSize) <> 0) or
      (SysFileSetSize(Handle, FileSize) <> 0)
  then
    ASetFileSize := 1
  else
    ASetFileSize := 0;
  end;
{/AK155}

constructor TDOSStream.Init(FileName: FNameStr; Mode: Word);
  var
    Success: Integer;
    {$IFDEF PACKFILE}
  label 1;
  {$ENDIF}
  begin
  inherited Init;
  FileName := FileName+#0;
  Move(FileName[1], FName, Length(FileName));
  FMode := Mode;
  {$IFDEF PACKFILE}
  if  (FMode and $000F) = fmPacked then
    begin
    Handle := pOpen(@FName, FMode and $FFF0);
    Success := 0;
    if Handle <> 0 then
      StreamSize := pFileSize(Handle)
    else
      begin
      FMode := FMode and $FFF0;
      goto 1;
      end;
    end
  else
    {$ENDIF}
    begin
    {$IFDEF PACKFILE}
1:
    {$ENDIF}
    Success := AFileOpen(FName, FMode, Handle);
    if Success = 0 then
      begin
      Success := ASetFilePos(Handle, 0, 2, StreamSize);
      if Success = 0 then
        Success := ASetFilePos(Handle, 0, 0, Position);
      end;
    end;
  if {AK155: под OS/2 бывает 0 (Handle = 0) or} (Success <> 0) then
    begin
{    if Handle = 0 then
      Handle := -1;}
    Error(stInitError, Success);
    end;
  end { TDOSStream.Init };

procedure TDOSStream.Open(FileName: FNameStr; Mode: Word);
  var
    Success: Integer;
  label 1;
  begin
  if  (Handle <> -1) then
    begin
    {$IFDEF PACKFILE}
    if  (FMode and $000F) = fmPacked then
      pClose(Handle)
    else
      {$ENDIF}
      AFileClose(Handle);
    end;
  FileName := FileName+#0;
  Move(FileName[1], FName, Length(FileName));
  FMode := Mode;
  {$IFDEF PACKFILE}
  if  (FMode and $000F) = fmPacked then
    begin
    Handle := pOpen(@FName, FMode and $FFF0);
    Success := 0;
    if Handle <> 0 then
      StreamSize := pFileSize(Handle)
    else
      begin
      FMode := FMode and $FFF0;
      goto 1;
      end;
    end
  else
    {$ENDIF}
    begin
1:
    Success := AFileOpen(FName, FMode, Handle);
    if Success = 0 then
      begin
      Success := ASetFilePos(Handle, 0, 2, StreamSize);
      if Success = 0 then
        Success := ASetFilePos(Handle, 0, 0, Position);
      end;
    end;
  if  (Handle = 0) or (Success <> 0) then
    begin
    if Handle = 0 then
      Handle := -1;
    Error(stInitError, Success);
    end;
  end { TDOSStream.Open };

destructor TDOSStream.Done;
  begin
  if  (Handle <> -1) then
    begin
    {$IFDEF PACKFILE}
    if  (FMode and $000F) = fmPacked then
      pClose(Handle)
    else
      {$ENDIF}
      AFileClose(Handle);
    end;
  Handle := -1;
  inherited Done;
  end;

procedure TDOSStream.Read(var Buf; Count: SW_Word);
  var
    Success: Integer;
    W, BytesMoved: SW_Word;
    P: PByteArray;
  begin
  P := @Buf;
  if Handle = -1 then
    Error(stReadError, 103)
  else if Position+Count > StreamSize then
    Error(stReadError, 0)
  else
    begin
    while (Count > 0) and (Status = stOK) do
      begin
      W := Count;
      {$IFDEF PACKFILE}
      if  (FMode and $000F) = fmPacked then
        begin
        BytesMoved := pRead(Handle, W, P);
        Success := IOResult;
        end
      else
        {$ENDIF}
        Success := AFileRead(Handle, P^, W, BytesMoved);
      if Success = 0 then
        begin
        Position := Position + BytesMoved;
        P := Pointer(LongInt(P)+BytesMoved);
        Dec(Count, BytesMoved);
        end;
      if  (Success <> 0) or (BytesMoved <> W) then
        begin
        Error(stReadError, Success);
        Break;
        end;
      end;
    end;
  if Count <> 0 then
    FillChar(P^, Count, #0); { Error clear buffer }
  end { TDOSStream.Read };

procedure TDOSStream.ReadBlock(var Buf; Count: SW_Word;
     var BytesRead: Word);
  var
    Success: Integer;
    W, BytesMoved: SW_Word;
    P: PByteArray;
  begin
  P := @Buf;
  Success := -1;
  BytesRead := 0;
  if Handle = -1 then
    Success := 103;
  if  (Position+Count > StreamSize) or (Status <> 0) then
    Success := 0;
  if Success <> -1 then
    Error(stReadError, Success)
  else
    begin
    while (Count > 0) and (Status = stOK) do
      begin
      W := Count;
      {$IFDEF PACKFILE}
      if  (FMode and $000F) = fmPacked then
        begin
        BytesMoved := pRead(Handle, W, P);
        Success := IOResult;
        end
      else
        {$ENDIF}
        Success := AFileRead(Handle, P^, W, BytesMoved);
      if Success = 0 then
        begin
        Position := Position + BytesMoved;
        P := Pointer(LongInt(P)+BytesMoved);
        Dec(Count, BytesMoved);
        Inc(BytesRead, BytesMoved);
        end;
      if  (Success <> 0) then
        begin
        Error(stReadError, Success);
        Break;
        end;
      if BytesMoved <> W then
        Break;
      end;
    end;
  end { TDOSStream.ReadBlock };

procedure TDOSStream.Seek(Pos: TFileSize);
  var
    Success: Integer;
    Li: TFileSize;
  begin
  if Status = stOK then
    begin
    if Pos < 0 then
      Pos := 0;
    if Handle = -1 then
      Success := 03
    else
      {$IFDEF PACKFILE}
     if (FMode and $000F) = fmPacked then
      begin
      Li := pSeek(Handle, Pos);
      Success := 0;
      if IOResult <> 0 then
        Success := -1;
      end
    else
      {$ENDIF}
      Success := ASetFilePos(Handle, Pos, 0, Li);
    if  (Success = -1) or (Li <> Pos) then
      begin
      if  (Success = -1) then
        Error(stSeekError, 0)
      else
        Error(stSeekError, Success);
      end
    else
      Position := Li;
    end;
  end { TDOSStream.Seek };

procedure TDOSStream.Truncate;
  var
    Success: Integer;
  begin
  if Status = stOK then
    begin
    if {$IFDEF PACKFILE}((FMode and $000F) = fmPacked) or {$ENDIF}
        ( (FMode and $000F) = fmReadOnly)
    then
      Success := 103
    else
      Success := ASetFileSize(Handle, Position);
    if Success = 0 then
      StreamSize := Position
    else
      Error(stError, Success);
    end;
  end;

procedure TDOSStream.Write(const Buf; Count: SW_Word);
  var
    Success: Integer;
    W, BytesMoved: SW_Word;
    P: PByteArray;
  begin
  if Handle = -1 then
    Error(stWriteError, 103)
  else
    begin
    if {$IFDEF PACKFILE}((FMode and $000F) = fmPacked) or {$ENDIF}
        ( (FMode and $000F) = fmReadOnly)
    then
      Error(stError, 103)
    else
      begin
      P := @Buf;
      while (Count > 0) and (Status = stOK) do
        begin
        W := Count;
        {$IFNDEF OS2}
        if Count > $FFFF then
          W := $FFFF; { Cant read >64K bytes }
        {$ENDIF}
        Success := AFileWrite(Handle, P^, W, BytesMoved);
        if Success = 0 then
          begin
          Position := Position + BytesMoved;
          P := Pointer(LongInt(P)+BytesMoved);
          Dec(Count, BytesMoved);
          if  (Position > StreamSize) then
            StreamSize := Position;
          end;
        if Success <> 0 then
          begin
          Error(stWriteError, Success);
          Break;
          end;
        end;
      end;
    end;
  end { TDOSStream.Write };

{Cat:warn правильно ли это работает?}
procedure TDOSStream.DoOpen(OpenMode: Word);
  {$IFDEF PACKFILE}
  label 1;
  {$ENDIF}
  var
    Success: Integer;
  begin
  if Status = stOK then
    begin
    if Handle = -1 then
      begin
      {$IFDEF PACKFILE}
1:
      {$ENDIF}
      FMode := OpenMode;
      {$IFDEF PACKFILE}
      if  (FMode and $000F) = fmPacked then
        begin
        Handle := pOpen(@FName, FMode and $FFF0);
        if Handle = 0 then
          begin
          FMode := FMode and $FFF0;
          goto 1;
          end;
        end
      else
        {$ENDIF}
        Success := AFileOpen(FName, FMode, Handle);
      Position := 0;
      if Handle = 0 then
        begin
        Handle := -1;
        Error(stOpenError, Success);
        end;
      end
    else
      Error(stOpenError, 104); { File already open }
    end;
  end { TDOSStream.DoOpen };

procedure TDOSStream.Close;
  begin
  if Handle <> -1 then
    begin
    {$IFDEF PACKFILE}
    if  (FMode and $000F) = fmPacked then
      pClose(Handle)
    else
      {$ENDIF}
      AFileClose(Handle);
    end;
  Position := 0;
  Handle := -1;
  end;

constructor TBufStream.Init(FileName: FNameStr; Mode: Word; Size: SW_Word);
  begin
  inherited Init(FileName, Mode);
  BufSize := Size;
  if Size = 0 then
    Error(stInitError, 0)
  else
    begin
    GetMem(Buffer, Size);
    if Buffer = nil then
      Error(stInitError, 0);
    ModBufStart := $FFFF;
    end;
  end;

destructor TBufStream.Done;
  begin
  Flush;
  if Buffer <> nil then
    begin
    FreeMem(Buffer, BufSize);
    Buffer := nil;
    end;
  inherited Done;
  end;

procedure TBufStream.Flush;
  var
    Success: Integer;
    W, Len: SW_Word;
    Pos: TFileSize;
  begin
  if ModBufEnd > ModBufStart then
    begin
    if Handle = -1 then
      Error(stError, 103)
    else
      {$IFDEF PACKFILE}
     if (FMode and $000F) = fmPacked then
      Error(stError, 103)
    else
      {$ENDIF}
      begin
      Len := ModBufEnd - ModBufStart;
      Pos := Position;
      inherited Seek(Position - BufPtr + ModBufStart);
      Success := AFileWrite(Handle, Buffer^[ModBufStart], Len, W);
      inherited Seek(Pos);
      if  (Success <> 0) or (W <> Len) then
        Error(stError, Success);
      end;
    ModBufStart := $FFFF; ModBufEnd := 0;
    end;
  end { TBufStream.Flush };

procedure TBufStream.Read(var Buf; Count: SW_Word);
  var
    Success: Integer;
    W, Bw: SW_Word;
    P: PByteArray;
  begin
  P := @Buf;
  if Handle = -1 then
    Error(stReadError, 103)
  else if Position+Count > StreamSize then
    Error(stReadError, 0)
  else
    begin
    while (Count > 0) and (Status = stOK) do
      begin
      if BufPtr = BufEnd then
        begin { Buffer is empty }
        Bw := MinBufSize(StreamSize-Position, BufSize);
        {$IFDEF PACKFILE}
        if  (FMode and $000F) = fmPacked then
          begin
          W := pRead(Handle, Bw, Buffer);
          Success := IOResult;
          end
        else
          {$ENDIF}
          begin
          Flush;
          Success := AFileRead(Handle, Buffer^, Bw, W);
          end;
        BufPtr := 0;
        BufEnd := W;
        if  (Success <> 0) or (Bw <> W) then
          begin
          Error(stReadError, Success);
          BufEnd := W;
          end;
        end;
      if Status = stOK then
        begin
        W := BufEnd-BufPtr;
        if Count < W then
          W := Count;
        Move(Buffer^[BufPtr], P^, W);
        Dec(Count, W);
        Inc(BufPtr, W);
        P := Pointer(LongInt(P)+W);
        Position := Position + W;
        end;
      end;
    end;
  if  (Status <> stOK) and (Count > 0) then
    FillChar(P^, Count, #0);
  end { TBufStream.Read };

procedure TBufStream.Seek(Pos: TFileSize);
  var
    BufStart: TFileSize; // файловый адрес начала буфера
  begin
  Status := stOK;
  BufStart := Position-BufPtr;
  if (Pos >= BufStart) and (Pos < BufStart+BufEnd) then
    begin { AK155 Установка в пределах буфера без обращения к файлу}
    BufPtr := i32(Pos - BufStart);
    Position := Pos;
    end
  else
    begin
    Flush;
    if  (Status <> stOK) then
      Exit;
    inherited Seek(Pos);
    BufPtr := 0;
    BufEnd := 0;
    end;
  end;

procedure TBufStream.Truncate;
  begin
  Flush;
  inherited Truncate;
  end;

procedure TBufStream.Write(const Buf; Count: SW_Word);
  var
    Success: Integer;
    W: SW_Word;
    P: PByteArray;
  begin
  if Handle = -1 then
    Error(stWriteError, 103)
  else
    begin
    P := @Buf;
    while (Count > 0) and (Status = stOK) do
      begin
      if BufPtr = BufSize then
        begin { Buffer is full }
        Flush;
        BufPtr := 0;
        BufEnd := 0;
        end;
      if Status = stOK then
        begin
        W := BufSize-BufPtr;
        if Count < W then
          W := Count;
        Move(P^, Buffer^[BufPtr], W);
        if BufPtr < ModBufStart then
          ModBufStart := BufPtr;
        Dec(Count, W);
        Inc(BufPtr, W);
        if BufPtr > ModBufEnd then
          ModBufEnd := BufPtr;
        P := Pointer(LongInt(P)+W);
        Position := Position + W;
        if Position > StreamSize then
          StreamSize := Position;
        if BufEnd < BufPtr then
          BufEnd := BufPtr;
        end;
      end;
    end;
  end { TBufStream.Write };

procedure TBufStream.Close;
  begin
  Flush;
  inherited Close;
  end;

procedure TBufStream.DoOpen(OpenMode: Word);
  begin
  if Status = stOK then
    begin
    BufPtr := 0;
    BufEnd := 0;
    inherited DoOpen(OpenMode);
    end;
  end;

constructor TMemoryStream.Init(ALimit: LongInt; ABlockSize: Word);
  var
    W: LongInt;
  begin
  inherited Init;
  if ABlockSize = 0 then
    BlkSize := 8192
  else
    BlkSize := ABlockSize;
  if ALimit <= 0 then
    W := 1
  else
    W := (ALimit+BlkSize-1) div BlkSize;
  if not ChangeListSize(W) then
    Error(stInitError, 0);
  (*
  StreamSize:=MemSize;
{AK155 2-03-2002
   При работе с Clipboard StreamSize используется в качестве укзателя
конца потока, поэтому у свежесозданного потока он должен быть равен нулю.
А это присвоение создает возможость чтения мусора за пределами фактически
записанного потока. Например, если в dn.ini указать MaxClipboardSize=0,
то DN почти гарантировано виснет при первом же взятии в буфер в редакторе.
   Но в FileCopy GetSize (то есть StreamSize) используется в качестве
размера потока (что неестественно), поэтому там тоже есть соответствующая
коррекция. Других мест, где используется StreamSize для TMemoryStream,
я не нашел.}
*)
  end { TMemoryStream.Init };

destructor TMemoryStream.Done;
  begin
  ChangeListSize(0);
  inherited Done;
  end;

function TMemoryStream.ChangeListSize(ALimit: LongInt): Boolean;
  var
    I, W: LongInt;
    Li: LongInt;
    P: PPointerArray;
  begin
  if  (ALimit <> BlkCount) then
    begin
    ChangeListSize := False;
    if ALimit > MaxPtrs then
      Exit;
    if ALimit > 0 then
      begin
      Li := ALimit*SizeOf(Pointer);
      if not LowMemory and (MaxAvail > Li) then
        begin
        GetMem(P, Li);
        FillChar(P^, Li, #0);
        end
      else
        Exit;
      if  (BlkCount <> 0) and (BlkList <> nil) then
        if BlkCount <= ALimit then
          Move(BlkList^, P^, BlkCount*SizeOf(Pointer))
        else
          Move(BlkList^, P^, Li);
      end
    else
      begin
      P := nil;
      ALimit := 0;
      end;
    if ALimit < BlkCount then
      for W := BlkCount-1 downto ALimit do
        FreeMem(BlkList^[W], BlkSize);
    if  (P <> nil) and (ALimit > BlkCount) then
      begin
      for W := BlkCount to ALimit-1 do
        begin
        if LowMemory or (MaxAvail < BlkSize) then
          begin
          for I := BlkCount to W-1 do
            FreeMem(P^[I], BlkSize);
          FreeMem(P, Li);
          Exit;
          end
        else
          GetMem(P^[W], BlkSize);
        end;
      end;
    if  (BlkCount <> 0) and (BlkList <> nil) then
      FreeMem(BlkList, BlkCount*SizeOf(Pointer));
    BlkList := P;
    BlkCount := ALimit;
    { * REMARK * - Do not shorten this, result can be > 64K }
    MemSize := BlkCount;
    MemSize := MemSize*BlkSize;
    { * REMARK END * - Leon de Boer }
    end;
  ChangeListSize := True;
  end { TMemoryStream.ChangeListSize };

procedure TMemoryStream.Read(var Buf; Count: SW_Word);
  var
    W, CurBlock: Word;
    BlockPos: Longint;
    Li: TFileSize;
    P, Q: PByteArray;
  begin
  P := @Buf;
  if Position+Count > StreamSize then
    Error(stReadError, 0)
  else
    begin
    while (Count > 0) and (Status = stOK) do
      begin
      CurBlock := Trunc(Position / BlkSize);
      { * REMARK * - Do not shorten this, result can be > 64K }
      Li := CurBlock;
      Li := Li*BlkSize;
      { * REMARK END * - Leon de Boer }
      BlockPos := i32(Position-Li);
      W := BlkSize-BlockPos;
      if W > Count then
        W := Count;
      Q := Pointer(LongInt(BlkList^[CurBlock])+BlockPos);
      Move(Q^, P^, W);
      Position := Position + W;
      P := Pointer(LongInt(P)+W);
      Dec(Count, W);
      end;
    end;
  if Count <> 0 then
    FillChar(P^, Count, #0);
  end { TMemoryStream.Read };

procedure TMemoryStream.Truncate;
  var
    W: Word;
  begin
  if Status = stOK then
    begin
    if Position = 0 then
      W := 1
    else
      W := Trunc((Position+BlkSize-1) / BlkSize);
    if ChangeListSize(W) then
      StreamSize := Position
    else
      Error(stError, 0);
    end;
  end;

procedure TMemoryStream.Write(const Buf; Count: SW_Word);
  var
    W, CurBlock, BlockPos: Word;
    Li: TFileSize;
    P, Q: PByteArray;
  begin
  if Position+Count > MemSize then
    begin
    if Position+Count = 0 then
      W := 1
    else
      W := Trunc((Position+Count+BlkSize-1) / BlkSize);
    if not ChangeListSize(W) then
      begin
      Error(stWriteError, 0);
      Exit;
      end;
    end;
  P := @Buf;
  while (Count > 0) and (Status = stOK) do
    begin
    CurBlock := Trunc(Position / BlkSize);
    { * REMARK * - Do not shorten this, result can be > 64K }
    Li := CurBlock;
    Li := Li*BlkSize;
    { * REMARK END * - Leon de Boer }
    BlockPos := i32(Position-Li);
    W := BlkSize-BlockPos;
    if W > Count then
      W := Count;
    Q := Pointer(LongInt(BlkList^[CurBlock])+BlockPos);
    Move(P^, Q^, W);
    Position := Position + W;
    P := Pointer(LongInt(P)+W);
    Dec(Count, W);
    if Position > StreamSize then
      StreamSize := Position;
    end;
  end { TMemoryStream.Write };

{Cat:warn нужна ли эта функция?}
function GetAnyMemoryStream: PStream;
  var
    S: PStream;
  begin
  S := New(PMemoryStream, Init(MaxBytes, 2048));
  if  (S <> nil) and (S^.Status <> stOK) then
    FreeObject(S);
  GetAnyMemoryStream := S;
  end;

end.
