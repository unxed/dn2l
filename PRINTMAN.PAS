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

unit PrintMan;

interface

{$IFDEF PrintManager}
uses
  Streams, Defines, Views, Drivers,
  Dialogs, Commands, Collect
  ;

type
  PStringCol = ^TStringCol;
  TStringCol = object(TCollection)
    procedure FreeItem(P: Pointer); virtual;
    procedure PutItem(var S: TStream; P: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    end;

  PPrintManager = ^TPrintManager;
  TPrintManager = object(TListBox)
    isValid: Boolean;
    OutName: PString;
    LockUpdate: Byte;
    Paused: Boolean;
    FileLen: TFileSize;
    FilePos: TFileSize;
    Status: PView;
    PrintStream: PStream;
    Buffer: PByteArray;
    BufSize: Word;
    BufCount: Word;
    PrintDevice: PDosStream;
    constructor Init(var Bounds: TRect; AStatus: PView;
         AScrollBar: PScrollBar);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    function PrintBuffer(Num: Word): Boolean;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure PrintFile(const FileName: String);
    function SetDestination: Boolean;
    function GetStatus: Byte;
    procedure InitPrinter;
    function Valid(C: Word): Boolean; virtual;
    procedure Update; virtual;
    destructor Done; virtual;
    end;

  PPrintStatus = ^TPrintStatus;
  TPrintStatus = object(TView)
    Print: PPrintManager;
    procedure Draw; virtual;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    end;

  PPMWindow = ^TPMWindow;
  TPMWindow = object(TDialog)
    constructor Init(R: TRect);
    end;

const
  Printer: PPrintManager = nil;
  MaxBufCount = 128;

procedure SetupPrinter;
{$ENDIF}
procedure PrintFile(const S: String);

implementation

{$IFDEF PrintManager}
uses
  Startup, DNHelp, DNApp, Messages
  , Advance, Advance1, Advance2
  , VpSysLow {для Open_Access_ReadOnly}
  {$IFDEF DPMI32}, LfnVP {$ENDIF}
  ;

procedure TStringCol.FreeItem;
  begin
  DisposeStr(PString(P));
  end;

procedure TStringCol.PutItem;
  begin
  S.WriteStr(P);
  end;

function TStringCol.GetItem;
  begin
  GetItem := S.ReadStr;
  end;

constructor TPMWindow.Init;
  var
    P: PView;
    S: PView;
  begin
  inherited Init(R, GetString(dlPManagerTitle));
  Number := GetNum;
  R.Assign(Size.X-13, 1, Size.X-12, Size.Y-4);
  P := New(PScrollBar, Init(R));
  Insert(P);
  R.Assign(2, Size.Y-4, Size.X-14, Size.Y-2);
  S := New(PPrintStatus, Init(R));
  Insert(S);
  R.Assign(2, 1, Size.X-13, Size.Y-4);
  P := New(PPrintManager, Init(R, S, PScrollBar(P)));
  Insert(P);
  R.Assign(Size.X-12, 2, Size.X-2, 4);
  Insert(New(PButton, Init(R, GetString(dlDeleteButton), cmOK, 0)));
  R.Assign(Size.X-12, 4, Size.X-2, 6);
  Insert(New(PButton, Init(R, GetString(dlCloseButton), cmClose, 0)));
  R.Assign(Size.X-12, 6, Size.X-2, 8);
  Insert(New(PButton, Init(R, GetString(dlPauseButton), cmNo, 0)));
  SelectNext(False);
  HelpCtx := hcPrintManager;
  end { TPMWindow.Init };

constructor TPrintStatus.Load;
  begin
  inherited Load(S);
  GetPeerViewPtr(S, Print);
  end;

procedure TPrintStatus.Store;
  begin
  inherited Store(S);
  PutPeerViewPtr(S, Printer);
  end;

procedure TPrintStatus.Draw;
  var
    B: TDrawBuffer;
    C: Word;
    S, S1: String;
  begin
  S1 := '';
  C := GetColor($0102);
  MoveChar(B, ' ', C, Size.X);
  if  (Print^.Paused) or (Print^.List = nil) or (Print^.List^.Count = 0)
  then
    S := GetString(dlPrintingPaused)
  else
    begin
    S := GetString(dlPrinting);
    S := S+CutH(PString(Print^.List^.At(0))^, Size.X-CStrLen(S));
    if Print^.FileLen <> 0 then
      S1 := '~'+Copy(
          Strg(#219, Trunc(((Size.X-6)*Print^.FilePos) / Print^.FileLen)) +
            Strg(#177, Size.X-6),
          1, Size.X-6) +
        ' ~' + Percent(Print^.FilePos+1, Print^.FileLen+1);
    end;
  MoveCStr(B, '~'+S+'~', C);
  WriteLine(0, 0, Size.X, 1, B);
  MoveChar(B, ' ', C, Size.X);
  MoveCStr(B, S1, C);
  WriteLine(0, 1, Size.X, 1, B);
  end { TPrintStatus.Draw };

constructor TPrintManager.Init;
  var
    S: String;
  begin
  inherited Init(Bounds, 1, AScrollBar);
  Options := Options or ofPostProcess;
  Status := AStatus;
  if Status <> nil then
    PPrintStatus(Status)^.Print := @Self;
  Printer := @Self;
  BufSize := MaxBufCount;
  GetMem(Buffer, BufSize);
  LockUpdate := 0;
  isValid := False;
  Paused := True;
  OutName := nil;
  if not SetDestination then
    Exit;
  isValid := True;
  Paused := False;
  RegisterToBackground(@Self);
  end;

function TPrintManager.SetDestination;
  var
    S, S1: String;
    P: Boolean;
    OldP: PDosStream;
    hFile: LongInt;
    NotDev: Boolean;
  begin
  SetDestination := False;
  NotDev := False;
  P := Paused;
  Paused := True;
  case RPrinterSetup.Device of
    0:
      S := 'LPT1';
    1:
      S := 'LPT2';
    2:
      S := 'LPT3';
    3:
      S := 'LPT4';
    5:
      S := 'NUL';
    6:
      S := 'COM1';
    7:
      S := 'COM2';
    8:
      S := 'COM3';
    9:
      S := 'COM4';
   10:
      S := RPrinterSetup.CustomDev1;
   11:
      S := RPrinterSetup.CustomDev2;
    else {4:} {case}
      begin
      S := '';
      if InputBox(GetString(dlPrintOut), GetString(dlFileName), S, 80,
           hsPrintOut) <> cmOK
      then
        Exit;
      end;
  end {case};

  S1 := S + #0;

  //JO: чтобы не нагадить в какой-либо файл на диске
  if RPrinterSetup.Device in [10,11] then
    if {$IFDEF DPMI32}LfnVP.{$ENDIF}SysFileOpen(@S1[1],
       Open_Access_ReadOnly or open_share_DenyNone, hFile) = 0
    then
      begin
      if SysFileIsDevice(hFile) and $FF = 0 then
        //AK155: 'and $FF' необходимо, так как хелп к SysFileIsDevice
        //       неправдивый; см. хелп к DosQueryHType
        NotDev := True;
      SysFileClose(hFile);
      end;

  if NotDev then
    begin
    MessageBox(GetString(dlNotPrinter), nil, mfError+mfOKButton);
    if PrintDevice <> nil then
      PrintDevice^.Status := 0;
    Exit;
    end;

  OldP := PrintDevice;
  PrintDevice := New(PDosStream, Init(S, stCreate));

//JO: для расшаренных сетевых принтеров статус не будет stOK, т.к.
//    для них не работает SysFileSeek, вызываемая в TDOSStream.Init,
//    что тем не менее не мешает производить на них запись. Ситуацию
//    можно отловить по коду ошибки 87 (проверено по крайней мере под Win XP)
  if (PrintDevice^.Status <> stOK)
      and (PrintDevice^.ErrorInfo = 87) then
    PrintDevice^.Status := stOK;

  if PrintDevice^.Status <> stOK then
    begin
    MessageBox(GetString(dlPrintNoInit) + ^M^C'RC=' +
               ItoS(PrintDevice^.ErrorInfo), nil, mfError+mfOKButton);
    PrintDevice := OldP;
    if PrintDevice <> nil then
      PrintDevice^.Status := 0;
    Exit;
    end;

  if (RPrinterSetup.Device in [10,11])
      and (SysFileIsDevice(PrintDevice^.Handle) and $FF = 0) then
    begin
    MessageBox(GetString(dlNotPrinter), nil, mfError+mfOKButton);
    Dispose(PrintDevice, Done);
    EraseByName(S);
    PrintDevice := OldP;
    if PrintDevice <> nil then
      PrintDevice^.Status := 0;
    Exit;
    end;

  if OldP <> nil then
    Dispose(OldP, Done);
  OldP := nil;
  DisposeStr(OutName);
  OutName := NewStr(S);
  InitPrinter;
  Paused := P;
  SetDestination := True;
  end { TPrintManager.SetDestination };

constructor TPrintManager.Load;
  begin
  inherited Load(S);
  GetPeerViewPtr(S, Status);
  OutName := S.ReadStr;
  S.Read(Paused, 1);
  S.Read(FilePos, 4);
  PrintDevice := New(PDosStream, Init(OutName^, stCreate));
  BufSize := MaxBufCount;
  GetMem(Buffer, BufSize);
  isValid := True;

//JO: см. комментарий к TPrintManager.SetDestination;
  if (PrintDevice^.Status <> stOK)
      and (PrintDevice^.ErrorInfo = 87) then
    PrintDevice^.Status := stOK;
  if PrintDevice^.Status <> stOK then
    begin
    isValid := False;
    MessageBox(GetString(dlPrintNoInit)+ ^M^C'RC=' +
               ItoS(PrintDevice^.ErrorInfo), nil, mfError+mfOKButton);
    PrintDevice^.Status := 0;
    Exit;
    end;
  PrintStream := New(PBufStream, Init(PString(List^.At(0))^, stOpenRead,
         $400));
  FileLen := PrintStream^.GetSize;
  PrintStream^.Seek(FilePos);
  {PrintStream := nil;}
  Printer := @Self;
  RegisterToBackground(@Self);
  end { TPrintManager.Load };

procedure TPrintManager.Store;
  begin
  inherited Store(S);
  PutPeerViewPtr(S, Status);
  S.WriteStr(OutName);
  S.Write(Paused, 1);
  S.Write(FilePos, 4);
  end;

destructor TPrintManager.Done;
  begin
  Printer := nil;
  if PrintStream <> nil then
    Dispose(PrintStream, Done);
  PrintStream := nil;
  if PrintDevice <> nil then
    Dispose(PrintDevice, Done);
  PrintDevice := nil;
  if Buffer <> nil then
    FreeMem(Buffer, BufSize);
  Buffer := nil;
  DisposeStr(OutName);
  inherited Done;
  end;

function TPrintManager.Valid;
  begin
  Inc(LockUpdate);
  case C of
    cmValid:
      Valid := isValid and inherited Valid(C);
    cmClose:
      if MessageBox(GetString(dlPrintCancelQuery), nil, mfYesNoConfirm)
         = cmYes
      then
        begin
        Valid := True;
        InitPrinter;
        end
      else
        Valid := False;
  end {case};
  Dec(LockUpdate);
  end;

procedure TPrintManager.HandleEvent;
  begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      case Event.Command of
        cmGetName:
          PString(Event.InfoPtr)^:= GetString(dlPManagerTitle);
        cmClose:
          Event.InfoPtr := nil;
        cmNo:
          begin
          Paused := not Paused;
          if Status <> nil then
            Status^.DrawView;
          ClearEvent(Event);
          end;
        cmOK:
          begin
          Inc(LockUpdate);
          if  (List <> nil) and (List^.Count > Focused) and

                      (MessageBox(GetString(dlPDeleteQeury1)+Cut(PString(List^.At(
                      Focused))^, 40)+
                GetString(dlPDeleteQeury2), nil, mfYesNoConfirm) = cmYes)
          then
            begin
            if  (Focused = 0) and (PrintStream <> nil) then
              begin
              Dispose(PrintStream, Done);
              PrintStream := nil;
              InitPrinter;
              PrintStream := nil;
              Buffer^[0] := 12;
              repeat
              until PrintBuffer(1);
              end;
            List^.AtFree(Focused);
            SetRange(List^.Count);
            DrawView;
            if Status <> nil then
              Status^.DrawView;
            end;
          ClearEvent(Event);
          Dec(LockUpdate);
          end;
      end {case};
  end {case};
  end { TPrintManager.HandleEvent };

procedure TPrintManager.PrintFile;
  begin
  if List = nil then
    List := New(PStringCol, Init(10, 10));
  {$IFDEF DPMI32}
  List^.Insert(NewStr(lfGetLongFileName(FileName)));
  {$ELSE}
  List^.Insert(NewStr(FileName));
  {$ENDIF}
  SetRange(List^.Count);
  DrawView;
  if Status <> nil then
    Status^.DrawView;
  end;

procedure TPrintManager.InitPrinter;
//JO: под многозадачками принтер инициализировать не надо;
//    нужно будет переписать эту процедуру для DPMI32
 {var
    Hndl: Word;}
  begin
(* Hndl := PrintDevice^.Handle;
  asm
    MOV  BX, Hndl
    MOV  AX,4400H   { IOCTL, GET DEV ATTR }
    INT  21H
    JC   @@100
    {MOV  [OLD_PRINTER_ATTR],AL}
    TEST AL,80H  {; 0 - disk}
    JZ   @@100
    MOV  DH,0
    OR   DL,20H  {; BINARY MODE}
    MOV  BX,4
    MOV  AX,4401H     {; IOCTL, SET DEV ATTR}
    INT  21H
   @@100:
 end; *)
  end;

function TPrintManager.GetStatus: Byte;
{ assembler;
asm
    mov ah, 2
    mov al, $90
    mov dx, word ptr RPrinterSetup
    cmp dx, 2
    jnc @@1
    int 17h
    mov al, ah
   @@1: }
begin
  Result := $90; //временно! Доделать!
end;

function TPrintManager.PrintBuffer;
  label 1;
  const
    TryCount: Byte = 0;
  var
    B: Byte;
    BB: Boolean;
  begin
  PrintBuffer := False;
  B := GetStatus;
  if  (B and 8 <> 0) or (B and $10 = 0) then
    begin
    Inc(TryCount);
    PrintBuffer := False;
    Abort := False;
    if TryCount > 20 then
      begin
1:
      TryCount := 0;
      Paused := True;
      MessageBox(GetString(dlCantPrint), nil, mfError+mfOKButton);
      PrintBuffer := Num = 1;
      end;
    Exit;
    end;
  if  (B and $90 = 0) or (B and 1 <> 0) then
    Exit;
  PrintBuffer := True;
  {BB := NeedAbort;
 NeedAbort := True; Abort := False;}
  PrintDevice^.Status := 0;
  PrintDevice^.Write(Buffer^, Num);
  PrintDevice^.Status := 0;
  {NeedAbort := BB;}
  if Abort then
    goto 1;
  end { TPrintManager.PrintBuffer };

procedure TPrintManager.Update;
  var
    I, L: LongInt;
    S: Byte;

  procedure WriteERROR;
    var
      S: String;
    begin
    if List <> nil then
      S := PString(List^.At(0))^;
    Inc(LockUpdate);
    if  (PrintStream^.Status <> stOK) then
      MessageBox(GetString(dlCantPrintFile)+S, nil,
        mfError+mfOKButton);
    Dispose(PrintStream, Done);
    PrintStream := nil;
    if InMask(GetName(S), '$DN????$.PRN') then
      EraseFile(S);
    List^.AtFree(0);
    SetRange(List^.Count);
    FocusItem(Focused-1);
    DrawView;
    if Status <> nil then
      Status^.DrawView;
    Dec(LockUpdate);
    end;

  begin { TPrintManager.Update }
  if  (List = nil) or Paused or (LockUpdate <> 0) then
    Exit;
  if  (PrintStream = nil) then
    begin
    if  (List = nil) or (List^.Count < 1) then
      begin
      if not Owner^.GetState(sfDragging) then
        Owner^.Free;
      Exit;
      end;
    PrintStream := New(PBufStream, Init(PString(List^.At(0))^,
           stOpenRead, $400));
    FileLen := 0;
    FilePos := 0;
    if PrintStream^.Status <> stOK
    then
      WriteERROR
    else
      begin
      FileLen := PrintStream^.GetSize;
      if RPrinterSetup.InitPrinter = '' then
        begin
        BufCount := MaxBufCount;
        if BufCount > FileLen then
          BufCount := i32(FileLen);
        PrintStream^.Read(Buffer^, BufCount);
        end
      else
        begin
        BufCount := Length(RPrinterSetup.InitPrinter);
        Move(RPrinterSetup.InitPrinter[1], Buffer^, BufCount);
        end;
      end;
    end
  else
    begin
    if PrintBuffer(BufCount) then
      begin
      FilePos := PrintStream^.GetPos;
      if  (FilePos >= FileLen) or (PrintStream^.Status <> stOK) then
        begin
        if RPrinterSetup.AfterFile <> '' then
          begin
          BufCount := Length(RPrinterSetup.AfterFile);
          Move(RPrinterSetup.AfterFile[1], Buffer^, BufCount);
          repeat
          until PrintBuffer(BufCount);
          if not Paused then
            WriteERROR;
          end
        else if not Paused then
          WriteERROR;
        end
      else
        begin
        BufCount := MaxBufCount;
        if BufCount+FilePos > FileLen then
          BufCount := i32(FileLen-FilePos);
        PrintStream^.Read(Buffer^, BufCount);
        end;
      end;
    end;
  if Status <> nil then
    Status^.DrawView;
  end { TPrintManager.Update };

procedure SetupPrinter;
  var
    LastDest: Word;
  begin
  LastDest := RPrinterSetup.Device;
  if ExecResource(dlgPrinterSetup, RPrinterSetup) = cmOK then
    Message(Application, evCommand, cmUpdateConfig, nil);
  if  (LastDest <> RPrinterSetup.Device) and (Printer <> nil) then
    Printer^.SetDestination;
  end;

procedure PrintFile(const S: String);
  var
    W: PView;
    R: TRect;
  begin
  R.Assign(0, 0, 50, 9);
  if Printer = nil then
    begin
    W := Application^.ValidView(New(PPMWindow, Init(R)));
    if W <> nil then
      begin
      W^.Hide;
      Desktop^.InsertView(W, Desktop^.Last);
      W^.Show;
      end;
    end;
  if Printer <> nil then
    Printer^.PrintFile(S);
  end;
{$ELSE}
uses
  Messages
  ;

procedure PrintFile(const S: String);
  begin
  MessageBox('Simulating: '+S, nil, mfOKButton);
  end;
{$ENDIF}

end.
