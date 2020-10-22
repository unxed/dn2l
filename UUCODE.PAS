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
{AK155 = Alexey Korop, 2:461/155@fidonet}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{AK155
   Переписал все ассемблерные и inline подпрограммы под
   32-разрядный VP. При этом никаких условных компиляций не делал.
   Мутную и неэффективную процедуру MoveSection переписал короче и проще.

   По состоянию на 08/03/2001 компилируется только декодирование.

   04/06/2001 - подключил кодирование с новым модулем uue2inc.pas (см.).
   По-прежнему все ассемблерные вставки чисто 32-разрядные.
}
{Cat
   23/08/2001 - переделал очень много чего, основное - заменил в операциях
   с дисковыми буферами, да и во многих других местах Word-ы на Longint-ы,
   что позволяет избавиться от ограничения в 900 строк в секции и наверняка
   ещё кучи глюков, которые выискивать не хочется - проще исправить  ;-)
}
unit UUCode;

interface

uses
  Defines
  {$IFDEF UUDECODE}
  , Collect
  {$ENDIF}
  {$IFDEF UUENCODE}
  , Uue2Inc
  {$ENDIF}
  ;
{$IFDEF UUDECODE}
procedure UUDecode(AFileCollection: PCollection);
{$ENDIF}

{$IFDEF UUENCODE}
procedure UUEncode(const FName: String);
{$ENDIF}

implementation

uses
  Files, VPUtils, Tree
  , Dos, Lfn {DataCompBoy}
  , Advance, Advance1, Advance2, Views, Startup, Dialogs,
  xTime, FilesCol, DNApp, Drivers, Gauge, Messages, Commands,
  FileCopy, HistList, DNUtil
  , Events
  , use16
  ;

function longmul(X, Y: LongInt): LongInt;
  inline;
  begin
  longmul := LongInt(X)*LongInt(Y);
  end;

const
  TmpExt = 'UUS';

function MaxAvail: LongInt;
  begin
  MaxAvail := MemAdjust(System.MaxAvail);
  end;

var
  Timer: TEventTimer;

function ApplicationIdle(Info: PWhileView): Boolean;
  var
    C: Boolean;
  begin
  C := False;
  ApplicationIdle := False;
  DispatchEvents(Info, C);
  C := C or CtrlBreakHit;
  if C then
    begin
    ApplicationIdle := Msg(dlQueryAbort, nil, mfYesNoConfirm) = cmYes;
    NewTimer(Timer, 0);
    end;
  CtrlBreakHit := False;
  end;

procedure RereadGlobal(OutputDir: String); {DataCompBoy}
  begin
  GlobalMessage(evCommand, cmPanelReread, @OutputDir);
  GlobalMessage(evCommand, cmRereadTree, @OutputDir);
  end;

{$IFDEF UUDECODE}

{!  описания !}

var
  PI: PWhileView;
  R: TRect;
var
  ErrorNumber: Integer;
  GoodNumber: Integer;
const
  Digits: set of Char = ['0'..'9'];
  OutBufSize = $1000;
type
  TDskBufArr = array[1..$FFFF] of Byte;
  PDskBufArr = ^TDskBufArr;
  PSection = ^TSection;
  TSection = record
    Number: LongInt;
    TmpFileId: LongInt;
    Size: TSize;
    end;

  TSectArr = array[1..990] of TSection;
  PSectArr = ^TSectArr;

  TSectInfo = record
    MaxSectKnw: Boolean;
    TotSect: LongInt;
    end;

  PFile = ^TFile;
  TFile = record
    FName: PString;
    EndFound: Boolean;
    CrcKnown: Boolean;
    ForceSkp: Boolean;
    Flushed: Boolean;
    RealSize: LongInt;
    Size: TSize;
    RealCRC: Word;
    CRC: Word;
    S: TSectInfo;
    FileTime: LongInt;
    NSect: LongInt;
    Sect: PSectArr;
    Broken: Boolean;
    end;

  TFileArr = array[1..1500] of TFile;
  PFileArr = ^TFileArr;

const
  GrowStep = 16;

var
  BackSize: Byte;
  KickBack: Boolean;
  BackCRC: Word;
  BackBuf: String;
  FileArr: PFileArr;
  NFileArr: LongInt;
  TmpCRC: Word;
  SectCRC: Word;
  SectSSz: LongInt;
  InputStream: PTextReader;
  TmpInFile: lFile;
  OutFile: lFile;
  CurFileName: String;
  OrgT: String;
  OrgTLen: Byte absolute OrgT;
  StrT: String;
  AuxT: String;
  OutFName: String;
  InFName: String;
  StrL: Byte absolute StrT;
  AuxL: Byte absolute AuxT;
  CurPSection: PSection;
  CurPFile: PFile;
  UUfound,
  BadSection,
  ForceSkip,
  SectionGo,
  exTable,
  exBegin,
  exSUM,
  exFileTime,
  exEnd,
  FileNameKnown,
  FileTimeKnown,
  NumSectKnown,
  PrevTmp,
  ForceEOF,
  MaxSectKnown: Boolean;
  MaxSectNum,
  CurSectNo: LongInt;
  NumErr: LongInt;
  SubPos: Byte;
  WholeSectCRC: Word;
  OutBufPos: LongInt;
  POutBuf: PDskBufArr;
  CurSectSize,
  CurFileTime,
  HeapBegin,
  HeapEnd: LongInt;
  {!  описания !}

function MatchCurFileName(const FName: string): Boolean;
  { AK155 8-11-2004 Делается попытка сопоставить FName и CurFileName.
  Если они равны (с точностью до регистра), то успех.
    Если не равны, то проверяется не случай ли это типа
  FName = 'Untitled-2_.jpg' и
  CurFileName = 'Untitled-2_.jpg  FastPOST/WIN32 2.3rc10/2.0.0',
  Заранее всё после пробела отбросить нельзя, так как пробел может быть
  и в имени файла. В этом случае также успех, и при этом CurFileName
  усекается до длины FName.
  }
  var
    l: Integer;
    i: Integer;
  begin
  l := Length(FName);
  Result := False;
  if l > Length(CurFileName) then
    exit; // совсем не совпадают
  for i := 1 to l do
    if Upcase(FName[i]) <> Upcase(CurFileName[i]) then
      exit; // совсем не совпадают
  if l <> Length(CurFileName) then
    begin // FName совпал с началом CurFileName
    if (CurFileName[l+1] <> ' ') then
      exit;
    SetLength(CurFileName, l);
    end;
  Result := true;
  end;

  {-DataCompBoy-}
function UU_Decode(
    AFileCollection: PCollection;
    const OutputDir: String; ChkOvr, DispErr, RecoverBrokenUUE: Boolean)
  : Boolean;
  {! тут были описания !}

  procedure FReadLn;
    begin
    OrgT := InputStream^.GetStr;
    end;

  procedure PrepareWrite;
    begin
    OutBufPos := 65535;
    GetMem(POutBuf, OutBufSize)
    end;

  procedure FileRewrite(const FName: String);
    begin
    lAssignFile(OutFile, FName);
    lReWriteFile(OutFile, 1);
    ErrorFCode := IOResult;
    if ErrorFCode = 0 then
      PrepareWrite;
    end;

  procedure BWrite(var Buf; Sz: LongInt);
    var
      a: LongInt;
    begin
    BlockWrite(OutFile.F, Buf, Sz, a);
    if a <> Sz then
      ErrorFCode := 101
    else
      ErrorFCode := IOResult;
    end;

  procedure PutBlock(var OutBuf; Sz: LongInt);
    var
      Buf: TDskBufArr absolute OutBuf;
    begin
    if Sz = 0 then
      Exit;
    if OutBufPos = 65535 then
      begin
      Move(Buf, POutBuf^, Sz);
      OutBufPos := Sz;
      end
    else if OutBufPos+Sz >= OutBufSize then
      begin
      if OutBufPos+Sz = OutBufSize then
        begin
        Move(Buf, POutBuf^[OutBufPos+1], Sz);
        BWrite(POutBuf^, OutBufSize);
        if ErrorFCode > 0 then
          Exit;
        OutBufPos := 0;
        end
      else
        begin
        Move(Buf, POutBuf^[OutBufPos+1], OutBufSize-OutBufPos);
        BWrite(POutBuf^, OutBufSize);
        if ErrorFCode > 0 then
          Exit;
        Move(Buf[OutBufSize-OutBufPos+1], POutBuf^,
             Sz-(OutBufSize-OutBufPos));
        OutBufPos := Sz-(OutBufSize-OutBufPos);
        end;
      end
    else
      begin
      Move(Buf, POutBuf^[OutBufPos+1], Sz);
      Inc(OutBufPos, Sz)
      end;
    end { PutBlock };

  procedure FlushOutBuf;
    var
      Dummy: Byte;
    begin
    if OutBufPos > 0 then
      BWrite(POutBuf^, OutBufPos);
    FreeMem(POutBuf, OutBufSize);
    Close(OutFile.F);
    Dummy := IOResult;
    POutBuf := nil;
    end;

  procedure CancelOutFile;
    var
      Dummy: Byte;
    begin
    Close(OutFile.F);
    Dummy := IOResult;
    if POutBuf <> nil then
      FreeMem(POutBuf, OutBufSize);
    POutBuf := nil;
    end;

  function RExpand(s: String; n: Byte): String;
    begin
    while Length(s) < n do
      s := s+' ';
    RExpand := s
    end;

  procedure GrArr(var p; var Index: LongInt; Sz: LongInt);
    var
      a1: Pointer;
      a: Pointer absolute p;
      BufS: LongInt;
    begin
    if Index mod GrowStep = 0 then
      begin
      GetMem(a1, (Index+GrowStep)*Sz);
      if Index > 0 then
        begin
        BufS := Index*Sz;
        Move(a^, a1^, BufS);
        FreeMem(a, BufS);
        end;
      a := a1;
      end;
    Inc(Index);
    end;

  procedure CalcBufCRC(var Buf; Size: LongInt; var PrevSum: Word);
    assembler;
    {&USES esi} {&FRAME-}
  asm
        mov     ecx,Size
        jecxz   @@Ext
        mov     esi,Buf
        mov     edi,PrevSum
        mov     dx,word ptr [edi]
        xor     ax,ax
 @@1:   lodsb
        ror     dx,1
        add     dx,ax
        loop    @@1
        mov     word ptr [edi],dx
 @@Ext:
end;

  {Cat: не всегда она будет возвращать Word, что приводит к глюкам}
  function SmartDiv(L: LongInt; W: LongInt): LongInt;
    assembler;
    {&USES None} {&FRAME-}
  asm
         mov     eax,L
         xor     edx,edx
         div     W
         mov     edx,eax
         and     edx,$FFFF0000
         jz      @@Exit
         inc     eax
@@Exit:
end;

  procedure FreeArr(var pp; Index, Sz: LongInt);
    var
      p: Pointer absolute pp;
    begin
    FreeMem(p, SmartDiv(Index, GrowStep)*GrowStep*Sz);
    end;

  function SSStr(a: LongInt; n: Byte; Ch: Char): String;
    var
      s: String;
      i: Byte;
    begin
    Str(a: n, s);
    for i := 1 to n do
      if s[i] = ' ' then
        s[i] := Ch
      else
        Break;
    SSStr := s;
    end;

  procedure DelSpaces(var s: String);
    begin
    DelLeft(s);
    DelRight(s)
    end;

  function EndOfFile: Boolean;
    begin
    if ForceEOF then
      begin
      EndOfFile := True;
      Exit
      end;
    EndOfFile := InputStream^.Eof;
    end;

  function UUString(var s: String; var CRC: Word): Boolean;
    assembler;
    {&USES esi} {&FRAME+}
  asm
        cld
        mov     esi,CRC
        mov     dx,word ptr[esi]
        mov     esi,s
        mov     ah,0
        xor     ecx,ecx
        lodsb
        mov     cl,al

        push    ecx
        mov     edi,s
        add     edi,ecx
        inc     edi
        mov     al,0
        mov     ecx,70
        rep     stosb
        pop     ecx

        lodsb
        ror     dx,1
        add     dx,ax
        cmp     al,'!'
        jb      @@NonUU
        cmp     al,'`'
        ja      @@NonUU
        sub     al,' '
        and     al,3Fh
        mov     byte ptr[esi-1],al
        dec     cl
 @@L:   lodsb
        cmp     al,0
        jz      @@UU
        ror     dx,1
        add     dx,ax
        cmp     al,' '
        jb      @@NonUU
        cmp     al,'`'
        ja      @@NonUU
        sub     al,' '
        and     al,3Fh
        mov     byte ptr[esi-1],al
        loop    @@L
        jmp     @@UU
@@NonUU:mov     al,0
        jmp     @@End
@@UU:   mov     esi,CRC
        ror     dx,1
        add     dx,0Ah
        mov     word ptr[esi],dx
        mov     al,1
@@End:

end;

  function MemEqu(var A, B; Size: LongInt): Boolean;
    assembler;
    {&USES esi} {&FRAME-}
  asm
         cld
         mov   ecx,Size
         mov   esi,A
         mov   edi,B
         repe  cmpsb
         mov   eax,1
         je    @@EQ
         dec   al
@@EQ:

end;

  procedure DelTBeg(b: Byte);
    begin
    Delete(StrT, 1, b)
    end;

  procedure FindStr(const s: String);
    begin
    SubPos := Pos(s, StrT)
    end;

  function StrTBegins(s: String): Boolean;
    var
      sl: Byte absolute s;
      b: Boolean;
    begin
    b := (StrL >= sl) and MemEqu(StrT[1], s[1], sl);
    StrTBegins := b;
    if b then
      DelTBeg(sl);
    DelLeft(StrT);
    end;

  procedure GetW(var s, w: String);
    var
      sl: Byte absolute s;
    begin
    w := '';
    DelSpaces(s);
    if s = '' then
      Exit;
    while (sl > 0) and (s[1] <> ' ') do
      begin
      w := w+s[1];
      Delete(s, 1, 1); {DelFC(s)}
      end;
    Delete(s, 1, 1); {DelFC(s);}
    end;

  procedure GetN(var s, w: String);
    var
      sl: Byte absolute s;
    begin
    w := '';
    DelSpaces(s);
    if s = '' then
      Exit;
    while (sl > 0) and (s[1] in Digits) do
      begin
      w := w+s[1];
      Delete(s, 1, 1); {DelFC(s)}
      end;
    Delete(s, 1, 1); {DelFC(s);}
    end;

  procedure GetWord;
    begin
    GetW(StrT, AuxT)
    end;

  function ValidWNumber(var Num: Word): Boolean;
    begin
    Val(AuxT, Num, NumErr);
    ValidWNumber := NumErr = 0
    end;

  function ValidLNumber(var Num: LongInt): Boolean;
    begin
    Val(AuxT, Num, NumErr);
    ValidLNumber := NumErr = 0
    end;

  procedure ClearEX;
    begin
    NumSectKnown := False;
    MaxSectKnown := False;
    FileNameKnown := False;
    SectionGo := False;
    FileTimeKnown := False;
    exFileTime := False;
    exEnd := False;
    exBegin := True;
    exTable := False;
    end;

  function Complete__File: Boolean;
    begin
    Complete__File := False;
    if not CurPFile^.S.MaxSectKnw then
      Exit;
    if CurPFile^.S.TotSect <> CurPFile^.NSect then
      Exit;
    Complete__File := True;
    end;

  procedure Local_Error(const s: String; Severe: Boolean);

    procedure MB(const ss: String);
      begin
      MessageBox(ss, nil, mfError+mfOKButton);
      NewTimer(Timer, 0);
      end;

    begin
    if DispErr then
      MB(s);
    Inc(ErrorNumber);
    if PI <> nil then
      PI^.Write(6, GetString(dlUUDecodeErrors)+SSStr(ErrorNumber, 3, ' '));
    if  (Severe) and (CurPFile <> nil) then
      with CurPFile^ do
        begin
        if not Broken then
          begin
          Broken := True;
          if  (not RecoverBrokenUUE) and (DispErr) then
            MB(GetString(dlleFailedFinal)+' '+CurPFile^.FName^);
          end;
        end;
    end { Local_Error };

  procedure LocalError(const s: String);
    begin
    Local_Error(s, True);
    end;

  procedure LocalWarning(const s: String);
    begin
    Local_Error(s, False);
    end;

  procedure CheckFileSize(Listed, Calculated: TSize);
    begin
    if Listed <> Calculated then
      LocalError(GetString(dlleFileSizeMismatch)+' '+CurPFile^.FName^+
        ', '+GetString(dlleListed)+'='+ZtoS(Listed)+', '+
        GetString(dlleCalculated)+'='+ZtoS(Calculated));
    end;

  procedure CheckFileCRC(Listed, Calculated: Word);
    begin
    if Listed <> Calculated then
      LocalError(GetString(dlleFileCRCMismatch)+' '+CurPFile^.FName^+
        ', '+GetString(dlleListed)+'='+ItoS(Listed)+', '+
        GetString(dlleCalculated)+'='+ItoS(Calculated));
    end;

  procedure CompileFile;
    var
      i: LongInt;
      PA, FA: LongInt;
      MissS: String;
      SingleMiss: Boolean;

    procedure MoveSection(var a: TSection);
      const
        BufSize = 1024;
      var
        Buf: array[1..BufSize] of Byte;
        c: LongInt;
        i: TSize;
        l: LongInt;

      begin
      InFName := CalcTmpFName(a.TmpFileId, TmpExt, False);
      lAssignFile(TmpInFile, InFName);
      FileMode := $40;
      lResetFile(TmpInFile, 1);
      if IOResult <> 0 then
        LocalError(GetString(dlleErrorOpenTMP));
      i := a.Size;
      while i <> 0 do
        begin
        l := BufSize;
        if i < BufSize then
          l := {Cat:warn}Round(i);
        BlockRead(TmpInFile.F, Buf, l);
        CalcBufCRC(Buf, l, CurPFile^.CRC);
        PutBlock(Buf, l);
        i := i-l;
        end;
      Close(TmpInFile.F);
      EraseFile(InFName);
      CurPFile^.Size := CurPFile^.Size+a.Size;
      end { MoveSection };

    procedure ClearFAPA;
      begin
      PA := 0;
      FA := 0
      end;

    procedure SetMiss;
      begin
      if MissS <> '' then
        MissS := MissS+', ';
      if PA = FA then
        MissS := MissS+ItoS(FA)
      else
        begin
        MissS := MissS+ItoS(FA)+'-'+ItoS(PA);
        end;
      ClearFAPA;
      end;

    procedure CheckPresence;
      var
        ii: LongInt;
      begin
      for ii := 1 to CurPFile^.NSect do
        if CurPFile^.Sect^[ii].Number = i then
          begin
          if FA > 0 then
            SetMiss;
          MoveSection(CurPFile^.Sect^[ii]);
          Exit;
          end;
      if FA = 0 then
        begin
        FA := i;
        PA := i;
        end
      else
        begin
        if PA+1 = i then
          Inc(PA)
        else
          SetMiss;
        end;
      end { CheckPresence };

    procedure CheckC(var T: TFile);
      begin
      with T do
        begin
        CheckFileSize(RealSize, Size);
        CheckFileCRC(RealCRC, CRC);
        end;
      end;

    procedure SetFileTime(ft: LongInt);
      var
        f: lFile;
      begin
      if ft = 0 then
        Exit;
      {Cat: изменил FileMode с $40 на 2 - иначе время не ставится}
      lAssignFile(f, OutputDir+OutFName);
      FileMode := 2;
      lResetFile(f, 1);
      SetFTime(f.f, ft);
      Close(f.f);
      end;

    procedure ReportMiss;
      var
        s: String;
      begin
      s := ' '+MissS+' '+GetString(dlleOfFile)
          +' '+OutFName+' ('+ItoS(CurPFile^.s.TotSect)+') ';
      SingleMiss := (Pos('-', MissS) = 0) and (Pos(',', MissS) = 0);
      if SingleMiss then
        LocalError(GetString(dlleSection)+s+GetString(dlleIsAbsent))
      else
        LocalError(GetString(dlleSections)+s+GetString(dlleAreAbsent));
      end;

    begin { CompileFile }
    ClearFAPA;
    MissS := '';
    SingleMiss := True;
    OutFName := CurPFile^.FName^;
    CurPFile^.Flushed := True;
    FileRewrite(OutputDir+OutFName);
    if ErrorFCode <> 0 then
      begin
      LocalError(GetString(dlleCantCreate)+' '+OutFName);
      Exit;
      end;
    for i := 1 to CurPFile^.S.TotSect do
      CheckPresence;
    FlushOutBuf;
    if CurPFile^.S.TotSect = CurPFile^.NSect then
      begin
      SetFileTime(CurPFile^.FileTime);
      if CurPFile^.EndFound then
        Inc(GoodNumber)
      else
        LocalError(GetString(dlleNoTerm)+' ('+OutFName+')');
      if CurPFile^.CrcKnown then
        CheckC(CurPFile^);
      end
    else
      begin
      if FA > 0 then
        SetMiss;
      ReportMiss;
      LocalError(GetString(dlleFailedToDecode)+' '+OutFName);
      end;
    FreeArr(CurPFile^.Sect, CurPFile^.NSect, SizeOf(TSection));
    CurFileName := '';
    end { CompileFile };

  procedure TerminateSection;

    procedure TerminateSmth;
      begin
      CurPSection^.Size := CurSectSize;
      if Complete__File then
        CompileFile;
      end;

    begin
    KickBack := False;
    if BadSection then
      begin
      CancelOutFile;
      Exit;
      end;
    FlushOutBuf;
    if NumSectKnown then
      TerminateSmth;
    end;

  procedure ClearCRC;
    begin
    SectSSz := 0;
    SectCRC := 0;
    CurSectSize := 0;
    WholeSectCRC := 0;
    end;

  procedure SetFN;
    var
      D: String;
      N: String;
      E: String;
      nl: Byte absolute N;
      el: Byte absolute E;
      i, i1: Integer;
    begin
    {AK155  8-11-2004
    В качестве имени файла не просто берём первое слово, что неверно для
    длинных имён с пробелами, а таки пытаемся угадать длинное имя.
    Если есть имя в кавычках - берём его; если есть знак '<' (который мы
    генерируем - берём до него); если нет ни того, ни другого - берём
    до конца строки. При этом можно подхватить и лишнее, но его, скорее
    всего, потом отсечёт MatchCurFileName }
    i := Pos('"', StrT);
    i1 := Pos('<', StrT)-1;
    if (i <> 0) and ((i < i1) or (i1 < 0)) then
      begin
      System.Delete(StrT, 1, i);
      i1 := Pos('"', StrT)-1;
      end;
    if i1 < 0 then
      i1 := length(StrT);
    while StrT[i1] = ' ' do
      Dec(i1);
    AuxT := Copy(StrT, 1, i1);
    {UpStr(AuxT);}lFSplit(AuxT, D, N, E); {nl:=Min(nl,8);el:=Min(el,4);}
    {JO: для OS/2 это как-то странно}
    AuxT := N+E;
    end;

  procedure OutString;forward;

  procedure Get_String;

    function SetSection: Boolean;

      procedure SetCurFName;
        begin
        SetFN;
        CurFileName := AuxT;
        SetSection := True;
        FileNameKnown := True;
        exFileTime := True;
        end;

      begin
      SetSection := False;
      if StrT = '' then
        Exit;
      GetWord;
      if StrT = '' then
        Exit;
      if not ValidLNumber(CurSectNo) then
        Exit;
      GetWord;
      if StrT = '' then
        Exit;
      if AuxT <> 'of' then
        Exit;
      GetWord;
      if StrT = '' then
        Exit;
      if not ValidLNumber(MaxSectNum) then
        MaxSectNum := 0
      else
        GetWord;
      if StrT = '' then
        Exit;
      if AuxT = 'file' then
        begin
        NumSectKnown := True;
        SetCurFName;
        Exit;
        end;
      FindStr('file');
      DelTBeg(SubPos-1);
      GetWord;
      if StrT = '' then
        Exit;
      SetCurFName;
      NumSectKnown := True;
      if MaxSectNum > 0 then
        MaxSectKnown := True;
      end { SetSection: };

    procedure SetBegin;
      begin
      GetWord;
      if StrT = '' then
        begin
        LocalError(GetString(dlleFileNameExp));
        Exit
        end;
      SetFN;
      if FileNameKnown and not MatchCurFileName(AuxT) then
        LocalError(GetString(dlleFileNamesMismatch));
      CurFileName := AuxT;
      FileNameKnown := True;
      exFileTime := False;
      end;

    function SetFileTime: Boolean;
      begin
      SetFileTime := False;
      if StrT = '' then
        begin
        LocalError(GetString(dlleFileTimeExp));
        Exit
        end;
      GetWord;
      if not ValidLNumber(CurFileTime) then
        begin
        LocalError(GetString(dlleInvFileTimeNum));
        Exit
        end;
      FileTimeKnown := True;
      end;

    procedure CalcLnCRC(var Strng: String; var CRC: Word);
      assembler;
      {&USES esi} {&FRAME-}
    asm
        cld
        xor     ecx,ecx
        mov     esi,Strng
        lodsb
        mov     cl,al
        jecxz    @@End
        mov     edi,CRC
        mov     dx,word ptr [edi]
        xor     ax,ax
 @@1:   lodsb
        ror     dx,1
        add     dx,ax
        loop    @@1
        ror     dx,1
        add     dx,0Ah
        mov     word ptr [edi],dx
 @@End:
end;

    procedure CheckSum;
      var
        Num: String;

      function CheckS: Boolean;
        var
          c: Word;
          s: LongInt;
        begin
        CheckS := False;
        GetN(Num, StrT);
        if Num = '' then
          Exit;
        if AuxT = 'section' then
          begin
          AuxT := StrT;
          if not ValidWNumber(TmpCRC) then
            Exit;
          if TmpCRC <> WholeSectCRC then
            begin
            LocalError
              (GetString(dlleCRC_Err)+' '#196' '+CurPFile^.FName^+', '+
              GetString(dlleSection)+' '+ItoS(CurSectNo));
            end;
          AuxT := Num;
          if not ValidLNumber(s) then
            Exit;
          if s <> SectSSz then
            begin
            LocalError
              (GetString(dlleSizeMism)+' '#196' '+CurPFile^.FName^+', '+
              GetString(dlleSection)+' '+ItoS(CurSectNo));
            end;
          CheckS := True;
          end
        else if AuxT = 'entire' then
          begin
          AuxT := StrT;
          if not ValidWNumber(c) then
            Exit;
          AuxT := Num;
          if not ValidLNumber(s) then
            Exit;
          CheckS := True;
          if CurPFile^.Flushed then
            begin
            CheckFileSize(s, CurPFile^.Size);
            CheckFileCRC(c, CurPFile^.CRC);
            end
          else
            begin
            CurPFile^.RealCRC := c;
            CurPFile^.RealSize := s;
            CurPFile^.CrcKnown := True;
            end;
          end;
        end { CheckS: };

      begin { CheckSum }
      GetWord;
      Num := AuxT;
      if StrT = '' then
        Exit;
      GetWord;
      if not CheckS then
        LocalError(GetString(dlleChkSumFmt));
      end { CheckSum };

    procedure CalcCRC;
      begin
      KickBack := False;
      CalcLnCRC(OrgT, SectCRC);
      Inc(SectSSz, OrgTLen+1);
      end;

    procedure FlushBack;
      var
        s: String;
      begin
      if POutBuf = nil then
        Exit;
      {AK155
Такое бывает при массовом раскодировании, когда в одном файле был ююк
без section, но с '`', затем встретился файл с 'M', а затем - файл с 'end',
но без ююков (скажем, программа на Паскале). Так по этому end происходит
вызов с POutBuf = nil. А предотвратить такое сложно, не разрушив при этом
возможность раскодирования всяких не вполне корректных ююков}
      if KickBack = True then
        begin
        s := StrT;
        StrT := BackBuf;
        OutString;
        StrT := s;
        SectCRC := BackCRC;
        Inc(SectSSz, BackSize);
        KickBack := False;
        end;
      end { FlushBack };

    function ClearT: Byte;
      var
        Idx: Byte;
        c: Char;
      begin
      Idx := 1;
      Inc(OrgTLen);
      OrgT[OrgTLen] := #0;
      repeat
        c := OrgT[Idx];
        if c = #0 then
          Break;
        if  (c = ' ') and (OrgT[Idx+1] > #$7F) then
          Delete(OrgT, Idx, 2)
        else
          Inc(Idx);
      until False;
      ClearT := Idx;
      Dec(OrgTLen);
      end;

    procedure DoTable;
      var
        i: Byte;
      begin
      ClearCRC;
      CalcCRC;
      for i := 0 to 1 do
        begin
        repeat
          FReadLn
        until OrgT <> '';
        CalcCRC
        end;
      end;

    begin { Get_String }
    repeat
      if EndOfFile then
        Exit;
      FReadLn;
      if  (OrgT = '') or (OrgTLen > 120)
           or (not (OrgT[1] in [' '..'z']))
      then
        Continue;
      if  (ClearT = 1) then
        Continue;
      if OrgT[1] = ' ' then
        Continue;
      FillChar(StrT, 70, ' ');
      StrT := OrgT;
      if StrT[1] = 'M' then
        begin
        TmpCRC := SectCRC;
        KickBack := False;
        if UUString(StrT, TmpCRC) then
          begin
          SectCRC := TmpCRC;
          Inc(SectSSz, OrgTLen+1);
          Break
          end;
        end
      else
        begin
        if SectionGo then
          if  (StrT = '`') or (StrT = '``') then
            begin
            if BadSection then
              begin
              exSUM := False;
              ClearEX;
              Continue;
              end
            else
              begin
              if CurPFile <> nil then
                begin
                CurPFile^.EndFound := True;
                CurPFile^.S.MaxSectKnw := True;
                CurPFile^.S.TotSect := CurSectNo;
                end;
              FlushBack;
              CalcCRC;
              TerminateSection;
              ClearEX;
              exEnd := True;
              Continue
              end;
            end;
        if exSUM then
          if StrTBegins('sum -r/size ') then
            begin
            exEnd := False;
            if BadSection then
              begin
              ClearEX;
              Continue;
              end
            else
              begin
              if WholeSectCRC = 0 then
                WholeSectCRC := SectCRC;
              if CurPFile <> nil then
                CheckSum;
              if SectionGo then
                begin
                TerminateSection;
                ClearEX
                end;
              ClearCRC;
              Continue;
              end
            end;
        if exFileTime then
          if StrTBegins('filetime ') then
            begin
            ClearCRC;
            if SetFileTime then
              ;
            Continue
            end;
        if exBegin then
          if StrTBegins('begin ') then
            begin
            if SectionGo then
              begin
              TerminateSection;
              ClearEX
              end;
            if exTable then
              exTable := False
            else
              ClearCRC;
            SetBegin;
            CalcCRC;
            Continue
            end;
        if exEnd then
          if StrTBegins('end') then
            begin
            FlushBack;
            CalcCRC;
            WholeSectCRC := SectCRC;
            exEnd := False;
            if CurPFile <> nil then
              if not CurPFile^.EndFound then
                LocalError(GetString(dlleUnexpEND));
            Continue
            end;
        if StrTBegins('section ') then
          begin
          if SectionGo then
            begin
            TerminateSection;
            ClearEX
            end;
          if not SetSection then
            begin
            LocalError(GetString(dlleSectionHdr));
            BadSection := True;
            end
          else
            begin
            if CurSectNo > 1 then
              exBegin := False;
            exTable := True;
            ClearCRC;
            end;
          Continue;
          end;
        if exTable then
          if StrT = 'table' then
            begin
            DoTable;
            Continue;
            end;
        BackCRC := SectCRC;
        BackBuf := StrT;
        BackSize := OrgTLen+1;
        if UUString(BackBuf, BackCRC) then
          begin
          if SectionGo then
            begin
            KickBack := True;
            Continue
            end
          else if FileNameKnown then
            begin
            SectCRC := BackCRC;
            Inc(SectSSz, BackSize);
            Move(BackBuf, StrT, BackSize);
            Break;
            end
          end;
        end;
    until False;
    end { Get_String };

  procedure DecodeStr(var Src, Dst);
    assembler;
    {&USES esi,ebx}
    var
      Cnt: Byte;
      asm
        cld
        mov     esi,Src
        mov     edi,Dst
        mov     Cnt,15
@@L:    lodsw
        mov     ch,ah
        mov     cl,2
        shl     al,cl
        mov     cl,4
        shr     ah,cl
        or      al,ah
        stosb
        lodsw
        mov     bl,al
        shl     ch,cl
        mov     cl,2
        shr     al,cl
        or      al,ch
        stosb
        mov     al,bl
        mov     cl,6
        shl     al,cl
        or      al,ah
        stosb
        dec     Cnt
        jnz     @@L
end;

    procedure OutString;
      var
        b: array[1..45] of Byte;
      begin
      if BadSection then
        Exit;
      DecodeStr(StrT[2], b);
      PutBlock(b, Byte(StrT[1]));
      CurSectSize := CurSectSize+Byte(StrT[1]);
      end;

    procedure GoSect;
      begin
      exBegin := True;
      exEnd := True;
      SectionGo := True
      end;

    procedure OpenOutFile;
      begin
      GoSect;
      if BadSection then
        Exit;
      FileRewrite(OutFName);
      if ErrorFCode <> 0 then
        begin
        LocalError(GetString(dlleCantCreate)+' '+OutFName);
        BadSection := True;
        end;
      end;

    procedure CheckExist;
      var
        f: String;
        P: Pointer;
        i, j: LongInt;
      begin
      if not ChkOvr then
        Exit;
      j := 0;
      for i := 1 to NFileArr do
        if UpStrg(FileArr^[i].FName^) = UpStrg(CurFileName) then
          begin
          j := i;
          Break
          end;
      if j = 0 then
        begin
        f := OutputDir+CurFileName;
        P := @F;
        if ExistFile(f) then
          begin
          case MessageBox(GetString(dlFileExist)+^M, @P,
             mfQuery+mfYesButton+mfNoButton+mfCancelButton+mfAllButton) of
            { cmYes:;}
            cmOK:
              begin
              ChkOvr := False;
              end;
            cmNo:
              begin
              BadSection := True;
              ForceSkip := True
              end;
            cmCancel:
              begin
              BadSection := True;
              ForceEOF := True
              end;
          end {case};
          NewTimer(Timer, 0);
          end;
        end
      else
        begin
        ForceSkip := FileArr^[j].ForceSkp;
        BadSection := ForceSkip;
        end;
      end { CheckExist };

    procedure DecodeUnheadered;

      function CalcUnknownName: String;
        var
          Num: LongInt;
          u: String;
        procedure CalcMaxUnk(var s: String);
          var
            v: LongInt;
            c: LongInt;
          begin
          if s[1] = '.' then
            Delete(s, 1, 1); {DelFC(s);}
          Val(s, v, c);
          if c > 0 then
            Exit;
          if v <= Num then
            Exit;
          Num := v;
          end;
        var
          Dir: lSearchRec;
          D: String;
          N: String;
          E: String;
        begin { CalcUnknownName: }
        u := 'Unknown.';
        Num := 0;
        lFindFirst(u+'*', Archive, Dir);
        while DosError = 0 do
          begin
          lFSplit(Dir.FullName, D, N, E);
          CalcMaxUnk(E);
          lFindNext(Dir);
          end;
        lFindClose(Dir);
        CalcUnknownName := u+SSStr(Num+1, 3, '0');
        end { CalcUnknownName: };

      begin { DecodeUnheadered }
      if FileNameKnown then
        begin
        OutFName := CurFileName;
        CheckExist
        end
      else
        OutFName := CalcUnknownName;
      OutFName := OutputDir+OutFName;
      OpenOutFile;
      while SectionGo do
        begin
        OutString;
        Get_String;
        if EndOfFile then
          Break;
        end;
      end { DecodeUnheadered };

    procedure MakeTmpFile;

      procedure MaxSectNumMism;
        begin
        LocalError(GetString(dlleMaxSectNumMism));
        BadSection := True;
        end;

      var
        j: LongInt;
      procedure SetTotSect(var s: TSectInfo);
        begin
        with s do
          begin
          if MaxSectKnown then
            begin
            if TotSect <> MaxSectNum then
              begin
              MaxSectNumMism;
              Exit
              end;
            end
          else if MaxSectKnw then
            begin
            if CurSectNo > TotSect then
              begin
              MaxSectNumMism;
              Exit
              end;
            end
          else
            begin
            TotSect := Max(TotSect, CurSectNo);
            end;
          end;
        end { SetTotSect };

      procedure CreateNewSItem(var s: TSection);
        begin
        with s do
          begin
          Number := CurSectNo;
          TmpFileId := CalcTmpId;
          OutFName := CalcTmpFName(TmpFileId, TmpExt, True);
          end;
        CurPSection := @s;
        end;

      procedure CreateNewFItem(var f: TFile);
        begin
        with f do
          begin
          with S do
            begin
            MaxSectKnw := MaxSectKnown;
            if MaxSectKnown then
              TotSect := MaxSectNum
            else
              TotSect := 0;
            end;
          SetTotSect(S);
          FName := NewStr(CurFileName);
          Size := 0;
          CRC := 0;
          NSect := 0;
          Broken := False;
          CrcKnown := False;
          EndFound := False;
          ForceSkp := ForceSkip;
          Flushed := ForceSkip;
          if FileTimeKnown then
            FileTime := CurFileTime
          else
            FileTime := 0;
          CurPFile := @f;
          if not ForceSkip then
            begin
            GrArr(Sect, NSect, SizeOf(TSection));
            CreateNewSItem(Sect^[NSect]);
            end;
          end;
        end { CreateNewFItem };

      procedure DuplicateSection;
        begin
        LocalWarning
          (GetString(dlleDuplicateSection)+' '+ItoS(CurSectNo)+' '+
          GetString(dlleOfFile)+' '+CurFileName);
        BadSection := True;
        end;

      procedure AddNewSItem(var f: TFile);
        var
          i: LongInt;
        begin
        with f do
          begin
          if ForceSkp then
            Exit;
          if Flushed then
            begin
            DuplicateSection;
            Exit;
            end;
          SetTotSect(S);
          if BadSection then
            Exit;
          if FileTimeKnown then
            FileTime := CurFileTime;
          for i := 1 to NSect do
            if Sect^[i].Number = CurSectNo then
              begin
              DuplicateSection;
              Exit
              end;
          GrArr(Sect, NSect, SizeOf(TSection));
          CreateNewSItem(Sect^[NSect]);
          CurPFile := @f;
          end
        end { AddNewSItem };

      var
        i: LongInt;
      begin { MakeTmpFile }
      if ForceEOF then
        Exit;
      for i := 1 to NFileArr do
        begin
        if MatchCurFileName(FileArr^[i].FName^) then
          begin
          AddNewSItem(FileArr^[i]);
          Exit;
          end;
        end;
      GrArr(FileArr, NFileArr, SizeOf(TFile));
      CreateNewFItem(FileArr^[NFileArr]);
      end { MakeTmpFile };

    procedure WriteOrphanedSection;
      begin
      CheckExist;
      MakeTmpFile;
      OpenOutFile;
      exSUM := True;
      while SectionGo do
        begin
        OutString;
        Get_String;
        if EndOfFile then
          Break;
        end;
      end;

    procedure WriteHeadered;
      begin
      CheckExist;
      MakeTmpFile;
      OpenOutFile;
      GoSect;
      exSUM := True;
      while SectionGo do
        begin
        OutString;
        Get_String;
        if EndOfFile then
          Break;
        end;
      end;

    procedure FlushLeftSections;
      var
        i: LongInt;
      begin
      for i := 1 to NFileArr do
        begin
        CurPFile := @FileArr^[i];
        if not CurPFile^.Flushed then
          CompileFile;
        if not RecoverBrokenUUE then
          begin
          if CurPFile^.Broken then
            EraseFile(OutputDir+OutFName);
          end;
        end;
      for i := 1 to NFileArr do
        DisposeStr(FileArr^[i].FName);
      FreeArr(FileArr, NFileArr, SizeOf(TFile));
      end;

    procedure DoUuDecode(AFileRec: Pointer);
      var
        PF: PFileRec absolute AFileRec;
        InputFileName: String;
        IdleTimer: TEventTimer;
      begin
      if  (PF^.Attr and Directory <> 0) then
        Exit;
      InputFileName := MakeNormName(PF^.Owner^, PF^.FlName[uLfn]);
      if not ExistFile(InputFileName) then
        Exit;
      InputStream := New(PTextReader, Init(InputFileName));
      if  (InputStream = nil) then
        Exit;
      LongWorkBegin;
      Get_String;
      if not EndOfFile then
        begin
        R.Assign(0, 0, 40, 12);
        New(PI, Init(R));
        PI^.Top := GetString(dlUUDecode);
        PI^.Write(1, GetString(dlUUDecodingTo)+Cut(InputFileName, 40));
        PI^.Write(2, GetString(dlFC_To)+' '+OutputDir);
        Desktop^.Insert(PI);
        end;
      NewTimer(IdleTimer, 0);
      while not EndOfFile do
        begin
        UUfound := True;
        PI^.Write(3, CurFileName);
        PI^.Write(5, GetString(dlUUDecodeFiles)+SSStr(GoodNumber, 3, ' '));
        PI^.Write(6, GetString(dlUUDecodeErrors)+SSStr(ErrorNumber, 3,
             ' '));
        BadSection := False;
        KickBack := False;
        ForceSkip := False;
        exSUM := False;
        CurPFile := nil;

        if not NumSectKnown then
          DecodeUnheadered
        else if MaxSectKnown then
          WriteHeadered
        else
          WriteOrphanedSection;
        if TimerExpired(IdleTimer) then
          begin
          if ApplicationIdle(PI) then
            Break;
          NewTimer(IdleTimer, 50);
          end;
        end;
      if SectionGo then
        TerminateSection;
      if PI <> nil then
        Dispose(PI, Done);
      PI := nil;
      Dispose(InputStream, Done);
      InputStream := nil;
      Message(Application, evCommand, cmCopyUnselect, PF);
      LongWorkEnd;
      end { DoUuDecode };

    {var
    PP,P1 : PView;
    PL : PSortedListBox;}

    begin { Uu_Decode }
    POutBuf := nil;
    NFileArr := 0;
    BadSection := False;
    KickBack := False;
    ForceEOF := False;
    UUfound := False;
    CurFileName := '';
    GoodNumber := 0;
    ErrorNumber := 0;
    CurPFile := nil;
    PI := nil;
    ClearEX;
    ClearCRC;
    AFileCollection^.ForEach(@DoUuDecode);
    if NFileArr > 0 then
      FlushLeftSections;
    if not UUfound then
      begin
      ErrMsg(eruuNoStuff);
      NewTimer(Timer, 0);
      end;
    UU_Decode := ForceEOF;
    end { DecodeStr };
  {-DataCompBoy-}

  procedure UUDecode(AFileCollection: PCollection);
    var
      Dr: record
        S: String;
        OPT: Word;
        end;
      BeepDisabled: Boolean;
    begin
    Inc(SkyEnabled);
    Dr.S := '';
    Dr.OPT := UUDecodeOptions;
    Message(Application, evCommand, cmPushFirstName, @DR.S);
    GlobalMessageL(evCommand, cmPushName, hsUUDecode);
    if {(DT.O and uudExtractAll <> 0) or}
        (ExecResource(dlgUUDecode, Dr) = cmOK)
    then
      begin
      if UUDecodeOptions <> Dr.OPT then
        ConfigModified := True;
      UUDecodeOptions := Dr.OPT;
      CreateDirInheritance(Dr.S, False);
      NewTimer(Timer, 0);
      BeepDisabled := UU_Decode(AFileCollection, Dr.S, Dr.OPT and 1 <> 0,
           Dr.OPT and 2 <> 0, Dr.OPT and 4 <> 0);
      RereadGlobal(Dr.S);
      if not BeepDisabled then
        begin
        if  (FMSetup.Options and fmoBeep <> 0) and
            (ElapsedTime(Timer) > 5*1000)
        then
          BeepAftercopy;
        end;
      end;
    Dec(SkyEnabled);
    end { UUDecode };

  {$ENDIF}

  {$IFDEF UUENCODE}

  (* {Это все не нужно, итак как используется не ассемблерный obj,
а паскальный unit}

type
        T64             = record
                          case Byte of
                           0: (l0,l1       : LongInt);
                           1: (w0,w1,w2,w3 : word);
                          end;

procedure Prepare1Str(var Sou,Dst);near;external;
function  GetUUxlt(b:byte):char;near;external;
function  GetLnCrc(var Buf;Size:word):Char;near;external;
procedure cCrc(var Buf;Size:word;var PrevSum:word);near;external;
procedure Crc64(var Buf;Size:word;var PrevSum;var Cnt:word);near;external;
procedure Clear64(n:T64);near;external;

{$IFDEF DPMI}
{$L UUE2INC.OBP}
{$ELSE}
{$L UUE2INC.OBJ}
{$ENDIF}
*)

  {Cat: не всегда она будет возвращать Word, что приводит к глюкам
      (кстати: ещё один потенциальный источник глюков -  в модуле две функции
      называются SmartDiv, но вместе с тем этот SmartDiv абсолютно отличается
      от того, который был раньше)}
  function SmartDiv(L: LongInt; W: LongInt): LongInt;
    assembler;
    {&USES None} {&FRAME-}
  asm
         mov     eax,L
         xor     edx,edx
         div     W
         or      edx,edx
         jz      @@Exit
         inc     eax
@@Exit:
end;

  {-DataCompBoy-}
  procedure UUEncode(const FName: String);
    type
      PDskBufArr = ^TDskBufArr;
      TDskBufArr = array[1..65535] of Byte;

    const
      SectNZ = 2000;
      {$IFDEF DNPRG}
      UUETitle = '< UUE by DN/2 '+VersionName+' >';
      {$ELSE}
      UUETitle = '< UUE by DN/2 >';
      {$ENDIF}
    var
      ST: lFile;
      GetLnEnd: String[2];
      NLines: LongInt;
      L, FL, CRC, RSize: LongInt;
      S: String;
      I, Start: Integer;
      Nm: String;
      Xt: String;
      Dr: String;
      P: Pointer;
      PI: PWhileView;
      R: TRect;
      Cancel, All, Skip: Boolean; { Flash 30-04-2003 }

      Sec64Pn,
      SecCRC,
      EntireP64,
      EntireCRC

      : Word;

      SouTime: LongInt;
      SouSize: Longint;
      WriteError: Boolean;

      MaxSectSize,
      SectSize,
      LastSectSize,
      NumSect: LongInt;
      EntireC64: T64;

      TxtBufSize,
      SecCRCs,
      StrPerLastS,
      SectNo,
      StrPerSect: LongInt;
      t: lFile;
      DskBuf, SouBuf: PDskBufArr;
      OutBufSize,
      MsgNameNum: LongInt;
      Sec64Crc: T64;
      CalcEntireCrc,
      CalcSectCrc,
      CalcLineCrc,
      Calc64Crc: Boolean;
      OutName: String;
      SouName: String;

    procedure GetData(var Buf; Count: LongInt);
      begin
      BlockRead(ST.F, Buf, Count);
      if CalcEntireCrc then
        cCRC(Buf, Count, EntireCRC);
      if Calc64Crc then
        CRC64(Buf, Count, EntireC64, EntireP64);
      end;

    procedure WriteT(const s: String);
      var
        sl: Byte absolute s;
      begin
      {Cat: шаманство?  ;-)
 if TxtBufSize + sl > OutBufSize then
 asm
   nop
 end;
}
      Move(s[1], DskBuf^[TxtBufSize+1], sl);
      Inc(TxtBufSize, sl);
      end;

    procedure WriteLnT(const s: String);
      begin
      WriteT(s+GetLnEnd)
      end;

    procedure WriteLnCRC(s: String);
      var
        sl: Byte;
      begin
      if CalcSectCrc then
        begin
        sl := Length(s);
        Inc(sl);
        Inc(SecCRCs, sl);
        s[sl] := #10;
        cCRC(s[1], sl, SecCRC);
        end;
      if Calc64Crc then
        CRC64(s[1], sl, Sec64Crc, Sec64Pn);
      WriteLnT(s);
      end;

    procedure FlushT;
      begin
      lReWriteFile(T, 1);
      if IOResult > 0 then
        begin
        WriteERROR := True;
        Exit
        end;
      BlockWrite(T.F, DskBuf^, TxtBufSize);
      if IOResult > 0 then
        WriteERROR := True;
      Close(T.F);
      if IOResult > 0 then
        WriteERROR := True;
      end;

    procedure WriteEmptyLn;
      begin
      WriteLnT('')
      end;

    {Cat: эта функция есть в advance1.pas}
    (*
function ItoS(a:longint):string;
 var s : string[40];
begin
 Str(a,s);
 ItoS:=s;
end;
*)

    procedure ClrIO;
      begin
      InOutRes := 0;
      DosError := 0;
      Abort := False;
      end;

    procedure InsertStatistics;

      procedure WriteInfo(s1: String; const S2: String);
        const
          sll = 31;
        begin
        while Length(s1) < sll do
          s1 := ' '+s1;
        WriteLnT(s1+' : '+S2);
        end;

      function SStrPerSect: String;
        var
          s: String;
        begin
        if NumSect = 0 then
          s := ItoS(StrPerLastS)
        else
          begin
          if StrPerSect = StrPerLastS then
            s := ItoS(StrPerSect)
          else
            s := ItoS(StrPerSect)+' ('+ItoS(StrPerLastS)+')';
          end;
        SStrPerSect := s;
        end;

      function KB(L: LongInt): String;
        begin
        KB := ItoS(L)+' ('+ItoS(SmartDiv(L, 1024))+'Kb)';
        end;

      function Kb2(L: LongInt): String;
        begin
        Kb2 := ItoS(SmartDiv(L, 1024))+'Kb';
        end;

      function EncSize: String;
        begin
        EncSize := Kb2(longmul((OutBufSize-SectNZ), NumSect+1));
        end;

      function GetDecimal(Number: Word): String;
        assembler; {Set 2-b}
        {&Frame-} {$USES EDI}
      asm
        cld
        mov     edi,@Result
        mov     al,2
        stosb
//        mov     eax,Number
        mov     ax,Number
        mov     cl,10
        div     cl
        add     ax,3030h
        stosw
 end;

      function GetMonth(Month: Word): String;
        begin
        GetMonth := GetString(TStrIdx(Integer(dlJanuary)+Month-1))
        end;

      function StdDateTime(Year, Month, Day, Hour, Minute, Second: Word)
        : String;
        var
          s: String[6];
          sl: Byte absolute s;
        begin
        s := ItoS(Year);
        while sl > 2 do
          Delete(s, 1, 1); {DelFC(s);}
        StdDateTime := GetDecimal(Day)+'-'+GetMonth(Month)+'-'+s+' '+
          GetDecimal(Hour)+':'+GetDecimal(Minute)+':'+GetDecimal(Second);
        end;

      function CurStdDateTime: String;
        var
          Dummy, Year, Month, Day, Hour, Minute, Second: LongInt;
        begin
        GetDate(Year, Month, Day, Dummy);
        GetTime(Hour, Minute, Second, Dummy);
        CurStdDateTime := StdDateTime(Year, Month, Day, Hour, Minute,
             Second);
        end;

      function CrTime: String;
        begin
        CrTime := CurStdDateTime;
        end;

      function FTime: String;
        var
          DT: DateTime;
        begin
        UnpackTime(SouTime, DT);
        with DT do
          FTime := StdDateTime(Year, Month, Day, Hour, Min, Sec);
        end;

      begin { InsertStatistics }
      WriteInfo(GetString(dlUUEncodeSFN), UpStrg(SouName));
      WriteInfo(GetString(dlUUEncodeOS), KB(SouSize));
      WriteInfo(GetString(dlUUEncodeCreated), FTime);
      WriteInfo(GetString(dlUUEncodeTime), CrTime);
      WriteInfo(GetString(dlUUEncodeSize), EncSize);
      WriteInfo(GetString(dlUUEncodeSections), ItoS(NumSect+1));
      WriteInfo(GetString(dlUUEncodeLines), SStrPerSect);
      WriteEmptyLn;
      WriteEmptyLn;
      end { InsertStatistics };

    var
      UUEData: TUUEncodeData;

    procedure StartSection;
      var
        LocalFreeStr, o: String;
        P: Pointer;
        II: Integer;
      begin
      Inc(SectNo);
      Skip := False; { Flash 30-04-2003 }
      if NumSect = 0 then
        LocalFreeStr := XT
      else if SectNo < 10 then
        LocalFreeStr := Copy(XT, 1, 3)+ItoS(SectNo)
      else if SectNo < 100 then
        LocalFreeStr := Copy(XT, 1, 2)+ItoS(SectNo)
      else
        LocalFreeStr := '.'+ItoS(SectNo);

      OutName := dr+Nm+LocalFreeStr;
      lAssignFile(T, OutName);
      {---------------------------------------------------------------}
      if not All and (NumSect > 0) then
        { Flash 30-04-2003 }
        begin
        { FileMode:=$40;}
        ClrIO;
        lResetFile(T, 1);
        if IOResult = 0 then
          begin
          Close(T.F);
          P := @OutName;
          II := MessageBox(GetString(dlFileExist)+^M, @P,
               mfQuery+mfYesButton+mfNoButton+mfCancelButton+mfAllButton);
          case II of
            cmOK:
              All := True;
            { Flash 29-04-2003 >>> }
            cmCancel:
              begin
              Cancel := True;
              Exit;
              end;
            cmNo:
              begin
              Skip := True;
              Exit;
              end;
            { Flash 29-04-2003 <<< }
          end {case};
          NewTimer(Timer, 0);
          end;
        end;
      {---------------------------------------------------------------}
      TxtBufSize := 0;
      if CalcSectCrc then
        begin
        SecCRC := 0;
        SecCRCs := 0
        end;
      if Calc64Crc then
        begin
        Clear64(Sec64Crc);
        Sec64Pn := 0
        end;

      PI^.Write(3, Cut(OutName, 40));

      if NumSect > 0 then
        o := ' of '+ItoS(NumSect+1)
      else
        o := '';
      o := 'section '+ItoS(SectNo)+o+' of file '+SouName+'  '+UUETitle;
      if SectNo = 1 then
        begin
        if  (UUEData.Prefix and ckStatistic) > 0 then
          InsertStatistics;
        WriteLnT(o+GetLnEnd);
        if  (UUEData.Prefix and ckFileTime) > 0 then
          WriteLnT('filetime '+ItoS(SouTime));
        if  (UUEData.Prefix and ckMapTable) > 0 then
          begin
          WriteLnCRC('table');
          WriteLnCRC('`!"#$%&''()*+,-./0123456789:;<=>?');
          WriteLnCRC('@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_');
          end;
        WriteLnCRC('begin 644 '+SouName);
        end
      else
        WriteLnT(o+GetLnEnd);
      end { StartSection };

    function SS64(n: T64): String;
      var
        s: String;
      begin
      SetLength(s, 16);
      Hex8Lo(n.l1, s[1]);
      Hex8Lo(n.l0, s[9]);
      SS64 := s;
      end;

    procedure EndSection;
      var
        f, t: String;

      begin
      if SectNo = NumSect+1 then
        begin
        if CalcLineCrc then
          WriteLnCRC('``')
        else
          WriteLnCRC('`');
        WriteLnCRC('end');
        end
      else
        begin
        WriteLnT('');
        end;
      if CalcSectCrc then
        begin
        if NumSect = 0 then
          begin
          f := '"begin"';
          t := '"end"'
          end
        else if SectNo = 1 then
          begin
          f := '"begin"';
          t := 'last encoded line'
          end
        else if SectNo = NumSect+1 then
          begin
          f := 'first encoded line';
          t := '"end"'
          end
        else
          begin
          f := 'first';
          t := 'last encoded line'
          end;
        f := ' section (from '+f+' to '+t+')';

        WriteLnT('sum -r/size '+ItoS(SecCRC)+'/'+ItoS(SecCRCs)+f);
        end;
      t := ' entire input file';
      if CalcEntireCrc then
        begin
        if SectNo = NumSect+1 then
          WriteLnT('sum -r/size '+ItoS(EntireCRC)+
            '/'+ItoS(SouSize)+t);
        end;

      if Calc64Crc then
        begin
        WriteEmptyLn;
        WriteLnT('crc64 '+SS64(Sec64Crc)+f);
        if SectNo = NumSect+1 then
          WriteLnT('crc64 '+SS64(EntireC64)+t);
        end;

      WriteEmptyLn;

      FlushT;
      end { EndSection };

    procedure WriteStr(var Buf; NumBytes: LongInt);
      var
        d: String[60];
        dl: Byte absolute d;
        s: array[1..45] of Byte;
      begin
      Move(Buf, s, NumBytes);
      if NumBytes < 45 then
        FillChar(s[NumBytes+1], 45-NumBytes, 0);
      Prepare1Str(s, d[1]);
      dl := SmartDiv((NumBytes), 3)*4;
      if CalcLineCrc then
        WriteLnCRC(GetUUxlt(NumBytes)+d+GetLnCrc(d[1], dl))
      else
        WriteLnCRC(GetUUxlt(NumBytes)+d);
      end;

    procedure WriteSection(var Buf; ss: LongInt);
      var
        i: LongInt;
        Buff: TDskBufArr absolute Buf;
      begin
      for i := 0 to ss-1 do
        WriteStr(Buff[(i)*45+1], 45);
      end;

    procedure WriteCompleteSection(var Buf);
      begin
      WriteSection(Buf, StrPerSect)
      end;

    procedure Write_Section;
      begin
      StartSection;
      if Cancel or Skip then
        Exit; {!! IB}
      { Flash 30-04-2003 }
      GetData(SouBuf^, SectSize);
      WriteCompleteSection(SouBuf^);
      EndSection;
      end;

    procedure WriteLastSection;
      var
        i: LongInt;
      begin
      StartSection;
      if Cancel or Skip then
        Exit; {!! IB}
      GetData(SouBuf^, LastSectSize);
      i := StrPerLastS-1;
      if i > 0 then
        WriteSection(SouBuf^, i);
      WriteStr(SouBuf^[(i)*45+1], LastSectSize-(i)*45);
      EndSection;
      end;

    procedure EncodeSections;
      var
        i: LongInt;
        IdleTimer: TEventTimer;
      begin
      Cancel := False;
      All := False;
      NewTimer(IdleTimer, 0);
      if CalcEntireCrc then
        EntireCRC := 0;
      if Calc64Crc then
        begin
        Clear64(EntireC64);
        EntireP64 := 0
        end;
      for i := 1 to NumSect do
        begin
        Write_Section;
        if WriteERROR or Cancel then
          Exit;
        if TimerExpired(IdleTimer) then
          begin
          if ApplicationIdle(PI) then
            Exit;
          NewTimer(IdleTimer, 50);
          end;
        end;
      WriteLastSection;
      end { EncodeSections };

    procedure CalcLSsize;
      var
        ls: LongInt;
      begin
      ls := SouSize-longmul(SectSize, NumSect);
      while ls < SectSize div 2 do
        begin
        Inc(ls, SectSize);
        Dec(NumSect);
        end;
      while ls > SectSize do
        begin
        Dec(ls, SectSize);
        Inc(NumSect);
        end;
      LastSectSize := ls;
      end;

    {Cat: незачем два раза открывать файл}
    (*
procedure GetFInfo;
var f:lfile;
begin
 lAssignFile(f,FName);
 FileMode:=$40;
 ClrIO;
 lResetFile(f,1);
 if IOResult<>0 then exit;
 GetFTime(f.f,SouTime);
 Close(f.f);
end;
*)

    function MakeNormName(S, S1: String): String;
      begin
      while S[Length(S)] = ' ' do
        SetLength(S1, Length(S1)-1);
      while S1[Length(S1)] = ' ' do
        SetLength(S1, Length(S1)-1);
      if S <> '' then
        begin
        if S[Length(S)] = '\' then
          MakeNormName := S+S1
        else
          MakeNormName := S+'\'+S1;
        end
      else
        MakeNormName := S1;
      end;

    procedure DoIt;
      var
        sss, Ma: LongInt;
        II: Integer;
        LocalFreeStr, UUEncodeDataName: String;
        Err: LongInt;
      label beg;
      begin

      WriteERROR := False;
      UUEData := UUEncodeData;
      UUEData.Name := '';
      lFSplit(FName, dr, Nm, XT);

      SouName := Nm+XT;

      Message(Application, evCommand, cmPushFirstName, @UUEData.Name);

      UUEData.Name := MakeNormName(UUEData.Name, Nm+'.uue');

beg:

      HistoryAdd(hsUUEncode, GetName(UUEData.Name));
      if ExecResource(dlgUUEncode, UUEData) <> cmOK then
        Exit;
      if  (UUEData.CheckSum <> UUEncodeData.CheckSum) or
          (UUEData.Prefix <> UUEncodeData.Prefix) or
          (UUEData.NLines <> UUEncodeData.NLines) or
          (UUEData.Format <> UUEncodeData.Format)
      then
        begin
        UUEncodeData := UUEData;
        UUEncodeData.Name := '';
        ConfigModified := True;
        end;

      CalcEntireCrc := UUEData.CheckSum >= ckEntire;
      CalcSectCrc := UUEData.CheckSum >= ckStd;
      CalcLineCrc := UUEData.CheckSum >= ckEach;
      Calc64Crc := UUEData.CheckSum >= ck64;

      GetLnEnd := #13#10;
      if UUEData.Format = 1 then
        GetLnEnd := #10;

      {Cat:todo ругаться при неправильном количестве строк вместо того,
          чтобы молча брать какое-то другое}
      NLines := 900;

      Val(UUEData.NLines, l, Err);
      if Err = 0 then
        NLines := Max(10, l);

      lFSplit(lFExpand(UUEData.Name), S, Nm, XT);
      if  (XT = '') or (XT = '.') then
        XT := '.uue';
      if Nm = '' then
        begin
        lFSplit(FName, dr, Nm, XT);
        dr := MakeNormName(S, '');
        XT := '.uue';
        end
      else
        dr := S;

      CreateDirInheritance(dr, False);

      lAssignFile(ST, FName);
      FileMode := $40;
      ClrIO;
      lResetFile(ST, 1);
      if IOResult <> 0 then
        begin
        MessageBox(GetString(dlArcMsg4)+Cut(FName, 40), nil,
           mfError+mfOKButton);
        NewTimer(Timer, 0);
        Exit
        end;

      SouSize := i32(FileSize(ST.F)); {!!s}

      if SouSize < 3 then
        begin
        Close(ST.F);
        Msg(dlFileIsSmall, nil, mfError+mfOKButton);
        NewTimer(Timer, 0);
        Exit; {Input file is too small}
        end;

      {Cat: GetFInfo всего лишь получает время модификации файла}
      (*
 GetFInfo;
*)
      GetFtime(ST.F, SouTime);
      {/Cat}

      MaxSectSize := longmul(NLines, 45);
      NumSect := SmartDiv(SouSize, MaxSectSize)-1;
      SectNo := 0;

      {Cat: раньше в этом месте проверялось наличие файла, но оно проверяется ещё
      и в StartSection, поэтому запрос на перезапись появлялся дважды}
      (*
 LocalFreeStr:=Dr+Nm+Xt;
 lAssignFile(t,LocalFreeStr);
 FileMode:=$40;
 ClrIO;
 lResetFile(t,1);
 if IOResult=0 then
    begin
     Close(t.f);
     P := @LocalFreeStr;
     II := MessageBox(GetString(dlFileExist)+^M, @P, mfQuery+mfYesButton+mfNoButton+mfCancelButton+mfAllButton);
     Case II of
      cmOk    :   All := True;
      cmCancel:   begin Close(ST.f); Exit; end;
      cmNo    :   begin Close(ST.f); Goto beg end;
     end;
    NewTimer(Timer, 0);
    end;
*)

      if NumSect = 0 then
        begin
        LastSectSize := SouSize;
        SectSize := 0
        end
      else
        begin
        SectSize := longmul(SmartDiv(SmartDiv(SouSize, NumSect+1), 45),
             45);
        CalcLSsize;
        end;

      StrPerSect := SmartDiv(SectSize, 45);
      StrPerLastS := SmartDiv(LastSectSize, 45);
      OutBufSize := longmul(Max(StrPerSect, StrPerLastS), 70)+SectNZ;
      sss := Max(SectSize, LastSectSize);
      SectNo := 0;
      Ma := MaxAvail-OutBufSize-sss;
      {Cat: убрал ограничение на размер буфера - в VP можно и побольше памяти
      выделить - и, соответственно, ограничение в 900 строк в секции}
      (*
 if (Ma<20000) or (OutBufSize>$FFEF) then
 begin
  Close(ST.f);
  if NLines > 900 then
    begin
      ErrMsg(dlMaxFiles);
      NewTimer(Timer, 0);
      Goto beg;
    end;
  ErrMsg(erNotEnoughMemory);
  NewTimer(Timer, 0);
  WriteError:=True;
  Exit;
 end;
*)
      {Cat}
      if Ma < 20000 then
        begin
        Close(ST.F);
        ErrMsg(erNotEnoughMemory);
        NewTimer(Timer, 0);
        WriteERROR := True;
        Exit;
        end;
      {/Cat}

      GetMem(DskBuf, OutBufSize);
      GetMem(SouBuf, sss);

      R.Assign(0, 0, 40, 9);
      New(PI, Init(R));
      PI^.Top := GetString(dlUUEncode);
      PI^.Write(1, GetString(dlUUEncoding)+Cut(FName, 40));
      PI^.Write(2, GetString(dlFC_To));
      Desktop^.Insert(PI);
      NewTimer(Timer, 0);

      EncodeSections;
      FreeMem(SouBuf, sss);
      FreeMem(DskBuf, OutBufSize);
      Close(ST.F);
      PI^.Free;
      if WriteERROR then
        begin
        P := @OutName;
        Msg(dlCanNotWrite, P, mfError+mfOKButton);
        NewTimer(Timer, 0);
        end;
      RereadGlobal(dr);
      if  (FMSetup.Options and fmoBeep <> 0) and
          (ElapsedTime(Timer) > 20*1000)
      then
        BeepAftercopy;
      end { DoIt };

    begin { UUEncode }
    LongWorkBegin;
    Inc(SkyEnabled);
    DoIt;
    Dec(SkyEnabled);
    LongWorkEnd;
    end { UUEncode };
  {-DataCompBoy-}

  {$ENDIF}
end.


