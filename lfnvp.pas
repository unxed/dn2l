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
{DataCompBoy = Anton Fedorov, 2:5000/111.33@fidonet}
{JO = Jaroslaw Osadtchiy, 2:5030/1082.53@fidonet}
{AK155 = Alexey Korop, 2:461/155@fidonet}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}
{Interface part from LFN.PAS}
{$AlignRec-}

{Cat
   05/12/2001 - попытка бороться с виндозной глюкофичей: запоминается текущий
   каталог не для всех дисков, а только для текущего диска, что приводит к
   различным мелким неприятностям; чтобы это пофиксить, при lChDir сохраняем
   устанавливаемый путь в массиве, а при lGetDir - извлекаем оттуда
}

unit LFNVP;

interface

uses
  VPSysLow, // см. комментарий в конце vpsysos2
  VPSysLo2, Dos, Defines
  ;

type
{$IFDEF DPMI32}
  lAPIType = (lDOS, lWIN95);
  CompRec = record
             Lo, Hi: LongInt
            end;
{$ENDIF}

  {Extended search structure to be used instead of SearchRec}
  lSearchRec = record
    SR: TOSSearchRecNew; {Basic field set}
    FullSize: TSize; {True file size}
    (*  LoCreationTime: Longint; {Time created (low-order byte)}
    HiCreationTime: Longint; {Time created (high-order byte)}
    LoLastAccessTime: Longint; {Time accessed (low-order byte)}
    HiLastAccessTime: Longint; {Time accessed (high-order byte)}
    LoLastModificationTime: Longint; {Time modified (low-order byte)}
    HiLastModificationTime: Longint; {Time modified (high-order byte)} *)
    FullName: String;
    {True file name or short name if LFNs not available}
    {$IFDEF OS2}
    PrevName: String;
    {$ENDIF}
    {$IFDEF DPMI32}
    FileHandle: Word; {Search handle, undefined in lDOS mode}
    FindFirstMode : lAPIType;
    {$ENDIF}
    { Other fields will be added later }
    end;

  TNameZ = array[0..259] of Char;
  {The type for file names. (String is suitable for most purpuses)}

  lFile = record
    {Extended file record to be used instead of File}
    F: file;
    {$IFDEF DPMI32}
    FullName       : TNameZ;
    AssignFileMode : lAPIType;
    {$ENDIF}
    { Other fields will be added later }
    end;

  lText = record
    {Extended text file record to be used instead of Text}
    T: Text;
    {$IFDEF DPMI32}
    FullName       : TNameZ;
    AssignTextMode : lAPIType;
    {$ENDIF}
    { Other fields will be added later }
    end;

{$IFDEF DPMI32}
var
  lAPI       : lApiType = lWin95;
{$ENDIF}

  {   Basic parameters   }
const

  {$IFDEF DPMI32}
  ltMod = 0;    {Store time modified}
  ltAcc = 1;    {Store time accessed}
  ltCre = 2;    {Store time created}

  LFNTimes: Byte = ltMod; { What time info to store in lSearchRec.SR.Time? }

  faOpen = 1;
  faTruncate = 2;
  faCreate = $10;
  faRewrite = faTruncate + faCreate;

  faGetAttr = 0;
  faSetAttr = 1;
  {$ENDIF}
  MaxPathLen: Byte = 255; { Maximum name length for the present moment }

const
  IllegalChars = '<>|:';
  { Characters invalid for short names }
  IllegalCharSet = ['<', '>', '|', ':'];
  { Characters invalid for short names }

  { File searching routines. lFindClose must be called /in all cases/ }
procedure lFindFirst(const Path: String; Attr: Word; var R: lSearchRec);
procedure lFindNext(var R: lSearchRec);
procedure lFindClose(var R: lSearchRec);

{$IFDEF DualName}
function lfGetShortFileName(const Name: String): String;
{$ENDIF}
{$IFDEF DPMI32}
function SysFileCreate(FileName: PChar; Mode,Attr: Longint; var Handle: Longint): Longint;
function SysFileOpen(FileName: PChar; Mode: Longint; var Handle: Longint): Longint;
function lfGetLongFileName(const Name: String): String;
procedure lWIN95FindFirst(const Path: String; Attr: Word; var R: lSearchRec);
{$ENDIF}

{ Name correction routine }
procedure lTrueName(const Name: String; var S: String);
procedure NameToNameZ(const Name: String; var NameZ: TNameZ);

function GetShareEnd(const S: String): Integer;
{` AK155 22-11-2003 Найти конец шары в пути. 0 - если это не UNC-путь `}

function GetRootStart(const Path: String): Integer;
{` Найти начало корня; например, для C:\DIR результат 3,
для \\Server\Share\Dir результат 15. Для путей вроде '' или C: результат
на 1 больше длины. `}

{ Basic file operation routines. To use IO functions from standard units,
              specify lFile.F or lText.T }

procedure lAssignFile(var F: lFile; const Name: String);
procedure lAssignText(var T: lText; const Name: String);
procedure lResetFile(var F: lFile; RecSize: Word);
procedure lResetFileReadOnly(var F: lFile; RecSize: Word);
procedure lReWriteFile(var F: lFile; RecSize: Word);
procedure lResetText(var F: lText);
  inline;
  begin
  Reset(F.T)
  end;
procedure lResetTextReadOnly(var F: lText);
procedure lRewriteText(var F: lText);
procedure lAppendText(var T: lText);
  inline;
  begin
  Append(T.T);
  end;
procedure lEraseFile(var F: lFile);
{$IFNDEF DPMI32}
  inline;
  begin
  Erase(F.F);
  end;
{$ENDIF}
procedure lEraseText(var T: lText);
{$IFNDEF DPMI32}
  inline;
  begin
  Erase(T.T);
  end;
{$ENDIF}
procedure lRenameFile(var F: lFile; const NewName: String);
procedure lRenameText(var T: lText; const NewName: String);
procedure lChangeFileName(const Name, NewName: String);
function lFileNameOf(var lF: lFile): String;
function lTextNameOf(var lT: lText): String;

{ File attributes manipulation }
procedure lGetFAttr(var F: lFile; var Attr: Word);
procedure lSetFAttr(var F: lFile; Attr: Word);
procedure lGetTAttr(var T: lText; var Attr: Word);
procedure lSetTAttr(var T: lText; Attr: Word);

{ Directory manipulation }
procedure lMkDir(const Path: String);
procedure lRmDir(const Path: String);
procedure lChDir(Path: String);
  {` Если указанный каталог существует, то перейти на него, то есть
  присвоить его ActiveDir. Код ошибки реально формируется в FindFirst,
  то есть находится в DosError. Для совместимости со стандартной
  ChDir он дублируется также в InOutRes. `}
procedure lGetDir(D: Byte; var Path: String);

{ Name expansion and splitting }
function lFExpand(Path: String): String;
  {` расширить Path относительно ActiveDir. Если в Path были
  '/', то в результате им будут соответствовать '\'.
  Всякие . и .. коректно удаляются, скажем, вместо C:\TEMP\$$$\..
  будет C:\TEMP. '\' на конце - только в корне диска.`}
procedure lFSplit(const Path: String; var Dir, Name, ext: String);

{$IFDEF DualName}
const
  NoShortName: String[12] = #22#22#22#22#22#22#22#22'.'#22#22#22;
  {JO, AK155: зачем нужны эти #22:
  В виндах функции API FindFileFirst и FindFileNext отдают два имени
файла: основное и альтернативное (короткое).
  Под НТ возможна ненормальная (с моей точки зрения,
по крайней мере) ситуация, когда для файла с длинным (не укладывающимся
в 8.3) имением файла альтернатвное имя недоступно. Так бывает, если
файловая система в принципе не поддерживает двухименности (HPFS), или
если формирование коротких имен отключено (на NTFS и FAT32).
  Тогда в режиме показа коротких имён мы будем иметь для таких файлов
обрезанные как попало имена в панели, не соответствующие
действительности. Для этого JO и придумал этот условный заменитель
недоступного короткого имени, так как символ #22 заведомо не
может быть в принципе в реальном имени файла. Так показывать в
режиме коротких имён файлы, для которых доступно только длинное имя, -
честнее, чем с обрезанным именем, под которым файл недоступен.
}
  {$ENDIF}

{Cat: Windows запоминает текущий каталог только для текущего диска;
запоминание остальных текущих каталогов приходится брать на себя}
var
  CurrentPaths: array[1..1+Byte('Z')-Byte('A')] of PathStr;
  ActiveDir: String; // всегда с '\' в конце
  CurrentRoot: String; // без '\' в конце; может быть шара
  StartDir: String;

implementation

uses
  {$IFDEF WIN32}Windows, {$ENDIF}
  Strings, Commands {Cat}
  , Advance1, Advance2, VPUtils
  {$IFDEF DPMI32} ,Startup ,Dpmi32 ,Dpmi32df {$ENDIF}
  , fnotify
  ;

function StrPas_(S: array of Char): String;
  var
    ss: String;
    i: Word;
  begin
  ss := '';
  for i := Low(S) to High(S) do
    if  (i < 255) and (S[i] <> #0)
    then
      ss := ss+S[i]
    else
      Break;
  StrPas_ := ss;
  end;

procedure NameToNameZ(const Name: String; var NameZ: TNameZ);
  begin
  Move(Name[1], NameZ, Length(Name));
  NameZ[Length(Name)] := #0;
  end;

(*
 Offset  Size    Description
  00h    DWORD   file attributes
                 bits 0-6 standard DOS attributes
                 bit 8: temporary file
  04h    QWORD   file creation time
                 (number of 100ns intervals since 1/1/1601)
  0Ch    QWORD   last access time
  14h    QWORD   last modification time
  1Ch    DWORD   file size (high 32 bits)
  20h    DWORD   file size (low 32 bits)
  24h  8 BYTEs   reserved
  2Ch 260 BYTEs  ASCIZ full filename
 130h 14 BYTEs   ASCIZ short filename (for backward compatibility)
*)

function SetDosError(ErrCode: Integer): Integer;
  begin
  DosError := ErrCode;
  SetDosError := ErrCode;
  end;

{AK155}
{$IFDEF DualName}
function NotShortName(const S: String): Boolean;
  var
    i, l: Integer;
    iPoint: Integer;
  begin
  NotShortName := True;
  if S[1] = '.' then
    Exit;
  l := Length(S);
  if l > 12 then
    Exit;
  iPoint := 0;
  for i := 1 to l do
    begin
    if S[i] = '.' then
      begin
      if  (iPoint <> 0) or (i > 9) then
        Exit;
      iPoint := i;
      end
    else if S[i] in IllegalCharSet then
      Exit; {DataCompBoy}
    end;
  if  (iPoint = 0) and (l > 8) then
    Exit;
  if  (iPoint <> 0) and (l-iPoint > 3) then
    Exit;
  NotShortName := False;
  end { NotShortName };
{$ENDIF}

procedure CorrectSearchRec(var R: lSearchRec);
  begin
  R.FullName := R.SR.Name;
  {$IFDEF Win32}
  if  (R.SR.Name <> '.') and (R.SR.Name <> '..') then
    begin
    if  (R.SR.ShortName <> '') then
      R.SR.Name := R.SR.ShortName
    else if NotShortName(R.FullName) then
      R.SR.Name := NoShortName;
    end;
  {$ENDIF}
  {$IFDEF DPMI32}
  {JO: CorrectSearchRec вызывается только в отсутствие Win32 LFN API}
  R.SR.CreationTime := 0;
  R.SR.LastAccessTime := 0;
  {/JO}
  {$ENDIF}
  (*R.LoCreationTime:= R.SR.Time;
  R.HiCreationTime:= 0;
  R.LoLastAccessTime:= R.SR.Time;
  R.HiLastAccessTime:= 0;
  R.LoLastModificationTime:= R.SR.Time;
  R.HiLastModificationTime:= 0; *)
  R.FullSize := R.SR.Size;
  end;

{$IFDEF DPMI32}{lfn functions for dpmi32}
type
  lFindDataRec = record
    LoAttr: SmallWord;
    HiAttr: SmallWord;
    LoCreationTime: Longint;
    HiCreationTime: Longint;
    LoLastAccessTime: Longint;
    HiLastAccessTime: Longint;
    LoLastModificationTime: Longint;
    HiLastModificationTime: Longint;
    HiSize: Longint;
    LoSize: Longint;
    Reserved: Array[0..7] of Byte;
    FullName: TNameZ;
    ShortName: Array[0..13] of Char;
  end;

Const
      DriveBuffer: array[1..4] of char = ('?',':','\',#0);

procedure CheckColonAndSlash(const Name: String; var S: String);
var
  ColonPos: Integer;
begin
  ColonPos := Pos(':', S);
  if (ColonPos > 2) and (Name[2] = ':') then
  begin
    Delete(S, 1, ColonPos - 1);
    S := Name[1] + S;
  end;

  if Name[Length(Name)] <> '/' then // slash change by unxed
    while S[Length(S)] = '/' do Dec(S[0]) // slash change by unxed
  else if (Name[Length(Name)] = '/') and // slash change by unxed
    (S[Length(S)] <> '/') and (Length(S) < 255) then // slash change by unxed
  begin
    Inc(S[0]);
    S[Length(S)] := '/'; // slash change by unxed
  end;
end;

procedure FindDataToSearchRec(var FindData: lFindDataRec; var R: lSearchRec);
begin
  R.SR.Attr := FindData.LoAttr;
{ if LFNTimes = ltCre then R.SR.Time := FindData.LoCreationTime
   else if LFNTimes = ltAcc then R.SR.Time := FindData.LoLastAccessTime
    else} R.SR.Time := FindData.LoLastModificationTime;
{JO}
  R.SR.CreationTime := FindData.LoCreationTime;
  R.SR.LastAccessTime := FindData.LoLastAccessTime;
{/JO}
  R.SR.Name := StrPas(FindData.ShortName);
  R.FullName := StrPas(FindData.FullName);
  if R.SR.Name = '' then R.SR.Name := R.FullName;
  if R.FullName = '' then R.FullName := R.SR.Name;
  R.FullSize:= FindData.LoSize;
  if FindData.HiSize=0 then R.SR.Size := FindData.LoSize
  else
   begin
    R.SR.Size := MaxLongInt;
    CompRec(R.FullSize).Hi:=FindData.HiSize;
   end;
end;

(*
 INT 21h  AX=714E
 INT 21 - Windows95 - LONG FILENAME - FIND FIRST MATCHING FILE
         AX = 714Eh
         CL = allowable-attributes mask (bits 0 and 5 ignored)
         CH = required-attributes mask
         SI = date/time format
         DS:DX -> ASCIZ filespec (both "*" and "*.*" match any filename)
         ES:DI -> FindData record
 Return: CF clear if successful
             AX = filefind handle (needed to continue search)
             CX = Unicode conversion flags
         CF set on error
             AX = error code
                 7100h if function not supported
 Notes:  this function is only available when IFSMgr is running,
         not under bare MS-DOS 7
         the application should close the filefind handle
         with AX=71A1h as soon as it has completed its search
*)

procedure lWIN95FindFirst(const Path: String; Attr: Word; var R: lSearchRec);
var
  FindData: ^lFindDataRec;
  regs: real_mode_call_structure_typ;
begin
  FindData:=Ptr(segdossyslow32);
  NameToNameZ(Path, FindData^.FullName);
  init_register(regs);
  with regs do
   begin
    ax_:= $714E;
    cx_:= Attr;
    si_:= 1;
    ds_:= segdossyslow16;
    dx_:= $2c;
    es_:= segdossyslow16;
    di_:= 0;
    flags_:=fCarry;
    intr_realmode(regs,$21);
    if flags_ and fCarry <> 0 then
     begin
      R.FileHandle := $FFFF;
      DosError := ax_;
    end
    else
    begin
      R.FileHandle := ax_;
      FindDataToSearchRec(FindData^, R);
      DosError := 0;
    end;
  end;
end;

(*
 INT 21h  AX=714F
 INT 21 - Windows95 - LONG FILENAME - FIND NEXT MATCHING FILE
         AX = 714Fh
         BX = filefind handle (from AX=714Eh)
         SI = date/time format
         ES:DI -> buffer for FindData record
 Return: CF clear if successful
             CX = Unicode conversion flags
         CF set on error
             AX = error code
                 7100h if function not supported
 Note:   this function is only available when IFSMgr is running,
         not under bare MS-DOS 7

*)

procedure lWIN95FindNext(var R: lSearchRec);
var
  FindData: ^lFindDataRec;
  regs: real_mode_call_structure_typ;
begin
  FindData:=Ptr(segdossyslow32);
  init_register(regs);
  with regs do
   begin
    ax_:= $714F;
    bx_:= R.FileHandle;
    si_:= 1;
    es_:= segdossyslow16;
    di_:= 0;
    flags_:=fCarry;
    intr_realmode(regs,$21);
    if flags_ and fCarry <> 0 then DosError := ax_
    else
    begin
      FindDataToSearchRec(FindData^, R);
      DosError := 0;
    end;
  end;
end;

(*
 INT 21h  AX=71A1
 INT 21 - Windows95 - LONG FILENAME - "FindClose" -
          TERMINATE DIRECTORY SEARCH
         AX = 71A1h
         BX = filefind handle (from AX=714Eh)
 Return: CF clear if successful
         CF set on error
            AX = error code
                 7100h if function not supported
 Notes:  this function must be called after starting a search
         with AX=714Eh, to indicate that the search handle
         returned by that function will no longer be used
         this function is only available when IFSMgr is running,
         not under bare MS-DOS 7
*)

procedure lWIN95FindClose(var R: lSearchRec);
var
  regs: real_mode_call_structure_typ;
begin
  init_register(regs);
  if R.FileHandle <> $FFFF then with regs do
  begin
    ax_:= $71A1;
    bx_:= R.FileHandle;
    intr_realmode(regs,$21);
  end;
end;

procedure lWIN95GetFileNameFunc(const Name: String; var S: String; AFunction: Byte);
var
  NameZ, GetNameZ: TNameZ;
  regs: real_mode_call_structure_typ;
begin
  NameToNameZ(Name, NameZ);
  init_register(regs);
  with regs do
  begin
    ax_ := $7160;
    cl_ := AFunction;
    ch_ := $80;
    ds_ := segdossyslow16;
    si_ := 0;
    es_ := segdossyslow16;
    di_ := SizeOf(TNameZ);
    Move(NameZ,Mem[segdossyslow32],SizeOf(TNameZ));
    flags_:=fCarry;
    intr_realmode(regs,$21);
    if flags_ and fCarry <> 0 then S := Name
    else
    begin
      Move(Mem[segdossyslow32+SizeOf(TNameZ)],GetNameZ,SizeOf(TNameZ));
      S := StrPas(GetNameZ);
      CheckColonAndSlash(Name, S);
    end;
  end;
end;

procedure WIN95TrueName(const Name: String; var S: String);
var
  NameZ, GetNameZ: TNameZ;
  regs: real_mode_call_structure_typ;
begin
  NameToNameZ(Name, NameZ);
  init_register(regs);
  with Regs do
  begin
    ax_ := $7160;
    cl_ := 0;
    ch_ := $00;
    ds_ := segdossyslow16;
    si_ := 0;
    es_ := segdossyslow16;
    di_ := SizeOf(TNameZ);
    Move(NameZ,Mem[segdossyslow32],SizeOf(TNameZ));
    flags_:=fCarry;
    intr_realmode(regs,$21);
    if flags_ and fCarry <> 0 then S := Name
    else
    begin
      Move(Mem[segdossyslow32+SizeOf(TNameZ)],GetNameZ,SizeOf(TNameZ));
      S := StrPas(GetNameZ);
      CheckColonAndSlash(Name, S);
    end;
  end;
end;

function SysFileCreate(FileName: PChar; Mode,Attr: Longint; var Handle: Longint): Longint;
var
  regs     : real_mode_call_structure_typ;
  FullName : TNameZ;
begin
 FillChar(FullName, SizeOf(FullName), #0);
 NameToNameZ(StrPas(FileName), FullName);
 init_register(regs);
 with regs do
  begin
   if lAPI=lDOS then
    begin
     ah_ := $3C;
     al_ := Mode;
     bx_ := 0;
     cx_ := Attr;
     ds_ := segdossyslow16;
    end else
    begin
     ax_ := $716C;
     bx_ := Mode;
     cx_ := Attr;
     dx_ := $12;
     ds_ := segdossyslow16;
     si_ := 0;
     di_ := 0;
    end;
   Move(FullName, Mem[segdossyslow32], SizeOf(FullName));
   flags_:=fCarry;
   intr_realmode(regs, $21);
   if flags_ and fCarry <> 0
     then begin
           Handle := 0;
           SysFileCreate := ax_;
          end
     else begin
           SysFileCreate := 0;
           Handle := ax_;
          end;
  end;
end;

function SysFileOpen(FileName: PChar; Mode: Longint; var Handle: Longint): Longint;
var
  regs     : real_mode_call_structure_typ;
  FullName : TNameZ;
begin
 FillChar(FullName, SizeOf(FullName), #0);
 NameToNameZ(StrPas(FileName), FullName);
 init_register(regs);
 with regs do
  begin
   If lapi=ldos then
    begin
     ah_ := $3D;
     al_ := Mode;
     bx_ := 0;
     cx_ := 0;
     ds_ := segdossyslow16;
    end else
    begin
     ax_ := $716C;
     bx_ := Mode;
     cx_ := 0;
     dx_ := 1;
     ds_ := segdossyslow16;
     si_ := 0;
     di_ := 0;
    end;
   Move(FullName, Mem[segdossyslow32], SizeOf(FullName));
   flags_ := fCarry;
   intr_realmode(regs, $21);
   if flags_ and fCarry <> 0
     then begin
           Handle := 0;
           SysFileOpen := ax_;
          end
     else begin
           SysFileOpen := 0;
           Handle := ax_;
          end;
  end;
end;

procedure lWIN95ChDir(const Path: string);
var
  regs: real_mode_call_structure_typ;
  C: Char;
  NameZ: TNameZ;
begin
  NameToNameZ(Path, NameZ);
  Move(NameZ,Mem[segdossyslow32],SizeOf(TNameZ));
  init_register(regs);
  with regs do
  begin
    C := Upcase(NameZ[0]);
    if (C in ['A'..'Z']) and (NameZ[1] = ':') then
    begin
      ah_ := $0E;
      dl_ := Byte(C) - $41;
      intr_realmode(regs,$21);
    end;

    ax_ := $713B;
    ds_ := segdossyslow16;
    dx_ := 0;
    flags_:=fCarry;
    intr_realmode(regs, $21);

    if (flags_ and fCarry <> 0)
    then InOutRes := ax_
    else InOutRes := 0;
  end;
end;

procedure lWIN95GetDir(D: Byte; var Path: string);
var
  regs: real_mode_call_structure_typ;
  NameZ: TNameZ;
begin
  init_register(regs);
  with regs do
  begin
    ax_ := $7147;
    dl_ := D;
    ds_ := segdossyslow16;
    si_ := 0;
    flags_:=fCarry;
    intr_realmode(regs,$21);
    if flags_ and fCarry <> 0 then NameZ[0] := #0
    else
     Move(Mem[segdossyslow32],NameZ,SizeOf(TNameZ));
    Path := Char(D + $40) + ':\' + StrPas(NameZ);
  end;
end;

procedure lWIN95OpenFile(var F: lFile; RecSize: Word; Action: Byte; Attr:Word);
var
  regs: real_mode_call_structure_typ;
begin
  init_register(regs);
  if FileRec(F.F).Mode <> fmClosed then Close(F.F);
  InOutRes := 0;

  if F.FullName[0] = #0 then
  begin
    FileRec(F.F).Mode := fmInOut;
    FileRec(F.F).RecSize := RecSize;
  end
  else with regs do
  begin
    ax_ := $716C;
    bx_ := FileMode;
    cx_ := Attr;
    dx_ := Action;
    ds_ := segdossyslow16;
    si_ := 0;
    di_ := 0;
    Move(F.FullName,Mem[segdossyslow32],SizeOf(F.FullName));
    flags_:=fCarry;
    intr_realmode(regs,$21);
    if flags_ and fCarry <> 0 then InOutRes := ax_
    else
    begin
      FileRec(F.F).Mode := fmInOut;
      FileRec(F.F).Handle := ax_;
      FileRec(F.F).RecSize := RecSize;
    end;
  end;
end;

procedure lWIN95EraseFile(var F: lFile);
var
  regs: real_mode_call_structure_typ;
begin
  Move(F.FullName,Mem[segdossyslow32],SizeOf(F.FullName));
  init_register(regs);
  with regs do
  begin
    ax_ := $7141;
    ds_ := segdossyslow16;
    dx_ := 0;
    si_ := 0;
    flags_:=fCarry;
    intr_realmode(regs, $21);
    if flags_ and fCarry <> 0
    then InOutRes := ax_
    else InOutRes := 0;
  end;
end;

procedure lWIN95EraseText(var T: lText);
var
  regs: real_mode_call_structure_typ;
begin
  Move(T.FullName,Mem[segdossyslow32],SizeOf(T.FullName));
  init_register(regs);
  with regs do
  begin
    ax_ := $7141;
    ds_ := segdossyslow16;
    dx_ := 0;
    si_ := 0;
    flags_:=fCarry;
    intr_realmode(regs, $21);
    if flags_ and fCarry <> 0
    then InOutRes := ax_
    else InOutRes := 0;
  end;
end;

procedure lWIN95RenameFile(var F: lFile; const NewName: string);
var
  NameZ: TNameZ;
  regs: real_mode_call_structure_typ;
begin
  NameToNameZ(NewName, NameZ);
  Move(F.FullName,Mem[segdossyslow32],SizeOf(F.FullName));
  Move(NameZ,Mem[segdossyslow32+SizeOf(F.FullName)],SizeOf(NameZ));
  init_register(regs);
  with Regs do
  begin
    ax_ := $7156;
    ds_ := segdossyslow16;
    dx_ := 0;
    es_ := segdossyslow16;
    di_ := SizeOf(NameZ);
    flags_:=fCarry;
    intr_realmode(regs,$21);
    if flags_ and fCarry <> 0
    then InOutRes := ax_
    else InOutRes := 0;
  end;
end;

procedure lWIN95RenameText(var T: lText; const NewName: shortstring);
var
  NameZ: TNameZ;
  regs: real_mode_call_structure_typ;
begin
  NameToNameZ(NewName, NameZ);
  Move(T.FullName,Mem[segdossyslow32],SizeOf(T.FullName));
  Move(NameZ,Mem[segdossyslow32+SizeOf(T.FullName)],SizeOf(NameZ));
  init_register(regs);
  with Regs do
  begin
    ax_ := $7156;
    ds_ := segdossyslow16;
    dx_ := 0;
    es_ := segdossyslow16;
    di_ := SizeOf(NameZ);
    flags_:=fCarry;
    intr_realmode(regs, $21);
    if flags_ and fCarry <> 0
    then InOutRes := ax_
    else InOutRes := 0;
  end;
end;

procedure lWIN95FileAttrFunc(var F: lFile; var Attr: Word; Action: Byte);
var
  regs: real_mode_call_structure_typ;
begin
  Move(F.FullName,Mem[segdossyslow32],SizeOf(F.FullName));
  init_register(regs);
  with regs do
  begin
    ax_ := $7143;
    bl_ := Action;
    if Action = faSetAttr then cx_ := Attr;
    ds_ := segdossyslow16;
    dx_ := 0;
    flags_:=fCarry;
    intr_realmode(regs,$21);
    if flags_ and fCarry <> 0
    then DosError := ax_
    else
    begin
      if Action = faGetAttr then Attr := cx_;
      DosError := 0;
    end;
  end;
end;

procedure lWIN95TextAttrFunc(var T: lText; var Attr: Word; Action: Byte);
var
  regs: real_mode_call_structure_typ;
begin
  Move(T.FullName,Mem[segdossyslow32],SizeOf(T.FullName));
  init_register(regs);
  with Regs do
  begin
    ax_ := $7143;
    bl_ := Action;
    if Action = faSetAttr then cx_ := Attr;
    ds_ := segdossyslow16;
    dx_ := 0;
    flags_:=fCarry;
    intr_realmode(regs,$21);
    if flags_ and fCarry <> 0
    then DosError := ax_
    else
    begin
      if Action = faGetAttr then Attr := cx_;
      DosError := 0;
    end;
  end;
end;

procedure lWIN95DirFunc(const Path: shortstring; AFunction: Word);
var
  regs: real_mode_call_structure_typ;
  NameZ: TNameZ;
begin
  NameToNameZ(Path, NameZ);
  Move(NameZ,Mem[segdossyslow32],SizeOf(TNameZ));
  init_register(regs);
  with regs do
  begin
    ax_ := AFunction;
    ds_ := segdossyslow16;
    dx_ := 0;
    flags_:=fCarry;
    intr_realmode(regs,$21);
    if flags_ and fCarry <> 0
    then InOutRes := ax_
    else InOutRes := 0;
  end;
end;

procedure lEraseFile(var F: lFile);
  begin
  if F.AssignFileMode = lWIN95 then lWIN95EraseFile(F) else
  Erase(F.F);
  end;

procedure lEraseText(var T: lText);
  begin
  if T.AssignTextMode = lWIN95 then lWIN95EraseText(T) else
  Erase(T.T);
  end;

{$ENDIF}{*** lfn functions for dpmi32 ***}

procedure lFindFirst(const Path: String; Attr: Word; var R: lSearchRec);
  var
    PathBuf: array[0..SizeOf(PathStr)-1] of Char;
  begin
  R.FullName := '';
{$IFDEF DPMI32}
  if lAPI = lDOS then begin
  R.FindFirstMode := lDOS;
{$ENDIF}
  SetDosError(SysFindFirstNew(StrPCopy(PathBuf, Path), Attr, R.SR, False));
  CorrectSearchRec(R);
{$IFDEF DPMI32}
  end else begin
            R.FindFirstMode := lWIN95;
            lWIN95FindFirst(Path, Attr, R);
           end;
{$ENDIF}
  {$IFDEF OS2}
  R.PrevName := R.FullName;
  {$ENDIF}
  end;

procedure lFindNext(var R: lSearchRec);
  begin
  R.FullName := '';
{$IFDEF DPMI32}
  if R.FindFirstMode = lDOS then begin
{$ENDIF}
  SetDosError(SysFindNextNew(R.SR, False));
  CorrectSearchRec(R);
{$IFDEF DPMI32}
  end else lWIN95FindNext(R);
{$ENDIF}
  {JO: ошибка 49 в оси зарезервирована; мы её будем использовать для}
  {    отлова дупов на HPFS}
  {$IFDEF OS2}
  if  (DosError = 0) and (R.FullName <> '') and (R.FullName <> '.')
       and (R.FullName <> '..')
  then
    begin
    if R.PrevName = R.FullName then
      DosError := 49;
    R.PrevName := R.FullName;
    end;
  {$ENDIF}
  end;

procedure lFindClose(var R: lSearchRec);
  var
    DEr: LongInt;
  begin
  DEr := DosError; {JO}
  {$IFDEF DPMI32}
  if R.FindFirstMode = lWin95 then lWIN95FindClose(R) else
  {$ENDIF}
  SysFindCloseNew(R.SR);
  DosError := DEr; {JO}
  end;

{$IFDEF WIN32}
function lfGetShortFileName(const Name: String): String;
  var
    NZ, NZ2: TNameZ;
    l: LongInt;
  begin
  if  (Name = '.') or (Name = '..') then
    begin
    lfGetShortFileName := Name;
    Exit;
    end;
  NameToNameZ(Name, NZ2);
  if SysPlatformId = 1 then
    OemToChar(@NZ2, @NZ2);
  {AK155 18.07.2003 Тут и ниже приходится испралять баг Win9x,
      в которых GetShortPathName работает в кодировке ANSI несмотря на
      SetFileApisToOEM
    }
  l := GetShortPathName(@NZ2, @NZ, SizeOf(NZ));
  if l = 0 then
    lfGetShortFileName := NoShortName
  else
    begin
    if SysPlatformId = 1 then
      CharToOEM(@NZ, @NZ);
    lfGetShortFileName := StrPas_(NZ);
    end;
  end { lfGetShortFileName };
{$ENDIF}
{$IFDEF DPMI32}
function lfGetShortFileName(const Name: String): String;
  begin
  if lApi = lDOS
  then Result := Name
  else lWIN95GetFileNameFunc(Name, Result, 1);
  end;

function lfGetLongFileName(const Name: String): String;
  begin
  if lApi = lDOS
  then Result := Name
  else lWIN95GetFileNameFunc(Name, Result, 2);
  end;
{$ENDIF}

procedure lTrueName(const Name: String; var S: String);
  begin
  {$IFDEF DPMI32}
  if lApi = lWin95 then
    WIN95TrueName(Name, S)
  else
  {$ENDIF}
  S := Name;
  end;

{AK155 22-11-2003 Найти конец шары в пути. 0 - если это не UNC-путь
}
function GetShareEnd(const S: String): Integer;
  var
    SlashFound: Boolean;
  begin
  Result := 0;
  if Copy(S, 1, 2) <> '\\' then
    Exit;
  { ищем '\' после '\\', и далее до конца или до второго '\' }
  Result := 3;
  SlashFound := False;
  while Result < Length(S) do
    begin
    if S[Result+1] = '\' then
      begin
      if SlashFound then
        Exit; // Успех. Сейчас Copy(S, 1, i) - это '\\server\share'
      SlashFound := True;
      end;
    Inc(Result);
    end;
  if not SlashFound then
    Result := 0;
  { Неправильный это путь: '\\' в начале есть,
      а '\' потом - нет. Надо бы как-то признак ошибки выставить,
      но непонятно как и для кого. }
  end { GetShareEnd };
{/AK155 22-11-2003}

function GetRootStart(const Path: String): Integer;
  begin
  Result := Min(Length(Path)+1, Max(3, GetShareEnd(Path)+1));
  end;

{AK155 22-11-2003 Доработано с учётом UNC-путей }
procedure lFSplit(const Path: String; var Dir, Name, ext: String);
  var
    DriveEnd: Integer;
    DotPos, SlashPos, B: Byte;
    D: String;
    N: String;
    E: String;
  begin
  begin
  Dir := '';
  Name := '';
  ext := '';
  DotPos := 0;
  SlashPos := 0;
  if  (Length(Path) > 1) and (Path[2] = ':') then
    DriveEnd := 2
  else
    DriveEnd := GetShareEnd(Path);

  for B := Length(Path) downto DriveEnd+1 do
    begin
    if  (Path[B] = '.') and (DotPos = 0) then
      begin
      DotPos := B; {JO: имена могут состоять только из расширения}
      if SlashPos <> 0 then
        Break;
      end;
    if  (Path[B] = '/') and (SlashPos = 0) then // slash change by unxed
      begin
      SlashPos := B;
      if DotPos <> 0 then
        Break;
      end;
    end;

  if DotPos+SlashPos = 0 then
    if DriveEnd <> 0 then
      Dir := Path
    else
      Name := Path
  else
    begin
    if DotPos > SlashPos then
      ext := Copy(Path, DotPos, MaxStringLength)
    else
      DotPos := 255;

    if SlashPos <> 0 then
      Dir := Copy(Path, 1, SlashPos);

    Name := Copy(Path, SlashPos+1, DotPos-SlashPos-1);
    end;
  end;
  end { lFSplit };

function lFileNameOf(var lF: lFile): String;
  begin
{$IFDEF DPMI32}
  if lF.AssignFileMode <> lDos then
    lFileNameOf := StrPas_(lF.FullName)
  else
{$ENDIF}
  lFileNameOf := StrPas_(FileRec(lF.F).Name);
  end;

function lTextNameOf(var lT: lText): String;
  begin
  lTextNameOf := StrPas_(TextRec(lT.T).Name);
  end;

procedure lResetFileReadOnly(var F: lFile; RecSize: Word);
  var
    SaveMode: Byte;
  begin
  SaveMode := FileMode;
  FileMode := 64;
  lResetFile(F, RecSize);
  FileMode := SaveMode;
  end;

procedure lReWriteFile(var F: lFile; RecSize: Word);
  var
    OldMode: Byte;
  begin
  OldMode := FileMode;
  FileMode := FileMode and $FC or 2;
{$IFDEF DPMI32}
  if F.AssignFileMode = lWIN95 then lWIN95OpenFile(F, RecSize, faRewrite, 0) else
{$ENDIF}
  Rewrite(F.F, RecSize);
  FileMode := OldMode;
  end;

procedure lResetTextReadOnly(var F: lText);
  var
    SaveMode: Byte;
  begin
  SaveMode := FileMode;
  FileMode := 64;
  lResetText(F);
  FileMode := SaveMode;
  end;

procedure lRewriteText(var F: lText);
  var
    OldMode: Byte;
  begin
  OldMode := FileMode;
  FileMode := FileMode and $FC or 2;
  Rewrite(F.T);
  FileMode := OldMode;
  end;

{ Inline functions, which temporary compiled as not inline, because VP are   }
{ crased on compiling 8(                                                     }

procedure lAssignFile(var F: lFile; const Name: String);
  var
    FName: String;
  begin
  FName := lFExpand(Name);
    { текущий каталог панели - это не такущий каталог ОС, поэтому
     надо развернуть имя до полного пути.}
{$IFDEF DPMI32}
  if lAPI = lDOS then
  begin
    F.AssignFileMode := lDOS;
    Assign(F.F, FName);
  end else
  begin
    F.AssignFileMode := lWIN95;
    FileRec(F.F).Handle := 0;
    FileRec(F.F).Mode := fmClosed;
    NameToNameZ(FName, F.FullName);
  end;
{$ELSE}
  Assign(F.F, FName);
{$ENDIF}
  end;

procedure lAssignText(var T: lText; const Name: String);
  begin
  Assign(T.T, lFExpand(Name));
    { см. комментарий к lAssignFile }
  end;

procedure lResetFile(var F: lFile; RecSize: Word);
  begin
{$IFDEF DPMI32}
  if F.AssignFileMode = lWIN95 then lWIN95OpenFile(F, RecSize, faOpen, 0) else
{$ENDIF}
  Reset(F.F, RecSize);
  end;
procedure lRenameFile(var F: lFile; const NewName: String);
  begin
{$IFDEF DPMI32}
  if F.AssignFileMode = lWIN95 then lWIN95RenameFile(F, NewName) else
{$ENDIF}
  Rename(F.F, NewName);
  end;
procedure lRenameText(var T: lText; const NewName: String);
  begin
{$IFDEF DPMI32}
  if T.AssignTextMode = lWIN95 then lWIN95RenameText(T, NewName) else
{$ENDIF}
  Rename(T.T, NewName);
  end;
procedure lChangeFileName(const Name, NewName: String);
  var
    F: lFile;
  begin
  lAssignFile(F, Name);
  lRenameFile(F, NewName);
  end;
procedure lGetFAttr(var F: lFile; var Attr: Word);
  begin
{$IFDEF DPMI32}
  if F.AssignFileMode = lWIN95 then lWIN95FileAttrFunc(F, Attr, faGetAttr) else
{$ENDIF}
  Dos.GetFAttr(F.F, Attr);
  end;
procedure lSetFAttr(var F: lFile; Attr: Word);
  begin
{$IFDEF DPMI32}
  if F.AssignFileMode = lWIN95 then lWIN95FileAttrFunc(F, Attr, faSetAttr) else
{$ENDIF}
  Dos.SetFAttr(F.F, Attr);
  end;
procedure lGetTAttr(var T: lText; var Attr: Word);
  begin
{$IFDEF DPMI32}
  if T.AssignTextMode = lWIN95 then lWIN95TextAttrFunc(T, Attr, faGetAttr) else
{$ENDIF}
  Dos.GetFAttr(T.T, Attr);
  end;
procedure lSetTAttr(var T: lText; Attr: Word);
  begin
{$IFDEF DPMI32}
  if T.AssignTextMode = lWIN95 then lWIN95TextAttrFunc(T, Attr, faSetAttr) else
{$ENDIF}
  Dos.SetFAttr(T.T, Attr);
  end;
procedure lMkDir(const Path: String);
  begin
{$IFDEF DPMI32}
  if lAPI = lWin95
  then lWIN95DirFunc(Path, $7139) else
{$ENDIF}
  MkDir(Path);
  end;

{AK155: В DN/2, если каталог имеет атрибут ReadOnly, то на FAT или HPFS
он удаляется нормально, а на FAT32 - не удаляется. Так что на всякий
случай надо ReadOnly снять. }
procedure lRmDir(const Path: String);
  var
    f: lFile;
    Attr: Word;
  begin
  lAssignFile(f, Path);
  lGetFAttr(f, Attr);
  if Attr and ReadOnly <> 0 then
    lSetFAttr(f, Attr and not ReadOnly);
  if DosError <> 0 then
    begin
    InOutRes := DosError;
    Exit;
    end;
  NotifyDeleteWatcher(Path);
{$IFDEF DPMI32}
  if lAPI = lWin95
  then lWIN95DirFunc(Path, $713A) else
{$ENDIF}
  RmDir(Path);
  end;
{/AK155}


function lFExpand(Path: String): String;
  var
    D: Byte;
    i, j: Integer;
  begin
  for i := 1 to length(Path) do
    if Path[i] = '/' then // slash change by unxed
      Path[i] := '/'; // slash change by unxed
  if Path = '' then
    Result := ActiveDir
  else if (Copy(Path, 2, 2) = ':\') or (Copy(Path, 1, 2) = '\\') then
    Result := Path // полный путь
  else if Path[1] = '/' then // slash change by unxed
    Result := CurrentRoot + Path // от корня текущего диска/шары
  else if  (Length(Path) >= 2) and (Path[2] = ':') then
    begin // относительный путь указанного диска
    D := Byte(UpCase(Path[1]))-Byte('A')+1;
    Result := CurrentPaths[D] + Copy(Path, 3, 255);
    end
  else
    Result := ActiveDir + Path; // относительный путь
  MakeNoSlash(Result);
  { Удаление '\..' }
  while True do
    begin
    j := Pos('/..', Result); // slash change by unxed
    if j = 0 then
      Break;
    i := j-1;
    while (i <> 0) and (Result[i] <> '/') do // slash change by unxed
      Dec(i);
    Delete(Result, i+1, j-i+2);
    end;
  Replace('./', '', Result); // slash change by unxed
  if Result[Length(Result)] = '.' then
    SetLength(Result, Length(Result)-1);
  end;

procedure lChDir(Path: String);
  var
    i: Longint;
  begin
  Path := lFExpand(Path);
  if PathExist(Path) then
    begin
    ActiveDir := Path;
    MakeSlash(ActiveDir);
    i := GetShareEnd(Path);
    if i = 0 then
      i := 2;
    CurrentRoot := Copy(ActiveDir, 1, i);
    if  (InOutRes = 0) and (Length(Path) > 2) and (Path[2] = ':') then
      CurrentPaths[Byte(UpCase(Path[1]))-Byte('A')+1] := ActiveDir;
    end
  else
    InOutRes := DosError;
  end;

procedure lGetDir(D: Byte; var Path: String);
  label
    DelDlash;
  begin
  if D = 0 then
    begin
    Path := ActiveDir;
    if Path[1] = '/' then // slash change by unxed
      goto DelDlash;
    D := Byte(UpCase(Path[1]))-Byte('A')+1;
    end;
  if CurrentPaths[D] = '' then
    Path := Char(D+Byte('A')-1)+':\' {GetDir(D, Path)}
  else
    Path := CurrentPaths[D];
DelDlash:
  MakeNoSlash(Path);
  end;
{/Cat}

procedure InitPath;
  var
    D: Integer;
    P: String[3];
{$IFDEF DPMI32}
    lsr: lSearchRec;
{$ENDIF}
  begin
{$IFDEF DPMI32}
  //check LFN Api presence
  lsr.FindFirstMode := lWIN95;
  lWIN95FindFirst(ParamStr(0), AnyFileDir, lsr);
  lFindClose(lsr);
  if DosError <> 0 then
    lApi := lDOS;
{$ENDIF}
  P := 'A:\';
  for D := 1 to High(CurrentPaths) do
    begin
    CurrentPaths[D] := P;
    Inc(P[1]);
    end;
  GetDir(0, StartDir);
  StartDir[1] := Upcase(StartDir[1]);
     //piwamoto: w32 shortcut may have c:\ as directory
     //but DN needs C:\ for internal use
  lChDir(StartDir);
  end;

begin
InitPath;
end.
