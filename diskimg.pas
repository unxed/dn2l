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

unit DiskImg;

interface

uses
  FilesCol
  ;

procedure UnpackDiskImages(AOwner: Pointer; Files: PFilesCollection);

implementation

uses
  Dos, Lfnvp, Messages, Views, Commands, Defines, Streams, DNApp, Drivers,
  Advance1, Advance2, FileCopy, Gauge, xTime
  , Files, Tree, VPUtils
  ;

{Cat: добавил сюда эти типы, вместо того, чтобы подключать модуль DiskTool}
type
  PBootRec = ^TBootRec;
  TBootRec = record
    JMP: array[0..2] of Byte;
    OEM: array[0..7] of Char;
    ByteSec: AWord;
    SecClu: Byte;
    ResSect: AWord;
    FATCps: Byte;
    RootEnt: AWord;
    TotalSc: AWord;
    BPB: Byte;
    SecFAT: AWord;
    SecTrack: AWord;
    Sides: AWord;
    HiddenSc: LongInt;
    NumSec: LongInt;
    PhysNum: AWord;
    ExtBootS: Byte;
    VolumeN: array[0..4] of Byte;
    VolumeL: array[0..10] of Char;
    FilSysID: array[0..5] of Char;
    end;

  TDirRec = record
    Name: array[0..10] of Char;
    Attr: Byte;
    Data: array[0..9] of Byte;
    Date: LongInt;
    Clus: AWord;
    len: LongInt;
    end;
  {/Cat}

  {-DataCompBoy-}
function UnpackImage(const FName, DestDir: String; PInfo: PWhileView)
  : Boolean;
  var
    F: PBufStream;
    Error, Cancel: Boolean;
    Tmr: TEventTimer;

  procedure CheckCancel;
    begin
    if Cancel then
      Exit;
    if TimerExpired(Tmr) then
      begin
      DispatchEvents(PInfo, Cancel);
      Cancel := (Cancel or CtrlBreakHit)
               and (MessageBox(GetString(dlQueryAbort), nil,
             mfYesNoConfirm) = cmYes);
      CtrlBreakHit := False;
      NewTimer(Tmr, 50);
      end;
    end;

  procedure DoIt;
    var
      BRec: PBootRec;
      FAT: PByteArray absolute BRec;

      Reserved: Word;
      FatCopies: Byte;
      SectPerClust: Byte;
      BytesPerClu: Word;
      SectPerFAT: Word;
      SectLen: Word;
      FirstClust: Word;
      RootEntries: Word;
      TotalClu: Word;
      FATSize: LongInt;
      NumSectors: LongInt;

      VolumeLabel: String[13];
      fPos: LongInt;
      SearchVolume: Boolean; {piwamoto}
      DSKoffset: AWord; {piwamoto}

    procedure fSeek(Pos: LongInt);
      begin
      if fPos = Pos then
        Exit;
      fPos := Pos;
      F^.Seek(Pos);
      end;

    procedure fRead(var Buf; Count: Word);
      begin
      F^.Read(Buf, Count);
      Inc(fPos, Count);
      end;

    function NextFAT(N: Word): Word;
      var
        D: Word;
      begin
      if Error or (N < 2) or (N > TotalClu+2) then
        begin
        Error := True;
        Exit
        end;
      D := (N*3) shr 1;
      D := FAT^[D]+FAT^[D+1] shl 8;
      if N and 1 = 0 then
        D := D and $FFF
      else
        D := D shr 4;
      NextFAT := D;
      end;

    function Fat2Sec(W: Word): LongInt;
      begin
      Fat2Sec := LongInt(FirstClust+(W-2)*SectPerClust);
      end;

    procedure WriteFile(const ADir: String; const ANm: Str12; Clus: Word;
        len, Date: LongInt; Attr: Byte);
      var
        B: PBufStream;
        Dir, FName: String;

      procedure TryInit;
        begin
        B := New(PBufStream, Init(FName, stCreate, $8000));
        if B^.Status <> stOK then
          begin
          Dispose(B, Done);
          B := nil;
          end;
        end;

      var
        CpyBuf: PByteArray;

      procedure Cpy;
        var
          A: Word;
          LocErr: Boolean;
        begin
        if Error or Cancel then
          Exit;
        CtrlBreakHit := False;
        LocErr := True;
        repeat
          CheckCancel;
          if Cancel then
            Break;
          if F^.Status <> stOK then
            Exit;
          fSeek(Fat2Sec(Clus)*SectLen+DskOffset);
          if F^.Status <> stOK then
            Exit;
          A := Min(len, BytesPerClu);
          Dec(len, A);
          fRead(CpyBuf^, A);
          if F^.Status <> stOK then
            Exit;
          B^.Write(CpyBuf^, A);
          if B^.Status <> stOK then
            Exit;
          if len = 0 then
            begin
            LocErr := False;
            Break;
            end;
          Clus := NextFAT(Clus);
        until Error;
        Error := Error or LocErr;
        end { Cpy };

      procedure SetFData;
        var
          F: lFile;
        begin
        FileMode := $22 {open_access_ReadWrite + open_share_DenyWrite};
        lAssignFile(F, FName);
        lResetFile(F, 1);
        SetFTime(F, Date);
        Close(F.F);
        if Attr <> Archive then
          lSetFAttr(F, Attr);
        end;

      begin { WriteFile }
      if Error then
        Exit;
      Dir := FmtStrDN(ADir, VolumeLabel);
      FName := MakeNormName(Dir, ANm);
      PInfo^.Write(3, LowStrg(GetName(FName)));
      TryInit;
      if B = nil then
        begin
        CreateDirInheritance(Dir, False);
        TryInit;
        if B = nil then
          Exit;
        end;
      GetMem(CpyBuf, BytesPerClu);
      Cpy;
      FreeMem(CpyBuf, BytesPerClu);
      Dispose(B, Done);
      B := nil;
      if Error then
        EraseFile(FName)
      else
        SetFData;
      end { WriteFile };

    procedure ProcessDir(const Dir: String; Ofst: Word;
         OfstIsCluster: Boolean);
      var
        _Dir: PByteArray;
        DR: TDirRec;
        Nm: Str12;
        J, I, FileCount, MemSize: Word;
      begin
      CtrlBreakHit := False;
      if OfstIsCluster then
        begin {subdir}
        MemSize := 0;
        FileCount := Ofst;
        {use FileCount for counting subdir's FAT entries}
        repeat
          {calc memsize for this subdir}
          FileCount := NextFAT(FileCount);
          Inc(MemSize);
        until (FileCount > $ff7);
        MemSize := MemSize*BytesPerClu;
        GetMem(_Dir, MemSize);
        if _Dir = nil then
          Exit;
        I := 0;
        repeat
          fSeek(Fat2Sec(Ofst)*SectLen+DskOffset);
          fRead(_Dir^[I], BytesPerClu);
          if F^.Status <> stOK then
            Exit;
          I := I+BytesPerClu;
          Ofst := NextFAT(Ofst);
        until (Ofst > $ff7);
        FileCount := I div 32;
        end
      else
        begin {root dir}
        fSeek(LongInt(Ofst)*SectLen+DskOffset);
        MemSize := RootEntries*32;
        GetMem(_Dir, MemSize);
        fRead(_Dir^, MemSize);
        if F^.Status <> stOK then
          Exit;
        FileCount := RootEntries;
        end;
      for I := 0 to FileCount-1 do
        begin
        CheckCancel;
        if Error or Cancel then
          Break;
        Move(_Dir^[I*SizeOf(DR)], DR, SizeOf(DR));
        case DR.Name[0] of
          #$E5:
            Continue;
          #0:
            Break;
          #5:
            DR.Name[0] := #$E5;
        end {case};
        if DR.Attr and (VolumeID+SysFile+Hidden+ReadOnly) = VolumeID
        then
          {LFN bugfix by piwamoto}
          if SearchVolume then
            begin
            SetLength(VolumeLabel, 11); {VolumeLabel[0] := #11}
            Move(DR.Name[0], VolumeLabel[1], 11);
            // fixme: commented by unxed
            //DelRight(VolumeLabel);
            VolumeLabel := VolumeLabel+'/'; // slash change by unxed
            Break;
            end
          else
            Continue;
        if SearchVolume then
          Continue;
        if DR.Attr and VolumeID <> 0 then
          Continue; {i'll skip VFAT names}
        SetLength(Nm, 12); {Nm[0] := #12;}
        Nm[9] := '.';
        Move(DR.Name[0], Nm[1], 8);
        Move(DR.Name[8], Nm[10], 3);
        J := 8;
        while (J > 0) and (Nm[J] = ' ') do
          begin
          Delete(Nm, J, 1);
          Dec(J);
          end;
        while Nm[Length(Nm)] = ' ' do
          SetLength(Nm, Length(Nm)-1); {Dec(Nm[0]);}
        if Nm[Length(Nm)] = '.' then
          SetLength(Nm, Length(Nm)-1); {Dec(Nm[0]);}

        if DR.Attr and Directory <> 0 then
          begin
          if not IsDummyDir(Nm) then
            ProcessDir(Dir+Nm+'/', DR.Clus, True); {JO} // slash change by unxed
          Continue;
          end;
        WriteFile(Dir, Nm, DR.Clus, DR.len, DR.Date, DR.Attr);
        end;
      FreeMem(_Dir, MemSize);
      _Dir := nil;
      end { ProcessDir };

    procedure ChkValid;
      begin
      if Error then
        Exit;
      Error := True;
      case SectLen of
        128, 256, 512, 1024, 2048, 4096:
          ;
        else {case}
          Exit
      end {case};
      case FatCopies of
        1, 2:
          ;
        else {case}
          Exit
      end {case};
      Error := False;
      end;

    begin { DoIt }
    VolumeLabel := '';
    fPos := 0;
    SearchVolume := True;
    DSKoffset := 0;
    if F^.Status <> stOK then
      Exit;
    GetMem(BRec, 4096);
    fRead(BRec^, 4096);
    if  (BRec^.JMP[0] = $49) and (BRec^.JMP[1] = $4D) then
      {.DDI support by piwamoto}
      case BRec^.OEM[7] of
        #2:
          DSKoffset := $1E00;
        #4:
          DSKoffset := $2400;
        else {case}
          DSKoffset := $1200; {#1,#3}
      end {case};

    {IBM diskimages support by piwamoto}
    if  (BRec^.JMP[0] = $AA) and
        (BRec^.JMP[1] in [$58..$5a]) and
        (BRec^.JMP[2] = $f0)
    then
      DSKoffset := BRec^.ExtBootS+256*BRec^.VolumeN[0];

    if DSKoffset <> 0 then
      begin
      fSeek(DSKoffset);
      fRead(BRec^, 4096);
      end;

    SectPerFAT := BRec^.SecFAT;
    SectLen := BRec^.ByteSec;
    FatCopies := BRec^.FATCps;
    Reserved := BRec^.ResSect;
    RootEntries := BRec^.RootEnt;
    SectPerClust := BRec^.SecClu;
    if BRec^.TotalSc = 0 then
      NumSectors := BRec^.NumSec
    else
      NumSectors := BRec^.TotalSc;
    FreeMem(BRec, 4096);
    BRec := nil;
    if F^.Status <> stOK then
      Exit;

    ChkValid;
    if Error then
      Exit;

    FATSize := LongInt(SectPerFAT)*SectLen;

    FirstClust := (SectPerFAT*FatCopies)+Reserved
      + ( (LongInt(RootEntries)*32) div SectLen);

    BytesPerClu := SectPerClust*SectLen;
    TotalClu := LongInt(NumSectors-FirstClust+1) div SectPerClust;
    if FATSize > $8000 then
      Exit;

    fSeek(Reserved*SectLen+DSKoffset);
    if F^.Status <> stOK then
      Exit;

    GetMem(FAT, FATSize);
    fRead(FAT^, FATSize);
    if F^.Status = stOK then
      begin {piwamoto}
      ProcessDir(DestDir+'%s', SectPerFAT*FatCopies+Reserved, False);
      SearchVolume := False;
      ProcessDir(DestDir+'%s', SectPerFAT*FatCopies+Reserved, False);
      end;
    FreeMem(FAT, FATSize);

    end { DoIt };

  begin { UnpackImage }
  PInfo^.Write(1, GetName(FName));
  Error := False;
  Cancel := False;
  NewTimer(Tmr, 0);
  F := New(PBufStream, Init(FName, stOpenRead, $8000));
  DoIt;
  Error := Error or (F^.Status <> stOK);
  Dispose(F, Done);
  F := nil;
  if Error then
    MessageBox(GetString(dlImageError)+' '+Cut(FName, 40), nil,
       mfError+mfOKButton);
  UnpackImage := not (Error or Cancel);
  end { UnpackImage };
{-DataCompBoy-}

procedure RereadGlobal(const OutputDir: String);
  var OD1 :String;
  begin
  OD1 := '>' + OutputDir; //признак перечитывания подкаталогов в ветви
  GlobalMessage(evCommand, cmPanelReread, @OD1);
  GlobalMessage(evCommand, cmRereadTree, @OutputDir);
  end;

procedure DoIt(AOwner: Pointer; AFiles: PFilesCollection;
     var ADestPath: String);
  var
    I: Integer;
    PF: PFileRec;
    PInfo: PWhileView;
    R: TRect;
  begin
  if ADestPath = '' then
    lGetDir(0, ADestPath);
  ADestPath := MakeNormName(ADestPath, '');

  R.Assign(1, 1, 26, 10);
  New(PInfo, Init(R));
  PInfo^.Top := GetString(dlImage);
  Desktop^.Insert(PInfo);

  CreateDirInheritance(ADestPath, False);
  for I := 0 to AFiles^.Count-1 do
    begin
    PF := AFiles^.At(I);
    if not UnpackImage(MakeNormName(CnvString(PF^.Owner),
           PF^.FlName[True]), ADestPath, PInfo)
    then
      Break;
    if AOwner <> nil then
      Message(AOwner, evCommand, cmCopyUnselect, PF);
    end;

  Dispose(PInfo, Done);
  PInfo := nil;
  RereadGlobal(ADestPath);
  end { DoIt };

procedure UnpackDiskImages;
  var
    DestPath: String;
  begin
  if  (Files = nil) or (Files^.Count = 0) then
    Exit;
  DestPath := '';
  Message(Application, evCommand, cmPushFullName, @DestPath);
  if InputBox(GetString(dlUnpackImages), GetString(dlTargetDir),
       DestPath, 255, 0) {DataCompBoy}
    = cmOK
  then
    DoIt(AOwner, Files, DestPath);
  Files^.DeleteAll;
  Dispose(Files, Done);
  Files := nil;
  end;

end.
