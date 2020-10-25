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
unit ArvidAvt;

interface

uses
  Arvid, Objects2, Streams, Messages, DNApp, Commands, Collect,
  Views, Drivers, Startup, UKeyMap, Advance, Lfnvp, Files, Dos, Tree,
  FilesCol, Advance2, Drives, FlPanel, Memory
  , Defines
  ;

function AvtCMP(S1, S2: String): Integer;
function AvtCellText(Offset: LongInt; var S: TStream): String;
function AvtCellName(const C: TAvtFileCell; var AStream: TStream): String;
function AvtCellDesc(const C: TAvtFileCell; var AStream: TStream): String;

function AvtGetCell(AvtDr: PArvidDrive): LongInt;
procedure AvtFreeCell(AvtDr: PArvidDrive; const l: LongInt);
function AvtCellRotateLeft(AvtDr: PArvidDrive; Loc: LongInt): LongInt;
function AvtCellRotateRight(AvtDr: PArvidDrive; Loc: LongInt): LongInt;
{procedure AvtCheckTree(AvtDr:PArvidDrive);}
function AvtDelFile(AvtDr: PArvidDrive; AName: String): Boolean;
function AvtNewFile(AvtDr: PArvidDrive;
    AName: String;
    ADescription: String;
    AIsDir: Boolean;
    AChildOrSize: LongInt;
    ATime: LongInt;
    AStartSector: LongInt;
    AAttr: Word): Word;
procedure AvtSeekDirectory(AvtDr: PArvidDrive);
procedure AvtGetDirectory(AvtDr: PArvidDrive; var ALocation: LongInt;
    var FC: PFilesCollection; const FileMask: String);
function CopyFilesToArvid(const S: String; Files: PCollection;
     MoveMode: Boolean; Owner: Pointer): Boolean;
procedure AvtCopyFilesInto(AvtDr: PArvidDrive; AFiles: PCollection;
    Own: PView; MoveMode: Boolean);
procedure AvtEraseFiles(AvtDr: PArvidDrive; AFiles: PCollection);
procedure AvtMakeDir(AvtDr: PArvidDrive);
procedure AvtEditDescription(AvtDr: PArvidDrive; var S, Nam: String);
procedure AvtCalcTotal(AvtDr: PArvidDrive; const Offset: LongInt;
     var LL: TSize);
function AvtInit(AvtDr: PArvidDrive): Boolean;

implementation
uses
  PDSetup, Advance1
  ;

var
  FCtemp, FCLtemp, FCRtemp: TAvtFileCell;
  ArvidDeleteAllFiles: Boolean;

function AvtCMP(S1, S2: String): Integer;
  begin
  UpStr(S1);
  UpStr(S2);
  if S1 = S2 then
    AvtCMP := 0
  else if S1 < S2 then
    AvtCMP := -1
  else
    AvtCMP := 1;
  end;

function AvtCellText(Offset: LongInt; var S: TStream): String;
  var
    T: TAvtTextCell;
    S1: String;
  begin
  S1 := '';
  while Offset <> 0 do
    begin
    S.Seek(Offset);
    S.Read(T, SizeOf(T));
    if S.Status <> stOK then
      Break;
    S1 := S1+Copy(T.Data, 1, 36);
    Offset := T.NextTextCell;
    end;
  AvtCellText := S1;
  end;

function AvtCellName(const C: TAvtFileCell; var AStream: TStream): String;
  var
    S: String;
  begin
  case C.Flags and AvtCellFormat of
    avtCell0:
      S := Copy(C.Name0, 1, 16);
    avtCell1:
      S := Copy(C.Name1, 1, 12);
    avtCell2:
      S := Copy(C.Name2, 1, 12);
    avtCell3:
      S := AvtCellText(C.NamePtr, AStream);
  end {case};
  while (Length(S) <> 0) and (S[Length(S)] = #0) do
    SetLength(S, Length(S)-1);
  AvtCellName := S;
  end;

function AvtCellDesc(const C: TAvtFileCell; var AStream: TStream): String;
  var
    S: String;
  begin
  case C.Flags and AvtCellFormat of
    avtCell2:
      S := AvtCellText(C.DescPtr2, AStream);
    avtCell3:
      S := AvtCellText(C.DescPtr3, AStream);
    else {case}
      S := '' {avtCell0,avtCell1};
  end {case};
  while (Length(S) <> 0) and (S[Length(S)] = #0) do
    SetLength(S, Length(S)-1);
  AvtCellDesc := S;
  end;

function AvtGetCell(AvtDr: PArvidDrive): LongInt;
  var
    T: TAvtTextCell;
    M: TAvtMediaCell;
    B: PChar;
  begin
  with AvtDr^ do
    begin
    if AVT.FreeCell = 0 then
      begin
      AvtGetCell := AVT.AfterLastCell;
      Inc(AVT.AfterLastCell, SizeOf(T));
      if  (PosTableOfs <> 0) and (AVT.AfterLastCell >= PosTableOfs) then
        begin
        Stream^.Seek(AVT.AvtMediaCell);
        Stream^.Read(M, SizeOf(M));
        GetMem(B, M.PositionTableSize);
        Stream^.Seek(PosTableOfs);
        Stream^.Read(B^, M.PositionTableSize);
        Inc(PosTableOfs, 512);
        M.PositionTable := PosTableOfs;
        Stream^.Seek(PosTableOfs);
        Stream^.Write(B^, M.PositionTableSize);
        FreeMem(B, M.PositionTableSize);
        Stream^.Seek(AVT.AvtMediaCell);
        Stream^.Write(M, SizeOf(M));
        end;
      end
    else
      begin
      AvtGetCell := AVT.FreeCell;
      Stream^.Seek(AVT.FreeCell);
      Stream^.Read(T, SizeOf(T));
      AVT.FreeCell := T.NextTextCell;
      end;
    end
  end { AvtGetCell };

procedure AvtFreeCell(AvtDr: PArvidDrive; const l: LongInt);
  var
    T: TAvtTextCell;
  begin
  with AvtDr^ do
    begin
    if l = 0 then
      Exit;
    T.NextTextCell := AVT.FreeCell;
    Stream^.Seek(l);
    Stream^.Write(T, SizeOf(T));
    AVT.FreeCell := l;
    end
  end;

function AvtCellRotateLeft(AvtDr: PArvidDrive; Loc: LongInt): LongInt;
  var
    Right: LongInt;
  begin
  with AvtDr^ do
    begin
    Stream^.Seek(Loc);
    Stream^.Read(FCtemp, SizeOf(FCtemp));
    Stream^.Seek(FCtemp.RightFileCell);
    Stream^.Read(FCRtemp, SizeOf(FCRtemp));
    if  (FCRtemp.Flags and avtBalance) = $00000300 then
      begin
      Right := AvtCellRotateRight(AvtDr, FCtemp.RightFileCell);
      Stream^.Seek(Loc);
      Stream^.Read(FCtemp, SizeOf(FCtemp));
      FCtemp.RightFileCell := Right;
      Stream^.Seek(FCtemp.RightFileCell);
      Stream^.Read(FCRtemp, SizeOf(FCRtemp));
      end;
    Right := FCtemp.RightFileCell;
    FCtemp.RightFileCell := FCRtemp.LeftFileCell;
    FCRtemp.LeftFileCell := Loc;
    if FCRtemp.Flags and avtBalance = 0
    then
      FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
        {or $00000000}
    else
      FCtemp.Flags := (FCtemp.Flags and (not avtBalance)) or $00000300;
    FCRtemp.Flags := (FCRtemp.Flags and (not avtBalance)) or $00000300;
    Stream^.Seek(Loc);
    Stream^.Write(FCtemp, SizeOf(FCtemp));
    Stream^.Seek(Right);
    Stream^.Write(FCRtemp, SizeOf(FCRtemp));
    AvtCellRotateLeft := Right;
    end
  end { AvtCellRotateLeft };

function AvtCellRotateRight(AvtDr: PArvidDrive; Loc: LongInt): LongInt;
  var
    Left: LongInt;
  begin
  with AvtDr^ do
    begin
    Stream^.Seek(Loc);
    Stream^.Read(FCtemp, SizeOf(FCtemp));
    Stream^.Seek(FCtemp.LeftFileCell);
    Stream^.Read(FCLtemp, SizeOf(FCLtemp));
    if  (FCLtemp.Flags and avtBalance) = $00000100 then
      begin
      Left := AvtCellRotateLeft(AvtDr, FCtemp.LeftFileCell);
      Stream^.Seek(Loc);
      Stream^.Read(FCtemp, SizeOf(FCtemp));
      FCtemp.LeftFileCell := Left;
      Stream^.Seek(FCtemp.LeftFileCell);
      Stream^.Read(FCLtemp, SizeOf(FCLtemp));
      end;
    Left := FCtemp.LeftFileCell;
    FCtemp.LeftFileCell := FCLtemp.RightFileCell;
    FCLtemp.RightFileCell := Loc;
    if  (FCLtemp.Flags and avtBalance) = 0
    then
      FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
        {or $00000000}
    else
      FCtemp.Flags := (FCtemp.Flags and (not avtBalance)) or $00000100;
    FCLtemp.Flags := (FCLtemp.Flags and (not avtBalance)) or $00000100;
    Stream^.Seek(Loc);
    Stream^.Write(FCtemp, SizeOf(FCtemp));
    Stream^.Seek(Left);
    Stream^.Write(FCLtemp, SizeOf(FCLtemp));
    AvtCellRotateRight := Left;
    end
  end { AvtCellRotateRight };

{
procedure AvtCheckTree(AvtDr: PArvidDrive);
  function AvtNodeHight(const Loc: LongInt): LongInt;
  var
    LeftHight:  LongInt;
    RightHight: LongInt;
  begin with AvtDr^ do begin
    AvtNodeHight:=0;
    if Loc <> 0 then begin
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      LeftHight:=AvtNodeHight(FCtemp.LeftFileCell);
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      RightHight:=AvtNodeHight(FCtemp.RightFileCell);
      if LeftHight >= RightHight then
        AvtNodeHight:=LeftHight + 1 else
        AvtNodeHight:=RightHight + 1;
    end;
  end end;

  procedure AvtNodeCheck(const Loc: LongInt);
  var
    LeftHight:  LongInt;
    RightHight: LongInt;
    Balance:    LongInt;
  begin with AvtDr^ do begin
    if Loc <> 0 then begin
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      if (FCtemp.Flags and avtIsDir) <> 0 then begin
        AvtNodeCheck(FCtemp.ChildOrSize);
        Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      end;
      AvtNodeCheck(FCtemp.LeftFileCell);
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      AvtNodeCheck(FCtemp.RightFileCell);
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));

      LeftHight:=AvtNodeHight(FCtemp.LeftFileCell);
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      RightHight:=AvtNodeHight(FCtemp.RightFileCell);
      Balance:=RightHight-LeftHight;
      if (Balance < -1) or (Balance > 1) then
        MessageBox(GetString(erInvalidFileFormat), nil, mfError + mfOKButton)
      else begin
        Balance:=(Balance shl 8) and $00000300;
        Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
        if Balance <> (FCtemp.Flags and $00000300) then
          MessageBox(GetString(erInvalidFileFormat), nil, mfError + mfOKButton);
      end;
    end;
  end end;

begin
  AvtNodeCheck(AvtDr^.AVT.RootDirCell);
end;
}

function AvtDelFile(AvtDr: PArvidDrive; AName: String): Boolean;
  var
    SaveCurDir: String;
    CurDir2: String;
    Dr: String;
    Nm: String;
    Xt: String;
    SN: String;
    FC: TAvtFileCell;
    FCL: TAvtFileCell;
    FCR: TAvtFileCell;
    NewCurDirPos: LongInt;
    DelDirAnswer: Word;

  label 1;

  procedure AvtDelCell(Loc: LongInt);
    var
      FC: TAvtFileCell;
      T: TAvtTextCell;
      L: LongInt;
    begin
    with AvtDr^ do
      begin
      Stream^.Seek(Loc);
      Stream^.Read(FC, SizeOf(FC));
      case FC.Flags and AvtCellFormat of
        avtCell2:
          L := FC.DescPtr2;
        avtCell3:
          L := FC.DescPtr3;
        else {case}
          L := 0 {avtCell0,avtCell1};
      end {case};
      while L <> 0 do
        begin
        Stream^.Seek(L);
        Stream^.Read(T, SizeOf(T));
        AvtFreeCell(AvtDr, L);
        L := T.NextTextCell;
        end;
      if FC.Flags and AvtCellFormat = avtCell3
      then
        L := FC.NamePtr
      else
        L := 0 {avtCell0,avtCell1,avtCell2};
      while L <> 0 do
        begin
        Stream^.Seek(L);
        Stream^.Read(T, SizeOf(T));
        AvtFreeCell(AvtDr, L);
        L := T.NextTextCell;
        end;
      AvtFreeCell(AvtDr, Loc);
      end
    end { AvtDelCell };

  procedure AvtFreeTree(const Loc: LongInt);
    begin
    with AvtDr^ do
      begin
      if Loc = 0 then
        Exit;
      Stream^.Seek(Loc);
      Stream^.Read(FC, SizeOf(FC));
      if FC.Flags and AvtIsDir <> 0 then
        begin
        AvtFreeTree(FC.ChildOrSize);
        Stream^.Seek(Loc);
        Stream^.Read(FC, SizeOf(FC));
        end;
      AvtFreeTree(FC.LeftFileCell);
      Stream^.Seek(Loc);
      Stream^.Read(FC, SizeOf(FC));
      AvtFreeTree(FC.RightFileCell);
      AvtDelCell(Loc);
      end
    end;

  function AvtTreeNodeRemove(const Loc: LongInt): LongInt;
    var
      OldBalance: LongInt;
      L0, L1, L2: LongInt;

    function AvtRestoreLeftBalance(Loc: LongInt;
        const OldBalance: LongInt): LongInt;
      var
        LeftBalance: LongInt;
        CurBalance: LongInt;
        RightZeroBal: Boolean;
      begin
      with AvtDr^ do
        begin
        Stream^.Seek(Loc);
        Stream^.Read(FC, SizeOf(FC));
        CurBalance := FC.Flags and avtBalance;
        if FC.LeftFileCell <> 0 then
          begin
          Stream^.Seek(FC.LeftFileCell);
          Stream^.Read(FCL, SizeOf(FCL));
          LeftBalance := FCL.Flags and avtBalance;
          end;
        if  (FC.LeftFileCell = 0) or
            ( (LeftBalance <> OldBalance) and (LeftBalance = 0))
        then
          begin
          FC.Flags := FC.Flags and (not avtBalance);
          {       if CurBalance = $00000300 then FC.Flags:=FC.Flags or $00000000 else}
          if CurBalance = $00000000 then
            FC.Flags := FC.Flags or $00000100
          else
            {       if CurBalance = $00000100}
            begin
            Stream^.Seek(FC.RightFileCell);
            Stream^.Read(FCR, SizeOf(FCR));
            RightZeroBal := (FCR.Flags and avtBalance) = 0;
            Loc := AvtCellRotateLeft(AvtDr, Loc);
            Stream^.Seek(Loc);
            Stream^.Read(FC, SizeOf(FC));
            Stream^.Seek(FC.LeftFileCell);
            Stream^.Read(FCL, SizeOf(FCL));
            if RightZeroBal then
              begin
              FC.Flags := (FC.Flags and (not avtBalance)) or $00000300;
              FCL.Flags := (FCL.Flags and (not avtBalance)) or $00000100;
              end
            else
              begin
              FC.Flags := (FC.Flags and (not avtBalance)) {or $00000000};
              FCL.Flags := (FCL.Flags and (not avtBalance)) {or $00000000};
              end;
            Stream^.Seek(FC.LeftFileCell);
            Stream^.Write(FCL, SizeOf(FCL));
            end;
          Stream^.Seek(Loc);
          Stream^.Write(FC, SizeOf(FC));
          end;
        AvtRestoreLeftBalance := Loc;
        end
      end { AvtRestoreLeftBalance };

    function AvtRestoreRightBalance(Loc: LongInt;
        const OldBalance: LongInt): LongInt;
      var
        RightBalance: LongInt;
        CurBalance: LongInt;
        LeftZeroBal: Boolean;
      begin
      with AvtDr^ do
        begin
        Stream^.Seek(Loc);
        Stream^.Read(FC, SizeOf(FC));
        CurBalance := FC.Flags and avtBalance;
        if FC.RightFileCell <> 0 then
          begin
          Stream^.Seek(FC.RightFileCell);
          Stream^.Read(FCR, SizeOf(FCR));
          RightBalance := FCR.Flags and avtBalance;
          end;
        if  (FC.RightFileCell = 0) or
            ( (RightBalance <> OldBalance) and (RightBalance = 0))
        then
          begin
          FC.Flags := FC.Flags and $FFFFFCFF;
          {       if CurBalance = $00000100 then FC.Flags:=FC.Flags or $00000000 else}
          if CurBalance = $00000000 then
            FC.Flags := FC.Flags or $00000300
          else
            {       if CurBalance = $00000300}
            begin
            Stream^.Seek(FC.LeftFileCell);
            Stream^.Read(FCL, SizeOf(FCL));
            LeftZeroBal := (FCL.Flags and avtBalance) = 0;
            Loc := AvtCellRotateRight(AvtDr, Loc);
            Stream^.Seek(Loc);
            Stream^.Read(FC, SizeOf(FC));
            Stream^.Seek(FC.RightFileCell);
            Stream^.Read(FCR, SizeOf(FCR));
            if LeftZeroBal then
              begin
              FC.Flags := (FC.Flags and (not avtBalance)) or $00000100;
              FCR.Flags := (FCR.Flags and (not avtBalance)) or $00000300;
              end
            else
              begin
              FC.Flags := (FC.Flags and (not avtBalance)) {or $00000000};
              FCR.Flags := (FCR.Flags and (not avtBalance)) {or $00000000};
              end;
            Stream^.Seek(FC.RightFileCell);
            Stream^.Write(FCR, SizeOf(FCR));
            end;
          Stream^.Seek(Loc);
          Stream^.Write(FC, SizeOf(FC));
          end;
        AvtRestoreRightBalance := Loc;
        end
      end { AvtRestoreRightBalance };

    function AvtTreeRemoveLeftmost(const Loc: LongInt): LongInt;
      var
        OldBalance: LongInt;
        L1, L2: LongInt;
      begin
      with AvtDr^ do
        begin
        Stream^.Seek(Loc);
        Stream^.Read(FC, SizeOf(FC));
        if FC.LeftFileCell = 0 then
          begin
          l0 := Loc;
          AvtTreeRemoveLeftmost := FC.RightFileCell;
          Exit;
          end;
        L1 := FC.LeftFileCell;
        Stream^.Seek(L1);
        Stream^.Read(FC, SizeOf(FC));
        OldBalance := FC.Flags and avtBalance;
        L2 := AvtTreeRemoveLeftmost(L1);
        if L1 <> L2 then
          begin
          Stream^.Seek(Loc);
          Stream^.Read(FC, SizeOf(FC));
          FC.LeftFileCell := L2;
          Stream^.Seek(Loc);
          Stream^.Write(FC, SizeOf(FC));
          end;
        AvtTreeRemoveLeftmost := AvtRestoreLeftBalance(Loc, OldBalance);
        end
      end { AvtTreeRemoveLeftmost };

    function AvtTreeRemoveRightmost(const Loc: LongInt): LongInt;
      var
        OldBalance3: LongInt;
        L1, L2: LongInt;
      begin
      with AvtDr^ do
        begin
        Stream^.Seek(Loc);
        Stream^.Read(FC, SizeOf(FC));
        if FC.RightFileCell = 0 then
          begin
          l0 := Loc;
          AvtTreeRemoveRightmost := FC.LeftFileCell;
          Exit;
          end;
        L1 := FC.RightFileCell;
        Stream^.Seek(L1);
        Stream^.Read(FC, SizeOf(FC));
        OldBalance := FC.Flags and avtBalance;
        L2 := AvtTreeRemoveRightmost(L1);
        if L1 <> L2 then
          begin
          Stream^.Seek(Loc);
          Stream^.Read(FC, SizeOf(FC));
          FC.RightFileCell := L2;
          Stream^.Seek(Loc);
          Stream^.Write(FC, SizeOf(FC));
          end;
        AvtTreeRemoveRightmost := AvtRestoreRightBalance(Loc, OldBalance);
        end
      end { AvtTreeRemoveRightmost };

    { function AvtTreeNodeRemove(const Loc: LongInt): LongInt; }
    begin { AvtTreeNodeRemove }
    with AvtDr^ do
      begin
      AvtTreeNodeRemove := Loc;
      if Loc = 0 then
        Exit;
      Stream^.Seek(Loc);
      Stream^.Read(FC, SizeOf(FC));
      SN := AvtCellName(FC, Stream^);
      if AvtCMP(AName, SN) < 0 then
        begin
        L1 := FC.LeftFileCell;
        if L1 <> 0 then
          begin
          Stream^.Seek(L1);
          Stream^.Read(FC, SizeOf(FC));
          OldBalance := FC.Flags and avtBalance;
          L2 := AvtTreeNodeRemove(L1);
          if L1 <> L2 then
            begin
            Stream^.Seek(Loc);
            Stream^.Read(FC, SizeOf(FC));
            FC.LeftFileCell := L2;
            Stream^.Seek(Loc);
            Stream^.Write(FC, SizeOf(FC));
            end;
          AvtTreeNodeRemove := AvtRestoreLeftBalance(Loc, OldBalance);
          end;
        end
      else if AvtCMP(AName, SN) > 0 then
        begin
        L1 := FC.RightFileCell;
        if L1 <> 0 then
          begin
          Stream^.Seek(L1);
          Stream^.Read(FC, SizeOf(FC));
          OldBalance := FC.Flags and avtBalance;
          L2 := AvtTreeNodeRemove(L1);
          if L1 <> L2 then
            begin
            Stream^.Seek(Loc);
            Stream^.Read(FC, SizeOf(FC));
            FC.RightFileCell := L2;
            Stream^.Seek(Loc);
            Stream^.Write(FC, SizeOf(FC));
            end;
          AvtTreeNodeRemove := AvtRestoreRightBalance(Loc, OldBalance);
          end;
        end
      else
        begin
        if  (FC.Flags and AvtIsDir) <> 0 then
          begin
          DelDirAnswer := cmOK;
          if  (not ArvidDeleteAllFiles) then
            begin
            Dec(SkyEnabled);
            if Confirms and cfEraseSubDir = 0 then
              DelDirAnswer := cmYes
            else
              begin
              DelDirAnswer := cmCancel;
              if FC.ChildOrSize = 0 then
                DelDirAnswer := cmOK
              else
                begin
                Stream^.Seek(FC.ChildOrSize);
                Stream^.Read(FCR, SizeOf(FCR));
                if  (FCR.LeftFileCell = 0) and (FCR.RightFileCell = 0)
                then
                  DelDirAnswer := cmOK
                else
                  begin
                  if AvtCellName(FCR, Stream^) = '..' then
                    DelDirAnswer := cmOK
                  else
                    DelDirAnswer := MessageBox
                        (^C+GetString(dlDirectory)+' '+
                        CharToOemStr(Cut(AName, 40))+
                        GetString(dlEraseDirNotEmpty), nil,
                        mfConfirmation+mfNoButton+mfAllButton+
                        mf2YesButton+mfCancelButton);
                  end;
                end;
              end;
            Inc(SkyEnabled);
            ArvidDeleteAllFiles := DelDirAnswer = cmOK;
            Abort := DelDirAnswer = cmCancel;
            end;
          if not (DelDirAnswer in [cmYes, cmOK]) then
            Exit;
          AvtFreeTree(FC.ChildOrSize);
          Stream^.Seek(Loc);
          Stream^.Read(FC, SizeOf(FC));
          end;
        AvtDelFile := True;
        if FC.RightFileCell = 0 then
          AvtTreeNodeRemove := FC.LeftFileCell
        else
          case (FC.Flags and avtBalance) of
            0, $00000100:
              begin
              L1 := FC.RightFileCell;
              Stream^.Seek(L1);
              Stream^.Read(FCR, SizeOf(FCR));
              OldBalance := FCR.Flags and avtBalance;
              L2 := AvtTreeRemoveLeftmost(L1);
              Stream^.Seek(Loc);
              Stream^.Read(FC, SizeOf(FC));
              FC.RightFileCell := L2;
              Stream^.Seek(L0);
              Stream^.Read(FCL, SizeOf(FCL));
              FCL.LeftFileCell := FC.LeftFileCell;
              FCL.RightFileCell := FC.RightFileCell;
              FCL.Flags := (FCL.Flags and (not avtBalance))
                   or (FC.Flags and $00000300);
              Stream^.Seek(L0);
              Stream^.Write(FCL, SizeOf(FCL));
              AvtTreeNodeRemove := AvtRestoreRightBalance(L0, OldBalance);
              end;
            $00000300:
              begin
              L1 := FC.LeftFileCell;
              Stream^.Seek(L1);
              Stream^.Read(FCL, SizeOf(FCL));
              OldBalance := FCL.Flags and avtBalance;
              L2 := AvtTreeRemoveRightmost(L1);
              Stream^.Seek(Loc);
              Stream^.Read(FC, SizeOf(FC));
              FC.LeftFileCell := L2;
              Stream^.Seek(L0);
              Stream^.Read(FCR, SizeOf(FCR));
              FCR.LeftFileCell := FC.LeftFileCell;
              FCR.RightFileCell := FC.RightFileCell;
              FCR.Flags := (FCR.Flags and (not avtBalance))
                   or (FC.Flags and $00000300);
              Stream^.Seek(L0);
              Stream^.Write(FCR, SizeOf(FCR));
              AvtTreeNodeRemove := AvtRestoreLeftBalance(L0, OldBalance);
              end;
          end {case};
        AvtDelCell(Loc);
        end;
      end
    end { AvtTreeNodeRemove };

  { function AvtDelFile( AName: PathStr): Boolean; }
  begin { AvtDelFile }
  with AvtDr^ do
    begin
    AvtDelFile := False;
    SaveCurDir := CurDir;
    if  (filetype <> avdAvt) or (Length(AName) = 0) then
      goto 1;
    CurDir2 := CurDir;
    MakeSlash(CurDir2);
    if AName[1] <> '/' then // slash change by unxed
      begin
      AName := CurDir2+AName;
      end;
    lFSplit(AName, Dr, Nm, Xt);
    if  (Dr[1] = '/') and (Length(Dr) > 1) then // slash change by unxed
      Delete(Dr, 1, 1); {DelFC(Dr);}
    if CurDir2 <> Dr then
      begin
      CurDir := Dr;
      SeekDirectory;
      CurDir2 := CurDir;
      MakeSlash(CurDir2);
      if CurDir <> Dr then
        goto 1;
      end;
    AName := Nm+Xt;
    if Length(AName) = 0 then
      goto 1;
    Stream^.Status := stOK;
    OemToCharSt(AName);
    NewCurDirPos := AvtTreeNodeRemove(CurDirPos);
    if NewCurDirPos <> CurDirPos then
      begin
      if CurDirCellPos = 0 then
        begin
        AVT.RootDirCell := NewCurDirPos;
        Stream^.Seek(0);
        Stream^.Write(AVT, SizeOf(AVT));
        end
      else
        begin
        Stream^.Seek(CurDirCellPos);
        Stream^.Read(FC, SizeOf(FC));
        FC.ChildOrSize := NewCurDirPos;
        Stream^.Seek(CurDirCellPos);
        Stream^.Write(FC, SizeOf(FC));
        end;
      CurDirPos := NewCurDirPos;
      end;
1:
    CurDir := SaveCurDir;
    SeekDirectory;
    end
  end { AvtDelFile };

function AvtNewFile(
    AvtDr: PArvidDrive;
    AName: String;
    ADescription: String;
    AIsDir: Boolean;
    AChildOrSize: LongInt;
    ATime: LongInt;
    AStartSector: LongInt;
    AAttr: Word): Word;

  var
    SaveCurDir: String;
    CurDir2: String;
    Dr: String;
    Nm: String;
    Xt: String;
    SN: String;
    SS: String;
    FC: TAvtFileCell;
    NewCurDirPos: LongInt;
    NewCell: LongInt;
    Lv: Integer;
    NewCellFailed: Boolean;
    FailedCellIsDir: Boolean;
    CreatedCellIsDir: Boolean;
    NewCellIsDir: Boolean;
    FirstNameProcessed: Boolean;

  function AvtPutText(S: String): LongInt;
    var
      T: TAvtTextCell;
      L, L2: LongInt;
      I: Integer;
    begin
    with AvtDr^ do
      begin
      if S = '' then
        begin
        AvtPutText := 0;
        Exit;
        end;
      L := AvtGetCell(AvtDr);
      AvtPutText := L;
      while L <> 0 do
        begin
        for I := 1 to 36 do
          begin
          if Length(S) <> 0 then
            begin
            T.Data[I] := S[1];
            Delete(S, 1, 1); {DelFC(S);}
            end
          else
            T.Data[I] := #0;
          end;
        L2 := 0;
        if Length(S) <> 0 then
          L2 := AvtGetCell(AvtDr);
        T.NextTextCell := L2;
        Stream^.Seek(L);
        L := L2;
        Stream^.Write(T, SizeOf(T));
        end;
      end
    end { AvtPutText };

  procedure AvtPutName(S: String);
    var
      I: Integer;
    begin
    with AvtDr^ do
      begin
      if Length(S) < 13 then
        begin
        FC.Flags := (FC.Flags and (not AvtCellFormat)) or avtCell2;
        for I := 1 to 12 do
          begin
          if Length(S) <> 0 then
            begin
            FC.Name2[I] := S[1];
            Delete(S, 1, 1); {DelFC(S);}
            end
          else
            FC.Name2[I] := #0;
          end;
        end
      else
        begin
        FC.Flags := (FC.Flags and (not AvtCellFormat)) or avtCell3;
        FC.NamePtr := AvtPutText(S);
        end;
      end
    end { AvtPutName };

  procedure AvtPutDesc(S: String);
    begin
    with AvtDr^ do
      begin
      if Length(S) <> 0 then
        FC.DescPtr2 := AvtPutText(S)
      else
        FC.DescPtr2 := 0;
      end
    end;

  procedure AvtNewCell;
    begin
    with AvtDr^ do
      begin
      NewCell := AvtGetCell(AvtDr);
      FC.Flags := 0;
      if  (AName[1] = '/') or AIsDir then // slash change by unxed
        AAttr := AAttr or Directory
      else
        AAttr := AAttr and not Directory;
      FC.Attr := AAttr;
      FC.LeftFileCell := 0;
      FC.RightFileCell := 0;
      FC.StartSector := 0;
      FC.ChildOrSize := AChildOrSize;
      FC.Time := ATime;
      if  (AName[1] = '/') or AIsDir then // slash change by unxed
        begin
        FC.Flags := (FC.Flags and (not AvtIsDir)) or AvtIsDir;
        end;
      if  (AName = '') or (AName = '/') then // slash change by unxed
        AvtPutDesc(ADescription)
      else
        AvtPutDesc('');
      AvtPutName(SS);
      CreatedCellIsDir := True;
      if  (FC.Flags and AvtIsDir) = 0 then
        begin
        FC.StartSector := AStartSector;
        CreatedCellIsDir := False;
        end;
      Stream^.Seek(NewCell);
      Stream^.Write(FC, SizeOf(FC));
      AvtNewFile := 1;
      end
    end { AvtNewCell };

  function AvtTreeNodeInsert(APos: LongInt): LongInt;
    var
      OldBalance: LongInt;
      NewPointer: LongInt;
    begin
    with AvtDr^ do
      begin
      if APos = 0 then
        begin
        AvtNewCell;
        AvtTreeNodeInsert := NewCell;
        Exit;
        end;
      Stream^.Seek(APos);
      Stream^.Read(FCtemp, SizeOf(FCtemp));
      SN := AvtCellName(FCtemp, Stream^);
      AvtTreeNodeInsert := APos;
      if SS = SN then
        begin
        FailedCellIsDir := (FCtemp.Flags and AvtIsDir) <> 0;
        NewCellFailed := True;
        Exit;
        end;
      if AvtCMP(SS, SN) < 0 then
        begin
        if FCtemp.LeftFileCell <> 0 then
          begin
          Stream^.Seek(FCtemp.LeftFileCell);
          Stream^.Read(FCLtemp, SizeOf(FCLtemp));
          OldBalance := FCLtemp.Flags and avtBalance;
          NewPointer := AvtTreeNodeInsert(FCtemp.LeftFileCell);
          Stream^.Seek(APos);
          Stream^.Read(FCtemp, SizeOf(FCtemp));
          if NewPointer <> FCtemp.LeftFileCell then
            begin
            FCtemp.LeftFileCell := NewPointer;
            Stream^.Seek(APos);
            Stream^.Write(FCtemp, SizeOf(FCtemp));
            end;
          Stream^.Seek(FCtemp.LeftFileCell);
          Stream^.Read(FCLtemp, SizeOf(FCLtemp));
          if  ( (FCLtemp.Flags and avtBalance) <> OldBalance) and
              ( (FCLtemp.Flags and avtBalance) <> 0)
          then
            begin
            if  (FCtemp.Flags and avtBalance) = $00000300 then
              begin
              APos := AvtCellRotateRight(AvtDr, APos);
              AvtTreeNodeInsert := APos;

              Stream^.Seek(APos);
              Stream^.Read(FCtemp, SizeOf(FCtemp));
              FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
                {or $00000000};
              Stream^.Seek(APos);
              Stream^.Write(FCtemp, SizeOf(FCtemp));

              Stream^.Seek(FCtemp.RightFileCell);
              Stream^.Read(FCRtemp, SizeOf(FCRtemp));
              FCRtemp.Flags := (FCRtemp.Flags and (not avtBalance))
                {or $00000000};
              Stream^.Seek(FCtemp.RightFileCell);
              Stream^.Write(FCRtemp, SizeOf(FCRtemp));
              end
            else
              begin
              if  (FCtemp.Flags and avtBalance) = $00000100 then
                FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
                  {or $00000000}
              else
                FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
                   or $00000300;
              Stream^.Seek(APos);
              Stream^.Write(FCtemp, SizeOf(FCtemp));
              end;
            end;
          end
        else
          begin {FCtemp.LeftFileCell = 0}
          AvtNewCell;
          FCtemp.LeftFileCell := NewCell;
          if  (FCtemp.Flags and avtBalance) = $00000100 then
            FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
              {or $00000000}
          else
            FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
               or $00000300;
          Stream^.Seek(APos);
          Stream^.Write(FCtemp, SizeOf(FCtemp));
          end;
        end
      else
        begin {SS > SN}
        if FCtemp.RightFileCell <> 0 then
          begin
          Stream^.Seek(FCtemp.RightFileCell);
          Stream^.Read(FCRtemp, SizeOf(FCRtemp));
          OldBalance := FCRtemp.Flags and avtBalance;
          NewPointer := AvtTreeNodeInsert(FCtemp.RightFileCell);
          Stream^.Seek(APos);
          Stream^.Read(FCtemp, SizeOf(FCtemp));
          if NewPointer <> FCtemp.RightFileCell then
            begin
            FCtemp.RightFileCell := NewPointer;
            Stream^.Seek(APos);
            Stream^.Write(FCtemp, SizeOf(FCtemp));
            end;
          Stream^.Seek(FCtemp.RightFileCell);
          Stream^.Read(FCRtemp, SizeOf(FCRtemp));
          if  ( (FCRtemp.Flags and avtBalance) <> OldBalance) and
              ( (FCRtemp.Flags and avtBalance) <> 0)
          then
            begin
            if  (FCtemp.Flags and avtBalance) = $00000100 then
              begin
              APos := AvtCellRotateLeft(AvtDr, APos);
              AvtTreeNodeInsert := APos;

              Stream^.Seek(APos);
              Stream^.Read(FCtemp, SizeOf(FCtemp));
              FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
                {or $00000000};
              Stream^.Seek(APos);
              Stream^.Write(FCtemp, SizeOf(FCtemp));

              Stream^.Seek(FCtemp.LeftFileCell);
              Stream^.Read(FCLtemp, SizeOf(FCLtemp));
              FCLtemp.Flags := (FCLtemp.Flags and (not avtBalance))
                {or $00000000};
              Stream^.Seek(FCtemp.LeftFileCell);
              Stream^.Write(FCLtemp, SizeOf(FCLtemp));
              end
            else
              begin
              if  (FCtemp.Flags and avtBalance) = $00000300 then
                FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
                  {or $00000000}
              else
                FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
                   or $00000100;
              Stream^.Seek(APos);
              Stream^.Write(FCtemp, SizeOf(FCtemp));
              end;
            end;
          end
        else
          begin {FCtemp.RightFileCell = 0}
          AvtNewCell;
          FCtemp.RightFileCell := NewCell;
          if  (FCtemp.Flags and avtBalance) = $00000300 then
            FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
              {or $00000000}
          else
            FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
               or $00000100;
          Stream^.Seek(APos);
          Stream^.Write(FCtemp, SizeOf(FCtemp));
          end;
        end;
      end
    end { AvtTreeNodeInsert };

  procedure AvtMakeNew;
    var
      NewCurDirPos: LongInt;
      DD: TAvtFileCell;
      I: Integer;
    begin
    with AvtDr^ do
      begin
      NewCellFailed := False;
      Lv := CurLevel+1;
      Stream^.Status := stOK;
      if AName[1] = '/' then // slash change by unxed
        Delete(AName, 1, 1); {DelFC(AName);}
      while AName <> '' do
        begin
        SS := '';
        while (AName[1] <> '/') and (AName <> '') do // slash change by unxed
          begin
          AddStr(SS, AName[1]);
          Delete(AName, 1, 1); {DelFC(AName);}
          if Length(SS) = 12 then
            Break;
          end;
        if Length(SS) = 12 then
          while (AName[1] <> '/') and (AName <> '') do // slash change by unxed
            Delete(AName, 1, 1); {DelFC(AName);}
        SN := CharToOemStr(CurDir2+SS);
        if  (SS = '.') or (SS = '..') or (Pos('?', SS) > 0)
               or (Pos('*', SS) > 0)
          or (Pos(':', SS) > 0) or (SS = '')
        then
          begin
          MessageBox(GetString(dlFCNoCreateDir)+SN, nil,
             mfError+mfOKButton);
          Exit;
          end;
        NewCurDirPos := AvtTreeNodeInsert(CurDirPos);
        NewCellIsDir := (AName[1] = '/') or AIsDir; // slash change by unxed
        Delete(AName, 1, 1); {DelFC(AName);}
        if NewCellFailed then
          begin
          if NewCellIsDir <> FailedCellIsDir then
            begin
            SN := CharToOemStr(CurDir2+SS);
            if NewCellIsDir then
              MessageBox(GetString(dlFCNoCreateDir)+SN, nil,
                 mfError+mfOKButton)
            else
              MessageBox(GetString(dlleCantCreate)+SN, nil,
                 mfError+mfOKButton);
            Exit;
            end;
          end;
        if FirstNameProcessed = False then
          begin
          FirstNameProcessed := True;
          if NewCellIsDir then
            if  (CurDir[Length(CurDir)] <> '/') and (CurDir <> '') then // slash change by unxed
              CreatedDir := GetDir+'/'+SS // slash change by unxed
            else
              CreatedDir := GetDir+SS;
          end;
        if NewCurDirPos <> CurDirPos then
          begin
          if CurDirCellPos = 0 then
            begin
            AVT.RootDirCell := NewCurDirPos;
            Stream^.Seek(0);
            Stream^.Write(AVT, SizeOf(AVT));
            end
          else
            begin
            Stream^.Seek(CurDirCellPos);
            Stream^.Read(FC, SizeOf(FC));
            FC.ChildOrSize := NewCurDirPos;
            Stream^.Seek(CurDirCellPos);
            Stream^.Write(FC, SizeOf(FC));
            end;
          CurDirPos := NewCurDirPos;
          end;
        if CreatedCellIsDir then
          begin
          MakeSlash(CurDir);
          CurDir := CurDir+SS;
          CurLevel := Lv;
          Inc(Lv);
          CurDirCellPos := NewCell;
          CurDirPos := 0;
          end;
        end;
      CurDate := ATime;
      end
    end { AvtMakeNew };

  { function AvtNewFile; }
  begin { AvtNewFile }
  with AvtDr^ do
    begin
    AvtNewFile := 0;
    if  (filetype <> avdAvt) or (Length(AName) = 0) then
      Exit;
    NewCell := 0;
    CurDir2 := CurDir;
    MakeSlash(CurDir2);
    SaveCurDir := CurDir2;
    if AName[1] <> '/' then // slash change by unxed
      begin
      AName := CurDir2+AName;
      end;
    if  (AName <> '') and (AName[1] = '/') then // slash change by unxed
      Delete(AName, 1, 1); {DelFC(AName);}
    lFSplit(AName, Dr, Nm, Xt);
    Stream^.Status := stOK;
    if CurDir2 <> Dr then
      begin
      CurDir := Dr;
      SeekDirectory;
      CurDir2 := CurDir;
      MakeSlash(CurDir2);
      end;
    if CurDir2 <> '/' then // slash change by unxed
      Dr := Copy(Dr, Length(CurDir2)+1, Length(Dr));
    AName := Dr+Nm+Xt;

    FirstNameProcessed := False;
    SN := Copy(CurDir2, 1, Length(SaveCurDir));
    if SN = SaveCurDir then
      begin
      SN := Copy(CurDir2, Length(SaveCurDir)+1, Length(CurDir2));
      SS := '';
      while (SN <> '/') and (SN <> '') do // slash change by unxed
        begin
        AddStr(SS, SN[1]);
        Delete(SN, 1, 1); {DelFC(SN);}
        end;
      if SS <> '' then
        begin
        SN := CurDir;
        CurDir := SaveCurDir;
        if  (CurDir[Length(CurDir)] <> '/') and (CurDir <> '') then // slash change by unxed
          CreatedDir := GetDir+'/'+SS // slash change by unxed
        else
          CreatedDir := GetDir+SS;
        CurDir := SN;
        FirstNameProcessed := True;
        end;
      end;
    if AName <> '' then
      begin
      OemToCharSt(AName);
      OemToCharSt(ADescription);
      OemToCharSt(CurDir2);
      AvtMakeNew;
      end;
    CurDir := SaveCurDir;
    SeekDirectory;
    end
  end { AvtNewFile };

procedure AvtSeekDirectory(AvtDr: PArvidDrive);
  var
    I, J: LongInt;
    Lv: Integer;
    DD: TAvtFileCell;
    SavedCurDirPos: LongInt;
    SeekFailed: Boolean;
    S, SS, S2: String;
  begin
  with AvtDr^ do
    begin
    {  S:=Ascii_Ansi(CurDir);}
    S := CurDir;
    CurDirPos := AVT.RootDirCell;
    SavedCurDirPos := CurDirPos;
    CurDir := '';
    CurLevel := 0;
    Lv := 1;
    CurDirCellPos := 0;
    Stream^.Status := stOK;
    SeekFailed := False;
    if S[1] = '/' then // slash change by unxed
      Delete(S, 1, 1); {DelFC(S);}
    while S <> '' do
      begin
      SS := '';
      while (S[1] <> '/') and (S <> '') do // slash change by unxed
        begin
        AddStr(SS, S[1]);
        Delete(S, 1, 1); {DelFC(S);}
        {          if SS[0] = #12 then Break;}
        end;
      {      if SS[0] = #12 then while (S[1] <> '/') and (S <> '') do DelFC(S);} // slash change by unxed
      while (S[1] <> '/') and (S <> '') do // slash change by unxed
        Delete(S, 1, 1); {DelFC(S);}
      Delete(S, 1, 1); {DelFC(S);}
      while True do
        begin
        if CurDirPos = 0 then
          begin
          SeekFailed := True;
          Break;
          end;
        Stream^.Seek(CurDirPos);
        Stream^.Read(DD, SizeOf(DD));
        if Stream^.Status <> stOK then
          Break;
        S2 := AvtCellName(DD, Stream^);
        if AvtCMP(SS, S2) < 0 then
          begin
          CurDirPos := DD.LeftFileCell;
          if CurDirPos = 0 then
            SeekFailed := True;
          end
        else if AvtCMP(SS, S2) > 0 then
          begin
          CurDirPos := DD.RightFileCell;
          if CurDirPos = 0 then
            SeekFailed := True;
          end
        else
          begin
          if  (DD.Flags and AvtIsDir) = 0 then
            begin
            SeekFailed := True;
            Break;
            end;
          CurDirCellPos := CurDirPos;
          CurDirPos := DD.ChildOrSize;
          SavedCurDirPos := CurDirPos;
          Break;
          end;
        end;
      if Stream^.Status <> stOK then
        Break;
      if SeekFailed then
        begin
        CurDirPos := SavedCurDirPos;
        Break;
        end;
      MakeSlash(CurDir);
      {      CurDir:=CurDir + Ansi_Ascii(SS);}
      CurDir := CurDir+SS;
      CurLevel := Lv;
      Inc(Lv);
      end;
    CurDate := DD.Time;
    end
  end { AvtSeekDirectory };

procedure AvtGetDirectory(AvtDr: PArvidDrive; var ALocation: LongInt;
    var FC: PFilesCollection; const FileMask: String);
  var
    TAttr: Word;
    F: PFileRec;
    Cell: TAvtFileCell;
    IsDir: Boolean;
    Str: String;
  begin
  with AvtDr^ do
    begin
    if ALocation = 0 then
      Exit;
    Stream^.Status := stOK;
    Stream^.Seek(ALocation);
    if Stream^.Status <> stOK then
      Exit;
    Stream^.Read(Cell, SizeOf(Cell));
    if Stream^.Status <> stOK then
      Exit;
    AvtGetDirectory(AvtDr, Cell.LeftFileCell, FC, FileMask);
    Str := CharToOemStr(AvtCellName(Cell, Stream^));
    IsDir := Cell.Flags and AvtIsDir <> 0;
    if IsDir then
      TAttr := Cell.Attr or Directory
    else
      TAttr := Cell.Attr and not Directory;
    if
      not (ArvidWithDN and Security and (TAttr and (Hidden+SysFile) <> 0))
      and (AllFiles or IsDir or InFilter(Str, FileMask))
    then
      begin
      if IsDir then
        F := NewFileRec(Str, {$IFDEF DualName}Str, {$ENDIF}0, Cell.Time, 0,
             0, TAttr, @CurDir)
      else
        F := NewFileRec(Str, {$IFDEF DualName}Str, {$ENDIF}Cell.ChildOrSize,
             Cell.Time, 0, 0, TAttr, @CurDir);
      New(F^.DIZ);
      F^.DIZ^.Container := nil;
      F^.DIZ^.Line := ALocation;
      F^.DIZ^.DIZText := CharToOemStr(AvtCellDesc(Cell, Stream^));
      if not IsDir then
        begin
        Inc(TotFiles);
        TotLen := TotLen+Cell.ChildOrSize;
        end;
      F^.PSize := ALocation; {non standard use: location in AVT file}
      FC^.Insert(F);
      end;
    AvtGetDirectory(AvtDr, Cell.RightFileCell, FC, FileMask);
    end
  end { AvtGetDirectory };

function CopyFilesToArvid(const S: String; Files: PCollection;
     MoveMode: Boolean; Owner: Pointer): Boolean;
  var
    I, J: Integer;
    PF: PFileRec;
    PD: PDrive;
    PAD: PArvidDrive;
    S2: String;
    ToStr: String;
    OldDir: String;

  function FindDrive(P: PArvidDrive): Boolean;
    begin
    FindDrive := P^.Name^ = S2;
    end;

  begin
  CopyFilesToArvid := False;
  if Owner = nil then
    Exit;
  PD := PFilePanel(Owner)^.Drive;
  if PD^.DriveType = dtArvid then
    Exit;
  if ArvidDrives = nil then
    Exit;
  I := Pos('\AVT:', S);
  J := Pos('\TDR:', S);
  if  ( (I = 0) and (J = 0)) or
      ( (I <> 0) and (J <> 0))
  then
    Exit;
  if I = 0 then
    I := J; {TDR found}
  S2 := Copy(S, 1, I);
  Inc(I, 5);
  while (I <= Length(S)) and (S[I] <> '/') do // slash change by unxed
    begin
    AddStr(S2, S[I]);
    Inc(I);
    end;
  if J <> 0 {TDR found} then
    S2 := S2+'.TDR'
  else
    S2 := S2+'.AVT';
  PAD := ArvidDrives^.FirstThat(@FindDrive);
  if PAD = nil then
    Exit;
  CopyFilesToArvid := True;
  ToStr := Copy(S, I, 256);
  if ToStr = '' then
    ToStr := '/'; // slash change by unxed
  OldDir := PAD^.CurDir;
  PAD^.CurDir := ToStr;
  PAD^.SeekDirectory;
  PAD^.CopyFilesInto(Files, Owner, MoveMode);
  PAD^.CurDir := OldDir;
  PAD^.SeekDirectory;
  end { CopyFilesToArvid };

procedure AvtCopyFilesInto(AvtDr: PArvidDrive; AFiles: PCollection;
    Own: PView; MoveMode: Boolean);
  var
    PD: PDrive;
    I, J: Integer;
    PF: PFileRec;
    T: lText;
    CmdFileCreated: Boolean;
    P: PView;
    Dr: String;
    Nm: String;
    Xt: String;
    Desc: String;
    OldDir: String;
    From, S1, S2, S3, S4, CmdFileNam: String;

  procedure AvtWalkTree;
    var
      I: Integer;
      PC: PFilesCollection;
      Dummy: TSize;
    begin
    with AvtDr^ do
      begin
      PD^.lChDir(S2);
      PC := PFilesCollection(PD^.GetDirectory(x_x, Dummy));
      PC^.SortMode := psmLongName;  {<sort141.001>}
      PC^.Sort;
      for I := 0 to PC^.Count-1 do
        begin
        PF := PC^.At(I);
        S3 := MakeNormName(S1, PF^.FlName[True]);
        S4 := MakeNormName(S2, PF^.FlName[True]);

        Desc := '';
        if  (PF^.DIZ <> nil) and (PF^.DIZ^.DIZText <> '') then
          Desc := PF^.DIZ^.DIZText;
        if  (PF^.Attr and Directory) = 0 then
          AvtNewFile(AvtDr, S3, Desc, False, {Cat:warn}Round(PF^.Size),
             PackedDate(PF), 0, PF^.Attr)
        else
          begin
          Nm := PF^.FlName[True];
          if  (Nm <> '.') and (Nm <> '..') then
            begin
            if AvtNewFile(AvtDr, S3, Desc, True, 0, PackedDate(PF), 0,
                 PF^.Attr) <> 0
            then
              begin
              S1 := S3+'/'; // slash change by unxed
              S2 := S4+'/'; // slash change by unxed
              AvtWalkTree;
              SetLength(S1, Length(S1)-1);
              SetLength(S2, Length(S2)-1);
              while (S1 <> '') and (S1[Length(S1)] <> '/') do // slash change by unxed
                begin
                SetLength(S1, Length(S1)-1);
                SetLength(S2, Length(S2)-1);
                end;
              end;
            end;
          end;
        end;
      Dispose(PC, Done);
      end
    end { AvtWalkTree };

  begin { AvtCopyFilesInto }
  with AvtDr^ do
    begin
    if Own = nil then
      Exit;
    PD := PFilePanel(Own)^.Drive;
    if PD^.DriveType = dtArvid then
      Exit;
    From := PD^.GetRealName;
    S2 := From;
    J := 0;
    I := Pos(':', S2);
    while I > 0 do
      begin
      Inc(J);
      S2[I] := ' ';
      I := Pos(':', S2);
      end;
    if  (J > 1) or MoveMode then
      begin
      MessageBox(GetString(dlArvidNeedDisk), nil, mfError+mfOKButton);
      Exit;
      end;
    S2 := UpStrg(From);
    I := Pos(':', S2);
    if  (I <> 2) or (S2[1] < 'A') or (S2[1] > 'Z') then
      begin
      MessageBox(GetString(dlArvidNeedDisk), nil, mfError+mfOKButton);
      Exit;
      end;
    if TapeFmt <> 0 then
      begin
      lFSplit(Name^, Dr, Nm, Xt);
      CmdFileNam := MakeNormName(Dr, Nm+'.WR');
      lAssignText(T, CmdFileNam);
      lAppendText(T);
      CmdFileCreated := False;
      if IOResult <> 0 then
        begin
        lRewriteText(T);
        Writeln(T.T, 'IDENT');
        CmdFileCreated := True;
        end;
      Writeln(T.T, '');
      P := WriteMsg(GetString(dlPleaseStandBy));
      for I := 0 to AFiles^.Count-1 do
        begin
        PF := AFiles^.At(I);
        S1 := MakeNormName('/'+CurDir, PF^.FlName[True]); // slash change by unxed
        S2 := MakeNormName(From, PF^.FlName[True]);
        if  (PF^.Attr and Directory) = 0 then
          Writeln(T.T,
             'COPY '+SquashesName(S2)+' TP:'+SquashesName(S1)+' /O/R/C/H')
        else
          begin
          Writeln(T.T,


             'COPYDIR '+SquashesName(S2)+' TP:'+SquashesName(S1)+' /I/O/R/C/H')
          end;
        end;
      P^.Free;
      if CmdFileCreated then
        MessageBox(GetString(dlArvidCmdFileCreated)+CmdFileNam,
          nil, mfInformation+mfOKButton)
      else
        MessageBox(GetString(dlArvidCmdFileAppended)+CmdFileNam,
          nil, mfInformation+mfOKButton);
      Close(T.T);
      end
    else
      begin
      if filetype <> avdAvt then
        begin
        MessageBox(GetString(dlArvidVolumeIsNotTape), nil,
           mfInformation+mfOKButton);
        MessageBox(GetString(dlArvidCanChangeOnlyAVT), nil,
           mfInformation+mfOKButton);
        Exit;
        end;
      P := WriteMsg(GetString(dlPleaseStandBy));
      for I := 0 to AFiles^.Count-1 do
        begin
        PF := AFiles^.At(I);
        S1 := MakeNormName('/'+CurDir, PF^.FlName[True]); // slash change by unxed
        S2 := MakeNormName(From, PF^.FlName[True]);
        Desc := '';
        if  (PF^.DIZ <> nil) and (PF^.DIZ^.DIZText <> '') then
          Desc := PF^.DIZ^.DIZText;
        if  (PF^.Attr and Directory) = 0 then
          AvtNewFile(AvtDr, S1, Desc, False, {Cat:warn}Round(PF^.Size),
             PackedDate(PF), 0, PF^.Attr)
        else
          begin
          AvtNewFile(AvtDr, S1, Desc, True, 0, PackedDate(PF), 0,
             PF^.Attr);
          S1 := S1+'/'; // slash change by unxed
          S2 := S2+'/'; // slash change by unxed
          OldDir := PD^.GetDir;
          AvtWalkTree;
          PD^.lChDir(OldDir);
          end;
        end;
      P^.Free;
      Stream^.Seek(0);
      Stream^.Write(AVT, SizeOf(AVT));
      Dispose(Stream, Done);
      Stream := New(PBufStream, Init(Name^, stOpen, 2048));
      end;
    GlobalMessage(evCommand, cmPanelReread, nil);
    end
  end { AvtCopyFilesInto };

procedure AvtEraseFiles(AvtDr: PArvidDrive; AFiles: PCollection);
  var
    PF: PFileRec;
    I: Word;
    P: PView;
    R: Boolean;
    S: String;
  begin
  with AvtDr^ do
    begin
    if filetype <> avdAvt then
      begin
      MessageBox(GetString(dlArvidCanChangeOnlyAVT), nil,
         mfInformation+mfOKButton);
      Exit;
      end;
    if AFiles^.Count = 0 then
      Exit;
    if AFiles^.Count = 1 then
      begin
      PF := AFiles^.At(0);
      if  (PF^.Attr and Directory) <> 0
      then
        S := GetString(dlEraseConfirmDir)+Cut(PF^.FlName[True], 40)+' ?'
      else
        S := GetString(dlEraseConfirm1)+Cut(PF^.FlName[True], 40)+' ?';
      end
    else
      S := GetString(dlEraseConfirms1);
    I := MessageBox(S, nil, mfConfirmation+mfYesButton+mfNoButton
        {+mfFastButton});
    if  (I <> cmYes) then
      Exit;
    if AFiles^.Count > 1 then
      begin
      S := GetString(dlEraseConfirm2)+ItoS(AFiles^.Count)
          +' '+GetString(dlDIFiles)+' ?';
      I := MessageBox(S, nil, mfConfirmation+mfYesButton+mfNoButton);
      if  (I <> cmYes) then
        Exit;
      end;
    ArvidDeleteAllFiles := False;
    P := WriteMsg(GetString(dlPleaseStandBy));
    for I := 0 to AFiles^.Count-1 do
      begin
      PF := AFiles^.At(I);
      S := PF^.FlName[True];
      R := AvtDelFile(AvtDr, S);
      if R = False then
        MessageBox(GetString(dlErasingNoFile)+S, nil, mfError+mfOKButton);
      end;
    { AvtCheckTree(AvtDr); }
    P^.Free;
    Stream^.Seek(0);
    Stream^.Write(AVT, SizeOf(AVT));
    Dispose(Stream, Done);
    Stream := New(PBufStream, Init(Name^, stOpen, 2048));
    GlobalMessage(evCommand, cmPanelReread, nil);
    end
  end { AvtEraseFiles };

procedure AvtMakeDir(AvtDr: PArvidDrive);
  var
    S: String;
  begin
  with AvtDr^ do
    begin
    if LowMemory then
      Exit;
    if filetype <> avdAvt then
      begin
      MessageBox(GetString(dlArvidCanChangeOnlyAVT), nil,
         mfInformation+mfOKButton);
      Exit;
      end;
    S := '';
    if ExecResource(dlgMkDir, S) <> cmOK then
      Exit;
    DelLeft(S);
    DelRight(S);
    if S = '' then
      Exit;
    MakeSlash(S);
    AvtNewFile(AvtDr, S, '', True, 0, 0, 0, 0);
    { AvtCheckTree(AvtDr); }
    Stream^.Seek(0);
    Stream^.Write(AVT, SizeOf(AVT));
    Dispose(Stream, Done);
    Stream := New(PBufStream, Init(Name^, stOpen, 2048));
    GlobalMessage(evCommand, cmPanelReread, nil);
    end
  end { AvtMakeDir };

procedure AvtEditDescription(AvtDr: PArvidDrive; var S, Nam: String);
  var
    C: TAvtFileCell;
    T: TAvtTextCell;
    L1, L2, L3, L: LongInt;
    I: Word;

  function AvtPutText(S: String): LongInt;
    var
      T: TAvtTextCell;
      L, L2: LongInt;
      I: Integer;
    begin
    with AvtDr^ do
      begin
      if S = '' then
        begin
        AvtPutText := 0;
        Exit;
        end;
      L := AvtGetCell(AvtDr);
      AvtPutText := L;
      while L <> 0 do
        begin
        for I := 1 to 36 do
          begin
          if Length(S) <> 0 then
            begin
            T.Data[I] := S[1];
            Delete(S, 1, 1); {DelFC(S);}
            end
          else
            T.Data[I] := #0;
          end;
        L2 := 0;
        if Length(S) <> 0 then
          L2 := AvtGetCell(AvtDr);
        T.NextTextCell := L2;
        Stream^.Seek(L);
        L := L2;
        Stream^.Write(T, SizeOf(T));
        end;
      end
    end { AvtPutText };

  procedure AvtPutName(S: String);
    var
      I: Integer;
    begin
    with AvtDr^ do
      begin
      if Length(S) < 13 then
        begin
        C.Flags := (C.Flags and (not AvtCellFormat)) or avtCell2;
        for I := 1 to 12 do
          begin
          if Length(S) <> 0 then
            begin
            C.Name2[I] := S[1];
            Delete(S, 1, 1); {DelFC(S);}
            end
          else
            C.Name2[I] := #0;
          end;
        end
      else
        begin
        C.Flags := (C.Flags and (not AvtCellFormat)) or avtCell3;
        C.NamePtr := AvtPutText(S);
        end;
      end
    end { AvtPutName };

  begin { AvtEditDescription }
  with AvtDr^ do
    begin
    FreeStr := OemToCharStr(S);
    L1 := i32(Stream^.GetPos);
    Stream^.Read(C, SizeOf(C));
    if Stream^.Status <> stOK then
      Exit;
    case C.Flags and AvtCellFormat of
      avtCell2:
        L := C.DescPtr2;
      avtCell3:
        L := C.DescPtr3;
      else {case}
        L := 0 {avtCell0,avtCell1};
    end {case};
    while L <> 0 do
      begin
      Stream^.Seek(L);
      Stream^.Read(T, SizeOf(T));
      if Stream^.Status <> stOK then
        Exit;
      AvtFreeCell(AvtDr, L);
      L := T.NextTextCell;
      end;
    if Length(FreeStr) = 0 then
      L2 := 0
    else
      begin
      L2 := AvtGetCell(AvtDr);
      L := L2;
      while L <> 0 do
        begin
        for I := 1 to 36 do
          begin
          if Length(FreeStr) <> 0 then
            begin
            T.Data[I] := FreeStr[1];
            Delete(FreeStr, 1, 1); {DelFC(FreeStr);}
            end
          else
            T.Data[I] := #0;
          end;
        L3 := 0;
        if Length(FreeStr) <> 0 then
          L3 := AvtGetCell(AvtDr);
        T.NextTextCell := L3;
        Stream^.Seek(L);
        L := L3;
        Stream^.Write(T, SizeOf(T));
        end;
      end;
    case C.Flags and AvtCellFormat of
      avtCell0..avtCell1:
        begin
        Nam := AvtCellName(C, Stream^);
        C.Flags := (C.Flags and not AvtCellFormat) or avtCell3;
        C.NonUsed1 := 0;
        C.NonUsed2 := 0;
        C.DescPtr2 := L2;
        AvtPutName(Nam);
        end;
      avtCell2:
        C.DescPtr2 := L2;
      avtCell3:
        C.DescPtr3 := L2;
    end {case};
    Stream^.Seek(L1);
    Stream^.Write(C, SizeOf(C));
    end
  end { AvtEditDescription };

procedure AvtCalcTotal(AvtDr: PArvidDrive; const Offset: LongInt;
     var LL: TSize);
  var
    AA: TAvtFileCell;
  begin
  with AvtDr^ do
    begin
    if Offset = 0 then
      Exit;
    Stream^.Seek(Offset);
    Stream^.Read(AA, SizeOf(AA));
    if Stream^.Status <> stOK then
      Exit;
    AvtCalcTotal(AvtDr, AA.LeftFileCell, LL);
    if AA.Flags and AvtIsDir <> 0
    then
      AvtCalcTotal(AvtDr, AA.ChildOrSize, LL)
    else
      LL := LL+AA.ChildOrSize;
    AvtCalcTotal(AvtDr, AA.RightFileCell, LL);
    end
  end;

function AvtInit;
  var
    A: TAvtMediaCell;
    I: LongInt;
    J: Word;
  begin
  with AvtDr^ do
    begin
    AvtInit := False;
    filetype := avdAvt;
    Stream^.Seek(AVT.AvtMediaCell);
    Stream^.Read(A, SizeOf(A));
    if Stream^.Status <> stOK then
      Exit;
    TapeFmt := A.TapeFmt;
    if TapeFmt <> 0 then
      begin
      TapeTotalTime := A.TapeLen*60;
      PosTableOfs := A.PositionTable;
      Stream^.Seek(A.PositionTable);
      if A.PositionTableSize <> 4656 then
        begin
        Stream^.Read(I, SizeOf(I));
        if Stream^.Status <> stOK then
          Exit;
        TapeRecordedTime := I*4;
        end
      else
        begin
        Stream^.Read(J, SizeOf(J));
        if Stream^.Status <> stOK then
          Exit;
        TapeRecordedTime := J*8;
        end;
      end
    else
      begin
      TapeTotalTime := 0;
      TapeRecordedTime := 0;
      PosTableOfs := 0;
      end;
    AvtInit := True;
    end
  end { AvtInit };

end.
