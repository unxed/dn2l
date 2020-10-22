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
unit ArvidTdr;

interface

uses
  Arvid, Objects2, Streams, Advance1, Messages, DNApp, Commands, Collect,
  Views, Drivers, Startup, U_KeyMap, Advance, Lfn, Files, Dos, Tree,
  FilesCol, Advance2, Drives, FlPanel, Memory
  , Defines
  ;

procedure TdrSeekDirectory(AvtDr: PArvidDrive);
procedure TdrGetDirectory(AvtDr: PArvidDrive; var ALocation: LongInt;
    var FC: PFilesCollection; const FileMask: String);
procedure TdrEditDescription(AvtDr: PArvidDrive; var S, Nam: String;
     var PF: PFileRec);
procedure TdrCalcTotal(AvtDr: PArvidDrive; const Offset: LongInt;
     var LL: TSize);
function TdrInit(AvtDr: PArvidDrive): Boolean;
function TdrMakeFileName(S: String): String; {JO}

implementation
uses
  Archiver {TdrInit:_Cardinal} {piwamoto}
  ;

function TdrMakeFileName(S: String): String;
  var
    I: Integer;
  begin
  S[9] := '.';
  I := 8;
  while (I > 0) and (S[I] = ' ') do
    begin
    Delete(S, I, 1);
    Dec(I);
    end;
  while S[Length(S)] = ' ' do
    SetLength(S, Length(S)-1);
  if S[Length(S)] = '.' then
    SetLength(S, Length(S)-1);
  Result := S;
  end;

procedure TdrSeekDirectory;
  var
    I, J: LongInt;
    Lv: Integer;
    DD: TTdrDirCell;
    SS: String[12];
    S: String;
  begin
  with AvtDr^ do
    begin
    Stream^.Status := stOK;
    Stream^.Seek(D.DirTableOfs);
    Stream^.Read(DD, SizeOf(DD));
    CurDirPos := i32(Stream^.GetPos);
    S := CurDir;
    CurLevel := 0;
    CurDir := '';
    Lv := 1;
    if S[1] = '\' then
      Delete(S, 1, 1); {DelFC(S);}
    while S <> '' do
      begin
      SS := '';
      while (S[1] <> '\') and (S <> '') do
        begin
        SS := SS+S[1]; {AddStr(SS, S[1]);}
        Delete(S, 1, 1); {DelFC(S);}
        if Length(SS) = 12 then
          Break;
        end;
      Delete(S, 1, 1); {DelFC(S);}
      SS := Norm12(SS);
      UpStr(SS); {JO ??? for OS/2}
      {AK155: нельзя убирать! При чем тут OS/2 к Арвиду? }
      Delete(SS, 9, 1);
      repeat
        Stream^.Read(DD, SizeOf(DD));
      until (UpStrg(Copy(DD.Name, 1, 11)) = SS) and (DD.Level = Lv)
         or (DD.Level < Lv);
      if  (DD.Level < Lv) or (DD.Level = 0) then
        Break;
      Insert('.', SS, 9);
      MakeSlash(CurDir);
      CurDir := CurDir+TdrMakeFileName(SS);
      CurDirPos := i32(Stream^.GetPos);
      CurLevel := Lv;
      Inc(Lv);
      end;

    CurDate := DD.Time;
    CurFile := DD.Files;
    CurFileNum := DD.NumFiles;
    end
  end { TdrSeekDirectory };

procedure TdrGetDirectory(AvtDr: PArvidDrive; var ALocation: LongInt;
    var FC: PFilesCollection; const FileMask: String);
  var
    FF: TTdrFileCell;
    DD: TTdrDirCell;
    I: LongInt;
    J, SeekPos: LongInt;{!!s}
    F: PFileRec;

  procedure AddFile;
    var
      S: String;
      b: Byte;
    begin
    with AvtDr^ do
      begin
      S := FF.Name;
      Insert('.', S, 9);
      {    if (s[01] in [#0..#31]) or
       (s[02] in [#0..#31]) or
       (s[03] in [#0..#31]) or
       (s[04] in [#0..#31]) or
       (s[05] in [#0..#31]) or
       (s[06] in [#0..#31]) or
       (s[07] in [#0..#31]) or
       (s[08] in [#0..#31]) or
       (s[09] in [#0..#31]) or
       (s[10] in [#0..#31]) or
       (s[11] in [#0..#31]) or
       (s[12] in [#0..#31]) then exit; }
      {AK155: в ритлабовском этого не было }

      if
        not (ArvidWithDN and Security and (FF.Attr and (Hidden+SysFile) <>
             0))
        and (AllFiles or (FF.Attr and Directory <> 0) or InFilter(S,
             FileMask))
      then
        begin
        {$IFDEF DualName}
        F := NewFileRec(TdrMakeFileName(S), S, FF.Size, FF.Time, 0, 0,
             FF.Attr, @CurDir);
        {$ELSE}
        F := NewFileRec(TdrMakeFileName(S), FF.Size, FF.Time, 0, 0,
             FF.Attr, @CurDir);
        {$ENDIF}
        New(F^.DIZ);
        F^.DIZ^.Container := nil;
        F^.DIZ^.Line := SeekPos;
        if FF.Description <> 0 then
          begin
          j := i32(Stream^.GetPos);
          Stream^.Seek(D.DescTableOfs+FF.Description-1);
          {Cat:warn AnsiString}
          Stream^.Read(FreeStr, 2);
          Stream^.Read(FreeStr[1], Length(FreeStr));
          Stream^.Seek(j);
          F^.DIZ^.DIZText := FreeStr;
          end
        else
          F^.DIZ^.DIZText := '';

        if FF.Attr and Directory = 0 then
          begin
          Inc(TotFiles);
          TotLen := TotLen+FF.Size;
          end;

        FC^.Insert(F);
        end;
      end
    end { AddFile };

  begin { TdrGetDirectory }
  with AvtDr^ do
    begin
    Stream^.Seek(CurDirPos);
    repeat
      SeekPos := i32(Stream^.GetPos+2);
      Stream^.Read(DD, SizeOf(DD));
      if DD.Level = CurLevel+1 then
        begin
        Move(DD.Name, FF, SizeOf(FF));
        FF.Attr := FF.Attr or Directory;
        AddFile;
        end;
    until (DD.Level = 0) or (DD.Level <= CurLevel);

    Stream^.Status := stOK;
    Stream^.Seek(D.FileTableOfs+CurFile*SizeOf(TTdrFileCell));

    FC^.SetLimit(FC^.Count+CurFileNum);

    for I := 1 to CurFileNum do
      begin
      SeekPos := i32(Stream^.GetPos);
      Stream^.Read(FF, SizeOf(FF));
      AddFile;
      end;

    end
  end { TdrGetDirectory };

procedure TdrEditDescription;
  var
    FF: TTdrFileCell;
    I, J{!!s}: LongInt;
  procedure ExpandStream;
    var
      I, J: LongInt;
      L: Word;
      B: array[1..512] of Byte;
    begin
    with AvtDr^ do
      begin
      J := i32(Stream^.GetSize);
      repeat
        I := J-512;
        if I < D.PosTableOfs then
          I := D.PosTableOfs;
        L := J-I;
        Stream^.Seek(I);
        Stream^.Read(B, L);
        Stream^.Seek(I+512);
        Stream^.Write(B, L);
        J := I;
      until J <= D.PosTableOfs;
      FillChar(B, 512, 0);
      Stream^.Seek(I);
      Stream^.Write(B, 512);
      Inc(D.PosTableOfs, 512);
      end
    end { ExpandStream };

  begin { TdrEditDescription }
  with AvtDr^ do
    begin
    Stream^.Read(FF, SizeOf(FF));
    if Length(S) <= Length(PF^.DIZ^.DIZText) then
      begin
      if S = '' then
        begin
        FF.Description := 0;
        Stream^.Seek(PF^.DIZ^.Line);
        Stream^.Write(FF, SizeOf(FF));
        end
      else
        begin
        Stream^.Seek(D.DescTableOfs+FF.Description-1);
        I := Length(S);
        Stream^.Write(I, 2);
        Stream^.Write(S[1], I);
        end;
      end
    else
      begin
      if D.PosTableOfs-D.DescTableOfs-D.DescTableLen < 2+Length(S)
      then
        ExpandStream;
      FF.Description := D.DescTableLen+1;
      {+1 by piwamoto:new desc creation fix}
      Stream^.Seek(PF^.DIZ^.Line);
      Stream^.Write(FF, SizeOf(FF));
      Inc(D.DescTableLen, Length(S)+2);
      Stream^.Seek(D.DescTableOfs+FF.Description-1);
      I := Length(S);
      Stream^.Write(I, 2);
      Stream^.Write(S[1], I);
      end;
    end
  end { TdrEditDescription };

procedure TdrCalcTotal;
  var
    DD: TTdrDirCell;
    SPos: LongInt;{!!s}
  procedure CountDirectory(DD, Num: LongInt);
    var
      FF: TTdrFileCell;
      I: Integer;
    begin
    with AvtDr^ do
      begin
      Stream^.Status := stOK;
      Stream^.Seek(D.FileTableOfs+DD*SizeOf(TTdrFileCell));
      for I := 1 to Num do
        begin
        Stream^.Read(FF, SizeOf(FF));
        LL := LL+FF.Size;
        end;
      end
    end;
  begin
  with AvtDr^ do
    begin
    SPos := CurDirPos;
    repeat
      Stream^.Seek(SPos);
      Stream^.Read(DD, SizeOf(DD));
      SPos := i32(Stream^.GetPos);
      if DD.Level > CurLevel then
        CountDirectory(DD.Files, DD.NumFiles);
    until (DD.Level = 0) or (DD.Level <= CurLevel);
    CountDirectory(CurFile, CurFileNum);
    end
  end { TdrCalcTotal };

function TdrInit;
  var
    J: Word;
  begin
  with AvtDr^ do
    begin
    TdrInit := False;
    filetype := avdTdr;
    Stream^.Seek(0);
    Stream^.Read(D, SizeOf(D));
    if  (Stream^.Status <> stOK) or
        (_Cardinal(D.PosTableOfs) > Stream^.GetSize)
    then
      Exit; {piwamoto: reject invalid TDRs}
    TapeFmt := D.TapeFmt;
    TapeTotalTime := D.TapeLen;
    PosTableOfs := D.PosTableOfs;
    Stream^.Seek(D.PosTableOfs);
    Stream^.Read(J, SizeOf(J));
    {  if Stream^.Status <> stOK then exit;} {commented by piwamoto}
    TapeRecordedTime := J*8;
    TdrInit := True;
    end
  end { TdrInit };

end.
