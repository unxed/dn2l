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
unit ArchRead;

interface

procedure ReadArcList;

implementation

uses
  Archiver, FStorage, Dos, ArcView, Advance, Advance1, Advance2, Messages,
  DNApp, Commands, Lfn, Views
  ;

{-DataCompBoy-}
procedure ReadArcList; {changed & AIN added by piwamoto}
  const
    FuckName = 'U$~RESLT.OK';
  var
    S, CurDir, ID: String;
    P: PArcFile;
    PC: PDirStorage;
    F: PTextReader;
    DT: DateTime;
    I, J: Integer;
    Drv: PArcDrive;
  label 1, 2, 3;
  begin
  S := TempFile;
  ID := Copy(S, 1, 4);
  Delete(S, 1, 4);
  I := PosChar(']', S);
  ArcFileName := Copy(S, I+1, MaxStringLength);
  SetLength(S, I-1);
  ClrIO;
  TempFile := '';
  if (Pos(ID, 'UC2:AIN:7Z!:')+3) mod 4 = 0 then
    begin
    if ID = 'UC2:' then
      begin
      if not ExistFile(FuckName) then
        begin
        MessageBox(GetString(dlArcMsg6), nil, mfOKButton or mfError);
        Exit;
        end;
      EraseFile(FuckName);
      lGetDir(0, CurDir); {GetDir(0, CurDir);}
      {Cat}
      GlobalMessage(evCommand, cmRereadDir, @CurDir);
      F := New(PTextReader, Init(S));
      if F = nil then
        Exit;
      P := nil;
      New(PC, Init);
      while not F^.Eof do
        begin
        S := F^.GetStr;
        DelLeft(S);
        DelRight(S);
        ID := Copy(S, 1, 4);
        if ID = 'LIST' then
          begin
          I := PosChar('[', S);
          if I = 0 then
            Continue;
          Delete(S, 1, I);
          I := PosChar(']', S);
          if I < 2 then
            Continue;
          SetLength(S, I-1);
          CurDir := S;
          end
        else if (ID = 'FILE') or (ID = 'DIR') then
          begin
          if P <> nil then
            begin
            PackTime(DT, P^.Date);
            PC^.AddFile(P^.FName^, P^.USize, P^.PSize, P^.Date, P^.Attr);
            DisposeStr(P^.FName);
            Dispose(P);
            end;
          New(P);
          P^.FName := nil;
          if ID = 'DIR' then
            begin
            P^.Attr := Directory;
            P^.PSize := 0;
            P^.USize := 0
            end
          else
            P^.Attr := 0;
          end
        else if ID = 'END' then
          begin
          if P <> nil then
            begin
            PackTime(DT, P^.Date);
            PC^.AddFile(P^.FName^, P^.USize, P^.PSize, P^.Date, P^.Attr);
            DisposeStr(P^.FName);
            Dispose(P);
            end;
          Break
          end
        else
          begin
          I := PosChar('=', S);
          if I = 0 then
            goto 1;
          Delete(S, 1, I);
          if ID = 'NAME' then
            begin
            I := PosChar('[', S);
            if I = 0 then
              Continue;
            Delete(S, 1, I);
            I := PosChar(']', S);
            if I < 2 then
              Continue;
            SetLength(S, I-1);
            if P^.Attr = Directory then
              S := S+'\';
            P^.Attr := 0;
            P^.FName := NewStr(CurDir+S);
            end
          else if ID = 'DATE' then
            begin
            DT.Month := StoI(Copy(S, 1, 2));
            DT.Day := StoI(Copy(S, 4, 2));
            DT.Year := StoI(Copy(S, 7, 4));
            end
          else if ID = 'TIME' then
            begin
            DT.Hour := StoI(Copy(S, 1, 2));
            DT.Min := StoI(Copy(S, 4, 2));
            DT.Sec := StoI(Copy(S, 7, 4));
            end
          else if ID = 'ATTR' then
            begin
            end
          else if ID = 'SIZE' then
            begin
            P^.USize := StoI(S);
            P^.PSize := StoI(S);
            end
          else if ID = 'VERS' then
            begin
            end;
          end;
        end;
1:
      ID := '';
      end;
    if ID = 'AIN:' then
      begin
      F := New(PTextReader, Init(S));
      if F = nil then
        Exit;
      P := nil;
      New(PC, Init);
      repeat
        S := F^.GetStr;
        ID := Copy(S, 9, 2);
        I := PosChar('%', S);
      until ((ID = ': ') and (I > 50)) or (F^.Eof);
      I := PosChar(',', S);
      if  (I < 12) or (F^.Eof) then
        goto 2;
      ID := Copy(S, 11, I-11);
      Val(ID, J, I);
      for I := 1 to 4 do
        S := F^.GetStr;
      while (J <> 0) and (not F^.Eof) do
        begin
        S := F^.GetStr;
        DelLeft(S);
        DelRight(S);
        New(P);
        P^.Attr := 0;
        I := PosChar(' ', S);
        if I = 0 then
          begin
          P^.FName := NewStr('\'+S);
          S := F^.GetStr;
          DelLeft(S);
          DelRight(S);
          end
        else
          begin
          P^.FName := NewStr('\'+Copy(S, 1, I-1));
          Delete(S, 1, I);
          DelLeft(S);
          end;
        I := PosChar(' ', S);
        P^.USize := StoI(Copy(S, 1, I-1));
        P^.PSize := StoI(Copy(S, 1, I-1));
        Delete(S, 1, I);
        DelLeft(S);
        DT.Month := StoI(Copy(S, 1, 2));
        DT.Day := StoI(Copy(S, 4, 2));
        DT.Year := 1900 + StoI(fDelRight(Copy(S,7,3)));
        Delete(S, 1, 10);
        DelLeft(S);
        DT.Hour := StoI(Copy(S, 1, 2));
        DT.Min := StoI(Copy(S, 4, 2));
        DT.Sec := StoI(Copy(S, 7, 2));
        PackTime(DT, P^.Date);
        PC^.AddFile(P^.FName^, P^.USize, P^.PSize, P^.Date, P^.Attr);
        DisposeStr(P^.FName);
        Dispose(P);
        P := nil;
        J := J-1;
        end;
2:
      ID := '';
      end;
    if ID = '7Z!:' then
      begin {piwamoto}
        F := New(PTextReader, Init(S));
        if F = nil then Exit;
        P := nil;
        New(PC, Init);
        Repeat
         S := F^.GetStr;
        Until (S[1] = '-') or (F^.EOF);
        if F^.EOF then Goto 3;
        repeat
         S := F^.GetStr;
         if (S[1] = '-') or (Length(S) < 54) then Goto 3;
         New(P);
         DT.Year := StoI(Copy(S,1,4));
         DT.Month := StoI(Copy(S,6,2));
         DT.Day := StoI(Copy(S,9,2));
         DT.Hour := StoI(Copy(S,12,2));
         DT.Min := StoI(Copy(S,15,2));
         DT.Sec := StoI(Copy(S,18,2));
         PackTime(DT, P^.Date);
         P^.USize := StoI(fDelLeft(Copy(S,27,12)));
         P^.PSize := StoI(fDelLeft(Copy(S,40,12)));
         if S[21] = 'D' {directory}
           then begin
            P^.Attr := Directory;
            S := S + '\';
           end
           else P^.Attr := 0;
         P^.FName := NewStr('\'+fDelRight(Copy(S, 54, 255)));
         PC^.AddFile(P^.FName^, P^.USize, P^.PSize, P^.Date, P^.Attr);
         DisposeStr(P^.FName);
         Dispose(P);
         P:=nil;
        until F^.EOF;
      3:
      ID := '';
      end;
    {next archive}

    S := F^.FileName;
    Dispose(F, Done);
    EraseFile(S);
    GlobalMessage(evCommand, cmRereadDir, @TempDir);
    if PC^.Files = 0 then
      Dispose(PC, Done)
    else
      begin
      New(Drv, InitCol(PC, ArcFileName, VArcFileName));
      if Message(Application, evBroadcast, cmFindForced, Drv) = nil then
        if Message(Application, evCommand, cmInsertDrive, Drv) = nil
        then
          begin
          end;
      end;
    end;
  end { ReadArcList };
{-DataCompBoy-}

end.
