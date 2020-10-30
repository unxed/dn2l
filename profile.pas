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
unit profile;

{ unit Profile, Version 1.01.001, Copyright 1994,1997 by Matthias K"oppe

  $Id: profile.pas 1.3 1999/02/09 11:15:29 mkoeppe Exp $

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free
  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$G+,X+}

interface

function GetPrivateProfileInt(ApplicationName, KeyName: PChar;
    Default: Integer; FileName: PChar): Word;
function GetPrivateProfileString(ApplicationName, KeyName: PChar;
    Default: PChar; ReturnedString: PChar; Size: Integer;
    FileName: PChar): Integer;
function WritePrivateProfileString(ApplicationName, KeyName, Str,
    FileName: PChar): Boolean;
procedure CloseProfile;

implementation

uses
  Objects, Strings, Streams, Advance1
  ;

{ The most expensive operation with buffered streams is seeking --
  especially seeking relatively since both GetPos and Seek call the
  dos move function, which takes much time.
  SeekRel provides a buffered seeking operation, which calls no DOS
  function if in buffer and one DOS function if out of buffer.
}
type
  PModBufStream = ^TModBufStream;
  TModBufStream = object(TBufStream)
    procedure SeekRel(Delta: Integer);
    end;

procedure TModBufStream.SeekRel;
  begin
  Seek(GetPos+Delta);
  end; {DataCompBoy}

{ Current parameters
}
const
  CurFile: PModBufStream = nil;
  CurFileName: PChar = nil;
  CurMode: Longint = 0;
  CurApp: LongInt = 0;
  CurAppName: PChar = nil;

procedure CloseFile;
  begin
  if CurFile <> nil then
    begin
    Dispose(CurFile, Done);
    CurFile := nil
    end;
  StrDispose(CurFileName);
  CurFileName := nil;
  CurApp := 0;
  StrDispose(CurAppName);
  CurAppName := nil
  end;

function OpenFile(FileName: PChar; Mode: Longint): Boolean;
  begin
  Result := True;
  if  (CurFileName = nil) or (FileName = nil) or
      (StrIComp(CurFileName, FileName) <> 0) or
      (Mode and not CurMode <> 0) { т.е. смена с stOpenRead на stOpen }
  then
    begin
    CloseFile;
    if FileName = nil then
      begin
      Result := False;
      Exit;
      end;
    CurFileName := StrNew(FileName);
    CurFile := New(PModBufStream, Init(StrPas(FileName), Mode, 4096));
    Result := (CurFile <> nil) and (CurFile^.Status = 0);
    CurMode := Mode;
    if not Result then
      CloseFile;
    end;
  end;

function CreateFile(FileName: PChar): Boolean;
  var
    Res: Boolean;
  begin
  CreateFile := False;
  if FileName = nil then
    Exit;
  CurFileName := StrNew(FileName);
  CurFile := New(PModBufStream, Init(StrPas(FileName), stCreate, 4096));
  Res := (CurFile <> nil) and (CurFile^.Status = 0);
  if not Res then
    CloseFile;
  CreateFile := Res
  end;

procedure ReadLine(Buf: PChar);
  var
    c: Char;
    c2: Char; {DataCompBoy}
    Count: Word;
  begin
  Count := 0;
  with CurFile^ do
    begin
    repeat
      Read(Buf[0], 1);
      c := Buf[0];
      Inc(Buf);
      if Count < 256 then
        Inc(Count);
    until (c in [#13, #10]) or (Status <> 0);
    {-DataCompBoy-}
    if Status = 0 then
      begin
      if  (c in [#13, #10]) and (Status = 0) then
        Read(c2, 1);
      if not (c2 in [#13, #10]) or (c2 = c) then
        SeekRel(-1)
      end;
    {-DataCompBoy-}
      (Buf-1)[0] := #0;
    end
  end { ReadLine };

function IsAppLine(Buf: PChar): Boolean;
  begin
  IsAppLine := (Buf[0] = '[') and ((StrEnd(Buf)-1)[0] = ']')
  end;

function FindApplication(AppName: PChar): Boolean;
  var
    Buf: array[0..255] of Char;
  begin
  FindApplication := False;
  if AppName = nil then
    Exit;
  if  (CurAppName <> nil) and (StrIComp(CurAppName, AppName) = 0)
  then
    begin
    CurFile^.Seek(CurApp);
    FindApplication := True;
    end
  else
    begin
    CurFile^.Seek(0);
    repeat
      ReadLine(Buf);
      if IsAppLine(Buf) then
        begin
          (StrEnd(Buf)-1)[0] := #0;
        StrDispose(CurAppName);
        CurAppName := StrNew(Buf+1);
        CurApp := i32(CurFile^.GetPos);
        // fixme: AppName sometimes becomes nil on linux build
        // and our StrIComp may have no nil check
        //if  (CurAppName <> nil) and (StrIComp(CurAppName, AppName) = 0)
        if  (CurAppName <> nil) and (AppName <> nil) and (StrIComp(CurAppName, AppName) = 0)
        then
          begin
          FindApplication := True;
          CurFile^.Reset;
          CurApp := i32(CurFile^.GetPos);
          Exit
          end
        end
    until CurFile^.Status <> 0;
    CurFile^.Reset;
    end
  end { FindApplication };

procedure AddApplication(AppName: PChar);
  const
    _L: array[0..2] of Char = #13#10'[';
    _R: array[0..2] of Char = ']'#13#10;
  begin
  with CurFile^ do
    begin
    Seek(GetSize);
    Write(_L, 3);
    Write(AppName[0], StrLen(AppName));
    Write(_R, 3);
    StrDispose(CurAppName);
    CurAppName := StrNew(AppName);
    CurApp := i32(CurFile^.GetPos)
    end
  end;

function FirstInsignificant(Str: PChar): PChar;
  var
    P: PChar;
  begin
  P := StrEnd(Str);
  if P = Str
  then
    FirstInsignificant := Str
  else
    begin
    repeat
      Dec(P);
    until P[0] > ' ';
    FirstInsignificant := P+1
    end
  end;

function FindKey(KeyName: PChar; Dest: PChar): Boolean;
  var
    Buf: array[0..255] of Char;
    P: PChar;
    pos: LongInt;
  begin
  FindKey := False;
  if KeyName = nil then
    Exit;
  repeat
    pos := i32(CurFile^.GetPos);
    ReadLine(Buf);
    P := StrScan(Buf, '=');
    if P <> nil then
      begin
      P[0] := #0;
      FirstInsignificant(Buf)[0] := #0;
      if StrIComp(Buf, KeyName) = 0 then
        begin
        CurFile^.Reset;
        if Dest = nil
        then
          CurFile^.Seek(pos)
        else
          StrCopy(Dest, P+1);
        FindKey := True;
        Exit
        end;
      end;
  until IsAppLine(Buf) or (CurFile^.Status <> 0);
  CurFile^.Reset;
  end { FindKey };

procedure DeleteBuf(Dest, Source: LongInt);
  var
    p, Count: LongInt;
    Buf: array[0..255] of Char;
  begin
  p := Dest;
  repeat
    if CurFile^.GetSize-Source >= 256
    then
      Count := 256
    else
      Count := i32(CurFile^.GetSize)-Source;
    CurFile^.Seek(Source);
    CurFile^.Read(Buf, Count);
    CurFile^.Seek(Dest);
    CurFile^.Write(Buf, Count);
    Inc(Source, Count);
    Inc(Dest, Count);
  until Source = CurFile^.GetSize;
  CurFile^.Truncate;
  CurFile^.Seek(p)
  end;

procedure DeleteLine;
  var
    pos: LongInt;
    Buf: array[0..255] of Char;
  begin
  pos := i32(CurFile^.GetPos);
  ReadLine(Buf);
  CurFile^.Reset;
  DeleteBuf(pos, i32(CurFile^.GetPos));
  end;

procedure InsertLine(Size: Word);
  var
    pos, Count, Source, Dest: LongInt;
    Buf: array[0..255] of Char;
  begin
  pos := i32(CurFile^.GetPos);
  Source := Round(CurFile^.GetSize);
  Dest := Source+Size;
  repeat
    if Source-pos >= 256
    then
      Count := 256
    else
      Count := Source-pos;
    Dec(Source, Count);
    Dec(Dest, Count);
    CurFile^.Seek(Source);
    CurFile^.Read(Buf, Count);
    CurFile^.Seek(Dest);
    CurFile^.Write(Buf, Count);
  until Source = pos;
  CurFile^.Seek(pos)
  end { InsertLine };

function InQuotes(Str: PChar): Boolean;
  var
    P: PChar;
  begin
  P := StrEnd(Str)-1;
  InQuotes :=
      ( (Str[0] = '"') and (P[0] = '"')) or
      ( (Str[0] = '''') and (P[0] = ''''))
  end;

function GetPrivateProfileString;
  var
    Buf: array[0..255] of Char;
    P, Copy: PChar;
    Res: Boolean;
  begin
  Copy := Default;
  if OpenFile(FileName, stOpenRead) and
    FindApplication(ApplicationName)
  then
    if KeyName = nil
    then
      begin
      { list all keys in section }
      Copy := ReturnedString;
      repeat
        ReadLine(Buf);
        Res := IsAppLine(Buf);
        if not Res and (Buf[0] <> ';') then
          begin
          P := StrScan(Buf, '=');
          if P <> nil then
            begin
            P[0] := #0;
            FirstInsignificant(Buf)[0] := #0;
            Copy := StrEnd(StrLCopy(Copy, Buf,
                   Size-(Copy-ReturnedString)-1))+1
            end
          end
      until Res or (CurFile^.Status <> 0);
      CurFile^.Reset;
      Copy[0] := #0;
      GetPrivateProfileString := Copy-ReturnedString-1;
      Exit
      end
    else if FindKey(KeyName, Buf) then
      if InQuotes(Buf)
      then
        begin
          (StrEnd(Buf)-1)[0] := #0;
        Copy := Buf+1
        end
      else
        Copy := @Buf;
  StrLCopy(ReturnedString, Copy, Size);
  GetPrivateProfileString := StrLen(ReturnedString)
  end { GetPrivateProfileString };

function GetInt(Str: PChar): Word;
  var
    Res: Word;
    E: Integer;
  begin
  { auch Hex erkennen (C-Format) }
  Val(Str, Res, E);
  if E = 1 then
    Res := 0
  else if E <> 0 then
    begin
    Str[E-1] := #0;
    Val(Str, Res, E)
    end;
  GetInt := Res
  end;

function GetPrivateProfileInt;
  var
    Buf: array[0..255] of Char;
  begin
  GetPrivateProfileInt := Default;
  if OpenFile(FileName, stOpenRead) and
    FindApplication(ApplicationName) and FindKey(KeyName, Buf)
  then
    GetPrivateProfileInt := GetInt(Buf);
  end;

function WritePrivateProfileString;
  var
    Buf: array[0..255] of Char;
    Res: Boolean;
    p: LongInt;
  begin
  if  (OpenFile(FileName, stOpen) or CreateFile(FileName))
       and (ApplicationName <> nil)
  then
    begin
    if not FindApplication(ApplicationName)
    then
      AddApplication(ApplicationName);
    if KeyName = nil
    then
      begin
      CurFile^.Seek(CurApp);
      repeat
        p := i32(CurFile^.GetPos);
        ReadLine(Buf);
        Res := IsAppLine(Buf) or (CurFile^.Status <> 0);
        if not Res and (Buf[0] <> ';') then
          DeleteBuf(p, i32(CurFile^.GetPos));
      until Res;
      CurFile^.Reset;
      end
    else
      begin
      if FindKey(KeyName, nil) then
        DeleteLine
      else
        CurFile^.Seek(CurApp);
      if Str <> nil then
        begin
        {
        StrLCopy(Buf, KeyName, 256);
        StrLCat(Buf, '=', 256);
        StrLCat(Buf, Str, 256);
        StrLCat(Buf, #13#10, 256);
        InsertLine(StrLen(Buf));
        CurFile^.Write(Buf, StrLen(Buf))
        }
        // fixme: commented by unxed
        // currently DN writes faulty .INI file with strings > 260 bytes in size
        // full of $00 chars, and faults to start after it
        // so let this part be commented until we fix this issue
        // two problems, actually:
        // 1) fatal error if string in ini file > 260 bytes
        // 2) generation of such strings on ini save
        end
      end
    end
  end { WritePrivateProfileString };

procedure CloseProfile;
  begin
  CloseFile
  end;

end.
