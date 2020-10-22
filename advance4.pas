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

unit Advance4; {OS/2 support}

interface

type
  SessionType = (stOS2SYS, stOS2FullScreen, stOS2Windowed, stPMSession,
    stVDMFullScreen, stWinFullScreen, stWinWindow, stVDMWindow);

procedure RunSession(Command: String; Bckg: Boolean;
     Session: SessionType);

implementation

uses
  Advance, Lfn, Advance1, Drivers
  , Dos, DnExec, Startup
  ;

{-DataCompBoy-}
procedure RunSession;
  var
    T: lText;
    I: Integer;
    S, M, EX: String;
    CmdExt1: String[4];
  begin
  if not (OS2exec or Win32exec) then
    Exit;
  I := 1;
  repeat
    ClrIO;
    CmdExt1 := CmdExt;
   {$IFDEF DPMI32}
    if OS2exec then CmdExt1 := '.CMD';
   {$ENDIF}
    EX := SwpDir+'$DN'+ItoS(I)+'$'+CmdExt1;
    lAssignText(T, EX);
    FileMode := $40;
    lResetText(T);
    if IOResult <> 0 then
      Break;
    Close(T.T);
    if InOutRes = 0 then
      Inc(I);
  until IOResult <> 0;
  ClrIO;
  lAssignText(T, EX);
  lRewriteText(T);
  lGetDir(0, S);
  Writeln(T.T, '@'+Copy(S, 1, 2));
  Writeln(T.T, '@cd "'+S+'"');
  S := Command;
  if PosChar(';', S) > 0 then
    begin
    Replace(';;', #0, S);
    while (S <> '') and (PosChar(';', S) <> 0) do
      begin
      I := PosChar(';', S);
      M := Copy(S, 1, I-1);
      Replace(#0, ';', M);
      Writeln(T.T, M);
      Delete(S, 1, I);
      end;
    Replace(#0, ';', S);
    Writeln(T.T, S);
    end
  else
    Writeln(T.T, Command);
  if not Bckg and (ShiftState and $20 = 0) then
    Writeln(T.T, '@pause');
  if OS2exec or (opSys and opWNT <> 0) then
    Write(T.T, '@del "'+EX+'" & exit'^Z)
  else
    Write(T.T, '@del "'+EX+'"'^Z);
  Close(T.T);
 {$IFDEF OS2}
  if Session = stOS2FullScreen then
    M := 'START "'+Command+'" /FS '+EX
  else
    M := 'START "'+Command+'" /WIN '+EX;
 {$ELSE}
  if (opSys and opWNT <> 0) then
    M := 'START "'+Command+'" '+EX
  else
    {$IFDEF DPMI32}
    if OS2exec then
      begin
      if Session = stOS2FullScreen then
        M := 'HSTART "'+Command+'" /C /FS '+EX
      else
        M := 'HSTART "'+Command+'" /C /WIN '+EX;
      end
    else
    {$ENDIF}
      M := 'START '+GetEnv('COMSPEC')+' /C '+EX;
 {$ENDIF}
  ExecString(M, '');
  end { RunSession };
{DataCompBoy}
end.
