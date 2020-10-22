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
unit Advance7;

interface

uses
  Advance, Advance2
  ;

function ValidLngId(LI: String; CheckForHelp: Boolean): Boolean;
function HelpLngId: String;
function LngId: String;

implementation

uses Dos, lfn, DnIni, DnIni_p;

function ValidLngId(LI: String; CheckForHelp: Boolean): Boolean;
  var
    S1, S2: String;
  begin
  ValidLngId := False;
  S1 := GetEnv('DNDLG');
  if S1 = '' then
    S1 := SourceDir;
  if not (S1[Length(S1)] in ['\', '/']) then
    S1 := S1+'\';
  S2 := StartupDir;
  if not (S2[Length(S2)] in ['\', '/']) then
    S2 := S2+'\';
  if  (not CheckForHelp) and (not ExistFile(S1+LI+'.DLG')) and
      (not ExistFile(S2+LI+'.DLG'))
  then
    Exit;
  if  (not CheckForHelp) and (not ExistFile(S1+LI+'.LNG')) and
      (not ExistFile(S2+LI+'.LNG'))
  then
    Exit;
  if CheckForHelp then
    begin
    S1 := SourceDir;
    if not (S1[Length(S1)] in ['\', '/']) then
      S1 := S1+'\';
    if  (not ExistFile(S1+LI+'.HLP')) and (not ExistFile(S2+LI+'.HLP'))
    then
      Exit
    end;
  ValidLngId := True;
  end { ValidLngId };

function HelpLngId: String;
  var
    S: String;
  begin
  S := HelpLanguageOverride;
  if not ValidLngId(S, True) then
    S := ActiveLanguage;
  if not ValidLngId(S, True) then
    S := GetEnv('DNLNG');
  if not ValidLngId(S, True) then
    S := 'English';
  HelpLngId := S
  end;

function LngId: String;
  var
    SR: lSearchRec;
    S: String;
  begin
  S := ActiveLanguage;
  if not ValidLngId(S, False) then
    S := GetEnv('DNLNG');
  if not ValidLngId(S, False) then
    S := 'English';
  if not ValidLngId(S, False) then
    begin
     lFindFirst(StartupDir+'*.LNG', AnyFileDir - Directory, SR);
     S := SR.FullName;
     if S <> '' then
       SetLength(S, Length(s)-4);
     lFindClose(SR);
    end;
  if not ValidLngId(ActiveLanguage, False) then
    begin
      ActiveLanguage := S;
      SaveDnIniSettings ( @ActiveLanguage );
    end;
  LngId := S
  end;

end.
