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
unit TitleSet;
interface

procedure SetTitle(Text: String);

implementation

{$IFDEF OS2}
uses
  Dn2PmApi
  ;

var
  CurrentTitle: String;

procedure SetTitle(Text: String);
  begin
  Text := 'DN/2: '+Text;
  if Text = CurrentTitle then
    Exit;
  CurrentTitle := Text;

  DN_XSetTitle(Text);
  end;

begin
{$ENDIF}
{$IFDEF WIN32}
uses
  Windows,
  VpSysLow, UKeyMap
  ;

var
  CurrentTitle: String;

procedure SetTitle(Text: String);
  begin
  Text := 'DN/2: '+Text;
  if Text = CurrentTitle then
    Exit;
  CurrentTitle := Text;

  Text := Text+#0;
  {AK155 Под Win NT заголовок окна отображается в кодировке OEM,
  а в Win 9x - в кодировке ANSI. }
  if  (SysPlatformId = 1 {Win 9x}) then
    OemToCharSt(Text);
  SetConsoleTitle(@Text[1]);
  end;
{$ENDIF}
{$IFDEF DPMI32}
procedure SetTitle(Text: String);
  begin
  {Cat:todo DPMI32}
  end;
{$ENDIF}
{$IFDEF LINUX}
procedure SetTitle(Text: String);
  begin
    // stub by unxed
  end;
{$ENDIF}

end.
