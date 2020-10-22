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

unit UniWin;

interface

uses
  Views
  ;

type
  { TUniWindow }

  PUniWindow = ^TUniWindow;
  TUniWindow = object(TWindow)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    function GetPalette: PPalette; virtual;
    function MakeScrollBar(AOptions: Word): PScrollBar;
    procedure InitFrame; virtual;
    function ReactOnCmd: Boolean; virtual;
    end;

  { TEditScrollBar }

  PEditScrollBar = ^TEditScrollBar;
  TEditScrollBar = object(TScrollBar)
    function GetPalette: PPalette; virtual;
    end;

  { TEditFrame }

  PEditFrame = ^TEditFrame;
  TEditFrame = object(TFrame)
    function GetPalette: PPalette; virtual;
    end;

const
  CUniWindow = #70#64#65#66#67#68#69#71#72#73#74#75#76#77#78#164#182#183+
  #184#185#189#190#191#197#198;

implementation
uses
  DnIni, Drivers, Defines;

function TUniWindow.GetPalette;
  const
    p: String[Length(CUniWindow)] = CUniWindow;
  begin
  GetPalette := @P;
  end;

function TEditFrame.GetPalette;
  const
    p: String[9] = #1#1#8#9#10#10#10#9#15;
  begin
  GetPalette := @P;
  end;

function TEditScrollBar.GetPalette;
  const
    p: String[3] = #11#12#12;
  begin
  GetPalette := @P;
  end;

procedure TUniWindow.InitFrame;
  var
    R: TRect;
  begin
  R.Assign(0, 0, Size.X, Size.Y);
  Frame := New(PEditFrame, Init(R));
  end;

function TUniWindow.MakeScrollBar;
  var
    P: PEditScrollBar;
    R: TRect;
  begin
  GetExtent(R);
  if AOptions and sbVertical <> 0
  then
    begin
    Inc(R.A.Y);
    Dec(R.B.Y);
    R.A.X := R.B.X-1;
    end
  else
    begin
    if FastBookmark
    then
      Inc(R.A.X, 52)
    else
      Inc(R.A.X, 39);
    Dec(R.B.X, 2);
    R.A.Y := R.B.Y-1;
    end;
  New(P, Init(R));
  if AOptions and sbVertical = 0 then
    P^.GrowMode := gfGrowLoY+gfGrowHiY+gfGrowHiX
  else {p^.GrowMode:=gfGrowHiY+gfGrowHiX}
    ;
  P^.Options := P^.Options or ofPostProcess;
  Insert(P);
  MakeScrollBar := P;
  end { TUniWindow.MakeScrollBar };

function TUniWindow.ReactOnCmd;
  begin
  ReactOnCmd := True
  end;

end.
