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

unit SWE;

interface

uses
  Drivers, Defines, Streams, Views, Dialogs
  ;

type
   { строка быстрого переименования (Alt-F6). Она исполняется прямо
   в менедждере, без объемлющего диалога, поэтому имеет свой метод
   Execute. Цвета палитры (C) заносятся в CM_RenameSingleL}
  PInputFName = ^TInputFName;
  TInputFName = object(TInputLine)
    EndView: Word;
    function Execute: Word; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PColorPoint = ^TColorPoint;
  TColorPoint = object(TView)
    Color: Byte;
    constructor Init(var ABounds: TRect; AColor: Byte);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); virtual;
    procedure Draw; virtual;
    end;

implementation

uses
  Advance, Commands, DNApp
  ;

function TInputFName.Execute: Word;
  var
    Event: TEvent;
  begin
  EndView := 0;
  repeat
    Owner^.GetEvent(Event);
    if Event.What = evNothing then
      TinySlice;
    HandleEvent(Event);
  until EndView <> 0;
  Result := EndView;
  end;

procedure TInputFName.HandleEvent;
  begin
  case Event.What of
    evKeyDown:
      begin
      case Event.KeyCode of
        kbEnter:
          begin
          EndView := cmOK;
          ClearEvent(Event);
          Exit;
          end;
        kbESC:
          begin
          EndView := cmCancel;
          ClearEvent(Event);
          Exit;
          end;
        end {case};
      end;
  end {case};
  if  (Event.What <> evNothing) then
    inherited HandleEvent(Event);
  end { TInputFName.HandleEvent };

constructor TColorPoint.Init(var ABounds: TRect; AColor: Byte);
  begin
  ABounds.B.X := ABounds.A.X+1;
  ABounds.B.Y := ABounds.A.Y+1;
  inherited Init(ABounds);
  Color := AColor;
  end;

constructor TColorPoint.Load(var S: TStream);
  begin
  inherited Load(S);
  S.Read(Color, SizeOf(Color));
  end;

procedure TColorPoint.Store(var S: TStream);
  begin
  inherited Store(S);
  S.Write(Color, SizeOf(Color));
  end;

procedure TColorPoint.Draw;
  var
    B: Word;
  begin
  B := Application^.GetColor(Color) shl 8+$00FE {*};
  WriteLine(0, 0, 1, 1, B);
  end;

end.
