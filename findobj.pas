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
unit FindObj;
{ Используется только для Арвида }
interface

uses
  Defines, Objects2, Dialogs, Objects
  ;

type
  PFindObject = ^TFindObject;
  TFindObject = object(TObject)
    Text: PString;
    TT: (ttTape, ttDir, ttFile);
    constructor Init(const S: String);
    function GetText: String; virtual;
    destructor Done; virtual;
    end;

  PFindDir = ^TFindDir;
  TFindDir = object(TFindObject)
    Pos: LongInt;
    constructor Init(const S: String; APos: LongInt);
    function GetText: String; virtual;
    end;

  PFindFile = ^TFindFile;
  TFindFile = object(TFindObject)
    Name: PString;
    Size: LongInt;
    Time: LongInt;
    constructor Init(const S: String; ASize, ATime: LongInt);
    destructor Done; virtual;
    function GetText: String; virtual;
    end;

  PFindBox = ^TFindBox;
  TFindBox = object(TListBox)
    function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    function IsSelected(Item: LongInt): Boolean; virtual;
    end;

const
  FindList: PCollection = nil;

implementation

uses
  Advance1, DNApp, Commands, Dos
  ;

constructor TFindObject.Init;
  begin
  inherited Init;
  Text := NewStr(S);
  TT := ttTape;
  end;

function TFindObject.GetText;
  begin
  GetText := GetString(dlArvid_TapeDir)+CnvString(Text);
  end;

destructor TFindObject.Done;
  begin
  DisposeStr(Text);
  end;

constructor TFindDir.Init;
  begin
  inherited Init(S);
  Pos := APos;
  TT := ttDir;
  end;

function TFindDir.GetText;
  begin
  GetText := GetString(dlDirectory)+' '+CnvString(Text);
  end;

constructor TFindFile.Init;
  begin
  inherited Init('');
  Name := NewStr(S);
  Size := ASize;
  Time := ATime;
  TT := ttFile;
  end;

destructor TFindFile.Done;
  begin
  DisposeStr(Name);
  inherited Done;
  end;

function TFindFile.GetText;
  var
    DT: DateTime;
    S: String;
  begin
  UnpackTime(Time, DT);
  MakeDate(DT.Day, DT.Month, DT.Year mod 100, DT.Hour, DT.Min,
     S);
  GetText := '  '+AddSpace(CnvString(Name), 13)+PredSpace(FStr(Size), 13)
    +' '+S;
  end;

function TFindBox.GetText;
  var
    P: PFindObject;
  begin
  P := List^.At(Item);
  if P <> nil then
    GetText := P^.GetText
  else
    GetText := '';
  end;

function TFindBox.IsSelected;
  var
    P: PFindObject;
  begin
  P := List^.At(Item);
  IsSelected := (P <> nil) and (P^.TT = ttTape);
  end;

end.
