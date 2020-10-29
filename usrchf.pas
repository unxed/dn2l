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

unit USrchF;

interface

uses
  Gauge;

function FindFileWithSPF(pFileName: String; Info: PWhileView): String;

implementation

uses
  Lfnvp, Objects2, Objects,
  Commands, DNApp,
  Dos, Advance, Advance1, Advance2, Messages, DnIni
  ;

{-DataCompBoy-}
function RecursiveCheck(Name, pPath: String; DoMore: Boolean;
     Info: PWhileView): String;
  var
    SR: lSearchRec;
    SC: TStringCollection;
  label q;
  begin
  if Abort then
    Exit;
  DispatchEvents(Info, Abort);
  if Abort then
    Exit;
  SC.Init($10, $10, False);
  RecursiveCheck := '';
  SC.AtInsert(0, NewStr(pPath));

  repeat
    DispatchEvents(Info, Abort);
    if Abort then
      goto q;
    pPath := CnvString(SC.At(0));
    SC.AtFree(0);

    if ExistFile(MakeNormName(pPath, Name)) then
      begin
      RecursiveCheck := MakeNormName(pPath, Name);
      lFindClose(SR);
      goto q;
      end;

    lFindFirst(MakeNormName(pPath, x_x), AnyFileDir, SR);
    while DosError = 0 do
      begin
      DispatchEvents(Info, Abort);
      if Abort then
        begin
        lFindClose(SR);
        goto q
        end;
      if  (SR.SR.Name <> '.') and (SR.SR.Name <> '..') and
          (SR.SR.Attr and Directory <> 0)
      then
        begin
        if DoMore
        then
          SC.Insert(NewStr(MakeNormName(pPath, SR.FullName)))
        else if ExistFile(MakeNormName(MakeNormName(pPath, SR.FullName),
               Name))
        then
          begin
          RecursiveCheck := MakeNormName(MakeNormName(pPath,
               SR.FullName), Name);
          lFindClose(SR);
          goto q;
          end;
        end;
      lFindNext(SR);
      end;
    lFindClose(SR);
  until SC.Count = 0;
q:
  SC.Done;
  end { RecursiveCheck };
{-DataCompBoy-}

{-DataCompBoy-}
function FindFileWithSPF(pFileName: String; Info: PWhileView): String;
  var
    F: lText;
    S, S2: String;
    IsSec, Found, InSq: Boolean;
    P, I: Integer;
  label ex;
  begin
  Result := lFExpand(pFileName);
  if ExistFile(Result) then
    Exit;
  Result := '';
  if CharCount('.', pFileName) = 0 then
    pFileName := pFileName+'.';
  lAssignText(F, SourceDir+'DN.SPF');
  lResetText(F);
  if IOResult <> 0 then
    begin
    lRewriteText(F);
    if IOResult <> 0 then
      Exit;
    Writeln(F.T, '; Dos Navigator Search Paths file');
    Writeln(F.T);
    Writeln(F.T, GetString(dlSPF01));
    Writeln(F.T, GetString(dlSPF02));
    Writeln(F.T, GetString(dlSPF03));
    Writeln(F.T, GetString(dlSPF04));
    Writeln(F.T, GetString(dlSPF05));
    Writeln(F.T, GetString(dlSPF06));
    Writeln(F.T, GetString(dlSPF07));
    Writeln(F.T, GetString(dlSPF08));
    Writeln(F.T, GetString(dlSPF09));
    Writeln(F.T, GetString(dlSPF10));
    Writeln(F.T, GetString(dlSPF11));
    Writeln(F.T, GetString(dlSPF12));
    Writeln(F.T, GetString(dlSPF13));
    Writeln(F.T, GetString(dlSPF14));
    Writeln(F.T, GetString(dlSPF15));
    Writeln(F.T, GetString(dlSPF16));
    Writeln(F.T, GetString(dlSPF17));
    Writeln(F.T, GetString(dlSPF18));
    Writeln(F.T, GetString(dlSPF19));
    Writeln(F.T, GetString(dlSPF20));
    Writeln(F.T, GetString(dlSPF21));
    Writeln(F.T, GetString(dlSPF22));
    Writeln(F.T, GetString(dlSPF23));
    Writeln(F.T, GetString(dlSPF24));
    Writeln(F.T, GetString(dlSPF25));
    Close(F.T);
    Info^.Hide; {Cat}
    MessageBox(GetString(dlSPFht), nil, mfOKButton);
    Exit;
    end;
  IsSec := False;
  Found := False;
  while (not Eof(F.T)) and (not Found) do
    begin
    if Abort then
      goto ex;
    Readln(F.T, S);
    S := fDelLeft(fDelRight(S));
    if  (S = '') or (Copy(S, 1, 1) = ';') then
      Continue;
    if  (S[1] = '[') then
      begin
      IsSec := False;
      P := Pos(']', S);
      if P > 0 then
        S := Copy(S, 2, P-2)
      else
        Delete(S, 1, 1);
      IsSec := InExtFilter(pFileName, S);
      end
    else if IsSec then
      begin
      DelLeft(S);
      InSq := False;
      P := 1;
      for I := 1 to Length(S)+1 do
        if  ( (I > Length(S)) and (P < Length(S))) or
            ( (S[I] in [';', ',']) and (not InSq))
        then
          begin
          S2 := Copy(S, P, I-P);
          if S2[1] in ['+', '*'] then
            S2 := RecursiveCheck(pFileName,
              Copy(S2, 2, MaxStringLength),
              S2[1]='*',
              Info)
          else
            begin
            S2 := MakeNormName(S2, pFileName);
            if not ExistFile(S2) then
              S2 := '';
            end;
          if Abort then
            goto ex;
          P := I+1;
          if S2 <> '' then
            begin
            Result := S2;
            Found := True;
            end
          end
        else if S[I] = '"' then
          InSq := not InSq;
      end;
    end;
ex:
  Close(F.T);
  {D.Filter := ''; MakeTMaskData(D);}
  end { FindFileWithSPF };
{-DataCompBoy-}

end.
