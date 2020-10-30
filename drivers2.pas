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

{$S-}
unit Drivers2;

{-----------------------------------------------------}
{ This module is based on Turbo Vision Drivers Unit   }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}

interface

const
  SkyEnabled: Integer = 0;
  ShiftState: Byte = 0;
  ShiftState2: Byte = 0;
  MouseVisible: Boolean = True;
  CurrentBlink: Boolean = False;

var
  MouseX: Word;
  MouseY: Word;

procedure DoDump;

implementation

uses
  dnsys, Defines, Dos, Advance, Lfnvp, Advance1, Drivers
  {, SysUtils}, VpSysLow
  ;

{$S-}
procedure DoDump;
  {$IFDEF DNPRG}
  type
    PByteArray = ^TByteArray;
    TByteArray = array[0..65528] of Byte;
  var
    C, AC, PP: Word;
    FH: LongInt;
    PPP: TByteArray absolute FreeStr;
    PhysAddr: Pointer;
    I: Byte;
    PExit, ActiveButton: Byte;
    Temp: TFileSize;
    {FileName: String;}

  function StoreBuffer(var Buf; Size: Word; FH: Word): Word;
    var
        sz: longint;
    begin
    sz := Size; // by unxed
    StoreBuffer := SysFileWrite(FH, Buf, sz, sz);
    end;

  {$ENDIF DNPRG}
  begin
  {$IFDEF DNPRG}
  { Create Report File. DRIVERS.PAS contains full dupe of this routine }

  I := Length(SourceDir);
  SourceDir := SourceDir+'DN.ERR'#0;

  if {$IFDEF DPMI32}LfnVP.{$ENDIF}SysFileOpen(@SourceDir[1], Open_Access_ReadWrite or
       open_share_DenyNone, FH) <> 0
  then
    if {$IFDEF DPMI32}LfnVP.{$ENDIF}SysFileCreate(@SourceDir[1], Open_Access_ReadWrite or
         open_share_DenyNone, $20 {Archive}, FH) <> 0
    then
      FH := Word(-1);
  SysFileSeek(FH, 0, 2, Temp);

  if FH <> Word(-1) then
    begin
    (*
     {$IFNDEF NONBP}
     AssignOutput(FH);
     {$ENDIF}
*)
    FreeStr :=
      ^M^J+
      '----<'+GetDateTime(False)+' '+GetDateTime(True)+'>'^M^J+
      'VER :'+VersionName+^M^J+
      'DATE:'+VersionDate+^M^J+
      'ERR :'+Hex2(ExitCode)+^M^J+
      'ADDR:'+Hex8(LongInt(ErrorAddr))+^M^J;
    StoreBuffer(FreeStr[1], Length(FreeStr), FH);

    FreeStr :=
      'CS  :'+Hex4(CSeg)+^M^J+
      'DS  :'+Hex4(DSeg)+^M^J+
      'SS  :'+Hex4(SSeg)+^M^J+
      'SP  :'+Hex8(LongInt(SPtr))+^M^J+
      'MEMm:'+Hex8(MaxAvail)+^M^J+
      'MEMa:'+Hex8(MemAvail)+^M^J+
      '';
    StoreBuffer(FreeStr[1], Length(FreeStr), FH);
    {Cat}
    {
     if GetLocationInfo(ErrorAddr, FileName, Temp) <> nil then
       begin
         Str(Temp, FreeStr);
         FreeStr := 'Source location: ' + FileName + ' line ' + FreeStr +^M^J;
         StoreBuffer(FreeStr[1] , length(FreeStr), FH);
       end;
}
    {$IFDEF LINEPOSIT}
    if  (SourceLineNo <> 0) and (SourceFileName <> nil) then
      begin
      Str(SourceLineNo, FreeStr);
      FreeStr := 'Source location: '+SourceFileName^+' line '+FreeStr+^M^J;
      StoreBuffer(FreeStr[1], Length(FreeStr), FH);
      end;
    {$ENDIF}
    {/Cat}
    FreeStr :=
      'SCR :'+Hex8(LongInt(ScreenBuffer))+^M^J;
    StoreBuffer(FreeStr[1], Length(FreeStr), FH);

    {    for C:=0 to Pred(ScreenHeight) do
      begin
          TempFile:='';
          PP := (ScreenWidth*C) shl 1;
          for AC := 0 to Pred(ScreenWidth) do Begin
           PPP[AC] :=  PByteArray(ScreenBuffer)^[PP+AC*2];
           TempFile:=TempFile+Hex2(PByteArray(ScreenBuffer)^[PP+AC*2+1]);
          End;
          TempFile := TempFile+#13#10 ;
          if StoreBuffer(PPP,         ScreenWidth,      FH) <> 0 then Break;
          if StoreBuffer(TempFile[1], Length(TempFile), FH) <> 0 then Break;
      end;} { for lines }
    SysFileClose(FH);
    end; { file write ok }

  SetLength(SourceDir, I);
  {$ENDIF DNPRG}
  end { DoDump };
{$S+}

end.
