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
unit ArchSet;

interface
uses
  Archiver,
  Types,
  sysutils
  ;

procedure SetupArchive(ArchCommand: Word);
procedure UpdateARH(Arch: PARJArchive);

implementation

uses
  Defines, Startup, ArchDet, Dialogs, Views, Menus,
  DNApp, Advance1, Commands, profile, DnIni
  ;

procedure UpdateARH(Arch: PARJArchive);
  var
    J: Word;
    A: PARJArchive;
    P: PView;
  begin
  if Arch <> nil then
    Arch^.Save
  else
    begin
    P := WriteMsg(GetString(dlPleaseStandBy));
    for J := 0 to NumSupportedArchs-1 do
      begin
      A := GetArchiveByTag(J);
      if A <> nil then
        begin
        A^.Save;
        Dispose(A, Done);
        A := nil;
        end;
      end;
    P^.Free;
    end;
  CloseProfile;
  end { UpdateARH };

procedure SetupArchive;
  var
    D: PDialog;
    P: PView;
    R: TRect;
    Arch: PARJArchive;
    W: Word;

    DT: record
      Pack: String; {Inputline}
      Unpack: String; {Inputline}
      Extract: String[20]; {Inputline}
      ExWP: String[20]; {Inputline}
      Add: String[20]; {Inputline}
      Move: String[20]; {Inputline}
      Delete: String[20]; {Inputline}
      Test: String[20]; {Inputline}
      Force: String[20]; {Inputline}
      IncludeP: String[20]; {Inputline}
      ExcludeP: String[20]; {Inputline}
      Password: String[20]; {Inputline}
      RecovRec: String[20]; {Inputline}
      SelfExtr: String[20]; {Inputline}
      Solid: String[20]; {Inputline}
      Recurse: String[20]; {Inputline}
      PthInside: String[20]; {Inputline}
      StoreC: String[20]; {Inputline}
      FastestC: String[20]; {Inputline}
      FastC: String[20]; {Inputline}
      NormC: String[20]; {Inputline}
      GoodC: String[20]; {Inputline}
      MaxC: String[20]; {Inputline}
      CList: String[6]; {Inputline}
      EList: String[6]; {Inputline}
      AllVersion: Word; {Checkbox}
      {AK155}
      PutDirs: Word; {Checkbox}
      {JO}
      {$IFNDEF DPMI32}
      ShortCmdLine: Word; {Checkbox}
      {$ELSE}
      SwapWhenExec: Word; {Checkbox}
      {$ENDIF}
      {/JO}
      {$IFNDEF OS2}
      UseLFN: Word; {Checkbox}
      {нужен в DOS и W32-версиях}
      {$ENDIF}
      end;

  function ArcName(ArcT: Word): String;
    var
      S: String;
      FreeByte: byte;
    begin
//    S := fReplace('~', '', CnvString(LookUpMenu(MenuBar^.Menu, ArcT,
//             dfByCommand)^.Name));
// fixme: commented by unxed
    FreeByte := PosChar('-', S);
    if FreeByte > 0 then
      Delete(S, 1, FreeByte+1);
    ArcName := S;
    end;

  procedure CndRpl;
    var
      s: String;
    begin
    s := ArcName(ArchCommand+cmLoConfigArchiver);
    if Length(s)+Length(D^.Title^)+10 < D^.Size.X then
      s := D^.Title^+' - '+s;
    ReplaceP2(D^.Title, s);
    end;

  label Ex;
  begin { SetupArchive }
  if ArchCommand >= cmLoConfigArchiver then
    Dec(ArchCommand, cmLoConfigArchiver);
  Arch := GetArchiveByTag(ArchCommand);
  if Arch = nil then
    Exit;
  with Arch^ do
    begin
    DT.Pack := CnvString2(Packer);
    DT.Unpack := CnvString2(UnPacker);
    DT.Extract := CnvString2(Extract);
    DT.ExWP := CnvString2(ExtractWP);
    DT.Add := CnvString2(Add);
    DT.Move := CnvString2(Move);
    DT.Test := CnvString2(Test);
    DT.Delete := CnvString2(Delete);
    DT.Password := CnvString2(Garble);
    DT.Force := CnvString2(ForceMode);
    DT.IncludeP := CnvString2(IncludePaths);
    DT.ExcludeP := CnvString2(ExcludePaths);
    DT.RecovRec := CnvString2(RecoveryRec);
    DT.SelfExtr := CnvString2(SelfExtract);
    DT.Solid := CnvString2(Solid);
    DT.Recurse := CnvString2(RecurseSubDirs);
    DT.PthInside := CnvString2(SetPathInside);
    DT.StoreC := CnvString2(StoreCompression);
    DT.FastestC := CnvString2(FastestCompression);
    DT.FastC := CnvString2(FastCompression);
    DT.NormC := CnvString2(NormalCompression);
    DT.GoodC := CnvString2(GoodCompression);
    DT.MaxC := CnvString2(UltraCompression);
    DT.CList := CnvString2(ComprListChar);
    DT.EList := CnvString2(ExtrListChar);
    DT.AllVersion := Word(AllVersion);
    DT.PutDirs := Word(PutDirs);
    {$IFNDEF DPMI32}
    DT.ShortCmdLine := Word(ShortCmdLine);
    {$ELSE}
    DT.SwapWhenExec := Word(SwapWhenExec);
    {$ENDIF}
    {$IFNDEF OS2}
    DT.UseLFN := Word(UseLFN);
    {$ENDIF}
    end;
  D := PDialog(Application^.ValidView(PDialog(LoadResource(dlgSetupArc))));
  if D = nil then
    goto Ex;
  D^.SetData(DT);
  CndRpl;
  W := Desktop^.ExecView(D);
  if W = cmOK then
    D^.GetData(DT);
  Dispose(D, Done);
  if W <> cmOK then
    goto Ex;
  if SystemData.ForceDefArch = '' then
    DefaultArchiver := ArchCommand;
  with Arch^ do
    begin
    Done;
    AssignStr(Packer, DT.Pack);
    Packer := NewStr(DT.Pack);
    UnPacker := NewStr(DT.Unpack);
    Extract := NewStr(DT.Extract);
    ExtractWP := NewStr(DT.ExWP);
    Add := NewStr(DT.Add);
    Move := NewStr(DT.Move);
    Test := NewStr(DT.Test);
    Delete := NewStr(DT.Delete);
    Garble := NewStr(DT.Password);
    ForceMode := NewStr(DT.Force);
    IncludePaths := NewStr(DT.IncludeP);
    ExcludePaths := NewStr(DT.ExcludeP);
    RecoveryRec := NewStr(DT.RecovRec);
    SelfExtract := NewStr(DT.SelfExtr);
    Solid := NewStr(DT.Solid);
    RecurseSubDirs := NewStr(DT.Recurse);
    SetPathInside := NewStr(DT.PthInside);
    StoreCompression := NewStr(DT.StoreC);
    FastestCompression := NewStr(DT.FastestC);
    FastCompression := NewStr(DT.FastC);
    NormalCompression := NewStr(DT.NormC);
    GoodCompression := NewStr(DT.GoodC);
    UltraCompression := NewStr(DT.MaxC);
    ComprListChar := NewStr(DT.CList);
    ExtrListChar := NewStr(DT.EList);
    {if DT.List <> '' then ListChar := DT.List[1] else ListChar := ' ';}
    AllVersion := (DT.AllVersion and 1) <> 0;
    PutDirs := (DT.PutDirs and 1) <> 0;
    {$IFNDEF DPMI32}
    ShortCmdLine := (DT.ShortCmdLine and 1) <> 0;
    {$ELSE}
    SwapWhenExec := (DT.SwapWhenExec and 1) <> 0;
    {$ENDIF}
    {$IFNDEF OS2}
    UseLFN := (DT.UseLFN and 1) <> 0;
    {$ENDIF}
    end;
  UpdateARH(Arch);
  Message(Application, evCommand, cmUpdateConfig, nil);
Ex:
  Dispose(Arch, Done);
  end { SetupArchive };

end.
