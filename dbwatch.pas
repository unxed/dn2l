{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
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
//////////////////////////////////////////////////////////////////////////
//
//  Version history:
//
//  1.6.RC1
//  dn16rc1-DBF_detection_diff130byMV.patch
//  dn16rc1-view_clipper_dbf_file.patch
//  dn16rc1-vp_noasm_compatible.patch
//
//  2.0.0
//  dn269_invalid_dbf_header_detection.patch
//
//  2.7.0
//  dn281-dbf_detect_fix.patch
//  dn21029-dbf_structure_detection_fix.patch
//  dn3323-DBFViewer(i)-header_corruption_warning_dialog_improve.patch
//
//  3.7.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit DBWatch;

interface

uses
  Objects, Objects2, Streams, Defines
  //,
  //use16
  //commented by unxed
  ;

type
  PFieldRec = ^Fieldrec;
  FieldRec = record
    Name: String[10];
    Who: Char;
    ln: AWord; { Kirill }
    { For Compatible Virtual Pascal Word -> AWord }
    len, Dec: AWord;
    Pos: AWord;
    end;

  PFieldCollection = ^TFieldCollection;
  TFieldCollection = object(TCollection)
    procedure FreeItem(P: Pointer); virtual;
    end;

  PDBFile = ^TDBFile;
  TDBFile = object(TObject)
    WriteMode: Byte;
    BaseName: String; {DataCompBoy}
    BaseFile: TBufStream;
    Date, NumRec: LongInt;
    HeaderLen, RecLen: AWord;
    Unused: array[0..31] of Byte;
    NumFields: Integer;
    Loc: LongInt;
    Fields: PCollection;
    constructor Init(FileName: String); {DataCompBoy}
    destructor Done; virtual;
    procedure Read(var Buf; Num: Word);
    procedure Seek(NewLoc: LongInt);
    function GetField(Name: String; var Buf): String;
    function GetNField(N: Word; var Buf): String;
    function GetFieldRec(N: Integer): PFieldRec;
    procedure OpenMode(Mode: Word);
    end;

implementation

uses
  Messages, Commands, DNApp, Lfnvp, Advance1
  ;

procedure TFieldCollection.FreeItem;
  begin
  Dispose(PFieldRec(P));
  end;

function NewField(Name: String; Who: Char; len: Word; Dec: Byte;
     Pos: Word): PFieldRec; { Kirill }
  var
    P: PFieldRec;
  begin
  New(P);
  P^.Name := Name;
  while Name[Length(Name)] = ' ' do
    System.Dec(Name[0]);
  P^.Who := Who;
  if len = 0
  then
    P^.len := 1
  else
    P^.len := len;
  P^.Dec := Dec;
  P^.Pos := Pos;
  NewField := P;
  if P^.Who = 'D'
  then
    if Length(Name) > 10 then
      P^.ln := Length(Name)
    else
      P^.ln := 10
  else if Length(Name) > len then
    P^.ln := Length(Name)
  else
    P^.ln := len;
  if P^.ln > 255 then
    P^.ln := 255; { Kirill }
  end { NewField };

procedure TDBFile.OpenMode(Mode: Word);
  begin
  BaseFile.Init(BaseName, Mode, 16384);
  end;

constructor TDBFile.Init;

  {--- start -------- Eugeny Zvyagintzev ---- 17-06-2002 ----}
  type
    TParams = record
      InHeader: LongInt;
      Actually: LongInt;
      end;
    {--- finish -------- Eugeny Zvyagintzev ---- 17-06-2002 ----}
  type
    TFldLenRec = record
      { Kirill }
      case Integer of
        0: (len, Dec: Byte);
        1: (CharFieldLen: AWord);
      end;
    frec = record
      Name: array[0..10] of Char;
      Who: Char;
      Info1: array[0..3] of Char;
      FldLen: TFldLenRec; { Kirill }
      Info2: array[0..13] of Char;
      end;

  var
    FBuf: frec;
    RL: Integer;
    Par: TParams; {JOHN_SW}

  function ReadFldStru(CharSizeWord: Boolean): Boolean;
    var
      I, J: Integer;
      S: String;
      FieldLen: AWord;
      FieldDec: Byte;
    begin
    ReadFldStru := True;
    BaseFile.Seek(32);
    if Fields <> nil then
      Dispose(Fields, Done);
    {piwamoto.src.end}
    Fields := New(PFieldCollection, Init(NumFields, NumFields));
    RL := 1;
    for I := 1 to NumFields do
      begin
      BaseFile.Read(FBuf, 32);
      if  (FBuf.Who = 'C') and (CharSizeWord) then
        begin { Kirill }
        FieldLen := FBuf.FldLen.CharFieldLen; { Kirill }
        FieldDec := 0; { Kirill }
        end
      else
        begin
        FieldLen := FBuf.FldLen.len; { Kirill }
        FieldDec := FBuf.FldLen.Dec; { Kirill }
        end;
      Inc(RL, FieldLen); { Kirill }
      S := '';
      J := 0;
      while FBuf.Name[J] >= #32 do
        begin
        S := S+FBuf.Name[J];
        Inc(J);
        end;
      S[0] := Char(J);
      if  (J = 0) or (BaseFile.Status <> stOK) then
        begin
        ReadFldStru := False;
        Exit;
        end;
      with FBuf do
        Fields^.Insert(NewField(S, Who, FieldLen, FieldDec, RL-FieldLen));
      end;
    end { ReadFldStru };

  var
    BaseFileSize: Longint; {!!s}
  begin { TDBFile.Init }
  inherited Init;
  BaseName := lFExpand(FileName); {DataCompBoy}
  OpenMode(stOpenRead);
  if BaseFile.Status <> stOK then
    begin
    BaseFile.Done;
    Fail;
    end;
  BaseFile.Read(Date, 32);
  if BaseFile.Status <> stOK then
    begin
    BaseFile.Done;
    Fail;
    end;
  BaseFileSize := i32(BaseFile.GetSize);
  if BaseFileSize-HeaderLen < 0 then
    begin
    BaseFile.Done;
    Fail;
    end; {JOHN_SW}
  {piwamoto.src.begin} {bugfixed .DBF detection}
  NumFields := -1;
  repeat
    BaseFile.Read(FBuf, 1);
    BaseFile.Seek(BaseFile.GetPos+31);
    Inc(NumFields);
  until (FBuf.Name[0] < #32) or
    (BaseFile.Eof) or
    (HeaderLen < BaseFile.GetPos);
  if  (NumFields < 1) or
      (FBuf.Name[0] <> #13)
  then
    begin
    BaseFile.Done;
    Fail;
    end;
  if not ReadFldStru(True) then
    begin
    Dispose(Fields, Done);
    Fields := nil;
    BaseFile.Done;
    Fail;
    end;
  {--- start -------- Eugeny Zvyagintzev ---- 17-06-2002 ----}
  {Check for correct header}
  if  (RL <> RecLen) then
    if not ReadFldStru(False) then
      begin
      Dispose(Fields, Done);
      Fields := nil;
      BaseFile.Done;
      Fail;
      end
    else if (RL <> RecLen) then
      begin
      Par.InHeader := RecLen;
      Par.Actually := RL;
      MessageBox(GetString(dlInvalidDBFHeader)+GetString(dlInvalidRecSize),
        @Par, mfError+mfOKButton);
      end
    else
      RL := RecLen;
  if NumRec <> ((BaseFileSize-HeaderLen) div RL) then
    begin
    if MessageBox
          (GetString(dlInvalidDBFHeader)+GetString(dlInvalidRecNumber)+
        ItoS(NumRec)+
        GetString(dlInvalidRecNumber1)+ItoS(((BaseFileSize-HeaderLen
              ) div RL))+GetString(dlUseCalcRecNumber),
        nil, mfError+mfYesNoConfirm) = cmNo
    then
      NumRec := (BaseFileSize-HeaderLen) div RL;
    end;
  {--- finish -------- Eugeny Zvyagintzev ---- 17-06-2002 ----}
  Loc := 0;
  BaseFile.Seek(HeaderLen);
  end { TDBFile.Init };

procedure TDBFile.Read;
  var
    I: LongInt;
  begin
  if Loc+Num >= NumRec then
    Num := NumRec-Loc;
  I := LongInt(RecLen)*LongInt(Num);
  if I > 65520 then
    I := 65520;
  BaseFile.Read(Buf, I);
  Inc(Loc, Num);
  end;

procedure TDBFile.Seek;
  begin
  BaseFile.Seek(NewLoc*RecLen+HeaderLen);
  Loc := NewLoc;
  end;

function TDBFile.GetFieldRec;
  begin
  GetFieldRec := Fields^.At(N);
  end;

function TDBFile.GetField;
  var
    I, K, N: Integer;
    B: array[0..65000] of Char absolute Buf;
    S: String;
    L: Word;
  begin
  I := 0;
  K := 1;
  while (I < NumFields) and (GetFieldRec(I)^.Name <> Name) do
    Inc(I);
  K := GetFieldRec(I)^.Pos;
  S := '';
  N := I;
  for I := K to K+GetFieldRec(N)^.len-1 do
    S[I-K] := B[I];
  S[0] := Char(GetFieldRec(N)^.len);
  GetField := S;
  end;

function TDBFile.GetNField;
  var
    I, K, J: Integer;
    B: array[0..65000] of Char absolute Buf;
    S: String;
    L: Word;
  begin
  K := GetFieldRec(N)^.Pos;
  S := '';
  for I := K to K+GetFieldRec(N)^.len-1 do
    S[I-K+1] := B[I];
  S[0] := Char(GetFieldRec(N)^.len);
  GetNField := S;
  end;

destructor TDBFile.Done;
  begin
  Dispose(Fields, Done);
  Fields := nil;
  BaseFile.Done;
  end;

end.
