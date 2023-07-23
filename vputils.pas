unit vputils;

// Some trivial code from drivers.pas of original DN

interface

uses
  Objects;

const

  TottalExit: Boolean = False;
  Exiting: Boolean = False;

  CE_LastMsg = 22;

  CurrentBlink: Boolean = False;

var
  CE_Buttons: array[1..3] of PShortString;
  CE_Messages: array[0..CE_LastMsg] of PShortString;
  ErrWndTitle: PShortString;

procedure InitDrivers;
procedure DoneDrivers; {Cat}

implementation

const
  DriversInit: Boolean = False;

procedure InitDrivers;
  var
    I: Integer;
    S: String;
  begin
  if DriversInit then
    Exit;
  DriversInit := True;

  S := GetString(dlAltTable);
  if S <> '' then
    Move(S[1], AltCodes1, Min(Length(S), 37));

  for I := 0 to CE_LastMsg do
    CE_Messages[I] := NewStr(GetString(CE_Idx[I]));

  for I := 1 to 3 do
    begin
    S := GetString(TStrIdx(Ord(dlCE_ButAbort)+I-1));
    CE_Buttons[I] := NewStr(S);
    CE_K[I] := GetAltCode(HotKey(S)) shr 8;
    end;

  ErrWndTitle := NewStr(GetString(dlCE_ErrWndTitle));
  InitSysError;
  end { InitDrivers };

{Cat}
procedure DoneDrivers;
  var
    I: Integer;
  begin
  for I := 0 to CE_LastMsg do
    DisposeStr(CE_Messages[I]);
  for I := 1 to 3 do
    DisposeStr(CE_Buttons[I]);
  DisposeStr(ErrWndTitle);
  end;
{/Cat}

end.