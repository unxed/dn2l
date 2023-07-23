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

procedure DoneDrivers; {Cat}

implementation

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