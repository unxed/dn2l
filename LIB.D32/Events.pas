{ DPMI-specific time and events tools by A.Korop (AK155)}
{$IFNDEF DPMI32} This is the DPMI32 version! {$ENDIF}

unit Events;

interface
{$I Events.inc}

implementation

uses
  VPUtils
  ;

function GetCurMSec: Longint;
  begin
  Result := GetTimeMSec;
  end;

procedure LongWorkBegin;
  begin
  end;

procedure LongWorkEnd;
  begin
  end;

end.

