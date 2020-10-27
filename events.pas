{$I STDEFINE.INC}

unit Events;

interface

{$I Events.inc}

implementation

uses
  VPSysLow,
  linux
  ;

function GetCurMSec: Longint;
var
  ms: longint;
begin
  SysGetDateTime(nil, nil, nil, nil, nil, nil, nil, @ms);
  GetCurMSec := ms;
end;

procedure LongWorkBegin;
  begin
    // fixme: stub by unxed
  end;

procedure LongWorkEnd;
  begin
    // fixme: stub by unxed
  end;

end.
