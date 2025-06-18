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
//  SysGetDateTime(nil, nil, nil, nil, nil, nil, nil, @ms);
//  GetCurMSec := ms;
  GetCurMSec := 0;
  // fixme: porting stub
end;

procedure LongWorkBegin;
  begin
    // fixme: porting stub
  end;

procedure LongWorkEnd;
  begin
    // fixme: porting stub
  end;

end.
