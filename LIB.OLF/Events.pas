{ OS/2-specific time and events tools by A.Korop (AK155)}
{$IFNDEF OS2} This is the OS/2 version! {$ENDIF}

unit Events;

interface

{$I Events.inc }

implementation

uses
  Os2Def, Os2Base
  ;

function GetCurMSec: Longint;
  begin
  DosQuerySysInfo(QSV_MS_COUNT, QSV_MS_COUNT, Result, SizeOf(Result));
  end;

procedure LongWorkBegin;
  begin
  end;

procedure LongWorkEnd;
  begin
  end;

end.
