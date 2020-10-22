{ Win32-specific time and events tools by A.Korop (AK155)}
{$IFNDEF Win32} This is the Win32 version! {$ENDIF}
{$I STDEFINE.INC}

unit Events;

interface

{$I Events.inc }

implementation

uses
  Windows, VPSysLow
  ;

function GetCurMSec: Longint;
  begin
  Result := GetTickCount;
  end;

procedure LongWorkBegin;
  begin
  {$IFDEF WIN95_HIGHPRIORITY}
  if SysPlatformId = 1 then
    {Win9x}
    SetPriorityClass(GetCurrentProcess, Normal_Priority_Class);
  {$ENDIF}
  end;

procedure LongWorkEnd;
  begin
  {$IFDEF WIN95_HIGHPRIORITY}
  if SysPlatformId = 1 then
    {Win9x}
    SetPriorityClass(GetCurrentProcess, High_Priority_Class);
  {$ENDIF}
  end;

end.
