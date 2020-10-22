unit fnotify;
(******

{Заглушка для D32}
Directory Change Notifier - Win32 version
Written by Cat 2:5030/1326.13
(for use in DN/2)

******)

interface

 uses xTime;

Var
 NotifyTmr: TEventTimer;       {JO}

procedure NotifyInit; inline; begin end;
procedure NotifyAddWatcher(const Path: String); inline; begin end;
procedure NotifyDeleteWatcher(const Path: String); inline; begin end;
function NotifyAsk(var S: String): Boolean;
  inline; begin Result := False end;
procedure NotifySuspend; inline; begin end;
procedure NotifyResume; inline; begin end;
procedure NotifyDone; inline; begin end;

implementation
end.
