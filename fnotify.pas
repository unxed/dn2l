unit fnotify;

interface

procedure NotifyInit;
procedure NotifyAddWatcher(const Path: String);
procedure NotifyDeleteWatcher(const Path: String);
function NotifyAsk(var S: String): Boolean;
procedure NotifySuspend;
procedure NotifyResume;
procedure NotifyDone;

implementation

procedure NotifyInit; inline; begin end;
procedure NotifyAddWatcher(const Path: String); inline; begin end;
procedure NotifyDeleteWatcher(const Path: String); inline; begin end;
function NotifyAsk(var S: String): Boolean;
  inline; begin Result := False end;
procedure NotifySuspend; inline; begin end;
procedure NotifyResume; inline; begin end;
procedure NotifyDone; inline; begin end;

end.
