unit Killer;

interface

procedure SetKillHandler;
procedure UnsetKillHandler;

const
  fQuit: Boolean = False;

implementation
uses
  Windows,
  DnIni, Commands, Startup, DnExec, FileCopy
  , VpSysLow, VideoMan, DNApp
  ;

function CtrlRoutine(dwCtrlType: DWord): Boolean;
  var
    RC: LongInt;
  label
    lCtrlBreak;
  begin
  CtrlRoutine := False;
  case dwCtrlType of
    CTRL_CLOSE_EVENT:
      begin
      if SmartWindowsBoxClose = 3 then
        goto lCtrlBreak;
      if SmartWindowsBoxClose = 2 then
        Confirms := Confirms and not cfExitConfirm;
      if SmartWindowsBoxClose >= 1 then
        begin
        fQuit := True;
        CtrlRoutine := True;
        end;
      end;
    CTRL_LOGOFF_EVENT, CTRL_SHUTDOWN_EVENT:
      Halt(-1);
    CTRL_BREAK_EVENT:
      begin
      if fExec then
        begin
        {AK155 На случай, когда DN не сумел распознать, что вызванная внешняя
программа выполяется в отдельном окне, и ушел в ожидание с черным экраном,
введена возможность "разбудить" DN при помощи Ctrl-Break. Выбрано именно
это событие, поскольку оно распознается и в Win9x. }
        FreeConsole;
        AllocConsole;
        SetKillHandler;
        TerminateProcess(hExtCmd, RC);
        DoneVideo;
        InitVideo;
        InitialiseKeyboardHandler;
        Application^.Redraw;
        end
      else if CtrlBreakKill then
lCtrlBreak:
        begin
        CloseWriteStream;
        Halt(-1);
        end;
      CtrlRoutine := True;
      end;
  end {case};
  end { CtrlRoutine };

procedure SetKillHandler;
  var
    ierr: LongInt;
  begin
  if not SetConsoleCtrlHandler(
      @CtrlRoutine, // address of handler function
      True // handler to add or remove
      )
  then
    ierr := GetLastError;
  end;

procedure UnsetKillHandler;
  var
    ierr: LongInt;
  begin
  if not SetConsoleCtrlHandler(
      @CtrlRoutine, // address of handler function
      False // handler to add or remove
      )
  then
    ierr := GetLastError;
  end;
end.
