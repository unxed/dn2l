unit Killer;

interface
uses
  Os2Def, Os2Base
  ;

procedure SetKillHandler(var RegRec: ExceptionRegistrationRecord);
procedure UnsetKillHandler(var RegRec: ExceptionRegistrationRecord);

const
  fQuit: Boolean = False;

implementation
uses
  Drivers
  , DnIni, Commands, Startup, DnExec, FileCopy
  ;

{$S-}
function CtrlRoutine(
    p1: PExceptionReportRecord; P2: PExceptionRegistrationRecord;
    p3: PContextRecord; PV: Pointer): ULong;
  cdecl;
  var
    rc: ApiRet;
  label
    lCtrlBreak;
  begin
  CtrlRoutine := xcpt_Continue_Search;
  case p1^.ExceptionNum of
    xcpt_Signal:
      begin
      case p1^.ExceptionInfo[0] of
        xcpt_Signal_Intr:
          CtrlRoutine := xcpt_Continue_Execution;
        xcpt_Signal_KillProc:
          begin
          if SmartWindowsBoxClose = 3 then
            goto lCtrlBreak;
          if SmartWindowsBoxClose = 2 then
            Confirms := Confirms and not cfExitConfirm;
          if SmartWindowsBoxClose >= 1 then
            begin
            fQuit := True;
            CtrlRoutine := xcpt_Continue_Execution;
            end;
          end;
        xcpt_Signal_Break:
          begin
          CtrlRoutine := xcpt_Continue_Execution;
          if CtrlBreakKill and not fExec then
            begin
lCtrlBreak:
            CloseWriteStream;
            rc := DosUnsetExceptionHandler(P2^);
            if rc = NO_ERROR then
              Halt(-1);
            end;
          end;
      end {case}
      end;
  end {case};
  end { CtrlRoutine };

procedure SetKillHandler(var RegRec: ExceptionRegistrationRecord);
  var
    rc: ApiRet;
  begin
  FillChar(RegRec, SizeOf(RegRec), 0);
  RegRec.ExceptionHandler := CtrlRoutine;
  rc := DosSetExceptionHandler(RegRec);
  if rc <> NO_ERROR then
    Writeln('DosSetExceptionHandler error: return code = ', rc);
  end;

procedure UnsetKillHandler(var RegRec: ExceptionRegistrationRecord);
  var
    rc: ApiRet;
  begin
  rc := DosUnsetExceptionHandler(RegRec);
  end;

end.
