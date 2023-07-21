unit CopyIni;
{AK155}

interface

procedure CopyIniVarsToCfgVars;

implementation
uses
  sysutils, DnIni, Startup, Advance1, FlPanelX
  , fnotify
  {$IFDEF Win32}
  , VpKbdW32 {for AltGreyAsAlt}
  {$ENDIF}
  , Commands
  ;

procedure CopyIniVarsToCfgVars;
  var
    i: Integer;
  begin
  if SystemDataOpt <> 65535 then
    SystemData.Options := SystemDataOpt;
  if InterfaceDataOpt <> 65535 then
    InterfaceData.Options := InterfaceDataOpt;
  if FMSetupOpt <> 65535 then
    Startup.FMSetup.Options := FMSetupOpt;
  if EditorDefaultsOpt <> 65535 then
    EditorDefaults.EdOpt := EditorDefaultsOpt;
  if EditorDefaultsOpt2 <> 65535 then
    EditorDefaults.EdOpt2 := EditorDefaultsOpt2;
  if ViewerOpt <> 65535 then
    EditorDefaults.ViOpt := ViewerOpt;
  if StartupDataLoad <> 65535 then
    StartupData.Load := StartupDataLoad;
  if StartupDataUnload <> 65535 then
    StartupData.Unload := StartupDataUnload;
  if ConfirmsOpt <> 65535 then
    Confirms := ConfirmsOpt;
  SystemData.CopyLimitBuf := CopyLimit;
  SystemData.ForceDefArch := ForceDefaultArchiver;
  for i := 0 to 8 do
    Dispose(DirsToChange[i]);
  AssignStr(DirsToChange[0], QDirs1);
  AssignStr(DirsToChange[1], QDirs2);
  AssignStr(DirsToChange[2], QDirs3);
  AssignStr(DirsToChange[3], QDirs4);
  AssignStr(DirsToChange[4], QDirs5);
  AssignStr(DirsToChange[5], QDirs6);
  AssignStr(DirsToChange[6], QDirs7);
  AssignStr(DirsToChange[7], QDirs8);
  AssignStr(DirsToChange[8], QDirs9);
    {$IFDEF Win32}
    AltGreyAsAlt := AltGrAsAlt; {JO}
    {$ENDIF}
  end { CopyIniVarsToCfgVars };

end.
