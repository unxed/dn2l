@set pause=rem
@Echo off
if [%1]==[P] goto plugin
if [%1]==[p] goto plugin
if not [%1]==[] goto help
goto end_plugin
:plugin
set plugin=/dPLUGIN
set rplugin=PLUGIN
:end_plugin

set H=O:OLF:OS2;LargeFileSupport
if [%Host%]==[OLF] goto end_host
set H=W:WLF:Win32;LargeFileSupport
if [%Host%]==[WLF] goto end_host
goto Help
:end_host

if [%target%]==[] set target=%Host%
set T=O:OLF:OS2;LargeFileSupport
if [%Target%]==[OLF] goto end_Target
set T=W:WLF:Win32;LargeFileSupport
if [%Target%]==[WLF] goto end_Target
set T=D
if [%Target%]==[D32] goto end_Target
goto Help
:end_Target

echo Host="%Host%" Target="%Target%" %rplugin%
%pause%

if not exist EXE.%Target% md EXE.%Target%

Echo        Compiling VERSION.EXE for %Host%
if exist EXE.%Host%\version.exe goto dover
vpc version /b /q /c%H%
@if errorlevel 1 goto Error
if exist EXE.%Host%\tvhc.exe del EXE.%Host%\thvc.exe
:dover
EXE.%Host%\version.exe EXE.%Target%\version.inc %Target%  %rplugin%
%pause%

if exist EXE.%Host%\tvhc.exe goto comphelp
Echo        Compiling TVHC.EXE for %Host%
vpc tvhc /b /q /c%H%
@if errorlevel 1 goto Error

:comphelp
Echo   Compiling help files for %Target%
EXE.%Host%\tvhc resource\english\dnhelp.htx EXE.%Target%\english.hlp EXE.%Target%\dnhelp.pas /4DN_OSP
EXE.%Host%\tvhc resource\russian\dnhelp.htx EXE.%Target%\russian.hlp EXE.%Target%\dnhelp.pas /4DN_OSP
EXE.%Host%\tvhc resource\ukrain\dnhelp.htx EXE.%Target%\ukrain.hlp EXE.%Target%\dnhelp.pas /4DN_OSP
rem EXE.%Target%\tvhc resource\hungary\dnhelp.htx EXE.%Target%\hungary.hlp EXE.%Target%\dnhelp.pas /4DN_OSP
%pause%

if exist EXE.%Host%\rcp.exe goto dores
Echo        Compiling RCP.EXE for %Host%
vpc rcp /b /dRCP /q %plugin% /c%H%
@if errorlevel 1 goto Error
:dores

Echo        Compiling resource files for %Target% %rplugin%
set RCP_Target=%Target%
if [%Target%]==[OLF] set RCP_P=O OS2
if [%Target%]==[WLF] set RCP_P=W Win32
if [%Target%]==[D32] set RCP_P=D DPMI32
rem EXE.%Host%\rcp %T% %RCP_P% %rplugin%
EXE.%Host%\rcp %RCP_P% %rplugin%
:endres
%pause%

Echo        Compiling DN.EXE for %Target%  %rplugin%

if not [%Target%]==[D32] goto OS2W32
vpc dn /dDN /dDNPRG -b -q -CW:d32:DPMI32
@if not errorlevel 1 goto pe2LE
@echo Это что-то переполняется в VP, со второго раза получится.
vpc dn /dDN /dDNPRG -m -q -CW:d32:DPMI32
@if errorlevel 1 goto Error
:pe2LE
PE2LE.EXE EXE.D32\DN.EXE EXE.D32\DN.PRG /S:LIB.D32\dos32a.exe /Q
@if errorlevel 1 goto Error
del EXE.D32\dn.exe
goto end_dn

:OS2W32
if [%Target%]==[WLF] goto W32
Echo        Compiling dnpmapil.dll
vpc dnpmapil -c%T% /q /b %plugin%
@if errorlevel 1 goto Error
%pause%
vpc dn -c%T% /b /dDN /dDNPRG /q %plugin%
if not errorlevel 1 goto end_dn
@echo Это что-то переполняется в VP, со второго раза получится.
vpc dn -c%T% /m /dDN /dDNPRG /q %plugin%
@if errorlevel 1 goto Error
if [%Host%]==[OLF] rc -p -x2 EXE.OLF\dn.res EXE.OLF\dn.exe
goto end_dn

:W32
LIB.W32\vpkbdw32.pas /m /dDN /dDNPRG /q %plugin% /c%T%
vpc dn -c%T% /b /dDN /dDNPRG /q %plugin%
if not errorlevel 1 goto end_dn
@echo Это что-то переполняется в VP, со второго раза получится.
vpc dn -c%T% /m /dDN /dDNPRG /q %plugin%
@if errorlevel 1 goto Error

:end_dn
%pause%

if [%plugin%]==[] goto end_plgman
Echo        Compiling plugman.dll for %Target%
vpc plugman /m /q /c%T%
if not errorlevel 1 goto end_plgman
@echo Это что-то переполняется в VP, со второго раза получится.
vpc plugman /m /q /c%T%
@if errorlevel 1 goto Error
:end_plgman
%pause%

rem lxlite exe.os2\dn.exe

%pause% Финальное удаление vpi и.т.п.
del EXE.%Target%\*.vpi
del EXE.%Target%\*.lib
del EXE.%Target%\*.map
del EXE.%Target%\*.bak
del EXE.%Target%\*.lnk
del EXE.%Target%\*.obj
goto ret

:Help
@echo Переменная Host должна указывать текущую платформу (OLF, WLF);
@echo Переменная Target должна указывать целевую платформу (OLF, WLF, D32),
@echo если она отлична от текущей.
@echo Параметр должен отсутствовать или иметь значение P
@echo (для компиляции плагинной версии).
@goto ret

:Error
  echo Error
:ret

