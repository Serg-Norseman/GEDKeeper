@echo off

set GKVER=2.17.0

call .\clean.cmd
call .\clean_all.cmd

set CONFIG_TYPE=Debug
for %%a in (release Release RELEASE) do if (%%a)==(%1) SET CONFIG_TYPE=Release

call .\build_all.mswin.x86.cmd %CONFIG_TYPE%

set BUILD_STATUS=%ERRORLEVEL% 
if %BUILD_STATUS%==0 goto installer
if not %BUILD_STATUS%==0 goto fail 
 
:fail 
pause 
exit /b %BUILD_STATUS% 
 
:installer 
"C:\Program Files (x86)\NSIS\makensis.exe" .\deploy\gk2_win_setup.nsi
"c:\Program Files\7-zip\7z.exe" a -tzip -mx5 -scsWIN .\deploy\gedkeeper_%GKVER%_win.zip .\deploy\gedkeeper_%GKVER%_winsetup.exe
pause 
exit /b 0
