call .\clean.cmd
call .\clean_all.cmd

call .\build_all.mswin.x86.cmd

set BUILD_STATUS=%ERRORLEVEL% 
if %BUILD_STATUS%==0 goto installer
if not %BUILD_STATUS%==0 goto fail 
 
:fail 
pause 
exit /b 1 
 
:installer 
"C:\Program Files (x86)\NSIS\makensis.exe" .\deploy\gk2_win_setup.nsi
"c:\Program Files\7-zip\7z.exe" a -tzip -mx5 -scsWIN .\deploy\gedkeeper_2.15.0_win.zip .\deploy\gedkeeper_2.15.0_winsetup.exe
pause 
exit /b 0
