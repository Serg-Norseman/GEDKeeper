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
cd .\deploy
call gk2_win_portable.cmd
cd ..
pause 
exit /b 0
