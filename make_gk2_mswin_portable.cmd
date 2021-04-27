@echo off

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
cd .\deploy
call gk2_win_portable.cmd
cd ..
pause 
exit /b 0
