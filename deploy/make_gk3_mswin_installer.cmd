@echo off

set APP_VER=3.4.0

call ..\clean.cmd

dotnet build ../projects/GKv3/GEDKeeper3.sln -c MSWin_Release

set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 goto installer
if not %BUILD_STATUS%==0 goto fail

:fail
pause 
exit /b %BUILD_STATUS% 

:installer
rem cd .\deploy
call gk3_win_installer.cmd %APP_VER%
rem cd ..
pause
exit /b 0
