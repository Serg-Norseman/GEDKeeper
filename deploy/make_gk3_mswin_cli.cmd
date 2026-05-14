@echo off

set APP_VER=3.15.0

call ..\clean.cmd

dotnet build ../projects/GKcli/GKcli.sln /p:Configuration=Release /p:Platform=MSWin64

set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 goto installer
if not %BUILD_STATUS%==0 goto fail

:fail
pause 
exit /b %BUILD_STATUS% 

:installer
rem cd .\deploy
call gk_win_cli.cmd %APP_VER% win64
rem cd ..
pause
exit /b 0
