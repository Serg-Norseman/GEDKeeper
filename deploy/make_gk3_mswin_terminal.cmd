@echo off

set APP_VER=3.14.0

call ..\clean.cmd

dotnet build ../projects/GKvT/GEDKeeperT.sln /p:Configuration=Release /p:Platform=MSWin64

set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 goto installer
if not %BUILD_STATUS%==0 goto fail

:fail
pause 
exit /b %BUILD_STATUS% 

:installer
rem cd .\deploy
call gk_win_terminal.cmd %APP_VER% win64
rem cd ..
pause
exit /b 0
