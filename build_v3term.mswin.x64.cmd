@echo off

dotnet build ./projects/GKvT/GEDKeeperT.sln /p:Configuration=Release /p:Platform="MSWin64"

set BUILD_STATUS=%ERRORLEVEL% 
if %BUILD_STATUS%==0 goto test 
if not %BUILD_STATUS%==0 goto fail 
 
:fail 
pause 
exit /b 1 
 
:test 
start ./bin/GEDKeeperT.exe
