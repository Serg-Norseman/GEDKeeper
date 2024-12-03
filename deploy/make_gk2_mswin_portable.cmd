@echo off

set APP_VER=2.32.0

call ..\clean.cmd

set MSBDIR="C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\MSBuild\Current\Bin"
@if exist %MSBDIR%\msbuild.exe goto build

set MSBDIR="C:\Program Files (x86)\Microsoft Visual Studio\2017\BuildTools\MSBuild\15.0\Bin"
@if exist %MSBDIR%\msbuild.exe goto build

echo Build is not possible!
goto quit

:build
%MSBDIR%\msbuild.exe ..\projects\GKv2\GEDKeeper2.sln /p:Configuration=Release /p:Platform="x86" /t:Rebuild /p:TargetFrameworkVersion=v4.7.1 /v:quiet

set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 goto installer
if not %BUILD_STATUS%==0 goto fail

:fail
pause 
exit /b %BUILD_STATUS% 

:installer
rem cd .\deploy
call gk_win_portable.cmd %APP_VER%
rem cd ..
pause
exit /b 0

:quit
