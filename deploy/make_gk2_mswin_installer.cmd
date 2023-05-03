@echo off

call ..\clean.cmd

set MSBDIR=@%WINDIR%\Microsoft.NET\Framework\v4.0.30319
%MSBDIR%\msbuild.exe ..\projects\GKv2\GEDKeeper2.sln /p:Configuration=Release /p:Platform="x86" /t:Rebuild /p:TargetFrameworkVersion=v4.7.1 /v:quiet

set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 goto installer
if not %BUILD_STATUS%==0 goto fail

:fail
pause 
exit /b %BUILD_STATUS% 

:installer
rem cd .\deploy
call gk2_win_installer.cmd
rem cd ..
pause
exit /b 0
