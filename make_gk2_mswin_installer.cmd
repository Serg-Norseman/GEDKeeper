@echo off

call .\clean.cmd

set MSBDIR=@%WINDIR%\Microsoft.NET\Framework\v4.0.30319
%MSBDIR%\msbuild.exe projects\GEDKeeper2.mswin.sln /p:Configuration=Release /p:Platform="x86" /t:Rebuild /p:TargetFrameworkVersion=v4.0 /v:quiet

set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 goto installer
if not %BUILD_STATUS%==0 goto fail

:fail
pause 
exit /b %BUILD_STATUS% 

:installer
cd .\deploy
call gk2_win_installer.cmd
cd ..
pause
exit /b 0
