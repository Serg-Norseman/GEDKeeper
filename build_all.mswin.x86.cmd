@echo off
@%WINDIR%\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe projects\GEDKeeper2.mswin.sln /p:Configuration=Debug /p:Platform="x86"

set BUILD_STATUS=%ERRORLEVEL% 
if %BUILD_STATUS%==0 goto test 
if not %BUILD_STATUS%==0 goto fail 
 
:fail 
pause 
exit /b 1 
 
:test 
"C:\Program Files (x86)\NUnit 2.6.4\bin\nunit-console-x86.exe" projects\GKTests\bin\Debug\GKTests.dll
pause 
exit /b 0
