@echo off

set CONFIG_TYPE=Debug
for %%a in (release Release RELEASE) do if (%%a)==(%1) SET CONFIG_TYPE=Release

rem set MSBDIR=@%WINDIR%\Microsoft.NET\Framework\v4.0.30319
rem %MSBDIR%\msbuild.exe projects\GKv2\GEDKeeper2.sln /p:Configuration=%CONFIG_TYPE% /p:Platform="x86" /t:Rebuild /p:TargetFrameworkVersion=v4.7.1
dotnet build ./projects/GKv3/GEDKeeper3.sln /p:Configuration=%CONFIG_TYPE% /p:Platform="MSWin86"

set BUILD_STATUS=%ERRORLEVEL% 
if %BUILD_STATUS%==0 goto test 
if not %BUILD_STATUS%==0 goto fail 
 
:fail 
pause 
exit /b 1 
 
:test 
rem set NUNIT="nunit_not_found"
rem @if exist "%PROGRAMFILES(X86)%\NUnit 2.6.4\bin\nunit-console-x86.exe" set NUNIT="%PROGRAMFILES(X86)%\NUnit 2.6.4\bin\nunit-console-x86.exe"
rem @if exist "%PROGRAMFILES(X86)%\NUnit.org\nunit-console\nunit3-console.exe" set NUNIT=@"%PROGRAMFILES(X86)%\NUnit.org\nunit-console\nunit3-console.exe" --x86
rem %NUNIT% projects\GKTests\bin\Debug\GKTests.dll
pause 
exit /b 0
