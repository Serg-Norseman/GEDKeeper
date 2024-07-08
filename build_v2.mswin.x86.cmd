@echo off

set CONFIG_TYPE=Debug
for %%a in (release Release RELEASE) do if (%%a)==(%1) SET CONFIG_TYPE=Release

set MSBDIR="C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\MSBuild\Current\Bin"
@if exist %MSBDIR%\msbuild.exe goto build

set MSBDIR="C:\Program Files (x86)\Microsoft Visual Studio\2017\BuildTools\MSBuild\15.0\Bin"
@if exist %MSBDIR%\msbuild.exe goto build

echo Build is not possible!
goto quit

:build
%MSBDIR%\msbuild.exe projects\GKv2\GEDKeeper2.sln /p:Configuration=%CONFIG_TYPE% /p:Platform="x86" /t:Rebuild /p:TargetFrameworkVersion=v4.7.1

set BUILD_STATUS=%ERRORLEVEL% 
if %BUILD_STATUS%==0 goto test 
if not %BUILD_STATUS%==0 goto fail 
 
:fail 
pause 
exit /b 1 
 
:test 
set NUNIT="nunit_not_found"
@if exist "%PROGRAMFILES(X86)%\NUnit 2.6.4\bin\nunit-console-x86.exe" set NUNIT="%PROGRAMFILES(X86)%\NUnit 2.6.4\bin\nunit-console-x86.exe"
@if exist "%PROGRAMFILES(X86)%\NUnit.org\nunit-console\nunit3-console.exe" set NUNIT=@"%PROGRAMFILES(X86)%\NUnit.org\nunit-console\nunit3-console.exe" --x86
%NUNIT% projects\GKTests\bin\Debug\GKTests.dll
pause 
exit /b 0

:quit
