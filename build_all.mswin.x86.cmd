@echo off

@echo/
@echo ****************************
@echo ** GEDKeeper full rebuild **
@echo ****************************
@echo/

:: Asssume a Debug build so we're backward compatible.
set CONFIG_TYPE=Debug
for %%a in (release Release RELEASE) do if (%%a)==(%1) SET CONFIG_TYPE=Release

@echo Configuration type: %CONFIG_TYPE%
@echo/

:: Ensure we are in the correct directory
if exist projects\GEDKeeper2.mswin.sln goto Build_It
@echo ERROR: the project solution file could not be found (projects\GEDKeeper2.mswin.sln)
pause
exit /b 4

:Build_It
    set MSBDIR=%WINDIR%\Microsoft.NET\Framework\v4.0.30319
    @if not exist "%MSBDIR%\*.dll" GOTO ERR_NoFramework
    %MSBDIR%\msbuild.exe projects\GEDKeeper2.mswin.sln /p:Configuration=%CONFIG_TYPE% /p:Platform="x86" /t:Rebuild /p:TargetFrameworkVersion=v4.0

    set BUILD_STATUS=%ERRORLEVEL%
    if %BUILD_STATUS%==0 IF %CONFIG_TYPE%==Release goto End_It
    if %BUILD_STATUS%==0 goto test 
    if not %BUILD_STATUS%==0 goto fail 
 
:fail 
    pause 
    exit /b 1

:test
    call test_win.cmd
    :: @if (%NUNIT%)==() if exist "%PROGRAMFILES(X86)%\NUnit 2.6.4\bin\nunit-console-x86.exe" set NUNIT="%PROGRAMFILES(X86)%\NUnit 2.6.4\bin\nunit-console-x86.exe"
    :: @if (%NUNIT%)==() if exist "%PROGRAMFILES(X86)%\NUnit.org\nunit-console\nunit3-console.exe" set NUNIT="%PROGRAMFILES(X86)%\NUnit.org\nunit-console\nunit3-console.exe" --x86
    :: @if (%NUNIT%)==() goto ERR_NoNUNIT
    :: %NUNIT% projects\GKTests\bin\Debug\GKTests.dll
    pause 
    exit /b 0

:ERR_NoFramework
    @echo/
    @echo ERROR: The required .NET framework was not found at "%MSBDIR%"
    @pause
    exit /b 2

:ERR_NoNUNIT
    @echo/
    @echo ERROR: NUNIT could not be found.
    @pause
    exit /b 3

:End_It
    exit /b 0
