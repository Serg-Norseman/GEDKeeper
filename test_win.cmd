@echo off

@echo/
@echo ***************************************
@echo ** Run the NUnit tests for GEDKeeper **
@echo ***************************************
@echo/

:: Set up our environment.
set NUNIT_CONSOLE="c:\Program Files (x86)\nunit 2.6.4\bin\nunit-console-x86.exe"
set TEST_DLL=projects\GKTests\bin\Debug\GKTests.dll

:: Set up our log files.
set LOGFILE=%TEMP%\GEDKeeper_NUNIT_Output.log
set ERRFILE=%TEMP%\GEDKeeper_NUNIT_Error.log
@echo Log file:   %LOGFILE%
@echo Error file: %ERRFILE%
if exist "%LOGFILE%" del /f "%LOGFILE%"
if exist "%ERRFILE%" del /f "%ERRFILE%"

:: This will be the exit code we return to the caller.
set EXIT_CODE=0

:: Check for our dependencies.
if not exist %NUNIT_CONSOLE% goto err_nunit
if not exist %TEST_DLL% goto err_dll

:: Run the tests.
@echo/
%NUNIT_CONSOLE% %TEST_DLL% /output="%LOGFILE%" /err="%ERRFILE%"

set EXIT_CODE=%ERRORLEVEL%
IF %EXIT_CODE%==0 goto end
@echo/
@echo Error encounted: %EXIT_CODE%
goto End

:err_dll
@echo/
@echo Could not find %TEST_DLL%
set EXIT_CODE=100
goto End

:err_nunit
@echo/
@echo Could not find %NUNIT_CONSOLE%
set EXIT_CODE=101
goto End

:: Do any cleanup and quit.
:end
if exist "%ERRFILE%" call "%ERRFILE%"
exit /b %EXIT_CODE%
