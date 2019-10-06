@ECHO OFF

@ECHO/
@ECHO ***************************************
@ECHO ** Run the NUnit tests for GEDKeeper **
@ECHO ***************************************
@ECHO/

:: Set up our environment.
SET Nunit_Console="c:\Program Files (x86)\nunit 2.6.4\bin\nunit-console-x86.exe"
SET Test_DLL=projects\GKTests\bin\Debug\GKTests.dll

:: Set up our log files.
SET LogFile=%Temp%\GEDKeeper_NUNIT_Output.log
SET ErrFile=%Temp%\GEDKeeper_NUNIT_Error.log
@ECHO Log file:   %LogFile%
@ECHO Error file: %ErrFile%
IF EXIST "%LogFile%" Del /f "%LogFile%"
IF EXIST "%ErrFile%" Del /f "%ErrFile%"

:: This will be the exit code we return to the caller.
SET Exit_Code=0

:: Check for our dependencies.
IF NOT EXIST %Nunit_Console% GOTO Err_Nunit
IF NOT EXIST %Test_DLL% GOTO Err_DLL

:: Run the tests.
@ECHO/
%Nunit_Console% %Test_DLL% /output="%LogFile%" /err="%ErrFile%"

SET Exit_Code=%ERRORLEVEL%
IF %Exit_Code%==0 GOTO End
@ECHO/
@ECHO Error encounted: %Exit_Code%
:: PAUSE
GOTO End

:Err_DLL
    @ECHO/
    @ECHO Could not find %Test_DLL%
    SET Exit_Code=100
::    PAUSE
    GOTO End

:Err_Nunit
    @ECHO/
    @ECHO Could not find %Nunit_Console%
    SET Exit_Code=101
::    PAUSE
    GOTO End

:: Do any cleanup and quit.
:End
    IF EXIST "%ErrFile%" %ComSpec% /k "%ErrFile%"
    exit /b %Exit_Code%
