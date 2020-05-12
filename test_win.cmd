@CLS

@ECHO OFF
@ECHO ** Run the NUnit tests from the Windows command line **
@ECHO/

:: Set up our environment.
SET Nunit_Console="c:\Program Files (x86)\nunit 2.6.4\bin\nunit-console-x86.exe"
SET Test_DLL=projects\GKTests\bin\Debug\GKTests.dll

:: Check for our dependencies.
IF NOT EXIST %Nunit_Console% GOTO Err_Nunit
IF NOT EXIST %Test_DLL% GOTO Err_DLL

:: Run the tests.
%Nunit_Console% %Test_DLL%

SET Build_Result=%ERRORLEVEL%
IF %Build_Result%==0 GOTO End
@ECHO/
@ECHO Error encounted: %Build_Result%
pause
exit /b %Build_Result%

:Err_DLL
    @ECHO/
    @ECHO Could not find %Test_DLL%
    pause
    exit /b 100

:Err_Nunit
    @ECHO/
    @ECHO Could not find %Nunit_Console%
    pause
    exit /b 101

:End
    exit /b 0
