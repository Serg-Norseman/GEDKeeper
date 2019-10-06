@echo off

@echo/
@echo **************************************
@echo ** Create GEDKeeper standalone file **
@echo **************************************
@ECHO/

set CONFIG_TYPE=Debug
for %%a in (release Release RELEASE) do if (%%a)==(%1) SET CONFIG_TYPE=Release

@echo/
@echo Preparing for the portable file creation

if exist .\LICENSE goto Make_It
@echo/
@echo ERROR: You are not in the correct directory to run this.
pause
exit /b 1

:Make_It
call .\clean.cmd
call .\clean_all.cmd

call .\build_all.mswin.x86.cmd %CONFIG_TYPE%

set BUILD_STATUS=%ERRORLEVEL% 
if %BUILD_STATUS%==0 goto installer
if not %BUILD_STATUS%==0 goto fail 
 
:fail 
pause 
exit /b 1 
 
:installer 
cd .\deploy
call gk2_win_portable.cmd
cd ..
pause 
exit /b 0
