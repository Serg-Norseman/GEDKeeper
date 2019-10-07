@echo off

set GK_VER=2.16.2

@echo/
@echo ***************************************
@echo ** Create GEDKeeper setup executable **
@echo ***************************************
@echo/

set CONFIG_TYPE=Debug
for %%a in (release Release RELEASE) do if (%%a)==(%1) SET CONFIG_TYPE=Release

if not exist .\clean.cmd goto no_clean
if not exist .\clean_all.cmd goto no_clean
if not exist .\build_all.mswin.x86.cmd goto no_builder
if not exist "C:\Program Files (x86)\NSIS\makensis.exe" goto no_nsis
if not exist "c:\Program Files\7-zip\7z.exe" goto no_7zip

call .\clean.cmd
call .\clean_all.cmd

call .\build_all.mswin.x86.cmd %CONFIG_TYPE%

set BUILD_STATUS=%ERRORLEVEL% 
if %BUILD_STATUS%==0 goto installer
if not %BUILD_STATUS%==0 goto fail 
 
:fail 
pause 
exit /b %BUILD_STATUS%
 
:installer 
"C:\Program Files (x86)\NSIS\makensis.exe" .\deploy\gk2_win_setup.nsi
"c:\Program Files\7-zip\7z.exe" a -tzip -mx5 -scsWIN .\deploy\gedkeeper_%GK_VER%_win.zip .\deploy\gedkeeper_%GK_VER%_winsetup.exe
pause 
exit /b 0

:no_clean
@echo/
@echo ERROR: Cannot find the clean and/or clean_all command in the current directory.
pause
exit /b 1

:no_nsis
@echo/
@echo ERROR: NSIS could not be found.
pause
exit /b 1

:no_7zip
@echo/
@echo ERROR: 7-zip could not be found.
pause
exit /b 1

:no_builder
@echo/
@echo ERROR: The build script could not be found.
pause
exit /b 1
