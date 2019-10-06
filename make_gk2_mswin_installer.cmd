@ECHO OFF

SET GK_VER=2.16.2

@ECHO/
@ECHO ***************************************
@ECHO ** Create GEDKeeper setup executable **
@ECHO ***************************************
@ECHO/

set CONFIG_TYPE=Debug
for %%a in (release Release RELEASE) do if (%%a)==(%1) SET CONFIG_TYPE=Release

if not exist .\clean.cmd goto No_Clean
if not exist .\clean_all.cmd goto No_Clean
if not exist .\build_all.mswin.x86.cmd goto No_Builder
if not exist "C:\Program Files (x86)\NSIS\makensis.exe" goto No_NSIS
if not exist "c:\Program Files\7-zip\7z.exe" goto No_7zip

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

:No_Clean
    @ECHO/
    @ECHO ERROR: Cannot find the clean and/or clean_all command in the current directory.
    pause
    exit /b 1

:No_NSIS
    @ECHO/
    @ECHO ERROR: NSIS could not be found.
    pause
    exit /b 1

:No_7zip
    @ECHO/
    @ECHO ERROR: 7-zip could not be found.
    pause
    exit /b 1

:No_Builder
    @ECHO/
    @ECHO ERROR: The build script could not be found.
    pause
    exit /b 1
