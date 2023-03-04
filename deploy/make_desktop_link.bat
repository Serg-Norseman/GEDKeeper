@echo off
cls

set CUR_DIR=%~dp0
echo Current directory: %CUR_DIR%
if exist "%CUR_DIR%\bin\GEDKeeper2.exe" goto start
goto quit

:start
set SHRT_LOCA=%userprofile%\Desktop\GEDKeeper2.url
set SHRT_DEST=%CUR_DIR%\bin\GEDKeeper2.exe
echo [InternetShortcut]> %SHRT_LOCA%
echo URL=file:///%SHRT_DEST%>> %SHRT_LOCA%
echo IconFile=%SHRT_DEST%>> %SHRT_LOCA%
echo IconIndex=^0>> %SHRT_LOCA%

:quit
pause
exit /b 0
