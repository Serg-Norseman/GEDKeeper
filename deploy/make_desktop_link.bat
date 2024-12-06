@echo off
cls

set CUR_DIR=%~dp0
echo Current directory: %CUR_DIR%
if exist "%CUR_DIR%\bin\GEDKeeper2.exe" set SHRT_DEST=%CUR_DIR%\bin\GEDKeeper2.exe
if exist "%CUR_DIR%\bin\GEDKeeper3.exe" set SHRT_DEST=%CUR_DIR%\bin\GEDKeeper3.exe
%SHRT_DEST% --createshortcut
exit /b 0
