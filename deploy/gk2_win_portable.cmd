@echo off

set GK_VER=2.16.2

cls
@echo GEDKeeper: the personal genealogical database editor.
@echo Copyright © 2009-2019 by Sergey V. Zhdanovskih.
@echo/
@echo This file creates the portable version of GEDKeeper %GK_VER%.

:: Ensure we have the zip progam we expect.
set ZIPR=%PROGRAMFILES%\7-zip\7z.exe
if exist "%ZIPR%" goto check_ged_exe
@echo/
@echo ERROR: Cannot find the file "%ZIPR%".
@echo        Consult the BUILD.md file for more information.
pause
exit /b 100

:: Make sure we are ready to actually make the portable file.
:check_ged_exe
if exist ..\GEDKeeper2.exe goto MakePortable
@echo/
@echo ERROR: You must first create the GEDKeeper executable before running this script.
@echo        Consult the BUILD.md file for more information.
pause
exit /b 101

:MakePortable
set lstfile=".\listfile.txt"
set out_fn="gedkeeper_%GK_VER%_win_portable"
set zip_fn=".\%out_fn%.zip"
set log_fn=".\%out_fn%.log"

@echo/
@echo Processing portable installation start

if not exist "..\appdata\" mkdir ..\appdata

if exist .\gedkeeper_*.zip del .\gedkeeper_*.zip /f /q
if exist ..\appdata\*.* del ..\appdata\*.* /f /q

@echo "..\GEDKeeper2.exe" > %lstfile%
@echo "..\GEDKeeper2.exe.config" >> %lstfile%
@echo "..\GKComponents.dll" >> %lstfile%
@echo "..\GKCore.dll" >> %lstfile%
@echo "..\GKTray.exe" >> %lstfile%

@echo "..\ArborGVT.dll" >> %lstfile%
@echo "..\BSLib.dll" >> %lstfile%
@echo "..\BSLib.Linguistics.dll" >> %lstfile%
@echo "..\BSLib.SmartGraph.dll" >> %lstfile%
@echo "..\DotNetRtfWriter.dll" >> %lstfile%
@echo "..\ExcelLibrary.dll" >> %lstfile%
@echo "..\GMap.NET.Core.dll" >> %lstfile%
@echo "..\GMap.NET.WindowsForms.dll" >> %lstfile%
@echo "..\itextsharp.dll" >> %lstfile%
@echo "..\lua51.dll" >> %lstfile%
@echo "..\LuaInterface.dll" >> %lstfile%
@echo "..\NLog.dll" >> %lstfile%
@echo "..\nVLC.dll" >> %lstfile%
@echo "..\Ude.dll" >> %lstfile%
@echo "..\YamlSerializer.dll" >> %lstfile%
@echo "..\ZedGraph.dll" >> %lstfile%

@echo "..\LICENSE" >> %lstfile%

@echo "..\appdata\" >> %lstfile%
@echo "..\locales\" >> %lstfile%
@echo "..\plugins\" >> %lstfile%
@echo "..\samples\" >> %lstfile%
@echo "..\scripts\" >> %lstfile%

rem "c:\Program Files\7-zip\7z.exe" a -tzip -mx5 -scsWIN %zip_fn% @%lstfile% > %log_fn%
"%ZIPR%" a -tzip -mx9 -scsWIN %zip_fn% @%lstfile%
del %lstfile%

@echo/
@echo Processing portable installation complete

exit /b 0
