@echo off
cls
rem "GEDKeeper", the personal genealogical database editor.
rem Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
rem This file is part of "GEDKeeper".

set GKVER=2.17.0

set lstfile=".\listfile.txt"
set out_fn="gedkeeper_%GKVER%_win_portable"
set zip_fn=".\%out_fn%.zip"
set log_fn=".\%out_fn%.log"

echo Processing portable installation start

del .\*.zip /q
del ..\appdata\*.* /q

echo "..\GEDKeeper2.exe" > %lstfile%
echo "..\GEDKeeper2.exe.config" >> %lstfile%
echo "..\GKComponents.dll" >> %lstfile%
echo "..\GKCore.dll" >> %lstfile%
echo "..\GKTray.exe" >> %lstfile%

echo "..\ArborGVT.dll" >> %lstfile%
echo "..\BSLib.dll" >> %lstfile%
echo "..\BSLib.Linguistics.dll" >> %lstfile%
echo "..\BSLib.SmartGraph.dll" >> %lstfile%
echo "..\DotNetRtfWriter.dll" >> %lstfile%
echo "..\ExcelLibrary.dll" >> %lstfile%
echo "..\GMap.NET.Core.dll" >> %lstfile%
echo "..\GMap.NET.WindowsForms.dll" >> %lstfile%
echo "..\itextsharp.dll" >> %lstfile%
echo "..\lua51.dll" >> %lstfile%
echo "..\LuaInterface.dll" >> %lstfile%
echo "..\NLog.dll" >> %lstfile%
echo "..\nVLC.dll" >> %lstfile%
echo "..\Ude.dll" >> %lstfile%
echo "..\YamlSerializer.dll" >> %lstfile%
echo "..\ZedGraph.dll" >> %lstfile%

echo "..\LICENSE" >> %lstfile%

echo "..\appdata\" >> %lstfile%
echo "..\locales\" >> %lstfile%
echo "..\plugins\" >> %lstfile%
echo "..\samples\" >> %lstfile%
echo "..\scripts\" >> %lstfile%

rem "c:\Program Files\7-zip\7z.exe" a -tzip -mx5 -scsWIN %zip_fn% @%lstfile% > %log_fn%
"c:\Program Files\7-zip\7z.exe" a -tzip -mx9 -scsWIN %zip_fn% @%lstfile%
del %lstfile%

echo Processing portable installation complete
