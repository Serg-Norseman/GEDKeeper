@echo off
cls
rem "GEDKeeper", the personal genealogical database editor.
rem Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
rem This file is part of "GEDKeeper".

set lstfile=".\listfile.txt"
set out_fn="GEDKeeper-v2.11.0-win-portable"
set zip_fn=".\%out_fn%.zip"
set log_fn=".\%out_fn%.log"

echo Processing portable installation start

echo "..\GEDKeeper2.exe" > %lstfile%
echo "..\GKCommon.dll" >> %lstfile%

echo "..\ArborGVT.dll" >> %lstfile%
echo "..\DotNetRtfWriter.dll" >> %lstfile%
echo "..\ExcelLibrary.dll" >> %lstfile%
echo "..\itextsharp.dll" >> %lstfile%
echo "..\lua51.dll" >> %lstfile%
echo "..\LuaInterface.dll" >> %lstfile%
echo "..\ZedGraph.dll" >> %lstfile%
echo "..\LICENSE" >> %lstfile%
echo "..\plugins\" >> %lstfile%
echo "..\locales\" >> %lstfile%
echo "..\scripts\" >> %lstfile%

rem "c:\Program Files\7-zip\7z.exe" a -tzip -mx5 -scsWIN %zip_fn% @%lstfile% > %log_fn%
"c:\Program Files\7-zip\7z.exe" a -tzip -mx5 -scsWIN %zip_fn% @%lstfile%
del %lstfile%

echo Processing portable installation complete
