@echo off
cls

set GKVER=%1

del .\*.exe /q
del .\*.zip /q

"C:\Program Files (x86)\NSIS\makensis.exe" /DARCH=x64 .\gk3_win_setup.nsi
"c:\Program Files\7-zip\7z.exe" a -tzip -mx=9 -scsWIN .\gedkeeper_%GKVER%_win64.zip .\gedkeeper_%GKVER%_win64.exe
