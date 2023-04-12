@echo off
cls

set GKVER=2.25.2

del .\*.exe /q
del .\*.zip /q

"C:\Program Files (x86)\NSIS\makensis.exe" .\gk2_win_setup.nsi
"c:\Program Files\7-zip\7z.exe" a -tzip -mx5 -scsWIN .\gedkeeper_%GKVER%_win86.zip .\gedkeeper_%GKVER%_win86.exe
