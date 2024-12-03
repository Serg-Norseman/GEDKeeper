@echo off
cls

set GKVER=2.32.0

del .\*.exe /q
del .\*.zip /q

"C:\Program Files (x86)\NSIS\makensis.exe" .\gk2_win_setup.nsi
"c:\Program Files\7-zip\7z.exe" a -tzip -mx=9 -scsWIN .\gedkeeper_%GKVER%_win86.zip .\gedkeeper_%GKVER%_win86.exe
