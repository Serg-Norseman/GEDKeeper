@echo off
cls

set GKVER=2.20.1

set lstfile=".\listfile.txt"
set out_fn="gedkeeper_%GKVER%_win_portable"
set zip_fn=".\%out_fn%.zip"
set log_fn=".\%out_fn%.log"

echo Processing portable installation start

del .\*.zip /q
del ..\appdata\*.* /q

echo "..\LICENSE" >> %lstfile%

echo "..\bin\" >> %lstfile%
echo "..\appdata\" >> %lstfile%
echo "..\locales\" >> %lstfile%
echo "..\plugins\" >> %lstfile%
echo "..\samples\" >> %lstfile%
echo "..\scripts\" >> %lstfile%

rem "c:\Program Files\7-zip\7z.exe" a -tzip -mx5 -scsWIN %zip_fn% @%lstfile% > %log_fn%
"c:\Program Files\7-zip\7z.exe" a -tzip -mx9 -scsWIN %zip_fn% @%lstfile%
del %lstfile%

echo Processing portable installation complete
