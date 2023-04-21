@echo off
cls

set GKVER=%1

set lstfile=".\listfile.txt"
set out_fn="gedkeeper_%GKVER%_win86_portable"
set zip_fn=".\%out_fn%.zip"
set log_fn=".\%out_fn%.log"

echo Processing portable installation start

del .\*.zip /q

del ..\appdata\*.* /q
for /d %%p in (..\appdata\*) do rd "%%p" /s /q
for /d %%p in (..\bin\*) do rd "%%p" /s /q
for /d %%p in (..\plugins\*) do rd "%%p" /s /q

echo "..\LICENSE" >> %lstfile%
echo ".\make_desktop_link.bat" >> %lstfile%

echo "..\bin\" >> %lstfile%
echo "..\plugins\" >> %lstfile%

echo "..\appdata\" >> %lstfile%
echo "..\backgrounds\" >> %lstfile%
echo "..\externals\" >> %lstfile%
echo "..\locales\" >> %lstfile%
echo "..\samples\" >> %lstfile%
echo "..\scripts\" >> %lstfile%
echo "..\themes\" >> %lstfile%

rem "c:\Program Files\7-zip\7z.exe" a -tzip -mx5 -scsWIN %zip_fn% @%lstfile% > %log_fn%
"c:\Program Files\7-zip\7z.exe" a -tzip -mx=9 -scsWIN %zip_fn% @%lstfile%
del %lstfile%

echo Processing portable installation complete
