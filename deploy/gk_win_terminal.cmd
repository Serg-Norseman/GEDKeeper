@echo off
cls

set GKVER=%1
set GKARCH=%2

set lstfile=".\listfile.txt"
set out_fn="gedkeeper_%GKVER%_%GKARCH%_terminal"
set zip_fn=".\%out_fn%.zip"
set log_fn=".\%out_fn%.log"

echo Processing portable installation start

del .\*.zip /q

del ..\appdata\*.* /q
for /d %%p in (..\appdata\*) do rd "%%p" /s /q
for /d %%p in (..\bin\*) do rd "%%p" /s /q
rem for /d %%p in (..\plugins\*) do rd "%%p" /s /q

echo "..\LICENSE" >> %lstfile%
rem echo ".\make_desktop_link.bat" >> %lstfile%

echo "..\bin\" >> %lstfile%
rem echo "..\plugins\" >> %lstfile%

echo "..\appdata\" >> %lstfile%
echo "..\externals\" >> %lstfile%
echo "..\locales\" >> %lstfile%
echo "..\samples\" >> %lstfile%
rem echo "..\scripts\" >> %lstfile%

"c:\Program Files\7-zip\7z.exe" a -tzip -mx=9 -scsWIN %zip_fn% @%lstfile%
del %lstfile%

echo Processing portable installation complete
