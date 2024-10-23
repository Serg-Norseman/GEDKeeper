@echo off

for /d %%p in (.\appdata\*) do rd "%%p" /s /q
del .\appdata\GEDKeeper2.log
del .\appdata\GEDKeeper2.nms
del .\appdata\*.tmp
