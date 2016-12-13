@echo OFF
cls
echo.
echo Delete %APPDATA%\GEDKeeper2\GEDKeeper2.ini ...
echo.
echo Be sure that GEDKeeper is not running.
echo Otherwise terminate GEDKeeper and start this batch file again.
echo.
if exist "%APPDATA%\GEDKeeper2\GEDKeeper2.ini" goto DEL
echo.
echo Config file is not existing (anymore).
echo.
goto END

:DEL
del /F "%APPDATA%\GEDKeeper2\GEDKeeper2.ini"
if exist "%APPDATA%\GEDKeeper2\GEDKeeper2.ini" goto FAIL
echo.
echo Successfully deleted config file.
echo.
goto END

:FAIL
echo.
echo Fail deleting config file.
echo.

:END
pause