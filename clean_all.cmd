@ECHO OFF

@ECHO/
@ECHO *****************
@ECHO ** Cleaning up **
@ECHO *****************
@ECHO/

if exist .\LICENSE GOTO Clean_Up
@ECHO/
@ECHO ERROR: You are not in the correct directory to run this.
pause
exit /b 1

:Clean_Up
    if exist .\GKCommon.*                   del /q .\GKCommon.*
    if exist .\GEDKeeper2.exe.*             del /q .\GEDKeeper2.exe.*
    if exist .\GKUpdater.*                  del /q .\GKUpdater.*
    if exist .\GKTray.*                     del /q .\GKTray.*
    if exist .\*.dll                        del /q .\*.dll
    if exist .\*.pdb                        del /q .\*.pdb
    if exist .\*.mdb                        del /q .\*.mdb
    if exist .\*.xml                        del /q .\*.xml
    if exist .\*.log                        del /q .\*.log
    if exist .\plugins\*.*                  del /q .\plugins\*.*
    if exist .\deploy\*.zip                 del /q .\deploy\*.zip

    if exist .\GEDKeeper3.Gtk2.exe.*        del /q .\GEDKeeper3.Gtk2.exe.*
    if exist .\GEDKeeper3.Gtk3.exe.*        del /q .\GEDKeeper3.Gtk3.exe.*
    if exist .\GEDKeeper3.Mac.exe.*         del /q .\GEDKeeper3.Mac.exe.*
    if exist .\GEDKeeper3.WinForms.exe.*    del /q .\GEDKeeper3.WinForms.exe.*
    if exist .\GEDKeeper3.Wpf.exe.*         del /q .\GEDKeeper3.Wpf.exe.*

    exit /b 0
