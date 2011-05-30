
Name "GEDKeeper"
OutFile "GEDKeeper-Installer.exe"
InstallDir $PROGRAMFILES\GEDKeeper
XPStyle on
;ShowInstDetails show
RequestExecutionLevel admin

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\GEDKeeper" "Install_Dir"

; Pages
Page components
Page directory
Page instfiles

LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
LoadLanguageFile "${NSISDIR}\Contrib\Language files\Russian.nlf"

UninstPage uninstConfirm
UninstPage instfiles

Section "GEDKeeper (необходимо)"
  SectionIn RO

  SetOutPath $INSTDIR
  File "GEDKeeper.exe"
  File "GEDKeeper.chm"
  File "lua51.dll"
  File "history.txt"
  File "rus-nobles.ged"

  CreateDirectory "$SMPROGRAMS\GEDKeeper"
  CreateShortCut "$SMPROGRAMS\GEDKeeper\GEDKeeper.lnk" "$INSTDIR\GEDKeeper.exe" "" "$INSTDIR\GEDKeeper.exe" 0
  CreateShortCut "$SMPROGRAMS\GEDKeeper\Справка.lnk" "$INSTDIR\GEDKeeper.chm" "" "$INSTDIR\GEDKeeper.chm" 0
  CreateShortCut "$SMPROGRAMS\GEDKeeper\Благородные фамилии России.lnk" "$INSTDIR\rus-nobles.ged" "" "$INSTDIR\rus-nobles.ged" 0
  CreateShortCut "$SMPROGRAMS\GEDKeeper\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0

  CreateDirectory "$INSTDIR\langs"
  SetOutPath "$INSTDIR\langs"
  File ".\langs\readme.txt"
  File ".\langs\russian.sample"

  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\GEDKeeper "Install_Dir" "$INSTDIR"

  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper" "DisplayName" "GEDKeeper"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
SectionEnd

Section "Примеры скриптов"
  CreateDirectory "$INSTDIR\scripts"
  SetOutPath "$INSTDIR\scripts"

  File ".\scripts\*.lua"
SectionEnd

Section "Регистрация в системе"
  CreateShortCut "$DESKTOP\GEDKeeper.lnk" "$INSTDIR\GEDKeeper.exe" "" "$INSTDIR\GEDKeeper.exe" 0

  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\App Paths\GEDKeeper.exe" "" "$INSTDIR\GEDKeeper.exe"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\App Paths\GEDKeeper.exe" "Path" "$INSTDIR"

  WriteRegStr HKCR ".ged" "" "GEDCOM.File"
  WriteRegStr HKCR "GEDCOM.File" "" "GEDCOM File"
  WriteRegStr HKCR "GEDCOM.File\DefaultIcon" "" "$INSTDIR\GEDKeeper.exe,0"
  WriteRegStr HKCR "GEDCOM.File\shell" "" "open"
  WriteRegStr HKCR "GEDCOM.File\shell\open" "" "&Открыть"
  WriteRegStr HKCR "GEDCOM.File\shell\open\command" "" '$INSTDIR\GEDKeeper.exe "%1"'
SectionEnd

SectionGroup /e "Языки" 

Section "English"
  SetOutPath "$INSTDIR\langs"
  File ".\langs\english.lng"
SectionEnd

Section "Украинский"
  SetOutPath "$INSTDIR\langs"
  File ".\langs\ukrainian.lng"
SectionEnd

SectionGroupEnd

Section "Uninstall"
  ; Remove registry keys
  DeleteRegKey HKCR ".ged"
  DeleteRegKey HKCR "GEDCOM.File"

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\App Paths\GEDKeeper.exe"
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper"
  DeleteRegKey HKLM "SOFTWARE\GEDKeeper"

  ; Remove files and uninstaller
  Delete $INSTDIR\GEDKeeper.exe
  Delete $INSTDIR\GEDKeeper.chm
  Delete $INSTDIR\lua51.dll
  Delete $INSTDIR\history.txt
  Delete $INSTDIR\rus-nobles.ged

  Delete $INSTDIR\uninstall.exe

  Delete "$INSTDIR\samples\*.*"
  RMDir "$INSTDIR\samples"

  Delete "$INSTDIR\scripts\*.lua"
  RMDir "$INSTDIR\scripts"

  Delete "$INSTDIR\langs\*.*"
  RMDir "$INSTDIR\langs"

  ; Remove shortcuts, if any
  Delete "$SMPROGRAMS\GEDKeeper\*.*"
  Delete "$DESKTOP\GEDKeeper.lnk"

  ; Remove directories used
  RMDir "$SMPROGRAMS\GEDKeeper"
  RMDir "$INSTDIR"
SectionEnd
