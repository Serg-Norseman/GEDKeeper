
; The name of the installer
Name "GEDKeeper"

; The file to write
OutFile "GEDKeeper-Installer.exe"

; The default installation directory
InstallDir $PROGRAMFILES\GEDKeeper

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\GEDKeeper" "Install_Dir"

; Request application privileges for Windows Vista
RequestExecutionLevel admin

; Pages
Page components
Page directory
Page instfiles

LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
LoadLanguageFile "${NSISDIR}\Contrib\Language files\Russian.nlf"

UninstPage uninstConfirm
UninstPage instfiles

; The stuff to install
Section "GEDKeeper (необходимо)"
  SectionIn RO

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR

  ; Put file there
  File "GEDKeeper.exe"

  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\GEDKeeper "Install_Dir" "$INSTDIR"

  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper" "DisplayName" "GEDKeeper"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper" "NoRepair" 1
  WriteUninstaller "uninstall.exe"

  CreateDirectory "$SMPROGRAMS\GEDKeeper"
  CreateShortCut "$SMPROGRAMS\GEDKeeper\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut "$SMPROGRAMS\GEDKeeper\GEDKeeper.lnk" "$INSTDIR\GEDKeeper.exe" "" "$INSTDIR\GEDKeeper.exe" 0
SectionEnd

; Optional section (can be disabled by the user)
Section "Справка"
  CreateDirectory "$INSTDIR\help"
  SetOutPath "$INSTDIR\help"
SectionEnd

; Optional section (can be disabled by the user)
Section "Примеры"
  CreateDirectory "$INSTDIR\samples"
  SetOutPath "$INSTDIR\samples"

  ; Put file there
  File ".\samples\rus-nobles.ged"

  CreateShortCut "$SMPROGRAMS\GEDKeeper\Благородные фамилии России.lnk" "$INSTDIR\samples\rus-nobles.ged" "" "$INSTDIR\samples\rus-nobles.ged" 0
SectionEnd

; Optional section (can be disabled by the user)
Section "Ярлык на рабочем столе"
  CreateShortCut "$DESKTOP\GEDKeeper.lnk" "$INSTDIR\GEDKeeper.exe" "" "$INSTDIR\GEDKeeper.exe" 0
SectionEnd

; Optional section (can be disabled by the user)
Section "Регистрация в системе"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\App Paths\GEDKeeper.exe" "" "$INSTDIR\GEDKeeper.exe"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\App Paths\GEDKeeper.exe" "Path" "$INSTDIR"

  WriteRegStr HKCR ".ged" "" "GEDCOM.File"
  WriteRegStr HKCR "GEDCOM.File" "" "GEDCOM File"
  WriteRegStr HKCR "GEDCOM.File\DefaultIcon" "" "$INSTDIR\GEDKeeper.exe,0"
  WriteRegStr HKCR "GEDCOM.File\shell" "" "open"
  WriteRegStr HKCR "GEDCOM.File\shell\open" "" "&Открыть"
  WriteRegStr HKCR "GEDCOM.File\shell\open\command" "" '$INSTDIR\GEDKeeper.exe "%1"'
SectionEnd

; Uninstaller
Section "Uninstall"
  ; Remove registry keys
  DeleteRegKey HKCR ".ged"
  DeleteRegKey HKCR "GEDCOM.File"

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\App Paths\GEDKeeper.exe"
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper"
  DeleteRegKey HKLM "SOFTWARE\GEDKeeper"

  ; Remove files and uninstaller
  Delete $INSTDIR\GEDKeeper.exe
  Delete $INSTDIR\uninstall.exe

  Delete "$INSTDIR\samples\*.*"
  RMDir "$INSTDIR\samples"

  Delete "$INSTDIR\help\*.*"
  RMDir "$INSTDIR\help"

  ; Remove shortcuts, if any
  Delete "$SMPROGRAMS\GEDKeeper\*.*"
  Delete "$DESKTOP\GEDKeeper.lnk"

  ; Remove directories used
  RMDir "$SMPROGRAMS\GEDKeeper"
  RMDir "$INSTDIR"
SectionEnd
