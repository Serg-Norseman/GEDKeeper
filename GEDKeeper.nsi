
Name "GEDKeeper"
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

Section "GEDKeeper (необходимо)"
  SectionIn RO

  SetOutPath $INSTDIR
  File "GEDKeeper.exe"
  File "history.txt"

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

Section "Справка"
  CreateDirectory "$INSTDIR\help"
  SetOutPath "$INSTDIR\help"

  File ".\help\GEDKeeper.htm"
  File ".\help\relations.htm"
  File ".\help\*.gif"
  CreateShortCut "$SMPROGRAMS\GEDKeeper\Справка.lnk" "$INSTDIR\help\GEDKeeper.htm" "" "$INSTDIR\help\GEDKeeper.htm" 0

  File ".\help\genres.htm"
  CreateShortCut "$SMPROGRAMS\GEDKeeper\Ресурсы в Интернете.lnk" "$INSTDIR\help\genres.htm" "" "$INSTDIR\help\genres.htm" 0

  File ".\help\faq.htm"
  CreateShortCut "$SMPROGRAMS\GEDKeeper\Часто задаваемые вопросы.lnk" "$INSTDIR\help\faq.htm" "" "$INSTDIR\help\faq.htm" 0

  File ".\help\ged551-5.pdf"
  CreateShortCut "$SMPROGRAMS\GEDKeeper\Формат GEDCOM.lnk" "$INSTDIR\help\ged551-5.pdf" "" "$INSTDIR\help\ged551-5.pdf" 0

  File ".\help\rus-nobles.ged"
  CreateShortCut "$SMPROGRAMS\GEDKeeper\Благородные фамилии России.lnk" "$INSTDIR\help\rus-nobles.ged" "" "$INSTDIR\help\rus-nobles.ged" 0
SectionEnd

Section "Ярлык на рабочем столе"
  CreateShortCut "$DESKTOP\GEDKeeper.lnk" "$INSTDIR\GEDKeeper.exe" "" "$INSTDIR\GEDKeeper.exe" 0
SectionEnd

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

Section "Uninstall"
  ; Remove registry keys
  DeleteRegKey HKCR ".ged"
  DeleteRegKey HKCR "GEDCOM.File"

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\App Paths\GEDKeeper.exe"
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper"
  DeleteRegKey HKLM "SOFTWARE\GEDKeeper"

  ; Remove files and uninstaller
  Delete $INSTDIR\GEDKeeper.exe
  Delete $INSTDIR\history.txt
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
