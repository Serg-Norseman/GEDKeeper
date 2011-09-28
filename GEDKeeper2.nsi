
Name "GEDKeeper2"
OutFile "GEDKeeper2-Installer.exe"
InstallDir $PROGRAMFILES\GEDKeeper2
XPStyle on
;ShowInstDetails show
RequestExecutionLevel admin

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\GEDKeeper2" "Install_Dir"

; Pages
Page components
Page directory
Page instfiles

LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
LoadLanguageFile "${NSISDIR}\Contrib\Language files\Russian.nlf"

UninstPage uninstConfirm
UninstPage instfiles

Section "GEDKeeper2 (необходимо)"
  SectionIn RO

  SetOutPath $INSTDIR
  File "GEDKeeper2.exe"
  File "GEDKeeper2.chm"
  File "history.txt"
  File "rus-nobles.ged"

  File "_XapianSharp.dll"
  File "lua51.dll"
  File "LuaInterface.dll"
  File "XapianCSharp.dll"
  File "ZedGraph.dll"
  File "ZedGraph.xml"
  File "zlib1.dll"

  CreateDirectory "$SMPROGRAMS\GEDKeeper2"
  CreateShortCut "$SMPROGRAMS\GEDKeeper2\GEDKeeper2.lnk" "$INSTDIR\GEDKeeper2.exe" "" "$INSTDIR\GEDKeeper2.exe" 0
  CreateShortCut "$SMPROGRAMS\GEDKeeper2\Справка.lnk" "$INSTDIR\GEDKeeper2.chm" "" "$INSTDIR\GEDKeeper2.chm" 0
  CreateShortCut "$SMPROGRAMS\GEDKeeper2\Благородные фамилии России.lnk" "$INSTDIR\rus-nobles.ged" "" "$INSTDIR\rus-nobles.ged" 0
  CreateShortCut "$SMPROGRAMS\GEDKeeper2\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0

  CreateDirectory "$INSTDIR\langs"
  SetOutPath "$INSTDIR\langs"
  File ".\langs\readme.txt"
  File ".\langs\russian.sample"

  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\GEDKeeper2 "Install_Dir" "$INSTDIR"

  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper2" "DisplayName" "GEDKeeper2"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper2" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper2" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper2" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
SectionEnd

Section "Примеры скриптов"
  CreateDirectory "$INSTDIR\scripts"
  SetOutPath "$INSTDIR\scripts"

  File ".\scripts\*.lua"
SectionEnd

Section "Регистрация в системе"
  CreateShortCut "$DESKTOP\GEDKeeper2.lnk" "$INSTDIR\GEDKeeper2.exe" "" "$INSTDIR\GEDKeeper2.exe" 0

  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\App Paths\GEDKeeper2.exe" "" "$INSTDIR\GEDKeeper2.exe"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\App Paths\GEDKeeper2.exe" "Path" "$INSTDIR"

  WriteRegStr HKCR ".ged" "" "GEDCOM.File"
  WriteRegStr HKCR "GEDCOM.File" "" "GEDCOM File"
  WriteRegStr HKCR "GEDCOM.File\DefaultIcon" "" "$INSTDIR\GEDKeeper2.exe,0"
  WriteRegStr HKCR "GEDCOM.File\shell" "" "open"
  WriteRegStr HKCR "GEDCOM.File\shell\open" "" "&Открыть"
  WriteRegStr HKCR "GEDCOM.File\shell\open\command" "" '$INSTDIR\GEDKeeper2.exe "%1"'
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

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\App Paths\GEDKeeper2.exe"
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper2"
  DeleteRegKey HKLM "SOFTWARE\GEDKeeper2"

  ; Remove files and uninstaller
  Delete $INSTDIR\GEDKeeper2.exe
  Delete $INSTDIR\GEDKeeper2.chm
  Delete $INSTDIR\history.txt
  Delete $INSTDIR\rus-nobles.ged

  Delete $INSTDIR\_XapianSharp.dll
  Delete $INSTDIR\lua51.dll
  Delete $INSTDIR\LuaInterface.dll
  Delete $INSTDIR\XapianCSharp.dll
  Delete $INSTDIR\ZedGraph.dll
  Delete $INSTDIR\ZedGraph.xml
  Delete $INSTDIR\zlib1.dll

  Delete $INSTDIR\uninstall.exe

  Delete "$INSTDIR\samples\*.*"
  RMDir "$INSTDIR\samples"

  Delete "$INSTDIR\scripts\*.lua"
  RMDir "$INSTDIR\scripts"

  Delete "$INSTDIR\langs\*.*"
  RMDir "$INSTDIR\langs"

  ; Remove shortcuts, if any
  Delete "$SMPROGRAMS\GEDKeeper2\*.*"
  Delete "$DESKTOP\GEDKeeper2.lnk"

  ; Remove directories used
  RMDir "$SMPROGRAMS\GEDKeeper2"
  RMDir "$INSTDIR"
SectionEnd
