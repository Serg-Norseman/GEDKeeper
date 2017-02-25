; "GEDKeeper", the personal genealogical database editor.
; Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
; This file is part of "GEDKeeper".

!include "MUI2.nsh"

Unicode true
Name "GEDKeeper"
OutFile "gedkeeper_2.11.0_winsetup.exe"
InstallDir $PROGRAMFILES\GEDKeeper2

CRCCheck on
SetCompress auto
SetCompressor lzma
SetDatablockOptimize on
AllowRootDirInstall false
XPStyle on

ShowInstDetails show
RequestExecutionLevel admin

!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "Russian"
!insertmacro MUI_LANGUAGE "Ukrainian"
!insertmacro MUI_LANGUAGE "Polish"
!insertmacro MUI_LANGUAGE "French"

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\GEDKeeper2" "Install_Dir"

; Pages
Page components
Page directory
Page instfiles

;LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
;LoadLanguageFile "${NSISDIR}\Contrib\Language files\Russian.nlf"


LangString gkreq ${LANG_ENGLISH} "GEDKeeper2 (required)"
LangString gkreq ${LANG_RUSSIAN} "GEDKeeper2 (необходимо)"
LangString gkreq ${LANG_UKRAINIAN} "GEDKeeper2 (потрібний)"
LangString gkreq ${LANG_POLISH} "GEDKeeper2 (właściwy)"
LangString gkreq ${LANG_FRENCH} "GEDKeeper2 (obligatoire)"

LangString gkscr ${LANG_ENGLISH} "Script samples"
LangString gkscr ${LANG_RUSSIAN} "Примеры скриптов"
LangString gkscr ${LANG_UKRAINIAN} "Приклади скриптів"
LangString gkscr ${LANG_POLISH} "Przykłady skryptów"
LangString gkscr ${LANG_FRENCH} "Échantillons de script"

LangString gkreg ${LANG_ENGLISH} "System registration"
LangString gkreg ${LANG_RUSSIAN} "Регистрация в системе"
LangString gkreg ${LANG_UKRAINIAN} "Реєстрація в системі"
LangString gkreg ${LANG_POLISH} "Rejestracja w systemie"
LangString gkreg ${LANG_FRENCH} "Enregistrement du système"

LangString gklang ${LANG_ENGLISH} "Languages"
LangString gklang ${LANG_RUSSIAN} "Языки"
LangString gklang ${LANG_UKRAINIAN} "Мови"
LangString gklang ${LANG_POLISH} "Języki"
LangString gklang ${LANG_FRENCH} "Langues"

LangString gkplg ${LANG_ENGLISH} "Plugins"
LangString gkplg ${LANG_RUSSIAN} "Плагины"
LangString gkplg ${LANG_UKRAINIAN} "Плагіни"
LangString gkplg ${LANG_POLISH} "Wtyczki"
LangString gkplg ${LANG_FRENCH} "Plugins"


LangString gkp_calc ${LANG_ENGLISH} "Expression calculator"
LangString gkp_calc ${LANG_RUSSIAN} "Калькулятор выражений"
LangString gkp_calc ${LANG_UKRAINIAN} "Калькулятор"
LangString gkp_calc ${LANG_POLISH} "Kalkulator"
LangString gkp_calc ${LANG_FRENCH} "Calculateur d'expression"

LangString gkp_calendar ${LANG_ENGLISH} "Calendar"
LangString gkp_calendar ${LANG_RUSSIAN} "Календарь"
LangString gkp_calendar ${LANG_UKRAINIAN} "Календар"
LangString gkp_calendar ${LANG_POLISH} "Kalendarz"
LangString gkp_calendar ${LANG_FRENCH} "Calendrier"

LangString gkp_nb ${LANG_ENGLISH} "Names book"
LangString gkp_nb ${LANG_RUSSIAN} "Справочник имен"
LangString gkp_nb ${LANG_UKRAINIAN} "Довідник імен"
LangString gkp_nb ${LANG_POLISH} "Kieszonkowy nazw"
LangString gkp_nb ${LANG_FRENCH} "Livre des noms"

LangString gkp_timeline ${LANG_ENGLISH} "Time line"
LangString gkp_timeline ${LANG_RUSSIAN} "Линия времени"
LangString gkp_timeline ${LANG_UKRAINIAN} "Линія часу"
LangString gkp_timeline ${LANG_POLISH} "Linia czasu"
LangString gkp_timeline ${LANG_FRENCH} "Chronologie"

LangString gkp_flowinput ${LANG_ENGLISH} "Flow input"
LangString gkp_flowinput ${LANG_RUSSIAN} "Поточный ввод"
LangString gkp_flowinput ${LANG_UKRAINIAN} "Поточне введення"
LangString gkp_flowinput ${LANG_POLISH} "Źródło wejścia"
LangString gkp_flowinput ${LANG_FRENCH} "Flux d'entrée"

LangString gkp_pi ${LANG_ENGLISH} "Pedigrees importer"
LangString gkp_pi ${LANG_RUSSIAN} "Импорт росписей"
LangString gkp_pi ${LANG_UKRAINIAN} "Імпорт розписів"
LangString gkp_pi ${LANG_POLISH} "Importuj rodowód"
LangString gkp_pi ${LANG_FRENCH} "Pedigrees importateur"

LangString gkp_ts ${LANG_ENGLISH} "Text search"
LangString gkp_ts ${LANG_RUSSIAN} "Полнотекстовый поиск"
LangString gkp_ts ${LANG_UKRAINIAN} "Повнотекстовий пошук"
LangString gkp_ts ${LANG_POLISH} "Wyszukiwanie pełnotekstowe"
LangString gkp_ts ${LANG_FRENCH} "Recherche de texte"

LangString gkp_tv ${LANG_ENGLISH} "3D TreeViz"
LangString gkp_tv ${LANG_RUSSIAN} "3D визуализация"
LangString gkp_tv ${LANG_UKRAINIAN} "3D візуалізатор"
LangString gkp_tv ${LANG_POLISH} "Wizualizacja 3D"
LangString gkp_tv ${LANG_FRENCH} "3D TreeViz"

LangString gkp_iv ${LANG_ENGLISH} "Image viewer"
LangString gkp_iv ${LANG_RUSSIAN} "Просмотр изображений"
LangString gkp_iv ${LANG_UKRAINIAN} "Перегляд зображень"
LangString gkp_iv ${LANG_POLISH} "Podgląd zdjęcia"
LangString gkp_iv ${LANG_FRENCH} "Visionneuse d'images"

LangString gkp_cl ${LANG_ENGLISH} "Conway Life"
LangString gkp_cl ${LANG_RUSSIAN} "Игра 'Жизнь Конвея'"
LangString gkp_cl ${LANG_UKRAINIAN} "Гра 'Життя Конвея'"
LangString gkp_cl ${LANG_POLISH} "Gra w życie"
LangString gkp_cl ${LANG_FRENCH} "Jeu de la vie"


function .onInit
  !insertmacro MUI_LANGDLL_DISPLAY
functionEnd

UninstPage uninstConfirm
UninstPage instfiles

Section "$(gkreq)"
  SectionIn RO

  SetOutPath $INSTDIR

  File "..\GEDKeeper2.exe"
  File "..\GKCommon.dll"

  File "..\ArborGVT.dll"
  File "..\DotNetRtfWriter.dll"
  File "..\ExcelLibrary.dll"
  File "..\itextsharp.dll"
  File "..\lua51.dll"
  File "..\LuaInterface.dll"
  File "..\ZedGraph.dll"

  CreateDirectory "$INSTDIR\locales"
  SetOutPath "$INSTDIR\locales"
  File "..\locales\readme_rus.html"
  File "..\locales\readme_eng.html"
  File "..\locales\english.sample"

  CreateDirectory "$INSTDIR\plugins"
  SetOutPath "$INSTDIR\plugins"
  File "..\plugins\GKCommon.dll"

  CreateDirectory "$INSTDIR\samples"
  SetOutPath "$INSTDIR\samples"
  File "..\samples\*.*"

  CreateDirectory "$INSTDIR\locales\help_enu"
  SetOutPath "$INSTDIR\locales\help_enu"
  File "..\locales\help_enu\*.*"

  CreateDirectory "$INSTDIR\locales\help_enu\images"
  SetOutPath "$INSTDIR\locales\help_enu\images"
  File "..\locales\help_enu\images\*.*"

  CreateDirectory "$SMPROGRAMS\GEDKeeper2"
  CreateShortCut "$SMPROGRAMS\GEDKeeper2\GEDKeeper2.lnk" "$INSTDIR\GEDKeeper2.exe" "" "$INSTDIR\GEDKeeper2.exe" 0
  CreateShortCut "$SMPROGRAMS\GEDKeeper2\Help (English).lnk" "$INSTDIR\locales\help_enu\GEDKeeper2.html" "" "$INSTDIR\locales\help_enu\GEDKeeper2.html" 0
  CreateShortCut "$SMPROGRAMS\GEDKeeper2\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0

  CreateShortCut "$SMPROGRAMS\GEDKeeper2\Bach family (sample).lnk" "$INSTDIR\samples\Bach_Family.ged" "" "$INSTDIR\samples\Bach_Family.ged" 0
  CreateShortCut "$SMPROGRAMS\GEDKeeper2\Nehru-Ghandi family (sample).lnk" "$INSTDIR\samples\Nehru-Ghandi_Family.ged" "" "$INSTDIR\samples\Nehru-Ghandi_Family.ged" 0
  CreateShortCut "$SMPROGRAMS\GEDKeeper2\Human Mitochondria DNA Haplogroups (sample).lnk" "$INSTDIR\samples\Human_Mitochondria_DNA_Haplogroups.ged" "" "$INSTDIR\samples\Human_Mitochondria_DNA_Haplogroups.ged" 0
  CreateShortCut "$SMPROGRAMS\GEDKeeper2\Human Y-chromosome DNA Haplogroups (sample).lnk" "$INSTDIR\samples\Human_Y-chromosome_DNA_Haplogroups.ged" "" "$INSTDIR\samples\Human_Y-chromosome_DNA_Haplogroups.ged" 0

  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\GEDKeeper2 "Install_Dir" "$INSTDIR"

  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper2" "DisplayName" "GEDKeeper2"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper2" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper2" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper2" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
SectionEnd

Section "$(gkscr)"
  CreateDirectory "$INSTDIR\scripts"
  SetOutPath "$INSTDIR\scripts"

  File "..\scripts\*.lua"
SectionEnd

Section "$(gkreg)"
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

SectionGroup /e "$(gklang)"
	Section "Русский"
  		SetOutPath "$INSTDIR\locales"
  		File "..\locales\russian.lng"

		CreateDirectory "$INSTDIR\locales\help_rus"
		SetOutPath "$INSTDIR\locales\help_rus"
		File "..\locales\help_rus\*.*"

		CreateDirectory "$INSTDIR\locales\help_rus\images"
		SetOutPath "$INSTDIR\locales\help_rus\images"
		File "..\locales\help_rus\images\*.*"

		CreateShortCut "$SMPROGRAMS\GEDKeeper2\Справка (русский).lnk" "$INSTDIR\locales\help_rus\GEDKeeper2.html" "" "$INSTDIR\locales\help_rus\GEDKeeper2.html" 0
		CreateShortCut "$SMPROGRAMS\GEDKeeper2\Род Пушкиных (пример).lnk" "$INSTDIR\samples\Sample_Russia.ged" "" "$INSTDIR\samples\Sample_Russia.ged" 0
	SectionEnd

	Section "Українська"
  		SetOutPath "$INSTDIR\locales"
  		File "..\locales\ukrainian.lng"
	SectionEnd

	Section "Polski"
		SetOutPath "$INSTDIR\locales"
		File "..\locales\polish.lng"
	SectionEnd

	Section "Français"
		SetOutPath "$INSTDIR\locales"
		File "..\locales\french.lng"
	SectionEnd
SectionGroupEnd

SectionGroup /e "$(gkplg)"
	Section "$(gkp_calc)"
  		SetOutPath "$INSTDIR\plugins"
  		File "..\plugins\GKCalculatorPlugin.dll"
  		File "..\plugins\GKCalculatorPlugin.rus"
  		File "..\plugins\GKCalculatorPlugin.eng"
  		File "..\plugins\GKCalculatorPlugin.ukr"
		File "..\plugins\GKCalculatorPlugin.pol"
	SectionEnd

	Section "$(gkp_calendar)"
  		SetOutPath "$INSTDIR\plugins"
  		File "..\plugins\GKCalendarPlugin.dll"
  		File "..\plugins\GKCalendarPlugin.rus"
  		File "..\plugins\GKCalendarPlugin.eng"
  		File "..\plugins\GKCalendarPlugin.ukr"
		File "..\plugins\GKCalendarPlugin.pol"
	SectionEnd

	Section "$(gkp_nb)"
  		SetOutPath "$INSTDIR\plugins"
  		File "..\plugins\GKNamesBookPlugin.dll"
  		File "..\plugins\GKNamesBookPlugin.rus"
  		File "..\plugins\GKNamesBookPlugin.eng"
  		File "..\plugins\GKNamesBookPlugin.ukr"
		File "..\plugins\GKNamesBookPlugin.pol"
	SectionEnd

	Section "$(gkp_timeline)"
  		SetOutPath "$INSTDIR\plugins"
  		File "..\plugins\GKTimeLinePlugin.dll"
  		File "..\plugins\GKTimeLinePlugin.rus"
  		File "..\plugins\GKTimeLinePlugin.eng"
  		File "..\plugins\GKTimeLinePlugin.ukr"
		File "..\plugins\GKTimeLinePlugin.pol"
	SectionEnd

	Section "$(gkp_flowinput)"
  		SetOutPath "$INSTDIR\plugins"
  		File "..\plugins\GKFlowInputPlugin.dll"
  		File "..\plugins\GKFlowInputPlugin.rus"
  		File "..\plugins\GKFlowInputPlugin.eng"
  		File "..\plugins\GKFlowInputPlugin.ukr"
		File "..\plugins\GKFlowInputPlugin.pol"
	SectionEnd

	Section "$(gkp_pi)"
  		SetOutPath "$INSTDIR\plugins"
  		File "..\plugins\GKPedigreeImporterPlugin.dll"
  		File "..\plugins\GKPedigreeImporterPlugin.rus"
  		File "..\plugins\GKPedigreeImporterPlugin.eng"
  		File "..\plugins\GKPedigreeImporterPlugin.ukr"
		File "..\plugins\GKPedigreeImporterPlugin.pol"
	SectionEnd

	Section "$(gkp_ts)"
  		SetOutPath "$INSTDIR\plugins"
  		File "..\plugins\_XapianSharp.dll"
  		File "..\plugins\XapianCSharp.dll"
  		File "..\plugins\zlib1.dll"
		File "..\plugins\GKTextSearchPlugin.dll"
  		File "..\plugins\GKTextSearchPlugin.rus"
  		File "..\plugins\GKTextSearchPlugin.eng"
  		File "..\plugins\GKTextSearchPlugin.ukr"
		File "..\plugins\GKTextSearchPlugin.pol"
	SectionEnd

	Section "$(gkp_tv)"
  		SetOutPath "$INSTDIR\plugins"
  		File "..\plugins\csgl.dll"
  		File "..\plugins\csgl.native.dll"
		File "..\plugins\GKTreeVizPlugin.dll"
  		File "..\plugins\GKTreeVizPlugin.rus"
  		File "..\plugins\GKTreeVizPlugin.eng"
  		File "..\plugins\GKTreeVizPlugin.ukr"
		File "..\plugins\GKTreeVizPlugin.pol"
		File "..\plugins\ArborGVT.dll"
	SectionEnd

	Section "$(gkp_iv)"
  		SetOutPath "$INSTDIR\plugins"
		File "..\plugins\GKImageViewerPlugin.dll"
  		File "..\plugins\GKImageViewerPlugin.rus"
  		File "..\plugins\GKImageViewerPlugin.eng"
  		File "..\plugins\GKImageViewerPlugin.ukr"
		File "..\plugins\GKImageViewerPlugin.pol"
	SectionEnd

	Section "$(gkp_cl)"
  		SetOutPath "$INSTDIR\plugins"
  		File "..\plugins\ConwayLife.dll"
  		File "..\plugins\GKLifePlugin.dll"
  		File "..\plugins\GKLifePlugin.rus"
  		File "..\plugins\GKLifePlugin.eng"
  		File "..\plugins\GKLifePlugin.ukr"
		File "..\plugins\GKLifePlugin.pol"
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
  Delete $INSTDIR\GKCommon.dll

  Delete $INSTDIR\ArborGVT.dll
  Delete $INSTDIR\DotNetRtfWriter.dll
  Delete $INSTDIR\ExcelLibrary.dll
  Delete $INSTDIR\itextsharp.dll
  Delete $INSTDIR\lua51.dll
  Delete $INSTDIR\LuaInterface.dll
  Delete $INSTDIR\ZedGraph.dll

  Delete $INSTDIR\uninstall.exe

  Delete "$INSTDIR\locales\help_rus\images\*.*"
  RMDir "$INSTDIR\locales\help_rus\images"

  Delete "$INSTDIR\locales\help_rus\*.*"
  RMDir "$INSTDIR\locales\help_rus"

  Delete "$INSTDIR\locales\help_enu\images\*.*"
  RMDir "$INSTDIR\locales\help_enu\images"

  Delete "$INSTDIR\locales\help_enu\*.*"
  RMDir "$INSTDIR\locales\help_enu"

  Delete "$INSTDIR\locales\*.*"
  RMDir "$INSTDIR\locales"

  Delete "$INSTDIR\scripts\*.lua"
  RMDir "$INSTDIR\scripts"

  Delete "$INSTDIR\plugins\*.*"
  RMDir "$INSTDIR\plugins"

  Delete "$INSTDIR\samples\*.*"
  RMDir "$INSTDIR\samples"

  ; Remove shortcuts, if any
  Delete "$SMPROGRAMS\GEDKeeper2\*.*"
  Delete "$DESKTOP\GEDKeeper2.lnk"

  ; Remove directories used
  RMDir "$SMPROGRAMS\GEDKeeper2"
  RMDir "$INSTDIR"
SectionEnd
