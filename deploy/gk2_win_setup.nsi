; "GEDKeeper", the personal genealogical database editor.
; Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
; This file is part of "GEDKeeper".

!include "MUI2.nsh"
!include "DotNetChecker.nsh"
!define MUI_ICON "..\projects\GKv2\GEDKeeper2\GEDKeeper2.ico"


Unicode true
Name "GEDKeeper"
OutFile "gedkeeper_2.16.2_winsetup.exe"
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
LangString gkreq ${LANG_ENGLISH} "GEDKeeper2 (required)"
LangString gkscr ${LANG_ENGLISH} "Script samples"
LangString gkreg ${LANG_ENGLISH} "System registration"
LangString gklang ${LANG_ENGLISH} "Languages"
LangString gkplg ${LANG_ENGLISH} "Plugins"
LangString gkp_calc ${LANG_ENGLISH} "Expression calculator"
LangString gkp_calendar ${LANG_ENGLISH} "Calendar"
LangString gkp_nb ${LANG_ENGLISH} "Names book"
LangString gkp_timeline ${LANG_ENGLISH} "Time line"
LangString gkp_flowinput ${LANG_ENGLISH} "Flow input"
LangString gkp_pi ${LANG_ENGLISH} "Pedigrees importer"
LangString gkp_ts ${LANG_ENGLISH} "Text search"
LangString gkp_tv ${LANG_ENGLISH} "3D TreeViz"
LangString gkp_iv ${LANG_ENGLISH} "Image viewer"
LangString gkp_cl ${LANG_ENGLISH} "Conway Life"
LangString gkp_chron ${LANG_ENGLISH} "Chronicle"
LangString gkp_cloud ${LANG_ENGLISH} "Words Cloud"
LangString gkp_dqlt ${LANG_ENGLISH} "Data Quality"
LangString gkp_histdata ${LANG_ENGLISH} "History Data (links)"
LangString gkp_stdreports ${LANG_ENGLISH} "Standard Reports"
LangString gk_shellopen ${LANG_ENGLISH} "Open"

!insertmacro MUI_LANGUAGE "Russian"
LangString gkreq ${LANG_RUSSIAN} "GEDKeeper2 (необходимо)"
LangString gkscr ${LANG_RUSSIAN} "Примеры скриптов"
LangString gkreg ${LANG_RUSSIAN} "Регистрация в системе"
LangString gklang ${LANG_RUSSIAN} "Языки"
LangString gkplg ${LANG_RUSSIAN} "Плагины"
LangString gkp_calc ${LANG_RUSSIAN} "Калькулятор выражений"
LangString gkp_calendar ${LANG_RUSSIAN} "Календарь"
LangString gkp_nb ${LANG_RUSSIAN} "Справочник имен"
LangString gkp_timeline ${LANG_RUSSIAN} "Линия времени"
LangString gkp_flowinput ${LANG_RUSSIAN} "Поточный ввод"
LangString gkp_pi ${LANG_RUSSIAN} "Импорт росписей"
LangString gkp_ts ${LANG_RUSSIAN} "Полнотекстовый поиск"
LangString gkp_tv ${LANG_RUSSIAN} "3D визуализация"
LangString gkp_iv ${LANG_RUSSIAN} "Просмотр изображений"
LangString gkp_cl ${LANG_RUSSIAN} "Игра 'Жизнь Конвея'"
LangString gkp_chron ${LANG_RUSSIAN} "Хроника"
LangString gkp_cloud ${LANG_RUSSIAN} "Облако слов/тэгов"
LangString gkp_dqlt ${LANG_RUSSIAN} "Качество данных"
LangString gkp_histdata ${LANG_RUSSIAN} "Исторические данные (ссылки)"
LangString gkp_stdreports ${LANG_RUSSIAN} "Стандартные отчеты"
LangString gk_shellopen ${LANG_RUSSIAN} "Открыть"

!insertmacro MUI_LANGUAGE "Ukrainian"
LangString gkreq ${LANG_UKRAINIAN} "GEDKeeper2 (потрібний)"
LangString gkscr ${LANG_UKRAINIAN} "Приклади скриптів"
LangString gkreg ${LANG_UKRAINIAN} "Реєстрація в системі"
LangString gklang ${LANG_UKRAINIAN} "Мови"
LangString gkplg ${LANG_UKRAINIAN} "Плагіни"
LangString gkp_calc ${LANG_UKRAINIAN} "Калькулятор"
LangString gkp_calendar ${LANG_UKRAINIAN} "Календар"
LangString gkp_nb ${LANG_UKRAINIAN} "Довідник імен"
LangString gkp_timeline ${LANG_UKRAINIAN} "Линія часу"
LangString gkp_flowinput ${LANG_UKRAINIAN} "Поточне введення"
LangString gkp_pi ${LANG_UKRAINIAN} "Імпорт розписів"
LangString gkp_ts ${LANG_UKRAINIAN} "Повнотекстовий пошук"
LangString gkp_tv ${LANG_UKRAINIAN} "3D візуалізатор"
LangString gkp_iv ${LANG_UKRAINIAN} "Перегляд зображень"
LangString gkp_cl ${LANG_UKRAINIAN} "Гра 'Життя Конвея'"
LangString gkp_chron ${LANG_UKRAINIAN} "Хроніка"
LangString gkp_cloud ${LANG_UKRAINIAN} "Слова хмара"
LangString gkp_dqlt ${LANG_UKRAINIAN} "Якість даних"
LangString gkp_histdata ${LANG_UKRAINIAN} "Історія даних (посилання)"
LangString gkp_stdreports ${LANG_UKRAINIAN} "Стандартні звіти"
LangString gk_shellopen ${LANG_UKRAINIAN} "Відкрити"

!insertmacro MUI_LANGUAGE "Polish"
LangString gkreq ${LANG_POLISH} "GEDKeeper2 (właściwy)"
LangString gkscr ${LANG_POLISH} "Przykłady skryptów"
LangString gkreg ${LANG_POLISH} "Rejestracja w systemie"
LangString gklang ${LANG_POLISH} "Języki"
LangString gkplg ${LANG_POLISH} "Wtyczki"
LangString gkp_calc ${LANG_POLISH} "Kalkulator"
LangString gkp_calendar ${LANG_POLISH} "Kalendarz"
LangString gkp_nb ${LANG_POLISH} "Kieszonkowy nazw"
LangString gkp_timeline ${LANG_POLISH} "Linia czasu"
LangString gkp_flowinput ${LANG_POLISH} "Źródło wejścia"
LangString gkp_pi ${LANG_POLISH} "Importuj rodowód"
LangString gkp_ts ${LANG_POLISH} "Wyszukiwanie pełnotekstowe"
LangString gkp_tv ${LANG_POLISH} "Wizualizacja 3D"
LangString gkp_iv ${LANG_POLISH} "Podgląd zdjęcia"
LangString gkp_cl ${LANG_POLISH} "Gra w życie"
LangString gkp_chron ${LANG_POLISH} "Kronika"
LangString gkp_cloud ${LANG_POLISH} "Chmura słów"
LangString gkp_dqlt ${LANG_POLISH} "Jakość danych"
LangString gkp_histdata ${LANG_POLISH} "Dane historyczne (linki)"
LangString gkp_stdreports ${LANG_POLISH} "Raporty standardowe"
LangString gk_shellopen ${LANG_POLISH} "Otworz"

!insertmacro MUI_LANGUAGE "French"
LangString gkreq ${LANG_FRENCH} "GEDKeeper2 (obligatoire)"
LangString gkscr ${LANG_FRENCH} "Échantillons de script"
LangString gkreg ${LANG_FRENCH} "Enregistrement du système"
LangString gklang ${LANG_FRENCH} "Langues"
LangString gkplg ${LANG_FRENCH} "Plugins"
LangString gkp_calc ${LANG_FRENCH} "Calculateur d'expression"
LangString gkp_calendar ${LANG_FRENCH} "Calendrier"
LangString gkp_nb ${LANG_FRENCH} "Livre des noms"
LangString gkp_timeline ${LANG_FRENCH} "Chronologie"
LangString gkp_flowinput ${LANG_FRENCH} "Flux d'entrée"
LangString gkp_pi ${LANG_FRENCH} "Pedigrees importateur"
LangString gkp_ts ${LANG_FRENCH} "Recherche de texte"
LangString gkp_tv ${LANG_FRENCH} "3D TreeViz"
LangString gkp_iv ${LANG_FRENCH} "Visionneuse d'images"
LangString gkp_cl ${LANG_FRENCH} "Jeu de la vie"
LangString gkp_chron ${LANG_FRENCH} "Chronique"
LangString gkp_cloud ${LANG_FRENCH} "Nuage de mots"
LangString gkp_dqlt ${LANG_FRENCH} "Qualité des données"
LangString gkp_histdata ${LANG_FRENCH} "Données historiques (liens)"
LangString gkp_stdreports ${LANG_FRENCH} "Rapports standards"
LangString gk_shellopen ${LANG_FRENCH} "Ouvrir"

!insertmacro MUI_LANGUAGE "Italian"
LangString gkreq ${LANG_ITALIAN} "GEDKeeper2 (necessario)"
LangString gkscr ${LANG_ITALIAN} "Esempi di script"
LangString gkreg ${LANG_ITALIAN} "Registrazione del sistema"
LangString gklang ${LANG_ITALIAN} "Le lingue"
LangString gkplg ${LANG_ITALIAN} "Plugins"
LangString gkp_calc ${LANG_ITALIAN} "Calcolatrice di espressione"
LangString gkp_calendar ${LANG_ITALIAN} "Calendario"
LangString gkp_nb ${LANG_ITALIAN} "Libro di nomi"
LangString gkp_timeline ${LANG_ITALIAN} "Sequenza temporale"
LangString gkp_flowinput ${LANG_ITALIAN} "Ingresso flusso"
LangString gkp_pi ${LANG_ITALIAN} "Pedigrees importer"
LangString gkp_ts ${LANG_ITALIAN} "Ricerca testo"
LangString gkp_tv ${LANG_ITALIAN} "3D TreeViz"
LangString gkp_iv ${LANG_ITALIAN} "Visualizzatore di immagini"
LangString gkp_cl ${LANG_ITALIAN} "Conway Life"
LangString gkp_chron ${LANG_ITALIAN} "Cronaca"
LangString gkp_cloud ${LANG_ITALIAN} "Nuvola di parole"
LangString gkp_dqlt ${LANG_ITALIAN} "Qualità dei dati"
LangString gkp_histdata ${LANG_ITALIAN} "Dati storici (collegamenti)"
LangString gkp_stdreports ${LANG_ITALIAN} "Rapporti standard"
LangString gk_shellopen ${LANG_ITALIAN} "Apri"

!insertmacro MUI_LANGUAGE "German"
LangString gkreq ${LANG_GERMAN} "GEDKeeper2 (benötigt)"
LangString gkscr ${LANG_GERMAN} "Skript Proben"
LangString gkreg ${LANG_GERMAN} "Systemregistrierung"
LangString gklang ${LANG_GERMAN} "Sprachen"
LangString gkplg ${LANG_GERMAN} "Plugins"
LangString gkp_calc ${LANG_GERMAN} "Ausdruckrechner"
LangString gkp_calendar ${LANG_GERMAN} "Kalender"
LangString gkp_nb ${LANG_GERMAN} "Buch von den Namen"
LangString gkp_timeline ${LANG_GERMAN} "Zeitleiste"
LangString gkp_flowinput ${LANG_GERMAN} "Strömungseingang"
LangString gkp_pi ${LANG_GERMAN} "Importeur von den Stammbäumen"
LangString gkp_ts ${LANG_GERMAN} "Textsuche"
LangString gkp_tv ${LANG_GERMAN} "3D TreeViz/3D-Baum-Visualisierung"
LangString gkp_iv ${LANG_GERMAN} "Bilderbetrachter"
LangString gkp_cl ${LANG_GERMAN} "Conway Spiel des Lebens"
LangString gkp_chron ${LANG_GERMAN} "Chronik"
LangString gkp_cloud ${LANG_GERMAN} "Wörter Wolke"
LangString gkp_dqlt ${LANG_GERMAN} "Datenqualität"
LangString gkp_histdata ${LANG_GERMAN} "Verlaufsdaten (Links)"
LangString gkp_stdreports ${LANG_GERMAN} "Standardberichte"
LangString gk_shellopen ${LANG_GERMAN} "Datei öffnen"

!insertmacro MUI_LANGUAGE "SimpChinese"
LangString gkreq ${LANG_SIMPCHINESE} "GEDKeeper2 (必装)"
LangString gkscr ${LANG_SIMPCHINESE} "脚本样例"
LangString gkreg ${LANG_SIMPCHINESE} "系统注册"
LangString gklang ${LANG_SIMPCHINESE} "语言"
LangString gkplg ${LANG_SIMPCHINESE} "插件"
LangString gkp_calc ${LANG_SIMPCHINESE} "公式计算器"
LangString gkp_calendar ${LANG_SIMPCHINESE} "日历"
LangString gkp_nb ${LANG_SIMPCHINESE} "花名册"
LangString gkp_timeline ${LANG_SIMPCHINESE} "时间轴"
LangString gkp_flowinput ${LANG_SIMPCHINESE} "流输入"
LangString gkp_pi ${LANG_SIMPCHINESE} "家谱导入器"
LangString gkp_ts ${LANG_SIMPCHINESE} "文本检索"
LangString gkp_tv ${LANG_SIMPCHINESE} "3D TreeViz"
LangString gkp_iv ${LANG_SIMPCHINESE} "图片浏览器"
LangString gkp_cl ${LANG_SIMPCHINESE} "康威生命游戏"
LangString gkp_chron ${LANG_SIMPCHINESE} "编年史"
LangString gkp_cloud ${LANG_SIMPCHINESE} "词云"
LangString gkp_dqlt ${LANG_SIMPCHINESE} "数据质量"
LangString gkp_histdata ${LANG_SIMPCHINESE} "历史数据（链接）"
LangString gkp_stdreports ${LANG_SIMPCHINESE} "标准报告"
LangString gk_shellopen ${LANG_SIMPCHINESE} "打开文件"

!insertmacro MUI_LANGUAGE "Czech"
LangString gkreq ${LANG_CZECH} "GEDKeeper2 (požadováno)"
LangString gkscr ${LANG_CZECH} "Vzorky skriptů"
LangString gkreg ${LANG_CZECH} "Registrace systému"
LangString gklang ${LANG_CZECH} "Jazyky"
LangString gkplg ${LANG_CZECH} "Pluginy"
LangString gkp_calc ${LANG_CZECH} "Expresní kalkulačka"
LangString gkp_calendar ${LANG_CZECH} "Kalendář"
LangString gkp_nb ${LANG_CZECH} "Kniha jmen"
LangString gkp_timeline ${LANG_CZECH} "Časový řádek"
LangString gkp_flowinput ${LANG_CZECH} "Průtokový vstup"
LangString gkp_pi ${LANG_CZECH} "Dovozce rodokmenů"
LangString gkp_ts ${LANG_CZECH} "Hledání textu"
LangString gkp_tv ${LANG_CZECH} "3D TreeViz"
LangString gkp_iv ${LANG_CZECH} "Prohlížeč obrázků"
LangString gkp_cl ${LANG_CZECH} "Conway Life"
LangString gkp_chron ${LANG_CZECH} "Kronika"
LangString gkp_cloud ${LANG_CZECH} "Slova Cloud"
LangString gkp_dqlt ${LANG_CZECH} "Kvalita dat"
LangString gkp_histdata ${LANG_CZECH} "Historie dat (odkazů)"
LangString gkp_stdreports ${LANG_CZECH} "Standardní přehledy"
LangString gk_shellopen ${LANG_CZECH} "Otevřete"


; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\GEDKeeper2" "Install_Dir"


; Pages
Page components
Page directory
Page instfiles


function .onInit
    !insertmacro MUI_LANGDLL_DISPLAY
functionEnd

UninstPage uninstConfirm
UninstPage instfiles

Section "$(gkreq)"
    SectionIn RO

    SetOutPath $INSTDIR

    !insertmacro CheckNetFramework 35

    File "..\GEDKeeper2.exe"
    File "..\GEDKeeper2.exe.config"
    File "..\GKComponents.dll"
    File "..\GKCore.dll"
    File "..\GKTray.exe"

    File "..\ArborGVT.dll"
    File "..\BSLib.dll"
    File "..\BSLib.Linguistics.dll"
    File "..\BSLib.SmartGraph.dll"
    File "..\DotNetRtfWriter.dll"
    File "..\ExcelLibrary.dll"
    File "..\GMap.NET.Core.dll"
    File "..\GMap.NET.WindowsForms.dll"
    File "..\itextsharp.dll"
    File "..\lua51.dll"
    File "..\LuaInterface.dll"
    File "..\NLog.dll"
    File "..\nVLC.dll"
    File "..\Ude.dll"
    File "..\YamlSerializer.dll"
    File "..\ZedGraph.dll"

    File "..\LICENSE"

    CreateDirectory "$INSTDIR\locales"
    SetOutPath "$INSTDIR\locales"

    CreateDirectory "$INSTDIR\plugins"
    SetOutPath "$INSTDIR\plugins"

    CreateDirectory "$INSTDIR\samples"
    SetOutPath "$INSTDIR\samples"
    File "..\samples\*.*"

    CreateDirectory "$SMPROGRAMS\GEDKeeper2"
    CreateShortCut "$SMPROGRAMS\GEDKeeper2\GEDKeeper2.lnk" "$INSTDIR\GEDKeeper2.exe" "" "$INSTDIR\GEDKeeper2.exe" 0
    CreateShortCut "$SMPROGRAMS\GEDKeeper2\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0

    ; Write the installation path into the registry
    WriteRegStr HKLM SOFTWARE\GEDKeeper2 "Install_Dir" "$INSTDIR"

    ; Write the uninstall keys for Windows
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper2" "DisplayName" "GEDKeeper2"
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GEDKeeper2" "Publisher" "Sergey V. Zhdanovskih"
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
    WriteRegStr HKCR "GEDCOM.File\shell\open" "" "$(gk_shellopen)"
    WriteRegStr HKCR "GEDCOM.File\shell\open\command" "" '$INSTDIR\GEDKeeper2.exe "%1"'
SectionEnd

SectionGroup /e "$(gklang)"
    Section "English"
        SectionIn RO

        SetOutPath "$INSTDIR\locales"
        File "..\locales\english.lng"
        File "..\locales\readme_eng.html"

        CreateDirectory "$INSTDIR\locales\help_enu"
        SetOutPath "$INSTDIR\locales\help_enu"
        File "..\locales\help_enu\*.*"

        CreateDirectory "$INSTDIR\locales\help_enu\images"
        SetOutPath "$INSTDIR\locales\help_enu\images"
        File "..\locales\help_enu\images\*.*"

        CreateShortCut "$SMPROGRAMS\GEDKeeper2\Manual (English).lnk" "$INSTDIR\locales\help_enu\GEDKeeper2.html" "" "$INSTDIR\locales\help_enu\GEDKeeper2.html" 0
        CreateShortCut "$SMPROGRAMS\GEDKeeper2\Bach family (sample).lnk" "$INSTDIR\samples\Bach_Family.ged" "" "$INSTDIR\samples\Bach_Family.ged" 0
        CreateShortCut "$SMPROGRAMS\GEDKeeper2\Nehru-Ghandi family (sample).lnk" "$INSTDIR\samples\Nehru-Ghandi_Family.ged" "" "$INSTDIR\samples\Nehru-Ghandi_Family.ged" 0
        CreateShortCut "$SMPROGRAMS\GEDKeeper2\Human Mitochondria DNA Haplogroups (sample).lnk" "$INSTDIR\samples\Human_Mitochondria_DNA_Haplogroups.ged" "" "$INSTDIR\samples\Human_Mitochondria_DNA_Haplogroups.ged" 0
        CreateShortCut "$SMPROGRAMS\GEDKeeper2\Human Y-chromosome DNA Haplogroups (sample).lnk" "$INSTDIR\samples\Human_Y-chromosome_DNA_Haplogroups.ged" "" "$INSTDIR\samples\Human_Y-chromosome_DNA_Haplogroups.ged" 0
    SectionEnd

    Section "Русский"
        SetOutPath "$INSTDIR\locales"
        File "..\locales\russian.lng"
        File "..\locales\readme_rus.html"

        CreateDirectory "$INSTDIR\locales\help_rus"
        SetOutPath "$INSTDIR\locales\help_rus"
        File "..\locales\help_rus\*.*"

        CreateDirectory "$INSTDIR\locales\help_rus\images"
        SetOutPath "$INSTDIR\locales\help_rus\images"
        File "..\locales\help_rus\images\*.*"

        CreateShortCut "$SMPROGRAMS\GEDKeeper2\Справка (русский).lnk" "$INSTDIR\locales\help_rus\GEDKeeper2.html" "" "$INSTDIR\locales\help_rus\GEDKeeper2.html" 0
        CreateShortCut "$SMPROGRAMS\GEDKeeper2\Род Пушкиных (пример).lnk" "$INSTDIR\samples\Sample_Russia.ged" "" "$INSTDIR\samples\Sample_Russia.ged" 0
        CreateShortCut "$SMPROGRAMS\GEDKeeper2\Происхождение Человечества (пример).lnk" "$INSTDIR\samples\Human_Origins.ged" "" "$INSTDIR\samples\Human_Origins.ged" 0
        CreateShortCut "$SMPROGRAMS\GEDKeeper2\Древние царства (пример).lnk" "$INSTDIR\samples\Ancient_Kingdoms.ged" "" "$INSTDIR\samples\Ancient_Kingdoms.ged" 0
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

    Section "Italiano"
        SetOutPath "$INSTDIR\locales"
        File "..\locales\italian.lng"
    SectionEnd

    Section "Deutsch"
        SetOutPath "$INSTDIR\locales"
        File "..\locales\german.lng"
    SectionEnd

    Section "中文"
        SetOutPath "$INSTDIR\locales"
        File "..\locales\Chinese Simplified.lng"
    SectionEnd

    Section "Čeština"
        SetOutPath "$INSTDIR\locales"
        File "..\locales\czech.lng"
    SectionEnd
SectionGroupEnd

SectionGroup /e "$(gkplg)"
    Section "$(gkp_calc)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\GKCalculatorPlugin.dll"
        File "..\plugins\GKCalculatorPlugin.rus"
        File "..\plugins\GKCalculatorPlugin.enu"
        File "..\plugins\GKCalculatorPlugin.ukr"
        File "..\plugins\GKCalculatorPlugin.pol"
    SectionEnd

    Section "$(gkp_calendar)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\GKCalendarPlugin.dll"
        File "..\plugins\GKCalendarPlugin.rus"
        File "..\plugins\GKCalendarPlugin.enu"
        File "..\plugins\GKCalendarPlugin.ukr"
        File "..\plugins\GKCalendarPlugin.pol"
    SectionEnd

    Section "$(gkp_nb)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\GKNamesBookPlugin.dll"
        File "..\plugins\GKNamesBookPlugin.rus"
        File "..\plugins\GKNamesBookPlugin.enu"
        File "..\plugins\GKNamesBookPlugin.ukr"
        File "..\plugins\GKNamesBookPlugin.pol"
    SectionEnd

    Section "$(gkp_timeline)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\GKTimeLinePlugin.dll"
        File "..\plugins\GKTimeLinePlugin.rus"
        File "..\plugins\GKTimeLinePlugin.enu"
        File "..\plugins\GKTimeLinePlugin.ukr"
        File "..\plugins\GKTimeLinePlugin.pol"
    SectionEnd

    Section "$(gkp_flowinput)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\GKFlowInputPlugin.dll"
        File "..\plugins\GKFlowInputPlugin.rus"
        File "..\plugins\GKFlowInputPlugin.enu"
        File "..\plugins\GKFlowInputPlugin.ukr"
        File "..\plugins\GKFlowInputPlugin.pol"
    SectionEnd

    Section "$(gkp_pi)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\GKPedigreeImporterPlugin.dll"
        File "..\plugins\GKPedigreeImporterPlugin.rus"
        File "..\plugins\GKPedigreeImporterPlugin.enu"
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
        File "..\plugins\GKTextSearchPlugin.enu"
        File "..\plugins\GKTextSearchPlugin.ukr"
        File "..\plugins\GKTextSearchPlugin.pol"
    SectionEnd

    Section "$(gkp_tv)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\csgl.dll"
        File "..\plugins\csgl.native.dll"
        File "..\plugins\GKTreeVizPlugin.dll"
        File "..\plugins\GKTreeVizPlugin.rus"
        File "..\plugins\GKTreeVizPlugin.enu"
        File "..\plugins\GKTreeVizPlugin.ukr"
        File "..\plugins\GKTreeVizPlugin.pol"
        File "..\plugins\ArborGVT.dll"
    SectionEnd

    Section "$(gkp_iv)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\GKImageViewerPlugin.dll"
        File "..\plugins\GKImageViewerPlugin.rus"
        File "..\plugins\GKImageViewerPlugin.enu"
        File "..\plugins\GKImageViewerPlugin.ukr"
        File "..\plugins\GKImageViewerPlugin.pol"
    SectionEnd

    Section "$(gkp_cl)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\GKLifePlugin.dll"
        File "..\plugins\GKLifePlugin.rus"
        File "..\plugins\GKLifePlugin.enu"
        File "..\plugins\GKLifePlugin.ukr"
        File "..\plugins\GKLifePlugin.pol"
    SectionEnd

    Section "$(gkp_chron)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\GKChroniclePlugin.dll"
        File "..\plugins\GKChroniclePlugin.rus"
        File "..\plugins\GKChroniclePlugin.enu"
        File "..\plugins\GKChroniclePlugin.ukr"
        File "..\plugins\GKChroniclePlugin.pol"
    SectionEnd

    Section "$(gkp_cloud)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\GKWordsCloudPlugin.dll"
        File "..\plugins\GKWordsCloudPlugin.rus"
        File "..\plugins\GKWordsCloudPlugin.enu"
        File "..\plugins\GKWordsCloudPlugin.ukr"
        File "..\plugins\GKWordsCloudPlugin.pol"
    SectionEnd

    Section "$(gkp_dqlt)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\BSLib.DataViz.Model.dll"
        File "..\plugins\BSLib.DataViz.WFControls.dll"
        File "..\plugins\GKDataQualityPlugin.dll"
        File "..\plugins\GKDataQualityPlugin.enu"
        File "..\plugins\GKDataQualityPlugin.rus"
    SectionEnd

    Section "$(gkp_histdata)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\GKHistoryDataPlugin.dll"
        File "..\plugins\GKHistoryDataPlugin.rus"
        File "..\plugins\GKHistoryDataPlugin.enu"
    SectionEnd

    Section "$(gkp_stdreports)"
        SetOutPath "$INSTDIR\plugins"
        File "..\plugins\GKStdReports.dll"
        File "..\plugins\GKStdReports.rus"
        File "..\plugins\GKStdReports.enu"
        File "..\plugins\GKStdReports.fra"
        File "..\plugins\GKStdReports.deu"
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
    Delete $INSTDIR\LICENSE
    Delete $INSTDIR\GEDKeeper2.exe
    Delete $INSTDIR\GEDKeeper2.exe.config
    Delete $INSTDIR\GKComponents.dll
    Delete $INSTDIR\GKCore.dll
    Delete $INSTDIR\GKTray.exe

    Delete $INSTDIR\LinqBridge.dll
    Delete $INSTDIR\NLog.dll
    Delete $INSTDIR\nVLC.dll
    Delete $INSTDIR\YamlSerializer.dll

    Delete $INSTDIR\BSLib.dll
    Delete $INSTDIR\BSLib.Linguistics.dll
    Delete $INSTDIR\BSLib.SmartGraph.dll
    Delete $INSTDIR\GMap.NET.Core.dll
    Delete $INSTDIR\GMap.NET.WindowsForms.dll

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
