
del .\bin\* /s /q
for /d %%p in (.\bin\*) do rd "%%p" /s /q

del .\plugins\* /s /q
del .\deploy\*.zip /q
del .\deploy\*.exe /q

del .\appdata\imagecache\*.* /q
del .\appdata\xdb\*.* /q
del .\appdata\ldb\*.* /q
del .\appdata\GEDKeeper2.log
del .\appdata\GEDKeeper2.nms

del .\TestResult.xml
del .\coverageResults.txt
del .\NUnitResults.xml
del .\opencover.xml

rem <<< Main assemblies cleaning >>>

rmdir .\locales\.vs /s /q

rmdir .\projects\.vs /s /q
rmdir .\projects\.idea /s /q

rmdir .\projects\GKCore\bin /s /q
rmdir .\projects\GKCore\obj /s /q

rmdir .\projects\GKTests\bin /s /q
rmdir .\projects\GKTests\obj /s /q
rmdir .\projects\GKTests\OpenCover /s /q
rmdir .\projects\GKTests\PartCover /s /q
rmdir .\projects\GKTests\ProfilingSessions /s /q

rmdir .\projects\GKv2\.vs /s /q
rmdir .\projects\GKv2\GEDKeeper2\bin /s /q
rmdir .\projects\GKv2\GEDKeeper2\obj /s /q
rmdir .\projects\GKv2\GEDKeeper2\ProfilingSessions /s /q

rmdir .\projects\GKv2\GKComponents\bin /s /q
rmdir .\projects\GKv2\GKComponents\obj /s /q

rmdir .\projects\GKv2\GKTray\bin /s /q
rmdir .\projects\GKv2\GKTray\obj /s /q

rem <<< GKMap >>>

rmdir .\projects\GKMap\GKMap.Core\bin /s /q
rmdir .\projects\GKMap\GKMap.Core\obj /s /q

rmdir .\projects\GKMap\GKMap.WinForms\bin /s /q
rmdir .\projects\GKMap\GKMap.WinForms\obj /s /q

rmdir .\projects\GKMap\GKMap.EtoForms\bin /s /q
rmdir .\projects\GKMap\GKMap.EtoForms\obj /s /q

rem <<< GKv3 >>>

rmdir .\projects\GKv3\.vs /s /q
rmdir .\projects\GKv3\.idea /s /q

rmdir .\projects\GKv3\GEDKeeper3\bin /s /q
rmdir .\projects\GKv3\GEDKeeper3\obj /s /q

rmdir .\projects\GKv3\GKComponents\bin /s /q
rmdir .\projects\GKv3\GKComponents\obj /s /q

rem <<< Plugins cleaning >>>

rmdir .\projects\GKv2\GKFlowInputPlugin\bin /s /q
rmdir .\projects\GKv2\GKFlowInputPlugin\obj /s /q

rmdir .\projects\GKv2\GKPedigreeImporterPlugin\bin /s /q
rmdir .\projects\GKv2\GKPedigreeImporterPlugin\obj /s /q

rmdir .\projects\GKv2\GKTextSearchPlugin\bin /s /q
rmdir .\projects\GKv2\GKTextSearchPlugin\obj /s /q

rmdir .\projects\GKv2\GKLifePlugin\bin /s /q
rmdir .\projects\GKv2\GKLifePlugin\obj /s /q

rmdir .\projects\GKv2\GKWordsCloudPlugin\bin /s /q
rmdir .\projects\GKv2\GKWordsCloudPlugin\obj /s /q

rmdir .\projects\GKv2\GEDmill\bin /s /q
rmdir .\projects\GKv2\GEDmill\obj /s /q

rem <<< Cross-platform Plugins cleaning >>>

rmdir .\projects\plugins\GKBackupPlugin\bin /s /q
rmdir .\projects\plugins\GKBackupPlugin\obj /s /q

rmdir .\projects\plugins\GKCalculatorPlugin\bin /s /q
rmdir .\projects\plugins\GKCalculatorPlugin\obj /s /q

rmdir .\projects\plugins\GKCalendarPlugin\bin /s /q
rmdir .\projects\plugins\GKCalendarPlugin\obj /s /q

rmdir .\projects\plugins\GKChroniclePlugin\bin /s /q
rmdir .\projects\plugins\GKChroniclePlugin\obj /s /q

rmdir .\projects\plugins\GKHistoryDataPlugin\bin /s /q
rmdir .\projects\plugins\GKHistoryDataPlugin\obj /s /q

rmdir .\projects\plugins\GKNamesBookPlugin\bin /s /q
rmdir .\projects\plugins\GKNamesBookPlugin\obj /s /q

rmdir .\projects\plugins\GKNavigatorPlugin\bin /s /q
rmdir .\projects\plugins\GKNavigatorPlugin\obj /s /q

rmdir .\projects\plugins\GKStdReports\bin /s /q
rmdir .\projects\plugins\GKStdReports\obj /s /q

rmdir .\projects\plugins\GKTimeLinePlugin\bin /s /q
rmdir .\projects\plugins\GKTimeLinePlugin\obj /s /q
