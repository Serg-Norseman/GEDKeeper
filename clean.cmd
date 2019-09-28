@ECHO OFF

@ECHO/
@ECHO ******************************
@ECHO ** Cleaning out built files **
@ECHO ******************************
@ECHO/

if exist .\LICENSE goto Clean_It
@ECHO/
@ECHO ERROR: You are not in the correct directory to run this.
pause
exit /b 1

:Clean_It
    if exist .\deploy\*.exe del /q .\deploy\*.exe

    if exist .\coverageResults.txt del .\coverageResults.txt

    if exist .\cov-int\*.* rmdir .\cov-int /s /q
    if exist .\coverity.zip del .\coverity.zip

    if exist .\.sonarqube\*.* rmdir .\.sonarqube /s /q

    if exist .\GKUpdater.ini del .\GKUpdater.ini
    if exist .\GKUpdater.log del .\GKUpdater.log

    rem <<< Main assemblies cleaning >>>

    if exist .\projects\.vs\*.* rmdir .\projects\.vs /s /q

    if exist .\projects\GKv2\GEDKeeper2\bin\*.* rmdir .\projects\GKv2\GEDKeeper2\bin /s /q
    if exist .\projects\GKv2\GEDKeeper2\obj\*.* rmdir .\projects\GKv2\GEDKeeper2\obj /s /q
    if exist .\projects\GKv2\GEDKeeper2\ProfilingSessions\*.* rmdir .\projects\GKv2\GEDKeeper2\ProfilingSessions /s /q

    if exist .\projects\GKCore\bin\*.* rmdir .\projects\GKCore\bin /s /q
    if exist .\projects\GKCore\obj\*.* rmdir .\projects\GKCore\obj /s /q

    if exist .\projects\GKv2\GKComponents\bin\*.* rmdir .\projects\GKv2\GKComponents\bin /s /q
    if exist .\projects\GKv2\GKComponents\obj\*.* rmdir .\projects\GKv2\GKComponents\obj /s /q

    if exist .\projects\GKIntl\bin\*.* rmdir .\projects\GKIntl\bin /s /q
    if exist .\projects\GKIntl\obj\*.* rmdir .\projects\GKIntl\obj /s /q

    if exist .\projects\GKv2\GKSandbox\bin\*.* rmdir .\projects\GKv2\GKSandbox\bin /s /q
    if exist .\projects\GKv2\GKSandbox\obj\*.* rmdir .\projects\GKv2\GKSandbox\obj /s /q

    if exist .\projects\GKv2\GKTray\bin\*.* rmdir .\projects\GKv2\GKTray\bin /s /q
    if exist .\projects\GKv2\GKTray\obj\*.* rmdir .\projects\GKv2\GKTray\obj /s /q

    if exist .\projects\GKTests\bin\*.* rmdir .\projects\GKTests\bin /s /q
    if exist .\projects\GKTests\obj\*.* rmdir .\projects\GKTests\obj /s /q
    if exist .\projects\GKTests\OpenCover\*.* rmdir .\projects\GKTests\OpenCover /s /q
    if exist .\projects\GKTests\PartCover\*.* rmdir .\projects\GKTests\PartCover /s /q
    if exist .\projects\GKTests\ProfilingSessions\*.* rmdir .\projects\GKTests\ProfilingSessions /s /q

    if exist .\projects\GKUpdater\bin\*.* rmdir .\projects\GKUpdater\bin /s /q
    if exist .\projects\GKUpdater\obj\*.* rmdir .\projects\GKUpdater\obj /s /q

    rem <<< Plugins cleaning >>>

    if exist .\projects\GKv2\GKSamplePlugin\bin\*.* rmdir .\projects\GKv2\GKSamplePlugin\bin /s /q
    if exist .\projects\GKv2\GKSamplePlugin\obj\*.* rmdir .\projects\GKv2\GKSamplePlugin\obj /s /q

    if exist .\projects\GKv2\GKCalculatorPlugin\bin\*.* rmdir .\projects\GKv2\GKCalculatorPlugin\bin /s /q
    if exist .\projects\GKv2\GKCalculatorPlugin\obj\*.* rmdir .\projects\GKv2\GKCalculatorPlugin\obj /s /q

    if exist .\projects\GKv2\GKCalendarPlugin\bin\*.* rmdir .\projects\GKv2\GKCalendarPlugin\bin /s /q
    if exist .\projects\GKv2\GKCalendarPlugin\obj\*.* rmdir .\projects\GKv2\GKCalendarPlugin\obj /s /q

    if exist .\projects\GKv2\GKFlowInputPlugin\bin\*.* rmdir .\projects\GKv2\GKFlowInputPlugin\bin /s /q
    if exist .\projects\GKv2\GKFlowInputPlugin\obj\*.* rmdir .\projects\GKv2\GKFlowInputPlugin\obj /s /q

    if exist .\projects\GKv2\GKImageViewerPlugin\bin\*.* rmdir .\projects\GKv2\GKImageViewerPlugin\bin /s /q
    if exist .\projects\GKv2\GKImageViewerPlugin\obj\*.* rmdir .\projects\GKv2\GKImageViewerPlugin\obj /s /q

    if exist .\projects\GKv2\GKNamesBookPlugin\bin\*.* rmdir .\projects\GKv2\GKNamesBookPlugin\bin /s /q
    if exist .\projects\GKv2\GKNamesBookPlugin\obj\*.* rmdir .\projects\GKv2\GKNamesBookPlugin\obj /s /q

    if exist .\projects\GKv2\GKNavigatorPlugin\bin\*.* rmdir .\projects\GKv2\GKNavigatorPlugin\bin /s /q
    if exist .\projects\GKv2\GKNavigatorPlugin\obj\*.* rmdir .\projects\GKv2\GKNavigatorPlugin\obj /s /q

    if exist .\projects\GKv2\GKPedigreeImporterPlugin\bin\*.* rmdir .\projects\GKv2\GKPedigreeImporterPlugin\bin /s /q
    if exist .\projects\GKv2\GKPedigreeImporterPlugin\obj\*.* rmdir .\projects\GKv2\GKPedigreeImporterPlugin\obj /s /q

    if exist .\projects\GKv2\GKTextSearchPlugin\bin\*.* rmdir .\projects\GKv2\GKTextSearchPlugin\bin /s /q
    if exist .\projects\GKv2\GKTextSearchPlugin\obj\*.* rmdir .\projects\GKv2\GKTextSearchPlugin\obj /s /q

    if exist .\projects\GKv2\GKTimeLinePlugin\bin\*.* rmdir .\projects\GKv2\GKTimeLinePlugin\bin /s /q
    if exist .\projects\GKv2\GKTimeLinePlugin\obj\*.* rmdir .\projects\GKv2\GKTimeLinePlugin\obj /s /q

    if exist .\projects\GKv2\GKTreeVizPlugin\bin\*.* rmdir .\projects\GKv2\GKTreeVizPlugin\bin /s /q
    if exist .\projects\GKv2\GKTreeVizPlugin\obj\*.* rmdir .\projects\GKv2\GKTreeVizPlugin\obj /s /q

    if exist .\projects\GKv2\GKLifePlugin\bin\*.* rmdir .\projects\GKv2\GKLifePlugin\bin /s /q
    if exist .\projects\GKv2\GKLifePlugin\obj\*.* rmdir .\projects\GKv2\GKLifePlugin\obj /s /q

    if exist .\projects\GKv2\GKChroniclePlugin\bin\*.* rmdir .\projects\GKv2\GKChroniclePlugin\bin /s /q
    if exist .\projects\GKv2\GKChroniclePlugin\obj\*.* rmdir .\projects\GKv2\GKChroniclePlugin\obj /s /q

    if exist .\projects\GKv2\GKWordsCloudPlugin\bin\*.* rmdir .\projects\GKv2\GKWordsCloudPlugin\bin /s /q
    if exist .\projects\GKv2\GKWordsCloudPlugin\obj\*.* rmdir .\projects\GKv2\GKWordsCloudPlugin\obj /s /q

    if exist .\projects\GKv2\GKDataQualityPlugin\bin\*.* rmdir .\projects\GKv2\GKDataQualityPlugin\bin /s /q
    if exist .\projects\GKv2\GKDataQualityPlugin\obj\*.* rmdir .\projects\GKv2\GKDataQualityPlugin\obj /s /q

    if exist .\projects\GKv2\GKHistoryDataPlugin\bin\*.* rmdir .\projects\GKv2\GKHistoryDataPlugin\bin /s /q
    if exist .\projects\GKv2\GKHistoryDataPlugin\obj\*.* rmdir .\projects\GKv2\GKHistoryDataPlugin\obj /s /q

    if exist .\projects\GKv2\GKStdReports\bin\*.* rmdir .\projects\GKv2\GKStdReports\bin /s /q
    if exist .\projects\GKv2\GKStdReports\obj\*.* rmdir .\projects\GKv2\GKStdReports\obj /s /q

    if exist .\projects\GKv2\GKFoldersPlugin\bin\*.* rmdir .\projects\GKv2\GKFoldersPlugin\bin /s /q
    if exist .\projects\GKv2\GKFoldersPlugin\obj\*.* rmdir .\projects\GKv2\GKFoldersPlugin\obj /s /q

    if exist .\projects\GKv2\GEDmill\bin\*.* rmdir .\projects\GKv2\GEDmill\bin /s /q
    if exist .\projects\GKv2\GEDmill\obj\*.* rmdir .\projects\GKv2\GEDmill\obj /s /q

    exit /b 0
