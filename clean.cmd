
del /q .\deploy\*.exe

del .\coverageResults.txt

rmdir .\cov-int /s /q
del .\coverity.zip

rmdir .\.sonarqube /s /q

del .\GKUpdater.ini
del .\GKUpdater.log

rem <<< Main assemblies cleaning >>>

rmdir .\projects\.vs /s /q

rmdir .\projects\GKv2\GEDKeeper2\bin /s /q
rmdir .\projects\GKv2\GEDKeeper2\obj /s /q

rmdir .\projects\GKCore\bin /s /q
rmdir .\projects\GKCore\obj /s /q

rmdir .\projects\GKv2\GKComponents\bin /s /q
rmdir .\projects\GKv2\GKComponents\obj /s /q

rmdir .\projects\GKIntl\bin /s /q
rmdir .\projects\GKIntl\obj /s /q

rmdir .\projects\GKv2\GKSandbox\bin /s /q
rmdir .\projects\GKv2\GKSandbox\obj /s /q

rmdir .\projects\GKTests\bin /s /q
rmdir .\projects\GKTests\obj /s /q
rmdir .\projects\GKTests\OpenCover /s /q
rmdir .\projects\GKTests\PartCover /s /q

rmdir .\projects\GKUpdater\bin /s /q
rmdir .\projects\GKUpdater\obj /s /q

rem <<< Plugins cleaning >>>

rmdir .\projects\GKv2\GKSamplePlugin\bin /s /q
rmdir .\projects\GKv2\GKSamplePlugin\obj /s /q

rmdir .\projects\GKv2\GKCalculatorPlugin\bin /s /q
rmdir .\projects\GKv2\GKCalculatorPlugin\obj /s /q

rmdir .\projects\GKv2\GKCalendarPlugin\bin /s /q
rmdir .\projects\GKv2\GKCalendarPlugin\obj /s /q

rmdir .\projects\GKv2\GKFlowInputPlugin\bin /s /q
rmdir .\projects\GKv2\GKFlowInputPlugin\obj /s /q

rmdir .\projects\GKv2\GKImageViewerPlugin\bin /s /q
rmdir .\projects\GKv2\GKImageViewerPlugin\obj /s /q

rmdir .\projects\GKv2\GKNamesBookPlugin\bin /s /q
rmdir .\projects\GKv2\GKNamesBookPlugin\obj /s /q

rmdir .\projects\GKv2\GKNavigatorPlugin\bin /s /q
rmdir .\projects\GKv2\GKNavigatorPlugin\obj /s /q

rmdir .\projects\GKv2\GKPedigreeImporterPlugin\bin /s /q
rmdir .\projects\GKv2\GKPedigreeImporterPlugin\obj /s /q

rmdir .\projects\GKv2\GKTextSearchPlugin\bin /s /q
rmdir .\projects\GKv2\GKTextSearchPlugin\obj /s /q

rmdir .\projects\GKv2\GKTimeLinePlugin\bin /s /q
rmdir .\projects\GKv2\GKTimeLinePlugin\obj /s /q

rmdir .\projects\GKv2\GKTreeVizPlugin\bin /s /q
rmdir .\projects\GKv2\GKTreeVizPlugin\obj /s /q

rmdir .\projects\GKv2\GKLifePlugin\bin /s /q
rmdir .\projects\GKv2\GKLifePlugin\obj /s /q

rmdir .\projects\GKv2\GKChroniclePlugin\bin /s /q
rmdir .\projects\GKv2\GKChroniclePlugin\obj /s /q

rmdir .\projects\GKv2\GKWordsCloudPlugin\bin /s /q
rmdir .\projects\GKv2\GKWordsCloudPlugin\obj /s /q

rmdir .\projects\GKv2\GKDataQualityPlugin\bin /s /q
rmdir .\projects\GKv2\GKDataQualityPlugin\obj /s /q
