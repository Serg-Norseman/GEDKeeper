
del /q .\deploy\*.exe

del .\coverageResults.txt

rmdir .\cov-int /s /q
del .\coverity.zip

rmdir .\.sonarqube /s /q

del .\GKUpdater.ini
del .\GKUpdater.log

rem <<< Main assemblies cleaning >>>

rmdir .\projects\GEDKeeper2\bin /s /q
rmdir .\projects\GEDKeeper2\obj /s /q

rmdir .\projects\GKCore\bin /s /q
rmdir .\projects\GKCore\obj /s /q

rmdir .\projects\GKCommon\bin /s /q
rmdir .\projects\GKCommon\obj /s /q

rmdir .\projects\GKMediaPlayer\bin /s /q
rmdir .\projects\GKMediaPlayer\obj /s /q

rmdir .\projects\GKIntl\bin /s /q
rmdir .\projects\GKIntl\obj /s /q

rmdir .\projects\GKSandbox\bin /s /q
rmdir .\projects\GKSandbox\obj /s /q

rmdir .\projects\GKTests\bin /s /q
rmdir .\projects\GKTests\obj /s /q
rmdir .\projects\GKTests\OpenCover /s /q
rmdir .\projects\GKTests\PartCover /s /q

rmdir .\projects\GKUpdater\bin /s /q
rmdir .\projects\GKUpdater\obj /s /q

rem <<< Plugins cleaning >>>

rmdir .\projects\GKSamplePlugin\bin /s /q
rmdir .\projects\GKSamplePlugin\obj /s /q

rmdir .\projects\GKCalculatorPlugin\bin /s /q
rmdir .\projects\GKCalculatorPlugin\obj /s /q

rmdir .\projects\GKCalendarPlugin\bin /s /q
rmdir .\projects\GKCalendarPlugin\obj /s /q

rmdir .\projects\GKFlowInputPlugin\bin /s /q
rmdir .\projects\GKFlowInputPlugin\obj /s /q

rmdir .\projects\GKImageViewerPlugin\bin /s /q
rmdir .\projects\GKImageViewerPlugin\obj /s /q

rmdir .\projects\GKNamesBookPlugin\bin /s /q
rmdir .\projects\GKNamesBookPlugin\obj /s /q

rmdir .\projects\GKNavigatorPlugin\bin /s /q
rmdir .\projects\GKNavigatorPlugin\obj /s /q

rmdir .\projects\GKPedigreeImporterPlugin\bin /s /q
rmdir .\projects\GKPedigreeImporterPlugin\obj /s /q

rmdir .\projects\GKTextSearchPlugin\bin /s /q
rmdir .\projects\GKTextSearchPlugin\obj /s /q

rmdir .\projects\GKTimeLinePlugin\bin /s /q
rmdir .\projects\GKTimeLinePlugin\obj /s /q

rmdir .\projects\GKTreeVizPlugin\bin /s /q
rmdir .\projects\GKTreeVizPlugin\obj /s /q

rmdir .\projects\GKLifePlugin\bin /s /q
rmdir .\projects\GKLifePlugin\obj /s /q

rmdir .\projects\GKChroniclePlugin\bin /s /q
rmdir .\projects\GKChroniclePlugin\obj /s /q
