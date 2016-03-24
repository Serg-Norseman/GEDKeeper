rem @echo off

cls

del /q .\GEDKeeper2-Installer.exe

rem del /q .\plugins\*.*
rem del .\BSLib.*
rem del .\GKCommon.*

rem <<< Main assemblies cleaning >>>

rmdir .\src\GEDKeeper2\bin /s /q
rmdir .\src\GEDKeeper2\obj /s /q

rmdir .\src\GKCommon\bin /s /q
rmdir .\src\GKCommon\obj /s /q

rmdir .\src\BSLib\bin /s /q
rmdir .\src\BSLib\obj /s /q

rmdir .\src\BSLibTests\bin /s /q
rmdir .\src\BSLibTests\obj /s /q
rmdir .\src\BSLibTests\OpenCover /s /q
rmdir .\src\BSLibTests\PartCover /s /q

rmdir .\src\GKTests\bin /s /q
rmdir .\src\GKTests\obj /s /q
rmdir .\src\GKTests\OpenCover /s /q
rmdir .\src\GKTests\PartCover /s /q

rem <<< Plugins cleaning >>>

rmdir .\src\GKSamplePlugin\bin /s /q
rmdir .\src\GKSamplePlugin\obj /s /q

rmdir .\src\GKCalculatorPlugin\bin /s /q
rmdir .\src\GKCalculatorPlugin\obj /s /q

rmdir .\src\GKCalendarPlugin\bin /s /q
rmdir .\src\GKCalendarPlugin\obj /s /q

rmdir .\src\GKFlowInputPlugin\bin /s /q
rmdir .\src\GKFlowInputPlugin\obj /s /q

rmdir .\src\GKImageViewerPlugin\bin /s /q
rmdir .\src\GKImageViewerPlugin\obj /s /q

rmdir .\src\GKNamesBookPlugin\bin /s /q
rmdir .\src\GKNamesBookPlugin\obj /s /q

rmdir .\src\GKPedigreeImporterPlugin\bin /s /q
rmdir .\src\GKPedigreeImporterPlugin\obj /s /q

rmdir .\src\GKTextSearchPlugin\bin /s /q
rmdir .\src\GKTextSearchPlugin\obj /s /q

rmdir .\src\GKTimeLinePlugin\bin /s /q
rmdir .\src\GKTimeLinePlugin\obj /s /q

rmdir .\src\GKTreeVizPlugin\bin /s /q
rmdir .\src\GKTreeVizPlugin\obj /s /q

rmdir .\src\GKLifePlugin\bin /s /q
rmdir .\src\GKLifePlugin\obj /s /q
