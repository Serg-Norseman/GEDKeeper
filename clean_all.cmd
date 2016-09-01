
del .\GKCommon.*
del .\GEDKeeper2.exe.*
del .\*.dll
del .\*.pdb
del .\*.xml

del .\coverageResults.txt

del /q .\plugins\*.*

rmdir .\cov-int /s /q
del .\coverity.zip
