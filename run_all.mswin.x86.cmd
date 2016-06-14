cd projects
rem erase gktests\bin\debug\*.dll
call compile.mswin.bat
cd gktests\bin\debug
"C:\Program Files (x86)\NUnit.org\nunit-console\nunit3-console.exe" GKTests.dll
cd ..\..\..
