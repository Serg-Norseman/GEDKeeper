call clean.cmd
call clean_all.cmd

path=C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\MSBuild\15.0\Bin;C:\Program Files (x86)\NUnit 2.6.4\bin

MSBuild.exe .\projects\GEDKeeper2.mswin.sln /t:Rebuild /p:Configuration=Debug /p:Platform="x86"
.\projects\packages\OpenCover\OpenCover.Console.exe -register:user -target:"nunit-console-x86.exe" -targetdir:".\\" -targetargs:"\".\projects\GKTests\bin\Debug\GKTests.dll\" /noxml" -output:".\coverage.xml" -filter:"+[*]* "

pause
