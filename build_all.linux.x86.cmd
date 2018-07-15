set MSBDIR=@%WINDIR%\Microsoft.NET\Framework\v4.0.30319
set NUNIT=@"C:\Program Files (x86)\NUnit 2.6.4\bin\nunit-console-x86.exe"

%MSBDIR%\msbuild ./projects/GEDKeeper2.linux.sln /p:Configuration=Debug /p:Platform="x86" /p:MonoCS=true /p:TargetFrameworkVersion=v4.5
%NUNIT% ./projects/GKTests/bin/Debug/GKTests.dll
