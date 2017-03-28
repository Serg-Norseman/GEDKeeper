# The GEDKeeper testing

GEDKeeper uses [NUnit 2.6.4](http://www.nunit.org/), patches should come with 
tests and all tests should pass before a commit or branch is merged.


### Quick start Windows command line

 - Download the [NUnit binaries](http://github.com/nunit/nunitv2/releases/download/2.6.4/NUnit-2.6.4.zip), unzip and add to your path
 - Build GEDKeeper project (usually in Visual Studio or SharpDevelop)
 - `nunit-console-x86.exe projects\GKTests\bin\Debug\GKTests.dll`


### Quick start Mono command line

 - Download the [NUnit binaries](http://github.com/nunit/nunitv2/releases/download/2.6.4/NUnit-2.6.4.zip), unzip and place somewhere accessible
 - Build GEDKeeper project (usually in Xamarin Studio or MonoDevelop)
 - `mono somewhere/nunit-console ./projects/GKTests/bin/Debug/GKTests.dll`

### Running Unit Tests in the IDE

NUnit is not incredibly robust in the IDEs at present, so results are mixed 
but here are some suggestions.

In **Visual Studio**, NUnit has an integrated Visual Studio add-in which can be 
downloaded from their website and used to run tests in the Test Explorer pane.

In **SharpDevelop**, in the Unit Tests window, you can right click on any test, 
and then `Run tests`, which will allow everything to work.
