# How to build GEDKeeper

## Dependencies

### Building

* Microsoft [.NET Framework](https://dotnet.microsoft.com/) 4.7.2
* Microsoft [Visual Studio 2017](https://visualstudio.microsoft.com/) or later

### Testing
* [NUnit](https://github.com/nunit) 2.6.4 or 3 (unit testing framework for .NET)

### Packaging

* [7-zip](https://sourceforge.net/projects/sevenzip/) compression
* Nullsoft Scriptable Install System ([NSIS](https://sourceforge.net/projects/nsis/)) 3.04

## For Windows

Run the script `build_all.mswin.x86.cmd` from the Windows command line.

Optionally pass the `release` option to the script to build without
debugging information. By default the script builds the executable with
embedded debugging information. The script skips testing for non-debug
builds.

## For Mono

Run the script `build_all.linux.x64.sh` from the Linux shell.


# The GEDKeeper testing

GEDKeeper uses [NUnit 2.6.4 or 3](http://www.nunit.org/), patches should come with 
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


# Frequently asked questions (FAQ)

**What are the prerequisites I need on my machine to build code in this project?**

On Linux
- Xamarin Studio or MonoDevelop with [Mono](http://www.mono-project.com/) installed.
- NUnit

On Windows

- Microsoft Windows® (Windows Vista or later) x86 or x64 versions 
- Visual Studio 2010 (or later) ([Community edition](https://www.visualstudio.com/en/vs/community/))
  or SharpDevelop 5.1 (or later) ([free IDE for C#](http://www.icsharpcode.net/OpenSource/SD/Download/))
- .NET Framework 4.7.2
- NUnit


**What platforms are supported?**

GEDKeeper is cross-platform and can be run on Linux and Windows. 
In particular, it supports the following platforms:

- Windows Vista (or later) and .NET 4.7.2 (or later)
- Linux (Debian/Ubuntu) and Mono 4.7.2 (or later)

We currently do not support iOS or Android, although it is possible it could 
be added in the future. 


**I am a new developer to the project and I want to build the project**

- First download the source code you want to start with (under the Source Code tab)
- Start selected IDE, then select File->Open -> Project/Solution 
- Select `GEDKeeper2.sln` in the open project dialog box
- Mouse or tab to the solution Explorer Window, right click on the solution and then choose "build solution"


**I found a bug - how do I report it?**

- Click the [Issues](https://github.com/serg-norseman/gedkeeper/issues) link.
- Click the "New Issue" button.
- Choose a descriptive title.
- List the component you found the issue in.
- List all the steps to reproduce the problem and include files or images if necessary.
- Describe the impact to you - like blocks all my development or minor usability issue.


**I have a suggestion for a new feature or some new ideas for the project. 
How do I make a suggestion?**

- Use the [Issues](https://github.com/serg-norseman/gedkeeper/issues) link.
