# How to build GEDKeeper

## Dependencies

### Building

* Microsoft [.NET Framework](https://dotnet.microsoft.com/) 4.0.30319
* Microsoft [Visual Studio 2017](https://visualstudio.microsoft.com/) or later

### Testing
* [NUnit](https://github.com/nunit) 2.6.4 (unit testing framework for .NET)

### Packaging

* [7-zip](https://sourceforge.net/projects/sevenzip/) comporession
* Nullsoft Scriptable Install System ([NSIS](https://sourceforge.net/projects/nsis/)) 3.04

## For Windows

Run the script `build_all_mswin.x86.cmd` from the Windows command line.

Optionally pass the `release` option to the script to build without
debugging information. By default the script builds the executable with
embedded debugging information. The script skips testing for non-debug
builds.

## For Mono

From the Windows command line, run the script `build_all.linux.x86.cmd`.

Optionally, start the Windows 10 bash shell and run the script `build_all.linux.x86.sh`.