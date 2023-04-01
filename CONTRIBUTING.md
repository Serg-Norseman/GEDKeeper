# How to contribute

No code is perfect and we always welcome third-party patches. This document describes guidelines that we need contributors to follow so that we can accept their pull requests.

## Getting started

- Make sure you have an account on this site,
- Submit a ticket for your issue, assuming one does not already exists.
    - Write in English,
    - Use the imperative mood in the subject/title field,
    - Clearly describe the issue including steps to reproduce when it is a bug,
    - Make sure you fill in the earliest version that you know has the issue.
- Fork the repository on this site and `git-clone(1)` it locally.

## Making changes

- Create a topic branch from where you want to base your work,
- Make your changes taking [style guide](CODINGSTYLE.md) into account,
- Check for whitespace errors issuing `git diff --check`, `git diff --cached --check` or `git diff HEAD --check`,
- Make commits writing good [commit messages](http://chris.beams.io/posts/git-commit/),

## Submitting changes

- Push your changes to a topic branch in your fork of the repository,
- Submit a pull request to the project's repository,
- Update your ticket to mark you have submitted code and are ready for it to be reviewed. Include a link to the pull request in the ticket.


# GEDKeeper contributors

* **[Sergey Zhdanovskih](https://github.com/serg-norseman)**

  * Lead developer, maintainer

* **[Ruslan Garipov](https://github.com/ruslangaripov)**

  * English help contents
  * UDN format and sorter
  * Makefiles
  * Debugging and bug reports
  * Improvements of charts drawing and printing

* **[Igor Tyulyakov](https://github.com/g10101k)**

  * Debugging and bug reports
  * Improvement of UI features
  * Design solutions (MediaPlayer and portraits)

* **[Alexey Dokuchaev](https://github.com/danfe)**

  * FreeBSD port developed (https://www.freshports.org/misc/gedkeeper/)

* **[Kevin D. Sandal](https://github.com/Dreamer451)**

  * Proofreading of the English manual
  * Debugging and bug reports

* **[Milan Kosina](https://github.com/m-kosina)**

  * Debugging and bug reports
  * Development of new features

* **[Kevin Routley](https://github.com/fire-eggs)**

  * Debugging, bug reports and bug fixes
  * Development of some tests

* **[Alexander Curtis](https://github.com/alexandercurtis)**

  * Author of GEDmill (Family History Website Generator), now it is external plugin

* **[Alexander Zaytsev](https://github.com/hazzik)**

  * Improving the architecture and efficiency of the core
  * Testing and bug fixes


# How to build GEDKeeper

## Dependencies

### Building

* Microsoft [.NET Framework](https://dotnet.microsoft.com/) 4.7.1
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
- .NET Framework 4.7.1
- NUnit


**What platforms are supported?**

GEDKeeper is cross-platform and can be run on Linux and Windows. 
In particular, it supports the following platforms:

- Windows Vista (or later) and .NET 4.7.1 (or later)
- Linux (Debian/Ubuntu) and Mono 4.7.1 (or later)

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
