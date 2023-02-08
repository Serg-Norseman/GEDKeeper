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

# Additional resources

- [GEDKeeper coding style guide](CODINGSTYLE.md)
- [GEDKeeper chat on gitter](https://gitter.im/Serg-Norseman/GEDKeeper)


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


# External components

```
Arbor
    Licensed under the MIT license.
    (C) 2011 Samizdat Drafting Co.
    https://github.com/samizdatco/arbor
    (C) 2012 Serg V. Zhdanovskih (fork to C#), 2016 Ruslan Garipov
    https://github.com/Serg-Norseman/Arbor


CalendarConverter
    Licensed under public domain.
    (C) John Walker <http://www.fourmilab.ch/documents/calendar/>
    (C) 2011 by Serg V. Zhdanovskih (fork to C#), 2016 Ruslan Garipov
    https://bitbucket.org/Serg-Norseman/calendarconverter


CSVReader
    Licensed under BSD License.
    (C) 2008 Andrew Stellman
    http://www.stellman-greene.com/CSVReader


DotNetRtfWriter
    Licensed under LGPLv3.
    (C) Matt Buckley and Thomson Reuters, https://sourceforge.net/projects/netrtfwriter/
    (C) Tom Dowden, https://github.com/elistia/DotNetRtfWriter


ExcelLibrary
    Licensed under GPLv3.
    https://github.com/darknessomi/excellibrary


ExpCalculator
    Licensed under <unknown>
    (C) Ivlev M.Dmitry, Sergey Pedora
    (C) 2011 by Serg V. Zhdanovskih  (fork to C#)


GEDmill (Family History Website Generator)
    Licensed under GPLv3.
    Copyright 2009 Alexander Curtis
    https://github.com/alexandercurtis/GEDmill


GMap.NET
    Licensed under "FLAT EARTH LICENSE"
    (C) radioman
    https://github.com/radioman/greatmaps


IniFiles
    Licensed under The Code Project Open License (CPOL).
    (C) 2007 Gajatko
    http://www.codeproject.com/Articles/20120/INI-Files


iTextSharp
    Licensed under GNU Affero GPLv3.
    https://github.com/itext/itextsharp


Life
    Licensed under (something similar to zlib License).
    (C) 1998 Ian Lane
    (C) 2011 Serg V. Zhdanovskih (fork to C#)
    https://github.com/Serg-Norseman/ConwayLifeGame


Lua
    NLua, Licensed under MIT, Vinicius Jarina, https://github.com/NLua/NLua


CsGL for OpenGL
    Licensed under BSD.
    (C) 2001, Lloyd Dupont <lloyd@galador.net>


nVLC
    Licensed under GPLv3
    (C) Roman Ginzburg, http://www.codeproject.com/Articles/109639/nVLC
    (C) fork Sergey Zhdanovskih, https://github.com/serg-norseman/nVLC


SingleInstance
    Licensed under The Code Project Open License (CPOL).
    (C) 2007 Shy Agam
    http://www.codeproject.com/Articles/19682/A-Pure-NET-Single-Instance-Application-Solution


StringTokenizer
    Licensed under (something similar to zlib License).
    (C) 2004 Andrew Deren


TimSort
    Licensed under the Apache License, version 2.0.
    (C) 2008 The Android Open Source Project


Xapian
    Licensed under GPLv2
    https://xapian.org/


ZedGraph
    Licensed under LGPLv2.1.
    https://github.com/discomurray/ZedGraph


ZipStorer
    Licensed under The MIT License (MIT).
    (C) 2010 Jaime Olivares
    https://zipstorer.codeplex.com
    https://github.com/jaime-olivares/zipstorer

```
