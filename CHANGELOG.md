
## ??.03.2021 [v2.17.0]
- Fixed a logical error in the default records filtering method.
- Optimized data model to save memory when loading large files (over 100 MB).
- Updated German translation [Patrik Studer].
- FreeBSD port developed [Alexey Dokuchaev].
- Improved import from WikiTree.
- Updated Czech translation [Jerry Verner].
- Added the feature to interrupt some long-running functions.
- Temporary fix for maps output (OSM replaced with GoogleMaps).
- Fixed work of geocoders.
- Added option for dotted lines of adopted children in the diagrams.
- Editing dates in dialogs adapted to regional formats.
- Added optional warnings for closing editing dialogs with unsaved changes.
- Added configurable DepthLimit for complete tree [Milan Kosina].
- Optimized regex searching in "Selecting record" form.
- Fixed bug of second first names displayed in a readonly 'Patronymic' field.
- Added option to automatically correct the first capital letter in the names.
- Fixed bug when adding spouse to existing person.
- Improved import from Geni (tag _MARNM).
- Fixed bug during importing data from "MyHeritage Family Tree Builder" [Milan Kosina].
- Fix inconsistent error message when loading GedML file [Walter Rawley].
- Added display of marriage dates of parents in tree diagrams.
- Added call quick information about the person in the tree diagrams.
- Implements maximum number of backups for each revision backup [Milan Kosina].

## 18.09.2019 [v2.16.2]
- Implemented moving notes up and down [Milan Kosina].
- Implemented line wrapping in the HyperView of information panel.
- Added check for file existence when creating multimedia records.
- Added verification of the database for multimedia records (lack of files, archives, storages).
- Added support for relative paths to media files [Milan Kosina].
- Fixed the choice of the type of file storage in the multimedia dialog.
- Implemented the removal of multimedia files when deleting their records.
- Fixed event output if an additional event type is not specified.
- Fixed renaming and deleting of location records.
- Fixed merging family and individual records, transition buttons in editing dialogs, button images.
- Fixed editing dates of events with periods.
- Added new diagnostics for data verification: for families without spouses and/or children [Kevin D. Sandal].
- Added hotkey for saving files from tree diagrams.
- Fixed support for formats with zero IDs of records.
- Fixed opening files and OSM geocoder operation.
- Cleaning and optimization, improved support for two third-party GEDCOM formats.
- Fixed the script of the portable package.
- Fixed the work of the "Compare databases" tool.
- Fixed external viewing of PDF files (and other not supported by the embedded viewer).
- Proofreading of the English manual [Kevin D. Sandal].
- Implemented preliminary support for GedML format.
- Fixed pedigrees localization.
- Fixed Calendar plugin.
- Fixed overlapping of neighboring persons in tree diagrams.
- Fixed incorrect parsing of interpretable dates from Ahnenblatt.
- Fixed missing substructures in media records from extraneous files.
- Fixed convert links to sources in source records .
- Fixed convert media links to media records, and convert non-standard x-references.
- Fixed reading standard coordinates of the place.
- Minor improvement of circle chart.
- Improved support for several GEDCOM file formats.
- Fix of sorting the columns of some tables.
- Fix of long-distance displacement of male nodes (tree chart) in the presence of more than 
one marriage and a large tree of descendants from the first wife.
- Improved date input in the event dialog with regional format settings.
- Bugfixes for FlowInput plugin.
- Added the possibility to create and connect plugins to replace record's edit dialogs.
- Restored loading recent files.
- Created a new plugin for managing "folders" in GEDCOM files (to separate work areas in large files).
- Added option to automatically detect charset of GEDCOM files at load.
- Fixed execution of Lua scripts.
- Reduced memory consumption when loading large files (25-42%).
- Optimized a number of functions, allowing to significantly increase the speed of loading and processing large files 
(from 30 thousand records to very large files - 91mb, 538 thousand records, load time 25 seconds!).

## 14.02.2019 [v2.15.0]
- Added control unit for several families of parents and for the type of 
child/parent linkage (adoption, foster, etc.).
- Improved handling of parts of names.
- Added option to disable checking for a valid tree size (in most cases, 
checking is outdated, but can sometimes be useful).
- Added saving and restoring selected sorting columns in tables.
- Added the feature to disable the sorting of children and enable sorting of 
spouses for family's records.
- Added the feature to select colors for persons in tree charts.
- Created the Czech translation [Michal Novák].
- Added GKTray application (birthdays reminder and recent files manager).
- Added detection of data loops.
- Fixed minor bugs.
- Added new validations of data.


## 10.08.2018 [v2.14.0]
- Fixed the bug with adding/removing portraits of persons.
- Returned the status bar to the chart's windows and added 
information about the size of the image.
- Fixed bug of loading FTBv6 files with the encoding Win1251 (ANSI + Rus).
- Improved collapsing tree branches.
- Fixed several minor bugs and simplified development projects.
- Finalized and included the generator of the Tree Album.
- Added the option of circle charts: arc text on/off.
- Added options for tree charts: margins, intervals between branches and 
intervals between generations.
- Added options for tree charts: hide unknown spouses and show birth and 
death places.
- Restored the work of the maps module.
- Implemented portable mode of the program.

## 14.07.2018 [v2.13.2]
- Fixed the bug of reading standard GEDCOM notes.

## 28.06.2018 [[v2.13.1](https://github.com/Serg-Norseman/GEDKeeper/releases/tag/v2.13.1)]
- Implemented the saving of circle charts in SVG.
- Fixed bug in the tree's chart with folding of branches.
- Fixed bug in the plugin "History Data".
- Fixed bug in the plugin "Data quality".
- Added option "Automatically check for updates".
- Fixed bug of loading of files "Agelong Tree" with incorrect encoding ANSEL.
- Fixed a crash in checking for new versions in WinXP.

## 08.06.2018 [[v2.13.0](https://github.com/Serg-Norseman/GEDKeeper/releases/tag/v2.13.0)]
- Added the folding of tree branches.
- The generator "Family Book" has been revised.
- Added option of displaying dates of marriages in tree diagrams.
- Added the inverted tree mode (ancestors at the bottom, descendants at the top).
- Added control keys for the number of generations displayed in circle charts.
- Fixed drawing of background of circle charts when outputting to a file.
- Fixed working of option "Hide empty segments" in circle charts.
- Added reports "Phonetics" and "Contemporaries".
- Created the first reporting plugin (reports "Frequency of names" and
"Personal events").
- Created the subsystem of external reporting plugins.
- Fixed auto-scrolling of tree diagram when clicking on personal record.
- Added an examples of the databases "The Human Origins" (rus, lat) and 
"Ancient Kingdoms" (rus).
- Added a new plugin "History Data (links)".
- Added support for editing names in different languages.
- Added support for saving a snapshot of a tree in SVG format.
- Rpm-packages for Fedora / Rosa Linux are added.
- Added a new geocoder (OSM).
- Added a new plugin "Data Quality".
- Added possibility of highlighting portraits on general photos.
- Restored supporting of Ansel GEDCOM encoding.
- Fixed minor translation errors.
- Added a new plugin "WordsCloud".
- Fixed the problem of extra lines in family records while saving
GEDCOM files, which led to import problems in other programs.

## 17.06.2017 [[v2.12.0](https://github.com/Serg-Norseman/GEDKeeper/releases/tag/v2.12.0)]
- Created the Chinese translation [Cui Jianquan (崔建全)].
- Created the German translation [Gleb Buzhinsky].
- The way in the interface for working with multiple files is changed 
from MDI to SDI (preparation for the future full porting).
- Considerable redesign of the program architecture.
- Created the Italian translation of the program [Amalia Boffa].
- Various minor improvements.
- Fixed error sending error log in Ubuntu 1604 LTS (Unity).
- Added a new plugin "Chronicle of events".
- Fixed saving of column widths in the list of individual records.
- Improved checking and excluding the second instance of the program.
- Improved operation of the record lists and processing of the columns.
- Implemented the formatting of text notes.
- Automatic replacement of direct links paths to multimedia files 
when using the program simultaneously between two operating systems (Windows / Linux);
you need to manually edit the file of paths.
- Implemented display of progression when adding very large media files.
- Implemented option to disable the ability to add multimedia files with direct links.
- Replacement of absolute paths to multimedia files is realized when using
 the program and databases between Windows and Linux (for advanced users).
- Fixed the cache of portraits in case of two links to one photo.

## 28.02.2017 [[v2.11.0](https://github.com/Serg-Norseman/GEDKeeper/releases/tag/v2.11.0)]

- Added French localization (thanks to Diana Shilnikova).
- Added images cache for portraits. This improves tree chart performance
with large number of attached portraits.
- Partially fixed the issue with access to media files when user switches
between two different operating systems - Linux and Microsoft
Windows (the issue was fixed only for relative paths).
- GEDKeeper is ready to introduce tips with national holidays.
- Updated photo displaying in the media viewer.
- Optimized the lists and added sorting arrow icons.
- Implemented VLC-based internal media player (VLC must be
installed separately).
- Added a new warning (and option to disable the warning) when
user adds a media file from removable disk.
- Added a new option to disable reopening of the most recent opened
databases on the program startup.
- Included YAML-parser implementation to process external databases and
configurations.
- Added ability to print a circle chart with print preview available.
- Improved charts rendering, zooming, scrolling and navigation inside
circle and tree charts [@ruslangaripov].
- Added new control to manage number of visible generations in a tree chart.
- Improved the popup control for tree chart zooming [@ruslangaripov].
- Fixed HOME directory processing error occurred after new operating
system has been installed (both Linux and Microsoft Windows).
- Added generating and exporting of a new document type -- &quot;Album of
Trees&quot;.
- &quot;GEDKeeper GUI localization manual&quot; was updated and translated
to English [@ruslangaripov].
- GUI improved within tree editor, person portraits management and
setting up default portraits showing in the tree [@g10101k].
- Made small improvements and optimizations.
- Considered more national language features aware of name processing.

## 26.12.2016 [[v2.10.0](https://github.com/Serg-Norseman/GEDKeeper/releases/tag/v2.10.0)]

- Added a geocoding engine (Yandex) and their choice in the options (Google/Yandex).
- Restored search for locations in the editor of locations and window of maps (GoogleAPI).
- Added language selection in the properties of the GEDCOM file.
- Save files in any encoding except UTF-8 are deprecated and disabled.
- Added check for updates on the website sourceforge.net.

## 18.12.2016 [[v2.9.0](https://github.com/Serg-Norseman/GEDKeeper/releases/tag/v2.9.0)]

- Added localization for Polish (Jacek).
- Fixed problems with European encodings of GEDCOM files.
- Small adaptation of import from the Ahnenblatt.
- Allow to process and edit maiden and married last names of women. Add several printing formats for such names.
- Improve culture-specific and culture-independent processing of names. Prepare a framework to attach languages to names and store this in a database.
- Create auto-update application sample.
- Combine GUI dialogs &quot;Adding person&quot; and &quot;Edit person&quot;.
- Add new types of facts: blood group, hair color and eyes color.
- Add demo databases for the Bach and Nehru–Gandhi families.
- Add demo databases for mitochondrial and Y-chromosome gaplogroups (for ones who are keen on molecular genealogy).
- To improve modifications safety, GEDKeeper locks a record while user is changing it.
- Implement modifications canceling after changes have been made in editing GUI dialogs.
- Fix application behavior after clicking buttons that change parents or parents’ families while user is changing a person in the GUI editor.
- Improve supporting of multi-monitor configurations [@ruslangaripov].
- Made a static analysis of the source code – fix over 60 possible defects and errors.
- Fix many small issues and improve overall application stability.
- Fix some errors.
- Add coding style guide.
- Use Russian name declination in birthday notifications and calculator of relation degree.
- Improve birthday notifications displaying [@ruslangaripov].
- Fix behavior of child MDI (database) windows.
- Improve calculation of UDN and dates sorting when mixing calendar types [@ruslangaripov].
- Improve English localization [@ruslangaripov].
- Add calculator of relation degree.

## 03.09.2016 [[v2.8.1](https://github.com/Serg-Norseman/GEDKeeper/releases/tag/v2.8.1)]

- Correct English localization of the user interface [@ruslangaripov].
- Add preliminary version of demonstration database for USA users (President).
- Refine and optimize the source code, fix some errors. GEDKeeper
successfully passed a stress test processing a database with more than 60,000 people.
- Change default interface language to English to improve further
internationalization of GEDKeeper.
- Move the question with calendars to a completion state.
- Begin unification of a part of the source code that handles locale-specific processing of people names.
- Add demonstration database for Russian users (Pushkin's family).
- Add prototype of the new plug-in: &quot;Navigator&quot;.

## 14.08.2016 [[v2.8.0](https://github.com/Serg-Norseman/GEDKeeper/releases/tag/v2.8.0)]

- Clean and optimize code.
- Included are a selection of the interface language at first run after installation.
- Implemented selection of portraits from photos.

## 31.07.2016 [[v2.7.0](https://github.com/Serg-Norseman/GEDKeeper/releases/tag/v2.7.0)]

- Added a diagram of the circle of descendants.
- Improved options of the circle of ancestors.
- Added generation of ascending pedigrees (ancestors).
- Fixed minor bugs.
- Added periodic autosave.
- In the installer for Windows added support for switching languages.
- Added backup of files before saving.
- Fixed display of links in location information.

## 20.06.2016 [[v2.6.0](https://github.com/Serg-Norseman/GEDKeeper/releases/tag/v2.6.0)]

- Added English localization of help [@ruslangaripov].
- Export pedigrees to RTF.
- Add package for distribution on Debian/Ubuntu Linux.
- Add Mono-supporting (Linux).
- Add new unique framework for unified encoding and sorting dates
in any calendar, with any precision [@ruslangaripov].
- Restore html-pedigree generation to be compatible with Linux.
- Simplify help content down to html only to improve Linux
compatible (help content has no CHM files).
- Add a function to duplicate person entries (for cases when you
need to share data between two entire namesakes).
- Add context menu to tables in the main work window.
- Show calendar label for a date.
- Remove supporting of Ansel GEDCOM encoding.
- Add ability to edit several names of one person.
- Add new statistics: demography, death-rate estimation for men
and women on each five years of age.
- Fix incorrect tree output happened in some modes.
- Improve personal records merger.
- Fix notes filtering, when adding into other records.
- Fix small error on displaying person's associations.
- Improve plug-in for 3D rendering of tree, but there is a lot
more work to do (need a collaborator get used to 3D and dynamic
transformation programming).
- Implement many small fixes and code improvements (speed, safety
and stability).


## 01.02.2016 [v2.5.0]

- Remake pedigree import.
- Update pedigree import plug-in; it works again.
- Restore Word and Excel documents loading in pedigree import plug-in.
- Improve &quot;Ancestors circle&quot; diagram, add ancestors
list analysis mode.
- Improve scrolling of text summaries and tree diagrams.
- Add slide show used for all images.

## 17.11.2015 [v2.4.1]

- Fix sorting by age and duration of life columns.
- Add export to Excel function to the statistics block.
- Improve sorting on columns in all lists of GEDKeeper.
- Complete storing and loading encrypted files.

## 08.11.2015 [v2.4.0]

- Increase size of help content appreciably.
- Optimize sorting of main lists with records.
- Remove flickering when scrolling main lists with records.
- Print tree diagrams with preview.
- Fix dates handling in filter dialog.
- Completely re-implement dates processing when sorting on
columns of main lists (sorting now is aware of relative dates; dates
rendering is completely separated from dates processing and sorting).
- Optimize file loading.
- Remove text flickering when output text in record summaries.
- Add an experiment: storing and loading encrypted files (it is
unable to use it on regular basis -- there is no complete specification
for encrypted file format).
- Update English and Ukrainian localizations.
- Improve locking against modifying of person records and
families using privacy/protection property.
- Optimize filtering by name of a person record.
- Fix privacy switch for persons and families.
- Improve handling of reopening the same database.
- Fix opening multiple program instances.
- Improve test coverage for GEDCOM code infrastructure up to 70%
(this allows us to get and fix some new errors introduced by new code,
before we will make a release).
- Fix navigation in tree diagrams (&quot;Previous record&quot;
and &quot;Next record&quot; buttons).
- Add four new types of user-defined reference for religion:
Islam, Catholicism, Orthodoxy and Old Belief.
- Completely re-implement algorithms in &quot;Ancestors
circle&quot; diagram; the plug-in was removed and its code was merged
into the main code base.
- Fix statistics window when changing GUI language.
- Fix opening of the scripts window.
- Improve the &quot;Life&quot; plug-in.
- Fix drawing in the &quot;Life&quot; plug-in.
- Improve GUI: toolbars in the database main window and in the
tree diagrams window have the same buttons for filtering and navigation
(&quot;Previous record&quot; and &quot;Next record&quot; buttons)

## 18.10.2015 [v2.3.0]

- Increase size of help content appreciably.
- Add new type of user-defined: &quot;Repressed&quot;.
- Add new statistics: men distribution by month of birth.
- Improve family book generation.
- Add quick search to search by name in database main window.
- Implement many code optimizations; code refactoring.
- Add authenticity index estimation and indices distribution
diagram into the statistics.
- Add new option to tree editor: user can toggle estimated
authenticity indices visibility (indices are estimated basing on
confidence factor of information sources).
- Save columns width in the persons list between application
sessions. Optimize column options.
- Calculate statistics for filtered entries set when filter is enabled.
- Add persons search in tree editor by matching a part of name.
- Fix ordering of persons list when generate pedigrees.
- Improve tree editor rendering; add scrolling smoothness.
- Improve navigation in tree editor; improve code performance.
- Implement small improvements in tree editor.
- Fix small errors; code refactoring and optimizations.
- Add new plug-in to view images in a window sibling peered to
database window. (this, for example, allows to view scanned image of a
census and add data simultaneously, without having resort to third
party image viewers).
- Move &quot;Ancestors circle&quot; diagram into an optional plug-in.
- Restore optional &quot;Life&quot; plug-in -- a game to relax a
little bit.

## 18.01.2015 [v2.2.0]

- Fix changing e-mails, web sites and phone numbers in addresses.
- Switch to &#39;semver&#39; versioning. There is no more build
number in version, only: major.minor.patch.

## 06.10.2014 [v2.1.1]

- Fix small errors.
- Improve extensions API for plug-ins. Four code blocks were moved
out from program to plug-ins.

## 06.03.2014 [v2.1.0]

- Implement emergency save -- save user data when application get crashed.
- Improve multimedia content viewing.
- Improve diagram functions and controlling.
- Change output of pedigree generation from HTML to PDF.
- Complete new filters framework on all lists.
- Add prototype of API for extensions (plug-ins).
- Add diagram for viewing patriarchs and their interrelations
(&quot;Service\Tools&quot).
- Add ability to show nicknames in tree diagrams.
- Improve editing of events and materials.
- Optimize program kernel; pass main tests.
- Fix large number of errors and defects.
- Fix an issue with code pages on export to Excel.
- Improve large database loading (speed up up to 20-30%).
- Remove databases merger by synchronization (it is a dead-end siding).
- Remove export to Web (it lacks of prospects).
- Remove Undo/Redo history (it is useless).

## 28.03.2012 [v2.0.4]

- Add ability to change foreground and background colors on trees.
- Fix small errors and defects.
- Replace names fuzzy search in duplicates finding with much
faster implementation.
- Add prototype for exporting data to PDF.
- Add new diagram: &quot;Ancestors circle&quot;.

## 25.02.2012 [v2.0.3]

- Improve UAC compatibility.

## 19.02.2012 [v2.0.2]

- Fix checking format of input files.
- Add ability to send application log with e-mail.

## 14.02.2012 [v2.0.1]

- Simplify handling of multimedia-storage and multimedia-archive.
- Improve processing of multimedia content.
- Fix errors and defects.
- Improve files opening speed, processing and statistics calculation.
- Improve lists sorting (up to 75%).
- Improve lists speed on large databases.
- Add search for duplicating families.
- Add new material type: &quot;DNA marker&quot;.
- Improve database checking tool.
- Complete full-text search.

## 05.08.2011 [v2.0.0]

- Add full-text search pre-release.
- Restore maps, statistics, network and archive support.
- Optimize and refactor the code.
- Clean up the code from porting footprints.
- Complete code porting to a new platform and language.

## 20.07.2011

- Complete porting to C#.

## 15.07.2011

- Remove rest of VCL code.

## 25.06.2011

- Complete GUI porting to Windows Forms.

## 18.06.2011

- Discontinue support for PedigreeGen project.

## 01.06.2011

- Start porting to .NET framework.

## 31.05.2011 [v1.9.1]

- Fix defects and optimize.

## 28.04.2011 [v1.9]

- Add person photos and shows it in the tree.
- Support changeable languages for GUI.
- Add English localization.
- Add Ukrainian localizations.
- Add ability to save current workspace.
- Fix multimedia processing.

## 04.04.2011 [v1.6.3]

- Add calendar to the names reference book.
- Add new tree type: combined tree with ancestors and descendants.
- Implement new help system.
- Optimize tree generation.

## 28.03.2011 [v1.6.2]

- Add new improved filters on tree diagrams.
- Improve and optimize tree diagram generation.
- Fix errors from the previous release.

## 14.03.2011 [v1.6.1]

- Add complementary export to Excel.
- Optimize file loading (speed up up to 7%) and lists filtering
(speed up up to 8%).
- Fix small defects in design and functions.
- Output locations on map for selected person: add ordering by date.
- Improve lists sorting (up to 50% faster on large data).
- Fix making screenshots on scaled trees.
- Fix rendering of multimedia content.
- Fix and optimize the code.

## 28.02.2011 [v1.6]

- Add Lua scripting support; this makes GEDKeeper more flexible
and extensible. Add help content pre-release.
- Remake trees output engine. It now supports generation and
rendering of superdimensioned trees while wasting minimal amount of
memory resources.
- Add persons filtering by materials' content.
- Add font settings in to tree generation.
- Add support for stream data input from register of births and
audit tales (beta pre-release).
- Add support for exporting large images of trees into EMF files.
- Improve events dialog -- user can specify calendar for dates.
- Improve statistics calculation speed.
- Add new tests for database checking.
- Improve supporting MyHeritage GEDCOM loading.
- Improve and enable for users: a function to determine relation
degree in trees.


## 14.12.2010 [v1.5]

- Optimize work with very large lists (thousands of personal records).
- Add ability to edit a tree with GUI (preliminary implementation).
- Add organizer (addresses, phone numbers, mails).
- Import from databases that are ADO compatible (MDB, CSV).
- Add locations management.
- Add database checking to verify conformity of name and sex
(required when importing large data arrays from exterior sources).
- Add new option for tree generation: show only years of life.
- Add options for person highlighting in lists: persons having no
parents or families.
- Add bookmarks.
- Improve duplicates finder and data filters.
- Improve pedigree generation.
- Optimize tools for databases splitting and merger.
- Improve pedigree import from TXT, DOC and XLS sources.
- Improve maps supporting and add mini-map in locations finder.

## 14.10.2010 [v1.2]

- Add new diagram option to exclude children that did not reach
childbearing age (this can reduce size of large trees).
- Extend nickname showing (the column may be disabled).
- Add locations filter.
- Add diagrams into statistics.
- Add search of patriarchs to the toolset.
- Improve &quot;Time line&quot; mode.
- Modify and improve an algorithm used to save a file.
- Improve Web exporting.
- Modify maps generation: it now considers current filters.
- Improve user interface.
- Improve creation of conventional pedigrees (UIRO format).

## 14.08.2010 [v1.1]

- Implement &quot;Window of time&quot; block.
- Add new type of individual materials.
- Improve compatibility with Windows XP, Windows Vista, Windows 7.
- Add &quot;Endless calendar&quot;.
- Add names reference book.
- Add expressions calculator.
- Optimize rendering and filtering of lists.
- Optimize maps creation.
- Fix adding and saving addresses.
- Add preliminary implementation of &quot;trusted&quot;
synchronization of databases.
- Improve lists&#39; information panel.
- Improve delete operation.
- Improve pedigrees import from Excel.
- Add user-defined references.
- Fix large amount of errors.

## 14.05.2010 [v1.0]

- Add groups to research.
- Add access restriction to confidential data.
- Add simple scaling of a tree image.
- Add choose of output format for pedigree generation; modify the
generation partially according to formats.
- Improve speed of duplicates finding.
- Add hints.
- Improve filtering.
- Optimize lists.
- Improve Excel export.
- Fix some small errors.

## 28.04.2010 [v0.9]

- Increase number of columns in the persons list; improve adjustment.
- Add FAQ.
- Fix many errors.

## 27.04.2010

- Save variants of relations in associations.
- Improve user interface.
- Fix lists of references from people and family facts to locations.
- Fix task list and correspondence list in researches.

## 26.04.2010

 - Add namesake list to information about a person.

## 23.04.2010

- Fix some errors: processing event year number in statistics and
list updating.
- Improve &quot;Add location&quot; dialog (name copying).
- Fix pedigree import: extent numeration recognition.

## 22.04.2010

- Add Undo&#47;Redo basis.
- Improve performance of lists updating.
- Remake navigation.

## 14.04.2010

- Fix errors and defects.
- Fix and improve web-trees generation.
- Fix web-export.

## 08.04.2010

- Add quick filter.

## 04.04.2010

- Add quality rating for quotes taken from a source.
- Fix researches, tasks and correspondence interface.

## 02.04.2010

- Add new object: locations.

## 30.03.2010

- Implement basis for new objects: research, task and correspondence.
- Fix saving proxy settings into the settings file.

## 11.03.2010

- Fix large amount of errors and defects.

## 26.02.2010

- Support archives and storages of multimedia content.

## 22.02.2010

- Add support of multiple document interface (MDI).

## 05.01.2010

- Add preliminary support of multimedia content.

## 28.01.2010

- Refactor and optimize large amount of code; fix small defects.
- Support archives according to the standard.

## 30.12.2009

- Add tree navigation.

## 22.12.2009

- Complete import of text pedigrees.

## 20.12.2009

 - Improve pedigree import.

## 08.12.2009

- Fix errors and defects.
- Improve source quotation.

## 06.12.2009

- Save filters by name and modified files.

## 04.12.2009

- Improve user interface.
- Optimize updating of lists.

## 01.12.2009

- Develop new navigation style.
- Refactor source code.

## 30.11.2009

- Improve handling of events and attributes.
- Fix code defects.
- Add new navigation functions.

## 28.11.2009

- Add six new non-standard tags: &quot;hobby&quot;,
&quot;award&quot;, &quot;military service&quot;, &quot;drafted&quot;,
&quot;demobilized&quot;, &quot;rank&quot;.

## 26.11.2009

- Redesign the entire application.
- Improve output of descending trees.
- Fix generation of descending trees.
- Redesign project architecture.

## 24.11.2009

- Fix errors.
- Improve help content and installer.
- Change project tree.

## 19.11.2009

- Fix some source code defects.
- Optimize performance.
- Refactor list update and filtering.
- Add fuzzy comparison with dates checking into duplicates finder.

## 14.11.2009

- Fix output of sources in pedigree generation.

## 30.09.2009

- Show numbers of generations in pedigrees as Roman numerals.
- Fix small defects.

## 19.09.2009

- Add Google maps supporting.

## 24.08.2009

- Fix errors and defects.

## 31.07.2009

- Fix errors.
- Improve files comparison.

## 30.07.2009

- Improve design.
- Improve filtering and user interface.

## 27.07.2009

- Modify editor of persons, families, groups and their relationships.

## 26.07.2009

- Refactor editor code.

## 24.07.2009

- Add support for groups.

## 22.07.2009

- Improve adding and modifying records.

## 05.07.2009

- Improve Web export.

## 01.07.2009

- Refactor code and resources. Optimize and cut down lines of further development.
- Improve cognate linkers and diagrams finder.
- Improve Web export.

## 30.06.2009

- Complete pedigree builder.

## 26.06.2009

- Refactor source code.
- Add export to Excel.
- Add initial code for pedigree builder.
- Add russian last names &quot;processing&quot; for statistics.

## 12.05.2009

- Add hypertext survey for a person.

## 12.02.2009

- Fix some errors. Add two editing functions.

## 06.02.2009

- Fix some errors.
- Add a warning before deleting an object.

## 04.02.2009

- Work through database cleaning.
- Fix small errors in dates processing and dates output in diagrams.

## 02.02.2009

- Fix defects.
- Improve record list.

## 28.01.2009

- Fix some small defects.
- Add ability to delete person record.
- Fix a fundamental error in tree calculation; now tree with
descendants and their wives works.
- Improve tree output; fix errors. Add tree picture saving.
- Add statistics types.

## 22.01.2009

- Add preliminary file checking.
- Work through output of tree with ancestors and descendants.
- Add statistics for names and number of ancestors.
- Add autocomplete for patronymic name and first name.
- Fix some errors.
- Add application log.

## 20.01.2009

- Fix errors.
- Add first and patronymic names parser, and dictionary.
- Complete descendants tree.
- Add preliminary version of service to check relation degree.

## 18.01.2009

- Complete event editing (notes, sources, multimedia).
- Complete ancestors tree.

## 17.01.2009

- Add ability to modify author of a document.
- Improve file headers.
- Improve user interface.
- Complete dates processing.

## 16.01.2009

- Add preliminary version of descendant tree.
- Fix some errors.
- Improve dates processing.
- Fix file headers.

## 15.01.2009

- Add preliminary help content [ElenAlexs].
- Adjust design.
- Add lists sorting.
- Improve event editing.

## 14.01.2009

- Add and remove children from families.
- Add the first tree -- ancestors tree; add setup.
- Add preliminary print preview.
- Save a tree setting between application sessions.
- Add dates processing.

## 13.01.2009

- Refactor and optimize source code.
- Add events, notes, objects and sources to families.
- Add filter by field into the person selector dialog (when it is necessary).
- Add &quot;About&quot; window.

## 12.01.2009

- Add events, notes, objects and sources to person records.

## 11.01.2009

- Add event, note, object and source dialogs.
- Add preliminary modify function to change family records.

## 10.01.2009

- Add modifying person records.

## 09.01.2009

- Add ability to add new person, to modify and select the parents.

## 08.01.2009

- Start the project up.
