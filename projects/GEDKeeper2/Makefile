SHELL := /bin/bash

.DEFAULT_GOAL := all

includemk := ../
include $(includemk)toolchain.mk

project := $(gedkeeper2)
projectext := .exe

# Get directories list for `vpath %.resx` via (run from this directory):
# find -regex '.+\.resx' | sed 's/\(.\+\)\/.\+/\1 \\/' | awk '!a[$0]++'
# Just do not forget to remove trailing " \" on the last line.
vpath %.resx \
. \
./GKUI \
./GKUI/Charts \
./GKUI/Controls \
./GKUI/Dialogs

objdir := obj/$(releasetype)/
objdirdos := obj\\$(releasetype)\\
outdir := $(objdir)
outdirdos := $(objdirdos)
installdir := ../../
# Get `sources` via:
# find -regex '.+\.cs' | sed 's/\(.\+\)/\1 \\/'
# Just do not forget to remove trailing " \" on the last line.
sources := \
./Externals/SingleInstancing/GlobalMutexPool.cs \
./Externals/SingleInstancing/IpcBroadcast.cs \
./Externals/SingleInstancing/ISingleInstanceEnforcer.cs \
./Externals/SingleInstancing/SingleInstanceException.cs \
./Externals/SingleInstancing/SingleInstanceProxy.cs \
./Externals/SingleInstancing/SingleInstanceTracker.cs \
./GKCore/BaseContext.cs \
./GKCore/BaseSearchStrategy.cs \
./GKCore/Export/CustomWriter.cs \
./GKCore/Export/ExcelExporter.cs \
./GKCore/Export/Exporter.cs \
./GKCore/Export/FamilyBookExporter.cs \
./GKCore/Export/HTMLWriter.cs \
./GKCore/Export/PDFExporter.cs \
./GKCore/Export/PDFWriter.cs \
./GKCore/Export/PedigreeExporter.cs \
./GKCore/Export/RTFWriter.cs \
./GKCore/GKData.cs \
./GKCore/GKUtils.cs \
./GKCore/Kinships/KinshipRec.cs \
./GKCore/Kinships/KinshipsMan.cs \
./GKCore/LangMan.cs \
./GKCore/Lists/CommunicationListMan.cs \
./GKCore/Lists/FamilyListMan.cs \
./GKCore/Lists/GroupListMan.cs \
./GKCore/Lists/IndividualListMan.cs \
./GKCore/Lists/ListColumns.cs \
./GKCore/Lists/ListFilter.cs \
./GKCore/Lists/ListManager.cs \
./GKCore/Lists/LocationListMan.cs \
./GKCore/Lists/MultimediaListMan.cs \
./GKCore/Lists/NoteListMan.cs \
./GKCore/Lists/RepositoryListMan.cs \
./GKCore/Lists/ResearchListMan.cs \
./GKCore/Lists/SourceListMan.cs \
./GKCore/Lists/TaskListMan.cs \
./GKCore/Maps/GMapPoint.cs \
./GKCore/NamesTable.cs \
./GKCore/NavigationStack.cs \
./GKCore/Options/GlobalOptions.cs \
./GKCore/Options/LangRecord.cs \
./GKCore/Options/MRUFile.cs \
./GKCore/Options/PedigreeOptions.cs \
./GKCore/Options/ProxyOptions.cs \
./GKCore/Options/TreeChartOptions.cs \
./GKCore/SCCrypt.cs \
./GKCore/ScriptEngine.cs \
./GKCore/Stats/CommonStats.cs \
./GKCore/Stats/CompositeItem.cs \
./GKCore/Stats/StatMode.cs \
./GKCore/Stats/StatsItem.cs \
./GKCore/Stats/TreeStats.cs \
./GKCore/SysUtils.cs \
./GKCore/Tools/PlaceObj.cs \
./GKCore/Tools/TreeTools.cs \
./GKProgram.cs \
./GKResources.Designer.cs \
./GKUI/AncestorsCircleWin.cs \
./GKUI/AncestorsCircleWin.Designer.cs \
./GKUI/BaseWin.cs \
./GKUI/BaseWin.Designer.cs \
./GKUI/Charts/ACOptionsControl.cs \
./GKUI/Charts/ACOptionsControl.Designer.cs \
./GKUI/Charts/AncestorsCircle.cs \
./GKUI/Charts/ChartFilter.cs \
./GKUI/Charts/CustomChart.cs \
./GKUI/Charts/ITreeControl.cs \
./GKUI/Charts/PersonControl.cs \
./GKUI/Charts/PersonList.cs \
./GKUI/Charts/PersonModifyEventArgs.cs \
./GKUI/Charts/ScaleControl.cs \
./GKUI/Charts/TreeChartBox.cs \
./GKUI/Charts/TreeChartPerson.cs \
./GKUI/Controls/GKComboItem.cs \
./GKUI/Controls/GKInputBox.cs \
./GKUI/Controls/GKInputBox.Designer.cs \
./GKUI/Controls/GKListItem.cs \
./GKUI/Controls/GKListView.cs \
./GKUI/Controls/GKMapBrowser.cs \
./GKUI/Controls/GKMenuItem.cs \
./GKUI/Controls/GKMergeControl.cs \
./GKUI/Controls/GKMergeControl.Designer.cs \
./GKUI/Controls/GKRecordsView.cs \
./GKUI/Controls/GKSheetList.cs \
./GKUI/Controls/GKTreeNode.cs \
./GKUI/Controls/ModifyEventArgs.cs \
./GKUI/Controls/SearchPanel.cs \
./GKUI/Dialogs/AboutDlg.cs \
./GKUI/Dialogs/AboutDlg.Designer.cs \
./GKUI/Dialogs/AddressEditDlg.cs \
./GKUI/Dialogs/AddressEditDlg.Designer.cs \
./GKUI/Dialogs/AssociationEditDlg.cs \
./GKUI/Dialogs/AssociationEditDlg.Designer.cs \
./GKUI/Dialogs/CommonFilterDlg.cs \
./GKUI/Dialogs/CommonFilterDlg.Designer.cs \
./GKUI/Dialogs/CommunicationEditDlg.cs \
./GKUI/Dialogs/CommunicationEditDlg.Designer.cs \
./GKUI/Dialogs/DayTipsDlg.cs \
./GKUI/Dialogs/DayTipsDlg.Designer.cs \
./GKUI/Dialogs/EventEditDlg.cs \
./GKUI/Dialogs/EventEditDlg.Designer.cs \
./GKUI/Dialogs/FamilyEditDlg.cs \
./GKUI/Dialogs/FamilyEditDlg.Designer.cs \
./GKUI/Dialogs/FilePropertiesDlg.cs \
./GKUI/Dialogs/FilePropertiesDlg.Designer.cs \
./GKUI/Dialogs/GroupEditDlg.cs \
./GKUI/Dialogs/GroupEditDlg.Designer.cs \
./GKUI/Dialogs/LocationEditDlg.cs \
./GKUI/Dialogs/LocationEditDlg.Designer.cs \
./GKUI/Dialogs/MediaEditDlg.cs \
./GKUI/Dialogs/MediaEditDlg.Designer.cs \
./GKUI/Dialogs/NameEditDlg.cs \
./GKUI/Dialogs/NameEditDlg.Designer.cs \
./GKUI/Dialogs/NoteEditDlg.cs \
./GKUI/Dialogs/NoteEditDlg.Designer.cs \
./GKUI/Dialogs/OptionsDlg.cs \
./GKUI/Dialogs/OptionsDlg.Designer.cs \
./GKUI/Dialogs/PersonalNameEditDlg.cs \
./GKUI/Dialogs/PersonalNameEditDlg.Designer.cs \
./GKUI/Dialogs/PersonEditDlg.cs \
./GKUI/Dialogs/PersonEditDlg.Designer.cs \
./GKUI/Dialogs/PersonNewDlg.cs \
./GKUI/Dialogs/PersonNewDlg.Designer.cs \
./GKUI/Dialogs/PersonsFilterDlg.cs \
./GKUI/Dialogs/PersonsFilterDlg.Designer.cs \
./GKUI/Dialogs/ProgressDlg.cs \
./GKUI/Dialogs/ProgressDlg.Designer.cs \
./GKUI/Dialogs/RecordSelectDlg.cs \
./GKUI/Dialogs/RecordSelectDlg.Designer.cs \
./GKUI/Dialogs/RepositoryEditDlg.cs \
./GKUI/Dialogs/RepositoryEditDlg.Designer.cs \
./GKUI/Dialogs/ResearchEditDlg.cs \
./GKUI/Dialogs/ResearchEditDlg.Designer.cs \
./GKUI/Dialogs/SexCheckDlg.cs \
./GKUI/Dialogs/SexCheckDlg.Designer.cs \
./GKUI/Dialogs/SourceCitEditDlg.cs \
./GKUI/Dialogs/SourceCitEditDlg.Designer.cs \
./GKUI/Dialogs/SourceEditDlg.cs \
./GKUI/Dialogs/SourceEditDlg.Designer.cs \
./GKUI/Dialogs/TaskEditDlg.cs \
./GKUI/Dialogs/TaskEditDlg.Designer.cs \
./GKUI/Dialogs/TreeFilterDlg.cs \
./GKUI/Dialogs/TreeFilterDlg.Designer.cs \
./GKUI/Dialogs/UserRefEditDlg.cs \
./GKUI/Dialogs/UserRefEditDlg.Designer.cs \
./GKUI/MainWin.cs \
./GKUI/MainWin.Designer.cs \
./GKUI/MapsViewerWin.cs \
./GKUI/MapsViewerWin.Designer.cs \
./GKUI/MediaViewerWin.cs \
./GKUI/MediaViewerWin.Designer.cs \
./GKUI/OrganizerWin.cs \
./GKUI/OrganizerWin.Designer.cs \
./GKUI/PatriarchsViewerWin.cs \
./GKUI/PatriarchsViewerWin.Designer.cs \
./GKUI/ScriptEditWin.cs \
./GKUI/ScriptEditWin.Designer.cs \
./GKUI/Sheets/GKCustomSheet.cs \
./GKUI/Sheets/GKEventsSheet.cs \
./GKUI/Sheets/GKMediaSheet.cs \
./GKUI/Sheets/GKNotesSheet.cs \
./GKUI/Sheets/GKSourcesSheet.cs \
./GKUI/SlideshowWin.cs \
./GKUI/SlideshowWin.Designer.cs \
./GKUI/StatisticsWin.cs \
./GKUI/StatisticsWin.Designer.cs \
./GKUI/TreeChartWin.cs \
./GKUI/TreeChartWin.Designer.cs \
./GKUI/TreeToolsWin.cs \
./GKUI/TreeToolsWin.Designer.cs
# Get files list for `resources` via (run from this directory):
# find -regex '.+\.resx' | sed 's/.\+\/\(.\+\)\.resx/\1.resources \\/'
resources := \
$(addprefix $(objdir), \
GKResources.resources \
AncestorsCircleWin.resources \
BaseWin.resources \
ACOptionsControl.resources \
GKInputBox.resources \
GKMergeControl.resources \
AboutDlg.resources \
AddressEditDlg.resources \
AssociationEditDlg.resources \
CommonFilterDlg.resources \
CommunicationEditDlg.resources \
DayTipsDlg.resources \
EventEditDlg.resources \
FamilyEditDlg.resources \
FilePropertiesDlg.resources \
GroupEditDlg.resources \
LocationEditDlg.resources \
MediaEditDlg.resources \
NameEditDlg.resources \
NoteEditDlg.resources \
OptionsDlg.resources \
PersonalNameEditDlg.resources \
PersonEditDlg.resources \
PersonNewDlg.resources \
PersonsFilterDlg.resources \
ProgressDlg.resources \
RecordSelectDlg.resources \
RepositoryEditDlg.resources \
ResearchEditDlg.resources \
SexCheckDlg.resources \
SourceCitEditDlg.resources \
SourceEditDlg.resources \
TaskEditDlg.resources \
TreeFilterDlg.resources \
UserRefEditDlg.resources \
MainWin.resources \
MapsViewerWin.resources \
MediaViewerWin.resources \
OrganizerWin.resources \
PatriarchsViewerWin.resources \
ScriptEditWin.resources \
SlideshowWin.resources \
StatisticsWin.resources \
TreeChartWin.resources \
TreeToolsWin.resources \
)
cscrefs := \
//reference:"..\\$(gkcommon)\\obj\\$(releasetype)\\$(gkcommon).dll" \
//reference:"libs\ArborGVT.dll" \
//reference:"libs\DotNetRtfWriter.dll" \
//reference:"libs\\ExcelLibrary.dll" \
//reference:"libs\\itextsharp.dll" \
//reference:"libs\\LuaInterface.dll" \
//reference:"libs\\ZedGraph.dll"
ifeq ($(windows), $(softwareplatform))
# Settings for Windows.
ifeq ($(x86-64), $(hardwareplatform))
# Settings for x86-64, Windows.
ifeq ($(release), $(releasetype))
# Settings for x86-64 release, Windows.
compilerflags := \
//noconfig //unsafe- //checked- //nowarn:1701,1702,2008 //nostdlib+ //platform:x64 //warn:4 \
//define:TRACE //main:GKUI.GKProgram //errorendlocation //preferreduilang:en-US //highentropyva- \
$(patsubst //r:%,//reference:%,$(coreresgenref$(dotnet))) $(cscrefs) \
//debug- //filealign:4096 //optimize+ \
//ruleset:$(analysis$(releasetype)ruleset) \
//target:winexe //warnaserror- //utf8output //win32icon:GEDKeeper2.ico //win32manifest:app.manifest
else
# Settings for x86-64 debug, Windows.
compilerflags := \
//noconfig //unsafe- //checked+ //nowarn:1701,1702,2008 //nostdlib+ //platform:x64 //warn:4 \
//define:DEBUG //define:TRACE //main:GKUI.GKProgram //errorendlocation //preferreduilang:en-US //highentropyva- \
$(patsubst //r:%,//reference:%,$(coreresgenref$(dotnet))) $(cscrefs) \
//debug+ //debug:Full //filealign:4096 //optimize- \
//ruleset:$(analysis$(releasetype)ruleset) \
//target:winexe //warnaserror- //utf8output //win32icon:GEDKeeper2.ico //win32manifest:app.manifest
endif
else
ifeq ($(x86), $(hardwareplatform))
# Settings for x86, Windows.
ifeq ($(release), $(releasetype))
# Settings for x86 release, Windows.
compilerflags := \
//noconfig //unsafe- //checked- //nowarn:1701,1702,2008 //nostdlib+ //platform:x86 //warn:4 //baseaddress:4194304 \
//define:TRACE //main:GKUI.GKProgram //errorendlocation //preferreduilang:en-US //highentropyva- \
$(patsubst //r:%,//reference:%,$(coreresgenref$(dotnet))) $(cscrefs) \
//debug- //filealign:4096 //optimize+ \
//ruleset:$(analysis$(releasetype)ruleset) \
//target:winexe //warnaserror- //utf8output //win32icon:GEDKeeper2.ico //win32manifest:app.manifest
else
# Settings for x86 debug, Windows.
compilerflags := \
//noconfig //unsafe- //checked+ //nowarn:1701,1702,2008 //nostdlib+ //platform:x86 //warn:4 //baseaddress:4194304 \
//define:DEBUG //define:TRACE //main:GKUI.GKProgram //errorendlocation //preferreduilang:en-US //highentropyva- \
$(patsubst //r:%,//reference:%,$(coreresgenref$(dotnet))) $(cscrefs) \
//debug+ //debug:Full //filealign:4096 //optimize- \
//ruleset:$(analysis$(releasetype)ruleset) \
//target:winexe //warnaserror- //utf8output //win32icon:GEDKeeper2.ico //win32manifest:app.manifest
endif
else
ifeq ($(any), $(hardwareplatform))
# Settings for any CPU, Windows.
ifeq ($(release), $(releasetype))
# Settings for any CPU release, Windows.
compilerflags := \
//noconfig //unsafe- //checked- //nowarn:1701,1702,2008 //nostdlib+ //platform:anycpu //warn:4 //baseaddress:4194304 \
//define:TRACE //main:GKUI.GKProgram //errorendlocation //preferreduilang:en-US //highentropyva- \
$(patsubst //r:%,//reference:%,$(coreresgenref$(dotnet))) $(cscrefs) \
//debug- //filealign:4096 //optimize+ \
//ruleset:$(analysis$(releasetype)ruleset) \
//target:winexe //warnaserror- //utf8output //win32icon:GEDKeeper2.ico //win32manifest:app.manifest
else
# Settings for any CPU debug, Windows.
compilerflags := \
//noconfig //unsafe- //checked+ //nowarn:1701,1702,2008 //nostdlib+ //platform:anycpu //warn:4 //baseaddress:4194304 \
//define:DEBUG //define:TRACE //main:GKUI.GKProgram //errorendlocation //preferreduilang:en-US //highentropyva- \
$(patsubst //r:%,//reference:%,$(coreresgenref$(dotnet))) $(cscrefs) \
//debug+ //debug:Full //filealign:4096 //optimize- \
//ruleset:$(analysis$(releasetype)ruleset) \
//target:winexe //warnaserror- //utf8output //win32icon:GEDKeeper2.ico //win32manifest:app.manifest
endif
endif
endif
endif
endif
rcflags := \
//useSourcePath \
$(coreresgenref$(dotnet)) \
$(patsubst //reference:%,//r:%,$(cscrefs))
gkdepdlls := \
$(addprefix $(installdir), \
ArborGVT.dll \
ExcelLibrary.dll \
itextsharp.dll \
lua51.dll \
LuaInterface.dll \
ZedGraph.dll \
)

.PHONY: all
all: $(gkcommon) $(outdir)$(project)$(projectext)

.PHONY: install
install: all $(gkdepdlls)
	@cp -f $(outdir)$(project)$(projectext) $(installdir)
	@cd ../$(gkcommon) && $(MAKE) --file=Makefile $@

.PHONY: clean
clean:
	@cd ../$(gkcommon) && $(MAKE) --file=Makefile $@
	@rm -rf -- $(objdir)
	@rm -f $(installdir)$(project)$(projectext)
	@rm -f $(gkdepdlls)

$(objdir):
	@mkdir -p $(objdir)

$(resources): | $(objdir)

$(outdir)$(project)$(projectext): ../$(gkcommon)/obj/$(releasetype)/$(gkcommon).dll $(resources) $(sources)
ifeq ($(windows), $(softwareplatform))
	$(compiler) //out:$(outdirdos)$(project)$(projectext) $(compilerflags) $(patsubst $(objdir)%,//resource:$(objdirdos)\\%,$(resources)) $(sources)
else
	@echo "csc: not yet implemented for $(linux)"
endif
	@echo "**** $(project)$(projectext): build completed ($(releasetype) for $(hardwareplatform), $(softwareplatform))."

$(objdir)%.resources: %.resx
ifeq ($(windows), $(softwareplatform))
	$(rc) $(rcflags) //compile $<,$@
else
	@echo "rc: not yet implemented for $(linux)"
endif

.PHONY: $(gkcommon)
$(gkcommon):
	@cd ../$(gkcommon) && $(MAKE) --file=Makefile

$(installdir)%.dll: libs/%.dll
	@cp -f $< $@
