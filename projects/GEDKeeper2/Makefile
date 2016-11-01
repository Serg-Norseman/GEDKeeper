SHELL := /bin/bash

.DEFAULT_GOAL := all

includemk := ../
include $(includemk)toolchain.mk

project := $(gedkeeper2)
projectext := .exe

objdir := obj/$(releasetype)/
objdirdos := obj\\$(releasetype)\\
outdir := $(objdir)
outdirdos := $(objdirdos)
installdir := ../../

ifeq ($(windows), $(softwareplatform))
  vsproj := GEDKeeper2.mswin.csproj
else
  vsproj := GEDKeeper2.linux.csproj
endif

# Get `sources` via (run from this directory):
# find -regex '.+\.cs' | sed 's/\(.\+\)/\1 \\/' | sort | sed -n "x;/.\+/ p;g;$ s/\(.\+\) \\\\/\1/; $ p"
# Or (this is more valid -- it takes data from a project file):
# grep "[[:space:]]\+<Compile[[:space:]]\+Include=\".*\"\([[:space:]]\?\/\)\?>" GEDKeeper2.mswin.csproj | sed "y/\\\\/\//;s/[[:space:]]\\+<Compile[[:space:]]\+Include=\"\(.*\)\"\([[:space:]]\?\/\)\?>/.\/\1 \\\\/" | sort | sed -n "x;/.\+/ p;g;$ s/\(.\+\) \\\\/\1/; $ p"
# grep "[[:space:]]\+<Compile[[:space:]]\+Include=\".*\"\([[:space:]]\?\/\)\?>" GEDKeeper2.linux.csproj | sed "y/\\\\/\//;s/[[:space:]]\\+<Compile[[:space:]]\+Include=\"\(.*\)\"\([[:space:]]\?\/\)\?>/.\/\1 \\\\/" | sort | sed -n "x;/.\+/ p;g;$ s/\(.\+\) \\\\/\1/; $ p"
# Because `$(sources)` must (may) have different values for Windows and Linux targets.
ifeq ($(windows), $(softwareplatform))

# `$(sources)` for `$(windows)`
sources := \
./Externals/MdiChildFormEx.cs \
./Externals/RusDeclension.cs \
./Externals/SingleInstancing/GlobalMutexPool.cs \
./Externals/SingleInstancing/IpcBroadcast.cs \
./Externals/SingleInstancing/ISingleInstanceEnforcer.cs \
./Externals/SingleInstancing/SingleInstanceException.cs \
./Externals/SingleInstancing/SingleInstanceProxy.cs \
./Externals/SingleInstancing/SingleInstanceTracker.cs \
./GKCore/BaseContext.cs \
./GKCore/BaseSearchStrategy.cs \
./GKCore/Cultures/AncientCulture.cs \
./GKCore/Cultures/IcelandCulture.cs \
./GKCore/Cultures/RussianCulture.cs \
./GKCore/Export/CustomWriter.cs \
./GKCore/Export/ExcelExporter.cs \
./GKCore/Export/Exporter.cs \
./GKCore/Export/FamilyBookExporter.cs \
./GKCore/Export/HTMLWriter.cs \
./GKCore/Export/PDFClownWriter.cs \
./GKCore/Export/PDFJetWriter.cs \
./GKCore/Export/PDFWriter.cs \
./GKCore/Export/PedigreeExporter.cs \
./GKCore/Export/RTFWriter.cs \
./GKCore/GKData.cs \
./GKCore/GKUtils.cs \
./GKCore/Kinships/KinshipRec.cs \
./GKCore/Kinships/KinshipsGraph.cs \
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
./GKCore/Operations/ChangeTracker.cs \
./GKCore/Operations/CustomOperation.cs \
./GKCore/Operations/OrdinaryOperation.cs \
./GKCore/Operations/UndoManager.cs \
./GKCore/Options/AncestorsCircleOptions.cs \
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
./GKUI/BaseWin.cs \
./GKUI/BaseWin.Designer.cs \
./GKUI/Charts/ACOptionsControl.cs \
./GKUI/Charts/ACOptionsControl.Designer.cs \
./GKUI/Charts/AncestorsCircle.cs \
./GKUI/Charts/ChartFilter.cs \
./GKUI/Charts/CircleChart.cs \
./GKUI/Charts/CustomChart.cs \
./GKUI/Charts/DescendantsCircle.cs \
./GKUI/Charts/ITreeControl.cs \
./GKUI/Charts/PersonControl.cs \
./GKUI/Charts/PersonList.cs \
./GKUI/Charts/PersonModifyEventArgs.cs \
./GKUI/Charts/ScaleControl.cs \
./GKUI/Charts/TreeChartBox.cs \
./GKUI/Charts/TreeChartPerson.cs \
./GKUI/CircleChartWin.cs \
./GKUI/CircleChartWin.Designer.cs \
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
./GKUI/Dialogs/EditorDialog.cs \
./GKUI/Dialogs/EventEditDlg.cs \
./GKUI/Dialogs/EventEditDlg.Designer.cs \
./GKUI/Dialogs/FamilyEditDlg.cs \
./GKUI/Dialogs/FamilyEditDlg.Designer.cs \
./GKUI/Dialogs/FilePropertiesDlg.cs \
./GKUI/Dialogs/FilePropertiesDlg.Designer.cs \
./GKUI/Dialogs/GroupEditDlg.cs \
./GKUI/Dialogs/GroupEditDlg.Designer.cs \
./GKUI/Dialogs/LanguageSelectDlg.cs \
./GKUI/Dialogs/LanguageSelectDlg.Designer.cs \
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
./GKUI/Dialogs/PortraitSelectDlg.cs \
./GKUI/Dialogs/PortraitSelectDlg.Designer.cs \
./GKUI/Dialogs/ProgressDlg.cs \
./GKUI/Dialogs/ProgressDlg.Designer.cs \
./GKUI/Dialogs/RecordSelectDlg.cs \
./GKUI/Dialogs/RecordSelectDlg.Designer.cs \
./GKUI/Dialogs/RelationshipCalculatorDlg.cs \
./GKUI/Dialogs/RelationshipCalculatorDlg.Designer.cs \
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

else

# `$(sources)` for `$(linux)`
sources := \
./Externals/MdiChildFormEx.cs \
./Externals/RusDeclension.cs \
./Externals/SingleInstancing/GlobalMutexPool.cs \
./Externals/SingleInstancing/IpcBroadcast.cs \
./Externals/SingleInstancing/ISingleInstanceEnforcer.cs \
./Externals/SingleInstancing/SingleInstanceException.cs \
./Externals/SingleInstancing/SingleInstanceProxy.cs \
./Externals/SingleInstancing/SingleInstanceTracker.cs \
./GKCore/BaseContext.cs \
./GKCore/BaseSearchStrategy.cs \
./GKCore/Cultures/AncientCulture.cs \
./GKCore/Cultures/IcelandCulture.cs \
./GKCore/Cultures/RussianCulture.cs \
./GKCore/Export/CustomWriter.cs \
./GKCore/Export/ExcelExporter.cs \
./GKCore/Export/Exporter.cs \
./GKCore/Export/FamilyBookExporter.cs \
./GKCore/Export/HTMLWriter.cs \
./GKCore/Export/PDFClownWriter.cs \
./GKCore/Export/PDFJetWriter.cs \
./GKCore/Export/PDFWriter.cs \
./GKCore/Export/PedigreeExporter.cs \
./GKCore/Export/RTFWriter.cs \
./GKCore/GKData.cs \
./GKCore/GKUtils.cs \
./GKCore/Kinships/KinshipRec.cs \
./GKCore/Kinships/KinshipsGraph.cs \
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
./GKCore/Operations/ChangeTracker.cs \
./GKCore/Operations/CustomOperation.cs \
./GKCore/Operations/OrdinaryOperation.cs \
./GKCore/Operations/UndoManager.cs \
./GKCore/Options/AncestorsCircleOptions.cs \
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
./GKUI/BaseWin.cs \
./GKUI/BaseWin.Designer.cs \
./GKUI/Charts/ACOptionsControl.cs \
./GKUI/Charts/ACOptionsControl.Designer.cs \
./GKUI/Charts/AncestorsCircle.cs \
./GKUI/Charts/ChartFilter.cs \
./GKUI/Charts/CircleChart.cs \
./GKUI/Charts/CustomChart.cs \
./GKUI/Charts/DescendantsCircle.cs \
./GKUI/Charts/ITreeControl.cs \
./GKUI/Charts/PersonControl.cs \
./GKUI/Charts/PersonList.cs \
./GKUI/Charts/PersonModifyEventArgs.cs \
./GKUI/Charts/ScaleControl.cs \
./GKUI/Charts/TreeChartBox.cs \
./GKUI/Charts/TreeChartPerson.cs \
./GKUI/CircleChartWin.cs \
./GKUI/CircleChartWin.Designer.cs \
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
./GKUI/Dialogs/EditorDialog.cs \
./GKUI/Dialogs/EventEditDlg.cs \
./GKUI/Dialogs/EventEditDlg.Designer.cs \
./GKUI/Dialogs/FamilyEditDlg.cs \
./GKUI/Dialogs/FamilyEditDlg.Designer.cs \
./GKUI/Dialogs/FilePropertiesDlg.cs \
./GKUI/Dialogs/FilePropertiesDlg.Designer.cs \
./GKUI/Dialogs/GroupEditDlg.cs \
./GKUI/Dialogs/GroupEditDlg.Designer.cs \
./GKUI/Dialogs/LanguageSelectDlg.cs \
./GKUI/Dialogs/LanguageSelectDlg.Designer.cs \
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
./GKUI/Dialogs/PortraitSelectDlg.cs \
./GKUI/Dialogs/PortraitSelectDlg.Designer.cs \
./GKUI/Dialogs/ProgressDlg.cs \
./GKUI/Dialogs/ProgressDlg.Designer.cs \
./GKUI/Dialogs/RecordSelectDlg.cs \
./GKUI/Dialogs/RecordSelectDlg.Designer.cs \
./GKUI/Dialogs/RelationshipCalculatorDlg.cs \
./GKUI/Dialogs/RelationshipCalculatorDlg.Designer.cs \
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

endif

# Get files list for `resources` via (run from this directory):
# find -regex '.+\.resx' | sed 's/.\+\/\(.\+\)\.resx/\1.resources \\/' | sort
# Or (this is more valid -- it takes data from a project file):
# sed -n -e "/[[:space:]]\+<EmbeddedResource[[:space:]]\+Include=\".*\"\([[:space:]]\?\/\)\?>/ {y/\\\\/\//;s/[[:space:]]\\+<EmbeddedResource[[:space:]]\+Include=.*[\"]\(.*\)\.resx\"\([[:space:]]\?\/\)\?>/\1.resources \\\\/;s/\//\./g;P}" < GEDKeeper2.mswin.csproj | sort
# sed -n -e "/[[:space:]]\+<EmbeddedResource[[:space:]]\+Include=\".*\"\([[:space:]]\?\/\)\?>/ {y/\\\\/\//;s/[[:space:]]\\+<EmbeddedResource[[:space:]]\+Include=.*[\"]\(.*\)\.resx\"\([[:space:]]\?\/\)\?>/\1.resources \\\\/;s/\//\./g;P}" < GEDKeeper2.linux.csproj | sort
# Because `$(resources)` must (may) have different values for Windows and Linux targets.
ifeq ($(windows), $(softwareplatform))

# `$(resources)` for `$(windows)`
resources := \
$(addprefix $(objdir), \
GKResources.resources \
GKUI.BaseWin.resources \
GKUI.Charts.ACOptionsControl.resources \
GKUI.CircleChartWin.resources \
GKUI.Controls.GKInputBox.resources \
GKUI.Controls.GKMergeControl.resources \
GKUI.Dialogs.AboutDlg.resources \
GKUI.Dialogs.AddressEditDlg.resources \
GKUI.Dialogs.AssociationEditDlg.resources \
GKUI.Dialogs.CommonFilterDlg.resources \
GKUI.Dialogs.CommunicationEditDlg.resources \
GKUI.Dialogs.DayTipsDlg.resources \
GKUI.Dialogs.EventEditDlg.resources \
GKUI.Dialogs.FamilyEditDlg.resources \
GKUI.Dialogs.FilePropertiesDlg.resources \
GKUI.Dialogs.GroupEditDlg.resources \
GKUI.Dialogs.LanguageSelectDlg.resources \
GKUI.Dialogs.LocationEditDlg.resources \
GKUI.Dialogs.MediaEditDlg.resources \
GKUI.Dialogs.NameEditDlg.resources \
GKUI.Dialogs.NoteEditDlg.resources \
GKUI.Dialogs.OptionsDlg.resources \
GKUI.Dialogs.PersonalNameEditDlg.resources \
GKUI.Dialogs.PersonEditDlg.resources \
GKUI.Dialogs.PersonNewDlg.resources \
GKUI.Dialogs.PersonsFilterDlg.resources \
GKUI.Dialogs.PortraitSelectDlg.resources \
GKUI.Dialogs.ProgressDlg.resources \
GKUI.Dialogs.RecordSelectDlg.resources \
GKUI.Dialogs.RelationshipCalculatorDlg.resources \
GKUI.Dialogs.RepositoryEditDlg.resources \
GKUI.Dialogs.ResearchEditDlg.resources \
GKUI.Dialogs.SexCheckDlg.resources \
GKUI.Dialogs.SourceCitEditDlg.resources \
GKUI.Dialogs.SourceEditDlg.resources \
GKUI.Dialogs.TaskEditDlg.resources \
GKUI.Dialogs.TreeFilterDlg.resources \
GKUI.Dialogs.UserRefEditDlg.resources \
GKUI.MainWin.resources \
GKUI.MapsViewerWin.resources \
GKUI.MediaViewerWin.resources \
GKUI.OrganizerWin.resources \
GKUI.PatriarchsViewerWin.resources \
GKUI.ScriptEditWin.resources \
GKUI.SlideshowWin.resources \
GKUI.StatisticsWin.resources \
GKUI.TreeChartWin.resources \
GKUI.TreeToolsWin.resources \
)

else

# `$(resources)` for `$(linux)`
resources := \
$(addprefix $(objdir), \
GKResources.resources \
GKUI.BaseWin.resources \
GKUI.Charts.ACOptionsControl.resources \
GKUI.CircleChartWin.resources \
GKUI.Controls.GKInputBox.resources \
GKUI.Controls.GKMergeControl.resources \
GKUI.Dialogs.AboutDlg.resources \
GKUI.Dialogs.AddressEditDlg.resources \
GKUI.Dialogs.AssociationEditDlg.resources \
GKUI.Dialogs.CommonFilterDlg.resources \
GKUI.Dialogs.CommunicationEditDlg.resources \
GKUI.Dialogs.DayTipsDlg.resources \
GKUI.Dialogs.EventEditDlg.resources \
GKUI.Dialogs.FamilyEditDlg.resources \
GKUI.Dialogs.FilePropertiesDlg.resources \
GKUI.Dialogs.GroupEditDlg.resources \
GKUI.Dialogs.LanguageSelectDlg.resources \
GKUI.Dialogs.LocationEditDlg.resources \
GKUI.Dialogs.MediaEditDlg.resources \
GKUI.Dialogs.NameEditDlg.resources \
GKUI.Dialogs.NoteEditDlg.resources \
GKUI.Dialogs.OptionsDlg.resources \
GKUI.Dialogs.PersonalNameEditDlg.resources \
GKUI.Dialogs.PersonEditDlg.resources \
GKUI.Dialogs.PersonNewDlg.resources \
GKUI.Dialogs.PersonsFilterDlg.resources \
GKUI.Dialogs.PortraitSelectDlg.resources \
GKUI.Dialogs.ProgressDlg.resources \
GKUI.Dialogs.RecordSelectDlg.resources \
GKUI.Dialogs.RelationshipCalculatorDlg.resources \
GKUI.Dialogs.RepositoryEditDlg.resources \
GKUI.Dialogs.ResearchEditDlg.resources \
GKUI.Dialogs.SexCheckDlg.resources \
GKUI.Dialogs.SourceCitEditDlg.resources \
GKUI.Dialogs.SourceEditDlg.resources \
GKUI.Dialogs.TaskEditDlg.resources \
GKUI.Dialogs.TreeFilterDlg.resources \
GKUI.Dialogs.UserRefEditDlg.resources \
GKUI.MainWin.resources \
GKUI.MapsViewerWin.resources \
GKUI.MediaViewerWin.resources \
GKUI.OrganizerWin.resources \
GKUI.PatriarchsViewerWin.resources \
GKUI.ScriptEditWin.resources \
GKUI.SlideshowWin.resources \
GKUI.StatisticsWin.resources \
GKUI.TreeChartWin.resources \
GKUI.TreeToolsWin.resources \
)

endif

resdep := $(objdir)resmake.d

cscrefs := \
//reference:"..\\$(gkcommon)\\obj\\$(releasetype)\\$(gkcommon).dll" \
//reference:"libs\ArborGVT.dll" \
//reference:"libs\DotNetRtfWriter.dll" \
//reference:"libs\\ExcelLibrary.dll" \
//reference:"libs\\itextsharp.dll" \
//reference:"libs\\mswin\\LuaInterface.dll" \
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
DotNetRtfWriter.dll \
ExcelLibrary.dll \
itextsharp.dll \
mswin/lua51.dll \
mswin/LuaInterface.dll \
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

$(resdep): | $(objdir)

$(outdir)$(project)$(projectext): ../$(gkcommon)/obj/$(releasetype)/$(gkcommon).dll $(resdep) $(resources) $(sources)
ifeq ($(windows), $(softwareplatform))
	$(compiler) //out:$(outdirdos)$(project)$(projectext) $(compilerflags) $(patsubst $(objdir)%,//resource:$(objdirdos)\\%,$(resources)) $(sources)
else
	@echo "csc: not yet implemented for $(linux)"
endif
	@echo "**** $(project)$(projectext): build completed ($(releasetype) for $(hardwareplatform), $(softwareplatform))."

$(resdep): $(vsproj)
	sed -n -e "/[[:space:]]\+<EmbeddedResource[[:space:]]\+Include=\".*\"\([[:space:]]\?\/\)\?>/ {y/\\\\/\//;s/[[:space:]]\\+<EmbeddedResource[[:space:]]\+Include=.*[\"]\(.*\.resx\)\"\([[:space:]]\?\/\)\?>/\1/;h;y/\//\./;s/\(.\+\.\)resx/$(subst /,\\/,$(objdir))\1resources/;G;s/\(.\+\)\n\(.\+\)/\1: \2\n\t$$\(rc\) $$\(rcflags\) \/\/compile $$<,$$\@/p}" < $< > $@

-include $(resdep)

.PHONY: $(gkcommon)
$(gkcommon):
	@cd ../$(gkcommon) && $(MAKE) --file=Makefile

$(installdir)%.dll: libs/%.dll
	@cp -f $< $(installdir)$(@F)
