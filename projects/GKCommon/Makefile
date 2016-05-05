SHELL := /bin/bash

.DEFAULT_GOAL := all

include ../variables.mk
include ../windows.mk

compiler := $($(softwareplatform)compilerdos$(hardwareplatform))
rc := $($(softwareplatform)rcdos$(hardwareplatform))

project := $(gkcommon)
projectext := .dll

vpath %.resx \
GKCommon/Controls

objdir := obj/$(releasetype)/
objdirdos := obj\\$(releasetype)\\
outdir := $(objdir)
outdirdos := $(objdirdos)
# Get `sources` via:
# find -regex '.+\.cs' | sed 's/\(.\+\)/\1 \\/'
# Just do not forget to remove trailing " \" on the last line.
sources := \
./AssemblyInfo.cs \
./Externals/CSV/CSVReader.cs \
./Externals/CSV/StringConverter.cs \
./Externals/IniFiles/IniFileBlankLine.cs \
./Externals/IniFiles/IniFileCommentary.cs \
./Externals/IniFiles/IniFileElement.cs \
./Externals/IniFiles/IniFileReader.cs \
./Externals/IniFiles/IniFiles.cs \
./Externals/IniFiles/IniFileSection.cs \
./Externals/IniFiles/IniFileSectionStart.cs \
./Externals/IniFiles/IniFileSettings.cs \
./Externals/IniFiles/IniFileValue.cs \
./Externals/IniFiles/IniFileWriter.cs \
./Externals/ListTimSort.cs \
./Externals/MapiMailMessage.cs \
./Externals/ZipStorer.cs \
./GEDCOM/EGEDCOMException.cs \
./GEDCOM/GEDCOMAddress.cs \
./GEDCOM/GEDCOMAlias.cs \
./GEDCOM/GEDCOMAssociation.cs \
./GEDCOM/GEDCOMChangeDate.cs \
./GEDCOM/GEDCOMChildToFamilyLink.cs \
./GEDCOM/GEDCOMCommunicationRecord.cs \
./GEDCOM/GEDCOMConsts.cs \
./GEDCOM/GEDCOMCustomDate.cs \
./GEDCOM/GEDCOMCustomEvent.cs \
./GEDCOM/GEDCOMCustomRecord.cs \
./GEDCOM/GEDCOMData.cs \
./GEDCOM/GEDCOMDate.cs \
./GEDCOM/GEDCOMDateApproximated.cs \
./GEDCOM/GEDCOMDateExact.cs \
./GEDCOM/GEDCOMDateInterpreted.cs \
./GEDCOM/GEDCOMDatePeriod.cs \
./GEDCOM/GEDCOMDateRange.cs \
./GEDCOM/GEDCOMDateStatus.cs \
./GEDCOM/GEDCOMDateValue.cs \
./GEDCOM/GEDCOMDelegates.cs \
./GEDCOM/GEDCOMEnums.cs \
./GEDCOM/GEDCOMEvent.cs \
./GEDCOM/GEDCOMEventDetail.cs \
./GEDCOM/GEDCOMFactory.cs \
./GEDCOM/GEDCOMFamilyEvent.cs \
./GEDCOM/GEDCOMFamilyRecord.cs \
./GEDCOM/GEDCOMFileReference.cs \
./GEDCOM/GEDCOMFileReferenceWithTitle.cs \
./GEDCOM/GEDCOMGroupRecord.cs \
./GEDCOM/GEDCOMHeader.cs \
./GEDCOM/GEDCOMIndividualAttribute.cs \
./GEDCOM/GEDCOMIndividualEvent.cs \
./GEDCOM/GEDCOMIndividualOrdinance.cs \
./GEDCOM/GEDCOMIndividualRecord.cs \
./GEDCOM/GEDCOMInterfaces.cs \
./GEDCOM/GEDCOMList.cs \
./GEDCOM/GEDCOMLocationRecord.cs \
./GEDCOM/GEDCOMMap.cs \
./GEDCOM/GEDCOMMultimediaLink.cs \
./GEDCOM/GEDCOMMultimediaRecord.cs \
./GEDCOM/GEDCOMNoteRecord.cs \
./GEDCOM/GEDCOMNotes.cs \
./GEDCOM/GEDCOMObject.cs \
./GEDCOM/GEDCOMPersonalName.cs \
./GEDCOM/GEDCOMPersonalNamePieces.cs \
./GEDCOM/GEDCOMPlace.cs \
./GEDCOM/GEDCOMPointer.cs \
./GEDCOM/GEDCOMPointerWithNotes.cs \
./GEDCOM/GEDCOMRecord.cs \
./GEDCOM/GEDCOMRecordWithEvents.cs \
./GEDCOM/GEDCOMRepositoryCitation.cs \
./GEDCOM/GEDCOMRepositoryRecord.cs \
./GEDCOM/GEDCOMResearchRecord.cs \
./GEDCOM/GEDCOMSourceCitation.cs \
./GEDCOM/GEDCOMSourceRecord.cs \
./GEDCOM/GEDCOMSpouseSealing.cs \
./GEDCOM/GEDCOMSpouseToFamilyLink.cs \
./GEDCOM/GEDCOMSubmissionRecord.cs \
./GEDCOM/GEDCOMSubmitterRecord.cs \
./GEDCOM/GEDCOMTag.cs \
./GEDCOM/GEDCOMTagWithLists.cs \
./GEDCOM/GEDCOMTaskRecord.cs \
./GEDCOM/GEDCOMTime.cs \
./GEDCOM/GEDCOMTree.cs \
./GEDCOM/GEDCOMUserReference.cs \
./GEDCOM/GEDCOMUtils.cs \
./GEDCOM/MatchParams.cs \
./GEDCOM/XRefReplacer.cs \
./GKCommon/BaseObject.cs \
./GKCommon/CalendarConverter.cs \
./GKCommon/CalendarData.cs \
./GKCommon/Controls/ExtResources.Designer.cs \
./GKCommon/Controls/GKScrollableControl.cs \
./GKCommon/Controls/HyperView.cs \
./GKCommon/Controls/ImageBox.cs \
./GKCommon/Controls/ImageView.cs \
./GKCommon/Controls/ImageView.Designer.cs \
./GKCommon/Controls/LogChart.cs \
./GKCommon/Controls/ReflectionHelper.cs \
./GKCommon/Controls/SlideList.cs \
./GKCommon/Controls/WizardPages.cs \
./GKCommon/ConvHelper.cs \
./GKCommon/CRC32.cs \
./GKCommon/EnumSet.cs \
./GKCommon/ExtList.cs \
./GKCommon/ExtRect.cs \
./GKCommon/FileHelper.cs \
./GKCommon/GfxHelper.cs \
./GKCommon/IndistinctMatching.cs \
./GKCommon/IniFile.cs \
./GKCommon/Logger.cs \
./GKCommon/Range.cs \
./GKCommon/SmartGraph/Edge.cs \
./GKCommon/SmartGraph/Graph.cs \
./GKCommon/SmartGraph/GraphvizWriter.cs \
./GKCommon/SmartGraph/IDataProvider.cs \
./GKCommon/SmartGraph/IEdge.cs \
./GKCommon/SmartGraph/IGraph.cs \
./GKCommon/SmartGraph/IVertex.cs \
./GKCommon/SmartGraph/Vertex.cs \
./GKCommon/SortHelper.cs \
./GKCommon/StringList.cs \
./GKCommon/StringTokenizer.cs \
./GKCommon/SysInfo.cs \
./GKCommon/Tween.cs \
./GKCommon/ValuesCollection.cs \
./GKCore/GKConsts.cs \
./GKCore/Interfaces/IBaseContext.cs \
./GKCore/Interfaces/IBaseEditor.cs \
./GKCore/Interfaces/IBaseWindow.cs \
./GKCore/Interfaces/IChartWindow.cs \
./GKCore/Interfaces/ICustomFilter.cs \
./GKCore/Interfaces/IHost.cs \
./GKCore/Interfaces/ILangMan.cs \
./GKCore/Interfaces/IListFilter.cs \
./GKCore/Interfaces/IListManager.cs \
./GKCore/Interfaces/ILocalization.cs \
./GKCore/Interfaces/INamesTable.cs \
./GKCore/Interfaces/IOptions.cs \
./GKCore/Interfaces/IOptionsControl.cs \
./GKCore/Interfaces/IPlugin.cs \
./GKCore/Interfaces/IProgressController.cs \
./GKCore/Interfaces/ISearchStrategy.cs \
./GKCore/Interfaces/ISubscriber.cs \
./GKCore/Interfaces/IWidget.cs \
./GKCore/Interfaces/IWorkWindow.cs \
./GKCore/Types/DateFormat.cs \
./GKCore/Types/FamilyTarget.cs \
./GKCore/Types/FilterGroupMode.cs \
./GKCore/Types/FilterLifeMode.cs \
./GKCore/Types/MediaFileNotFoundException.cs \
./GKCore/Types/MediaStoreType.cs \
./GKCore/Types/MultimediaKind.cs \
./GKCore/Types/NameEntry.cs \
./GKCore/Types/NameFormat.cs \
./GKCore/Types/PatriarchObj.cs \
./GKCore/Types/PedigreeFormat.cs \
./GKCore/Types/PersonEventKind.cs \
./GKCore/Types/PGNode.cs \
./GKCore/Types/PGNodeType.cs \
./GKCore/Types/RecordAction.cs \
./GKCore/Types/RelationKind.cs \
./GKCore/Types/ShieldState.cs \
./GKCore/Types/SpecialUserRef.cs \
./GKCore/Types/TargetMode.cs \
./GKCore/Types/WomanSurnameFormat.cs
resources := \
$(addprefix $(objdir), \
ExtResources.resources \
SlideList.resources)
ifeq ($(windows), $(softwareplatform))
# Settings for Windows.
ifeq ($(x86-64), $(hardwareplatform))
# Settings for x86-64, Windows.
ifeq ($(release), $(releasetype))
# Settings for x86-64 release, Windows.
compilerflags := \
//noconfig //unsafe- //checked- //nowarn:1701,1702,2008 //nostdlib+ //platform:x64 //warn:4 \
//define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref)) \
//debug- //filealign:4096 //optimize+ \
//ruleset:"C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\Team Tools\\Static Analysis Tools\\Rule Sets\\MinimumRecommendedRules.ruleset" //subsystemversion:6.00 \
//target:library //warnaserror- //utf8output
else
# Settings for x86-64 debug, Windows.
compilerflags := \
//noconfig //unsafe- //checked+ //nowarn:1701,1702,2008 //nostdlib+ //platform:x64 //warn:4 \
//define:DEBUG //define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref)) \
//debug+ //debug:Full //filealign:4096 //optimize- \
//ruleset:..\\gk21.ruleset //subsystemversion:6.00 \
//target:library //warnaserror- //utf8output
endif
else
ifeq ($(x86), $(hardwareplatform))
# Settings for x86, Windows.
ifeq ($(release), $(releasetype))
# Settings for x86 release, Windows.
compilerflags := \
//noconfig //unsafe- //checked- //nowarn:1701,1702,2008 //nostdlib+ //platform:x86 //warn:4 /baseaddress:4194304 \
//define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref)) \
//debug- //filealign:4096 //optimize+ \
//ruleset:"C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\Team Tools\\Static Analysis Tools\\Rule Sets\\MinimumRecommendedRules.ruleset" //subsystemversion:6.00 \
//target:library //warnaserror- //utf8output
else
# Settings for x86 debug, Windows.
compilerflags := \
//noconfig //unsafe- //checked+ //nowarn:1701,1702,2008 //nostdlib+ //platform:x86 //warn:4 /baseaddress:4194304 \
//define:DEBUG //define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref)) \
//debug+ //debug:Full //filealign:4096 //optimize- \
//ruleset:..\\gk21.ruleset //subsystemversion:6.00 \
//target:library //warnaserror- //utf8output
endif
else
ifeq ($(any), $(hardwareplatform))
# Settings for any CPU, Windows.
ifeq ($(release), $(releasetype))
# Settings for any CPU release, Windows.
compilerflags := \
//noconfig //unsafe- //checked- //nowarn:1701,1702,2008 //nostdlib+ //platform:x86 //warn:4 /baseaddress:4194304 \
//define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref)) \
//debug- //filealign:4096 //optimize+ \
//ruleset:"C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\Team Tools\\Static Analysis Tools\\Rule Sets\\MinimumRecommendedRules.ruleset" //subsystemversion:6.00 \
//target:library //warnaserror- //utf8output
else
# Settings for any CPU debug, Windows.
compilerflags := \
//noconfig //unsafe- //checked+ //nowarn:1701,1702,2008 //nostdlib+ //platform:x86 //warn:4 /baseaddress:4194304 \
//define:DEBUG //define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref)) \
//debug+ //debug:Full //filealign:4096 //optimize- \
//ruleset:..\\gk21.ruleset //subsystemversion:6.00 \
//target:library //warnaserror- //utf8output
endif
endif
endif
endif
endif
rcflags := \
//useSourcePath \
$(coreresgenref)

.PHONY: all
all: $(outdir)$(project)$(projectext)

.PHONY: clean
clean:
	@rm -rf -- $(objdir)
	@echo "$(objdir) removed"

$(objdir):
	@mkdir -p $(objdir)

$(resources): | $(objdir)

$(outdir)$(project)$(projectext): $(resources) $(sources)
ifeq ($(windows), $(softwareplatform))
	$(compiler) //out:$(outdirdos)$(project)$(projectext) $(compilerflags) $(patsubst $(objdir)%,//resource:$(objdirdos)\\%,$(resources)) $(sources)
else
	@echo "Not yet implemented"
endif
	@echo "**** $(project)$(projectext): build completed ($(releasetype) for $(hardwareplatform), $(softwareplatform))."

$(objdir)%.resources: %.resx
ifeq ($(windows), $(softwareplatform))
	$(rc) $(rcflags) //compile $<,$@
else
	@echo "Not yet implemented"
endif
