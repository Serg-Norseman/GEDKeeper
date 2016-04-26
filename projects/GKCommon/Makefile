SHELL := /bin/bash

.DEFAULT_GOAL := all

include ../values.mk
include ../windows.mk

# Define target hardware platform. Supported values: `x86-64`, `x86` and `any`
hardwareplatform := $(any)
# Define target sowftware platform. Supported values: `windows` and `linux`
softwareplatform := $(windows)
# Define release type. Supported values: `debug` and `release`
releasetype := $(release)

ifndef hardwareplatform
$(error Target hardware platform is not defined. The `hardwareplatform` variable must be set to `$(x86-64)`, `$(x86)` or `$(any)`)
endif
ifndef softwareplatform
$(error Target software platform is not defined. The `softwareplatform` variable must be set to `$(windows)` or `$(linux)`)
endif
ifndef releasetype
$(error Release type is not defined. The `releasetype` variable must be set to `$(release)` or `$(debug)`)
endif

ifneq ($(x86-64), $(hardwareplatform))
ifneq ($(x86), $(hardwareplatform))
ifneq ($(any), $(hardwareplatform))
$(error `$(hardwareplatform)` is an incorrect value for the `hardwareplatform` variable; must be set to `$(x86-64)`, `$(x86)` or `$(any)`)
endif
endif
endif

ifneq ($(windows), $(softwareplatform))
ifneq ($(linux), $(softwareplatform))
$(error `$(softwareplatform)` is an incorrect value for the `softwareplatform` variable; must be set to `$(windows)` or `$(linux)`)
endif
endif

ifneq ($(debug), $(releasetype))
ifneq ($(release), $(releasetype))
$(error `$(releasetype)` is an incorrect value for the `releasetype` variable; must be set to `$(debug)` or `$(release)`)
endif
endif

compiler := $($(softwareplatform)compilerdos$(hardwareplatform))
rc := $($(softwareplatform)rcdos$(hardwareplatform))

project := $(gkcommon)
projectext := .dll

vpath %.cs \
Externals Externals/CSV Externals/IniFiles Externals/SingleInstancing \
GEDCOM \
GKCommon GKCommon/Controls GKCommon/SmartGraph \
GKCore GKCore/Interfaces GKCore/Types

vpath %.resx GKCommon/Controls

objdir := obj/$(releasetype)/
objdirdos := obj\\$(releasetype)\\
outdir := $(objdir)
outdirdos := $(objdirdos)
sources := \
AssemblyInfo.cs \
BaseObject.cs \
CalendarConverter.cs \
CalendarData.cs \
ConvHelper.cs \
CRC32.cs \
CSVReader.cs \
DateFormat.cs \
Edge.cs \
EGEDCOMException.cs \
EnumSet.cs \
ExtList.cs \
ExtRect.cs \
ExtResources.Designer.cs \
FamilyTarget.cs \
FileHelper.cs \
FilterGroupMode.cs \
FilterLifeMode.cs \
GEDCOMAddress.cs \
GEDCOMAlias.cs \
GEDCOMAssociation.cs \
GEDCOMChangeDate.cs \
GEDCOMChildToFamilyLink.cs \
GEDCOMCommunicationRecord.cs \
GEDCOMConsts.cs \
GEDCOMCustomDate.cs \
GEDCOMCustomEvent.cs \
GEDCOMCustomRecord.cs \
GEDCOMData.cs \
GEDCOMDate.cs \
GEDCOMDateApproximated.cs \
GEDCOMDateExact.cs \
GEDCOMDateInterpreted.cs \
GEDCOMDatePeriod.cs \
GEDCOMDateRange.cs \
GEDCOMDateStatus.cs \
GEDCOMDateValue.cs \
GEDCOMDelegates.cs \
GEDCOMEnums.cs \
GEDCOMEvent.cs \
GEDCOMEventDetail.cs \
GEDCOMFactory.cs \
GEDCOMFamilyEvent.cs \
GEDCOMFamilyRecord.cs \
GEDCOMFileReference.cs \
GEDCOMFileReferenceWithTitle.cs \
GEDCOMGroupRecord.cs \
GEDCOMHeader.cs \
GEDCOMIndividualAttribute.cs \
GEDCOMIndividualEvent.cs \
GEDCOMIndividualOrdinance.cs \
GEDCOMIndividualRecord.cs \
GEDCOMInterfaces.cs \
GEDCOMList.cs \
GEDCOMLocationRecord.cs \
GEDCOMMap.cs \
GEDCOMMultimediaLink.cs \
GEDCOMMultimediaRecord.cs \
GEDCOMNoteRecord.cs \
GEDCOMNotes.cs \
GEDCOMObject.cs \
GEDCOMPersonalName.cs \
GEDCOMPersonalNamePieces.cs \
GEDCOMPlace.cs \
GEDCOMPointer.cs \
GEDCOMPointerWithNotes.cs \
GEDCOMRecord.cs \
GEDCOMRecordWithEvents.cs \
GEDCOMRepositoryCitation.cs \
GEDCOMRepositoryRecord.cs \
GEDCOMResearchRecord.cs \
GEDCOMSourceCitation.cs \
GEDCOMSourceRecord.cs \
GEDCOMSpouseSealing.cs \
GEDCOMSpouseToFamilyLink.cs \
GEDCOMSubmissionRecord.cs \
GEDCOMSubmitterRecord.cs \
GEDCOMTag.cs \
GEDCOMTagWithLists.cs \
GEDCOMTaskRecord.cs \
GEDCOMTime.cs \
GEDCOMTree.cs \
GEDCOMUserReference.cs \
GEDCOMUtils.cs \
GfxHelper.cs \
GKConsts.cs \
GKScrollableControl.cs \
Graph.cs \
GraphvizWriter.cs \
HyperView.cs \
IBaseContext.cs \
IBaseEditor.cs \
IBaseWindow.cs \
IChartWindow.cs \
ICustomFilter.cs \
IDataProvider.cs \
IEdge.cs \
IGraph.cs \
IHost.cs \
ILangMan.cs \
IListFilter.cs \
IListManager.cs \
ILocalization.cs \
ImageBox.cs \
ImageView.cs \
ImageView.Designer.cs \
INamesTable.cs \
IndistinctMatching.cs \
IniFile.cs \
IniFileBlankLine.cs \
IniFileCommentary.cs \
IniFileElement.cs \
IniFileReader.cs \
IniFiles.cs \
IniFileSection.cs \
IniFileSectionStart.cs \
IniFileSettings.cs \
IniFileValue.cs \
IniFileWriter.cs \
IOptions.cs \
IOptionsControl.cs \
IPlugin.cs \
IProgressController.cs \
ISearchStrategy.cs \
ISingleInstanceEnforcer.cs \
ISubscriber.cs \
IVertex.cs \
IWidget.cs \
IWorkWindow.cs \
ListTimSort.cs \
LogChart.cs \
Logger.cs \
MapiMailMessage.cs \
MatchParams.cs \
MediaFileNotFoundException.cs \
MediaStoreType.cs \
MessageEventArgs.cs \
MultimediaKind.cs \
NameEntry.cs \
NameFormat.cs \
PatriarchObj.cs \
PedigreeFormat.cs \
PersonEventKind.cs \
PGNode.cs \
PGNodeType.cs \
Range.cs \
RecordAction.cs \
RelationKind.cs \
ShieldState.cs \
SingleInstanceException.cs \
SingleInstanceProxy.cs \
SingleInstanceTracker.cs \
SlideList.cs \
SortHelper.cs \
SpecialUserRef.cs \
StringConverter.cs \
StringList.cs \
StringTokenizer.cs \
SysInfo.cs \
TargetMode.cs \
Tween.cs \
ValuesCollection.cs \
Vertex.cs \
WizardPages.cs \
WomanSurnameFormat.cs \
XRefReplacer.cs \
ZipStorer.cs
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
//resource:$(objdirdos)ExtResources.resources //resource:$(objdirdos)SlideList.resources \
//target:library //warnaserror- //utf8output
else
# Settings for x86-64 debug, Windows.
compilerflags := \
//noconfig //unsafe- //checked+ //nowarn:1701,1702,2008 //nostdlib+ //platform:x64 //warn:4 \
//define:DEBUG //define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref)) \
//debug+ //debug:Full //filealign:4096 //optimize- \
//ruleset:..\\gk21.ruleset //subsystemversion:6.00 \
//resource:$(objdirdos)ExtResources.resources //resource:$(objdirdos)SlideList.resources \
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
//resource:$(objdirdos)ExtResources.resources //resource:$(objdirdos)SlideList.resources \
//target:library //warnaserror- //utf8output
else
# Settings for x86 debug, Windows.
compilerflags := \
//noconfig //unsafe- //checked+ //nowarn:1701,1702,2008 //nostdlib+ //platform:x86 //warn:4 /baseaddress:4194304 \
//define:DEBUG //define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref)) \
//debug+ //debug:Full //filealign:4096 //optimize- \
//ruleset:..\\gk21.ruleset //subsystemversion:6.00 \
//resource:$(objdirdos)ExtResources.resources //resource:$(objdirdos)SlideList.resources \
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
//resource:$(objdirdos)ExtResources.resources //resource:$(objdirdos)SlideList.resources \
//target:library //warnaserror- //utf8output
else
# Settings for any CPU debug, Windows.
compilerflags := \
//noconfig //unsafe- //checked+ //nowarn:1701,1702,2008 //nostdlib+ //platform:x86 //warn:4 /baseaddress:4194304 \
//define:DEBUG //define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref)) \
//debug+ //debug:Full //filealign:4096 //optimize- \
//ruleset:..\\gk21.ruleset //subsystemversion:6.00 \
//resource:$(objdirdos)ExtResources.resources //resource:$(objdirdos)SlideList.resources \
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

$(outdir)$(project)$(projectext): $(resources) buildcs
	@echo "**** $(project)$(projectext): build completed ($(releasetype) for $(hardwareplatform), $(softwareplatform))."

.PHONY: buildcs
buildcs: $(sources)
ifeq ($(windows), $(softwareplatform))
	$(compiler) //out:$(outdirdos)$(project)$(projectext) $(compilerflags) "Externals\\SingleInstancing\\Global Delegates.cs" $^
else
	@echo "Not yet implemented"
endif

$(objdir)%.resources: %.resx
	$(rc) $(rcflags) //compile $<,$@
