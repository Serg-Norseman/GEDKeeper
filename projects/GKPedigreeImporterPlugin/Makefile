SHELL := /bin/bash

.DEFAULT_GOAL := all

includemk := ../
include $(includemk)toolchain.mk

project := $(gkpedigreeimporterplugin)
projectext := .dll

# Get directories list for `vpath %.resx` via (run from this directory):
# find -regex '.+\.resx' | sed 's/\(.\+\)\/.\+/\1 \\/' | awk '!a[$0]++' | sed -n "x;/.\+/ p;g;$ s/\(.\+\) \\\\/\1/; $ p"
vpath %.resx \
.

objdir := obj/$(releasetype)/
objdirdos := obj\\$(releasetype)\\
outdir := $(objdir)
outdirdos := $(objdirdos)
installdir := ../../plugins/

# Get `sources` via (run from this directory):
# find -regex '.+\.cs' | sed 's/\(.\+\)/\1 \\/' | sort | sed -n "x;/.\+/ p;g;$ s/\(.\+\) \\\\/\1/; $ p"
# Or (this is more valid -- it takes data from a project file):
# grep "[[:space:]]\+<Compile[[:space:]]\+Include=\".*\"\([[:space:]]\?\/\)\?>" GKPedigreeImporterPlugin.mswin.csproj | sed "y/\\\\/\//;s/[[:space:]]\\+<Compile[[:space:]]\+Include=\"\(.*\)\"\([[:space:]]\?\/\)\?>/.\/\1 \\\\/" | sort | sed -n "x;/.\+/ p;g;$ s/\(.\+\) \\\\/\1/; $ p"
# grep "[[:space:]]\+<Compile[[:space:]]\+Include=\".*\"\([[:space:]]\?\/\)\?>" GKPedigreeImporterPlugin.linux.csproj | sed "y/\\\\/\//;s/[[:space:]]\\+<Compile[[:space:]]\+Include=\"\(.*\)\"\([[:space:]]\?\/\)\?>/.\/\1 \\\\/" | sort | sed -n "x;/.\+/ p;g;$ s/\(.\+\) \\\\/\1/; $ p"
# Because `$(sources)` must (may) have different values for Windows and Linux targets.
ifeq ($(windows), $(softwareplatform))

# `$(sources)` for `$(windows)`
sources := \
./GKPedigreeImporterPlugin.cs \
./ImpUtils.cs \
./Importer.cs \
./PedigreeImporterDlg.Designer.cs \
./PedigreeImporterDlg.cs

else

# `$(sources)` for `$(linux)`
sources := \
./GKPedigreeImporterPlugin.cs \
./ImpUtils.cs \
./Importer.cs \
./PedigreeImporterDlg.Designer.cs \
./PedigreeImporterDlg.cs

endif

# Get files list for `resources` via (run from this directory):
# find -regex '.+\.resx' | sed 's/.\+\/\(.\+\)\.resx/\1.resources \\/' | sort
# Or (this is more valid -- it takes data from a project file):
# grep "[[:space:]]\+<EmbeddedResource[[:space:]]\+Include=\".*\"\([[:space:]]\?\/\)\?>" GKPedigreeImporterPlugin.mswin.csproj | sed "y/\\\\/\//;s/[[:space:]]\\+<EmbeddedResource[[:space:]]\+Include=.*[\/|\"]\(.*\)\.resx\"\([[:space:]]\?\/\)\?>/\1.resources \\\\/" | sort
# grep "[[:space:]]\+<EmbeddedResource[[:space:]]\+Include=\".*\"\([[:space:]]\?\/\)\?>" GKPedigreeImporterPlugin.linux.csproj | sed "y/\\\\/\//;s/[[:space:]]\\+<EmbeddedResource[[:space:]]\+Include=.*[\/|\"]\(.*\)\.resx\"\([[:space:]]\?\/\)\?>/\1.resources \\\\/" | sort
# Because `$(resources)` must (may) have different values for Windows and Linux targets.
ifeq ($(windows), $(softwareplatform))

# `$(resources)` for `$(windows)`
resources := \
$(addprefix $(objdir), \
PedigreeImporterDlg.resources \
)

else

# `$(resources)` for `$(linux)`
resources := \
$(addprefix $(objdir), \
PedigreeImporterDlg.resources \
)

endif

cscrefs := \
//reference:"..\\$(gkcommon)\\obj\\$(releasetype)\\$(gkcommon).dll" \
//reference:$(msftexcelinterop) \
//reference:$(msftwordinterop)
ifeq ($(windows), $(softwareplatform))
# Settings for Windows.
ifeq ($(x86-64), $(hardwareplatform))
# Settings for x86-64, Windows.
ifeq ($(release), $(releasetype))
# Settings for x86-64 release, Windows.
compilerflags := \
//noconfig //unsafe- //checked- //nowarn:1701,1702,2008 //nostdlib+ //platform:x64 //warn:4 \
//define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref$(dotnet))) $(cscrefs) \
//debug- //filealign:4096 //optimize+ \
//ruleset:$(analysis$(releasetype)ruleset) \
//target:library //warnaserror- //utf8output
else
# Settings for x86-64 debug, Windows.
compilerflags := \
//noconfig //unsafe- //checked+ //nowarn:1701,1702,2008 //nostdlib+ //platform:x64 //warn:4 \
//define:DEBUG //define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref$(dotnet))) $(cscrefs) \
//debug+ //debug:Full //filealign:4096 //optimize- \
//ruleset:$(analysis$(releasetype)ruleset) \
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
$(patsubst //r:%,//reference:%,$(coreresgenref$(dotnet))) $(cscrefs) \
//debug- //filealign:4096 //optimize+ \
//ruleset:$(analysis$(releasetype)ruleset) \
//target:library //warnaserror- //utf8output
else
# Settings for x86 debug, Windows.
compilerflags := \
//noconfig //unsafe- //checked+ //nowarn:1701,1702,2008 //nostdlib+ //platform:x86 //warn:4 /baseaddress:4194304 \
//define:DEBUG //define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref$(dotnet))) $(cscrefs) \
//debug+ //debug:Full //filealign:4096 //optimize- \
//ruleset:$(analysis$(releasetype)ruleset) \
//target:library //warnaserror- //utf8output
endif
else
ifeq ($(any), $(hardwareplatform))
# Settings for any CPU, Windows.
ifeq ($(release), $(releasetype))
# Settings for any CPU release, Windows.
compilerflags := \
//noconfig //unsafe- //checked- //nowarn:1701,1702,2008 //nostdlib+ //platform:anycpu //warn:4 /baseaddress:4194304 \
//define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref$(dotnet))) $(cscrefs) \
//debug- //filealign:4096 //optimize+ \
//ruleset:$(analysis$(releasetype)ruleset) \
//target:library //warnaserror- //utf8output
else
# Settings for any CPU debug, Windows.
compilerflags := \
//noconfig //unsafe- //checked+ //nowarn:1701,1702,2008 //nostdlib+ //platform:anycpu //warn:4 /baseaddress:4194304 \
//define:DEBUG //define:TRACE //errorendlocation //preferreduilang:en-US //highentropyva+ \
$(patsubst //r:%,//reference:%,$(coreresgenref$(dotnet))) $(cscrefs) \
//debug+ //debug:Full //filealign:4096 //optimize- \
//ruleset:$(analysis$(releasetype)ruleset) \
//target:library //warnaserror- //utf8output
endif
endif
endif
endif
endif
rcflags := \
//useSourcePath \
$(coreresgenref$(dotnet)) \
$(patsubst //reference:%,//r:%,$(cscrefs))
deps := \
$(addprefix $(installdir)$(project), \
.eng .rus .ukr \
)

.PHONY: all
all: $(gkcommon) $(outdir)$(project)$(projectext)

.PHONY: install
install: all $(deps)
	@cp -f $(outdir)$(project)$(projectext) $(installdir)
	@cd ../$(gkcommon) && $(MAKE) --file=Makefile $@

.PHONY: clean
clean:
	@cd ../$(gkcommon) && $(MAKE) --file=Makefile $@
	@rm -rf -- $(objdir)
	@rm -f $(installdir)$(project)$(projectext)
	@rm -f $(deps)

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

$(deps):
	@cp -f $(@F) $@
