SHELL := /bin/bash

.DEFAULT_GOAL := all

includemk := projects/
include $(includemk)toolchain.mk

projectsdir := projects/
outdir := ./
pluginsdir := $(outdir)plugins/
gkplugins := \
$(addprefix $(pluginsdir), \
$(gksampleplugin).dll \
$(gkcalculatorplugin).dll \
$(gkcalendarplugin).dll \
$(gkflowinputplugin).dll \
$(gkimageviewerplugin).dll \
$(gklifeplugin).dll \
$(gknamesbookplugin).dll \
$(gknavigatorplugin).dll \
$(gkpedigreeimporterplugin).dll \
$(gktextsearchplugin).dll \
$(gktimelineplugin).dll \
$(gktreevizplugin).dll \
)

.PHONY: all
all: $(gkplugins) $(outdir)$(gedkeeper2).exe

.PHONY: clean
clean:
	@cd $(projectsdir)$(gkcommon) && $(MAKE) --file=Makefile $@
	@cd $(projectsdir)$(gedkeeper2) && $(MAKE) --file=Makefile $@
	@cd $(projectsdir)$(gksampleplugin) && $(MAKE) --file=Makefile $@
	@cd $(projectsdir)$(gkcalculatorplugin) && $(MAKE) --file=Makefile $@
	@cd $(projectsdir)$(gkcalendarplugin) && $(MAKE) --file=Makefile $@
	@cd $(projectsdir)$(gkflowinputplugin) && $(MAKE) --file=Makefile $@
	@cd $(projectsdir)$(gkimageviewerplugin) && $(MAKE) --file=Makefile $@
	@cd $(projectsdir)$(gklifeplugin) && $(MAKE) --file=Makefile $@
	@cd $(projectsdir)$(gknamesbookplugin) && $(MAKE) --file=Makefile $@
	@cd $(projectsdir)$(gknavigatorplugin) && $(MAKE) --file=Makefile $@
	@cd $(projectsdir)$(gkpedigreeimporterplugin) && $(MAKE) --file=Makefile $@
	@cd $(projectsdir)$(gktextsearchplugin) && $(MAKE) --file=Makefile $@
	@cd $(projectsdir)$(gktimelineplugin) && $(MAKE) --file=Makefile $@
	@cd $(projectsdir)$(gktreevizplugin) && $(MAKE) --file=Makefile $@

$(pluginsdir):
	@mkdir -p $(pluginsdir)

.PHONY: $(outdir)$(gedkeeper2).exe
$(outdir)$(gedkeeper2).exe:
	@cd $(projectsdir)$(gedkeeper2) && $(MAKE) --file=Makefile all install

.PHONY: $(pluginsdir)$(gksampleplugin).dll
$(pluginsdir)$(gksampleplugin).dll: | $(pluginsdir)
	@cd $(projectsdir)$(gksampleplugin) && $(MAKE) --file=Makefile all install

.PHONY: $(pluginsdir)$(gkcalculatorplugin).dll
$(pluginsdir)$(gkcalculatorplugin).dll: | $(pluginsdir)
	@cd $(projectsdir)$(gkcalculatorplugin) && $(MAKE) --file=Makefile all install

.PHONY: $(pluginsdir)$(gkcalendarplugin).dll
$(pluginsdir)$(gkcalendarplugin).dll: | $(pluginsdir)
	@cd $(projectsdir)$(gkcalendarplugin) && $(MAKE) --file=Makefile all install

.PHONY: $(pluginsdir)$(gkflowinputplugin).dll
$(pluginsdir)$(gkflowinputplugin).dll: | $(pluginsdir)
	@cd $(projectsdir)$(gkflowinputplugin) && $(MAKE) --file=Makefile all install

.PHONY: $(pluginsdir)$(gkimageviewerplugin).dll
$(pluginsdir)$(gkimageviewerplugin).dll: | $(pluginsdir)
	@cd $(projectsdir)$(gkimageviewerplugin) && $(MAKE) --file=Makefile all install

.PHONY: $(pluginsdir)$(gklifeplugin).dll
$(pluginsdir)$(gklifeplugin).dll: | $(pluginsdir)
	@cd $(projectsdir)$(gklifeplugin) && $(MAKE) --file=Makefile all install

.PHONY: $(pluginsdir)$(gknamesbookplugin).dll
$(pluginsdir)$(gknamesbookplugin).dll: | $(pluginsdir)
	@cd $(projectsdir)$(gknamesbookplugin) && $(MAKE) --file=Makefile all install

.PHONY: $(pluginsdir)$(gknavigatorplugin).dll
$(pluginsdir)$(gknavigatorplugin).dll: | $(pluginsdir)
	@cd $(projectsdir)$(gknavigatorplugin) && $(MAKE) --file=Makefile all install

.PHONY: $(pluginsdir)$(gkpedigreeimporterplugin).dll
$(pluginsdir)$(gkpedigreeimporterplugin).dll: | $(pluginsdir)
	@cd $(projectsdir)$(gkpedigreeimporterplugin) && $(MAKE) --file=Makefile all install

.PHONY: $(pluginsdir)$(gktextsearchplugin).dll
$(pluginsdir)$(gktextsearchplugin).dll: | $(pluginsdir)
	@cd $(projectsdir)$(gktextsearchplugin) && $(MAKE) --file=Makefile all install

.PHONY: $(pluginsdir)$(gktimelineplugin).dll
$(pluginsdir)$(gktimelineplugin).dll: | $(pluginsdir)
	@cd $(projectsdir)$(gktimelineplugin) && $(MAKE) --file=Makefile all install

.PHONY: $(pluginsdir)$(gktreevizplugin).dll
$(pluginsdir)$(gktreevizplugin).dll: | $(pluginsdir)
	@cd $(projectsdir)$(gktreevizplugin) && $(MAKE) --file=Makefile all install
