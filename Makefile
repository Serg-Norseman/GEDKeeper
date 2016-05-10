SHELL := /bin/bash

.DEFAULT_GOAL := all

includemk := projects/
include $(includemk)toolchain.mk

projects := projects/

outdir := ./
gkdepdlls := \
$(addprefix $(outdir), \
ArborGVT.dll \
ExcelLibrary.dll \
itextsharp.dll \
lua51.dll \
LuaInterface.dll \
ZedGraph.dll \
$(gkcommon).dll \
)

.PHONY: all
all: $(outdir)$(gedkeeper2).exe $(gkdepdlls)

.PHONY: clean
clean:
	@cd $(projects)$(gkcommon) && $(MAKE) --file=Makefile $@
	@cd $(projects)$(gedkeeper2) && $(MAKE) --file=Makefile $@
	@rm -f $(outdir)$(gedkeeper2).exe
	@rm -f $(gkdepdlls)

$(outdir)$(gedkeeper2).exe:
	@cd $(projects)$(gedkeeper2) && $(MAKE) --file=Makefile
	@cp -f $(projects)$(gedkeeper2)/obj/$(releasetype)/$(gedkeeper2).exe $@

$(outdir)%.dll: $(projects)$(gedkeeper2)/libs/%.dll
	@cp -f $< $@
	@echo "**** $< dependency was copied."

$(outdir)$(gkcommon).dll: $(projects)$(gkcommon)/obj/$(releasetype)/$(gkcommon).dll
	@cp -f $< $@
	@echo "**** $< dependency was copied."
