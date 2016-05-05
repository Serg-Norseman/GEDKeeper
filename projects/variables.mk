include ../values.mk

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
