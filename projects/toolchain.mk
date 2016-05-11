include $(includemk)variables.mk
include $(includemk)windows.mk

compiler := $($(softwareplatform)compilerdos$(hardwareplatform))
rc := $($(softwareplatform)rcdos$(dotnet))
