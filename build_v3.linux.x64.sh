#!/bin/sh

# If you have mono x86 installed on a amd64 linux.
#export CFLAGS=-m64
#export CXXFLAGS=-m64
#export LDFLAGS=-m64
#export LD_LIBRARY_PATH=$PWD/external/lua/linux/lib64

dotnet build projects/GKv3/GEDKeeper3.sln /p:Configuration=Debug /p:Platform="Linux"

#export TEMP="$( mktemp -d )"
#nunit-console ./projects/GKTests/bin/Linux/Debug/GKTests.dll && rm -r "$TEMP"
