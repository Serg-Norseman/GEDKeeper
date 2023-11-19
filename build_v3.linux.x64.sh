#!/bin/sh

# If you have mono x86 installed on a amd64 linux.
#export CFLAGS=-m64
#export CXXFLAGS=-m64
#export LDFLAGS=-m64
#export LD_LIBRARY_PATH=$PWD/external/lua/linux/lib64
#xbuild ./projects/GKv3/GEDKeeper3.sln /p:Configuration=Linux_Debug /p:Platform="x64" /p:MonoCS=true /p:TargetFrameworkVersion=v4.7.1 /v:quiet

dotnet build projects/GKv3/GEDKeeper3.sln /p:Configuration=Debug /p:Platform="Linux"

#export TEMP="$( mktemp -d )"
#nunit-console ./projects/GKTests/bin/Linux/Debug/GKTests.dll && rm -r "$TEMP"
