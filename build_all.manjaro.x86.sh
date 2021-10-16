#!/bin/sh

xbuild ./projects/GEDKeeper2.linux.sln /p:Configuration=Debug /p:Platform="x86" /p:MonoCS=true /p:TargetFrameworkVersion=v4.5
#nunit-console ./projects/GKTests/bin/Debug/GKTests.dll
