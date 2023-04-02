#!/bin/sh

#xbuild ./projects/GKv3/GEDKeeper3.sln /p:Configuration=Linux_Debug /p:Platform="Any CPU" /p:MonoCS=true /p:TargetFrameworkVersion=v4.7.1 /v:quiet
dotnet build projects/GKv3/GEDKeeper3.sln -c Linux_Debug
