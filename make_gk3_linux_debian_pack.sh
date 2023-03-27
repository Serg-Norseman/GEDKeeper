#!/bin/sh

APP_VER="3.0.3"

dotnet build projects/GKv3/GEDKeeper3.sln -c Linux_Release
#xbuild ./projects/GKv2/GEDKeeper2.sln /p:Configuration=Release /p:Platform="x64" /p:MonoCS=true /p:TargetFrameworkVersion=v4.7.2 /v:quiet
cd ./deploy/
sh ./gk2_linux_deb_package.sh $APP_VER
