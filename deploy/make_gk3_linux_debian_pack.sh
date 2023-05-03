#!/bin/sh

APP_VER="3.2.0"
dotnet build ../projects/GKv3/GEDKeeper3.sln -c Linux_Release
#cd ./deploy/
sh ./gk_linux_deb_package.sh $APP_VER
