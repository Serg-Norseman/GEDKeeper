#!/bin/sh

APP_VER="3.11.0"
dotnet build ../projects/GKv3/GEDKeeper3.sln /p:Configuration=Release /p:Platform=Linux
#cd ./deploy/
sh ./gk_linux_pkg_manjaro_package.sh $APP_VER
