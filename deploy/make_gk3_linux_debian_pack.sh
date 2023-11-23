#!/bin/sh

APP_VER="3.4.1"
dotnet build ../projects/GKv3/GEDKeeper3.sln /p:Configuration=Release /p:Platform=Linux
#cd ./deploy/
sh ./gk_linux_deb_package.sh $APP_VER
