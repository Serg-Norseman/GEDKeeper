#!/bin/sh

APP_VER="3.13.0"
dotnet build ../projects/GKv3/GEDKeeper3.sln /p:Configuration=Release /p:Platform=Linux
#cd ./deploy/
sh ./gk_linux_rpm_fedora_package.sh $APP_VER
