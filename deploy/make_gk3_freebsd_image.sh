#!/bin/sh

APP_VER="3.8.0"
dotnet build ../projects/GKv3/GEDKeeper3.sln /p:Configuration=Release /p:Platform=FreeBSD
#cd ./deploy/
sh ./gk_freebsd_image.sh $APP_VER
