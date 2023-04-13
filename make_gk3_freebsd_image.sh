#!/bin/sh

APP_VER="3.1.0"
dotnet build projects/GKv3/GEDKeeper3.sln -c FreeBSD_Release
cd ./deploy/
sh ./gk_freebsd_image.sh $APP_VER