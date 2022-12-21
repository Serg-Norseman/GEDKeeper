#!/bin/sh

xbuild ./projects/GEDKeeper2.linux.sln /p:Configuration=Release /p:Platform="x86" /p:MonoCS=true /p:TargetFrameworkVersion=v4.5.2 /v:quiet
cd ./deploy/
sh ./gk2_linux_pkg_manjaro_package.sh
