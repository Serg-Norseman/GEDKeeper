#!/bin/sh

xbuild ./projects/GEDKeeper2.linux.sln /p:Configuration=Debug /p:Platform="x86" /p:MonoCS=true /p:TargetFrameworkVersion=v4.5 /v:quiet
cd ./deploy/
sh ./gk2_linux_rpm_fedora_package.sh "2.19.0"
