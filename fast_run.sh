#!/bin/sh

GK=./bin/GEDKeeper2.exe
if [ ! -f "$GK" ]; then
    xbuild ./projects/GEDKeeper2.linux.sln /p:Configuration=Debug /p:Platform="x86" /p:MonoCS=true /p:TargetFrameworkVersion=v4.5 /v:quiet
fi

exec /usr/bin/mono "$GK" "$@"
