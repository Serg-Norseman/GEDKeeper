#!/bin/sh

GK=./bin/GEDKeeper2.exe
if [ ! -f "$GK" ]; then
    xbuild ./projects/GKv2/GEDKeeper2.sln /p:Configuration=Release /p:Platform="x64" /p:MonoCS=true /p:TargetFrameworkVersion=v4.7.1 /v:quiet
fi

exec /usr/bin/mono "$GK" "$@"
