#!/bin/sh

#GK=./bin/GEDKeeper3.exe
GK=./bin/GEDKeeper3

if [ ! -f "$GK" ]; then
    dotnet build projects/GKv3/GEDKeeper3.sln -p:Configuration="Debug" -p:Platform="Linux"
fi

#exec /usr/bin/mono "$GK"
./bin/GEDKeeper3
