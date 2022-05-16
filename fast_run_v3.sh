#!/bin/sh

GK=./bin/GEDKeeper3
if [ ! -f "$GK" ]; then
    dotnet build projects/GKv3/GEDKeeper3.sln -c Linux_Debug
fi

./bin/GEDKeeper3
