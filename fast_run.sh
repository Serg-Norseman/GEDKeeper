#!/bin/sh

# .NET Framework build
#GK=./bin/GEDKeeper3.exe

# .NET build
GK=./bin/GEDKeeper3

if [ ! -f "$GK" ]; then
    dotnet build projects/GKv3/GEDKeeper3.sln -c Linux_Debug
fi

# .NET Framework build
#exec /usr/bin/mono "$GK"

# .NET build
./bin/GEDKeeper3
