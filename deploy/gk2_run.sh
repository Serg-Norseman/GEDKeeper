#!/bin/sh

GKEx86=/usr/lib/gedkeeper/bin/GEDKeeper2.exe
if [ -f "$GKEx86" ]; then
    exec /usr/bin/mono /usr/lib/gedkeeper/bin/GEDKeeper2.exe "$@"
else 
    exec /usr/bin/mono /usr/lib64/gedkeeper/bin/GEDKeeper2.exe "$@"
fi
