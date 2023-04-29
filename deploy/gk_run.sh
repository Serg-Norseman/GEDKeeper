#!/bin/sh

if [ -f "/usr/lib/gedkeeper/bin/GEDKeeper3" ]; then
    exec /usr/lib/gedkeeper/bin/GEDKeeper3 "$@"
elif [ -f "/usr/lib/gedkeeper/bin/GEDKeeper2.exe" ]; then
    exec /usr/bin/mono /usr/lib/gedkeeper/bin/GEDKeeper2.exe "$@"
elif [ -f "/usr/lib64/gedkeeper/bin/GEDKeeper2.exe" ]; then
    exec /usr/bin/mono /usr/lib64/gedkeeper/bin/GEDKeeper2.exe "$@"
elif [ -f "/usr/lib/gedkeeper/bin/GEDKeeper3.exe" ]; then
    exec /usr/bin/mono /usr/lib/gedkeeper/bin/GEDKeeper3.exe "$@"
elif [ -f "/usr/lib64/gedkeeper/bin/GEDKeeper3.exe" ]; then 
    exec /usr/bin/mono /usr/lib64/gedkeeper/bin/GEDKeeper3.exe "$@"
fi
