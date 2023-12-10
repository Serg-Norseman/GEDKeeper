#!/bin/sh

dotnet build projects/GKv3/GEDKeeper3.sln -p:Configuration=Debug -p:Platform="MacOS" -p:MacBuildBundle=true

# For details how to create a proper DMG HSF+ file see here: https://stackoverflow.com/a/7553878/259946.
# The ISO9660/HFS is a supported format for DMG files and can be treated as uncompressed DMG.

# sudo apt install mkisofs
cd projects/GKv3/GEDKeeper3/bin/MacOS_Debug
mkisofs -V GEDKeeper -D -R -apple -no-pad -o GEDKeeper3-x64.dmg osx-x64
mkisofs -V GEDKeeper -D -R -apple -no-pad -o GEDKeeper3-arm64.dmg osx-arm64
cd -
