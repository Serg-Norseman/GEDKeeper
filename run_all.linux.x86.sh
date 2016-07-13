#!/bin/sh

# If you have mono x86 installed on a amd64 linux.
#export CFLAGS=-m64
#export CXXFLAGS=-m64
#export LDFLAGS=-m64
#export LD_LIBRARY_PATH=$PWD/external/lua/linux/lib64

cd projects/
#erase gktests/bin/debug/*.dll
#./compile.linux.sh
xbuild GEDKeeper2.linux.sln /p:Configuration=Debug /p:Platform="x86"
cd ./GKTests/bin/Release
nunit-console GKTests.dll
cd ../../..
./generateCoverageConfig.sh > ./coverageConfig.json
mono ./packages/SharpCover/SharpCover.exe instrument ./coverageConfig.json
mono ./packages/SharpCover/SharpCover.exe check
