#!/bin/sh

# If you have mono x86 installed on a amd64 linux.
#export CFLAGS=-m64
#export CXXFLAGS=-m64
#export LDFLAGS=-m64
#export LD_LIBRARY_PATH=$PWD/external/lua/linux/lib64

cd projects/
#erase gktests/bin/debug/*.dll
./compile.linux.sh
cd ./GKTests/bin/Release
nunit-console GKTests.dll
cd ../../..
