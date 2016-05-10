#!/bin/sh
APP="GEDKeeper2"
EXT="ged"
COMMENT="Personal genealogical database editor"
# APP_PATH=/usr/bin/
DEV_PATH="/home/norseman/share/GEDKeeper"
PACK_PATH="gedkeeper"
LIB_PATH="$PACK_PATH/usr/lib/gedkeeper"
BIN_PATH="$PACK_PATH/usr/bin"

# Create directories if missing
mkdir -p $PACK_PATH
mkdir -p $BIN_PATH
mkdir -p $LIB_PATH
mkdir -p $PACK_PATH/usr/share/applications
mkdir -p $PACK_PATH/usr/share/pixmaps

cp $DEV_PATH/deploy/gk2_run.sh ~/$BIN_PATH
chmod a+x ~/$BIN_PATH/gk2_run.sh

cp $DEV_PATH/deploy/GEDKeeper2.png ~/$LIB_PATH
cp $DEV_PATH/deploy/GEDKeeper2.png ~/$PACK_PATH/usr/share/pixmaps
cp $DEV_PATH/deploy/GEDKeeper2.desktop ~/$PACK_PATH/usr/share/applications

cp $DEV_PATH/GEDKeeper2.exe ~/$LIB_PATH
cp $DEV_PATH/GKCommon.dll ~/$LIB_PATH
cp $DEV_PATH/ArborGVT.dll ~/$LIB_PATH
cp $DEV_PATH/ExcelLibrary.dll ~/$LIB_PATH
cp $DEV_PATH/itextsharp.dll ~/$LIB_PATH
cp $DEV_PATH/lua51.dll ~/$LIB_PATH
cp $DEV_PATH/LuaInterface.dll ~/$LIB_PATH
cp $DEV_PATH/ZedGraph.dll ~/$LIB_PATH
cp $DEV_PATH/LICENSE ~/$LIB_PATH
cp -r $DEV_PATH/locales/ ~/$LIB_PATH/

#echo "..\plugins\" >> %lstfile%
#echo "..\locales\" >> %lstfile%
#echo "..\scripts\" >> %lstfile%
