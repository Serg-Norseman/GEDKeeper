#!/bin/sh
APP="GEDKeeper2"
EXT="ged"
COMMENT="Personal genealogical database editor"
# APP_PATH=/usr/bin/
APP_VER="2.6.0"
DEV_PATH="/home/norseman/share/GEDKeeper"
PACK_PATH="gedkeeper-$APP_VER"
LIB_PATH="$PACK_PATH/usr/lib/gedkeeper"
BIN_PATH="$PACK_PATH/usr/bin"

# Create directories if missing
mkdir -p $PACK_PATH
mkdir -p $BIN_PATH
mkdir -p $LIB_PATH
mkdir -p $PACK_PATH/usr/share/applications
mkdir -p $PACK_PATH/usr/share/pixmaps
mkdir -p $PACK_PATH/usr/share/mime/packages

cp -r $DEV_PATH/deploy/DEBIAN/ ~/$PACK_PATH/

cp $DEV_PATH/deploy/gk2_run.sh ~/$BIN_PATH
chmod a+x ~/$BIN_PATH/gk2_run.sh

cp $DEV_PATH/deploy/gedkeeper.png ~/$LIB_PATH
cp $DEV_PATH/deploy/gedkeeper.png ~/$PACK_PATH/usr/share/pixmaps
cp $DEV_PATH/deploy/gedkeeper.desktop ~/$PACK_PATH/usr/share/applications
cp $DEV_PATH/deploy/application-x-gedkeeper.xml ~/$PACK_PATH/usr/share/mime/packages

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
cp -r $DEV_PATH/plugins/ ~/$LIB_PATH/
cp -r $DEV_PATH/scripts/ ~/$LIB_PATH/

fakeroot dpkg-deb -b ~/$PACK_PATH/ .
