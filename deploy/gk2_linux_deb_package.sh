#!/bin/sh
APP="GEDKeeper2"
EXT="ged"
COMMENT="Personal genealogical database editor"
# APP_PATH=/usr/bin/
APP_VER="2.12.0"
DEV_PATH="/home/norseman/share/gkdev/GEDKeeper"
PACK_PATH="gedkeeper-$APP_VER"
LIB_PATH="$PACK_PATH/usr/lib/gedkeeper"
BIN_PATH="$PACK_PATH/usr/bin"

rm -f ~/gedkeeper_2.12.0-1_all.deb
rm -rf ~/$PACK_PATH

# Create directories if missing
mkdir -p ~/$PACK_PATH
mkdir -p ~/$BIN_PATH
mkdir -p ~/$LIB_PATH
mkdir -p ~/$PACK_PATH/usr/share/applications
mkdir -p ~/$PACK_PATH/usr/share/pixmaps
mkdir -p ~/$PACK_PATH/usr/share/mime/packages

cp -r $DEV_PATH/deploy/DEBIAN/ ~/$PACK_PATH/

cp $DEV_PATH/deploy/gk2_run.sh ~/$BIN_PATH
chmod a+x ~/$BIN_PATH/gk2_run.sh

cp $DEV_PATH/deploy/gedkeeper.png ~/$LIB_PATH
cp $DEV_PATH/deploy/gedkeeper.png ~/$PACK_PATH/usr/share/pixmaps
cp $DEV_PATH/deploy/gedkeeper.desktop ~/$PACK_PATH/usr/share/applications
cp $DEV_PATH/deploy/application-x-gedkeeper.xml ~/$PACK_PATH/usr/share/mime/packages

cp $DEV_PATH/GEDKeeper2.exe ~/$LIB_PATH
cp $DEV_PATH/GKCore.dll ~/$LIB_PATH
cp $DEV_PATH/GKComponents.dll ~/$LIB_PATH

cp $DEV_PATH/LinqBridge.dll ~/$LIB_PATH
cp $DEV_PATH/NLog.dll ~/$LIB_PATH
cp $DEV_PATH/nVLC.dll ~/$LIB_PATH
cp $DEV_PATH/YamlSerializer.dll ~/$LIB_PATH

cp $DEV_PATH/ArborGVT.dll ~/$LIB_PATH
cp $DEV_PATH/DotNetRtfWriter.dll ~/$LIB_PATH
cp $DEV_PATH/ExcelLibrary.dll ~/$LIB_PATH
cp $DEV_PATH/itextsharp.dll ~/$LIB_PATH
cp $DEV_PATH/KopiLua.dll ~/$LIB_PATH
cp $DEV_PATH/NLua.dll ~/$LIB_PATH
cp $DEV_PATH/ZedGraph.dll ~/$LIB_PATH

cp $DEV_PATH/LICENSE ~/$LIB_PATH

cp -r $DEV_PATH/locales/ ~/$LIB_PATH/
cp -r $DEV_PATH/plugins/ ~/$LIB_PATH/
cp -r $DEV_PATH/scripts/ ~/$LIB_PATH/
cp -r $DEV_PATH/samples/ ~/$LIB_PATH/

chmod -x ~/$LIB_PATH/GKCore.dll
chmod -x ~/$LIB_PATH/GKComponents.dll

chmod -x ~/$LIB_PATH/LinqBridge.dll
chmod -x ~/$LIB_PATH/NLog.dll
chmod -x ~/$LIB_PATH/nVLC.dll
chmod -x ~/$LIB_PATH/YamlSerializer.dll

chmod -x ~/$LIB_PATH/ArborGVT.dll
chmod -x ~/$LIB_PATH/DotNetRtfWriter.dll
chmod -x ~/$LIB_PATH/ExcelLibrary.dll
chmod -x ~/$LIB_PATH/itextsharp.dll
chmod -x ~/$LIB_PATH/KopiLua.dll
chmod -x ~/$LIB_PATH/NLua.dll
chmod -x ~/$LIB_PATH/ZedGraph.dll
chmod -x ~/$LIB_PATH/LICENSE

find ~/$LIB_PATH/locales -type f -exec chmod -x '{}' \;
find ~/$LIB_PATH/plugins -type f -exec chmod -x '{}' \;
find ~/$LIB_PATH/scripts -type f -exec chmod -x '{}' \;
find ~/$LIB_PATH/samples -type f -exec chmod -x '{}' \;

chmod -x ~/$LIB_PATH/gedkeeper.png
chmod -x ~/$PACK_PATH/usr/share/pixmaps/gedkeeper.png
chmod -x ~/$PACK_PATH/usr/share/applications/gedkeeper.desktop
chmod -x ~/$PACK_PATH/usr/share/mime/packages/application-x-gedkeeper.xml

cd ~/$PACK_PATH
#md5deep -r -l usr > DEBIAN/md5sums
#hashdeep -rlc md5 usr > DEBIAN/md5sums
hashdeep -rlc md5 usr | tail -n +6 | awk -F',' '{ print $2 "  " $3 }' > DEBIAN/md5sums
cd ..

fakeroot dpkg-deb -b ~/$PACK_PATH/ .
