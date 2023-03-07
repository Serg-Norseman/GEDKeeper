#!/bin/sh

APP="GEDKeeper2"
APP_VER="$1"
DEV_PATH="/home/norseman/share/gkdev/GEDKeeper"
PACK_PATH="gedkeeper-$APP_VER/$2"
LIB_PATH="$PACK_PATH/usr/lib/gedkeeper"
BIN_PATH="$PACK_PATH/usr/bin"

rm -rf ~/$PACK_PATH

mkdir -p ~/$PACK_PATH
mkdir -p ~/$BIN_PATH
mkdir -p ~/$LIB_PATH
mkdir -p ~/$PACK_PATH/usr/share/applications
mkdir -p ~/$PACK_PATH/usr/share/pixmaps
mkdir -p ~/$PACK_PATH/usr/share/mime/packages
mkdir -p ~/$PACK_PATH/usr/share/metainfo/

cp $DEV_PATH/deploy/gk2_run.sh ~/$BIN_PATH
chmod a+x ~/$BIN_PATH/gk2_run.sh

cp $DEV_PATH/deploy/gedkeeper.png ~/$LIB_PATH
cp $DEV_PATH/deploy/gedkeeper.png ~/$PACK_PATH/usr/share/pixmaps
cp $DEV_PATH/deploy/gedkeeper.desktop ~/$PACK_PATH/usr/share/applications
cp $DEV_PATH/deploy/application-x-gedkeeper.xml ~/$PACK_PATH/usr/share/mime/packages
cp $DEV_PATH/deploy/gedkeeper.appdata.xml ~/$PACK_PATH/usr/share/metainfo/

cp $DEV_PATH/LICENSE ~/$LIB_PATH

cp -r $DEV_PATH/bin/ ~/$LIB_PATH/
cp -r $DEV_PATH/locales/ ~/$LIB_PATH/
cp -r $DEV_PATH/plugins/ ~/$LIB_PATH/
cp -r $DEV_PATH/scripts/ ~/$LIB_PATH/
cp -r $DEV_PATH/samples/ ~/$LIB_PATH/
cp -r $DEV_PATH/themes/ ~/$LIB_PATH/
cp -r $DEV_PATH/externals/resources.yaml ~/$LIB_PATH/externals/

find ~/$LIB_PATH/bin -type f -iname "*.dll" -exec chmod -x {} \;
find ~/$LIB_PATH/locales -type f -exec chmod -x '{}' \;
find ~/$LIB_PATH/plugins -type f -exec chmod -x '{}' \;
find ~/$LIB_PATH/scripts -type f -exec chmod -x '{}' \;
find ~/$LIB_PATH/samples -type f -exec chmod -x '{}' \;
find ~/$LIB_PATH/themes -type f -exec chmod -x '{}' \;
find ~/$LIB_PATH/externals -type f -exec chmod -x '{}' \;
chmod -x ~/$LIB_PATH/LICENSE
chmod -x ~/$LIB_PATH/gedkeeper.png
chmod -x ~/$PACK_PATH/usr/share/pixmaps/gedkeeper.png
chmod -x ~/$PACK_PATH/usr/share/applications/gedkeeper.desktop
chmod -x ~/$PACK_PATH/usr/share/mime/packages/application-x-gedkeeper.xml
chmod -x ~/$PACK_PATH/usr/share/metainfo/gedkeeper.appdata.xml
