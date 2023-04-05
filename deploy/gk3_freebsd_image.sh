#!/bin/sh

APP_VER="$1"
DEV_PATH="/home/norseman/share/gkdev/GEDKeeper"
PACK_PATH="gedkeeper-$APP_VER"
IMG_FILE="gedkeeper_$APP_VER-image.tar.gz"

rm -f $IMG_FILE

sh ./gk2_linux_image.sh $APP_VER

cd ~/$PACK_PATH
tar -zcf ~/$IMG_FILE usr
cd ..