#!/bin/sh

APP_VER="$1"
DEV_PATH="/home/norseman/share/gkdev/GEDKeeper"
PACK_PATH="gedkeeper-$APP_VER"

sh ./gk_linux_image.sh $APP_VER src

cp -r $DEV_PATH/deploy/Manjaro/* ~/$PACK_PATH/

cd ~/$PACK_PATH

makepkg -f
