#!/bin/sh

APP_VER="2.23.0"
DEV_PATH="/home/norseman/share/gkdev/GEDKeeper"
PACK_PATH="gedkeeper-$APP_VER"

rm -f ~/gedkeeper_$APP_VER-1_all.deb

sh ./gk2_linux_image.sh $APP_VER

cp -r $DEV_PATH/deploy/DEBIAN/ ~/$PACK_PATH/

cd ~/$PACK_PATH
hashdeep -rlc md5 usr | tail -n +6 | awk -F',' '{ print $2 "  " $3 }' > DEBIAN/md5sums
cd ..

fakeroot dpkg-deb -b ~/$PACK_PATH/ .
