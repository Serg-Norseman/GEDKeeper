#!/bin/sh
sh ./build_all.linux.x86.sh
cd ./deploy/
sh ./gk2_linux_deb_package.sh
