#!/bin/sh

APP_VER="2.21.0"
DIR="$( cd "$( dirname "$0" )" && pwd )"

rm -rf ~/rpmbuild/
mkdir -p ~/rpmbuild/BUILD
mkdir -p ~/rpmbuild/RPMS
mkdir -p ~/rpmbuild/SOURCES
mkdir -p ~/rpmbuild/SPECS
mkdir -p ~/rpmbuild/SRPMS

tar -zcf ~/rpmbuild/SOURCES/gedkeeper-$APP_VER.tar.gz -T "$DIR/rpm/gk_files.txt"
cp "$DIR/rpm/gedkeeper.spec" ~/rpmbuild/SPECS/gedkeeper.spec
cd ~/rpmbuild/SPECS/

# build from binary
rpmbuild -bb gedkeeper.spec

cd "$DIR"
