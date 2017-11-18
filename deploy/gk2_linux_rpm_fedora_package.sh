#!/bin/sh
DIR="$( cd "$( dirname "$0" )" && pwd )"
rm -rf ~/rpmbuild/
mkdir -p ~/rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
tar -zcf v2.12.0.tar.gz -T "$DIR/rpm/gk_files.txt"
cp "$DIR/rpm/gedkeeper_fedora.spec" ~/rpmbuild/SPECS/gedkeeper.spec
mv v2.12.0.tar.gz ~/rpmbuild/SOURCES/
cd ~/rpmbuild/SPECS/
rpmbuild -ba gedkeeper.spec
cd "$DIR"
