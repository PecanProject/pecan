#!/bin/bash

RELEASE="1.5.0"
RC=2
VERSION="${RELEASE}-{RC}"

cd /home/carya/pecan
git checkout release/${RELEASE}
git reset --hard
git pull

$(dirname $0)/updateVersion.sh "${VERSION}"

cat > /tmp/motd << EOF
PEcAn version ${VERSION}

For more information about:
Pecan    - http://pecanproject.org
BETY     - http://www.betydb.org

For a list of all models currently supported see:
https://pecan.gitbooks.io/pecan-documentation/content/models/
EOF
sudo cp /tmp/motd /etc/motd
rm /tmp/motd

make clean
make
