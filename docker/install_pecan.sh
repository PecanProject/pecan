#!/bin/bash

. /build/install_pecan_preprocessor.sh

echo "######################################################################"
echo "PECAN"
echo "######################################################################"
if [ ! -e ${HOME}/pecan ]; then
  cd
  git clone https://github.com/PecanProject/pecan.git
fi
cd ${HOME}/pecan
git pull
mkdir .install
make
