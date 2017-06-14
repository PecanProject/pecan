#!/bin/bash

. /build/install_pecan_preprocessor.sh

echo "######################################################################"
echo "PECAN"
echo "######################################################################"
git clone https://github.com/PecanProject/pecan.git
cd pecan/
make
