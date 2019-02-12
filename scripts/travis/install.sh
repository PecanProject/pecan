#!/bin/bash

set -e
. $( dirname $0 )/func.sh

# INSTALLING SIPNET
(
    travis_time_start "install_sipnet" "Installing SIPNET for testing"

    curl -o sipnet_unk.tar.gz http://isda.ncsa.illinois.edu/~kooper/EBI/sipnet_unk.tar.gz
    tar zxf sipnet_unk.tar.gz
    cd sipnet_unk
    make

    travis_time_end
)

if [ ! -e /home/travis/sipnet_unk/sipnet ]; then
    echo "ERROR NO SIPNET!"
    exit -1
fi
ls -l /home/travis/sipnet_unk/sipnet

# INSTALLING BIOCRO
(
    travis_time_start "install_biocro" "Installing BioCro"

    curl -sL https://github.com/ebimodeling/biocro/archive/0.95.tar.gz | tar zxf -
    cd biocro-0.95
    rm configure
    R CMD INSTALL .

    travis_time_end
)
