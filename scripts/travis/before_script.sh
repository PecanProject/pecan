#!/bin/bash

set -x

. $( dirname $0 )/func.sh

# INSTALLING SIPNET
(
    travis_time_start "install_sipnet" "Installing SIPNET for testing"
    #fold_start "install_sipnet" "Installing SIPNET for testing"

    pushd $HOME
    curl -o sipnet_unk.tar.gz http://isda.ncsa.illinois.edu/~kooper/EBI/sipnet_unk.tar.gz
    tar zxf sipnet_unk.tar.gz
    cd sipnet_unk
    make
    popd

    travis_time_end
    #fold_end
)
