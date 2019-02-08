#!/bin/bash

. $( dirname $0 )/func.sh

set -e

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

# INSTALLING DEPENDENCIES
(
    travis_time_start "install_r_dependencies" "Installing R dependencies"

    Rscript -e 'install.packages(c("littler", "devtools"))'
    export PATH=${PATH}:${R_LIBS_USER}/littler/examples:${R_LIBS_USER}/littler/bin
    bash docker/depends/pecan.depends

    travis_time_end
)
