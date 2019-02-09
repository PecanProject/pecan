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
    cd ..

    travis_time_end
)

# INSTALLING R BUILD TOOLS
(
    travis_time_start "install_r_build" "Installing R build tools"
    Rscript -e 'install.packages(c("littler", "devtools"), dependencies=TRUE)'
    travis_time_end
)

# INSTALLING PECAN DEPENDENCIES
(
    travis_time_start "install_pecan_dependencies" "Installing PEcAn dependencies"
    export PATH=${PATH}:${R_LIBS_USER}/littler/examples:${R_LIBS_USER}/littler/bin
    ls ${R_LIBS_USER}
    ls ${R_LIBS_USER}/littler/examples
    ls ${R_LIBS_USER}/littler/bin
    bash docker/depends/pecan.depends
    travis_time_end
)
