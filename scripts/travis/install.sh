#!/bin/bash

set -e
. $( dirname $0 )/func.sh

# FIXING R BINARIES
(
    travis_time_start "pkg_version_check" "Checking R package binaries"

    Rscript scripts/travis/rebuild_pkg_binaries.R

    travis_time_end
)

# INSTALLING SIPNET
(
    travis_time_start "install_sipnet" "Installing SIPNET for testing"

    cd ${HOME}
    curl -o sipnet_unk.tar.gz http://isda.ncsa.illinois.edu/~kooper/EBI/sipnet_unk.tar.gz
    tar zxf sipnet_unk.tar.gz
    cd sipnet_unk
    make
    ls -l sipnet

    travis_time_end
)

# INSTALLING BIOCRO
(
    travis_time_start "install_biocro" "Installing BioCro"

    cd ${HOME}
    curl -sL https://github.com/ebimodeling/biocro/archive/0.95.tar.gz | tar zxf -
    cd biocro-0.95
    rm configure
    R CMD INSTALL .

    travis_time_end
)
