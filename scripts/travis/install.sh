#!/bin/bash

set -e
. $( dirname $0 )/func.sh

# FIXING R BINARIES
(
    travis_time_start "pkg_version_check" "Checking R package binaries"

    Rscript scripts/travis/rebuild_pkg_binaries.R

    travis_time_end
)

# ROLL A FEW R PACKAGES BACK TO SPECIFIED VERSIONS
(
    travis_time_start "pecan_install_roxygen" "Installing Roxygen 7.0.2 to match comitted documentation version"
    # We keep Roxygen pinned to a known version, merely to avoid hassle /
    # merge conflicts from updates causing unplanned documentation changes.
    # It's OK to bump the Roxygen version when needed, but please coordinate
    # with the team to update all documentation at once and to get all
    # PEcAn developers to update Roxygen on their own machines to match.
    Rscript -e 'if (!requireNamespace("devtools", quietly = TRUE)) { install.packages("devtools", repos = "https://cloud.r-project.org") }' \
        -e 'devtools::install_version("roxygen2", version = "7.0.2", repos = "https://cloud.r-project.org")'
    travis_time_end

    # MCMCpack >= 1.4-5 requires R >= 3.6;
    # fall back to last compatible version if running older R
    # (but only then -- new MCMCpack version is more efficient)
    if [[ `Rscript -e 'cat(getRversion() < "3.6")'` = "TRUE" ]]; then
        travis_time_start "pecan_install_MCMCpack" "Installing MCMCpack 1.4-4 (last version compatible with R < 3.6)"
        Rscript -e 'devtools::install_version("MCMCpack", version = "1.4-4", repos = "https://cloud.r-project.org")'
        travis_time_end
    fi
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
