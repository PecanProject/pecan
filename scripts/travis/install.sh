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
    travis_time_start "pecan_install_roxygen" "Installing Roxygen 6.1.1 to match comitted documentation version"
    # Later Roxygen versions produce a lot of formatting changes (mostly whitespace), so waiting to upgrade.
    # When ready we will upgrade to Roxygen 7.0, commit all changes at once,
    # and make all developers update their own Roxygen installations at the same time.
    Rscript -e 'devtools::install_version("roxygen2", version = "6.1.1", repos = "http://cran.us.r-project.org")'
    travis_time_end

    # MCMCpack >= 1.4-5 requires R >= 3.6;
    # fall back to last compatible version if running older R
    # (but only then -- new MCMCpack version is more efficient)
    if [[ `Rscript -e 'cat(getRversion() < "3.6")'` = "TRUE" ]]; then
        travis_time_start "pecan_install_MCMCpack" "Installing MCMCpack 1.4-4 (last version compatible with R < 3.6)"
        Rscript -e 'devtools::install_version("MCMCpack", version = "1.4-4", repos = "http://cran.us.r-project.org")'
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
