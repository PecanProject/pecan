#!/bin/bash

set -e
. $( dirname $0 )/func.sh

# INSTALLING PECAN
(
    travis_time_start "install_pkgs" "Installing PEcAn packages"
    Rscript scripts/generate_dependencies.R
    # TODO: Would probably be faster to use -j2 NCPUS=1 as for other steps,
    # but many dependency compilations seem not parallel-safe.
    # More debugging needed.
    NCPUS=2 make -j1
    travis_time_end
)

# TESTING PECAN
(
    travis_time_start "test_pkgs" "Testing PEcAn packages"
    make test
    travis_time_end
)

# CHECKING PECAN
(
    travis_time_start "check_pkgs" "Checking PEcAn packages"
    REBUILD_DOCS=FALSE make check
    travis_time_end
)

# RUNNING SIMPLE PECAN WORKFLOW
(
    travis_time_start "integration_test" "Testing Integration using simple PEcAn workflow"
    ./tests/integration.sh travis
    travis_time_end
)

# CHECK FOR CHANGES TO DOC/DEPENDENCIES
if [[ `git status -s` ]]; then 
    echo "These files were changed by the build process:";
    git status -s;
    echo "Have you run devtools::check and commited any updated Roxygen outputs?";
    exit 1; 
fi
