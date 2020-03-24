#!/bin/bash

set -e
. $( dirname $0 )/func.sh

# GENERATING DEPENDENCIES
(
    travis_time_start "dependency_generate" "Generate PEcAn package dependencies"
    Rscript scripts/generate_dependencies.R
    travis_time_end
    check_git_clean
)

# DUMP PACKAGE VERSIONS
(
    travis_time_start "installed_packages" \
        "Version info of all installed R packages, for debugging"
    Rscript -e 'op <- options(width = 1000)' \
        -e 'pkgs <- as.data.frame(installed.packages())' \
        -e 'cols <- c("Package", "Version", "MD5sum", "Built", "LibPath")' \
        -e 'print(pkgs[order(pkgs$Package), cols], row.names = FALSE)' \
        -e 'options(op)'
    travis_time_end
)

# COMPILE PECAN
(
    travis_time_start "pecan_make_all" "Compiling PEcAn"
    # TODO: Would probably be faster to use -j2 NCPUS=1 as for other steps,
    # but many dependency compilations seem not parallel-safe.
    # More debugging needed.
    NCPUS=2 make -j1
    travis_time_end
    check_git_clean
)


# INSTALLING PECAN (compile, intall, test, check)
(
    travis_time_start "pecan_make_test" "Testing PEcAn"
    make test
    travis_time_end
    check_git_clean
)

# INSTALLING PECAN (compile, intall, test, check)
(
    travis_time_start "pecan_make_check" "Checking PEcAn"
    REBUILD_DOCS=FALSE RUN_TESTS=FALSE make check
    travis_time_end
    check_git_clean
)


# RUNNING SIMPLE PECAN WORKFLOW
(
    travis_time_start "integration_test" "Testing Integration using simple PEcAn workflow"
    ./tests/integration.sh travis
    travis_time_end
    check_git_clean
)
