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
