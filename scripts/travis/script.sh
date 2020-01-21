#!/bin/bash

set -e
. $( dirname $0 )/func.sh

# GENERATING DEPENDENCIES
(
    travis_time_start "dependency_generate" "Generate PEcAn package dependencies"
    Rscript scripts/generate_dependencies.R
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
)


# INSTALLING PECAN (compile, intall, test, check)
(
    travis_time_start "pecan_make_test" "Testing PEcAn"
    make test
    travis_time_end
)


# INSTALLING PECAN (compile, intall, test, check)
(
    travis_time_start "pecan_make_check" "Checking PEcAn"
    REBUILD_DOCS=FALSE RUN_TESTS=FALSE make check
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
    echo -e "\nThese files were changed by the build process:";
    git status -s;
    echo "Have you run devtools::check and commited any updated Roxygen outputs?";
    echo -e "travis_fold:start:gitdiff\nFull diff:\n";
    git diff;
    echo -e "travis_fold:end:gitdiff\n\n";
    exit 1;
fi
