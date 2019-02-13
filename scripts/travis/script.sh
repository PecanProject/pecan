#!/bin/bash

set -e
. $( dirname $0 )/func.sh

# GENERATING DEPENDENCIES
(
    travis_time_start "dependency_generate" "Generate PEcAn package dependencies"
    Rscript scripts/generate_makefile_deps.R
    travis_time_end
)

# # INSTALLING PECAN (compile, intall, test, check)
(
    travis_time_start "compile_start" "Compiling PEcAn"
    . $( dirname $0 )/compile.sh
    travis_time_end
)


# CHECK FOR CHANGES TO DOC/DEPENDENCIES
if [[ `git status -s` ]]; then 
    echo "These files were changed by the build process:";
    git status -s;
    echo "Have you run devtools::check and commited any updated Roxygen outputs?";
    exit 1; 
fi

# RUNNING SIMPLE PECAN WORKFLOW
(
    travis_time_start "integration_test" "Testing Integration using simple PEcAn workflow"
    ./tests/integration.sh travis
    travis_time_end
)
