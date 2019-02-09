#!/bin/bash

set -e
. $( dirname $0 )/func.sh

# (
#     fold_start "generate_dependencies" "Generate dependencies"
#     Rscript scripts/generate_dependencies.R
#     fold_end
# )


# - set -e
# - Rscript scripts/generate_dependencies.R
# - echo 'Installing PEcAn packages' && echo -en 'travis_fold:start:install_pkgs\\r'
# # TODO: Would probably be faster to use -j2 NCPUS=1 as for other steps,
# # but many dependency compilations seem not parallel-safe.
# # More debugging needed.
# - NCPUS=2 make -j1
# - echo -en 'travis_fold:end:install_pkgs\\r'
# #
# - echo 'Testing PEcAn packages' && echo -en 'travis_fold:start:test_pkgs\\r'
# - make test
# - echo -en 'travis_fold:end:test_pkgs\\r'
# #
# - echo 'Checking PEcAn packages' && echo -en 'travis_fold:start:check_pkgs\\r'
# - REBUILD_DOCS=FALSE make check
# - echo -en 'travis_fold:end:check_pkgs\\r'
# #
# - echo 'Testing Integration' && echo -en 'travis_fold:start:integration_test\\r'
# - ./tests/integration.sh travis
# - echo -en 'travis_fold:end:integration_test\\r'
# #

# (
#     fold_start "check_git_status" "Check for changed files"
#     if [[ `git status -s` ]]; then 
#         echo "These files were changed by the build process:";
#         git status -s;
#         echo "Have you run devtools::check and commited any updated Roxygen outputs?";
#         exit 1; 
#     fi
#     fold_end
# )
