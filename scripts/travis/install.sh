#!/bin/bash

set -x
. $( dirname $0 )/func.sh

apt-cache search r-cran | awk '/r-cran/ { print $1 }' | sed "s/\(.*\)/           '\1',/"

# # INSTALLING SIPNET
# (
#     travis_time_start "install_sipnet" "Installing SIPNET for testing"
#     #fold_start "install_sipnet" "Installing SIPNET for testing"

#     pushd $HOME
#     curl -o sipnet_unk.tar.gz http://isda.ncsa.illinois.edu/~kooper/EBI/sipnet_unk.tar.gz
#     tar zxf sipnet_unk.tar.gz
#     cd sipnet_unk
#     make
#     popd

#     travis_time_end
#     #fold_end
# )

# # INSTALLING R BUILD TOOLS
# (
#     travis_time_start "install_r_build" "Installing R build tools"
#     Rscript -e 'install.packages(c("littler", "devtools"), dependencies=TRUE)'
#     export PATH=${PATH}:${R_LIBS_USER}/littler/examples:${R_LIBS_USER}/littler/bin
#     travis_time_end
# )

# # # INSTALLING PECAN DEPENDENCIES
# # (
# #     travis_time_start "install_pecan_dependencies" "Installing PEcAn dependencies"
# #     bash docker/depends/pecan.depends
# #     travis_time_end
# # )
