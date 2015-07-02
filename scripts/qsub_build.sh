#!/bin/bash
#PBS -N pecan-build.sh
#PBS -j oe
#PBS -S /bin/bash
#PBS -d /home/a-m/dlebauer/dev/pecan/
#PBS -m abe
#PBS -e dlebauer+biocluster@gmail.com

source ${HOME}/.profile

${HOME}/dev/pecan/scripts/install.dependencies.R
${HOME}/dev/pecan/scripts/build.sh #--force --check --install --test
