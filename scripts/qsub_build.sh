#!/bin/bash
#PBS -N pecan-build.sh
#PBS -j oe
#PBS -S /bin/bash
#PBS -d /home/a-m/dlebauer/dev/pecan/
#PBS -m abe
#PBS -e dlebauer+biocluster@gmail.com

module load R/3.1.1
module load nco/4.4.8
module load netcdf/4.3.3.1
module load parallel-netcdf/1.4.1
module load udunits/2.1.24
export R_LIBS_USER=/home/a-m/dlebauer/library/R

/home/a-m/dlebauer/dev/pecan/scripts/install.dependencies.R
/home/a-m/dlebauer/dev/pecan/scripts/build.sh --force --check --install --test
