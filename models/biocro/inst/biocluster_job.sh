#!/bin/bash
# ----------------QSUB Parameters----------------- #
#PBS -S /bin/bash
#PBS -q default
#PBS -M dlebauer+biocluster@gmail.com
#PBS -m abe
# ----------------Load Modules-------------------- #
module load R/3.1.1
module load nco/4.4.8
module load netcdf/4.3.3.1
module load udunits/2.1.24
# ----------------Your Commands------------------- #
export R_LIBS_USER="/home/a-m/dlebauer/library/R"
