#!/bin/bash
#PBS -N biocro-globaltest
#PBS -j oe
#PBS -S /bin/bash
#PBS -d /home/a-m/dlebauer/.pecan/
#PBS -m abe
#PBS -e dlebauer+biocluster@gmail.com

module load gsl hdf5 netcdf nco R/3.0.2

export R_LIBS_USER="/home/a-m/dlebauer/library/R"
/home/a-m/dlebauer/dev/pecan/models/biocro/inst/globalbiocro.Rscript