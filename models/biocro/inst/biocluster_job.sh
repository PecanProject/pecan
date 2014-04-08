#!/bin/bash
# ----------------QSUB Parameters----------------- #
#PBS -S /bin/bash
#PBS -q default
#PBS -l nodes=1:ppn=8,mem=24000mb
#PBS -M dlebauer+biocluster@gmail.com
#PBS -m abe
#PBS -N biocro-ustest
#PBS -d /home/a-m/dlebauer/.pecan/
#PBS -oe /home/a-m/dlebauer/.pecan/
# ----------------Load Modules-------------------- #
module load R/3.0.2
module load nco/4.4.2
module load netcdf/4.3.1.1
module load parallel-netcdf/1.4.1
module load udunits/2.1.24
# ----------------Your Commands------------------- #
export R_LIBS_USER="/home/a-m/dlebauer/library/R"
/home/a-m/dlebauer/dev/pecan/models/biocro/inst/globalbiocro.Rscript