#!/bin/bash
#$ -j y
#$ -S /bin/bash
#$ -o concat.log

module load netcdf/4.3
cd /home/scratch/pecan/met/cruncep/out/

/usr/bin/ncrcat -n 36,4,1 1975.nc cruncep1975_2010timeslice.nc 
/usr/bin/ncrcat -n 74,4,1 1901.nc cruncep1901_1975timeslice.nc 



