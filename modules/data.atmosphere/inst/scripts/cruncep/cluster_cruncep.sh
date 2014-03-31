#!/bin/bash
#$ -j y
#$ -S /bin/bash
OUTPUT="/home/scratch/pecan/met/cruncep/out2"
echo "$(date) processing $y"
f=${1}.nc
echo $f
/usr/bin/ncks -A ${f} ${OUTPUT}/foo.nc
echo "$(date) done processing $y"


#echo "$(date) concatting all years"
#ionice -n 7 ncrcat -n 1975,36,1 ${OUTPUT}/1975.nc ${OUTPUT}/total.nc
#echo "$(date) done concatting all years"
