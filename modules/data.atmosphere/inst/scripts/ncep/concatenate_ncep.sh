#!/bin/bash

INPUT="/home/djaiswal/database/NCEP"
OUTPUT="/home/kooper/weatherfiles"

mkdir -p ${OUTPUT}
for y in `seq 1948 2012`; do
  echo "$(date) processing $y"
  for v in ${INPUT}/* ; do
     if [[ $v != *4hr ]]; then
       for f in ${v}/*${y}.nc; do
         ionice -n 7 ncks -A ${f} ${OUTPUT}/${y}.nc
       done
     fi
  done
  echo "$(date) done processing $y"
done

echo "$(date) concatting all years"
ionice -n 7 ncrcat -n 1948,65,1 ${OUTPUT}/1948.nc ${OUTPUT}/total.nc
echo "$(date) done concatting all years"

wget http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Diagnostic/.surface/.LAND/data.cdf
mv data.cdf mask.nc
ncrename -d X,lat -d Y,lon -v X,lat -v Y,lon mask.nc 
ncwa -a T mask.nc newmask.nc
ncks -x -v T mask.nc mask.nc
