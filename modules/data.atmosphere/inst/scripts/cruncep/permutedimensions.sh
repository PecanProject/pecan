#!/bin/bash
#$ -j y
#$ -S /bin/bash
#$ -o /home/scratch/pecan/met/cruncep/permute.log

cd /home/scratch/pecan/met/cruncep/
/usr/bin/ncpdq -a time,lon,lat $1.nc $1ts.nc



