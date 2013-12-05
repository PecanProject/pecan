#!/bin/bash
for dir in lwdown press qair rain swdown swdown_6hourly_total tair uwind vwind;
do
    for file in /home/scratch/pecan/met/cruncep/${dir}/*.nc 
    do
      ionice -c2 -n7 ln -s $file /home/scratch/pecan/met/cruncep/in/
    done
done
