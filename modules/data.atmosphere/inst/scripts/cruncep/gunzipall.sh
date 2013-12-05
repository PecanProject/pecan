#!/bin/bash
for dir in lwdown press qair rain swdown swdown_6hourly_total tair uwind vwind;
do
    cd $dir
    for i in *.nc.gz 
    do
	ionice -c2 -n7 gunzip $i
    done
    cd ..
done
