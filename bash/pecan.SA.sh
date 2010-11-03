#!/bin/bash
echo "begin Sensitivity Analysis"
rsync /home/scratch/$USER/pecan/edin/DATE $PWD/out/
DATE=`cat $PWD/out/DATE`

R --vanilla < $PECANHOME/rscripts/pecan.SA.runscript.R
echo "end Sensitivity Analysis"

