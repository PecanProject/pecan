#!/bin/bash
echo "begin Sensitivity Analysis"
#rsync ebi-cluster:/home/scratch/$USER/pecan/edin/DATE $PWD/out/
DATE=`cat $PWD/out/DATE`

R --vanilla < rscripts/pecan.SA.runscript.R
echo "end Sensitivity Analysis"

