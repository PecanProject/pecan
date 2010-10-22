#!/bin/bash
#to be run from cluster

cd /home/scratch/$USER/pecan/
DATE=`cat edin/DATE`

env YR0=$1 YRF=$2 DATE=$DATE R --vanilla < ./rscripts/pecan.SA.runscript.R
#rsync ebi-cluster:/home/scratch/pecan/$USER/out$DATE/satables.Rdata ./
 