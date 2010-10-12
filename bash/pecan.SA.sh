#!/bin/bash

env YR0=$1 YRF=$2 date=`cat DATE` R --vanilla < ./rscripts/pecan.SA.runscript.R
#rsync ebi-cluster:/home/scratch/pecan/$USER/out$DATE/satables.Rdata ./
 