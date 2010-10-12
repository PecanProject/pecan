#!/bin/bash
DATE=`cat DATE`
rsync ebi-cluster:/home/scratch/pecan/$USER/out$DATE/satables.Rdata ./
env yr0=$1 yrf=2 date=DATE R --vanilla < ./rscripts/pecan.SA.runscript.R
 