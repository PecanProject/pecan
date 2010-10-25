#!/bin/bash
#to be run from forecast
PECANHOME=$3
rsync -routi ebi-cluster:/home/scratch/$USER/pecan/pecan.samps.Rdata $PECANOUT/
rsync -routi ebi-cluster:/home/scratch/$USER/pecan/edin/DATE $PECANOUT/

DATE=`cat $PECANHOME/out/DATE`
cd $PECANHOME
env YR0=$1 YRF=$2 DATE=$DATE $PECANHOME R --vanilla < $PECANHOME/rscripts/pecan.SA.runscript.R

