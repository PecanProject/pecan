#!/bin/bash
echo "start meta-analysis"
PECANDIR = grep pecanDir $1
echo $PECANDIR
#echo PFT is $1
#echo MCMC chain length is $2
#echo Ensemble Size is $3 
#env PFT=$1 ITER=$2 ENSN=$3 PECANOUT=$4 R --vanilla < ./rscripts/pecan.MA.runscript.R
env PECANSETTINGS=$1 R --vanilla < ./rscripts/pecan.MA.runscript.R
wait
echo "Meta-analysis complete"
