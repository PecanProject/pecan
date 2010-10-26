#!/bin/bash
echo "begin Sensitivity Analysis"
DATE=`cat $PWD/out/DATE`

R --vanilla < $PECANHOME/rscripts/pecan.SA.runscript.R
echo "end Sensitivity Analysis"

