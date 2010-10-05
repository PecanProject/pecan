#!/bin/bash
env PFT=$1 ITER=$2 ENSN=$3 R --vanilla < ./rscripts/pecan.start.R
R --vanilla < ./rscripts/pecan.MA.runscript.R