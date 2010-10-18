#!/bin/bash
env PFT=$1 ITER=$2 ENSN=$3 PECANOUT=$4 R --vanilla < ./rscripts/pecan.MA.runscript.R
