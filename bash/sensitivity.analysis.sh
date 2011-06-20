#!/bin/bash
echo "begin Sensitivity Analysis"
env PECANSETTINGS=$1 R --vanilla < rscripts/sensitivity.analysis.R
echo "end Sensitivity Analysis"

