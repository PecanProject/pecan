#!/bin/bash
echo "begin Sensitivity Analysis"
R --vanilla --args $1 <rscripts/sensitivity.analysis.R
echo "end Sensitivity Analysis"