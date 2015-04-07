#!/bin/sh

R CMD BATCH ./sugarcane.R
#  rm ./sugarcane.Rout
sed s/\"//g ./ooutput  > ./plot_data/output
# rm ./ooutput
echo "done"
