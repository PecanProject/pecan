#!/bin/bash
echo "start allometry analysis"
env PECANSETTINGS=$1 R --vanilla < ./rscripts/AllomAve.R
wait
echo "Allometries complete"
