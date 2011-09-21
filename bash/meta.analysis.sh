#!/bin/bash
echo "start meta-analysis"
env PECANSETTINGS=$1 R --vanilla < ./rscripts/meta.analysis.R
wait
echo "Meta-analysis complete"
echo "         next command is:" 
echo "         ./bash/meta.analysis.sh 2011.08.26/settings..."
