#!/bin/bash
echo "start meta-analysis"
env PECANSETTINGS=$1 R --vanilla < ./rscripts/meta.analysis.R
wait
echo "Meta-analysis complete"
echo "         next command is:" 
echo "         ./bash/write.configs.sh settings*xml"
