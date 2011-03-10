#!/bin/bash
env PECANSETTINGS=$1 R --vanilla < ./rscripts/settings.R
R --vanilla < ./rscripts/query.bety.R
echo "start meta-analysis"
R --vanilla < ./rscripts/metaanalysis.runscript.R
wait
echo "Meta-analysis complete"
