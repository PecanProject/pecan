#!/bin/bash
echo "start querying database"
env PECANSETTINGS=$1 R --vanilla < ./rscripts/query.bety.R
wait
echo "Database queries complete"
echo "         next command is:" 
echo "         ./bash/meta.analysis.sh settings.*.xml"
