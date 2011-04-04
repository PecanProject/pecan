#!/bin/bash
echo "start writing configs"
R --vanilla < $PECANHOME/rscripts/write.configs.R
cd $PECANOUT
echo "finished writing configs"
echo "renaming config files"
rename _ '' c.*
rename factor '' c.*
rename root rt c.*
rename turnover tnvr c.*
rename conductance cndctnc c.*
rename respiration resp c.*
rename nonlocaldispersal nldisprs c.*
rename quantumefficiency quantef c.*
rename water h2o c.*
rename stomatalslope stmslope c.*

echo "zipping config files to saconfigs.tgz"
tar zcf saconfigs.tgz c.*
rm c.*
## next: pecan.sendjobs.sh
