#!/bin/bash
echo "start writing configs"
R --vanilla < $PECANHOME/rscripts/pecan.writeconfigs.R
cd $PECANOUT
echo "finished writing configs"
echo "renaming config files"
rename config c *xml
rename _ '' *xml
rename factor '' *xml
rename root rt *xml
rename turnover tnvr *xml
rename conductance cndctnc *xml
rename respiration resp *xml
rename nonlocaldispersal nldisprs *xml
rename quantumefficiency quantef *xml
rename water h2o *xml
rename stomatalslope stmslope *xml
rename .xml '' *xml

echo "zipping config files to saconfigs.tgz"
tar zcf saconfigs.tgz c.*
rm c.*
## next: pecan.sendjobs.sh