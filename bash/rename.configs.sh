#!/bin/bash
OUTDIR=$1
cd $OUTDIR
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
