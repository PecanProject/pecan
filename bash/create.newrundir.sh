#!/bin/bash
env SETTINGSTEMPLATE=$1 RUNDIRNAME=$2 R --vanilla < ./rscripts/create.newrundir.R
echo "new rundir $2 created on localhost and modelhost"
echo "settings and ED2IN templates added to $PWD/$2"
echo "1. Edit settings and ED2IN if desired before running meta.analysis.sh"
echo "2. next command is: ./bash/meta.analysis.sh $2/settings.*"


