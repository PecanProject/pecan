#!/bin/bash
env SETTINGSTEMPLATE=$1 RUNDIRNAME=$2 R --vanilla < ./rscripts/create.newrundir.R
echo "new rundir $RUNDIRNAME created with settings and ED2IN template files"
