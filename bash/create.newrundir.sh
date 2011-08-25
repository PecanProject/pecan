#!/bin/bash
env SETTINGSTEMPLATE=$1 RUNDIRNAME=$2 R --vanilla < ./rscripts/create.newrundir.R
echo "new rundir $2 created on localhost with settings and ED2IN template files"
echo "new rundir $2 created on modelhost with output directory and met drivers"
