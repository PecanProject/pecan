#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
chmod a+x run
for f in ED2INc*[0-9n]; do
  LOG="$f-`date +%Y.%m.%d-%H.%M`.log" #name with date tag for log files, one per ED2IN file 
  if which qsub > /dev/null; then
      CMD="qsub -N $f -o $LOG ./run 1 $f" #defines command to be run
  else
      CMD="ed2 -f $f"
  fi
  echo $CMD > $LOG # enters command into first line of log file
  echo $f > $LOG
  $CMD             # runs command
done
