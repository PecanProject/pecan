#!/bin/bash
for f in ED2INc*[0-9n]; do
  LOG="$f-`date +%Y.%m.%d-%H.%M`.log" #name with date tag for log files, one per ED2IN file 
  if which qsub > /dev/null; then
      CMD="qsub -N $f -o $LOG ./run 1 $f" #defines command to be run
  else
      CMD="ed2 -f $f"
  fi
  echo $CMD > $LOG # enters command into first line of log file
  echo $f >> $LOG
  $CMD             # runs command
done
