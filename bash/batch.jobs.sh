#!/bin/bash
rm c.* 
rm ED2INc*
tar -zxf configs.tgz 
rm configs.tgz
for f in ED2INc*; do
  LOG="$f-`date +%Y.%m.%d-%H.%M`.log" #name with date tag for log files, one per ED2IN file 
  CMD="qsub -cwd -pe mpich 1 -j y -o $LOG ./run 1 $f" #defines command to be run
  echo $CMD > $LOG # enters command into first line of log file
  echo $f > $LOG
  $CMD             # runs command
done
