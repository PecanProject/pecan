#!/bin/bash
## run jobs
rm *.log
for f in ED2IN*; do
  LOG="$f-`date +%Y.%m.%d-%H.%M`.log" #name with date tag for log files, one per ED2IN file 
  CMD="qsub -cwd -pe mpich 1 -j y -m eas -M dlebauer@illinois.edu -o $LOG ./run 1 $f" #defines command to be run
  echo $CMD > $LOG #enters command into first line of log file
  echo $f > $LOG
  $CMD             #runs command
  echo $JOBID >> jobs #enters JOBIDs into jobs file
done

# put backups in output directory
tar -zcf $OUTDIR/pecanconfigs$DATE.tgz ED2IN* c.*