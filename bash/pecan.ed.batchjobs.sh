#!/bin/bash

DATE=`date +%Y%m%d`

if [ ! -d /home/scratch/dlebauer/output/grassSA/out$DATE/ ] #if [output directory] exists 
then
    mkdir /home/scratch/dlebauer/output/grassSA/out$DATE    #if not, make new directory
fi

tar zxf saconfigs.tgz #unzip config files

##Make new ED2IN file for each config file
cp aED2IN ED2IN
sed -i 's/YYYYMMDD/'$DATE'/g' ED2IN

for f in c.*
do 
    cp ED2IN ED2IN$f
    sed -i 's/CONFIGFILE/'$f'/g' ED2IN$f
    sed -i 's/OUTFILE/out'$f'/g' ED2IN$f 
    sed -i 's/outconfig./out./g' ED2IN$f
    sed -i 's/HISTFILE/hist'$f'/g' ED2IN$f
    sed -i 's/histconfig./hist./g' ED2IN$f
done

rm ED2IN


## run jobs
for f in ED2IN*; do
  LOG="$f-`date +%Y.%m.%d-%H.%M`.log" #name with date tag for log files, one per ED2IN file 
  CMD="qsub -cwd -pe mpich 1 -j y -m eas -M dlebauer@illinois.edu -o $LOG ./run 1 $f" #defines command to be run
  echo $CMD > $LOG #enters command into first line of log file
  echo $f > $LOG
  $CMD             #runs command
  echo $JOBID >> jobs #enters JOBIDs into jobs file
done

##when jobs are done
##tar -zcf histfiles.tgz /home/scratch/dlebauer/grassSA/$DATE/hist*xml
##tar -zcf logfiles.tgz *log

