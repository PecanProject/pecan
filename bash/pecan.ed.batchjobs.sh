EDIN=/home/scratch/$USER/pecan/edin
DATE=`date +%Y%m%d`
OUTDIR=/home/scratch/pecan/$USER/out$DATE
ED_RUN=$HOME/EDBRAMS/ED/run

## remove old ED2IN, configs, and logs
for i in $ED_RUN/c.* 
do 
    if [ -a $i ]
    then rm $i
    fi
done

for i in $ED_RUN/ED2IN.*
do 
    if [ -a $i ] 
    then rm $i
    fi
done

rsync $EDIN/ED2IN* $ED_RUN/
rsync $EDIN/c.p* $ED_RUN/

cd $ED_RUN
for f in ED2IN*; do
  LOG="$f-`date +%Y.%m.%d-%H.%M`.log" #name with date tag for log files, one per ED2IN file 
  CMD="qsub -cwd -pe mpich 1 -j y -m eas -M dlebauer@gmail.edu -o $LOG ./run 1 $f" #defines command to be run
  echo $CMD > $LOG #enters command into first line of log file
  echo $f > $LOG
  $CMD             #runs command
done
