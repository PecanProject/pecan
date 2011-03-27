#!/bin/bash
EDIN=/home/scratch/$USER/pecan/edin
PECANDIR=~/pecan/
OUTDIR=$1
#runs after pecan.writeconfigs.sh
cd $OUTDIR
tar -zcf saconfigs.tgz c.*
rsync -outi saconfigs.tgz ebi-cluster:$EDIN/
## write ED2IN files
ssh -T ebi-cluster < $PECANDIR/bash/pecan.ed2in.create.sh
wait
## unzip config files, set env vars, run ED ensemble, 
ssh -T ebi-cluster < $PECANDIR/bash/pecan.ed.batchjobs.sh
#next: wait for runs to compled, can use $edstat
#then  pecan.SA.sh
