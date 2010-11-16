#!/bin/bash
EDIN=/home/scratch/$USER/pecan/edin
cd $PECANHOME
rsync -outi saconfigs.tgz ebi-cluster:$EDIN/
ssh -T ebi-cluster < bash/pecan.ed2in.create.sh
wait
## unzip config files, set env vars, write ED2IN files, run ED ensemble, 
ssh -T ebi-cluster < bash/pecan.ed.batchjobs.sh
