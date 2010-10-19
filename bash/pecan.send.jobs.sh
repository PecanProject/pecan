#!/bin/bash
EDIN=/home/scratch/$USER/pecan/edin
cp out/saconfigs.tgz ./
tar -zcf pecan.edrunfiles.tgz saconfigs.tgz bash/pecan.ed2in.create.sh bash/pecan.ed.batchjobs.sh

rsync -outi pecan.edrunfiles.tgz ebi-cluster:$EDIN/pecan.edrunfiles.tgz
wait
ssh ebi-cluster "cd $EDIN; tar -zxf pecan.edrunfiles.tgz; wait; tar -zxf saconfigs.tgz; chmod +x bash/*sh; wait; bash/pecan.ed2in.create.sh"
wait
## unzip config files, set env vars, write ED2IN files, run ED ensemble, 
ssh ebi-cluster "cd $EDIN; bash/pecan.ed.batchjobs.sh"


