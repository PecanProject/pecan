#!/bin/bash

## send files to cluster
tar -zcf pecan.edrunfiles.tgz saconfigs.tgz pecan.env.vars.sh pecan.ed2in.create pecan.batchjobs.sh
ssh put pecan.edrunfiles.tgz ebi-cluster:/home/$USER/EDBRAMS/ED/run/pecan.edrunfiles.tgz

## unzip config files, set env vars, write ED2IN files, run ED ensemble, 
ssh ebi-cluster "cd $HOME/EDBRAMS/ED/run; tar -zxf pecan.edrunfiles.tgz; tar-zxf saconfigs.tgz; chmod +x *.sh; ./pecan.env.vars.sh; ./pecan.ed2in.create.sh; ./pecan.ed.batchjobs.sh"


