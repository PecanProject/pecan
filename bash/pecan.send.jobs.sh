#!/bin/bash

## send files to cluster
tar -zcf pecan.edrunfiles.tgz saconfigs.tgz bash/pecan.ed2in.create.sh bash/pecan.batchjobs.sh
echo 'pecan.edrunfiles.tgz, saconfigs.tgz, bash/pecan.ed2in.create
rsync -outi pecan.edrunfiles.tgz ebi-cluster:/home/$USER/EDBRAMS/ED/run/pecan.edrunfiles.tgz


