#!/bin/bash

## send files to cluster
tar -zcf pecan.edrunfiles.tgz saconfigs.tgz bash/pecan.ed2in.create bash/pecan.batchjobs.sh
rsync -outi pecan.edrunfiles.tgz ebi-cluster:/home/$USER/EDBRAMS/ED/run/pecan.edrunfiles.tgz


