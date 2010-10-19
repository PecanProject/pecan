#!/bin/bash
tar -zcf pecan.edrunfiles.tgz saconfigs.tgz bash/pecan.ed2in.create.sh bash/pecan.ed.batchjobs.sh

rsync -outi pecan.edrunfiles.tgz ebi-cluster:/home/$USER/EDBRAMS/ED/run/pecan.edrunfiles.tgz


