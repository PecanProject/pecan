#!/bin/bash
rsync saconfigs.tgz ebi-cluster:/home/dlebauer/EDBRAMS/ED/run/
rsync ./bash/pecan.ed.batchjobs.sh ebi-cluster:/home/dlebauer/EDBRAMS/ED/run/
ssh ebi-cluster '/home/dlebauer/EDBRAMS/ED/run/pecan.ed.batchjobs.sh'
DATE=`date +%Y%m%d`
ssh cp saconfigs.tgz saconfigs.$DATE.tgz

