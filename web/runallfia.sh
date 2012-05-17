#!/bin/bash

export PECANSETTINGS="pecan.xml" 
export PECANHOME="@PECAN_HOME@"

rm STATUS

# setup pecan (create scripts, folders, etc.)
echo -e -n "SETUP\t`date +'%F %T'`" >> STATUS
R CMD BATCH --vanilla ${PECANHOME}/rscripts/setup.R
bash ./setup.sh
echo -e "\t`date +'%F %T'`" >> STATUS

# setup pss/css by running fia2ED
echo -e -n "FIA2ED\t`date +'%F %T'`" >> STATUS
R CMD BATCH --vanilla ${PECANHOME}/rscripts/fia2ED.R
echo -e "\t`date +'%F %T'`" >> STATUS

# get data from bety
echo -e -n "BETY\t`date +'%F %T'`" >> STATUS
R CMD BATCH --vanilla ${PECANHOME}/rscripts/query.bety.R
echo -e "\t`date +'%F %T'`" >> STATUS

# run meta-analysis
echo -e -n "META\t`date +'%F %T'`" >> STATUS
R CMD BATCH --vanilla ${PECANHOME}/rscripts/meta.analysis.R
echo -e "\t`date +'%F %T'`" >> STATUS

# write model specific configs
echo -e -n "CONFIG\t`date +'%F %T'`" >> STATUS
R CMD BATCH --vanilla ${PECANHOME}/rscripts/write.configs.R
echo -e "\t`date +'%F %T'`" >> STATUS

#launch job
echo -e -n "MODEL\t`date +'%F %T'`" >> STATUS
JOBS=$( bash ./launcher.sh )
echo $JOBS > jobs

# check if job is done
bash ./check.sh
rm jobs
echo -e "\t`date +'%F %T'`" >> STATUS

# diagnostics plots
echo -e -n "PLOTS\t`date +'%F %T'`" >> STATUS
R CMD BATCH --vanilla "--args ${PWD}/run ${PWD}/run/ED2INc.ENS00001" ${PECANHOME}/rscripts/pecan.ed2.diagnostics.R
echo -e "\t`date +'%F %T'`" >> STATUS

# all done
echo -e -n "FINISHED\t`date +'%F %T'`" >> STATUS
R CMD BATCH --vanilla ${PECANHOME}/rscripts/finished.R
echo -e "\t`date +'%F %T'`" >> STATUS
