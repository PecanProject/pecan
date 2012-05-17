#!/bin/bash

export PECANSETTINGS="pecan.xml" 
export PECANHOME="@PECAN_HOME@"

rm STATUS

# setup pecan (create scripts, folders, etc.)
echo -e -n "SETUP\t`date +'%F %T'`" >> STATUS
R CMD BATCH --vanilla ${PECANHOME}/rscripts/setup.R
if [ $? -ne 0 ]; then
    echo -e "\t`date +'%F %T'`\tERROR" >> STATUS
    exit 1
fi
bash ./setup.sh
if [ $? -ne 0 ]; then
    echo -e "\t`date +'%F %T'`\tERROR" >> STATUS
    exit 2
fi
echo -e "\t`date +'%F %T'`\tDONE" >> STATUS

# setup pss/css by running fia2ED
echo -e -n "FIA2ED\t`date +'%F %T'`" >> STATUS
#R CMD BATCH --vanilla ${PECANHOME}/rscripts/fia2ED.R
if [ $? -ne 0 ]; then
    echo -e "\t`date +'%F %T'`\tERROR" >> STATUS
    exit 3
fi
echo -e "\t`date +'%F %T'`\tDONE" >> STATUS

# get data from bety
echo -e -n "BETY\t`date +'%F %T'`" >> STATUS
R CMD BATCH --vanilla ${PECANHOME}/rscripts/query.bety.R
if [ $? -ne 0 ]; then
    echo -e "\t`date +'%F %T'`\tERROR" >> STATUS
    exit 4
fi
echo -e "\t`date +'%F %T'`\tDONE" >> STATUS

# run meta-analysis
echo -e -n "META\t`date +'%F %T'`" >> STATUS
R CMD BATCH --vanilla ${PECANHOME}/rscripts/meta.analysis.R
if [ $? -ne 0 ]; then
    echo -e "\t`date +'%F %T'`\tERROR" >> STATUS
    exit 5
fi
echo -e "\t`date +'%F %T'`\tDONE" >> STATUS

# write model specific configs
echo -e -n "CONFIG\t`date +'%F %T'`" >> STATUS
R CMD BATCH --vanilla ${PECANHOME}/rscripts/write.configs.R
if [ $? -ne 0 ]; then
    echo -e "\t`date +'%F %T'`\tERROR" >> STATUS
    exit 6
fi
echo -e "\t`date +'%F %T'`\tDONE" >> STATUS

#launch job
echo -e -n "MODEL\t`date +'%F %T'`" >> STATUS
JOBS=$( bash ./launcher.sh )
echo $JOBS > jobs

# check if job is done
bash ./check.sh
rm jobs
echo -e "\t`date +'%F %T'`\tDONE" >> STATUS

# diagnostics plots
echo -e -n "PLOTS\t`date +'%F %T'`" >> STATUS
R CMD BATCH --vanilla "--args ${PWD}/run ${PWD}/run/ED2INc.ENS00001" ${PECANHOME}/rscripts/pecan.ed2.diagnostics.R
if [ $? -ne 0 ]; then
    echo -e "\t`date +'%F %T'`\tERROR" >> STATUS
#    exit 8
else
    echo -e "\t`date +'%F %T'`\tDONE" >> STATUS
fi

# all done
echo -e -n "FINISHED\t`date +'%F %T'`" >> STATUS
R CMD BATCH --vanilla ${PECANHOME}/rscripts/finished.R
if [ $? -ne 0 ]; then
    echo -e "\t`date +'%F %T'`\tERROR" >> STATUS
    exit 9
fi
echo -e "\t`date +'%F %T'`\tDONE" >> STATUS
