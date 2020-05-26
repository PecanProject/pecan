#!/bin/bash

NAME=${1:-$HOSTNAME}

set -o pipefail

cd $( dirname $0 )
for f in ${NAME}.*.xml; do
    echo -en "travis_fold:start:TEST $f\r"
    rm -rf pecan output.log
    Rscript --vanilla ../web/workflow.R --settings $f 2>&1 | tee output.log
    if [ $? -ne 0 ]; then
        echo "----------------------------------------------------------------------"
        echo "TEST $f FAILED (RETURN CODE != 0)"
        echo "----------------------------------------------------------------------"
        exit -1
    elif [ $(grep -v DONE pecan/STATUS | wc -l) -ne 0 ]; then
        echo "----------------------------------------------------------------------"
        echo "TEST $f FAILED (ERROR IN STATUS)"
        echo "----------------------------------------------------------------------"
        cat pecan/STATUS
        exit -1
    else
        echo "----------------------------------------------------------------------"
        echo "TEST $f OK"
        echo "----------------------------------------------------------------------"
    fi
    rm -rf output.log pecan
    echo -en 'travis_fold:end:TEST $f\r'
done
