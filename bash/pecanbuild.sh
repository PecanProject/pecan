#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR/../..
R CMD build pecan
PEcAn=`ls -v PEcAn*tar.gz | tail -n 1`
R CMD INSTALL $PEcAn -l ~/lib/R/
