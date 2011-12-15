#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR/../..
R CMD build pecan
PECAn=`ls -v PECAn*tar.gz | tail -n 1`
R CMD INSTALL $PECAn
