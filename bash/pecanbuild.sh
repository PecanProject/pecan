#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR/../..

## allow optional input
## alternative would be to use default value syntax, remove ifelse and replace $pecandir with ${1:-pecan}
if [ ! -z $1 ] 
then 
    pecandir=$1
else
    pecandir=pecan
fi

R --vanilla CMD build $pecandir
PECAn=`ls -v PECAn*tar.gz | tail -n 1`
R --vanilla CMD INSTALL $PECAn
