#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# version of pecan
VERSION=$1
shift
DATE=`date +"%Y-%m-%d"`

# if no arguments passed in update all DESCRIPTION files
if [ $# -gt 0 ]; then
  FILES="$*"
else
  FILES=$( find . -name DESCRIPTION -print )
fi

echo "Version   : $VERSION"
echo "Date      : $DATE"

for d in $FILES; do
  DIR=$( dirname $d )

  # update DESCRIPTION file version/date/license
  echo "Modifying : $d"
  sed -i.bak -e "s/^Version: .*$/Version: $VERSION/" \
             -e "s/^Date: .*$/Date: $DATE/" \
             -e "s/^License: .*/License: BSD_3_clause + file LICENSE/" $d
  if [ ! -e "${DIR}/LICENSE" ]; then
  	if [ -e LICENSE ]; then
  		echo "Copied LICENSE file to ${DIR}"
  		cp LICENSE ${DIR}
  	else
  		echo "Missing LICENSE file in ${DIR}"
  	fi
  fi
done

# update pecan version in web page
if [ $# -eq 0 ]; then
  echo "Modifying : web/common.php"
  sed -i.bak -e "s/PEcAn Version [0-9\.\-]*/PEcAn Version ${VERSION}/" web/common.php
fi
