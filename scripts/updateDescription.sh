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
VERSION="1.3.1"
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
  # update DESCRIPTION file version/date/license
  echo "Modifying : $d"
  sed -i.bak -e "s/^Version: .*$/Version: $VERSION/" \
             -e "s/^Date: .*$/Date: $DATE/" \
             -e "s/^License: .*/License: FreeBSD + file LICENSE/" $d
done
