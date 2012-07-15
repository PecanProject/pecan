#!/bin/bash

# version of pecan
VERSION="1.2"
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
