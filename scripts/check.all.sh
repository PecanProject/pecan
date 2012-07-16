for f in common utils db modules/meta.analysis modules/uncertainty modules/rtm models/ed
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
do
  echo "---- Checking PEcAn package: $f"
  R CMD check $f
  wait
  echo -n "Move on to next package? [ENTER]"
  read
  clear
done
