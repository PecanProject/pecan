#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# packages that are to be compiled
PACKAGES="utils db"
PACKAGES="${PACKAGES} modules/meta.analysis modules/uncertainty"
PACKAGES="${PACKAGES} modules/data.land modules/data.atmosphere"
PACKAGES="${PACKAGES} modules/assim.batch modules/assim.sequential modules/priors"
PACKAGES="${PACKAGES} models/ed models/sipnet models/biocro"
PACKAGES="${PACKAGES} all"

for p in ${PACKAGES}; 
do
    R CMD INSTALL --build $p
done

  # cleanup
rm -rf *.Rcheck PEcAn.*.tar.gz
