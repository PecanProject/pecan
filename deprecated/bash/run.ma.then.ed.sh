#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
cd /home/dlebauer/pecan
./bash/query.bety.sh $1
wait
./bash/meta.analysis.sh $1
wait
./bash/write.configs.sh $1
wait
./bash/start.runs.sh $1