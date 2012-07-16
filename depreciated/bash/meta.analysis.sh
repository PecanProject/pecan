#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
echo "start meta-analysis"
env PECANSETTINGS=$1 R --vanilla < ./rscripts/meta.analysis.R
wait
echo "Meta-analysis complete"
echo "         next command is:" 
echo "         ./bash/write.configs.sh settings*xml"
