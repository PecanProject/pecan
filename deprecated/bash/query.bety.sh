#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
echo "start querying database"
env PECANSETTINGS=$1 R --vanilla < ./rscripts/query.bety.R
wait
echo "Database queries complete"
echo "         next command is:" 
echo "         ./bash/meta.analysis.sh settings.*.xml"
