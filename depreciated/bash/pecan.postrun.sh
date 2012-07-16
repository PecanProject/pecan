#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
cd $PECANHOME
rsync -routi R/trait.dictionary.R  $CLUSTERPECAN/R/
rsync -routi out/*.Rdata  $CLUSTERPECAN/out/
rsync -routi R/pecan.edout.R  $CLUSTERPECAN/R/
rsync -routi rscripts/pecan.postrun.R $CLUSTERPECAN/rscripts/
echo "begin post-run data extraction"
ssh ebi-cluster "cd /home/scratch/$USER/pecan/; env YR0=$1 YRF=$2 R --vanilla < rscripts/pecan.postrun.R"
echo "done with post-run data extraction"
echo "copying files from cluster to forecast"
rsync -routi ebi-cluster:/home/scratch/$USER/pecan/out/*Rdata ~/pecan/out/

