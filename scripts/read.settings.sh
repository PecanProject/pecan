#!/bin/sh
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
############################################################################################
#  read.settings.sh
#  
#
#  
# v1 - 05/31/2012
############################################################################################


clear
echo "********************************************************************"
echo "********************************************************************"
echo "****************** Reading in PEcAn Settings File ******************"
echo "********************************************************************"
echo "********************************************************************"


############################################################################################
# Info: Launches R script to open and parse PEcAn XML config file for use throughout
# the PEcAn workflow.

R --vanilla --args $1 < ./common/R/read.settings.R
#R CMD BATCH --args $1 < ./common/R/read.settings.R
wait

echo " "
echo " "
echo "---- Opening PEcAn XML file is complete"

############################################################################################
### EOF
############################################################################################
