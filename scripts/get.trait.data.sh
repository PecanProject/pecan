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
#  get.trait.data.sh
#  
#
#  
# v1 - 05/31/2012
############################################################################################

clear
echo "********************************************************************"
echo "********************************************************************"
echo "***************** Querying database for trait data *****************"
echo "********************************************************************"
echo "********************************************************************"


############################################################################################
# Info: First launches R script to open and parse PEcAn XML config file for use throughout
# the PEcAn workflow.  Then queries database for trait data specific to each selected PFT.

#R --vanilla --args $1 < ./db/R/get.trait.data.R
