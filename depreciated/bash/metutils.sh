#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
####################################################################################################
#/file												   #
#		Calls MetUtils.R script	to generate diganostics for ED2 met drivers		   #
#												   #
#		--- Useful for viewing summary stats and generating diagnostic			   #
#		    plots of ED2 input met drivers						   #
#												   #
#		--- Usage: MetUtils [arg1] [arg2] [arg3]					   #
#		    arg1 - ED2 run directory. E.g. "pwd" to run in local directory		   #
#					      or "/path/to/directory"				   #
#												   #
#		    arg2 - ED2IN file name. Could also set to "-f" to find file with "ED2IN"	   #
#			   in the filename. E.g. MetUtils pwd -f 2				   #
#												   #
#		    arg3 - 1 for stats only, 2 for stats and diagnostic plots			   #
#		    E.g. MetUtils pwd -f 2.  Option 1 not yet implimented.  Defaults to option 2   #
#												   #
#												   #
####################################################################################################

clear
echo "********************************************************************"
echo "********************************************************************"
echo "********************* Running ED2 Met Utilities ********************"
echo "********************************************************************"
echo "********************************************************************"
echo " "

######################################### Start Script #############################################
if [[ $1=="pwd" ]];then
	loc=$(pwd)
	echo "Running in: "$loc
else
	loc=$1
fi

echo " "

### This needs to be edited to reflect your local setup.
script_loc="/home/$USER/pecan/rscripts/"		# ----> Local pecan directory
script="MetUtils.R"					# ----> R script name

### Run MetUtils Script ---- Output log to MetUtils.log in working dir
R --vanilla --args $1 $2 $3 < $script_loc$script > $loc"/"MetUtils.log

clear
echo "********************************************************************"
echo "********************************************************************"
echo "**************** Finished Running ED2 Met Utilities ****************"
echo "********************************************************************"
echo "********************************************************************"


####################################################################################################
### EOF
####################################################################################################
