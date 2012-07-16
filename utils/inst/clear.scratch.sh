#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#
#--------------------------------------------------------------------------------------------------#
LOC=/scratch/$USER
if [ -d "$LOC" ]; then
	echo ""
	echo "---- Removing scratch directory: "$LOC;
	rm -r $LOC
else
	echo ""
        echo "---- Scratch directory: "$LOC" doesn't exist";
fi
wait
echo ""
echo "------ Process complete ------"
echo ""
exit
#--------------------------------------------------------------------------------------------------#
