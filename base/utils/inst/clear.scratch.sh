#!/bin/bash

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
