ssh ebi-cluster "
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
cd ~/EDBRAMS/ED/run/ 
for i in *log 
do 
    if tail -n 1 $i | grep error > /dev/null 
    then 
	echo $i
    fi
done"