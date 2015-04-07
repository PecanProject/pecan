#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

## if put on a system path (echo $PATH), can be executed as
## find.string string\.to\.find
## where the '\' is used to escape periods and spaces
## this can be done using, e.g.
## sudo ln scripts/find.string.sh /usr/local/bin/find.string
## find.string 
find . -name "*.R" | xargs grep $1