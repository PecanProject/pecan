#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

## test for feature 896 
test_that("all references to BETY have been removed from source code",{
  betygrep <- system("find ../../../ -type f | grep -v depreciated | grep -v bzr | grep -v Rhistory | grep -v test |  xargs grep -s bety", intern = TRUE)
  warning("PEcAn still references bety\nsee list of occurances above")
  writeLines(betygrep)
})
