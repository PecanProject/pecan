#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
library("PEcAn.BIOCRO")

test_that("run.write.configs works for sensitivity analyses and ensembles",{
  settings.file <- system.file("pecan.biocro.xml", package = "PEcAn.BIOCRO")
  settings <- read.settings(settings.file)
  run.write.configs("BIOCRO")
})
