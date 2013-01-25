#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
context("tests of overall workflow")

settings.file <- system.file("inst/extdata/test.settings.xml", package = "PEcAn.utils")
settings <- read.settings(settings.file)

# get.trait.data()
# run.meta.analysis()     
# run.write.configs("ED2")
# clear.scratch(settings)
# start.model.runs("ED2")
# get.model.output("ED2")
