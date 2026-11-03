#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# https://opensource.org/licenses/NCSA
#-------------------------------------------------------------------------------
library(testthat)
library(PEcAn.utils)

PEcAn.logger::logger.setQuitOnSevere(FALSE)
test_check("PEcAn.MAAT")
