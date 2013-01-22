#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
library(testthat)
library(PEcAn.DB)

settings <- list(database = 
                   list(userid = "bety", 
                        passwd = "bety", 
                        location = "localhost",
                        name = "bety"))
if(!grepl("Error", try(query.base.con(), silent = TRUE))){
  test_package("PEcAn.DB")
}