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
library(RPostgreSQL)
dbparms <- list(driver = "PostgreSQL", user = "bety", dbname = "bety", password = "bety")

if(db.exists(dbparms)){
  con <- db.open(dbparms)
  logger.setQuitOnSevere(FALSE)
  test_check("PEcAn.DB")
  db.close(con)
}
