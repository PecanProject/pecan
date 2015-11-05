#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
context("Testing utility functions")

con <- db.open(list(driver = "PostgreSQL", user = "bety", dbname = "bety", password = "bety"))

test_that("get.id works on some tables, and with different inputs", {
  pftid <- get.id("pfts", "name", "salix", con)
  expect_is(pftid, "numeric")
  
  pftname <- 'ebifarm.salix'
  modeltypeid <- 1
  pftid <- get.id("pfts", c("name", "modeltype_id"), c(pftname, modeltypeid), con)
  pft <- db.query(paste0("select name, modeltype_id from pfts where id = ", pftid), con)
  expect_equal(pft$name, pftname)
  expect_equal(pft$modeltype_id, modeltypeid)
})
