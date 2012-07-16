#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
dvr <-  dbDriver("MySQL")
newcon <- function() dbConnect(dvr, group = "bety", dbname = "bety", password = "3Magic", username = "dlebauer")
lapply(dbListConnections(drv = drv), dbDisconnect)

test_that("query.covariates returns expected data.frame";{
  ids <- 1:10
  test.query <- query.covariates(ids, con = newcon())
  expect_equal(colnames(test.query), c("trait_id", "level", "name"))
  expect_true(nrow(test.query) >= length(ids))
})

test_that("append.covariates appends managements to yields",{
  test.traits <- query.bety("select * from traits where id < 10;", con = newcon())
  covariates <- query.covariates(ids, con = newcon())
  append.test <- append.covariate(test.traits, "id", covariates)
  expect_true(nrow(append.test) >= nrow(covariates))
  expect_true(nrow(append.test) >= nrow(test.traits))
})

test_that("query.data works",{
  expect_true(nrow(query.data("SLA", "938", con = newcon())) > 0)
  expect_equal(nrow(query.data("xyz", "-1", con = newcon())), 0)
  expect_true(nrow(query.data("LMA",spstr = vecpaste(1:20000), con = newcon())) > 1000)
})

  test_that("query.bety.trait.data does not return 0 rows, refs issue #567",{
  
