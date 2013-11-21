#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

test_that("query.covariates returns expected data.frame",{
  settings <<- xmlToList(xmlParse("pecan.xml"))
  if(db.exists(settings$database)){
    ids <- 1:10
    test.query <- query.covariates(ids)
    expect_equal(colnames(test.query), c("trait_id", "level", "name"))
    expect_true(nrow(test.query) >= length(ids))
  }
})
  
test_that("append.covariates appends managements to yields",{
  settings <<- xmlToList(xmlParse("pecan.xml"))
  if(db.exists(settings$database)){
#    test.traits <- query.base("select * from traits where id < 10;")
#    covariates <- query.covariates(ids)
#    append.test <- append.covariate(test.traits, "id", covariates)
#    expect_true(nrow(append.test) >= nrow(covariates))
#    expect_true(nrow(append.test) >= nrow(test.traits))
  }
})
  
test_that("query.data works",{
  settings <<- xmlToList(xmlParse("pecan.xml"))
  if(db.exists(settings$database)){
#    expect_true(nrow(query.data("SLA", "938")) > 0)
    expect_equal(nrow(query.data("xyz", "-1")), 0)
#    expect_true(nrow(query.data("LMA", spstr = vecpaste(1:20000))) > 1000)
  }
})
