##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------settings.text <- "

settings <- read.settings(system.file("tests/test.settings.xml", package = "PEcAn.all"))


## the following test should work after #1128 has been resolved

if(db.exists()){
  test_that("database has a managements table with appropriate columns",{
    workflows <- query.base("describe workflows;", con = query.base.con(settings))
    expect_true(all(c("site_id", "model_id", "hostname", "start_date", "end_date", "params", "folder", "started_at", "finished_at", "created_at", "updated_at", "advanced_edit") %in% workflows$Field))
  })
}