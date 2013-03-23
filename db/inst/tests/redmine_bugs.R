##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html


## the following test should work after #1128 has been resolved


test_that("database has a managements table with appropriate columns",{
  settings <<- xmlToList(xmlParse("pecan.xml"))
  if(db.exists(settings$database)){
    workflows <- db.query("describe workflows;", params=settings$database)
    expect_true(all(c("site_id", "model_id", "hostname", "start_date", "end_date", "params", "folder", "started_at", "finished_at", "created_at", "updated_at", "advanced_edit") %in% workflows$Field))
  }
})
