#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


test_that("query base can execute a trivial SQL statement and return results",{  
  if(db.exists(dbparms)){
    con <- db.open(dbparms)
    ans <- db.query("select count(*) from traits;", con = con)
    expect_is(ans, "data.frame")
    expect_is(ans[,1], "numeric")
    expect_true(length(ans) == 1)
    
    tables <- db.query('show tables;', con = con)
    expect_true(is.data.frame(tables))
    expect_true(is.character(tables[,1]))
    expect_true(ncol(tables) == 1)
  }
})
