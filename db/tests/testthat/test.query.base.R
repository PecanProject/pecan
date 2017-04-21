#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
source('db.setup.R')

context("test db.query")

test_that("db.query can execute a trivial SQL statement and return results",{  
    con <- check_db_test()
    ans <- db.query("select count(*) from traits;", con = con)
    expect_is(ans, "data.frame")
    expect_is(ans[,1], "numeric")
    expect_true(length(ans) == 1)
    try(db.close(con))
})
