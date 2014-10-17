#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

con <- db.open(list(driver = "PostgreSQL", user = "bety", dbname = "bety", password = "bety"))

test_that("db.query can execute a trivial SQL statement and return results",{  
    ans <- db.query("select count(*) from traits;", con = con)
    expect_is(ans, "data.frame")
    expect_is(ans[,1], "numeric")
    expect_true(length(ans) == 1)
})
