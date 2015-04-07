#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

con <- db.open(list(driver = "PostgreSQL", user = "bety", dbname = "bety", password = "bety"))
test_that("take.samples works",{
  expect_equal(take.samples(summary = data.frame(mean = 1, stat = NA)), 1)
  set.seed(0)
  test.sample <- take.samples(summary = data.frame(mean = 1, stat = 1),
                               sample.size = 2)
  expect_equal(test.sample, c(2.26295428488079, 0.673766639294351))
})

test_that("derive.traits works",{
  set.seed(0)
  input <- list(x = data.frame(mean = 1, stat = 1, n = 1))
  test.derived <- derive.trait(FUN = function(x){1/x},
                              input = input,
                              var.name = 'x')
  expect_equal(test.derived,
               structure(list(mean = 0.0944129994366609, stat = 687.395104414576, n = 1, vname = "x"), .Names = c("mean", "stat", "n", "vname"), row.names = c(NA, -1L), class = "data.frame"))
})
db.close(con = con)
