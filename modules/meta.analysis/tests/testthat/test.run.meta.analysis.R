#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
test_that("p.point.in.prior function is functional",{
  prior <- list(distn = "norm", parama = 0, paramb = 1)
  expect_equal(p.point.in.prior(point = 3, prior = prior),
               pnorm(3))
  expect_equal(p.point.in.prior(point = -3, prior = prior),
               pnorm(-3))
})


test_that("singleMA gives expected result for example inputs",{
  ## need to calculate x
  ## x <- singleMA(....)
  #expect_equal(round(summary(x)$statistics["beta.o", "Mean"]), 5)
})