#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
test_that("convert.samples.ED works as expected",{
  expect_equivalent(convert.samples.ED(c("Vcmax" = 1)),
                    0.7052557)
  expect_equivalent(signif(convert.samples.ED(c("root_respiration_rate" = 1)),5),
                    c("root_respiration_factor" = 0.35263))
})
