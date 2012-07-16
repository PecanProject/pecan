#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
test_that("time utils work",{
  eng.months  <- c("jan", "feb", "mar", "apr", "may", "jun",
                  "jul", "aug", "sep", "oct", "nov", "dec")
  port.months <- c("jan", "fev", "mar", "abr", "mai", "jun",
                   "jul", "ago", "set", "out", "nov", "dez")
  expect_equal(mon2mmm(1), 'jan')
  expect_equal(mmm2mon('jan'), 1)
  expect_equal(mon2mmm(1:12), eng.months)
  expect_equal(mon2mmm(1:12, "Portuguese"), port.months)
  expect_equal(mmm2mon(port.months, "Portuguese"), 1:12)
  expect_equal(mmm2mon(eng.months), 1:12)
})
  
