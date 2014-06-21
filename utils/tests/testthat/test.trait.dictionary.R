#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

test_that("trait dictionary loads and has expected columns",{
  rm(list = ls())
  data(trait.dictionary, package = "PEcAn.utils")
  expect_true(exists("trait.dictionary"))
  expect_true(all(c("id", "figid", "units", "model.id") %in%
                  colnames(trait.dictionary)))
  expect_true(ncol(trait.dictionary) >= 4) # dim = 49 x 4 at time of writing
  expect_true(nrow(trait.dictionary) >=49)
})


test_that("trait dictionary can be loaded ",{
  data("trait.dictionary", package = "PEcAn.utils")
  td <- read.csv(system.file("data", "trait.dictionary.csv", package = "PEcAn.utils"), sep = ";")
  expect_equal(trait.dictionary, td)
})