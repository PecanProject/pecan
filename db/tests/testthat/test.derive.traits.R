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

test_that("arrhenius.scaling.traits works",{
  set.seed(0)
  test.traits <- db.query("select * from traits where variable_id = 4 and id in (select trait_id from covariates where variable_id in (81, 86)) and mean > 0 limit 50;", con = con)
  test.cov <- query.covariates(test.traits$id, con = con)
  test.traits.5C <- arrhenius.scaling.traits(data = test.traits, covariates = test.cov, temp.covariates = 'leafT', new.temp = 5)
  test.traits.4C <- arrhenius.scaling.traits(data = test.traits, covariates = test.cov, temp.covariates = 'leafT', new.temp = 4)
  
  # Values scaled to 5 degC should be greater than those scaled to 4 degC (some will be equal, 
  # if they don't have covariates available).
  expect_true(all(test.traits.5C$mean >= test.traits.4C$mean))

  test.traits.leafT <- arrhenius.scaling.traits(data = test.traits, covariates = test.cov, temp.covariates = 'leafT', new.temp = 25)
  test.traits.leafTairT <- arrhenius.scaling.traits(data = test.traits, covariates = test.cov, temp.covariates = c('leafT','airT'), new.temp = 25)

  # Traits that have a leafT covariate should have scaled the same in both cases, since leafT
  # is listed first in the latter, and so takes precedence
  id.check <- test.cov$trait_id[test.cov$name=='leafT']
  expect_true(all(test.traits.leafT$mean     [test.traits.leafT$id     %in% id.check] == 
                  test.traits.leafTairT$mean [test.traits.leafTairT$id %in% id.check]))

  # But the results should differ for any trait that has airT, but not leafT (unless
  # airT is equal to the default of 25, in which case no scaling will have been done in 
  # either case
  id.check <- setdiff(test.cov$trait_id[test.cov$name=='airT' & test.cov$level!=25], id.check)
  expect_true(all(test.traits.leafT$mean     [test.traits.leafT$id     %in% id.check] != 
                  test.traits.leafTairT$mean [test.traits.leafTairT$id %in% id.check]))

})
