#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
test.stats <- data.frame(Y=rep(1,5),
                           stat=rep(1,5),
                           n=rep(4,5),
                           statname=c('SD', 'MSE', 'LSD', 'HSD', 'MSD'))

test_that("transformstats works",{
  expect_equal(signif(transformstats(test.stats)$stat, 5),
               c(0.5, 0.5, 0.12734, 0.071333, 1.1559))
  expect_true(all(transformstats(test.stats)$statname == "SE"))
  expect_equal(test.stats$Y, transformstats(test.stats)$Y)
  expect_equal(test.stats$n, transformstats(test.stats)$n)          
  expect_false(any(as.character(test.stats$statname) ==
                   as.character(transformstats(test.stats)$statname)))
})


test_that('arrhenius scaling works', {
  expect_equivalent(signif(arrhenius.scaling(1,2,3),5),
                    c(1.0403))
  expect_equivalent(signif(arrhenius.scaling(1,3,2),5),
                    c(0.96129))  
})


test_that("vecpaste works",{

  ## vecpaste()
  expect_that(vecpaste(c('a','b')),
              equals("'a','b'"))
  expect_that(vecpaste(1),
              equals("'1'"))
})
test_that("left.pad.zeros works",{
  ## left.pad.zeros()
  expect_that(left.pad.zeros(1,2),
              equals("01"))
  expect_that(left.pad.zeros(100,2),
              equals("100"))
})

test_that("get.run.id works",{
  expect_equal(get.run.id("a", "b", "c", "d"), "a-d-c-b")
  expect_equal(get.run.id("a", "b", "c"), "a-c-b")
  expect_equal(get.run.id("a", "b"), "a-b")
  expect_equal(get.run.id("ENS", left.pad.zeros(1, 5)), "ENS-00001")
  expect_equal(get.run.id("SA", round(pnorm(-3),3), trait = "Vcmax"), "SA-Vcmax-0.001")
})

test_that("summarize.result works appropriately", {
  require(plyr)
  ## generate testdata  
  testresult <- data.frame(citation_id = 1,
                           site_id = 1:10,
                           trt_id = rep(c('control', 'fert'),5),
                           control = rep(c(0,1), 5), 
                           greenhouse = c(rep(0,5), rep(1,5)),
                           date = 1,
                           time = NA,
                           cultivar_id = 1,
                           specie_id = 1,
                           n = 1,
                           mean = sqrt(1:10),
                           stat = 'none',
                           statname = 'none'
                           )
  testresult2 <- transform(testresult, site_id= 1) 
  expect_that(summarize.result(testresult)$mean, equals(testresult$mean))
  expect_that(nrow(summarize.result(testresult2)), equals(4))
})


test_that("as.sequence works",{
  expect_identical(as.sequence(c("a", "b")),
                   c(1,2))
  expect_identical(as.sequence(c("a", NA)),
                   c(1,2))
  expect_equal(as.sequence(c("a", NA), na.rm = FALSE),
               c(1,NA))
  expect_equal(as.sequence(c(NA,NA)), c(1,1))
})

test_that("tryl returns FALSE if error, else true ", {
  expect_true(tryl(1+1))
  expect_true(!tryl(log("a")))
})
