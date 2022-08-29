#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
context("Other utilities")

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
  ## generate test data  
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
                           stat = NA,
                           statname = NA,
                           name = NA,
                           treatment_id = NA
  )
  # check that individual means produced for distinct sites
  expect_that(summarize.result(testresult)$mean, equals(testresult$mean)) 
  
  # check that four means are produced for a single site
  testresult2 <- transform(testresult, site_id= 1) 
  expect_that(nrow(summarize.result(testresult2)), equals(4))  
  
  # check that if stat == NA, SE will be computed
  testresult3 <- summarize.result(testresult2)
  expect_true(all(!is.na(testresult3$stat)))
  expect_equal(testresult3$n, c(3L, 2L, 2L, 3L))
  expect_equal(round(testresult3$stat, 3), c(0.359, 0.177, 0.293, 0.206))
  expect_equal(round(testresult3$mean, 3), c(1.656, 2.823, 1.707, 2.813))
  
  # check that site groups correctly for length(site) > 1
  testresult4 <- rbind.data.frame(testresult2, transform(testresult2, site_id= 2))
  testresult5 <- summarize.result(testresult4)
  expect_true(all(!is.na(testresult5$stat)))
  expect_equal(nrow(testresult5), 8)
  
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

test_that("mstmipvar works with defaults", {
  expect_s3_class(mstmipvar("NPP"), "ncvar4")
})

test_that("mstmipvar works with args specified", {
  lat <- 
    ncdf4::ncdim_def(
      name = "lat",
      longname = "station_latitude",
      units = "degrees_north",
      vals = 40,
      unlim = FALSE
    )
  lon <-
    ncdf4::ncdim_def(
      name = "lon",
      longname = "station_longitude",
      units = "degrees_east",
      vals = -80,
      unlim = FALSE
    )
  time <-
    ncdf4::ncdim_def(
      name = "time",
      units = "days since 1900-01-01 00:00:00",
      vals = 30798:44765,
      calendar = "standard",
      unlim = TRUE
    )
  nsoil <- 
    ncdf4::ncdim_def(
      name = "SoilLayerMidpoint",
      longname = "SoilLayerMidpoint",
      units = "meters",
      vals = c(-1.75,-1.25,-0.9,-0.7,-0.5,-0.3,-0.15,-0.075, 0),
      unlim = FALSE
    )
  
  expect_s3_class(mstmipvar("NPP", lat = lat, lon = lon, time = time, nsoil = nsoil),
                  "ncvar4")
})

# doesn't work because PEcAn.logger doesn't use message()
# test_that("mstmipvar prints message with unknown var", {
#   expect_message(
#     mstmipvar("banana"),
#     "Don't know about variable banana in standard_vars in PEcAn.utils"
#   )
# })
