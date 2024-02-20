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
  testresult2 <- dplyr::mutate(testresult, site_id= 1) 
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


test_that("`left.pad.zeros()` able to add zeros to the left of a number based on `digits`", {
  expect_equal(left.pad.zeros(123), "00123")
  expect_equal(left.pad.zeros(42, digits = 3), "042")
  expect_equal(left.pad.zeros(42, digits = 1), "42")
})

test_that("`zero.truncate()` able to truncate vector at zero", {
  input <- c(1, NA, -3, NA, 5)
  expect_equal(zero.truncate(input), c(1, 0, 0, 0, 5))
})

test_that("`tabnum()` able to convert positive and negative numbers to `n` significant figures", {
  
  # case where n specified
  x <- c(-2.345, 6.789)
  result <- tabnum(x, 2)
  expect_equal(result, c(-2.3, 6.8))

  # case where n is default
  result <- tabnum(3.5435)
  expect_equal(result, 3.54)
})

test_that("`capitalize()` able to capitalize words in a sentence", {
  # single word
  expect_equal(capitalize("pecan"), "Pecan")

  # sentence with leading and trailing spaces
  expect_equal(capitalize("    pecan project    "), "    Pecan Project   ")
})

test_that("`bibtexify()` able to convert parameters passed to bibtex citation format", {
  expect_equal(bibtexify("author", "1999", "Here Goes The Title"), "author1999HGTT")
})

test_that("`rsync()` able to correctly make the command passed to `system` function", {
  mocked_res <- mockery::mock(0)
  mockery::stub(rsync, 'system', mocked_res)
  rsync(args = '-avz', from = 'pecan:test_src', to = 'pecan:test_des')
  args <- mockery::mock_args(mocked_res)
  expect_equal(args[[1]][[1]], "rsync -avz pecan:test_src pecan:test_des")
})

test_that("`ssh()` able to correctly make the command passed to `system` function", {
  mocked_res <- mockery::mock(0)
  mockery::stub(ssh, 'system', mocked_res)
  ssh(host = 'pecan')
  args <- mockery::mock_args(mocked_res)
  expect_equal(args[[1]][[1]], "ssh -T pecan \"\" ")
})

test_that("`temp.settings()` able to create a temporary settings file", {
  expect_equal(temp.settings('<pecan></pecan>'), '<pecan></pecan>')
})

test_that("`misc.convert()` able to unit conversions for known and unknown units to the function", {

  # units known to misc.convert
  expect_equal(misc.convert(1, "kg C m-2 s-1", "umol C m-2 s-1"), 83259094)
  # units not known to misc.convert
  expect_equal(misc.convert(10, "kg", "g"), 10000)
})

test_that("`misc.are.convertible()` able to check if units are convertible by `misc.convert`", {
  # units known to misc.convert
  expect_true(misc.are.convertible("kg C m-2 s-1", "umol C m-2 s-1"))
  # units known but not interconvertible
  expect_false(misc.are.convertible("kg C m-2 s-1", "Mg ha-1"))
  # units not known to misc.convert
  expect_false(misc.are.convertible("kg", "g"))
})

test_that("`convert.expr()` able to convert expression to variable names", {
  res <- convert.expr("a+b=c+d")
  expect_equal(res$variable.drv, "a+b")
  expect_equal(res$variable.eqn$variables, c("c", "d"))
  expect_equal(res$variable.eqn$expression, "c+d")
})

test_that("`paste.stats()` able to print inputs to specific format(for building a Latex Table)", {
  expect_equal(paste.stats(3.333333, 5.00001, 6.88888, n = 3), "$3.33(5,6.89)$")
})

test_that("`zero.bounded.density()` returns output containing required components", {
  res <- zero.bounded.density(c(1, 2, 3))
  expect_true("x" %in% names(res))
  expect_true("y" %in% names(res))
})

test_that("`pdf.stats()` able to calculate mean, variance statistics, and CI from a known distribution", {
  expect_equal(
    pdf.stats("beta", 1, 2), 
    unlist(list(mean = 0.33333333, var = 0.05555556, lcl = 0.01257912, ucl = 0.84188612))
  )
})

test_that("`newxtable()` generates correct xtable object", {
  data <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
  expect_true(grepl("\\hline.*& A & B.*& 1.00 & 4.00.*& 2.00 & 5.00.*& 3.00 & 6.00", newxtable(data)))
})

test_that("`tryl()` able to check if a function gives an error when called", {
  # case where function does not give an error
  expect_true(tryl(1+1))

  # case where function gives an error
  expect_false(tryl(log("a")))
})

test_that("`download_file()` able to correctly construct the inputs command to system function", {
  mocked_res <- mockery::mock(0)
  mockery::stub(download_file, 'system', mocked_res)
  download_file("ftp://testpecan.com", "test", "ncftpget")
  args <- mockery::mock_args(mocked_res)
  expect_equal(args[[1]][[1]], "ncftpget -c ftp://testpecan.com > test")
})

test_that("`retry.func()` able to retry a function before returning an error", {
  defaultW <- getOption("warn") 
  options(warn = -1)
  on.exit(options(warn = defaultW))
  expect_error(
    retry.func(ncdf4::nc_open("http://pecan"), maxErrors = 2, sleep = 2),
    "retry: too many retries"
  )

  # case where function does not give an error
  expect_equal(retry.func(1+1, maxErrors = 2, sleep = 2), 2)
})

