#----------------------------------------------------------------------------

## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
## #-------------------------------------------------------------------------------

context("tests for read.settings and related functions")

settings <- read.settings(system.file("tests/pecan.sipnet.xml", package = "PEcAn.settings"))

test_that("read.settings returned correctly", {
	expect_true(file.exists(settings$outdir))
	expect_true(file.info(settings$outdir)$isdir)
	expect_true(file.exists(file.path(settings$outdir, "pecan.xml")))
})

test_that("read settings returns error if no settings file found (issue #1124)",{
	## TODO RK : test does not work yet
	## expect_message(read.settings("nofile.xml"), "Could not find a pecan.xml file")
})


test_that("check.settings throws error if required content not there", {
  for(node in c("pfts", "run")){
    s <- settings
    s[[node]] <- NULL
    expect_error(check.settings(s))    
  }
  for(date in c("start.date", "end.date")){
    s <- settings
    s$run[[date]] <- NULL
    expect_error(check.settings(s))
  }
})

test_that("check settings converts start/end year to start/end date",{
  s <- settings
  s1 <- list(pfts = s$pfts, database = s$database)

  s1$run <- list(start.year = 2000, end.year = 1000)
  expect_error(check.settings(s1))
  
  s1$run <- list(start.year = 1000, end.year = 1001)  
  s2 <- check.settings(s1)
  expect_equal(s2$run$start.date, "1000-01-01 00:00:00")
  expect_equal(s2$run$end.date, "1001-12-31 23:59:59")
  check.settings(s1)
  
  
  s1 <- list(pfts = s$pfts, database = s$database, run = s$run)
  s1$ensemble <- list(start.year = 2001, end.year = 2003, variable = "FOO")
  s2 <- check.settings(s1)
  expect_equal(s2$ensemble$start.date, s2$run$start.date)
  
  s1$ensemble$start.date <- NULL
  s1$ensemble$start.year <- 2004
  s1$ensemble$end.year <- 2004
  s2 <- check.settings(s1)
  expect_equal(s2$ensemble$start.date, "2004-01-01 00:00:00")
  
  for(node in c("sensitivity.analysis", "ensemble")) {
    s1 <- list(pfts = s$pfts, database = s$database, run = s$run)
    s1[[node]] <- list(start.year = 2003, end.year = 2004, variable = "FOO")
    s3 <- check.settings(s1)
    expect_equal(s3[[node]]$start.date, "2003-01-01 00:00:00")
    expect_equal(s3[[node]]$end.date, "2004-12-31 23:59:59")

    s1[[node]] <- list(start.year = 2004, end.year = 2003, variable = "FOO")
    expect_error(check.settings(s1))
  }
  
 
})