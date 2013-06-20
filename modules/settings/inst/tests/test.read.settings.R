#----------------------------------------------------------------------------

## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
## #-------------------------------------------------------------------------------

test.settings <- system.file("extdata/test.settings.xml", package = "PEcAn.utils")
settings <- read.settings(test.settings)

test_that("read.settings works ",{
  expect_true(file.remove(file.path(settings$outdir, "pecan.xml")))
})



context("tests for read.settings and related functions")
test_that("read settings returns error if no settings file found (issue #1124)",{
  default.settings.files <- c("/etc/pecan.xml", "~/.pecan.xml",
                              "pecan.xml", Sys.getenv("PECAN_SETTINGS"))
  ## need to be revised for use with log.* functions
  ## if(!any(sapply(default.settings.files, file.exists))){
  ##   print("the following error messages are expected results of log.error")
  ##   expect_message(read.settings(), "Did not find any settings file to load.")
  ## }
})

context("check that example settings file is valid")

settings.list <- read.settings(inputfile = system.file("extdata/test.settings.xml",
                                  package = "PEcAn.utils"))
