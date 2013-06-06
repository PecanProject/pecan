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

## test_that("test.settings.xml has an unique output directory for each PFT",{
##   pfts <- unlist(settings.list$pfts)
##   i.pfts   <- names(pfts) == "pft.name"
##   i.outdir <- names(pfts) == "pft.outdir"
##   expect_equal(sum(i.pfts), sum(i.outdir))
##   expect_equal(sum(i.pfts), length(unique(pfts[i.pfts])))
##   expect_equal(length(unique(pfts[i.pfts])), length(unique(pfts[i.outdir])))
##   rm(i.pfts, i.outdir)      
## })

test_that("read.settings gives expected warnings",{
  writeLines(con = "warning1144.xml",
             text = "<pecan><pfts><pft>
                      <name>testPFTname</name>
                      <outdir>/tmp/</outdir></pft></pfts></pecan>") 
  writeLines(con = "fixed1144.xml",
             text = "<pecan> 
                      <outdir>/tmp/</outdir>
                      <pfts><pft>
                        <name>testPFTname</name>
                        <outdir>/tmp/</outdir>
                      </pft></pfts></pecan>")
  expect_output(read.settings("warning1144.xml"), "No output folder")
  fixed1144 <- read.settings("fixed1144.xml")
  expect_equal(fixed1144$pfts$pft$name, "testPFTname")
  file.remove("bug1144.xml", "warning1144.xml", "fixed1144.xml")
})
