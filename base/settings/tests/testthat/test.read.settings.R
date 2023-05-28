#----------------------------------------------------------------------------

## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
#------------------------------------------------------------------------------
context("tests for read.settings and related functions")

PEcAn.logger::logger.setQuitOnSevere(FALSE)
PEcAn.logger::logger.setLevel("OFF")
testdir <- tempfile()
dir.create(testdir, showWarnings = FALSE)
teardown(unlink(testdir, recursive = TRUE))


test_that("read.settings() strips comments", {
  s_comments <- read.settings("testsettings-comment.xml")
  s <- read.settings("testsettings.xml")
  expect_equal(s_comments, s)
})

test_that("read.settings() warns if named input file doesn't exist (but pecan.xml does)", {
  old_setting <- PEcAn.logger::logger.setLevel("DEBUG")
  on.exit(PEcAn.logger::logger.setLevel(old_setting))
  #hacky way to check for errors b/c PEcAn.logger errors are non-standard and
  #not captured by testthat::expect_message() or expect_error()
  x <- capture.output(
    read.settings("blahblahblah.xml"),
    type = "message"
  )
  expect_true(any(grepl("WARN", x)))
  expect_true(any(grepl("blahblahblah.xml not found", x)))
})

test_that("read settings returns error if no settings file found (#1124)", {
  withr::with_tempdir({ #in a dir with no pecan.xml
    expect_error(read.settings("nofile.xml"), "Could not find a pecan.xml file")
  })
})