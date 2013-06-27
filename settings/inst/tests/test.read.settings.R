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
