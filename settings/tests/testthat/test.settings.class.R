#----------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
## #-------------------------------------------------------------------------------
context("test settings class")

.getNewTestList <- function() {
  return(list(aa=1, bb=2))
}

.getNewTestSettings = function() {
  s <- .getNewTestList()
  class(s) = c("Settings", "list")
  return(s)
}


test_that("settings indexing works as expected", {
  l <- .getNewTestList()
  s <- .getNewTestSettings()
  
  # [[ works same for list and settings object
  expect_equal(s[["bb"]], 2) 
  expect_equal(l[["bb"]], 2) 
  expect_equal(s[["bb", exact=T]], 2) 
  expect_equal(l[["bb", exact=T]], 2)   
  expect_equal(l[["b", exact=F]], 2) 
  expect_equal(s[["b", exact=F]], 2) 
  expect_null(l[["b", exact=T]]) 
  expect_null(s[["b", exact=T]]) 
  
  # $ operator works the same for both when exact match
  expect_equal(l$bb, 2)
  expect_equal(s$bb, 2)
  
  # $ operator returns NULL (same as [[ name, exact=T]]) if no exact match
  expect_equal(l$b, 2)
  expect_null(s$b)
})
