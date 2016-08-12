#----------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
## #-------------------------------------------------------------------------------
context("test Settings classes")

if(FALSE) {
  library(testthat)
  library(devtools)
  rm(list=ls())
  load_all()
}


test_that("MultiSettings constructor works as expected", {
  l <- list(aa=1, bb=2, cc=list(dd=3, ee=4))
  sl <- SafeList(l)
  settings <- Settings(l)
  
  expect_error(MultiSettings(l, l))
  expect_error(MultiSettings(sl, l))
  expect_error(MultiSettings(settings, l))
  
  multiSettings <- MultiSettings(settings, settings, settings)
  multiSettings2 <- MultiSettings(list(settings, settings, settings))
  multiSettings3 <- MultiSettings(multiSettings)
  expect_identical(multiSettings2, multiSettings)
  expect_identical(multiSettings3, multiSettings)
  
  for(i in seq_along(multiSettings)) {
    expect_identical(multiSettings[[i]], settings)
  }
  
  expect_true(is(multiSettings, "list"))
  expect_true(is(multiSettings, "MultiSettings"))
  expect_true(is.MultiSettings(multiSettings))
  expect_false(is.MultiSettings(l))
  expect_equal(length(class(multiSettings)), 2)
})


test_that("Numeric assignments work at the MultiSettings level", {
  l <- list(aa=1, bb=2, cc=list(dd=3, ee=4))
  settings <- Settings(l)
  multiSettings <- MultiSettings(settings)
  
  expect_identical(multiSettings[[1]], settings)
  expect_equal(length(multiSettings), 1)
  
  # Error thrown for trying to add anything other than Settings
  expect_error(multiSettings[[1]] <- 3)
  expect_error(multiSettings[[1]] <- l)
  expect_error(multiSettings[2:3] <- c("a", "b"))
  expect_error(multiSettings[2:3] <- list(l, l))
  
  # Or for trying to add the wrong length MultiSettings by [
  expect_error(multiSettings[2:5] <- multiSettings)
  
  # Can add Settings though
  # First, by [[
  expect_silent(multiSettings[[2]] <- settings)
  expect_equal(length(multiSettings), 2)
  expect_identical(multiSettings[[1]], multiSettings[[2]])
  
  # Also by [
  newSettings <- settings
  newSettings$aa <- 2
  newMultiSettings <- MultiSettings(newSettings, settings)
  
  expect_silent(multiSettings[2:3] <- newMultiSettings)
  expect_equal(length(multiSettings), 3)
  expect_identical(multiSettings[[1]], settings)
  expect_identical(multiSettings[[2]], newSettings)
  expect_identical(multiSettings[[3]], settings)
  
  expect_silent(multiSettings[4] <- newSettings)
  expect_equal(length(multiSettings), 4)
  expect_identical(multiSettings[[1]], settings)
  expect_identical(multiSettings[[2]], newSettings)
  expect_identical(multiSettings[[3]], settings)
  expect_identical(multiSettings[[4]], newSettings)
})


test_that("Assignments by name work at the Settings level", {
  settings <- Settings(aa=1, bb=2, cc=list(dd=3, ee=4))
  multiSettings <- MultiSettings(settings)
  
  expect_identical(multiSettings[[1]], settings)
  expect_equal(length(multiSettings), 1)
  
  expect_silent(multiSettings$x <- 1)
  new.settings <- settings
  new.settings$x <- 1
  for(i in seq_along(multiSettings)) {
    expect_identical(multiSettings[[i]], new.settings)
  }
  
  expect_silent(multiSettings[["y"]] <- "y")
  new.settings$y <- "y"
  for(i in seq_along(multiSettings)) {
    expect_identical(multiSettings[[i]], new.settings)
  }
  
  expect_silent(multiSettings[["y"]] <- NULL)
  new.settings$y <- NULL
  for(i in seq_along(multiSettings)) {
    expect_identical(multiSettings[[i]], new.settings)
  }
})


test_that("MultiSettings extracts work as expected", {
  s1 <- Settings(a=1, b=2, c=3)
  s2 <- Settings(a=1, b=22, d=4)
  s3 <- s2
  multiSettings <- MultiSettings(s1, s2, s3)
  
  # -- Normal extraction
  expect_identical(multiSettings[[1]], s1)
  expect_identical(multiSettings[1], MultiSettings(s1))
  expect_identical(multiSettings[1:3], multiSettings)
  
  # Extract by name
  expect_equal(multiSettings[["a"]], 1)
  expect_equal(multiSettings$a, 1)
  expect_error(multiSettings[["b"]]) # Because not identical
  expect_error(multiSettings$b) # Because not identical
  expect_error(multiSettings[["c"]]) # Because not shared by all
  expect_error(multiSettings$c) # Because not identical
  expect_error(multiSettings["a"]) # Because explicitly prohibited to prevent confusion
})





















