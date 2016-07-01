#----------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
## #-------------------------------------------------------------------------------
context("test Settings classes")

test_that("Settings constructors work as expected", {
  l <- list(aa=1, bb=2, cc=list(dd=3, ee=4))
  sl <- SafeList(l)
  settings1 <- Settings(aa=1, bb=2, cc=list(dd=3, ee=4))
  settings2 <- Settings(l)
  settings3 <- Settings(sl)
  settings4 <- Settings(settings1)
  
  for(i in seq_along(l)) {
    expect_identical(settings1[[i]], l[[i]])
  }
  expect_identical(settings1, settings2)
  expect_identical(settings1, settings3)
  expect_identical(settings1, settings4)
  
  expect_true(is(settings1, "list"))
  expect_true(is(settings1, "SafeList"))
  expect_true(is(settings1, "Settings"))
  expect_true(is.Settings(settings1))
  expect_false(is.Settings(sl))
  expect_false(is.Settings(l))
  expect_equal(length(class(settings1)), 3)
})


test_that("SettingsList constructor works as expected", {
  l <- list(aa=1, bb=2, cc=list(dd=3, ee=4))
  sl <- SafeList(l)
  settings <- Settings(l)
  
  expect_error(SettingsList(l, l))
  expect_error(SettingsList(sl, l))
  expect_error(SettingsList(settings, l))
  
  settingsList <- SettingsList(settings, settings, settings)
  settingsList2 <- SettingsList(list(settings, settings, settings))
  settingsList3 <- SettingsList(settingsList)
  expect_identical(settingsList2, settingsList)
  expect_identical(settingsList3, settingsList)
  
  for(i in seq_along(settingsList)) {
    expect_identical(settingsList[[i]], settings)
  }
  
  expect_true(is(settingsList, "list"))
  expect_true(is(settingsList, "SettingsList"))
  expect_true(is.SettingsList(settingsList))
  expect_false(is.SettingsList(l))
  expect_equal(length(class(settingsList)), 2)
})


test_that("SettingsList assignments work as expected", {
  l <- list(aa=1, bb=2, cc=list(dd=3, ee=4))
  settings <- Settings(l)
  
  settingsList <- SettingsList()
  
  expect_error(settingsList[[1]] <- l)
  expect_silent(settingsList[[1]] <- settings)
  expect_identical(settingsList[[1]], settings)
  expect_equal(length(settingsList), 1)
  
  expect_error(settingsList$a <- l)
  expect_silent(settingsList$a <- settings)
  expect_identical(settingsList[[2]], settings)
  expect_equal(length(settingsList), 2)
  expect_identical(names(settingsList), c("", "a"))
  
  expect_silent(settingsList[[1]] <- NULL)
  expect_equal(length(settingsList), 1)
  expect_identical(names(settingsList), c("a"))
  
  expect_silent(settingsList$a <- NULL)
  expect_equal(length(settingsList), 0)
  
  expect_error(settingsList[1] <- l)
  expect_error(settingsList[1] <- settings)
})


















