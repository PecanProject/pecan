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


test_that("SettingsList assignments are blocked", {
  settings <- Settings(aa=1, bb=2, cc=list(dd=3, ee=4))
  settingsList <- SettingsList(settings)
  
  expect_identical(settingsList[[1]], settings)
  expect_equal(length(settingsList), 1)
  
  expect_error(settingsList[[1]] <- l)
  expect_error(settingsList[[1]] <- settings)
  expect_error(settingsList[2:3] <- list(l, l))
  expect_error(settingsList[2:3] <- settingsList)
  expect_error(settingsList$a <- l)
  expect_error(settingsList$a <- settings)
  expect_error(settingsList[[1]] <- NULL)
  expect_error(settingsList$a <- NULL)
  expect_error(settingsList[1:2] <- NULL)

  expect_identical(settingsList[[1]], settings)
  expect_equal(length(settingsList), 1)
})


test_that("SettingsList extracts work as expected", {
  s1 <- Settings(a=1, b=2, c=3)
  s2 <- Settings(a=1, b=22, d=4)
  s3 <- s2
  settingsList <- SettingsList(s1, s2, s3)
  
  # -- Normal extraction
  expect_identical(settingsList[[1]], s1)
  expect_identical(settingsList[1], SettingsList(s1))
  expect_identical(settingsList[1:3], settingsList)
  
  # Extract by name
  expect_equal(settingsList[["a"]], 1)
  expect_equal(settingsList$a, 1)
  expect_equivalent(settingsList[["b"]], list(2, 22, 22))
  expect_equivalent(settingsList$b, list(2, 22, 22))
  expect_equivalent(settingsList[["c"]], list(3, NULL, NULL))
  expect_equivalent(settingsList$c, list(3, NULL, NULL))
  expect_error(settingsList["a"]) # Because explicitly prohibited to prevent confusion
})

test_that("names.SettingsList works as expected", {
  s1 <- Settings(a=1, b=2, c=3)
  s2 <- Settings(a=1, b=22, d=4)
  s3 <- s2
  settingsList <- SettingsList(s1, s2, s3)
  
  expect_identical(names(settingsList), c("a", "b"))
  expect_error(names(settingsList) <- c("a", "b"))
  expect_error(names(settingsList) <- NULL)
  expect_error(names(settingsList)[1] <- "c")
})





















