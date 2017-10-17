#----------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
## #-------------------------------------------------------------------------------
context("test SafeList class")

test_that("SafeList constructors work as expected", {
    l <- list(aa=1, bb=2, cc=list(dd=3, ee=4))
    s1 <- SafeList(aa=1, bb=2, cc=list(dd=3, ee=4))
    s2 <- SafeList(l)
    s3 <- as.SafeList(l)
    
    for(i in seq_along(l)) {
      expect_identical(s1[[i]], l[[i]])
    }
    expect_identical(s1, s2)
    expect_identical(s1, s3)
    
    expect_true(inherits(s1, "list"))
    expect_true(inherits(s1, "SafeList"))
    expect_true(is.SafeList(s1))
    expect_false(is.SafeList(l))
    expect_equal(length(class(s1)), 2)
})

test_that("SafeList indexing works as expected", {
  l <- list(aa=1, bb=2)
  s <- SafeList(l)
  
  # [[ works same for list and SafeList object
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

