#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
test_that('is.localhost works', {
  expect_true(is.localhost("localhost"))
  expect_true(is.localhost(PEcAn.utils::fqdn()))
  expect_true(is.localhost(list(name = PEcAn.utils::fqdn())))
  expect_false(is.localhost("notarealmachine"))
})
