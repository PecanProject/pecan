#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

test_that("logger prints right messages",{
  logger.enable("DEBUG", TRUE)
  expect_output(logger.debug("message"), "DEBUG \\[.*\\] : message")
  logger.enable("DEBUG", FALSE)
  expect_output(logger.debug("message"), "^$")

  logger.enable("INFO", TRUE)
  expect_output(logger.info("message"), "INFO  \\[.*\\] : message")
  logger.enable("INFO", FALSE)
  expect_output(logger.info("message"), "^$")

  logger.enable("WARN", TRUE)
  expect_output(logger.warn("message"), "WARN  \\[.*\\] : message")
  logger.enable("WARN", FALSE)
  expect_output(logger.warn("message"), "^$")

  logger.enable("ERROR", TRUE)
  expect_output(logger.error("message"), "ERROR \\[.*\\] : message")
  logger.enable("ERROR", FALSE)
  expect_output(logger.error("message"), "^$")

  logger.enable("ERROR", TRUE)
  logger.output(console=FALSE)
  expect_output(logger.error("message"), "^$")
})
