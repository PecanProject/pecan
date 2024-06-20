
context("Testing Logger Settings")

test_that("`logger.getLevelNumber` returns correct level number",{
  expect_equal(logger.getLevelNumber("all"), 0)
  expect_equal(logger.getLevelNumber("debug"), 10)
  expect_equal(logger.getLevelNumber("info"), 20)
  expect_equal(logger.getLevelNumber("warn"), 30)
  expect_equal(logger.getLevelNumber("error"), 40)
  expect_equal(logger.getLevelNumber("severe"), 40)
  expect_equal(logger.getLevelNumber("off"), 60)
  
  old_settings <- logger.setLevel("ERROR")
  on.exit(logger.setLevel(old_settings), add = TRUE)
  expect_equal(logger.getLevelNumber("INVALID"), 20)
})

test_that("`setWidth` works for different specified number of chars per line",{
  logger.setUseConsole(TRUE, FALSE)
  on.exit(logger.setUseConsole(TRUE, TRUE), add = TRUE)
  
  expect_output(logger.info("A long error message that helps us understand what the error in the function is"),
                "INFO   \\[.*\\] : \\n   A long error message that helps us understand what the error in the \\n   function is ")
  
  old_width <- .utils.logger$width
  logger.setWidth(10)
  on.exit(logger.setWidth(old_width), add = TRUE)
  expect_output(logger.info("A long error message that helps us understand what the error in the function is"),
                "INFO   \\[.*\\] : \\n   A long \\n   error \\n   message \\n   that \\n   helps \\n   us \\n   understand \\n   what \\n   the \\n   error \\n   in the \\n   function \\n   is ")
  
  logger.setWidth(30)
  expect_output(logger.info("A long error message that helps us understand what the error in the function is"),
                "INFO   \\[.*\\] : \\n   A long error message that \\n   helps us understand what \\n   the error in the function \\n   is ")
  
})

test_that("logger prints right messages, responds correctly to logger.setLevel",{

  logger.setUseConsole(TRUE, FALSE)
  on.exit(logger.setUseConsole(TRUE, TRUE), add = TRUE)

  old_settings <- logger.setLevel("ALL")
  on.exit(logger.setLevel(old_settings), add = TRUE)
  expect_equal(logger.getLevel(), "ALL")
  expect_output(logger.debug("message"), "DEBUG  \\[.*\\] : message")
  expect_output(logger.info("message"),  "INFO   \\[.*\\] : message")
  expect_output(logger.warn("message"),  "WARN   \\[.*\\] : message")
  expect_output(logger.error("message"), "ERROR  \\[.*\\] : message")

  logger.setLevel("DEBUG")
  expect_equal(logger.getLevel(), "DEBUG")
  expect_output(logger.debug("message"), "DEBUG  \\[.*\\] : message")
  expect_output(logger.info("message"),  "INFO   \\[.*\\] : message")
  expect_output(logger.warn("message"),  "WARN   \\[.*\\] : message")
  expect_output(logger.error("message"), "ERROR  \\[.*\\] : message")

  logger.setLevel("INFO")
  expect_equal(logger.getLevel(), "INFO")
  expect_silent(logger.debug("message"))
  expect_output(logger.info("message"),  "INFO   \\[.*\\] : message")
  expect_output(logger.warn("message"),  "WARN   \\[.*\\] : message")
  expect_output(logger.error("message"), "ERROR  \\[.*\\] : message")

  logger.setLevel("WARN")
  expect_equal(logger.getLevel(), "WARN")
  expect_silent(logger.debug("message"))
  expect_silent(logger.info("message"))
  expect_output(logger.warn("message"),  "WARN   \\[.*\\] : message")
  expect_output(logger.error("message"), "ERROR  \\[.*\\] : message")

  logger.setLevel("ERROR")
  expect_equal(logger.getLevel(), "ERROR")
  expect_silent(logger.debug("message"))
  expect_silent(logger.info("message"))
  expect_silent(logger.warn("message"))
  expect_output(logger.error("message"), "ERROR  \\[.*\\] : message")

  logger.setLevel("OFF")
  expect_equal(logger.getLevel(), "OFF")
  expect_silent(logger.debug("message"))
  expect_silent(logger.info("message"))
  expect_silent(logger.warn("message"))
  expect_silent(logger.error("message"))

  logger.setUseConsole(FALSE)
  logger.setLevel("DEBUG")
  expect_silent(logger.debug("message"))
  expect_silent(logger.info("message"))
  expect_silent(logger.warn("message"))
  expect_silent(logger.error("message"))

  logger.setUseConsole(TRUE)
  logger.setLevel("ALL")
  expect_equal(logger.getLevel(), "ALL")
  expect_silent(logger.debug("message"))
  expect_silent(logger.info("message"))
  expect_silent(logger.warn("message"))
  expect_silent(logger.error("message"))

  logger.setQuitOnSevere(FALSE)
  on.exit(logger.setQuitOnSevere(TRUE), add = TRUE)
  expect_error(logger.severe("message"), "message")
})

test_that("logger message labels match enclosing function", {
  logger.setUseConsole(console = TRUE, stderr = FALSE)
  on.exit(logger.setUseConsole(console = TRUE, stderr = TRUE), add = TRUE)

  expect_output(identity(logger.info("message")), "[identity] : message", fixed = TRUE)
  expect_output(identity(PEcAn.logger::logger.info("message")), "[identity] : message", fixed = TRUE)
})

test_that("`logger.message` able to redirect logging information to file set by `logger.setOutputFile`", {
  on.exit(logger.setOutputFile(NA), add = TRUE)
  f <- withr::with_tempfile("tf", {
    logger.setOutputFile(tf)
    logger.message("WARN", "message")
    readLines(tf)
  })
  expect_true(grepl(".*WARN   \\[.*\\] : message", f))
})