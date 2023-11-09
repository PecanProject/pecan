test_that("`is_definitely_true` handles invalid conditionals passed",{
  expect_equal(is_definitely_true(NULL), FALSE)
  expect_equal(is_definitely_true(""), FALSE)
  expect_equal(is_definitely_true("pecan"), FALSE)
})

test_that("`is_definitely_true` handles single conditional statement correctly",{
  test_list <- list(1:10)
  expect_equal(is_definitely_true("pecan"=="carya"), FALSE)
  expect_equal(is_definitely_true(is.list(test_list)), TRUE)
  expect_equal(is_definitely_true("pecan"=="pecan" && "pecan"!="bety" && is.list(test_list)), TRUE)
  expect_equal(is_definitely_true("pecan"=="pecan" || ("pecan"=="bety" && is.list(test_list))), TRUE)
})

test_that("`check_conditions` handles multiple conditional statements correctly",{
  test_list <- list(1:10)
  expect_equal(check_conditions(FALSE && TRUE, "pecan"=="pecan"), FALSE)
  expect_equal(check_conditions("pecan"=="pecan", TRUE || FALSE && is.list(test_list)), TRUE)
})


test_that( "logifnot prints right message based on the conditions passed, responds correctly to logger.setLevel",{
  logger.setUseConsole(TRUE, FALSE)
  on.exit(logger.setUseConsole(TRUE, TRUE), add = TRUE)

  old_settings <- logger.setLevel("ALL")
  on.exit(logger.setLevel(old_settings), add = TRUE)

  expect_output(debugifnot("message", FALSE), "DEBUG  \\[.*\\] : message")
  expect_output(infoifnot("message", FALSE), "INFO   \\[.*\\] : message")
  expect_output(warnifnot("message", FALSE),  "WARN   \\[.*\\] : message")
  expect_output(errorifnot("message", FALSE), "ERROR  \\[.*\\] : message")
  expect_silent(debugifnot("message", TRUE))
  expect_silent(infoifnot("message", TRUE))
  expect_silent(warnifnot("message", TRUE))
  expect_silent(errorifnot("message", TRUE))

  logger.setLevel("DEBUG")
  expect_output(debugifnot("message", FALSE), "DEBUG  \\[.*\\] : message")
  expect_output(infoifnot("message", FALSE),  "INFO   \\[.*\\] : message")
  expect_output(warnifnot("message", FALSE),  "WARN   \\[.*\\] : message")
  expect_output(errorifnot("message", FALSE), "ERROR  \\[.*\\] : message")

  logger.setLevel("INFO")
  expect_silent(debugifnot("message", FALSE))
  expect_output(infoifnot("message", FALSE),  "INFO   \\[.*\\] : message")
  expect_output(warnifnot("message", FALSE),  "WARN   \\[.*\\] : message")
  expect_output(errorifnot("message", FALSE), "ERROR  \\[.*\\] : message")

  logger.setLevel("WARN")
  expect_silent(debugifnot("message", FALSE))
  expect_silent(infoifnot("message", FALSE))
  expect_output(warnifnot("message", FALSE),  "WARN   \\[.*\\] : message")
  expect_output(errorifnot("message", FALSE), "ERROR  \\[.*\\] : message")

  logger.setLevel("ERROR")
  expect_silent(debugifnot("message", FALSE))
  expect_silent(infoifnot("message", FALSE))
  expect_silent(warnifnot("message", FALSE))
  expect_output(errorifnot("message", FALSE), "ERROR  \\[.*\\] : message")

  logger.setLevel("OFF")
  expect_silent(debugifnot("message", FALSE))
  expect_silent(infoifnot("message", FALSE))
  expect_silent(warnifnot("message", FALSE))
  expect_silent(errorifnot("message", FALSE))
    
  logger.setQuitOnSevere(FALSE)
  on.exit(logger.setQuitOnSevere(TRUE), add = TRUE)
  expect_error(severeifnot("message", FALSE), "message")
})




