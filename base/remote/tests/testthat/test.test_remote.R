test_that("test_remote succeeds when output has extra lines around the test string", {
  host <- list(name = "somehost")
  test_string <- "Testing remote somehost"
  mockery::stub(
    test_remote,
    "remote.execute.cmd",
    c("Warning: Permanently added 'somehost' to the list of known hosts.", test_string)
  )
  expect_true(test_remote(host))
})

test_that("test_remote reports failure, without crashing, when multi-line output lacks the test string", {
  host <- list(name = "somehost")
  mockery::stub(
    test_remote,
    "remote.execute.cmd",
    c("first line of an error", "second line of an error")
  )
  mockery::stub(test_remote, "PEcAn.logger::logger.error", NULL)
  expect_false(test_remote(host, stderr = FALSE))
})

test_that("test_remote throws the documented error, with all output, when stderr = TRUE", {
  host <- list(name = "somehost")
  mockery::stub(
    test_remote,
    "remote.execute.cmd",
    c("first line of an error", "second line of an error")
  )
  PEcAn.logger::logger.setQuitOnSevere(FALSE)
  on.exit(PEcAn.logger::logger.setQuitOnSevere(TRUE), add = TRUE)
  expect_error(test_remote(host), "second line of an error")
})

test_that("test_remote treats empty output as failure", {
  host <- list(name = "somehost")
  mockery::stub(test_remote, "remote.execute.cmd", character(0))
  mockery::stub(test_remote, "PEcAn.logger::logger.error", NULL)
  expect_false(test_remote(host, stderr = FALSE))
})
