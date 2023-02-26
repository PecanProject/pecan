
host <- list(name = "localhost")
echo_string <- "pecan"
out <- remote.execute.cmd(host = host, cmd = "echo", args = echo_string)

test_that("Basic remote execution works as expected", {
  expect_identical(out, echo_string)
})

test_that("`remote.execute.cmd()` works correctly for incomplete inputs", {
  expect_error(remote.execute.cmd(NULL, "echo", ""), "`host` cannot be `NULL` for remote execution")
  expect_error(remote.execute.cmd(list(name = "geo.bu.edu", tunnel = "path/to/non/existent/tunnel"), "echo", ""), "Could not find tunnel")
  expect_error(remote.execute.cmd("", "echo", ""), "`name`parameter in `host` object cannot be `NULL` or empty for remote execution")
})