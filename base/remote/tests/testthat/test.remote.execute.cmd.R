test_that("`remote.execute.cmd()` executes command on localhost", {
  # Defining the host
  host <- list(
    name = "localhost",
    user = NULL,
    tunnel = NULL
  )
  
  # checking the output
  output <- remote.execute.cmd(host, "echo", "pecan")
  expect_equal(output, "pecan")
})
