test_that("remote.execute.cmd executes command on localhost without ssh", {
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
