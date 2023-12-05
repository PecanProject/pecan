test_that("`clear.scratch()` able to build the correct system command prompt to remove previous model run output", {
  mocked_res <- mockery::mock(TRUE)
  mockery::stub(clear.scratch, 'system', mocked_res)
  mockery::stub(clear.scratch, 'seq', 0)
  settings <- list(host = list(name = "cluster"))
  expect_output(
    clear.scratch(settings),
    ".*Removing.*all.q@compute-0-0.local"
  )
  args <- mockery::mock_args(mocked_res)
  expect_true(
    grepl(
      "ssh -T cluster qlogin -q all.q@compute-0-0.local.*clear.scratch.sh", 
      args[[1]][[1]]
    )
  )

  # host name not cluster
  settings <- list(host = list(name = "test"))
  expect_output(
    clear.scratch(settings),
    ".*No output to delete.*"
  )
})