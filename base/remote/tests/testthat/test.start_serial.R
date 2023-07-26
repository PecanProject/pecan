test_that("`start_serial()` able to pass desired parameters to execute command remotely to start model execution in serial mode",{
  mocked_res <- mockery::mock(TRUE)
  mockery::stub(start_serial, 'remote.execute.cmd', mocked_res)
  res <- start_serial('test_run', 'pecan', 'test_rundir', 'test_host_rundir', 'test_job_script')
  args <- mockery::mock_args(mocked_res)
  expect_equal(args[[1]][[1]], 'pecan')
  expect_equal(args[[1]][[2]], 'test_host_rundir/test_run/test_job_script')
  expect_equal(res, TRUE)
})