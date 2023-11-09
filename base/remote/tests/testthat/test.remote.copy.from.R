test_that("`remote.copy.from()` constructs the correct system command to be executed for doing the copy", {
  mocked_res <- mockery::mock(0)
  mockery::stub(remote.copy.from, 'system2', mocked_res)
  mockery::stub(remote.copy.from, 'file.exists', TRUE)
  remote.copy.from(host = data.frame(name = 'pecan', tunnel = 'test_tunnel'), src = 'tmp/', dst = 'tmp/', delete = TRUE)
  args <- mockery::mock_args(mocked_res)
  expect_equal(args[[1]][[1]], 'rsync')
  expect_equal(
    args[[1]][[2]], 
    shQuote(c("-az", "-q", "--delete", "-e", "ssh -o ControlPath=\"test_tunnel\"", "pecan:tmp/", "tmp/"))
  )
})