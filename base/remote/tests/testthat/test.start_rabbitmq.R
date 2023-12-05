test_that("`start_rabbitmq()` able to correctly read the environment varibles and send desired values to rabbitmq_post_message", {
  withr::with_envvar(c("RABBITMQ_PREFIX" = "prefix", "RABBITMQ_PORT" = "3000"),{
    mocked_res <- mockery::mock(TRUE)
    mockery::stub(start_rabbitmq, 'rabbitmq_post_message', mocked_res)
    res <- start_rabbitmq('test_folder', 'test_uri', 'test_queue')
    args <- mockery::mock_args(mocked_res)
    expect_equal(args[[1]][[1]], 'test_uri')
    expect_equal(args[[1]][[2]], 'test_queue')
    expect_equal(args[[1]][[3]], list(folder = 'test_folder'))
    expect_equal(args[[1]][[4]], 'prefix')
    expect_equal(args[[1]][[5]], '3000')
  })
})