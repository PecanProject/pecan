test_that("`get_args` throws an error with missing settings file", {
  withr::with_envvar(c(PECAN_SETTINGS = "doesnotexists.xml"), {
    expect_error(
      get_args(), 
      "--settings \"doesnotexists.xml\" not a valid file"
    )
  })
})

test_that("`get_args` works for existing settings file", {
  withr::with_envvar(c(PECAN_SETTINGS = "pecan.xml"), {
    mockery::stub(get_args, 'file.exists', TRUE)
    args <- get_args()
    expect_equal(args$settings, "pecan.xml")
    expect_equal(args$continue, FALSE)
    expect_equal(args$help, FALSE)
  })
})