test_that("fqdn() returns exactly one result", {
  expect_length(fqdn(), 1)
})

test_that("`fqdn()` returns expected `FQDN` value", {
  
  withr::with_envvar(c(FQDN = "pecan_host"), {
    expect_equal(fqdn(), "pecan_host")
  })
  withr::with_envvar(c(FQDN = ""), {
    expect_equal(fqdn(), as.character(Sys.info()["nodename"]))
  })
})