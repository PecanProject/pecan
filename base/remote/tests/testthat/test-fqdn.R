test_that("fqdn() returns exactly one result", {
  expect_length(fqdn(), 1)
})

test_that("`fqdn()` returns expected `FQDN` value", {
  
  #set FQDN
  Sys.setenv(FQDN = "pecan_host")
  expect_equal(fqdn(), "pecan_host")
  
  #unset FQDN
  Sys.unsetenv("FQDN")
  expect_equal(fqdn(), as.character(Sys.info()["nodename"]))
})