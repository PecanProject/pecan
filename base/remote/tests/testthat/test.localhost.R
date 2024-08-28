test_that('is.localhost works', {
  expect_true(is.localhost("localhost"))
  expect_true(is.localhost(fqdn()))
  expect_true(is.localhost(list(name = fqdn())))
  expect_false(is.localhost("notarealmachine"))
})
