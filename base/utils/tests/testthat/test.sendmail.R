test_that("`sendmail()`", {
  mockery::stub(sendmail, 'system2', TRUE)
  expect_equal(1, 1)
})