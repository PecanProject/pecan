test_that("query.covariates returns expected data.frame";{
  ids <- 1:10
  test.query <- query.covariates(ids, con = newcon())
  expect_equal(colnames(test.query), c("trait_id", "level", "name"))
  expect_true(nrow(test.query) >= length(ids))
})

test_that("append.covariates appends managements to yields";{
  test.yields <- query.bety("select * from yields where id < 100;", con = newcon())
  covariates <- query.covariates(ids, con = newcon())
  
})

