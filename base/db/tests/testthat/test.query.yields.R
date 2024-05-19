test_that("`query.yields()` able to form the query correctly for trait set to 'yield' and with no extra columns", {
  mocked_function <- mockery::mock(data.frame(Y=rep(1,5), stat=rep(1,5), n=rep(4,5), mean = rep(3,5), statname=c('SD', 'MSE', 'LSD', 'HSD', 'MSD')))
  mockery::stub(query.yields, 'db.query', mocked_function, 2)
  result <- query.yields(spstr = "test_spstr", con = 1)

  args <- mockery::mock_args(mocked_function)
  expect_true(
    grepl(
      paste0(
        "month\\(yields.date\\) as month,treatments.control.*",
        "where specie_id in \\(test_spstr\\);"
      ), 
      args[[1]]$query
    )
  )
})

test_that("`query.yields()` throws an error if extra columns is not a string", {
  expect_error(
    query.yields(spstr = "test_spstr", con = 1, extra.columns = 1),
    "`extra.columns` must be a string"
  )
  expect_error(
    query.yields(spstr = "test_spstr", con = 1, extra.columns = c("a","b")),
    "`extra.columns` must be a string"
  )
})

test_that("`query.yields()` able to form the query correctly for trait not equal to 'yield' and with extra columns",{
  mocked_function <- mockery::mock(data.frame(Y=rep(1,5), stat=rep(1,5), n=rep(4,5), mean = rep(3,5), statname=c('SD', 'MSE', 'LSD', 'HSD', 'MSD')))
  mockery::stub(query.yields, 'db.query', mocked_function, 2)
  result <- query.yields(trait = 'test_trait', spstr = "test_spstr", extra.columns = 'test_col', con = 1)
  args <- mockery::mock_args(mocked_function)
  expect_true(
    grepl(
      paste0(
        "month\\(yields.date\\) as month,test_col,treatments.control.*",
        "where specie_id in \\(test_spstr\\) and variables.name in \\('test_trait'\\)"
      ),
      args[[1]]$query
    )
  )
})