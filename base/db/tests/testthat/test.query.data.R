test_that("`query.data()` able to correctly form the query and return result in SE", {
  mocked_function <- mockery::mock(data.frame(Y=rep(1,5), stat=rep(1,5), n=rep(4,5), mean = rep(3,5), statname=c('SD', 'MSE', 'LSD', 'HSD', 'MSD')))
  mockery::stub(query.data, 'db.query', mocked_function, 2)
  result <- query.data(con = 1, trait = "test_trait", spstr = "test_spstr", store.unconverted = TRUE)
  args <- mockery::mock_args(mocked_function)
  expect_true(
    grepl(
      paste(
        "ST_X\\(ST_CENTROID\\(sites\\.geometry\\)\\) AS lon,",
        "ST_Y\\(ST_CENTROID\\(sites\\.geometry\\)\\) AS lat,.*",
        "where specie_id in \\(test_spstr\\).*",
        "variables.name in \\('test_trait'\\);"
      ), 
      args[[1]]$query
    )
  )
  expect_equal(result$mean_unconverted, result$mean)
  expect_equal(result$stat_unconverted, result$stat)
  expect_equal(result$statname, rep('SE', 5))
})