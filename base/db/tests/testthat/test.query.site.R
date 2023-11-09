test_that("`query.site()` correctly forms the query and returns the site", {
  mock_site_data <- data.frame(id = c(1), lon = c(1), lat = c(1))
  mocked_function <- mockery::mock(mock_site_data)
  mockery::stub(query.site, 'db.query', mocked_function)
  site <- query.site(1, con = 1)
  expect_equal(site, mock_site_data)
  args <- mockery::mock_args(mocked_function)
  expect_true(
    grepl(
      "WHERE id = 1", 
      args[[1]]$query
    )
  )
})