test_that("`query.priors()` correctly forms the query based on the parameters passed and returns priors",{
  mocked_function <- mockery::mock(data.frame(name = c("A", "B"), value = c(0.1, 0.2)))
  mockery::stub(query.priors, 'db.query', mocked_function)
  priors <- query.priors("ebifarm.pavi", c("SLA"), con = 1)
  expect_equal(priors, c(0.1, 0.2))
  args <- mockery::mock_args(mocked_function)
  expect_true(
    grepl(
      "WHERE pfts.id =  ebifarm.pavi AND variables.name IN .* SLA", 
      args[[1]]$query
    )
  )
})