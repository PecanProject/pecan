test_that("`met_inputs()` able to correctly place input parameters in the database query to retrieve available met inputs", {
  mocked_res <- mockery::mock(0)
  mockery::stub(met_inputs, 'db.query', mocked_res)
  met_inputs(dbcon = NULL, site_id = 100, model_id = 200, hostname = "pecan")
  args <- mockery::mock_args(mocked_res)

  expect_true(
    grepl("inputs.site_id = \\$1.*machines.hostname = \\$2.*models.id = \\$3", args[[1]][[1]])
  )
})