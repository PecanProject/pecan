PEcAn.logger::logger.setQuitOnSevere(FALSE)

test_that("`query.format.vars` stops when no format is found for a format id", {
  mockery::stub(query.format.vars, "PEcAn.DB::db.query", \(...) data.frame())
  expect_error(
    query.format.vars(bety = NULL, format.id = 0),
    "No format found"
  )
})

test_that("`query.format.vars` stops when no format is found for an input id", {
  mockery::stub(query.format.vars, "PEcAn.DB::db.query", \(...) data.frame())
  expect_error(
    query.format.vars(bety = NULL, input.id = 0),
    "No format found"
  )
})
