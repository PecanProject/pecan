test_that("`check.database` able to set the database object with defaults correctly if nothing specified", {
  rdriver <- paste0("R", "PostgreSQL") 
  withr::with_package(rdriver, {
    mockery::stub(check.database, 'PEcAn.DB::db.exists', TRUE)
    database <- list()
    updated_database <- check.database(database)
    expect_equal(updated_database$driver, "PostgreSQL")
    expect_equal(updated_database$host, "localhost")
    expect_equal(updated_database$user, "bety")
    expect_equal(updated_database$password, "bety")
    expect_equal(updated_database$dbname, "bety")
  })
})

test_that("`check.database.settings` able to set bety parameters correctly if they are not specified", {
  mockery::stub(check.database.settings, 'PEcAn.DB::db.exists', TRUE)
  mockery::stub(
    check.database.settings, 
    'check.database', 
    list(
      driver = "PostgreSQL",
      host = "localhost",
      user = "bety",
      password = "bety",
      dbname = "bety"
    )
  )
  mockery::stub(check.database.settings, 'PEcAn.DB::db.open', TRUE)
  mockery::stub(check.database.settings, 'PEcAn.DB::db.close', TRUE)
  mockery::stub(check.database.settings, 'check.bety.version', TRUE)

  settings <- list(
    database = list(
      bety = list()
    )
  )

  checked_settings <- check.database.settings(settings)
  expect_equal(checked_settings$database$bety$driver, "PostgreSQL")
  expect_equal(checked_settings$database$bety$host, "localhost")
  expect_equal(checked_settings$database$bety$user, "bety")
  expect_equal(checked_settings$database$bety$password, "bety")
  expect_equal(checked_settings$database$bety$dbname, "bety")
  expect_equal(checked_settings$database$bety$write, TRUE)
})

test_that("`check.ensemble.settings` throws an error if not variables specified to compute ensemble", {
  settings <- list(
    ensemble = list()
  )
  expect_error(
    check.ensemble.settings(settings),
    "No variable specified to compute ensemble for."
  )
})

test_that("`check.ensemble.settings` able to update ensemble settings when variables, size, start and end year and sampling space not specified", {
  settings <- list(
    ensemble = list(),
    sensitivity.analysis = list(
        variable = "GPP"
    ),
    run = list(
      start.date = "2000-01-01",
      end.date = "2003-12-31"
    )
  )
  settings <- check.ensemble.settings(settings)
  expect_equal(settings$ensemble$variable, "GPP")
  expect_equal(settings$ensemble$size, 1)
  expect_equal(settings$ensemble$start.year, 2000)
  expect_equal(settings$ensemble$end.year, 2003)
  expect_equal(settings$ensemble$samplingspace$parameters$method, "uniform")
  expect_equal(settings$ensemble$samplingspace$met$method, "sampling")
})