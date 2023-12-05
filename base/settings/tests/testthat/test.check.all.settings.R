test_that("`check.inputs()` able to set dbfile path for inputs", {
  mockery::stub(check.inputs, 'PEcAn.DB::db.open', TRUE)
  mockery::stub(check.inputs, 'PEcAn.DB::db.close', TRUE)
  mockery::stub(check.inputs, 'PEcAn.DB::dbfile.file', "test/path/to/file")

  mocked_query_res = mockery::mock(
    data.frame(
      tag = "test",
      format_id = 1,
      required = TRUE
    ),
    data.frame(
      format_id = 1
    )
  )
  mockery::stub(check.inputs, 'PEcAn.DB::db.query', mocked_query_res)

  settings <- list(
    database = list(
      bety = list()
    ),
    run = list(
      inputs = list(
        test = list(
          id = 1
        )
      )
    ),
    model = list(
      type = "ed"
    )
  )
  
  updated_settings <- check.inputs(settings)
  expect_equal(updated_settings$run$inputs$test$path, "test/path/to/file")
})

test_that("`check.run.settings` throws error if start date greater than end date in run settings", {
  settings <- list(
    run = list(
      start.date = "2010-01-01",
      end.date = "2009-01-01"
    )
  )
  expect_error(
    check.run.settings(settings),
    "Start date should come before the end date."
  )
})

test_that("`check.run.settings` able to set sensitivity analysis parameters based on ensemble and run params", {
  settings <- list(
    sensitivity.analysis = list(),
    ensemble = list(
      variable = "GPP"
    ),
    run = list(
      start.date = "2010-01-01",
      end.date = "2015-01-01"
    )
  )
  updated_settings <- check.run.settings(settings)
  expect_equal(updated_settings$sensitivity.analysis$variable, "GPP")
  expect_equal(updated_settings$sensitivity.analysis$start.year, 2010)
  expect_equal(updated_settings$sensitivity.analysis$end.year, 2015)
})

test_that("`check.run.settings` able to update run site parameters based on site id passed", {
  mockery::stub(
    check.run.settings, 
    'PEcAn.DB::db.query', 
    data.frame(
      sitename = "US-1",
      lat = 45,
      lon = -90
    )
  )
  settings <- list(
    run = list(
      site = list(
        id = 5
      )
    )
  )
  updated_settings <- check.run.settings(settings, 1)
  expect_equal(updated_settings$run$site$id, 5)
  expect_equal(updated_settings$run$site$name, "US-1")
  expect_equal(updated_settings$run$site$lat, 45.0)
  expect_equal(updated_settings$run$site$lon, -90.0)
})

test_that("`check.model.settings` able to update model parameters based on passed model id in settings", {
  mockery::stub(
    check.model.settings, 
    'PEcAn.DB::db.query', 
    data.frame(
      id = 7,
      revision = 82,
      name = "ED2",
      type = "ed"
    )
  )
  mockery::stub(
    check.model.settings, 
    'PEcAn.DB::dbfile.file', 
    "/usr/local/bin/ed2.r82"
  )
  settings <- list(
    model = list(
      id = 7
    )
  )  

  updated_settings <- check.model.settings(settings, 1)
  expect_equal(updated_settings$model$id, 7)
  expect_equal(updated_settings$model$revision, 82)
  expect_equal(updated_settings$model$type, "ed")
  expect_equal(updated_settings$model$delete.raw, FALSE)
  expect_equal(updated_settings$model$binary, "/usr/local/bin/ed2.r82")
})

test_that("`check.model.settings` able to update model parameters based on passed model type in settings", {
  mockery::stub(
    check.model.settings, 
    'PEcAn.DB::db.query', 
    data.frame(
      id = 7,
      revision = 82,
      name = "ED2",
      type = "ed"
    )
  )
  mockery::stub(
    check.model.settings, 
    'PEcAn.DB::dbfile.file', 
    "/usr/local/bin/ed2.r82"
  )
  settings <- list(
    model = list(
      type = "ed"
    )
  )  

  updated_settings <- check.model.settings(settings, 1)
  expect_equal(updated_settings$model$id, 7)
  expect_equal(updated_settings$model$revision, 82)
  expect_equal(updated_settings$model$type, "ed")
  expect_equal(updated_settings$model$delete.raw, FALSE)
  expect_equal(updated_settings$model$binary, "/usr/local/bin/ed2.r82")
})

test_that("`check.workflow.settings` able to set workflow defaults in case they are not specified", {
  mockery::stub(check.workflow.settings, 'PEcAn.DB::db.query', list(id = 100))
  mockery::stub(check.workflow.settings, 'file.exists', TRUE)

  settings <- list(
    database = list(
      bety = list(
        write = TRUE
      )
    ),
    model = list(
      id = 1
    )
  )
  updated_settings <- check.workflow.settings(settings, 1)
  expect_equal(updated_settings$workflow$id, 100)
  expect_equal(updated_settings$outdir, file.path(getwd(), "PEcAn_100"))
})

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