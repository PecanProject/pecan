
PEcAn.logger::logger.setQuitOnSevere(FALSE)
on.exit(PEcAn.logger::logger.setQuitOnSevere(TRUE), add = TRUE)


test_that("`check.bety.version`` gives errors for missing significant versions", {
  dbcon <- 1
  mockery::stub(
    check.bety.version, 
    "PEcAn.DB::db.query", 
    list(version = c("2"))
  )
  expect_error(
    check.bety.version(dbcon), 
    "No version 1, how did this database get created?"
  )

  mockery::stub(
    check.bety.version, 
    "PEcAn.DB::db.query", 
    list(version = c("1"))
  )
  expect_error(
    check.bety.version(dbcon), 
    "Missing migration 20140617163304, this associates files with models."
  )
  
  mockery::stub(
    check.bety.version, 
    "PEcAn.DB::db.query", list(version = c("1","20140617163304"))
  )
  expect_error(
    check.bety.version(dbcon),
    "Missing migration 20140708232320, this introduces geometry column in sites"
  )
  
  mockery::stub(
    check.bety.version, 
    "PEcAn.DB::db.query", 
    list(version = c("1","20140617163304","20140708232320"))
  )
  expect_error(
    check.bety.version(dbcon),
    "Missing migration 20140729045640, this introduces modeltypes table"
  )

  mockery::stub(
    check.bety.version, 
    "PEcAn.DB::db.query", 
    list(version = c("1","20140617163304","20140708232320","20140729045640"))
  )
  expect_error(
    check.bety.version(dbcon),
    "Missing migration 20151011190026, this introduces notes and user_id in workflows"
  )

  mockery::stub(
    check.bety.version, 
    "PEcAn.DB::db.query", 
    list(version = c("1","20140617163304","20140708232320","20140729045640","20151011190026"))
  )
  expect_silent(check.bety.version(dbcon))
})


test_that("`check.bety.version` warns, without error, about migrations unknown to this release", {
  known <- c("1", "20140617163304", "20140708232320", "20140729045640", "20151011190026")

  warn_mock <- mockery::mock(NULL)
  mockery::stub(
    check.bety.version,
    "PEcAn.DB::db.query",
    list(version = c(known, "99999999999999"))
  )
  mockery::stub(check.bety.version, "PEcAn.logger::logger.warn", warn_mock)
  expect_no_error(check.bety.version(1))
  mockery::expect_called(warn_mock, 1)
  expect_match(
    paste(mockery::mock_args(warn_mock)[[1]], collapse = " "),
    "99999999999999"
  )
})

test_that("`check.bety.version` does not warn when all migrations are known", {
  known <- c("1", "20140617163304", "20140708232320", "20140729045640", "20151011190026")

  warn_mock <- mockery::mock(NULL)
  mockery::stub(check.bety.version, "PEcAn.DB::db.query", list(version = known))
  mockery::stub(check.bety.version, "PEcAn.logger::logger.warn", warn_mock)
  check.bety.version(1)
  mockery::expect_called(warn_mock, 0)
})
