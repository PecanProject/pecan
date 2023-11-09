
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

