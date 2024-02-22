# test_that("`db.print.connections()` able to log out details about connections", {
#   PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
#   on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)
#   expect_output(
#     db.print.connections(), 
#     paste0(
#       ".* Created 0 connections and executed 0 queries .* ",
#       "Created 0 connections and executed 0 queries.*",
#       "No open database connections."
#     )
#   )
# })

test_that("`db.showQueries()` and `db.getShowQueries()` able to set and get the value of the .db.utils$showquery variable respectively", {
  showquery_old <- db.getShowQueries()
  on.exit(db.showQueries(showquery_old))
  db.showQueries(TRUE)
  expect_equal(db.getShowQueries(), TRUE)
})

test_that("`default_hostname()` fixes hostname if the host is localhost", {
  expect_equal(default_hostname("localhost"), PEcAn.remote::fqdn())

  # if not localhost
  expect_equal(default_hostname("pecan"), "pecan")
})