# Check if running on continuous integration
# If yes, skip this test
check_db_test <- function() {
  # This provides a way to run these tests with a custom BETY setup.
  # Set these options by adding something like the following to
  # `~/.Rprofile`:
  # ```r
  # options(pecan.db.params = list(driver = "PostgreSQL",
  #                                dbname = "bety",
  #                                user = "bety",
  #                                password = "bety",
  #                                host = "localhost",
  #                                port = 5432))
  # ```
  option_params <- getOption("pecan.db.params")
  is_ci <- Sys.getenv('CI') != ''
  con <- NULL
  if (!is.null(option_params)) {
    con <- db.open(params = option_params)
  } else if (is_ci) {
    # Do this on Travis
    con <- tryCatch(db.open(params = list(
      host = "localhost",
      user = "bety",
      password = "bety",
    )), error = function(e) NULL
    )
  } else {
    try({
      if(PEcAn.remote::fqdn() == "pecan2.bu.edu") {
        con <- db.open(list(host = "psql-pecan.bu.edu", driver = "PostgreSQL",
                            user = "bety", dbname = "bety", password = "bety"))
      } else {
        con <- db.open(list(host = "localhost", driver = "PostgreSQL",
                            user = "bety", dbname = "bety", password = "bety"))
      }
    }, silent = TRUE)
  }

  if (is.null(con)) {
    testthat::skip("Can't get a valid test connection right now. Skipping test. ")
  } else {
    return(con)
  }
}
