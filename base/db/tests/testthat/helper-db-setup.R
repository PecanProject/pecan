get_db_params <- function() {
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
  # OR by setting Postgres environment parameters in your shell:
  # ```
  # export PGHOST=localhost
  # export PGUSER=bety
  # [etc]
  # ```
  option_params <- getOption("pecan.db.params")
  # Check if running on continuous integration (CI)
  # If yes, skip this test
  is_ci <- Sys.getenv("CI") != ""
  if (!is.null(option_params)) {
    return(option_params)
  } else if (is_ci) {
    return(get_postgres_envvars(
      host = "localhost",
      user = "bety",
      password = "bety",
      driver = "Postgres"))
  } else {
    if (PEcAn.remote::fqdn() == "pecan2.bu.edu") {
      return(list(host = "psql-pecan.bu.edu", driver = "PostgreSQL",
                  dbname = "bety", user = "bety", password = "bety"))
    } else {
      return(get_postgres_envvars(
        host = "localhost",
        driver = "PostgreSQL",
        user = "bety",
        dbname = "bety",
        password = "bety"))
    }
  }
}

check_db_test <- function() {
  con <- tryCatch(
    db.open(params = get_db_params()),
    error = function(e) {
      message("Failed to open connection with the following error:\n",
              conditionMessage(e))
      return(NULL)
    })

  if (is.null(con)) {
    testthat::skip("Can't get a valid test connection right now. Skipping test. ")
  } else {
    return(con)
  }
}
