context("Check met input file correctly detects errors")

test_that(
  "Check met input correctly finds errors in bad met files",
  {
    urbana_daily_met <- system.file(
      "tests/testthat/data/urbana_daily_test.nc",
      package = "PEcAn.data.atmosphere"
    )
    expect_error(
      check_met_input_file(urbana_daily_met),
      regexp = "At least one error was found\\."
    )
    urbana_subdaily_met <- system.file(
      "tests/testthat/data/urbana_subdaily_test.nc",
      package = "PEcAn.data.atmosphere"
    )
    expect_error(
      check_met_input_file(urbana_subdaily_met),
      regexp = "At least one error was found\\."
    )
  }
)
