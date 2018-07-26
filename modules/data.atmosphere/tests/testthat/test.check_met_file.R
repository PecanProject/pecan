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
      regexp = "nc, \"time\", \"units\".* does not match"
    )
    urbana_daily_results <- check_met_input_file(urbana_daily_met, throw_error = FALSE)
    expect_s3_class(urbana_daily_results, "data.frame")
    expect_true(
      all(urbana_daily_results %>%
            dplyr::filter(test_type == "var format and units") %>%
            dplyr::pull(test_passed))
    )
    expect_false(
      all(urbana_daily_results %>%
            dplyr::filter(target_variable %in% c("dimensions", "air_pressure", "eastward_wind")) %>%
            dplyr::pull(test_passed))
    )
    urbana_subdaily_met <- system.file(
      "tests/testthat/data/urbana_subdaily_test.nc",
      package = "PEcAn.data.atmosphere"
    )
    nn <- ncdf4::nc_open(urbana_subdaily_met)
    expect_error(
      check_met_input_file(urbana_subdaily_met),
      regexp = "length\\(dimensions\\) not equal to 3"
    )
  }
)

test_that(
  "Check that lower-level checking functions detect errors",
  {
    nc_vars <- c("a", "b", "c")
    expect_true(
      check_has_required_variable(nc_vars, "b")
    )
    expect_s3_class(
      check_has_required_variable(nc_vars, "d"),
      "error"
    )
  }
)
