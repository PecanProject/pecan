context("Check met input file correctly detects errors")

test_that(
  "Check met input correctly finds errors in bad met files",
  {
    urbana_daily_met <- system.file(
      "tests/testthat/data/urbana_daily_test.nc",
      package = "PEcAn.data.atmosphere"
    )
    urbana_daily_results <- check_met_input_file(urbana_daily_met)
    expect_s3_class(urbana_daily_results, "data.frame")
    expect_true(
      all(c("correct dimensions", "required variable present", "variable has correct units") %in%
            urbana_daily_results[["test_type"]])
    )
    expect_true(
      all(urbana_daily_results %>%
            dplyr::filter(test_type == "variable has correct units") %>%
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
    urbana_subdaily_results <- check_met_input_file(urbana_subdaily_met)
    urbana_subdaily_dims <- urbana_subdaily_results %>%
      dplyr::filter(target_variable == "dimensions")
    expect_false(urbana_subdaily_dims[["test_passed"]])
    expect_match(
      urbana_subdaily_dims[["test_error_message"]],
      regexp = "length\\(dimensions\\) not equal to 3"
    )
  }
)
