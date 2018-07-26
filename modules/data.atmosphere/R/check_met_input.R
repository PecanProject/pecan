#' Check a meteorology data file for compliance with the PEcAn standard
#'
#' @param metfile Path of met file to check, as a scalar character.
#' @param throw_error Logical. If `TRUE`, throw an informative error
#'   if any tests fail. If `FALSE`,
#' @param variable_table `data.frame` linking standard names to their
#'   units. Defaults to [pecan_standard_met_table].
#' @return Tidy `data.frame` summarizing the results of the tests.
#' @author Alexey Shiklomanov
#' @export
check_met_input_file <- function(metfile,
                                 throw_error = TRUE,
                                 variable_table = pecan_standard_met_table,
                                 required_vars = pecan_standard_met_table %>%
                                   dplyr::filter(is_required) %>%
                                   dplyr::pull(cf_standard_name)
                                 ) {

  metfile <- normalizePath(metfile, mustWork = FALSE)

  PEcAn.logger::severeifnot(
    file.exists(metfile),
    msg = glue::glue("File '{metfile}' does not exist.")
  )

  nc <- ncdf4::nc_open(metfile)

  test_dims <- tryCatch(testthat::test_that(
    "Dimensions are correct.",
    {
      dimensions <- nc[["dim"]]
      testthat::expect_type(dimensions, "list")
      testthat::expect_equal(length(dimensions), 3)
      testthat::expect_true("time" %in% names(dimensions))
      time_regex <- paste0(
        "^(seconds|minutes|hours|hours|days) since ",
        "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}",
        "T[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}Z$"
      )
      testthat::expect_match(
        ncdf4::ncatt_get(nc, "time", "units")[["value"]],
        time_regex
      )
      testthat::expect_true("latitude" %in% names(dimensions))
      testthat::expect_equal(
        ncdf4::ncatt_get(nc, "latitude", "units")[["value"]],
        "degrees_north"
      )
      testthat::expect_true("longitude" %in% names(dimensions))
      testthat::expect_equal(
        ncdf4::ncatt_get(nc, "longitude", "units")[["value"]],
        "degrees_east"
      )
    }
  ), error = function(e) conditionMessage(e)
  )

  test_dims_summary <- summarize_error_list(list(test_dims), "dimensions", "correct dimensions")

  nc_vars <- names(nc[["var"]])

  test_has_vars_raw <- purrr::map(
    required_vars,
    check_has_required_variable,
    nc_vars = nc_vars
  )
  test_has_vars <- process_error_list(test_has_vars_raw, required_vars)
  test_has_vars_summary <- summarize_error_list(test_has_vars_raw, required_vars, "has required variable")

  test_vars_raw <- purrr::map(
    nc_vars,
    check_met_variable,
    nc = nc
  )
  test_vars <- process_error_list(test_vars_raw, nc_vars)
  test_vars_summary <- summarize_error_list(test_vars_raw, nc_vars, "var format and units")

  all_errors <- c(list(dimensions = test_dims), test_has_vars, test_vars)

  test_summary <- tibble::tibble(
    target_variable = c(NA, required_vars, nc_vars),
    test_type = c(
      "dimensions",
      rep("required variables present", length(required_vars)),
      rep("correct units", length(nc_vars))
    )
  )

  if (length(all_errors) > 0 && throw_error) {
    error_string <- paste(unlist(all_errors), collapse = "\n\n")
    PEcAn.logger::logger.severe(
      "\nThe following errors were detected:\n\n",
      error_string,
      wrap = FALSE
    )
  }

  results_df <- dplyr::bind_rows(
    test_dims_summary,
    test_has_vars_summary,
    test_vars_summary
  )

  return(results_df)
}

#' Clean up list output of tests mapped across a vector of variables
#'
#' @param error_list List resulting from calling `purrr::map(...,
#'   purrr::safely(some_test))
#' @param var_names Character vector to use to name errors in output
#'   list.
#' @return List of error messages, named according to the variable
#'   that threw the error. If no error messages were thrown, an empty
#'   list.
#' @author Alexey Shiklomanov
process_error_list <- function(error_list, var_names) {
  tests_raw <- error_list %>%
    setNames(var_names) %>%
    purrr::discard(isTRUE)
  tests_which_errors <- !purrr::map_lgl(tests_raw, is.null)
  test_errors <- tests_raw[tests_which_errors] %>%
    purrr::map(conditionMessage)
  test_errors
}

#' Convert an error list into a tidy `data.frame`
#'
#' @param error_list List of `testthat` results
#' @param var_names Vector of variable names corresponding to `error_list`.
#' @param test_type Type of test, as a character
#' @return Tidy `data.frame` summarizing the error output
#' @author Alexey Shiklomanov
summarize_error_list <- function(error_list, var_names, test_type) {
  tibble::tibble(
    target_variable = var_names,
    test_type = test_type,
    test_passed = purrr::map_lgl(error_list, isTRUE),
    test_error_message = purrr::map_chr(error_list, purrr::possibly(conditionMessage, NA))
  )
}

#' Check a met variable for PEcAn standard compliance
#'
#' Checks that the variable has a correct CF standard name, and the
#' correct units.
#' @param nc NetCDF object (as returned by [ncdf4::nc_open]).
#' @param variable Variable to check, as a character scalar.
#' @param warn_unknown Logical. If `TRUE`, warn about unknown
#'   variables.
#' @return `TRUE` (invisibly) if successful, otherwise throw an informative error.
#' @author Alexey Shiklomanov
#' @inheritParams check_met_input_file
check_met_variable <- function(nc, variable, variable_table = pecan_standard_met_table, warn_unknown = TRUE) {
  if (!(variable %in% variable_table[["cf_standard_name"]]) && warn_unknown) {
    PEcAn.logger::logger.warn(glue::glue("Variable '{variable}' not in known variables"))
    return(TRUE)
  }
  tryCatch(
    testthat::test_that(
      glue::glue("Variable '{variable}' has correct units."),
      {
        var_correct_unit <- variable_table %>%
          dplyr::filter(cf_standard_name == variable) %>%
          dplyr::pull(units)
        ncvar_unit <- ncdf4::ncatt_get(nc, variable, "units")[["value"]]
        testthat::expect_true(
          units_are_equivalent(ncvar_unit, var_correct_unit),
          glue::glue("NetCDF unit '{ncvar_unit}' not equivalent to expected unit '{var_correct_unit}'.")
        )
      }
    ), error = function(e) e
  )
}
#' Check that a required variable is present in the given met input
#' NetCDF
#'
#' @param nc_vars Character vector of NetCDF variable names to check
#' @param required_var Character vector if required variables.
#' @return Error message if absent, or `TRUE` if present.
#' @author Alexey Shiklomanov
check_has_required_variable <- function(nc_vars, required_var) {
  tryCatch(testthat::test_that(
    glue::glue("Required variable '{required_var}' is present."),
    testthat::expect_true(required_var %in% nc_vars)
  ), error = function(e) e)
}
#' Check if two unit strings are equivalent
#'
#' This is to allow multiple forms of the same unit to work, such as
#' `m/s` vs. `m s-1` or `K` and `Kelvin`.
#' @param x A unit string, as character
#' @param y Another unit string for comparison, as character
#' @return `TRUE` if equivalent, `FALSE` otherwise
#' @author Alexey Shiklomanov
units_are_equivalent <- function(x, y) {
  x2y <- udunits2::ud.convert(1, x, y)
  1 == x2y
}
