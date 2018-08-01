#' Check a meteorology data file for compliance with the PEcAn standard
#'
#' @param metfile Path of met file to check, as a scalar character.
#' @param variable_table `data.frame` linking standard names to their
#'   units. Must contain columns "cf_standard_name" and "units".
#'   Default is [pecan_standard_met_table].
#' @param required_vars Character vector of required variables.
#'   Defaults to variables marked as required in `variable_table`.
#' @param warn_unknown Logical. If `TRUE` (default), throw a warning
#'   for variables not in `variable_table`. Otherwise, ignore unknown
#'   variables.
#' @return `data.frame` summarizing the results of the tests.
#' @author Alexey Shiklomanov
#' @export
check_met_input_file <- function(metfile,
                                 variable_table = pecan_standard_met_table,
                                 required_vars = variable_table %>%
                                   dplyr::filter(is_required) %>%
                                   dplyr::pull(cf_standard_name),
                                 warn_unknown = TRUE
                                 ) {

  metfile <- normalizePath(metfile, mustWork = FALSE)

  PEcAn.logger::severeifnot(
    file.exists(metfile),
    msg = glue::glue("File '{metfile}' does not exist.")
  )

  nc <- ncdf4::nc_open(metfile)

  dimensions <- nc[["dim"]]
  time_regex <- paste0(
    "^(seconds|minutes|hours|hours|days) since ",
    "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}",
    "T[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}Z$"
  )

  try2 <- purrr::partial(try, silent = TRUE)

  dimensions <- nc[["dim"]]
  time_regex <- paste0(
    "^(seconds|minutes|hours|hours|days) since ",
    "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}",
    "T[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}Z$"
  )
  test_dims <- list(
    try2(testthat::expect_type(dimensions, "list")),
    try2(testthat::expect_equal(length(dimensions), 3)),
    try2(testthat::expect_true("time" %in% names(dimensions))),
    try2(testthat::expect_match(
      ncdf4::ncatt_get(nc, "time", "units")[["value"]],
      time_regex
    )),
    try2(testthat::expect_true("latitude" %in% names(dimensions))),
    try2(testthat::expect_equal(
      ncdf4::ncatt_get(nc, "latitude", "units")[["value"]],
      "degrees_north"
    )),
    try2(testthat::expect_true("longitude" %in% names(dimensions))),
    try2(testthat::expect_equal(
      ncdf4::ncatt_get(nc, "longitude", "units")[["value"]],
      "degrees_east"
    ))
  )

  dim_errors_lgl <- purrr::map_lgl(test_dims, inherits, "try-error")
  dim_errors <- test_dims[dim_errors_lgl] %>%
    purrr::map_chr(as.character) %>%
    paste(collapse = "\n\n")

  test_dims_summary <- tibble::tibble(
    target_variable = "dimensions",
    test_type = "correct dimensions",
    test_passed = all(!dim_errors_lgl),
    test_error_message = dim_errors
  )

  nc_vars <- names(nc[["var"]])
  test_required_vars <- tibble::tibble(
    test_type = "required variable present",
    target_variable = required_vars,
    test_passed = required_vars %in% nc_vars,
    test_error_message = dplyr::if_else(
      test_passed,
      NA_character_,
      as.character(glue::glue("Missing variable '{target_variable}'."))
    )
  )

  test_var_units <- tibble::tibble(
    test_type = "variable has correct units",
    target_variable = nc_vars,
    test_raw = purrr::map(nc_vars, check_unit, nc = nc, variable_table = variable_table),
    test_passed = !purrr::map_lgl(test_raw, inherits, "try-error"),
    test_error_message = purrr::map_chr(test_raw, purrr::possibly(as.character, NA_character_))
  ) %>% dplyr::select(-test_raw)

  results_df <- dplyr::bind_rows(test_dims_summary, test_required_vars, test_var_units)

  return(results_df)
}

#' Check that the unit of a variable in a NetCDF file is equivalent to
#' the expected unit.
#'
#' @param variable Name of target variable, as a length 1 character
#' @param nc NetCDF object containing target variable
#' @inheritParams check_met_input_file
#' @return `TRUE` if unit is correct, or `try-error` object if there is a mismatch.
#' @author Alexey Shiklomanov
check_unit <- function(variable, nc, variable_table, warn_unknown = TRUE) {
  if (!(variable %in% variable_table[["cf_standard_name"]]) && warn_unknown) {
    PEcAn.logger::logger.warn(glue::glue("Variable '{variable}' not in known variables"))
    return(TRUE)
  }
  var_correct_unit <- variable_table %>%
    dplyr::filter(cf_standard_name == variable) %>%
    dplyr::pull(units)
  ncvar_unit <- ncdf4::ncatt_get(nc, variable, "units")[["value"]]
  try(testthat::expect_true(
    PEcAn.utils::units_are_equivalent(ncvar_unit, var_correct_unit),
    glue::glue("NetCDF unit '{ncvar_unit}' not equivalent to expected unit '{var_correct_unit}'.")
  ))
}
