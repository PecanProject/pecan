#' Check a meteorology data file for compliance with the PEcAn standard
#'
#' @param metfile Path of met file to check, as a scalar character.
#' @param variable_table `data.frame` linking standard names to their
#'   units. Defaults to [pecan_standard_met_table].
#' @return `TRUE` (invisibly) if successful, otherwise throw an informative error.
#' @author Alexey Shiklomanov
#' @export
check_met_input_file <- function(metfile, variable_table = pecan_standard_met_table) {

  metfile <- normalizePath(metfile)

  PEcAn.logger::severeifnot(
    file.exists(metfile),
    msg = glue::glue("File '{metfile}' does not exist.")
  )

  nc <- ncdf4::nc_open(metfile)

  test_dims <- tryCatch(
    testthat::test_that(
      "Dimensions are correct.",
      {
        dimensions <- nc[["dim"]]
        testthat::expect_type(dimensions, "list")
        testthat::expect_equal(length(dimensions), 3)
        testthat::expect_true("time" %in% names(dimensions))
        testthat::expect_match(
          ncdf4::ncatt_get(nc, "time", "units")[["value"]],
          "^days since [[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}Z$"
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

  test_vars <- purrr::map(
    names(nc$var),
    purrr::safely(check_met_variable),
    nc = nc,
    variable_table = variable_table
  ) %>%
    setNames(names(nc$var)) %>%
    purrr::transpose()

  test_vars_which_errors <- !purrr::map_lgl(test_vars[["error"]], is.null)
  test_vars_errors <- test_vars[["error"]][test_vars_which_errors] %>%
    purrr::map(conditionCall)
  all_errors <- c(list(dimensions = test_dims), test_vars_errors)

  if (length(all_errors) > 0) {
    cat(unlist(all_errors), sep = "\n\n")
    stop("At least one error was found.")
  }

  invisible(TRUE)
}

#' Check a met variable for PEcAn standard compliance
#'
#' Checks that the variable has a correct CF standard name, and the
#' correct units.
#' @param nc NetCDF object (as returned by [ncdf4::nc_open]).
#' @param variable Variable to check, as a character scalar.
#' @return `TRUE` (invisibly) if successful, otherwise throw an informative error.
#' @author Alexey Shiklomanov
#' @inheritParams check_met_input_file
check_met_variable <- function(nc, variable, variable_table = pecan_standard_met_table) {
  tryCatch(
    testthat::test_that(
      glue::glue("Variable '{variable}' has correct name and units."),
      {
        testthat::expect_true(
          variable %in% variable_table$cf_standard_name,
          info = paste0("Variable is not a known name in the CF standard. ",
                        "See PEcAn.data.atmosphere::pecan_standard_met_table.")
        )
        var_correct_unit <- variable_table %>%
          dplyr::filter(cf_standard_name == variable) %>%
          dplyr::pull(units)
        testthat::expect_equal(
          ncdf4::ncatt_get(nc, variable, "units")[["value"]],
          var_correct_unit
        )
      }
    ), error = function(e) conditionMessage(e)
  )
}
