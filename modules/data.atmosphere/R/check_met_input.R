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

##' @export
##' @author Tony Gardella
##'
##' @param con  SQL connection object to BETY database
##' @param path string containing path to file
##' @param model string containing model type name, not specific version name
##' @param machine dataframe containing all columns from machine table for speifci hostname
##' @return executes a logger statement. Either exits workflow with statement explaining why the path provided is not valid or allows things to continue
##'
##' @description This is a function that takes in a path, model type name, and information about the machine
##' and checks if the path is on the machine and matches the format the model needs.
##'
check_path_in_db <- function(con, path, model, machine) {
  
  path_check <- tryCatch(
      tbl(con, "dbfiles") %>% filter(grepl(path, file_path) &
                                       machine$id == machine_id) %>%
        select("id", "file_name", "file_path", "container_id") %>% collect(),
      error = function(err)
        "Does not Exist"
    )
  
  input_check <- tryCatch(
    tbl(con, "inputs") %>%
      filter(path_check$container_id == id) %>%
      collect()
    ,
    error = function(err)
      NULL
  )
  
  
  format_check <-
    tryCatch(
      inner_join(
        tbl(con, "modeltypes"),
        tbl(con, "modeltypes_formats"),
        by = c("id" = "modeltype_id")
      ) %>%
        filter(grepl(model, name)) %>%
        filter(input_check$format_id == format_id) %>%
        filter("met" == tag) %>%
        collect(),
      error = function(err)
        "Wrong Format"
    )
  
  
  check <-
    dplyr::case_when(
      purrr::is_bare_character(path_check) ~ "DOESN'T EXIST",
      purrr::is_null(input_check) ~ "NO INPUT",
      purrr::is_bare_character(format_check) ~ "WRONG FORMAT",
      purrr::is_tibble(format_check) ~ "EXISTS"
    )
  
  check_message(check)
}

#' Execute a message statement based on 
#'
#' @param check Character string saying 
#' @return Executes a logger statement dependant on 
#' @author Tony Gardella
check_message <- function(check) {
  switch(
    check,
    "EXISTS" = PEcAn.logger::logger.info(
      "Met path provided exists in database and format matches model. Proceeding with provided Met."
      ,
      wrap = TRUE
    ),
    "NO INPUT" = PEcAn.logger::logger.severe(
      "File provided by path exists within the database but is not associated with an Input.
      If this file will be used by a model, please associate it with an input"
      ,
      wrap = TRUE
    )
    "WRONG FORMAT" = PEcAn.logger::logger.severe(
      "Met path provided exists in database but does not match format required for model.
      Please change pecan.xml met section and
      select an input a source to process data from."
      ,
      wrap = TRUE
    ),
    "DOESN'T EXIST" =  PEcAn.logger::logger.severe(
      "Met path provided does not match any records in the database.
      Please add it to the database or select a source of met to process."
      ,
      wrap = TRUE
    ),
  )
}