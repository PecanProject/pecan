#' Check ED met header object
#'
#' Check that the object has all components, and throw an error if anything is 
#' wrong. Optionally, do some basic checks of actualy meteorology files as 
#' well.
#'
#' `check_ed_metheader_format` checks an individual format (one item in the 
#' `ed_metheader` list). `check_ed_metheader` applies these checks to each item 
#' in the format list.
#'
#' @param ed_metheader ED meteorology header object (see [read_ed_metheader])
#' @param ed_metheader_format A single format inside the met header object
#' @inheritParams read_ed_metheader
#' @export
check_ed_metheader <- function(ed_metheader, check_files = TRUE) {
  testthat::test_that(
    "ED met header object is a nested list",
    {
      testthat::expect_true(!is.null(names(ed_metheader[[1]])))
    }
  )
  .z <- lapply(ed_metheader, check_ed_metheader_format, check_files = check_files)
  invisible(TRUE)
}

check_ed_metheader_format <- function(ed_metheader_format, check_files = TRUE) {
  testthat::test_that(
    "Format has the correct names",
    {
      correct_names <- c("path_prefix", "nlon", "nlat", "dx", "dy", "xmin", "ymin", "variables")
      all(names(ed_metheader_format) %in% correct_names)
    }
  )
  testthat::test_that(
    "ED met header files exist and are not empty",
    {
      met_files <- PEcAn.utils::match_file(ed_metheader_format$path_prefix)
      testthat::expect_gte(length(met_files), 1)
      testthat::expect_true(all(file.exists(met_files)))
      testthat::expect_true(all(file.size(met_files) > 0))
    }
  )

  number <- c("numeric", "integer")
  testthat::test_that(
    "Met header metadata fields are valid",
    {
      testthat::expect_true(is.numeric(ed_metheader_format$nlon), "numeric")
      testthat::expect_true(is.numeric(ed_metheader_format$nlat), "numeric")
      testthat::expect_true(is.numeric(ed_metheader_format$dx), "numeric")
      testthat::expect_true(is.numeric(ed_metheader_format$dy), "numeric")
      testthat::expect_true(is.numeric(ed_metheader_format$xmin), "numeric")
      testthat::expect_true(is.numeric(ed_metheader_format$ymin), "numeric")
      testthat::expect_is(ed_metheader_format$variables, "data.frame")
    }
  )

  if (check_files) {
    met_files <- PEcAn.utils::match_file(ed_metheader_format$path_prefix, suffix = "h5")
    .z <- lapply(met_files, check_ed_metfile, variables = ed_metheader_format$variables)
  }
}

#' Check individual ED metfile
#'
#' @param metfile Path to meteorology file
#' @param variables Variables table from [ed_metheader][read_ed_metheader] object
#' @return `NULL`, invisibly, if successful or throw an error
check_ed_metfile <- function(metfile, variables) {
  hfile <- hdf5r::H5File$new(metfile, mode = "r")
  # Remove variables that are not constants
  variables <- variables[variables$flag != 4, ]
  testthat::test_that(
    "All variables present in metfile",
    {
      testthat::expect_true(all(variables$variable %in% hfile$ls()$name))
    }
  )
}
