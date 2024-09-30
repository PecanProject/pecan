#' Read ED meteorology header file
#'
#' Read a ED_MET_DRIVER_HEADER file into a list-like object that can be 
#' manipulated within R. Returns a list of file formats.
#'
#' The output is an unnamed list with each element corresponding to a single 
#' file format. Each file format contains the following elements:
#'
#' - `path_prefix` -- Path and prefix of files
#' - `nlon` -- Number of longitude grid cells
#' - `nlat` -- Number of latitude grid cells
#' - `dx` -- Size of longitude grid cell
#' - `dy` -- Size of latitude grid cell
#' - `xmin` -- Minimum longitude
#' - `ymin` -- Minimum latitude
#' - `variables` -- Data frame of variables, with the columns described below.  
#' Starred columns are required for writing. This table is left joined with 
#' [met_variable_description] and [met_flag_description].
#'    - `variable` -- Variable name
#'    - `description` -- Variable description
#'    - `unit` -- Variable unit
#'    - `update_frequency` -- Update frequency (seconds) or scalar values if 
#'    `flag=4`
#'    - `flag` -- Variable flags.
#'    - `flag_description` -- Description of variable flag
#'
#' The formatting of a meteorology header file is as follows (from the [ED
#' GitHub Wiki](https://github.com/EDmodel/ED2/wiki/Drivers)):
#'
#' ```
#' <number of file formats>    # Repeat lines below this number of times
#' <path and prefix of files>
#' <nlon>, <nlat>, <dx>, <dy>, <xmin>, <ymin>
#' <number of variables>
#' <list of variable names>
#' <list of update frequencies (seconds) or scalar values if flag=4>
#' <list of variable flags>
#' ```
#'
#' The variables in the third row are defined as follows:
#' @param filename File name (including path) of met driver header file, as 
#' character
#' @param check Logical, whether or not to check file for correctness (default 
#' = `TRUE`)
#' @param check_files Logical. If `TRUE`, perform basic diagnostics on met 
#' files as well.
#' @return List of ED met input parameters. See Details.
#' @importFrom rlang .data
#' @export
read_ed_metheader <- function(filename, check = TRUE, check_files = TRUE) {
  if (!file.exists(filename)) {
    PEcAn.logger::logger.severe("File", filename, "not found")
  }
  full_file <- readLines(filename)
  comment_line <- character()
  nvar_rxp <- "^[[:space:]]*[[:digit:]]+[[:space:]]*$"
  for (check_line in full_file) {
    is_nvars <- grepl(nvar_rxp, check_line)
    if (is_nvars) {
      nvars <- as.numeric(check_line)
      break
    } else {
      comment_line <- c(comment_line, check_line)
    }
  }
  if (!exists("nvars")) {
    PEcAn.logger::logger.severe(
      "Unable to parse ED met driver header file: ", filename
    )
  }
  ed_metheader <- vector("list", nvars)
  var_lines <- 6
  for (i in seq_len(nvars)) {
    block <- seq(2 + length(comment_line) + (i-1) * var_lines, length.out = var_lines)
    sub_file <- full_file[block]
    path_prefix <- sub_file[1]
    met_files <- PEcAn.utils::match_file(path_prefix)
    if (!length(met_files) >= 0) {
      msg <- paste("No files matched for prefix", path_prefix)
      if (check) {
        PEcAn.logger::logger.severe(msg)
      } else {
        PEcAn.logger::logger.warn(msg)
      }
    }
    metadata <- unlist(utils::read.table(text = sub_file[2]))
    names(metadata) <- c("nlon", "nlat", "dx", "dy", "xmin", "ymin")
    nvars <- as.numeric(sub_file[3])
    variables_raw <- utils::read.table(text = sub_file[4:6], header = TRUE)
    variables_raw$value_type <- c("update_frequency", "flag")
    variables_table <- variables_raw %>%
      tidyr::gather("variable", "value", -.data$value_type) %>%
      tidyr::spread("value_type", "value") %>%
      dplyr::left_join(met_variable_description, by = "variable") %>%
      dplyr::left_join(met_flag_description, by = "flag")
    ed_metheader[[i]] <- list(
      path_prefix = path_prefix,
      nlon = metadata["nlon"],
      nlat = metadata["nlat"],
      dx = metadata["dx"],
      dy = metadata["dy"],
      xmin = metadata["xmin"],
      ymin = metadata["ymin"],
      variables = variables_table
    )
  }
  if (check) {
    check_ed_metheader(ed_metheader)
  }
  ed_metheader
}

#' Description of meteorology variables
#'
#' Helpful information about ED_MET_DRIVER files.
#'
#' `data.frame` with the following columns:
#' - `variable` -- Variable name
#' - `description` -- Variable description
#' - `unit` -- Variable unit (character, parse-able by `udunints2`)
#' @export
met_variable_description <- tibble::tribble(
  ~variable, ~description, ~unit,
  "nbdsf", "near IR beam downward solar radiation", "W m-2",
  "nddsf", "near IR diffuse downward solar radiation", "W m-2",
  "vbdsf", "visible beam downward solar radiation", "W m-2",
  "vddsf", "visible diffuse downward solar radiation", "W m-2",
  "prate", "precipitation rate (kg H2O)", "kg m-2 s-1",
  "dlwrf", "downward long wave radiation", "W m-2",
  "pres", "pressure", "Pa",
  "hgt", "geopotential height", "m",
  "ugrd", "zonal wind", "m s-1",
  "vgrd", "meridional wind", "m s-1",
  "sh", "specific humidity (kg water / kg air)", "kg kg-1",
  "tmp", "temperature", "K",
  "co2", "surface co2 concentration", "ppm",
  "lat", "grid of latitude coordinates, if this variable is present line 3 is ignored", NA,
  "lon", "grid of longitude coordinates, if this variable is present line 3 is ignored", NA
)

#' Description of meteorology flags
#'
#' Descriptions of ED met header variable flags.
#' 
#' `data.frame` with the following columns:
#'  - `flag` -- Numeric flag (in header file)
#'  - `flag_description` -- Description of flag
#' @export
met_flag_description <- tibble::tribble(
  ~flag, ~flag_description,
  0, "read gridded data - no time interpolation",
  1, "read gridded data - with time interpolation",
  2, "read gridded data - constant in time, not changing (if this is lat/lon, will overwrite line 3 information)",
  3, "read one value representing the whole grid - no time interpolation",
  4, "specify a constant for all polygons, constant in time (most likely reference height)"
)
