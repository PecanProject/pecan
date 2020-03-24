#' Modify an ED2IN object
#'
#' This is a convenience function for modifying an `ed2in` list object.  
#' Arguments passed in all caps are assumed to be ED2IN namelist parameters and 
#' are inserted directly into the `ed2in` list objects. Lowercase arguments are 
#' defined explicitly (see "Parameters"), and those that do not match explicit 
#' arguments will be ignored with a warning. Because the lowercase arguments 
#' come with additional validity checks, they are recommended over modifying 
#' the ED2IN file directly via uppercase arguments. For all lowercase
#' arguments, the default (`NULL`) means to use whatever is currently
#' in the input `ed2in`.
#'
#' Namelist arguments are applied last, and will silently overwrite any 
#' arguments set by special case arguments.
#'
#' Namelist arguments can be stored in a list and passed in via the `.dots` 
#' argument (e.g. `.dots = list(SFILIN = "/path/prefix_", ...)`), or using the 
#' `rlang::!!!` splicing operator. If both are provided, they will be spliced 
#' together, with the `...` taking precedence.
#'
#' For `output_types`, select one or more of the following:
#'  - "fast" -- Fast analysis; mostly polygon-level averages (`IFOUTPUT`)
#'  - "daily -- Daily means (one file per day) (`IDOUTPUT`)
#'  - "monthly" -- Monthly means (one file per month) (`IMOUTPUT`)
#'  - "monthly_diurnal" -- Monthly means of the diurnal cycle (one file per 
#'  month) (`IQOUTPUT`)
#'  - "annual" -- Annual (one file per year) (`IYOUTPUT`)
#'  - "instant" -- Instantaneous fluxes, mostly polygon-level variables, one 
#'  file per year (`ITOUTPUT`)
#'  - "restart" -- Restart file for HISTORY runs. (`ISOUTPUT`)
#'  - "all" -- All output types
#'
#' @inheritParams read_ed2in
#' @param ... Namelist arguments (see Description and Details)
#' @param veg_prefix Vegetation file prefix (`SFILIN`). If `lat` and `lon` are part of the prefix, 
#' @param latitude Run latitude coordinate. If `veg_prefix` is also provided, 
#' pass to [read_ed_veg], otherwise set in ED2IN directly. Should be omitted if 
#' `lat` and `lon` are already part of `veg_prefix`.
#' @param longitude Run longitude coordinate. If `veg_prefix` is also provided, 
#' pass to [read_ed_veg], otherwise set in ED2IN directly. Should be omitted if 
#' `lat` and `lon` are already part of `veg_prefix`.
#' @param met_driver Path and filename of met driver header 
#' (`ED_MET_DRIVER_DB`)
#' @param start_date Run start date (`IMONTHA`, `IDATEA`, `IYEARA`, `ITIMEA`)
#' @param end_date Run end date (`IMONTHZ`, `IDATEZ`, `IYEARZ` `ITIMEZ`)
#' @param EDI_path Path to `EDI` directory, which often has the `VEG_DATABASE` 
#' and `THSUMS_DATABASE` files.
#' @param output_types Character vector of output types (see Details)
#' @param output_dir Output directory, for `FFILOUT` (analysis) and `SFILOUT` 
#'(history) files
#' @param run_dir Directory in which to store run-related config files (e.g. `config.xml`).
#' @param runtype ED initialization mode; either "INITIAL" or "HISTORY"
#' @param run_name Give the run an informative name/description. Sets
#'   the ED2IN `EXPNME` tag. (default is `NULL`)
#' @param include_these_pft Numeric vector describing the PFTs to
#'   include in ED. Note that this is in addition to any PFTs
#'   specified by the `config.xml` -- regardless of what this is set
#'   to, those PFTs will be included, so if you want to only use PFTs
#'   defined in `config.xml`, set this to `numeric(0)`. The default
#'   (`NULL`) means to use whatever is already in the current ED2IN
#'   file, which is usually all (1-17) of ED's PFTs.
#' @param pecan_defaults Logical. If `TRUE`, set common `ED2IN` defaults.
#' @param add_if_missing Logical. If `TRUE`, all-caps arguments not found in 
#'existing `ed2in` list will be added to the end.  Default = `FALSE`.
#' @param check_paths Logical. If `TRUE` (default), for any parameters that 
#' expect files, check that files exist and throw an error if they don't.
#' @param .dots A list of `...` arguments.
#' @return Modified `ed2in` list object. See [read_ed2in].
#' @export
modify_ed2in <- function(ed2in, ...,
                         veg_prefix = NULL,
                         latitude = NULL,
                         longitude = NULL,
                         met_driver = NULL,
                         start_date = NULL,
                         end_date = NULL,
                         EDI_path = NULL,
                         output_types = NULL,
                         output_dir = NULL,
                         run_dir = NULL,
                         runtype = NULL,
                         run_name = NULL,
                         include_these_pft = NULL,
                         pecan_defaults = FALSE,
                         add_if_missing = FALSE,
                         check_paths = TRUE,
                         .dots = list()) {

  if (is.null(.dots)) {
    .dots <- list()
  }
  dots <- modifyList(.dots, list(...))
  is_upper <- names(dots) == toupper(names(dots))
  lower_args <- names(dots)[!is_upper]
  if (length(lower_args) > 0) {
    PEcAn.logger::logger.warn(
      "The following lowercase arguments are not supported, and will be dropped: ",
      paste(lower_args, collapse = ", ")
    )
  }

  if (pecan_defaults) {
    ed2in[["IMETAVG"]] <- -1
    ed2in[["IPHEN_SCHEME"]] <- 0
    ed2in[["IPHENYS1"]] <- NA
    ed2in[["IPHENYSF"]] <- NA
    ed2in[["PHENPATH"]] <- ""
    ed2in[["IED_INIT_MODE"]] <- 0
    ed2in[["SOIL_DATABASE"]] <- ""
    ed2in[["LU_DATABASE"]] <- ""
  }

  if (!is.null(runtype)) {
    if (!toupper(runtype) %in% c("INITIAL", "HISTORY")) {
      PEcAn.logger::logger.severe(
        "Invalid runtype ", runtype,
        ". Must be either 'INITIAL' or 'HISTORY'."
      )
    }
    ed2in[["RUNTYPE"]] <- runtype
  }

  if (!is.null(run_name)) {
    ed2in[["EXPNME"]] <- run_name
  }

  if (!is.null(veg_prefix)) {
    if (check_paths) {
      .z <- PEcAn.utils::match_file(veg_prefix, suffix = "css", expect = 1)
      .z <- PEcAn.utils::match_file(veg_prefix, suffix = "pss", expect = 1)
      .z <- PEcAn.utils::match_file(veg_prefix, suffix = "site", expect = 1)
    }
    ed2in[["SFILIN"]] <- normalizePath(veg_prefix, mustWork = FALSE)
    ed2in[["IED_INIT_MODE"]] <- 3
    ed_veg <- read_ed_veg(veg_prefix, latitude = latitude, longitude = longitude)
    ed2in[["POI_LAT"]] <- ed_veg$latitude
    ed2in[["POI_LON"]] <- ed_veg$longitude
  }

  if (is.null(veg_prefix) && !is.null(latitude)) {
    ed2in[["POI_LAT"]] <- latitude
  }

  if (is.null(veg_prefix) && !is.null(longitude)) {
    ed2in[["POI_LON"]] <- longitude
  }

  if (!is.null(EDI_path)) {
    ed2in[["VEG_DATABASE"]] <- normalizePath(file.path(EDI_path, "oge2OLD", "OGE2_"), mustWork = FALSE)
    ed2in[["THSUMS_DATABASE"]] <- paste0(normalizePath(file.path(EDI_path, "ed_inputs")), "/")
  }

  if (!is.null(met_driver)) {
    if (check_paths) {
      if (!file.exists(met_driver)) {
        PEcAn.logger::logger.severe(
          "Met driver file ", met_driver, " not found."
        )
      }
      if (!file.size(met_driver) > 0) {
        PEcAn.logger::logger.severe(
          "Met driver file ", met_driver, " found but is empty."
        )
      }
    }
    ed2in[["ED_MET_DRIVER_DB"]] <- normalizePath(met_driver)
  }

  if (!is.null(start_date)) {
    # Change dates for both initial and history
    # This should be OK because initial (I...A) are ignored if runtype = HISTORY,
    # and history (I...H) are ignored if runtype = INITIAL
    ed2in[["IYEARA"]] <- ed2in[["IYEARH"]] <- lubridate::year(start_date)
    ed2in[["IMONTHA"]] <- ed2in[["IMONTHH"]] <- lubridate::month(start_date)
    ed2in[["IDATEA"]] <- ed2in[["IDATEH"]] <- lubridate::day(start_date)
    ed2in[["ITIMEA"]] <- ed2in[["ITIMEH"]] <-
      as.numeric(strftime(start_date, "%H%M", tz = "UTC"))
    ed2in[["METCYC1"]] <- ed2in[["IYEARA"]]
  }
  

  if (!is.null(end_date)) {
    ed2in[["IYEARZ"]] <- lubridate::year(end_date)
    ed2in[["IMONTHZ"]] <- lubridate::month(end_date)
    ed2in[["IDATEZ"]] <- lubridate::day(end_date)
    ed2in[["ITIMEZ"]] <- as.numeric(strftime(end_date, "%H%M", tz = "UTC"))
    ed2in[["METCYCF"]] <- ed2in[["IYEARZ"]]
  }

  if (!is.null(output_types)) {
    valid_types <- c(
      IFOUTPUT = "fast",
      IDOUTPUT = "daily",
      IMOUTPUT = "monthly",
      IQOUTPUT = "monthly_diurnal",
      IYOUTPUT = "annual",
      ITOUTPUT = "instant",
      ISOUTPUT = "restart"
    )
    if (output_types == "all") {
      output_types <- valid_types
    }
    invalid_types <- output_types[!output_types %in% valid_types]
    if (length(invalid_types) > 0) {
      PEcAn.logger::logger.severe(
        "Invalid output types provided: ", paste(invalid_types, collapse = ", "),
        ". Only the following output types are supported: ",
        paste(valid_types, collapse = ", ")
      )
    }
    # 3 for HDF5 output, 0 for no output
    on_types <- (valid_types %in% output_types) * 3
    names(on_types) <- names(valid_types)
    ed2in <- modifyList(ed2in, as.list(on_types))
  }

  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE)
    ed2in[["FFILOUT"]] <- file.path(normalizePath(output_dir), "analysis")
    ed2in[["SFILOUT"]] <- file.path(normalizePath(output_dir), "history")
  }

  if (!is.null(run_dir)) {
    dir.create(run_dir, showWarnings = FALSE)
    ed2in[["IEDCNFGF"]] <- file.path(normalizePath(run_dir), "config.xml")
    ed2in[["EVENT_FILE"]] <- file.path(normalizePath(run_dir), "myevents.xml")
  }

  if (!is.null(include_these_pft)) {
    if (!is.numeric(include_these_pft)) PEcAn.logger::logger.severe("include_these_pft must be numeric vector")
    ed2in[["INCLUDE_THESE_PFT"]] <- include_these_pft
  }

  namelist_args <- dots[is_upper]
  in_ed2in <- names(namelist_args) %in% names(ed2in)
  if (sum(!in_ed2in) > 0) {
    new_args <- namelist_args[!in_ed2in]
    if (!add_if_missing) {
      PEcAn.logger::logger.warn(
        "The following namelist arguments were missing from ED2IN ",
        "and will be ignored because `add_if_missing = FALSE`: ",
        paste(names(new_args), collapse = ", ")
      )
      namelist_args <- namelist_args[in_ed2in]
    } else {
      PEcAn.logger::logger.debug(
        "Adding the following new arguments to ED2IN: ",
        paste(names(new_args), collapse = ", ")
      )
    }
  }
  ed2in <- modifyList(ed2in, namelist_args)
  ed2in
}
