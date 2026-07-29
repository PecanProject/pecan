#' Writes a RothC config file.
#'
#' Requires a pft xml object, a list of trait values for a single model run,
#' and the name of the file to create
#'
#' @param defaults list of defaults to process
#' @param trait.values vector of samples for a given trait
#' @param settings list of settings from pecan settings file
#' @param run.id id of run
#' @return configuration file for MODEL for given run
#' @export
#' @author Chris Black
write.config.RothC <- function(defaults, trait.values, settings, run.id) {

  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)

  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate)
      && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(
      con = system.file("template.job", package = "PEcAn.RothC"),
      n = -1
    )
  }

  # create host specific setttings
  hostsetup <- ""
  if (!is.null(settings$model$prerun)) {
    hostsetup <- paste(hostsetup,
                       paste(settings$model$prerun, collapse = "\n"),
                       sep = "\n")
  }
  if (!is.null(settings$host$prerun)) {
    hostsetup <- paste(hostsetup,
                       paste(settings$host$prerun, collapse = "\n"),
                       sep = "\n")
  }

  hostteardown <- ""
  if (!is.null(settings$model$postrun)) {
    hostteardown <- paste(hostteardown,
                          paste(settings$model$postrun, collapse = "\n"),
                          sep = "\n")
  }
  if (!is.null(settings$host$postrun)) {
    hostteardown <- paste(hostteardown,
                          paste(settings$host$postrun, collapse = "\n"),
                          sep = "\n")
  }
  cdosetup <- ""
  if (!is.null(settings$host$cdosetup)) {
    cdosetup <- paste(cdosetup,
                      paste(settings$host$cdosetup, collapse = "\n"),
                      sep = "\n")
  }

  # create job.sh
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@CDO_SETUP@", cdosetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)

  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  jobsh <- gsub("@SITE_MET@", settings$run$inputs$met$path, jobsh)

  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)

  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)

  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))

  #-----------------------------------------------------------------------
  ### Edit a templated config file for runs
  config.text <- "RothC_input_template.dat" |>
    system.file(package = "PEcAn.RothC") |>
    readLines()

  # TODO PEcAn model template shows the following to accept templates from user.
  # This may be worth supporting with more thought, especially around:
  #   - is settings$model$config the right path? What do other models use?
  #   - version-specific?
  #   - if user-provided, should it be treated as a template or an already
  #     complete config?
  #   - Support looking up template path from DB? I suspect no
  #
  # if (!is.null(settings$model$config) && file.exists(settings$model$config)) {
  #   config.text <- readLines(con = settings$model$config, n = -1)
  # } else {
  #   filename <- system.file(settings$model$config, package = "PEcAn.RothC")
  #   if (filename == "") {
  #     if (!is.null(settings$model$revision)) {
  #       filename <- system.file(paste0("config.", settings$model$revision), package = "PEcAn.RothC")
  #     }
  #   }
  #   if (filename == "") {
  #     PEcAn.logger::logger.severe("Could not find config template")
  #   }
  #   PEcAn.logger::logger.info("Using", filename, "as template")
  #   config.text <- readLines(con = filename, n = -1)
  # }

  start_date <- as.Date(settings$run$start.date)
  end_date <- as.Date(settings$run$end.date)
  config.text <- gsub("@SITE_LAT@", settings$run$site$lat, config.text)
  config.text <- gsub("@SITE_LON@", settings$run$site$lon, config.text)
  config.text <- gsub("@SITE_MET@", settings$run$inputs$met$path, config.text)
  config.text <- gsub("@MET_START@", settings$run$site$met.start, config.text)
  config.text <- gsub("@MET_END@", settings$run$site$met.end, config.text)
  config.text <- gsub("@START_MONTH@", format(start_date, "%m"), config.text)
  config.text <- gsub("@START_DAY@", format(start_date, "%d"), config.text)
  config.text <- gsub("@START_YEAR@", format(start_date, "%Y"), config.text)
  config.text <- gsub("@END_MONTH@", format(end_date, "%m"), config.text)
  config.text <- gsub("@END_DAY@", format(end_date, "%d"), config.text)
  config.text <- gsub("@END_YEAR@", format(end_date, "%Y"), config.text)
  config.text <- gsub("@OUTDIR@", settings$host$outdir, config.text)
  config.text <- gsub("@ENSNAME@", run.id, config.text)
  config.text <- gsub("@OUTFILE@", paste0("out", run.id), config.text)

  # OPT_RMMOIST: soil water parameterization.
  #   1: Standard RothC soil water parameters
  #   2: Van Genuchten soil properties and soil is allowed to be drier
  #     (ie hygroscopic / capillary water, -1000bar)
  #   3: Van Genuchten soil properties, but uses the Standard RothC
  #     soil water function
  rmmoist <- settings$model$opt_RMmoist %||% 1
  config.text <- gsub("@OPT_RMMOIST@", rmmoist, config.text)

  # Bare SMD: wilting point configuration
  #   1: Standard RothC bareSMD
  #   2: bareSMD is set to wilting point -15bar (could be better for dry soils)
  smdbare <- settings$model$opt_RMmoist %||% 1
  config.text <- gsub("@OPT_SMDBARE@", smdbare, config.text)

  # min_RM_moist: Lowest value taken by the soil moisture rate modifier,
  # reached when soil water potential is -1500 kPa.
  # Note: Only used if smdbare == 2.
  # 0.2 is compatible with RothC 1.0; 0.15 or 0.1 may be better for dry systems.
  # see Farina et al 2013 10.1016/j.geoderma.2013.01.021
  min_rmmoist <- settings$model$min_RM_moist %||% 0.2
  # not added to config.text here bc written as part of @SOIL_PARAMS@ below

  # Soil depth to simulate.
  # Caution: Currently gets overridden by layer boundaries in soil file,
  # so it's really "simulate no deeper than this". TODO: FIXME.
  model_depth_cm <- settings$model$soil_depth_cm %||% 23

  ## Climate data
  # (we read it here to use its length in soil params,
  # remainder of processing happens below)
  met_path <- settings$run$inputs$met$path
  met_in <- utils::read.table(met_path, header = TRUE)
  n_met <- nrow(met_in)

  soil_params <- read_soil_physics(settings$run$inputs$soil_physics$path)

  ## Build string from soil params
  ## (plus number of timesteps, weirdly snuck into the middle)
  soil_param_string <- paste(
    round(soil_params$clay_pct, 1),
    round(soil_params$depth_cm, 1),
    round(soil_params$iom_tC_ha, 2),
    n_met + 12, # nsteps (includes extra year for spinup)
    # NB these last 4 params are always written,
    # but only used by model if smdbar == 2
    round(soil_params$silt_pct, 1),
    round(soil_params$bulkdens_g_cm3, 3),
    round(soil_params$org_C_pct, 1),
    min_rmmoist,
    sep = "      " # just to align with headers, RothC doesn't care
  )
  config.text <- gsub("@SOIL_PARAMS@", soil_param_string, config.text)

  # Climate data + management inputs
  # TODO all managements hardcoded for MVP
  zros <- rep(0, n_met)
  inputs <- data.frame(
    modern_pct = rep(100, n_met),
    # Plant C inputs -- not trying to be realistic yet!
    # RothC example input has ~2 t/yr, increasing through time and mostly
    # (but not always) added as one big spike in August
    C_inp_tC_ha = 2 / 12,
    FYM_tC_ha = zros,
    PC = zros,
    # defaults from RothC 2.1.0 manual
    # PL_* values are for "arable and improved grass"
    # OA_* values are for "farmyard manure"
    PL_DPM_f = 0.59,
    PL_RPM_f = 0.41,
    OA_DPM_f = 0.49,
    OA_RPM_f = 0.49,
    OA_BIO_f = 0.00,
    OA_HUM_f = 0.02
  )

  input_rows <- met_in |>
    dplyr::bind_cols(inputs) |>
    dplyr::select(
      "year", "month",
      "modern_pct",
      "Tmp_C", "Rain_mm", "Evap_mm",
      "C_inp_tC_ha", "FYM_tC_ha", "PC",
      "PL_DPM_f", "PL_RPM_f",
      "OA_DPM_f", "OA_RPM_f", "OA_BIO_f", "OA_HUM_f"
    ) |>
    # Duplicate first year as the equilibrium block
    # TODO we probably want a more principled approach here
    duplicate_first_year() |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.double), zapsmall)
    ) |>
    # Kinda ugly: Convert to one string to cram it into the template via gsub
    format() |>
    apply(1, paste, collapse = " ") |>
    paste(collapse = "\n")
  config.text <- gsub("@CLIM_DATA@", input_rows, config.text)

  writeLines(config.text, con = file.path(rundir, "RothC_input.dat"))

  invisible(config.text)
}

duplicate_first_year <- function(df) {
  df |>
    dplyr::slice_min(.data$year) |>
    dplyr::mutate(year = 1) |>
    dplyr::bind_rows(df)
}
