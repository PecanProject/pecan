##' Writes a PEPRMT config file.
##'
##' Requires a pft xml object, a list of trait values for a single PEPRMT run,
##' and the name of the file to create
##'
##' @name write.config.PEPRMT
##' @title Write PEPRMT configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for PEPRMT for given run
##' @export
##' @author Rob Kooper, edited by Abby Lewis
## ------------------------------------------------------------------------------------------------#
write.config.PEPRMT <- function(defaults, trait.values, settings, run.id) {
  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)

  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.PEPRMT"), n = -1)
  }

  # create host specific setttings
  hostsetup <- ""
  if (!is.null(settings$model$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$model$prerun, collapse = "\n"))
  }
  if (!is.null(settings$host$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$host$prerun, collapse = "\n"))
  }

  hostteardown <- ""
  if (!is.null(settings$model$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$model$postrun, collapse = "\n"))
  }
  if (!is.null(settings$host$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$host$postrun, collapse = "\n"))
  }

  # Handle parameters
  #
  # If more than one PFT is given, lump them together
  trait_values <- Reduce(trait.values, f = append)
  trait_names <- names(trait_values)
  # That was easy!
  # Now we'll do some extra work to complain informatively
  #  about any traits defined in more than one PFT.
  dup_traitnames <- trait_names[duplicated(trait_names)]
  if (length(dup_traitnames) > 0) {
    for (trt in dup_traitnames) {
      # NB this is the raw trait.values, not trait_values
      trt_in_pft <- sapply(trait.values, \(pft) trt %in% names(pft))
      pfts_with_trt <- names(trait.values)[trt_in_pft]
      PEcAn.logger::logger.info(
        "Parameter", dQuote(trt),
        "defined in multiple PFTs (", toString(pfts_with_trt), ").",
        "write.config.PEPRMT will use the one it saw first (",
        pfts_with_trt[[1]], ")."
      )
    }
  }

  params <- c(
    "wetland_type", "a0", "a1", "Ha", "Hd", "T_opt_GPP", "Ea_SOM",
    "kM_SOM", "Ea_labile", "kM_labile", "Ea_SOM_CH4", "kM_SOM_CH4",
    "Ea_labile_CH4", "kM_labile_CH4", "Ea_oxi_CH4", "kM_oxi_CH4",
    "kI_SO4", "kI_NO3", "k_plant_oxi"
  )
  provided_traitnames <- intersect(params, trait_names)
  if (!"wetland_type" %in% provided_traitnames) {
    trait_values["wetland_type"] <- 2
    provided_traitnames <- c(provided_traitnames, "wetland_type")
  }
  missing_traitnames <- setdiff(params, trait_names)
  if (length(missing_traitnames) > 0) {
    PEcAn.logger::logger.warn(
      "Parameters missing from trait.values. Will use default",
      sQuote(missing_traitnames)
    )
  }

  # create job.sh
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)

  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  # jobsh <- gsub("@SITE_MET@", settings$run$site$met, jobsh)

  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)

  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)

  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
  jobsh <- gsub("@DELETE_RAW@", as.logical(settings$model$delete.raw %||% FALSE), jobsh)

  param_str <- paste0(
    "list(",
    paste(
      paste0(provided_traitnames, " = ", trait_values[provided_traitnames]),
      collapse = ", "
    ),
    ")"
  )

  jobsh <- gsub("@PARAMS@", param_str, jobsh, fixed = TRUE)

  # MET
  met_path <- settings$run$inputs$met$path
  met <- utils::read.table(met_path, header = T)
  met_vars <- colnames(met)[!colnames(met) %in% c("Year", "DOY_disc")]

  peprmt_specific_input_path <- settings$run$inputs$PEPRMT$path

  run_data <- utils::read.csv(peprmt_specific_input_path) |>
    dplyr::select(-dplyr::any_of(met_vars)) |>
    dplyr::right_join(met) |>
    dplyr::mutate(site = settings$run$site$id)

  # Event handling
  event_file <- settings$run$inputs$event_json$path
  if (!is.null(event_file)) {
    stopifnot(file.exists(event_file))
    evts <- jsonlite::read_json(event_file) |>
      Filter(f = \(x) x$site_id == settings$run$site$id)
    if (length(evts) < 1) {
      PEcAn.logger::logger.error(
        "No events found for site", settings$run$site$id,
        "in file", event_file
      )
    } else if (length(evts) > 1) {
      PEcAn.logger::logger.warn(
        "Event file", event_file,
        "contains multiple entries for site", settings$run$site$id,
        ". Using only the first one."
      )
    }
    evts <- evts[[1]]$events

    known_event_types <- c("salinity", "elevation")
    evt_types <- sapply(evts, \(x) x$event_type)
    use_evt <- evt_types %in% known_event_types
    if (!all(use_evt)) {
      PEcAn.logger::logger.info(
        "Ignoring unsupported event types",
        sQuote(unique(evt_types[!use_evt])),
        ".  Supported event types are:",
        sQuote(known_event_types)
      )
      evts <- evts[use_evt]
    }
    evt_order <- sapply(evts, \(x) x$date) |>
      as.Date() |>
      order()
    evts <- evts[evt_order]

    # need to handle events sequentially so effects can stack
    # TODO think about efficiency, like, at all
    for (evt in evts) {
      run_data <- impose_event_on_data(run_data, evt)
    }
  }

  utils::write.csv(run_data, file.path(rundir, "run_data.csv"), row.names = FALSE)
  writeLines(jobsh, con = file.path(rundir, "job.sh"))
  Sys.chmod(file.path(rundir, "job.sh"))
} # write.config.PEPRMT



impose_event_on_data <- function(data, event) {
  evt_yr <- lubridate::year(event$date)
  evt_day <- lubridate::yday(event$date)
  after_start <- which(
    data$Year > evt_yr |
      (data$Year == evt_yr & data$DOY_disc >= evt_day)
  )
  if (length(after_start) == 0) {
    # event is out of data range
    return(data)
  }
  startline <- min(after_start)

  # lots of options for cleaner structure,
  # but for now here's some spaghetti code
  if (event$event_type == "elevation") {
    # one-time offset assumed to change all future dates by same amount
    endline <- nrow(data)
    data$WTD_cm[startline:endline] <- data$WTD_cm[startline:endline] +
      event$cm_elevation_rise
  } else if (event$event_type == "salinity") {
    # Multiplicative change that lasts a set time window (constant for now)
    # duration minus 1 because event date is day 1
    end_date <- as.Date(event$date) + lubridate::days(event$days_duration - 1)
    end_yr <- lubridate::year(end_date)
    end_day <- lubridate::yday(end_date)
    before_end <- which(
      data$Year < end_yr |
        (data$Year == end_yr & data$DOY_disc <= end_day)
    )
    if (length(before_end) == 0) {
      return(data)
    }
    endline <- max(before_end)
    data$Salinity_daily_ave_ppt[startline:endline] <-
      data$Salinity_daily_ave_ppt[startline:endline] *
        (1 + (event$pct_relative_salinity_change / 100))
  }

  data
}
