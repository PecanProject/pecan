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
##-------------------------------------------------------------------------------------------------#
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
  GPP_names <- c("GPP_a0", "GPP_a1", "GPP_Ha", "GPP_Hd")
  Reco_names <- c("Reco_Ea_som", "Reco_kM_som",
                  "Reco_Ea_labile", "Reco_kM_labile")
  CH4_names <- paste("CH4", 1:8, sep = "_") # TODO get meaningful names from Patty
  missing_traitnames <- setdiff(c(GPP_names, Reco_names, CH4_names), trait_names)
  if (length(missing_traitnames) > 0) {
    PEcAn.logger::logger.error(
      "Parameters missing from trait.values",
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

  jobsh <- gsub("@GPP_THETA@", paste(trait_values[GPP_names], collapse = ", "), jobsh)
  jobsh <- gsub("@RECO_THETA@", paste(trait_values[Reco_names], collapse = ", "), jobsh)
  jobsh <- gsub("@CH4_THETA@", paste(trait_values[CH4_names], collapse = ", "), jobsh)


  # yes, this will be replaced with real params once demo is working
  run_data <- PEPRMT::example_data |>
    dplyr::filter(.data$site == settings$run$site$id)

  utils::write.csv(run_data, file.path(rundir, "run_data.csv"), row.names = FALSE)
  writeLines(jobsh, con = file.path(rundir, "job.sh"))
  Sys.chmod(file.path(rundir, "job.sh"))
} # write.config.PEPRMT
