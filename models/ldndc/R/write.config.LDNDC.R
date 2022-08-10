
##-------------------------------------------------------------------------------------------------#
##' Writes a LDNDC config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.LDNDC
##' @title Write LDNDC configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for LDNDC for given run
##' @export
##' @author Henri Kajasilta
##-------------------------------------------------------------------------------------------------#
write.config.LDNDC <- function(defaults, trait.values, settings, run.id) {

  
  # To enforce minimum version to be used in simulations.
  # Should not be necessary, but informs users who want to
  # inspect input files closer (or do simulatons on different
  # environment. Set based on current model version (1.33)
  # and probably no reason to change.
  MinPackReq <- "1.3" # Current version 1.33
  
  
  # Create Schedule time
  if(!is.null(settings$run$start.date) & !is.null(settings$run$end.date)){
    
    steps <- 48 # Hard-coded for now
    ScheduleTime <- paste0(format(as.POSIXlt(settings$run$start.date), "%Y-%m-%d"), "/",
                           steps, " -> ", as.Date(format(as.POSIXlt(settings$run$end.date), "%Y-%m-%d"))+1)
                          # One extra day added to the end day of simulations, because the simulations will stop to
                          # the first timestep in a given end day. As a result we  got our real end date simulated
                          # and one extra output line from the day after.
                          # This one extra output line is taken out it model2netcdf to not include extra values
                          # in netcdf file
  }

  
  # Find out where to write run/ouput dirs
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)
  
  # Source
  if(!is.null(settings$run$inputs$met$path)){
    # For climate data
    MetPath <- settings$run$inputs$met$path
    # Info for project file from which directory to read the inputs
    SourcePrefix <- paste0(rundir, "/")
    # Raw model outputs are written into own directory
    OutputPrefix <- file.path(outdir, "Output/")
  }
  
  #-----------------------------------------------------------------------
  ## Fill .ldndc template with the given settings
  projectfile <- readLines(con = system.file("project.ldndc", package = "PEcAn.LDNDC"), n = -1)
  
  
  # Changes
  projectfile <- gsub('@PackMinVerReq@', MinPackReq, projectfile)
  
  projectfile <- gsub('@ScheduleTime@', ScheduleTime, projectfile)
  
  projectfile <- gsub('@SourcePrefix@', SourcePrefix, projectfile)
  projectfile <- gsub('@OutputPrefix@', OutputPrefix, projectfile)
  
  # Write project file to rundir
  writeLines(projectfile, con = file.path(settings$rundir, run.id, "project.ldndc"))
  
  
  
  
  #-----------------------------------------------------------------------
  # Create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.LDNDC"), n = -1)
  }
  
  # Create host specific settings
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
  
  
  
  # Create job.sh based on the given settings
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)
  
  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  
  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
  
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  jobsh <- gsub("@METPATH@", MetPath, jobsh)
  
  jobsh <- gsub("@BINARY@", paste(settings$model$binary, paste0(rundir, "/project.ldndc")), jobsh)
  
  if(is.null(settings$model$delete.raw)){
    settings$model$delete.raw <- FALSE
  }
  
  jobsh <- gsub("@DELETE.RAW@", settings$model$delete.raw, jobsh)
  
  # Write job.sh file to rundir
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(rundir, "job.sh")) # Permissions
  
  
  
  #### write run-specific PFT parameters here #### Get parameters being handled by PEcAn
  speciesparfile <- readLines(con = system.file("speciesparameter_template.xml", package = "PEcAn.LDNDC"), n = -1)
  
  
  #----------------------
    
  ## Set-up the necessary files in to the run directory so
  ## model is able to function properly. Later on, these
  ## files should be populated with initial values.
    
  
  ###### THIS NEEDS TO BE FUNCTION AT SOME POINT
  ### PROBABLY SIMILAR FUNCTION FOR siteparameters as well
  #
  mnemonic_1 <- "__grasses__"
  group <- "grass"
  mnemonic_2 <- "perennialgrass"
  
  a.1 <- paste0("<species mnemonic='", mnemonic_1, "' group='", group, "' > \n")
  b.1 <- paste0("\t\t\t\t\t<species mnemonic='", mnemonic_2, "' > \n")
  b.2 <- ""
  # Keep old version as a reference this need to reconstruct at some point properly anyway
  #b.2 <- apply(trait.values[[1]], 1, function(x){paste0("\t\t\t\t\t\t<par name='", names(x), "' value='", x, "' /> \n")})
  b.3 <- paste0("\t\t\t\t</species> \n")
  a.2 <- paste0("\t\t\t</species>")
  
  
  
  for (pft in seq_along(trait.values)) {
    
    pft.traits <- unlist(trait.values[[pft]])
    pft.names <- names(pft.traits)
    
    
    # Number at the beginning refers to the number of species parameters in LDNDC guide book.
    # First there is name in LDNDC and the second is name in BETY database
    
    #18 ALB (-) - SW_albedo (-)
    if ("SW_albedo" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t\t<par name='ALB' value='", pft.traits[which(pft.names == "SW_albedo")], "' /> \n"), collapse="")
    }
    
    #34 DIAMMAX (m) - stem_diameter (cm)
    if ("stem_diameter" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t\t<par name='DIAMMAX' value='",
                               PEcAn.utils::ud_convert(
                                 pft.traits[which(pft.names == "stem_diameter")], "m", "cm"
                                 ),"' /> \n"), collapse="")
    }
    
    #58 EXT - extinction_coefficient_diffuse
    if ("extinction_coefficient_diffuse" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t\t<par name='EXT' value='", pft.traits[which(pft.names == "extinction_coefficient_diffuse")], "' /> \n"), collapse="")
    }
    
    #79 FRACTION_ROOT - root_biomass_fraction
    if ("root_biomass_fraction" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t\t<par name='FRACTION_ROOT' value='", pft.traits[which(pft.names == "root_biomass_fraction")], "' /> \n"), collapse="")
    }
    
    #89 GDD_BASE_TEMPERATURE (C) - gdd_tbase (C)
    if ("gdd_tbase" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t\t<par name='GDD_BASE_TEMPERATURE' value='", pft.traits[which(pft.names == "gdd_tbase")], "' /> \n"), collapse="")
    }
    
    #167 PSNTMAX (C) -  pstemp_max (C)
    if ("pstemp_max" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t\t<par name='PSNTMAX' value='", pft.traits[which(pft.names == "pstemp_max")], "' /> \n"), collapse="")
    }
    
    #168 PSNTMIN (C) -  pstemp_min (C)
    if ("pstemp_min" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t\t<par name='PSNTMIN' value='", pft.traits[which(pft.names == "pstemp_min")], "' /> \n"), collapse="")
    }
    
    #169 PSNTOPT (C) -  psnTOpt (C)
    if ("psnTOpt" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t\t<par name='PSNTOPT' value='", pft.traits[which(pft.names == "psnTOpt")], "' /> \n"), collapse="")
    }
    
    #193 SLAMAX (m2 kg-1) -  SLAMAX (m2 kg-1)
    if ("SLAMAX" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t\t<par name='SLAMAX' value='", pft.traits[which(pft.names == "SLAMAX")], "' /> \n"), collapse="")
    }
    
    #194 SLAMIN (m2 kg-1) -  SLAMIN (m2 kg-1)
    if ("SLAMIN" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t\t<par name='SLAMIN' value='", pft.traits[which(pft.names == "SLAMIN")], "' /> \n"), collapse="")
    }
    
  }
  
  speciesparfile <- gsub("@Info@", paste(a.1,b.1,b.2,b.3,a.2), speciesparfile)
  
  writeLines(speciesparfile, con = file.path(settings$rundir, run.id, "speciesparameters.xml"))
  

  
  # Default events file, need to change later on. Only takes care of some initial biomass and
  # then some random events. Not made for real use, but testing.
  eventsfile <- readLines(con = system.file("events_template.xml", package = "PEcAn.LDNDC"), n = -1)
  
  eventsfile <- gsub("@Startdate@", as.Date(settings$run$start.date, format = "%Y/%m/%d"), eventsfile)
  eventsfile <- gsub("@Event_1_Time@", as.Date(settings$run$start.date, format = "%Y/%m/%d") + sample(120:160, 1), eventsfile)
  eventsfile <- gsub("@Event_2_Time@", as.Date(settings$run$start.date, format = "%Y/%m/%d") + sample(170:180, 1), eventsfile)
  eventsfile <- gsub("@Event_3_Time@", as.Date(settings$run$start.date, format = "%Y/%m/%d") + sample(250:300, 1), eventsfile)
  
  writeLines(eventsfile, con = file.path(settings$rundir, run.id, "events.xml"))



  # Default setup file, need to change later on
  setupfile <- readLines(con = system.file("setup.xml", package = "PEcAn.LDNDC"), n = -1)
  writeLines(setupfile, con = file.path(settings$rundir, run.id, "setup.xml"))

  
  # Default site file, need to change later on
  sitefile <- readLines(con = system.file("site.xml", package = "PEcAn.LDNDC"), n = -1)
  writeLines(sitefile, con = file.path(settings$rundir, run.id, "site.xml"))
  
  # Default site file, need to change later on
  siteparfile <- readLines(con = system.file("siteparameters.xml", package = "PEcAn.LDNDC"), n = -1)
  writeLines(siteparfile, con = file.path(settings$rundir, run.id, "siteparameters.xml"))
  
  
  # Use ready airchemistry file for now
  airchemistry <- readLines(con = system.file("airchemistry.txt", package = "PEcAn.LDNDC"), n = -1)
  writeLines(airchemistry, con = file.path(settings$rundir, run.id, "airchemistry.txt"))
  
  
  #------------------------
  
} # write.config.LDNDC
