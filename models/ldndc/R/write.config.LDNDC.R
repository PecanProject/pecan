
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

  
  MinPackReq <- "1.35" # Current version 1.35
  
  
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
  
  
  # Add groundwater file, if it is available for site
  # Not obligatory file for model run
  if(!is.null(settings$run$inputs$groundwater$path1)){
    GroundWater = '<groundwater source="groundwater.txt" format="txt" />'
    groundwaterfile <- readLines(con = file.path(settings$run$inputs$groundwater$path1))
    writeLines(groundwaterfile, con = file.path(settings$rundir, run.id, "groundwater.txt"))
  }else{GroundWater = ""}
  
  
  
  
  #-----------------------------------------------------------------------
  ## Fill .ldndc template with the given settings
  projectfile <- readLines(con = system.file("project.ldndc", package = "PEcAn.LDNDC"), n = -1)
  
  
  # Changes
  projectfile <- gsub('@PackMinVerReq@', MinPackReq, projectfile)
  
  projectfile <- gsub('@ScheduleTime@', ScheduleTime, projectfile)
  
  projectfile <- gsub('@SourcePrefix@', SourcePrefix, projectfile)
  projectfile <- gsub('@OutputPrefix@', OutputPrefix, projectfile)
  
  projectfile <- gsub('@Groundwater@', GroundWater, projectfile)
  
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
  
  # LDNDC binaries in this server are located here. This binary also points to model own configurations.
  jobsh <- gsub("@BINARY@", paste(settings$model$binary, paste0(rundir, "/project.ldndc")), jobsh)
  
  if(is.null(settings$model$delete.raw)){
    settings$model$delete.raw <- FALSE
  }
  
  jobsh <- gsub("@DELETE.RAW@", settings$model$delete.raw, jobsh)
  
  # Write job.sh file to rundir
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(rundir, "job.sh")) # Permissions
  
  
  
  ## ----- Preparing the setup file ----- ##
  
  ## Setup file -- This may differ based on the site properties and the ecosystem we are simulating
  setupfile <- readLines(con = system.file("setup_template.xml", package = "PEcAn.LDNDC"), n = -1)
  
  ## Timemode
  # Timemode currently supports only subdaily
  timemode <- "subdaily"
  setupfile <- gsub("@timemode@", timemode, setupfile)
  
  
  ## Elevation, latitude and longitude
  setupfile <- gsub("@elevation@", "10", setupfile)
  setupfile <- gsub("@latitude@", settings$run$site$lat, setupfile)
  setupfile <- gsub("@longitude@", settings$run$site$lon, setupfile)
  
  
  ## Check the site id
  site_id <- settings$run$site$id
  
  
  
  ## Handle the setups, when working with grass, crop and forest fields
  # Possibly to hard code species to the list, this differentiation is done only
  # for the purpose of separating the setups between forest and grassland/crops
  # If more species is wanted to be add, update also the part where parameters of pfts are written into speciesparameter file.
  # Available species for grass/crops: timothy, oat and barley
  # Available species for forest:      pipy
  pfts_grasscrops <- c("barley", "oat", "triticale", "timothy", "meadow", "soil")
  pfts_forest <- c("pipy")
  pfts_run <- NULL
  for(pft_names in 1:length(settings$pfts)){
    pfts_run <- c(pfts_run, settings$pfts[[pft_names]]$name)
  }
  
  
  # Setup file created  for grass and crop simulations:
  if(all(pfts_run %in% pfts_grasscrops)){
  
    ## Modules
    # Microclimate module
    setupfile <- gsub("@microclimate@", "canopyecm", setupfile)
    
    # Watercycle module and option
    setupfile <- gsub("@watercycle@", "watercycledndc", setupfile)
    setupfile <- gsub("@pevapotrans@", "penman", setupfile)
    
    # Airchemistry module
    setupfile <- gsub("@airchemistry@", "airchemistrydndc", setupfile)
    
    # Physiology module
    setupfile <- gsub("@physiology@", "plamox", setupfile)
    setupfile <- gsub("@plantfamilies@", "crops grass", setupfile)
    
    # Soil modules and options
    setupfile <- gsub("@soilchemistry@", "metrx", setupfile)
    
    # Report
    setupfile <- gsub("@reportarable@", "<module id='output:report:arable' timemode='subdaily' />", setupfile)
    
    # Write the populated setup file as an xml-file
    writeLines(setupfile, con = file.path(settings$rundir, run.id, "setup.xml"))

  }
  
  # Setup file created for forest simulations
  else if(all(pfts_run %in% pfts_forest)){
    
    ## Modules
    # Microclimate module
    setupfile <- gsub("@microclimate@", "canopyecm", setupfile)
    
    # Watercycle module and option
    setupfile <- gsub("@watercycle@", "echy", setupfile)
    setupfile <- gsub("@pevapotrans@", "penman", setupfile)
    
    # Airchemistry module
    setupfile <- gsub("@airchemistry@", "airchemistrydndc", setupfile)
    
    # Physiology module
    setupfile <- gsub("@physiology@", "psim", setupfile)
    
    # Soil modules and options
    setupfile <- gsub("@soilchemistry@", "metrx", setupfile)
    
    # Report
    setupfile <- gsub("@reportarable@", "\n", setupfile)
    
    # Write the populated setup file as an xml-file
    writeLines(setupfile, con = file.path(settings$rundir, run.id, "setup.xml"))
    
    
  }
  
  # Given pfts were not among the supported species
  else{
    PEcAn.logger::logger.severe("Given species are not currently supported. This can be fixed by updating the write.config.LDNDC.R file.")
  }
  
  
  ## ----- Fetching other site specific file templates ----- ##
  
  ### Event, site and airchemistry files ###
  
  # Fetch event file from the given path, this might be modified, if initial
  # conditions are given, check the part of handling initial conditions later on
  eventsfile <- readLines(con = file.path(settings$run$inputs$events$path1))
  
  # Fetch default site file. Will also be populated based on the given initial conditions
  sitefile <- readLines(con = system.file("site_template.xml", package = "PEcAn.LDNDC"), n = -1)
  
  # Use airchemistry file, which represents Finland
  if(!is.null(settings$run$inputs$airchemistry$path1)){
    airchemistryfile <- readLines(con = file.path(settings$run$inputs$airchemistry$path1))
  } else{
    airchemistryfile <- readLines(con = system.file("airchemistry.txt", package = "PEcAn.LDNDC"), n = -1)
  }
  
  
  #### write run-specific PFT parameters here #### Get parameters being handled by PEcAn
  # For species, read the speciesparameters template
  speciesparfile <- readLines(con = system.file("speciesparameter_template.xml", package = "PEcAn.LDNDC"), n = -1)
  
  # For site (parameters), read the siteparameters template
  siteparfile <- readLines(con = system.file("siteparameters_template.xml", package = "PEcAn.LDNDC"), n = -1)
  
  
  #----------------------
    
  ## Set-up the necessary files in to the run directory so
  ## model is able to function properly. Later on, these
  ## files should be populated with initial values.
  
  # Species and Siteparameters
  b.2 <- ""
  h.2 <- ""
  
  species_par_values <- list()
  
  for (pft in seq_along(trait.values)) {
    
    pft.traits <- unlist(trait.values[[pft]])
    pft.names <- names(pft.traits)
    
    
    # Number at the beginning refers to the number of species parameters in LDNDC guide book.
    # NOTE! LDNDC Userguide has been updated later on so the numbering can be a little bit off compared
    # to the latest version.
    # First there is name in LDNDC and the second is name in BETY database
    
    
    #8 NDFLUSH -
    if ("ndflush" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NDFLUSH' value='", pft.traits[which(pft.names == "ndflush")], "' /> \n"), collapse="")
    }
    
    #9 NDMORTA -
    if ("ndmorta" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NDMORTA' value='", pft.traits[which(pft.names == "ndmorta")], "' /> \n"), collapse="")
    }
    
    #10 DLEAFSHED -
    if ("dleafshed" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='DLEAFSHED' value='", pft.traits[which(pft.names == "dleafshed")], "' /> \n"), collapse="")
    }
    
    #12 AEJM J/mol -
    if ("aejm" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='AEJM' value='", pft.traits[which(pft.names == "aejm")], "' /> \n"), collapse="")
    }
    
    #13 AEKC J/mol -
    if ("aekc" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='AEKC' value='", pft.traits[which(pft.names == "aekc")], "' /> \n"), collapse="")
    }
    
    #14 AEKO J/mol -
    if ("aeko" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='AEKO' value='", pft.traits[which(pft.names == "aeko")], "' /> \n"), collapse="")
    }
    
    #15 AERD J/mol -
    if ("aerd" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='AERD' value='", pft.traits[which(pft.names == "aerd")], "' /> \n"), collapse="")
    }
    
    #16 AEVC J/mol -
    if ("aevc" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='AEVC' value='", pft.traits[which(pft.names == "aevc")], "' /> \n"), collapse="")
    }
    
    #17 AEVO J/mol -
    if ("aevo" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='AEVO' value='", pft.traits[which(pft.names == "aevo")], "' /> \n"), collapse="")
    }
    
    #18 ALB (-) - SW_albedo (-)
    if ("SW_albedo" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='ALB' value='", pft.traits[which(pft.names == "SW_albedo")], "' /> \n"), collapse="")
    }
    
    #21 AMAXA (-) -
    if ("amaxa" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='AMAXA' value='", pft.traits[which(pft.names == "amaxa")], "' /> \n"), collapse="")
    }
    
    #22 AMAXB (-) - Amax (-)
    if ("Amax" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='AMAXB' value='", pft.traits[which(pft.names == "Amax")], "' /> \n"), collapse="")
    }
    
    #23 AMAXFRAC -
    if ("amaxfrac" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='AMAXFRAC' value='", pft.traits[which(pft.names == "amaxfrac")], "' /> \n"), collapse="")
    }
    
    #24 BASEFOLRESPFRAC -
    if ("basefolrespfrac" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='BASEFOLRESPFRAC' value='", pft.traits[which(pft.names == "basefolrespfrac")], "' /> \n"), collapse="")
    }
    
    #25 CB -
    if ("cb" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='CB' value='", pft.traits[which(pft.names == "cb")], "' /> \n"), collapse="")
    }
    
    #26 CDAMP -
    if ("cdamp" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='CDAMP' value='", pft.traits[which(pft.names == "cdamp")], "' /> \n"), collapse="")
    }
    
    #27 CL_P1 -
    if ("cl_p1" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='CL_P1' value='", pft.traits[which(pft.names == "cl_p1")], "' /> \n"), collapse="")
    }
    
    #28 CL_P2 -
    if ("cl_p2" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='CL_P2' value='", pft.traits[which(pft.names == "cl_p2")], "' /> \n"), collapse="")
    }
    
    #32 CELLULOSE -
    if ("cellulose" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='CELLULOSE' value='", pft.traits[which(pft.names == "cellulose")], "' /> \n"), collapse="")
    }
    
    #34 CHILL_UNITS - 
    if ("chill_units" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='CHILL_UNITS' value='", pft.traits[which(pft.names == "chill_units")], "' /> \n"), collapse="")
    }
    
    #35 CHILL_TEMP_MAX - 
    if ("chill_temp_max" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='CHILL_TEMP_MAX' value='", pft.traits[which(pft.names == "chill_temp_max")], "' /> \n"), collapse="")
    }
    
    #36 CT_IS -
    if ("ct_is" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='CT_IS' value='", pft.traits[which(pft.names == "ct_is")], "' /> \n"), collapse="")
    }
    
    #37 CT_MT -
    if ("ct_mt" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='CT_MT' value='", pft.traits[which(pft.names == "ct_mt")], "' /> \n"), collapse="")
    }
    
    #38 DBRANCH kg/m3 -
    if ("dbranch" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='DBRANCH' value='", pft.traits[which(pft.names == "dbranch")], "' /> \n"), collapse="")
    }
    
    #39 DF_EXP -
    if ("df_exp" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='DF_EXP' value='", pft.traits[which(pft.names == "df_exp")], "' /> \n"), collapse="")
    }
    
    #40 DF_LIMIT m2/ha -
    if ("df_limit" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='DF_LIMIT' value='", pft.traits[which(pft.names == "df_limit")], "' /> \n"), collapse="")
    }
    
    #41 DFOL - leaf_density
    if ("leaf_density" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='DFOL' value='", pft.traits[which(pft.names == "leaf_density")], "' /> \n"), collapse="")
    }
    
    #42 DFRTOPT - 
    if ("dfrtopt" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='DFRTOPT' value='", pft.traits[which(pft.names == "dfrtopt")], "' /> \n"), collapse="")
    }
    
    #43 DIAMMAX (m) - stem_diameter (cm)
    if ("stem_diameter" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='DIAMMAX' value='",
                               PEcAn.utils::ud_convert(
                                 pft.traits[which(pft.names == "stem_diameter")], "m", "cm"
                               ),"' /> \n"), collapse="")
    }
    
    #44 DOC_RESP_RATIO - coarseRootExudation
    if ("coarseRootExudation" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t<par name='DOC_RESP_RATIO' value='", pft.traits[which(pft.names == "coarseRootExudation")], "' /> \n"), collapse="")
    }
    
    #45 DRAGC -
    if ("dragc" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t<par name='DRAGC' value='", pft.traits[which(pft.names == "dragc")], "' /> \n"), collapse="")
    }
    
    #46 DSAP -
    if ("dsap" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t<par name='DSAP' value='", pft.traits[which(pft.names == "dsap")], "' /> \n"), collapse="")
    }
    
    #47 DS_IS J/mol K -
    if ("ds_is" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t<par name='DS_IS' value='", pft.traits[which(pft.names == "ds_is")], "' /> \n"), collapse="")
    }
    
    #48 DS_MT J/mol K -
    if ("ds_mt" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t<par name='DS_MT' value='", pft.traits[which(pft.names == "ds_mt")], "' /> \n"), collapse="")
    }
    
    #49 DVPD1 -
    if ("dvpd1" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t<par name='DVPD1' value='", pft.traits[which(pft.names == "dvpd1")], "' /> \n"), collapse="")
    }
    
    #50 DVPD2 -
    if ("dvpd2" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t<par name='DVPD2' value='", pft.traits[which(pft.names == "dvpd2")], "' /> \n"), collapse="")
    }
    
    #54 EF_OVOC ug/gDW h -
    if ("ef_ovoc" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t<par name='EF_OVOC' value='", pft.traits[which(pft.names == "ef_ovoc")], "' /> \n"), collapse="")
    }
    
    #55 EXPL_NH4 -
    if ("expl_nh4" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t<par name='EXPL_NH4' value='", pft.traits[which(pft.names == "expl_nh4")], "' /> \n"), collapse="")
    }
    
    #56 EXPL_NO3 -
    if ("expl_no3" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t<par name='EXPL_NO3' value='", pft.traits[which(pft.names == "expl_no3")], "' /> \n"), collapse="")
    }
    
    #57 EXP_ROOT_DISTRIBUTION - 
    if ("exp_root_distribution" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t\t<par name='EXP_ROOT_DISTRIBUTION' value='", pft.traits[which(pft.names == "exp_root_distribution")], "' /> \n"), collapse="")
    }
    
    #58 EXT - extinction_coefficient_diffuse
    if ("extinction_coefficient_diffuse" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='EXT' value='", pft.traits[which(pft.names == "extinction_coefficient_diffuse")], "' /> \n"), collapse="")
    }
    
    #59 FAGE -
    if ("fage" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='FAGE' value='", pft.traits[which(pft.names == "fage")], "' /> \n"), collapse="")
    }
    
    #62 FFACMAX -
    if ("ffacmax" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='FFACMAX' value='", pft.traits[which(pft.names == "ffacmax")], "' /> \n"), collapse="")
    }
    
    #78 FOLRELGROMAX -
    if ("folrelgromax" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='FOLRELGROMAX' value='", pft.traits[which(pft.names == "folrelgromax")], "' /> \n"), collapse="")
    }
    
    #79 FRACTION_ROOT - root_biomass_fraction
    if ("root_biomass_fraction" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='FRACTION_ROOT' value='", pft.traits[which(pft.names == "root_biomass_fraction")], "' /> \n"), collapse="")
    }
    
    #80 FRACTION_FRUIT - 
    if ("fraction_fruit" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='FRACTION_FRUIT' value='", pft.traits[which(pft.names == "fraction_fruit")], "' /> \n"), collapse="")
    }
    
    #81 FRACTION_FOLIAGE - 
    if ("fraction_foliage" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='FRACTION_FOLIAGE' value='", pft.traits[which(pft.names == "fraction_foliage")], "' /> \n"), collapse="")
    }
    
    #82 FRET_N - 
    if ("fret_n" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='FRET_N' value='", pft.traits[which(pft.names == "fret_n")], "' /> \n"), collapse="")
    }
    
    #86 FRTALLOC_REL -
    if ("frtalloc_rel" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='FRTALLOC_REL' value='", pft.traits[which(pft.names == "frtalloc_rel")], "' /> \n"), collapse="")
    }
    
    #87 FRTLOSS_SCALE -
    if ("frtloss_scale" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='FRTLOSS_SCALE' value='", pft.traits[which(pft.names == "frtloss_scale")], "' /> \n"), collapse="")
    }
    
    #88 FYIELD - 
    if ("fyield" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='FYIELD' value='", pft.traits[which(pft.names == "fyield")], "' /> \n"), collapse="")
    }
    
    #89 GDD_BASE_TEMPERATURE (C) - gdd_tbase (C)
    if ("gdd_tbase" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GDD_BASE_TEMPERATURE' value='", pft.traits[which(pft.names == "gdd_tbase")], "' /> \n"), collapse="")
    }
    
    #90 GDD_MAX_TEMPERATURE - gdd_tmax
    if ("gdd_tmax" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GDD_MAX_TEMPERATURE' value='", pft.traits[which(pft.names == "gdd_tmax")], "' /> \n"), collapse="")
    }
    
    #91 GDD_EMERGENCE -
    if ("gdd_emergence" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GDD_EMERGENCE' value='", pft.traits[which(pft.names == "gdd_emergence")], "' /> \n"), collapse="")
    }
    
    #92 GDD_STEM_ELONGATION -
    if ("gdd_stem_elongation" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GDD_STEM_ELONGATION' value='", pft.traits[which(pft.names == "gdd_stem_elongation")], "' /> \n"), collapse="")
    }
    
    #93 GDD_FLOWERING - 
    if ("gdd_flowering" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GDD_FLOWERING' value='", pft.traits[which(pft.names == "gdd_flowering")], "' /> \n"), collapse="")
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GDD_GRAIN_FILLING' value='", pft.traits[which(pft.names == "gdd_flowering")] + pft.traits[which(pft.names == "gdd_grain_filling")], "' /> \n"), collapse="")
    }
    
    # #94 GDD_GRAIN_FILLING - GRAIN FILLING RELATIVE TO FLOWERING
    # if ("gdd_grain_filling" %in% pft.names) {
    #   b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GDD_GRAIN_FILLING' value='", pft.traits[which(pft.names == "gdd_grain_filling")], "' /> \n"), collapse="")
    # }
    
    #95 GDD_MATURITY - 
    if ("gdd_maturity" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GDD_MATURITY' value='", pft.traits[which(pft.names == "gdd_maturity")], "' /> \n"), collapse="")
    }
    
    #96 GDDFOLEND -
    if ("gddfolend" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GDDFOLEND' value='", pft.traits[which(pft.names == "gddfolend")], "' /> \n"), collapse="")
    }
    
    #97 GDDFOLSTART -
    if ("gddfolstart" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GDDFOLSTART' value='", pft.traits[which(pft.names == "gddfolstart")], "' /> \n"), collapse="")
    }
    
    #100 GGDPS_B (umol L-1 s-1) -
    if ("ggdps_b" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GGDPS_B' value='", pft.traits[which(pft.names == "ggdps_b")], "' /> \n"), collapse="")
    }
    
    #101 GSMAX (mmolH2O m-2 s-1) -
    if ("gsmax" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GSMAX' value='", pft.traits[which(pft.names == "gsmax")], "' /> \n"), collapse="")
    }
    
    #102 GSMIN (mmolH2O m-2 s-1) -
    if ("gsmin" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GSMIN' value='", pft.traits[which(pft.names == "gsmin")], "' /> \n"), collapse="")
    }
    
    #103 GZRTZ -
    if ("gzrtz" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='GZRTZ' value='", pft.traits[which(pft.names == "gzrtz")], "' /> \n"), collapse="")
    }
    
    #104 H2OREF_A - 
    if ("h2oref_a" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='H2OREF_A' value='", pft.traits[which(pft.names == "h2oref_a")], "' /> \n"), collapse="")
    }
    
    #107 H2OREF_GS - 
    if ("h2oref_gs" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='H2OREF_GS' value='", pft.traits[which(pft.names == "h2oref_gs")], "' /> \n"), collapse="")
    }
    
    #109 HALFSAT - 
    if ("halfsat" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='HALFSAT' value='", pft.traits[which(pft.names == "halfsat")], "' /> \n"), collapse="")
    }
    
    #110 HA_IS (J mol-1) - 
    if ("ha_is" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='HA_IS' value='", pft.traits[which(pft.names == "ha_is")], "' /> \n"), collapse="")
    }
    
    #111 HA_MT (J mol-1) - 
    if ("ha_mt" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='HA_MT' value='", pft.traits[which(pft.names == "ha_mt")], "' /> \n"), collapse="")
    }
    
    #112 HD_IS (J mol-1) - 
    if ("hd_is" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='HD_IS' value='", pft.traits[which(pft.names == "hd_is")], "' /> \n"), collapse="")
    }
    
    #113 HDJ - 
    if ("hdj" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='HDJ' value='", pft.traits[which(pft.names == "hdj")], "' /> \n"), collapse="")
    }
    
    #114 HD_EXP - 
    if ("hd_exp" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='HD_EXP' value='", pft.traits[which(pft.names == "hd_exp")], "' /> \n"), collapse="")
    }
    
    #115 HD_MAX (m m-1) - 
    if ("hd_max" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='HD_MAX' value='", pft.traits[which(pft.names == "hd_max")], "' /> \n"), collapse="")
    }
    
    #116 HD_MIN (m m-1) - 
    if ("hd_min" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='HD_MIN' value='", pft.traits[which(pft.names == "hd_min")], "' /> \n"), collapse="")
    }
    
    #117 HD_MT (J mol-1) - 
    if ("hd_mt" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='HD_MT' value='", pft.traits[which(pft.names == "hd_mt")], "' /> \n"), collapse="")
    }
    
    #118 HREF - 
    if ("href" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='HREF' value='", pft.traits[which(pft.names == "href")], "' /> \n"), collapse="")
    }
    
    #119 INI_N_FIX - 
    if ("ini_n_fix" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='INI_N_FIX' value='", pft.traits[which(pft.names == "ini_n_fix")], "' /> \n"), collapse="")
    }
    
    #120 KC25 (mmol mol-1 mbar-1)- 
    if ("kc25" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='KC25' value='", pft.traits[which(pft.names == "kc25")], "' /> \n"), collapse="")
    }
    
    #121 KM20 - 
    if ("km20" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='KM20' value='", pft.traits[which(pft.names == "km20")], "' /> \n"), collapse="")
    }
    
    #126 K_MM_NITROGEN_UPTAKE - 
    if ("k_mm_nitrogen_uptake" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='K_MM_NITROGEN_UPTAKE' value='", pft.traits[which(pft.names == "k_mm_nitrogen_uptake")], "' /> \n"), collapse="")
    }
    
    #127 KO25 - 
    if ("ko25" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='KO25' value='", pft.traits[which(pft.names == "ko25")], "' /> \n"), collapse="")
    }
    
    #128 KRC_WOOD - 
    if ("krc_wood" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='KRC_WOOD' value='", pft.traits[which(pft.names == "krc_wood")], "' /> \n"), collapse="")
    }
    
    #129 LIGNIN - 
    if ("lignin" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='LIGNIN' value='", pft.traits[which(pft.names == "lignin")], "' /> \n"), collapse="")
    }
    
    #130 MAINTENANCE_TEMP_REF - 
    if ("maintenance_temp_ref" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='MAINTENANCE_TEMP_REF' value='", pft.traits[which(pft.names == "maintenance_temp_ref")], "' /> \n"), collapse="")
    }
    
    #131 MC_LEAF -
    if ("mc_leaf" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='MC_LEAF' value='", pft.traits[which(pft.names == "mc_leaf")], "' /> \n"), collapse="")
    }
    
    #132 MC_STEM -
    if ("mc_stem" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='MC_STEM' value='", pft.traits[which(pft.names == "mc_stem")], "' /> \n"), collapse="")
    }
    
    #133 MC_ROOT - 
    if ("mc_root" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='MC_ROOT' value='", pft.traits[which(pft.names == "mc_root")], "' /> \n"), collapse="")
    }
    
    #134 MC_STORAGE -
    if ("mc_storage" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='MC_STORAGE' value='", pft.traits[which(pft.names == "mc_storage")], "' /> \n"), collapse="")
    }
    
    #135 MFOLOPT -
    if ("mfolopt" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='MFOLOPT' value='", pft.traits[which(pft.names == "mfolopt")], "' /> \n"), collapse="")
    }
    
    #136 M_FRUIT_OPT -
    if ("m_fruit_opt" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='M_FRUIT_OPT' value='", pft.traits[which(pft.names == "m_fruit_opt")], "' /> \n"), collapse="")
    }
    
    #139 MUE_IS (s-1) -
    if ("mue_is" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='MUE_IS' value='", pft.traits[which(pft.names == "mue_is")], "' /> \n"), collapse="")
    }
    
    #140 MUE_MT (s-1) -
    if ("mue_mt" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='MUE_MT' value='", pft.traits[which(pft.names == "mue_mt")], "' /> \n"), collapse="")
    }
    
    #141 MWFM -
    if ("mwfm" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='MWFM' value='", pft.traits[which(pft.names == "mwfm")], "' /> \n"), collapse="")
    }
    
    #143 NC_FOLIAGE_MIN -
    if ("nc_foliage_min" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NC_FOLIAGE_MIN' value='", pft.traits[which(pft.names == "nc_foliage_min")], "' /> \n"), collapse="")
    }
    
    #144 NC_FOLIAGE_MAX -
    if ("nc_foliage_max" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NC_FOLIAGE_MAX' value='", pft.traits[which(pft.names == "nc_foliage_max")], "' /> \n"), collapse="")
    }
    
    #145 NCFOLOPT (kg kg-1) -
    if ("ncfolopt" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NCFOLOPT' value='", pft.traits[which(pft.names == "ncfolopt")], "' /> \n"), collapse="")
    }
    
    #146 NC_FINEROOTS_MAX - 
    if ("nc_fineroots_max" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NC_FINEROOTS_MAX' value='", pft.traits[which(pft.names == "nc_fineroots_max")], "' /> \n"), collapse="")
    }
    
    #147 NC_FINEROOTS_MIN - 
    if ("nc_fineroots_min" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NC_FINEROOTS_MIN' value='", pft.traits[which(pft.names == "nc_fineroots_min")], "' /> \n"), collapse="")
    }
    
    #148 NC_FRUIT_MAX - 
    if ("nc_fruit_max" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NC_FRUIT_MAX' value='", pft.traits[which(pft.names == "nc_fruit_max")], "' /> \n"), collapse="")
    }
    
    #149 NC_FRUIT_MIN - 
    if ("nc_fruit_min" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NC_FRUIT_MIN' value='", pft.traits[which(pft.names == "nc_fruit_min")], "' /> \n"), collapse="")
    }
    
    #150 NC_STRUCTURAL_TISSUE_MAX - 
    if ("nc_structural_tissue_max" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NC_STRUCTURAL_TISSUE_MAX' value='", pft.traits[which(pft.names == "nc_structural_tissue_max")], "' /> \n"), collapse="")
    }
    
    #151 NC_STRUCTURAL_TISSUE_MIN - 
    if ("nc_structural_tissue_min" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NC_STRUCTURAL_TISSUE_MIN' value='", pft.traits[which(pft.names == "nc_structural_tissue_min")], "' /> \n"), collapse="")
    }
    
    #152 NCSAPOPT (kg kg-1) -
    if ("ncsapopt" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NCSAPOPT' value='", pft.traits[which(pft.names == "ncsapopt")], "' /> \n"), collapse="")
    }
    
    #153 N_DEF_FACTOR -
    if ("n_def_factor" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='N_DEF_FACTOR' value='", pft.traits[which(pft.names == "n_def_factor")], "' /> \n"), collapse="")
    }
    
    #154 N_DEMAND_VEG -
    if ("n_demand_veg" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='N_DEMAND_VEG' value='", pft.traits[which(pft.names == "n_demand_veg")], "' /> \n"), collapse="")
    }
    
    #155 N_DEMAND_REPROD -
    if ("n_demand_reprod" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='N_DEMAND_REPROD' value='", pft.traits[which(pft.names == "n_demand_reprod")], "' /> \n"), collapse="")
    }
    
    #156 NFIX_CEFF -
    if ("nfix_ceff" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NFIX_CEFF' value='", pft.traits[which(pft.names == "nfix_ceff")], "' /> \n"), collapse="")
    }
    
    #157 NFIX_TMAX -
    if ("nfix_tmax" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NFIX_TMAX' value='", pft.traits[which(pft.names == "nfix_tmax")], "' /> \n"), collapse="")
    }
    
    #158 NFIX_TOPT -
    if ("nfix_topt" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NFIX_TOPT' value='", pft.traits[which(pft.names == "nfix_topt")], "' /> \n"), collapse="")
    }
    
    #159 NFIX_TMIN -
    if ("nfix_tmin" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NFIX_TMIN' value='", pft.traits[which(pft.names == "nfix_tmin")], "' /> \n"), collapse="")
    }
    
    #160 NFIX_W -
    if ("nfix_w" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NFIX_W' value='", pft.traits[which(pft.names == "nfix_w")], "' /> \n"), collapse="")
    }
    
    #161 NFIX_RATE (kg N kg-1 DM-1 d-1) -
    if ("nfix_rate" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='NFIX_RATE' value='", pft.traits[which(pft.names == "nfix_rate")], "' /> \n"), collapse="")
    }
    
    #163 PEXS - 
    if ("pexs" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='PEXS' value='", pft.traits[which(pft.names == "pexs")], "' /> \n"), collapse="")
    }
    
    #164 PFL - 
    if ("pfl" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='PFL' value='", pft.traits[which(pft.names == "pfl")], "' /> \n"), collapse="")
    }
    
    #165 PSL - 
    if ("psl" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='PSL' value='", pft.traits[which(pft.names == "psl")], "' /> \n"), collapse="")
    }
    
    #167 PSNTMAX (C) -  pstemp_max (C)
    if ("pstemp_max" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='PSNTMAX' value='", pft.traits[which(pft.names == "pstemp_max")], "' /> \n"), collapse="")
    }
    
    #168 PSNTMIN (C) -  pstemp_min (C)
    if ("pstemp_min" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='PSNTMIN' value='", pft.traits[which(pft.names == "pstemp_min")], "' /> \n"), collapse="")
    }
    
    #169 PSNTOPT (C) -  psnTOpt (C)
    if ("psnTOpt" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='PSNTOPT' value='", pft.traits[which(pft.names == "psnTOpt")], "' /> \n"), collapse="")
    }
    
    #170 QHRD -
    if ("qhrd" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='QHRD' value='", pft.traits[which(pft.names == "qhrd")], "' /> \n"), collapse="")
    }
    
    #171 QJVC -
    if ("qjvc" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='QJVC' value='", pft.traits[which(pft.names == "qjvc")], "' /> \n"), collapse="")
    }
    
    #172 QRD25 (umol m-2 s-1) -
    if ("qrd25" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='QRD25' value='", pft.traits[which(pft.names == "qrd25")], "' /> \n"), collapse="")
    }
    
    #173 QRF -
    if ("qrf" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='QRF' value='", pft.traits[which(pft.names == "qrf")], "' /> \n"), collapse="")
    }
    
    #174 QSF_P1 (m2 cm-2) -
    if ("qsf_p1" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='QSF_P1' value='", pft.traits[which(pft.names == "qsf_p1")], "' /> \n"), collapse="")
    }
    
    #175 QSF_P2 (m2 cm-2) -
    if ("qsf_p2" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='QSF_P2' value='", pft.traits[which(pft.names == "qsf_p2")], "' /> \n"), collapse="")
    }
    
    #176 QVOVC -
    if ("qvovc" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='QVOVC' value='", pft.traits[which(pft.names == "qvovc")], "' /> \n"), collapse="")
    }
    
    #177 QWODFOLMIN -
    if ("qwodfolmin" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='QWODFOLMIN' value='", pft.traits[which(pft.names == "qwodfolmin")], "' /> \n"), collapse="")
    }
    
    #178 RBUDDEM -
    if ("rbuddem" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='RBUDDEM' value='", pft.traits[which(pft.names == "rbuddem")], "' /> \n"), collapse="")
    }
    
    
    #179 RESP -  resp
    if ("resp" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='RESP' value='", pft.traits[which(pft.names == "resp")], "' /> \n"), collapse="")
    }
    
    #180 RESPQ10 - leaf_respiration_Q10
    if ("leaf_respiration_Q10" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='RESPQ10' value='", pft.traits[which(pft.names == "leaf_respiration_Q10")], "' /> \n"), collapse="")
    }
    
    #181 ROOTMRESPFRAC -
    if ("rootmrespfrac" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='ROOTMRESPFRAC' value='", pft.traits[which(pft.names == "rootmrespfrac")], "' /> \n"), collapse="")
    }
    
    #182 RS_CONDUCT -
    if ("rs_conduct" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='RS_CONDUCT' value='", pft.traits[which(pft.names == "rs_conduct")], "' /> \n"), collapse="")
    }
    
    #183 SCALE_I -
    if ("scale_i" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SCALE_I' value='", pft.traits[which(pft.names == "scale_i")], "' /> \n"), collapse="")
    }
    
    #184 SCALE_M -
    if ("scale_m" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SCALE_M' value='", pft.traits[which(pft.names == "scale_m")], "' /> \n"), collapse="")
    }
    
    #185 SDJ - 
    if ("sdj" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SDJ' value='", pft.traits[which(pft.names == "sdj")], "' /> \n"), collapse="")
    }
    
    #186 SENESCENCE_AGE - 
    if ("senescence_age" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SENESCENCE_AGE' value='", pft.traits[which(pft.names == "senescence_age")], "' /> \n"), collapse="")
    }
    
    #187 SENESCENCE_DROUGHT - 
    if ("senescence_drought" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SENESCENCE_DROUGHT' value='", pft.traits[which(pft.names == "senescence_drought")], "' /> \n"), collapse="") # In order to have zeros
    }
    
    #188 SENESCENCE_FROST - 
    if ("senescence_frost" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SENESCENCE_FROST' value='", pft.traits[which(pft.names == "senescence_frost")], "' /> \n"), collapse="")
    }
    
    #?? SENESCSTART - 
    if ("senescstart" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SENESCSTART' value='", pft.traits[which(pft.names == "senescstart")], "' /> \n"), collapse="")
    }
    
    #192 SHOOT_STIMULATION_REPROD - 
    if ("shoot_stimulation_reprod" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SHOOT_STIMULATION_REPROD' value='", pft.traits[which(pft.names == "shoot_stimulation_reprod")], "' /> \n"), collapse="")
    }
    
    #193 SLAMAX (m2 kg-1) -  SLAMAX (m2 kg-1)
    if ("SLAMAX" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SLAMAX' value='", pft.traits[which(pft.names == "SLAMAX")], "' /> \n"), collapse="")
    }
    
    #194 SLAMIN (m2 kg-1) -  SLAMIN (m2 kg-1)
    if ("SLAMIN" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SLAMIN' value='", pft.traits[which(pft.names == "SLAMIN")], "' /> \n"), collapse="")
    }
    
    #195 SLADECLINE -
    if ("sladecline" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SLADECLINE' value='", pft.traits[which(pft.names == "sladecline")], "' /> \n"), collapse="")
    }
    
    #196 SLOPE_GSA -
    if ("slope_gsa" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SLOPE_GSA' value='", pft.traits[which(pft.names == "slope_gsa")], "' /> \n"), collapse="")
    }
    
    #197 SLOPE_GSCO2 -
    if ("slope_gsco2" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SLOPE_GSCO2' value='", pft.traits[which(pft.names == "slope_gsco2")], "' /> \n"), collapse="")
    }
    
    #198 SLOPE_GSH2O -
    if ("slope_gsh2o" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SLOPE_GSH2O' value='", pft.traits[which(pft.names == "slope_gsh2o")], "' /> \n"), collapse="")
    }
    
    #199 SLOPE_NC -
    if ("slope_nc" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='SLOPE_NC' value='", pft.traits[which(pft.names == "slope_nc")], "' /> \n"), collapse="")
    }
    
    #200 TAP_P1 -
    if ("tap_p1" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='TAP_P1' value='", pft.traits[which(pft.names == "tap_p1")], "' /> \n"), collapse="")
    }
    
    #201 TAP_P2 -
    if ("tap_p2" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='TAP_P2' value='", pft.traits[which(pft.names == "tap_p2")], "' /> \n"), collapse="")
    }
    
    #202 TAP_P3 -
    if ("tap_p3" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='TAP_P3' value='", pft.traits[which(pft.names == "tap_p3")], "' /> \n"), collapse="")
    }
    
    #203 TAU -
    if ("tau" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='TAU' value='", pft.traits[which(pft.names == "tau")], "' /> \n"), collapse="")
    }
    
    #204 THETA -
    if ("theta" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='THETA' value='", pft.traits[which(pft.names == "theta")], "' /> \n"), collapse="")
    }
    
    #205 TLIMIT - 
    if ("tlimit" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='TLIMIT' value='", pft.traits[which(pft.names == "tlimit")], "' /> \n"), collapse="")
    }
    
    #206 TOFRTBAS - 
    if ("tofrtbas" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='TOFRTBAS' value='", pft.traits[which(pft.names == "tofrtbas")], "' /> \n"), collapse="")
    }
    
    #207 TOSAPMAX - 
    if ("tosapmax" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='TOSAPMAX' value='", pft.traits[which(pft.names == "tosapmax")], "' /> \n"), collapse="")
    }
    
    #208 UCMAX (kgN m-2 leaf area) - 
    if ("ucmax" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='UCMAX' value='", pft.traits[which(pft.names == "ucmax")], "' /> \n"), collapse="")
    }
    
    #210 US_NH4 (kgN kg-1 fine root dry weight day-1) - 
    if ("us_nh4" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='US_NH4' value='", pft.traits[which(pft.names == "us_nh4")], "' /> \n"), collapse="")
    }
    
    #211 US_NH4MYC (kgN kg-1 fine root dry weight day-1) - 
    if ("us_nh4myc" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='US_NH4MYC' value='", pft.traits[which(pft.names == "us_nh4myc")], "' /> \n"), collapse="")
    }
    
    #213 US_NO3 (kgN kg-1 fine root dry weight day-1) - 
    if ("us_no3" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='US_NO3' value='", pft.traits[which(pft.names == "us_no3")], "' /> \n"), collapse="")
    }
    
    #214 US_NO3MYC (kgN kg-1 fine root dry weight day-1) - 
    if ("us_no3myc" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='US_NO3MYC' value='", pft.traits[which(pft.names == "us_no3myc")], "' /> \n"), collapse="")
    }
    
    #215 VCFACT - 
    if ("vcfact" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='VCFACT' value='", pft.traits[which(pft.names == "vcfact")], "' /> \n"), collapse="")
    }
    
    #216 VCMAX25 - 
    if ("vcmax25" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='VCMAX25' value='", pft.traits[which(pft.names == "vcmax25")], "' /> \n"), collapse="")
    }
    
    #217 VPDREF (kPa) - 
    if ("vpdref" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='VPDREF' value='", pft.traits[which(pft.names == "vpdref")], "' /> \n"), collapse="")
    }
    
    #219 WOODMRESPA - 
    if ("woodmrespa" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='WOODMRESPA' value='", pft.traits[which(pft.names == "woodmrespa")], "' /> \n"), collapse="")
    }
    
    #220 WUECMAX - wuecmax
    if ("wuecmax" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='WUECMAX' value='", pft.traits[which(pft.names == "wuecmax")], "' /> \n"), collapse="")
    }
    
    #221 WUECMIN - wuecmin
    if ("wuecmin" %in% pft.names) { # CHECK THAT THE VALUE IS NOT OVER MAX
      wuecmin_val <- ifelse(pft.traits[which(pft.names == "wuecmin")] > pft.traits[which(pft.names == "wuecmax")], pft.traits[which(pft.names == "wuecmax")], pft.traits[which(pft.names == "wuecmin")])
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='WUECMIN' value='", wuecmin_val, "' /> \n"), collapse="")
    }
    
    #222 ZRTMC - 
    if ("zrtmc" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='ZRTMC' value='", pft.traits[which(pft.names == "zrtmc")], "' /> \n"), collapse="")
    }
    
    #??? HEIGHT_MAX - 
    if ("height_max" %in% pft.names) {
      b.2 <- paste(b.2, paste0("\t\t\t\t<par name='HEIGHT_MAX' value='", pft.traits[which(pft.names == "height_max")], "' /> \n"), collapse="")
    }
    
    ## SITEPARAMETERS
    # Number at the beginning refers to the number of site parameters in LDNDC guide book.
    
    
    #58 EVALIM
    if ("evalim" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='EVALIM' value='", pft.traits[which(pft.names == "evalim")], "' /> \n"), collapse="")
    }
    
    #82 GROUNDWATER_NUTRIENT_RENEWAL - 
    if ("groundwater_nutrient_renewal" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='GROUNDWATER_NUTRIENT_RENEWAL' value='", pft.traits[which(pft.names == "groundwater_nutrient_renewal")], "' /> \n"), collapse="")
    }
    
    #121 METRX_AMAX - 
    if ("metrx_amax" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_AMAX' value='", pft.traits[which(pft.names == "metrx_amax")], "' /> \n"), collapse="")
    }
    
    #122 METRX_AMAX_ALGAE - 
    if ("metrx_amax_algae" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_AMAX_ALGAE' value='", pft.traits[which(pft.names == "metrx_amax_algae")], "' /> \n"), collapse="")
    }
    
    #123 METRX_BETA_LITTER_TYPE - 
    if ("metrx_beta_litter_type" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_BETA_LITTER_TYPE' value='", pft.traits[which(pft.names == "metrx_beta_litter_type")], "' /> \n"), collapse="")
    }
    
    #124 METRX_BIOSYNTH_EFF - 
    if ("metrx_biosynth_eff" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_BIOSYNTH_EFF' value='", pft.traits[which(pft.names == "metrx_biosynth_eff")], "' /> \n"), collapse="")
    }
    
    #125 METRX_CN_ALGAE - 
    if ("metrx_cn_algae" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_CN_ALGAE' value='", pft.traits[which(pft.names == "metrx_cn_algae")], "' /> \n"), collapse="")
    }
    
    #127 METRX_CN_MIC_MAX - 
    if ("metrx_cn_mic_max" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_CN_MIC_MAX' value='", pft.traits[which(pft.names == "metrx_cn_mic_max")], "' /> \n"), collapse="")
    }
    
    #128 METRX_CN_MIC_MIN - 
    if ("metrx_cn_mic_min" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_CN_MIC_MIN' value='", pft.traits[which(pft.names == "metrx_cn_mic_min")], "' /> \n"), collapse="")
    }
    
    #129 METRX_CO2_PROD_DECOMP - 
    if ("metrx_co2_prod_decomp" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_CO2_PROD_DECOMP' value='", pft.traits[which(pft.names == "metrx_co2_prod_decomp")], "' /> \n"), collapse="")
    }
    
    #130 METRX_D_EFF_REDUCTION - 
    if ("metrx_d_eff_reduction" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_D_EFF_REDUCTION' value='", pft.traits[which(pft.names == "metrx_d_eff_reduction")], "' /> \n"), collapse="")
    }
    
    #131 METRX_F_CHEMODENIT_PH_ONEILL_1 - 
    if ("metrx_f_chemodenit_ph_oneill_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_CHEMODENIT_PH_ONEILL_1' value='", pft.traits[which(pft.names == "metrx_f_chemodenit_ph_oneill_1")], "' /> \n"), collapse="")
    }
    
    #132 METRX_F_CHEMODENIT_PH_ONEILL_2 - 
    if ("metrx_f_chemodenit_ph_oneill_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_CHEMODENIT_PH_ONEILL_2' value='", pft.traits[which(pft.names == "metrx_f_chemodenit_ph_oneill_2")], "' /> \n"), collapse="")
    }
    
    #133 METRX_F_CHEMODENIT_PH_ONEILL_3 - 
    if ("metrx_f_chemodenit_ph_oneill_3" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_CHEMODENIT_PH_ONEILL_3' value='", pft.traits[which(pft.names == "metrx_f_chemodenit_ph_oneill_3")], "' /> \n"), collapse="")
    }
    
    #134 METRX_F_CHEMODENIT_T_EXP_1 - 
    if ("metrx_f_chemodenit_t_exp_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_CHEMODENIT_T_EXP_1' value='", pft.traits[which(pft.names == "metrx_f_chemodenit_t_exp_1")], "' /> \n"), collapse="")
    }
    
    #135 METRX_F_CHEMODENIT_T_EXP_2 - 
    if ("metrx_f_chemodenit_t_exp_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_CHEMODENIT_T_EXP_2' value='", pft.traits[which(pft.names == "metrx_f_chemodenit_t_exp_2")], "' /> \n"), collapse="")
    }
    
    #136 METRX_F_DECOMP_M_WEIBULL_1 - 
    if ("metrx_f_decomp_m_weibull_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DECOMP_M_WEIBULL_1' value='", pft.traits[which(pft.names == "metrx_f_decomp_m_weibull_1")], "' /> \n"), collapse="")
    }
    
    #137 METRX_F_DECOMP_M_WEIBULL_2 - 
    if ("metrx_f_decomp_m_weibull_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DECOMP_M_WEIBULL_2' value='", pft.traits[which(pft.names == "metrx_f_decomp_m_weibull_2")], "' /> \n"), collapse="")
    }
    
    #138 METRX_F_DECOMP_M_WEIBULL_3 - 
    if ("metrx_f_decomp_m_weibull_3" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DECOMP_M_WEIBULL_3' value='", pft.traits[which(pft.names == "metrx_f_decomp_m_weibull_3")], "' /> \n"), collapse="")
    }
    
    #139 METRX_F_CH4_OXIDATION_T_EXP_1 -
    if ("metrx_f_ch4_oxidation_t_exp_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_CH4_OXIDATION_T_EXP_1' value='", pft.traits[which(pft.names == "metrx_f_ch4_oxidation_t_exp_1")], "' /> \n"), collapse="")
    }
    
    #140 METRX_F_CH4_OXIDATION_T_EXP_2 -
    if ("metrx_f_ch4_oxidation_t_exp_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_CH4_OXIDATION_T_EXP_2' value='", pft.traits[which(pft.names == "metrx_f_ch4_oxidation_t_exp_2")], "' /> \n"), collapse="")
    }
    
    #141 METRX_F_CH4_PRODUCTION_T_EXP_1 - 
    if ("metrx_f_ch4_production_t_exp_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_CH4_PRODUCTION_T_EXP_1' value='", pft.traits[which(pft.names == "metrx_f_ch4_production_t_exp_1")], "' /> \n"), collapse="")
    }
    
    #142 METRX_F_CH4_PRODUCTION_T_EXP_2 - 
    if ("metrx_f_ch4_production_t_exp_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_CH4_PRODUCTION_T_EXP_2' value='", pft.traits[which(pft.names == "metrx_f_ch4_production_t_exp_2")], "' /> \n"), collapse="")
    }
    
    #143 METRX_F_DECOMP_T_EXP_1 - 
    if ("metrx_f_decomp_t_exp_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DECOMP_T_EXP_1' value='", pft.traits[which(pft.names == "metrx_f_decomp_t_exp_1")], "' /> \n"), collapse="")
    }
    
    #144 METRX_F_DECOMP_T_EXP_2 - 
    if ("metrx_f_decomp_t_exp_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DECOMP_T_EXP_2' value='", pft.traits[which(pft.names == "metrx_f_decomp_t_exp_2")], "' /> \n"), collapse="")
    }
    
    #145 METRX_F_DECOMP_CLAY_1 - 
    if ("metrx_f_decomp_clay_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DECOMP_CLAY_1' value='", pft.traits[which(pft.names == "metrx_f_decomp_clay_1")], "' /> \n"), collapse="")
    }
    
    #146 METRX_F_DECOMP_CLAY_2 - 
    if ("metrx_f_decomp_clay_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DECOMP_CLAY_2' value='", pft.traits[which(pft.names == "metrx_f_decomp_clay_2")], "' /> \n"), collapse="")
    }
    
    #147 METRX_F_DENIT_N2_MIN - 
    if ("metrx_f_denit_n2_min" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DENIT_N2_MIN' value='", pft.traits[which(pft.names == "metrx_f_denit_n2_min")], "' /> \n"), collapse="")
    }
    
    #148 METRX_F_DENIT_N2_MAX - 
    if ("metrx_f_denit_n2_max" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DENIT_N2_MAX' value='", pft.traits[which(pft.names == "metrx_f_denit_n2_max")], "' /> \n"), collapse="")
    }
    
    #149 METRX_F_DENIT_NO - 
    if ("metrx_f_denit_no" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DENIT_NO' value='", pft.traits[which(pft.names == "metrx_f_denit_no")], "' /> \n"), collapse="")
    }
    
    #150 METRX_F_DENIT_PH_EXP_1 - 
    if ("metrx_f_denit_ph_exp_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DENIT_PH_EXP_1' value='", pft.traits[which(pft.names == "metrx_f_denit_ph_exp_1")], "' /> \n"), collapse="")
    }
    
    #151 METRX_F_DENIT_PH_EXP_2 - 
    if ("metrx_f_denit_ph_exp_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DENIT_PH_EXP_2' value='", pft.traits[which(pft.names == "metrx_f_denit_ph_exp_2")], "' /> \n"), collapse="")
    }
    
    #152 METRX_F_DENIT_M_WEIBULL_1 - 
    if ("metrx_f_denit_m_weibull_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DENIT_M_WEIBULL_1' value='", pft.traits[which(pft.names == "metrx_f_denit_m_weibull_1")], "' /> \n"), collapse="")
    }
    
    #153 METRX_F_DENIT_M_WEIBULL_2 - 
    if ("metrx_f_denit_m_weibull_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DENIT_M_WEIBULL_2' value='", pft.traits[which(pft.names == "metrx_f_denit_m_weibull_2")], "' /> \n"), collapse="")
    }
    
    #154 METRX_F_N_ALGAE - 
    if ("metrx_f_n_algae" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_N_ALGAE' value='", pft.traits[which(pft.names == "metrx_f_n_algae")], "' /> \n"), collapse="")
    }
    
    #155 METRX_F_N_CH4_OXIDATION -
    if ("metrx_f_n_ch4_oxidation" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_N_CH4_OXIDATION' value='", pft.traits[which(pft.names == "metrx_f_n_ch4_oxidation")], "' /> \n"), collapse="")
    }
    
    #156 METRX_F_NIT_NO_M_EXP_1 -
    if ("metrx_f_nit_no_m_exp_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_NO_M_EXP_1' value='", pft.traits[which(pft.names == "metrx_f_nit_no_m_exp_1")], "' /> \n"), collapse="")
    }
    
    #157 METRX_F_NIT_NO_M_EXP_2 -
    if ("metrx_f_nit_no_m_exp_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_NO_M_EXP_2' value='", pft.traits[which(pft.names == "metrx_f_nit_no_m_exp_2")], "' /> \n"), collapse="")
    }
    
    #158 METRX_F_NIT_NO_T_EXP_1 -
    if ("metrx_f_nit_no_t_exp_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_NO_T_EXP_1' value='", pft.traits[which(pft.names == "metrx_f_nit_no_t_exp_1")], "' /> \n"), collapse="")
    }
    
    #159 METRX_F_NIT_NO_T_EXP_2 -
    if ("metrx_f_nit_no_t_exp_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_NO_T_EXP_2' value='", pft.traits[which(pft.names == "metrx_f_nit_no_t_exp_2")], "' /> \n"), collapse="")
    }
    
    #160 METRX_F_NIT_NO_PH_LIN_1 -
    if ("metrx_f_nit_no_ph_lin_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_NO_PH_LIN_1' value='", pft.traits[which(pft.names == "metrx_f_nit_no_ph_lin_1")], "' /> \n"), collapse="")
    }
    
    #161 METRX_F_NIT_NO_PH_LIN_2 -
    if ("metrx_f_nit_no_ph_lin_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_NO_PH_LIN_2' value='", pft.traits[which(pft.names == "metrx_f_nit_no_ph_lin_2")], "' /> \n"), collapse="")
    }
    
    #162 METRX_F_NIT_N2O_M_WEIBULL_1 - 
    if ("metrx_f_nit_n2o_m_weibull_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_N2O_M_WEIBULL_1' value='", pft.traits[which(pft.names == "metrx_f_nit_n2o_m_weibull_1")], "' /> \n"), collapse="")
    }
    
    #163 METRX_F_NIT_N2O_M_WEIBULL_2 - 
    if ("metrx_f_nit_n2o_m_weibull_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_N2O_M_WEIBULL_2' value='", pft.traits[which(pft.names == "metrx_f_nit_n2o_m_weibull_2")], "' /> \n"), collapse="")
    }
    
    #164 METRX_F_NIT_N2O_M_WEIBULL_3 - 
    if ("metrx_f_nit_n2o_m_weibull_3" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_N2O_M_WEIBULL_3' value='", pft.traits[which(pft.names == "metrx_f_nit_n2o_m_weibull_3")], "' /> \n"), collapse="")
    }
    
    #165 METRX_F_NIT_N2O_T_EXP_1 -
    if ("metrx_f_nit_n2o_t_exp_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_N2O_T_EXP_1' value='", pft.traits[which(pft.names == "metrx_f_nit_n2o_t_exp_1")], "' /> \n"), collapse="")
    }
    
    #166 METRX_F_NIT_N2O_T_EXP_2 -
    if ("metrx_f_nit_n2o_t_exp_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_N2O_T_EXP_2' value='", pft.traits[which(pft.names == "metrx_f_nit_n2o_t_exp_2")], "' /> \n"), collapse="")
    }
    
    #167 METRX_F_NIT_PH_ONEILL_1 -
    if ("metrx_f_nit_ph_oneill_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_PH_ONEILL_1' value='", pft.traits[which(pft.names == "metrx_f_nit_ph_oneill_1")], "' /> \n"), collapse="")
    }
    
    #168 METRX_F_NIT_PH_ONEILL_2 -
    if ("metrx_f_nit_ph_oneill_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_PH_ONEILL_2' value='", pft.traits[which(pft.names == "metrx_f_nit_ph_oneill_2")], "' /> \n"), collapse="")
    }
    
    #169 METRX_F_NIT_PH_ONEILL_3 -
    if ("metrx_f_nit_ph_oneill_3" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_PH_ONEILL_3' value='", pft.traits[which(pft.names == "metrx_f_nit_ph_oneill_3")], "' /> \n"), collapse="")
    }
    
    #170 METRX_FE_REDUCTION -
    if ("metrx_fe_reduction" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_FE_REDUCTION' value='", pft.traits[which(pft.names == "metrx_fe_reduction")], "' /> \n"), collapse="")
    }
    
    #171 METRX_FRAC_FE_CH4_PROD - 
    if ("metrx_frac_fe_ch4_prod" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_FRAC_FE_CH4_PROD' value='", pft.traits[which(pft.names == "metrx_frac_fe_ch4_prod")], "' /> \n"), collapse="")
    }
    
    #172 METRX_MAX_DEPTH_DENIT - 
    if ("metrx_max_depth_denit" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_MAX_DEPTH_DENIT' value='", pft.traits[which(pft.names == "metrx_max_depth_denit")], "' /> \n"), collapse="")
    }
    
    #173 METRX_MIC_EFF - 
    if ("metrx_mic_eff" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_MIC_EFF' value='", pft.traits[which(pft.names == "metrx_mic_eff")], "' /> \n"), collapse="")
    }
    
    #174 METRX_MIC_EFF_METANE_OX - 
    if ("metrx_mic_eff_metane_ox" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_MIC_EFF_METANE_OX' value='", pft.traits[which(pft.names == "metrx_mic_eff_metane_ox")], "' /> \n"), collapse="")
    }
    
    #175 METRX_MUEMAX_C_ALGAE - 
    if ("metrx_muemax_c_algae" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_MUEMAX_C_ALGAE' value='", pft.traits[which(pft.names == "metrx_muemax_c_algae")], "' /> \n"), collapse="")
    }
    
    #176 METRX_MUEMAX_C_CH4_OX - 
    if ("metrx_muemax_c_ch4_ox" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_MUEMAX_C_CH4_OX' value='", pft.traits[which(pft.names == "metrx_muemax_c_ch4_ox")], "' /> \n"), collapse="")
    }
    
    #177 METRX_MUEMAX_C_CH4_PROD - 
    if ("metrx_muemax_c_ch4_prod" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_MUEMAX_C_CH4_PROD' value='", pft.traits[which(pft.names == "metrx_muemax_c_ch4_prod")], "' /> \n"), collapse="")
    }
    
    #178 METRX_MUEMAX_C_DENIT - 
    if ("metrx_muemax_c_denit" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_MUEMAX_C_DENIT' value='", pft.traits[which(pft.names == "metrx_muemax_c_denit")], "' /> \n"), collapse="")
    }
    
    #179 METRX_MUEMAX_C_FERM - 
    if ("metrx_muemax_c_ferm" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_MUEMAX_C_FERM' value='", pft.traits[which(pft.names == "metrx_muemax_c_ferm")], "' /> \n"), collapse="")
    }
    
    #180 METRX_MUEMAX_C_NIT - 
    if ("metrx_muemax_c_nit" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_MUEMAX_C_NIT' value='", pft.traits[which(pft.names == "metrx_muemax_c_nit")], "' /> \n"), collapse="")
    }
    
    #181 METRX_MUEMAX_C_FE_RED - 
    if ("metrx_muemax_c_fe_red" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_MUEMAX_C_FE_RED' value='", pft.traits[which(pft.names == "metrx_muemax_c_fe_red")], "' /> \n"), collapse="")
    }
    
    #182 METRX_MUEMAX_H2_CH4_PROD - 
    if ("metrx_muemax_h2_ch4_prod" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_MUEMAX_H2_CH4_PROD' value='", pft.traits[which(pft.names == "metrx_muemax_h2_ch4_prod")], "' /> \n"), collapse="")
    }
    
    #183 METRX_MUEMAX_N_ASSI - 
    if ("metrx_muemax_n_assi" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_MUEMAX_N_ASSI' value='", pft.traits[which(pft.names == "metrx_muemax_n_assi")], "' /> \n"), collapse="")
    }
    
    #184 METRX_NITRIFY_MAX - 
    if ("metrx_nitrify_max" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_NITRIFY_MAX' value='", pft.traits[which(pft.names == "metrx_nitrify_max")], "' /> \n"), collapse="")
    }
    
    #185 METRX_KF_NIT_NO - 
    if ("metrx_kf_nit_no" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KF_NIT_NO' value='", pft.traits[which(pft.names == "metrx_kf_nit_no")], "' /> \n"), collapse="")
    }
    
    #186 METRX_KF_NIT_N2O - 
    if ("metrx_kf_nit_n2o" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KF_NIT_N2O' value='", pft.traits[which(pft.names == "metrx_kf_nit_n2o")], "' /> \n"), collapse="")
    }
    
    #187 METRX_K_O2_CH4_PROD - 
    if ("metrx_k_o2_ch4_prod" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_K_O2_CH4_PROD' value='", pft.traits[which(pft.names == "metrx_k_o2_ch4_prod")], "' /> \n"), collapse="")
    }
    
    #188 METRX_K_O2_FE_RED - 
    if ("metrx_k_o2_fe_red" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_K_O2_FE_RED' value='", pft.traits[which(pft.names == "metrx_k_o2_fe_red")], "' /> \n"), collapse="")
    }
    
    #189 METRX_KF_FE_FE_RED - 
    if ("metrx_kf_fe_fe_red" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KF_FE_FE_RED' value='", pft.traits[which(pft.names == "metrx_kf_fe_fe_red")], "' /> \n"), collapse="")
    }
    
    #190 METRX_KMM_AC_CH4_PROD - 
    if ("metrx_kmm_ac_ch4_prod" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_AC_CH4_PROD' value='", pft.traits[which(pft.names == "metrx_kmm_ac_ch4_prod")], "' /> \n"), collapse="")
    }
    
    #191 METRX_KMM_AC_FE_RED - 
    if ("metrx_kmm_ac_fe_red" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_AC_FE_RED' value='", pft.traits[which(pft.names == "metrx_kmm_ac_fe_red")], "' /> \n"), collapse="")
    }
    
    #192 METRX_KMM_H2_FE_RED - 
    if ("metrx_kmm_h2_fe_red" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_H2_FE_RED' value='", pft.traits[which(pft.names == "metrx_kmm_h2_fe_red")], "' /> \n"), collapse="")
    }
    
    #193 METRX_KMM_C_DENIT - 
    if ("metrx_kmm_c_denit" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_C_DENIT' value='", pft.traits[which(pft.names == "metrx_kmm_c_denit")], "' /> \n"), collapse="")
    }
    
    #194 METRX_KMM_CH4_CH4_OX - 
    if ("metrx_kmm_ch4_ch4_ox" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_CH4_CH4_OX' value='", pft.traits[which(pft.names == "metrx_kmm_ch4_ch4_ox")], "' /> \n"), collapse="")
    }
    
    #195 METRX_KMM_C_MIC - 
    if ("metrx_kmm_c_mic" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_C_MIC' value='", pft.traits[which(pft.names == "metrx_kmm_c_mic")], "' /> \n"), collapse="")
    }
    
    #196 METRX_KMM_O2_NIT - 
    if ("metrx_kmm_o2_nit" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_O2_NIT' value='", pft.traits[which(pft.names == "metrx_kmm_o2_nit")], "' /> \n"), collapse="")
    }
    
    #197 METRX_KMM_H2_FERM - 
    if ("metrx_kmm_h2_ferm" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_H2_FERM' value='", pft.traits[which(pft.names == "metrx_kmm_h2_ferm")], "' /> \n"), collapse="")
    }
    
    #198 METRX_KMM_H2_CH4_PROD - 
    if ("metrx_kmm_h2_ch4_prod" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_H2_CH4_PROD' value='", pft.traits[which(pft.names == "metrx_kmm_h2_ch4_prod")], "' /> \n"), collapse="")
    }
    
    #201 METRX_KMM_N_DENIT - 
    if ("metrx_kmm_n_denit" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_N_DENIT' value='", pft.traits[which(pft.names == "metrx_kmm_n_denit")], "' /> \n"), collapse="")
    }
    
    #202 METRX_KMM_N_MIC - 
    if ("metrx_kmm_n_mic" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_N_MIC' value='", pft.traits[which(pft.names == "metrx_kmm_n_mic")], "' /> \n"), collapse="")
    }
    
    #203 METRX_KMM_NH4_NIT - 
    if ("metrx_kmm_nh4_nit" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_NH4_NIT' value='", pft.traits[which(pft.names == "metrx_kmm_nh4_nit")], "' /> \n"), collapse="")
    }
    
    #204 METRX_KMM_NO2_NIT - 
    if ("metrx_kmm_no2_nit" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_NO2_NIT' value='", pft.traits[which(pft.names == "metrx_kmm_no2_nit")], "' /> \n"), collapse="")
    }
    
    #205 METRX_KMM_O2_CH4_OX - 
    if ("metrx_kmm_o2_ch4_ox" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_O2_CH4_OX' value='", pft.traits[which(pft.names == "metrx_kmm_o2_ch4_ox")], "' /> \n"), collapse="")
    }
    
    #206 METRX_KMM_FE_OX - 
    if ("metrx_kmm_fe_ox" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_FE_OX' value='", pft.traits[which(pft.names == "metrx_kmm_fe_ox")], "' /> \n"), collapse="")
    }
    
    #207 METRX_KMM_PH_INCREASE_FROM_UREA - 
    if ("metrx_kmm_ph_increase_from_urea" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KMM_PH_INCREASE_FROM_UREA' value='", pft.traits[which(pft.names == "metrx_kmm_ph_increase_from_urea")], "' /> \n"), collapse="")
    }
    
    #208 METRX_KR_ANVF_DIFF_GAS - 
    if ("metrx_kr_anvf_diff_gas" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_ANVF_DIFF_GAS' value='", pft.traits[which(pft.names == "metrx_kr_anvf_diff_gas")], "' /> \n"), collapse="")
    }
    
    #209 METRX_KR_ANVF_DIFF_LIQ - 
    if ("metrx_kr_anvf_diff_liq" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_ANVF_DIFF_LIQ' value='", pft.traits[which(pft.names == "metrx_kr_anvf_diff_liq")], "' /> \n"), collapse="")
    }
    
    #210 METRX_KR_DC_ALGAE - 
    if ("metrx_kr_dc_algae" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_DC_ALGAE' value='", pft.traits[which(pft.names == "metrx_kr_dc_algae")], "' /> \n"), collapse="")
    }
    
    #211 METRX_KR_DC_AORG - 
    if ("metrx_kr_dc_aorg" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_DC_AORG' value='", pft.traits[which(pft.names == "metrx_kr_dc_aorg")], "' /> \n"), collapse="")
    }
    
    #212 METRX_KR_DC_CEL - 
    if ("metrx_kr_dc_cel" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_DC_CEL' value='", pft.traits[which(pft.names == "metrx_kr_dc_cel")], "' /> \n"), collapse="")
    }
    
    #213 METRX_KR_DC_HUM_0 - 
    if ("metrx_kr_dc_hum_0" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_DC_HUM_0' value='", pft.traits[which(pft.names == "metrx_kr_dc_hum_0")], "' /> \n"), collapse="")
    }
    
    #214 METRX_KR_DC_HUM_1 - 
    if ("metrx_kr_dc_hum_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_DC_HUM_1' value='", pft.traits[which(pft.names == "metrx_kr_dc_hum_1")], "' /> \n"), collapse="")
    }
    
    #215 METRX_KR_DC_HUM_2 - 
    if ("metrx_kr_dc_hum_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_DC_HUM_2' value='", pft.traits[which(pft.names == "metrx_kr_dc_hum_2")], "' /> \n"), collapse="")
    }
    
    #216 METRX_KR_DC_LIG - 
    if ("metrx_kr_dc_lig" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_DC_LIG' value='", pft.traits[which(pft.names == "metrx_kr_dc_lig")], "' /> \n"), collapse="")
    }
    
    #217 METRX_KR_DC_RAW_LITTER - 
    if ("metrx_kr_dc_raw_litter" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_DC_RAW_LITTER' value='", pft.traits[which(pft.names == "metrx_kr_dc_raw_litter")], "' /> \n"), collapse="")
    }
    
    #218 METRX_KR_DC_SOL - 
    if ("metrx_kr_dc_sol" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_DC_SOL' value='", pft.traits[which(pft.names == "metrx_kr_dc_sol")], "' /> \n"), collapse="")
    }
    
    #219 METRX_KR_DC_WOOD - 
    if ("metrx_kr_dc_wood" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_DC_WOOD' value='", pft.traits[which(pft.names == "metrx_kr_dc_wood")], "' /> \n"), collapse="")
    }
    
    #220 METRX_KR_DENIT_CHEMO - 
    if ("metrx_kr_denit_chemo" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_DENIT_CHEMO' value='", pft.traits[which(pft.names == "metrx_kr_denit_chemo")], "' /> \n"), collapse="")
    }
    
    #221 METRX_KR_FRAC_FRAG_ABOVE - 
    if ("metrx_kr_frac_frag_above" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_FRAC_FRAG_ABOVE' value='", pft.traits[which(pft.names == "metrx_kr_frac_frag_above")], "' /> \n"), collapse="")
    }
    
    #223 METRX_KR_OX_FE - 
    if ("metrx_kr_ox_fe" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_OX_FE' value='", pft.traits[which(pft.names == "metrx_kr_ox_fe")], "' /> \n"), collapse="")
    }
    
    #224 METRX_KR_HU_AORG_HUM_0 - 
    if ("metrx_kr_hu_aorg_hum_0" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_HU_AORG_HUM_0' value='", pft.traits[which(pft.names == "metrx_kr_hu_aorg_hum_0")], "' /> \n"), collapse="")
    }
    
    #225 METRX_KR_HU_AORG_HUM_1 - 
    if ("metrx_kr_hu_aorg_hum_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_HU_AORG_HUM_1' value='", pft.traits[which(pft.names == "metrx_kr_hu_aorg_hum_1")], "' /> \n"), collapse="")
    }
    
    #227 METRX_KR_HU_SOL - 
    if ("metrx_kr_hu_sol" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_HU_SOL' value='", pft.traits[which(pft.names == "metrx_kr_hu_sol")], "' /> \n"), collapse="")
    }
    
    #229 METRX_KR_HU_HUM_0 - 
    if ("metrx_kr_hu_hum_0" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_HU_HUM_0' value='", pft.traits[which(pft.names == "metrx_kr_hu_hum_0")], "' /> \n"), collapse="")
    }
    
    #230 METRX_KR_HU_HUM_1 - 
    if ("metrx_kr_hu_hum_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_HU_HUM_1' value='", pft.traits[which(pft.names == "metrx_kr_hu_hum_1")], "' /> \n"), collapse="")
    }
    
    #231 METRX_KR_HU_LIG - 
    if ("metrx_kr_hu_lig" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_HU_LIG' value='", pft.traits[which(pft.names == "metrx_kr_hu_lig")], "' /> \n"), collapse="")
    }
    
    #233 METRX_KR_REDUCTION_CN -
    if ("metrx_kr_reduction_cn" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_REDUCTION_CN' value='", pft.traits[which(pft.names == "metrx_kr_reduction_cn")], "' /> \n"), collapse="")
    }
    
    #234 METRX_KR_REDUCTION_ANVF -
    if ("metrx_kr_reduction_anvf" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_REDUCTION_ANVF' value='", pft.traits[which(pft.names == "metrx_kr_reduction_anvf")], "' /> \n"), collapse="")
    }
    
    #235 METRX_KR_REDUCTION_CONIFEROUS -
    if ("metrx_kr_reduction_coniferous" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_KR_REDUCTION_CONIFEROUS' value='", pft.traits[which(pft.names == "metrx_kr_reduction_coniferous")], "' /> \n"), collapse="")
    }
    
    #236 METRX_LIG_HUMIFICATION -
    if ("metrx_lig_humification" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_LIG_HUMIFICATION' value='", pft.traits[which(pft.names == "metrx_lig_humification")], "' /> \n"), collapse="")
    }
    
    #237 METRX_RET_HUMUS -
    if ("metrx_ret_humus" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_RET_HUMUS' value='", pft.traits[which(pft.names == "metrx_ret_humus")], "' /> \n"), collapse="")
    }
    
    #238 METRX_RET_LITTER -
    if ("metrx_ret_litter" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_RET_LITTER' value='", pft.traits[which(pft.names == "metrx_ret_litter")], "' /> \n"), collapse="")
    }
    
    #239 METRX_RET_MICROBES -
    if ("metrx_ret_microbes" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_RET_MICROBES' value='", pft.traits[which(pft.names == "metrx_ret_microbes")], "' /> \n"), collapse="")
    }
    
    #240 METRX_TILL_STIMULATION_1 -
    if ("metrx_till_stimulation_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_TILL_STIMULATION_1' value='", pft.traits[which(pft.names == "metrx_till_stimulation_1")], "' /> \n"), collapse="")
    }
    
    #241 METRX_TILL_STIMULATION_2 -
    if ("metrx_till_stimulation_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_TILL_STIMULATION_2' value='", pft.traits[which(pft.names == "metrx_till_stimulation_2")], "' /> \n"), collapse="")
    }
    
    #242 METRX_V_EBULLITION -
    if ("metrx_v_ebullition" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_V_EBULLITION' value='", pft.traits[which(pft.names == "metrx_v_ebullition")], "' /> \n"), collapse="")
    }
    
    #303 RETDOC -
    if ("retdoc" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='RETDOC' value='", pft.traits[which(pft.names == "retdoc")], "' /> \n"), collapse="")
    }
    
    #304 RETNO3 -
    if ("retno3" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='RETNO3' value='", pft.traits[which(pft.names == "retno3")], "' /> \n"), collapse="")
    }
    
    #317 TEXP -
    if ("texp" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='TEXP' value='", pft.traits[which(pft.names == "texp")], "' /> \n"), collapse="")
    }
    
    
    # SOILWATER RELATED
    
    #2 BY_PASSF -
    if ("by_passf" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='BY_PASSF' value='", pft.traits[which(pft.names == "by_passf")], "' /> \n"), collapse="")
    }
    
    #69 FPERCOL -
    if ("fpercol" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='FPERCOL' value='", pft.traits[which(pft.names == "fpercol")], "' /> \n"), collapse="")
    }
    
    #80 FRUNOFF -
    if ("frunoff" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='FRUNOFF' value='", pft.traits[which(pft.names == "frunoff")], "' /> \n"), collapse="")
    }
    
    #84 IMPEDANCE_PAR -
    if ("impedance_par" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='IMPEDANCE_PAR' value='", pft.traits[which(pft.names == "impedance_par")], "' /> \n"), collapse="")
    }
    
    #307 ROOT_DEPENDENT_TRANS -
    if ("root_dependent_trans" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='ROOT_DEPENDENT_TRANS' value='", pft.traits[which(pft.names == "root_dependent_trans")], "' /> \n"), collapse="")
    }
    
    # 349 WCDNDC_EVALIM_FRAC_WCMIN
    if ("wcdndc_evalim_frac_wcmin" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='WCDNDC_EVALIM_FRAC_WCMIN' value='", pft.traits[which(pft.names == "wcdndc_evalim_frac_wcmin")], "' /> \n"), collapse="")
    }
    
    #335 WCDNDC_INCREASE_POT_EVAPOTRANS -
    if ("wcdndc_increase_pot_evapotrans" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='WCDNDC_INCREASE_POT_EVAPOTRANS' value='", pft.traits[which(pft.names == "wcdndc_increase_pot_evapotrans")], "' /> \n"), collapse="")
    }
    
    
    
    
    
    # Assigning pft values
    species_par_values[names(trait.values)[pft]] <- b.2
    
    b.2 <- ""
      
    
  }

  
  ## INITIAL SOIL CONDITIONS
  soil_layer <- list()
  
  
  ## Soil layers, if not external files are given
  if(is.null(settings$run$inputs$poolinitcond$path)){
    
    # Set different layers, which will be used based on the soil data that is available
    # For example, if we have soil data for top layer, then that will be used instead of soil_layer_1
    soil_layer[1] <- '<layer depth="60" split="3" bd="1.68" clay="0.42" corg="0.04" norg="0.0032" ph="6.94" vangenuchten_n ="1.31" vangenuchten_alpha ="1.9" sand="0.3"  scel="0.005" sks="0.003886" wmin="100" wcmax="380"/>'
    soil_layer[2] <- '<layer depth="90" split="2" bd="1.68" clay="0.42" corg="0.04" norg="0.0032" ph="6.94" vangenuchten_n ="1.31" vangenuchten_alpha ="1.9" sand="0.3"  scel="0.005" sks="0.003886" wmin="100" wcmax="380"/>'
    soil_layer[3] <- '<layer depth="200" split="4" bd="1.18"  clay="0.32" corg="0.0250"  norg="0.002219" ph="6.94" vangenuchten_n ="1.3" vangenuchten_alpha ="1.9" sand="0.25"  scel="0.005" sks="0.003186" wmin="100" wcmax="380" />'
    soil_layer[4] <- '<layer depth="200" split="4" bd="1.09"  clay="0.12" corg="0.0150"  norg="0.001219" ph="6.94" vangenuchten_n ="1.3" vangenuchten_alpha ="1.9"  sand="0.25"  scel="0.005" sks="0.003094" wmin="100" wcmax="380" />'
    soil_layer[5] <- '<layer depth="200" split="4" bd="1.00"  clay="0.02" corg="0.0050"  norg="0.000219" ph="7" vangenuchten_n ="1.3" vangenuchten_alpha ="1.9"  sand="0.05"  scel="0.005" sks="0.003000" />'
    soil_layer[6] <- '<layer depth="100" bd="1.00"  clay="0.02" corg="0.0050"  norg="0.000219" ph="7" vangenuchten_n ="1.3" vangenuchten_alpha ="1.9"  sand="0.05"  scel="0.005" sks="0.001794" />'
    soil_layer[7] <- '<layer depth="100" bd="1.00"  clay="0.02" corg="0.0050"  norg="0.000219" ph="7" vangenuchten_n ="1.3" vangenuchten_alpha ="1.9"  sand="0.05"  scel="0.005" sks="0.00004000" />'
    
    soil_layer_values <- paste(soil_layer, collapse = "\n \t")
  }
  

  ## One soil layer is given
  else if(!is.null(settings$run$inputs$poolinitcond$path)){
    # Set empty
    soil_all_block <- NULL
    
    # Reading soil file
    soil_IC_list <- PEcAn.data.land::pool_ic_netcdf2list(settings$run$inputs$poolinitcond$path)
    
    
    
    ## --- Initial condtions for the site --- ##
    
    # Before moving to write site file, check siteparameter initial conditions and site initial condition
    
    ## Siteparameter file
    #300 RCNM -
    # C:N ratio of humus
    if ("c2n_humus" %in% names(soil_IC_list$vals)) {
      h.2 <- paste(h.2, paste0("\t\t<par name='RCNM' value='", unlist(soil_IC_list$vals["c2n_humus"])[[1]], "' /> \n"), collapse="")
    }
    
    ## Event file
    # Populate the events file, if there are placeholders for initial biomasses or fractional cover
    # Initial biomass in the field
    if(any(grepl("@InitialBiomass@", eventsfile))){
      if ("AGB" %in% names(soil_IC_list$vals)) {
        initialbiomass <- round( PEcAn.utils::ud_convert(unlist(soil_IC_list$vals["AGB"])[[1]], "kg m-2", "kg ha-1"), 1 )
      }
      else{
        initialbiomass <- 100
      }
      # Fill in the value
      eventsfile <- gsub("@InitialBiomass@", paste0("'", initialbiomass, "'"), eventsfile)
    }
    # Fractional cover of the plants
    if(any(grepl("@FractionalCover@", eventsfile))){
      if ("fractional_cover" %in% names(soil_IC_list$vals)) {
        fractionalcover <- unlist(soil_IC_list$vals["fractional_cover"])[[1]]
      }
      else{
        fractionalcover <- 0.5
      }
      # Fill in the value
      eventsfile <- gsub("@FractionalCover@", paste0("'", fractionalcover, "'"), eventsfile)
    }
    
    
    ## Site file (general)
    # Soil use history
    if(any(grepl("@Info_Use_History@", sitefile))){
      if ("history" %in% names(soil_IC_list$vals)){
        soil_use_history <- unlist(soil_IC_list$vals["history"])[[1]]
      }
      else{
        soil_use_history <- "arable"
      }
      sitefile <- gsub("@Info_Use_History@", paste0("'", soil_use_history, "'"), sitefile)
    }
    
    # Soil type
    if(any(grepl("@Soil_Type@", sitefile))){
      if ("soil_type" %in% names(soil_IC_list$vals)){
        soil_type <- unlist(soil_IC_list$vals["soil_type"])[[1]]
      }
      else{
        soil_type <- "SALO"
      }
      sitefile <- gsub("@Soil_Type@", paste0("'", soil_type, "'"), sitefile)
    }
    
    # Litter height
    if(any(grepl("@Litter_Height@", sitefile))){
      if ("litter_height" %in% names(soil_IC_list$vals)){
        litter_height <- unlist(soil_IC_list$vals["litter_height"])[[1]]
      }
      else{
        litter_height <- "0.0"
      }
      sitefile <- gsub("@Litter_Height@", paste0("'", litter_height, "'"), sitefile)
    }
    
    ## -- Layers -- ##
    
    # Check how many depth layers is given and the depth of each
    depth <- soil_IC_list$dims$depth
    layer_count <- length(depth)
    # Check what stratums is given for the layers
    layer_div <- soil_IC_list$vals$stratum
    
    
    for(depth_level in 1:layer_count){
      
      soil_one_block <- NULL
      # Diskretization -- Every soil layer is still divided to several layers, this layer that contains these
      # sublayers are here called a block. In LDNDC it is not suggested to use too tight layers so still will be
      # divided to smaller layers that are not so thick.
      
      
      
      # For 1st level
      if(depth_level == 1){ 
        disk <- depth[depth_level] * 1000 / layer_div[depth_level]
      }

      # For rest of layers, depth is informed as cumulative, but LDNDC uses thickness
      else{
        disk <- (depth[depth_level] - depth[depth_level-1]) * 1000 / layer_div[depth_level]
      }
      
      for(disk_level in 1:layer_div[depth_level]){
        
        
        # Start creating a soil layer
        soil_layer_values <- paste0("<layer depth='", disk, "' ")
        
        
        ## Check which values are found from the soil netcdf file
        if("soil_density" %in% names(soil_IC_list$vals)){
          # Bulk density in a soil
          bd <- PEcAn.utils::ud_convert(unlist(soil_IC_list$vals["soil_density"])[[depth_level]], "kg m-3", "kg dm-3")
          soil_layer_values <- ifelse(!is.na(bd), paste0(soil_layer_values, paste0("bd='", bd, "' ")), soil_layer_values)
        }
        
        if("mass_fraction_of_clay_in_soil" %in% names(soil_IC_list$vals)){
          # Clay content value
          clay <- unlist(soil_IC_list$vals["mass_fraction_of_clay_in_soil"])[[depth_level]]
          soil_layer_values <- ifelse(!is.na(clay), paste0(soil_layer_values, paste0("clay='", clay, "' ")), soil_layer_values)
        }
        
        if("mass_fraction_of_sand_in_soil" %in% names(soil_IC_list$vals)){
          # Sand content value
          sand <- unlist(soil_IC_list$vals["mass_fraction_of_sand_in_soil"])[[depth_level]]
          soil_layer_values <- ifelse(!is.na(sand), paste0(soil_layer_values, paste0("sand='", sand, "' ")), soil_layer_values)
        }
        
        if("mass_fraction_of_silt_in_soil" %in% names(soil_IC_list$vals)){
          # Silt content value
          silt <- unlist(soil_IC_list$vals["mass_fraction_of_silt_in_soil"])[[depth_level]]
          soil_layer_values <- ifelse(!is.na(silt), paste0(soil_layer_values, paste0("silt='", silt, "' ")), soil_layer_values)
        }
        
        if("pH" %in% names(soil_IC_list$vals)){
          # pH value
          ph <- unlist(soil_IC_list$vals["pH"])[[depth_level]]
          soil_layer_values <- ifelse(!is.na(ph), paste0(soil_layer_values, paste0("ph='", ph, "' ")), soil_layer_values)
        }
        
        if("soil_carbon_content" %in% names(soil_IC_list$vals)){
          # Total Carbon Content - kg N m-2 to kg N kg-1 ??
          corg <- unlist(soil_IC_list$vals["soil_carbon_content"])[[depth_level]]
          soil_layer_values <- ifelse(!is.na(corg), paste0(soil_layer_values, paste0("corg='", corg, "' ")), soil_layer_values)
        }
        
        if("soil_nitrogen_content" %in% names(soil_IC_list$vals)){
          # Total Nitrogen Content - kg N m-2 to kg N kg-1 ??
          norg <- unlist(soil_IC_list$vals["soil_nitrogen_content"])[[depth_level]]
          soil_layer_values <- ifelse(!is.na(norg), paste0(soil_layer_values, paste0("norg='", norg, "' ")), soil_layer_values)
        }
        
        if("soil_iron_content" %in% names(soil_IC_list$vals)){
          # Total Iron Content - kg Fe m-2 to kg Fe kg-1 ??
          iron <- unlist(soil_IC_list$vals["soil_iron_content"])[[depth_level]]
          soil_layer_values <- ifelse(!is.na(iron), paste0(soil_layer_values, paste0("iron='", iron, "' ")), soil_layer_values)
        }
        
        if("vangenuchten_n" %in% names(soil_IC_list$vals)){
          # Vangenuchten_n - 
          vn <- unlist(soil_IC_list$vals["vangenuchten_n"])[[depth_level]]
          soil_layer_values <- ifelse(!is.na(vn), paste0(soil_layer_values, paste0("vangenuchten_n='", vn, "' ")), soil_layer_values)
        }
        
        if("vangenuchten_alpha" %in% names(soil_IC_list$vals)){
          # Vangenuchten_alpha - 
          va <- unlist(soil_IC_list$vals["vangenuchten_alpha"])[[depth_level]]
          soil_layer_values <- ifelse(!is.na(va), paste0(soil_layer_values, paste0("vangenuchten_alpha='", va, "' ")), soil_layer_values)
        }
        
        if("volume_fraction_of_water_in_soil_at_field_capacity" %in% names(soil_IC_list$vals)){
          # Field capacity - Change m3 m-3 to dm3 m-3
          wcmax <- unlist(soil_IC_list$vals["volume_fraction_of_water_in_soil_at_field_capacity"])[[depth_level]] * 1000
          soil_layer_values <- ifelse(!is.na(wcmax), paste0(soil_layer_values, paste0("wcmax='", wcmax, "' ")), soil_layer_values)
        }
        
        if("volume_fraction_of_condensed_water_in_soil_at_wilting_point" %in% names(soil_IC_list$vals)){
          # Wilting point - Change m3 m-3 to dm3 m-3
          wcmin <- unlist(soil_IC_list$vals["volume_fraction_of_condensed_water_in_soil_at_wilting_point"])[[depth_level]] * 1000
          soil_layer_values <- ifelse(!is.na(wcmin), paste0(soil_layer_values, paste0("wcmin='", wcmin, "' ")), soil_layer_values)
        }
        
        if("soil_hydraulic_conductivity_at_saturation" %in% names(soil_IC_list$vals)){
          # Hydraulic conductivity - change m s-1 to cm/min
          sks <- unlist(soil_IC_list$vals["soil_hydraulic_conductivity_at_saturation"])[[depth_level]] * 100 * 60
          soil_layer_values <- ifelse(!is.na(sks), paste0(soil_layer_values, paste0("sks='", sks, "' ")), soil_layer_values)
        }
        
        soil_layer_values <- paste(soil_layer_values, "/> \n")
        
        # Add one individual layer to the block
        soil_one_block <- paste(soil_one_block, soil_layer_values)
      }
      
      # Combine the previous block of layers this and inform that "layer" changes which indicates that new
      # parameter values has been used
      if(depth_level != layer_count){
        soil_all_block <- paste(soil_all_block, soil_one_block, "\n <!-- # Layer changes  --> \n")
      } else {
        soil_all_block <- paste(soil_all_block, soil_one_block, "\n")
      }
    }
    
    # If there is less than seven layer blocks initialised, use the default ones for bottom
    # if(depth_level < 6){
    #   soil_combine <- paste(soil_all_block, "\t\t", paste(soil_layer[-c(1:depth_level)], collapse = "\n \t\t"))
    # }
    # else{
    soil_combine <- soil_all_block
    #}
    
  }
  
  else{
    PEcAn.logger::logger.severe("More than one soil path given: only one soil path is supported")
  }
  
  
  ## Writing and saving species- and siteparameters + initial soil conditions
  speciesparfile_pfts <- NULL
  
  # Handle the populating of speciesparameters after we have read the info from priors
  for(pftn in pfts_run){
    ## Crops ##
    # Barley
    if(pftn == "barley"){
      speciesparfile_pfts <- paste0(speciesparfile_pfts,
                                    "\t\t\t <species mnemonic='sbar'> \n",
                                    species_par_values["barley"][[1]],
                                    "\t\t\t </species> \n\n")
    }
    # Oat
    if(pftn == "oat"){
      speciesparfile_pfts <- paste0(speciesparfile_pfts,
                                    "\t\t\t <species mnemonic='oats'> \n",
                                    species_par_values["oat"][[1]],
                                    "\t\t\t </species> \n\n")
    }
    # Triticale
    if(pftn == "triticale"){
      speciesparfile_pfts <- paste0(speciesparfile_pfts,
                                    "\t\t\t <species mnemonic='trse'> \n",
                                    species_par_values["triticale"][[1]],
                                    "\t\t\t </species> \n\n")
    }
    ## Grass
    # Timothy
    if(pftn == "timothy"){
      speciesparfile_pfts <- paste0(speciesparfile_pfts,
                                    "\t\t\t <species mnemonic='perg'> \n",
                                    species_par_values["timothy"][[1]],
                                    "\t\t\t </species> \n\n")
    }
    
    # Meadow
    if(pftn == "meadow"){
      speciesparfile_pfts <- paste0(speciesparfile_pfts,
                                    "\t\t\t <species mnemonic='mead'> \n",
                                    species_par_values["meadow"][[1]],
                                    "\t\t\t </species> \n\n")
    }
    
    ## Forest
    # Pipy, need to check a correct name for this wood species
    if(pftn == "pipy"){
      speciesparfile_pfts <- paste0(speciesparfile_pfts,
                                    "\t\t\t <species mnemonic='pipy'> \n",
                                    species_par_values["pipy"][[1]],
                                    "\t\t\t </species> \n\n")
    }
  }
  
  
  # Combine the speciesparameter info
  speciesparfile <- gsub("@Info@", speciesparfile_pfts, speciesparfile)
  
  
  # Write to a new xml-file, which will be used on a run. Every simulation run will have
  # their own set of speciesparameters values
  writeLines(speciesparfile, con = file.path(settings$rundir, run.id, "speciesparameters.xml"))
  
  ## Write events to a new xml file
  writeLines(eventsfile, con = file.path(settings$rundir, run.id, "events.xml"))
  
  
  # Handle the populating of siteparameters
  siteparfile <- gsub("@Info@", h.2, siteparfile)
  
  # Write siteparameters
  writeLines(siteparfile, con = file.path(settings$rundir, run.id, "siteparameters.xml"))
  
  
  
  # Populate sitefile layer info with given parameter
  sitefile <- gsub("@Info_Surface_Layer@", soil_combine, sitefile)
  
  # Write soil conditions
  writeLines(sitefile, con = file.path(settings$rundir, run.id, "site.xml"))
  
  # Write airchemistry file (not modified anywhere)
  writeLines(airchemistryfile, con = file.path(settings$rundir, run.id, "airchemistry.txt"))
  
  
  
  #------------------------
  
} # write.config.LDNDC
