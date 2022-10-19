
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
  # For species, read the speciesparameters template
  speciesparfile <- readLines(con = system.file("speciesparameter_template.xml", package = "PEcAn.LDNDC"), n = -1)
  
  # For site (parameters), read the siteparameters template
  siteparfile <- readLines(con = system.file("siteparameters_template.xml", package = "PEcAn.LDNDC"), n = -1)
  
  
  #----------------------
    
  ## Set-up the necessary files in to the run directory so
  ## model is able to function properly. Later on, these
  ## files should be populated with initial values.
    
  
  ###### THIS NEEDS TO BE FUNCTION AT SOME POINT
  ### PROBABLY SIMILAR FUNCTION FOR siteparameters as well
  #
  mnemonic_1 <- "__crop__"
  group <- "crop"
  mnemonic_2.1 <- "barley"
  mnemonic_2.2 <- "oats"
  
  
  ## Crops
  a.1 <- paste0("<species mnemonic='", mnemonic_1, "' group='", group, "' > \n")
  
  # Barley
  b.1.1 <- paste0("\t\t\t\t\t<species mnemonic='", mnemonic_2.1, "' > \n")
  b.2.1 <- ""
  # Keep old version as a reference this need to reconstruct at some point properly anyway
  #b.2 <- apply(trait.values[[1]], 1, function(x){paste0("\t\t\t\t\t\t<par name='", names(x), "' value='", x, "' /> \n")})
  b.3.1 <- paste0("\t\t\t\t</species> \n\n")
  
  # Oats
  b.1.2 <- paste0("\t\t\t\t\t<species mnemonic='", mnemonic_2.2, "' > \n")
  b.2.2 <- ""
  b.3.2 <- paste0("\t\t\t\t</species> \n")
  
  # Indentation (crops)
  a.2 <- paste0("\t\t\t</species>")
  
  # Siteparameters
  h.2 <- ""
  
  
  
  ## Initial conditions, updated if given as parameters
  bd_1 <- 0.05; bd_2 <- 1.68
  clay_1 <- 0; clay_2 <- 0.42
  corg_1 <- corg_2 <- 0.04
  norg_1 <- norg_2 <- 0.0032
  ph_1 <- ph_2 <- 6.9
  vangenuchten_n <- 1.5
  vangenuchten_alpha <- 1.9
  sand_1 <- 0; sand_2 <- 0.3
  scel_1 <- scel_2 <- 0.005
  sks_1 <- 0.8; sks_2 <- 0.003886
  wcmin_1 <- 70; wcmin_2 <- 100
  wcmax_1 <- 330; wcmax_2 <- 380
  
  #browser()
  
  for (pft in seq_along(trait.values)) {
    
    pft.traits <- unlist(trait.values[[pft]])
    pft.names <- names(pft.traits)
    
    
    # Number at the beginning refers to the number of species parameters in LDNDC guide book.
    # First there is name in LDNDC and the second is name in BETY database
    
    #18 ALB (-) - SW_albedo (-)
    if ("SW_albedo" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='ALB' value='", pft.traits[which(pft.names == "SW_albedo")], "' /> \n"), collapse="")
    }
    
    #22 AMAXB (-) - Amax (-)
    if ("Amax" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='AMAXB' value='", pft.traits[which(pft.names == "Amax")], "' /> \n"), collapse="")
    }
    
    #34 DIAMMAX (m) - stem_diameter (cm)
    if ("stem_diameter" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='DIAMMAX' value='",
                                   udunits2::ud.convert(
                                     pft.traits[which(pft.names == "stem_diameter")], "m", "cm"
                                   ),"' /> \n"), collapse="")
    }
    
    #41 DFOL - leaf_density
    if ("leaf_density" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='DFOL' value='", pft.traits[which(pft.names == "leaf_density")], "' /> \n"), collapse="")
    }
    
    #44 DOC_RESP_RATIO - coarseRootExudation
    if ("coarseRootExudation" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t\t<par name='DOC_RESP_RATIO' value='", pft.traits[which(pft.names == "coarseRootExudation")], "' /> \n"), collapse="")
    }
    
    #57 EXP_ROOT_DISTRIBUTION - 
    if ("exp_root_distribution" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t\t<par name='EXP_ROOT_DISTRIBUTION' value='", pft.traits[which(pft.names == "exp_root_distribution")], "' /> \n"), collapse="")
    }
    
    #58 EXT - extinction_coefficient_diffuse
    if ("extinction_coefficient_diffuse" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='EXT' value='", pft.traits[which(pft.names == "extinction_coefficient_diffuse")], "' /> \n"), collapse="")
    }
    
    #79 FRACTION_ROOT - root_biomass_fraction
    if ("root_biomass_fraction" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='FRACTION_ROOT' value='", pft.traits[which(pft.names == "root_biomass_fraction")], "' /> \n"), collapse="")
    }
    
    #80 FRACTION_FRUIT - 
    if ("fraction_fruit" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='FRACTION_FRUIT' value='", pft.traits[which(pft.names == "fraction_fruit")], "' /> \n"), collapse="")
    }
    
    #81 FRACTION_FOLIAGE - 
    if ("fraction_foliage" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='FRACTION_FOLIAGE' value='", pft.traits[which(pft.names == "fraction_foliage")], "' /> \n"), collapse="")
    }
    
    #89 GDD_BASE_TEMPERATURE (C) - gdd_tbase (C)
    if ("gdd_tbase" %in% pft.names) {
      b.2.1 <- paste(b.2.1, paste0("\t\t\t\t\t\t<par name='GDD_BASE_TEMPERATURE' value='", pft.traits[which(pft.names == "gdd_tbase")], "' /> \n"), collapse="")
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='GDD_BASE_TEMPERATURE' value='", pft.traits[which(pft.names == "gdd_tbase")], "' /> \n"), collapse="")
    }
    
    #90 GDD_MAX_TEMPERATURE - 
    if ("gdd_tmax" %in% pft.names) {
      b.2.1 <- paste(b.2.1, paste0("\t\t\t\t\t\t<par name='GDD_MAX_TEMPERATURE' value='", pft.traits[which(pft.names == "gdd_tmax")], "' /> \n"), collapse="")
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='GDD_MAX_TEMPERATURE' value='", pft.traits[which(pft.names == "gdd_tmax")], "' /> \n"), collapse="")
    }
    
    #93 GDD_FLOWERING - 
    if ("gdd_flowering" %in% pft.names) {
      b.2.1 <- paste(b.2.1, paste0("\t\t\t\t\t\t<par name='GDD_FLOWERING' value='", pft.traits[which(pft.names == "gdd_flowering")], "' /> \n"), collapse="")
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='GDD_FLOWERING' value='", pft.traits[which(pft.names == "gdd_flowering")], "' /> \n"), collapse="")
    }
    
    #94 GDD_GRAIN_FILLING - 
    if ("gdd_grain_filling" %in% pft.names) {
      b.2.1 <- paste(b.2.1, paste0("\t\t\t\t\t\t<par name='GDD_GRAIN_FILLING' value='", pft.traits[which(pft.names == "gdd_grain_filling")], "' /> \n"), collapse="")
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='GDD_GRAIN_FILLING' value='", pft.traits[which(pft.names == "gdd_grain_filling")], "' /> \n"), collapse="")
    }
    
    #95 GDD_MATURITY - 
    if ("gdd_maturity" %in% pft.names) {
      b.2.1 <- paste(b.2.1, paste0("\t\t\t\t\t\t<par name='GDD_MATURITY' value='", pft.traits[which(pft.names == "gdd_maturity")], "' /> \n"), collapse="")
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='GDD_MATURITY' value='", pft.traits[which(pft.names == "gdd_maturity")], "' /> \n"), collapse="")
    }
    
    #104 H2OREF_A - 
    if ("h2oref_a" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='H2OREF_A' value='", pft.traits[which(pft.names == "h2oref_a")], "' /> \n"), collapse="")
    }
    
    #119 INI_N_FIX - 
    if ("ini_n_fix" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='INI_N_FIX' value='", pft.traits[which(pft.names == "ini_n_fix")], "' /> \n"), collapse="")
    }
    
    #126 K_MM_NITROGEN_UPTAKE - 
    if ("k_mm_nitrogen_uptake" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='K_MM_NITROGEN_UPTAKE' value='", pft.traits[which(pft.names == "k_mm_nitrogen_uptake")], "' /> \n"), collapse="")
    }
    
    #130 MAINTENANCE_TEMP_REF - 
    if ("maintenance_temp_ref" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='MAINTENANCE_TEMP_REF' value='", pft.traits[which(pft.names == "maintenance_temp_ref")], "' /> \n"), collapse="")
    }
    
    #133 MC_ROOT - 
    if ("mc_root" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='MC_ROOT' value='", pft.traits[which(pft.names == "mc_root")], "' /> \n"), collapse="")
    }
    
    #136 M_FRUIT_OPT -
    if ("m_fruit_opt" %in% pft.names) {
      b.2.1 <- paste(b.2.1, paste0("\t\t\t\t\t\t<par name='M_FRUIT_OPT' value='", pft.traits[which(pft.names == "m_fruit_opt")], "' /> \n"), collapse="")
    }
    
    #146 NC_FINEROOTS_MAX - 
    if ("nc_fineroots_max" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='NC_FINEROOTS_MAX' value='", pft.traits[which(pft.names == "nc_fineroots_max")], "' /> \n"), collapse="")
    }
    
    #147 NC_FINEROOTS_MIN - 
    if ("nc_fineroots_min" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='NC_FINEROOTS_MIN' value='", pft.traits[which(pft.names == "nc_fineroots_min")], "' /> \n"), collapse="")
    }
    
    #148 NC_FRUIT_MAX - 
    if ("nc_fruit_max" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='NC_FRUIT_MAX' value='", pft.traits[which(pft.names == "nc_fruit_max")], "' /> \n"), collapse="")
    }
    
    #149 NC_FRUIT_MIN - 
    if ("nc_fruit_min" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='NC_FRUIT_MIN' value='", pft.traits[which(pft.names == "nc_fruit_min")], "' /> \n"), collapse="")
    }
    
    #150 NC_STRUCTURAL_TISSUE_MAX - 
    if ("nc_structural_tissue" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='NC_STRUCTURAL_TISSUE_MAX' value='", pft.traits[which(pft.names == "nc_structural_tissue_max")], "' /> \n"), collapse="")
    }
    
    #151 NC_STRUCTURAL_TISSUE_MIN - 
    if ("nc_structural_tissue_min" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='NC_STRUCTURAL_TISSUE_MIN' value='", pft.traits[which(pft.names == "nc_structural_tissue_min")], "' /> \n"), collapse="")
    }
    
    #167 PSNTMAX (C) -  pstemp_max (C)
    if ("pstemp_max" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='PSNTMAX' value='", pft.traits[which(pft.names == "pstemp_max")], "' /> \n"), collapse="")
    }
    
    #168 PSNTMIN (C) -  pstemp_min (C)
    if ("pstemp_min" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='PSNTMIN' value='", pft.traits[which(pft.names == "pstemp_min")], "' /> \n"), collapse="")
    }
    
    #169 PSNTOPT (C) -  psnTOpt (C)
    if ("psnTOpt" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='PSNTOPT' value='", pft.traits[which(pft.names == "psnTOpt")], "' /> \n"), collapse="")
    }
    
    #186 SENESCENCE_AGE - 
    if ("senescence_age" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='SENESCENCE_AGE' value='", pft.traits[which(pft.names == "senescence_age")], "' /> \n"), collapse="")
    }
    
    #187 SENESCENCE_DROUGHT - 
    if ("senescence_drought" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='SENESCENCE_DROUGHT' value='", pft.traits[which(pft.names == "senescence_drought")], "' /> \n"), collapse="")
    }
    
    #188 SENESCENCE_FROST - 
    if ("senescence_frost" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='SENESCENCE_frost' value='", pft.traits[which(pft.names == "senescence_frost")], "' /> \n"), collapse="")
    }
    
    #193 SLAMAX (m2 kg-1) -  SLAMAX (m2 kg-1)
    if ("SLAMAX" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='SLAMAX' value='", pft.traits[which(pft.names == "SLAMAX")], "' /> \n"), collapse="")
    }
    
    #194 SLAMIN (m2 kg-1) -  SLAMIN (m2 kg-1)
    if ("SLAMIN" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='SLAMIN' value='", pft.traits[which(pft.names == "SLAMIN")], "' /> \n"), collapse="")
    }
    
    #205 TLIMIT - 
    if ("tlimit" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='TLIMIT' value='", pft.traits[which(pft.names == "tlimit")], "' /> \n"), collapse="")
    }
    
    #206 TOFRTBAS - 
    if ("tofrtbas" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='TOFRTBAS' value='", pft.traits[which(pft.names == "tofrtbas")], "' /> \n"), collapse="")
    }
    
    #220 WUECMAX - wuecmax
    if ("wuecmax" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='WUECMAX' value='", pft.traits[which(pft.names == "wuecmax")], "' /> \n"), collapse="")
    }
    
    #221 WUECMIN - wuecmin
    if ("wuecmin" %in% pft.names) {
      b.2.2 <- paste(b.2.2, paste0("\t\t\t\t\t\t<par name='WUECMIN' value='", pft.traits[which(pft.names == "wuecmin")], "' /> \n"), collapse="")
    }
    
    
    ## SITEPARAMETERS
    # Number at the beginning refers to the number of site parameters in LDNDC guide book.
    
    #82 GROUNDWATER_NUTRIENT_RENEWAL - 
    if ("groundwater_nutrient_renewal" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t\t\t\t\t<par name='GROUNDWATER_NUTRIENT_RENEWAL' value='", pft.traits[which(pft.names == "groundwater_nutrient_renewal")], "' /> \n"), collapse="")
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
    
    #128 METRX_CN_MIC_MIN - 
    if ("metrx_cn_mic_min" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_CN_MIC_MIN' value='", pft.traits[which(pft.names == "metrx_cn_mic_min")], "' /> \n"), collapse="")
    }
    
    #129 METRX_CO2_PROD_DECOMP - 
    if ("metrx_co2_prod_decomp" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_CO2_PROD_DECOMP' value='", pft.traits[which(pft.names == "metrx_co2_prod_decomp")], "' /> \n"), collapse="")
    }
    
    #130 METRX_D_EFF_REDUCTION - 
    if ("metrx_co2_prod_decomp" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_CO2_PROD_DECOMP' value='", pft.traits[which(pft.names == "metrx_co2_prod_decomp")], "' /> \n"), collapse="")
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
    
    #141 METRX_F_CH4_PRODUCTION_T_EXP_1 - 
    if ("metrx_f_ch4_production_t_exp_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_CH4_PRODUCTION_T_EXP_1' value='", pft.traits[which(pft.names == "metrx_f_ch4_production_t_exp_1")], "' /> \n"), collapse="")
    }
    
    #143 METRX_F_DECOMP_T_EXP_1 - 
    if ("metrx_f_decomp_t_exp_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DECOMP_T_EXP_1' value='", pft.traits[which(pft.names == "metrx_f_decomp_t_exp_1")], "' /> \n"), collapse="")
    }
    
    #145 METRX_F_DECOMP_CLAY_1 - 
    if ("metrx_f_decomp_clay_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_DECOMP_CLAY_1' value='", pft.traits[which(pft.names == "metrx_f_decomp_clay_1")], "' /> \n"), collapse="")
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
    
    #158 METRX_F_NIT_NO_M_EXP_1 -
    if ("metrx_f_nit_no_m_exp_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_NO_M_EXP_1' value='", pft.traits[which(pft.names == "metrx_f_nit_no_m_exp_1")], "' /> \n"), collapse="")
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
    
    #165 METRX_F_NIT_N2O_M_EXP_1 -
    if ("metrx_f_nit_n2o_t_exp_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_F_NIT_N2O_M_EXP_1' value='", pft.traits[which(pft.names == "metrx_f_nit_n2o_t_exp_1")], "' /> \n"), collapse="")
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
    
    #240 METRX_TILL_SIMULATION_1 -
    if ("metrx_till_simulation_1" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_TILL_SIMULATION_1' value='", pft.traits[which(pft.names == "metrx_till_simulation_1")], "' /> \n"), collapse="")
    }
    
    #241 METRX_TILL_SIMULATION_2 -
    if ("metrx_till_simulation_2" %in% pft.names) {
      h.2 <- paste(h.2, paste0("\t\t<par name='METRX_TILL_SIMULATION_2' value='", pft.traits[which(pft.names == "metrx_till_simulation_2")], "' /> \n"), collapse="")
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
    
    
    # Soil conditions
    if("bd_1" %in% pft.names) {bd_1 <- pft.traits[which(pft.names == "bd_1")]}
    if("bd_2" %in% pft.names) {bd_2 <- pft.traits[which(pft.names == "bd_2")]}
    
    if("clay_1" %in% pft.names) {clay_1 <- pft.traits[which(pft.names == "clay_1")]}
    if("clay_2" %in% pft.names) {clay_2 <- pft.traits[which(pft.names == "clay_2")]}
    
    if("corg_1" %in% pft.names) {corg_1 <- pft.traits[which(pft.names == "corg_1")]}
    if("corg_2" %in% pft.names) {corg_2 <- pft.traits[which(pft.names == "corg_2")]}
    
    if("norg_1" %in% pft.names) {norg_1 <- pft.traits[which(pft.names == "norg_1")]}
    if("norg_2" %in% pft.names) {norg_2 <- pft.traits[which(pft.names == "norg_2")]}
    
    if("ph_1" %in% pft.names) {ph_1 <- pft.traits[which(pft.names == "ph_1")]}
    if("ph_2" %in% pft.names) {ph_2 <- pft.traits[which(pft.names == "ph_2")]}
    
    if("sand_1" %in% pft.names) {sand_1 <- pft.traits[which(pft.names == "sand_1")]}
    if("sand_2" %in% pft.names) {sand_2 <- pft.traits[which(pft.names == "sand_2")]}
    
    if("vangenuchten_n" %in% pft.names) {vangenuchten_n <- pft.traits[which(pft.names == "vangenuchten_n")]}
    if("vangenuchten_alpha" %in% pft.names) {vangenuchten_alpha <- pft.traits[which(pft.names == "vangenuchten_alpha")]}
    
    if("scel_1" %in% pft.names) {scel_1 <- pft.traits[which(pft.names == "scel_1")]}
    if("scel_2" %in% pft.names) {scel_2 <- pft.traits[which(pft.names == "scel_2")]}
    
    if("sks_1" %in% pft.names) {sks_1 <- pft.traits[which(pft.names == "sks_1")]}
    if("sks_2" %in% pft.names) {sks_2 <- pft.traits[which(pft.names == "sks_2")]}
    
    if("wcmin_1" %in% pft.names) {wcmin_1 <- pft.traits[which(pft.names == "wcmin_1")]}
    if("wcmin_2" %in% pft.names) {wcmin_2 <- pft.traits[which(pft.names == "wcmin_2")]}
    
    if("wcmax_1" %in% pft.names) {wcmax_1 <- pft.traits[which(pft.names == "wcmax_1")]}
    if("wcmax_2" %in% pft.names) {wcmax_2 <- pft.traits[which(pft.names == "wcmax_2")]}
    
  }
  
  
  ## INITIAL SOIL CONDITIONS
  # Surface Layer
  soil_surface <- paste0("bd='", bd_1, "' clay='", clay_1, "' corg='", corg_1, "' norg='", norg_1, "' ph='", ph_1,
                         "' vangenuchten_n='", vangenuchten_n, "' vangenuchten_alpha='", vangenuchten_alpha, "' sand='",
                         sand_1, "' scel='", scel_1, "' sks='", sks_1, "' wcmax='", wcmax_1, "' wcmin='", wcmin_1, "'")
  
  # Second Layer
  soil_second_layer <- paste0("bd='", bd_2, "' clay='", clay_2, "' corg='", corg_2, "' norg='", norg_2, "' ph='", ph_2,
                              "' vangenuchten_n='", vangenuchten_n, "' vangenuchten_alpha='", vangenuchten_alpha,"' sand='",
                              sand_2, "' scel='", scel_2, "' sks='", sks_2, "' wcmax='", wcmax_2, "' wcmin='", wcmin_2, "'")
  
  
  
  ## Writing and saving species- and siteparameters + initial soil conditions
  
  # Handle the populating of speciesparameters after we have read the info from priors
  speciesparfile <- gsub("@Info@", paste(a.1,
                                         b.1.1,b.2.1,b.3.1,
                                         b.1.2,b.2.2,b.3.2,
                                         a.2), speciesparfile)
  
  # Write to a new xml-file, which will be used on a run. Every simulation run will have
  # their own set of speciesparameters values
  writeLines(speciesparfile, con = file.path(settings$rundir, run.id, "speciesparameters.xml"))
  
  
  
  # Handle the populating of siteparameters
  siteparfile <- gsub("@Info@", h.2, siteparfile)
  
  # Write siteparameters
  writeLines(siteparfile, con = file.path(settings$rundir, run.id, "siteparameters.xml"))
  
  
  
  
  # Default site file
  sitefile <- readLines(con = system.file("site_template.xml", package = "PEcAn.LDNDC"), n = -1)
  
  # Populate sitefile with given parameter. NOTE! Initial conditions are expected here to
  # be given as a parameters, so those should be found from prior table
  # If not set, then use the values defined earlier on this file.
  sitefile <- gsub("@Info_Surface_Layer@", soil_surface, sitefile)
  sitefile <- gsub("@Info_Second_Layer@", soil_second_layer, sitefile)
  
  
  # Write soil conditions
  writeLines(sitefile, con = file.path(settings$rundir, run.id, "site.xml"))
  
  ##
  
  
  
  # Default events file, need to change later on. Only takes care of some initial biomass and
  # then some random events. Not made for real use, but testing.
  eventsfile <- readLines(con = system.file("events_template.xml", package = "PEcAn.LDNDC"), n = -1)
  
  #eventsfile <- gsub("@Startdate@", as.Date(settings$run$start.date, format = "%Y/%m/%d"), eventsfile)
  #eventsfile <- gsub("@Event_1_Time@", as.Date(settings$run$start.date, format = "%Y/%m/%d") + 170, eventsfile)
  #eventsfile <- gsub("@Event_2_Time@", as.Date(settings$run$start.date, format = "%Y/%m/%d") + 200, eventsfile)
  #eventsfile <- gsub("@Event_3_Time@", as.Date(settings$run$start.date, format = "%Y/%m/%d") + 290, eventsfile)
  
  writeLines(eventsfile, con = file.path(settings$rundir, run.id, "events.xml"))
  
  
  
  
  # Default setup file, need to change later on
  setupfile <- readLines(con = system.file("setup.xml", package = "PEcAn.LDNDC"), n = -1)
  writeLines(setupfile, con = file.path(settings$rundir, run.id, "setup.xml"))
  
  
  
  # Use ready airchemistry file for now
  airchemistry <- readLines(con = system.file("airchemistry.txt", package = "PEcAn.LDNDC"), n = -1)
  writeLines(airchemistry, con = file.path(settings$rundir, run.id, "airchemistry.txt"))
  
  
  #------------------------
  
} # write.config.LDNDC
