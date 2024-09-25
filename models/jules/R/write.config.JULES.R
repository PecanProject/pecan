##' Writes a JULES config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for JULES for given run
##' @author Mike Dietze, Rob Kooper
##'
##' @export
##' @examples
##' \dontrun{
##'   write.config.JULES(defaults, trait.values, settings, run.id)
##' }
##-------------------------------------------------------------------------------------------------#
write.config.JULES <- function(defaults, trait.values, settings, run.id) {
  # constants
  molH2O_to_grams <- 18.01528
  leafC <- 0.48
  useTRIFFID <- "TRIFFID" %in% toupper(names(settings$model))
  start_date <- settings$run$start.date
  run.local <- settings$host$name == "localhost" | settings$host$name == PEcAn.remote::fqdn()

  # find out where to write run/output
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)
  local.outdir <- file.path(settings$outdir,run.id)
  local.rundir <- file.path(settings$rundir, run.id)

  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.JULES"), n = -1)
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

  # create job.sh
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)

  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  jobsh <- gsub("@RUNID@", run.id, jobsh)
  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
  writeLines(jobsh, con = file.path(local.rundir, "job.sh"))
  Sys.chmod(file.path(local.rundir, "job.sh"))

  #-----------------------------------------------------------------------
  ### Copy templated NAMELIST files to local rundir
  if (!is.null(settings$model$config) && dir.exists(settings$model$config)) {
    template.dir <- settings$model$config
  } else {
    template.dir <- file.path(system.file(package = "PEcAn.JULES"), paste0("template_nml_", settings$model$revision))
  }
  system2("cp", args = paste0(template.dir, "/* ", local.rundir))

  ## ------------------ Detect time step of met data ------------------
  nchar.path <- nchar(settings$run$inputs$met$path)
  if(substring(settings$run$inputs$met$path,nchar.path)=="/"){
    prefix <- ""
    met.regexp <- NULL
    met.dir <- settings$run$inputs$met$path
  } else {
    met.regexp <- prefix <- basename(settings$run$inputs$met$path)
    met.dir <- dirname(settings$run$inputs$met$path)
  }
  if(nchar(prefix)>0) prefix <- paste0(prefix,".")
  if(run.local){
    dt <- detect.timestep(met.dir,met.regexp,start_date)
  } else {
    rmt.cmd <- paste0("PEcAn.JULES::detect.timestep(met.dir='",
                      met.dir,"', met.regexp='",met.regexp,
                      "', start_date= '",start_date,"')")
    dt <- PEcAn.remote::remote.execute.R(script=rmt.cmd,host=settings$host,verbose=TRUE)
  }
  ## -------------------- END DETECT TIMESTEP --------------------

  ## PEcAn SPIN-UP: symlink met files, change start date.
  if(!is.null(settings$spin)){
    if(run.local){
      ## run local
      start_date <- PEcAn.data.atmosphere::spin.met(dirname(settings$run$inputs$met$path),
                                                  prefix,
                                                  settings$run$site$met.start,
                                                  settings$run$site$met.end,
                                                  settings$spin$nyear,
                                                  settings$spin$nsample,
                                                  settings$spin$resample,
                                                  start_date)
    } else {
      ## run spin for remote met
      rmt.cmd <- paste0("PEcAn.data.atmosphere::spin.met(in.path ='",
                        dirname(settings$run$inputs$met$path),
                        "', in.prefix ='",prefix,
                        "', start_date = '",settings$run$site$met.start,
                        "', end_date = '",settings$run$site$met.end,
                        "', nyear = ",settings$spin$nyear,
                        ",  nsample = ",settings$spin$nsample,
                        ",  resample = ",settings$spin$resample,
                        ",  run_start_date = '",start_date,
                        "')")
      start_date <- PEcAn.remote::remote.execute.R(script=rmt.cmd,host=settings$host,verbose=TRUE)
    }
  }  ## end spin

  ## set up date strings
  start_char <- format(as.Date(start_date), "%F %H:%M:%S")
  year_char <- strsplit(start_char,"-")[[1]][1]
  if(nchar(year_char)<4){
    start_char <- paste0(formatC(as.numeric(year_char),width = 4, format = "d", flag = "0"),
                         substr(start_char,nchar(year_char)+1,nchar(start_char))
    )
  }
  end_char <- format(as.Date(settings$run$end.date), "%F %H:%M:%S")
  met_end_char <- format(as.Date(settings$run$site$met.end), "%F %H:%M:%S")


  ## Edit DRIVE.NML to set met variables
  drive.file <- file.path(local.rundir, "drive.nml")
  drive.text <- readLines(con = drive.file, n = -1)
  drive.text <- gsub("@MET_START@", start_char, drive.text)
  drive.text <- gsub("@MET_END@", met_end_char, drive.text)
  drive.text <- gsub("@SITE_MET@", file.path(dirname(settings$run$inputs$met$path),prefix), drive.text)
  drive.text <- gsub("@DT@", as.numeric(dt), drive.text)
  writeLines(drive.text, con = drive.file)

  ## Edit TIMESTEPS.NML to set start/end date
  timesteps.file <- file.path(local.rundir, "timesteps.nml")
  timesteps.text <- readLines(con = timesteps.file, n = -1)
  timesteps.text <- gsub("@START_DATE@", start_char, timesteps.text)
  timesteps.text <- gsub("@END_DATE@", end_char, timesteps.text)
  writeLines(timesteps.text, con = timesteps.file)

  ## Edit PRESCRIBED_DATA.NML to add CO2 data
  if("co2" %in% tolower(names(settings$run$inputs))){
    pd.file <- file.path(local.rundir, "prescribed_data.nml")
    pd.text <- readLines(con = pd.file, n = -1)

    ## SPIN the CO2 file
    if(!is.null(settings$spin)){
      dt.co2 = PEcAn.utils::ud_convert(as.numeric(as.Date(settings$run$end.date)-
                                             as.Date(settings$run$start.date)),"days","years")
      co2.dat <- utils::read.table(settings$run$inputs$co2$path,header=FALSE)
      co2.per.year <- round(nrow(co2.dat)/dt.co2)

      ## as first pass, just repeat the whole sequence. Not doing resampling. Not worrying about how to loop the file
      co2.dat <- c(as.vector(co2.dat[seq_len(as.numeric(settings$spin$nyear)*co2.per.year+1),]),unlist(co2.dat))

      co2.local <- file.path(local.rundir,basename(settings$run$inputs$co2$path))
      utils::write.table(co2.dat,file = co2.local,col.names = FALSE,row.names = FALSE)
      if(run.local){
        settings$run$inputs$co2$path <- co2.local
      } else {
        co2.remote <- file.path(rundir,basename(settings$run$inputs$co2$path))
        settings$run$inputs$co2$path <- co2.remote
      }

      PEcAn.logger::logger.debug("co2.local",co2.local,length(co2.dat))
    }

    ## add CO2 file
    pdn <- length(pd.text)
    pd.text[pdn+1] <- paste0("")
    pd.text[pdn+2] <- paste0("&JULES_PRESCRIBED_DATASET")
    pd.text[pdn+3] <- paste0("data_start  = '",start_char,"',")
    pd.text[pdn+4] <- paste0("data_end    = '",end_char,"',")
    pd.text[pdn+5] <- paste0("data_period=-1")
    pd.text[pdn+6] <- paste0("file='",settings$run$inputs$co2$path,"',")
    pd.text[pdn+7] <- paste0("nvars=1")
    pd.text[pdn+8] <- paste0("var='co2_mmr'")
    pd.text[pdn+9] <- paste0("interp='i'")
    pd.text[pdn+10] <- paste0("/")

    # EXAMPLE
    # &JULES_PRESCRIBED_DATASET
    # data_start  = '0850-01-01 00:00:00',
    # data_end    = '2011-01-01 00:00:00',
    #
    # data_period=-1
    #
    # file='../../../../phase1a_env_drivers_v4/Paleon_CO2_mmr.txt'
    #
    # nvars=1
    # var='co2_mmr'
    # interp='i'
    #
    # /

    ## update n_datasets
    nd_i  <- grep("n_datasets",pd.text)
    pd_nd <- as.numeric(sub(",","",strsplit(pd.text[nd_i],"=")[[1]][2]))
    pd.text[nd_i] = paste0("n_datasets=",pd_nd+1,",")

    writeLines(pd.text, con = pd.file)
  }

  ## Edit MODEL_GRID.NML to set lat/lon
  grid.file <- file.path(local.rundir, "model_grid.nml")
  grid.text <- readLines(con = grid.file, n = -1)
  grid.text <- gsub("@SITE_LAT@", settings$run$site$lat, grid.text)
  grid.text <- gsub("@SITE_LON@", settings$run$site$lon, grid.text)
  writeLines(grid.text, con = grid.file)

  ## Edit OUTPUT.NML to set run.id
  output.file <- file.path(local.rundir, "output.nml")
  output.text <- readLines(con = output.file, n = -1)
  output.text <- gsub("@RUNID@", run.id, output.text)
  output.text <- gsub("@OUTDIR@", outdir, output.text)
  if(useTRIFFID){
    ## find rows in output.nml
    out_nvar_i   <- grep("nvars",output.text)
    out_varname_i  <- grep("var_name",output.text)
    out_var_i    <- grep("var",output.text)
      out_var_i  <- out_var_i[which(!(out_var_i %in% c(out_nvar_i,out_varname_i)))] ## process of elimination
    out_type_i   <- grep("output_type",output.text)
    len <- nchar(trimws(output.text))

    ## update number of variables
    out_nvar <- as.numeric(sub(",","",strsplit(output.text[out_nvar_i],"=")[[1]][2]))
    output.text[out_nvar_i] = paste0("nvars = ",out_nvar+3,",")
    output.text[out_type_i] = paste0("output_type = ",out_nvar+3,"*'M',")

    ## add to out_varname
    k <- which(rev((len > 0)[1:(out_type_i-1)]))[1] ## how many lines back is previous block
    output.text[out_type_i-k] <- paste0(output.text[out_type_i-k],
                                        " 'Fcomp', 'TotLivBio_PFT', 'Height',")

    ## add extra output variables
    k <- which(rev((len > 0)[1:(out_varname_i-1)]))[1] ## how many lines back is previous block
    output.text[out_varname_i-k] <- paste0(output.text[out_varname_i-k],
                                        " 'frac', 'c_veg', 'canht',")
  }
  writeLines(output.text, con = output.file)

  ## Edit ANCILLARIES.NML tile frac soil physical parameters [[OPTIONAL]]
  if("soil" %in% names(settings$run$inputs)){
    ## open soil file
    soil <- settings$run$inputs$soil
    nc.soil <- ncdf4::nc_open(soil$path)

    ## extract JULES variables
    in.soil <- list()
    in.soil[['b']]      <- ncdf4::ncvar_get(nc.soil,"soil_hydraulic_b")
    # sathh
    in.soil[['satcon']] <- ncdf4::ncvar_get(nc.soil,"soil_hydraulic_conductivity_at_saturation")
    in.soil[['satcon']] <- PEcAn.utils::ud_convert(in.soil[['satcon']],"m s-1","mm s-1")
    in.soil[['sm_sat']] <- ncdf4::ncvar_get(nc.soil,"volume_fraction_of_water_in_soil_at_saturation")
    #sm_crit
    in.soil[['sm_wilt']] <- ncdf4::ncvar_get(nc.soil,"volume_fraction_of_condensed_water_in_soil_at_wilting_point")
    hcap    <- ncdf4::ncvar_get(nc.soil,"soil_thermal_capacity") ## J/kg/K
    bulk    <- ncdf4::ncvar_get(nc.soil,"soil_bulk_density") ## kg m-3
    in.soil[['hcap']]    <- hcap * bulk ## J/kg/K * kg m-3 -> J m-3 K-1
    in.soil[['hcon']]    <- ncdf4::ncvar_get(nc.soil,"soil_thermal_conductivity") ## W m-1 K-1
    in.soil[['albsoil']] <- ncdf4::ncvar_get(nc.soil,"soil_albedo")
    ncdf4::nc_close(nc.soil)

    ## open namelist
    anc.file <- file.path(local.rundir, "ancillaries.nml")
    anc.text <- readLines(con = anc.file, n = -1)

    ## parse variable names
    const_val_i <- grep("const_val",anc.text)
    const_val <- strsplit(strsplit(anc.text[const_val_i],"=")[[1]][2],",")[[1]]
    soil_var_i  <- grep("^var",anc.text)
    soil_var <- strsplit(strsplit(anc.text[soil_var_i],"=")[[1]][2],",")[[1]]
    soil_var <- gsub("'","",soil_var)

    ## substitute in new values
    for(i in seq_along(soil_var)){
      k = which(names(in.soil) == soil_var[i])
      if(length(k)==1){
        const_val[i] <- in.soil[[k]][1] ## for now only use surface values
                                                  ## need to figure out how to set depth profile later
      }
    }

    ## insert back into text
    anc.text[const_val_i] <- paste0("const_val=",paste(const_val,sep = "",collapse = ","),",")
    writeLines(anc.text, con = anc.file)

  } ## end ancillary

  ## PARSE JULES_VEGETATION.NML some of these settings affect which parameter settings are used
  veg.file     <- file.path(local.rundir, "jules_vegetation.nml")
  veg.text     <- readLines(con = veg.file, n = -1)
  l_trait_phys <- grep("l_trait_phys", veg.text)
  if (length(l_trait_phys) > 0) {
    l_trait_phys <- grepl("true", veg.text[l_trait_phys], ignore.case = TRUE)
  } else {
    l_trait_phys <- FALSE  ## default value
  }
  ## Turn on TRIFFID??
  if(useTRIFFID){

    l_triffid <- grep("l_triffid",veg.text)
    veg.text[l_triffid] <- sub("false",'true',veg.text[l_triffid])

    l_trif_eq <- grep("l_trif_eq",veg.text)
    if(length(l_trif_eq) == 0){
      veg.text[length(veg.text)] <- "l_trif_eq=.false.,"
      veg.text[length(veg.text)+1] <- "/"
    } else {
      veg.text[l_trif_eq] <- sub("true",'false',veg.text[l_triffid]) # set to FALSE
    }

    l_veg_compete <- grep("l_veg_compete",veg.text)
    if(length(l_veg_compete) == 0){
      veg.text[length(veg.text)] <- "l_veg_compete=.true.,"
      veg.text[length(veg.text)+1] <- "/"
    } else {
      veg.text[l_veg_compete] <- sub('false',"true",veg.text[l_triffid]) # set to TRUE
    }

    l_triffid_period <- grep("l_triffid_period",veg.text)
    if(length(l_triffid_period) == 0){
      veg.text[length(veg.text)] <- "triffid_period=10,"
      veg.text[length(veg.text)+1] <- "/"
    } ## no else because right now not adjusting dynamically

  }
  writeLines(veg.text, con = veg.file)

  ## --------------------- Edit PFT_PARAMS.NML to set model parameters -------------------------
  pft.file <- file.path(local.rundir, "pft_params.nml")
  pft.text <- readLines(con = pft.file, n = -1)
  if (length(pft.text) < 3) {
    PEcAn.logger::logger.severe("No DEFAULT parameters provided for JULES")
  }

  ## split NML into variable list and parameter values
  pft.parse <- unlist(strsplit(pft.text[2:(length(pft.text) - 1)], "="))
  variables <- pft.parse[seq(1, length(pft.parse), by = 2)]
  defaults <- pft.parse[seq(2, length(pft.parse), by = 2)]

  ## expand out NML multiplication notation
  mult <- grep("*", defaults, fixed = TRUE)
  for (i in mult) {
    tmp <- unlist(strsplit(defaults[i], "*", fixed = TRUE))
    defaults[i] <- paste0(rep(tmp[2], tmp[1]), collapse = "")
  }

  ## parse into matrix of current defaults
  defaults <- utils::read.csv(textConnection(defaults), header = FALSE)
  defaults <- defaults[, -ncol(defaults)]  ## remove extra column created by NML line ending comma
  rownames(defaults) <- variables
  colnames(defaults) <- c("DEC", "EV", "C3", "C4", "SH")[1:ncol(defaults)]

  ## match selected PFTs to correct defaults
  npft <- length(trait.values) - 1
  pft.id <- rep(NA, npft)
  for (i in seq_len(npft)) {
    pft.name <- names(trait.values)[i]
    if (grepl("DEC", pft.name) | grepl("decid", pft.name) | grepl("hardwood", pft.name)) {
      pft.id[i] <- 1
    } else if (grepl("EV", pft.name) | grepl("everg", pft.name) | grepl("conif", pft.name)) {
      pft.id[i] <- 2
    } else if (grepl("C3", pft.name, ignore.case = TRUE)) {
      pft.id[i] <- 3
    } else if (grepl("C4", pft.name, ignore.case = TRUE)) {
      pft.id[i] <- 4
    } else if (is.na(pft.id[i])) {
      pft.id[i] <- 5
    } else {
      PEcAn.logger::logger.severe("Unknown PFT")
    }
  }

  ## reorder defaults to match supplied PFTs  ### WON'T WORK WITH TRIFFID
  # pft.ord <- pft.id
  # unused <- NULL
  # if (length(pft.ord) < 5) {
  #   unused <- (1:5)[!(1:5 %in% pft.ord)]
  #   pft.ord <- c(pft.ord, unused)[1:5]
  #   unused <- (npft + 1):5
  # }
  # defaults <- defaults[, pft.ord]

  ## Loop over PFTS
  for (i in seq_len(npft)) {
    pft <- trait.values[[i]]

    for (v in seq_along(pft)) {

      ## convert names and units see JULES variable definitions at
      ## http://jules-lsm.github.io/vn4.2/namelists/pft_params.nml.html
      var <- names(pft)[v]
      if (var == "height") {
        names(pft)[v] <- "canht_ft_io"  ## Canopy height, JULES: meters  NOTE: prognostic if TRIFFID is on;  BETY: meters
      }
      ## c3_io     ## C3 photosynthesis = 1; C4 = 0;  BETY: unmatched
      ## orient_io ## leaf angle distribution. 0 - Spherical. 1 - Horizontal;  BETY: unmatched
      ## a_wl_io   ## Allometric coefficient relating the target woody biomass to the leaf area index (kg carbon m-2); BETY: unmatched
      ## possible to estimate from wood allometry, leaf allometry, and SLA?
      ## a_ws_io   ## Woody biomass as a multiple of live stem biomass; BETY: unmatched
      ## possible to estimate from DBH allometries?
      ## albsnc_max_io ## Snow-covered albedo for large leaf area index.; BETY: unmatched
      ## albsnc_min_io ## Snow-covered albedo for zero leaf area index.; BETY: unmatched
      ## albsnf_max_io ## Snow-free albedo for large LAI.; BETY: unmatched
      ## link to RTM package
      ## albsnf_maxu_io ## Upper bound for the snow-free albedo for large LAI, when scaled to match input obs
      ## albsnf_maxl_io ## Lower bound for the snow-free albedo for large LAI, when scaled to match input obs
      if (var == "quantum_efficiency") {
        names(pft)[v] <- "alpha_io"  ## JULES: mol CO2 per mol PAR photons; BETY: fraction
      }
      if (var == "leaf_reflect_nir") {
        names(pft)[v] <- "alnir_io"  ## Leaf reflection coefficient for NIR
      }
      ## alniru_io ## Upper limit for the leaf reflection coefficient for NIR
      ## alnirl_io ## Lower limit for the leaf reflection coefficient for NIR
      if (var == "leaf_reflect_par")  {
        names(pft)[v] <- "alpar_io"
      }
      ## alparu_io ## Upper limit for the leaf reflection coefficient for VIS
      ## alparl_io ## Lower limit for the leaf reflection coefficient for VIS
      ## b_wl_io   ## Allometric exponent relating the target woody biomass to the leaf area index.
      ## catch0_io ## Minimum canopy capacity (kg m-2).
      ## dcatch_dlai_io ## Rate of change of canopy capacity with LAI (kg m-2).
      ## Canopy capacity is calculated as catch0 + dcatch_dlai*lai
      ## dgl_dm_io ## Rate of change of leaf turnover rate with moisture availability
      ## dgl_dt_io ## Rate of change of leaf turnover rate with temperature (K-1)
      ## dqcrit_io ## Critical humidity deficit (kg H2O per kg air).
      ## dz0v_dh_io ## Rate of change of vegetation roughness length for momentum with height.
      ## Roughness length is calculated as dz0v_dh * canht_ft
      ## eta_sl_io ## Live stemwood coefficient (kg C/m/LAI)
      ## fd_io     ## Scale factor for dark respiration
      ## **** look up equation
      ## fsmc_of_io ## Moisture availability below which leaves are dropped
      ## f0_io     ## CI / CA for DQ = 0
      if (var == "leaf_turnover_rate") {
        names(pft)[v] <- "g_leaf_0_io"
        pft[v] <- pft[v] / 365 * 360
      }
      if (var == "cuticular_cond") {
        ## Minimum leaf conductance for H2O (m s-1) -> m3/m2 s-1, BETY: umol H2O m-2 s-1
        names(pft)[v] <- "glmin_io"
        pft[v] <- pft[v] * molH2O_to_grams * 1e-12  # 10^-6 mol/umol * 18 g/mol * 1kg(= 1 mm)/1000g * 1m/1000mm
      }
      ## infil_f_io ## Infiltration enhancement factor
      if (var == "extinction_coefficient\t") {
        names(pft)[v] <- "kext_io"  ## Light extinction coefficient - used with Beer’s Law for light absorption through tile canopies
      }
      ## kpar_io  ## PAR Extinction coefficient (m2 leaf / m2 ground)
      ## neff_io  ## Scale factor relating Vcmax with leaf nitrogen concentration
      ## nl0_io   ## Top leaf nitrogen concentration (kg N/kg C)
      ## nr_nl_io ## Ratio of root nitrogen concentration to leaf nitrogen concentration
      ## calcuate this from leaf and root N as well as leaf and root C:N
      ## ns_nl_io ## Ratio of stem nitrogen concentration to leaf nitrogen concentration
      ## omega_io ## Leaf scattering coefficient for PAR
      ## omegau_io ## Upper limit for the leaf scattering coefficient for PAR
      ## omegal_io ## Lower limit for the leaf scattering coefficient for PAR
      ## omnir_io  ## Leaf scattering coefficient for NIR
      ## omniru_io ## Upper limit for the leaf scattering coefficient for NIR
      ## omnirl_io ## Lower limit for the leaf scattering coefficient for NIR
      if (var == "growth_resp_factor") {
        names(pft)[v] <- "r_grow_io"  ## Growth respiration fraction (fraction of NPP = GPP - Ra)
      }
      ## rootd_ft_io ## Root depth e-folding length assuming exponential model (meters).  BETY: m2 kg-1
      if (var == "SLA") {
        if (l_trait_phys) {
          names(pft)[v] <- "lma_io"  ## Leaf mass per unit area (kgLeaf m-2).
          pft[v] <- 1 / pft[v]
        } else {
          names(pft)[v] <- "xsigl_io"  ## Specific density of leaf carbon (kg C/m2 leaf).
          pft[v] <- leafC / pft[v]
        }
      }
      ## tleaf_of_io ## Temperature below which leaves are dropped (K).
      if (var == "pstemp_min") {
        names(pft)[v] <- "tlow_io"  ## Lower temperature for photosynthesis (deg C); BETY: degrees C
      }
      if (var == "pstemp_max") {
        names(pft)[v] <- "tupp_io"  ## Upper temperature for photosynthesis (deg C)
      }
      if (var == "emis_v") {
        ## Surface emissivity. BETY: per mil
        names(pft)[v] <- "emis_pft_io"
        pft[v] <- pft[v] / 1000
      }
      ## z0hm_pft_io ## Ratio of the roughness length for heat to the roughness length for momentum
      ## fl_o3_ct_io ## Critical flux of O3 to vegetation (mmol m-2 s-1)
      ## dfp_dcuo_io ## Fractional reduction of photosynthesis with the cumulative uptake of O3 by leaves (mmol m-2)
      ## ief_io      ## Isoprene Emission Factor (μg g-1 h-1)
      ## tef_io      ## Monoterpene Emission Factor (μg g-1 h-1)
      ## mef_io      ## Methanol Emission Factor (μg g-1 h-1)
      ## aef_io      ## Acetone Emission Factor (μg g-1 h-1)
      ## ci_st_io    ## Leaf-internal CO2concentration at standard conditions (Pa)
      ## gpp_st_io   ## Gross primary production (GPP) at standard conditions (kgC m-2 s-1)
      if (var == "leafN") {
        names(pft)[v] <- "nmass_io"
        pft[v] <- pft[v] / 100
      }
      ## There is a linear relationship between Vcmax and Narea. Previously Vcmax was calculated as the product of nl0 and neff.
      ## This is now replaced by a linear regression based on data reported in Kattge et al. 2009. Vint is the y-intercept, vsl is the slope.
      ## vint_io     ## Units: μmol CO2 m-2 s-1.
      ## vsl_io      ## Units: μmol CO2 gN-1 s-1
      ## kn_io       ## Decay of nitrogen through the canopy
      if (var == "leaf_respiration Q10") {
        names(pft)[v] <- "q10_leaf_io"
      }

      ## detect any unmatched variables
      mch <- which(rownames(defaults) == names(pft[v]))
      if (length(mch) != 1) {
        PEcAn.logger::logger.warn("unmatched parameter in write.configs.JULES", names(pft[v]), "in PFT",
                    names(trait.values)[i])
      } else {
        ## insert into defaults table
#        defaults[mch, i] <- pft[v]   ## STATIC allows pft reorder
        defaults[mch, pft.id[i]] <- pft[v]    ## TRIFFID enforces pft order
      }
    }  ## end loop over parameters
  }  ## end loop over PFTs

  ## write out new file
  write(pft.text[1], pft.file)  ## Header
  for (i in seq_len(nrow(defaults))) {
    write(paste0(rownames(defaults)[i], "=", paste(defaults[i, ], collapse = ","), ","), pft.file,
          append = TRUE)
  }
  write(pft.text[length(pft.text)], pft.file, append = TRUE)  ## Footer

  ## set npft to the value needed for surface type definition
  npft <- max(c(npft, 5))

  ## --------------------------- END PFTS ------------------------------------------

  ## Edit jules_surface_types.nml to set correct number of PFTS

  ## Edit INITIAL_CONDITIONS.NML soil carbon LAI
  if(useTRIFFID){
    ic.file <- file.path(local.rundir, "initial_conditions.nml")
    ic.text <- readLines(con = ic.file, n = -1)

    ## update number of variables
    ic_nvar_i   <- grep("nvars",ic.text)
    ic_nvar     <- as.numeric(sub(",","",strsplit(ic.text[ic_nvar_i],"=")[[1]][2]))
    ic.text[ic_nvar_i] <- paste0("nvars = ",ic_nvar+2,",")

    ## update use_file
    use_file <- grep("use_file",ic.text)
    ic.text[use_file] <- paste0(ic.text[use_file],".true.,.true.,")

    ## update var
    ic_var <- grep("^var=",ic.text)
    ic.text[ic_var] <- paste0(ic.text[ic_var],",'canht','frac',")

    ## write namelist
    writeLines(ic.text, con = ic.file)

    ## also load and parse IC dat file
    ic.dat <- file.path(local.rundir, "initial_conditions.dat")
    ic.text <- readLines(con = ic.dat, n = -1)
    ic.text[2] <- paste(ic.text[2]," 5.0 5.0 0.5 0.5 0.5 0.2 0.2 0.2 0.2 0.2 0.0 0.0 0.0 0.0")
    writeLines(ic.text, con = ic.dat)
  }
} # write.config.JULES


#' Detect timestep of JULES met files
#'
#' @param met.dir path to directory containing met files
#' @param met.regexp pattern to match to find a met file in met.dir.
#'  If more than one file matches, only the first will be used.
#' @param start_date When in file to start checking deltas.
#'  Not used if timestep can be detected from the CDF header.
#'
#' @return a difftime object
#' @export
#'
detect.timestep <- function(met.dir,met.regexp,start_date){
  met.file <- dir(met.dir, pattern = met.regexp, full.names = TRUE)[1]
  PEcAn.logger::logger.info("Detect timestep:",met.dir,met.regexp)
  met.header <- system(paste("ncdump -h ", met.file), intern = TRUE)
  id <- grep("time:delta_t", met.header)
  if (length(id) > 0) {
    delta_t <- met.header[id]
    # Example: delta_t = ' time:delta_t = '0000-00-00 03:00:00' ;'
    dt <- diff(strptime(c("00:00:00", substring(strsplit(delta_t, "\"")[[1]][2], 11)), format = "%T"))
    units(dt) <- "secs"
  } else {
    id <- grep("time_coverage_resolution", met.header)
    if (length(id) > 0) {
      delta_t <- met.header[id]
      dt <- strsplit(strsplit(delta_t, "\"")[[1]][2], " ")[[1]]
      if (dt[2] == "min") {
        dt[1] <- as.numeric(dt[1]) * 60
        dt[2] <- "sec"
      }
      if (dt[2] == "sec") {
        dt <- dt[1]
      } else {
        print(c("write.config.JULES timestep not detected", dt))
        dt <- 1800
      }
    } else {
      tlen <- grep("time =", met.header)
      if (length(tlen) > 0) {
        tlen <- as.numeric(gsub(pattern = "[^[:digit:]]", "", met.header[tlen]))
        diy <- PEcAn.utils::days_in_year(lubridate::year(as.Date(start_date)))
        dt <- 86400 / round(tlen/(diy))
      } else {
        print(c("write.config.JULES timestep not detected", dt))
        dt <- 1800
      }
    }
  }
  return(dt)
}
