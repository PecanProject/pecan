#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes a JULES config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.JULES
##' @title Write JULES configuration files
##' @param defaults list of defaults to process
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for JULES for given run
##' @author Mike Dietze, Rob Kooper
##' 
##' @export
##-------------------------------------------------------------------------------------------------#
write.config.JULES <- function(defaults, trait.values, settings, run.id){
  # constants
  molH2O_to_grams = 18.01528
  
  # find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, run.id)
  outdir <- file.path(settings$run$host$outdir, run.id)

  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$run$jobtemplate) && file.exists(settings$run$jobtemplate)) {
    jobsh <- readLines(con=settings$run$jobtemplate, n=-1)
  } else {
    jobsh <- readLines(con=system.file("template.job", package = "PEcAn.JULES"), n=-1)
  }
  
  # create host specific settings
  hostspecific <- ""
  if (!is.null(settings$model$job.sh)) {
    hostspecific <- paste(hostspecific, sep="\n", paste(settings$model$job.sh, collapse="\n"))
  }
  if (!is.null(settings$run$host$job.sh)) {
    hostspecific <- paste(hostspecific, sep="\n", paste(settings$run$host$job.sh, collapse="\n"))
  }

  # create job.sh
  jobsh <- gsub('@HOSTSPECIFIC@', hostspecific, jobsh)
  jobsh <- gsub('@OUTDIR@', outdir, jobsh)
  jobsh <- gsub('@RUNDIR@', rundir, jobsh)
  jobsh <- gsub('@RUNID@', run.id, jobsh)
  jobsh <- gsub('@BINARY@', settings$model$binary, jobsh)
  writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
  #-----------------------------------------------------------------------
  ### Copy templated NAMELIST files to rundir
  if(!is.null(settings$model$config) && dir.exists(settings$model$config)){
    template.dir = settings$model$config
  } else {
    template.dir = file.path(system.file(package = "PEcAn.JULES"),paste0("template_nml_", settings$model$revision))
  }
  system2("cp",args = paste0(template.dir,"/* ",rundir))
  
  ## ------------------ Detect time step of met data ------------------
  met.file = dir(dirname(settings$run$inputs$met$path),pattern = basename(settings$run$inputs$met$path),full.names = TRUE)[1]
  met.header = system(paste("ncdump -h ",met.file),intern = TRUE)
  id = grep("time:delta_t",met.header) 
  if(length(id)>0){
    delta_t = met.header[id]
    #Example: delta_t = '		time:delta_t = "0000-00-00 03:00:00" ;'
    dt = diff(strptime(c("00:00:00",substring(strsplit(delta_t,'"')[[1]][2],11)),format="%T"))
    units(dt) <- "secs"
  } else {
    id = grep("time_coverage_resolution",met.header)
    if(length(id)>0){
      delta_t = met.header[id]
      dt = strsplit(strsplit(delta_t,'"')[[1]][2]," ")[[1]]
      if(dt[2]=="min") {dt[1] = as.numeric(dt[1])*60; dt[2]="sec"}
      if(dt[2]=="sec") {
        dt = dt[1]
      } else {
        print(c("write.config.JULES timestep not detected",dt))
        dt = 1800
      }
    } else {
      print(c("write.config.JULES timestep not detected",dt))
      dt = 1800
    }
  }
  ## --------------------  END DETECT TIMESTEP --------------------
  
  ## Edit DRIVE.NML to set met variables
  drive.file <- file.path(rundir,"drive.nml")
  drive.text <- readLines(con=drive.file, n=-1)
  drive.text <- gsub('@MET_START@', settings$run$site$met.start, drive.text)
  drive.text <- gsub('@MET_END@', settings$run$site$met.end, drive.text)
  drive.text <- gsub('@SITE_MET@', settings$run$inputs$met$path, drive.text)
  drive.text <- gsub('@DT@',as.numeric(dt),drive.text)
  writeLines(drive.text, con = drive.file)

  ## Edit TIMESTEPS.NML to set start/end date
  timesteps.file <- file.path(rundir,"timesteps.nml")
  timesteps.text <- readLines(con=timesteps.file, n=-1)
  timesteps.text <- gsub('@START_DATE@', format(as.Date(settings$run$start.date),"%F %H:%M:%S"), timesteps.text)
  timesteps.text <- gsub('@END_DATE@', format(as.Date(settings$run$end.date),"%F %H:%M:%S"), timesteps.text)
  writeLines(timesteps.text, con = timesteps.file)
  
  ## Edit MODEL_GRID.NML to set lat/lon
  grid.file <- file.path(rundir,"model_grid.nml")
  grid.text <- readLines(con=grid.file, n=-1)
  grid.text <- gsub('@SITE_LAT@', settings$run$site$lat, grid.text)
  grid.text <- gsub('@SITE_LON@', settings$run$site$lon, grid.text)  
  writeLines(grid.text, con = grid.file)
  
  ## Edit OUTPUT.NML to set run.id
  output.file <- file.path(rundir,"output.nml")
  output.text <- readLines(con=output.file, n=-1)
  output.text <- gsub('@RUNID@', run.id, output.text)
  output.text <- gsub('@OUTDIR@', outdir, output.text)
  writeLines(output.text, con = output.file)
 
  ## Edit ANCILLARIES.NML
  # tile frac
  # soil physical parameters
  
  
  ## --------------------- Edit PFT_PARAMS.NML to set model parameters  -------------------------
  pft.file <- file.path(rundir,"pft_params.nml")
  pft.text <- readLines(con=pft.file, n=-1)
  if(length(pft.text)<3) logger.severe("No DEFAULT parameters provided for JULES")

  ##split NML into variable list and parameter values
  pft.parse = unlist(strsplit(pft.text[2:(length(pft.text)-1)],"="))
  variables = pft.parse[seq(1,length(pft.parse),by=2)]
  defaults = pft.parse[seq(2,length(pft.parse),by=2)]
  
  ## expand out NML multiplication notation
  mult = grep("*",defaults,fixed=TRUE)
  for(i in mult){
    tmp = unlist(strsplit(defaults[i],"*",fixed=TRUE))
    defaults[i] = paste0(rep(tmp[2],tmp[1]),collapse = "")
  }
  
  ## parse into matrix of current defaults
  defaults = read.csv(textConnection(defaults),header=FALSE)
  defaults = defaults[,-ncol(defaults)] ## remove extra column created by NML line ending comma
  rownames(defaults) <- variables
  colnames(defaults) <- c("DEC","EV","C3","C4","SH")[1:ncol(defaults)]
    
  ## match selected PFTs to correct defaults
  npft = length(trait.values)-1
  pft.id = rep(NA,npft)
  for(i in 1:npft){
    pft.name = names(trait.values)[i]
    if(grepl("DEC",pft.name)|grepl("decid",pft.name)|grepl("hardwood",pft.name)) pft.id[i] = 1
    if(grepl("EV",pft.name)|grepl("everg",pft.name)|grepl("conif",pft.name)) pft.id[i] = 2
    if(grepl("C3",pft.name,ignore.case = TRUE)) pft.id[i] = 3
    if(grepl("C4",pft.name,ignore.case = TRUE)) pft.id[i] = 4
    if(is.na(pft.id[i])) pft.id[i] = 5
  }
  
  ## reorder defaults to match supplied PFTs
  pft.ord = pft.id
  unused = NULL
  if(length(pft.ord)<5){
    unused = (1:5)[!(1:5 %in% pft.ord)]
    pft.ord = c(pft.ord,unused)[1:5]
    unused = (npft+1):5
  } 
  defaults = defaults[,pft.ord]
  
  ## Loop over PFTS
  for(i in 1:npft){
    pft = trait.values[[i]]
    
    for(v in 1:length(pft)){
      
      ## convert names and units
      ## see JULES variable definitions at http://jules-lsm.github.io/vn4.2/namelists/pft_params.nml.html
      var = names(pft)[v]
      if(var == "height") names(pft)[v] <- "canht_ft_io"           ## Canopy height, JULES: meters  NOTE: prognostic if TRIFFID is on;  BETY: meters
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
      if(var == "quantum_efficiency") names(pft)[v] <- "alpha_io"  ## JULES: mol CO2 per mol PAR photons; BETY: fraction
      if(var == "leaf_reflect_nir")   names(pft)[v] <- "alnir_io"    ## Leaf reflection coefficient for NIR
      ## alniru_io ## Upper limit for the leaf reflection coefficient for NIR
      ## alnirl_io ## Lower limit for the leaf reflection coefficient for NIR
      if(var == "leaf_reflect_par")   names(pft)[v] <- "alpar_io"
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
      if(var == "leaf_turnover_rate"){         ## Minimum turnover rate for leaves (/360days); BETY: year-1
          names(pft)[v] <- "g_leaf_0_io"
          pft[v] = pft[v]/365*360
      }
      if(var == "cuticular_cond"){ ## Minimum leaf conductance for H2O (m s-1) -> m3/m2 s-1, BETY: umol H2O m-2 s-1
        names(pft)[v] <- "glmin_io"  
        pft[v] = pft[v] * molH2O_to_grams # 10^6 umol/mol * 18 g/mol * 1kg(= 1 mm)/1000g * 1m/1000mm
      }
      ## infil_f_io ## Infiltration enhancement factor
      if(var == "extinction_coefficient	") names(pft)[v] = "kext_io" ## Light extinction coefficient - used with Beer’s Law for light absorption through tile canopies
      ## kpar_io  ## PAR Extinction coefficient (m2 leaf / m2 ground)
      ## neff_io  ## Scale factor relating Vcmax with leaf nitrogen concentration
      ## nl0_io   ## Top leaf nitrogen concentration (kg N/kg C)
      ## nr_nl_io ## Ratio of root nitrogen concentration to leaf nitrogen concentration
          ## calcuate this from leaf and root N as well as leaf and root C:N
      ## ns_nl_io ## Ratio of stem nitrogen concentration to leaf nitrogen concentration
      
      
        
      ## detect any unmatched variables
      mch = which(rownames(defaults) == names(pft[v]))
      if(length(mch) != 1){
        logger.warn("unmatched parameter in write.configs.JULES", names(pft[v]), "in PFT", names(trait.values)[i])
      } else {
        
        ## insert into defaults table
        defaults[mch,i] = pft[v]     
      }
    } ## end loop over parameters
  }   ## end loop over PFTs
    
  ## write out new file
  write(pft.text[1],pft.file)  ## Header
  for(i in 1:nrow(defaults)){
    write(paste0(rownames(defaults)[i],"=",paste(defaults[i,],collapse=","),","),pft.file,append=TRUE)
  }
  write(pft.text[length(pft.text)],pft.file,append=TRUE) ## Footer
    
  ## set npft to the value needed for surface type definition
  npft = max(c(npft,5))

  ## ---------------------------   END PFTS ------------------------------------------
  
  
  ## Edit jules_surface_types.nml to set correct number of PFTS
  
  ## Edit INITIAL_CONDITIONS.NML
  # soil carbon
  # LAI
  
}
