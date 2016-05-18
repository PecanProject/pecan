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
      var = names(pft)[v]
      if(var == "quantum_efficiency") names(pft)[v] <- "alpha_io"  ## double check units
      
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
