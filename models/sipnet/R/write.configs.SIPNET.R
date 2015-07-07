#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------#
##' Writes a configuration files for your model
##' @name write.config.SIPNET
##' @title Writes a configuration files for SIPNET model
##' @export
##' @author Michael Dietze
#--------------------------------------------------------------------------------------------------#
write.config.SIPNET <- function(defaults, trait.values, settings, run.id, inputs=NULL, IC=NULL){
  ### WRITE sipnet.in
  template.in <- system.file("sipnet.in", package="PEcAn.SIPNET")
  config.text <- readLines(con=template.in, n=-1)
  writeLines(config.text, con = file.path(settings$rundir, run.id, "sipnet.in"))
  
  ### WRITE *.clim
  template.clim <- settings$run$input$met$path      ## read from settings
  if(!is.null(inputs)){                       ## override if specified in inputs
    if('met' %in% names(inputs)){
      template.clim <- inputs$met$path
    }
  }
  
  # find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, as.character(run.id))
  outdir <- file.path(settings$run$host$outdir, as.character(run.id))
  if (is.null(settings$run$host$qsub) && (settings$run$host$name == "localhost")) {
    rundir <- file.path(settings$rundir, as.character(run.id))
    outdir <- file.path(settings$modeloutdir, as.character(run.id))
  }
  
  # create launch script (which will create symlink)
  if (!is.null(settings$run$jobtemplate) && file.exists(settings$run$jobtemplate)) {
    jobsh <- readLines(con=settings$run$jobtemplate, n=-1)
  } else {
    jobsh <- readLines(con=system.file("template.job", package = "PEcAn.SIPNET"), n=-1)
  }
  
  jobsh <- gsub('@SITE_LAT@', settings$run$site$lat, jobsh)
  jobsh <- gsub('@SITE_LON@', settings$run$site$lon, jobsh)
  jobsh <- gsub('@SITE_MET@', template.clim, jobsh)
  
  jobsh <- gsub('@OUTDIR@', outdir, jobsh)
  jobsh <- gsub('@RUNDIR@', rundir, jobsh)
  
  jobsh <- gsub('@START_DATE@', settings$run$start.date, jobsh)
  jobsh <- gsub('@END_DATE@', settings$run$end.date, jobsh)
  
  jobsh <- gsub('@BINARY@', settings$model$binary, jobsh)

  writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
  ### WRITE *.param-spatial
  template.paramSpatial <- system.file("template.param-spatial",package="PEcAn.SIPNET")
  file.copy(template.paramSpatial, file.path(settings$rundir, run.id, "sipnet.param-spatial"))
  
  ### WRITE *.param
  template.param <- system.file("template.param",package="PEcAn.SIPNET")
  if("default.param" %in% names(settings$model)){template.param <- settings$model$default.param}
  
  param <- read.table(template.param)
  
  #### write INITAL CONDITIONS here ####
  if(!is.null(IC)){
    ic.names = names(IC)
    ##plantWoodInit gC/m2
    if("plantWood" %in% ic.names){
      param[which(param[,1] == 'plantWoodInit'),2] = IC$plantWood
    } 
    ##laiInit m2/m2
    if("lai" %in% ic.names){
      param[which(param[,1] == 'laiInit'),2] = IC$lai
    }
    ##litterInit gC/m2
    if("litter" %in% ic.names){
      param[which(param[,1] == 'litterInit'),2] = IC$litter
    }
    ##soilInit gC/m2
    if("soil" %in% ic.names){
      param[which(param[,1] == 'soilInit'),2] = IC$soil
    }
    ##litterWFracInit fraction
    if("litterWFrac" %in% ic.names){
      param[which(param[,1] == 'litterWFracInit'),2] = IC$litterWFrac
    }
    ##soilWFracInit fraction
    if("soilWFrac" %in% ic.names){
      param[which(param[,1] == 'soilWFracInit'),2] = IC$soilWFrac
    }
    ##snowInit cm water equivalent
    if("snow" %in% ic.names){
      param[which(param[,1] == 'snowInit'),2] = IC$snow
    }
    ##microbeInit mgC/g soil
    if("microbe" %in% ic.names){
      param[which(param[,1] == 'microbeInit'),2] = IC$microbe
    }
  }
  
  #### write run-specific environmental parameters here ####
  env.traits <- which(names(trait.values) %in% 'env')
  env.traits <- trait.values[[env.traits]]
  env.names <- names(env.traits)
  
  
  if("turn_over_time" %in% env.names){
    id = which(param[,1] == 'litterBreakdownRate')
    param[id,2] = env.traits[which(env.names == 'turn_over_time')]
  }
  
  #### write run-specific PFT parameters here ####
  ## Get parameters being handled by PEcAn
  pft.traits <- which(!(names(trait.values) %in% 'env'))[1]
  pft.traits <- unlist(trait.values[[pft.traits]])
  pft.names  <- names(pft.traits)

  ## Append/replace params specified as constants
  constant.traits <- unlist(defaults[[1]]$constants)
  constant.names  <- names(constant.traits)
  
  # Replace matches

    for(i in seq_along(constant.traits)) {
      ind = match(constant.names[i], pft.names)
      if(is.na(ind)) { # Add to list
        pft.names <- c(pft.names, constant.names[i])
        pft.traits <- c(pft.traits, constant.traits[i])
      } else {  # Replace existing value
        pft.traits[ind] <- constant.traits[i]
      }
    }

    
  # Remove NAs. Constants may be specified as NA to request template defaults. Note that it is "NA" (character) not actual NA due to being read in as XML
  pft.names  <- pft.names [ pft.traits != "NA" & !is.na(pft.traits) ]
  pft.traits <- pft.traits[ pft.traits != "NA" & !is.na(pft.traits) ]
  pft.traits <- as.numeric(pft.traits)
  
  # Leaf carbon concentration
  leafC = 0.48  #0.5
  if("leafC" %in% pft.names){
    leafC = pft.traits[which(pft.names == 'leafC')]
    id = which(param[,1] == 'cFracLeaf')
    param[id,2] = leafC*0.01 # convert to percentage from 0 to 1
  }

  # Specific leaf area converted to SLW
  SLA = NA
  id = which(param[,1] == 'leafCSpWt')
  if("SLA" %in% pft.names){
    SLA = pft.traits[which(pft.names == 'SLA')]
    param[id,2] = 1000*leafC*0.01/SLA
  } else {
    SLA = 1000*leafC/param[id,2]
  }
  
  # Maximum photosynthesis
  Amax = NA
  id = which(param[,1] == 'aMax')
  if("Amax" %in% pft.names){
    Amax = pft.traits[which(pft.names == 'Amax')]
    param[id,2] = Amax*SLA
  } else {
    Amax = param[id,2]*SLA
  }
  
  # Daily fraction of maximum photosynthesis
  if("AmaxFrac" %in% pft.names){
    param[which(param[,1] == 'aMaxFrac'),2] = pft.traits[which(pft.names == 'AmaxFrac')]
  }  
  
  ### Canopy extinction coefficiet (k)
  if("extinction_coefficient" %in% pft.names){
    param[which(param[,1] == 'attenuation'),2] = pft.traits[which(pft.names == 'extinction_coefficient')]
  }
  
  # Leaf respiration rate converted to baseFolRespFrac
  if("leaf_respiration_rate_m2" %in% pft.names){
    Rd = pft.traits[which(pft.names == 'leaf_respiration_rate_m2')]
    id = which(param[,1] == 'baseFolRespFrac')
    param[id,2] = max(min(Rd/Amax,1),0)
  }
  
  # Low temp threshold for photosynethsis
  if("Vm_low_temp" %in% pft.names){
    param[which(param[,1] == 'psnTMin'),2] = pft.traits[which(pft.names == 'Vm_low_temp')]
  }
  
  # Opt. temp for photosynthesis
  if("psnTOpt" %in% pft.names){
    param[which(param[,1] == 'psnTOpt'),2] = pft.traits[which(pft.names == 'psnTOpt')]
  }
  
  # Growth respiration factor (fraction of GPP)
  if("growth_resp_factor" %in% pft.names){
    param[which(param[,1] == 'growthRespFrac'),2] =
      pft.traits[which(pft.names == 'growth_resp_factor')]
  }
  
  ### !!! NOT YET USED
  #Jmax = NA
  #if("Jmax" %in% pft.names){
  #  Jmax = pft.traits[which(pft.names == 'Jmax')]
  ### Using Jmax scaled to 25 degC. Maybe not be the best approach
  #}
  
  #alpha = NA
  #if("quantum_efficiency" %in% pft.names){
  #  alpha = pft.traits[which(pft.names == 'quantum_efficiency')]
  #}
  
  # Half saturation of PAR.  PAR at which photosynthesis occurs at 1/2 theoretical maximum (Einsteins * m^-2 ground area * day^-1).
  #if(!is.na(Jmax) & !is.na(alpha)){
  # param[which(param[,1] == "halfSatPar"),2] = Jmax/(2*alpha)
  ### WARNING: this is a very coarse linear approximation and needs improvement *****
  ### Yes, we also need to work on doing a paired query where we have both data together.
  ### Once halfSatPar is calculated, need to remove Jmax and quantum_efficiency from param list so they are not included in SA
  #}
  ### !!!
  
  # Half saturation of PAR.  PAR at which photosynthesis occurs at 1/2 theoretical maximum (Einsteins * m^-2 ground area * day^-1).
  # Temporary implementation until above is working.
  if("half_saturation_PAR" %in% pft.names){ 
    param[which(param[,1] == 'halfSatPar'),2] = pft.traits[which(pft.names == 'half_saturation_PAR')]
  }
  
  # Ball-berry slomatal slope parameter m
  if("stomatal_slope.BB" %in% pft.names){
    id = which(param[,1] == 'm_ballBerry')
    param[id,2] = pft.traits[which(pft.names == 'stomatal_slope.BB')]
  }
  
  # Slope of VPD–photosynthesis relationship. dVpd = 1 - dVpdSlope * vpd^dVpdExp  
  if('dVPDSlope' %in% pft.names){
    param[which(param[,1] == 'dVpdSlope'),2] = pft.traits[which(pft.names == 'dVPDSlope')]
  }
  
  # VPD–water use efficiency relationship.  dVpd = 1 - dVpdSlope * vpd^dVpdExp
  if('dVpdExp' %in% pft.names){
    param[which(param[,1] == 'dVpdExp'),2] = pft.traits[which(pft.names == 'dVpdExp')]
  }
  
  # Leaf turnover rate average turnover rate of leaves, in fraction per day NOTE: read in as per-year rate!
  if("leaf_turnover_rate" %in% pft.names){
    param[which(param[,1] == 'leafTurnoverRate'),2] =
      pft.traits[which(pft.names == 'leaf_turnover_rate')]
  }
  
  # vegetation respiration Q10.
  if('veg_respiration_Q10' %in% pft.names){
    param[which(param[,1] == 'vegRespQ10'),2] = 
      pft.traits[which(pft.names == 'veg_respiration_Q10')]
  }
  
  # Base vegetation respiration. vegetation maintenance respiration at 0 degrees C (g C respired * g^-1 plant C * day^-1) 
  # NOTE: only counts plant wood C - leaves handled elsewhere (both above and below-ground: assumed for now to have same resp. rate)
  # NOTE: read in as per-year rate!
  if("stem_respiration_rate" %in% pft.names){
    vegRespQ10 = param[which(param[,1] == "vegRespQ10"),2]
    id = which(param[,1] == 'baseVegResp')
    ## Convert from umols CO2 kg s-1 to gC g day-1
    stem_resp_g <- (((pft.traits[which(pft.names=='stem_respiration_rate')])*(44.0096/1000000)*(12.01/44.0096))/1000)*86400
    ## use Q10 to convert stem resp from reference of 25C to 0C
    #param[id,2] = pft.traits[which(pft.names=='stem_respiration_rate')]*vegRespQ10^(-25/10)
    param[id,2] = stem_resp_g*vegRespQ10^(-25/10)
  }
  
  # turnover of fine roots (per year rate)
  if("root_turnover_rate" %in% pft.names){
    id = which(param[,1] == 'fineRootTurnoverRate')
    param[id,2] = pft.traits[which(pft.names == 'root_turnover_rate')]
  }
  
  # fine root respiration Q10
  if('fine_root_respiration_Q10' %in% pft.names){
    param[which(param[,1] == 'fineRootQ10'),2] =
      pft.traits[which(pft.names == 'fine_root_respiration_Q10')]
  }
  
  # base respiration rate of fine roots  (per year rate)
  if("root_respiration_rate" %in% pft.names){
    fineRootQ10 = param[which(param[,1] == "fineRootQ10"),2]
    id = which(param[,1] == 'baseFineRootResp')
    ## Convert from umols CO2 kg s-1 to gC g day-1
    root_resp_rate_g <- (((pft.traits[which(pft.names=='root_respiration_rate')])*(44.0096/1000000)*(12.01/44.0096))/1000)*86400
    ## use Q10 to convert stem resp from reference of 25C to 0C
    #param[id,2] = pft.traits[which(pft.names=='root_respiration_rate')]*fineRootQ10^(-25/10)
    param[id,2] = root_resp_rate_g*fineRootQ10^(-25/10)
  }
  
  # coarse root respiration Q10
  if('coarse_root_respiration_Q10' %in% pft.names){
    param[which(param[,1] == 'coarseRootQ10'),2] =
      pft.traits[which(pft.names == 'coarse_root_respiration_Q10')]
  } 
  
  ### ----- Phenology parameters
  # GDD leaf on
  if("GDD" %in% pft.names){
    param[which(param[,1] == 'gddLeafOn'),2] = pft.traits[which(pft.names == 'GDD')]
  }
  
  # Fraction of leaf fall per year (should be 1 for decid)
  if('fracLeafFall' %in% pft.names){
    param[which(param[,1] == 'fracLeafFall'),2] = pft.traits[which(pft.names == 'fracLeafFall')]
  }
  
  # Leaf growth.  Amount of C added to the leaf during the greenup period
  if('leafGrowth' %in% pft.names){
    param[which(param[,1] == 'leafGrowth'),2] = pft.traits[which(pft.names == 'leafGrowth')]
  }
  
  write.table(param,file.path(settings$rundir, run.id,"sipnet.param"),row.names=FALSE,col.names=FALSE,quote=FALSE)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' @name write.run.generic
##' @title Function to generate generic model run script files
##' @author <unknown>
##' @import PEcAn.utils
#--------------------------------------------------------------------------------------------------#
write.run.generic <- function(settings){
  run.script.template = system.file("data", "run.template.generic", package="PEcAn.generic")
  run.text <- scan(file = run.script.template, 
                   what="character",sep='@', quote=NULL, quiet=TRUE)
  run.text  <- gsub('TMP', paste("/scratch/",Sys.getenv("USER"),sep=""), run.text)
  run.text  <- gsub('BINARY', settings$run$host$ed$binary, run.text)
  run.text <- gsub('OUTDIR', settings$run$host$outdir, run.text)
  runfile <- paste(settings$outdir, 'run', sep='')
  writeLines(run.text, con = runfile)
  if(settings$run$host$name == 'localhost') {
    system(paste('cp ', runfile, settings$run$host$rundir))
  }else{
    system(paste("rsync -outi ", runfile , ' ', settings$run$host$name, ":", settings$run$host$rundir, sep = ''))
  }
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' Clear out previous SIPNET config and parameter files.
##'
##' @name remove.config.SIPNET
##' @title Clear out previous SIPNET config and parameter files.
##' @param main.outdir Primary PEcAn output directory (will be depreciated)
##' @param settings PEcAn settings file 
##' @return nothing, removes config files as side effect
##' @export
##'
##' @author Shawn Serbin, David LeBauer
remove.config.SIPNET <- function(main.outdir,settings) {
  
  ### Remove files on localhost
  if(settings$run$host$name == 'localhost'){
    files <- paste(settings$outdir,
                   list.files(path=settings$outdir, recursive=FALSE),sep="") # Need to change this to the run folder when implemented
    files <- files[-grep('*.xml',files)] # Keep pecan.xml file
    pft.dir <- strsplit(settings$pfts$pft$outdir,"/")[[1]]
    ln <- length(pft.dir)
    pft.dir <- pft.dir[ln]
    files <- files[-grep(pft.dir,files)] # Keep pft folder
    #file.remove(files,recursive=TRUE)
    system(paste("rm -r ",files,sep="",collapse=" "),ignore.stderr = TRUE) # remove files/dirs
    
    ### On remote host
  } else {
    print("*** WARNING: Removal of files on remote host not yet implemented ***")
  }
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################
