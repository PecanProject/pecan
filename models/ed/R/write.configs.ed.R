##------------------------------------------------------------------------------
##Copyright (c) 2012 University of Illinois, NCSA.
##All rights reserved. This program and the accompanying materials
##are made available under the terms of the 
##University of Illinois/NCSA Open Source License
##which accompanies this distribution, and is available at
##http://opensource.ncsa.illinois.edu/license.html
##------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------------#
##Functions to prepare and write out ED2.2 config.xml files for MA, SA, and Ensemble runs
##-------------------------------------------------------------------------------------------------#


##-------------------------------------------------------------------------------------------------#
PREFIX_XML <- '<?xml version="1.0"?>\n<!DOCTYPE config SYSTEM "ed.dtd">\n'

## TODO: Update this script file to use the database for setting up ED2IN and config files
##-------------------------------------------------------------------------------------------------#

##-------------------------------------------------------------------------------------------------#
##' convert parameters from PEcAn database default units to ED defaults
##' 
##' Performs model specific unit conversions on a a list of trait values,
##' such as those provided to write.config
##' @name convert.samples.ED
##' @title Convert samples for ed
##' @param trait.samples a matrix or dataframe of samples from the trait distribution
##' @return matrix or dataframe with values transformed
##' @author Shawn Serbin, David LeBauer, Carl Davidson, Ryan Kelly
convert.samples.ED <- function(trait.samples){
  DEFAULT.LEAF.C <- 0.48
  DEFAULT.MAINTENANCE.RESPIRATION <- 1/2
  ## convert SLA from m2 / kg leaf to m2 / kg C 
  
  if('SLA' %in% names(trait.samples)){
    sla <- as.numeric(trait.samples[['SLA']])
    trait.samples[['SLA']] <- sla / DEFAULT.LEAF.C
  }
  
  ## convert leaf width / 1000
  if('leaf_width' %in% names(trait.samples)){
    lw <- as.numeric(trait.samples[['leaf_width']])
    trait.samples[['leaf_width']] <- lw / 1000.0
  }
  
  if('root_respiration_rate' %in% names(trait.samples)) {
    rrr1 <- as.numeric(trait.samples[['root_respiration_rate']])
    rrr2 <-  rrr1 * DEFAULT.MAINTENANCE.RESPIRATION
    trait.samples[['root_respiration_rate']] <- arrhenius.scaling(rrr2, old.temp = 25, 
                                                                  new.temp = 15)
  }
  
  if('Vcmax' %in% names(trait.samples)) {
    vcmax <- as.numeric(trait.samples[['Vcmax']])
    trait.samples[['Vcmax']] <- arrhenius.scaling(vcmax, old.temp = 25, new.temp = 15)
    
    ## Convert leaf_respiration_rate_m2 to dark_resp_factor; requires Vcmax
    if('leaf_respiration_rate_m2' %in% names(trait.samples)) {
      leaf_resp = as.numeric(trait.samples[['leaf_respiration_rate_m2']])
      
      ## First scale variables to 15 degC
      trait.samples[['leaf_respiration_rate_m2']] <- 
        arrhenius.scaling(leaf_resp, old.temp = 25, new.temp = 15)
      
      ## Calculate dark_resp_factor -- Will be depreciated when moving from older versions of ED2
      trait.samples[['dark_respiration_factor']] <- trait.samples[['leaf_respiration_rate_m2']]/
        trait.samples[['Vcmax']]
      
      ## Remove leaf_respiration_rate from trait samples
      trait.samples$leaf_respiration_rate_m2 = NULL # !!!WHY DO WE DO THIS??!!!
      
    } ## End dark_respiration_factor loop
  } ## End Vcmax  
  # for debugging conversions
  #save(trait.samples, file = file.path(settings$outdir, 'trait.samples.Rdata'))
  
  # return converted samples
  return(trait.samples)
}
#==================================================================================================#


##-------------------------------------------------------------------------------------------------#
##' Writes an xml and ED2IN config files for use with the Ecological Demography model.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##' @name write.config.ED2
##' @title Write ED configuration files
##' @param trait.values Named list of trait values, with names corresponding to PFT
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @param defaults list of defaults to process. Default=settings$constants
##' @return configuration file and ED2IN namelist for given run
##' @export
##' @author David LeBauer, Shawn Serbin, Carl Davidson, Alexey Shiklomanov
##-------------------------------------------------------------------------------------------------#
write.config.ED2 <- function(trait.values, settings, run.id, defaults=settings$constants){
  
  
  jobsh <- write.config.jobsh.ED2(settings = settings,
                                  run.id = run.id)

  writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))

  ## Write ED2 config.xml file
  xml <- write.config.xml.ED2(defaults = defaults,
                              settings = settings,
                              trait.values = trait.values)

  saveXML(xml, file = file.path(settings$rundir, run.id, "config.xml"), indent=TRUE, prefix = PREFIX_XML)
  
  startdate <- as.Date(settings$run$start.date)
  enddate <- as.Date(settings$run$end.date)
  
  ##----------------------------------------------------------------------
  ## Edit ED2IN file for runs
  if (!is.null(settings$model$edin) && file.exists(settings$model$edin)) {
    ed2in.text <- readLines(con=settings$model$edin, n=-1)
  } else {
    filename <- system.file(settings$model$edin, package = "PEcAn.ED2")
    if (filename == "") {
      if (!is.null(settings$model$revision)) {
        rev <- gsub('^r', '', settings$model$revision)
      } else {
        model <- db.query(paste("SELECT * FROM models WHERE id =", settings$model$id), params=settings$database$bety)
        rev <- gsub('^r', '', model$revision)
      }
      filename <- system.file(paste0("ED2IN.r", rev), package = "PEcAn.ED2")
    }
    if (filename == "") {
      logger.severe("Could not find ED template")
    }
    logger.info("Using", filename, "as template")
    ed2in.text <- readLines(con=filename, n=-1)
  }
  
  metstart <- tryCatch(format(as.Date(settings$run$site$met.start), "%Y"), error=function(e) settings$run$site$met.start)
  metend   <- tryCatch(format(as.Date(settings$run$site$met.end), "%Y"), error=function(e) settings$run$site$met.end)
  
  ed2in.text <- gsub('@SITE_LAT@', settings$run$site$lat, ed2in.text)
  ed2in.text <- gsub('@SITE_LON@', settings$run$site$lon, ed2in.text)
  ed2in.text <- gsub('@SITE_MET@', settings$run$inputs$me$path, ed2in.text)
  ed2in.text <- gsub('@MET_START@', metstart, ed2in.text)
  ed2in.text <- gsub('@MET_END@', metend, ed2in.text)
  
  if(is.null(settings$model$phenol.scheme)){
    print(paste("no phenology scheme set; \n",
                "need to add <phenol.scheme> tag under <model> tag in settings file"))
  } else if(settings$model$phenol.scheme==1) {
    ## Set prescribed phenology switch in ED2IN
    ed2in.text <- gsub('@PHENOL_SCHEME@', settings$model$phenol.scheme, ed2in.text)
    ## Phenology filename
    ed2in.text <- gsub('@PHENOL@', settings$model$phenol, ed2in.text)
    ## Set start year of phenology
    ed2in.text <- gsub('@PHENOL_START@', settings$model$phenol.start, ed2in.text)
    ## Set end year of phenology
    ed2in.text <- gsub('@PHENOL_END@', settings$model$phenol.end, ed2in.text)
    
    ## If not prescribed set alternative phenology scheme.
  } else {
    ed2in.text <- gsub(' @PHENOL_SCHEME@', settings$model$phenol.scheme, ed2in.text)
    # Insert blanks into ED2IN file so ED2 runs without error
    ed2in.text <- gsub('@PHENOL@', "", ed2in.text)
    ed2in.text <- gsub('@PHENOL_START@', "", ed2in.text)
    ed2in.text <- gsub('@PHENOL_END@', "", ed2in.text)
  }
  
  ##----------------------------------------------------------------------
  # Get prefix of filename, append to dirname. 
  # Assumes pattern 'DIR/PREFIX.lat<REMAINDER OF FILENAME>' 
  # Slightly overcomplicated to avoid error if path name happened to contain '.lat'
  
  # when pss or css not exists, case 0
  if (is.null(settings$run$inputs$pss$path)|is.null(settings$run$inputs$css$path)){
    ed2in.text <- gsub('@INIT_MODEL@', 0, ed2in.text)
    ed2in.text <- gsub('@SITE_PSSCSS@', "", ed2in.text)
  }
  else{
    prefix.pss <- sub(".lat.*", "", settings$run$inputs$css$path)
    prefix.css <- sub(".lat.*", "", settings$run$inputs$pss$path)
    # pss and css prefix is not the same, kill
    if (!identical(prefix.pss , prefix.css)){
      logger.severe("ED2 css/pss/ files have different prefix")
    }
    # pss and css are both present
    else{
      value <- 2
      # site exists 
      if (!is.null(settings$run$inputs$site$path)){
        prefix.sites <- sub(".lat.*", "", settings$run$inputs$site$path)
        # sites and pss have different prefix name, kill 
        if (!identical (prefix.sites, prefix.pss)){
          logger.severe("ED2 sites/pss/ files have different prefix")
        }
        #sites and pass same prefix name, case 3
        else{
          value <- 3
        }
      }
    }
    ed2in.text <- gsub('@INIT_MODEL@', value, ed2in.text)
    ed2in.text <- gsub('@SITE_PSSCSS@', paste0(prefix.pss, '.'), ed2in.text)
  } 
  
  
  ##----------------------------------------------------------------------
  
  ed2in.text <- gsub('@ED_VEG@', settings$run$inputs$veg$path, ed2in.text)
  ed2in.text <- gsub('@ED_SOIL@', settings$run$inputs$soil$path, ed2in.text)
  ed2in.text <- gsub('@ED_LU@', settings$run$inputs$lu$path, ed2in.text)
  ed2in.text <- gsub('@ED_THSUM@', ifelse(str_sub(settings$run$inputs$thsum$path, -1) == "/",
                                          settings$run$inputs$thsum$path,
                                          paste0(settings$run$inputs$thsum$path, "/")), ed2in.text)
  
  ##----------------------------------------------------------------------
  ed2in.text <- gsub('@START_MONTH@', format(startdate, "%m"), ed2in.text)
  ed2in.text <- gsub('@START_DAY@', format(startdate, "%d"), ed2in.text)
  ed2in.text <- gsub('@START_YEAR@', format(startdate, "%Y"), ed2in.text)
  ed2in.text <- gsub('@END_MONTH@', format(enddate, "%m"), ed2in.text)
  ed2in.text <- gsub('@END_DAY@', format(enddate, "%d"), ed2in.text)
  ed2in.text <- gsub('@END_YEAR@', format(enddate, "%Y"), ed2in.text)
  
  ##----------------------------------------------------------------------
  if (is.null(settings$run$host$scratchdir)) {
    modeloutdir <- file.path(settings$run$host$outdir, run.id)
  } else {
    modeloutdir <- file.path(settings$run$host$scratchdir, run.id)
  }
  ed2in.text <- gsub('@OUTDIR@', modeloutdir, ed2in.text)
  ed2in.text <- gsub('@ENSNAME@', run.id, ed2in.text)
  ed2in.text <- gsub('@CONFIGFILE@', file.path(settings$run$host$rundir, run.id, "config.xml"), ed2in.text)
  #ed2in.text <- gsub('@CONFIGFILE@',"config.xml", ed2in.text) # for ED2.r81 on Kang.  Temporary hack
  
  ##----------------------------------------------------------------------
  ed2in.text <- gsub('@FFILOUT@', file.path(modeloutdir, "analysis"), ed2in.text)
  ed2in.text <- gsub('@SFILOUT@', file.path(modeloutdir, "history"), ed2in.text)
  
  ##----------------------------------------------------------------------
  writeLines(ed2in.text, con = file.path(settings$rundir, run.id, "ED2IN"))
}
#==================================================================================================#


##-------------------------------------------------------------------------------------------------#
##'
##' @name write.run.ED
##' @title Function to generate ED2.2 model run script files
##' @export
##' @author David LeBauer, Shawn Serbin, Rob Kooper, Mike Dietze
##' @import PEcAn.utils
##-------------------------------------------------------------------------------------------------#
write.run.ED <- function(settings){
  scratch = paste(Sys.getenv("USER"),"/",settings$run$scratch, sep='')
  run.script.template = system.file("run.template.ED2", package="PEcAn.ED2")
  run.text <- scan(file = run.script.template, 
                   what="character",sep='@', quote=NULL, quiet=TRUE)
  run.text <- gsub('TMP', paste("/scratch/",scratch,sep=""), run.text)
  run.text <- gsub('BINARY', settings$model$binary, run.text)
  run.text <- gsub('OUTDIR', settings$run$host$outdir, run.text)
  runfile <- paste(settings$outdir, 'run', sep='')
  writeLines(run.text, con = runfile)
  if(settings$run$host$name == 'localhost') {
    system(paste('cp ', runfile, settings$run$host$rundir))
  }else{
    system(paste("rsync -outi ", runfile , ' ', settings$run$host$name, ":",
                 settings$run$host$rundir, sep = ''))
  }
}
#==================================================================================================#


##-------------------------------------------------------------------------------------------------#
##' Clear out old config and ED model run files.
##'
##' @name remove.config.ED2
##' @title Clear out old config and ED model run files.
##' @return nothing, removes config files as side effect
##' @export
##' @author Shawn Serbin, David LeBauer
remove.config.ED2 <- function(main.outdir = settings$outdir, settings) {
  
  
  print(" ")
  print("---- Removing previous ED2 config files and output before starting new run ----")
  print(" ")
  
  todelete <- dir(settings$outdir,
                  pattern = c('/c.*', '/ED2INc.*'),
                  recursive=TRUE, full.names = TRUE)
  
  if(length(todelete>0)){
    file.remove(todelete)
  } 
  rm(todelete)
  
  ## Remove model run configs and model run log files on local/remote host
  if(!settings$run$host$name == 'localhost'){
    ## Remove model run congfig and log files on remote host
    config <- system(paste("ssh ", settings$run$host$name, " 'ls ", 
                           settings$run$host$rundir, 
                           "c.*'", sep = ''), intern = TRUE)
    ed2in <- system(paste("ssh ", settings$run$host$name, " 'ls ", 
                          settings$run$host$rundir, 
                          "ED2INc.", "*'", sep = ''), intern = TRUE)
    output <- paste(settings$run$host$outdir,
                    system(paste("ssh ", settings$run$host$name, " 'ls ", 
                                 settings$run$host$outdir,
                                 "'", sep = ''), intern = TRUE),sep="/")
    if(length(config) > 0 | length(ed2in) > 0) {
      todelete <- c(config,ed2in[-grep('log', ed2in)],output) ## Keep log files
      
      ## Very slow method.  NEEDS UPDATING
      for(i in todelete){
        print(i)
        system(paste("ssh -T ", settings$run$host$name, " 'rm ",i,"'",sep=""))
      }
      
    }
  }
}
#==================================================================================================#
#' @name write.config.xml.ED2
#' @title Write ED2 config.xml file
#' @details Refactored by Alexey Shiklomanov to allow use in PEcAn RTM module.
#' @export
#' @param settings PEcAn settings file. Settings required for this script are: model$revision, model$config.header, constants
#' @param trait.values List of trait values with which to replace defaults
#' @param defaults List of defaults to process. Default = settings$constants
#' @return R XML object containing full ED2 XML file
#' @author David LeBauer, Shawn Serbin, Carl Davidson, Alexey Shiklomanov
write.config.xml.ED2 <- function(settings, trait.values, defaults=settings$constants){

  ## Find history file
  ## TODO this should come from the database
  histfile <- paste("data/history.r", settings$model$revision, ".csv", sep='')
  if (file.exists(system.file(histfile, package="PEcAn.ED2"))) {
    #print(paste("--- Using ED2 History File: ","data/history.r", settings$model$revision, ".csv", sep=''))
    edhistory <- read.csv2(system.file(histfile, package="PEcAn.ED2"), sep=";", 
                           stringsAsFactors=FALSE, dec='.')
  } else {
    #print("--- Using Generic ED2 History File: data/history.csv")
    edhistory <- read.csv2(system.file("data/history.csv",  package="PEcAn.ED2"), sep=";",
                           stringsAsFactors=FALSE, dec='.')
  }

  edtraits <- names(edhistory)
  data(pftmapping)

  ## Get ED2 specific model settings and put into output config xml file
  xml <- listToXml(settings$model$config.header, 'config')

  ## Process the names in defaults. Runs only if names(defaults) are null or have at least one instance of name attribute "pft". Otherwise, AS assumes that names in defaults are already set to the corresponding PFT names.
  currentnames <- names(defaults)
  if(is.null(currentnames) | "pft" %in% currentnames){
    newnames <- sapply(defaults, "[[", "name")
    newnames.notnull <- which(!sapply(newnames, is.null))
    names(defaults)[newnames.notnull] <- newnames[newnames.notnull]
  }
  
  for(i in seq_along(trait.values)){
    group <- names(trait.values)[i]
    if(group == "env"){
      
      ## set defaults from config.header
      
      ##
      
    } else {
      # Make this agnostic to the way PFT names are defined in `trait.values` -- either directly as list names or as object "name" within each sublist is fine
      if(group == "pft"){
        pft <- trait.values[[i]]$name
      } else {
        pft <- group
      }
      # TODO: Not sure if this is how this is supposed to work, but idea is to check for pft.number in defaults (settings$constants) first
      pft.number <- defaults[[pft]]$num
      if(is.null(pft.number)){
          pft.number <- pftmapping$ED[which(pftmapping == pft)]
      }
      if(length(pft.number) == 0){
          logger.error(pft, 'was not matched with a number in settings$constants or pftmapping data. Consult the PEcAn instructions on defining new PFTs.')
          stop('Unable to set PFT number')
      }
        # TODO: Also modify web app to not default to 1 

      ## Get default trait values from ED history
      vals <- as.list(edhistory[edhistory$num == pft.number,])

      ## Convert trait values to ED units
      converted.trait.values <- convert.samples.ED(trait.values[[i]])
      
      ## Selectively replace defaults with trait values
      vals <- modifyList(vals, converted.trait.values)

      ## Convert settings constants to ED units
      converted.defaults <- convert.samples.ED(defaults[[pft]]$constants)

      ## Selectively replace defaults and trait values with constants from settings
      if(!is.null(converted.defaults)) vals <- modifyList(vals, converted.defaults)

      pft.xml <- listToXml(vals, 'pft')
      xml <- append.xmlNode(xml, pft.xml)
    }
  }
  return(xml)
}

#==================================================================================================#
#' @name write.config.jobsh.ED2
#' @title Write ED2 config.xml file
#' @description Function for writing job.sh file for ED2 runs
#' @details Refactored by Alexey Shiklomanov to allow use in PEcAn RTM module.
#' @export
#' @param settings PEcAn settings list. For this function, need the following: 
#' run$host$rundir, run$host$outdir, run$host$scratchdir, 
#' run$host$clearscratch, run$jobtemplate, model$job.sh, run$host$job.sh, 
#' run$site$lat, run$site$lon, run$inputs$met$path, run$start.date, 
#' run$end.date, model$binary
#' @param run.id PEcAn run ID
#' @return Character vector containing job.sh file
#' @author David LeBauer, Shawn Serbin, Carl Davidson, Alexey Shiklomanov

write.config.jobsh.ED2 <- function(settings, run.id){
  # find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, run.id)
  outdir <- file.path(settings$run$host$outdir, run.id)
  
  # command if scratch is used
  if (is.null(settings$run$host$scratchdir)) {
    modeloutdir <- outdir
    copyscratch <- "# no need to copy from scratch"
    clearscratch <- "# no need to clear scratch"
  } else {
    modeloutdir <- file.path(settings$run$host$scratchdir, run.id)
    copyscratch <- paste("rsync", "-a", paste0('"', file.path(modeloutdir, "*"), '"'), paste0('"', outdir, '"'))
    if (is.null(settings$run$host$clearscratch) || is.na(as.logical(settings$run$host$clearscratch)) || as.logical(settings$run$host$clearscratch)) {
      clearscratch <- paste("rm", "-rf", paste0('"', modeloutdir, '"'))
    } else {
      clearscratch <- "# scratch is not cleared"
    }
  }
  # create launch script (which will create symlink)
  if (!is.null(settings$run$jobtemplate) && file.exists(settings$run$jobtemplate)) {
    jobsh <- readLines(con=settings$run$jobtemplate, n=-1)
  } else {
    jobsh <- readLines(con=system.file("template.job", package = "PEcAn.ED2"), n=-1)
  }
  
  # create host specific setttings
  hostspecific <- ""
  if (!is.null(settings$model$job.sh)) {
    hostspecific <- paste(hostspecific, sep="\n", paste(settings$model$job.sh, collapse="\n"))
  }
  if (!is.null(settings$run$host$job.sh)) {
    hostspecific <- paste(hostspecific, sep="\n", paste(settings$run$host$job.sh, collapse="\n"))
  }

  # create job.sh
  jobsh <- gsub('@HOSTSPECIFIC@', hostspecific, jobsh)

  jobsh <- gsub('@SITE_LAT@', settings$run$site$lat, jobsh)
  jobsh <- gsub('@SITE_LON@', settings$run$site$lon, jobsh)
  jobsh <- gsub('@SITE_MET@', settings$run$inputs$met$path, jobsh)
  
  jobsh <- gsub('@SCRATCH_COPY@', copyscratch, jobsh)
  jobsh <- gsub('@SCRATCH_CLEAR@', clearscratch, jobsh)
  
  jobsh <- gsub('@START_DATE@', settings$run$start.date, jobsh)
  jobsh <- gsub('@END_DATE@', settings$run$end.date, jobsh)
  
  jobsh <- gsub('@OUTDIR@', outdir, jobsh)
  jobsh <- gsub('@RUNDIR@', rundir, jobsh)
  
  jobsh <- gsub('@BINARY@', settings$model$binary, jobsh)

  return(jobsh)
  
}


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
