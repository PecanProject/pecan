##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------

PREFIX_XML <- '<?xml version="1.0"?>\n<!DOCTYPE config SYSTEM "biocro.dtd">\n'


##------------------------------------------------------------------------------------------------#
##' convert parameters from PEcAn database default units to biocro defaults
##' 
##' Performs model specific unit conversions on a a list of trait values,
##' such as those provided to write.config
##' @name convert.samples.BIOCRO
##' @title Convert samples for biocro
##' @param trait.samples a matrix or dataframe of samples from the trait distribution
##' @return matrix or dataframe with values transformed
##' @export
##' @author David LeBauer
convert.samples.BIOCRO <- function(trait.samples){

    if(is.list(trait.samples)) trait.samples <- as.data.frame(trait.samples)
    ## first rename variables
    trait.names <- colnames(trait.samples)
    trait.names[trait.names == "Vcmax"] <- "vmax"
    trait.names[trait.names == "Jmax"] <- "jmax"
    trait.names[trait.names == "leaf_respiration_rate_m2"] <- "Rd"
    trait.names[trait.names == "cuticular_cond"] <- "b0"
    trait.names[trait.names == "stomatal_slope.BB"] <- "b1"
    trait.names[trait.names == "SLA"] <- "Sp"
    trait.names[trait.names == "growth_respiration_coefficient"] <- "GrowthRespFraction"
    trait.names[trait.names == "extinction_coefficient_diffuse"] <- "kd"
    trait.names[trait.names == "chi_leaf"] <- "chi.l"
    trait.names[trait.names == "quantum_efficiency"] <- "alpha"
  
    colnames(trait.samples) <- trait.names    
    ## Partitioning coefficients: especially leaf
    ## phenology

    ## iRhizome
    ## iStem
    ## ifrRhizome
    ## ifrStem
 
    ## transform values with different units
    ## cuticular conductance - BETY default is umol; BioCro uses mol
    if("b0" %in% trait.names){
      trait.samples <- transform(trait.samples, b0 = ud.convert(b0 , "umol", "mol"))
    }
    if("Sp" %in% trait.names){
      trait.samples <- transform(trait.samples, Sp = ud.convert(Sp, "kg/m2", "g/cm2"))
    }
    if("vmax" %in% trait.names){##HAAAACK
      trait.samples <- transform(trait.samples, vmax = vmax)
    }
    if("Rd" %in% trait.names){##HAAAACK
      trait.samples <- transform(trait.samples, Rd = Rd)
    }
    
    # kd = k*omega from $e^{-kL\omega}$,
    #if(all(c("kd", "clumping") %in% trait.names)){
    #  trait.samples <- transform(trait.samples, kd = clumping * kd, clumping = NULL)
    #}
    return(trait.samples)
}
##==================================================================================================#

##' Writes a configuration files for the biocro model
##' 
##' @name write.config.BIOCRO
##' @title Write configuration files for the biocro model
##' @param defaults named list with default model parameter values 
##' @param trait.values named list (or dataframe of trait values)
##'  can either be a data.frame or named list of traits, e.g.
##' \code{data.frame(vmax = 1, b0 = 2)} or \code{list(vmax = 1, b0 = 2)}
##' @param settings pecan settings file configured for BioCro
##' @param run.id
##' @export
##' @return nothing, writes configuration file as side effect 
##' @author David LeBauer
write.config.BIOCRO <- function(defaults = NULL,
                                trait.values,
                                settings,
                                run.id) {
  
  ## find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, as.character(run.id))
  outdir <- file.path(settings$run$host$outdir, as.character(run.id))
  if (is.null(settings$run$host$qsub) && (settings$run$host$name == "localhost")) {
    rundir <- file.path(settings$rundir, as.character(run.id))
    outdir <- file.path(settings$modeloutdir, as.character(run.id))
  }
  
  ## create launch script (which will create symlink)
  writeLines(c("#!/bin/bash",
               paste("mkdir -p", outdir),
               paste("cd", rundir),
               ## model binary takes rundir, outdir as arguments
               paste(settings$model$binary, normalizePath(rundir, mustWork=FALSE), normalizePath(outdir, mustWork=FALSE)),
               #             "./convert.R",
               paste("cp ", file.path(rundir, "README.txt"), file.path(outdir, "README.txt"))),
             con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))

  ## write configuraiton file
  traits  <- convert.samples.BIOCRO(trait.values[[settings$pfts$pft$name]])

    
    species <- read.csv(file.path(settings$pfts$pft$outdir, "species.csv"))
    genus <- unique(species$genus)
    if(length(genus) > 1) logger.severe("BioCro can not combine multiple genera")

  
  ### Set defaults
    defaults.file <- defaults$pft$constants$file
    if(!is.null(defaults.file)){
        if(grepl("xml", defaults.file)){
            defaults.xml <- defaults.file      
            defaults <- xmlToList(xmlParse(defaults.xml))
        } else if(grepl("RData", defaults.file)){
            load(defaults.file)
        } else {
            logger.severe("Defaults file",
                          defaults.file, " not found; using package defaults")
            defaults.file <- NULL
        }
    } else if(is.null(defaults.file)){
    defaults.file <- system.file(file.path("extdata/defaults", paste0(tolower(genus), ".xml")), package = "PEcAn.BIOCRO")
  }
  if(file.exists(defaults.file)) {
    defaults <- xmlToList(xmlParse(defaults.file))
  } else {
    logger.severe("no defaults file given and ",
                  genus, "not supported in BioCro")
  }
  
    if(is.null(defaults)) logger.error("No defaults values set")
    
    traits.used <- sapply(defaults, is.null)
    for(parm.type in names(defaults)){
        for(parm in names(defaults[[parm.type]])){
            if(!is.null(traits[[parm]])){
                defaults[[parm.type]][[parm]] <- as.character(traits[[parm]])
                traits.used[[parm]] <- TRUE
            }        
        }
    }
  
    defaults$type$name <- settings$pfts$pft$name
  
  ### Replace Defaults with meta-analysis results
    unused.traits <- !traits.used
    ## a clunky way to only use logger for MEDIAN rather than all runs
    if(any(grepl("MEDIAN",
                 scan(file = file.path(settings$rundir, run.id, "README.txt"), character(0),
                      sep = ":", strip.white = TRUE)))){
        if(sum(unused.traits) > 0){
            logger.warn("the following traits parameters are not added to config file:",
                        vecpaste(names(unused.traits)[unused.traits == TRUE]))
        }
    }
    
    
    ## this is where soil parms can be set
    ## defaults$soilControl$FieldC <- 

  ### Put defaults and other parts of config file together
    parms.xml    <- listToXml(defaults, "pft")
    location.xml <- listToXml(list(latitude = settings$run$site$lat,
                                   longitude = settings$run$site$lon),
                              "location")
    run.xml <- listToXml(list(start.date = settings$run$start.date,
                              end.date = settings$run$end.date,
                              met.file = settings$run$inputs$met$path,
                              soil.file = settings$run$inputs$soil$path),
                         "run")
    
    slashdate <- function(x) substr(gsub("-", "/", x), 1, 10)
    simulationPeriod.xml <- listToXml(
        list(dateofplanting = slashdate(settings$run$start.date),
             dateofharvest = slashdate(settings$run$end.date)),
        "simulationPeriod")
    
    config.xml <- xmlNode("config")
    config.xml <- append.xmlNode(config.xml, run.xml)
    config.xml <- append.xmlNode(config.xml, location.xml)
    config.xml <- append.xmlNode(config.xml, simulationPeriod.xml)
    config.xml <- append.xmlNode(config.xml, parms.xml)
    
    saveXML(config.xml,
            file = file.path(settings$rundir, run.id, "config.xml"),
            indent=TRUE)
}
##==================================================================================================#

##--------------------------------------------------------------------------------------------------#
##' Clear out previous config and parameter files.
##'
##' @name remove.config.BIOCRO
##' @title Clear out previous biocro config and parameter files.
##' @param main.outdir Primary PEcAn output directory (will be depreciated)
##' @param settings PEcAn settings file 
##' @return nothing, removes config files as side effect
##' @export
##' @author Shawn Serbin, David LeBauer
remove.config.BIOCRO <- function(main.outdir, settings) {
    
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
##==================================================================================================#


####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################
