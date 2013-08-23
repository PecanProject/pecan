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

    # iRhizome
    # iStem
    # ifrRhizome
    # ifrStem
    # growth Repiration factor
    # 
    colnames(trait.samples) <- trait.names

    ## transform values with different units
    ## cuticular conductance - BETY default is umol; BioCro uses mol
    if("b0" %in% trait.names){
        trait.samples[, trait.names == "b0"] <- trait.samples[, trait.names == "b0"]/1e6
    }
    if("SLA" %in% trait.names){
        trait.samples[, trait.names == "SLA"] <- trait.samples[, trait.names == "Sp"]/ 10
    }
    
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

    
    if(settings$run$host$name == "localhost"){
        rundir <- file.path(settings$rundir, as.character(run.id))
        outdir <- file.path(settings$outdir, as.character(run.id))
    } else {
        rundir <- file.path(settings$run$host$rundir, as.character(run.id))
        outdir <- file.path(settings$run$host$outdir, as.character(run.id))
    }
    
    dir.create(rundir, showWarnings = FALSE, recursive = TRUE)
    ##   writeLines(c("#!/usr/bin/Rscript",               
    ##                paste("cp ", file.path(rundir, "README.txt"), 
    ##                      file.path(outdir, "README.txt"))),
    ##              con=file.path(settings$rundir, run.id, "job.sh"))
    ##   Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
    ##   
    ##
    traits  <- convert.samples.BIOCRO(trait.values)

    
    species <- read.csv(file.path(settings$pfts$pft$outdir, "species.csv"))
    genus <- unique(species$genus)
    if(length(genus) > 1) logger.severe("BioCro can not combine multiple genera")
    
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
    }
    if(is.null(defaults.file)){
        defaults.dir <- system.file("extdata/defaults/", package = "PEcAn.BIOCRO")
        if(genus == "Saccharum") {
            defaults.xml <- file.path(defaults.dir, "saccharum.xml")
        } else if (genus == "Salix") {
            defaults.xml <- file.path(defaults.dir, "salix.xml")
        } else if (genus == "Miscanthus") {
            defaults.xml <- file.path(defaults.dir, "miscanthus.xml")
        } else if (genus == "Panicum") {
            defaults.xml <- file.path(defaults.dir, "panicum.xml")
            ##     } else if (genus == "Populus") {
            ##       defaults.xml <- file.path(defaults.dir, "populus.xml")
        } else {
            logger.severe("no defaults file given and ",
                          genus, "not supported in BioCro")
        }
        defaults <- xmlToList(xmlParse(defaults.xml))
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
    
    unused.traits <- !traits.used
    if(sum(unused.traits) > 0){
        logger.warn("the following traits parameters are not added to config file:",
                    vecpaste(names(traits[unused.traits,])))
    }
    
    defaults$genus <- genus
    
    ## this is where soil parms can be set
    ## defaults$soilControl$FieldC <- 
    
    parms.xml    <- listToXml(defaults, "pft")
    location.xml <- listToXml(list(latitude = settings$run$site$lat,
                                   longitude = settings$run$site$lon),
                              "location")
    
    
    slashdate <- function(x) substr(gsub("-", "/", x), 1, 10)
    simulationPeriod.xml <- listToXml(
        list(dateofplanting = slashdate(settings$run$start.date),
             dateofharvest = slashdate(settings$run$end.date)),
        "simulationPeriod")
    
    config.xml <- xmlNode("config")
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
