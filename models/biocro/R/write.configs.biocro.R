#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

PREFIX_XML <- '<?xml version="1.0"?>\n<!DOCTYPE config SYSTEM "ed.dtd">\n'


#--------------------------------------------------------------------------------------------------#
##' convert parameters from PEcAn database default units to biocro defaults
##' 
##' Performs model specific unit conversions on a a list of trait values,
##' such as those provided to write.config
##' @name convert.samples.biocro
##' @title Convert samples for biocro
##' @param trait.samples a matrix or dataframe of samples from the trait distribution
##' @export
##' @return matrix or dataframe with values transformed
##' @author David LeBauer
convert.samples.biocro <- function(trait.samples){

  ## first rename variables
  trait.names <- colnames(trait.samples)
  trait.names[trait.names == "Vcmax"] <- "vmax"
  trait.names[trait.names == "leaf_respiration_rate_m2"] <- "Rd"
  trait.names[trait.names == "cuticular_cond"] <- "b0"
  trait.names[trait.names == "stomatal_slope.BB"] <- "b1"
  colnames(trait.samples) <- trait.names

  ## transform values with different units
  ## cuticular conductance - BETY default is umol; BioCro uses mol
  if("b1" %in% trait.names){
    trait.samples[, trait.names == "b0"] <- trait.samples[, trait.names == "b0"]/1e6
  }

  return(trait.samples)
}
#==================================================================================================#

##' Writes a configuration files for the biocro model
##' @name write.config.biocro
##' @title Write configuration files for the biocro model
##' @param defaults named list with default model parameter values 
##' @param trait.values 
##' @param settings 
##' @param run.id
##' @export
##' @return nothing, writes configuration file as side effect 
##' @author David LeBauer
write.config.biocro <- function(defaults, trait.values, settings, run.id) {
file.path(settings$rundir, run.id, "config.xml")
  trait.values  <- convert.samples.biocro(trait.values[[1]])
  trait.names   <- names(trait.values)
  parms.xml <- xmlNode("parms")
  for(trait in trait.names) {
    parms.xml <- append.xmlNode(parms.xml, xmlNode(trait, trait.values[trait]))
  }
  config.xml <- append.xmlNode(xmlNode('traits'), parms.xml)

  saveXML(config.xml, file=file.path(settings$rundir, run.id, "data.xml"), indent=TRUE, prefix=PREFIX_XML)
}
#==================================================================================================#

#--------------------------------------------------------------------------------------------------#
##' Clear out previous config and parameter files.
##'
##' @name remove.config.biocro
##' @title Clear out previous biocro config and parameter files.
##' @param main.outdir Primary PEcAn output directory (will be depreciated)
##' @param settings PEcAn settings file 
##' @return nothing, removes config files as side effect
##' @export
##' @author Shawn Serbin, David LeBauer
remove.config.biocro <- function(main.outdir, settings) {
  
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
