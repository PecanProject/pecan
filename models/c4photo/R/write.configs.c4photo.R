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
##' convert parameters from PEcAn database default units to c4photo defaults
##' 
##' Performs model specific unit conversions on a a list of trait values,
##' such as those provided to write.config
##' @name convert.samples.c4photo
##' @title Convert samples for c4photo
##' @param trait.samples a matrix or dataframe of samples from the trait distribution
##' @return matrix or dataframe with values transformed
##' @author David LeBauer
convert.samples.c4photo <- function(trait.samples){
  trait.names <- names(trait.samples)
  names(trait.samples)[trait.names == "Vcmax"] <- "vmax"
  names(trait.samples)[trait.names == "stomatal_slope.BB"] <- "b1"
  names(trait.samples)[trait.names == "leaf_respiration_rate_m2"] <- "Rd"
  names(trait.samples)[trait.names == "quantum_efficiency"] <- "alpha"  
  return(trait.samples)
}
#==================================================================================================#



##' Writes a configuration files for the c4photo model
##' @name write.config.c4photo
##' @title Write configuration files for the c4photo model
##' @param defaults named list with default model parameter values 
##' @param trait.values 
##' @param settings 
##' @param outdir 
##' @param run.id
##' @export
##' @return nothing, writes configuration file as side effect 
##' @author David LeBauer
write.config.c4photo <- function(defaults = NULL,
                                 trait.values,
                                 settings,
                                 outdir,
                                 run.id){

  trait.values  <- convert.samples.c4photo(trait.values[[trait]])
  trait.names   <- names(trait.values)
  parms.xml <- xmlNode("parms")
  for(trait in trait.names) {
    parms.xml <- append.xmlNode(parms.xml, xmlNode(trait, trait.values[trait]))
  }

  config.xml <- append.xmlNode(xmlNode('traits'), parms.xml)
  run.outdir    <- paste(outdir,"/",run.id,"/",sep="")
  xml.file.name <- paste('c.',run.id,sep='')
  if (! file.exists(my.outdir)) dir.create(my.outdir)
  saveXML(xml, file = paste(outdir, xml.file.name, sep=''), 
          indent=TRUE, prefix = PREFIX_XML)

  ### Display info to the console.
  print(run.id)

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
    system(paste("rsync -outi ", runfile , ' ', settings$run$host$name, ":",
                 settings$run$host$rundir, sep = ''))
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
remove.config.SIPNET <- function(main.outdir, settings) {
  
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
