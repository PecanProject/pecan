#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
# Template for functions to prepare and write out files model-specific configuration files for MA
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Writes a configuration files for your model
#--------------------------------------------------------------------------------------------------#
##' Writes an xml and BIOCRO2IN config files for use with the BIOCRO MODEL.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##' @title Write BIOCRO configuration files
##' @param pft 
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param outdir directory for config files to be written to
##' @param run.id id of run
##' @return configuration file and ED2IN namelist for given run
##' @author David
write.config.BioCro <- function(defaults, trait.values, settings, outdir, run.id){
  #defaults = settings$pfts
  xml <- listToXml(settings$config.header, 'config')
  names(defaults) <- sapply(defaults,function(x) x$name)
  for(group in names(trait.samples)){
    if(group == "env"){
      
      ## set defaults from config.header
      
      ##
      
    } else {
      ##is a PFT
      pft <- defaults[[group]]
      pft.xml <- listToXml(pft$constants, 'pft')
      ## copy values
      if(!is.null(trait.values[[group]])){
        vals <- convert.samples.BioCro(trait.values[[group]])
        names(vals) <- trait.lookup(names(vals))$model.id
        for(trait in names(vals)){
          pft.xml <- append.xmlNode(pft.xml, 
              xmlNode(trait, vals[trait]))
        }
      }
      xml <- append.xmlNode(xml, pft.xml)
    }
  }
  xml.file.name <-paste('c2.',run.id,sep='')  
# There is not problem with long file names in BioCro
  ###################################################
  # if(nchar(xml.file.name) >= 32) 
  #  stop(paste('The file name, "',xml.file.name,
  #          '" is too long and will cause your ED run to crash ',
  #          'if allowed to continue. '))
  ####################################################
  saveXML(xml, file = paste(outdir, xml.file.name, sep=''), 
      indent=TRUE, prefix = PREFIX_XML)
  
  startdate <- as.Date(settings$run$start.date)
  enddate <- as.Date(settings$run$end.date)
  print(run.id)
}


write.run.BioCro <- function(settings){
  run.text <- readLines(con=paste(settings$pecanDir,
                        'bash/run-template.BIOCRO', sep = '/'), n=-1)
  run.text <- gsub('@OUTDIR@', settings$run$host$outdir, run.text)
  run.text <- gsub('@MODELDIR@', settings$run$host$modelfile, run.text)
  runfile <- paste(settings$outdir, 'run', sep='/')
  writeLines(run.text, con = runfile)
  if(settings$run$host$name == 'localhost') {
    system(paste('cp ', runfile, settings$run$host$rundir))
  }else{
    system(paste("rsync -outi ", runfile , ' ', settings$run$host$name, ":",
                 settings$run$host$rundir, sep = ''))
  }
}


####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################
