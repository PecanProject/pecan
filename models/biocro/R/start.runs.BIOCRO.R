#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------#
##' 
##' Start biocro model runs on local
##' @title Start run of biocro model
##' @param runid the id of the run (folder in runs) to execute
##' @export
##' @author Rob Kooper, David LeBauer, Deepak Jaiswal
start.runs.BIOCRO <- function(runid) {
  if (settings$run$host$name != "localhost") {
    stop("Only local runs are executed here")
  }

  rundir <- file.path(settings$run$host$rundir, as.character(runid))
  outdir <- file.path(settings$run$host$outdir, as.character(runid))

  cwd <- getwd()
  setwd(rundir)

  # run model
  require(EnCro)
  require(XML)

  # compute/download weather
  lat <- as.numeric(settings$run$site$lat)
  lon <- as.numeric(settings$run$site$lon)
  start <- ymd_hms(settings$run$start.date)
  end <- ymd_hms(settings$run$end.date)
  weather <- InputForWeach(lat, lon, year(start), year(end))

  # run model
  config <- xmlToList(xmlParse("data.xml"))
  pp<-do.call(photoParms,list(unlist(config$parms)))
  result<-BioGro(weather, photoControl=pp)

  # save results
  save(result, file=file.path(outdir, "result.Rdata"))
  file.copy(file.path(rundir, "README.txt"), file.path(outdir, "README.txt"))
}

#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
