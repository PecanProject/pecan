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

  rundir <- settings$run$host$rundir
  outdir <- settings$run$host$outdir

  # run model
  # compute/download weather
  lat <- as.numeric(settings$run$site$lat)
  lon <- as.numeric(settings$run$site$lon)
  start.date <- settings$run$start.date
  end.date <- settings$run$end.date
  site_id = settings$run$site$id
  
  
  ### TODO the following code should be run during write_configs and the file name passed to the start.runs function
  metfiles <- query.base(paste("select start_date, end_date, file_name, file_path ",
                   "from inputs join dbfiles on dbfiles.file_id = inputs.file_id ",
                   "where start_date <= '", start.date, 
                   "' and end_date >= '", end.date, 
                   "' and site_id =", site_id, ";", sep = ""))

  if(nrow(metfiles == 1)){
    weather <- read.csv(file.path(metfiles$file_path, metfiles$file_name), row.names = NULL)
  } else {
    weather <- InputForWeach(lat, lon, year(start.date), year(end.date))
  }

  weather2 <- weachNEW(weather, lati = lat, ts = 1, 
                       temp.units="Celsius", rh.units="fraction", 
                       ws.units="mph", pp.units="in")

  # run model
  config <- xmlToList(xmlParse(file.path(settings$outdir, runid, "data.xml")))
  pp <- do.call(photoParms, list(unlist(config$parms)))
  
  BioGro_result <- BioGro(weather2, photoControl=pp)

  # save results
  write.csv(weather, file = file.path(outdir, "weather.csv"))
  write.csv(with(BioGro_result, data.frame(DayofYear, Hour, ThermalT, Stem, Leaf, Root)), 
            file=file.path(outdir, "result.csv"))
  file.copy(file.path(rundir, "README.txt"), file.path(outdir, "README.txt"))
  setwd(cwd)
}

#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
