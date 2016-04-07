#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


#---------------- Load libraries. -----------------------------------------------------------------#
require(PEcAn.all)
library(RCurl)
#--------------------------------------------------------------------------------------------------#

#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings(system.file("pecan.maat.xml",package = "PEcAn.MAAT"))
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# get traits of pfts
settings$pfts <- get.trait.data(settings$pfts, settings$model$type, settings$run$dbfiles, 
	settings$database$bety, settings$meta.analysis$update)
saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# run meta-analysis
run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$meta.analysis$random.effects, 
                  settings$meta.analysis$threshold, settings$run$dbfiles, settings$database$bety)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# write configurations
if (!file.exists(file.path(settings$rundir, "runs.txt")) | settings$meta.analysis$update == "TRUE") {
  run.write.configs(settings, settings$database$bety$write)
} else {
  logger.info("Already wrote configuraiton files")    
}
#--------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------#
# run model
if (!file.exists(file.path(settings$rundir, "runs.txt"))) {
  logger.severe("No ensemble or sensitivity analysis specified in pecan.xml, work is done.")
} else {
  start.model.runs(settings, settings$database$bety$write)
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# get results
get.results(settings)
#--------------------------------------------------------------------------------------------------#