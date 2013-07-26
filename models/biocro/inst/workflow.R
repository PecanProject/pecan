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
#--------------------------------------------------------------------------------------------------#

#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.

settings <- read.settings(system.file("extdata/pecan.biocro.xml",
                                      package = "PEcAn.BIOCRO"))
#--------------------------------------------------------------------------------------------------#
model <- settings$model$name
settings$meta.analysis$update <- TRUE

#---------------- Run PEcAn workflow. -------------------------------------------------------------#
get.trait.data()        	# Query the trait database for data and priors

run.meta.analysis()     	# Run the PEcAn meta.analysis

run.write.configs(model)        # Calls model specific write.configs e.g. write.config.ed.R
## load met data
start.model.runs(model)         # Start ecosystem model runs

get.model.output(model)         # Get results of model runs

#run.sensitivity.analysis()      # Run sensitivity analysis and variance decomposition on model output

#run.ensemble.analysis()		      # Run ensemble analysis on model output. 
                                # OPTIONAL: run.ensemble.analysis(plot.timeseries=TRUE) to get an esemble 
                                # time-series output for the target variables set in the PEcAn.xml file

### PEcAn workflow run complete
print("---------- PEcAn Workflow Complete ----------")
#--------------------------------------------------------------------------------------------------#
