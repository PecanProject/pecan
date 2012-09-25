#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html

rm(list = ls())
.libPaths(c("/home/dlebauer/lib/R",
            "/usr/local/R-2.15/lib64/R/library")) ## this last one should be removed
require("PEcAn.common")
require("PEcAn.DB")
require("PEcAn.ED")
require("PEcAn.BioCro")
require("PEcAn.MA")
require("PEcAn.uncertainty")
require("PEcAn.utils")
remove.config.BioCro <- PEcAn.c4photo::remove.config.c4photo
remove.config.BioCro <- PEcAn.c4photo::remove.config.c4photo

#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings("pecan.xml")
#--------------------------------------------------------------------------------------------------#

model <- "BioCro"

#---------------- Run PEcAn workflow. -------------------------------------------------------------#
get.trait.data()        	# Query the trait database for data and priors

run.meta.analysis()     	# Run the PEcAn meta.analysis

run.write.configs(model)        # Calls model specific write.configs e.g. write.config.ed.R

clear.scratch(settings)         # Clear any old model output in ebi-cluster scratch/$USER on worker nodes

start.model.runs(model)         # Start ecosystem model runs

get.model.output(model)         # Get results of model runs

run.sensitivity.analysis()      # Run sensitivity analysis and variance decomposition on model output

run.ensemble.analysis()		      # Run ensemble analysis on model output. 
                                # OPTIONAL: run.ensemble.analysis(plot.timeseries=TRUE) to get an esemble 
                                # time-series output for the target variables set in the PEcAn.xml file

### PEcAn workflow run complete
print("---------- PEcAn Workflow Complete ----------")
#--------------------------------------------------------------------------------------------------#
