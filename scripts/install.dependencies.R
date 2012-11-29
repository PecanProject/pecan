#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
list.of.packages <- c('car', 'chron', 'coda', 'doSNOW', 'dplR', 'emulator',
                      'ggplot2', 'gridExtra', 'hdf5', 'Hmisc', 'kernlab',
                      'lubridate', 'MASS', 'MCMCpack', 'mvtnorm', 'ncdf',
                      'ncdf4', 'plotrix', 'plyr', 'randtoolbox', 'rjags',
                      'Rmpi', 'RMySQL', 'roxygen2', 'stringr', 'testthat',
                      'XML', 'RNCEP')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  print("installing : ")
  print(new.packages)
  install.packages(new.packages, repos="http://cran.us.r-project.org")
}

