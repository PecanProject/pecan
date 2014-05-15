#!/usr/bin/env Rscript
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
list.of.packages <- c('abind', 'car', 'chron', 'coda', 'data.table', 'doSNOW', 'dplR', 'earth', 'emulator',
                      'ggmap', 'ggplot2', 'gridExtra', 'Hmisc', 'kernlab',
                      'knitr', 'lubridate', 'MASS', 'MCMCpack', 'mvtnorm', 'ncdf4',
                      'plotrix', 'plyr', 'raster', 'randtoolbox', 'rjags',
                      'rgdal', 'tgp', 'DBI', 'roxygen2', 'stringr', 'testthat',
                      'XML', 'RNCEP', 'foreign', 'RCurl', 'udunits2')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  print("installing : ")
  print(new.packages)
  install.packages(new.packages, repos="http://cran.rstudio.com/")
}


# Check for R database drivers.
if (!any(c('RMySQL', 'RPostgreSQL') %in% installed.packages()[,"Package"])) {
  print("No database drivers installed, please select appropriate driver")
  print("RMySQL      : for MySQL database")
  print("RPostgreSQL : for PostgreSQL database")
  print("For PEcAn, PostgreSQL is recommended")
  print("You can install the driver using the following command in R.")
  print("install.packages('DRIVER', repos='http://cran.rstudio.com/')")
}
