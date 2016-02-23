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
                      'XML', 'RNCEP', 'foreign', 'RCurl', 'udunits2', 'RPostgreSQL',
                      'rPython','minpack.lm','geonames', 'Rcpp','devtools', 'inline', 'segmented')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  print("installing : ")
  print(new.packages)
  install.packages(new.packages, repos="http://cran.rstudio.com/")
}

if(!("REddyProc" %in% installed.packages()[,"Package"])) {
  install.packages("REddyProc", repos="http://R-Forge.R-project.org", type="source")
}
if(!("BioCro" %in% installed.packages()[,"Package"])) {
  devtools::install_github("ebimodeling/biocro")
}

# install rhdf5 from bioconductor for met2model.ED
if(!("rhdf5" %in% installed.packages()[,"Package"])) {
  source("http://bioconductor.org/biocLite.R")
  biocLite('rhdf5')
}

# install graph for MCMCpack for allometry module
if(!("graph" %in% installed.packages()[,"Package"])) {
  source("http://bioconductor.org/biocLite.R")
  biocLite('graph')
}

#install Rgraphviz for MCMCpack for allometry module
if(!("Rgraphviz" %in% installed.packages()[,"Package"])) {
  source("http://bioconductor.org/biocLite.R")
  biocLite('Rgraphviz')
}
