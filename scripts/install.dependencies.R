#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
list.of.packages <- c('MCMCpack', 'PECAn', 'RMySQL', 'Rmpi', 'XML', 'chron', 'doSNOW', 'hdf5', 'mvtnorm', 'ncdf', 'stringr', 'time')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  print("installing : ")
  print(new.packages)
  install.packages(new.packages, repos="http://cran.us.r-project.org")
}

