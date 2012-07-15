list.of.packages <- c('MCMCpack', 'PECAn', 'RMySQL', 'Rmpi', 'XML', 'chron', 'doSNOW', 'hdf5', 'mvtnorm', 'ncdf', 'stringr', 'time')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  print("installing : ")
  print(new.packages)
  install.packages(new.packages, repos="http://cran.us.r-project.org")
}

