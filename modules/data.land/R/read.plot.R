#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
read.plot <- function(file){
  dat <- read.csv(file)
  plot <- dat[,which(toupper(names(dat)) == "PLOT")]
  tree <- dat[,which(toupper(names(dat)) == "TREE")]
  spp <- as.character(dat[,which(toupper(names(dat)) == "SPECIES")])
  dbh <- dat[,which(toupper(names(dat)) == "DBH")]

  data.frame(plot=plot,tree=tree,spp=spp,dbh=dbh)
  
}
