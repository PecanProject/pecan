#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
read.plot <- function(file){
  dat <- read.csv(file, na.strings = "") #, sep = "\t")
  site <- dat[,which(toupper(names(dat)) == "SITE")]
  plot <- dat[,which(toupper(names(dat)) == "PLOT")]
  subplot <- dat[,which(toupper(names(dat)) == "SUBPLOT")]
  tree <- as.character(dat[,which(toupper(names(dat)) == "TREEID")])
  tag <- dat[,which(toupper(names(dat)) == "TAG")]
  spp <- as.character(dat[,which(toupper(names(dat)) == "SPP")])
  dbh.cols = which(substr(toupper(names(dat)),1,3) == "DBH")
  census.years = as.numeric(substring(names(dat)[dbh.cols],4))
  census.years[census.years < 30] = census.years[census.years < 30] + 2000
  census.years[census.years < 100] = census.years[census.years < 100] + 1900
  dbh <- dat[,dbh.cols]
  for(i in 1:ncol(dbh)){dbh[,i] = as.numeric(as.character(dbh[,i]))}
  names(dbh) = census.years
  
  cored = grep("CORE",toupper(names(dat)))
  if(length(cored)>0) cored = dat[,cored]

  plot.data<<-list(site=site,plot=plot,subplot=subplot,
                         tree=tree,tag=tag,spp=spp,dbh=dbh,cored=cored)  
}

