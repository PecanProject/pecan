#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes a LINKAGES config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.LINKAGES
##' @title Write LINKAGES configuration files
##' @param defaults list of defaults to process
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for LINKAGES for given run
##' @export
##' @author Ann Raiho
##-------------------------------------------------------------------------------------------------#
write.config.LINKAGES <- function(defaults=NULL, trait.values=NULL, settings, run.id){

  # find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, run.id)
  if(!file.exists(rundir)) dir.create(rundir)
  outdir <- file.path(settings$run$host$outdir, run.id)
  if(!file.exists(outdir)) dir.create(outdir)

  #-----------------------------------------------------------------------
  
  start.year = as.numeric(strftime(settings$run$start.date,"%Y"))
  end.year = as.numeric(strftime(settings$run$end.date,"%Y"))
  year = seq(start.year,end.year,1)
  
  iplot <- 1
  nyear <- length(year)
  nspec <- 5
  bgs <- 120
  egs <- 273
  max.ind <- 15000
  plat <- abs(settings$run$site$lat)
  
  texture <- read.csv(system.file("texture.csv",package = "PEcAn.LINKAGES"))
  
  dbcon <- db.open(settings$database$bety)
  soils <- db.query(paste("SELECT soil,som,sand_pct,clay_pct,soilnotes FROM sites WHERE id =", settings$run$site$id), con=dbcon)
  db.close(dbcon)
  
  sand = as.numeric(soils[3])/100
  clay = as.numeric(soils[4])/100
  
  soil.texture <- function(sand,clay){
    silt = 1 - sand - clay
    
    sand.keep = which(texture$xsand < sand + .1 & texture$xsand > sand - .1)
    clay.keep = which(texture$xclay[sand.keep] < clay + .1 & texture$xclay[sand.keep] > clay - .1)
    silt.keep = which(texture$xsilt[sand.keep[clay.keep]] < silt + .1 & texture$xsilt[sand.keep[clay.keep]] > silt - .1)
    
    row.keep = sand.keep[clay.keep[silt.keep]]
    
    return(texture[round(mean(row.keep)),c(8,14)]*100) # might need to divide by 3 or something because linkages wants cm water/30cm soil...
  }
  
  fc = round(as.numeric(unlist(soil.texture(sand = sand, clay = clay)[2])),digits = 2)
  dry = round(as.numeric(unlist(soil.texture(sand = sand, clay = clay)[1])),digits = 2)
  
  fdat <- read.csv(system.file("fdat.csv", package = "PEcAn.LINKAGES")) #litter quality parameters
  spp.params <- read.csv(system.file("spp_matrix.csv", package = "PEcAn.LINKAGES"))
  clat <- read.csv(system.file("clat.csv", package = "PEcAn.LINKAGES"),header = FALSE)
  load(system.file("switch.mat.Rdata", package = "PEcAn.LINKAGES"))
  
  temp.mat <- matrix(c(-8.6,-7.6,-1.9,6.9,13.7,19,21.6,20.5,15.9,9.6,.8,-6.1),nyear,12,byrow = TRUE)
  precip.mat <- matrix(c(2.9,2.7,4.2,7,9.2,11.2,8,8.9,8.9,5.7,5.5,2.9),nyear,12,byrow=TRUE)
  
  #linkages.out <- linkages(iplot = iplot, nyear = nyear,nspec = nspec, fc = fc, dry = dry,
   #                        bgs = bgs, egs = egs, max.ind=max.ind, plat = plat, temp.mat = temp.mat,
    #                       precip.mat = precip.mat, spp.params = spp.params, switch.mat = switch.mat,
     #                      fdat = fdat, clat = clat, basesc = 74, basesn = 1.64)
  
  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$run$jobtemplate) && file.exists(settings$run$jobtemplate)) {
    jobsh <- readLines(con=settings$run$jobtemplate, n=-1)
  } else {
    jobsh <- readLines(con=system.file("template.job", package = "PEcAn.LINKAGES"), n=-1)
  }

  jobsh <- gsub('@SITE_LAT@', settings$run$site$lat, jobsh)
  jobsh <- gsub('@SITE_LON@', settings$run$site$lon, jobsh)
  jobsh <- gsub('@SITE_MET@', settings$run$inputs$met$path, jobsh)

  jobsh <- gsub('@START_DATE@', settings$run$start.date, jobsh)
  jobsh <- gsub('@END_DATE@', settings$run$end.date, jobsh)

  jobsh <- gsub('@OUTDIR@', outdir, jobsh)
  jobsh <- gsub('@RUNDIR@', rundir, jobsh)

  jobsh <- gsub('@BINARY@', settings$model$binary, jobsh)

  writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))

}
