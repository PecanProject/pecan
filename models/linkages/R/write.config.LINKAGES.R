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
##' @author Ann Raiho, Betsy Cowdery
##-------------------------------------------------------------------------------------------------#
write.config.LINKAGES <- function(defaults=NULL, trait.values, settings, run.id,
                                  restart=NULL, spinup=NULL){
  #850-869 repeated to fill 1000 years
  if(is.null(restart)) restart = FALSE
  if(is.null(spinup)) spinup = TRUE
  
  require(linkages) 
  
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
  bgs <- 120
  egs <- 273
  max.ind <- 15000
  plat <- abs(as.numeric(settings$run$site$lat))
  
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
  
  fdat <- read.csv(system.file("fdat.csv", package = "linkages"),header = FALSE) #litter quality parameters
  clat <- read.csv(system.file("clat.csv", package = "linkages"),header = FALSE)
  load(system.file("switch.mat.Rdata", package = "linkages"))
  
  climate_file <- settings$run$inputs$met$path
  load(climate_file)
  temp.mat <- temp.mat[start.year:end.year-start.year+1,]
  precip.mat <- precip.mat[start.year:end.year-start.year+1,]

  basesc = 74
  basesn = 1.64
  
  spp.params.default <- read.csv(system.file("spp_matrix.csv", package = "linkages")) #default spp.params
  nspec <- length(settings$pfts)
  spp.params.save <- numeric(nspec)
  for(i in 1:nspec){
    spp.params.save[i] <- which(spp.params.default[,1]%in%settings$pfts[i]$pft$name)
  }     
  spp.params <- spp.params.default[spp.params.save,]
  
  ### Create species parameter matrix with correct PFTs
  # trait.values$`Hemlock(Tsuga Canadensis)`$
  #group will be each spp. 
#   for(group in names(trait.values)){
#     if(group == "env"){
#       
#       ## leave defaults
#       ##
#       
#     } else {
#       ## copy values
#       if(!is.null(trait.values[[group]])){
#         vals <- trait.values[[group]]
#         
#         #replace defaults with traits
#         new.params.locs <- which(names(spp.params) %in% names(vals))
#         new.vals.locs <- which(names(vals) %in% names(spp.params))
#         spp.params[spp.params$Spp_Name==group,new.params.locs] <- vals[new.vals.locs]
#         
#         #conversion of some traits to match what LINKAGES needs
#         #Going to have to look up this paper Botkin 1972 Some Ecological Consequences of a computer model of forest growth
#         if('HTMAX' %in% names(vals) & 'DBHMAX' %in% names(vals)){
#           spp.params[spp.params$Spp_Name==group,]$B2 <- 2*(((vals$HTMAX*100) - 137) / (vals$DBHMAX*100))
#           spp.params[spp.params$Spp_Name==group,]$B3 <- (vals$HTMAX*100 - 137) / (vals$DBHMAX*100 ^ 2)
#         }
#         
#         if('root2shoot' %in% names(vals)){
#           spp.params[spp.params$Spp_Name==group,]$RTST <- vals$root2shoot
#         }
#         
#         if('leaf_longevity' %in% names(vals)){
#           spp.params[spp.params$Spp_Name==group,]$FRT <- vals$leaf_longevity
#         }
#         
#         if('TL' %in% names(vals)){
#           spp.params[spp.params$Spp_Name==group,]$TL <- ceiling(vals$TL)
#         }
#         
#         
#       }
#     }
#   }

  switch.mat <- switch.mat[spp.params.save,]
  
  if(spinup==TRUE){
    spinup.out <- spinup.LINKAGES(start.year,end.year,temp.mat,precip.mat)
    start.year <- spinup.out$start.year
    end.year <- spinup.out$end.year
    nyear <- spinup.out$nyear
    temp.mat <- spinup.out$temp.mat
    precip.mat <- spinup.out$precip.mat
    settings$run$start.date <- paste0(spinup.out$start.year,strftime(settings$run$start.date,"/%m/%d"))
    precip.mat <- spinup.out$precip.mat
  }

  input <- file.path(settings$rundir,run.id,"linkages.input.Rdata")  
  
  save(iplot, nyear, nspec, fc, dry, bgs, egs, max.ind,
       plat, temp.mat, precip.mat, spp.params, switch.mat,
       fdat, clat, basesc, basesn, start.year, end.year, file = input)
  
  if(restart==TRUE){
  restartfile <- file.path(settings$rundir,run.id,"linkages.restart.Rdata")
  }else{
    restartfile <- NULL
  }
  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$run$jobtemplate) && file.exists(settings$run$jobtemplate)) {
    jobsh <- readLines(con=settings$run$jobtemplate, n=-1)
  } else {
    jobsh <- readLines(con=system.file("template.job", package = "PEcAn.LINKAGES"), n=-1)
  }
  
  # create host specific setttings
  hostspecific <- ""
  if (!is.null(settings$model$job.sh)) {
    hostspecific <- paste(hostspecific, sep="\n", paste(settings$model$job.sh, collapse="\n"))
  }
  if (!is.null(settings$run$host$job.sh)) {
    hostspecific <- paste(hostspecific, sep="\n", paste(settings$run$host$job.sh, collapse="\n"))
  }

  # create job.sh
  jobsh <- gsub('@HOSTSPECIFIC@', hostspecific, jobsh)

  jobsh <- gsub('@SITE_LAT@', settings$run$site$lat, jobsh)
  jobsh <- gsub('@SITE_LON@', settings$run$site$lon, jobsh)
  jobsh <- gsub('@SITE_MET@', settings$run$inputs$met$path, jobsh)
  
  jobsh <- gsub('@START_DATE@', settings$run$start.date, jobsh)
  jobsh <- gsub('@END_DATE@', settings$run$end.date, jobsh)
  
  jobsh <- gsub('@OUTDIR@', outdir, jobsh)
  jobsh <- gsub('@RUNDIR@', rundir, jobsh)
  
  jobsh <- gsub('@INPUT@', input, jobsh)
  jobsh <- gsub('@RESTART@', restart, jobsh)
  if(restart==TRUE){
  jobsh <- gsub('@RESTARTFILE@', restartfile, jobsh)
  }
  
  writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  

}
