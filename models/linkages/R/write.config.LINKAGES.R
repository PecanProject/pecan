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
  #Things LINKAGES needs to run
  #iplot = iplot, nyear = nyear,nspec = nspec, fc = fc, dry = dry,
  #bgs = bgs, egs = egs, max.ind=max.ind, plat = plat, temp.mat = temp.mat,
  #precip.mat = precip.mat, spp.params = spp.params, switch.mat = switch.mat,
  #fdat = fdat, clat = clat, basesc = 74, basesn = 1.64

  # write LINKAGES settings file
  start.year = as.numeric(strftime(settings$run$start.date,"%Y"))
  end.year = as.numeric(strftime(settings$run$end.date,"%Y"))
  year = seq(start.year,end.year,1)

  kprnt = 2 #year interval for output
  klast = 90 #number of plots
  nyear = length(year) #number of years to simulate
  ipolat_nums = seq(2,nyear,25) #years for climate interpolation #need to make break points generalizable someday
  ipolat = length(ipolat_nums)-1 #number of years for climate interpolation
  plat = abs(settings$run$site$lat) #latitude
  plong = abs(settings$run$site$lon) #longitude
  bgs = 127 #DOY to begin growing season
  egs = 275 #DOY to end growing season

 texture =  read.csv(system.file("texture.csv",package = "PEcAn.LINKAGES"))

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


  sink(file.path(rundir,"settings.txt"))
  cat(kprnt,klast,nyear,sep=",")
  cat("\n")
  cat(ipolat)
  cat("\n")
  cat(ipolat_nums,sep=",")
  cat("\n")
  cat(plong,plat,bgs,egs,fc,dry,sep=",")
  sink()
  unlink("settings")

  ## as initial hack, copy parameter file from inst to rundir
  ##param.file=system.file("SPP.DAT", package = "PEcAn.LINKAGES")
  ##file.copy(from = param.file,rundir)
  ## Parameters from specific spp. for Acer,betula,carya,castanea dentata,
  ##fagus grandifolia,picea,pinus,tsuga canadensis,quercus (in that order)

 #####
 ##### Write species data table #####
 #####

 nspec = 9
 bmspec = nspec
 all_spp_params = read.csv(system.file("spp_matrix.csv",package = "PEcAn.LINKAGES"))
 pick_spp = c(38,72,58,8,2,1,6,7,11)
 spp_params = all_spp_params[pick_spp,3:ncol(all_spp_params)]
 spec_nums = all_spp_params[pick_spp,2]
 spp_params[is.na(spp_params)] <- 0
 spp_params <- as.data.frame(spp_params)

  sink(file.path(rundir,"spp.txt"))
  cat(nspec,bmspec,sep=",")
  cat("\n")
  cat(spec_nums)
  cat("\n")
  write.table(spp_params,sep=",",col.names=FALSE,row.names=FALSE)
  sink()


 #####
 ##### Write switch text file #####
 #####

 switch_chars_list = read.csv(system.file("switch.csv",package = "PEcAn.LINKAGES"))
 switch_chars = as.character(switch_chars_list[spec_nums,3])
 sink(file.path(rundir,"/switch.txt"))
 cat(switch_chars,sep="\n")
 sink()

 #####
 ##### Write underground parameters file #####
 #####

  NLVAR = 10
  NLT = 17
  NCOHRT = 1

  init_litter_wt = c(rep(0,17)) #The weight of an incoming cohort of litter (initialized to zero)
  init_perc_N = c(.0068,.0076,.0109,.0106,.0079,.0081,.0085,.0057,
                  .0090,.0056,.0063,.0046,.0096,.0038,.0038,.0038,.0050) #Initial percent of nitrogen
  g_N_per_g_wt_loss = c(.0251,.0315,.0574,.0377,.0256,.0286,.0336,
                        .0477,.0341,.0326,.0220,.0163,.0284,.0195,
                        .0195,.0195,.0364) #Grams of nitrogen immobilized per gram weight loss;
  crit_perc_N = c(.0183,.0239,.0465,.0271,.0177,.0205,.0251,
                  .0420,.0251,.0270,.0157,.0117,.0188,.0157,
                  .0157,.0157,.0314) #Critical percent of nitrogen
  litter_type = seq(1,17,1) #Litter type: 1 through 12 are the 12 leaf-litter types in order of decreasing decay rate and increasing nitrogen-immobilization rate and correspond to species parameter TL. Thirteen is root litter. Fourteen and fifteen are fresh wood from trees less than or greater than 10 cm dbh, respectively. Sixteen is twig litter. Seventeen is well-decayed wood not yet humus;
  dest = c(1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1) #Destination when cohort reaches critical percent to nitrogen (1 = humus; 2 = well-decayed wood);
  init_perc_lignin = c(.039,.121,.193,.158,.187,.206,.214,
                       .241,.248,.280,.216,.283,.253,.173,
                       .173,.173,.423) #Initial percent of lignin;
  lignin_decay_param1 = c(.5217,.5219,.7787,.6693,.5194,.6839,.7059,
                          1.1967,.6105,.5926,.9052,.5646,.7000,.4831,
                          .4831,.4831,.7222) #Lignin decay parameters [see Eq. B-8, Appendix 2, in Pastor and Post (1985)]
  lignin_decay_param2 = c(.336,.400,.508,.435,.315,.475,.460,.790,.359,
                          .383,.594,.327,.456,.299,.299,.299,.299)
  ash_corr_factor = c(.90,.90,.92,.92,.93,.96,.94,.91,.95,.97,.97,.96,.98,.99,.99,.96,.99)

  dirt_params = cbind(init_litter_wt,init_perc_N,g_N_per_g_wt_loss,crit_perc_N,
                      litter_type,dest,init_perc_lignin,lignin_decay_param1,lignin_decay_param2,
                      ash_corr_factor)
  basesc = 74. #starting humus weight
  basesn = 1.640 #starting N content

  sink(file.path(rundir,"dirt.txt"))
  cat(NLVAR,NLT,NCOHRT,sep=" ")
  cat("\n")
  write.table(dirt_params,sep=",",col.names=FALSE,row.names=FALSE)
  cat("\n")
  cat(basesc,basesn,sep=" ")
  sink()

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
