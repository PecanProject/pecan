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
  # write LINKAGES settings file
  start.year = as.numeric(strftime(settings$run$start.date,"%Y"))
  end.year = as.numeric(strftime(settings$run$end.date,"%Y"))
  year = seq(start.year,end.year,1)
  
  kprnt = 50 #year interval for output
  klast = 90 #number of plots
  nyear = length(year) #number of years to simulate
  ipolat_nums = seq(2,nyear,25) #years for climate interpolation #need to make break points generalizable someday
  ipolat = length(ipolat_nums)-1 #number of years for climate interpolation
  plat = settings$run$site$lat #latitude
  plong = settings$run$site$lon #longitude
  bgs = 127 #DOY to begin growing season
  egs = 275 #DOY to end growing season
  
 texture =  read.csv("/Users/paleolab/pecan/models/LINKAGES/inst/texture.csv")
 #soil.dat = read.csv("/Users/paleolab/Linkages/phase1a_env_drivers_v4/PalEON_Phase1a_sites.csv")

 soil.texture <- function(sand,clay){
    silt = 1 - sand - clay
    
    sand.keep = which(texture$xsand < sand + .1 & texture$xsand > sand - .1) 
    clay.keep = which(texture$xclay[sand.keep] < clay + .1 & texture$xclay[sand.keep] > clay - .1)
    silt.keep = which(texture$xsilt[sand.keep[clay.keep]] < silt + .1 & texture$xsilt[sand.keep[clay.keep]] > silt - .1)
  
    row.keep = sand.keep[clay.keep[silt.keep]]

    return(texture[round(mean(row.keep)),7:8]*100) # might need to divide by 3 or something because linkages wants cm water/30cm soil...
  }

  fc = soil.texture(sand = as.numeric(settings$run$soil$sand)/100, clay = as.numeric(settings$run$soil$clay)/100)[1] #settings$run$site$%sand -> add later when check.settings also gives soil info.
  wp = soil.texture(sand = as.numeric(settings$run$soil$sand)/100, clay = as.numeric(settings$run$soil$clay)/100)[2] #settings$run$site$%clay
  
  
  sink(file.path(rundir,"settings.txt"))
  cat(kprnt,klast,nyear,sep=",")
  cat("\n")
  cat(ipolat)
  cat("\n")
  cat(ipolat_nums,sep=",")
  cat("\n")
  cat(plat,plong,bgs,egs,fc,dry,sep=",")
  sink()
  unlink("settings")

  ## as initial hack, copy parameter file from inst to rundir
  ##param.file=system.file("SPP.DAT", package = "PEcAn.LINKAGES")
  ##file.copy(from = param.file,rundir)
  ## Parameters from specific spp. for Acer,betula,carya,castanea dentata,
  ##fagus grandifolia,picea,pinus,tsuga canadensis,quercus (in that order)

  
  nspec = 9 
  bmspec = 9
  spec_nums = seq(1,bmspec)
  DMAX = c(4700,2500,5993,4571,5537,1911,3165,3800,4571) #- MAXIMUM GROWING DEGREE DAYS
  DMIN = c(1600,1100,1910,1910,1326,280,1100,1324,1100) #- MINIMUM GROWING DEGREE DAYS
  B3 = c(.1988,.5013,.2663,.1495,.2863,.7105,.1495,.1495,.2863) # - INDIVIDUAL SPECIES CONSTANT USED IN GROW
  B2 = c(47.72,76.40,53.26,44.84,57.26,90.96,44.84,44.84,57.26) # - INDIVIDUAL SPECIES CONSTANT USED IN GROW
  ITOL = c(2,1,1,1,1,1,2,1,1) # - LIGHT TOLERANCE CLASS
  AGEMX = c(125,250,300,300,366,200,450,650,400) # - MAXIMUM AGE OF SPECIES
  G = c(212,106,82,102,72,132,68,47,66) # - SCALES THE GROWTH RATE OF EACH SPECIES
  SPRTND = c(31,43,631,63,172,3,37,0,32) #- TENDENCY TO STUMP SPROUT
  SPRTMN = c(6,12,12,12,6,0,0,0,12) # - MINIMUM SIZE TREE THAT WILL SPROUT
  SPRTMX = c(50,100,200,200,30,0,0,0,40) # - MAXIMUM SIZE TREE THAT WILL SPROUT
  MPLANT = c(20,120,20,20,40,16,140,8,40) # - MAXIMUM NUMBER OF SEEDLINGS TO PLANT
  D3 = c(.268,.2,.3,.3,.2,.309,.31,.18,.225) # - PROPORTION OF GROWING SEASON SPECIES CAN WITHSTAND DROUGHT
  FROST = c(-12,-18,-4,-2,-12,-30,-20,-12,-12) # - MINIMUM JANUARY TEMPERATURE SPECIES CAN WITHSTAND
  TL = c(2,4,4,2,8,11,12,6,9) # - LEAF LITTER TYPE
  CM1 = c(2.79,2.94,2.94,2.99,2.94,2.79,2.79,2.79,2.94) #THROUGH CM5 - PARAMETERS TO CALCULATE NITROGEN GROWTH FACTORS
  CM2 = c(219.77,117.52,117.52,207.43,117.52,219.77,291.77,219.77,117.52)
  CM3 = c(.00179,.00234,.00234,.00175,.00234,.00179,.00179,.00179,.00234)
  CM4 = c(-0.6,-1.2,-1.2,-5.0,-1.2,-0.6,-0.6,-0.6,-1.2)
  CM5 = c(1.0,1.3,1.3,2.9,1.3,1.0,1.0,1.0,1.3)
  FWT = c(440,248,248,440,440,440,440,440,440) #- LEAF WEIGHT/UNIT CROWN AREA
  SLTA = c(.814,.804,.804,.814,.904,.804,.804,.804,.904) #- PARAMETERS TO CALCULATE CROWN AREA
  SLTB = c(.078,.069,.069,.078,.095,.069,.069,.069,.095)
  RTST = c(1.0,.8,.8,1,1,1,1,1,1) # - ROOT/SHOOT RATIO
  FRT = c(1,1,1,1,1,3,2,3,1) #- FOLIAGE RETENTION TIME IN YEARS
  
  spp_params = cbind(DMAX,DMIN,B3,B2,ITOL,AGEMX,G,SPRTND,SPRTMN,SPRTMX,MPLANT,D3,FROST,TL,CM1,CM2,CM3,CM4,CM5,FWT,SLTA,SLTB,RTST,FRT)
  
  sink(file.path(rundir,"spp.txt"))
  cat(nspec,bmspec,sep=",")
  cat("\n")
  cat(spec_nums)
  cat("\n")
  write.table(spp_params,sep=",",col.names=FALSE,row.names=FALSE)
  sink()
  
  
  switch_chars = c("FFTFF","FFTTF","TFTFF","TFFFF","FFTFF","FFFFF","TTTFF","FFTTF","TFFFF")
  sink(file.path(rundir,"switch.txt"))
  cat(switch_chars,sep="\n")
  sink()
  
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
