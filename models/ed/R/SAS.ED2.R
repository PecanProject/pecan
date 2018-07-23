##' @name SAS.ED2
##' @title Use semi-analytical solution to accellerate model spinup
##' @author Christine Rollinson, modified from original by Jaclyn Hatala-Matthes (2/18/14)
##'         2014 Feb: Original ED SAS solution Script at PalEON modeling HIPS sites (Matthes)
##'         2015 Aug: Modifications for greater site flexibility & updated ED
##'         2016 Jan: Adaptation for regional-scale runs (single-cells run independently, but executed in batches)
##'         2018 Jul: Conversion to function, Christine Rollinson July 2018
##'@description This functions approximates landscape equilibrium steady state for vegetation and 
##'             soil pools using the successional trajectory of a single patch modeled with disturbance
##'             off and the prescribed disturbance rates for runs (Xia et al. 2012 GMD 5:1259-1271). 
##' @param dir.analy Location of ED2 analyis files; expects monthly and yearly output
##' @param dir.histo Location of ED2 history files (for vars not in analy); expects monthly
##' @param outdir Location to write SAS .css & .pss files
##' @param block Number of years between patch ages
##' @param lat site latitude; used for file naming
##' @param lon site longitude; used for file naming
##' @param yrs.met Number of years cycled in model spinup part 1
##' @param treefall Value to be used for TREEFALL_DISTURBANCE_RATE in ED2IN for full runs (disturbance on)
##' @param sm_fire Value to be used for SM_FIRE if INCLUDE_FIRE=2; defaults to 0 (fire off)
##' @param fire_intensity Value to be used for FIRE_PARAMTER; defaults to 0 (fire off)
##' @param slxsand Soil percent sand; used to calculate expected fire return interval
##' @param slxclay Soil percent clay; used to calculate expected fire return interval
##' @param sufx ED2 out file suffix; used in constructing file names(default "g01.h5) 
##' @param decomp_scheme Decomposition scheme specified in ED2IN
##' @param kh_active_depth
##' @param Lc Used to compute nitrogen immpobilzation factor; ED default is 0.049787 (soil_respiration.f90)
##' @param c2n_slow Carbon to Nitrogen ratio, slow pool; ED Default 10.0
##' @param c2n_structural Carbon to Nitrogen ratio, structural pool. ED default 150.0
##' @param r_stsc Decomp param
##' @param rh_decay_low Param used for ED-1/CENTURY decomp schemes; ED default = 0.24
##' @param rh_decay_high Param used for ED-1/CENTURY decomp schemes; ED default = 0.60
##' @param rh_low_temp Param used for ED-1/CENTURY decomp schemes; ED default = 291
##' @param rh_high_temp Param used for ED-1/CENTURY decomp schemes; ED default = 318.15
##' @param rh_decay_dry Param used for ED-1/CENTURY decomp schemes; ED default = 12.0
##' @param rh_decay_wet Param used for ED-1/CENTURY decomp schemes; ED default = 36.0
##' @param rh_dry_smoist Param used for ED-1/CENTURY decomp schemes; ED default = 0.48
##' @param rh_wet_smoist Param used for ED-1/CENTURY decomp schemes; ED default = 0.98
##' @param resp_opt_water Param used for decomp schemes 0 & 3, ED default = 0.8938
##' @param resp_water_below_opt Param used for decomp schemes 0 & 3, ED default = 5.0786
##' @param resp_water_above_opt Param used for decomp schemes 0 & 3, ED default = 4.5139
##' @param resp_temperature_increase Param used for decomp schemes 0 & 3, ED default = 0.0757
##' @param rh_lloyd_1 Param used for decomp schemes 1 & 4 (Lloyd & Taylor 1994); ED default = 308.56
##' @param rh_lloyd_2 Param used for decomp schemes 1 & 4 (Lloyd & Taylor 1994); ED default = 1/56.02
##' @param rh_lloyd_3 Param used for decomp schemes 1 & 4 (Lloyd & Taylor 1994); ED default = 227.15
##' @export
##'
SAS.ED2 <- function(dir.analy, dir.histo, outdir, prefix, lat, lon, block, yrs.met=30, 
                    treefall, sm_fire=0, fire_intensity=0, slxsand=0.33, slxclay=0.33,
                    sufx="g01.h5",
                    decomp_scheme=2,
                    kh_active_depth = -0.20,
                    decay_rate_fsc=11, decay_rate_stsc=4.5, decay_rate_ssc=0.2,
                    Lc=0.049787, c2n_slow=10.0, c2n_structural=150.0, r_stsc=0.3, # Constants from ED
                    rh_decay_low=0.24, rh_decay_high=0.60, 
                    rh_low_temp=18.0+273.15, rh_high_temp=45.0+273.15,
                    rh_decay_dry=12.0, rh_decay_wet=36.0,
                    rh_dry_smoist=0.48, rh_wet_smoist=0.98,
                    resp_opt_water=0.8938, resp_water_below_opt=5.0786, resp_water_above_opt=4.5139,
                    resp_temperature_increase=0.0757,
                    rh_lloyd_1=308.56, rh_lloyd_2=1/56.02, rh_lloyd_3=227.15
                    ) {
  if(!decomp_scheme %in% 0:4) stop("Invalid decomp_scheme")
  # create a directory for the initialization files
  dir.create(outdir, recursive=T, showWarnings=F)
  
  #---------------------------------------
  # Setting up some specifics that vary by site (like soil depth)
  #---------------------------------------
  #Set directories
  # dat.dir    <- dir.analy
  ann.files  <- dir(dir.analy, "-Y-") #yearly files only  
  
  #Get time window
  # Note: Need to make this more flexible to get the thing after "Y"
  yrind <- which(strsplit(ann.files,"-")[[1]] == "Y")
  yeara  <- as.numeric(strsplit(ann.files,"-")[[1]][yrind+1]) #first year
  yearz  <- as.numeric(strsplit(ann.files,"-")[[length(ann.files)]][yrind+1]) #last full year
  yrs    <- seq(yeara+1, yearz, by=blckyr) # The years we're going to use as time steps for the demography
  nsteps <- length(yrs) # The number of blocks = the number steps we'll have
  
  # Need to get the layers being used for calculating temp & moist
  # Note: In ED there's a pain in the butt way of doing this with the energy, but we're going to approximate
  # slz  <- c(-5.50, -4.50, -2.17, -1.50, -1.10, -0.80, -0.60, -0.45, -0.30, -0.20, -0.12, -0.06)
  # dslz <- c(1.00,   2.33,  0.67,  0.40,  0.30,  0.20,  0.15,  0.15,  0.10,  0.08,  0.06,  0.06)
  nc.temp <- ncdf4::nc_open(file.path(dir.analy, ann.files[1]))
  slz <- ncdf4::ncvar_get(nc.temp, "SLZ")
  ncdf4::nc_close(nc.temp)
  
  dslz <- vector(length=length(slz))
  dslz[length(dslz)] <- 0-slz[length(dslz)]
  
  for(i in 1:(length(dslz)-1)){
    dslz[i] <- slz[i+1] - slz[i]    
  }
  
  nsoil=which(slz >= kh_active_depth-1e-3)  # Maximum depth for avg. temperature and moisture; adding a fudge factor bc it's being weird
  # nsoil=length(slz)
  #---------------------------------------
  
  #---------------------------------------
  # First loop over analy files (faster than histo) to aggregate initial 
  # 	.css and .pss files for each site
  #---------------------------------------
  #create an emtpy storage for the patch info
  pss.big <- matrix(nrow=length(yrs),ncol=13) # save every X yrs according to chunks specified above
  colnames(pss.big) <- c("time","patch","trk","age","area","water","fsc","stsc","stsl",
                         "ssc","psc","msn","fsn")

  #---------------------------------------
  # Finding the mean soil temp & moisture
  # NOTE:  I've been plyaing around with finding the best temp & soil moisture to initialize things
  #        with; if using the means from the spin met cycle work best, insert them here
  # This will also be necessary for helping update disturbance parameter
  #---------------------------------------
  slmsts <- calc.slmsts(slxsand, slxclay)
  slpots <- calc.slpots(slxsand, slxclay)
  slbs   <- calc.slbs(slxsand, slxclay)
  soilcp <- calc.soilcp(slmsts, slpots, slbs)
  
  # Calculating Soil fire characteristics
  soilfr=0
  if(abs(sm_fire)>0){
    if(sm_fire>0){
      soilfr <- smfire.pos(slmsts, soilcp, smfire=sm_fire)
    } else {
      soilfr <- smfire.neg(slmsts, slpots, smfire=sm_fire, slbs)
    }
  }

  
  month.begin = 1
  month.end = 12
  
  tempk.air <- tempk.soil <- moist.soil <- moist.soil.mx <- moist.soil.mn <- nfire <- vector()
  for(y in yrs){
    air.temp.tmp <- soil.temp.tmp <- soil.moist.tmp <- soil.mmax.tmp <- soil.mmin.tmp <- vector()
    ind <- which(yrs == y)
    for(m in month.begin:month.end){
      #Make the file name. 
      year.now  <-sprintf("%4.4i",y)
      month.now <- sprintf("%2.2i",m)
      day.now   <- sprintf("%2.2i",0)
      hour.now  <- sprintf("%6.6i",0)
      
      file.now    <- paste(prefix,"-E-",year.now,"-",month.now,"-",day.now,"-"
                           ,hour.now,"-",sufx,sep="")
      
      # cat(" - Reading file :",file.now,"...","\n")
      now <- ncdf4::nc_open(file.path(dir.analy,file.now))
      
      air.temp.tmp  [m] <- ncdf4::ncvar_get(now, "MMEAN_ATM_TEMP_PY")
      soil.temp.tmp [m] <- sum(ncdf4::ncvar_get(now, "MMEAN_SOIL_TEMP_PY")[nsoil]*dslz[nsoil]/sum(dslz[nsoil]))
      soil.moist.tmp[m] <- sum(ncdf4::ncvar_get(now, "MMEAN_SOIL_WATER_PY")[nsoil]*dslz[nsoil]/sum(dslz[nsoil]))
      soil.mmax.tmp [m] <- max(ncdf4::ncvar_get(now, "MMEAN_SOIL_WATER_PY"))
      soil.mmin.tmp [m] <- min(ncdf4::ncvar_get(now, "MMEAN_SOIL_WATER_PY"))
      
      ncdf4::nc_close(now)
    } # End month loop
    # Finding yearly means
    tempk.air    [ind] <- mean(air.temp.tmp)
    tempk.soil   [ind] <- mean(soil.temp.tmp) 
    moist.soil   [ind] <- mean(soil.moist.tmp)
    moist.soil.mx[ind] <- max(soil.mmax.tmp)
    moist.soil.mn[ind] <- min(soil.mmin.tmp)
    nfire        [ind] <- length(which(soil.moist.tmp<soilfr)) # Number of time fire should get triggered
  }

  soil_tempk     <- mean(tempk.soil)
  # rel_soil_moist <- mean(moist.soil)+.2
  rel_soil_moist <- mean(moist.soil/slmsts) # Relativizing by max moisture capacity
  pfire = sum(nfire)/(length(nfire)*12)
  fire_return = ifelse(max(nfire)>0, length(nfire)/length(which(nfire>0)), 0)
  
  cat(paste0("mean soil temp  : ", round(soil_tempk, 2), "\n"))
  cat(paste0("mean soil moist : ", round(rel_soil_moist, 3), "\n"))
  cat(paste0("fire return interval (yrs) : ", fire_return), "\n")
  #---------------------------------------
  
  #---------------------------------------  
  # Calculate area distribution based on geometric decay based loosely on your disturbance rates
  # Note: This one varies from Jackie's original in that it lets your oldest, undisturbed bin 
  #       start a bit larger (everything leftover) to let it get cycled in naturally
  #---------------------------------------
  # ------
  # Calculate the Rate of fire & total disturbance
  # ------
  fire_rate <- pfire * fire_intensity
  
  # Total disturbance rate = treefall + fire
  #  -- treefall = % area/yr
  disturb <- treefall + fire_rate 
  # ------
  
  stand.age <- seq(yrs[1]-yeara,nrow(pss.big)*blckyr,by=blckyr)
  area.dist <- vector(length=nrow(pss.big))
  area.dist[1] <- sum(dgeom(0:(stand.age[2]-1), disturb))
  for(i in 2:(length(area.dist)-1)){
    area.dist[i] <- sum(dgeom((stand.age[i]):(stand.age[i+1]-1),disturb))
  }
  area.dist[length(area.dist)] <- 1 - sum(area.dist[1:(length(area.dist)-1)])
  pss.big[,"area"] <- area.dist
  #---------------------------------------  
  
  
  #---------------------------------------  
  # Extraction Loop Part 1: Cohorts!!
  # This loop does the following:
  #  -- Extract cohort info from each age slice from *annual* *analy* files (these are annual means)
  #  -- Write cohort info to the .css file as a new patch for each age slice
  #  -- Dummy extractions of patch-level variables; all of the important variables here are place holders
  #---------------------------------------  
  cat(" - Reading analy files ...","\n")
  for (y in yrs){
    now <- ncdf4::nc_open(file.path(dir.analy,ann.files[y-yeara+1]))
    ind <- which(yrs == y)
    
    #Grab variable to see how many cohorts there are
    ipft      <- ncdf4::ncvar_get(now,'PFT')
    
    #---------------------------------------
    # organize into .css variables (Cohorts)
    # Note: all cohorts from a time slice are assigned to a single patch representing a stand of age X
    #---------------------------------------
    css.tmp <- matrix(nrow=length(ipft),ncol=10)
    colnames(css.tmp) <- c("time", "patch", "cohort", "dbh", "hite", "pft", "n", "bdead", "balive", "Avgrg")
    
    css.tmp[,"time"  ] <- rep(yeara,length(ipft))
    css.tmp[,"patch" ] <- rep(floor((y-yeara)/blckyr)+1,length(ipft))
    css.tmp[,"cohort"] <- 1:length(ipft)
    css.tmp[,"dbh"   ] <- ncdf4::ncvar_get(now,'DBH')
    css.tmp[,"hite"  ] <- ncdf4::ncvar_get(now,'HITE')
    css.tmp[,"pft"   ] <- ipft
    css.tmp[,"n"     ] <- ncdf4::ncvar_get(now,'NPLANT')
    css.tmp[,"bdead" ] <- ncdf4::ncvar_get(now,'BDEAD')
    css.tmp[,"balive"] <- ncdf4::ncvar_get(now,'BALIVE')
    css.tmp[,"Avgrg" ] <- rep(0,length(ipft))
    
    #save big .css matrix
    if(y==yrs[1]){
      css.big <- css.tmp
    } else{
      css.big <- rbind(css.big,css.tmp)
    }
    #---------------------------------------
    
    
    #---------------------------------------
    # save .pss variables (Patches)
    # NOTE: patch AREA needs to be adjusted to be equal to the probability of a stand of age x on the landscape
    #---------------------------------------
    pss.big[ind,"time"]  <- 1800
    pss.big[ind,"patch"] <- floor((y-yeara)/blckyr)+1
    pss.big[ind,"trk"]   <- 1
    pss.big[ind,"age"]   <- y-yeara
    # Note: the following are just place holders that will be overwritten post-SAS
    # pss.big[ind,6]  <- ncdf4::ncvar_get(now,"AREA")
    pss.big[ind,"water"]  <- 0.5 
    pss.big[ind,"fsc"]  <- ncdf4::ncvar_get(now,"FAST_SOIL_C")
    pss.big[ind,"stsc"]  <- ncdf4::ncvar_get(now,"STRUCTURAL_SOIL_C")
    pss.big[ind,"stsl"] <- ncdf4::ncvar_get(now,"STRUCTURAL_SOIL_L")
    pss.big[ind,"ssc"] <- ncdf4::ncvar_get(now,"SLOW_SOIL_C")
    pss.big[ind,"psc"] <- 0
    pss.big[ind,"msn"] <- ncdf4::ncvar_get(now,"MINERALIZED_SOIL_N")
    pss.big[ind,"fsn"] <- ncdf4::ncvar_get(now,"FAST_SOIL_N")
    
    ncdf4::nc_close(now)
  }
  #---------------------------------------  
  
  #---------------------------------------  
  # Extraction Loop Part 2: Patches!
  # This loop does the following:
  #  -- Extract age slice (new patch) soil carbon conditions from *monthly* *histo* files
  #       -- Note: this is done because most of the necessary inputs for SAS are instantaneous values that 
  #                are not currently tracked in analy files, let alone annual analy files; this could 
  #                theoretically change in the future
  #       -- Monthly data is then aggregated to a yearly value: sum for carbon inputs; mean for temp/moist 
  #          (if not calculated above)
  #---------------------------------------
  pss.big <- pss.big[complete.cases(pss.big),]
  
  # some empty vectors for storage etc
  fsc_in_y <- ssc_in_y <- ssl_in_y <- fsn_in_y <- pln_up_y <- vector()
  fsc_in_m <- ssc_in_m <- ssl_in_m <- fsn_in_m <- pln_up_m <-  vector()
  # # NOTE: The following line should get removed if we roll with 20-year mean temp & moist
  # soil_tempk_y <- soil_tempk_m <- swc_max_m <- swc_max_y <- swc_m <- swc_y <- vector()
  
  # switch to the histo directory
  # dat.dir    <- file.path(in.base,sites[s],"/analy/")
  mon.files  <- dir(dir.histo, "-S-") # monthly files only  
  
  #Get time window
  yeara <- as.numeric(strsplit(mon.files,"-")[[1]][yrind+1]) #first year
  yearz <- as.numeric(strsplit(mon.files,"-")[[length(mon.files)-1]][yrind+1]) #last year
  
  montha <- as.numeric(strsplit(mon.files,"-")[[1]][yrind+2]) #first month
  monthz <- as.numeric(strsplit(mon.files,"-")[[length(mon.files)-1]][yrind+2]) #last month
  
  cat(" - Processing History Files \n")
  for (y in yrs){      
    dpm <- lubridate::days_in_month(1:12)
    if(lubridate::leap_year(y)) dpm[2] <- dpm[2]+1
    #calculate month start/end based on year 
    if (y == yrs[1]){
      month.begin = montha
    }else{
      month.begin = 1
    }
    if (y == yrs[length(yrs)]){
      month.end = monthz
    }else{
      month.end = 12
    }
    
    for(m in month.begin:month.end){
      #Make the file name. 
      year.now  <-sprintf("%4.4i",y)
      month.now <- sprintf("%2.2i",m)
      day.now   <- sprintf("%2.2i",1)
      hour.now  <- sprintf("%6.6i",0)
      
      # dat.dir     <- paste(in.base,sites[s],"/histo/",sep="")
      file.now    <- paste(prefix,"-S-",year.now,"-",month.now,"-",day.now,"-"
                           ,hour.now,"-",sufx,sep="")
      
      # cat(" - Reading file :",file.now,"...","\n")
      now <- ncdf4::nc_open(file.path(dir.histo,file.now))
      
      # Note: we have to convert the daily value for 1 month by days per month to get a monthly estimate
      fsc_in_m[m-month.begin+1] <- ncdf4::ncvar_get(now,"FSC_IN")*dpm[m] #kg/(m2*day) --> kg/(m2*month)
      ssc_in_m[m-month.begin+1] <- ncdf4::ncvar_get(now,"SSC_IN")*dpm[m]
      ssl_in_m[m-month.begin+1] <- ncdf4::ncvar_get(now,"SSL_IN")*dpm[m]
      fsn_in_m[m-month.begin+1] <- ncdf4::ncvar_get(now,"FSN_IN")*dpm[m]
      pln_up_m[m-month.begin+1] <- ncdf4::ncvar_get(now,"TOTAL_PLANT_NITROGEN_UPTAKE")*dpm[m]
      # ssc_in_m[m-month.begin+1] <- ncdf4::ncvar_get(now,"SSC_IN")*dpm[m]
      
      # # NOTE: the following lines shoudl get removed if using 20-year means
      # soil_tempk_m[m-month.begin+1] <- ncdf4::ncvar_get(now,"SOIL_TEMPK_PA")[nsoil] # Surface soil temp
      # swc_max_m[m-month.begin+1] <- max(ncdf4::ncvar_get(now,"SOIL_WATER_PA")) # max soil moist to avoid digging through water capacity stuff
      # swc_m[m-month.begin+1] <- ncdf4::ncvar_get(now,"SOIL_WATER_PA")[nsoil] #Surface soil moist
      
      ncdf4::nc_close(now)
    }
    # Find which patch we're working in
    ind <- (y-yeara)/blckyr + 1
    
    # Sum monthly values to get a total estimated carbon input
    fsc_in_y[ind] <- sum(fsc_in_m,na.rm=TRUE)
    ssc_in_y[ind] <- sum(ssc_in_m,na.rm=TRUE)
    ssl_in_y[ind] <- sum(ssl_in_m,na.rm=TRUE)
    fsn_in_y[ind] <- sum(fsn_in_m,na.rm=TRUE)
    pln_up_y[ind] <- sum(pln_up_m,na.rm=TRUE)
    
    # # Soil temp & moisture here should get deleted if using the 20-year means
    # soil_tempk_y[ind] <- mean(soil_tempk_m,na.rm=TRUE) 
    # swc_y[ind] <- mean(swc_m,na.rm=TRUE)/max(swc_max_m,na.rm=TRUE) 
  }
  #---------------------------------------
  
  #---------------------------------------  
  # Calculate steady-state soil pools!
  #
  # These are the equations from soil_respiration.f90 -- if this module has changed, these need 
  # Note: We ignore the unit conversions here because we're now we're working with the yearly 
  #       sum so that we end up with straight kgC/m2
  # fast_C_loss <- kgCday_2_umols * A_decomp * decay_rate_fsc * fast_soil_C
  # struc_C_loss <- kgCday_2_umols * A_decomp * Lc * decay_rate_stsc * struct_soil_C * f_decomp
  # slow_C_loss <- kcCday_2_umols * A_decomp * decay_rate_ssc * slow_soil_C
  #---------------------------------------
  
  # -----------------------
  # Calculate the annual carbon loss if things are stable
  # -----------
  fsc_loss <- decay_rate_fsc
  ssc_loss <- decay_rate_ssc
  ssl_loss <- decay_rate_stsc
  # -----------
  
  
  # *************************************
  # Calculate A_decomp according to your DECOMP_SCPEME
  # A_decomp <- temperature_limitation * water_limitation # aka het_resp_weight
  # *************************************
  # ========================
  # Temperature Limitation 
  # ========================
  # soil_tempk <- sum(soil_tempo_y*area.dist)
  if(decomp_scheme %in% c(0, 3)){
    temperature_limitation = min(1,exp(resp_temperature_increase * (soil_tempk-318.15)))
  } else if(decomp_scheme %in% c(1,4)){
    lnexplloyd             = rh_lloyd_1 * ( rh_lloyd_2 - 1. / (soil_tempk - rh_lloyd_3))
    lnexplloyd             = max(-38.,min(38,lnexplloyd))
    temperature_limitation = min( 1.0, resp_temperature_increase * exp(lnexplloyd) )
  } else if(decomp_scheme==2) {
    # Low Temp Limitation
    lnexplow <- rh_decay_low * (rh_low_temp - soil_tempk)
    lnexplow <- max(-38, min(38, lnexplow))
    tlow_fun <- 1 + exp(lnexplow)
    
    # High Temp Limitation
    lnexphigh <- rh_decay_high*(soil_tempk - rh_high_temp)
    lnexphigh <- max(-38, min(38, lnexphigh))
    thigh_fun <- 1 + exp(lnexphigh)
    
    temperature_limitation <- 1/(tlow_fun*thigh_fun)
  } 
  # ========================
  
  # ========================
  # Moisture Limitation 
  # ========================
  # rel_soil_moist <- sum(swc_y*area.dist)
  if(decomp_scheme %in% c(0,1)){
    if (rel_soil_moist <= resp_opt_water){
      water_limitation = exp( (rel_soil_moist - resp_opt_water) * resp_water_below_opt)
    } else {
      water_limitation = exp( (resp_opt_water - rel_soil_moist) * resp_water_above_opt)
    }
  } else if(decomp_scheme==2){
    # Dry soil Limitation
    lnexpdry <- rh_decay_dry * (rh_dry_smoist - rel_soil_moist)
    lnexpdry <- max(-38, min(38, lnexpdry))
    smdry_fun <- 1+exp(lnexpdry)
    
    # Wet Soil limitation
    lnexpwet <- rh_decay_wet * (rel_soil_moist - rh_wet_smoist)
    lnexpwet <- max(-38, min(38, lnexpwet))
    smwet_fun <- 1+exp(lnexpwet)
    
    water_limitation <- 1/(smdry_fun * smwet_fun)
  } else {
    water_limitation = rel_soil_moist * 4.0893 - rel_soil_moist**2 * 3.1681 - 0.3195897
  }
  # ========================
  
  A_decomp <- temperature_limitation * water_limitation # aka het_resp_weight
  # *************************************
  
  # *************************************
  # Calculate the steady-state pools
  # NOTE: Current implementation weights carbon input by patch size rather than using the 
  #       carbon balance from the oldest state (as was the first implementation)
  # *************************************
  # -------------------
  # Do the carbon and fast nitrogen pools
  # -------------------
  fsc_ss <- fsc_in_y[length(fsc_in_y)]/(fsc_loss * A_decomp)
  ssl_ss <- ssl_in_y[length(ssl_in_y)]/(ssl_loss * A_decomp * Lc) # Structural soil C
  ssc_ss <- ((ssl_loss * A_decomp * Lc * ssl_ss)*(1 - r_stsc))/(ssc_loss * A_decomp )
  fsn_ss <- fsn_in_y[length(fsn_in_y)]/(fsc_loss * A_decomp)
  # -------------------
  
  # -------------------
  # Do the mineralized nitrogen calculation
  # -------------------
  #ED2: csite%mineralized_N_loss  = csite%total_plant_nitrogen_uptake(ipa)             
  # + csite%today_Af_decomp(ipa) * Lc * K1 * csite%structural_soil_C(ipa)                     
  # * ( (1.0 - r_stsc) / c2n_slow - 1.0 / c2n_structural)
  msn_loss <- pln_up_y[length(pln_up_y)] + 
    A_decomp*Lc*ssl_loss*ssl_in_y[length(ssl_in_y)]*
    ((1.0-r_stsc)/c2n_slow - 1.0/c2n_structural)
  
  #fast_N_loss + slow_C_loss/c2n_slow
  msn_med  <- fsc_loss*A_decomp*fsn_in_y[length(fsn_in_y)]+ (ssc_loss * A_decomp)/c2n_slow 
  
  msn_ss   <- msn_med/msn_loss
  # -------------------
  # *************************************
  
  # *************************************
  # Replace dummy values in patch matrix with the steady state calculations
  # *************************************
  # Figure out what steady state value index we shoudl use 
  # Note: In the current implementaiton this should be 1 because we did the weighted averaging up front, 
  #       but if something went wrong and dimensions are off, use this to pick the last (etc)
  p.use <- 1
  
  # write the values to file
  pss.big[,"patch"]  <- 1:nrow(pss.big)
  pss.big[,"area"]  <- area.dist
  pss.big[,"fsc"]  <- rep(fsc_ss[p.use],nrow(pss.big)) # fsc
  pss.big[,"stsc"]  <- rep(ssl_ss[p.use],nrow(pss.big)) # stsc
  pss.big[,"stsl"] <- rep(ssl_ss[p.use],nrow(pss.big)) # stsl (not used)
  pss.big[,"ssc"] <- rep(ssc_ss[p.use],nrow(pss.big)) # ssc
  pss.big[,"msn"] <- rep(msn_ss[p.use],nrow(pss.big)) # msn
  pss.big[,"fsn"] <- rep(fsn_ss[p.use],nrow(pss.big)) # fsn
  # *************************************
  #---------------------------------------
  
  #---------------------------------------
  # Write everything to file!!
  #---------------------------------------
  file.prefix=paste0(prefix, "-lat", lat, "lon", lon)
  write.table(css.big,file=file.path(outdir,paste0(file.prefix,".css")),row.names=FALSE,append=FALSE,
              col.names=TRUE,quote=FALSE)
  
  write.table(pss.big,file=file.path(outdir,paste0(file.prefix,".pss")),row.names=FALSE,append=FALSE,
              col.names=TRUE,quote=FALSE)
  #---------------------------------------
  
}
  