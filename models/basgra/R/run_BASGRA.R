
##-------------------------------------------------------------------------------------------------#
##' BASGRA wrapper function. Runs and writes model outputs in PEcAn standard.
##'
##' BASGRA is written in fortran is run through R by wrapper functions written by Marcel Van Oijen.
##' This function makes use of those wrappers but gives control of datastream in and out of the model to PEcAn.
##' With this function we skip model2netcdf, we can also skip met2model but keeping it for now. 
##' write.config.BASGRA modifies args of this function through template.job
##' then job.sh runs calls this function to run the model
##'
##' @name run_BASGRA
##' @title run BASGRA model
##' @param run_met path to CF met
##' @param run_params parameter vector
##' @param site_harvest path to harvest file
##' @param site_fertilize path to fertilizer application file
##' @param start_date start time of the simulation
##' @param end_date end time of the simulation
##' @param outdir where to write BASGRA output
##' @param sitelat latitude of the site
##' @param sitelon longitude of the site
##' @param co2_file path to daily atmospheric CO2 concentration file, optional, defaults to 350 ppm when missing
##' 
##' @export
##' @useDynLib PEcAn.BASGRA, .registration = TRUE
##' @author Istem Fer
##-------------------------------------------------------------------------------------------------#

run_BASGRA <- function(run_met, run_params, site_harvest, site_fertilize, start_date, end_date, outdir, 
                       sitelat, sitelon, co2_file = NULL){
  
  start_date  <- as.POSIXlt(start_date, tz = "UTC")
  end_date    <- as.POSIXlt(end_date, tz = "UTC")
  start_year  <- lubridate::year(start_date)
  end_year    <- lubridate::year(end_date)
  

  ################################################################################
  ### FUNCTIONS FOR READING WEATHER DATA
  mini_met2model_BASGRA <- function(file_path,
                                    start_date, start_year,
                                    end_date, end_year) {
    
    # TODO: read partial years
    
    out.list <- list()
    
    ctr <- 1
    for(year in seq(start_year, end_year)) {
      
      if(year == start_year & year != end_year){
        simdays <- seq(lubridate::yday(start_date), PEcAn.utils::days_in_year(year))
      }else if(year != start_year & year == end_year){
        simdays <- seq(1, lubridate::yday(end_date))
      }else{
        if(year == start_year & year == end_year){
          simdays <- seq(lubridate::yday(start_date), lubridate::yday(end_date))
        }else{
          simdays <- 1:365 #seq_len(PEcAn.utils::days_in_year(year))
        }
        
      }
      
      
      NDAYS          <- length(simdays)
      NWEATHER       <- as.integer(8)
      matrix_weather <- matrix( 0., nrow = NDAYS, ncol = NWEATHER )
      
      
      # prepare data frame for BASGRA format, daily inputs, but doesn't have to be full year
      
      
      matrix_weather[ ,1] <- rep(year, NDAYS) # year
      matrix_weather[ ,2] <- simdays
      
      old.file <- file.path(dirname(file_path), paste(basename(file_path), year, "nc", sep = "."))
      
      if (file.exists(old.file)) {
        
        ## open netcdf
        nc <- ncdf4::nc_open(old.file)  
        on.exit(ncdf4::nc_close(nc), add = TRUE)
        
        ## convert time to seconds
        sec <- nc$dim$time$vals
        sec <- udunits2::ud.convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")
        
        dt <- PEcAn.utils::seconds_in_year(year) / length(sec)
        tstep <- round(86400 / dt)
        dt <- 86400 / tstep
        
        ind <- rep(simdays, each = tstep)
        
        rad <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")
        gr  <- rad *  0.0864 # W m-2 to MJ m-2 d-1
        # temporary hack, not sure if it will generalize with other data products
        # function might need a splitting arg
        gr  <- gr[nc$dim$time$vals %in% simdays] 
        
        matrix_weather[ ,3]  <- round(tapply(gr, ind, mean, na.rm = TRUE), digits = 2) # irradiation (MJ m-2 d-1)
        
        Tair   <- ncdf4::ncvar_get(nc, "air_temperature")  ## in Kelvin
        Tair   <- Tair[nc$dim$time$vals %in% simdays]
        Tair_C <- udunits2::ud.convert(Tair, "K", "degC")
        
        
        #in BASGRA tmin and tmax is only used to calculate the average daily temperature, see environment.f90
        t_dmean <- round(tapply(Tair_C, ind, mean, na.rm = TRUE), digits = 2) # maybe round these numbers 
        matrix_weather[ ,4] <- t_dmean # mean temperature (degrees Celsius)
        matrix_weather[ ,5] <- t_dmean # that's what they had in read_weather_Bioforsk
        
        RH <-ncdf4::ncvar_get(nc, "relative_humidity")  # %
        RH <- RH[nc$dim$time$vals %in% simdays]
        RH <- round(tapply(RH, ind, mean, na.rm = TRUE), digits = 2) 
        
        # This is vapor pressure according to BASGRA.f90#L86 and environment.f90#L49
        matrix_weather[ ,6] <- round(exp(17.27*t_dmean/(t_dmean+239)) * 0.6108 * RH / 100, digits = 2)
        
        # TODO: check these
        Rain  <- ncdf4::ncvar_get(nc, "precipitation_flux") # kg m-2 s-1
        Rain  <- Rain[nc$dim$time$vals %in% simdays]
        raini <- tapply(Rain*86400, ind, mean, na.rm = TRUE) 
        matrix_weather[ ,7] <- round(raini, digits = 2) # precipitation (mm d-1)	
        
        U <- try(ncdf4::ncvar_get(nc, "eastward_wind"))
        V <- try(ncdf4::ncvar_get(nc, "northward_wind"))
        if(is.numeric(U) & is.numeric(V)){
          U  <- U[nc$dim$time$vals %in% simdays]
          V  <- V[nc$dim$time$vals %in% simdays]
          ws <- sqrt(U ^ 2 + V ^ 2)      
        }else{
          ws <- try(ncdf4::ncvar_get(nc, "wind_speed"))
          ws <- ws[nc$dim$time$vals %in% simdays]
          if (is.numeric(ws)) {
            PEcAn.logger::logger.info("eastward_wind and northward_wind absent; using wind_speed")
          }else{
            PEcAn.logger::logger.severe("No variable found to calculate wind_speed")
          }
        }
        
        
        matrix_weather[ ,8] <- round(tapply(ws, ind, mean,  na.rm = TRUE), digits = 2) # mean wind speed (m s-1)			
        
        ncdf4::nc_close(nc)
      } else {
        PEcAn.logger::logger.info("File for year", year, "not found. Skipping to next year")
        next
      }
      
      out.list[[ctr]] <- matrix_weather
      ctr <- ctr + 1
    } # end for-loop around years
    
    matrix_weather <- do.call("rbind", out.list)
    
    #BASGRA wants the matrix_weather to be of 10000 x 8 matrix
    NMAXDAYS <- as.integer(365000)
    nmw      <- nrow(matrix_weather)
    if(nmw > NMAXDAYS){
      matrix_weather <- matrix_weather[seq_len(NMAXDAYS), ]
      PEcAn.logger::logger.info("BASGRA currently runs only", NMAXDAYS, 
                                "simulation days. Limiting the run to the first ", NMAXDAYS, "days of the requested period.")
    }else{
      # append zeros at the end
      matrix_weather <- rbind(matrix_weather, matrix( 0., nrow = (NMAXDAYS - nmw), ncol = 8 ))
    }
    
    return(matrix_weather)
  }
  
  
  
  ################################################################################
  ### OUTPUT VARIABLES (from BASGRA scripts)
  outputNames <- c(
    "Time"      , "year"     , "doy"      , "DAVTMP"    , "CLV"      , "CLVD"     ,
    "YIELD"     , "CRES"     , "CRT"      , "CST"       , "CSTUB"    , "DRYSTOR"  ,
    "Fdepth"    , "LAI"      , "LT50"     , "O2"        , "PHEN"     , "ROOTD"    ,
    "Sdepth"    , "TANAER"   , "TILG"     , "TILV"      , "WAL"      , "WAPL"     ,
    "WAPS"      , "WAS"      , "WETSTOR"  , "DM"        , "RES"      , "PHENCR"     , 
    "NELLVG"    , "NELLVM"    , "SLA"      , "TILTOT"    , "FRTILG"   , "TILG1"  ,
    "TILG2"   , "RDRT"     , "VERN"     ,
    "CLITT"      , "CSOMF", "CSOMS"   , "NLITT"       , "NSOMF",
    "NSOMS"      , "NMIN" , "PHOT"    , "RplantAer"   ,"Rsoil"   , "NemissionN2O",
    "NemissionNO", "Nfert", "Ndep"    , "RWA"         ,
    "NSH"        , "GNSH" , "DNSH"    , "HARVNSH"     ,  "NCSH" ,
    "NCGSH"      , "NCDSH", "NCHARVSH",
    "fNgrowth","RGRTV","FSPOT","RESNOR","TV2TIL","NSHNOR","KNMAX","KN",    # 63:70
    "DMLV"       , "DMST"             , "NSH_DMSH"    ,                    # 71:73
    "Nfert_TOT"  , "YIELD_TOT"        , "DM_MAX"      ,                    # 74:76
    "F_PROTEIN"  , "F_ASH"            ,                                    # 77:78
    "F_WALL_DM"  , "F_WALL_DMSH"      , "F_WALL_LV"   , "F_WALL_ST",       # 79:82
    "F_DIGEST_DM", "F_DIGEST_DMSH"    ,                                    # 83:84
    "F_DIGEST_LV", "F_DIGEST_ST"      , "F_DIGEST_WALL",                   # 85:87
    "RDRS"       , "Precipitation"    , "Nleaching"   , "NSHmob",          # 88:91
    "NSHmobsoil" , "Nfixation"        , "Nupt"        , "Nmineralisation", # 92:95
    "NSOURCE"    , "NSINK"            ,                                    # 96:97
    "NRT"        , "NCRT"             ,                                    # 98:99
    "rNLITT"     , "rNSOMF"           ,                                    # 100:101
    "DAYL"       , "EVAP"             , "TRAN"                             # 102:104
  )
  
  outputUnits <- c(
    "(y)"       , "(y)"      , "(d)"      , "(degC)"    , "(g C m-2)", "(g C m-2)",  #  1: 6
    "(g DM m-2)", "(g C m-2)", "(g C m-2)", "(g C m-2)" , "(g C m-2)", "(mm)"     ,  #  7:12
    "(m)"       , "(m2 m-2)" , "(degC)"   , "(mol m-2)" , "(-)"      , "(m)"      ,  # 13:18
    "(m)"       , "(d)"      , "(m-2)"    , "(m-2)"     , "(mm)"     , "(mm)"     ,  # 19:24
    "(mm)"      , "(mm)"     , "(mm)"     , "(g DM m-2)", "(g g-1)"  , "(m d-1)"  ,  # 25:30
    "(tiller-1)", "(d-1)"    , "(m2 g-1)" , "(m-2)"     , "(-)"      , "(-)"      ,  # 31:36
    "(-)"       , "(d-1)"    , "(-)"      ,                                          # 37:39
    "(g C m-2)"    , "(g C m-2)"    , "(g C m-2)"    , "(g N m-2)"    , "(g N m-2)", # 40:44
    "(g N m-2)"    , "(g N m-2)"    , "(g C m-2 d-1)", "(g N m-2 d-1)",              # 45:48
    "(g C m-2 d-1)", "(g C m-2 d-1)",                                                # 49:50
    "(g N m-2 d-1)", "(g N m-2 d-1)", "(g N m-2 d-1)", "(-)"          ,              # 51:54
    "(g N m-2)"    , "(g N m-2 d-1)", "(g N m-2 d-1)", "(g N m-2 d-1)", "(-)"      , # 55:59
    "(-)"          , "(-)"          , "(-)"          ,                               # 60:62
    "(-)", "(d-1)", "(-)", "(-)", "(d-1)", "(-)", "(m2 m-2)", "(m2 m-2)",            # 63:70
    "(g DM m-2)"   , "(g DM m-2)"   , "(g N g-1 DM)"  ,                              # 71:73
    "(g N m-2)"    , "(g DM m-2)"   , "(g DM m-2)"    ,                              # 74:76
    "(g g-1 DM)"   , "(g g-1 DM)"   ,                                                # 77:78
    "(g g-1 DM)"   , "(g g-1 DM)"   , "(g g-1 DM)"    , "(g g-1 DM)"  ,              # 79:82
    "(-)"          , "(-)"          ,                                                # 83:84
    "(-)"          , "(-)"          , "(-)"           ,                              # 85:87
    "(d-1)"        , "(mm d-1)"     , "(g N m-2 d-1)" , "(g N m-2 d-1)",             # 88:91
    "(g N m-2 d-1)", "(g N m-2 d-1)", "(g N m-2 d-1)" , "(g N m-2 d-1)",             # 92:95
    "(g N m-2 d-1)", "(g N m-2 d-1)",                                                # 96:97
    "(g N m-2)"    , "(g N g-1 C)"  ,                                                # 98:99
    "(g N m-2)"    , "(g N g-1 C)"  ,                                                # 100:101
    "(d d-1)"      , "(mm d-1)"     , "(mm d-1)"                                     # 102:104
  )
  
  NOUT <- as.integer( length(outputNames) )
  
  
  ############################# SITE CONDITIONS  ########################
  # this part corresponds to  initialise_BASGRA_***.R functions  
  
  year_start  <- as.integer(start_year)
  doy_start   <- as.integer(lubridate::yday(start_date))
  
  matrix_weather <- mini_met2model_BASGRA(run_met, start_date, start_year, end_date, end_year)
  
  NDAYS <- as.integer(sum(matrix_weather[,1] != 0))
  
  matrix_weather <- cbind( matrix_weather, matrix_weather[,8]) #add a col
  if(!is.null(co2_file)){
    co2val <- utils::read.table(co2_file, header=TRUE, sep = ",")
    
    weird_line <- which(!paste0(matrix_weather[1:NDAYS,1], matrix_weather[1:NDAYS,2]) %in% paste0(co2val[,1], co2val[,2]))
    if(length(weird_line)!=0){
      matrix_weather <- matrix_weather[-weird_line,]
      NDAYS <- NDAYS-length(weird_line)
    }
    matrix_weather[1:NDAYS,9] <- co2val[paste0(co2val[,1], co2val[,2]) %in% paste0(matrix_weather[1:NDAYS,1], matrix_weather[1:NDAYS,2]),3]
  }else{
    PEcAn.logger::logger.info("No atmospheric CO2 concentration was provided. Using default 350 ppm.")
    matrix_weather[1:NDAYS,9] <- 350
  }

  
  calendar_fert     <- matrix( 0, nrow=300, ncol=3 )
  
  # read in harvest days
  f_days <- as.matrix(utils::read.table(site_fertilize, header = TRUE, sep = ","))
  calendar_fert[1:nrow(f_days),] <- f_days
  
  calendar_Ndep     <- matrix( 0, nrow=300, ncol=3 )
  #calendar_Ndep[1,] <- c(1900,  1,0)
  #calendar_Ndep[2,] <- c(2100, 366, 0)
  
  # hardcoding these for now, should be able to modify later on
  #    calendar_fert[3,] <- c( 2001, 123, 0*1000/ 10000      ) # 0 kg N ha-1 applied on day 123
  calendar_Ndep[1,] <- c( 1900,   1,  0*1000/(10000*365) ) #  2 kg N ha-1 y-1 N-deposition in 1900
  calendar_Ndep[2,] <- c( 1980, 366,  0*1000/(10000*365) ) # 20 kg N ha-1 y-1 N-deposition in 1980
  calendar_Ndep[3,] <- c( 2100, 366,  0*1000/(10000*365) ) # 20 kg N ha-1 y-1 N-deposition in 2100
  
  days_harvest      <- matrix(as.integer(-1), nrow= 300, ncol = 2)
  # read in harvest days
  h_days <- as.matrix(utils::read.table(site_harvest, header = TRUE, sep = ","))
  days_harvest[1:nrow(h_days),] <- h_days[,1:2]

  days_harvest <- as.integer(days_harvest)
  
  # This is a management specific parameter
  # I'll pass it via harvest file as the 3rd column
  # even though it won't change from harvest to harvest, it may change from run to run
  # but just in case users forgot to add the third column to the harvest file:
  if(ncol(h_days) > 2){
    run_params[names(run_params) == "CLAIV"]     <- h_days[1,3]
    run_params[names(run_params) == "TRANCO"]    <- h_days[1,4]
    run_params[names(run_params) == "SLAMAX"]    <- h_days[1,5]
    run_params[names(run_params) == "FSOMFSOMS"] <- h_days[1,6]
  }else{
    PEcAn.logger::logger.info("CLAIV, TRANCO, SLAMAX not provided via harvest file. Using defaults.")
  }
  
  
  # run  model
  output <- .Fortran('BASGRA',
                     run_params,
                     matrix_weather,
                     calendar_fert,
                     calendar_Ndep,
                     days_harvest,
                     NDAYS,
                     NOUT,
                     matrix(0, NDAYS, NOUT))[[8]]
  
  # for now a hack to write other states out
  save(output, file = file.path(outdir, "output_basgra.Rdata"))
  last_vals <- output[nrow(output),]
  names(last_vals) <- outputNames
  save(last_vals, file = file.path(outdir, "last_vals_basgra.Rdata"))
  
  ############################# WRITE OUTPUTS ###########################
  # writing model outputs already in standard format
  
  # only LAI and CropYield for now
  sec_in_day <- 86400
  
  years <- seq(start_year, end_year)
  for (y in years) {
    
    thisyear <- output[ , outputNames == "year"] == y
    
    outlist <- list()
    outlist[[1]]  <- output[thisyear, which(outputNames == "LAI")]  # LAI in (m2 m-2)
    
    CropYield     <- output[thisyear, which(outputNames == "YIELD")] # (g DM m-2)
    outlist[[2]]  <- udunits2::ud.convert(CropYield, "g m-2", "kg m-2")  
    
    clitt         <- output[thisyear, which(outputNames == "CLITT")] # (g C m-2)
    outlist[[3]]  <- udunits2::ud.convert(clitt, "g m-2", "kg m-2")  
    
    csomf         <- output[thisyear, which(outputNames == "CSOMF")] # (g C m-2)
    outlist[[4]]  <- udunits2::ud.convert(csomf, "g m-2", "kg m-2")  
    
    csoms         <- output[thisyear, which(outputNames == "CSOMS")] # (g C m-2)
    outlist[[5]]  <- udunits2::ud.convert(csoms, "g m-2", "kg m-2")  
    
    outlist[[6]]  <- udunits2::ud.convert(csomf + csoms, "g m-2", "kg m-2") 
    
    # Soil Respiration in kgC/m2/s
    rsoil         <- output[thisyear, which(outputNames == "Rsoil")] # (g C m-2 d-1)
    outlist[[7]]  <- udunits2::ud.convert(rsoil, "g m-2", "kg m-2") / sec_in_day
    
    # Autotrophic Respiration in kgC/m2/s
    rplantaer     <- output[thisyear, which(outputNames == "RplantAer")] # (g C m-2 d-1)
    outlist[[8]]  <- udunits2::ud.convert(rplantaer, "g m-2", "kg m-2") / sec_in_day
    
    # NEE in kgC/m2/s
    # NOTE: According to BASGRA_N documentation: LUEMXQ (used in PHOT calculation) accounts for carbon lost to maintenance respiration, 
    # but not growth respiration. So, photosynthesis rate is gross photosynthesis minus maintenance respiration
    # So this is not really GPP, but it wasn't obvious to add what to get GPP, but I just want NEE for now, so it's OK
    phot          <- output[thisyear, which(outputNames == "PHOT")] # (g C m-2 d-1)
    nee           <- -1.0 * (phot - (rsoil + rplantaer))
    outlist[[9]]  <- udunits2::ud.convert(nee, "g m-2", "kg m-2") / sec_in_day
    
    # again this is not technically GPP
    outlist[[10]]  <- udunits2::ud.convert(phot, "g m-2", "kg m-2") / sec_in_day
    
    # Qle W/m2
    outlist[[11]]  <- ( output[thisyear, which(outputNames == "EVAP")] + output[thisyear, which(outputNames == "TRAN")] * 
                          PEcAn.data.atmosphere::get.lv()) / sec_in_day  
    
    # ******************** Declare netCDF dimensions and variables ********************#
    t <- ncdf4::ncdim_def(name = "time", 
                          units = paste0("days since ", y, "-01-01 00:00:00"), 
                          matrix_weather[matrix_weather[,1] == y, 2], # allow partial years, this info is already in matrix_weather
                          calendar = "standard", 
                          unlim = TRUE)
    
    
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")
    
    dims <- list(lon = lon, lat = lat, time = t)
    
    nc_var <- list()
    nc_var[[1]]   <- PEcAn.utils::to_ncvar("LAI", dims)
    nc_var[[2]]   <- PEcAn.utils::to_ncvar("CropYield", dims)
    nc_var[[3]]   <- PEcAn.utils::to_ncvar("litter_carbon_content", dims)
    nc_var[[4]]   <- PEcAn.utils::to_ncvar("fast_soil_pool_carbon_content", dims)
    nc_var[[5]]   <- PEcAn.utils::to_ncvar("slow_soil_pool_carbon_content", dims)
    nc_var[[6]]   <- PEcAn.utils::to_ncvar("TotSoilCarb", dims)
    nc_var[[7]]   <- PEcAn.utils::to_ncvar("SoilResp", dims)
    nc_var[[8]]   <- PEcAn.utils::to_ncvar("AutoResp", dims)
    nc_var[[9]]   <- PEcAn.utils::to_ncvar("NEE", dims)
    nc_var[[10]]  <- PEcAn.utils::to_ncvar("GPP", dims)
    nc_var[[11]]  <- PEcAn.utils::to_ncvar("Qle", dims)
    
    # ******************** Declare netCDF variables ********************#
    
    ### Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    for (i in seq_along(nc_var)) {
      # print(i)
      ncdf4::ncvar_put(nc, nc_var[[i]], outlist[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile, sep = "\n")
    }
    close(varfile)
    ncdf4::nc_close(nc)
  } # end year-loop over outputs
  
} # run_BASGRA

