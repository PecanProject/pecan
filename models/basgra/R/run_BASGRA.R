
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
##' @param write_raw_output write raw output in csv or not
##' 
##' @export
##' @useDynLib PEcAn.BASGRA, .registration = TRUE
##' @author Istem Fer, Julius Vira
##-------------------------------------------------------------------------------------------------#

run_BASGRA <- function(run_met, run_params, site_harvest, site_fertilize, start_date, end_date, outdir, 
                       sitelat, sitelon, co2_file = NULL, write_raw_output = FALSE){
  
  start_date  <- as.POSIXlt(start_date, tz = "UTC")
  if(lubridate::hour(start_date) == 23){ 
    # could be made more sophisticated but if it is specified to the hour this is probably coming from SDA
    start_date <- lubridate::ceiling_date(start_date, "day")
  }
  end_date    <- as.POSIXlt(end_date, tz = "UTC")
  start_year  <- lubridate::year(start_date)
  end_year    <- lubridate::year(end_date)
  
  if(length(co2_file) > 0 && co2_file == "NULL") co2_file <- NULL
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
          simdays <- seq_len(PEcAn.utils::days_in_year(year))
        }
        
      }
      
      NDAYS          <- length(simdays)
      NWEATHER       <- as.integer(9)
      matrix_weather <- matrix( 0., nrow = NDAYS, ncol = NWEATHER )
      
      
      # prepare data frame for BASGRA format, daily inputs, but doesn't have to be full year
      
      
      matrix_weather[ ,1] <- rep(year, NDAYS) # year
      matrix_weather[ ,2] <- simdays

      if(endsWith(file_path, '.nc')){
        # we probably have a (near-term) forecast met
        old.file <- file_path
      }else{
        old.file <- file.path(dirname(file_path), paste(basename(file_path), year, "nc", sep = "."))
      }
      
      if (file.exists(old.file)) {
        
        ## open netcdf
        nc <- ncdf4::nc_open(old.file)  
        on.exit(ncdf4::nc_close(nc), add = TRUE)
        
        ## convert time to seconds
        sec <- nc$dim$time$vals
        sec <- PEcAn.utils::ud_convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")
        
        dt <- diff(sec)[1]
        tstep <- round(86400 / dt)
        dt <- 86400 / tstep
        
        ind <- rep(simdays, each = tstep)
        
        if(unlist(strsplit(nc$dim$time$units, " "))[1] %in% c("days", "day")){
          #this should always be the case, butorigin just in case
          origin_dt <- (as.POSIXct(unlist(strsplit(nc$dim$time$units, " "))[3], "%Y-%m-%d", tz="UTC") + 60*60*24) - dt
          # below -dt means that midnights belong to the day that ends. This is consistent
          # with data files which are exclusive of the 1 Jan midnight + dt till 1 Jan next year.
          # ydays <- lubridate::yday(origin_dt + sec - dt)
          ydays <- lubridate::yday(origin_dt + sec) 
          all_days <- origin_dt + sec
        } else {
          PEcAn.logger::logger.error("Check units of time in the weather data.")
        }

        rad <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")
        gr  <- rad *  0.0864 # W m-2 to MJ m-2 d-1
        # temporary hack, not sure if it will generalize with other data products
        # function might need a splitting arg
        gr  <- gr[(ydays %in% simdays) & (lubridate::year(all_days) == year)]
        if (length(ind) > length(gr)) {
          PEcAn.logger::logger.severe('The input does not cover the requested simulation period')
        }
        matrix_weather[ ,3]  <- round(tapply(gr, ind, mean, na.rm = TRUE), digits = 2) # irradiation (MJ m-2 d-1)
        
        Tair   <- ncdf4::ncvar_get(nc, "air_temperature")  ## in Kelvin
        Tair   <- Tair[(ydays %in% simdays) & (lubridate::year(all_days) == year)]
        Tair_C <- PEcAn.utils::ud_convert(Tair, "K", "degC")
        
        #in BASGRA tmin and tmax is only used to calculate the average daily temperature, see environment.f90
        t_dmean <- round(tapply(Tair_C, ind, mean, na.rm = TRUE), digits = 2) # maybe round these numbers 
        t_dmin <- round(tapply(Tair_C, ind, min, na.rm = TRUE), digits = 2)
        t_dmax <- round(tapply(Tair_C, ind, max, na.rm = TRUE), digits = 2)
        matrix_weather[ ,4] <- t_dmin # mean temperature (degrees Celsius)
        matrix_weather[ ,5] <- t_dmax # that's what they had in read_weather_Bioforsk
        
        RH <- ncdf4::ncvar_get(nc, "relative_humidity")  # %
        RH <- RH[(ydays %in% simdays) & (lubridate::year(all_days) == year)]
        RH <- round(tapply(RH, ind, mean, na.rm = TRUE), digits = 2) 
     
        # This is vapor pressure according to BASGRA.f90#L86 and environment.f90#L49
        matrix_weather[ ,6] <- round(exp(17.27*t_dmean/(t_dmean+239)) * 0.6108 * RH / 100, digits = 2)
        
        # TODO: check these
        Rain  <- ncdf4::ncvar_get(nc, "precipitation_flux") # kg m-2 s-1
        Rain  <- Rain[(ydays %in% simdays) & (lubridate::year(all_days) == year)]
        raini <- tapply(Rain*86400, ind, mean, na.rm = TRUE) 
        matrix_weather[ ,7] <- round(raini, digits = 2) # precipitation (mm d-1)	
        
        U <- try(ncdf4::ncvar_get(nc, "eastward_wind"))
        V <- try(ncdf4::ncvar_get(nc, "northward_wind"))
        if(is.numeric(U) & is.numeric(V)){
          U  <- U[(ydays %in% simdays) & (lubridate::year(all_days) == year)]
          V  <- V[(ydays %in% simdays) & (lubridate::year(all_days) == year)]
          ws <- sqrt(U ^ 2 + V ^ 2)      
        }else{
          ws <- try(ncdf4::ncvar_get(nc, "wind_speed"))
          ws <- ws[(ydays %in% simdays) & (lubridate::year(all_days) == year)]
          if (is.numeric(ws)) {
            PEcAn.logger::logger.info("eastward_wind and northward_wind absent; using wind_speed")
          }else{
            PEcAn.logger::logger.severe("No variable found to calculate wind_speed")
          }
        }
        
        matrix_weather[ ,8] <- round(tapply(ws, ind, mean,  na.rm = TRUE), digits = 2) # mean wind speed (m s-1)			
        
        # CO2
        co2 <- try(ncdf4::ncvar_get(nc, "mole_fraction_of_carbon_dioxide_in_air"))
        if(is.numeric(co2)){
          co2 <- co2[(ydays %in% simdays) & (lubridate::year(all_days) == year)] / 1e-06 # ppm
          co2 <- round(tapply(co2, ind, mean, na.rm = TRUE), digits = 2) 
        }else{
          co2 <- NA
        }
        
        # This is new BASGRA code that can be passed CO2 cals
        matrix_weather[ ,9] <- co2
        
        ncdf4::nc_close(nc)
      } else {
        PEcAn.logger::logger.info("File for year", year, old.file, "not found. Skipping to next year")
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
      matrix_weather <- rbind(matrix_weather, matrix( 0., nrow = (NMAXDAYS - nmw), ncol = 9 ))
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
    "Nfert_TOT"  , "YIELD_POT"        , "DM_MAX"      ,                    # 74:76
    "F_PROTEIN"  , "F_ASH"            ,                                    # 77:78
    "F_WALL_DM"  , "F_WALL_DMSH"      , "F_WALL_LV"   , "F_WALL_ST",       # 79:82
    "F_DIGEST_DM", "F_DIGEST_DMSH"    ,                                    # 83:84
    "F_DIGEST_LV", "F_DIGEST_ST"      , "F_DIGEST_WALL",                   # 85:87
    "RDRS"       , "Precipitation"    , "Nleaching"   , "NSHmob",          # 88:91
    "NSHmobsoil" , "Nfixation"        , "Nupt"        , "Nmineralisation", # 92:95
    "NSOURCE"    , "NSINK"            ,                                    # 96:97
    "NRT"        , "NCRT"             ,                                    # 98:99
    "rNLITT"     , "rNSOMF"           ,                                    # 100:101
    "DAYL"       , "EVAP"             , "TRAN"        , "FLITTC_LEAF",     # 102:105
    "FLITTC_ROOT", "NEE"              , "FHARVC"      , "FRUNOFFC",        # 106:109
    "CSOM_A"     , "CSOM_W"           , "CSOM_E"      , "CSOM_N",          # 110:113
    "CSOM_H"     , "NSOM"             , "TEMPR30"     , "PRECIP30",        # 114:117
    "FSOILAMDC"                                                            # 118
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
    "(d d-1)"      , "(mm d-1)"     , "(mm d-1)"      , "(g C m-2 d-1)",             # 102:105
    "(g C m-2 d-1)", "(g C m-2 d-1)", "(g C m-2 d-1)" , "(g C m-2 d-1)",             # 106:109
    "(g C m-2)"    , "(g C m-2)"    , "(g C m-2)"     , "(g C m-2)",                 # 110:113
    "(g C m-2)"    , "(g N m-2)"    , "(degC)"        , "(mm)",                      # 114:117
    "(g C m-2 d-1)"                                                                  # 118
  )
  
  NOUT <- as.integer( length(outputNames) )
  
  if (length(outputUnits) != NOUT) { PEcAn.logger::logger.severe('#outputNames != #outputUnits') }
  
  ############################# SITE CONDITIONS  ########################
  # this part corresponds to  initialise_BASGRA_***.R functions  
  
  year_start  <- as.integer(start_year)
  doy_start   <- as.integer(lubridate::yday(start_date))
  
  matrix_weather <- mini_met2model_BASGRA(run_met, start_date, start_year, end_date, end_year)
  
  NDAYS <- as.integer(sum(matrix_weather[,1] != 0))
  
  if(!is.null(co2_file)){ # if a separate co2 file was passed use that
    co2val <- utils::read.table(co2_file, header=TRUE, sep = ",")
    
    weird_line <- which(!paste0(matrix_weather[1:NDAYS,1], matrix_weather[1:NDAYS,2]) %in% paste0(co2val[,1], co2val[,2]))
    if(length(weird_line)!=0){
      matrix_weather <- matrix_weather[-weird_line,]
      NDAYS <- NDAYS-length(weird_line)
    }
    matrix_weather[1:NDAYS,9] <- co2val[paste0(co2val[,1], co2val[,2]) %in% paste0(matrix_weather[1:NDAYS,1], matrix_weather[1:NDAYS,2]),3]
  }else if(all(is.na(matrix_weather[1:NDAYS,9]))){ # this means there were no CO2 in the netcdf as well
    PEcAn.logger::logger.info("No atmospheric CO2 concentration was provided. Using default 420 ppm.")
    matrix_weather[1:NDAYS,9] <- 420
  }

  # checking/debugging met
  # write.table(matrix_weather[1:NDAYS,], file=paste0(outdir,"/clim",start_date,".",substr(end_date, 1,10),".csv"), 
  #            sep=",", row.names = FALSE, col.names=FALSE)
  
  calendar_fert     <- matrix( 0, nrow=300, ncol=6)
  
  # read in fertilization
  f_days <- as.matrix(utils::read.table(site_fertilize, header = TRUE, sep = ","))
  if (ncol(f_days) == 3) {
    # old-style fertilization file
    calendar_fert[1:nrow(f_days),1:3] <- f_days
  } else {
    if (ncol(f_days) != 6) {
      PEcAn.logger::logger.severe(sprintf('Wrong number of columns (%i) in fertilization file', ncol(f_days)))
    }
    columns <- c('year', 'doy', 'Nmin', 'Norg', 'C_soluble', 'C_compost')
    calendar_fert[1:nrow(f_days),] <- f_days[,columns]
  }
    
  calendar_Ndep     <- matrix( 0, nrow=300, ncol=3 )
  #calendar_Ndep[1,] <- c(1900,  1,0)
  #calendar_Ndep[2,] <- c(2100, 366, 0)
  
  # hardcoding these for now to be 0, should be able to modify later on
  #    calendar_fert[3,] <- c( 2001, 123, 0*1000/ 10000      ) # 0 kg N ha-1 applied on day 123
  calendar_Ndep[1,] <- c( 1900,   1,  0*1000/(10000*365) ) #  0 kg N ha-1 y-1 N-deposition in 1900
  calendar_Ndep[2,] <- c( 1980, 366,  0*1000/(10000*365) ) #  0 kg N ha-1 y-1 N-deposition in 1980
  calendar_Ndep[3,] <- c( 2100, 366,  0*1000/(10000*365) ) #  0 kg N ha-1 y-1 N-deposition in 2100
  
  harvest_params <- matrix(0.0, nrow=300, ncol=2)
  df_harvest <- utils::read.csv(site_harvest)
  n_events <- nrow(df_harvest)
  allowed_harv_colnames <- c('year', 'doy', 'CLAIV', 'cut_only')
  if (!all(colnames(df_harvest) %in% allowed_harv_colnames)) {
    PEcAn.logger::logger.severe(c('Bad column names in harvest file: ', colnames(df_harvest)))
  }
  days_harvest <- matrix(as.integer(-1), nrow= 300, ncol = 2)
  if (n_events > 0) {
    days_harvest[1:n_events,1:2] <- as.matrix(df_harvest[,c('year', 'doy')])
  }
  if ('CLAIV' %in% colnames(df_harvest)) {
    harvest_params[1:n_events,1] <- df_harvest$CLAIV
  } else { # default
    harvest_params[1:n_events,1] <- run_params[names(run_params) == "CLAIV"]
  }
  if ('cut_only' %in% colnames(df_harvest)) {
    harvest_params[1:n_events,2] <- df_harvest$cut_only
  } else {
    harvest_params[1:n_events,2] <- 0.0
  }
  # read in harvest days
  #h_days <- as.matrix(utils::read.table(site_harvest, header = TRUE, sep = ","))
  #days_harvest[1:nrow(h_days),1:2] <- h_days[,1:2]
  
  # This is a management specific parameter
  # CLAIV is used to determine LAI remaining after harvest
  # I modified BASGRA code to use different values for different harvests
  # I'll pass it via harvest file as the 3rd column
  # but just in case users forgot to add the third column to the harvest file:
  #if(ncol(h_days) == 3){
  #  days_harvest[1:nrow(h_days),3] <- h_days[,3]*10 # as.integer
  #}else{
  #  PEcAn.logger::logger.info("CLAIV not provided via harvest file. Using defaults.")
  #  days_harvest[1:nrow(h_days),3] <- run_params[names(run_params) == "CLAIV"] 
  #}
  #days_harvest <- as.integer(days_harvest)
  
  # run  model
  NPARAMS = as.integer(160) # from set_params.f90
  if (length(run_params) != NPARAMS) {
    PEcAn.logger::logger.severe(sprintf('%i parameters required, %i given', NPARAMS, length(run_params)))
  }
  if (NOUT < 118) { # from BASGRA.f90
    PEcAn.logger::logger.severe("at least 118 parameters required,", NOUT, "given")
  }


  output <- .Fortran('BASGRA',
                     run_params,
                     matrix_weather,
                     calendar_fert,
                     calendar_Ndep,
                     as.integer(days_harvest),
                     harvest_params,
                     NPARAMS, 
                     NDAYS,
                     NOUT,
                     matrix(0, NDAYS, NOUT))[[10]]
  # for now a hack to write other states out
  # save(output, file = file.path(outdir, "output_basgra.Rdata"))
  if (write_raw_output) {
    utils::write.csv(stats::setNames(as.data.frame(output), outputNames), file.path(outdir, "output_basgra.csv"))
  }
  last_vals <- output[nrow(output),]
  names(last_vals) <- outputNames
  save(last_vals, file = file.path(outdir, "last_vals_basgra.Rdata"))
  
  ############################# WRITE OUTPUTS ###########################
  # writing model outputs already in standard format
  
  # only LAI and CropYield for now
  sec_in_day <- 86400
  
  years <- seq(start_year, end_year)

  # Having the Yasso soil affects how some C pools are aggregated
  have_yasso <- run_params[137] > 0
  
  for (y in years) {
    
    thisyear <- output[ , outputNames == "year"] == y
    
    outlist <- list()
    outlist[[length(outlist)+1]]  <- output[thisyear, which(outputNames == "LAI")]  # LAI in (m2 m-2)
    
    CropYield     <- output[thisyear, which(outputNames == "YIELD_POT")] # (g DM m-2)
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(CropYield, "g m-2", "kg m-2")  
    
    clitt         <- output[thisyear, which(outputNames == "CLITT")] # (g C m-2)
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(clitt, "g m-2", "kg m-2")  
    
    cstub         <- output[thisyear, which(outputNames == "CSTUB")] # (g C m-2)
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(cstub, "g m-2", "kg m-2")  
    
    cst           <- output[thisyear, which(outputNames == "CST")] # (g C m-2)
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(cst, "g m-2", "kg m-2") 
    
    crt           <- output[thisyear, which(outputNames == "CRT")] # (g C m-2)
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(crt, "g m-2", "kg m-2") 
    
    cres          <- output[thisyear, which(outputNames == "CRES")] # (g C m-2)
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(cres, "g m-2", "kg m-2") 
    
    clv           <- output[thisyear, which(outputNames == "CLV")] # (g C m-2)
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(clv, "g m-2", "kg m-2") 
    
    clvd         <- output[thisyear, which(outputNames == "CLVD")] # (g C m-2)
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(clvd, "g m-2", "kg m-2") 

    if (have_yasso) {
      csomf         <- rowSums(output[thisyear, outputNames %in% c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N'), drop=FALSE]) # (g C m-2)
      outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(csomf, "g m-2", "kg m-2")  
      csoms         <- output[thisyear, outputNames == "CSOM_H"] # (g C m-2)
      outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(csoms, "g m-2", "kg m-2")
      nsom          <- output[thisyear, outputNames == "NSOM"] # (g N m-2)
      outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(nsom, "g m-2", "kg m-2")
    } else {
      csomf         <- output[thisyear, which(outputNames == "CSOMF")] # (g C m-2)
      outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(csomf, "g m-2", "kg m-2")  
      csoms         <- output[thisyear, which(outputNames == "CSOMS")] # (g C m-2)
      outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(csoms, "g m-2", "kg m-2")  
      nsomf          <- output[thisyear, outputNames == "NSOMF"] # (g N m-2)
      nsoms          <- output[thisyear, outputNames == "NSOMS"] # (g N m-2)
      outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(nsomf+nsoms, "g m-2", "kg m-2")
    }
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(csomf + csoms, "g m-2", "kg m-2") 
    outlist[[length(outlist)+1]]  <- output[thisyear, which(outputNames == "TILG1")] 
    outlist[[length(outlist)+1]]  <- output[thisyear, which(outputNames == "TILG2")] 
    outlist[[length(outlist)+1]]  <- output[thisyear, which(outputNames == "TILV")] 
    outlist[[length(outlist)+1]]  <- output[thisyear, which(outputNames == "PHEN")] 
    
    outlist[[length(outlist) + 1]] <- output[thisyear, which(outputNames == "TILG1")] + 
      output[thisyear, which(outputNames == "TILG2")] + output[thisyear, which(outputNames == "TILV")]
    
    # Soil Respiration in kgC/m2/s
    rsoil         <- output[thisyear, which(outputNames == "Rsoil")] # (g C m-2 d-1)
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(rsoil, "g m-2", "kg m-2") / sec_in_day
    
    # Autotrophic Respiration in kgC/m2/s
    rplantaer     <- output[thisyear, which(outputNames == "RplantAer")] # (g C m-2 d-1)
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(rplantaer, "g m-2", "kg m-2") / sec_in_day
    
    # NEE in kgC/m2/s
    # NOTE: According to BASGRA_N documentation: LUEMXQ (used in PHOT calculation) accounts for carbon lost to maintenance respiration, 
    # but not growth respiration. So, photosynthesis rate is gross photosynthesis minus maintenance respiration
    # So this is not really GPP, but it wasn't obvious to add what to get GPP, but I just want NEE for now, so it's OK
    phot          <- output[thisyear, which(outputNames == "PHOT")] # (g C m-2 d-1)
    nee           <- -1.0 * (phot - (rsoil + rplantaer))
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(nee, "g m-2", "kg m-2") / sec_in_day
    
    # again this is not technically GPP
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(phot, "g m-2", "kg m-2") / sec_in_day
    
    # NPP 
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(nee - rsoil, "g m-2", "kg m-2") / sec_in_day
    
    # Qle W/m2
    outlist[[length(outlist)+1]]  <- ( output[thisyear, which(outputNames == "EVAP")] + output[thisyear, which(outputNames == "TRAN")] * 
                          PEcAn.data.atmosphere::get.lv()) / sec_in_day  
    
    # SoilMoist (!!! only liquid water !!!) kg m-2
    # during the groowing season its depth will mainly be equal to the rooting depth, but during winter its depth will be ROOTD-Fdepth
    soilm <- output[thisyear, which(outputNames == "WAL")] # mm
    outlist[[length(outlist)+1]]  <- PEcAn.utils::ud_convert(soilm, "mm", "m") * 1000 # (kg m-3) density of water in soil

    # WCL = WAL*0.001 / (ROOTD-Fdepth) Water concentration in non-frozen soil
    # need to think about ice! but the sensors maybe don't measure that
    ROOTD  <- output[thisyear, which(outputNames == "ROOTD")] 
    Fdepth <- output[thisyear, which(outputNames == "Fdepth")] 
    outlist[[length(outlist)+1]]  <- soilm * 0.001 / (ROOTD - Fdepth)
    
    # Additional C fluxes
    outlist[[length(outlist)+1]] <- PEcAn.utils::ud_convert(output[thisyear, outputNames == "FLITTC_LEAF"],
                                                         "g m-2", "kg m-2") / sec_in_day    
    outlist[[length(outlist)+1]] <- PEcAn.utils::ud_convert(output[thisyear, outputNames == "FLITTC_ROOT"],
                                                         "g m-2", "kg m-2") / sec_in_day
    outlist[[length(outlist)+1]] <- PEcAn.utils::ud_convert(output[thisyear, outputNames == "FHARVC"],
                                                         "g m-2", "kg m-2") / sec_in_day
    outlist[[length(outlist)+1]] <- PEcAn.utils::ud_convert(output[thisyear, outputNames == "NEE"],
                                                         "g m-2", "kg m-2") / sec_in_day
    outlist[[length(outlist)+1]] <- PEcAn.utils::ud_convert(output[thisyear, outputNames == "FRUNOFFC"],
                                                         "g m-2", "kg m-2") / sec_in_day
    outlist[[length(outlist)+1]] <- PEcAn.utils::ud_convert(output[thisyear, outputNames == "FSOILAMDC"],
                                                         "g m-2", "kg m-2") / sec_in_day
    outlist[[length(outlist)+1]] <- output[thisyear, outputNames == "TEMPR30"]
    outlist[[length(outlist)+1]] <- output[thisyear, outputNames == "PRECIP30"]

    
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
    nc_var[[length(nc_var)+1]]   <- PEcAn.utils::to_ncvar("LAI", dims)
    nc_var[[length(nc_var)+1]]   <- PEcAn.utils::to_ncvar("CropYield", dims)
    nc_var[[length(nc_var)+1]]   <- PEcAn.utils::to_ncvar("litter_carbon_content", dims)
    nc_var[[length(nc_var)+1]]   <- ncdf4::ncvar_def("stubble_carbon_content", units = "kg C m-2", dim = dims, missval = -999,
                        longname = "Stubble Carbon Content")
    nc_var[[length(nc_var)+1]]   <- ncdf4::ncvar_def("stem_carbon_content", units = "kg C m-2", dim = dims, missval = -999,
                                      longname = "Stem Carbon Content")
    nc_var[[length(nc_var)+1]]   <- PEcAn.utils::to_ncvar("root_carbon_content", dims)
    nc_var[[length(nc_var)+1]]   <- ncdf4::ncvar_def("reserve_carbon_content", units = "kg C m-2", dim = dims, missval = -999,
                                      longname = "Reserve Carbon Content")
    nc_var[[length(nc_var)+1]]   <- PEcAn.utils::to_ncvar("leaf_carbon_content", dims)
    nc_var[[length(nc_var)+1]]   <- ncdf4::ncvar_def("dead_leaf_carbon_content", units = "kg C m-2", dim = dims, missval = -999,
                                      longname = "Dead Leaf Carbon Content")
    nc_var[[length(nc_var)+1]]  <- PEcAn.utils::to_ncvar("fast_soil_pool_carbon_content", dims)
    nc_var[[length(nc_var)+1]]  <- PEcAn.utils::to_ncvar("slow_soil_pool_carbon_content", dims)
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("soil_organic_nitrogen_content", units = "kg N m-2", dim = dims, missval = -999,
                                                    longname = "Soil Organic Nitrogen Content by Layer	") 
    nc_var[[length(nc_var)+1]]  <- PEcAn.utils::to_ncvar("TotSoilCarb", dims)
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("nonelongating_generative_tiller", units = "m-2", dim = dims, missval = -999,
                                      longname = "Non-elongating generative tiller density") 
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("elongating_generative_tiller", units = "m-2", dim = dims, missval = -999,
                                      longname = "Elongating generative tiller density") 
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("nonelongating_vegetative_tiller", units = "m-2", dim = dims, missval = -999,
                                      longname = "Non-elongating vegetative tiller density")
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("phenological_stage", units = "-", dim = dims, missval = -999,
                                      longname = "Phenological stage")
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("tiller_density", units = "m-2", dim = dims, missval = -999,
                                                    longname = "Tiller density")
    nc_var[[length(nc_var)+1]]  <- PEcAn.utils::to_ncvar("SoilResp", dims)
    nc_var[[length(nc_var)+1]]  <- PEcAn.utils::to_ncvar("AutoResp", dims)
    nc_var[[length(nc_var)+1]]  <- PEcAn.utils::to_ncvar("NEE", dims)
    nc_var[[length(nc_var)+1]]  <- PEcAn.utils::to_ncvar("GPP", dims)
    nc_var[[length(nc_var)+1]]  <- PEcAn.utils::to_ncvar("NPP", dims)
    nc_var[[length(nc_var)+1]]  <- PEcAn.utils::to_ncvar("Qle", dims)
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("SoilMoist", units = "kg m-2", dim = dims, missval = -999,
                                      longname = "Average Layer Soil Moisture")
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("SoilMoistFrac", units = "m3 m-3", dim = dims, missval = -999,
                                                    longname = "Average Layer Fraction of Saturation")
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("leaf_litter_carbon_flux", units = "kg C m-2 s-1", dim = dims,
                                                    missval = -999, longname='Flux of carbon from leaf litter to soil pools')
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("fine_root_litter_carbon_flux", units = "kg C m-2 s-1", dim = dims,
                                                    missval = -999, longname='Flux of carbon from fine root litter to soil pools')
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("harvest_carbon_flux", units = "kg C m-2 s-1", dim = dims,
                                                    missval = -999, longname='Flux of carbon removed by harvest')
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("NEE_alt", units = "kg C m-2 s-1", dim = dims,
                                                    missval = -999, longname='Alternative NEE')
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("FRUNOFFC", units = "kg C m-2 s-1", dim = dims,
                                                    missval = -999, longname='C in runoff')
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("FSOILAMDC", units = "kg C m-2 s-1", dim = dims,
                                                    missval = -999, longname='Flux of carbon input in soil amendments')
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("TEMPR30", units = "degC", dim = dims,
                                                    missval = -999, longname='Smoothed air temperature')
    nc_var[[length(nc_var)+1]]  <- ncdf4::ncvar_def("PRECIP30", units = "mm/day", dim = dims,
                                                    missval = -999, longname='Smoothed daily precipitation')

    
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

