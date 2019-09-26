
##-------------------------------------------------------------------------------------------------#
##' BASGRA wrapper function.
##'
##' BASGRA is written in fortran is run through R by wrapper functions written by Marcel Van Oijen.
##' This function makes use of those wrappers but gives control of datastream in and out of the model to PEcAn.
##' With this function we skip model2netcdf, we can also skip met2model but keeping it for now. 
##' write.config.BASGRA modifies args of this function through template.job
##' then job.sh runs calls this function to run the model
##'
##' @name run_BASGRA
##' @title run BASGRA model
##' @param defaults list of defaults to process
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return OK
##' @export
##' @author Istem Fer
##-------------------------------------------------------------------------------------------------#

# template.job 
run_BASGRA('@BINARY@', '@SITE_MET@', '@RUN_PARAMS@', '@START_DATE@', '@END_DATE@', 
           @SITE_LAT@, @SITE_LON@, ,'@OUTDIR@')

run_BASGRA <- function(binary_path, file_weather, file_params, start_date, end_date, 
                       outdir, sitelat, sitelon){
  

  file_weather <- "/fs/data1/pecan.data/dbfiles/Fluxnet2015_BASGRA_site_1-523/FLX_DK-ZaH_FLUXNET2015_SUBSET_HH_2000-2014_2-3.2000-01-01.2007-12-31.txt"
  file_params <- "/fs/data3/istfer/BASGRA_N/parameters/parameters.txt"
  start_date <- '2000/01/01'
  end_date <- '2007/12/31'
  
  ############################# GENERAL INITIALISATION ########################
  # this part corresponds to initialise_BASGRA_general.R function  
  
  # load DLL
  dyn.load(binary_path)
  
  ################################################################################
  calendar_fert     <- matrix( 0, nrow=100, ncol=3 )
  calendar_Ndep     <- matrix( 0, nrow=100, ncol=3 )
  calendar_Ndep[1,] <- c(1900,  1,0)
  calendar_Ndep[2,] <- c(2100,366,0)
  days_harvest      <- matrix( as.integer(-1), nrow=100, ncol=2 )
  
  ################################################################################
  ### 1. MODEL LIBRARY FILE & FUNCTION FOR RUNNING THE MODEL
  run_model <- function(p = params,
                        w = matrix_weather,
                        calf = calendar_fert,
                        calN = calendar_Ndep,                      
                        h = days_harvest,
                        n = NDAYS) {
    .Fortran('BASGRA', p,w,calf,calN,h,n, NOUT,matrix(0,n,NOUT))[[8]]
  }
  
  ################################################################################
  ### 2. FUNCTIONS FOR READING WEATHER DATA
  read_weather_Bioforsk <- function(y = year_start,
                                    d = doy_start,
                                    n = NDAYS,
                                    f = file_weather) {
    df_weather            <- read.table( f, header=TRUE )
    row_start             <- 1
    while( df_weather[row_start,]$YR  < y ) { row_start <- row_start+1 }
    while( df_weather[row_start,]$doy < d ) { row_start <- row_start+1 }
    df_weather_sim        <- df_weather[row_start:(row_start+n-1),]
    NMAXDAYS              <- as.integer(10000)
    NWEATHER              <- as.integer(8)
    matrix_weather        <- matrix( 0., nrow=NMAXDAYS, ncol=NWEATHER )
    matrix_weather[1:n,1] <- df_weather_sim$YR
    matrix_weather[1:n,2] <- df_weather_sim$doy
    matrix_weather[1:n,3] <- df_weather_sim$GR
    matrix_weather[1:n,4] <- df_weather_sim$T
    matrix_weather[1:n,5] <- df_weather_sim$T
    matrix_weather[1:n,6] <- exp(17.27*df_weather_sim$T/(df_weather_sim$T+239)) *
      0.6108 * df_weather_sim$RH / 100
    matrix_weather[1:n,7] <- df_weather_sim$RAINI
    matrix_weather[1:n,8] <- df_weather_sim$WNI   
    return(matrix_weather)
  }
  
  
  
  ################################################################################
  ### 3. OUTPUT VARIABLES
  outputNames <- c(
    "Time"      , "year"     , "doy"      , "DAVTMP"    , "CLV"      , "CLVD"     ,
    "YIELD"     , "CRES"     , "CRT"      , "CST"       , "CSTUB"    , "DRYSTOR"  ,
    "Fdepth"    , "LAI"      , "LT50"     , "O2"        , "PHEN"     , "ROOTD"    ,
    "Sdepth"    , "TANAER"   , "TILG"     , "TILV"      , "WAL"      , "WAPL"     ,
    "WAPS"      , "WAS"      , "WETSTOR"  , "DM"        , "RES"      , "LERG"     , 
    "NELLVG"    , "RLEAF"    , "SLA"      , "TILTOT"    , "FRTILG"   , "FRTILG1"  ,
    "FRTILG2"   , "RDRT"     , "VERN"     ,
    "CLITT"      , "CSOMF", "CSOMS"   , "NLITT"       , "NSOMF",
    "NSOMS"      , "NMIN" , "Rsoil"   , "NemissionN2O",
    "NemissionNO", "Nfert", "Ndep"    , "RWA"         ,
    "NSH"        , "GNSH" , "DNSH"    , "HARVNSH"     ,  "NCSH" ,
    "NCGSH"      , "NCDSH", "NCHARVSH",
    "fNgrowth","RGRTV","FSPOT","RESNOR","TV2TIL","NSHNOR","KNMAX","KN",    # 61:68
    "DMLV"       , "DMST"             , "NSH_DMSH"    ,                    # 69:71
    "Nfert_TOT"  , "YIELD_TOT"        , "DM_MAX"      ,                    # 72:74
    "F_PROTEIN"  , "F_ASH"            ,                                    # 75:76
    "F_WALL_DM"  , "F_WALL_DMSH"      , "F_WALL_LV"   , "F_WALL_ST",       # 77:80
    "F_DIGEST_DM", "F_DIGEST_DMSH"    ,                                    # 81:82
    "F_DIGEST_LV", "F_DIGEST_ST"      , "F_DIGEST_WALL",                   # 83:85
    "RDRS"       , "Precipitation"    , "Nleaching"   , "NSHmob",          # 86:89
    "NSHmobsoil" , "Nfixation"        , "Nupt"        , "Nmineralisation", # 90:93
    "NSOURCE"    , "NSINK"            ,                                    # 94:95
    "NRT"        , "NCRT"             ,                                    # 96:97
    "rNLITT"     , "rNSOMF"           ,                                    # 98:99
    "DAYL"                                                                 # 100
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
    "(g N m-2 d-1)", "(g N m-2 d-1)", "(g N m-2 d-1)", "(-)"          ,              # 49:52
    "(g N m-2)"    , "(g N m-2 d-1)", "(g N m-2 d-1)", "(g N m-2 d-1)", "(-)"      , # 53:57
    "(-)"          , "(-)"          , "(-)"          ,                               # 58:60
    "(-)", "(d-1)", "(-)", "(-)", "(d-1)", "(-)", "(m2 m-2)", "(m2 m-2)",            # 61:68
    "(g DM m-2)"   , "(g DM m-2)"   , "(g N g-1 DM)"  ,                              # 69:71
    "(g N m-2)"    , "(g DM m-2)"   , "(g DM m-2)"    ,                              # 72:74
    "(g g-1 DM)"   , "(g g-1 DM)"   ,                                                # 75:76
    "(g g-1 DM)"   , "(g g-1 DM)"   , "(g g-1 DM)"    , "(g g-1 DM)"  ,              # 77:80
    "(-)"          , "(-)"          ,                                                # 81:82
    "(-)"          , "(-)"          , "(-)"           ,                              # 83:85
    "(d-1)"        , "(mm d-1)"     , "(g N m-2 d-1)" , "(g N m-2 d-1)",             # 86:89
    "(g N m-2 d-1)", "(g N m-2 d-1)", "(g N m-2 d-1)" , "(g N m-2 d-1)",             # 90:93
    "(g N m-2 d-1)", "(g N m-2 d-1)",                                                # 94:95
    "(g N m-2)"    , "(g N g-1 C)"  ,                                                # 96:97
    "(g N m-2)"    , "(g N g-1 C)"  ,                                                # 98:99
    "(d d-1)"                                                                        # 100
  )
  
  NOUT <- as.integer( length(outputNames) )
  
  
  ############################# SITE CONDITIONS  ########################
  # this part corresponds to  initialise_BASGRA_***.R functions  
  
  start_date  <- as.POSIXlt(start_date, tz = "UTC")
  end_date    <- as.POSIXlt(end_date, tz = "UTC")
  start_year  <- lubridate::year(start_date)
  end_year    <- lubridate::year(end_date)
  
  year_start  <- as.integer(start_year)
  doy_start   <- as.integer(lubridate::yday(start_date))
  NDAYS       <- as.integer(sum(PEcAn.utils::days_in_year(start_year:end_year))) # could be partial years, change later
  parcol      <- 13  
  
  matrix_weather    <- read_weather_Bioforsk(year_start,doy_start,NDAYS,file_weather)
  
  # hardcoding these for now, should be able to modify later on
  calendar_fert[1,] <- c( 2000, 115, 140*1000/ 10000      ) # 140 kg N ha-1 applied on day 115
  calendar_fert[2,] <- c( 2000, 150,  80*1000/ 10000      ) #  80 kg N ha-1 applied on day 150
  #    calendar_fert[3,] <- c( 2001, 123, 0*1000/ 10000      ) # 0 kg N ha-1 applied on day 123
  calendar_Ndep[1,] <- c( 1900,   1,  2*1000/(10000*365) ) #  2 kg N ha-1 y-1 N-deposition in 1900
  calendar_Ndep[2,] <- c( 1980, 366, 20*1000/(10000*365) ) # 20 kg N ha-1 y-1 N-deposition in 1980
  calendar_Ndep[3,] <- c( 2100, 366, 20*1000/(10000*365) ) # 20 kg N ha-1 y-1 N-deposition in 2100
  days_harvest [1,] <- c( 2000, 150 )
  days_harvest [2,] <- c( 2000, 216 )
  
  days_harvest      <- as.integer(days_harvest)
  
  # create vector params
  df_params      <- read.table(file_params,header=T,sep="\t",row.names=1)
  parcol         <- 13
  params         <- df_params[,parcol]
 
  
  # run  model
  output <- .Fortran('BASGRA', 
                     params,
                     matrix_weather, 
                     calendar_fert,
                     calendar_Ndep,
                     days_harvest,
                     NDAYS, 
                     NOUT,
                     matrix(0,NDAYS,NOUT))[[8]]

  head(output)
  
  ############################# WRITE OUTPUTS ###########################
  # writing model outputs already in standard format
  
  # only LAI and CropYield for now
  
  lai <- output[,which(outputNames == "LAI")]
    
  CropYield <- output[,which(outputNames == "YIELD")]
    
    
} # run_BASGRA

################################################################################






