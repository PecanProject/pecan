#' @title sda.enkf.multisite
#' @name  sda.enkf.multisite
#' @author Michael Dietze, Ann Raiho and Alexis Helgeson \email{dietze@@bu.edu}
#' 
#' @param settings  PEcAn settings object
#' @param obs.mean  List of dataframe of observation means, named with observation datetime.
#' @param obs.cov   List of covariance matrices of state variables , named with observation datetime.
#' @param Q         Process covariance matrix given if there is no data to estimate it.
#' @param restart   Used for iterative updating previous forecasts. Default NULL. List object includes file path to previous runs and start date for SDA
#' @param forceRun  Used to force job.sh files that were not run for ensembles in SDA (quick fix) 
#' @param keepNC    Used for debugging issues. .nc files are usually removed after each year in the out folder. This flag will keep the .nc + .nc.var files for futher investigations.
#' @param control   List of flags controlling the behaviour of the SDA. trace for reporting back the SDA outcomes, interactivePlot for plotting the outcomes after each step, 
#' TimeseriesPlot for post analysis examination, BiasPlot for plotting the correlation between state variables, plot.title is the title of post analysis plots and debug mode allows for pausing the code and examinign the variables inside the function.
#'
#’ @details
#’ Restart mode:  Basic idea is that during a restart (primary case envisioned as an iterative forecast), a new workflow folder is created and the previous forecast for the start_time is copied over. During restart the initial run before the loop is skipped, with the info being populated from the previous run. The function then dives right into the first Analysis, then continues on like normal.
#' 
#' @description State Variable Data Assimilation: Ensemble Kalman Filter and Generalized ensemble filter. Check out SDA_control function for more details on the control arguments.
#' 
#' @return NONE
#' @import nimble furrr
#' @export
#' 
sda.enkf.multisite <- function(settings, 
                               obs.mean, 
                               obs.cov, 
                               Q = NULL, 
                               restart = NULL, 
                               forceRun = TRUE, 
                               keepNC = TRUE,
                               control=list(trace = TRUE,
                                            FF = FALSE,
                                            interactivePlot = FALSE,
                                            TimeseriesPlot = FALSE,
                                            BiasPlot = FALSE,
                                            plot.title = NULL,
                                            facet.plots = FALSE,
                                            debug = FALSE,
                                            pause = FALSE,
                                            Profiling = FALSE,
                                            OutlierDetection=FALSE),
                               ...) {
  #add if/else for when restart points to folder instead if T/F set restart as T
  if(is.list(restart)){
    old.dir <- restart$filepath
    start.cut <- restart$start.cut
    restart_flag = TRUE
  }else{
    restart_flag = FALSE
  }
  future::plan(multiprocess)
  if (control$debug) browser()
  tictoc::tic("Prepration")
  ###-------------------------------------------------------------------###
  ### read settings                                                     ###
  ###-------------------------------------------------------------------###
  adjustment <- settings$state.data.assimilation$adjustment
  model      <- settings$model$type
  write      <- settings$database$bety$write
  defaults   <- settings$pfts
  outdir     <- settings$modeloutdir # currently model runs locally, this will change if remote is enabled
  rundir     <- settings$host$rundir
  host       <- settings$host
  
  
  forecast.time.step <- settings$state.data.assimilation$forecast.time.step  #idea for later generalizing
  nens       <- as.numeric(settings$ensemble$size)
  processvar <- settings$state.data.assimilation$process.variance
  if(processvar=="TRUE"){
    processvar <- TRUE
  }else{
    processvar <- FALSE
  }
  Localization.FUN <- settings$state.data.assimilation$Localization.FUN # localization function
  scalef <- settings$state.data.assimilation$scalef %>% as.numeric() # scale factor for localization
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  names(var.names) <- NULL
  multi.site.flag <- PEcAn.settings::is.MultiSettings(settings)
  readsFF <- NULL # this keeps the forward forecast
  
  is.local <- PEcAn.remote::is.localhost(settings$host)
  #------------------Reading up the MCMC settings
  nitr.GEF <- ifelse(is.null(settings$state.data.assimilation$nitrGEF), 1e6, settings$state.data.assimilation$nitrGEF %>%as.numeric)
  nthin <- ifelse(is.null(settings$state.data.assimilation$nthin), 100, settings$state.data.assimilation$nthin %>%as.numeric)
  nburnin<- ifelse(is.null(settings$state.data.assimilation$nburnin), 1e4, settings$state.data.assimilation$nburnin %>%as.numeric)
  censored.data<-ifelse(is.null(settings$state.data.assimilation$censored.data), TRUE, settings$state.data.assimilation$censored.data %>% as.logical)
  #--------Initialization
  FORECAST    <- ANALYSIS <- list()
  enkf.params <- list()
  restart.list <- NULL
  #create SDA folder to store output
  if(!dir.exists("SDA")) dir.create("SDA",showWarnings = FALSE)
  
  ##### Creating matrices that describe the bounds of the state variables
  ##### interval is remade everytime depending on the data at time t
  ##### state.interval stays constant and converts new.analysis to be within the correct bounds
  interval    <- NULL
  state.interval <- cbind(as.numeric(lapply(settings$state.data.assimilation$state.variables,'[[','min_value')),
                          as.numeric(lapply(settings$state.data.assimilation$state.variables,'[[','max_value')))
  rownames(state.interval) <- var.names
  #------------------------------Multi - site specific - settings
  #Here I'm trying to make a temp config list name and put it into map to iterate
  if(multi.site.flag){
    conf.settings<-settings
    site.ids <- conf.settings %>% map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()
    # a matrix ready to be sent to spDistsN1 in sp package - first col is the long second is the lat and row names are the site ids
    site.locs <- conf.settings %>% map(~.x[['run']] ) %>% map('site') %>% map_dfr(~c(.x[['lon']],.x[['lat']]) %>%as.numeric)%>% 
      t %>%
      `colnames<-`(c("Lon","Lat")) %>%
      `rownames<-`(site.ids)
    #Finding the distance between the sites
    distances <- sp::spDists(site.locs, longlat = TRUE)
    #turn that into a blocked matrix format
    blocked.dis <- block_matrix(distances %>% as.numeric(), rep(length(var.names), length(site.ids)))
    
  }else{
    conf.settings <- list(settings)
    site.ids <- as.character(settings$run$site$id)
  }
  
  
  ###-------------------------------------------------------------------###
  ### check dates before data assimilation                              ###
  ###-------------------------------------------------------------------###----  
  #filtering obs data based on years specifited in setting > state.data.assimilation
  if (restart_flag) {
    start.cut <- lubridate::ymd_hms(start.cut) #start.cut taken from restart list as date to begin runs
    Start.year <-lubridate::year(start.cut)
    
  }else{
    start.cut <- lubridate::ymd_hms(settings$state.data.assimilation$start.date, truncated = 3)
    Start.year <- (lubridate::year(settings$state.data.assimilation$start.date))
  }
  
  End.year <- lubridate::year(settings$state.data.assimilation$end.date) # dates that assimilations will be done for - obs will be subsetted based on this
  assim.sda <- Start.year:End.year
  obs.mean <- obs.mean[sapply(lubridate::year(names(obs.mean)), function(obs.year) obs.year %in% (assim.sda))] #checks obs.mean dates against assimyear dates
  obs.cov <- obs.cov[sapply(lubridate::year(names(obs.cov)), function(obs.year) obs.year %in% (assim.sda))] #checks obs.cov dates against assimyear dates
  #checking that there are dates in obs.mean and adding midnight as the time
  obs.times <- names(obs.mean)
  obs.times.POSIX <- lubridate::ymd_hms(obs.times)
  for (i in seq_along(obs.times)) {
    if (is.na(obs.times.POSIX[i])) {
      if (is.na(lubridate::ymd(obs.times[i]))) {
        PEcAn.logger::logger.warn("Error: no dates associated with observations")
      } else {
        ### Data does not have time associated with dates 
        ### Adding 12:59:59PM assuming next time step starts one second later
        PEcAn.logger::logger.warn("Pumpkin Warning: adding one minute before midnight time assumption to dates associated with data")
        obs.times.POSIX[i] <- lubridate::ymd_hms(paste(obs.times[i], "23:59:59"))
      }
    }
  }
  obs.times <- obs.times.POSIX
  read_restart_times <- c(lubridate::ymd_hms(start.cut, truncated = 3), obs.times)
  nt  <- length(obs.times) #sets length of for loop for Forecast/Analysis
  if (nt==0) PEcAn.logger::logger.severe('There has to be at least one Obs.')

# Model Specific Setup ----------------------------------------------------

  #--get model specific functions
  my.write_restart <- paste0("PEcAn.", model, "::write_restart.", model)
  my.read_restart <- paste0("PEcAn.", model, "::read_restart.", model)
  my.split_inputs  <- paste0("PEcAn.", model, "::split_inputs.", model)
  #- Double checking some of the inputs
  if (is.null(adjustment)) adjustment <- TRUE
  # models that don't need split_inputs, check register file for that
  register.xml <- system.file(paste0("register.", model, ".xml"), package = paste0("PEcAn.", model))
  register <- XML::xmlToList(XML::xmlParse(register.xml))
  no_split <- !as.logical(register$exact.dates)
  
  if (!exists(my.split_inputs)  &  !no_split) {
    PEcAn.logger::logger.warn(my.split_inputs, "does not exist")
    PEcAn.logger::logger.severe("please make sure that the PEcAn interface is loaded for", model)
    PEcAn.logger::logger.warn(my.split_inputs, "If your model does not need the split function you can specify that in register.Model.xml in model's inst folder by adding <exact.dates>FALSE</exact.dates> tag.")
    
  }
  #split met if model calls for it
  #create a folder to store extracted met files
  if(!file.exists(paste0(settings$outdir, "/Extracted_met/"))){
    dir.create(paste0(settings$outdir, "/Extracted_met/"))
  }
  
  conf.settings<-conf.settings %>%
    `class<-`(c("list")) %>% #until here, it separates all the settings for all sites that listed in the xml file
    furrr::future_map(function(settings) {
      library(paste0("PEcAn.",settings$model$type), character.only = TRUE)#solved by including the model in the settings
      # source('/projectnb/dietzelab/dongchen/dongchenpecan/models/sipnet/R/split_inputs.SIPNET.R')
      inputs.split <- list()
      if (!no_split) {
        for (i in 1:length(settings$run$inputs$met$path)) {
          #---------------- model specific split inputs
          ### model specific split inputs
          settings$run$inputs$met$path[[i]] <- do.call(
            my.split_inputs,
            args = list(
              settings = settings,
              start.time = settings$run$site$met.start, # This depends if we are restart or not
              stop.time = lubridate::ymd_hms(settings$run$site$met.end, truncated = 3),
              inputs =  settings$run$inputs$met$path[[i]],
              outpath = paste0(paste0(settings$outdir, "/Extracted_met/"), settings$run$site$id),
              overwrite =F
            )
          )
          # changing the start and end date which will be used for model2netcdf.model
          settings$run$start.date <- lubridate::ymd_hms(settings$state.data.assimilation$start.date, truncated = 3)
          settings$run$end.date <- lubridate::ymd_hms(settings$state.data.assimilation$end.date, truncated = 3)
          
        }
      } else{
        inputs.split <- inputs
      }
      settings
    })
  conf.settings<- PEcAn.settings::as.MultiSettings(conf.settings)
  ###-------------------------------------------------------------------###
  ### If this is a restart - Picking up were we left last time          ###
  ###-------------------------------------------------------------------###----   
  if (restart_flag){
    #TO DO grab soil files
    #add else for when sda.out.Rdata is missing
    if(file.exists(file.path(old.dir,"sda.output.Rdata"))){
      load(file.path(old.dir,"sda.output.Rdata"))
      # #this is where the old simulation will be moved to
      # old.sda <- lubridate::year(names(FORECAST)) %>% tail(1)
      # #--- Updating the nt and etc
      # if(!dir.exists(file.path(old.dir,"SDA",old.sda))) dir.create(file.path(old.dir,"SDA",old.sda))
      # # finding/moving files to it's end year dir
      # files.last.sda <- list.files(file.path(old.dir,"out"))
      # #copying
      # file.copy(file.path(file.path(old.dir,"out"),files.last.sda),
      #           file.path(file.path(settings$outdir,"SDA"),paste0(old.sda,"/",files.last.sda))
      # )
      #sim.time <-2:nt # if It's restart I added +1 from the start to nt (which is the last year of old sim) to make the first sim in restart time t=2
      #new.params and params.list are already loaded in the environment only need to grab X
      X <-FORECAST[[length(FORECAST)]]
    }else{
      PEcAn.logger::logger.info("The SDA output from the older simulation doesn't exist, assuming first SDA run with unconstrainded forecast output")
      #loading param info from previous forecast
      load(file.path(old.dir, "samples.Rdata"))
      #assuming that will only use previous unconstrained forecast runs for first run with SDA which means we are at t=1
      #sim.time<-seq_len(nt)
      #create params object using previous forecast ensemble members
      new.params <- sda_matchparam(settings, ensemble.samples, site.ids, nens)
      
      #create inputs object for met using previous forecast ensemble members
      ####add function here, pause on this feature until we add feature to model runs that saves driver ensemble members
      
      #build X using previous forecast output
      #out.configs object required to build X and restart.list object required for build X
      restart.list <- vector("list", length(conf.settings))
      out.configs <- conf.settings %>%
        `class<-`(c("list")) %>%
        furrr::future_map2(restart.list, function(settings, restart.arg) {
          # Loading the model package - this is required bc of the furrr
          library(paste0("PEcAn.",settings$model$type), character.only = TRUE)
          # wrtting configs for each settings - this does not make a difference with the old code
          write.ensemble.configs(
            defaults = settings$pfts,
            ensemble.samples = ensemble.samples,
            settings = settings,
            model = settings$model$type,
            write.to.db = settings$database$bety$write,
            restart = restart.arg
          )
        }) %>%
        setNames(site.ids)
      #now all build_X args are properly formatted for the function to return X
      reads <- build_X(out.configs = out.configs, settings = settings, new.params = new.params, nens = nens, read_restart_times = read_restart_times, outdir = paste0(old.dir, "out/"), t = 1, var.names = var.names, my.read_restart = my.read_restart)
      #let's read the parameters of each site/ens
      params.list <- reads %>% map(~.x %>% map("params"))
      # Now let's read the state variables of site/ens
      X <- reads %>% map(~.x %>% map_df(~.x[["X"]] %>% t %>% as.data.frame))
      
      # Now we have a matrix that columns are state variables and rows are ensembles.
      # this matrix looks like this
      #         GWBI    AbvGrndWood   GWBI    AbvGrndWood
      #[1,]  3.872521     37.2581  3.872521     37.2581
      # But therer is an attribute called `Site` which tells yout what column is for what site id - check out attr (X,"Site")
      if (multi.site.flag){
        X <- X %>%
          map_dfc(~.x) %>% 
          as.matrix() %>%
          `colnames<-`(c(rep(var.names, length(X)))) %>%
          `attr<-`('Site',c(rep(site.ids, each=length(var.names))))
      }
    }
  }
  
  ###-------------------------------------------------------------------###
  ### set up for data assimilation                                      ###
  ###-------------------------------------------------------------------###----
  # weight matrix
  wt.mat <- matrix(NA, nrow = nens, ncol = nt)
  # Reading param samples------------------------------- 
    #create params object using samples generated from TRAITS functions
    if(restart_flag){
      new.params <- new.params
    }else{
      if(!file.exists(file.path(settings$outdir, "samples.Rdata"))) PEcAn.logger::logger.severe("samples.Rdata cannot be found. Make sure you generate samples by running the get.parameter.samples function before running SDA.")
      #Generate parameter needs to be run before this to generate the samples. This is hopefully done in the main workflow.
      load(file.path(settings$outdir, "samples.Rdata"))  ## loads ensemble.samples
      new.params <- sda_matchparam(settings, ensemble.samples, site.ids, nens) 
    }
    #sample met ensemble members
    inputs <- conf.settings %>% map(function(setting) {
      input.ens.gen(
        settings = setting,
        input = "met",
        method = setting$ensemble$samplingspace$met$method,
        parent_ids = NULL 
      )
    }) 
  
  
  ###------------------------------------------------------------------------------------------------###
  ### loop over time                                                                                 ###
  ###------------------------------------------------------------------------------------------------###
  for(t in 1:nt){
      obs <- obs.mean[[t]]
      obs.check <- obs[[1]]
      obs.t<-names(obs.mean)[t]
      obs.year <- lubridate::year(obs.t)
      
      ###-------------------------------------------------------------------------###
      ###  Taking care of Forecast. Splitting /  Writting / running / reading back###
      ###-------------------------------------------------------------------------###-----  
      #- Check to see if this is the first run or not and what inputs needs to be sent to write.ensemble configs
      if (t>1){
        #removing old simulations
        #list.files(outdir, "*.nc", recursive = T, full.names = T) %>%
        #furrr::future_map(~ unlink(.x))
        
        #for next time step split the met if model requires
        inputs.split <- metSplit(conf.settings, inputs, settings, model, no_split, obs.times, t, nens, restart_flag, my.split_inputs)
        
        ##browser()
        #---------------- setting up the restart argument for each site separatly and keeping them in a list
        restart.list <-
          furrr::future_pmap(list(out.configs, conf.settings %>% `class<-`(c("list")), params.list, inputs.split),
                             function(configs, settings, new.params, inputs) {
                               list(
                                 runid = configs$runs$id,
                                 start.time = strptime(obs.times[t -1], format = "%Y-%m-%d %H:%M:%S") + lubridate::second(lubridate::hms("00:00:01")),
                                 stop.time = strptime(obs.times[t], format ="%Y-%m-%d %H:%M:%S"),
                                 settings = settings,
                                 new.state = new.state[, which(attr(X, "Site") %in% settings$run$site$id)],
                                 #!!!!!!!!!!
                                 new.params = new.params,
                                 inputs = inputs,
                                 RENAME = TRUE,
                                 ensemble.id = settings$ensemble$ensemble.id
                               )
                             })
      } else {
        restart.list <- vector("list", length(conf.settings))
      }
      #add flag for restart t=1 to skip model runs
      if(restart_flag & t == 1){
        #for restart when t=1 do not need to do model runs and X should already exist in environment by this point
        X <- X
      }else{
        
        if (control$debug) browser()
        #if restart then use restart.list, include site for debugging purposes
        if(restart_flag){
          restart.arg = restart.list$`646`
        }else{
          restart.arg = NULL
        }
        out.configs <- conf.settings %>%
          `class<-`(c("list")) %>%
          furrr::future_map2(restart.list, function(settings, restart.arg = restart.arg) {
            # Loading the model package - this is required bc of the furrr
            library(paste0("PEcAn.",settings$model$type), character.only = TRUE)
            # wrtting configs for each settings - this does not make a difference with the old code
            write.ensemble.configs(
              defaults = settings$pfts,
              ensemble.samples = ensemble.samples,
              settings = settings,
              model = settings$model$type,
              write.to.db = settings$database$bety$write,
              restart = restart.arg
            )
          }) %>%
          setNames(site.ids)
        
        #I'm rewrting the runs because when I use the parallel appraoch for wrting configs the run.txt will get messed up; because multiple cores want to write on it at the same time.
        runs.tmp <- list.dirs(rundir, full.names = F)
        writeLines(runs.tmp[runs.tmp != ''], file.path(rundir, 'runs.txt'))
        
        PEcAn.workflow::start_model_runs(settings, write=settings$database$bety$write)
        
        #------------- Reading - every iteration and for SDA
        
        #put building of X into a function that gets called
        reads <- build_X(out.configs = out.configs, settings = settings, new.params = new.params, nens = nens, read_restart_times = read_restart_times, outdir = outdir, t = t, var.names = var.names, my.read_restart = my.read_restart)
        
        if (control$debug) browser()
        #let's read the parameters of each site/ens
        params.list <- reads %>% map(~.x %>% map("params"))
        # Now let's read the state variables of site/ens
        #don't need to build X when t=1
        X <- reads %>% map(~.x %>% map_df(~.x[["X"]] %>% t %>% as.data.frame))
        
        
        #replacing crazy outliers before it's too late
        if (control$OutlierDetection) X <- outlier.detector.boxplot(X)
        
        # Now we have a matrix that columns are state variables and rows are ensembles.
        # this matrix looks like this
        #         GWBI    AbvGrndWood   GWBI    AbvGrndWood
        #[1,]  3.872521     37.2581  3.872521     37.2581
        # But therer is an attribute called `Site` which tells yout what column is for what site id - check out attr (X,"Site")
        if (multi.site.flag)
          X <- X %>%
          map_dfc(~.x) %>% 
          as.matrix() %>%
          `colnames<-`(c(rep(var.names, length(X)))) %>%
          `attr<-`('Site',c(rep(site.ids, each=length(var.names))))
        
      }  ## end else from restart & t==1
        
        
      FORECAST[[obs.t]] <- X
      
      
      ###-------------------------------------------------------------------###
      ###  preparing OBS                                                    ###
      ###-------------------------------------------------------------------###---- 
      if (!is.na(obs.check)) {
        if (control$debug) browser()
        #Making R and Y
        Obs.cons <- Construct.R(site.ids, var.names, obs.mean[[t]], obs.cov[[t]])
        
        Y <- Obs.cons$Y
        R <- Obs.cons$R
        
        if (length(Y) > 1) {
          PEcAn.logger::logger.info("The zero variances in R and Pf is being replaced by half and one fifth of the minimum variance in those matrices respectively.")
          diag(R)[which(diag(R)==0)] <- min(diag(R)[which(diag(R) != 0)])/2
        }
        # making the mapping operator
        H <- Construct.H.multisite(site.ids, var.names, obs.mean[[t]]) #works for only 1 site 
        
        aqq         <- NULL
        bqq         <- numeric(nt + 1)
        #if t>1
        if (processvar && t > 1) {
          aqq <- enkf.params[[t-1]]$aqq
          bqq <- enkf.params[[t-1]]$bqq
          X.new<-enkf.params[[t-1]]$X.new
        }
        if(!exists('Cmcmc_tobit2space')) {
          recompileTobit = TRUE
        }else{
          recompileTobit = FALSE
        }
        
        if(!exists('Cmcmc')) {
          recompileGEF = TRUE
        }else{
          recompileGEF = FALSE
        }
        #weight list
        # This reads ensemble weights generated by `get_ensemble_weights` function from assim.sequential package
        weight_list <- list()
        if(!file.exists(file.path(settings$outdir, "ensemble_weights.Rdata"))){
          PEcAn.logger::logger.warn("ensemble_weights.Rdata cannot be found. Make sure you generate samples by running the get.ensemble.weights function before running SDA if you want the ensembles to be weighted.")
          #create null list
          for(tt in 1:length(obs.times)){
            weight_list[[tt]] <- rep(1,nens) #no weights
          }
        } else{
          load(file.path(settings$outdir, "ensemble_weights.Rdata"))  ## loads ensemble.samples
        }
        wts <- unlist(weight_list[[t]])
        ###-------------------------------------------------------------------###
        ### Analysis                                                          ###
        ###-------------------------------------------------------------------###----
        if(processvar == FALSE){an.method<-EnKF  }else{    an.method<-GEF.MultiSite   }
        
        #-analysis function

        
        enkf.params[[obs.t]] <- GEF.MultiSite(
          settings,
          FUN = an.method,
          Forecast = list(Q = Q, X = X),
          Observed = list(R = R, Y = Y),
          H = H,
          extraArg = list(
            aqq = aqq,
            bqq = bqq,
            t = t,
            nitr.GEF = nitr.GEF,
            nthin = nthin,
            nburnin = nburnin,
            censored.data = censored.data,
            recompileGEF = recompileGEF,
            recompileTobit = recompileTobit,
            wts = wts
          ),
          choose = choose,
          nt = nt,
          obs.mean = obs.mean,
          nitr = 100000,
          nburnin = 10000,
          
          obs.cov = obs.cov,
          site.ids = site.ids,
          blocked.dis = blocked.dis,
          distances = distances
        )
        tictoc::tic(paste0("Preparing for Adjustment for cycle = ", t))
        #Forecast
        mu.f <- enkf.params[[obs.t]]$mu.f
        Pf <- enkf.params[[obs.t]]$Pf
        #Analysis
        Pa <- enkf.params[[obs.t]]$Pa
        mu.a <- enkf.params[[obs.t]]$mu.a
        #extracting extra outputs
        if (control$debug) browser()
        if (processvar) {
          aqq <- enkf.params[[obs.t]]$aqq
          bqq <- enkf.params[[obs.t]]$bqq
        }
        # Adding obs elements to the enkf.params
        #This can later on help with diagnostics
        enkf.params[[obs.t]] <-
          c(
            enkf.params[[obs.t]],
            R = list(R),
            Y = list(Y),
            RestartList = list(restart.list %>% setNames(site.ids))
          )
        
        ###-------------------------------------------------------------------###
        ### Trace                                                             ###
        ###-------------------------------------------------------------------###----      
        #-- writing Trace--------------------
        if(control$trace) {
          PEcAn.logger::logger.warn ("\n --------------------------- ",obs.year," ---------------------------\n")
          PEcAn.logger::logger.warn ("\n --------------Obs mean----------- \n")
          print(Y)
          PEcAn.logger::logger.warn ("\n --------------Obs Cov ----------- \n")
          print(R)
          PEcAn.logger::logger.warn ("\n --------------Obs H ----------- \n")
          print(H)
          PEcAn.logger::logger.warn ("\n --------------Forecast mean ----------- \n")
          print(enkf.params[[obs.t]]$mu.f)
          PEcAn.logger::logger.warn ("\n --------------Forecast Cov ----------- \n")
          print(enkf.params[[obs.t]]$Pf)
          PEcAn.logger::logger.warn ("\n --------------Analysis mean ----------- \n")
          print(t(enkf.params[[obs.t]]$mu.a))
          PEcAn.logger::logger.warn ("\n --------------Analysis Cov ----------- \n")
          print(enkf.params[[obs.t]]$Pa)
          PEcAn.logger::logger.warn ("\n ------------------------------------------------------\n")
        }
        if (control$debug) browser()
        if (control$pause) readline(prompt="Press [enter] to continue \n")
      } else {
        ###-------------------------------------------------------------------###
        ### No Observations --                                                ###----
        ###-----------------------------------------------------------------### 
        ### no process variance -- forecast is the same as the analysis ###
        if (processvar==FALSE) {
          mu.a <- mu.f
          Pa   <- Pf + Q
          ### yes process variance -- no data
        } else {
          mu.f <- colMeans(X) #mean Forecast - This is used as an initial condition
          mu.a <- mu.f
          if(is.null(Q)){
            q.bar <- diag(ncol(X))
            PEcAn.logger::logger.warn('Process variance not estimated. Analysis has been given uninformative process variance')
          }
          # Pa   <- Pf + matrix(solve(q.bar), dim(Pf)[1], dim(Pf)[2])
          #will throw an error when q.bar and Pf are different sizes i.e. when you are running with no obs and do not variance for all state variables
          #Pa <- Pf + solve(q.bar)
          #hack have Pa = Pf for now
          Pf = stats::cov(X) # Cov Forecast - This is used as an initial condition
          Pa <- Pf
        }
        enkf.params[[obs.t]] <- list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa)
        
      }
      
      ###-------------------------------------------------------------------###
      ### adjustement/update state matrix                                   ###
      ###-------------------------------------------------------------------###---- 
      tictoc::tic(paste0("Adjustment for cycle = ", t))
      if(adjustment == TRUE){
        analysis <-adj.ens(Pf, X, mu.f, mu.a, Pa)
      } else {
        analysis <- as.data.frame(mvtnorm::rmvnorm(as.numeric(nrow(X)), mu.a, Pa, method = "svd"))
      }
      analysis[analysis<0] <- 0
      colnames(analysis) <- colnames(X)
      ##### Mapping analysis vectors to be in bounds of state variables
      if(processvar==TRUE){
        for(i in 1:ncol(analysis)){
          int.save <- state.interval[which(startsWith(colnames(analysis)[i],
                                                      var.names)),]
          analysis[analysis[,i] < int.save[1],i] <- int.save[1]
          analysis[analysis[,i] > int.save[2],i] <- int.save[2]
        }
      }
      ## in the future will have to be separated from analysis
      
      new.state  <- as.data.frame(analysis)
      ANALYSIS[[obs.t]] <- analysis
      ANALYSIS <-ANALYSIS
      ###-------------------------------------------------------------------###
      ### save outputs                                                      ###
      ###-------------------------------------------------------------------###---- 
      Viz.output <- list(settings, obs.mean, obs.cov) #keeping obs data and settings for later visualization in Dashboard
      
      save(site.locs,
           t,
           FORECAST,
           ANALYSIS,
           enkf.params,
           new.state, new.params,params.list,
           out.configs, ensemble.samples, inputs, Viz.output,
           file = file.path(settings$outdir, "sda.output.Rdata"))
      
      tictoc::tic(paste0("Visulization for cycle = ", t))
      
      #writing down the image - either you asked for it or nor :)
      try(post.analysis.multisite.ggplot(settings, t, obs.times, obs.mean, obs.cov, FORECAST, ANALYSIS, plot.title = "test"))
      if ((t%%2==0 | t==nt) & (control$TimeseriesPlot))   post.analysis.multisite.ggplot(settings, t, obs.times, obs.mean, obs.cov, FORECAST, ANALYSIS ,plot.title=control$plot.title, facetg=control$facet.plots, readsFF=readsFF)
      #Saving the profiling result
      if (control$Profiling) alltocs(file.path(settings$outdir,"SDA", "Profiling.csv"))
      
    # remove files as SDA runs
    if (!(keepNC))
    {
      unlink(list.files(outdir, "*.nc", recursive = TRUE, full.names = TRUE))
    } 
    # useful for debugging to keep .nc files for assimilated years. T = 2, because this loops removes the files that were run when starting the next loop
    if (keepNC && t == 1)
    {
      unlink(list.files(outdir, "*.nc", recursive = TRUE, full.names = TRUE))
    }
    
  } ### end loop over time
  
} # sda.enkf