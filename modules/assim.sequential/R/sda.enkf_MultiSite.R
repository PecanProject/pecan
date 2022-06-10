#' @title sda.enkf.multisite
#' @name  sda.enkf.multisite
#' @author Michael Dietze and Ann Raiho \email{dietze@@bu.edu}
#' 
#' @param settings  PEcAn settings object
#' @param obs.mean  List of dataframe of observation means, named with observation datetime.
#' @param obs.cov   List of covariance matrices of state variables , named with observation datetime.
#' @param Q         Process covariance matrix given if there is no data to estimate it.
#' @param restart   Used for iterative updating previous forecasts. When the restart is TRUE it read the object in SDA folder written from previous SDA.
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
                               restart = FALSE, 
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
  aqq         <- NULL

  #q.bar        <- NULL #default process covariance matrix
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
  }else{
    conf.settings <- list(settings)
  }
  
  #Finding the distance between the sites
  distances <- sp::spDists(site.locs, longlat = TRUE)
  #turn that into a blocked matrix format
  blocked.dis <- block_matrix(distances %>% as.numeric(), rep(length(var.names), length(site.ids)))
  
  #filtering obs data based on years specifited in setting > state.data.assimilation
  
  if (restart) {
    start.cut <- lubridate::ymd_hms(settings$state.data.assimilation$start.date, truncated = 3)-1
    Start.Year <-(lubridate::year(settings$state.data.assimilation$start.date)-1)
    
  }else{
    start.cut <- lubridate::ymd_hms(settings$state.data.assimilation$start.date, truncated = 3)
    Start.Year <- (lubridate::year(settings$state.data.assimilation$start.date))
  }
  
  End.Year <- lubridate::year(settings$state.data.assimilation$end.date) # years that assimilations will be done for - obs will be subsetted based on this
  assimyears <- Start.Year:End.Year
  obs.mean <- obs.mean[sapply(year(names(obs.mean)), function(obs.year) obs.year %in% (assimyears))]
  obs.cov <- obs.cov[sapply(year(names(obs.cov)), function(obs.year) obs.year %in% (assimyears))]
  # dir address based on the end date
  if(!dir.exists("SDA")) dir.create("SDA",showWarnings = FALSE)
  #--get model specific functions
  do.call("library", list(paste0("PEcAn.", model)))
  my.write_restart <- paste0("write_restart.", model)
  my.read_restart <- paste0("read_restart.", model)
  my.split_inputs  <- paste0("split_inputs.", model)
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
  
  ###-------------------------------------------------------------------###
  ### tests before data assimilation                                    ###
  ###-------------------------------------------------------------------###----  
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
  #obs.times <- c(start.cut, obs.times)
  nt          <- length(obs.times)
  if (nt==0) PEcAn.logger::logger.severe('There has to be at least one Obs.')
  bqq         <- numeric(nt + 1)
  

  ###-------------------------------------------------------------------###
  ### If this is a restart - Picking up were we left last time          ###
  ###-------------------------------------------------------------------###----   
  if (restart){
    
    if(!file.exists(file.path(settings$outdir,"SDA", "sda.output.Rdata"))) PEcAn.logger::logger.severe("The SDA output from the older simulation doesn't exist.")
    load(file.path(settings$outdir,"SDA", "sda.output.Rdata"))
    
    #this is where the old simulation will be moved to
    old.dir <- lubridate::year(names(FORECAST)) %>% tail(1)
    #--- Updating the nt and etc
    if(!dir.exists(file.path(settings$outdir,"SDA",old.dir))) dir.create(file.path(settings$outdir,"SDA",old.dir))
    # finding/moving files to it's end year dir
    files.last.sda <- list.files(file.path(settings$outdir,"SDA"))
    #copying
    file.copy(file.path(file.path(settings$outdir,"SDA"),files.last.sda),
              file.path(file.path(settings$outdir,"SDA"),paste0(old.dir,"/",files.last.sda))
    )
    params<-new.params
    sim.time <-2:nt # if It's restart I added +1 from the start to nt (which is the last year of old sim) to make the first sim in restart time t=2
    
    X <-FORECAST[[length(FORECAST)]]
    
  }else{
    sim.time<-seq_len(nt)
  }
  
  ### Splitting/Cutting the mets to the start and the end  of SDA       ###
  ###-------------------------------------------------------------------###---- 
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
              start.time = start.cut, # This depends if we are restart or not
              stop.time = lubridate::ymd_hms(settings$state.data.assimilation$end.date, truncated = 3),
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
  
  conf.settings<-PEcAn.settings::as.MultiSettings(conf.settings)
  
  
  ###-------------------------------------------------------------------###

  ###-------------------------------------------------------------------###
  ### set up for data assimilation                                      ###
  ###-------------------------------------------------------------------###----
  # weight matrix
  wt.mat <- matrix(NA, nrow = nens, ncol = nt)
  # Reading param samples------------------------------- 
  if(!file.exists(file.path(settings$outdir, "samples.Rdata"))) PEcAn.logger::logger.severe("samples.Rdata cannot be found. Make sure you generate samples by running the get.parameter.samples function before running SDA.")
  #Generate parameter needs to be run before this to generate the samples. This is hopefully done in the main workflow.
  load(file.path(settings$outdir, "samples.Rdata"))  ## loads ensemble.samples
  
  #reformatting params
  new.params <- list()
  all.pft.names <- names(ensemble.samples)
  for (i in 1:length(settings)) {
    #match pft name
    site.pft.name <- settings[[i]]$run$site$site.pft$pft.name
    which.pft <- which(all.pft.names==site.pft.name)
    
    site.param <- list()
    site.samples <- ensemble.samples[which.pft]
    for (j in seq_len(nens)) {
      site.param[[j]] <- lapply(site.samples, function(x, n) {
        x[j, ] }, n = j)
    } 
    new.params[[i]] <- site.param
  }
  names(new.params) <- site.ids
  
  
  # This is gonna be used just for the first round
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
  for(t in 1:nt){#sim.time
      obs <- which(!is.na(obs.mean[[t]]))
      obs.t<-names(obs.mean)[t]
      obs.year <- year(obs.t)
      
      ###-------------------------------------------------------------------------###
      ###  Taking care of Forecast. Splitting /  Writting / running / reading back###
      ###-------------------------------------------------------------------------###-----  
      #- Check to see if this is the first run or not and what inputs needs to be sent to write.ensemble configs
      if (t>1){
        #removing old simulations
        #list.files(outdir, "*.nc", recursive = T, full.names = T) %>%
        #furrr::future_map(~ unlink(.x))
        
        #-Splitting the input for the models that they don't care about the start and end time of simulations and they run as long as their met file.
        inputs.split <- conf.settings %>%
          `class<-`(c("list")) %>%
          purrr::map2(inputs, function(settings, inputs) {
            # Loading the model package - this is required bc of the furrr
            library(paste0("PEcAn.",model), character.only = TRUE)
            
            inputs.split <- list()
            if (!no_split) {
              for (i in seq_len(nens)) {
                #---------------- model specific split inputs
                inputs.split$samples[i] <- do.call(
                  my.split_inputs,
                  args = list(
                    settings = settings,
                    start.time = (lubridate::ymd_hms(obs.times[t - 1], truncated = 3) + lubridate::second(lubridate::hms("00:00:01"))),
                    stop.time =   lubridate::ymd_hms(obs.times[t], truncated = 3),
                    inputs = inputs$samples[[i]])
                )
              }
            } else{
              inputs.split <- inputs
            }
            inputs.split
          })
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
      if (control$debug) browser()
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
      
      #I'm rewrting the runs because when I use the parallel appraoch for wrting configs the run.txt will get messed up; because multiple cores want to write on it at the same time.
      runs.tmp <- list.dirs(rundir, full.names = F)
      writeLines(runs.tmp[runs.tmp != ''], file.path(rundir, 'runs.txt'))
      PEcAn.workflow::start_model_runs(settings, write=settings$database$bety$write)
      
      #------------- Reading - every iteration and for SDA
        reads <-
          furrr::future_pmap(list(out.configs %>% `class<-`(c("list")), settings, new.params),function(configs,settings,siteparams) {
            # Loading the model package - this is required bc of the furrr
            #library(paste0("PEcAn.",settings$model$type), character.only = TRUE)
            #source("~/pecan/models/sipnet/R/read_restart.SIPNET.R")
            
            X_tmp <- vector("list", 2)
            
            for (i in seq_len(nens)) {
              X_tmp[[i]] <- do.call( my.read_restart,
                                     args = list(
                                       outdir = outdir,
                                       runid = configs$runs$id[i] %>% as.character(),
                                       stop.time = read_restart_times[t+1],
                                       settings = settings,
                                       var.names = var.names,
                                       params = siteparams[[i]]
                                     )
              )
              
            }
            return(X_tmp)
          })
      
        
      if (control$debug) browser()
      #let's read the parameters of each site/ens
      params.list <- reads %>% map(~.x %>% map("params"))
      # Now let's read the state variables of site/ens
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
      
      
      
      
      FORECAST[[obs.t]] <- X
      ###-------------------------------------------------------------------###
      ###  preparing OBS                                                    ###
      ###-------------------------------------------------------------------###---- 
      if (any(obs)) {
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
        H <- Construct.H.multisite(site.ids, var.names, obs.mean[[t]])
        
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
        if (processvar=="FALSE") {
          mu.a <- mu.f
          Pa   <- Pf + Q
          ### yes process variance -- no data
        } else {
          mu.a <- mu.f
          if(is.null(q.bar)){
            q.bar <- diag(ncol(X))
            PEcAn.logger::logger.warn('Process variance not estimated. Analysis has been given uninformative process variance')
          } 
          Pa   <- Pf + solve(q.bar)
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