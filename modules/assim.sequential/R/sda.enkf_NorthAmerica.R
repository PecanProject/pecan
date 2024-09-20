#' @title sda.enkf_NorthAmerica
#' @name  sda.enkf_NorthAmerica
#' @author Michael Dietze, Ann Raiho and Alexis Helgeson \email{dietze@@bu.edu}
#' 
#' @param settings  PEcAn settings object
#' @param obs.mean  Lists of date times named by time points, which contains lists of sites named by site ids, which contains observation means for each state variables of each site for each time point. 
#' @param obs.cov   Lists of date times named by time points, which contains lists of sites named by site ids, which contains observation covariances for all state variables of each site for each time point. 
#' @param Q         Process covariance matrix given if there is no data to estimate it.
#' @param pre_enkf_params Used for passing pre-existing time-series of process error into the current SDA runs to ignore the impact by the differences between process errors.
#' @param ensemble.samples Pass ensemble.samples from outside to avoid GitHub check issues.
#' @param control   List of flags controlling the behavior of the SDA. 
#' `TimeseriesPlot` for post analysis examination; 
#' `OutlierDetection` decide if we want to execute the outlier detection each time after the model forecasting;
#' `send_email` contains lists for sending email to report the SDA progress;
#' `keepNC` decide if we want to keep the NetCDF files inside the out directory;
#' `forceRun` decide if we want to proceed the Bayesian MCMC sampling without observations;
#' `MCMC.args` include lists for controling the MCMC sampling process (iteration, nchains, burnin, and nthin.).
#'
#’ @details
#’ Restart mode:  Basic idea is that during a restart (primary case envisioned as an iterative forecast), a new workflow folder is created and the previous forecast for the start_time is copied over. During restart the initial run before the loop is skipped, with the info being populated from the previous run. The function then dives right into the first Analysis, then continues on like normal.
#' 
#' @description State Variable Data Assimilation: Ensemble Kalman Filter and Generalized ensemble filter. Check out SDA_control function for more details on the control arguments.
#' 
#' @return NONE
#' @import nimble
#' @export
#' 
sda.enkf_NorthAmerica <- function(settings, 
                                  obs.mean, 
                                  obs.cov, 
                                  Q = NULL, 
                                  pre_enkf_params = NULL,
                                  ensemble.samples = NULL,
                                  control=list(TimeseriesPlot = FALSE,
                                               OutlierDetection=FALSE,
                                               send_email = NULL,
                                               keepNC = TRUE,
                                               forceRun = TRUE,
                                               MCMC.args = NULL)) {
  # foreach.
  cores <- parallel::detectCores()
  cl <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)
  #progress bar
  pb <- utils::txtProgressBar(min=1, max=length(settings), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  ###-------------------------------------------------------------------###
  ### read settings                                                     ###
  ###-------------------------------------------------------------------###
  adjustment <- settings$state.data.assimilation$adjustment
  model      <- settings$model$type
  defaults   <- settings$pfts
  outdir     <- settings$modeloutdir # currently model runs locally, this will change if remote is enabled
  rundir     <- settings$host$rundir
  nens       <- as.numeric(settings$ensemble$size)
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  names(var.names) <- NULL
  #--------Initialization
  restart.list <- NULL
  #create SDA folder to store output
  if(!dir.exists(settings$outdir)) dir.create(settings$outdir, showWarnings = FALSE)
  
  ##### Creating matrices that describe the bounds of the state variables
  ##### interval is remade everytime depending on the data at time t
  ##### state.interval stays constant and converts new.analysis to be within the correct bounds
  interval    <- NULL
  state.interval <- cbind(as.numeric(lapply(settings$state.data.assimilation$state.variables,'[[','min_value')),
                          as.numeric(lapply(settings$state.data.assimilation$state.variables,'[[','max_value')))
  rownames(state.interval) <- var.names
  #------------------------------Multi - site specific - settings
  #Here I'm trying to make a temp config list name and put it into map to iterate
  conf.settings <- settings
  site.ids <- conf.settings$run %>% purrr::map('site') %>% purrr::map('id') %>% base::unlist() %>% base::as.character()
  # a matrix ready to be sent to spDistsN1 in sp package - first col is the long second is the lat and row names are the site ids
  site.locs <- conf.settings$run %>% purrr::map('site') %>% purrr::map_dfr(~c(.x[['lon']],.x[['lat']]) %>% as.numeric)%>% 
    t %>%
    `colnames<-`(c("Lon","Lat")) %>%
    `rownames<-`(site.ids)
  ###-------------------------------------------------------------------###
  ### check dates before data assimilation                              ###
  ###-------------------------------------------------------------------###----  
  #filtering obs data based on years specifited in setting > state.data.assimilation
  start.cut <- lubridate::ymd_hms(settings$state.data.assimilation$start.date, truncated = 3)
  Start.year <- (lubridate::year(settings$state.data.assimilation$start.date))
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
  #split met if model calls for it
  #create a folder to store extracted met files
  if(!file.exists(paste0(settings$outdir, "/Extracted_met/"))){
    dir.create(paste0(settings$outdir, "/Extracted_met/"))
  }
  
  PEcAn.logger::logger.info("Splitting mets!")
  conf.settings.before.split <- conf.settings %>%
    `class<-`(c("list"))#until here, it separates all the settings for all sites that listed in the xml file
  conf.settings <-
    foreach::foreach(settings = conf.settings.before.split, 
                     .packages=c("Kendall", "PEcAn.SIPNET", "purrr"), 
                     .options.snow=opts) %dopar% {
                       inputs.split <- list()
                       if (!no_split) {
                         for (i in 1:length(settings$run$inputs$met$path)) {
                           #---------------- model specific split inputs
                           ### model specific split inputs
                           settings$run$inputs$met$path[[i]] <- do.call(
                             my.split_inputs,
                             args = list(
                               settings = settings,
                               start.time = lubridate::ymd_hms(settings$run$site$met.start, truncated = 3), # This depends if we are restart or not
                               stop.time = lubridate::ymd_hms(settings$run$site$met.end, truncated = 3),
                               inputs =  settings$run$inputs$met$path[[i]],
                               outpath = paste0(paste0(settings$outdir, "/Extracted_met/"), settings$run$site$id),
                               overwrite = F
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
                     }
  conf.settings<- PEcAn.settings::as.MultiSettings(conf.settings)
  ###-------------------------------------------------------------------###
  ### set up for data assimilation                                      ###
  ###-------------------------------------------------------------------###----
  # Reading param samples------------------------------- 
  #create params object using samples generated from TRAITS functions
  load(file.path(settings$outdir, "samples.Rdata"))
  #reformatting params
  new.params <- sda_matchparam(settings, ensemble.samples, site.ids, nens)
  #sample met ensemble members
  #TODO: incorporate Phyllis's restart work
  #sample all inputs specified in the settings$ensemble not just met
  inputs <- PEcAn.settings::papply(conf.settings,function(setting) {
    PEcAn.uncertainty::input.ens.gen(
      settings = setting,
      input = "met",
      method = setting$ensemble$samplingspace$met$method,
      parent_ids = NULL 
    )
  })
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  ###------------------------------------------------------------------------------------------------###
  ### loop over time                                                                                 ###
  ###------------------------------------------------------------------------------------------------###
  for(t in 1:nt){
    # initialize dat for saving memory usage.
    sda.outputs <- FORECAST <- enkf.params <- ANALYSIS <- ens_weights <- list()
    ############################################################
    # foreach.
    cores <- parallel::detectCores()-1
    cl <- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)
    #progress bar
    pb <- utils::txtProgressBar(min=1, max=length(settings), style=3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress=progress)
    ############################################################
    obs.t <- as.character(lubridate::date(obs.times[t]))
    obs.year <- lubridate::year(obs.t)
    ###-------------------------------------------------------------------------###
    ###  Taking care of Forecast. Splitting /  Writting / running / reading back###
    ###-------------------------------------------------------------------------###-----  
    #- Check to see if this is the first run or not and what inputs needs to be sent to write.ensemble configs
    if (t>1){
      #for next time step split the met if model requires
      #-Splitting the input for the models that they don't care about the start and end time of simulations and they run as long as their met file.
      conf.settings.before.split <- conf.settings %>% `class<-`(c("list"))
      inputs.split <- parallel.split.met(settings,
                                         my.split_inputs, 
                                         conf.settings.before.split, 
                                         (lubridate::ymd_hms(obs.times[t - 1], truncated = 3) + 
                                            lubridate::second(lubridate::hms("00:00:01"))), 
                                         lubridate::ymd_hms(obs.times[t], truncated = 3), 
                                         inputs)
      #---------------- setting up the restart argument for each site separately and keeping them in a list
      restart.list <-
        foreach::foreach(i = seq_along(conf.settings.before.split), 
                         .packages=c("Kendall", "PEcAn.SIPNET", "purrr", "lubridate"), 
                         .options.snow=opts) %dopar% {
                           new_state_site <- new.state[, which(attr(X, "Site") %in% conf.settings.before.split[[i]]$run$site$id)]
                           if(is.vector(new_state_site)){
                             new_state_site <- matrix(new_state_site)
                           }
                           list(
                             runid = out.configs[[i]]$runs$id,
                             start.time = strptime(obs.times[t -1], format = "%Y-%m-%d %H:%M:%S") + lubridate::second(lubridate::hms("00:00:01")),
                             stop.time = strptime(obs.times[t], format ="%Y-%m-%d %H:%M:%S"),
                             settings = conf.settings.before.split[[i]],
                             new.state = new_state_site,
                             new.params = params.list[[i]],
                             inputs = inputs.split[[i]],
                             RENAME = TRUE,
                             ensemble.id = conf.settings.before.split[[i]]$ensemble$ensemble.id
                           )
                         }
    } else { ## t == 1
      restart.list <- vector("list", length(conf.settings))
    }
    # release memory.
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
    gc()
    # initialize foreach.
    cores <- parallel::detectCores()-1
    cl <- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)
    # progress bar
    pb <- utils::txtProgressBar(min=1, max=length(settings), style=3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress=progress)
    # submit jobs for writing configs.
    out.configs <- parallel.write.configs(settings, ensemble.samples, restart.list) %>%
      purrr::set_names(site.ids)
    # collect run info.
    runs.tmp <- list.files(rundir, full.names = F)
    runs.tmp <- runs.tmp[grepl("ENS-*|[0-9]", runs.tmp)] 
    writeLines(runs.tmp[runs.tmp != ''], file.path(rundir, 'runs.txt'))
    # submit jobs for model executions.
    job.sub(settings, F, F)
    # submit jobs for reading sda outputs.
    reads <- parallel.read.sda(settings, my.read_restart, out.configs, read_restart_times[t+1], var.names, new.params)
    #let's read the parameters of each site/ens
    params.list <- reads %>% purrr::map(~.x %>% purrr::map("params"))
    # add namespace for variables inside the foreach.
    r <- NULL
    X <- foreach::foreach(r = reads, 
                          .packages=c("Kendall", "purrr"), 
                          .options.snow=opts) %dopar% {
                            r %>% purrr::map_df(~.x[["X"]] %>% t %>% as.data.frame)
                          }
    
    #replacing crazy outliers before it's too late
    if (control$OutlierDetection){
      X <- outlier.detector.boxplot(X)
      PEcAn.logger::logger.info("Outlier Detection.")
    }
    X <- foreach::foreach(i = seq_along(X), 
                          .packages=c("Kendall", "purrr"), 
                          .options.snow=opts) %dopar% {
                            temp <- do.call(cbind, X[i])
                            colnames(temp) <- paste0(var.names, ".", i)
                            return(temp)
                          } %>% 
      dplyr::bind_cols() %>%
      `colnames<-`(c(rep(var.names, length(X)))) %>%
      `attr<-`('Site',c(rep(site.ids, each=length(var.names))))
    FORECAST[[obs.t]] <- X
    # release memory.
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
    gc()
    ###-------------------------------------------------------------------###
    ###  preparing OBS                                                    ###
    ###-------------------------------------------------------------------###---- 
    #To trigger the analysis function with free run, you need to first specify the control$forceRun as TRUE,
    #Then specify the settings$state.data.assimilation$scalef as 0, and settings$state.data.assimilation$free.run as TRUE.
    if (!is.null(obs.mean[[t]][[1]]) | (as.logical(settings$state.data.assimilation$free.run) & control$forceRun)) {
      #decide if we want the block analysis function or multi-site analysis function.
      #initialize block.list.all.
      if (t == 1 | !exists("block.list.all")) {
        block.list.all <- obs.mean %>% purrr::map(function(l){NULL})
      }
      #initialize MCMC arguments.
      if (is.null(control$MCMC.args)) {
        MCMC.args <- list(niter = 1e5,
                          nthin = 10,
                          nchain = 1,
                          nburnin = 5e4)
      } else {
        MCMC.args <- control$MCMC.args
      }
      #running analysis function.
      enkf.params[[obs.t]] <- analysis_sda_block(settings, block.list.all, X, obs.mean, obs.cov, t, nt, MCMC.args, pre_enkf_params)
      enkf.params[[obs.t]] <- c(enkf.params[[obs.t]], RestartList = list(restart.list %>% stats::setNames(site.ids)))
      block.list.all <- enkf.params[[obs.t]]$block.list.all
      #Forecast
      mu.f <- enkf.params[[obs.t]]$mu.f
      Pf <- enkf.params[[obs.t]]$Pf
      #Analysis
      Pa <- enkf.params[[obs.t]]$Pa
      mu.a <- enkf.params[[obs.t]]$mu.a
    }
    ###-------------------------------------------------------------------###
    ### adjust/update state matrix                                   ###
    ###-------------------------------------------------------------------###---- 
    analysis <- enkf.params[[obs.t]]$analysis
    enkf.params[[obs.t]]$analysis <- NULL
    ##### Mapping analysis vectors to be in bounds of state variables
    for(i in 1:ncol(analysis)){
      int.save <- state.interval[which(startsWith(colnames(analysis)[i], var.names)),]
      analysis[analysis[,i] < int.save[1],i] <- int.save[1]
      analysis[analysis[,i] > int.save[2],i] <- int.save[2]
    }
    ## in the future will have to be separated from analysis
    new.state  <- as.data.frame(analysis)
    ANALYSIS[[obs.t]] <- analysis
    ens_weights[[obs.t]] <- PEcAnAssimSequential::sda_weights_site(FORECAST, ANALYSIS, 1, nens)
    ###-------------------------------------------------------------------###
    ### save outputs                                                      ###
    ###-------------------------------------------------------------------###---- 
    sda.outputs <- list(obs.mean = obs.mean[[t]],
                        obs.cov = obs.cov[[t]],
                        forecast = FORECAST[[obs.t]],
                        analysis = ANALYSIS[[obs.t]],
                        enkf.params = enkf.params[[obs.t]],
                        ens_weights[[obs.t]],
                        params.list = params.list)
    save(sda.outputs, file = file.path(settings$outdir, paste0("sda.output", t, ".Rdata")))
    
    #writing down the image - either you asked for it or nor :)
    if ((t%%2 == 0 | t == nt) & (control$TimeseriesPlot)){
      if (as.logical(settings$state.data.assimilation$free.run)) {
        SDA_timeseries_plot(ANALYSIS, FORECAST, obs.mean, obs.cov, settings$outdir, by = "var", types = c("FORECAST", "ANALYSIS"))
      } else {
        SDA_timeseries_plot(ANALYSIS, FORECAST, obs.mean, obs.cov, settings$outdir, by = "var", types = c("FORECAST", "ANALYSIS", "OBS"))
      }
    }
    
    # remove files as SDA runs
    if (!(control$keepNC) && t == 1){
      PEcAn.logger::logger.info("Deleting NC files!")
      job.sub(settings, T, T)
      PEcAn.logger::logger.info("Finished!")
    }
    if(!is.null(control$send_email)){
      sendmail <- Sys.which("sendmail")
      mailfile <- tempfile("mail")
      cat(paste0("From: ", control$send_email$from, "\n", "Subject: ", "SDA progress report", "\n", "To: ", control$send_email$to, "\n", "\n", paste("Time point:", obs.times[t], "has been completed!")), file = mailfile)
      system2(sendmail, c("-f", paste0("\"", control$send_email$from, "\""), paste0("\"", control$send_email$to, "\""), "<", mailfile))
      unlink(mailfile)
    }
  }
} # sda.enkf