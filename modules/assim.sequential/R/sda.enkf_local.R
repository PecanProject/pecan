#' @description This function provides complete support for the multi-core and multi-node computation on the general HPC system.
#' Thus, this script will be more computationally efficient, making it possible to run SDA over thousands of locations.
#' @title sda.enkf_local
#' @name  sda.enkf_local
#' @author Dongchen Zhang \email{zhangdc@@bu.edu}
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
#' @return NONE
#' @export
#' 
sda.enkf_local <- function(settings, 
                           obs.mean, 
                           obs.cov, 
                           Q = NULL, 
                           pre_enkf_params = NULL,
                           ensemble.samples = NULL,
                           outdir = NULL,
                           job.folder = NULL,
                           cores = NULL,
                           control=list(TimeseriesPlot = FALSE,
                                        OutlierDetection = FALSE,
                                        send_email = NULL,
                                        keepNC = TRUE,
                                        forceRun = TRUE,
                                        MCMC.args = NULL)) {
  # initialize parallel.
  if (future::supportsMulticore()) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession)
  }
  # Tweak outdir if it's specified from outside.
  if (!is.null(outdir)) {
    settings$outdir <- outdir
    settings$rundir <- file.path(outdir, "run")
    settings$modeloutdir <- file.path(outdir, "out")
    settings$host$folder <- file.path(outdir, "out")
    settings$host$outdir <- file.path(outdir, "out")
    settings$host$rundir <- file.path(outdir, "run")
  }
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
  site.ids <- conf.settings %>% purrr::map(~.x[['run']] ) %>% purrr::map('site') %>% purrr::map('id') %>% base::unlist() %>% base::as.character()
  # a matrix ready to be sent to spDistsN1 in sp package - first col is the long second is the lat and row names are the site ids
  site.locs <- conf.settings %>% purrr::map(~.x[['run']] ) %>% 
    purrr::map('site') %>% purrr::map(function(s){
      temp <- as.numeric(c(s$lon, s$lat))
      names(temp) <- c("Lon", "Lat")
      temp
    }) %>% 
    dplyr::bind_rows() %>% 
    as.data.frame() %>%
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
        # PEcAn.logger::logger.warn("Pumpkin Warning: adding one minute before midnight time assumption to dates associated with data")
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
  conf.settings <-conf.settings %>%
    `class<-`(c("list")) %>% #until here, it separates all the settings for all sites that listed in the xml file
    furrr::future_map(function(settings) {
      library(paste0("PEcAn.",settings$model$type), character.only = TRUE)#solved by including the model in the settings
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
    }, .progress = F)
  conf.settings<- PEcAn.settings::as.MultiSettings(conf.settings)
  ###-------------------------------------------------------------------###
  ### set up for data assimilation                                      ###
  ###-------------------------------------------------------------------###----
  # Reading param samples------------------------------- 
  #create params object using samples generated from TRAITS functions
  if (is.null(ensemble.samples)) {
    load(file.path(settings$outdir, "samples.Rdata"))
  }
  #reformatting params
  new.params <- PEcAnAssimSequential:::sda_matchparam(settings, ensemble.samples, site.ids, nens)
  #sample met ensemble members
  #sample all inputs specified in the settings$ensemble
  #now looking into the xml
  samp <- conf.settings$ensemble$samplingspace
  #finding who has a parent
  parents <- lapply(samp,'[[', 'parent')
  #order parents based on the need of who has to be first
  order <- names(samp)[lapply(parents, function(tr) which(names(samp) %in% tr)) %>% unlist()] 
  #new ordered sampling space
  samp.ordered <- samp[c(order, names(samp)[!(names(samp) %in% order)])]
  #performing the sampling
  inputs <- vector("list", length(conf.settings))
  # For the tags specified in the xml I do the sampling
  for (s in seq_along(conf.settings)){
    if (is.null(inputs[[s]])) {
      inputs[[s]] <- list() 
    }
    for (i in seq_along(samp.ordered)){
      #call the function responsible for generating the ensemble
      inputs[[s]][[names(samp.ordered)[i]]] <- input.ens.gen(settings=conf.settings[[s]],
                                                             input=names(samp.ordered)[i],
                                                             method=samp.ordered[[i]]$method,
                                                             parent_ids=NULL)
    }
  }
  ###------------------------------------------------------------------------------------------------###
  ### loop over time                                                                                 ###
  ###------------------------------------------------------------------------------------------------###
  for(t in 1:nt){
    # initialize dat for saving memory usage.
    sda.outputs <- FORECAST <- enkf.params <- ANALYSIS <- ens_weights <- list()
    obs.t <- as.character(lubridate::date(obs.times[t]))
    obs.year <- lubridate::year(obs.t)
    PEcAn.logger::logger.info(paste("Processing Year:", obs.year))
    ###-------------------------------------------------------------------------###
    ###  Taking care of Forecast. Splitting /  Writting / running / reading back###
    ###-------------------------------------------------------------------------###-----  
    #- Check to see if this is the first run or not and what inputs needs to be sent to write.ensemble configs
    if (t>1){
      #for next time step split the met if model requires
      #-Splitting the input for the models that they don't care about the start and end time of simulations and they run as long as their met file.
      PEcAn.logger::logger.info("Splitting mets!")
      inputs.split <- 
        furrr::future_pmap(list(conf.settings %>% `class<-`(c("list")), inputs, model), function(settings, inputs, model) {
          # Loading the model package - this is required bc of the furrr
          library(paste0("PEcAn.",model), character.only = TRUE)
          inputs.split <- inputs
          if (!no_split) {
            for (i in seq_len(nens)) {
              in_path <- tryCatch(inputs$met$samples[[i]], error = function(e) NULL)
              
              # unwrap lists, coerce to a single character, guard empties
              if (is.list(in_path)) in_path <- in_path[[1]]
              if (length(in_path) == 0 || is.null(in_path)) in_path <- NA_character_
              in_path <- as.character(in_path)[1]
              
              # skip if still invalid/empty
              if (is.na(in_path) || !nzchar(in_path)) {
                next
              }
              
              inputs.split$met$samples[i] <- do.call(
                my.split_inputs,
                args = list(
                  settings   = settings,
                  start.time = lubridate::ymd_hms(settings$run$site$met.start, truncated = 3),
                  stop.time  = lubridate::ymd_hms(settings$run$site$met.end, truncated = 3),                       
                  inputs     = in_path
                )
              )
            }
          } else{
            inputs.split <- inputs
          }
          inputs.split
        })
      #---------------- setting up the restart argument for each site separately and keeping them in a list
      restart.list <-
        furrr::future_pmap(
          list(out.configs, conf.settings %>% `class<-`(c("list")), params.list, inputs.split),
          function(configs, settings, new.params, inputs) {
            
            idx_cols <- which(as.character(attr(X, "Site")) %in% as.character(settings$run$site$id))
            new_state_site <- new.state[, idx_cols, drop = FALSE]
            
            if (ncol(new_state_site) == 0) {
              stop(paste0(
                "No matching columns in new.state for site ", settings$run$site$id,
                ". Check attr(X,'Site') vs settings$run$site$id and prior-step rename."
              ))
            }
            if (is.vector(new_state_site)) {
              new_state_site <- matrix(new_state_site)
            }

            list(
              runid       = configs$runs$id,
              start.time  = strptime(obs.times[t - 1], format = "%Y-%m-%d %H:%M:%S") + lubridate::second(lubridate::hms("00:00:01")),
              stop.time   = strptime(obs.times[t], format = "%Y-%m-%d %H:%M:%S"),
              settings    = settings,
              new.state   = new_state_site,
              new.params  = new.params,
              inputs      = list(met = as.list(inputs$met$samples), soilinitcond = as.list(inputs$soilinitcond$samples)),
              RENAME      = TRUE,
              ensemble.id = settings$ensemble$ensemble.id
            )
          }
        )
    } else { ## t == 1
      restart.list <- vector("list", length(conf.settings))
    }
    # release memory.
    gc()
    
    # submit jobs for writing configs.
    PEcAn.logger::logger.info("Writting configs!")
    out.configs <- furrr::future_pmap(
      list(conf.settings %>% `class<-`(c("list")), restart.list, if (t > 1) inputs.split else inputs),
      function(settings, restart.arg, inputs) {
        library(paste0("PEcAn.",settings$model$type), character.only = TRUE)
        samples_arg <- list()
        if (!is.null(inputs) && length(inputs) > 0) {
          for (nm in names(inputs)) {
            smp <- tryCatch(inputs[[nm]]$samples, error = function(e) NULL)
            if (is.null(smp)) next
            smp <- as.list(smp)
            smp <- lapply(smp, function(z) {
              if (is.null(z)) return(NA_character_)
              if (is.list(z)) z <- z[[1]]
              as.character(z)[1]
            })
            samples_arg[[nm]] <- smp
          }
        }
        
        PEcAn.uncertainty::write.ensemble.configs(
          defaults         = settings$pfts,
          ensemble.samples = ensemble.samples,
          settings         = settings,
          model            = settings$model$type,
          write.to.db      = settings$database$bety$write,
          restart          = restart.arg,
          samples          = samples_arg,
          rename           = isTRUE(restart.arg$RENAME)
        )
        
      }) %>% stats::setNames(site.ids)
    
    
    # collect run info.
    # get ensemble ids for each site.
    ensemble.ids <- site.ids %>% furrr::future_map(function(i){
      run.list <- c()
      for (j in 1:nens) {
        run.list <- c(run.list, paste0("ENS-", sprintf("%05d", j), "-", i))
      }
      return(run.list)}, .progress = F) %>% unlist
    runs.tmp <- file.path(rundir, ensemble.ids)
    # local model executions.
    PEcAn.logger::logger.info("Running models!")
    job.files <- file.path(runs.tmp, "job.sh")
    temp <- job.files %>% furrr::future_map(function(f){
      cmd <- paste0("cd ", dirname(f), ";./job.sh")
      system(cmd, intern = F, ignore.stdout = T, ignore.stderr = T)
    }, .progress = F)
    # submit jobs for reading sda outputs.
    PEcAn.logger::logger.info("Reading forecast outputs!")
    reads <- PEcAnAssimSequential:::build_X(out.configs = out.configs, 
                                            settings = settings, 
                                            new.params = new.params, 
                                            nens = nens, 
                                            read_restart_times = read_restart_times, 
                                            outdir = outdir, 
                                            t = t, 
                                            var.names = var.names, 
                                            my.read_restart = my.read_restart,
                                            restart_flag = FALSE)
    #let's read the parameters of each site/ens
    params.list <- reads %>% purrr::map(~.x %>% purrr::map("params"))
    # add namespace for variables inside the foreach.
    X <- reads %>% furrr::future_map(function(r){
      r %>% purrr::map_df(~.x[["X"]] %>% t %>% as.data.frame)
    })
    #replacing crazy outliers before it's too late
    if (control$OutlierDetection){
      X <- outlier.detector.boxplot(X)
      PEcAn.logger::logger.info("Outlier Detection.")
    }
    # convert from forecast list to data frame.
    X <- seq_along(X) %>% furrr::future_map(function(i){
      temp <- do.call(cbind, X[i])
      colnames(temp) <- paste0(var.names, ".", i)
      return(temp)
    }) %>% 
      dplyr::bind_cols() %>%
      `colnames<-`(c(rep(var.names, length(X)))) %>%
      `attr<-`('Site',c(rep(site.ids, each=length(var.names))))
    
    # Make sure both sides are character to avoid 0-col slices later
    attr(X, "Site") <- as.character(attr(X, "Site"))
    site.ids <- as.character(site.ids)
    
    FORECAST[[obs.t]] <- X
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
      # forbid submitting jobs to remote.
      settings$state.data.assimilation$batch.settings$analysis <- NULL
      
      # sanitize each obs covariance: finite, symmetric, positive-ish diagonal
      sanitize_cov <- function(S) {
        if (is.null(S)) return(S)
        S <- as.matrix(S)
        S[!is.finite(S)] <- 0
        # force symmetry
        S <- (S + t(S)) / 2
        d <- diag(S)
        d[!is.finite(d) | d <= 0] <- 1e-6   # tiny ridge on bad/zero diagonals
        diag(S) <- d
        S
      }
      obs.cov[[t]] <- lapply(obs.cov[[t]], sanitize_cov)
      
      # ensure forecast matrix X has no NA/Inf and no zero-variance columns
      X_mat <- as.matrix(X)
      X_mat[!is.finite(X_mat)] <- 0
      col_sd <- apply(X_mat, 2, stats::sd)
      zeroish <- !is.finite(col_sd) | col_sd == 0
      if (any(zeroish)) {
        nZ <- sum(zeroish)
        Xc <- scale(X_mat[, zeroish, drop = FALSE], center = TRUE, scale = FALSE)
        Xc[is.na(Xc)] <- 0
        X_mat[, zeroish] <- Xc + matrix(rnorm(nrow(X_mat) * nZ, sd = 1e-8),
                                        nrow = nrow(X_mat), ncol = nZ)
      }
      X[] <- X_mat
      

      enkf.params[[obs.t]] <- PEcAnAssimSequential:::analysis_sda_block(settings, block.list.all, X, obs.mean, obs.cov, t, nt, MCMC.args, pre_enkf_params, cores)
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
                        params.list = params.list,
                        restart.list = restart.list)
    # save file to the job folder if it's specified.
    if (!is.null(job.folder)) {
      # create the job folder if it doesn't exist.
      if (!file.exists(job.folder)) {
        dir.create(job.folder)
      }
      save(sda.outputs, file = file.path(job.folder, paste0("sda.output", t, ".Rdata")))
    } else {
      save(sda.outputs, file = file.path(settings$outdir, paste0("sda.output", t, ".Rdata")))
    }
    # remove files as SDA runs
    if (!(control$keepNC) && t == 1){
      PEcAn.logger::logger.info("Deleting NC files!")
      outs.tmp <- file.path(outdir, ensemble.ids)
      temp <- outs.tmp %>% furrr::future_map(function(f){
        temp <- list.files(f, "*.nc", full.names = T)
        unlink(temp)
      }, .progress = F)
    }
    if(!is.null(control$send_email)){
      sendmail <- Sys.which("sendmail")
      mailfile <- tempfile("mail")
      cat(paste0("From: ", control$send_email$from, "\n", "Subject: ", "SDA progress report", "\n", "To: ", control$send_email$to, "\n", "\n", paste("Time point:", obs.times[t], "has been completed!")), file = mailfile)
      system2(sendmail, c("-f", paste0("\"", control$send_email$from, "\""), paste0("\"", control$send_email$to, "\""), "<", mailfile))
      unlink(mailfile)
    }
  }
  # assemble results.
  sda.out.files <- file.path(settings$outdir, paste0("sda.output", 1:nt, ".Rdata"))
  analysis.all <- forecast.all <- vector("list", nt)
  for (file in seq_along(sda.out.files)) {
    res_env <- new.env()
    load(sda.out.files[file], envir = res_env)
    analysis.all[[file]] <- res_env$sda.outputs$analysis
    forecast.all[[file]] <- res_env$sda.outputs$forecast
  }
  names(analysis.all) <- as.character(lubridate::date(obs.times))
  names(forecast.all) <- as.character(lubridate::date(obs.times))
  if (!is.null(job.folder)) {
    save(list = c("analysis.all", "forecast.all"), file = file.path(job.folder, "sda.all.forecast.analysis.Rdata"))
  } else {
    save(list = c("analysis.all", "forecast.all"), file = file.path(settings$outdir, "sda.all.forecast.analysis.Rdata"))
  }
  gc()
} # sda.enkf
