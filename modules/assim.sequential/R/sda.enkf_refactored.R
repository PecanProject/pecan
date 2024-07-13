#' @title sda.enkf
#' @name  sda.enkf
#' @author Michael Dietze and Ann Raiho \email{dietze@@bu.edu}
#' 
#' @param settings  PEcAn settings object
#' @param obs.mean  List of dataframe of observation means, named with observation datetime.
#' @param obs.cov   List of covariance matrices of state variables , named with observation datetime.
#' @param Q         Process covariance matrix given if there is no data to estimate it.
#' @param restart   Used for iterative updating previous forecasts. When the restart is TRUE it read the object in SDA folder written from previous SDA.
#' @param control   List of flags controlling the behaviour of the SDA. trace for reporting back the SDA outcomes, interactivePlot for plotting the outcomes after each step, 
#' TimeseriesPlot for post analysis examination, BiasPlot for plotting the correlation between state variables, plot.title is the title of post analysis plots and debug mode allows for pausing the code and examining the variables inside the function.
#'
#’ @details
#’ Restart mode:  Basic idea is that during a restart (primary case envisioned as an iterative forecast), a new workflow folder is created and the previous forecast for the start_time is copied over. During restart the initial run before the loop is skipped, with the info being populated from the previous run. The function then dives right into the first Analysis, then continues on like normal.
#' 
#' @description State Variable Data Assimilation: Ensemble Kalman Filter and Generalized ensemble filter
#' 
#' @return NONE
#' @import nimble
#' @export
#' 

sda.enkf <- function(settings,
                     obs.mean,
                     obs.cov,
                     Q = NULL,
                     restart=NULL, 
                     control=list(trace=TRUE,
                                  interactivePlot=TRUE,
                                  TimeseriesPlot=TRUE,
                                  BiasPlot=FALSE,
                                  plot.title=NULL,
                                  debug=FALSE,
                                  pause=FALSE),
                     ...) {


  if (control$debug) browser()
  ###-------------------------------------------------------------------###
  ### read settings                                                     ###
  ###-------------------------------------------------------------------###
  weight_list <- list()
  adjustment <- settings$state.data.assimilation$adjustment
  model      <- settings$model$type
  write      <- settings$database$bety$write
  defaults   <- settings$pfts
  outdir     <- settings$modeloutdir # currently model runs locally, this will change if remote is enabled
  rundir     <- settings$host$rundir
  host       <- settings$host
  forecast.time.step <- settings$state.data.assimilation$forecast.time.step  #idea for later generalizing
  nens       <- as.numeric(settings$ensemble$size)
  processvar <- as.logical(settings$state.data.assimilation$process.variance)
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  names(var.names) <- NULL
  
  input.vars <- sapply(settings$state.data.assimilation$inputs, '[[', "variable.name")
  operators <- sapply(settings$state.data.assimilation$inputs, '[[', "operator")
  
  # Site location first col is the long second is the lat and row names are the site ids
  site.ids <- settings$run$site$id
  
  site.locs <- data.frame(Lon = as.numeric(settings$run$site$lon),
                          Lat = as.numeric(settings$run$site$lat))
  colnames(site.locs) <- c("Lon","Lat")
  rownames(site.locs) <- site.ids
  # start cut determines what is the best year to start spliting the met based on if we start  with a restart or not.  
  if (!is.null(restart)) {
    start.cut <-lubridate::ymd_hms(settings$state.data.assimilation$start.date, truncated = 3)-1
    Start.Year <-(lubridate::year(settings$state.data.assimilation$start.date)-1)
    
  }else{
    start.cut <-lubridate::ymd_hms(settings$state.data.assimilation$start.date, truncated = 3)
    Start.Year <-(lubridate::year(settings$state.data.assimilation$start.date))
  }
  
  End.Year <-   lubridate::year(settings$state.data.assimilation$end.date) # years that assimilations will be done for - obs will be subsetted based on this
  # filtering obs data based on years specifited in setting > state.data.assimilation
  assimyears<-Start.Year:End.Year
  obs.mean <- obs.mean[sapply(lubridate::year(names(obs.mean)), function(obs.year) obs.year %in% (assimyears))]
  obs.cov <- obs.cov[sapply(lubridate::year(names(obs.cov)), function(obs.year) obs.year %in% (assimyears))]
  # dir address based on the end date
  if(!dir.exists("SDA")) dir.create("SDA",showWarnings = F)
  #--get model specific functions
  do.call("library", list(paste0("PEcAn.", model)))
  my.write_restart <- paste0("write_restart.", model)
  my.read_restart <- paste0("read_restart.", model)
  my.split_inputs  <- paste0("split_inputs.", model)
  #- Double checking some of the inputs
  if (is.null(adjustment)) adjustment<-T
  # models that don't need split_inputs, check register file for that
  register.xml <- system.file(paste0("register.", model, ".xml"), package = paste0("PEcAn.", model))
  register <- XML::xmlToList(XML::xmlParse(register.xml))
  no_split <- !as.logical(register$exact.dates)
  
  if (!exists(my.split_inputs)  &  !no_split) {
    PEcAn.logger::logger.warn(my.split_inputs, "does not exist")
    PEcAn.logger::logger.severe("please make sure that the PEcAn interface is loaded for", model)
    PEcAn.logger::logger.warn(
      my.split_inputs,
      "If your model does not need the split function you can specify that in register.Model.xml in model's inst folder by adding <exact.dates>FALSE</exact.dates> tag."
    )
  }
  ###-------------------------------------------------------------------###
  ### Splitting/Cutting the mets to the start and the end  of SDA       ###
  ###-------------------------------------------------------------------### 
 
  if(!no_split){ 
    for(i in seq_along(settings$run$inputs$met$path)){
      
      ### model specific split inputs
      settings$run$inputs$met$path[[i]] <- do.call(my.split_inputs, 
                                                  args = list(settings = settings, 
                                                              start.time = start.cut, 
                                                              stop.time = lubridate::ymd_hms(settings$state.data.assimilation$end.date, truncated = 3, tz="UTC"),
                                                              inputs =  settings$run$inputs$met$path[[i]],
                                                              overwrite=T)) 
    }
  }
  if (control$debug) browser()
  ###-------------------------------------------------------------------###
  ### tests before data assimilation                                    ###
  ###-------------------------------------------------------------------###----  
  obs.times <- names(obs.mean)
  
  obs.times.POSIX <- lubridate::ymd_hms(obs.times)
  
  ### TO DO: Need to find a way to deal with years before 1000 for paleon ### need a leading zero
  for (i in seq_along(obs.times)) {
    if (is.na(obs.times.POSIX[i])) {
      if (is.na(lubridate::ymd(obs.times[i]))) {
        PEcAn.logger::logger.warn("Error: no dates associated with observations")
      } else {
        ### Data does not have time associated with dates 
        ### Adding 12:59:59PM assuming next time step starts one second later
        PEcAn.logger::logger.warn("Pumpkin Warning: adding one minute before midnight time assumption to dates associated with data")
        obs.times.POSIX[i] <- strptime(paste(obs.times[i], "23:59:59"),format="%Y-%m-%d %H:%M:%S",tz='UTC')#lubridate::ymd_hms(paste(obs.times[i], "23:59:59"))
      }
    }
  }
  obs.times <- obs.times.POSIX
  #obs.times[1] <- strptime('0950-12-31 23:59:59',format="%Y-%m-%d %H:%M:%S",tz="UTC")
  #obs.times[2] <- strptime('0970-12-31 23:59:59',format="%Y-%m-%d %H:%M:%S",tz="UTC")
  #obs.times[3] <- strptime('0990-12-31 23:59:59',format="%Y-%m-%d %H:%M:%S",tz="UTC")
  
  ###-------------------------------------------------------------------###
  ### set up for data assimilation                                      ###
  ###-------------------------------------------------------------------###-----  
  nt          <- length(obs.times)
  if (nt==0)     PEcAn.logger::logger.severe('There has to be at least one observation, before you can start the SDA code.')
  FORECAST    <- ANALYSIS <- list()
  enkf.params <- list()
  #The aqq and bqq are shape parameters estimated over time for the proccess covariance. #see GEF help
  aqq         <- NULL
  bqq         <- numeric(nt + 1)
  ##### Creating matrices that describe the bounds of the state variables
  ##### interval is remade everytime depending on the data at time t
  ##### state.interval stays constant and converts new.analysis to be within the correct bounds
  #### This needs to be moved to GEF
  interval    <- NULL
  state.interval <- cbind(as.numeric(lapply(settings$state.data.assimilation$state.variables, '[[', 'min_value')),
                          as.numeric(lapply(settings$state.data.assimilation$state.variables, '[[', 'max_value')))
  rownames(state.interval) <- var.names
  
  
  # This reads ensemble weights generated by `get_ensemble_weights` function from AssimSequential package
  if(!file.exists(file.path(settings$outdir, "ensemble_weights.Rdata"))){
    PEcAn.logger::logger.warn("ensemble_weights.Rdata cannot be found. Make sure you generate samples by running the get.ensemble.weights function before running SDA if you want the ensembles to be weighted.")
    #create null list
    for(tt in 1:length(obs.times)){
      weight_list[[tt]] <- rep(1,nens) #no weights
    }
  } else{
    load(file.path(settings$outdir, "ensemble_weights.Rdata"))  ## loads ensemble.samples
  }
    


  #Generate parameter needs to be run before this to generate the samples. This is hopefully done in the main workflow.
  if(!file.exists(file.path(settings$outdir, "samples.Rdata"))) PEcAn.logger::logger.severe("samples.Rdata cannot be found. Make sure you generate samples by running the get.parameter.samples function before running SDA.")
  load(file.path(settings$outdir, "samples.Rdata"))  ## loads ensemble.samples
  #reformatting params
  new.params <- list()
  for (i in seq_len(nens)) {
    new.params[[i]] <- lapply(ensemble.samples, function(x, n) {
      x[i, ] }, n = i)
  } 
  
  ###-------------------------------------------------------------------###
  ### If this is a restart - Picking up were we left last time          ###
  ###-------------------------------------------------------------------### 
  if (restart){
    if(!file.exists(file.path(settings$outdir,"SDA", "sda.output.Rdata"))){
      PEcAn.logger::logger.warn("The SDA output from the older simulation doesn't exist.")
      t <- 1
    } else {
      load(file.path(settings$outdir,"SDA", "sda.output.Rdata"))
    }
    
    load(file.path(settings$outdir,"SDA", "outconfig.Rdata"))
    run.id <- outconfig$runs$id
    ensemble.id <- outconfig$ensemble.id
    
    if(FALSE){ # I think let's do this outside of the sda function because sometimes you might want to restart from what you've set up to restart from and it's confusing if your file systems change within the function
      #--- Updating the nt and etc
      if(!dir.exists(file.path(settings$outdir,"SDA",assimyears[t]))) dir.create(file.path(settings$outdir,"SDA",assimyears[t]))
      
      # finding/moving files to it's end year dir
      files.last.sda<-list.files.nodir(file.path(settings$outdir,"SDA"))
      
      #copying
      file.copy(file.path(file.path(settings$outdir,"SDA"),files.last.sda),
                file.path(file.path(settings$outdir,"SDA"),paste0(assimyears[t],"/",files.last.sda)))
    }
    
    if(length(FORECAST) == length(ANALYSIS) && length(FORECAST) > 0) t = 1 + length(FORECAST) #if you made it through the forecast and the analysis in t and failed on the analysis in t+1 so you didn't save t
    
  }else{
    t = 1
  }

  ###------------------------------------------------------------------------------------------------###
  ### loop over time                                                                                 ###
  ###------------------------------------------------------------------------------------------------###---- 
  for(t in t:nt){
    if (control$debug) browser()
    # do we have obs for this time - what year is it ?
    obs <- which(!is.na(obs.mean[[t]]))
    obs.year <- lubridate::year(names(obs.mean)[t])
    ###-------------------------------------------------------------------------###
    ###  Taking care of Forecast. Splitting / Writting / running / reading back ###
    ###-------------------------------------------------------------------------###-----  
    #- Check to see if this is the first run or not and what inputs needs to be sent to write.ensemble configs
    # Why t>1 is different ? Because the ensemble.write.config would be different. It has the restart argument and it needs it's own setup.
    # Also, assumes that sda has gotten through at least one analysis step
    # plus in t>1 we split the met data for the models that they need that.

    ## First question, do we have forecast output to compare to our data?
    ## If not, need to run forecast
    ## using paste because dont want to confuse with ensemble ids
    if(file.exists('run') & file.exists(file.path(settings$outdir,"SDA", "outconfig.Rdata"))){
      
      load(file.path(settings$outdir,"SDA", "outconfig.Rdata")) #need to load these in case during t==1 the analysis crashed so you have a forecast but didn't get to save the sda.output.Rdata
      run.id <- outconfig$runs$id
      ensemble.id <- outconfig$ensemble.id
      if(t==1) inputs <- outconfig$samples$met 
      
      sum_files <-
        sum(unlist(sapply(
          X = run.id,
          FUN = function(x){
            pattern = paste0(x, '/*.nc$')[1]
            grep(
              pattern = pattern,
              x = list.files(file.path(outdir,x), "*.nc$", recursive = F, full.names = T)
            )
          },
          simplify = T
        )))
      
    }else{
      sum_files <- 0 #if rundir hasn't been created yet
    }
    
    
    if (sum_files == 0){ #removing:t > 1
      #removing old simulations #why? don't we need them to restart?
      #unlink(list.files(outdir,"*.nc",recursive = T,full.names = T))
      #-Splitting the input for the models that they don't care about the start and end time of simulations and they run as long as their met file.
      inputs.split <- list()
      if(!no_split & exists('outconfig')){
        for(i in seq_len(nens)){
          #---------------- model specific split inputs
          inputs.split$samples[i] <- do.call(
            my.split_inputs,
            args = list(
              settings = settings,
              start.time = (lubridate::ymd_hms(
                obs.times[t - 1], truncated = 3, tz = "UTC"
              )),
              stop.time = (lubridate::ymd_hms(
                obs.times[t], truncated = 3, tz = "UTC"
              )),
              inputs = inputs$samples[[i]]
            )
          )
          
        } 
        
      }else{
        if(t > 1) inputs.split <- inputs
      }
      #---------------- setting up the restart argument
      
      if(exists('new.state')){ #Has the analysis been run? Yes, then restart from analysis.
          
        if (t == 2) {
            start.time = lubridate::ymd_hms(settings$run$start.date, truncated = 3)
          } else {
            start.time = lubridate::ymd_hms(obs.times[t - 1], truncated = 3)
          }
         restart.arg<-list(runid = run.id, 
                          start.time = start.time,
                          stop.time = lubridate::ymd_hms(obs.times[t], truncated = 3), 
                          settings = settings,
                          new.state = new.state, 
                          new.params = new.params, 
                          inputs = inputs.split, 
                          RENAME = TRUE,
                          ensemble.id=ensemble.id)
      }else{ #The analysis has not been run. Start from beginning with no restart.
        restart.arg = NULL
      }
      
       if(t == 1){
          config.settings = settings 
          config.settings$run$end.date = format(lubridate::ymd_hms(obs.times[t], truncated = 3), "%Y/%m/%d")
          } else {
          config.settings = settings
          }
       
      
      
      #-------------------------- Writing the config/Running the model and reading the outputs for each ensemble
      outconfig <- PEcAn.uncertainty::write.ensemble.configs(defaults = config.settings$pfts, 
                                          ensemble.samples = ensemble.samples, 
                                          settings = config.settings,
                                          model = config.settings$model$type, 
                                          write.to.db = config.settings$database$bety$write,
                                          restart = restart.arg)
      
      save(outconfig, file = file.path(settings$outdir,"SDA", "outconfig.Rdata"))
      
      run.id <- outconfig$runs$id
      ensemble.id <- outconfig$ensemble.id
      if(t==1) inputs <- outconfig$samples$met # for any time after t==1 the met is the split met
      
      if(control$debug) browser()
      #-------------------------------------------- RUN
      PEcAn.workflow::start_model_runs(settings, settings$database$bety$write)
      
      
      
    }
    #------------------------------------------- Reading the output
    X_tmp <- vector("list", 2)
    X <- list()
    for (i in seq_len(nens)) {
      X_tmp[[i]] <- do.call(
        my.read_restart,
        args = list(
          outdir = outdir,
          runid = run.id[i],
          stop.time = obs.times[t],
          settings = settings,
          var.names = var.names,
          params = new.params[[i]]
        )
      )
      
      # states will be in X, but we also want to carry some deterministic relationships to write_restart
      # these will be stored in params
      X[[i]]      <- X_tmp[[i]]$X
      if (!is.null(X_tmp[[i]]$params))
        new.params[[i]] <- X_tmp[[i]]$params
      
    }
    
    #----chaning the extension of nc files to a more specific date related name
   files <-  list.files(
      path = file.path(settings$outdir, "out"),
      "*.nc$",
      recursive = TRUE,
      full.names = TRUE)
   files <-  files[grep(pattern = "SDA*", basename(files), invert = TRUE)]
    
    
   file.rename(files, 
               file.path(dirname(files), 
                  paste0("SDA_", basename(files), "_", gsub(" ", "", names(obs.mean)[t]), ".nc") ) )
    
    #--- Reformating X
    X <- do.call(rbind, X)
    

    #unit scaling if needed 
    
    X <-  rescaling_stateVars(settings, X, multiply = TRUE)
    
    
    if(sum(X,na.rm=T) == 0){
      PEcAn.logger::logger.severe(paste('NO FORECAST for',obs.times[t],'Check outdir logfiles or read restart. Do you have the right variable names?'))
    }
    
    ###-------------------------------------------------------------------###
    ###  preparing OBS                                                    ###
    ###-------------------------------------------------------------------###
    
    if (any(obs)) {
      # finding obs data
      
      #which type of observation do we have at this time point?
      input.order <- sapply(input.vars, agrep, x=names(obs.mean[[t]]))
      names(input.order) <- operators
      input.order.cov <- sapply(input.vars, agrep, x=colnames(obs.cov[[t]]))
      names(input.order.cov) <- operators
      
      ### this is for pfts not sure if it's always nessecary?
      choose <- unlist(sapply(colnames(X), agrep, x=names(obs.mean[[t]]), max=1, USE.NAMES = F))
      choose.cov <- unlist(sapply(colnames(X), agrep, x=colnames(obs.cov[[t]]), max=1, USE.NAMES = F))
      
      if(!any(choose)){
        choose <- unlist(input.order)
        choose <- order(names(obs.mean[[t]]))
        choose.cov <- unlist(input.order.cov)
        choose.cov <- order(colnames(obs.cov[[t]]))
        #substr(names(obs.mean[[t]]),nchar(names(choose)[1])+1,max(nchar(names(obs.mean[[t]]))))
      }
      # droping the ones that their means are zero 
      na.obs.mean <- which(is.na(unlist(obs.mean[[t]][choose])))
      na.obs.cov <- which(is.na(unlist(obs.cov[[t]][choose])))
      if (length(na.obs.mean) > 0) choose <- choose [-na.obs.mean]
      if (length(na.obs.cov) > 0) choose.cov <- choose[-na.obs.cov]
      
      Y <- unlist(obs.mean[[t]][choose])
    
      R <- as.matrix(obs.cov[[t]][choose.cov,choose.cov])
      R[is.na(R)]<-0.1
      
      if (control$debug) browser()
      
      # making the mapping matrix
      #TO DO: doesn't work unless it's one to one
      if(length(operators)==0) H <- Construct_H(choose, Y, X)
      ###-------------------------------------------------------------------###
      ### Analysis                                                          ###
      ###-------------------------------------------------------------------###----
      if(processvar == FALSE){an.method<-EnKF  }else{    an.method<-GEF   }  
      #-extraArgs
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
      
      
      if (is.null(outconfig$samples$met$ids)) {
        wts <- unlist(weight_list[[t]])
      } else {
        wts <- unlist(weight_list[[t]][outconfig$samples$met$ids])
      }
      
      #-analysis function
      enkf.params[[t]] <- Analysis.sda(settings,
                                       FUN=an.method,
                                       Forecast=list(Q=Q, X=X),
                                       Observed=list(R=R, Y=Y),
                                       H=H,
                                       extraArg=list(aqq=aqq, bqq=bqq, t=t,
                                                     recompileTobit=recompileTobit,
                                                     recompileGEF=recompileGEF,
                                                     wts = wts),
                                       nt=nt,
                                       obs.mean=obs.mean,
                                       obs.cov=obs.cov)
      
      #Reading back mu.f/Pf and mu.a/Pa
      FORECAST[[t]] <- X
      #Forecast
      mu.f <- enkf.params[[t]]$mu.f
      Pf <- enkf.params[[t]]$Pf
      #Analysis
      Pa <- enkf.params[[t]]$Pa
      mu.a <- enkf.params[[t]]$mu.a
      
      diag(Pf)[which(diag(Pf) == 0)] <-
        0.1 ## hack for zero variance
      #extracting extra outputs
      if (processvar) {
        aqq <- enkf.params[[t]]$aqq
        bqq <- enkf.params[[t]]$bqq
        X.new <- enkf.params[[t]]$X.new
      }
      ###-------------------------------------------------------------------###
      ### Trace                                                             ###
      ###-------------------------------------------------------------------###----      
      #-- writing Trace--------------------
      if (control$trace) {
        PEcAn.logger::logger.info ("\n --------------------------- ",
                                   obs.year,
                                   " ---------------------------\n")
        PEcAn.logger::logger.info ("\n --------------Obs mean----------- \n")
        print(Y)
        PEcAn.logger::logger.info ("\n --------------Obs Cov ----------- \n")
        print(R)
        PEcAn.logger::logger.info ("\n --------------Forecast mean ----------- \n")
        print(enkf.params[[t]]$mu.f)
        PEcAn.logger::logger.info ("\n --------------Forecast Cov ----------- \n")
        print(enkf.params[[t]]$Pf)
        PEcAn.logger::logger.info ("\n --------------Analysis mean ----------- \n")
        print(t(enkf.params[[t]]$mu.a))
        PEcAn.logger::logger.info ("\n --------------Analysis Cov ----------- \n")
        print(enkf.params[[t]]$Pa)
        PEcAn.logger::logger.info ("\n ------------------------------------------------------\n")
      }
      if (control$debug) browser()
      if (control$pause) readline(prompt="Press [enter] to continue \n")
      
    } else {
      mu.f <- as.numeric(apply(X, 2, mean, na.rm = TRUE))
      Pf <- stats::cov(X)
      ###-------------------------------------------------------------------###
      ### No Observations --                                                ###----
      ###-----------------------------------------------------------------### 
      ### no process variance -- forecast is the same as the analysis ###
      if (processvar==FALSE) {
        mu.a <- mu.f
        Pa   <- Pf + Q
        ### yes process variance -- no data
      } else {
        mu.a <- mu.f
        if(!exists('q.bar')){
          q.bar <- diag(ncol(X))
          PEcAn.logger::logger.info('Process variance not estimated. Analysis has been given uninformative process variance')
        } 
        Pa   <- Pf + solve(q.bar)
      }
      enkf.params[[t]] <- list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa)
    }
    ###-------------------------------------------------------------------###
    ### adjustement/update state matrix                                   ###
    ###-------------------------------------------------------------------###---- 
    
    if(adjustment == TRUE){
      #if we have process var then x is x.new
      if (processvar & exists('X.new')) {X.adj.arg <- X.new }else{ X.adj.arg <- X ; print('using X not X.new. Assuming GEF was skipped this iteration?')}
      analysis <-adj.ens(Pf, X.adj.arg, mu.f, mu.a, Pa)
    }else{
      analysis <- as.data.frame(mvtnorm::rmvnorm(as.numeric(nrow(X)), mu.a, Pa, method = "svd"))
    }
    
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
    ANALYSIS[[t]] <- analysis
    FORECAST[[t]] <- X
    
    
    ###-------------------------------------------------------------------###
    ### save outputs                                                      ###
    ###-------------------------------------------------------------------###---- 
    Viz.output <- list(settings, obs.mean, obs.cov) #keeping obs data and settings for later visualization in Dashboard
    
    save(site.locs, t, X, FORECAST, ANALYSIS, enkf.params, new.state, new.params, run.id,
         ensemble.id, ensemble.samples, inputs, Viz.output,  file = file.path(settings$outdir,"SDA", "sda.output.Rdata"))
    
    
    ### Interactive plotting ------------------------------------------------------   
    if (t > 1 & control$interactivePlot) { #
      print(interactive.plotting.sda(settings,t,obs.times,obs.mean,obs.cov,obs,X,FORECAST,ANALYSIS))
    }
    
  } ### end loop over time
  if (control$debug) browser()
  ###-------------------------------------------------------------------###
  ### time series plots                                                 ###
  ###-------------------------------------------------------------------###----- 
  if(control$TimeseriesPlot) post.analysis.ggplot(settings,t,obs.times,obs.mean,obs.cov,obs,X,FORECAST,ANALYSIS,plot.title=control$plot.title)
  if(control$TimeseriesPlot) PEcAnAssimSequential::post.analysis.ggplot.violin(settings, t, obs.times, obs.mean, obs.cov, obs, X, FORECAST, ANALYSIS)
  ###-------------------------------------------------------------------###
  ### bias diagnostics                                                  ###
  ###-------------------------------------------------------------------###----
  if(control$BiasPlot)   PEcAnAssimSequential::postana.bias.plotting.sda(settings,t,obs.times,obs.mean,obs.cov,obs,X,FORECAST,ANALYSIS)
  ###-------------------------------------------------------------------###
  ### process variance plots                                            ###
  ###-------------------------------------------------------------------###----- 
  if (processvar & control$BiasPlot) postana.bias.plotting.sda.corr(t,obs.times,X,aqq,bqq)
  
} # sda.enkf
