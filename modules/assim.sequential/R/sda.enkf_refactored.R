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
#' TimeseriesPlot for post analysis examination, BiasPlot for plotting the correlation between state variables, plot.title is the title of post analysis plots and debug mode allows for pausing the code and examinign the variables inside the function.
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

sda.enkf <- function(settings, obs.mean, obs.cov, Q = NULL, restart=F, 
                     control=list(trace=T,
                                  interactivePlot=T,
                                  TimeseriesPlot=T,
                                  BiasPlot=F,
                                  plot.title=NULL,
                                  debug=FALSE),...) {

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
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  names(var.names) <- NULL
  # Site location first col is the long second is the lat and row names are the site ids
  site.ids <- settings$run$site$id
    
  site.locs <- data.frame(Lon=settings$run$site$lon %>% as.numeric,
                          Lat=settings$run$site$lat %>% as.numeric) %>%
    `colnames<-`(c("Lon","Lat")) %>%
    `rownames<-`(site.ids)
  # filtering obs data based on years specifited in setting > state.data.assimilation
  assimyears <- lubridate::year(settings$state.data.assimilation$start.date) : lubridate::year(settings$state.data.assimilation$end.date) # years that assimilations will be done for - obs will be subsetted based on this
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
    PEcAn.logger::logger.warn(my.split_inputs, "If your model does not need the split function you can specify that in register.Model.xml in model's inst folder by adding <exact.dates>FALSE</exact.dates> tag.")
  }
  ###-------------------------------------------------------------------###
  ### Splitting/Cutting the mets to the start and the end  of SDA       ###
  ###-------------------------------------------------------------------###---- 

  if(!no_split){ 
    for(i in seq_along(settings$run$inputs$met$path)){

      ### model specific split inputs
      settings$run$inputs$met$path[[i]] <-do.call(my.split_inputs, 
                                                  args = list(settings = settings, 
                                                              start.time = lubridate::ymd_hms(settings$state.data.assimilation$start.date, truncated = 3), 
                                                              stop.time = lubridate::ymd_hms(settings$state.data.assimilation$end.date, truncated = 3),
                                                              inputs =  settings$run$inputs$met$path[[i]],
                                                              overwrite=F)) 
    }
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
  interval    <- NULL
  state.interval <- cbind(as.numeric(lapply(settings$state.data.assimilation$state.variables, '[[', 'min_value')),
                          as.numeric(lapply(settings$state.data.assimilation$state.variables, '[[', 'max_value')))
  rownames(state.interval) <- var.names
  # weight matrix
  wt.mat <- matrix(NA, nrow = nens, ncol = nt)
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
  ###-------------------------------------------------------------------###----   
  if (restart){
    if(!file.exists(file.path(settings$outdir,"SDA", "sda.output.Rdata"))) PEcAn.logger::logger.severe("The SDA output from the older simulation doesn't exist.")
    load(file.path(settings$outdir,"SDA", "sda.output.Rdata"))
    if(length(FORECAST)==length(ANALYSIS)) t = t + 1
    #--- Updating the nt and etc
    if(!dir.exists(file.path(settings$outdir,"SDA",assimyears[t]))) dir.create(file.path(settings$outdir,"SDA",assimyears[t]))
    # finding/moving files to it's end year dir
    files.last.sda<-list.files.nodir(file.path(settings$outdir,"SDA"))
    #copying
    file.copy(file.path(file.path(settings$outdir,"SDA"),files.last.sda),
              file.path(file.path(settings$outdir,"SDA"),paste0(assimyears[t],"/",files.last.sda))
    )
    if (processvar) {
      
      if(length(enkf.params) > 1){
        aqq<-enkf.params[[t-1]]$aqq
        bqq<-enkf.params[[t-1]]$bqq
        X.new<-enkf.params[[t-1]]$X.new
      }
      
    }
  }else{
    t = 1
  }
  
  ###------------------------------------------------------------------------------------------------###
  ### loop over time                                                                                 ###
  ###------------------------------------------------------------------------------------------------###---- 
  for(t in seq_len(nt)){
    # do we have obs for this time - what year is it ?
    obs <- which(!is.na(obs.mean[[t]]))
    obs.year <- lubridate::year(names(obs.mean)[t])
    ###-------------------------------------------------------------------------###
    ###  Taking care of Forecast. Splitting / Writting / running / reading back ###
    ###-------------------------------------------------------------------------###-----  
    #- Check to see if this is the first run or not and what inputs needs to be sent to write.ensemble configs
    # Why t>1 is different ? Because the ensemble.write.config would be different. It has the restart argument and it needs it's own setup.
    # plus in t>1 we split the met data for the models that they need that.
    if (t>1){
      #removing old simulations
      unlink(list.files(outdir,"*.nc",recursive = T,full.names = T))
      #-Splitting the input for the models that they don't care about the start and end time of simulations and they run as long as their met file.
      inputs.split <- list()
      if(!no_split){
        for(i in seq_len(nens)){
          #---------------- model specific split inputs
          inputs.split$samples[i] <-do.call(my.split_inputs, 
                                            args = list(settings = settings, 
                                                        start.time = (lubridate::ymd_hms(obs.times[t-1],truncated = 3) + lubridate::second(hms("00:00:01"))), 
                                                        stop.time = obs.times[t],
                                                        inputs = inputs$samples[[i]])) 
          

        } 
      }else{
        inputs.split<-inputs
      }
      #---------------- setting up the restart argument
      restart.arg<-list(runid = run.id, 
                        start.time = strptime(obs.times[t-1],format="%Y-%m-%d %H:%M:%S"),
                        stop.time = strptime(obs.times[t],format="%Y-%m-%d %H:%M:%S"), 
                        settings = settings,
                        new.state = new.state, 
                        new.params = new.params, 
                        inputs = inputs.split, 
                        RENAME = TRUE,
                        ensemble.id=ensemble.id)
      
    }else{
     if(restart == TRUE & length(FORECAST) < t){ #Here when t==1 so spin up has been run but the forcast for t has not. 
        #-Splitting the input for the models that they don't care about the start and end time of simulations and they run as long as their met file.
        inputs.split <- list()
        if(!no_split){
          for(i in seq_len(nens)){
            #---------------- model specific split inputs
            inputs.split$samples[i] <-do.call(my.split_inputs, 
                                              args = list(settings = settings, 
                                                          start.time = (lubridate::ymd_hms(obs.times[t-1],truncated = 3) + lubridate::second(hms("00:00:01"))), 
                                                          stop.time = obs.times[t],
                                                          inputs = inputs$samples[[i]])) 
            
            
          } 
        }else{
          inputs.split<-inputs
        }
        
        restart.arg <- list(runid = run.id, 
                            start.time = settings$run$start.date, #assuming t=1
                            stop.time = strptime(obs.times[t],format="%Y-%m-%d %H:%M:%S"), 
                            settings = settings,
                            new.state = NULL, 
                            new.params = new.params, #this needs to !=NULL because of t=1?
                            inputs = inputs.split, 
                            RENAME = TRUE,
                            ensemble.id=ensemble.id)
      }else{ #
        restart.arg <- NULL
        new.state <- NULL
        new.params <- new.params #this needs to !=NULL because of t=1?
      }
    }
    
    if(length(FORECAST) < t){ #FORECAST for time t needs to run
    #-------------------------- Writing the config/Running the model and reading the outputs for each ensemble
    outconfig <- write.ensemble.configs(defaults = settings$pfts, 
                                        ensemble.samples = ensemble.samples, 
                                        settings = settings,
                                        model = settings$model$type, 
                                        write.to.db = settings$database$bety$write,
                                        restart = restart.arg)
    save(outconfig, file = file.path(settings$outdir,"SDA", "outconfig.Rdata"))
    
    
    run.id <- outconfig$runs$id
    ensemble.id <- outconfig$ensemble.id
    if(t==1) inputs <- outconfig$samples$met # for any time after t==1 the met is the splitted met
    #-------------------------------------------- RUN
    PEcAn.remote::start.model.runs(settings, settings$database$bety$write)
    #------------------------------------------- Reading the output
    X_tmp <- vector("list", 2) 
    X <- list()
    if (control$debug) browser()
    for (i in seq_len(nens)) {
      
      X_tmp[[i]] <- do.call(my.read_restart, args = list(outdir = outdir, 
                                                         runid = run.id[i], 
                                                         stop.time = obs.times[t], 
                                                         settings = settings, 
                                                         var.names = var.names, 
                                                         params = new.params[[i]]))

      # states will be in X, but we also want to carry some deterministic relationships to write_restart
      # these will be stored in params
      X[[i]]      <- X_tmp[[i]]$X
      if (!is.null(X_tmp[[i]]$params)) new.params[[i]] <- X_tmp[[i]]$params
    }
      X <- do.call(rbind, X)
      FORECAST[[t]] <- X
      
    }else{ #FORECAST for time t has ran -- moving to ANALYSIS
      X <- FORECAST[[t]]
      print('Using FORECAST[[t]] from sda.output.Rdata')
      load(file.path(settings$outdir,"SDA", "outconfig.Rdata"))
      run.id <- outconfig$runs$id
      ensemble.id <- outconfig$ensemble.id
      if(t==1) inputs <- outconfig$samples$met # for any time after t==1 the met is the splitted met
      
      if (processvar) {
        aqq<-enkf.params[[t-1]]$aqq
        bqq<-enkf.params[[t-1]]$bqq
      }
    }
    
    if(!exists('Cmcmc_tobit2space') | !exists('Cmcmc')) {
      recompile = TRUE
    }else{
      recompile = FALSE
    }
    save(t, X, FORECAST, ANALYSIS, enkf.params, new.state, new.params, run.id, ensemble.id, ensemble.samples, inputs, file = file.path(settings$outdir,"SDA", "sda.output.Rdata"))
    
    ###-------------------------------------------------------------------###
    ###  preparing OBS                                                    ###
    ###-------------------------------------------------------------------###---- 
    if (any(obs)) {
      # finding obs data
      
      #which type of observation do we have at this time point?
      input.order <- sapply(input.vars, agrep, x=names(obs.mean[[t]]))
      names(input.order) <- operators
      input.order.cov <- sapply(input.vars, agrep, x=colnames(obs.cov[[t]]))
      names(input.order.cov) <- operators
      
      ### this is for pfts not sure if it's always nessecary?
      choose <- sapply(colnames(X), agrep, x=names(obs.mean[[t]]), max=1, USE.NAMES = F) %>% unlist
      choose.cov <- sapply(colnames(X), agrep, x=colnames(obs.cov[[t]]), max=1, USE.NAMES = F) %>% unlist
      
      if(!any(choose)){
        choose <- unlist(input.order)
        choose <- order(names(obs.mean[[t]]))
        choose.cov <- unlist(input.order.cov)
        choose.cov <- order(colnames(obs.cov[[t]]))
        #substr(names(obs.mean[[t]]),nchar(names(choose)[1])+1,max(nchar(names(obs.mean[[t]]))))
      }
      # droping the ones that their means are zero 
      na.obs.mean <- which(is.na(unlist(obs.mean[[t]][choose])))
      if (length(na.obs.mean)>0) choose <- choose [-na.obs.mean]
      
      Y <- unlist(obs.mean[[t]][choose])
      
      R <- as.matrix(obs.cov[[t]][choose.cov,choose.cov])
      R[is.na(R)]<-0.1
      
      if (control$debug) browser()
      
      # making the mapping matrix
      H <- Construct_H(choose, Y, X)
      ###-------------------------------------------------------------------###
      ### Analysis                                                          ###
      ###-------------------------------------------------------------------###----
      if(processvar == FALSE){an.method<-EnKF  }else{    an.method<-GEF   }  
      #-analysis function
      enkf.params[[t]] <- Analysis.sda(settings,
                                       FUN=an.method,
                                       Forecast=list(Q=Q, X=X),
                                       Observed=list(R=R, Y=Y),
                                       H=H,
                                       extraArg=list(aqq=aqq, bqq=bqq, t=t),
                                       nt=nt,
                                       obs.mean=obs.mean,
                                       obs.cov=obs.cov
                                       
      )
      #Reading back mu.f/Pf and mu.a/Pa
      #Forecast
      mu.f <- enkf.params[[t]]$mu.f
      Pf <- enkf.params[[t]]$Pf
      #Analysis
      Pa <- enkf.params[[t]]$Pa
      mu.a <- enkf.params[[t]]$mu.a
      
      diag(Pf)[which(diag(Pf) == 0)] <- 0.1 ## hack for zero variance
      #extracting extra outputs
      if (processvar) {
        aqq<-enkf.params[[t]]$aqq
        bqq<-enkf.params[[t]]$bqq
        X.new<-enkf.params[[t]]$X.new
      }
      ###-------------------------------------------------------------------###
      ### Trace                                                             ###
      ###-------------------------------------------------------------------###----      
      #-- writing Trace--------------------
      if(control$trace) {
        PEcAn.logger::logger.info ("\n --------------------------- ",obs.year," ---------------------------\n")
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
        mu.a <- mu.f
        if(is.null(q.bar)){
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
      if (processvar) {X.adj.arg <- X.new }else{ X.adj.arg <- X }
      analysis <-adj.ens(Pf, X.adj.arg, mu.f, mu.a, Pa)
    }else{
      analysis <- as.data.frame(rmvnorm(as.numeric(nrow(X)), mu.a, Pa, method = "svd"))
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
    ### Interactive plotting ------------------------------------------------------   
    if (t > 1 & control$interactivePlot) { #
      print(interactive.plotting.sda(settings,t,obs.times,obs.mean,obs.cov,obs,X,FORECAST,ANALYSIS))
    }
    ###-------------------------------------------------------------------###
    ### save outputs                                                      ###
    ###-------------------------------------------------------------------###---- 
    Viz.output <- list(settings, obs.mean, obs.cov) #keeping obs data and settings for later visualization in Dashboard
    
    save(site.locs, t, X, FORECAST, ANALYSIS, enkf.params, new.state, new.params, run.id,
         ensemble.id, ensemble.samples, inputs, Viz.output,  file = file.path(settings$outdir,"SDA", "sda.output.Rdata"))
    #writing down the image - either you asked for it or nor :)
    post.analysis.ggplot(settings, t, obs.times, obs.mean, obs.cov, obs, X, FORECAST, ANALYSIS, plot.title=control$plot.title)
    
  } ### end loop over time
  ###-------------------------------------------------------------------###
  ### time series plots                                                 ###
  ###-------------------------------------------------------------------###----- 
  #post.alaysis.ggplot(settings,t,obs.times,obs.mean,obs.cov,obs,X,FORECAST,ANALYSIS,plot.title=control$plot.title)
  if(control$TimeseriesPlot) PEcAn.assim.sequential:::post.analysis.ggplot.violin(settings, t, obs.times, obs.mean, obs.cov, obs, X, FORECAST, ANALYSIS)
  #if(control$TimeseriesPlot) postana.timeser.plotting.sda(settings,t,obs.times,obs.mean,obs.cov,obs,X,FORECAST,ANALYSIS)
  ###-------------------------------------------------------------------###
  ### bias diagnostics                                                  ###
  ###-------------------------------------------------------------------###----
  if(control$BiasPlot)   PEcAn.assim.sequential:::postana.bias.plotting.sda(settings,t,obs.times,obs.mean,obs.cov,obs,X,FORECAST,ANALYSIS)
  ###-------------------------------------------------------------------###
  ### process variance plots                                            ###
  ###-------------------------------------------------------------------###----- 
  if (processvar) postana.bias.plotting.sda.corr(t,obs.times,X,aqq,bqq)
  
} # sda.enkf
