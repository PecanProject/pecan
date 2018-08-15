#' @export
sda.enkf.refactored <- function(settings,
                                obs.mean,
                                obs.cov,
                                Q = NULL,
                                restart=F,
                                control=list(trace=T,
                                             interactivePlot=T,
                                             TimeseriesPlot=T,
                                             BiasPlot=F,
                                             plot.title=NULL,
                                             debug=FALSE
                                             ),...) {
  # My personal notes -----------------------
  # Two analysis function was initially developed into this:
  # 1-EnKF
  # 2-Generalized Ensubmle Filter: -tobit / -wish
  #------------- Some important variables  
  # muf/Pf - forcast mean and covariance
  # Y/R    - Observed data and covariance
  # mu.a/Pa  - afetr analysis - new mean and covariance
  # nt is the length of observed  
  # When processvar == FALSE it means we are doin EnKF and when it's TRUE Generlized Ensumble Filter
  # Generlized Ensumble Filter NEEDS process variance to avoid filter divergence and it does not
  # have analytical solution - needs MCMC
  # X stores IC of state variables and then collects state variables in each loop
  # Y stores the observed mean
  # Assimilation is done for start:end setting assimilation section - if it's a continuation of another sda (restart)
  # then start date in the second xml should be the same as the first pecan xml
  # Models that they wanna be added for SDA their read_restart needs to be in a certain format. look into read_restart_SIPNET
  #-------------------------------------------------------------------------------  
  ymd_hms <- lubridate::ymd_hms
  hms     <- lubridate::hms
  second  <- lubridate::second
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
  var.names <- unlist(sapply(settings$state.data.assimilation$state.variable, 
                             function(x) {
                               x$variable.name
                             }, 
                             USE.NAMES = FALSE), 
                      use.names = FALSE)
  names(var.names) <- NULL
  #filtering obs data based on years specifited in setting > state.data.assimilation
  assimyears<- year(settings$state.data.assimilation$start.date):year(settings$state.data.assimilation$end.date) # years that assimilations will be done for - obs will be subsetted based on this
  obs.mean<-obs.mean[sapply(year(names(obs.mean)),function(obs.year) obs.year%in%(assimyears))]
  obs.cov<-obs.cov[sapply(year(names(obs.cov)),function(obs.year) obs.year%in%(assimyears))]
  # dir address based on the end date
  if(!dir.exists("SDA")) dir.create("SDA",showWarnings = F)
  #--get model specific functions
  do.call("require", list(paste0("PEcAn.", model)))
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
  }
  ###-------------------------------------------------------------------###
  ### Splitting/Cutting the mets to the start and the end  of SDA       ###
  ###-------------------------------------------------------------------###---- 
  
  for(i in seq_along(settings$run$inputs$met$path)){
    if(!no_split){ 
      ### model specific split inputs
      
      settings$run$inputs$met$path[[i]] <-do.call(my.split_inputs, 
                                        args = list(settings = settings, 
                                                    start.time = ymd_hms(settings$state.data.assimilation$start.date,truncated = 3), 
                                                    stop.time = settings$state.data.assimilation$end.date,
                                                    inputs =  settings$run$inputs$met$path[[i]],
                                                    overwrite=F)) 
      
    }
  }
  
  ###-------------------------------------------------------------------###
  ### tests before data assimilation                                    ###
  ###-------------------------------------------------------------------###----  
  obs.times <- names(obs.mean)
  obs.times.POSIX <- ymd_hms(obs.times)

  for (i in seq_along(obs.times)) {
    if (is.na(obs.times.POSIX[i])) {
      if (is.na(lubridate::ymd(obs.times[i]))) {
        print("Error: no dates associated with observations")
      } else {
        ### Data does not have time associated with dates 
        ### Adding 12:59:59PM assuming next time step starts one second later
        print("Pumpkin Warning: adding one minute before midnight time assumption to dates associated with data")
        obs.times.POSIX[i] <- ymd_hms(paste(obs.times[i], "23:59:59"))
      }
    }
  }
  obs.times <- obs.times.POSIX
  ###-------------------------------------------------------------------###
  ### set up for data assimilation                                      ###
  ###-------------------------------------------------------------------###-----  
  nt          <- length(obs.times)
  if (nt==0) stop('There has to at least one Obs')
  FORECAST    <- ANALYSIS <- list()
  enkf.params <- list()
  aqq         <- NULL
  bqq         <- numeric(nt + 1)
  CI.X1       <- matrix(0, 3, nt) # it was taken care of
  CI.X2       <- CI.X1            # it was taken care of
  #q.bar        <- NULL #default process covariance matrix
  ##### Creating matrices that describe the bounds of the state variables
  ##### interval is remade everytime depending on the data at time t
  ##### state.interval stays constant and converts new.analysis to be within the correct bounds
  interval    <- NULL
  state.interval <- cbind(as.numeric(lapply(settings$state.data.assimilation$state.variables,'[[','min_value')),
                          as.numeric(lapply(settings$state.data.assimilation$state.variables,'[[','max_value')))
  rownames(state.interval) <- var.names
  
  # weight matrix
  wt.mat <- matrix(NA, nrow = nens, ncol = nt)
  #Generate parameter needs to be run before this to generate the samples. This is hopefully done in the main workflow.
  load(file.path(settings$outdir, "samples.Rdata"))  ## loads ensemble.samples

  ###-------------------------------------------------------------------###
  ### If this is a restart - Picking up were we left last time          ###
  ###-------------------------------------------------------------------###---- 
  #reformatting params
  params <- list()
  for (i in seq_len(nens)) {
    params[[i]] <- lapply(ensemble.samples, function(x, n) {
      x[i, ] }, n = i)
  } 
  new.params <- params
  
  if (restart){
    load(file.path(settings$outdir,"SDA", "sda.output.Rdata"))
    #--- Updating the nt and etc
    if(!dir.exists(file.path(settings$outdir,"SDA",assimyears[t]))) dir.create(file.path(settings$outdir,"SDA",assimyears[t]))
    # finding/moving files to it's end year dir
    files.last.sda<-list.files.nodir(file.path(settings$outdir,"SDA"))
    #copying
       file.copy(file.path(file.path(settings$outdir,"SDA"),files.last.sda),
                 file.path(file.path(settings$outdir,"SDA"),paste0(assimyears[t],"/",files.last.sda))
                 )
       params<-new.params
  }else{
    t<-0
  }

  ###------------------------------------------------------------------------------------------------###
  ### loop over time                                                                                 ###
  ###------------------------------------------------------------------------------------------------###---- 
  while(t<nt){
    t<-t+1
    # do we have obs for this time - what year is it ?
    obs <- which(!is.na(obs.mean[[t]]))
    obs.year<-year(names(obs.mean)[t])
    ###-------------------------------------------------------------------------###
    ###  Taking care of Forecast. Splitting /  Writting / running / reading back###
    ###-------------------------------------------------------------------------###-----  
    #- Check to see if this is the first run or not and what inputs needs to be sent to write.ensemble configs
    if (t>1){
      #removing old simulations
      unlink(list.files(outdir,"*.nc",recursive = T,full.names = T))
      #-Splitting the input for the models that they don't care about the start and end time of simulations and they run as long as their met file.
      inputs.split <- list()
      for(i in seq_len(nens)){
        if(!no_split){ 
     #---------------- model specific split inputs
          inputs.split$samples[i] <-do.call(my.split_inputs, 
                                      args = list(settings = settings, 
                                                  start.time = (ymd_hms(obs.times[t-1],truncated = 3) + second(hms("00:00:01"))), 
                                                  stop.time = obs.times[t],
                                                  inputs = inputs$samples[[i]])) 
          
        }else{
          inputs.split<-inputs
          }
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
      restart.arg<-NULL
      new.params <- params
    }
  #-------------------------- Writting the config/ Running the model and reading the outputs for each ensemble
    write.ensemble.configs(defaults = settings$pfts, 
                           ensemble.samples = ensemble.samples, 
                           settings = settings,
                           model = settings$model$type, 
                           write.to.db = settings$database$bety$write,
                           restart = restart.arg)->outconfig


    run.id<-outconfig$runs$id
    ensemble.id<-outconfig$ensemble.id
   if(t==1) inputs<-outconfig$samples$met # for any time after t==1 the met is the splitted met
    #-------------------------------------------- RUN
    PEcAn.remote::start.model.runs(settings, settings$database$bety$write)
    #------------------------------------------- Reading the output
    X_tmp <- vector("list", 2) 
    X <- list()
    for (i in seq_len(nens)) {

      X_tmp[[i]] <- do.call(my.read_restart, args = list(outdir = outdir, 
                                                         runid = run.id[i], 
                                                         stop.time = obs.times[t], 
                                                         settings = settings, 
                                                         var.names = var.names, 
                                                         params = params[[i]]
                                                         )
                            )
      # states will be in X, but we also want to carry some deterministic relationships to write_restart
      # these will be stored in params
      X[[i]]      <- X_tmp[[i]]$X
      if (!is.null(X_tmp[[i]]$params)) new.params[[i]] <- X_tmp[[i]]$params
        
    }
   
    X <- do.call(rbind, X)
    FORECAST[[t]] <- X
    mu.f <- as.numeric(apply(X, 2, mean, na.rm = TRUE))
    Pf <- cov(X)
    diag(Pf)[which(diag(Pf) == 0)] <- 0.1 ## hack for zero variance
    ###-------------------------------------------------------------------###
    ###  preparing OBS                                                    ###
    ###-------------------------------------------------------------------###---- 
    if (any(obs)) {
      #Hamze: used agrep instead of charmatch to take advantage of fuzzy matching
      #there might be little typo/mistake in names, now this would not be a problem
      #choose <- na.omit(charmatch(colnames(X),names(obs.mean[[t]])))
      choose <-sapply(colnames(X),agrep,x=names(obs.mean[[t]]),max=1,USE.NAMES = F)%>%unlist
      
      Y <- unlist(obs.mean[[t]][choose])
      Y[is.na(Y)] <- 0 
      
      R <- as.matrix(obs.cov[[t]][choose,choose])
      R[is.na(R)]<-0
      
      if (length(obs.mean[[t]]) > 1) {
        diag(R)[which(diag(R)==0)] <- min(diag(R)[which(diag(R) != 0)])/2
        diag(Pf)[which(diag(Pf)==0)] <- min(diag(Pf)[which(diag(Pf) != 0)])/5
      }
      if (control$debug) browser()
    ###-------------------------------------------------------------------###
    ### Analysis                                                          ###
    ###-------------------------------------------------------------------###----
      if(processvar == FALSE){an.method<-EnKF  }else{    an.method<-GEF   }  
      #-analysis function
        enkf.params[[t]] <-Analysis.sda(settings,
                                        FUN=an.method,
                                        Forcast=list(Pf=Pf,mu.f=mu.f,Q=Q,X=X),
                                        Observed=list(R=R,Y=Y),
                                        choose=choose,
                                        nt=nt,
                                        obs.mean=obs.mean,
                                        obs.cov=obs.cov,
                                        extraArg=list(aqq=aqq,bqq=bqq,t=t)
                                        )

      Pa<- enkf.params[[t]]$Pa
      mu.a<- enkf.params[[t]]$mu.a
      #extracting extra outputs
      if (processvar) {
        CI.X1[, t] <- enkf.params[[t]]$CIX1
        CI.X2[, t] <- enkf.params[[t]]$CIX2
        aqq<-enkf.params[[t]]$aqq
        bqq<-enkf.params[[t]]$bqq
        X.new<-enkf.params[[t]]$X.new
      }
      ###-------------------------------------------------------------------###
      ### Trace                                                             ###
      ###-------------------------------------------------------------------###----      
      #-- writing Trace--------------------
      if(control$trace) {
        cat ("\n --------------------------- ",obs.year," ---------------------------\n")
        cat ("\n --------------Obs mean----------- \n")
        print(Y)
        cat ("\n --------------Obs Cov ----------- \n")
        print(R)
        cat ("\n --------------Forcast mean ----------- \n")
        print(enkf.params[[t]]$mu.f)
        cat ("\n --------------Forcast Cov ----------- \n")
        print(enkf.params[[t]]$Pf)
        cat ("\n --------------Analysis mean ----------- \n")
        print(t(enkf.params[[t]]$mu.a))
        cat ("\n --------------Analysis Cov ----------- \n")
        print(enkf.params[[t]]$Pa)
        cat ("\n ------------------------------------------------------\n")
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
          print('Process variance not estimated. Analysis has been given uninformative process variance')
        } 
        Pa   <- Pf + solve(q.bar)
      }
      enkf.params[[t]] <- list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa)
      }
    ###-------------------------------------------------------------------###
    ### adjustement/update state matrix                                   ###
    ###-------------------------------------------------------------------###---- 

    if(adjustment == TRUE){
      analysis <-adj.ens(Pf,X,X.new,mu.f,mu.a,Pa,processvar)
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
    save(t, FORECAST, ANALYSIS, enkf.params,new.state,new.params,run.id,ensemble.id,ensemble.samples,inputs, file = file.path(settings$outdir,"SDA", "sda.output.Rdata"))
    #wrtting down the image - either you asked for it or nor :)
    post.alaysis.ggplot(settings,t,obs.times,obs.mean,obs.cov,obs,X,FORECAST,ANALYSIS,plot.title=control$plot.title)
    
  } ### end loop over time
  ###-------------------------------------------------------------------###
  ### time series plots                                                 ###
  ###-------------------------------------------------------------------###----- 
  #post.alaysis.ggplot(settings,t,obs.times,obs.mean,obs.cov,obs,X,FORECAST,ANALYSIS,plot.title=control$plot.title)
  if(control$TimeseriesPlot) post.alaysis.ggplot.violin(settings,t,obs.times,obs.mean,obs.cov,obs,X,FORECAST,ANALYSIS)
  #if(control$TimeseriesPlot) postana.timeser.plotting.sda(settings,t,obs.times,obs.mean,obs.cov,obs,X,FORECAST,ANALYSIS)
  ###-------------------------------------------------------------------###
  ### bias diagnostics                                                  ###
  ###-------------------------------------------------------------------###----
   if(control$BiasPlot)   postana.bias.plotting.sda(settings,t,obs.times,obs.mean,obs.cov,obs,X,FORECAST,ANALYSIS)
  ###-------------------------------------------------------------------###
  ### process variance plots                                            ###
  ###-------------------------------------------------------------------###----- 
  if (processvar) postana.bias.plotting.sda(t,obs.times,X,aqq,bqq)

} # sda.enkf
