#' @title sda.enkf
#' @name  sda.enkf
#' @author Michael Dietze\email{dietze@@bu.edu}, Ann Raiho, Istem fer, Hamze Dokoohaki
#' 
#' @param settings    PEcAn multi-settings object
#' @param obs.mean    list of observations of the means of state variable (time X nstate). For each date there has to be a list of means for all sites named by the siteid.
#' @param obs.cov     list of observations of covariance matrices of state variables (time X nstate X nstate). For each date there has to be a list of cov for all sites named by the siteid.
#' @param Q           process covariance matrix given if there is no data to estimate it.
#' @param restart     Used for iterative updating previous forecasts. When the restart is TRUE it read the obejct in SDA folder writen from previous SDA.
#' @param control    List of flags controling the behaviour of the SDA. trace for reporting back the SDA outcomes, interactivePlot for ploting the outcomes after each step, 
#' TimeseriesPlot for post analysis examination, BiasPlot for plotting ..., plot.title is the title of post analysis plots and debug mode allows for pausing the code and examinign the variables inside the function.
#'
#’ @details
#’ Restart mode:  Basic idea is that during a restart (primary case envisioned as an iterative forecast), a new workflow folder is created and the previous forecast for the start_time is copied over. During restart the initial run before the loop is skipped, with the info being populated from the previous run. The function then dives right into the first Analysis, then continues on like normal.
#' 
#' @description State Variable Data Assimilation: Ensemble Kalman Filter and Generalized ensemble kalman file=ter
#' 
#' @return NONE
#' @import nimble
#' @export
#' 
sda.enkf.multisite <- function(settings, obs.mean, obs.cov, Q = NULL, restart=F, 
                     control=list(trace=T,
                                  interactivePlot=T,
                                  TimeseriesPlot=T,
                                  BiasPlot=F,
                                  plot.title=NULL,
                                  debug=FALSE),...) {
  #------------- Some important variables
  # out.configs is a list of all the configs after write.condfig.ens
  # conf.settings this keeps a list of setting either multi sites or not allowing to run map/loop for everything
  # params.list  list of params per settings
  # inputs.split list of inputs per settings
  # muf/Pf - forecast mean and covariance
  # Y/R    - Observed data and covariance
  # mu.a/Pa  - after analysis - new mean and covariance
  # nt is the length of observed  
  # When processvar == FALSE it means we are doing EnKF and when it's TRUE Generalized Ensemble Filter
  # Generalized Ensemble Filter NEEDS process variance to avoid filter divergence and it does not
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
  Localization.FUN <- settings$state.data.assimilation$Localization.FUN # localization function
  scalef <- settings$state.data.assimilation$scalef %>% as.numeric() # scale factor for localization
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  names(var.names) <- NULL
  multi.site.flag <- PEcAn.settings::is.MultiSettings(settings)
  #------------------------------Multi - site specific - settings
  #Here i'm trying to make a temp config list name and put it into map to iterate
  if(multi.site.flag){
    conf.settings<-settings
    site.ids <- conf.settings %>% map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()
    # a matrix ready to be sent to spDistsN1 in sp package - first col is the long second is the lat and row names are the site ids
    site.locs <- conf.settings %>% map(~.x[['run']] ) %>% map('site') %>% map_dfr(~c(.x[['lon']],.x[['lat']]) %>%as.numeric)%>% 
        t %>%
      `colnames<-`(c("Lon","Lat")) %>%
        `rownames<-`(site.ids)
  }else{
    conf.settings<-list(settings)
  }
  

  #filtering obs data based on years specifited in setting > state.data.assimilation
  assimyears <- year(settings$state.data.assimilation$start.date) : year(settings$state.data.assimilation$end.date) # years that assimilations will be done for - obs will be subsetted based on this
  obs.mean <- obs.mean[sapply(year(names(obs.mean)), function(obs.year) obs.year %in% (assimyears))]
  obs.cov <- obs.cov[sapply(year(names(obs.cov)), function(obs.year) obs.year %in% (assimyears))]
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
  #reformatting params
  params <- list()
  for (i in seq_len(nens)) {
    params[[i]] <- lapply(ensemble.samples, function(x, n) {
      x[i, ] }, n = i)
  } 
  new.params <- params
  
  ###-------------------------------------------------------------------###
  ### If this is a restart - Picking up were we left last time          ###
  ###-------------------------------------------------------------------###----   
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
    obs.year <- year(names(obs.mean)[t])
    ###-------------------------------------------------------------------------###
    ###  Taking care of Forecast. Splitting /  Writting / running / reading back###
    ###-------------------------------------------------------------------------###-----  
    #- Check to see if this is the first run or not and what inputs needs to be sent to write.ensemble configs
    if (t>1){
   
      #removing old simulations
      unlink(list.files(outdir,"*.nc",recursive = T,full.names = T))
      #-Splitting the input for the models that they don't care about the start and end time of simulations and they run as long as their met file.
      inputs.split <- conf.settings %>%
        purrr::map2(inputs,function(settings,inputs){
          inputs.split <- list()
          for(i in seq_len(nens)){
            if(!no_split){ 
              #---------------- model specific split inputs
              inputs.split$samples[i] <- do.call(my.split_inputs, 
                                                 args = list(settings = settings, 
                                                            start.time = (ymd_hms(obs.times[t-1],truncated = 3) + second(hms("00:00:01"))), 
                                                            stop.time = obs.times[t],
                                                            inputs = inputs$samples[[i]]))
            }else{
              inputs.split<-inputs
            }
          }
          inputs.split
        })
          
      #---------------- setting up the restart argument for each site separatly and keeping them in a list
      restart.list <- purrr::pmap(list(out.configs, conf.settings, params.list, inputs.split), 
                                  function(configs, settings, new.params, inputs){
                                
                                    list(runid = configs$runs$id, 
                                         start.time = strptime(obs.times[t-1],format="%Y-%m-%d %H:%M:%S"),
                                         stop.time = strptime(obs.times[t],format="%Y-%m-%d %H:%M:%S"), 
                                         settings = settings,
                                         new.state = new.state, #!!!!!!!!!!
                                         new.params = new.params, 
                                         inputs = inputs, 
                                         RENAME = TRUE,
                                         ensemble.id=settings$ensemble$ensemble.id)
                                  })
      
      
    }else{
      restart.list <- vector("list",length(conf.settings))
      new.params <- params
    }
    #-------------------------- Writing the config/Running the model and reading the outputs for each ensemble

    conf.settings %>%
      purrr::map2(restart.list,function(settings,restart.arg){
    
        # wrtting configs for each settings - this does not make a difference with the old code 
        write.ensemble.configs(defaults = settings$pfts, 
                               ensemble.samples = ensemble.samples, 
                               settings = settings,
                               model = settings$model$type, 
                               write.to.db = settings$database$bety$write,
                               restart = restart.arg)
      })->out.configs
    
     
  
    #run.id <- outconfig$runs$id
    #ensemble.id <- outconfig$ensemble.id
    if(t==1)  inputs<-out.configs %>% map(~.x[['samples']][['met']]) # for any time after t==1 the met is the splitted met
   
    #-------------------------------------------- RUN
    PEcAn.remote::start.model.runs(settings, settings$database$bety$write)
    #------------------------------------------- Reading the output
    out.configs %>%
      purrr::map(function(configs){
        
        X_tmp <- vector("list", 2) 
        
        for (i in seq_len(nens)) {
          
          X_tmp[[i]] <- do.call(my.read_restart, args = list(outdir = outdir, 
                                                             runid = configs$runs$id[i], 
                                                             stop.time = obs.times[t], 
                                                             settings = settings, 
                                                             var.names = var.names, 
                                                             params = params[[i]])
                              )

        }
        return(X_tmp)
      })->reads
    
  
    #let's read the parameters of each site/ens
    params.list <- reads %>% map(~.x %>% map("params"))
    # Now let's read the state variables of site/ens
    X <- reads %>% map(~.x %>% map_df(~.x[["X"]] %>% t %>% as.data.frame))
    
    # Now we have a matrix that columns are state variables and rows are ensembles.
    # this matrix looks like this
    #         GWBI    AbvGrndWood   GWBI    AbvGrndWood
    #[1,]  3.872521     37.2581  3.872521     37.2581
    # But therer is an attribute called site which tells yout what column is for what site id - check out attr (X,"Site")
    if (multi.site.flag)
    X <- X %>%
          map_dfc(~.x) %>% 
          as.matrix() %>%
          `colnames<-`(c(rep(var.names, length(X)))) %>%
          `attr<-`('Site',c(rep(site.ids, each=length(X))))
    
    
 
    #X <- do.call(rbind, X)
    FORECAST[[t]] <- X
    mu.f <- as.numeric(apply(X, 2, mean, na.rm = TRUE)) %>%
            `attr<-`('Site', c(rep(site.ids, each=length(site.ids))))
    # I make the Pf in a separate function
    if(multi.site.flag & length(site.ids)>1){
  
      #Finding the dis between sites
      distances <- sp::spDists(site.locs+rnorm(4,0,1),longlat=T)
      #turn that into a blocked matrix format
      blocked.dis<-block_matrix(distances %>% as.numeric(), rep(length(var.names), length(site.ids)))

      # This the function and makes the Pf by creating blocks in it for different sites
      # We can also send a localization functions to this 
      # for extra argumnets like distance matrix for localization use elipsis
      Pf <- Contruct.Pf (site.ids, var.names, X, localization.FUN=eval(parse(text = Localization.FUN)), blocked.dis, scalef)
    }else{
      Pf <- cov(X) 
    }

    #diag(Pf)[which(diag(Pf) == 0)] <- 0.1 ## hack for zero variance
    ###-------------------------------------------------------------------###
    ###  preparing OBS                                                    ###
    ###-------------------------------------------------------------------###---- 
    if (any(obs)) {
      #Hamze: used agrep instead of charmatch to take advantage of fuzzy matching
      #there might be little typo/mistake in the names, now this would not be a problem
      #choose <- na.omit(charmatch(colnames(X),names(obs.mean[[t]])))
      Obs.cons <-Construct.R(site.ids, var.names, obs.mean[[t]], obs.cov[[t]])
      
      Y <- Obs.cons$Y
      R <- Obs.cons$R

      if (length(obs.mean[[t]]) > 1) {
        diag(R)[which(diag(R)==0)] <- min(diag(R)[which(diag(R) != 0)])/2
        diag(Pf)[which(diag(Pf)==0)] <- min(diag(Pf)[which(diag(Pf) != 0)])/5
      }
      # making the mapping oprator
      H <- Construct.H.multisite(site.ids, var.names, obs.mean[[t]])
      if (control$debug) browser()
      ###-------------------------------------------------------------------###
      ### Analysis                                                          ###
      ###-------------------------------------------------------------------###----
      if(processvar == FALSE){an.method<-EnKF  }else{    an.method<-GEF   }  
      #-analysis function
      enkf.params[[t]] <- Analysis.sda(settings,
                                       FUN=an.method,
                                       Forcast=list(Pf=Pf,mu.f=mu.f,Q=Q,X=X),
                                       Observed=list(R=R,Y=Y),
                                       H,
                                       choose=choose,
                                       nt=nt,
                                       obs.mean=obs.mean,
                                       obs.cov=obs.cov,
                                       extraArg=list(aqq=aqq,bqq=bqq,t=t)
      )
      
      Pa <- enkf.params[[t]]$Pa
      mu.a <- enkf.params[[t]]$mu.a
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
    save(t, FORECAST, ANALYSIS, enkf.params,new.state,new.params,out.configs,ensemble.samples,inputs, file = file.path(settings$outdir,"SDA", "sda.output.Rdata"))
    #writing down the image - either you asked for it or nor :)
    

  } ### end loop over time
  post.analysis.multisite.ggplot(settings,t,obs.times,obs.mean,obs.cov,obs,X,FORECAST,ANALYSIS,plot.title=control$plot.title)
} # sda.enkf
